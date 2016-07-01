/*
 * ssssort.h
 *
 * Super Scalar Sample Sort
 *
 *******************************************************************************
 * Copyright (C) 2014 Timo Bingmann <tb@panthema.net>
 * Copyright (C) 2016 Lorenz Hübschle-Schneider <lorenz@4z2.de>
 * Copyright (C) 2016 Morwenn <morwenn29@hotmail.fr>
 *
 * The MIT License (MIT)
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.

 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */
#ifndef CPPSORT_DETAIL_SSSSORT_H_
#define CPPSORT_DETAIL_SSSSORT_H_

////////////////////////////////////////////////////////////
// Headers
////////////////////////////////////////////////////////////
#include <algorithm>
#include <cassert>
#include <cmath>
#include <cstddef>
#include <cstdint>
#include <cstring>
#include <ctime>
#include <iterator>
#include <memory>
#include <random>
#include <type_traits>
#include <cpp-sort/sorters/pdq_sorter.h>
#include <cpp-sort/utility/iter_move.h>

// Assumptions, better than assertions
#if defined(__GNUC__) || defined(__clang__)
#   define CPPSORT_ASSUME(cond) do { if (!(cond)) __builtin_unreachable(); } while (0)
#else
#   define CPPSORT_ASSUME(cond) (void)0
#endif

namespace cppsort
{
namespace detail
{
    /*
     * Bucket or input size below which to fall back to the base case sorter
     * (std::sort)
     */
    constexpr std::size_t basecase_size = 1024;

    /*
     * logBuckets determines how many splitters are used.  Sample Sort partitions
     * the data into buckets, whose number is typically a power of two.  Thus, we
     * specify its base-2 logarithms.  For the partitioning into k buckets, we then
     * need k-1 splitters.  logBuckets is a tuning parameter, typically 7 or 8.
     */
    constexpr std::size_t logBuckets = 8;
    constexpr std::size_t numBuckets = 1 << logBuckets;

    /*
     * Type to be used for bucket indices.  In this case, a uint32_t is overkill,
     * but turned out to be fastest.  16-bit arithmetic is peculiarly slow on recent
     * Intel CPUs.  Needs to fit 2*numBuckets-1 (for the step() function), so
     * uint8_t would work for logBuckets = 7
     */
    using bucket_t = std::uint32_t;

    // Provides different sampling strategies to choose splitters
    template<typename Iterator>
    struct Sampler
    {
        using value_type = typename std::iterator_traits<Iterator>::value_type;

        static auto gen()
            -> std::mt19937&
        {
            // Random number generation engine for sampling.  You can swap this out for
            // std::minstd_rand if the Mersenne Twister is too slow on your hardware.

#ifdef __MINGW32__
            thread_local std::mt19937 engine(std::time(nullptr));
#else
            thread_local std::mt19937 engine{std::random_device{}()};
#endif
            return engine;
        }

        // Draw a random sample without replacement using the Fisher-Yates Shuffle.
        // This reorders the input somewhat but the sorting does that anyway.
        static auto draw_sample_fisheryates(Iterator begin, Iterator end,
                                            value_type* samples, std::size_t sample_size)
            -> void
        {
            // Random generator
            assert(begin <= end);
            std::size_t max = static_cast<std::size_t>(end - begin);
            assert(gen().max() >= max);

            for (std::size_t i = 0; i < sample_size; ++i) {
                std::size_t index = gen()() % max--; // biased, don't care
                std::swap(*(begin + index), *(begin + max));
                samples[i] = *(begin + max);
            }
        }

        // Draw a random sample with replacement by generating random indices. On my
        // machine this results in measurably slower sorting than a
        // Fisher-Yates-based sample, so beware the apparent simplicity.
        static auto draw_sample_simplerand(Iterator begin, Iterator end,
                                           value_type* samples, std::size_t sample_size)
            -> void
        {
            // Random generator
            assert(begin <= end);
            const std::size_t size = static_cast<std::size_t>(end - begin);
            assert(gen().max() >= size);

            for (std::size_t i = 0; i < sample_size; ++i) {
                std::size_t index = gen()() % size; // biased, don't care
                samples[i] = *(begin + index);
            }
        }

        // A completely non-random sample that's beyond terrible on sorted inputs
        static auto draw_sample_first(Iterator begin, Iterator,
                                      value_type *samples, std::size_t sample_size)
            -> void
        {
            for (std::size_t i = 0; i < sample_size; ++i) {
                samples[i] = *(begin + i);
            }
        }

        static auto draw_sample(Iterator begin, Iterator end,
                                value_type *samples, std::size_t sample_size)
            -> void
        {
            draw_sample_fisheryates(begin, end, samples, sample_size);
        }
    };

    /*
     * Classify elements into buckets. Template parameter treebits specifies the
     * log2 of the number of buckets (= 1 << treebits).
     */
    template<typename InputIterator,  typename OutputIterator, typename Compare,
             std::size_t treebits=logBuckets, typename bktsize_t=std::size_t>
    struct Classifier
    {
        using value_type = typename std::iterator_traits<InputIterator>::value_type;

        const std::size_t num_splitters = (1 << treebits) - 1;
        const std::size_t splitters_size = 1 << treebits;
        value_type splitters[1 << treebits];

        // maps items to buckets
        bucket_t* const bktout;
        // counts bucket sizes
        std::unique_ptr<bktsize_t[]> bktsize;

        /*
         * Constructs the splitter tree from the given samples
         */
        Classifier(const value_type *samples, const std::size_t sample_size,
                   bucket_t* const bktout):
            bktout(bktout),
            bktsize(std::make_unique<bktsize_t[]>(1 << treebits))
        {
            std::fill(bktsize.get(), bktsize.get() + (1 << treebits), 0);
            build_recursive(samples, samples + sample_size, 1);
        }

        // recursively builds splitter tree. Used by constructor.
        auto build_recursive(const value_type* lo, const value_type* hi, std::size_t pos)
            -> void
        {
            CPPSORT_ASSUME(hi >= lo);
            const value_type *mid = lo + (hi - lo)/2;
            splitters[pos] = *mid;

            if (2 * pos < num_splitters) {
                build_recursive(lo, mid, 2*pos);
                build_recursive(mid + 1, hi , 2*pos + 1);
            }
        }

        // Push an element down the tree one step. Inlined.
        constexpr auto step(bucket_t i, const value_type &key, Compare compare) const
            -> bucket_t
        {
            CPPSORT_ASSUME(i > 0);
            return 2*i + compare(splitters[i], key);
        }

        // Find the bucket for a single element
        constexpr auto find_bucket(const value_type &key, Compare compare) const
            -> bucket_t
        {
            bucket_t i = 1;
            while (i <= num_splitters) i = step(i, key, compare);
            return (i - static_cast<bucket_t>(splitters_size));
        }

        /*
         * Find the bucket for U elements at the same time. This version will be
         * unrolled by the compiler.  Degree of unrolling is a template parameter, 4
         * is a good choice usually.
         */
        template<int U>
        auto find_bucket_unroll(InputIterator key, bucket_t* obkt, Compare compare)
            -> void
        {
            bucket_t i[U];
            for (int u = 0; u < U; ++u) i[u] = 1;

            for (std::size_t l = 0; l < treebits; ++l) {
                // step on all U keys
                for (int u = 0; u < U; ++u) i[u] = step(i[u], *(key + u), compare);
            }
            for (int u = 0; u < U; ++u) {
                i[u] -= splitters_size;
                obkt[u] = i[u];
                bktsize[i[u]]++;
            }
        }

        // classify all elements by pushing them down the tree and saving bucket id
        auto classify(InputIterator begin, InputIterator end, Compare compare,
                      bucket_t* bktout = nullptr)
            -> void
        {
            if (bktout == nullptr) bktout = this->bktout;
            for (InputIterator it = begin; it != end;) {
                bucket_t bucket = find_bucket(*it++, compare);
                *bktout++ = bucket;
                bktsize[bucket]++;
            }
        }

        // Classify all elements with unrolled bucket finding implementation
        template<int U>
        auto classify_unroll(InputIterator begin, InputIterator end, Compare compare)
            -> void
        {
            bucket_t* bktout = this->bktout;
            InputIterator it = begin;
            for (; it + U < end; it += U, bktout += U) {
                find_bucket_unroll<U>(it, bktout, compare);
            }
            // process remainder
            CPPSORT_ASSUME(end - it <= U);
            classify(it, end, compare, bktout);
        }

        /*
         * Distribute the elements in [in_begin, in_end) into consecutive buckets,
         * storage for which begins at out_begin.  Need to class classify or
         * classify_unroll before to fill the bktout and bktsize arrays.
         */
        template<std::size_t U>
        auto distribute(InputIterator in_begin, InputIterator in_end,
                        OutputIterator out_begin)
            -> void
        {
            assert(in_begin <= in_end);
            // exclusive prefix sum
            for (std::size_t i = 0, sum = 0; i < numBuckets; ++i) {
                bktsize_t curr_size = bktsize[i];
                bktsize[i] = sum;
                sum += curr_size;
            }
            const std::size_t n = static_cast<std::size_t>(in_end - in_begin);
            std::size_t i;
            for (i = 0; i + U < n; i += U) {
                for (std::size_t u = 0; u < U; ++u) {
                    *(out_begin + bktsize[bktout[i+u]]++) = std::move(*(in_begin + i + u));
                }
            }
            // process the rest
            CPPSORT_ASSUME(n-i <= U);
            for (; i < n; ++i) {
                *(out_begin + bktsize[bktout[i]]++) = std::move(*(in_begin + i));
            }
        }
    };

    // Factor to multiply number of buckets by to obtain the number of samples drawn
    inline auto oversampling_factor(std::size_t n)
        -> std::size_t
    {
        double r = std::sqrt(double(n) / (2*numBuckets*(logBuckets+4)));
        return std::max<std::size_t>(r, 1);
    }

    /*
     * Internal sorter (argument list isn't all that pretty).
     *
     * begin_is_home indicates whether the output should be stored in the range
     * given by begin and end (=true) or out_begin and out_begin + (end - begin)
     * (=false).
     *
     * It is assumed that the range out_begin to out_begin + (end - begin) is valid.
     */
    template<typename InputIterator, typename OutputIterator, typename Compare>
    auto ssssort_int(InputIterator begin, InputIterator end,
                     OutputIterator out_begin, Compare compare,
                     bucket_t* bktout, bool begin_is_home)
        -> void
    {
        using value_type = typename std::iterator_traits<InputIterator>::value_type;

        assert(begin <= end);
        const std::size_t n = static_cast<std::size_t>(end - begin);

        // draw and sort sample
        const std::size_t sample_size = oversampling_factor(n) * numBuckets;
        auto samples = std::make_unique<value_type[]>(sample_size);
        Sampler<InputIterator>::draw_sample(begin, end, samples.get(), sample_size);
        pdq_sort(samples.get(), samples.get() + sample_size, compare);

        if (not compare(samples[0], samples[sample_size - 1])) {
            // All samples are equal. Clean up and fall back to std::sort
            samples.reset(nullptr);
            pdq_sort(begin, end, compare);
            if (!begin_is_home) {
                std::move(begin, end, out_begin);
            }
            return;
        }

        // classify elements
        Classifier<InputIterator, OutputIterator, Compare, logBuckets>
            classifier(samples.get(), sample_size, bktout);
        samples.reset(nullptr);
        classifier.template classify_unroll<6>(begin, end, compare);
        classifier.template distribute<4>(begin, end, out_begin);

        // Recursive calls. offset is the offset into the arrays (/iterators) for
        // the current bucket.
        std::size_t offset = 0;
        for (std::size_t i = 0; i < numBuckets; ++i) {
            auto size = classifier.bktsize[i] - offset;
            if (size == 0) continue; // empty bucket
            if (size <= basecase_size || (n / size) < 2) {
                // Either it's a small bucket, or very large (more than half of all
                // elements). In either case, we fall back to std::sort.  The reason
                // we're falling back to std::sort in the second case is that the
                // partitioning into buckets is obviously not working (likely
                // because a single value made up the majority of the items in the
                // previous recursion level, but it's also surrounded by lots of
                // other infrequent elements, passing the "all-samples-equal" test.
                pdq_sort(out_begin + offset, out_begin + classifier.bktsize[i], compare);
                if (begin_is_home) {
                    // uneven recursion level, we have to move the result
                    std::move(out_begin + offset,
                              out_begin + classifier.bktsize[i],
                              begin + offset);
                }
            } else {
                // large bucket, apply sample sort recursively
                ssssort_int(
                    out_begin + offset,
                    out_begin + classifier.bktsize[i], // = out_begin + offset + size
                    begin + offset,
                    compare,
                    bktout + offset,
                    !begin_is_home);
            }
            offset += size;
        }
    }

    /*
     * Sort the range [begin, end).
     *
     * Uses <= 3*(end-begin)*sizeof(value_type) bytes of additional memory
     */
    template<typename Iterator, typename Compare>
    auto ssssort(Iterator begin, Iterator end, Compare compare)
        -> void
    {
        using value_type = typename std::iterator_traits<Iterator>::value_type;
        static_assert(std::is_convertible<bool, std::result_of_t<Compare(value_type, value_type)>>::value,
                      "the result of the predicate shall be convertible to bool");

        assert(begin <= end);
        const std::size_t n = static_cast<std::size_t>(end - begin);

        if (n < basecase_size) {
            // base case
            pdq_sort(begin, end, compare);
            return;
        }

        auto out = std::make_unique<value_type[]>(n);
        auto bktout = std::make_unique<bucket_t[]>(n);
        ssssort_int(begin, end, out.get(), compare, bktout.get(), true);
    }
}}

#undef CPPSORT_ASSUME

#endif // CPPSORT_DETAIL_SSSSORT_H_
