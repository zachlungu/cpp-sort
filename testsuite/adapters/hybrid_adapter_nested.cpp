/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2015 Morwenn
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
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 * THE SOFTWARE.
 */
#include <iterator>
#include <vector>
#include <catch.hpp>
#include <cpp-sort/adapters/hybrid_adapter.h>
#include <cpp-sort/sort.h>
#include <cpp-sort/sorter_base.h>
#include <cpp-sort/sorter_traits.h>

// Type of sorter used for checks
enum class sorter_type
{
    forward,
    bidirectional,
    random_access
};

struct forward_sorter:
    cppsort::sorter_base<forward_sorter>
{
    using cppsort::sorter_base<forward_sorter>::operator();

    template<typename ForwardIterator>
    auto operator()(ForwardIterator, ForwardIterator)
        -> sorter_type
    {
        return sorter_type::forward;
    }
};

struct bidirectional_sorter:
    cppsort::sorter_base<bidirectional_sorter>
{
    using cppsort::sorter_base<bidirectional_sorter>::operator();

    template<typename BidirectionalIterator>
    auto operator()(BidirectionalIterator, BidirectionalIterator)
        -> sorter_type
    {
        return sorter_type::bidirectional;
    }
};

struct random_access_sorter:
    cppsort::sorter_base<random_access_sorter>
{
    using cppsort::sorter_base<random_access_sorter>::operator();

    template<typename RandomAccessIterator>
    auto operator()(RandomAccessIterator, RandomAccessIterator)
        -> sorter_type
    {
        return sorter_type::random_access;
    }
};

namespace cppsort
{
    template<>
    struct sorter_traits<forward_sorter>
    {
        using iterator_category = std::forward_iterator_tag;
        static constexpr bool is_stable = false;
    };

    template<>
    struct sorter_traits<bidirectional_sorter>
    {
        using iterator_category = std::bidirectional_iterator_tag;
        static constexpr bool is_stable = false;
    };

    template<>
    struct sorter_traits<random_access_sorter>
    {
        using iterator_category = std::random_access_iterator_tag;
        static constexpr bool is_stable = false;
    };
}

TEST_CASE( "nested hybrid_adapter types", "[hybrid_adapter]" )
{
    // Make sure that the nesting of hybrid_adapter
    // does not hide more suitable sorters because
    // of the iterator category merging

    // Collection to sort
    std::vector<int> vec(3);

    SECTION( "unwrapping from the front" )
    {
        using sorter = cppsort::hybrid_adapter<
            cppsort::hybrid_adapter<
                forward_sorter,
                random_access_sorter
            >,
            bidirectional_sorter
        >;

        sorter_type res = cppsort::sort(std::begin(vec), std::end(vec), sorter{});
        CHECK( res == sorter_type::random_access );
    }

    SECTION( "unwrapping from the back" )
    {
        using sorter = cppsort::hybrid_adapter<
            bidirectional_sorter,
            cppsort::hybrid_adapter<
                forward_sorter,
                random_access_sorter
            >
        >;

        sorter_type res = cppsort::sort(std::begin(vec), std::end(vec), sorter{});
        CHECK( res == sorter_type::random_access );
    }
}
