/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2016 Morwenn
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
#ifndef CPPSORT_PROBES_EXC_H_
#define CPPSORT_PROBES_EXC_H_

////////////////////////////////////////////////////////////
// Headers
////////////////////////////////////////////////////////////
#include <algorithm>
#include <cstdlib>
#include <functional>
#include <iterator>
#include <type_traits>
#include <vector>
#include <cpp-sort/sorter_facade.h>
#include <cpp-sort/sorter_traits.h>
#include <cpp-sort/sorters/pdq_sorter.h>
#include <cpp-sort/utility/functional.h>
#include <cpp-sort/utility/static_const.h>
#include "../detail/indirect_compare.h"
#include "../detail/iterator_traits.h"

namespace cppsort
{
namespace probe
{
    namespace detail
    {
        struct exc_impl
        {
            template<
                typename ForwardIterator,
                typename Compare = std::less<>,
                typename Projection = utility::identity,
                typename = std::enable_if_t<
                    is_projection_iterator<Projection, ForwardIterator, Compare>
                >
            >
            auto operator()(ForwardIterator first, ForwardIterator last,
                            Compare compare={}, Projection projection={}) const
                -> cppsort::detail::difference_type_t<ForwardIterator>
            {
                using difference_type = cppsort::detail::difference_type_t<ForwardIterator>;

                auto size = std::distance(first, last);
                if (size < 2)
                {
                    return 0;
                }

                ////////////////////////////////////////////////////////////
                // Indirectly sort the iterators

                // Copy the iterators in a vector
                std::vector<ForwardIterator> iterators;
                iterators.reserve(size);
                for (ForwardIterator it = first ; it != last ; ++it)
                {
                    iterators.push_back(it);
                }

                // Sort the iterators on pointed values
                pdq_sorter{}(
                    iterators,
                    cppsort::detail::indirect_compare<Compare, Projection>(compare, projection)
                );

                ////////////////////////////////////////////////////////////
                // Count the number of cycles

                std::vector<bool> sorted(size, false);

                // Element where the current cycle starts
                ForwardIterator start = first;

                difference_type cycles = 0;
                while (start != last)
                {
                    // Find the element to put in current's place
                    ForwardIterator current = start;
                    auto next_pos = std::distance(first, current);
                    ForwardIterator next = iterators[next_pos];
                    sorted[next_pos] = true;

                    // Process the current cycle
                    if (next != current)
                    {
                        while (next != start)
                        {
                            current = next;
                            auto next_pos = std::distance(first, next);
                            next = iterators[next_pos];
                            sorted[next_pos] = true;
                        }
                    }

                    ++cycles;

                    // Find the next cycle
                    auto&& sorted_it = std::begin(sorted) + std::distance(first, start);
                    do
                    {
                        ++start;
                        ++sorted_it;
                    }
                    while (start != last && *sorted_it);
                }
                return size - cycles;
            }
        };
    }

    namespace
    {
        constexpr auto&& exc = utility::static_const<
            sorter_facade<detail::exc_impl>
        >::value;
    }
}}

#endif // CPPSORT_PROBES_EXC_H_
