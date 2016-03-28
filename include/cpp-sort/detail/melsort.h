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
#ifndef CPPSORT_DETAIL_MELSORT_H_
#define CPPSORT_DETAIL_MELSORT_H_

////////////////////////////////////////////////////////////
// Headers
////////////////////////////////////////////////////////////
#include <iterator>
#include <list>
#include <type_traits>
#include <utility>
#include <vector>
#include <cpp-sort/utility/as_function.h>
#include <cpp-sort/utility/inplace_merge.h>
#include <cpp-sort/utility/iter_move.h>
#include "iterator_traits.h"
#include "merge_move.h"
#include "move.h"
#include "projection_compare.h"

namespace cppsort
{
namespace detail
{
    template<typename ForwardIterator, typename Compare, typename Projection>
    auto melsort(ForwardIterator first, ForwardIterator last,
                 Compare compare, Projection projection)
        -> void
    {
        using utility::iter_move;
        using rvalue_reference = std::decay_t<rvalue_reference_t<ForwardIterator>>;
        auto&& proj = utility::as_function(projection);

        std::vector<std::list<rvalue_reference>> lists;

        ////////////////////////////////////////////////////////////
        // Create encroaching lists

        for (auto it = first ; it != last ; ++it)
        {
            bool found = false;
            for (auto& list: lists)
            {
                if (not compare(proj(list.front()), proj(*it)))
                {
                    list.emplace_front(iter_move(it));
                    found = true;
                    break;
                }
                if (not compare(proj(*it), proj(list.back())))
                {
                    list.emplace_back(iter_move(it));
                    found = true;
                    break;
                }
            }

            if (not found)
            {
                lists.emplace_back();
                lists.back().emplace_back(iter_move(it));
            }
        }

        ////////////////////////////////////////////////////////////
        // Merge encroaching lists

        while (lists.size() > 2)
        {
            if (lists.size() % 2 != 0)
            {
                auto last_it = std::prev(std::end(lists));
                auto last_1_it = std::prev(last_it);
                last_1_it->merge(*last_it, make_projection_compare(compare, projection));
                lists.pop_back();
            }

            auto first_it = std::begin(lists);
            auto half_it = first_it + lists.size() / 2;
            while (half_it != std::end(lists))
            {
                first_it->merge(*half_it, make_projection_compare(compare, projection));
                ++first_it;
                ++half_it;
            }

            lists.erase(std::begin(lists) + lists.size() / 2, std::end(lists));
        }

        // Merge lists back into the original collection

        if (lists.size() == 2)
        {
            merge_move(
                lists.front().begin(), lists.front().end(),
                lists.back().begin(), lists.back().end(),
                first, compare, projection, projection
            );
        }
        else if (lists.size() == 1)
        {
            detail::move(lists.front().begin(), lists.front().end(), first);
        }
    }
}}

#endif // CPPSORT_DETAIL_MELSORT_H_
