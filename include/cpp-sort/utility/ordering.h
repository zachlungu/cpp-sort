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
#ifndef CPPSORT_UTILITY_ORDERING_H_
#define CPPSORT_UTILITY_ORDERING_H_

////////////////////////////////////////////////////////////
// Headers
////////////////////////////////////////////////////////////
#include <cmath>
#include <type_traits>
#include <utility>
#include "../detail/static_const.h"

namespace cppsort
{
namespace utility
{
    ////////////////////////////////////////////////////////////
    // Standard orderings

    enum struct partial_ordering: int
    {
        less        = -1,
        unordered   = 0,
        greater     = 1
    };

    enum struct weak_ordering: int
    {
        less        = -1,
        equivalent  = 0,
        greater     = 1
    };

    enum struct total_ordering: int
    {
        less    = -1,
        equal   = 0,
        greater = 1
    };

    namespace detail
    {
        ////////////////////////////////////////////////////////////
        // Ordering for integral types

        template<typename T>
        constexpr auto total_order(T lhs, T rhs)
            -> std::enable_if_t<
                std::is_integral<T>::value,
                total_ordering
            >
        {
            if (lhs < rhs)
                return total_ordering::less;
            if (rhs < lhs)
                return total_ordering::greater;
            return total_ordering::equal;
        }

        ////////////////////////////////////////////////////////////
        // Ordering for floating point types

        template<typename FloatingPoint>
        auto weak_weight(FloatingPoint value)
            -> int
        {
            // Only matters for NaN and infinity
            int res = std::signbit(value) ? -1 : 1;
            switch (std::fpclassify(value))
            {
                case FP_NAN:
                    return res * 2;
                case FP_INFINITE:
                    return res;
                default:
                    return 0;
            }
        }

        template<typename FloatingPoint>
        auto total_weight(FloatingPoint value)
            -> int
        {
            // Only matters for NaN and infinity
            int res = std::signbit(value) ? -1 : 1;
            switch (std::fpclassify(value))
            {
                case FP_NAN:
                    // TODO: discriminate quiet and signaling NaNs
                    return res * 2;
                case FP_INFINITE:
                    return res;
                default:
                    return 0;
            }
        }

        template<typename T>
        constexpr auto partial_order(T lhs, T rhs)
            -> std::enable_if_t<
                std::is_floating_point<T>::value,
                partial_ordering
            >
        {
            if (lhs < rhs)
                return partial_ordering::less;
            if (rhs < lhs)
                return partial_ordering::greater;
            return partial_ordering::unordered;
        }

        template<typename T>
        auto weak_order(T lhs, T rhs)
            -> std::enable_if_t<
                std::is_floating_point<T>::value,
                weak_ordering
            >
        {
            if (std::isfinite(lhs) && std::isfinite(rhs))
            {
                if (lhs < rhs)
                    return weak_ordering::less;
                if (rhs < lhs)
                    return weak_ordering::greater;
                return weak_ordering::equivalent;
            }

            int lhs_weight = weak_weight(lhs);
            int rhs_weight = weak_weight(rhs);

            if (lhs_weight < rhs_weight)
                return weak_ordering::less;
            if (rhs_weight < lhs_weight)
                return weak_ordering::greater;
            return weak_ordering::equivalent;
        }

        template<typename T>
        auto total_order(T lhs, T rhs)
            -> std::enable_if_t<
                std::is_floating_point<T>::value,
                total_ordering
            >
        {
            if (std::isfinite(lhs) && std::isfinite(rhs))
            {
                if (lhs == 0 && rhs == 0)
                {
                    lhs = std::signbit(lhs) ? -1 : 1;
                    rhs = std::signbit(rhs) ? -1 : 1;
                }

                if (lhs < rhs)
                    return total_ordering::less;
                if (rhs < lhs)
                    return total_ordering::greater;
                return total_ordering::equal;
            }

            int lhs_weight = total_weight(lhs);
            int rhs_weight = total_weight(rhs);

            if (lhs_weight < rhs_weight)
                return total_ordering::less;
            if (rhs_weight < lhs_weight)
                return total_ordering::greater;
            return total_ordering::equal;
        }

        ////////////////////////////////////////////////////////////
        // Generic ordering functions

        template<typename T>
        constexpr auto weak_order(const T& lhs, const T& rhs)
            -> std::enable_if_t<
                not std::is_floating_point<T>::value,
                weak_ordering
            >
        {
            total_ordering order = total_order(lhs, rhs);
            switch (order)
            {
                case total_ordering::less:
                    return weak_ordering::less;
                case total_ordering::greater:
                    return weak_ordering::greater;
                default:
                    return weak_ordering::equivalent;
            }
        }

        template<typename T>
        constexpr auto partial_order(const T& lhs, const T& rhs)
            -> std::enable_if_t<
                not std::is_floating_point<T>::value,
                partial_ordering
            >
        {
            weak_ordering order = weak_order(lhs, rhs);
            switch (order)
            {
                case weak_ordering::less:
                    return partial_ordering::less;
                case weak_ordering::greater:
                    return partial_ordering::greater;
                default:
                    return partial_ordering::unordered;
            }
        }

        ////////////////////////////////////////////////////////////
        // Customization points basis

        struct partial_order_fn
        {
            template<typename T, typename U>
            constexpr auto operator()(T&& lhs, U&& rhs) const
                -> partial_ordering
            {
                return partial_order(std::forward<T>(lhs), std::forward<U>(rhs));
            }
        };

        struct weak_order_fn
        {
            template<typename T, typename U>
            constexpr auto operator()(T&& lhs, U&& rhs) const
                -> weak_ordering
            {
                return weak_order(std::forward<T>(lhs), std::forward<U>(rhs));
            }
        };

        struct total_order_fn
        {
            template<typename T, typename U>
            constexpr auto operator()(T&& lhs, U&& rhs) const
                -> total_ordering
            {
                return total_order(std::forward<T>(lhs), std::forward<U>(rhs));
            }
        };
    }

    ////////////////////////////////////////////////////////////
    // Global function objects

    namespace
    {
        constexpr auto&& partial_order = cppsort::detail::static_const<
            detail::partial_order_fn
        >::value;

        constexpr auto&& weak_order = cppsort::detail::static_const<
            detail::weak_order_fn
        >::value;

        constexpr auto&& total_order = cppsort::detail::static_const<
            detail::total_order_fn
        >::value;
    }
}}

#endif // CPPSORT_UTILITY_ORDERING_H_
