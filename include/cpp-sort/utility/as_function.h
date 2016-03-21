// Range v3 library
//
//  Copyright Eric Niebler 2013-2014
//  Modified in 2015-2016 by Morwenn for inclusion into cpp-sort
//
//  Use, modification and distribution is subject to the
//  Boost Software License, Version 1.0. (See accompanying
//  file LICENSE_1_0.txt or copy at
//  http://www.boost.org/LICENSE_1_0.txt)
//
// Project home: https://github.com/ericniebler/range-v3
//
#ifndef CPPSORT_UTILITY_AS_FUNCTION_H_
#define CPPSORT_UTILITY_AS_FUNCTION_H_

////////////////////////////////////////////////////////////
// Headers
////////////////////////////////////////////////////////////
#include <functional>
#include <type_traits>
#include <utility>
#include <cpp-sort/utility/static_const.h>

namespace cppsort
{
namespace utility
{
    namespace detail
    {
        struct as_function_fn
        {
        private:
            template<typename Ret, typename... Args>
            struct ptr_fn_
            {
            private:
                Ret (*pfn_)(Args...);

            public:
                ptr_fn_() = default;

                constexpr explicit ptr_fn_(Ret (*pfn)(Args...)):
                    pfn_(pfn)
                {}

                auto operator()(Args...args) const
                    -> Ret
                {
                    return (*pfn_)(std::forward<Args>(args)...);
                }
            };

        public:
            template<typename Ret, typename... Args>
            constexpr auto operator()(Ret (*p)(Args...)) const
                -> ptr_fn_<Ret, Args...>
            {
                return ptr_fn_<Ret, Args...>(p);
            }

            template<typename Ret, typename T>
            auto operator()(Ret T::* p) const
                -> decltype(std::mem_fn(p))
            {
                return std::mem_fn(p);
            }

            template<typename T, typename U = std::decay_t<T>>
            constexpr auto operator()(T && t) const
                -> std::enable_if_t<
                    !std::is_pointer<U>::value && !std::is_member_pointer<U>::value,
                    T
                >
            {
                return std::forward<T>(t);
            }
        };
    }

    namespace
    {
        constexpr auto&& as_function = static_const<
            detail::as_function_fn
        >::value;
    }
}}

#endif // CPPSORT_UTILITY_AS_FUNCTION_H_
