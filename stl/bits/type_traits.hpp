//
// Defines type trait structs for types in the STL. We'll be adding to this
// header as we go along.
//
// author: Dalton M. Woodard
// contact: daltonmwoodard@gmail.com
// repository: https://github.com/daltonwoodard/stl-from-scratch.git
// license:
//
// MIT License (MIT)
//
// Copyright (c) 2016 Dalton M. Woodard
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in
// all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
// SOFTWARE.
//

#ifndef STL_FROM_SCRATCH_BITS_TYPE_TRAITS_HPP
#define STL_FROM_SCRATCH_BITS_TYPE_TRAITS_HPP

#include <cstddef>
#include <type_traits>
#include <stl/bits/type_helpers.hpp>


namespace stl
{
    /* Forward declaration of pair. */
    template <class T1, class T2>
    class pair;

    /* Forward declaration of tuple. */
    template <class ... Ts>
    class tuple;

namespace bits
{
    template <std::size_t I, class Tup>
    struct tuple_element_helper;

    template <std::size_t I, class ... Ts>
    struct tuple_element_helper <I, tuple <Ts...>>
    {
        static_assert (
            I < sizeof... (Ts), "invalid index into tuple type list"
        );

    private:
        template <std::size_t Idx, class ... Us>
        struct select_type;

        template <class U, class ... Us>
        struct select_type <0, U, Us...>
        {
            using type = U;
        };

        template <std::size_t Idx, class U, class ... Us>
        struct select_type <Idx, U, Us...> : select_type <Idx - 1, Us...> {};

    public:
        using type = typename select_type <I, Ts...>::type;
    };

    template <std::size_t I, class T1, class T2>
    struct tuple_element_helper <I, pair <T1, T2>>
    {
        static_assert (I == 0 || I == 1, "invalid index into pair type list");
        using type = typename std::conditional <I == 0, T1, T2>::type;
    };
}   // namespace bits

    /* Forward declaration of uses_allocator. */
    template <class T, class Allocator>
    struct uses_allocator;

    /*
     * Implementation of uses_allocator for tuples. As per the spec it always
     * inherits from std::true_type.
     */
    template <class ... Ts, class Allocator>
    struct uses_allocator <tuple <Ts...>, Allocator> : std::true_type {};

    /* Implementation of tuple_element and tuple_size for tuples and pairs. */
    template <std::size_t, class>
    class tuple_element;

    template <std::size_t I, class ... Ts>
    class tuple_element <I, tuple <Ts...>>
    {
    public:
        using type = typename bits::tuple_element_helper <
            I, tuple <Ts...>
        >::type;
    };

    template <std::size_t I, class T1, class T2>
    class tuple_element <I, pair <T1, T2>>
    {
    public:
        using type = typename bits::tuple_element_helper <
            I, pair <T1, T2>
        >::type;
    };

    template <std::size_t I, class ... Ts>
    class tuple_element <I, tuple <Ts...> const>
    {
    public:
        using type = typename std::add_const <
            typename bits::tuple_element_helper <
                I, tuple <Ts...>
            >::type
        >::type;
    };

    template <std::size_t I, class T1, class T2>
    class tuple_element <I, pair <T1, T2> const>
    {
    public:
        using type = typename std::add_const <
            typename bits::tuple_element_helper <
                I, pair <T1, T2>
            >::type
        >::type;
    };

    template <std::size_t I, class ... Ts>
    class tuple_element <I, tuple <Ts...> volatile>
    {
    public:
        using type = typename std::add_volatile <
            typename bits::tuple_element_helper <
                I, tuple <Ts...>
            >::type
        >::type;
    };

    template <std::size_t I, class T1, class T2>
    class tuple_element <I, pair <T1, T2> volatile>
    {
    public:
        using type = typename std::add_volatile <
            typename bits::tuple_element_helper <
                I, pair <T1, T2>
            >::type
        >::type;
    };

    template <std::size_t I, class ... Ts>
    class tuple_element <I, tuple <Ts...> const volatile>
    {
    public:
        using type = typename std::add_cv <
            typename bits::tuple_element_helper <
                I, tuple <Ts...>
            >::type
        >::type;
    };

    template <std::size_t I, class T1, class T2>
    class tuple_element <I, pair <T1, T2> const volatile>
    {
    public:
        using type = typename std::add_cv <
            typename bits::tuple_element_helper <
                I, pair <T1, T2>
            >::type
        >::type;
    };

    template <class Tup>
    class tuple_size;

    template <class ... Ts>
    class tuple_size <tuple <Ts...>>
        : public std::integral_constant <std::size_t, sizeof... (Ts)> {};

    template <class T1, class T2>
    class tuple_size <pair <T1, T2>>
        : public std::integral_constant <std::size_t, 2> {};

    template <class Tup>
    class tuple_size <Tup const>
        : public std::integral_constant <std::size_t, tuple_size <Tup>::value>
    {};

    template <class Tup>
    class tuple_size <Tup volatile>
        : public std::integral_constant <std::size_t, tuple_size <Tup>::value>
    {};

    template <class Tup>
    class tuple_size <Tup const volatile>
        : public std::integral_constant <std::size_t, tuple_size <Tup>::value>
    {};
}   // namespace stl

#endif  // #ifndef STL_FROM_SCRATCH_BITS_TYPE_TRAITS_HPP
