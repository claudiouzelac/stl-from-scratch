//
// Defines helper structs for detecting certain types. We'll be adding to
// this header as we go along.
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

#ifndef STL_FROM_SCRATCH_BITS_TYPE_HELPERS_HEADER
#define STL_FROM_SCRATCH_BITS_TYPE_HELPERS_HEADER

#include <cstddef>
#include <type_traits>
#include <memory>


namespace stl
{
    /* Forward declaration of reference_wrapper. */
    template <class>
    class reference_wrapper;

namespace bits
{
    /* Detects a reference_wrapper object. */
    template <typename>
    struct is_reference_wrapper : std::false_type {};

    template <typename T>
    struct is_reference_wrapper <reference_wrapper <T>> : std::true_type {};

    /* Destructures a reference wrapper template. */
    template <typename T>
    struct decay_reference_wrapper
    {
        using type = T;
    };

    template <typename T>
    struct decay_reference_wrapper <reference_wrapper <T>>
    {
        using type = T &;
    };

    /*
     * Variadic template testing for boolean conjuction for
     * many instances of a template predicate.
     */
    template <bool...>
    struct fold_and;

    template <>
    struct fold_and <> : std::integral_constant <bool, true> {};

    template <bool b>
    struct fold_and <b> : std::integral_constant <bool, b> {};

    template <bool b, bool ... bs>
    struct fold_and <b, bs...>
        : std::integral_constant <bool, b && fold_and <bs...>::value> {};

    template <template <class> class Predicate, class ... Args>
    struct predicate_and : fold_and <Predicate <Args>::value...> {};

    /* Helper type for selecting function overloads dependent on I. */
    template <std::size_t I>
    struct index_tag : std::integral_constant <std::size_t, I> {};

    /* Helper type for selecting uses_allocator construction for tuple nodes */
    template <bool b>
    struct uses_allocator_tag : std::integral_constant <bool, b> {};

    template <class T, class Allocator>
    using make_uses_allocator_tag = uses_allocator_tag <
        std::uses_allocator <T, Allocator>::value
    >;

    /* Selects the I'th type from a parameter pack. */
    template <std::size_t, class ...>
    struct select_type_helper;

    template <class T, class ... Ts>
    struct select_type_helper <0, T, Ts...>
    {
        using type = T;
    };

    template <std::size_t I, class T, class ... Ts>
    struct select_type_helper <I, T, Ts...> : select_type_helper <I - 1, Ts...>
    {};

    template <std::size_t I, class ... Ts>
    struct select_type
    {
        static_assert (
            I < sizeof... (Ts),
            "invalid index into template type parameter pack"
        );

        using type = typename select_type_helper <I, Ts...>::type;
    };

    template <class ... Ts>
    using head = select_type <0, Ts...>;

    /* Determines the index of a type in a parameter pack. */
    template <std::size_t, class, class...>
    struct type_index_helper;

    template <std::size_t I, class T>
    struct type_index_helper <I, T>
    {
        static_assert (sizeof (T) == 0, "type not found in parameter pack");
    };

    template <std::size_t I, class T, class ... Ts>
    struct type_index_helper <I, T, T, Ts...>
        : std::integral_constant <std::size_t, I>
    {};

    template <std::size_t I, class T, class U, class ... Ts>
    struct type_index_helper <I, T, U, Ts...>
        : type_index_helper <I + 1, T, Ts...>
    {};

    template <class T, class ... Ts>
    struct type_index : type_index_helper <0, T, Ts...> {};

    /*
     * Counts the number of times that a type appears in a parameter pack.
     */
    template <std::size_t, class, class...>
    struct type_multiplicity_helper;

    template <std::size_t M, class T, class... Ts>
    struct type_multiplicity_helper <M, T, T, Ts...>
        : std::integral_constant <
            std::size_t, type_multiplicity_helper <M + 1, T, Ts...>::value
        >
    {};

    template <std::size_t M, class T, class U, class... Ts>
    struct type_multiplicity_helper <M, T, U, Ts...>
        : std::integral_constant <
            std::size_t, type_multiplicity_helper <M, T, Ts...>::value
        >
    {};

    template <class T, class ... Ts>
    struct type_multiplicity : type_multiplicity_helper <0, T, Ts...> {};
}   // namespace bits
}   // namespace stl

#endif  // #ifndef STL_FROM_SCRATCH_BITS_TYPE_HELPERS_HEADER
