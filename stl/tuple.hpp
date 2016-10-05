//
// The tuple header. This file contains our implementation of tuple and related
// types and methods.
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

#ifndef STL_FROM_SCRATCH_TUPLE_HEADER
#define STL_FROM_SCRATCH_TUPLE_HEADER

/*
 * memory defines std::allocator_arg_t for tuple constructor selection and
 * std::uses_allocator for selective construction with an allocator of tuple
 * member objects.
 */
#include <memory>
#include <type_traits>

/* index_sequence from the sequence generator header */
#include <stl/bits/sequence_generator.hpp>

/* contains the implementation of ignore_type */
#include <stl/bits/ignore_type.hpp>

/* tuple_size, tuple_element, and uses_allocator are implemented here */
#include <stl/bits/type_traits.hpp>

/* predicate_and, and [make_]uses_allocator_tag are defined here */
#include <stl/bits/type_helpers.hpp>

/* pair, move, and forward */
#include <stl/utility.hpp>


namespace stl
{
    /* Our const ignore_type object. */
    bits::ignore_type const ignore;

namespace detail
{
    /*
     * This is a wrapper around a single type that implements construction of
     * its member object with or without uses-allocator construction,
     * appropriately detected by the tuple_impl class.
     * 
     * type_node structs appear in the multiple inheritence list for the
     * tuple_impl class.
     */
    template <std::size_t I, class T>
    struct type_node
    {
        using member_type = T;
        member_type member;

        /*
         * For an explanation of the constructors and why they are qualified the
         * way they are, look below at the constructors for tuple itself.
         */
        constexpr type_node (void)
            noexcept (
                std::is_nothrow_default_constructible <member_type>::value
            )
            : member ()
        {}

        ~type_node (void) noexcept = default;

        explicit constexpr
        type_node (member_type const & m,
                   typename std::enable_if <
                        std::is_copy_constructible <member_type>::value, void
                   >::type * = nullptr)
            : member (m)
        {}

        template <class U>
        explicit constexpr
        type_node (U && u,
                   typename std::enable_if <
                        std::is_constructible <member_type, U &&>::value, void
                   >::type * = nullptr)
            : member (stl::forward <U> (u))
        {}

        template <class U>
        constexpr type_node (type_node <I, U> const & node,
                             typename std::enable_if <
                                std::is_constructible <
                                    member_type, U const &
                                >::value, void
                             >::type * = nullptr)
            : member (node.member)
        {}

        template <class U>
        constexpr type_node (type_node <I, U> && node,
                             typename std::enable_if <
                                std::is_constructible <
                                    member_type, U &&
                                >::value, void
                             >::type * = nullptr)
            : member (stl::forward <U> (node.member))
        {}

        /* defaulted copy and move construction */
        constexpr type_node (type_node const &) = default;
        constexpr type_node (type_node &&) = default;

        /*
         * Constructors for uses-allocator construction. There are two of each
         * in order to faciliate ease of use in calling these constructors; one
         * taking a bits::uses_allocator_tag <true> arguent, and the other
         * taking a bits::uses_allocator_tag <false> argument.
         */
        template <class Allocator>
        type_node (bits::uses_allocator_tag <true>, Allocator const & alloc,
                   typename std::enable_if <
                        std::is_constructible <
                            member_type, Allocator const &
                        >::value, void
                   >::type * = nullptr)
            : member (alloc)
        {}

        template <class Allocator>
        type_node (bits::uses_allocator_tag <false>, Allocator const &)
            : member ()
        {}

        template <class Allocator>
        type_node (bits::uses_allocator_tag <true>,
                   Allocator const & alloc,
                   member_type const & m,
                   typename std::enable_if <
                        std::is_constructible <
                            member_type,
                            member_type const &,
                            Allocator const &
                        >::value, void
                   >::type * = nullptr)
            : member (m, alloc)
        {}

        template <class Allocator>
        type_node (bits::uses_allocator_tag <false>,
                   Allocator const &,
                   member_type const & m,
                   typename std::enable_if <
                        std::is_constructible <
                            member_type, member_type const &
                        >::value, void
                   >::type * = nullptr)
            : member (m)
        {}

        template <class Allocator>
        type_node (bits::uses_allocator_tag <true>,
                   Allocator const & alloc,
                   type_node const & node,
                   typename std::enable_if <
                        std::is_constructible <
                            member_type,
                            member_type const &,
                            Allocator const &
                        >::value, void
                   >::type * = nullptr)
            : member (node.member, alloc)
        {}

        template <class Allocator>
        type_node (bits::uses_allocator_tag <false>,
                   Allocator const &,
                   type_node const & node,
                   typename std::enable_if <
                        std::is_constructible <
                            member_type, member_type const &
                        >::value, void
                   >::type * = nullptr)
            : member (node.member)
        {}

        template <class Allocator>
        type_node (bits::uses_allocator_tag <true>,
                   Allocator const & alloc,
                   type_node && node,
                   typename std::enable_if <
                        std::is_constructible <
                            member_type,
                            member_type &&,
                            Allocator const &
                        >::value, void
                   >::type * = nullptr)
            : member (stl::move (node.member), alloc)
        {}

        template <class Allocator>
        type_node (bits::uses_allocator_tag <false>,
                   Allocator const &,
                   type_node && node,
                   typename std::enable_if <
                        std::is_constructible <
                            member_type, member_type &&
                        >::value, void
                   >::type * = nullptr)
            : member (stl::move (node.member))
        {}

        template <class Allocator, class U>
        type_node (bits::uses_allocator_tag <true>,
                   Allocator const & alloc,
                   U && u,
                   typename std::enable_if <
                        std::is_constructible <
                            member_type, U &&, Allocator const &
                         >::value, void
                   >::type * = nullptr)
            : member (stl::forward <U> (u), alloc)
        {}

        template <class Allocator, class U>
        type_node (bits::uses_allocator_tag <false>,
                   Allocator const &,
                   U && u,
                   typename std::enable_if <
                        std::is_constructible <
                            member_type, U &&
                        >::value, void
                   >::type * = nullptr)
            : member (stl::forward <U> (u))
        {}

        template <class Allocator, class U>
        type_node (bits::uses_allocator_tag <true>,
                   Allocator const & alloc,
                   type_node <I, U> const & node,
                   typename std::enable_if <
                        std::is_constructible <
                            member_type, U const &, Allocator const &
                        >::value, void
                   >::type * = nullptr)
            : member (node.member, alloc)
        {}

        template <class Allocator, class U>
        type_node (bits::uses_allocator_tag <false>,
                   Allocator const &,
                   type_node <I, U> const & node,
                   typename std::enable_if <
                        std::is_constructible <
                            member_type, U const &
                        >::value, void
                    >::type * = nullptr)
            : member (node.member)
        {}

        template <class Allocator, class U>
        type_node (bits::uses_allocator_tag <true>,
                   Allocator const & alloc,
                   type_node <I, U> && node,
                   typename std::enable_if <
                        std::is_constructible <
                            member_type, U &&, Allocator const &
                        >::value, void
                    >::type * = nullptr)
            : member (stl::forward <U> (node.member), alloc)
        {}

        template <class Allocator, class U>
        type_node (bits::uses_allocator_tag <false>,
                   Allocator const &,
                   type_node <I, U> && node,
                   typename std::enable_if <
                        std::is_constructible <
                            member_type, U &&
                        >::value, void
                   >::type * = nullptr)
            : member (stl::forward <U> (node.member))
        {}
    };

    template <class, class ...>
    class tuple_impl;

    /*
     * This is our implementation of the inheritence for tuples. Instead of an
     * inheritence hierarchy we'll implement tuples with a single multiple
     * inheritence list. This is not only a fun application of multiple
     * inheritence, it is also a fun application of parameter pack expansion,
     * and in more ways than one. First, we can use it to construct a multiple-
     * inheritence list for the type nodes. Second, we can use parameter pack
     * expansion to create delegating constructor lists to each type node.
     *
     * Since this takes an parameter packs std::size_t ... I and class ... Ts as
     * template arguments we are free to expand them inline for each constructor
     * with no extra work involved on our part.
     */
    template <std::size_t ... I, class ... Ts>
    class tuple_impl <stl::index_sequence <I...>, Ts...>
        : public type_node <I, Ts>...
    {
        using size = std::integral_constant <std::size_t, sizeof... (Ts)>;

        template <class ... Us>
        using rebind = tuple_impl <stl::index_sequence <I...>, Us...>;
    public:
        /*
         * The source for the constructor specifications can be found at
         * en.cppreference.com/w/cpp/tuple/tuple
         *
         * As always, we'll be using the C++14 signatures.
         */

        /*
         * The tuple of two elements is handled by enable_if's for the
         * appropriate construction and assignment by/from pairs. 
         */

        /*
         * Default constructor is requires that all Ts... are  themselves
         * default constructible.
         */
        constexpr tuple_impl (void) : type_node <I, Ts> ()... {}
        ~tuple_impl (void) noexcept = default;

        /*
         * Copy construction of each node from values inhabiting the full list
         * of template types. sizeof... (Ts) must be at least 1, and
         * is_copy_constructible must be true for each of Ts... These
         * requirements are met and handled, respectively, by the empty tuple
         * specialization above and the constructors of each tuple_node.
         *
         * We have to do a little leg work here to ensure that each level of
         * the inheritence heirarchy is constructed with the appropriate type,
         * which is why we forward to the private constructor that correctly
         * selects the types from which to construct each base.
         */
        explicit constexpr tuple_impl (Ts const &... ts)
            : type_node <I, Ts> (ts)...
        {}

        /*
         * Converting construction of each member object from the values in
         * us..., passed to constructors as stl::forward <Ui> (ui) for each i.
         *
         * This constructor requires that sizeof... (Us) == sizeof... (Ts).
         * The requirement that std::is_constructible <Ti, Ui &&>::value is true
         * for each i is checked in the constructors for each inherited node.
         */
        template <
            class ... Us,
            typename = typename std::enable_if <
                sizeof... (Us) == size::value
            >::type
        >
        explicit constexpr tuple_impl (Us && ... us)
            : type_node <I, Ts> (stl::forward <Us> (us))...
        {}

        /*
         * Converting construction of each member object from the member objects
         * in other, passed to constructors as if by stl::get <i> (other) for
         * each i.
         *
         * This constructor requires that sizeof... (Us) == sizeof... (Ts).
         * The requirement that std::is_constructible <Ti, Ui const &>::value is
         * true for each i is checked in the constructors for each inherited
         * node.
         *
         * This constructor delegates to the converting constructor taking a
         * list of references to the const member objects of other.
         */
        template <
            class ... Us,
            typename = typename std::enable_if <
                sizeof... (Us) == size::value
            >::type
        >
        constexpr tuple_impl (rebind <Us...> const & other)
            : tuple_impl (other.template type_node <I, Us>::member...)
        {}

        /*
         * Converting construction of each member object from the member objects
         * in other, passed to constructors as if by
         * stdl::forward <Ui> (stl::get <i> (other)) for each i.
         *
         * This constructor requires that sizeof... (Us) == sizeof... (Ts).
         * The requirement that std::is_constructible <Ti, Ui &&>::value is true
         * for each i is checked in the constructors for each inherited node.
         *
         * This constructor delegates to the converting constructor taking a
         * list of forwarding references to the member objects of other.
         */
        template <
            class ... Us,
            typename = typename std::enable_if <
                sizeof... (Us) == size::value
            >::type
        >
        constexpr tuple_impl (rebind <Us...> && other)
            : tuple_impl (
                stl::forward <Us> (other.template type_node <I, Us>::member)...
            )
        {}

        /*
         * Converting copy construction of each member object from the member
         * objects of the pair p, in order from first to second.
         *
         * This constructor is enabled if and only if sizeof... (Ts) == 2.
         *
         * The requirement that std::is_constructible <T0, U1>::value and
         * std::is_constructible <T1, U2>::value are both true is checked by
         * the constructors for each inherited node.
         *
         * This constructor delegates to the converting constructor taking a
         * list of references to the const member objects of p.
         */
        template <class U1, class U2>
        constexpr tuple_impl (pair <U1, U2> const & p)
            : tuple_impl (p.first, p.second)
        {}

        /*
         * Converting move construction of each member object from the member
         * objects of the pair p, in order from first to second, as if by
         * stl::forward <U1> (p.first) and stl::forward <U2> (p.second).
         *
         * This constructor is enabled if and only if sizeof... (Ts) == 2.
         *
         * The requirement that std::is_constructible <T0, U1 &&>::value and
         * std::is_constructible <T1, U2 &&>::value are both true is checked by
         * the constructors for each inherited node.
         *
         * This constructor delegates to the converting constructor taking a
         * list of forwarding references to the member objects of p.
         */
        template <class U1, class U2>
        constexpr tuple_impl (pair <U1, U2> && p)
            : tuple_impl (stl::forward <U1> (stl::move (p.first)),
                          stl::forward <U2> (stl::move (p.second)))
        {}

        /* Defaulted copy and move constructors. */
        tuple_impl (tuple_impl const &) = default;
        tuple_impl (tuple_impl &&)      = default;

        /*
         * Default construction by uses-allocator construction for each member
         * object, appropriately detected by the constructor for each inherited
         * node.
         */
        template <class Allocator>
        tuple_impl (std::allocator_arg_t, Allocator const & alloc)
            : type_node <I, Ts> (
                bits::make_uses_allocator_tag <Ts, Allocator> {}, alloc
            )...
        {}

        /*
         * Copy construction by uses-allocator construction for each member
         * object, appropriately detected by the constructor for each inherited
         * node.
         *
         * The requirement that std::is_copy_constructible <Ti>::value is true
         * for each i is checked in the constructors for each inherited node.
         */
        template <class Allocator>
        tuple_impl (std::allocator_arg_t,
                    Allocator const & alloc,
                    Ts const & ... ts)
            : type_node <I, Ts> (
                bits::make_uses_allocator_tag <Ts, Allocator> {}, alloc, ts
            )...
        {}

        /*
         * Converting construction by uses-allocator construction for each
         * member object from the values in us..., passed to constructos by
         * stl::forward <Ui> (ui) for each i, appropriately detected by the
         * constructor for each inherited node.
         *
         * This constructor requires that sizeof... (Us) == sizeof... (Ts).
         * The requirement that std::is_constructible <Ti, Ui &&>::value is true
         * for each i is checked in the constructors for each inherited node.
         */
        template <
            class Allocator, class ... Us,
            typename = typename std::enable_if <
                sizeof... (Us) == size::value
            >::type
        >
        tuple_impl (std::allocator_arg_t, Allocator const & alloc, Us && ... us)
            : type_node <I, Ts> (
                bits::make_uses_allocator_tag <Ts, Allocator> {},
                alloc,
                stl::forward <Us> (us)
            )...
        {}

        /*
         * Copy construction by uses-allocator construction for each member
         * object, passed to constructors as if by stl::get <i> (other) for each
         * i, appropriately detected by the constructor for each inherited node.
         *
         * The requirement that std::is_copy_constructible <Ti>::value is true
         * for each i is checked in the constructors for each inherited node.
         *
         * This constructor delegates to the uses-allocator constructor taking
         * a list of references to the const member objects of other.
         */
        template <class Allocator>
        tuple_impl (std::allocator_arg_t,
                    Allocator const & alloc,
                    tuple_impl const & other)
            : tuple_impl (
                std::allocator_arg_t {},
                alloc,
                other.template type_node <I, Ts>::member...
            )
        {}

        /*
         * Move construction by uses-allocator construction for each member
         * object, passed to constructors as if by
         * stl::move (stl::get <i> (other)) for each i, appropriately detected
         * by the constructor for each inherited node.
         *
         * The requirement that std::is_move_constructible <Ti>::value is true
         * for each i is checked in the constructors for each inherited node.
         *
         * This constructor delegates to the uses-allocator constructor taking
         * a list of forwarding references to the member objects of other.
         */
        template <class Allocator>
        tuple_impl (std::allocator_arg_t,
                    Allocator const & alloc,
                    tuple_impl && other)
            : tuple_impl (
                std::allocator_arg_t {},
                alloc,
                stl::forward <Ts> (other.template type_node <I, Ts>::member)...
            )
        {}

        /*
         * Converting construction by uses-allocator construction of each member
         * object from the member objects in other, passed to constructors as if
         * by stl::get <i> (other) for each i.
         *
         * This constructor requires that sizeof... (Us) == sizeof... (Ts).
         * The requirement that std::is_constructible <Ti, Ui const &>::value is
         * true for each i is checked in the constructors for each inherited
         * node.
         *
         * This constructor delegates to the converting uses-allocator
         * constructor taking a list of references to the const member objects
         * of other.
         */
        template <
            class Allocator, class ... Us,
            typename = typename std::enable_if <
                sizeof... (Us) == size::value
            >::type
        >
        tuple_impl (std::allocator_arg_t,
                    Allocator const & alloc,
                    rebind <Us...> const & other)
            : tuple_impl (
                std::allocator_arg_t {},
                alloc,
                other.template type_node <I, Us>::member...
            )
        {}

        /*
         * Converting construction by uses-allocator construction of each member
         * object from the member objects in other, passed to constructors as if
         * by stdl::forward <Ui> (stl::get <i> (other)) for each i.
         *
         * This constructor requires that sizeof... (Us) == sizeof... (Ts).
         * The requirement that std::is_constructible <Ti, Ui &&>::value is true
         * for each i is checked in the constructors for each inherited node.
         *
         * This constructor delegates to the converting uses-allocator
         * constructor taking a list of forwarding references to the member
         * objects of other.
         */
        template <
            class Allocator, class ... Us,
            typename = typename std::enable_if <
                sizeof... (Us) == size::value
            >::type
        >
        tuple_impl (std::allocator_arg_t,
                    Allocator const & alloc,
                    rebind <Us...> && other)
            : tuple_impl (
                std::allocator_arg_t {},
                alloc,
                stl::forward <Us> (other.template type_node <I, Us>::member)...
            )
        {}

        /*
         * Converting copy construction of each member object from the member
         * objects of the pair p, in order from first to second, by uses-
         * allocator construction, appropriately detected by the constructor
         * for each inherited node.
         *
         * This constructor is enabled if and only if sizeof... (Ts) == 2.
         *
         * The requirement that std::is_constructible <T0, U1>::value and
         * std::is_constructible <T1, U2>::value are both true is checked by
         * the constructors for each inherited node.
         *
         * This constructor delegates to the converting uses-allocator
         * constructor taking a list of references to the const member objects
         * of p.
         */
        template <class Allocator, class U1, class U2>
        constexpr tuple_impl (std::allocator_arg_t,
                              Allocator const & alloc,
                              pair <U1, U2> const & p)
            : tuple_impl (std::allocator_arg_t {}, alloc, p.first, p.second)
        {}

        /*
         * Converting move construction of each member object from the member
         * objects of the pair p, in order from first to second, as if by
         * stl::forward <U1> (p.first) and stl::forward <U2> (p.second).
         *
         * This constructor is enabled if and only if sizeof... (Ts) == 2.
         *
         * The requirement that std::is_constructible <T0, U1 &&>::value and
         * std::is_constructible <T1, U2 &&>::value are both true is checked by
         * the constructors for each inherited node.
         *
         * This constructor delegates to the converting uses-allocator
         * constructor taking a list of forwarding references to the member
         * objects of p.
         */
        template <class Allocator, class U1, class U2>
        constexpr tuple_impl (std::allocator_arg_t,
                              Allocator const & alloc,
                              pair <U1, U2> && p)
            : tuple_impl (
                std::allocator_arg_t {},
                alloc,
                stl::forward <U1> (stl::move (p.first)),
                stl::forward <U2> (stl::move (p.second))
            )
        {}

        /*
         * Copy assignment operator of each member object by the corresponding
         * member object of other.
         */
        tuple_impl & operator= (tuple_impl const & other)
        {
            bits::ignore_type _expression_eater [] = {{
                ((type_node <I, Ts>::member =
                    other.template type_node <I, Ts>::member),
                 I)...
            }};

            (void) _expression_eater;
            return *this;
        }

        /*
         * Move assignment operator of each member object by the corresponding
         * member object of other, using move semantics.
         */
        tuple_impl & operator= (tuple_impl && other)
            noexcept (
                bits::predicate_and <
                    std::is_nothrow_move_assignable, Ts...
                >::value
            )
        {
            bits::ignore_type _expression_eater [] = {{
                ((type_node <I, Ts>::member =
                    stl::move (other.template type_node <I, Ts>::member)),
                 I)...
            }};

            (void) _expression_eater;
            return *this;
        }

        /*
         * Converting copy assignment operator of each member object by the
         * corresponding member object of other.
         *
         * This assignment operator requires that
         * sizeof... (Us) == sizeof... (Ts).
         */
        template <
            class ... Us,
            typename = typename std::enable_if <
                sizeof... (Us) == size::value
            >::type
        >
        tuple_impl & operator= (rebind <Us...> const & other)
        {
            bits::ignore_type _expression_eater [] = {{
                ((type_node <I, Ts>::member =
                    other.template type_node <I, Us>::member),
                 I)...
            }};

            (void) _expression_eater;
            return *this;
        }

        /*
         * Converting move assignment operator of each member object by the
         * corresponding member object of other, using move semantics.
         *
         * This assignment operator requires that
         * sizeof... (Us) == sizeof... (Ts).
         */
        template <
            class ... Us,
            typename = typename std::enable_if <
                sizeof... (Us) == size::value
            >::type
        >
        tuple_impl & operator= (rebind <Us...> && other)
        {
            bits::ignore_type _expression_eater [] = {{
                ((type_node <I, Ts>::member = stl::forward <Us> (
                    other.template type_node <I, Us>::member
                )), I)...
            }};

            (void) _expression_eater;
            return *this;
        }

        /*
         * Converting copy assignment operator of each member object by the
         * corresponding member objects of the pair p, from first to second.
         *
         * This assignment operator requires that sizeof... (Ts) == 2.
         */
        template <class U1, class U2>
        tuple_impl & operator= (pair <U1, U2> const & p)
        {
            static_assert (
                size::value == 2 && sizeof (U1), "cannot assign pair to tuple"
            );

            bits::ignore_type _expression_eater [] = {{
                ((type_node <I, Ts>::member = stl::get <I> (p)), I)...
            }};

            (void) _expression_eater;
            return *this;
        }

        /*
         * Converting move assignment operator of each member object by the
         * corresponding member objects of the pair p, from first to second,
         * using move semantics.
         *
         * This assignment operator requires that sizeof... (Ts) == 2.
         */
        template <class U1, class U2>
        tuple_impl & operator= (pair <U1, U2> && p) noexcept
        {
            static_assert (
                size::value == 2 && sizeof (U1), "cannot assign pair to tuple"
            );

            bits::ignore_type _expression_eater [] = {{
                ((type_node <I, Ts>::member = stl::get <I> (stl::move (p))),
                 I)...
            }};

            (void) _expression_eater;
            return *this;
        }

        /*
         * Swaps each member object with the corresponding member object of
         * other.
         */
        void swap (tuple_impl & other)
            noexcept (
                bits::fold_and <
                    noexcept (
                        swap (stl::declval <Ts &> (), stl::declval <Ts &> ())
                    )...
                >::value
            )
        {
            using stl::swap;

            bits::ignore_type _expression_eater [] = {{
                ((swap (type_node <I, Ts>::member,
                        other.template type_node <I, Ts>::member)), I)...
            }};
            (void) _expression_eater;
        }

        /* 
         * Here are our implementations of operator== and operator<. As per the
         * spec we must short-circuit evaluation, which means we have to do a
         * bit of leg-work with helper templates.
         */
        template <std::size_t, class ...>
        struct compare;

        template <class ... Us>
        struct compare <size::value - 1, Us...>
        {
            static constexpr bool
                compare_eq (tuple_impl const lhs, rebind <Us...> const & rhs)
                noexcept
            {
                using t_type = typename bits::select_type <
                    size::value - 1, Ts...
                >::type;
                using u_type = typename bits::select_type <
                    size::value - 1, Us...
                >::type;

                return lhs.template type_node <size::value - 1, t_type>::member
                    == rhs.template type_node <size::value - 1, u_type>::member;
            }

            static constexpr bool
                compare_lt (tuple_impl const lhs, rebind <Us...> const & rhs)
                noexcept
            {
                using t_type = typename bits::select_type <
                    size::value - 1, Ts...
                >::type;
                using u_type = typename bits::select_type <
                    size::value - 1, Us...
                >::type;

                return lhs.template type_node <size::value - 1, t_type>::member
                     < rhs.template type_node <size::value - 1, u_type>::member;
            }
        };

        template <std::size_t Idx, class ... Us>
        struct compare
        {
            static constexpr bool
                compare_eq (tuple_impl const lhs, rebind <Us...> const & rhs)
                noexcept
            {
                using t_type = typename bits::select_type <Idx, Ts...>::type;
                using u_type = typename bits::select_type <Idx, Us...>::type;

                return (lhs.template type_node <Idx, t_type>::member
                        == rhs.template type_node <Idx, u_type>::member)
                     ? compare <Idx + 1, Us...>::compare_eq (lhs, rhs)
                     : false;
            }

            static constexpr bool
                compare_lt (tuple_impl const lhs, rebind <Us...> const & rhs)
                noexcept
            {
                using t_type = typename bits::select_type <Idx, Ts...>::type;
                using u_type = typename bits::select_type <Idx, Us...>::type;

                return (lhs.template type_node <Idx, t_type>::member
                        < rhs.template type_node <Idx, u_type>::member)
                     ? true
                     : (rhs.template type_node <Idx, t_type>::member
                        < lhs.template type_node <Idx, u_type>::member)
                     ? false
                     : compare <Idx + 1, Us...>::compare_lt (lhs, rhs);
            }
        };

        template <
            class ... Us,
            typename = typename std::enable_if <
                sizeof... (Us) == size::value
            >::type
        >
        constexpr bool operator== (rebind <Us...> const & rhs) const noexcept
        {
            return compare <0, Us...>::compare_eq (*this, rhs);
        }

        template <
            class ... Us,
            typename = typename std::enable_if <
                sizeof... (Us) == size::value
            >::type
        >
        constexpr bool operator< (rebind <Us...> const & rhs) const noexcept
        {
            return compare <0, Us...>::compare_lt (*this, rhs);
        }
    };
}

    /*
     * We will be writing two cases for tuple:
     *      (i)  the empty tuple;
     *      (ii) the tuple of arbitrary elements.
     *
     * This is the forward declaration of tuple that we'll specialize further
     * down.
     */
    template <class ...>
    class tuple;

    /*
     * Forward declare helper structs for get and comparison implementations.
     */
namespace detail
{
    template <class>
    struct get_impl;

    struct compare_impl;
};

    /*
     * Empty tuple. This differs from other tuples in a few ways, most notably
     * in the list of constructors and the fact that it does not inherit from
     * any tuple_node base. Each constructor and assignment operator also
     * happens to be trivial.
     */
    template <>
    class tuple <>
    {
    public:
        constexpr tuple (void) {}
        ~tuple (void) noexcept = default;

        constexpr tuple (tuple const &) = default;
        constexpr tuple (tuple &&)      = default;

        /*
         * For completeness we'll also have to define the allocator versions of
         * tuple constructors, although each of these is trivial.
         */
        template <class Allocator>
        tuple (std::allocator_arg_t, Allocator const &) {}

        template <class Allocator>
        constexpr tuple (std::allocator_arg_t, Allocator const &, tuple const &)
        {}

        template <class Allocator>
        constexpr tuple (std::allocator_arg_t, Allocator const &, tuple &&)
        {}

        tuple & operator= (tuple const &)
        {
            return *this;
        }

        tuple & operator= (tuple &&) noexcept
        {
            return *this;
        }

        void swap (tuple &) noexcept {}
    };

    /*
     * General implementation of tuple. We make a single inheritence from the
     * tuple_impl class, which itself has a multiple inheritence from the type
     * nodes for each type in Ts... This ensures that the object layout of tuple
     * is correct (that is, in the same order as that of Ts...).
     *
     * Since tuple_impl does all the magic for us, all we need do here is
     * forward arguments to the correct delegate constructors.
     */
    template <class ... Ts>
    class tuple : private detail::tuple_impl <
                    stl::make_index_sequence <sizeof... (Ts)>, Ts...
                  >
    {
        using base = detail::tuple_impl <
            stl::make_index_sequence <sizeof... (Ts)>, Ts...
        >;

        /*
         * These are helper methods for object-slicing references to our tuple
         * into references to the tuple_impl.
         */
        static base & base_slice (tuple & t) noexcept
        {
            return static_cast <base &> (t);
        }

        static base const & base_slice (tuple const & t) noexcept
        {
            return static_cast <base const &> (t);
        }

        static base && base_slice (tuple && t) noexcept
        {
            return static_cast <base &&> (stl::move (t));
        }

        friend struct detail::get_impl <tuple>;
        friend struct detail::compare_impl;

    public:
        /*
         * The source for the constructor specifications can be found at
         * en.cppreference.com/w/cpp/tuple/tuple
         *
         * As always, we'll be using the C++14 signatures.
         *
         * The implementation notes for each constructor can be found above in
         * the implementation of the tuple_impl class.
         *
         * The tuple of two elements is handled by enable_if's for the
         * appropriate construction and assignment by/from pairs in the
         * tuple_impl class.
         */

        constexpr tuple (void) : base ()
        {}

        explicit constexpr tuple (Ts const &... ts) : base (ts...)
        {}

        template <class ... Us>
        explicit constexpr tuple (Us && ... us)
            : base (stl::forward <Us> (us)...)
        {}

        template <class ... Us>
        constexpr tuple (tuple <Us...> const & other)
            : base (tuple <Us...>::base_slice (other))
        {}

        template <class ... Us>
        constexpr tuple (tuple <Us...> && other)
            : base (tuple <Us...>::base_slice (stl::move (other)))
        {}

        template <class U1, class U2>
        constexpr tuple (pair <U1, U2> const & p) : base (p)
        {}

        template <class U1, class U2>
        constexpr tuple (pair <U1, U2> && p) : base (stl::move (p))
        {}

        tuple (tuple const &) = default;
        tuple (tuple &&)      = default;

        template <class Allocator>
        tuple (std::allocator_arg_t, Allocator const & alloc)
            : base (std::allocator_arg_t {}, alloc)
        {}

        template <class Allocator>
        tuple (std::allocator_arg_t, Allocator const & alloc, Ts const & ... ts)
            : base (std::allocator_arg_t {}, alloc, ts...)
        {}

        template <class Allocator, class ... Us>
        tuple (std::allocator_arg_t, Allocator const & alloc, Us && ... us)
            : base (std::allocator_arg_t {}, alloc, stl::forward <Us> (us)...)
        {}

        template <class Allocator>
        tuple (std::allocator_arg_t,
               Allocator const & alloc,
               tuple const & other)
            : base (std::allocator_arg_t {}, alloc, base_slice (other))
        {}

        template <class Allocator>
        tuple (std::allocator_arg_t,
               Allocator const & alloc,
               tuple && other)
            : base (std::allocator_arg_t {},
                    alloc,
                    base_slice (stl::move (other)))
        {}

        template <class Allocator, class ... Us>
        tuple (std::allocator_arg_t,
               Allocator const & alloc,
               tuple <Us...> const & other)
            : base (std::allocator_arg_t {},
                    alloc,
                    tuple <Us...>::base_slice (other))
        {}

        template <class Allocator, class ... Us>
        tuple (std::allocator_arg_t,
               Allocator const & alloc,
               tuple <Us...> && other)
            : base (std::allocator_arg_t {},
                    alloc,
                    tuple <Us...>::base_slice (stl::move (other)))
        {}

        template <class Allocator, class U1, class U2>
        constexpr tuple (std::allocator_arg_t,
                         Allocator const & alloc,
                         pair <U1, U2> const & p)
            : base (std::allocator_arg_t {}, alloc, p)
        {}

        template <class Allocator, class U1, class U2>
        constexpr tuple (std::allocator_arg_t,
                         Allocator const & alloc,
                         pair <U1, U2> && p)
            : base (std::allocator_arg_t {}, alloc, stl::move (p))
        {}

        tuple & operator= (tuple const & other)
        {
            base_slice (*this) = base_slice (other);
            return *this;
        }

        tuple & operator= (tuple && other)
            noexcept (noexcept (
                stl::declval <base &> () = stl::declval <base &&> ()
            ))
        {
            base_slice (*this) = base_slice (stl::move (other));
            return *this;
        }

        template <class ... Us>
        tuple & operator= (tuple <Us...> const & other)
        {
            base_slice (*this) = tuple <Us...>::base_slice (other);
            return *this;
        }

        template <class ... Us>
        tuple & operator= (tuple <Us...> && other)
        {
            base_slice (*this) = tuple <Us...>::base_slice (stl::move (other));
            return *this;
        }

        template <class U1, class U2>
        tuple & operator= (pair <U1, U2> const & p)
        {
            base_slice (*this) = p;
            return *this;
        }

        template <class U1, class U2>
        tuple & operator= (pair <U1, U2> && p) noexcept
        {
            base_slice (*this) = stl::move (p);
            return *this;
        }

        void swap (tuple & other)
            noexcept (noexcept (
                stl::declval <base &> ().swap (stl::declval <base &&> ())
            ))
        {
            base_slice (*this).swap (base_slice (other));
        }
    };

    /*
     * Creates a tuple object from the passed parameters. If any of the deduced
     * types are reference wrappers the reference wrapper types are further
     * decayed to the underlying reference types.
     *
     * We will be using the C++14 signature.
     */
    template <class ... Ts>
    constexpr
    tuple <
        typename bits::decay_reference_wrapper <
            typename std::decay <Ts>::type
        >::type...
    > make_tuple (Ts && ... ts)
    {
        using tuple_type = tuple <
            typename bits::decay_reference_wrapper <
                typename std::decay <Ts>::type
            >::type...
        >;
        return tuple_type (stl::forward <Ts> (ts)...);
    }

    /*
     * Creates a tuple of lvalue references to its arguments (or references to
     * the stl::ignore object).
     *
     * We will be using the C++14 signature.
     */
    template <class ... Ts>
    constexpr tuple <Ts &...> tie (Ts & ... ts) noexcept
    {
        return tuple <Ts &...> (ts...);
    }

    /*
     * Creates a tuple of references to its arguments suitable for forwarding
     * to a function call. rvalue references are preserved for the appropriate
     * arguments, and lvalue references are taken to all other arguments.
     *
     * We will be using the C++14 signature.
     */
    template <class ... Ts>
    constexpr tuple <Ts &&...> forward_as_tuple (Ts && ... ts) noexcept
    {
        return tuple <Ts &&...> (stl::forward <Ts> (ts)...);
    }

namespace detail
{
    template <class ... Tup>
    struct concat_type;

    template <>
    struct concat_type <>
    {
        using type = tuple <>;
    };

    template <class Tup>
    struct concat_type <Tup>
    {
        using type = typename std::decay <Tup>::type;
    };

    template <class ... As, class ... Bs, class ... Tups>
    struct concat_type <tuple <As...>, tuple <Bs...>, Tups...>
    {
        using type = typename concat_type <
            tuple <As..., Bs...>, Tups...
        >::type;
    };

    template <std::size_t N>
    struct tuple_cat_helper;

    template <>
    struct tuple_cat_helper <0>
    {
        static constexpr tuple <> dispatch (void) noexcept
        {
            return tuple <> ();
        }
    };

    template <>
    struct tuple_cat_helper <1>
    {
        template <class Tuple>
        static constexpr typename std::decay <Tuple>::type
            dispatch (Tuple && t) noexcept
        {
            return std::decay <Tuple>::type (stl::forward <Tuple> (t));
        }
    };

    template <>
    struct tuple_cat_helper <2>
    {
        template <
            std::size_t ... I1, std::size_t ... I2, class Tuple1, class Tuple2
        >
        static constexpr typename concat_type <Tuple1, Tuple2>::type
            concat (stl::index_sequence <I1...>,
                    stl::index_sequence <I2...>,
                    Tuple1 && t1,
                    Tuple2 && t2) noexcept
        {
            using result = typename concat_type <Tuple1, Tuple2>::type;
            return result (
                stl::get <I1> (stl::forward <Tuple1> (t1))...,
                stl::get <I2> (stl::forward <Tuple2> (t2))...
            );
        }

        template <class Tuple1, class Tuple2>
        static constexpr typename concat_type <Tuple1, Tuple2>::type
            dispatch (Tuple1 && t1, Tuple2 && t2) noexcept
        {
            using size1 = stl::tuple_size <Tuple1>;
            using size2 = stl::tuple_size <Tuple2>;
            return concat (
                stl::make_index_sequence <size1::value> {},
                stl::make_index_sequence <size2::value> {},
                stl::forward <Tuple1> (t1),
                stl::forward <Tuple2> (t2)
            );
        }
    };

    /*
     * Because of the above specializations we know that N >= 3, which ensures
     * correctness of the concat implementation.
     */
    template <std::size_t N>
    struct tuple_cat_helper
    {
        template <
            std::size_t ... I1, std::size_t ... I2,
            class Tuple1, class Tuple2, class ... Tuples
        >
        static constexpr typename concat_type <Tuple1, Tuple2, Tuples...>::type
            concat (stl::index_sequence <I1...>,
                    stl::index_sequence <I2...>,
                    Tuple1 && t1, Tuple2 && t2,
                    Tuples && ... ts)
        {
            using partial = typename concat_type <Tuple1, Tuple2>::type;
            return tuple_cat_helper <2>::dispatch (
                partial (get <I1> (stl::forward <Tuple1> (t1))...,
                         get <I2> (stl::forward <Tuple2> (t2))...),
                tuple_cat_helper <N - 2>::dispatch (
                    stl::forward <Tuples> (ts)...
                )
            );
        }

        template <class Tuple1, class Tuple2, class ... Tuples>
        static constexpr typename concat_type <Tuple1, Tuple2, Tuples...>::type
            dispatch (Tuple1 && t1, Tuple2 && t2, Tuples && ... ts) noexcept
        {
            using size1 = stl::tuple_size <Tuple1>;
            using size2 = stl::tuple_size <Tuple2>;
            return concat (
                stl::make_index_sequence <size1::value> {},
                stl::make_index_sequence <size2::value> {},
                stl::forward <Tuple1> (t1),
                stl::forward <Tuple2> (t2),
                stl::forward <Tuples> (ts)...
            );
        }
    };
}
    /*
     * Concatenates one or more tuples with member objects ordered by the
     * appearance in which their original tuples appeared as parameters.
     *
     * We use the C++14 signature.
     */
    template <class ... Tuples>
    constexpr typename detail::concat_type <Tuples...>::type
        tuple_cat (Tuples && ... tuples)
    {
        return detail::tuple_cat_helper <sizeof... (Tuples)>::dispatch (
            stl::forward <Tuples> (tuples)...
        );
    }

    /*
     * Index based get <>. This does not compile if the index is greater than
     * one less than the number of types.
     *
     * We will be using the C++14 signatures.
     */
namespace detail
{
    template <class ... Ts>
    struct get_impl <tuple <Ts...>>
    {
        template <std::size_t I>
        static constexpr typename stl::tuple_element <I, tuple <Ts...>>::type &
            do_get (tuple <Ts...> & t) noexcept
        {
            using tup  = tuple <Ts...>;
            using type = typename stl::tuple_element <I, tup>::type;
            using node = typename tup::base::template type_node <I, type>;

            return static_cast <node &> (t).member;
        }

        template <std::size_t I>
        static constexpr
        typename stl::tuple_element <I, tuple <Ts...>>::type const &
            do_get (tuple <Ts...> const & t) noexcept
        {
            using tup  = tuple <Ts...>;
            using type = typename stl::tuple_element <I, tup>::type;
            using node = typename tup::base::template type_node <I, type>;

            return static_cast <node const &> (t).member;
        }

        template <std::size_t I>
        static constexpr typename stl::tuple_element <I, tuple <Ts...>>::type &&
            do_get (tuple <Ts...> && t) noexcept
        {
            using tup  = tuple <Ts...>;
            using type = typename stl::tuple_element <I, tup>::type;
            using node = typename tup::base::template type_node <I, type>;

            return stl::forward <type &&> (static_cast <node &> (t).member);
        }
    };
}   // namespace detail

    template <std::size_t I, class ... Ts>
    constexpr typename stl::tuple_element <I, tuple <Ts...>>::type &
        get (tuple <Ts...> & t) noexcept
    {
        return detail::get_impl <tuple <Ts...>>::template do_get <I> (t);
    }

    template <std::size_t I, class ... Ts>
    constexpr typename stl::tuple_element <I, tuple <Ts...>>::type const &
        get (tuple <Ts...> const & t) noexcept
    {
        return detail::get_impl <tuple <Ts...>>::template do_get <I> (t);
    }

    template <std::size_t I, class ... Ts>
    constexpr typename stl::tuple_element <I, tuple <Ts...>>::type &&
        get (tuple <Ts...> && t) noexcept
    {
        return detail::get_impl <tuple <Ts...>>::template do_get <I> (
            stl::move (t)
        );
    }

    /*
     * Type based get <>. This does not compile if the tuple has more than
     * one element of that type or if this is the empty tuple.
     *
     * We will be using the C++14 signatures.
     */
    template <class S, class ... Ts>
    constexpr S & get (tuple <Ts...> & t) noexcept
    {
        static_assert (sizeof... (Ts) != 0, "cannot extract from empty tuple");

        using index        = bits::type_index <S, Ts...>;
        using multiplicity = bits::type_multiplicity <S, Ts...>;
        static_assert (
            multiplicity::value == 1, "cannot extract repeated element type"
        );

        return get <index::value> (t);
    }

    template <class S, class ... Ts>
    constexpr S const & get (tuple <Ts...> const & t) noexcept
    {
        static_assert (sizeof... (Ts) != 0, "cannot extract from empty tuple");

        using index        = bits::type_index <S, Ts...>;
        using multiplicity = bits::type_multiplicity <S, Ts...>;
        static_assert (
            multiplicity::value == 1, "cannot extract repeated element type"
        );

        return get <index::value> (t);
    }

    template <class S, class ... Ts>
    constexpr S && get (tuple <Ts...> && t) noexcept
    {
        static_assert (sizeof... (Ts) != 0, "cannot extract from empty tuple");

        using index        = bits::type_index <S, Ts...>;
        using multiplicity = bits::type_multiplicity <S, Ts...>;
        static_assert (
            multiplicity::value == 1, "cannot extract repeated element type"
        );

        return get <index::value> (stl::move (t));
    }

    /*
     * This is our free-function swap specialization for tuples. It will simply
     * forward to the member swap function for tuple.
     */
    template <class ... Types>
    void swap (tuple <Types...> & lhs, tuple <Types...> & rhs)
        noexcept (noexcept (lhs.swap (rhs)))
    {
        lhs.swap (rhs);
    }

    /*
     * Here we must write lexicographic comparisons for tuples (as per the spec,
     * all comparisons must be short-circuited). The two comparisons that we
     * actually implement by hand are operator== and operator<.
     *
     * We'll be using the C++14 signature.
     */
namespace detail
{
    struct compare_impl
    {
        static constexpr bool do_eq (tuple <> const &,
                                     tuple <> const &) noexcept
        {
            return true;
        }

        static constexpr bool do_lt (tuple <> const &,
                                     tuple <> const &) noexcept
        {
            return false;
        }

        template <class ... Ts, class ... Us>
        static constexpr bool do_eq (tuple <Ts...> const & lhs,
                                     tuple <Us...> const & rhs) noexcept
        {
            using lhs_tuple = tuple <Ts...>;
            using rhs_tuple = tuple <Ts...>;

            return lhs_tuple::base_slice (lhs) == rhs_tuple::base_slice (rhs);
        }

        template <class ... Ts, class ... Us>
        static constexpr bool do_lt (tuple <Ts...> const & lhs,
                                     tuple <Us...> const & rhs) noexcept
        {
            using lhs_tuple = tuple <Ts...>;
            using rhs_tuple = tuple <Ts...>;

            return lhs_tuple::base_slice (lhs) < rhs_tuple::base_slice (rhs);
        }
    };
}   // namespace detail

    template <class ... Ts, class ... Us>
    constexpr bool operator== (tuple <Ts...> const & lhs,
                               tuple <Us...> const & rhs)
    {
        return detail::compare_impl::do_eq (lhs, rhs);
    }

    template <class ... Ts, class ... Us>
    constexpr bool operator!= (tuple <Ts...> const & lhs,
                               tuple <Us...> const & rhs)
    {
        return !(lhs == rhs);
    }

    template <class ... Ts, class ... Us>
    constexpr bool operator< (tuple <Ts...> const & lhs,
                              tuple <Us...> const & rhs)
    {
        return detail::compare_impl::do_lt (lhs, rhs);
    }

    template <class ... Ts, class ... Us>
    constexpr bool operator<= (tuple <Ts...> const & lhs,
                               tuple <Us...> const & rhs)
    {
        return !(rhs < lhs);
    }

    template <class ... Ts, class ... Us>
    constexpr bool operator> (tuple <Ts...> const & lhs,
                              tuple <Us...> const & rhs)
    {
        return rhs < lhs;
    }

    template <class ... Ts, class ... Us>
    constexpr bool operator>= (tuple <Ts...> const & lhs,
                               tuple <Us...> const & rhs)
    {
        return !(lhs < rhs);
    }

    /*
     * Lasly, now that tuples have been implemented we can define the
     * piecewise constructor for pair.
     */
    template <class T1, class T2>
        template <
            std::size_t ... I1, std::size_t ... I2,
            class ... Args1, class ... Args2
        >
    pair <T1, T2>::pair (stl::index_sequence <I1...>,
                         stl::index_sequence <I2...>,
                         tuple <Args1...> first_args,
                         tuple <Args2...> second_args)
        : first  (stl::get <I1> (first_args)...)
        , second (stl::get <I2> (second_args)...)
    {}

    template <class T1, class T2>
        template <class ... Args1, class ... Args2>
    pair <T1, T2>::pair (stl::piecewise_construct_t,
                        tuple <Args1...> first_args,
                        tuple <Args2...> second_args)
        : pair (
            stl::make_index_sequence <sizeof... (Args1)> {},
            stl::make_index_sequence <sizeof... (Args2)> {},
            first_args,
            second_args
        )
    {}
}   // namespace stl

#endif  // #ifndef STL_FROM_SCRATCH_TUPLE_HEADER
