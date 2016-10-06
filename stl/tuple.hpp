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

        ~type_node (void) noexcept (std::is_nothrow_destructible <T>::value)
            = default;

        explicit constexpr
        type_node (member_type const & m)
            : member (m)
        {}

        template <class U>
        explicit constexpr
        type_node (U && u)
            : member (stl::forward <U> (u))
        {}

        template <class U>
        constexpr type_node (type_node <I, U> const & node)
            : member (node.member)
        {}

        template <class U>
        constexpr type_node (type_node <I, U> && node)
            : member (stl::forward <U> (node.member))
        {}

        /* defaulted copy and move construction */
        constexpr type_node (type_node const &) = default;
        constexpr type_node (type_node &&) = default;

        /*
         * Constructors for uses-allocator construction. There are two of each
         * in order to faciliate ease of use in calling these constructors; one
         * taking a bits::uses_allocator_tag <true> arguent, and the other
         * taking a bits::uses_allocator_tag <false> argument, indicating,
         * respectively, that they member object is to be constructed with or
         * without the given allocator.
         *
         * Since the tuple class checks for constructibility of the member
         * objects with or without the allocator we need not perform any
         * checks here.
         */
        template <class Allocator>
        type_node (bits::uses_allocator_tag <true>, Allocator const & alloc)
            : member (alloc)
        {}

        template <class Allocator>
        type_node (bits::uses_allocator_tag <false>, Allocator const &)
            : member ()
        {}

        template <class Allocator>
        type_node (bits::uses_allocator_tag <true>,
                   Allocator const & alloc,
                   member_type const & m)
            : member (m, alloc)
        {}

        template <class Allocator>
        type_node (bits::uses_allocator_tag <false>,
                   Allocator const &,
                   member_type const & m)
            : member (m)
        {}

        template <class Allocator>
        type_node (bits::uses_allocator_tag <true>,
                   Allocator const & alloc,
                   type_node const & node)
            : member (node.member, alloc)
        {}

        template <class Allocator>
        type_node (bits::uses_allocator_tag <false>,
                   Allocator const &,
                   type_node const & node)
            : member (node.member)
        {}

        template <class Allocator>
        type_node (bits::uses_allocator_tag <true>,
                   Allocator const & alloc,
                   type_node && node)
            : member (stl::move (node.member), alloc)
        {}

        template <class Allocator>
        type_node (bits::uses_allocator_tag <false>,
                   Allocator const &,
                   type_node && node)
            : member (stl::move (node.member))
        {}

        template <class Allocator, class U>
        type_node (bits::uses_allocator_tag <true>,
                   Allocator const & alloc,
                   U && u)
            : member (stl::forward <U> (u), alloc)
        {}

        template <class Allocator, class U>
        type_node (bits::uses_allocator_tag <false>,
                   Allocator const &,
                   U && u)
            : member (stl::forward <U> (u))
        {}

        template <class Allocator, class U>
        type_node (bits::uses_allocator_tag <true>,
                   Allocator const & alloc,
                   type_node <I, U> const & node)
            : member (node.member, alloc)
        {}

        template <class Allocator, class U>
        type_node (bits::uses_allocator_tag <false>,
                   Allocator const &,
                   type_node <I, U> const & node)
            : member (node.member)
        {}

        template <class Allocator, class U>
        type_node (bits::uses_allocator_tag <true>,
                   Allocator const & alloc,
                   type_node <I, U> && node)
            : member (stl::forward <U> (node.member), alloc)
        {}

        template <class Allocator, class U>
        type_node (bits::uses_allocator_tag <false>,
                   Allocator const &,
                   type_node <I, U> && node)
            : member (stl::forward <U> (node.member))
        {}
    };

    template <class, class ...>
    class tuple_impl;

    /*
     * This is our implementation of the inheritence for tuples. Instead of an
     * inheritence hierarchy we'll implement tuples with a single multiple
     * inheritence list. This is not only a fun application of multiple
     * inheritence, it is also a fun application of parameter pack expansion
     * (and in more ways than one). First, we can use it to construct a multiple
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
        ~tuple_impl (void)
            noexcept (
                bits::predicate_and <std::is_nothrow_destructible, Ts...>::value
            )
            = default;

        /*
         * Copy construction of each node from values inhabiting the full list
         * of template types.
         */
        explicit constexpr tuple_impl (Ts const &... ts)
            : type_node <I, Ts> (ts)...
        {}

        /*
         * Converting construction of each member object from the values in
         * us..., passed to constructors as stl::forward <Ui> (ui) for each i.
         */
        template <class ... Us>
        explicit constexpr tuple_impl (Us && ... us)
            : type_node <I, Ts> (stl::forward <Us> (us))...
        {}

        /*
         * Converting construction of each member object from the member objects
         * in other, passed to constructors as if by stl::get <i> (other) for
         * each i.
         *
         * This constructor delegates to the converting constructor taking a
         * list of references to the const member objects of other.
         */
        template <class ... Us>
        constexpr tuple_impl (rebind <Us...> const & other)
            : tuple_impl (other.template type_node <I, Us>::member...)
        {}

        /*
         * Converting construction of each member object from the member objects
         * in other, passed to constructors as if by
         * stdl::forward <Ui> (stl::get <i> (other)) for each i.
         *
         * This constructor delegates to the converting constructor taking a
         * list of forwarding references to the member objects of other.
         */
        template <class ... Us>
        constexpr tuple_impl (rebind <Us...> && other)
            : tuple_impl (
                stl::forward <Us> (other.template type_node <I, Us>::member)...
            )
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
         */
        template <class Allocator, class ... Us>
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
         * This constructor delegates to the converting uses-allocator
         * constructor taking a list of references to the const member objects
         * of other.
         */
        template <class Allocator, class ... Us>
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
         * This constructor delegates to the converting uses-allocator
         * constructor taking a list of forwarding references to the member
         * objects of other.
         */
        template <class Allocator, class ... Us>
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
         * Copy assignment operator of each member object by the corresponding
         * member object of other.
         */
        tuple_impl & operator= (tuple_impl const & other)
        {
            bits::ignore_type _expression_eater [] = {
                ((type_node <I, Ts>::member =
                    other.template type_node <I, Ts>::member),
                 I)...
            };

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
            bits::ignore_type _expression_eater [] = {
                ((type_node <I, Ts>::member =
                    stl::move (other.template type_node <I, Ts>::member)),
                 I)...
            };

            (void) _expression_eater;
            return *this;
        }

        /*
         * Converting copy assignment operator of each member object by the
         * corresponding member object of other.
         */
        template <class ... Us>
        tuple_impl & operator= (rebind <Us...> const & other)
        {
            bits::ignore_type _expression_eater [] = {
                ((type_node <I, Ts>::member =
                    other.template type_node <I, Us>::member),
                 I)...
            };

            (void) _expression_eater;
            return *this;
        }

        /*
         * Converting move assignment operator of each member object by the
         * corresponding member object of other, using move semantics.
         */
        template <class ... Us>
        tuple_impl & operator= (rebind <Us...> && other)
        {
            bits::ignore_type _expression_eater [] = {
                ((type_node <I, Ts>::member = stl::forward <Us> (
                    other.template type_node <I, Us>::member
                )), I)...
            };

            (void) _expression_eater;
            return *this;
        }

        /*
         * Converting copy assignment operator of each member object by the
         * corresponding member objects of the pair p, from first to second.
         */
        template <class U1, class U2>
        tuple_impl & operator= (pair <U1, U2> const & p)
        {
            bits::ignore_type _expression_eater [] = {
                ((type_node <I, Ts>::member = stl::get <I> (p)), I)...
            };

            (void) _expression_eater;
            return *this;
        }

        /*
         * Converting move assignment operator of each member object by the
         * corresponding member objects of the pair p, from first to second,
         * using move semantics.
         */
        template <class U1, class U2>
        tuple_impl & operator= (pair <U1, U2> && p) noexcept
        {
            bits::ignore_type _expression_eater [] = {
                ((type_node <I, Ts>::member = stl::get <I> (stl::move (p))),
                 I)...
            };

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

            bits::ignore_type _expression_eater [] = {
                ((swap (type_node <I, Ts>::member,
                        other.template type_node <I, Ts>::member)), I)...
            };
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

        template <class ... Us>
        constexpr bool operator== (rebind <Us...> const & rhs) const noexcept
        {
            return compare <0, Us...>::compare_eq (*this, rhs);
        }

        template <class ... Us>
        constexpr bool operator< (rebind <Us...> const & rhs) const noexcept
        {
            return compare <0, Us...>::compare_lt (*this, rhs);
        }
    };
}

    /*
     * Forward declare helper structs for get and comparison implementations.
     */
namespace detail
{
    template <class>
    struct base_access;

    template <class>
    struct get_impl;

    struct compare_impl;
};

    /*
     * We will be writing two cases for tuple:
     *      (i)   the empty tuple;
     *      (ii)  the tuple of two elements
     *      (iii) the tuple of arbitrary elements.
     *
     * This is the forward declaration of tuple that we'll specialize further
     * down.
     */
    template <class ...>
    class tuple;

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
     * Tuple of two elements. This is similar to the general case except in that
     * constructors and assignment operators taking pairs are included.
     */
    template <class T1, class T2>
    class tuple <T1, T2>
        : private detail::tuple_impl <stl::index_sequence <0, 1>, T1, T2>
    {
        using base = detail::tuple_impl <stl::index_sequence <0, 1>, T1, T2>;

        friend struct detail::base_access <tuple>;
        friend struct detail::get_impl <tuple>;
        friend struct detail::compare_impl;

    public:
        /*
         * The source for the constructor specifications can be found at
         * en.cppreference.com/w/cpp/tuple/tuple
         *
         * As always, we'll be u1, u2 the C++14 signatures.
         *
         * The implementation notes for each constructor can be found above in
         * the implementation of the tuple_impl class. The appropriate enable_if
         * checks will be performed up-front in the definitions of each
         * constructor below, allowing u1, u2 keep the actual implementations in
         * tuple_impl reletively clean.
         *
         * The tuple of two element1, t2 handled by enable_if's for the
         * appropriate construction and assignment by/from pairs.
         */

        /*
         * Since the first two constructors are not templated, these are the
         * only construct1, t2ere the requirement1, t2e not checked up-front.
         * Both the requirement1, t2at t1, t2 are default constructible (for the
         * default constructor) and that t1, t2 are copy constructible (for the
         * second constructor) are checked for each type, respectively, in
         * the type_node <T> structs.
         */

        /*
         * Default constructor. Requires that all t1, t2 are  themselves default
         * constructible.
         */
        constexpr tuple (void) : base ()
        {}

        ~tuple (void) noexcept (std::is_nothrow_destructible <base>::value)
            = default;

        /*
         * Copy construction of each node from values inhabiting the full list
         * of template types.
         */
        explicit constexpr tuple (T1 const & t1, T2 const & t2) : base (t1, t2)
        {}

        /*
         * Converting construction of each member object from the values in
         * u1, u2, passed to constructors as stl::forward <Ui> (ui) for each i.
         *
         * The requirement that std::is_constructible <Ti, Ui &&>::value is true
         * for each i must also be met.
         */
        template <
            class U1, class U2,
            typename = typename std::enable_if <
                std::is_constructible <T1, U1 &&>::value &&
                std::is_constructible <T1, U2 &&>::value
            >::type
        >
        explicit constexpr tuple (U1 && u1, U2 && u2)
            : base (stl::forward <U1> (u1), stl::forward <U2> (u2))
        {}

        /*
         * Converting construction of each member object from the member objects
         * in other, passed to constructors as if by stl::get <i> (other) for
         * each i.
         *
         * The requirement that std::is_constructible <Ti, Ui const &>::value is
         * true for each i must also be met.
         */
        template <
            class U1, class U2,
            typename = typename std::enable_if <
                std::is_constructible <T1, U1 const &>::value &&
                std::is_constructible <T2, U2 const &>::value
            >::type
        >
        constexpr tuple (tuple <U1, U2> const & other)
            : base (detail::base_access <tuple <U1, U2>>::slice (other))
        {}

        /*
         * Converting construction of each member object from the member objects
         * in other, passed to constructors as if by
         * stdl::forward <Ui> (stl::get <i> (other)) for each i.
         *
         * The requirement that std::is_constructible <Ti, Ui &&>::value is true
         * for each i must also be met.
         */
        template <
            class U1, class U2,
            typename = typename std::enable_if <
                std::is_constructible <T1, U1 &&>::value &&
                std::is_constructible <T2, U2 &&>::value
            >::type
        >
        constexpr tuple (tuple <U1, U2> && other)
            : base (
                detail::base_access <tuple <U1, U2>>::slice (stl::move (other))
            )
        {}

        /*
         * Converting copy construction of each member object from the member
         * object1, t2 the pair p, in order from first to second.
         *
         * This constructor is enabled if and only if sizeof... (t1, t2= 2.
         *
         * The requirement that std::is_constructible <T0, U1>::value and
         * std::is_constructible <T1, U2>::value are both true must also be met.
         */
        template <
            class U1, class U2,
            typename = typename std::enable_if <
                std::is_constructible <T1, U1 const &>::value &&
                std::is_constructible <T2, U2 const &>::value
            >::type
        >
        constexpr tuple (pair <U1, U2> const & p)
            : base (p.first, p.second)
        {}

        /*
         * Converting move construction of each member object from the member
         * object1, t2 the pair p, in order from first to second, as if by
         * stl::forward <U1> (p.first) and stl::forward <U2> (p.second).
         *
         * This constructor is enabled if and only if sizeof... (t1, t2= 2.
         *
         * The requirement that std::is_constructible <T0, U1 &&>::value and
         * std::is_constructible <T1, U2 &&>::value are both true must also be
         * met.
         */
        template <
            class U1, class U2,
            typename = typename std::enable_if <
                std::is_constructible <T1, U1 &&>::value &&
                std::is_constructible <T2, U2 &&>::value
            >::type
        >
        constexpr tuple (pair <U1, U2> && p)
            : base (stl::forward <U1> (p.first), stl::forward <U2> (p.second))
        {}

        /* Explicitly defaulted copy and move constructors. */
        tuple (tuple const &) = default;
        tuple (tuple &&)      = default;

        /*
         * Default construction by uses-allocator construction for each member
         * object, appropriately detected by the constructor for each inherited
         * node.
         */
        template <
            class Allocator,
            typename = typename std::enable_if <
                (std::is_default_constructible <T1>::value ||
                 std::is_constructible <T1, Allocator const &>::value) &&
                (std::is_default_constructible <T2>::value ||
                 std::is_constructible <T2, Allocator const &>::value)
            >::type
        >
        tuple (std::allocator_arg_t, Allocator const & alloc)
            : base (std::allocator_arg_t {}, alloc)
        {}

        /*
         * Copy construction by uses-allocator construction for each member
         * object, appropriately detected by the constructor for each inherited
         * node.
         *
         * The requirement that std::is_copy_constructible <Ti>::value is true
         * for each i must also be met.
         */
        template <
            class Allocator,
            typename = typename std::enable_if <
                (std::is_copy_constructible <T1>::value ||
                 std::is_constructible <
                    T1, T1 const &, Allocator const &
                 >::value) &&
                (std::is_copy_constructible <T2>::value ||
                 std::is_constructible <
                    T2, T2 const &, Allocator const &
                 >::value)
            >::type
        >
        tuple (std::allocator_arg_t,
               Allocator const & alloc,
               T1 const & t1,
               T2 const & t2)
            : base (std::allocator_arg_t {}, alloc, t1, t2)
        {}

        /*
         * Converting construction by uses-allocator construction for each
         * member object from the values in u1, u2, passed to constructos by
         * stl::forward <Ui> (ui) for each i, appropriately detected by the
         * constructor for each inherited node.
         *
         * The requirement that std::is_constructible <Ti, Ui &&>::value is true
         * for each i must also be met.
         */
        template <
            class Allocator, class U1, class U2,
            typename = typename std::enable_if <
                (std::is_constructible <T1, U1 &&>::value ||
                 std::is_constructible <T1, U1 &&, Allocator const &>::value) &&
                (std::is_constructible <T2, U2 &&>::value ||
                 std::is_constructible <T2, U2 &&, Allocator const &>::value)
            >::type
        >
        tuple (std::allocator_arg_t,
               Allocator const & alloc,
               U1 && u1,
               U2 && u2)
            : base (std::allocator_arg_t {}, alloc, u1, u2)
        {}

        /*
         * Copy construction by uses-allocator construction for each member
         * object, passed to constructors as if by stl::get <i> (other) for each
         * i, appropriately detected by the constructor for each inherited node.
         *
         * The requirement that std::is_copy_constructible <Ti>::value is true
         * for each i must also be met.
         */
        template <
            class Allocator,
            typename = typename std::enable_if <
                (std::is_copy_constructible <T1>::value ||
                 std::is_constructible <
                    T1, T1 const &, Allocator const &
                 >::value) &&
                (std::is_copy_constructible <T2>::value ||
                 std::is_constructible <
                    T2, T2 const &, Allocator const &
                 >::value)
            >::type
        >
        tuple (std::allocator_arg_t,
               Allocator const & alloc,
               tuple const & other)
            : base (std::allocator_arg_t {}, alloc,
                    detail::base_access <tuple>::slice (other))
        {}

        /*
         * Move construction by uses-allocator construction for each member
         * object, passed to constructors as if by
         * stl::move (stl::get <i> (other)) for each i, appropriately detected
         * by the constructor for each inherited node.
         *
         * The requirement that std::is_move_constructible <Ti>::value is true
         * for each i must also be met.
         */
        template <
            class Allocator, class U1, class U2,
            typename = typename std::enable_if <
                (std::is_move_constructible <T1>::value ||
                 std::is_constructible <T1, T1 &&, Allocator const &>::value) &&
                (std::is_move_constructible <T2>::value ||
                 std::is_constructible <T2, T2 &&, Allocator const &>::value)
            >::type
        >
        tuple (std::allocator_arg_t,
               Allocator const & alloc,
               tuple && other)
            : base (std::allocator_arg_t {}, alloc,
                    detail::base_access <tuple>::slice (stl::move (other)))
        {}

        /*
         * Converting construction by uses-allocator construction of each member
         * object from the member object1, t2 other, passed to constructors as
         * if by stl::get <i> (other) for each i.
         *
         * The requirement that std::is_constructible <Ti, Ui const &>::value is
         * true for each i must also be met.
         */
        template <
            class Allocator, class U1, class U2,
            typename = typename std::enable_if <
                (std::is_constructible <T1, U1 const &>::value ||
                 std::is_constructible <
                    T1, U1 const &, Allocator const &
                 >::value) &&
                (std::is_constructible <T2, U2 const &>::value ||
                 std::is_constructible <
                    T2, U2 const &, Allocator const &
                 >::value)
            >::type
        >
        tuple (std::allocator_arg_t,
               Allocator const & alloc,
               tuple <U1, U2> const & other)
            : base (std::allocator_arg_t {}, alloc,
                    detail::base_access <tuple <U1, U2>>::slice (other))
        {}

        /*
         * Converting construction by uses-allocator construction of each member
         * object from the member objects in other, passed to constructors as if
         * by stdl::forward <Ui> (stl::get <i> (other)) for each i.
         *
         * The requirement that std::is_constructible <Ti, Ui &&>::value is true
         * for each i must also be met.
         */
        template <
            class Allocator, class U1, class U2,
            typename = typename std::enable_if <
                (std::is_constructible <T1, U1 &&>::value ||
                 std::is_constructible <T1, U1 &&, Allocator const &>::value) &&
                (std::is_constructible <T2, U2 &&>::value ||
                 std::is_constructible <T2, U2 &&, Allocator const &>::value)
            >::type
        >
        tuple (std::allocator_arg_t,
               Allocator const & alloc,
               tuple <U1, U2> && other)
            : base (
                std::allocator_arg_t {}, alloc,
                detail::base_access <tuple <U1, U2>>::slice (stl::move (other))
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
         * std::is_constructible <T1, U2>::value are both true must also be met.
         */
        template <
            class Allocator, class U1, class U2,
            typename = typename std::enable_if <
                (std::is_constructible <T1, U1 const &>::value ||
                 std::is_constructible <
                    T1, U1 const &, Allocator const &
                 >::value) &&
                (std::is_constructible <T2, U2 const &>::value ||
                 std::is_constructible <
                    T2, U2 const &, Allocator const &
                 >::value)
            >::type
        >
        constexpr tuple (std::allocator_arg_t,
                         Allocator const & alloc,
                         pair <U1, U2> const & p)
            : base (std::allocator_arg_t {}, alloc, p.first, p.second)
        {}

        /*
         * Converting move construction of each member object from the member
         * objects of the pair p, in order from first to second, as if by
         * stl::forward <U1> (p.first) and stl::forward <U2> (p.second).
         *
         * This constructor is enabled if and only if sizeof... (Ts) == 2.
         *
         * The requirement that std::is_constructible <T0, U1 &&>::value and
         * std::is_constructible <T1, U2 &&>::value are both true must also be
         * met.
         */
        template <
            class Allocator, class U1, class U2,
            typename = typename std::enable_if <
                (std::is_constructible <T1, U1 &&>::value ||
                 std::is_constructible <T1, U1 &&, Allocator const &>::value) &&
                (std::is_constructible <T2, U2 &&>::value ||
                 std::is_constructible <T2, U2 &&, Allocator const &>::value)
            >::type
        >
        constexpr tuple (std::allocator_arg_t,
                         Allocator const & alloc,
                         pair <U1, U2> && p)
            : base (
                std::allocator_arg_t {},
                alloc,
                stl::forward <U1> (p.first),
                stl::forward <U2> (p.second)
            )
        {}

        /*
         * Copy assignment operator of each member object by the corresponding
         * member object of other.
         */
        tuple & operator= (tuple const & other)
        {
            detail::base_access <tuple> (*this) =
                detail::base_access <tuple>::slice (other);
            return *this;
        }

        /*
         * Moveassignment operator of each member object by the corresponding
         * member object of other, using move semantics.
         */
        tuple & operator= (tuple && other)
            noexcept (noexcept (
                stl::declval <base &> () = stl::declval <base &&> ()
            ))
        {
            detail::base_access <tuple> (*this) =
                detail::base_access <tuple> (stl::move (other));
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
            class U1, class U2,
            typename = typename std::enable_if <
                std::is_assignable <T1, U1 const &>::value &&
                std::is_assignable <T2, U2 const &>::value
            >::type
        >
        tuple & operator= (tuple <U1, U2> const & other)
        {
            detail::base_access <tuple>::slice (*this) =
                detail::base_access <tuple <U1, U2>>::slice (other);
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
            class U1, class U2,
            typename = typename std::enable_if <
                std::is_assignable <T1, U1 &&>::value &&
                std::is_assignable <T2, U2 &&>::value
            >::type
        >
        tuple & operator= (tuple <U1, U2> && other)
        {
            detail::base_access <tuple>::slice (*this) =
                detail::base_access <tuple <U1, U2>>::slice (stl::move (other));
            return *this;
        }

        /*
         * Converting copy assignment operator of each member object by the
         * corresponding member objects of the pair p, from first to second.
         *
         * This assignment operator requires that sizeof... (Ts) == 2.
         */
        template <
            class U1, class U2,
            typename = typename std::enable_if <
                std::is_assignable <T1, U1 const &>::value &&
                std::is_assignable <T2, U2 const &>::value
            >::type
        >
        tuple & operator= (pair <U1, U2> const & p)
        {
            detail::base_access <tuple>::slice (*this) = p;
            return *this;
        }

        /*
         * Converting move assignment operator of each member object by the
         * corresponding member objects of the pair p, from first to second,
         * using move semantics.
         *
         * This assignment operator requires that sizeof... (Ts) == 2.
         */
        template <
            class U1, class U2,
            typename = typename std::enable_if <
                std::is_assignable <T1, U1 &&>::value &&
                std::is_assignable <T2, U2 &&>::value
            >::type
        >
        tuple & operator= (pair <U1, U2> && p) noexcept
        {
            detail::base_access <tuple>::slice = stl::move (p);
            return *this;
        }

        /*
         * Swaps each member object with the corresponding member object of
         * other.
         */
        void swap (tuple & other)
            noexcept (noexcept (
                stl::declval <base &> ().swap (stl::declval <base &> ())
            ))
        {
            detail::base_access <tuple>::slice (*this).swap (
                detail::base_access <tuple>::slice (other)
            );
        }
    };

    /*
     * General implementation of tuple. We make a single inheritence from the
     * tuple_impl class, which itself has a multiple inheritence from the type
     * nodes for each type in Ts... This ensures that the object layout of tuple
     * is correct (that is, in the same order as that of Ts...).
     *
     * Since tuple_impl does all the magic for us, all we need do here is check
     * that constructor requirements are met and delegate to the correct
     * constructors from tuple_impl.
     */
    template <class ... Ts>
    class tuple : private detail::tuple_impl <
                    stl::make_index_sequence <sizeof... (Ts)>, Ts...
                  >
    {
        using base = detail::tuple_impl <
            stl::make_index_sequence <sizeof... (Ts)>, Ts...
        >;
        using size = std::integral_constant <std::size_t, sizeof... (Ts)>;

        friend struct detail::base_access <tuple>;
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
         * the implementation of the tuple_impl class. The appropriate enable_if
         * checks will be performed up-front in the definitions of each
         * constructor below, allowing us to keep the actual implementations in
         * tuple_impl reletively clean.
         */

        /*
         * Since the first two constructors are not templated, these are the
         * only constructs where the requirements are not checked up-front.
         * Both the requirements that Ts... are default constructible (for the
         * default constructor) and that Ts... are copy constructible (for the
         * second constructor) are checked for each type, respectively, in
         * the type_node <T> structs.
         */

        /*
         * Default constructor. Requires that all Ts... are  themselves default
         * constructible.
         */
        constexpr tuple (void) : base ()
        {}

        /*
         * Copy construction of each node from values inhabiting the full list
         * of template types.
         *
         * Requires that sizeof... (Ts) be at least 1, and each type in Ts...
         * must be copy constructible.
         */
        explicit constexpr tuple (Ts const &... ts) : base (ts...)
        {}

        /*
         * Converting construction of each member object from the values in
         * us..., passed to constructors as stl::forward <Ui> (ui) for each i.
         *
         * This constructor requires that sizeof... (Us) == sizeof... (Ts).
         * The requirement that std::is_constructible <Ti, Ui &&>::value is true
         * for each i must also be met.
         */
        template <
            class ... Us,
            typename = typename std::enable_if <
                sizeof... (Us) == size::value &&
                bits::fold_and <
                    std::is_constructible <Ts, Us &&>::value...
                >::value
            >::type
        >
        explicit constexpr tuple (Us && ... us)
            : base (stl::forward <Us> (us)...)
        {}

        /*
         * Converting construction of each member object from the member objects
         * in other, passed to constructors as if by stl::get <i> (other) for
         * each i.
         *
         * This constructor requires that sizeof... (Us) == sizeof... (Ts).
         * The requirement that std::is_constructible <Ti, Ui const &>::value is
         * true for each i must also be met.
         */
        template <
            class ... Us,
            typename = typename std::enable_if <
                sizeof... (Us) == size::value &&
                bits::fold_and <
                    std::is_constructible <Ts, Us const &>::value...
                >::value
            >::type
        >
        constexpr tuple (tuple <Us...> const & other)
            : base (detail::base_access <tuple <Us...>>::slice (other))
        {}

        /*
         * Converting construction of each member object from the member objects
         * in other, passed to constructors as if by
         * stdl::forward <Ui> (stl::get <i> (other)) for each i.
         *
         * This constructor requires that sizeof... (Us) == sizeof... (Ts).
         * The requirement that std::is_constructible <Ti, Ui &&>::value is true
         * for each i must also be met.
         */
        template <
            class ... Us,
            typename = typename std::enable_if <
                sizeof... (Us) == size::value &&
                bits::fold_and <
                    std::is_constructible <Ts, Us &&>::value...
                >::value
            >::type
        >
        constexpr tuple (tuple <Us...> && other)
            : base (
                detail::base_access <tuple <Us...>>::slice (stl::move (other))
            )
        {}

        /* Explicitly defaulted copy and move constructors. */
        tuple (tuple const &) = default;
        tuple (tuple &&)      = default;

        /*
         * Default construction by uses-allocator construction for each member
         * object, appropriately detected by the constructor for each inherited
         * node.
         */
        template <
            class Allocator,
            typename = typename std::enable_if <
                bits::fold_and <
                    (std::is_default_constructible <Ts>::value ||
                     std::is_constructible <Ts, Allocator const &>::value)...
                >::value
            >::type
        >
        tuple (std::allocator_arg_t, Allocator const & alloc)
            : base (std::allocator_arg_t {}, alloc)
        {}

        /*
         * Copy construction by uses-allocator construction for each member
         * object, appropriately detected by the constructor for each inherited
         * node.
         *
         * The requirement that std::is_copy_constructible <Ti>::value is true
         * for each i must also be met.
         */
        template <
            class Allocator,
            typename = typename std::enable_if <
                bits::fold_and <
                    (std::is_copy_constructible <Ts>::value ||
                     std::is_constructible <
                        Ts, Ts const &, Allocator const &
                     >::value)...
                >::value
            >::type
        >
        tuple (std::allocator_arg_t, Allocator const & alloc, Ts const & ... ts)
            : base (std::allocator_arg_t {}, alloc, ts...)
        {}

        /*
         * Converting construction by uses-allocator construction for each
         * member object from the values in us..., passed to constructos by
         * stl::forward <Ui> (ui) for each i, appropriately detected by the
         * constructor for each inherited node.
         *
         * This constructor requires that sizeof... (Us) == sizeof... (Ts).
         * The requirement that std::is_constructible <Ti, Ui &&>::value is true
         * for each i must also be met.
         */
        template <
            class Allocator, class ... Us,
            typename = typename std::enable_if <
                sizeof... (Us) == size::value &&
                bits::fold_and <
                    (std::is_constructible <Ts, Us &&>::value ||
                     std::is_constructible <
                        Ts, Us &&, Allocator const &
                     >::value)...
                >::value
            >::type
        >
        tuple (std::allocator_arg_t, Allocator const & alloc, Us && ... us)
            : base (std::allocator_arg_t {}, alloc, stl::forward <Us> (us)...)
        {}

        /*
         * Copy construction by uses-allocator construction for each member
         * object, passed to constructors as if by stl::get <i> (other) for each
         * i, appropriately detected by the constructor for each inherited node.
         *
         * The requirement that std::is_copy_constructible <Ti>::value is true
         * for each i must also be met.
         */
        template <
            class Allocator,
            typename = typename std::enable_if <
                bits::fold_and <
                    (std::is_copy_constructible <Ts>::value ||
                     std::is_constructible <
                        Ts, Ts const &, Allocator const &
                     >::value)...
                >::value
            >::type
        >
        tuple (std::allocator_arg_t,
               Allocator const & alloc,
               tuple const & other)
            : base (std::allocator_arg_t {}, alloc,
                    detail::base_access <tuple>::slice (other))
        {}

        /*
         * Move construction by uses-allocator construction for each member
         * object, passed to constructors as if by
         * stl::move (stl::get <i> (other)) for each i, appropriately detected
         * by the constructor for each inherited node.
         *
         * The requirement that std::is_move_constructible <Ti>::value is true
         * for each i must also be met.
         */
        template <
            class Allocator,
            typename = typename std::enable_if <
                bits::fold_and <
                    (std::is_copy_constructible <Ts>::value ||
                     std::is_constructible <
                        Ts, Ts &&, Allocator const &
                     >::value)...
                >::value
            >::type
        >
        tuple (std::allocator_arg_t,
               Allocator const & alloc,
               tuple && other)
            : base (std::allocator_arg_t {}, alloc,
                    detail::base_access <tuple>::slice (stl::move (other)))
        {}

        /*
         * Converting construction by uses-allocator construction of each member
         * object from the member objects in other, passed to constructors as if
         * by stl::get <i> (other) for each i.
         *
         * This constructor requires that sizeof... (Us) == sizeof... (Ts).
         * The requirement that std::is_constructible <Ti, Ui const &>::value is
         * true for each i must also be met.
         */
        template <
            class Allocator, class ... Us,
            typename = typename std::enable_if <
                sizeof... (Us) == size::value &&
                bits::fold_and <
                    (std::is_constructible <Ts, Us const &>::value ||
                     std::is_constructible <
                        Ts, Us const &, Allocator const &
                     >::value)...
                >::value
            >::type
        >
        tuple (std::allocator_arg_t,
               Allocator const & alloc,
               tuple <Us...> const & other)
            : base (std::allocator_arg_t {}, alloc,
                    detail::base_access <tuple <Us...>>::slice (other))
        {}

        /*
         * Converting construction by uses-allocator construction of each member
         * object from the member objects in other, passed to constructors as if
         * by stdl::forward <Ui> (stl::get <i> (other)) for each i.
         *
         * This constructor requires that sizeof... (Us) == sizeof... (Ts).
         * The requirement that std::is_constructible <Ti, Ui &&>::value is true
         * for each i must also be met.
         */
        template <
            class Allocator, class ... Us,
            typename = typename std::enable_if <
                sizeof... (Us) == size::value &&
                bits::fold_and <
                    (std::is_constructible <Ts, Us &&>::value ||
                     std::is_constructible <
                        Ts, Us &&, Allocator const &
                     >::value)...
                >::value
            >::type
        >
        tuple (std::allocator_arg_t,
               Allocator const & alloc,
               tuple <Us...> && other)
            : base (
                std::allocator_arg_t {}, alloc,
                detail::base_access <tuple <Us...>>::slice (stl::move (other))
            )
        {}

        /*
         * Copy assignment operator of each member object by the corresponding
         * member object of other.
         */
        tuple & operator= (tuple const & other)
        {
            detail::base_access <tuple>::slice (*this) =
                detail::base_access <tuple>::slice (other);
            return *this;
        }

        /*
         * Moveassignment operator of each member object by the corresponding
         * member object of other, using move semantics.
         */
        tuple & operator= (tuple && other)
            noexcept (noexcept (
                stl::declval <base &> () = stl::declval <base &&> ()
            ))
        {
            detail::base_access <tuple>::slice (*this) =
                detail::base_access <tuple>::slice (stl::move (other));
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
                sizeof... (Us) == size::value &&
                bits::fold_and <
                    std::is_assignable <Ts, Us const &>::value...
                >::value
            >::type
        >
        tuple & operator= (tuple <Us...> const & other)
        {
            detail::base_access <tuple>::slice (*this) =
                detail::base_access <tuple <Us...>>::slice (other);
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
                sizeof... (Us) == size::value &&
                bits::fold_and <
                    std::is_assignable <Ts, Us &&>::value...
                >::value
            >::type
        >
        tuple & operator= (tuple <Us...> && other)
        {
            detail::base_access <tuple>::slice (*this) =
                detail::base_access <tuple <Us...>>::slice (stl::move (other));
            return *this;
        }

        /*
         * Swaps each member object with the corresponding member object of
         * other.
         */
        void swap (tuple & other)
            noexcept (noexcept (
                stl::declval <base &> ().swap (stl::declval <base &> ())
            ))
        {
            detail::base_access <tuple>::slice (*this).swap (
                detail::base_access <tuple>::slice (other)
            );
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

    /*
     * Index based get <>. This does not compile if the index is greater than
     * one less than the number of types.
     *
     * We will be using the C++14 signatures.
     */
namespace detail
{
    template <class ... Ts>
    struct base_access <tuple <Ts...>>
    {
        using base = typename tuple <Ts...>::base;

        /*
         * These are helper methods for object-slicing references to our tuple
         * into references to the tuple_impl.
         */
        static base & slice (tuple <Ts...> & t) noexcept
        {
            return static_cast <base &> (t);
        }

        static base const & slice (tuple <Ts...> const & t) noexcept
        {
            return static_cast <base const &> (t);
        }

        static base && slice (tuple <Ts...> && t) noexcept
        {
            return static_cast <base &&> (stl::move (t));
        }
    };

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
            return typename std::decay <Tuple>::type (stl::forward <Tuple> (t));
        }
    };

    template <>
    struct tuple_cat_helper <2>
    {
        template <
            std::size_t ... I1, std::size_t ... I2, class Tuple1, class Tuple2
        >
        static constexpr typename concat_type <
            typename std::decay <Tuple1>::type,
            typename std::decay <Tuple2>::type
        >::type
            concat (stl::index_sequence <I1...>,
                    stl::index_sequence <I2...>,
                    Tuple1 && t1,
                    Tuple2 && t2) noexcept
        {
            using result = typename concat_type <
                typename std::decay <Tuple1>::type,
                typename std::decay <Tuple2>::type
            >::type;
            return result (
                stl::get <I1> (stl::forward <Tuple1> (t1))...,
                stl::get <I2> (stl::forward <Tuple2> (t2))...
            );
        }

        template <class Tuple1, class Tuple2>
        static constexpr typename concat_type <
            typename std::decay <Tuple1>::type,
            typename std::decay <Tuple2>::type
        >::type
            dispatch (Tuple1 && t1, Tuple2 && t2) noexcept
        {
            using size1 = stl::tuple_size <typename std::decay <Tuple1>::type>;
            using size2 = stl::tuple_size <typename std::decay <Tuple2>::type>;
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
        static constexpr typename concat_type <
            typename std::decay <Tuple1>::type,
            typename std::decay <Tuple2>::type,
            typename std::decay <Tuples>::type...
        >::type
            concat (stl::index_sequence <I1...>,
                    stl::index_sequence <I2...>,
                    Tuple1 && t1, Tuple2 && t2,
                    Tuples && ... ts)
        {
            using partial = typename concat_type <
                typename std::decay <Tuple1>::type,
                typename std::decay <Tuple2>::type
            >::type;
            return tuple_cat_helper <2>::dispatch (
                partial (get <I1> (stl::forward <Tuple1> (t1))...,
                         get <I2> (stl::forward <Tuple2> (t2))...),
                tuple_cat_helper <N - 2>::dispatch (
                    stl::forward <Tuples> (ts)...
                )
            );
        }

        template <class Tuple1, class Tuple2, class ... Tuples>
        static constexpr typename concat_type <
            typename std::decay <Tuple1>::type,
            typename std::decay <Tuple2>::type,
            typename std::decay <Tuples>::type...
        >::type
            dispatch (Tuple1 && t1, Tuple2 && t2, Tuples && ... ts) noexcept
        {
            using size1 = stl::tuple_size <typename std::decay <Tuple1>::type>;
            using size2 = stl::tuple_size <typename std::decay <Tuple2>::type>;
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
    constexpr typename detail::concat_type <
        typename std::decay <Tuples>::type...
    >::type
        tuple_cat (Tuples && ... tuples)
    {
        return detail::tuple_cat_helper <sizeof... (Tuples)>::dispatch (
            stl::forward <Tuples> (tuples)...
        );
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

            return detail::base_access <lhs_tuple>::slice (lhs) ==
                detail::base_access <rhs_tuple>::slice (rhs);
        }

        template <class ... Ts, class ... Us>
        static constexpr bool do_lt (tuple <Ts...> const & lhs,
                                     tuple <Us...> const & rhs) noexcept
        {
            using lhs_tuple = tuple <Ts...>;
            using rhs_tuple = tuple <Ts...>;

            return detail::base_access <lhs_tuple>::slice (lhs) <
                detail::base_access <rhs_tuple>::slice (rhs);
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
