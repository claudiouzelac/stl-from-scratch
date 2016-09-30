//
// The utility header. This file contains our implementation of integer
// sequences, pairs, and a few additional constructs.
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

#ifndef STL_FROM_SCRATCH_UTILITY_HEADER
#define STL_FROM_SCRATCH_UTILITY_HEADER

/*
 * We need <cstddef> for std::size_t, <initizlier_list> for compability, and
 * <type_traits> because it would take too long to implement ourselves.
 */
#include <cstddef>
#include <initializer_list>
#include <type_traits>

/*
 * The sequence_generator header defines our utility types for generating
 * integer sequences.
 */
#include <stl/bits/sequence_generator.hpp>

/*
 * The type_helpers header defines some utility structs for decaying and
 * extracting types. It also contains the implementations of tuple_size
 * and tuple_element for pairs and tuples.
 */
#include <stl/bits/type_helpers.hpp>


namespace stl
{
/*
 * This small namespace is here only for completeness. There's not much to it.
 *
 * From cppreference:
 *      The rel_ops namespace provides automatic comparison operators built
 *      from user defined operator== and operator<.
 */
namespace rel_ops
{
    template <typename T>
    bool operator!= (T const & a, T const & b)
    {
        return !(a == b);
    }

    template <typename T>
    bool operator> (T const & a, T const & b)
    {
        return b < a;
    }

    template <typename T>
    bool operator<= (T const & a, T const & b)
    {
        // same semantics as: a < b || a == b
        return !(a > b);
    }

    template <typename T>
    bool operator>= (T const & a, T const & b)
    {
        // same semantics as: b < a || a == b
        return !(a < b);
    }
}

    /*
     * The move operation converts lvalues to rvalues, which is to say it merely
     * casts its argument to an rvalue reference (technically speaking it
     * converts its argument to an xvalue expression, see cppreference - value
     * categories).
     *
     * We will be writing the C++17 signature of move.
     */
    template <class T>
    constexpr typename std::remove_reference <T>::type && move (T && t) noexcept
    {
        return static_cast <typename std::remove_reference <T>::type &&> (t);
    }

    /*
     * The move_if_noexcept operation converts lvalues to rvalues, if it is
     * the case that T is nothrow move constructible, and otherwise converts
     * lvalues to lvalue references to const. One technicality, we convert to
     * an lvalue reference to const only in the case that T also happens to be
     * copy constructible.
     *
     * We will be writing the C++14 signature of move_if_noexcept.
     */
    template <class T>
    constexpr typename std::conditional <
        !std::is_nothrow_move_constructible <T>::value &&
        std::is_copy_constructible <T>::value,
        T const &,
        T &&
    >::type move_if_noexcept (T & t) noexcept
    {
        using forward_type = typename std::conditional <
            !std::is_nothrow_move_constructible <T>::value &&
            std::is_copy_constructible <T>::value,
            T const &,
            T &&
        >::type;

        return static_cast <forward_type> (t);
    }

    /*
     * There are two signatures for forward, one accepting an lvalue reference,
     * the other accepting an rvalue reference. Both "forward" their argument
     * as a result of the same value catagory as was provided.
     *
     * The reference collapsing rules for template parameter types are:
     *      - (T &) & =>   T &
     *      - (T &) && =>  T &
     *      - (T &&) & =>  T &
     *      - (T &&) && => T &&
     *
     * Like with move, we will be writing the C++17 signatures for forward.
     */
    template <class T>
    constexpr T && forward (typename std::remove_reference <T>::type & t)
        noexcept
    {
        /*
         * What to do with an lvalue? We want to return in the same value
         * category, so forward `t` as an lvalue. Notice the difference between
         * this and the implementation of move. By writting `T &&` explicitly
         * the expression undergoes reference collapsing.
         */
        return static_cast <T &&> (t);
    }

    template <class T>
    constexpr
    typename std::enable_if <!std::is_lvalue_reference <T>::value, T &&>::type
        forward (typename std::remove_reference <T>::type && t) noexcept
    {
        /*
         * What to do with an rvalue? We want to return in the same value
         * category, so forward `t` as an rvalue. Notice the difference between
         * this and the implementation of move. By writting `T &&` explicitly
         * the expression undergoes reference collapsing.
         *
         * There is one caveat: we cannot forward an rvalue as an lvalue,
         * hence the enable_if in the declaration. If we try to forward an
         * rvalue as an lvalue this will fail to compile.
         */
        return static_cast <T &&> (t);
    }

    /*
     * declval is an unimplemented method with return type equal to an
     * rvalue reference to a value of its template type T. For this reason it
     * can be used only in unevaluated contexts.
     *
     * The only thing we have to do is write the signature.
     */
    template <class T>
    typename std::add_rvalue_reference <T>::type declval (void) noexcept;

    /*
     * There are two signatures for swap that we need to implement. Single
     * pairs of values and arrays of values.
     *
     * Swapping is straightforward, just switch the contents of the two
     * variables using move construction and assignment.
     */
    template <class T>
    void swap (T & a, T & b)
        noexcept (std::is_nothrow_move_constructible <T>::value &&
                  std::is_nothrow_move_assignable <T>::value)
    {
        T temp {stl::move (a)};
        a = stl::move (b);
        b = stl::move (temp);
    }

    template <class T, std::size_t N>
    void swap (T (&a_array) [N], T (&b_array) [N])
        noexcept (noexcept (
            swap (stl::declval <T &> (), stl::declval <T &> ())
        ))
    {
        for (std::size_t i = 0; i < N; ++i) {
            swap (a_array [i], b_array [i]);
        }
    }

    /*
     * Exchange is similar to swap, except that it returns the value held
     * previously in its first argument.
     *
     * Notice that the second argument is a forwarding reference (not an
     * rvalue reference, since it is a template parameter). Therefore, in
     * our implementation we will be forwarding `val` instead of moving it.
     */
    template <class T, class U = T>
    T exchange (T & a, U && val)
    {
        T ret {stl::move (a)};
        a = stl::forward <U> (val);
        return ret;
    }

    /*
     * Integer sequences are compile time lists of integral values in a type T.
     *
     * Our implementation is going to use the naive O(N) recursive sequence
     * generation. A common O(log N) generation technique to avoid the O(N)
     * runtime of the recursive implementation is to generate the sequence two
     * at a time, recursively, and then merge from the bottom up.
     */
    template <class T, T ... Is>
    class integer_sequence
    {
    public:
        using type = integer_sequence;
        using value_type = T;

        static constexpr std::size_t size (void) noexcept
        {
            return sizeof... (Is);
        }
    };

    /*
     * Index sequences are the special case of integer sequences with a value
     * type of std::size_t. We will use them later on to implement tuples, among
     * other things.
     */
    template <std::size_t ... Is>
    using index_sequence = integer_sequence <std::size_t, Is...>;

    template <typename T, T N>
    using make_integer_sequence =
        typename bits::sequence_generator_helper <T, N>::type;

    template <std::size_t N>
    using make_index_sequence = make_integer_sequence <std::size_t, N>;

    /*
     * The following contains our implementation of pair. First we'll have
     * define a helper classes that will allow us to disambiguate construction
     * of pairs and tuples, and forward declare tuple itself.
     */
    template <class ... Ts>
    class tuple;

    /*
     * We also need to forward declare get for tuples. These are the C++14
     * signatures.
     */
    template <std::size_t I, class ... Ts>
    constexpr typename bits::tuple_element_helper <I, tuple <Ts...>>::type &
        get (tuple <Ts...> & t) noexcept;

    template <std::size_t I, class ... Ts>
    constexpr typename bits::tuple_element_helper <I, tuple <Ts...>>::type &&
        get (tuple <Ts...> && t) noexcept;

    template <std::size_t I, class ... Ts>
    constexpr
    typename bits::tuple_element_helper <I, tuple <Ts...>>::type const &
        get (tuple <Ts...> const & t) noexcept;

    template <std::size_t I, class ... Ts>
    constexpr
    typename bits::tuple_element_helper <I, tuple <Ts...>>::type const &&
        get (tuple <Ts...> const && t) noexcept;

    /* 
     * piecewise_construct_t is a type tag used to select pair and tuple
     * constructors where the constructor arguments may be ambiguous.
     */
    struct piecewise_construct_t {};

    /*
     * We'll be using the C++14 signatures for pair's constructors and member
     * functions.
     */
    template <class T1, class T2>
    class pair
    {
    public:
        /* first and second are public member objects of pair */
        T1 first;
        T2 second;

        using first_type  = T1;
        using second_type = T2;

        /* we can default this and let the compiler generate the constructor */
        constexpr pair (void) = default;

        constexpr pair (T1 const & t1, T2 const & t2)
            : first  (t1)
            , second (t2)
        {}

        /*
         * This constructor forwards its arguments to the constructors of first
         * and second, respectively. It participates in overload resolution
         * only if U1 is implicitly convertible to T1 and U2 is implicitly
         * convertible to T2, which is why we use the defaulted third parameter
         * enable_if expression.
         */
        template <class U1, class U2>
        constexpr pair (U1 && u1, U2 && u2,
                        typename std::enable_if <
                            std::is_convertible <U1, T1>::value &&
                            std::is_convertible <U2, T2>::value
                        >::type * = nullptr)
            : first  (stl::forward <U1> (u1))
            , second (stl::forward <U2> (u2))
        {}

        /*
         * Constructs first from the first element of the argument, and second
         * from the second element of the argument. The constructions must be
         * well-formed for the types T1 and T2, or use of this constructor will
         * not compile.
         */
        template <class U1, class U2>
        constexpr pair (pair <U1, U2> const & p)
            : first  (p.first)
            , second (p.second)
        {}

        /*
         * Constructs first from the moved-from value of the first element of
         * the argument, and second from the moved-from value of the second
         * element of the argument. The constructions must be well-formed for
         * the types T1 and T2, otherwise use of this constructor will not
         * compile.
         */
        template <class U1, class U2>
        constexpr pair (pair <U1, U2> && p)
            : first  (stl::move (p.first))
            , second (stl::move (p.second))
        {}

    private:
        /*
         * Helper constructor for the piecewise_construct_t overload of pair.
         */
        template <class T, class ... Args, std::size_t ... I>
        static T &&
        element_constructor (tuple <Args...> && tup, index_sequence <I...>)
        {
            static_assert (
                std::is_constructible <T, Args...>::value,
                "cannot construct member object type from provided arguments"
            );

            /* both Args and I are simultaneously unpacked */
            return T {stl::forward <Args> (get <I> (stl::move (tup)))...};
        }

    public:
        /*
         * Constructs first and second from the forwarded elements of each
         * argument tuple, respectively.
         */
        template <class ... Args1, class ... Args2>
        pair (piecewise_construct_t,
              tuple <Args1...> first_args,
              tuple <Args2...> second_args)
            : first (
                element_constructor <first_type> (
                    stl::move (first_args),
                    make_index_sequence <sizeof... (Args1)> {}
                )
            )
            , second (
                element_constructor <second_type> (
                    stl::move (second_args),
                    make_index_sequence <sizeof... (Args2)> {}
                )
            )
        {}

        /* we can default this and let the compiler generate the destructor */
        ~pair (void) noexcept = default;

        /*
         * As per the specification, the copy and move constructors of pair are
         * defaulted.
         */
        constexpr pair (pair const &) = default;
        constexpr pair (pair &&) = default;

        /*
         * We have four assignment operators to implement. The first two are
         * the standard copy and move assignment operators, while the second
         * two are templated versions of each (the entailed assignments must
         * be well formed for the types T1 and T2, otherwise use of those
         * assignment operators will not compile).
         */
        pair & operator= (pair const & other)
        {
            this->first  = other.first;
            this->second = other.second;
            return *this;
        }

        pair & operator= (pair && other)
            noexcept (std::is_nothrow_move_assignable <T1>::value &&
                      std::is_nothrow_move_assignable <T2>::value)
        {
            this->first  = stl::move (other.first);
            this->second = stl::move (other.second);
            return *this;
        }

        template <class U1, class U2>
        pair & operator= (pair <U1, U2> const & other)
        {
            this->first  = other.first;
            this->second = other.second;
            return *this;
        }

        template <class U1, class U2>
        pair & operator= (pair <U1, U2> && other)
        {
            this->first  = stl::forward <U1> (other.first);
            this->second = stl::forward <U2> (other.second);
            return *this;
        }

        /*
         * Our last member function to implement is swap. It swaps across the
         * first elements of this and other, and across the second elements of
         * this and other. The method will be noexcept if the swapping of each
         * of the first and second elements is noexcept.
         */
        void swap (pair & other)
            noexcept (
                noexcept (
                    stl::swap (stl::declval <T1 &> (), stl::declval <T1 &> ())
                ) &&
                noexcept (
                    stl::swap (stl::declval <T2 &> (), stl::declval <T2 &> ())
                )
            )
        {
            using stl::swap;
            swap (this->first, other.first);
            swap (this->second, other.second);
        }

    private:
        T1 & select (bits::index_tag <0>) & noexcept
        {
            return this->first;
        }

        T2 & select (bits::index_tag <1>) & noexcept
        {
            return this->second;
        }

        T1 const & select (bits::index_tag <0>) const & noexcept
        {
            return this->first;
        }

        T2 const & select (bits::index_tag <1>) const & noexcept
        {
            return this->second;
        }

        T1 && select (bits::index_tag <0>) && noexcept
        {
            return stl::move (this->first);
        }

        T2 && select (bits::index_tag <1>) && noexcept
        {
            return stl::move (this->second);
        }
    };

    /*
     * Now we'll implement the free functions operating on pairs.
     */

    /*
     * Comparison for pairs is lexicographic (meaning comparisons will happen
     * for the first elements, and then conditionally for the second elements).
     *
     * We will be using the C++14 signatures for these.
     */
    template <class T1, class T2>
    constexpr bool
        operator== (pair <T1, T2> const & p1, pair <T1, T2> const & p2)
    {
        return p1.first == p2.first && p1.second == p2.second;
    }

    template <class T1, class T2>
    constexpr bool
        operator!= (pair <T1, T2> const & p1, pair <T1, T2> const & p2)
    {
        return p1.first != p2.first || p1.second != p2.second;
    }

    template <class T1, class T2>
    constexpr bool
        operator< (pair <T1, T2> const & p1, pair <T1, T2> const & p2)
    {
        return p1.first < p2.first ? true  :
               p2.first < p2.first ? false :
               p1.second < p2.second;
    }

    template <class T1, class T2>
    constexpr bool
        operator> (pair <T1, T2> const & p1, pair <T1, T2> const & p2)
    {
        return p2 < p1;
    }

    template <class T1, class T2>
    constexpr bool
        operator<= (pair <T1, T2> const & p1, pair <T1, T2> const & p2)
    {
        return !(p2 < p1); 
    }

    template <class T1, class T2>
    constexpr bool
        operator>= (pair <T1, T2> const & p1, pair <T1, T2> const & p2)
    {
        return !(p1 < p2); 
    }

    /*
     * make_pair constructs a new pair with types deduced from its arguments
     * after applying std::decay to the template type parameters (unless the
     * decay produces a reference wrapper, in which case we convert to the
     * underlying reference). In the implementation below, note that when
     * forwarding to the constructor of pair we rely on stl::reference_wrapper
     * to implicitly convert to its underlying reference.
     *
     * This is the C++14 signature of make_pair.
     */
    template <typename T1, typename T2>
    constexpr pair <
        typename bits::decay_reference_wrapper <
            typename std::decay <T1>::type
        >::type,
        typename bits::decay_reference_wrapper <
            typename std::decay <T2>::type
        >::type
    > make_pair (T1 && t1, T2 && t2)
    {
        using t1_decay = typename bits::decay_reference_wrapper <
            typename std::decay <T1>::type
        >::type;
        using t2_decay = typename bits::decay_reference_wrapper <
            typename std::decay <T2>::type
        >::type;

        return pair <t1_decay, t2_decay> {
            stl::forward <t1_decay> (t1), stl::forward <t2_decay> (t2)
        };
    }

    /*
     * With pairs implemented we can write our specializations of free-function
     * swap and get. We use the C++14 signatures for the specializations of get.
     */
    template <class T1, class T2>
    void swap (pair <T1, T2> & p1, pair <T1, T2> & p2)
        noexcept (noexcept (p1.swap (p2)))
    {
        p1.swap (p2);
    }

namespace detail
{
    template <class T1, class T2>
    constexpr T1 & get_helper (pair <T1, T2> & p, bits::index_tag <0>) noexcept
    {
        return p.first;
    }

    template <class T1, class T2>
    constexpr T2 & get_helper (pair <T1, T2> & p, bits::index_tag <1>) noexcept
    {
        return p.second;
    }

    template <class T1, class T2>
    constexpr T1 const &
        get_helper (pair <T1, T2> const & p, bits::index_tag <0>) noexcept
    {
        return p.first;
    }

    template <class T1, class T2>
    constexpr T2 const &
        get_helper (pair <T1, T2> const & p, bits::index_tag <1>) noexcept
    {
        return p.second;
    }

    template <class T1, class T2>
    constexpr T1 && get_helper (pair <T1, T2> && p, bits::index_tag <0>)
        noexcept
    {
        return stl::move (p.first);
    }

    template <class T1, class T2>
    constexpr T2 && get_helper (pair <T1, T2> && p, bits::index_tag <1>)
        noexcept
    {
        return stl::move (p.second);
    }
}   // namespace detail

    /*
     * For type based versions of get, the types T1 and T2 cannot be the same.
     */
    template <std::size_t I, class T1, class T2>
    constexpr typename tuple_element <I, pair <T1, T2>>::type &
        get (pair <T1, T2> & p) noexcept
    {
        return detail::get_helper (p, bits::index_tag <I> {});
    }

    template <std::size_t I, class T1, class T2>
    constexpr typename tuple_element <I, pair <T1, T2>>::type const &
        get (pair <T1, T2> const & p) noexcept
    {
        return detail::get_helper (p, bits::index_tag <I> {});
    }

    template <std::size_t I, class T1, class T2>
    constexpr typename tuple_element <I, pair <T1, T2>>::type &&
        get (pair <T1, T2> && p) noexcept
    {
        return detail::get_helper (stl::move (p), bits::index_tag <I> {});
    }

    template <class T, class U>
    constexpr T & get (pair <T, U> & p) noexcept
    {
        return p.first;
    }

    template <class T, class U>
    constexpr T const & get (pair <T, U> const & p) noexcept
    {
        return p.first;
    }

    template <class T, class U>
    constexpr T && get (pair <T, U> && p) noexcept
    {
        return stl::move (p.first);
    }

    template <class T, class U>
    constexpr T & get (pair <U, T> & p) noexcept
    {
        return p.second;
    }

    template <class T, class U>
    constexpr T const & get (pair <U, T> const & p) noexcept
    {
        return p.second;
    }

    template <class T, class U>
    constexpr T && get (pair <U, T> && p) noexcept
    {
        return stl::move (p.second);
    }
}   // namespace stl

#endif  // #ifndef STL_FROM_SCRATCH_UTILITY_HEADER
