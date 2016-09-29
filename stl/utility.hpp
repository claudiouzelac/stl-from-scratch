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
            stl::swap (stl::declval <T &> (), stl::declval <T &> ())
        ))
    {
        for (std::size_t i = 0; i < N; ++i) {
            stl::swap (a_array [i], b_array [i]);
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

namespace detail
{
    template <class T, std::size_t N, T ... Is>
    struct sequence_generator
    {
        using type = typename sequence_generator <
            T, N - 1, T {N - 1}, Is...
        >::type;
    };

    template <class T, T ... Is>
    struct sequence_generator <T, 0, Is...> : integer_sequence <T, Is...> {};

    template <class T, T N>
    struct sequence_generator_helper
    {
        static_assert (N >= 0, "cannot produce sequence of negative length");

        using type = typename sequence_generator <T, std::size_t {N}>::type;
    };
}   // namespace detail

    /*
     * Index sequences are the special case of integer sequences with a value
     * type of std::size_t. We will use them later on to implement tuples, among
     * other things.
     */
    template <std::size_t ... Is>
    using index_sequence = integer_sequence <std::size_t, Is...>;

    template <typename T, T N>
    using make_integer_sequence =
        typename detail::sequence_generator_helper <T, N>::type;

    template <std::size_t N>
    using make_index_sequence = make_integer_sequence <std::size_t, N>;
}   // namespace stl

#endif  // #ifndef STL_FROM_SCRATCH_UTILITY_HEADER
