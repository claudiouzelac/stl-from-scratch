//
// Unit tests for our implementation of utility (sans pari). This is really more
// of a check to be sure that things compile correctly.
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

#include <type_traits>
#include <stl/utility.hpp>


int main (void)
{
    /* return type of move and move_if_noexcept */
    {
        struct foo
        {
            foo (foo &&) noexcept (false) {}
            foo (foo const &) = default;
        };

        struct bar
        {
            bar (bar &&) noexcept (true) {}
        };

        static_assert (
            std::is_same <
                foo &&, decltype (stl::move (stl::declval <foo &> ()))
            >::value,
            "expected rvalue reference"
        );

        static_assert (
            std::is_same <
                bar &&, decltype (stl::move_if_noexcept (stl::declval <bar &> ()))
            >::value,
            "expected rvalue reference"
        );

        static_assert (
            std::is_same <
                foo const &,
                decltype (stl::move_if_noexcept (stl::declval <foo &> ()))
            >::value,
            "expected lvalue reference to const"
        );
    }

    /* return type of forward */
    {
        struct foo {};

        static_assert (
            std::is_same <
                foo &,
                decltype (stl::forward <foo &> (stl::declval <foo &> ()))
            >::value,
            "excpeted lvalue reference"
        );

        static_assert (
            std::is_same <
                foo &&,
                decltype (stl::forward <foo &&> (stl::declval <foo &&> ()))
            >::value,
            "excpeted rvalue reference"
        );

        static_assert (
            std::is_same <
                foo &&,
                decltype (stl::forward <foo &&> (stl::declval <foo &> ()))
            >::value,
            "excpeted rvalue reference"
        );

        static_assert (
            std::is_same <
                foo const &,
                decltype (stl::forward <foo const &> (stl::declval <foo const &> ()))
            >::value,
            "excpeted lvalue reference to const"
        );

        static_assert (
            std::is_same <
                foo const &,
                decltype (stl::forward <foo const &> (stl::declval <foo &> ()))
            >::value,
            "excpeted lvalue reference to const"
        );
    }

    /* integer sequences */
    static_assert (
        std::is_same <stl::index_sequence <>, stl::make_index_sequence <0>::type>::value,
        "wrong sequence"
    );

    static_assert (
        std::is_same <stl::index_sequence <0>, stl::make_index_sequence <1>::type>::value,
        "wrong sequence"
    );

    static_assert (
        std::is_same <
            stl::index_sequence <0, 1, 2, 3, 4>,
            stl::make_index_sequence <5>::type
        >::value,
        "wrong sequence"
    );

    static_assert (
        std::is_same <
            stl::index_sequence <0, 1, 2, 3, 4>,
            stl::make_index_sequence <5>::type
        >::value,
        "wrong sequence"
    );

    static_assert (
        std::is_same <
            stl::integer_sequence <int, 0, 1, 2, 3, 4, 5, 6, 7, 8>,
            stl::make_integer_sequence <int, 9>::type
        >::value,
        "wrong sequence"
    );
}
