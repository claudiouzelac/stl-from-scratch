//
// Unit tests for our implementation of pair. This is really more
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

#include <cassert>
#include <type_traits>
#include <stl/utility.hpp>


int main (void)
{
    /* can we define a pair of default constructible types? */
    {
        stl::pair <int, double> a {};
        stl::pair <double, int> b {};
        stl::pair <int, int> c {};

        /* are they different types? */
        static_assert (
            !std::is_same <decltype (a), decltype (b)>::value,
            "expected different types"
        );
        static_assert (
            !std::is_same <decltype (a), decltype (c)>::value,
            "expected different types"
        );
        static_assert (
            !std::is_same <decltype (b), decltype (c)>::value,
            "expected different types"
        );

        /* are the types correct? */
        static_assert (
            std::is_same <decltype(a.first), int>::value,
            "first type is incorrect"
        );
        static_assert (
            std::is_same <decltype(stl::get <0> (a)), int &>::value,
            "first type is incorrect"
        );
        static_assert (
            std::is_same <decltype(a.second), double>::value,
            "second type is incorrect"
        );
        static_assert (
            std::is_same <decltype(stl::get <1> (a)), double &>::value,
            "first type is incorrect"
        );
        static_assert (
            std::is_same <decltype(c.first), int>::value,
            "first type is incorrect"
        );
        static_assert (
            std::is_same <decltype(c.second), int>::value,
            "second type is incorrect"
        );
    }

    /* can we define a pair of non-default constructible types? */
    {
        struct foo
        {
            foo (void) = delete;
            foo (int) {}
        };

        struct bar
        {
            bar (void) = delete;
            bar (int) {}
        };

        stl::pair <foo, bar> a {foo {0}, bar {1}};
        stl::pair <int, bar> b {0, bar {1}};
        stl::pair <foo, int> c {foo {0}, 1};

        /* can they be correctly constructed with implicit arguments? */
        stl::pair <foo, bar> d {0, 1};

        /* are the types correct? */
        static_assert (
            std::is_same <decltype(a.first), foo>::value,
            "first type is incorrect"
        );
        static_assert (
            std::is_same <decltype(a.second), bar>::value,
            "second type is incorrect"
        );
    }

    /* can we define a pair constructed from make_pair? */
    {
        struct foo
        {
            foo (void) = default;
        };

        struct bar
        {
            bar (void) = default;
        };

        stl::pair <foo, bar> a {stl::make_pair (foo {}, bar {})};
        auto b = stl::make_pair (foo {}, bar {});

        /* are the types the same? */
        static_assert (
            std::is_same <decltype(a), decltype(b)>::value,
            "expected the same type"
        );
    }

    /* can we create a pair of references? */
    {
        int x = 0;
        int y = 1;
        stl::pair <int &, int &> a {x, y};

        /* are the values correct? */
        assert (a.first == 0 && "first value is incorrect");
        assert (a.second == 1 && "second value is incorrect");

        /* if we change the values of x and y, are the values correct? */
        x = 10;
        y = 20;
        assert (a.first == 10 && "first value is incorrect");
        assert (a.second == 20 && "second value is incorrect");

        /*
         * If we change the values of the pair, are the new values of x and y
         * correct?
         */
        a.first *= 5;
        a.second /= 5;
        assert (x == 50 && "first value is incorrect");
        assert (y == 4 && "second value is incorrect");

        /* how about if we use `get`? */
        stl::get <0> (a) = 100;
        stl::get <1> (a) = 0;
        assert (x == 100 && "first value is incorrect");
        assert (y == 0 && "second value is incorrect");

        /* and now if we swap? */
        int w = 1;
        int z = 2;
        stl::pair <int &, int &> b {w, z};
        a.swap (b);
        assert (x == 1 && "first value is incorrect");
        assert (y == 2 && "second value is incorrect");
        assert (w == 100 && "first value is incorrect");
        assert (z == 0 && "second value is incorrect");
    }

    /* are comparisons correct? */
    {
        stl::pair <int, int> a {0, 0};
        stl::pair <int, int> b {1, 1};
        stl::pair <int, int> c {0, 1};
        stl::pair <int, int> d {1, 0};

        assert (a == a && "incorrect comparison");
        assert (a != b && "incorrect comparison");
        assert (a != c && "incorrect comparison");
        assert (a != d && "incorrect comparison");
        assert (a < b && "incorrect comparison");
        assert (a < c && "incorrect comparison");
        assert (a < d && "incorrect comparison");
        assert (b > c && "incorrect comparison");
        assert (b > d && "incorrect comparison");
        assert (c <= d && "incorrect comparison");
        assert (b >= c && "incorrect comparison");
    }
}
