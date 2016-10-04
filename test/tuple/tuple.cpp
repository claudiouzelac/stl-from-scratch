//
// Unit tests for our implementation of tuple. This is really more
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

#ifdef NDEBUG
#undef NDEBUG
#endif

#include <cassert>
#include <type_traits>
#include <stl/tuple.hpp>


int main (void)
{
    /* can we construct an object of the empty tuple type? */
    {
        stl::tuple <> empty {};

        /* do they always compare equal? */
        stl::tuple <> other {};
        static_assert (empty == other, "empty tuples do not compare equal");

        /* are other comparisons correct? */
        static_assert (
            !(empty < other), "empty tuples do not compare correctly"
        );
        static_assert (
            !(empty > other), "empty tuples do not compare correctly"
        );
        static_assert (
            empty <= other, "empty tuples do not compare correctly"
        );
        static_assert (
            empty >= other, "empty tuples do not compare correctly"
        );
    }

    /* can we define tuples of a single default-constructible type? */
    {
        stl::tuple <int> a {};
        stl::tuple <double> b {};
        stl::tuple <float> const c {};

        /* are they different types? */
        static_assert (
            !std::is_same <decltype (a), decltype (b)>::value,
            "expected different types"
        );
        static_assert (
            !std::is_same <decltype (a), decltype (c)>::value,
            "expected different types"
        );

        /* are the types correct? */
        static_assert (
            std::is_same <decltype (stl::get <0> (a)), int &>::value,
            "wrong type obtained from get"
        );
        static_assert (
            std::is_same <decltype (stl::get <0> (b)), double &>::value,
            "wrong type obtained from get"
        );
        static_assert (
            std::is_same <
                decltype (stl::get <0> (stl::move (a))), int &&
            >::value,
            "wrong type obtained from get"
        );
        static_assert (
            std::is_same <decltype (stl::get <0> (c)), float const &>::value,
            "wrong type obtained from get"
        );
    }

    /* can we define tuples of two default constructible types? */
    {
        stl::tuple <int, double> a {};
        stl::tuple <double, int> b {};
        stl::tuple <int, int> c {};

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
            std::is_same <decltype(stl::get <0> (a)), int &>::value,
            "first type is incorrect"
        );
        static_assert (
            std::is_same <decltype(stl::get <int> (a)), int &>::value,
            "first type is incorrect"
        );
        static_assert (
            std::is_same <decltype(stl::get <1> (a)), double &>::value,
            "second type is incorrect"
        );
        static_assert (
            std::is_same <decltype(stl::get <double> (a)), double &>::value,
            "second type is incorrect"
        );
        static_assert (
            std::is_same <decltype(stl::get <0> (c)), int &>::value,
            "first type is incorrect"
        );
        static_assert (
            std::is_same <decltype(stl::get <1> (c)), int &>::value,
            "second type is incorrect"
        );

        /* are the default values correct? */
        assert (stl::get <0> (a) == 0);
        assert (stl::get <1> (a) == 0.0);
    }
}
