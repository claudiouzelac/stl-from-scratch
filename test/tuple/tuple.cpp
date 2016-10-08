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
#include <memory>
#include <string>
#include <type_traits>
#include <vector>
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

    /* can we define tuples of non-default constructible types? */
    {
        struct foo
        {
            foo (void) = delete;
            foo (int) {}
        };

        // does not compile, as expected
        // stl::tuple <foo, foo> f {};

        // compiles, as expected
        stl::tuple <foo, foo> a {0, 1};
        stl::tuple <foo, int> b {0, 1};
        stl::tuple <int, foo> c {0, 1};

        // are the types correct?
        static_assert (
            std::is_same <decltype (stl::get <0> (a)), foo &>::value,
            "first type is incorrect"
        );
        static_assert (
            std::is_same <decltype (stl::get <1> (a)), foo &>::value,
            "second type is incorrect"
        );
        static_assert (
            std::is_same <decltype (stl::get <0> (b)), foo &>::value,
            "first type is incorrect"
        );
        static_assert (
            std::is_same <decltype (stl::get <1> (b)), int &>::value,
            "second type is incorrect"
        );
        static_assert (
            std::is_same <decltype (stl::get <1> (c)), foo &>::value,
            "first type is incorrect"
        );
        static_assert (
            std::is_same <decltype (stl::get <0> (c)), int &>::value,
            "second type is incorrect"
        );
    }

    /*
     * Can we define tuples with a single element type taking an allocators argument?
     */
    {
        using tuple_allocator = std::allocator <int>;

        // default construction
        stl::tuple <std::vector <int, tuple_allocator>> a {};

        // is the tuple element empty?
        assert (stl::get <0> (a).empty ());

        // uses-allocator default construction
        stl::tuple <std::vector <int, tuple_allocator>> b {
            std::allocator_arg_t {}, tuple_allocator {}
        };

        // is the tuple element empty?
        assert (stl::get <0> (b).empty ());

        // uses-allocator element construction
        stl::tuple <std::vector <int, tuple_allocator>> c {
            std::allocator_arg_t {}, tuple_allocator {}, {1, 2, 3}
        };

        // copy uses-allocator construction
        std::vector <int, tuple_allocator> d_vec {{1, 2, 3}};
        stl::tuple <std::vector <int, tuple_allocator>> d {
            std::allocator_arg_t {}, tuple_allocator {}, d_vec
        };

        // move uses-allocator construction
        std::vector <int, tuple_allocator> e_vec {{1, 2, 3}};
        stl::tuple <std::vector <int, tuple_allocator>> e {
            std::allocator_arg_t {}, tuple_allocator {}, stl::move (e_vec)
        };

        // are the sizes/values correct/the same?
        assert (stl::get <0> (c).size () == 3);
        assert (stl::get <0> (d).size () == 3);
        assert (stl::get <0> (e).size () == 3);

        assert (stl::get <0> (c) [0] == 1);
        assert (stl::get <0> (c) [1] == 2);
        assert (stl::get <0> (c) [2] == 3);
        assert (stl::get <0> (d) [0] == 1);
        assert (stl::get <0> (d) [1] == 2);
        assert (stl::get <0> (d) [2] == 3);
        assert (stl::get <0> (e) [0] == 1);
        assert (stl::get <0> (e) [1] == 2);
        assert (stl::get <0> (e) [2] == 3);
    }

    /*
     * Can we define tuples with multiple element types, only one of which
     * takes an allocator as an argument?
     */
    {
        using tuple_allocator = std::allocator <int>;

        // default construction
        stl::tuple <std::vector <int, tuple_allocator>, double> a {};

        // is the tuple element empty?
        assert (stl::get <0> (a).empty ());
        assert (stl::get <1> (a) == 0.0);

        // uses-allocator default construction
        stl::tuple <std::vector <int, tuple_allocator>, double> b {
            std::allocator_arg_t {}, tuple_allocator {}
        };

        // is the tuple element empty?
        assert (stl::get <0> (b).empty ());
        assert (stl::get <1> (b) == 0.0);

        // uses-allocator element construction
        stl::tuple <std::vector <int, tuple_allocator>, double> c {
            std::allocator_arg_t {}, tuple_allocator {}, {1, 2, 3}, 1.0
        };

        // copy uses-allocator construction
        std::vector <int, tuple_allocator> d_vec {{1, 2, 3}};
        stl::tuple <std::vector <int, tuple_allocator>, double> d {
            std::allocator_arg_t {}, tuple_allocator {}, d_vec, 2.0
        };

        // move uses-allocator construction
        std::vector <int, tuple_allocator> e_vec {{1, 2, 3}};
        stl::tuple <std::vector <int, tuple_allocator>, double> e {
            std::allocator_arg_t {}, tuple_allocator {}, stl::move (e_vec), 3.0
        };

        // are the sizes/values correct/the same?
        assert (stl::get <0> (c).size () == 3);
        assert (stl::get <0> (d).size () == 3);
        assert (stl::get <0> (e).size () == 3);

        assert (stl::get <0> (c) [0] == 1);
        assert (stl::get <0> (c) [1] == 2);
        assert (stl::get <0> (c) [2] == 3);
        assert (stl::get <1> (c) == 1.0);

        assert (stl::get <0> (d) [0] == 1);
        assert (stl::get <0> (d) [1] == 2);
        assert (stl::get <0> (d) [2] == 3);
        assert (stl::get <1> (d) == 2.0);

        assert (stl::get <0> (e) [0] == 1);
        assert (stl::get <0> (e) [1] == 2);
        assert (stl::get <0> (e) [2] == 3);
        assert (stl::get <1> (e) == 3.0);
    }

    /* Can we create tuples of references? */
    {
        std::vector <int> vec {{1, 2, 3}};
        stl::tuple <std::vector <int> &> t {vec};

        /* is the size right? */
        assert (stl::get <0> (t).size () == 3);

        /* can we modify vec? */
        stl::get <0> (t).push_back (4);
        assert (vec.size () == 4);
        assert (vec [3] == 4);
    }

    /* do tie and make_tuple work as expected? */
    {
        struct foo { int x; };

        int a;
        double b;
        foo c;

        stl::tie (a, b, c) = [] (void) {
            return stl::make_tuple (1, 2.0, foo {3});
        } ();

        assert (a == 1);
        assert (b == 2.0);
        assert (c.x == 3);
    }

    /* does tuple_cat work as expected? */
    {
        stl::tuple <char, unsigned char>   a {-1, 1};
        stl::tuple <short, unsigned short> b {-2, 2};
        stl::tuple <std::string, std::vector <int>> c {"test", {1, 2, 3}};

        auto empty = stl::tuple_cat ();
        auto a2    = stl::tuple_cat (a);
        auto ab    = stl::tuple_cat (a, b);
        auto ac    = stl::tuple_cat (a, c);
        auto abc   = stl::tuple_cat (a, b, c);

        /* are the types correct? */
        using ab_type = stl::tuple <char, unsigned char, short, unsigned short>;
        using ac_type = stl::tuple <
            char, unsigned char, std::string, std::vector <int>
        >;
        using abc_type = stl::tuple <
            char, unsigned char, short, unsigned short, std::string, std::vector <int>
        >;

        static_assert (
            std::is_same <decltype(empty), stl::tuple <>>::value,
            "expected empty tuple"
        );
        static_assert (
            std::is_same <decltype(a2), decltype (a)>::value,
            "wrong type for tuple_cat (1-arg) result"
        );
        static_assert (
            std::is_same <decltype (ab), ab_type>::value,
            "wrong type for tuple_cat (2-arg) result"
        );
        static_assert (
            std::is_same <decltype (ac), ac_type>::value,
            "wrong type for tuple_cat (2-arg) result"
        );
        static_assert (
            std::is_same <decltype (abc), abc_type>::value,
            "wrong type for tuple_cat (3-arg) result"
        );

        /* are the values correct? */
        assert (stl::get <0> (a2) == -1);
        assert (stl::get <1> (a2) == 1);

        assert (stl::get <0> (ab) == -1);
        assert (stl::get <1> (ab) == 1);
        assert (stl::get <2> (ab) == -2);
        assert (stl::get <3> (ab) == 2);

        assert (stl::get <0> (ac) == -1);
        assert (stl::get <1> (ac) == 1);
        assert (stl::get <2> (ac) == "test");
        assert (stl::get <3> (ac) [0] == 1);
        assert (stl::get <3> (ac) [1] == 2);
        assert (stl::get <3> (ac) [2] == 3);

        assert (stl::get <0> (abc) == -1);
        assert (stl::get <1> (abc) == 1);
        assert (stl::get <2> (abc) == -2);
        assert (stl::get <3> (abc) == 2);
        assert (stl::get <4> (abc) == "test");
        assert (stl::get <5> (abc) [0] == 1);
        assert (stl::get <5> (abc) [1] == 2);
        assert (stl::get <5> (abc) [2] == 3);
    }
}
