//
// Defines helper classes for generating integer sequences.
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

#ifndef STL_FROM_SCRATCH_BITS_SEQUENCE_GENERATOR_HEADER
#define STL_FROM_SCRATCH_BITS_SEQUENCE_GENERATOR_HEADER

#include <cstddef>

namespace stl
{
    /* forward declaration of integer sequence */
    template <class T, T ... Is>
    class integer_sequence;

namespace bits
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
}   // namespace bits
}   // namespace stl

#endif  // #ifndef STL_FROM_SCRATCH_BITS_SEQUENCE_GENERATOR_HEADER
