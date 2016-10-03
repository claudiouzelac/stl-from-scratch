//
// Defines a helper type ignore_type used to select tuple lanes to ignore
// when using stl::tie ().
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

#ifndef STL_FROM_SCRATCH_BITS_IGNORE_TYPE_HEADER
#define STL_FROM_SCRATCH_BITS_IGNORE_TYPE_HEADER

namespace stl
{
namespace bits
{
    /* 
     * As per the spec this is a type such that any value can be assigned to it
     * with no effect. There is a single const object `ignore` of this type that
     * is to be declared in the <tuple> header.
     */
    class ignore_type
    {
    public:
        ignore_type (void) noexcept {}
        ~ignore_type (void) noexcept {}

        /*
         * Construction from any type; it performs nothing and does not modify
         * its argument in any way.
         */
        template <typename T>
        ignore_type (T &&) noexcept {}

        /* default all the special constructors */
        ignore_type (ignore_type &&) noexcept      = default;
        ignore_type (ignore_type const &) noexcept = default;

        /* 
         * The required assignment operator; it performs nothing and does not
         * modify its argument in any way.
         */
        template <typename T>
        ignore_type const & operator= (T &&) const noexcept
        {
            return *this;
        }
    };

}   // namespace bits
}   // namespace stl

#endif  // #ifndef STL_FROM_SCRATCH_BITS_IGNORE_TYPE_HEADER
