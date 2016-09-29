//
// The testing harness for our STL implementation
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

#ifndef STL_FROM_SCRATCH_TEST_HARNESS_HPP
#define STL_FROM_SCRATCH_TEST_HARNESS_HPP

#include <algorithm>
#include <cstddef>
#include <functional>
#include <iostream>
#include <memory>
#include <random>
#include <string>
#include <sstream>
#include <tuple>
#include <type_traits>
#include <utility>
#include <vector>


namespace test
{
    enum class result
    {
        pass,
        fail
    };

    class data
    {
    protected:
        data (void) noexcept = default;
        virtual ~data (void) noexcept = default;

    public:
        virtual std::string description (void) = 0;
    };

    class generator
    {
    protected:
        generator (void) noexcept = default;
        virtual ~generator (void) noexcept = default;

    public:
        virtual bool requires_input (void) noexcept = 0;
        virtual std::shared_ptr <data> operator() (void) = 0;
        virtual std::shared_ptr <data> operator() (std::string const &) = 0;
    };

    class test
    {
    protected:
        test (void) noexcept = default;
        virtual ~test (void) noexcept = default;

    public:
        virtual bool is_repeatable (void) noexcept = 0;
        virtual std::size_t repeat_count (void) noexcept = 0;

        virtual result operator() (data &) noexcept = 0;
        virtual std::string name (void) = 0;
    };

    class harness
    {
        using case_type = std::tuple <
            std::shared_ptr <test>,
            std::shared_ptr <generator>,
            std::vector <std::string>
        >;

        std::vector <case_type> _cases;

    public:
        void emplace (case_type && c)
        {
            this->_cases.emplace_back (std::move (c));
        }

        std::pair <result, std::string> run (void)
        {
            std::stringstream report {};
            std::size_t failed_cases = 0;

            for (auto & c : _cases) {
                auto & test  = std::get <0> (c);
                auto & gen   = std::get <1> (c);
                auto & input = std::get <2> (c);

                if (test->is_repeatable ()) {
                    if (gen->requires_input ()) {
                        auto const count = input.size ();
                        for (std::size_t i = 0; i < count; ++i) {
                            auto data = gen->operator() (input [i]);
                            switch (test->operator() (*data)) {
                                case result::pass:
                                    break;
                                case result::fail:
                                    failed_cases += 1;
                                    report << "failed test ["
                                           << test->name ()
                                           << "] for case ["
                                           << i + 1 << "/" << count
                                           << "] ["
                                           << data->description ()
                                           << "]\n";
                                    break;
                            }
                        }
                    } else {
                        auto const count = test->repeat_count ();
                        for (std::size_t i = 0; i < count; ++i) {
                            auto data = gen->operator() (input [i]);
                            switch (test->operator() (*data)) {
                                case result::pass:
                                    break;
                                case result::fail:
                                    failed_cases += 1;
                                    report << "failed test ["
                                           << test->name ()
                                           << "] for case ["
                                           << i + 1 << "/" << count
                                           << "] ["
                                           << data->description ()
                                           << "]\n";
                                    break;
                            }
                        }
                    }
                } else {
                    if (gen->requires_input ()) {
                        auto data = gen->operator() ();
                        switch (test->operator() (*data)) {
                            case result::pass:
                                break;
                            case result::fail:
                                failed_cases += 1;
                                report << "failed test ["
                                       << test->name ()
                                       << "] for case [1/1] ["
                                       << data->description ()
                                       << "]\n";
                                break;
                        }
                    } else {
                        auto const & in = input.front ();
                        auto data = gen->operator() (in);
                        switch (test->operator() (*data)) {
                            case result::pass:
                                break;
                            case result::fail:
                                failed_cases += 1;
                                report << "failed test ["
                                       << test->name ()
                                       << "] for case [1/1] ["
                                       << data->description ()
                                       << "]\n";
                                break;
                        }
                    }
                }
            }

            return std::make_pair (
                failed_cases ? result::fail : result::pass, report.str ()
            );
        }
    };
}   // namespace test

#endif  // #ifndef STL_FROM_SCRATCH_TEST_HARNESS_HPP
