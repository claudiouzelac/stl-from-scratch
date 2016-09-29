base := .

test_dir     := $(base)/test
build_dir    := $(test_dir)/bin

test_sources := $(wildcard $(test_dir)/**/*.cpp)
test_executables := $(patsubst $(test_dir)/%, $(build_dir)/%, $(basename $(test_sources)))

cxx      := $(CXX)
cxx_std  := c++11

ifeq ($(build), debug)
cxx_gflg := -g
cxx_oflg := -O0
cxx_dflg := -DDEBUG
cxx_vflg := --verbose
else ifeq ($(build), opt1)
cxx_gflg := -g0
cxx_oflg := -O1
cxx_dflg := -DNDEBUG
else ifeq ($(build), opt2)
cxx_gflg := -g0
cxx_oflg := -O2
cxx_dflg := -DNDEBUG
else ifeq ($(build), opt3)
cxx_gflg := -g0
cxx_oflg := -O3
cxx_dflg := -DNDEBUG
else ifeq ($(build), sanitize_debug)
cxx_gflg := -g
cxx_oflg := -O0
cxx_dflg := -DDEBUG
cxx_vflg := --verbose
cxx_fflg := -fsanitize=address -fsanitize=undefined
else ifeq ($(build), sanitize_opt1)
cxx_gflg := -g0
cxx_oflg := -O1
cxx_dflg := -DNDEBUG
cxx_fflg := -fsanitize=address -fsanitize=undefined
else ifeq ($(build), sanitize_opt2)
cxx_gflg := -g0
cxx_oflg := -O2
cxx_dflg := -DNDEBUG
cxx_fflg := -fsanitize=address -fsanitize=undefined
else ifeq ($(build), sanitize_opt3)
cxx_gflg := -g0
cxx_oflg := -O3
cxx_dflg := -DNDEBUG
cxx_fflg := -fsanitize=address -fsanitize=undefined
endif

cxx_iflg := -I. -I$(test_dir)
cxx_wflg := -Wall -Wextra -Wmissing-braces -Wmissing-include-dirs \
	-Wsequence-point -Wswitch-default -Wswitch-bool -Wunused-local-typedefs \
	-Wunused-result -Wnarrowing -Wshadow -Wpointer-arith -Wcast-qual \
	-Wcast-align -Wwrite-strings -Wsign-conversion -Wpacked \
	-Wredundant-decls -Winline -Wvla

ifeq ($(findstring clang++, $(cxx)), clang++)
cxx_slflg := -stdlib=libc++
else ifeq ($(findstring g++, $(cxx)), g++)
cxx_lflg := -lstdc++
endif

cxx_flgs := $(cxx_vflg) -std=$(cxx_std) $(cxx_slflg) $(cxx_gflg) $(cxx_oflg) \
	$(cxx_fflg) $(cxx_dflg) $(cxx_iflg) $(cxx_wflg) $(OPTFLAG)

.PHONY: all test clean

all: $(test_executables)

test: $(test_executables)

clean:
	@rm -rf $(build_dir)

$(build_dir)/%: $(test_dir)/%.cpp
	@mkdir -p $(dir $@)
	$(cxx) $(cxx_flgs) $< -o $@ $(cxx_lflg)
