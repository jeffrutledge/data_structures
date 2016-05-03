# ----- Make Variables -----

CXXFLAGS = -g -std=c++11 -Wall -Wextra -pedantic
CXX = clang++

# ----- Google Test Setup -----

GTEST_DIR = ../googletest
GTEST_HEADERS = $(GTEST_DIR)/include/gtest/gtest*.h \
		$(GTEST_DIR)/include/gtest/internal/*.h
GTEST_SRCS_ = $(GTEST_DIR)/src/*.cc $(GTEST_DIR)/src/*.h $(GTEST_HEADERS)
CPPFLAGS = -isystem $(GTEST_DIR)/include

# ----- Assignment-specific targets ---

TARGETS = test_red_black_bst

all: $(TARGETS)

# ----- Google Test Rules -----

gtest-all.o: $(GTEST_SRCS_)
	$(CXX) $(CPPFLAGS) -I$(GTEST_DIR) $(CXXFLAGS) -c \
		-pthread $(GTEST_DIR)/src/gtest-all.cc

# ----- Assignment-specific rules -----

# Compile the tests
test_red_black_bst: test_red_black_bst.cc gtest-all.o red_black_bst.h
	$(CXX) $(CPPFLAGS) $(CXXFLAGS) $< -o $@ -pthread gtest-all.o

test: test_red_black_bst
	./test_red_black_bst

clean:
	rm -rf $(TARGETS) *.o documentation gtest.a gtest_main.a

doxygen:
	doxygen doxygen.config
