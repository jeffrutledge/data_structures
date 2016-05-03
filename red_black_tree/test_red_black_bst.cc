// -*- C++ -*-
/**
 * \file test_red_black_bst.cc
 * \author Jeffrey Rutledge jrutledge@hmc.edu
 *
 * \brief Tests for correctness of RedBlackBst
 */

#include "red_black_bst.h"

#include <gtest/gtest.h>
#include <cinttypes>
#include <cmath>
#include <cstddef>
#include <random>
#include <set>
#include <sstream>
#include <string>

using std::string;

//--------------------------------------------------
//           TEST HELPER FUNCTIONS
//--------------------------------------------------
double CalculateAverageNodeDepthForPerfectTree(const size_t numberOfNodes) {
  size_t numberOfNodesLeft = numberOfNodes;
  size_t depthTotal = 0;
  size_t currentDepth = 0;
  size_t twoToTheCurrentDepth = 1;
  while (numberOfNodesLeft > 0) {
    const size_t numberOfNodesAtThisDepth =
        std::min(numberOfNodesLeft, twoToTheCurrentDepth);
    depthTotal += currentDepth * numberOfNodesAtThisDepth;
    numberOfNodesLeft -= numberOfNodesAtThisDepth;
    twoToTheCurrentDepth *= 2;
    ++currentDepth;
  }
  return depthTotal / double(numberOfNodes);
}

std::default_random_engine random_engine;

/**
 * \brief Inserts none duplicate random numbers from 0 to amount_to_insert
 *
 * \return The a set of the numbers inserted, (only the unique inserts)
 */
std::set<size_t> InsertRandomNums(RedBlackBst<size_t>* tree,
                                         const size_t amount_to_insert) {
  std::set<size_t> inserted_nums;

  for (size_t i = 0; i < amount_to_insert; ++i) {
    std::uniform_int_distribution<size_t> randomDistribution(0,
                                                             amount_to_insert);
    size_t num_to_insert = randomDistribution(random_engine);
    if (inserted_nums.find(num_to_insert) == inserted_nums.end()) {
      EXPECT_TRUE(tree->Insert(num_to_insert));
    } else {
      EXPECT_FALSE(tree->Insert(num_to_insert));
    }
    inserted_nums.insert(num_to_insert);
  }
  return inserted_nums;
}

//--------------------------------------------------
//           TESTS
//--------------------------------------------------

// Force C++ to compile the TreeSet Interface
template class RedBlackBst<size_t>;

TEST(ConstructorTests, Defualt) {
  RedBlackBst<size_t> test;

  EXPECT_EQ(0, test.size());
  EXPECT_EQ(-1, test.CalculateHeight());
}

TEST(FindTests, EmptyTable) {
  RedBlackBst<size_t> test;

  EXPECT_FALSE(test.Find(0));
}

TEST(InsertTests, SmallTree) {
  RedBlackBst<size_t> test;

  EXPECT_EQ(0, test.size());
  EXPECT_EQ(-1, test.CalculateHeight());

  EXPECT_TRUE(test.Insert(0));

  EXPECT_EQ(1, test.size());
  EXPECT_EQ(0, test.CalculateHeight());
  EXPECT_EQ(0, test.CalculateAverageNodeDepth());
}

TEST(SizeTests, DuplicateInsert) {
  RedBlackBst<size_t> test;

  EXPECT_EQ(0, test.size());
  EXPECT_EQ(-1, test.CalculateHeight());

  EXPECT_TRUE(test.Insert(0));
  EXPECT_EQ(1, test.size());
  EXPECT_EQ(0, test.CalculateHeight());
  EXPECT_EQ(0, test.CalculateAverageNodeDepth());

  // Insert Duplicate and expect no change in the table
  EXPECT_FALSE(test.Insert(0));
  EXPECT_EQ(1, test.size());
  EXPECT_EQ(0, test.CalculateHeight());
  EXPECT_EQ(0, test.CalculateAverageNodeDepth());
}

TEST(FindTests, LargeTable) {
  RedBlackBst<size_t> test;
  const size_t SIZE_OF_TEST = 1000;

  EXPECT_EQ(0, test.size());
  EXPECT_EQ(-1, test.CalculateHeight());

  std::set<size_t> inserted_nums = InsertRandomNums(&test, SIZE_OF_TEST);

  EXPECT_EQ(inserted_nums.size(), test.size());
  EXPECT_GE(test.CalculateAverageNodeDepth(),
            CalculateAverageNodeDepthForPerfectTree(test.size()));

  // Test succesfull find on every unique number inserted
  for (const size_t num : inserted_nums) {
    EXPECT_TRUE(test.Find(num));
  }

  // Test for unsuccesful find for nums not inserted
  for (size_t i = SIZE_OF_TEST + 1; i < 2 * SIZE_OF_TEST; ++i) {
    EXPECT_FALSE(test.Find(i));
  }
}

TEST(InsertTests, LargeTableWithDups) {
  RedBlackBst<size_t> test;
  const size_t SIZE_OF_TEST = 1000;

  EXPECT_EQ(0, test.size());
  EXPECT_EQ(-1, test.CalculateHeight());

  std::set<size_t> inserted_nums = InsertRandomNums(&test, SIZE_OF_TEST);

  EXPECT_EQ(inserted_nums.size(), test.size());
  EXPECT_GE(test.CalculateAverageNodeDepth(),
            CalculateAverageNodeDepthForPerfectTree(test.size()));

  // Insert same nums again
  for (const size_t num : inserted_nums) {
    test.Insert(num);
  }

  // Expect nothing to change
  EXPECT_EQ(inserted_nums.size(), test.size());
  EXPECT_GE(test.CalculateAverageNodeDepth(),
            CalculateAverageNodeDepthForPerfectTree(test.size()));
}

//--------------------------------------------------
//           TESTING TREE PERFORMANCE
//--------------------------------------------------

// #define TEST_PERFORMANCE
#ifdef TEST_PERFORMANCE
TEST(EvaluatePerformance, CompareToPerfectTree) {
  RedBlackBst<size_t> test;
  const size_t SIZE_OF_TEST = 1000;

  std::cout << "size, "
            << "test, "
            << "perfect, "
            << "perc diff" << std::endl;
  for (size_t i = 0; i < SIZE_OF_TEST; ++i) {
    test.Insert(i);
    if ((i % 100) == 0) {
      const double testAvgDepth = test.CalculateAverageNodeDepth();
      const double perfectAvgDepth =
          CalculateAverageNodeDepthForPerfectTree(test.size());
      const double percentDiff = testAvgDepth / perfectAvgDepth;
      std::cout << i << ", " << testAvgDepth << ", " << perfectAvgDepth << ", "
                << percentDiff << std::endl;
    }
  }
}
#endif

//--------------------------------------------------
//           RUNNING THE TESTS
//--------------------------------------------------

// Called if the test runs too long.
static void timeout_handler(int) {
  // We go super-low-level here, because we can't trust anything in
  // the C/C++ library to really be working right.
  write(STDERR_FILENO, "Timeout occurred!\n", 18);
  abort();
}

/// Run tests
int main(int argc, char** argv) {
  // Initialize random num generator
  std::random_device random_seeder;
  random_engine.seed(random_seeder());
  
  // Initalize testing environment
  ::testing::InitGoogleTest(&argc, argv);

  const size_t MAX_RUNTIME = 30;  // Timeout, in seconds
  if (not::testing::GTEST_FLAG(break_on_failure)) {
    signal(SIGALRM, timeout_handler);  // What to call when timer expires
    alarm(MAX_RUNTIME);                // set timer at MAX_RUNTIME seconds
  }
  return RUN_ALL_TESTS();
}
