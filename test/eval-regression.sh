#!/bin/bash
# Regression tests for eval mode pattern matching
# Ensures the pattern matching fix never breaks again

set -e  # Exit on first failure

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

echo -e "${BLUE}=== Eval Mode Pattern Matching Regression Tests ===${NC}"
echo

# Helper function to test expressions
test_eval() {
    local test_name="$1"
    local expression="$2"
    local expected="$3"

    echo -e "${BLUE}Testing:${NC} $test_name"
    echo -e "  Expression: $expression"

    # Run the eval command and capture output
    local result
    result=$(./smt eval "$expression" 2>&1 | tail -1)

    if [ "$result" = "$expected" ]; then
        echo -e "  ${GREEN}✅ PASS${NC} - Output: $result"
    else
        echo -e "  ${RED}❌ FAIL${NC} - Expected: '$expected', Got: '$result'"
        exit 1
    fi
    echo
}

# Ensure binary exists
if [ ! -f "./smt" ]; then
    echo -e "${RED}Error: smt binary not found. Run 'make build' first.${NC}"
    exit 1
fi

# Test Result type matching
test_eval "Result Ok matching" '(match (Ok 42) ((Ok x) x) ((Err _) 0))' "42"
test_eval "Result Err matching" '(match (Err "fail") ((Ok x) x) ((Err _) -1))' "-1"

# Test Optional type matching
test_eval "Optional Some matching" '(match (Some 10) ((Some x) (* x 2)) ((None) 0))' "20"
test_eval "Optional None matching" '(match None ((Some x) x) ((None) -1))' "-1"

# Test arithmetic with pattern matching
test_eval "Result with arithmetic" '(match (Ok 5) ((Ok x) (+ x 10)) ((Err _) 0))' "15"
test_eval "Optional with arithmetic" '(match (Some 3) ((Some x) (* x x)) ((None) 0))' "9"

# Test nested pattern matching
test_eval "Nested Some(Ok) matching" '(match (Some (Ok 5)) ((Some (Ok x)) x) ((Some (Err _)) -1) ((None) 0))' "5"
test_eval "Nested Some(Err) matching" '(match (Some (Err "fail")) ((Some (Ok x)) x) ((Some (Err _)) -1) ((None) 0))' "-1"
test_eval "Nested None matching" '(match None ((Some (Ok x)) x) ((Some (Err _)) -1) ((None) 0))' "0"

# Test basic arithmetic still works
test_eval "Simple addition" '(+ 1 2)' "3"
test_eval "Simple multiplication" '(* 6 7)' "42"
test_eval "Simple subtraction" '(- 10 3)' "7"

echo -e "${GREEN}🎉 All regression tests passed!${NC}"
echo -e "${GREEN}Pattern matching in eval mode is working correctly.${NC}"