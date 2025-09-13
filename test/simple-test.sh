#!/bin/bash
# Simple, reliable test suite for Smelter core functionality
# Focuses on essential features that must work for development confidence

set -e

BINARY="./smt"
PASSED=0
FAILED=0
TOTAL=0

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
BLUE='\033[0;34m'
NC='\033[0m'

log_test() { echo -e "${BLUE}[TEST]${NC} $1"; ((TOTAL++)); }
log_pass() { echo -e "${GREEN}[PASS]${NC} $1"; ((PASSED++)); }
log_fail() { echo -e "${RED}[FAIL]${NC} $1"; ((FAILED++)); }

test_command() {
    local name="$1"
    local cmd="$2"
    local expected="$3"
    
    log_test "$name"
    
    if $BINARY $cmd 2>&1 | grep -q "$expected"; then
        log_pass "$name"
    else
        log_fail "$name - Expected '$expected' not found"
        echo "  Output was: $($BINARY $cmd 2>&1 | head -2)"
    fi
}

test_eval() {
    local name="$1"
    local expr="$2"
    local expected="$3"
    
    log_test "$name"
    
    local output
    output=$($BINARY eval "$expr" 2>&1)
    if echo "$output" | grep -q "$expected"; then
        log_pass "$name"
    else
        log_fail "$name - Expected '$expected' not found"
        echo "  Output was: $output"
    fi
}

echo "=========================================="
echo "    Smelter Simple Test Suite"
echo "    Core Functionality Verification"
echo "=========================================="
echo

# Verify binary exists
if [[ ! -f "$BINARY" ]]; then
    echo "Error: Binary $BINARY not found. Run 'make build' first."
    exit 1
fi

echo "Testing basic CLI functionality..."

# These work due to SBCL runtime handling (documented limitation)
test_command "version_shows_sbcl" "--version" "SBCL"
test_command "help_shows_usage" "--help" "Usage"

# This should show Smelter help (core fix verification)
log_test "default_shows_smelter_help"
if $BINARY 2>&1 | grep -q "Smelter.*Industrial-strength"; then
    log_pass "default_shows_smelter_help"
else
    log_fail "default_shows_smelter_help - Expected Smelter help"
fi

# Core evaluation functionality
echo
echo "Testing expression evaluation..."

test_eval "basic_number" "5" "5"
test_eval "basic_string" "\"hello\"" "hello"

# I/O library functionality  
echo
echo "Testing I/O library..."

test_eval "io_println" "(smelter.stdlib.io:io-println \"Test\")" "Test"
test_eval "io_print" "(smelter.stdlib.io:io-print \"NoNewline\")" "NoNewline"

# Error handling
echo
echo "Testing error handling..."

log_test "unknown_command_error"
if $BINARY unknown-cmd 2>&1 | grep -q "Unknown command"; then
    log_pass "unknown_command_error"
else
    log_fail "unknown_command_error - Expected 'Unknown command' message"
fi

# Summary
echo
echo "=========================================="
echo "Test Results: $PASSED/$TOTAL passed, $FAILED failed"
echo "=========================================="

if [[ $FAILED -eq 0 ]]; then
    echo -e "${GREEN}✅ All tests passed! Smelter core functionality is reliable.${NC}"
    exit 0
else
    echo -e "${RED}❌ Some tests failed. Core functionality needs attention.${NC}"
    exit 1
fi