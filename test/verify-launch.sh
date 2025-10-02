#!/bin/bash
# Smelter Launch Verification Script
# Verifies all critical functionality before HN launch

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Counters
TESTS_RUN=0
TESTS_PASSED=0
TESTS_FAILED=0

# Helper functions
log_info() {
    echo -e "${BLUE}[INFO]${NC} $1"
}

log_success() {
    echo -e "${GREEN}[PASS]${NC} $1"
    ((TESTS_PASSED++))
}

log_error() {
    echo -e "${RED}[FAIL]${NC} $1"
    ((TESTS_FAILED++))
}

run_test() {
    local test_name="$1"
    local test_command="$2"
    local expected_pattern="$3"

    ((TESTS_RUN++))
    log_info "Test: $test_name"

    if output=$(eval "$test_command" 2>&1); then
        if echo "$output" | grep -q "$expected_pattern"; then
            log_success "$test_name"
            return 0
        else
            log_error "$test_name - Output doesn't match '$expected_pattern'"
            echo "Got: $output"
            return 1
        fi
    else
        log_error "$test_name - Command failed"
        echo "Error: $output"
        return 1
    fi
}

echo "========================================"
echo "  Smelter Launch Verification"
echo "========================================"
echo

# Test 1: Basic Commands
log_info "=== Basic Commands ==="
run_test "Version command" "./smt --version" "Smelter"
run_test "Help command" "./smt --help" "Usage"
run_test "Simple eval" "./smt eval '(+ 2 3)'" "5"

echo

# Test 2: Launch Examples
log_info "=== Launch Examples ==="
run_test "Hello World" "./smt run examples/launch/hello.coal" "Hello from Smelter"
run_test "Fibonacci" "./smt run examples/launch/fibonacci.coal" "fib(10) = 55"
run_test "Factorial" "./smt run examples/launch/factorial.coal" "5! = 120"
run_test "FizzBuzz" "./smt run examples/launch/fizzbuzz.coal" "FizzBuzz"
run_test "Double/Triple" "./smt run examples/launch/temperature.coal" "42"

echo

# Test 3: Show Functions
log_info "=== Show Functions ==="
cat > /tmp/test-show-verify.coal << 'EOF'
(define main
  (progn
    (smelter.stdlib.io:io-print "The answer is ")
    (smelter.stdlib.io:io-println (smelter.stdlib.io:show-int 42))))
EOF

run_test "Show integer" "./smt run /tmp/test-show-verify.coal" "The answer is 42"

echo

# Test 4: Performance
log_info "=== Performance Checks ==="

# Binary size
size=$(ls -lh smt | awk '{print $5}')
size_mb=${size%M}
log_info "Binary size: $size"
if [ -n "$size_mb" ] && [ "$size_mb" -lt "60" ] 2>/dev/null; then
    log_success "Binary size acceptable (<60MB)"
else
    log_error "Binary size too large or invalid: $size"
fi
((TESTS_RUN++))

# Startup time (rough measurement)
start=$(date +%s%N)
./smt eval '(+ 1 1)' > /dev/null 2>&1
end=$(date +%s%N)
duration_ms=$(( (end - start) / 1000000 ))
log_info "Startup time: ${duration_ms}ms"
if [ "$duration_ms" -lt "200" ]; then
    log_success "Startup time acceptable (<200ms)"
else
    log_error "Startup time too slow: ${duration_ms}ms"
fi
((TESTS_RUN++))

echo

# Summary
echo "========================================"
echo "  Verification Results"
echo "========================================"
echo "Tests run:    $TESTS_RUN"
echo "Tests passed: $TESTS_PASSED"
echo "Tests failed: $TESTS_FAILED"
echo

if [ "$TESTS_FAILED" -eq 0 ]; then
    log_success "ALL TESTS PASSED! Ready for launch 🚀"
    exit 0
else
    log_error "$TESTS_FAILED test(s) failed"
    exit 1
fi
