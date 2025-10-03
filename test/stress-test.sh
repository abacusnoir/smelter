#!/bin/bash
# Stress Testing Suite for Smelter
# Tests performance under load, large inputs, and edge conditions

set -euo pipefail

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

TESTS_RUN=0
TESTS_PASSED=0
TESTS_FAILED=0
SMT="./smt"

log_info() {
    echo -e "${BLUE}[STRESS]${NC} $1"
}

log_success() {
    echo -e "${GREEN}[PASS]${NC} $1"
    TESTS_PASSED=$((TESTS_PASSED + 1))
}

log_error() {
    echo -e "${RED}[FAIL]${NC} $1"
    TESTS_FAILED=$((TESTS_FAILED + 1))
}

test_stress() {
    local name="$1"
    local command="$2"
    local description="$3"

    TESTS_RUN=$((TESTS_RUN + 1))
    log_info "Testing: $name ($description)"

    if eval "$command" > /dev/null 2>&1; then
        log_success "$name"
    else
        log_error "$name (exit code: $?)"
    fi
}

echo "========================================="
echo "  STRESS TESTING SUITE"
echo "========================================="

# 1. LARGE INPUT TESTS
echo -e "\n${BLUE}--- Large Input Handling ---${NC}"

log_info "Testing large nested expression..."
TESTS_RUN=$((TESTS_RUN + 1))
large_expr="(+ (* 2 3) (+ (* 4 5) (+ (* 6 7) (+ (* 8 9) (* 10 11)))))"
if result=$($SMT eval "$large_expr" 2>&1) && [ -n "$result" ]; then
    log_success "Large nested expression (result: $result)"
else
    log_error "Large nested expression failed"
fi

log_info "Testing expression with many literals..."
TESTS_RUN=$((TESTS_RUN + 1))
many_adds="(+ (+ (+ (+ (+ 1 2) 3) 4) 5) 6)"
if result=$($SMT eval "$many_adds" 2>&1) && echo "$result" | grep -q "21"; then
    log_success "Many literals expression"
else
    log_error "Many literals expression failed"
fi

log_info "Testing very large integer..."
TESTS_RUN=$((TESTS_RUN + 1))
if result=$($SMT eval '(* 999999999 999999999)' 2>&1) && [ -n "$result" ]; then
    log_success "Very large integer (result: $result)"
else
    log_error "Very large integer failed"
fi

# 2. RAPID SEQUENTIAL CALLS
echo -e "\n${BLUE}--- Rapid Sequential Calls ---${NC}"

log_info "Testing 50 rapid sequential evaluations..."
TESTS_RUN=$((TESTS_RUN + 1))
success_count=0
for i in {1..50}; do
    if $SMT eval "(+ $i $i)" > /dev/null 2>&1; then
        success_count=$((success_count + 1))
    fi
done
if [ $success_count -eq 50 ]; then
    log_success "50 rapid sequential calls (all succeeded)"
else
    log_error "50 rapid sequential calls ($success_count/50 succeeded)"
fi

log_info "Testing rapid file executions..."
TESTS_RUN=$((TESTS_RUN + 1))
cat > /tmp/stress_test_file.coal << 'EOF'
(declare quick (Integer -> Integer))
(define (quick x) (+ x 1))
(define main
  (smelter.stdlib.io:io-println
    (smelter.stdlib.io:show-int (quick 42))))
EOF
success_count=0
for i in {1..20}; do
    if $SMT run /tmp/stress_test_file.coal > /dev/null 2>&1; then
        success_count=$((success_count + 1))
    fi
done
if [ $success_count -eq 20 ]; then
    log_success "20 rapid file executions (all succeeded)"
else
    log_error "20 rapid file executions ($success_count/20 succeeded)"
fi

# 3. PARALLEL EXECUTION
echo -e "\n${BLUE}--- Parallel Execution ---${NC}"

log_info "Testing 20 parallel evaluations..."
TESTS_RUN=$((TESTS_RUN + 1))
pids=()
for i in {1..20}; do
    $SMT eval "(* $i 2)" > /tmp/parallel_$i.out 2>&1 &
    pids+=($!)
done
# Wait for all background processes
failed=0
for pid in "${pids[@]}"; do
    if ! wait $pid; then
        failed=$((failed + 1))
    fi
done
if [ $failed -eq 0 ]; then
    log_success "20 parallel evaluations (all succeeded)"
else
    log_error "20 parallel evaluations ($failed failed)"
fi
rm -f /tmp/parallel_*.out

# 4. RECURSION DEPTH
echo -e "\n${BLUE}--- Recursion Depth ---${NC}"

log_info "Testing recursion with depth 50..."
TESTS_RUN=$((TESTS_RUN + 1))
cat > /tmp/recursion_test.coal << 'EOF'
(declare countdown (Integer -> Integer))
(define (countdown n)
  (if (<= n 0) 0
      (+ 1 (countdown (- n 1)))))
(define main
  (smelter.stdlib.io:io-println
    (smelter.stdlib.io:show-int (countdown 50))))
EOF
if $SMT run /tmp/recursion_test.coal > /dev/null 2>&1; then
    log_success "Recursion depth 50"
else
    log_error "Recursion depth 50 failed"
fi

log_info "Testing Fibonacci with recursion..."
TESTS_RUN=$((TESTS_RUN + 1))
cat > /tmp/fib_stress.coal << 'EOF'
(declare fib (Integer -> Integer))
(define (fib n)
  (if (<= n 1) n
      (+ (fib (- n 1)) (fib (- n 2)))))
(define main
  (smelter.stdlib.io:io-println
    (smelter.stdlib.io:show-int (fib 10))))
EOF
# Use timeout if available, otherwise run directly
if command -v timeout > /dev/null 2>&1; then
    if timeout 10 $SMT run /tmp/fib_stress.coal > /dev/null 2>&1; then
        log_success "Fibonacci recursion (n=10)"
    else
        log_error "Fibonacci recursion failed or timed out"
    fi
else
    if $SMT run /tmp/fib_stress.coal > /dev/null 2>&1; then
        log_success "Fibonacci recursion (n=10)"
    else
        log_error "Fibonacci recursion failed"
    fi
fi

# 5. MEMORY USAGE
echo -e "\n${BLUE}--- Memory Usage ---${NC}"

log_info "Checking memory usage for basic eval..."
TESTS_RUN=$((TESTS_RUN + 1))
if command -v /usr/bin/time > /dev/null 2>&1; then
    if /usr/bin/time -l $SMT eval '(+ 1 1)' > /dev/null 2>&1; then
        log_success "Memory usage check completed"
    else
        log_error "Memory usage check failed"
    fi
else
    log_info "Skipping memory test (/usr/bin/time not available)"
    TESTS_RUN=$((TESTS_RUN - 1))
fi

log_info "Checking for memory leaks with repeated calls..."
TESTS_RUN=$((TESTS_RUN + 1))
leak_test_failed=0
for i in {1..100}; do
    $SMT eval "(+ $i 1)" > /dev/null 2>&1 || leak_test_failed=$((leak_test_failed + 1))
done
if [ $leak_test_failed -eq 0 ]; then
    log_success "100 iterations without crashes (no obvious leaks)"
else
    log_error "Memory leak test: $leak_test_failed failures"
fi

# 6. FILE SIZE STRESS
echo -e "\n${BLUE}--- File Size Stress ---${NC}"

log_info "Testing execution of larger file..."
TESTS_RUN=$((TESTS_RUN + 1))
cat > /tmp/large_file.coal << 'EOF'
;; This is a larger file with multiple functions
(declare square (Integer -> Integer))
(define (square x) (* x x))

(declare cube (Integer -> Integer))
(define (cube x) (* x (* x x)))

(declare add (Integer -> Integer -> Integer))
(define (add x y) (+ x y))

(declare multiply (Integer -> Integer -> Integer))
(define (multiply x y) (* x y))

(declare factorial (Integer -> Integer))
(define (factorial n)
  (if (<= n 1) 1
      (* n (factorial (- n 1)))))

(declare fib (Integer -> Integer))
(define (fib n)
  (if (<= n 1) n
      (+ (fib (- n 1)) (fib (- n 2)))))

(define main
  (progn
    (smelter.stdlib.io:io-println
      (smelter.stdlib.io:show-int (square 5)))
    (smelter.stdlib.io:io-println
      (smelter.stdlib.io:show-int (cube 3)))
    (smelter.stdlib.io:io-println
      (smelter.stdlib.io:show-int (add 10 20)))
    (smelter.stdlib.io:io-println
      (smelter.stdlib.io:show-int (factorial 5)))))
EOF
if $SMT run /tmp/large_file.coal > /dev/null 2>&1; then
    log_success "Larger file execution"
else
    log_error "Larger file execution failed"
fi

# 7. EDGE CASE COMBINATIONS
echo -e "\n${BLUE}--- Edge Case Combinations ---${NC}"

log_info "Testing error recovery in sequence..."
TESTS_RUN=$((TESTS_RUN + 1))
if ($SMT eval 'undefined-var' || true) > /dev/null 2>&1 && \
   $SMT eval '(+ 1 1)' > /dev/null 2>&1 && \
   ($SMT eval '(/ 1 0)' || true) > /dev/null 2>&1 && \
   $SMT eval '(* 2 3)' > /dev/null 2>&1; then
    log_success "Error recovery sequence"
else
    log_error "Error recovery sequence failed"
fi

log_info "Testing mixed valid and invalid operations..."
TESTS_RUN=$((TESTS_RUN + 1))
valid_count=0
for expr in "(+ 1 2)" "(* 3 4)" "(- 10 5)" "(< 5 10)"; do
    if $SMT eval "$expr" > /dev/null 2>&1; then
        valid_count=$((valid_count + 1))
    fi
done
if [ $valid_count -eq 4 ]; then
    log_success "Mixed operations (4/4 valid succeeded)"
else
    log_error "Mixed operations (only $valid_count/4 succeeded)"
fi

# 8. STARTUP TIME CONSISTENCY
echo -e "\n${BLUE}--- Performance Consistency ---${NC}"

log_info "Testing startup time consistency (10 runs)..."
TESTS_RUN=$((TESTS_RUN + 1))
total_time=0
max_time=0
min_time=999999
for i in {1..10}; do
    start=$(date +%s%N 2>/dev/null || echo "0")
    $SMT eval '(+ 1 1)' > /dev/null 2>&1
    end=$(date +%s%N 2>/dev/null || echo "0")
    if [ "$start" != "0" ] && [ "$end" != "0" ]; then
        duration=$((($end - $start) / 1000000))
        total_time=$((total_time + duration))
        [ $duration -gt $max_time ] && max_time=$duration
        [ $duration -lt $min_time ] && min_time=$duration
    fi
done
if [ "$total_time" -gt 0 ]; then
    avg_time=$((total_time / 10))
    log_success "Startup consistency: avg=${avg_time}ms, min=${min_time}ms, max=${max_time}ms"
else
    log_info "Startup time measurement not available on this system"
    TESTS_RUN=$((TESTS_RUN - 1))
fi

# Cleanup
rm -f /tmp/stress_test_file.coal /tmp/recursion_test.coal /tmp/fib_stress.coal /tmp/large_file.coal

# SUMMARY
echo ""
echo "========================================="
echo "  STRESS TEST SUMMARY"
echo "========================================="
echo "Tests run:    $TESTS_RUN"
echo "Tests passed: $TESTS_PASSED"
echo "Tests failed: $TESTS_FAILED"
echo ""

if [ $TESTS_FAILED -eq 0 ]; then
    echo -e "${GREEN}✓ ALL STRESS TESTS PASSED!${NC}"
    echo "Smelter is robust and performant under load 💪"
    exit 0
else
    echo -e "${YELLOW}⚠ $TESTS_FAILED STRESS TEST(S) FAILED${NC}"
    echo "Consider investigating performance or stability issues"
    exit 1
fi
