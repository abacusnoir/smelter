#!/bin/bash
# Comprehensive Test Suite for Smelter
# Ensures production-ready quality before HN launch

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

# Test function
test_case() {
    local name="$1"
    local command="$2"
    local expected="$3"

    TESTS_RUN=$((TESTS_RUN + 1))
    echo -n "Testing: $name... "

    output=$(eval "$command" 2>&1 || true)
    if echo "$output" | grep -q "$expected"; then
        echo -e "${GREEN}PASS${NC}"
        TESTS_PASSED=$((TESTS_PASSED + 1))
    else
        echo -e "${RED}FAIL${NC}"
        echo "  Expected: $expected"
        echo "  Got: $output"
        TESTS_FAILED=$((TESTS_FAILED + 1))
    fi
}

echo "========================================="
echo "  COMPREHENSIVE TEST SUITE"
echo "========================================="

# 1. BASIC FUNCTIONALITY TESTS
echo -e "\n${BLUE}--- Basic Functionality ---${NC}"
test_case "Version command" "$SMT --version" "Smelter"
test_case "Help command" "$SMT --help" "Usage:"
test_case "Simple arithmetic" "$SMT eval '(+ 2 3)'" "5"
test_case "Multiplication" "$SMT eval '(* 6 7)'" "42"
test_case "Subtraction" "$SMT eval '(- 10 3)'" "7"
test_case "Nested arithmetic" "$SMT eval '(+ (* 2 3) (* 4 5))'" "26"
test_case "Negative numbers" "$SMT eval '(+ -5 10)'" "5"
test_case "Multiple operations" "$SMT eval '(+ (+ 1 2) (+ 3 (+ 4 5)))'" "15"
test_case "String output" "$SMT eval '\"hello\"'" "hello"
test_case "Boolean True" "$SMT eval 'True'" "T"

# 2. EDGE CASES
echo -e "\n${BLUE}--- Edge Cases ---${NC}"
test_case "Empty input" "$SMT eval ''" "NIL"
test_case "Invalid syntax (unclosed)" "$SMT eval '(+ 2'" "end of file"
test_case "Division by zero" "$SMT eval '(/ 1 0)'" "INFINITY"
test_case "Undefined variable" "$SMT eval '(undefined-var)'" "unknown variable"
test_case "Undefined function" "$SMT eval '(undefined-func)'" "unknown variable"
test_case "Wrong arity" "$SMT eval '(+ 1)'" "1"
test_case "Mismatched types (string + int)" "$SMT eval '(+ \"hello\" 1)'" "Error"
test_case "Very large integer" "$SMT eval '(+ 999999999999 1)'" "1000000000000"
test_case "Zero arithmetic" "$SMT eval '(* 0 999)'" "0"
test_case "Negative result" "$SMT eval '(- 5 10)'" "-"
test_case "Comparison operators" "$SMT eval '(< 5 10)'" "T"
test_case "Equality" "$SMT eval '(== 5 5)'" "T"
test_case "Inequality" "$SMT eval '(== 5 6)'" "NIL"
test_case "Boolean and" "$SMT eval '(and True False)'" "NIL"
test_case "Boolean or" "$SMT eval '(or True False)'" "T"

# 3. PATTERN MATCHING TESTS
echo -e "\n${BLUE}--- Pattern Matching ---${NC}"
test_case "Simple Result Ok" "$SMT eval '(match (Ok 42) ((Ok x) x) ((Err _) -1))'" "42"
test_case "Simple Result Err" "$SMT eval '(match (Err \"fail\") ((Ok x) x) ((Err _) (- 0 1)))'" "-"
test_case "Optional Some" "$SMT eval '(match (Some 5) ((Some x) x) ((None) 0))'" "5"
test_case "Optional None" "$SMT eval '(match None ((Some x) x) ((None) 0))'" "0"
test_case "Result with arithmetic" "$SMT eval '(match (Ok 10) ((Ok x) (+ x 5)) ((Err _) 0))'" "15"
test_case "Optional with multiplication" "$SMT eval '(match (Some 7) ((Some x) (* x x)) ((None) 0))'" "49"
test_case "Nested Some(Ok)" "$SMT eval '(match (Some (Ok 5)) ((Some (Ok x)) x) ((Some (Err _)) -1) ((None) 0))'" "5"
test_case "Nested Some(Err)" "$SMT eval '(match (Some (Err \"x\")) ((Some (Ok x)) x) ((Some (Err _)) (- 0 1)) ((None) 0))'" "-"
test_case "Nested None" "$SMT eval '(match None ((Some (Ok x)) x) ((Some (Err _)) -1) ((None) 0))'" "0"
test_case "Pattern with wildcard" "$SMT eval '(match (Ok 100) ((Ok _) 999) ((Err _) 0))'" "999"

# 4. FILE OPERATIONS
echo -e "\n${BLUE}--- File Operations ---${NC}"

# Create test files
cat > /tmp/test_basic.coal << 'EOF'
(define main
  (smelter.stdlib.io:io-println "File test passed"))
EOF
test_case "Run basic file" "$SMT run /tmp/test_basic.coal" "File test passed"

cat > /tmp/test_arithmetic.coal << 'EOF'
(declare add (Integer -> Integer -> Integer))
(define (add x y) (+ x y))
(define main
  (smelter.stdlib.io:io-println
    (smelter.stdlib.io:show-int (add 10 32))))
EOF
test_case "Run arithmetic file" "$SMT run /tmp/test_arithmetic.coal" "42"

test_case "Nonexistent file" "$SMT run /nonexistent_file.coal" "not found"
test_case "Run without file arg" "$SMT run" "Usage:"

# Shebang test
cat > /tmp/test_shebang.coal << 'EOF'
#!/usr/bin/env smt run
(define main (smelter.stdlib.io:io-println "Shebang works"))
EOF
chmod +x /tmp/test_shebang.coal
export PATH="$PWD:$PATH"
test_case "Shebang execution" "/tmp/test_shebang.coal" "Shebang works"

# Test with syntax error in file
cat > /tmp/test_syntax_error.coal << 'EOF'
(define main (+ 1 2
EOF
test_case "File with syntax error" "$SMT run /tmp/test_syntax_error.coal" "end of file"

# Test with clean syntax
cat > /tmp/test_clean_syntax.coal << 'EOF'
(declare square (Integer -> Integer))
(define (square x) (* x x))
(define main
  (smelter.stdlib.io:io-println
    (smelter.stdlib.io:show-int (square 9))))
EOF
test_case "Clean syntax file" "$SMT run /tmp/test_clean_syntax.coal" "81"

# 5. CLEAN SYNTAX TESTS
echo -e "\n${BLUE}--- Clean Syntax ---${NC}"

cat > /tmp/test_function.coal << 'EOF'
(declare triple (Integer -> Integer))
(define (triple x) (* x 3))
(define main
  (smelter.stdlib.io:io-println
    (smelter.stdlib.io:show-int (triple 14))))
EOF
test_case "Function definition" "$SMT run /tmp/test_function.coal" "42"

cat > /tmp/test_conditional.coal << 'EOF'
(declare abs-value (Integer -> Integer))
(define (abs-value x)
  (if (< x 0) (- 0 x) x))
(define main
  (smelter.stdlib.io:io-println
    (smelter.stdlib.io:show-int (abs-value -42))))
EOF
test_case "Conditional logic" "$SMT run /tmp/test_conditional.coal" "42"

cat > /tmp/test_recursion.coal << 'EOF'
(declare factorial (Integer -> Integer))
(define (factorial n)
  (if (<= n 1) 1
      (* n (factorial (- n 1)))))
(define main
  (smelter.stdlib.io:io-println
    (smelter.stdlib.io:show-int (factorial 5))))
EOF
test_case "Recursive function" "$SMT run /tmp/test_recursion.coal" "120"

cat > /tmp/test_let_binding.coal << 'EOF'
(define main
  (let ((x 10) (y 32))
    (smelter.stdlib.io:io-println
      (smelter.stdlib.io:show-int (+ x y)))))
EOF
test_case "Let bindings" "$SMT run /tmp/test_let_binding.coal" "42"

cat > /tmp/test_progn.coal << 'EOF'
(define main
  (progn
    (smelter.stdlib.io:io-println "First line")
    (smelter.stdlib.io:io-println "Second line")))
EOF
test_case "Progn sequencing" "$SMT run /tmp/test_progn.coal" "First line"

# 6. PERFORMANCE TESTS
echo -e "\n${BLUE}--- Performance ---${NC}"

# Binary size
SIZE=$(stat -f%z "$SMT" 2>/dev/null || stat -c%s "$SMT")
SIZE_MB=$((SIZE / 1024 / 1024))
TESTS_RUN=$((TESTS_RUN + 1))
if [ "$SIZE_MB" -lt 60 ]; then
    echo -e "Testing: Binary size... ${GREEN}PASS${NC} (${SIZE_MB}MB < 60MB)"
    TESTS_PASSED=$((TESTS_PASSED + 1))
else
    echo -e "Testing: Binary size... ${RED}FAIL${NC} (${SIZE_MB}MB >= 60MB)"
    TESTS_FAILED=$((TESTS_FAILED + 1))
fi

# Startup time
START=$(date +%s%N 2>/dev/null || date +%s000000000)
$SMT eval '(+ 1 1)' > /dev/null 2>&1
END=$(date +%s%N 2>/dev/null || date +%s000000000)
STARTUP=$((($END - $START) / 1000000))
TESTS_RUN=$((TESTS_RUN + 1))
if [ "$STARTUP" -lt 200 ]; then
    echo -e "Testing: Startup time... ${GREEN}PASS${NC} (${STARTUP}ms < 200ms)"
    TESTS_PASSED=$((TESTS_PASSED + 1))
else
    echo -e "Testing: Startup time... ${YELLOW}WARN${NC} (${STARTUP}ms >= 200ms)"
    TESTS_FAILED=$((TESTS_FAILED + 1))
fi

# Quick evaluation
START=$(date +%s%N 2>/dev/null || date +%s000000000)
$SMT eval '(* 123 456)' > /dev/null 2>&1
END=$(date +%s%N 2>/dev/null || date +%s000000000)
EVAL_TIME=$((($END - $START) / 1000000))
TESTS_RUN=$((TESTS_RUN + 1))
if [ "$EVAL_TIME" -lt 500 ]; then
    echo -e "Testing: Eval speed... ${GREEN}PASS${NC} (${EVAL_TIME}ms < 500ms)"
    TESTS_PASSED=$((TESTS_PASSED + 1))
else
    echo -e "Testing: Eval speed... ${YELLOW}WARN${NC} (${EVAL_TIME}ms >= 500ms)"
    TESTS_FAILED=$((TESTS_FAILED + 1))
fi

# File execution speed
START=$(date +%s%N 2>/dev/null || date +%s000000000)
$SMT run /tmp/test_basic.coal > /dev/null 2>&1
END=$(date +%s%N 2>/dev/null || date +%s000000000)
FILE_TIME=$((($END - $START) / 1000000))
TESTS_RUN=$((TESTS_RUN + 1))
if [ "$FILE_TIME" -lt 1000 ]; then
    echo -e "Testing: File execution speed... ${GREEN}PASS${NC} (${FILE_TIME}ms < 1000ms)"
    TESTS_PASSED=$((TESTS_PASSED + 1))
else
    echo -e "Testing: File execution speed... ${YELLOW}WARN${NC} (${FILE_TIME}ms >= 1000ms)"
    TESTS_FAILED=$((TESTS_FAILED + 1))
fi

# Binary permissions
TESTS_RUN=$((TESTS_RUN + 1))
if [ -x "$SMT" ]; then
    echo -e "Testing: Binary executable... ${GREEN}PASS${NC}"
    TESTS_PASSED=$((TESTS_PASSED + 1))
else
    echo -e "Testing: Binary executable... ${RED}FAIL${NC}"
    TESTS_FAILED=$((TESTS_FAILED + 1))
fi

# 7. ERROR RECOVERY
echo -e "\n${BLUE}--- Error Recovery ---${NC}"
test_case "Recover after undefined var" "$SMT eval '(+ 1 1)' && ($SMT eval 'undefined-var' || true) && $SMT eval '(+ 2 2)'" "4"
test_case "Multiple evals in sequence" "$SMT eval '(+ 1 1)' && $SMT eval '(+ 2 2)' && $SMT eval '(+ 3 3)'" "6"
test_case "Error then success" "($SMT eval '(/ 1 0)' || true) && $SMT eval '(+ 5 5)'" "10"
test_case "File error then eval" "($SMT run /nonexistent.coal || true) && $SMT eval '42'" "42"
test_case "Syntax error then recovery" "($SMT eval '(+ 1' || true) && $SMT eval '(* 7 6)'" "42"

# 8. STDLIB TESTS
echo -e "\n${BLUE}--- Standard Library ---${NC}"

cat > /tmp/test_io_println.coal << 'EOF'
(define main
  (smelter.stdlib.io:io-println "Hello from stdlib"))
EOF
test_case "io-println" "$SMT run /tmp/test_io_println.coal" "Hello from stdlib"

cat > /tmp/test_io_print.coal << 'EOF'
(define main
  (smelter.stdlib.io:io-print "No newline"))
EOF
test_case "io-print" "$SMT run /tmp/test_io_print.coal" "No newline"

cat > /tmp/test_show_int.coal << 'EOF'
(define main
  (smelter.stdlib.io:io-println
    (smelter.stdlib.io:show-int 42)))
EOF
test_case "show-int" "$SMT run /tmp/test_show_int.coal" "42"

cat > /tmp/test_show_bool.coal << 'EOF'
(define main
  (smelter.stdlib.io:io-println
    (smelter.stdlib.io:show-bool True)))
EOF
test_case "show-bool" "$SMT run /tmp/test_show_bool.coal" "True"

cat > /tmp/test_multiple_io.coal << 'EOF'
(define main
  (progn
    (smelter.stdlib.io:io-println "Line 1")
    (smelter.stdlib.io:io-println "Line 2")
    (smelter.stdlib.io:io-println "Line 3")))
EOF
test_case "Multiple I/O operations" "$SMT run /tmp/test_multiple_io.coal" "Line 1"

# 9. LAUNCH EXAMPLES
echo -e "\n${BLUE}--- Launch Examples ---${NC}"
test_case "Launch: hello.coal" "$SMT run examples/launch/hello.coal" "Hello"
test_case "Launch: fibonacci.coal" "$SMT run examples/launch/fibonacci.coal" "fib"
test_case "Launch: factorial.coal" "$SMT run examples/launch/factorial.coal" "120"
test_case "Launch: fizzbuzz.coal" "$SMT run examples/launch/fizzbuzz.coal" "FizzBuzz"
test_case "Launch: temperature.coal" "$SMT run examples/launch/temperature.coal" "42"

# 10. COMMAND LINE INTERFACE
echo -e "\n${BLUE}--- Command Line Interface ---${NC}"
test_case "No arguments shows help" "$SMT" "Usage:"
test_case "Invalid command" "$SMT invalid-command" "Unknown command"
test_case "Help flag -h" "$SMT -h" "Usage:"
test_case "Help flag --help" "$SMT --help" "Usage:"
test_case "Version flag --version" "$SMT --version" "Smelter"
test_case "Eval missing argument" "$SMT eval" "Usage:"
test_case "Run missing argument" "$SMT run" "Usage:"

# Cleanup
rm -f /tmp/test_*.coal

# SUMMARY
echo ""
echo "========================================="
echo "  TEST SUMMARY"
echo "========================================="
echo "Tests run:    $TESTS_RUN"
echo "Tests passed: $TESTS_PASSED"
echo "Tests failed: $TESTS_FAILED"
echo ""

if [ $TESTS_FAILED -eq 0 ]; then
    echo -e "${GREEN}✓ ALL TESTS PASSED!${NC}"
    echo "Smelter is production-ready for HN launch 🚀"
    exit 0
else
    echo -e "${RED}✗ $TESTS_FAILED TEST(S) FAILED${NC}"
    echo "Please fix failing tests before launch"
    exit 1
fi
