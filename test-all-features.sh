#!/bin/bash
# test-all-features.sh - Verify all Smelter features work with optimized binary

set -e

echo "🔥 Smelter Feature Test Suite"
echo "=============================="
echo

BINARY="${1:-./smt}"
PASS="✅"
FAIL="❌"

# Color codes
GREEN='\033[0;32m'
RED='\033[0;31m'
YELLOW='\033[0;33m'
NC='\033[0m'

# Test function
test_feature() {
    local name="$1"
    local cmd="$2"
    local expected="$3"
    
    printf "%-30s" "Testing $name... "
    
    # Measure time and capture output
    start=$(perl -MTime::HiRes=time -e 'printf "%.6f", time')
    output=$($BINARY eval "$cmd" 2>/dev/null || echo "ERROR")
    end=$(perl -MTime::HiRes=time -e 'printf "%.6f", time')
    
    # Calculate time in ms
    time_ms=$(echo "scale=1; ($end - $start) * 1000" | bc)
    
    # Check result
    if [ "$output" = "$expected" ]; then
        echo -e "${GREEN}${PASS}${NC} (${time_ms}ms)"
        return 0
    else
        echo -e "${RED}${FAIL}${NC} (got: '$output', expected: '$expected')"
        return 1
    fi
}

# Track results
total=0
passed=0

# Core Language Features
echo "=== Core Language ==="
test_feature "Arithmetic" '(+ 1 2)' "3" && ((passed++)) || true; ((total++))
test_feature "Multiplication" '(* 5 7)' "35" && ((passed++)) || true; ((total++))
test_feature "Division" '(/ 10 2)' "5" && ((passed++)) || true; ((total++))
test_feature "Nested math" '(+ (* 2 3) 4)' "10" && ((passed++)) || true; ((total++))
test_feature "Let binding" '(let ((x 5)) (+ x 3))' "8" && ((passed++)) || true; ((total++))
test_feature "Multiple bindings" '(let ((x 2) (y 3)) (* x y))' "6" && ((passed++)) || true; ((total++))
echo

# Coalton-specific features
echo "=== Coalton Features ==="
test_feature "Type inference" '(the Integer 5)' "5" && ((passed++)) || true; ((total++))
test_feature "Function definition" '(defun square (x) (* x x))' "*" && ((passed++)) || true; ((total++))
echo

# String Operations
echo "=== Strings ==="
test_feature "String literal" '"hello"' '"hello"' && ((passed++)) || true; ((total++))
test_feature "Empty string" '""' '""' && ((passed++)) || true; ((total++))
echo

# Boolean Operations
echo "=== Booleans ==="
test_feature "True literal" 'True' "TRUE" && ((passed++)) || true; ((total++))
test_feature "False literal" 'False' "FALSE" && ((passed++)) || true; ((total++))
test_feature "Boolean and" '(and True True)' "TRUE" && ((passed++)) || true; ((total++))
test_feature "Boolean or" '(or False True)' "TRUE" && ((passed++)) || true; ((total++))
echo

# List Operations
echo "=== Lists ==="
test_feature "Empty list" 'Nil' "NIL" && ((passed++)) || true; ((total++))
test_feature "List constructor" '(Cons 1 Nil)' "(1)" && ((passed++)) || true; ((total++))
echo

# Performance Benchmarks
echo "=== Performance ==="
echo -n "Average startup (10 runs): "
total_time=0
for i in {1..10}; do
    start=$(perl -MTime::HiRes=time -e 'printf "%.6f", time')
    $BINARY eval '(+ 1 2)' > /dev/null 2>&1
    end=$(perl -MTime::HiRes=time -e 'printf "%.6f", time')
    time_ms=$(echo "scale=3; ($end - $start) * 1000" | bc)
    total_time=$(echo "$total_time + $time_ms" | bc)
done
avg_time=$(echo "scale=1; $total_time / 10" | bc)
echo "${avg_time}ms"

if (( $(echo "$avg_time < 50" | bc -l) )); then
    echo -e "${GREEN}✅ Excellent performance (<50ms)${NC}"
elif (( $(echo "$avg_time < 100" | bc -l) )); then
    echo -e "${YELLOW}⚡ Good performance (<100ms)${NC}"
else
    echo -e "${RED}❌ Performance needs work (>100ms)${NC}"
fi
echo

# Binary info
echo "=== Binary Analysis ==="
echo -n "Binary size: "
ls -lh $BINARY | awk '{print $5}'
echo -n "Binary type: "
file $BINARY | cut -d: -f2
echo

# Summary
echo "=== Test Summary ==="
echo "Tests passed: $passed/$total"
percent=$((passed * 100 / total))
echo -n "Success rate: $percent% - "
if [ "$percent" -eq 100 ]; then
    echo -e "${GREEN}PERFECT!${NC}"
elif [ "$percent" -ge 80 ]; then
    echo -e "${YELLOW}GOOD${NC}"
else
    echo -e "${RED}NEEDS WORK${NC}"
fi

echo
echo -n "Overall verdict: "
if [ "$passed" -eq "$total" ] && (( $(echo "$avg_time < 50" | bc -l) )); then
    echo -e "${GREEN}🎉 READY FOR RELEASE!${NC}"
    echo "All features work and performance exceeds targets!"
elif [ "$passed" -eq "$total" ]; then
    echo -e "${YELLOW}✅ Functionally ready, performance acceptable${NC}"
else
    echo -e "${RED}⚠️  Not ready - fix failing tests${NC}"
fi