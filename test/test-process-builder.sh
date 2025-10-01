#!/bin/bash
set -e

echo "Testing Process Adapter Builder Module..."
echo "=========================================="

PASS=0
FAIL=0

run_test() {
    local test_name="$1"
    local test_code="$2"
    local expected="$3"

    echo -n "Test: $test_name... "

    OUTPUT=$(./smt eval "$test_code" 2>&1 || echo "ERROR")

    if [[ "$OUTPUT" == *"$expected"* ]]; then
        echo "✅ PASS"
        ((PASS++)) || true
    else
        echo "❌ FAIL"
        echo "  Expected: $expected"
        echo "  Got: $OUTPUT"
        ((FAIL++)) || true
        return 1
    fi
}

# Test 1: Escape argument with spaces
run_test "Escape arg with spaces" \
'(smelter/adapters/process/builder:escape-shell-arg "hello world")' \
"'hello world'"

# Test 2: Escape argument with single quotes
run_test "Escape arg with quotes" \
'(smelter/adapters/process/builder:escape-shell-arg "it'\''s working")' \
"it"

# Test 3: Build command from list - simple
run_test "Build simple command" \
'(smelter/adapters/process/builder:build-command "ls" (list "-la"))' \
"ls"

# Test 4: Build command from list - complex
run_test "Build complex command" \
'(smelter/adapters/process/builder:build-command "grep" (list "-r" "search term" "/path"))' \
"grep"

# Test 5: Run command via run-args - basic
run_test "run-args basic" \
'(match (smelter/adapters/process/builder:run-args "echo" (list "hello")) ((Ok (smelter/adapters/process:ProcessResult stdout _ _)) stdout) ((Err _) "ERROR"))' \
"hello"

# Test 6: Run command via run-args - with spaces
run_test "run-args with spaces" \
'(match (smelter/adapters/process/builder:run-args "echo" (list "hello world")) ((Ok (smelter/adapters/process:ProcessResult stdout _ _)) stdout) ((Err _) "ERROR"))' \
"hello world"

# Test 7: Run command via run-args - multiple args
run_test "run-args multiple args" \
'(match (smelter/adapters/process/builder:run-args "echo" (list "one" "two" "three")) ((Ok (smelter/adapters/process:ProcessResult stdout _ _)) stdout) ((Err _) "ERROR"))' \
"one two three"

# Test 8: Run command via run-args - special characters
run_test "run-args special chars" \
'(match (smelter/adapters/process/builder:run-args "echo" (list "test$VAR")) ((Ok (smelter/adapters/process:ProcessResult stdout _ _)) stdout) ((Err _) "ERROR"))' \
"test\$VAR"

echo ""
echo "=========================================="
echo "Builder Test Results:"
echo "  Passed: $PASS"
echo "  Failed: $FAIL"
echo "=========================================="

if [ $FAIL -eq 0 ]; then
    echo "✅ All builder tests passed!"
    exit 0
else
    echo "❌ Some tests failed"
    exit 1
fi
