#!/bin/bash
set -e

echo "Testing Process Adapter Execution Options..."
echo "============================================"

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

# Test 1: Run with working directory
run_test "Run with working directory" \
'(let ((opts (smelter/adapters/process/options:make-options (Some "/tmp") None None))) (match (smelter/adapters/process/options:run-with-options "pwd" opts) ((Ok (smelter/adapters/process:ProcessResult stdout _ _)) stdout) ((Err _) "ERROR")))' \
"/tmp"

# Test 2: Run with custom environment variable
run_test "Run with custom environment" \
'(let ((opts (smelter/adapters/process/options:make-options None (Some (list (Tuple "TEST_VAR" "test_value"))) None))) (match (smelter/adapters/process/options:run-with-options "echo \$TEST_VAR" opts) ((Ok (smelter/adapters/process:ProcessResult stdout _ _)) stdout) ((Err _) "ERROR")))' \
"test_value"

# Test 3: Run with stdin input
run_test "Run with stdin input" \
'(let ((opts (smelter/adapters/process/options:make-options None None (Some "hello from stdin")))) (match (smelter/adapters/process/options:run-with-options "cat" opts) ((Ok (smelter/adapters/process:ProcessResult stdout _ _)) stdout) ((Err _) "ERROR")))' \
"hello from stdin"

# Test 4: Combine multiple options (dir + env + stdin)
run_test "Combine dir + env + stdin" \
'(let ((opts (smelter/adapters/process/options:make-options (Some "/tmp") (Some (list (Tuple "GREETING" "hi"))) (Some "test input")))) (match (smelter/adapters/process/options:run-with-options "cat" opts) ((Ok (smelter/adapters/process:ProcessResult stdout _ _)) stdout) ((Err _) "ERROR")))' \
"test input"

# Test 5: No options (should work like regular run)
run_test "No options (default behavior)" \
'(let ((opts (smelter/adapters/process/options:make-options None None None))) (match (smelter/adapters/process/options:run-with-options "echo default" opts) ((Ok (smelter/adapters/process:ProcessResult stdout _ _)) stdout) ((Err _) "ERROR")))' \
"default"

echo ""
echo "============================================"
echo "Execution Options Test Results:"
echo "  Passed: $PASS"
echo "  Failed: $FAIL"
echo "============================================"

if [ $FAIL -eq 0 ]; then
    echo "✅ All execution options tests passed!"
    exit 0
else
    echo "❌ Some tests failed"
    exit 1
fi
