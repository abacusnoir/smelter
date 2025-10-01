#!/bin/bash
set -e

echo "Testing Process Adapter Integration..."
echo "======================================"

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

echo ""
echo "Backward Compatibility Tests"
echo "-----------------------------"

# Test 1: Old string API still works
run_test "String API unchanged" \
'(match (smelter/adapters/process:run "echo test") ((Ok (smelter/adapters/process:ProcessResult stdout _ _)) stdout) ((Err _) "ERROR"))' \
"test"

# Test 2: Shell API still works
run_test "Shell API unchanged" \
'(match (smelter/adapters/process:shell "echo shell | tr a-z A-Z") ((Ok (smelter/adapters/process:ProcessResult stdout _ _)) stdout) ((Err _) "ERROR"))' \
"SHELL"

# Test 3: Environment API still works
run_test "Env API unchanged" \
'(progn (smelter/adapters/process:set-env "TEST_VAR" "value") (match (smelter/adapters/process:get-env "TEST_VAR") ((Some v) v) (None "NONE")))' \
"value"

echo ""
echo "Integration Tests"
echo "-----------------"

# Test 4: Builder and core work together
run_test "Builder + core integration" \
'(match (smelter/adapters/process/builder:run-args "echo" (list "integration" "test")) ((Ok (smelter/adapters/process:ProcessResult stdout _ _)) stdout) ((Err _) "ERROR"))' \
"integration test"

# Test 5: Platform detection + execution
run_test "Platform + execution" \
'(let ((os (smelter/adapters/process/platform:detect-os Unit)) (cmd (smelter/adapters/process/platform:platform-command os "list"))) (match (smelter/adapters/process/builder:run-args cmd (list)) ((Ok _) "OK") ((Err _) "ERROR")))' \
"OK"

# Test 6: Complex real-world scenario - grep with builder
run_test "Complex grep scenario" \
'(match (smelter/adapters/process/builder:run-args "echo" (list "line1\nline2\nline3")) ((Ok (smelter/adapters/process:ProcessResult stdout _ _)) (if (>= (length stdout) 5) "OK" "SHORT")) ((Err _) "ERROR"))' \
"OK"

# Test 7: Escaping prevents shell injection
run_test "Shell injection prevention" \
'(match (smelter/adapters/process/builder:run-args "echo" (list "safe$(whoami)")) ((Ok (smelter/adapters/process:ProcessResult stdout _ _)) stdout) ((Err _) "ERROR"))' \
"safe\$(whoami)"

echo ""
echo "======================================"
echo "Integration Test Results:"
echo "  Passed: $PASS"
echo "  Failed: $FAIL"
echo "======================================"

if [ $FAIL -eq 0 ]; then
    echo "✅ All integration tests passed!"
    echo "✅ Backward compatibility maintained!"
    exit 0
else
    echo "❌ Some tests failed"
    exit 1
fi
