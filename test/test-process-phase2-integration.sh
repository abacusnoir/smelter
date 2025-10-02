#!/bin/bash
set -e

echo "Testing Process Adapter Phase 2 Integration..."
echo "=============================================="

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
echo "Integration Tests"
echo "-----------------"

# Test 1: Options work with working directory
run_test "Options with working directory" \
'(let ((opts (smelter/adapters/process/options:make-options (Some "/tmp") None None))) (match (smelter/adapters/process/options:run-with-options "pwd" opts) ((Ok (smelter/adapters/process:ProcessResult stdout _ _)) stdout) ((Err _) "ERROR")))' \
"/tmp"

# Test 2: Options with environment variable
run_test "Options with environment" \
'(let ((opts (smelter/adapters/process/options:make-options None (Some (list (Tuple "VAR" "value"))) None))) (match (smelter/adapters/process/options:run-with-options "echo \$VAR" opts) ((Ok (smelter/adapters/process:ProcessResult stdout _ _)) stdout) ((Err _) "ERROR")))' \
"value"

# Test 3: Phase 1 builder works with Phase 2 options
run_test "Builder + options integration" \
'(let ((cmd (smelter/adapters/process/builder:build-command "echo" (list "test" "args"))) (opts (smelter/adapters/process/options:make-options None None None))) (match (smelter/adapters/process/options:run-with-options cmd opts) ((Ok (smelter/adapters/process:ProcessResult stdout _ _)) stdout) ((Err _) "ERROR")))' \
"test"

# Test 4: Backward compatibility - old API still works
run_test "Backward compatibility check" \
'(match (smelter/adapters/process:run "echo backward") ((Ok (smelter/adapters/process:ProcessResult stdout _ _)) stdout) ((Err _) "ERROR"))' \
"backward"

# Test 5: shell-with-options works
run_test "Shell with options" \
'(let ((opts (smelter/adapters/process/options:make-options None None None))) (match (smelter/adapters/process/options:shell-with-options "echo shell | tr a-z A-Z" opts) ((Ok (smelter/adapters/process:ProcessResult stdout _ _)) stdout) ((Err _) "ERROR")))' \
"SHELL"

echo ""
echo "=============================================="
echo "Phase 2 Integration Test Results:"
echo "  Passed: $PASS"
echo "  Failed: $FAIL"
echo "=============================================="

if [ $FAIL -eq 0 ]; then
    echo "✅ All Phase 2 integration tests passed!"
    echo "✅ Backward compatibility maintained!"
    exit 0
else
    echo "❌ Some tests failed"
    exit 1
fi
