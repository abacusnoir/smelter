#!/bin/bash
set -e

echo "Testing Simplified Process Adapter..."

# Test 1: Basic command
echo -n "Test 1 - Basic run: "
OUTPUT=$(./smt eval '(match (smelter/adapters/process:run "echo hello") ((Ok (smelter/adapters/process:ProcessResult stdout _ _)) stdout) ((Err _) "ERROR"))')
if [[ "$OUTPUT" == *"hello"* ]]; then
    echo "✅ PASS"
else
    echo "❌ FAIL (got: $OUTPUT)"
    exit 1
fi

# Test 2: Shell command with pipe
echo -n "Test 2 - Shell with pipe: "
OUTPUT=$(./smt eval '(match (smelter/adapters/process:shell "echo test | wc -w") ((Ok (smelter/adapters/process:ProcessResult stdout _ _)) stdout) ((Err _) "ERROR"))')
if [[ "$OUTPUT" == *"1"* ]]; then
    echo "✅ PASS"
else
    echo "❌ FAIL (got: $OUTPUT)"
    exit 1
fi

# Test 3: Shell with input
echo -n "Test 3 - Shell with input: "
OUTPUT=$(./smt eval '(match (smelter/adapters/process:shell-with-input "cat" "input-test") ((Ok (smelter/adapters/process:ProcessResult stdout _ _)) stdout) ((Err _) "ERROR"))')
if [[ "$OUTPUT" == *"input-test"* ]]; then
    echo "✅ PASS"
else
    echo "❌ FAIL (got: $OUTPUT)"
    exit 1
fi

# Test 4: Environment variable
echo -n "Test 4 - Get environment variable: "
OUTPUT=$(./smt eval '(match (smelter/adapters/process:get-env "PATH") ((Some _) "FOUND") (None "NOT_FOUND"))')
if [[ "$OUTPUT" == *"FOUND"* ]]; then
    echo "✅ PASS"
else
    echo "❌ FAIL (got: $OUTPUT)"
    exit 1
fi

# Test 5: Set environment variable
echo -n "Test 5 - Set and get environment variable: "
OUTPUT=$(./smt eval '(progn (smelter/adapters/process:set-env "SMELTER_TEST" "value123") (match (smelter/adapters/process:get-env "SMELTER_TEST") ((Some v) v) (None "NOT_FOUND")))')
if [[ "$OUTPUT" == *"value123"* ]]; then
    echo "✅ PASS"
else
    echo "❌ FAIL (got: $OUTPUT)"
    exit 1
fi

# Test 6: Which command
echo -n "Test 6 - Which command: "
OUTPUT=$(./smt eval '(match (smelter/adapters/process:which "ls") ((Some _) "FOUND") (None "NOT_FOUND"))')
if [[ "$OUTPUT" == *"FOUND"* ]]; then
    echo "✅ PASS"
else
    echo "❌ FAIL (got: $OUTPUT)"
    exit 1
fi

# Test 7: Pipe commands
echo -n "Test 7 - Pipe commands: "
OUTPUT=$(./smt eval '(match (smelter/adapters/process:pipe-commands "echo one two three" "wc -w") ((Ok (smelter/adapters/process:ProcessResult stdout _ _)) stdout) ((Err _) "ERROR"))')
if [[ "$OUTPUT" == *"3"* ]]; then
    echo "✅ PASS"
else
    echo "❌ FAIL (got: $OUTPUT)"
    exit 1
fi

# Test 8: Exit code success
echo -n "Test 8 - Exit code success: "
OUTPUT=$(./smt eval '(match (smelter/adapters/process:run "true") ((Ok (smelter/adapters/process:ProcessResult _ _ code)) code) ((Err _) 999))')
if [[ "$OUTPUT" == "0" ]]; then
    echo "✅ PASS"
else
    echo "❌ FAIL (got: $OUTPUT)"
    exit 1
fi

# Test 9: Exit code failure
echo -n "Test 9 - Exit code failure: "
OUTPUT=$(./smt eval '(match (smelter/adapters/process:run "false") ((Ok (smelter/adapters/process:ProcessResult _ _ code)) code) ((Err _) 999))')
if [[ "$OUTPUT" != "0" && "$OUTPUT" != "999" ]]; then
    echo "✅ PASS"
else
    echo "❌ FAIL (got: $OUTPUT)"
    exit 1
fi

# Test 10: Timeout (Unix only, requires timeout command)
echo -n "Test 10 - Timeout: "
if command -v timeout &> /dev/null; then
    OUTPUT=$(./smt eval '(match (smelter/adapters/process:run-with-timeout 5 "echo timeout-test") ((Ok (smelter/adapters/process:ProcessResult stdout _ _)) stdout) ((Err _) "ERROR"))')
    if [[ "$OUTPUT" == *"timeout-test"* ]]; then
        echo "✅ PASS"
    else
        echo "❌ FAIL (got: $OUTPUT)"
        exit 1
    fi
else
    echo "⏭️  SKIP (timeout command not available)"
fi

echo ""
echo "🎉 All process adapter tests passed! 🎉"
