#!/bin/bash
set -e

echo "Testing Process Adapter Platform Module..."
echo "==========================================="

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

# Test 1: Detect OS (should be MacOS on this system)
run_test "Detect MacOS" \
'(match (smelter/adapters/process/platform:detect-os Unit) (smelter/adapters/process/platform:MacOS "MacOS") (smelter/adapters/process/platform:Linux "Linux") (_ "Other"))' \
"MacOS"

# Test 2: OS to string conversion (via detect-os)
run_test "OS to string via detect" \
'(smelter/adapters/process/platform:os-name (smelter/adapters/process/platform:detect-os Unit))' \
"MacOS"

# Test 3: Platform command integration
run_test "Platform command integration" \
'(let ((os (smelter/adapters/process/platform:detect-os Unit)) (cmd (smelter/adapters/process/platform:platform-command os "list"))) (if (or (== cmd "ls") (== cmd "dir /b")) "OK" "WRONG"))' \
"OK"

# Test 6: Current platform detection works
run_test "Current platform usable" \
'(let ((os (smelter/adapters/process/platform:detect-os Unit))) (smelter/adapters/process/platform:os-name os))' \
"MacOS"

echo ""
echo "==========================================="
echo "Platform Test Results:"
echo "  Passed: $PASS"
echo "  Failed: $FAIL"
echo "==========================================="

if [ $FAIL -eq 0 ]; then
    echo "✅ All platform tests passed!"
    exit 0
else
    echo "❌ Some tests failed"
    exit 1
fi
