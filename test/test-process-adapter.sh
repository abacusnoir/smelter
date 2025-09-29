#!/bin/bash
set -e

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

echo -e "${BLUE}🧪 Testing Smelter Process Adapter Integration...${NC}"
echo "================================================"

# Check if smelter binary exists
if [ ! -f "./smt" ]; then
    echo -e "${RED}❌ FAIL - smt binary not found. Run 'make build' first.${NC}"
    exit 1
fi

TESTS_PASSED=0
TESTS_FAILED=0

# Helper function to run a test
run_test() {
    local test_name="$1"
    local test_code="$2"
    local expected="$3"

    echo -n "Test ${test_name}: "

    # Run the test and capture output
    OUTPUT=$(./smt eval "$test_code" 2>/dev/null || echo "ERROR")

    if [[ "$OUTPUT" == *"$expected"* ]]; then
        echo -e "${GREEN}✅ PASS${NC}"
        ((TESTS_PASSED++))
    else
        echo -e "${RED}❌ FAIL${NC}"
        echo -e "${YELLOW}  Expected: $expected${NC}"
        echo -e "${YELLOW}  Got: $OUTPUT${NC}"
        ((TESTS_FAILED++))
    fi
}

# Test 1: Basic command execution
echo -e "\n${BLUE}1. Basic Command Execution${NC}"
echo "   ========================"

run_test "1.1 - Basic echo" \
'(progn
  (use-package :smelter/adapters/process)
  (match (run (list "echo" "test"))
    ((Ok (ProcessResult stdout _ _)) stdout)
    ((Err e) "ERROR")))' \
"test"

run_test "1.2 - Command with args" \
'(progn
  (use-package :smelter/adapters/process)
  (match (run (list "echo" "-n" "no-newline"))
    ((Ok (ProcessResult stdout _ _)) stdout)
    ((Err e) "ERROR")))' \
"no-newline"

run_test "1.3 - Exit code success" \
'(progn
  (use-package :smelter/adapters/process)
  (match (run (list "true"))
    ((Ok (ProcessResult _ _ (ExitSuccess))) "SUCCESS")
    ((Ok (ProcessResult _ _ (ExitFailure _))) "FAILED")
    ((Err e) "ERROR")))' \
"SUCCESS"

run_test "1.4 - Exit code failure" \
'(progn
  (use-package :smelter/adapters/process)
  (match (run (list "false"))
    ((Ok (ProcessResult _ _ (ExitFailure _))) "FAILED")
    ((Ok (ProcessResult _ _ (ExitSuccess))) "SUCCESS")
    ((Err e) "ERROR")))' \
"FAILED"

# Test 2: Shell command execution
echo -e "\n${BLUE}2. Shell Command Execution${NC}"
echo "   ==========================="

run_test "2.1 - Basic shell" \
'(progn
  (use-package :smelter/adapters/process)
  (match (shell "echo hello")
    ((Ok (ProcessResult stdout _ _)) stdout)
    ((Err e) "ERROR")))' \
"hello"

run_test "2.2 - Shell pipeline" \
'(progn
  (use-package :smelter/adapters/process)
  (match (shell "echo \"one two three\" | wc -w")
    ((Ok (ProcessResult stdout _ _)) stdout)
    ((Err e) "ERROR")))' \
"3"

run_test "2.3 - Shell variables" \
'(progn
  (use-package :smelter/adapters/process)
  (match (shell "VAR=test; echo $VAR")
    ((Ok (ProcessResult stdout _ _)) stdout)
    ((Err e) "ERROR")))' \
"test"

# Test 3: Input handling
echo -e "\n${BLUE}3. Input Handling${NC}"
echo "   ================"

run_test "3.1 - Stdin input" \
'(progn
  (use-package :smelter/adapters/process)
  (match (run-with-input (list "cat") "input-test")
    ((Ok (ProcessResult stdout _ _)) stdout)
    ((Err e) "ERROR")))' \
"input-test"

run_test "3.2 - Shell with input" \
'(progn
  (use-package :smelter/adapters/process)
  (match (shell-with-input "cat" "shell-input")
    ((Ok (ProcessResult stdout _ _)) stdout)
    ((Err e) "ERROR")))' \
"shell-input"

# Test 4: Piping
echo -e "\n${BLUE}4. Command Piping${NC}"
echo "   ================"

run_test "4.1 - Simple pipe" \
'(progn
  (use-package :smelter/adapters/process)
  (match (pipe (list "echo" "one two three") (list "wc" "-w"))
    ((Ok (ProcessResult stdout _ _)) stdout)
    ((Err e) "ERROR")))' \
"3"

run_test "4.2 - Chain commands" \
'(progn
  (use-package :smelter/adapters/process)
  (match (chain-commands (list (list "echo" "test") (list "cat")))
    ((Ok (ProcessResult stdout _ _)) stdout)
    ((Err e) "ERROR")))' \
"test"

# Test 5: Output capture
echo -e "\n${BLUE}5. Output Capture${NC}"
echo "   ================"

run_test "5.1 - Capture stdout" \
'(progn
  (use-package :smelter/adapters/process)
  (match (capture-output (list "echo" "capture-test"))
    ((Ok output) output)
    ((Err e) "ERROR")))' \
"capture-test"

run_test "5.2 - Capture stderr" \
'(progn
  (use-package :smelter/adapters/process)
  (match (capture-error (list "sh" "-c" "echo error >&2"))
    ((Ok output) output)
    ((Err e) "ERROR")))' \
"error"

# Test 6: Environment variables
echo -e "\n${BLUE}6. Environment Variables${NC}"
echo "   ======================"

run_test "6.1 - Get PATH" \
'(progn
  (use-package :smelter/adapters/process)
  (match (get-env "PATH")
    ((Some path) "FOUND")
    (None "NOT_FOUND")))' \
"FOUND"

run_test "6.2 - Set and get env" \
'(progn
  (use-package :smelter/adapters/process)
  (set-env "SMELTER_TEST_VAR" "test123")
  (match (get-env "SMELTER_TEST_VAR")
    ((Some value) value)
    (None "NOT_FOUND")))' \
"test123"

run_test "6.3 - Env exists check" \
'(progn
  (use-package :smelter/adapters/process)
  (set-env "SMELTER_EXISTS_TEST" "exists")
  (if (env-exists? "SMELTER_EXISTS_TEST") "EXISTS" "NOT_EXISTS"))' \
"EXISTS"

# Test 7: Working directory
echo -e "\n${BLUE}7. Working Directory${NC}"
echo "   =================="

run_test "7.1 - Get working dir" \
'(progn
  (use-package :smelter/adapters/process)
  (let ((cwd (get-working-directory)))
    (if (not (== cwd "")) "HAS_CWD" "NO_CWD")))' \
"HAS_CWD"

run_test "7.2 - With working dir" \
'(progn
  (use-package :smelter/adapters/process)
  (match (with-working-directory "/tmp" (list "pwd"))
    ((Ok (ProcessResult stdout _ _))
     (if (contains? "/tmp" stdout) "IN_TMP" "NOT_TMP"))
    ((Err e) "ERROR")))' \
"IN_TMP"

# Test 8: Utilities
echo -e "\n${BLUE}8. Utility Functions${NC}"
echo "   =================="

run_test "8.1 - Which command" \
'(progn
  (use-package :smelter/adapters/process)
  (match (which "sh")
    ((Some path) "FOUND")
    (None "NOT_FOUND")))' \
"FOUND"

run_test "8.2 - Executable check" \
'(progn
  (use-package :smelter/adapters/process)
  (match (which "sh")
    ((Some path) (if (executable? path) "EXECUTABLE" "NOT_EXECUTABLE"))
    (None "NOT_FOUND")))' \
"EXECUTABLE"

run_test "8.3 - Shell escaping" \
'(progn
  (use-package :smelter/adapters/process)
  (let ((escaped (escape-shell-arg "test'\''quote")))
    (if (contains? "'" escaped) "ESCAPED" "NOT_ESCAPED")))' \
"ESCAPED"

run_test "8.4 - Parse command" \
'(progn
  (use-package :smelter/adapters/process)
  (let ((parsed (parse-command "ls -la /tmp")))
    (== (length parsed) 3)))' \
"True"

# Test 9: Error handling
echo -e "\n${BLUE}9. Error Handling${NC}"
echo "   ==============="

run_test "9.1 - Non-existent command" \
'(progn
  (use-package :smelter/adapters/process)
  (match (run (list "nonexistent-command-xyz"))
    ((Ok _) "UNEXPECTED_SUCCESS")
    ((Err e) "ERROR_CAUGHT")))' \
"ERROR_CAUGHT"

run_test "9.2 - Which non-existent" \
'(progn
  (use-package :smelter/adapters/process)
  (match (which "definitely-does-not-exist-xyz")
    ((Some _) "FOUND")
    (None "NOT_FOUND")))' \
"NOT_FOUND"

# Test 10: Legacy API compatibility
echo -e "\n${BLUE}10. Legacy API Compatibility${NC}"
echo "    =========================="

run_test "10.1 - Legacy run-process" \
'(progn
  (use-package :smelter/adapters/process)
  (match (run-process "echo legacy")
    ((Ok result) (process-stdout result))
    ((Err e) "ERROR")))' \
"legacy"

run_test "10.2 - Legacy command-exists?" \
'(progn
  (use-package :smelter/adapters/process)
  (if (command-exists? "sh") "EXISTS" "NOT_EXISTS"))' \
"EXISTS"

run_test "10.3 - Legacy capture-command" \
'(progn
  (use-package :smelter/adapters/process)
  (match (capture-command "echo capture")
    ((Ok output) output)
    ((Err e) "ERROR")))' \
"capture"

# Test summary
echo ""
echo "================================================"
echo -e "${BLUE}Test Summary${NC}"
echo "============"
echo -e "Tests passed: ${GREEN}${TESTS_PASSED}${NC}"
echo -e "Tests failed: ${RED}${TESTS_FAILED}${NC}"
echo -e "Total tests:  $((TESTS_PASSED + TESTS_FAILED))"

if [ $TESTS_FAILED -eq 0 ]; then
    echo ""
    echo -e "${GREEN}🎉 All process adapter tests passed! 🎉${NC}"
    echo "The Smelter process adapter is working correctly."
    exit 0
else
    echo ""
    echo -e "${RED}❌ Some tests failed. Check the output above.${NC}"
    exit 1
fi