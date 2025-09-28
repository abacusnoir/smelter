#!/bin/bash
# JSON functionality regression tests
# Ensures JSON parsing capabilities work correctly

set -e  # Exit on first failure

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

echo -e "${BLUE}=== JSON Functionality Regression Tests ===${NC}"
echo

# Helper function to test JSON expressions via eval
test_json_eval() {
    local test_name="$1"
    local expression="$2"
    local expected_pattern="$3"

    echo -e "${BLUE}Testing:${NC} $test_name"
    echo -e "  Expression: $expression"

    # Run the eval command and capture output
    local result
    if result=$(./smt eval "$expression" 2>&1); then
        if echo "$result" | grep -q "$expected_pattern"; then
            echo -e "  ${GREEN}✅ PASS${NC} - Found expected pattern: $expected_pattern"
        else
            echo -e "  ${RED}❌ FAIL${NC} - Expected pattern '$expected_pattern' not found"
            echo -e "  Actual output: $result"
            exit 1
        fi
    else
        echo -e "  ${RED}❌ FAIL${NC} - Command failed with output: $result"
        exit 1
    fi
    echo
}

# Helper function to test JSON scripts
test_json_script() {
    local test_name="$1"
    local script_path="$2"
    local expected_pattern="$3"

    echo -e "${BLUE}Testing:${NC} $test_name"
    echo -e "  Script: $script_path"

    if [ ! -f "$script_path" ]; then
        echo -e "  ${RED}❌ FAIL${NC} - Script file not found: $script_path"
        exit 1
    fi

    # Run the script and capture output
    local result
    if result=$(./smt run "$script_path" 2>&1); then
        if echo "$result" | grep -q "$expected_pattern"; then
            echo -e "  ${GREEN}✅ PASS${NC} - Found expected pattern: $expected_pattern"
        else
            echo -e "  ${RED}❌ FAIL${NC} - Expected pattern '$expected_pattern' not found"
            echo -e "  Actual output: $result"
            exit 1
        fi
    else
        echo -e "  ${RED}❌ FAIL${NC} - Script failed with output: $result"
        exit 1
    fi
    echo
}

# Ensure binary exists
if [ ! -f "./smt" ]; then
    echo -e "${RED}Error: smt binary not found. Run 'make build' first.${NC}"
    exit 1
fi

# Test JSON parsing via eval mode (basic functionality)
echo -e "${BLUE}Testing JSON parsing via eval mode...${NC}"
echo

# Test simple JSON number parsing
test_json_eval "JSON number parsing" \
    '(match (smelter.stdlib.json:parse-json "42") ((Ok _) "SUCCESS") ((Err _) "FAILED"))' \
    "SUCCESS"

# Test simple JSON string parsing
test_json_eval "JSON string parsing" \
    '(match (smelter.stdlib.json:parse-json "\"hello\"") ((Ok _) "SUCCESS") ((Err _) "FAILED"))' \
    "SUCCESS"

# Test simple JSON boolean parsing
test_json_eval "JSON boolean parsing" \
    '(match (smelter.stdlib.json:parse-json "true") ((Ok _) "SUCCESS") ((Err _) "FAILED"))' \
    "SUCCESS"

# Test JSON null parsing
test_json_eval "JSON null parsing" \
    '(match (smelter.stdlib.json:parse-json "null") ((Ok _) "SUCCESS") ((Err _) "FAILED"))' \
    "SUCCESS"

# Test invalid JSON handling
test_json_eval "Invalid JSON error handling" \
    '(match (smelter.stdlib.json:parse-json "invalid") ((Ok _) "FAILED") ((Err _) "SUCCESS"))' \
    "SUCCESS"

# Test JSON parsing via script mode
echo -e "${BLUE}Testing JSON parsing via script mode...${NC}"
echo

# Test the simple JSON test script
test_json_script "JSON script execution" \
    "test/simple-json-test.coal" \
    "JSON tests completed"

echo -e "${GREEN}🎉 All JSON regression tests passed!${NC}"
echo -e "${GREEN}JSON functionality is working correctly in both eval and script modes.${NC}"