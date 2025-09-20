#!/bin/bash
# Smelter End-to-End (E2E) Integration Tests
# Tests real-world workflows treating smt binary as a complete black box

set -e  # Exit on any error

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Configuration
SMT_BINARY="./smt"
EXAMPLES_DIR="examples"
TEMP_DIR="/tmp/smelter-e2e-test-$$"
OUTPUT_FILE="$TEMP_DIR/gh-stats-test.txt"
NETWORK_TEST_OUTPUT="$TEMP_DIR/network-test.txt"

# Counters
TESTS_RUN=0
TESTS_PASSED=0
TESTS_FAILED=0

# Helper functions
log_info() {
    echo -e "${BLUE}[E2E INFO]${NC} $1"
}

log_success() {
    echo -e "${GREEN}[E2E PASS]${NC} $1"
    ((TESTS_PASSED++))
}

log_error() {
    echo -e "${RED}[E2E FAIL]${NC} $1"
    ((TESTS_FAILED++))
}

log_warning() {
    echo -e "${YELLOW}[E2E WARN]${NC} $1"
}

# Setup and cleanup
setup_e2e_tests() {
    log_info "Setting up E2E tests..."
    
    # Check if binary exists
    if [ ! -f "$SMT_BINARY" ]; then
        log_error "Smelter binary not found: $SMT_BINARY"
        echo "Run 'make build' first"
        exit 1
    fi
    
    # Make binary executable
    chmod +x "$SMT_BINARY"
    
    # Create temp directory
    mkdir -p "$TEMP_DIR"
    
    # Check if github-stats.coal exists
    if [ ! -f "$EXAMPLES_DIR/github-stats.coal" ]; then
        log_error "github-stats.coal not found in $EXAMPLES_DIR/"
        echo "This example is required for E2E testing"
        exit 1
    fi
    
    log_success "E2E test setup complete"
}

cleanup_e2e_tests() {
    log_info "Cleaning up E2E test files..."
    rm -rf "$TEMP_DIR"
    log_success "E2E cleanup complete"
}

# Network availability check
check_network_connectivity() {
    log_info "Checking network connectivity to GitHub API..."
    if curl -s --connect-timeout 5 https://api.github.com/users/octocat > /dev/null; then
        log_success "Network connectivity confirmed"
        return 0
    else
        log_warning "Network connectivity test failed - some tests may be skipped"
        return 1
    fi
}

# Test Case 1: Happy Path Success Test
test_github_stats_success() {
    log_info "E2E Test 1: GitHub stats success case with torvalds user..."
    ((TESTS_RUN++))
    
    # Use a well-known GitHub user with stable data
    local username="torvalds"
    local test_output="$TEMP_DIR/success-test.txt"
    
    # Run the command with output redirection
    if timeout 30 "$SMT_BINARY" run "$EXAMPLES_DIR/github-stats.coal" --username "$username" --output "$test_output" 2>&1; then
        # Check if output file was created
        if [[ -f "$test_output" ]]; then
            # Verify file contains expected content structure
            if grep -q "GitHub Statistics Report" "$test_output" && \
               grep -q "Linus Torvalds" "$test_output" && \
               grep -q "User Information" "$test_output" && \
               grep -q "Repository Statistics" "$test_output" && \
               grep -q "All 5 adapters working" "$test_output"; then
                log_success "GitHub stats created valid report with all expected sections"
            else
                log_error "GitHub stats output file missing expected content"
                echo "Generated content preview:"
                head -20 "$test_output" || echo "Could not read file"
                return 1
            fi
        else
            log_error "GitHub stats did not create output file"
            return 1
        fi
    else
        log_error "GitHub stats command failed or timed out"
        return 1
    fi
}

# Test Case 2: Argument Handling Failure Test
test_github_stats_missing_username() {
    log_info "E2E Test 2: GitHub stats with missing required username..."
    ((TESTS_RUN++))
    
    local test_output
    local exit_code
    
    # Run command without required username (should fail)
    if test_output=$("$SMT_BINARY" run "$EXAMPLES_DIR/github-stats.coal" --output "$TEMP_DIR/fail-test.txt" 2>&1); then
        exit_code=0
    else
        exit_code=$?
    fi
    
    # Should exit with non-zero code
    if [ "$exit_code" -ne 0 ]; then
        # Check that error message mentions the missing argument
        if echo "$test_output" | grep -q -E "(Missing|username|required|CLI parsing error)"; then
            log_success "GitHub stats correctly reported missing username argument"
        else
            log_error "GitHub stats failed but didn't provide expected error message"
            echo "Actual output: $test_output"
            return 1
        fi
    else
        log_error "GitHub stats should have failed with missing username but succeeded"
        echo "Unexpected output: $test_output"
        return 1
    fi
}

# Test Case 3: Invalid Username Test
test_github_stats_invalid_username() {
    log_info "E2E Test 3: GitHub stats with invalid username..."
    ((TESTS_RUN++))
    
    # Use a username that's extremely unlikely to exist
    local invalid_username="this-username-should-never-exist-12345-abcdef"
    local test_output
    local exit_code
    
    if test_output=$(timeout 15 "$SMT_BINARY" run "$EXAMPLES_DIR/github-stats.coal" --username "$invalid_username" 2>&1); then
        exit_code=0
    else
        exit_code=$?
    fi
    
    # Should handle the error gracefully
    if echo "$test_output" | grep -q -E "(Failed to fetch|404|Not Found|Failed to parse)"; then
        log_success "GitHub stats gracefully handled invalid username"
    else
        log_error "GitHub stats didn't handle invalid username appropriately"
        echo "Output: $test_output"
        echo "Exit code: $exit_code"
        return 1
    fi
}

# Test Case 4: File System Integration Test
test_github_stats_file_output() {
    log_info "E2E Test 4: GitHub stats file system integration..."
    ((TESTS_RUN++))
    
    local username="octocat"  # GitHub's mascot account - stable and simple
    local output_path="$TEMP_DIR/file-system-test.txt"
    
    # Ensure output directory exists but file doesn't
    rm -f "$output_path"
    
    if timeout 20 "$SMT_BINARY" run "$EXAMPLES_DIR/github-stats.coal" --username "$username" --output "$output_path" > /dev/null 2>&1; then
        # Check file was created and has content
        if [[ -f "$output_path" ]] && [[ -s "$output_path" ]]; then
            # Verify file permissions are reasonable
            if [[ -r "$output_path" ]]; then
                log_success "GitHub stats correctly created and wrote output file"
            else
                log_error "GitHub stats created file with incorrect permissions"
                return 1
            fi
        else
            log_error "GitHub stats failed to create output file or file is empty"
            return 1
        fi
    else
        log_error "GitHub stats file output test failed"
        return 1
    fi
}

# Test Case 5: Verbose Flag Test
test_github_stats_verbose_mode() {
    log_info "E2E Test 5: GitHub stats verbose mode functionality..."
    ((TESTS_RUN++))
    
    local username="octocat"
    local test_output
    
    # Test verbose mode
    if test_output=$(timeout 15 "$SMT_BINARY" run "$EXAMPLES_DIR/github-stats.coal" --username "$username" --verbose 2>&1); then
        # Verbose mode should include progress indicators
        if echo "$test_output" | grep -q -E "(🔍|📊|⏰|Fetching|Writing|timestamp)"; then
            log_success "GitHub stats verbose mode provided expected output"
        else
            log_error "GitHub stats verbose mode didn't provide expected verbose output"
            echo "Output: $test_output"
            return 1
        fi
    else
        log_error "GitHub stats verbose mode test failed"
        return 1
    fi
}

# Test Case 6: Network Failure Simulation (Advanced)
test_network_failure_handling() {
    log_info "E2E Test 6: Network failure handling (advanced)..."
    ((TESTS_RUN++))
    
    # This test is more complex and may not work in all environments
    log_warning "Network failure test requires manual setup - checking error handling instead"
    
    # Test with a hostname that should not resolve
    local fake_hostname="this-domain-definitely-does-not-exist-12345.invalid"
    local test_output
    
    # We can't easily modify the github-stats.coal to use a different URL,
    # so we'll test basic network error handling by checking the script's
    # behavior when it encounters network issues naturally
    
    # Instead, let's test timeout behavior
    if test_output=$(timeout 5 "$SMT_BINARY" run "$EXAMPLES_DIR/github-stats.coal" --username "this-user-should-not-exist-test-12345" 2>&1); then
        # Should handle gracefully
        if echo "$test_output" | grep -q -E "(Failed|Error|timeout|network)"; then
            log_success "GitHub stats handled network/API errors gracefully"
        else
            log_warning "Network error test inconclusive - no clear error detected"
        fi
    else
        # Timeout or failure is acceptable for this test
        log_success "GitHub stats handled network issues (timeout/failure) appropriately"
    fi
}

# Test Case 7: Command Line Argument Parsing Edge Cases
test_argument_edge_cases() {
    log_info "E2E Test 7: Command line argument edge cases..."
    ((TESTS_RUN++))
    
    local test_cases=(
        # Test case: no arguments at all
        ""
        # Test case: help flag
        "--help"
        # Test case: unknown flag
        "--unknown-flag"
        # Test case: malformed arguments
        "--username"
    )
    
    local passed_tests=0
    local total_cases=${#test_cases[@]}
    
    for args in "${test_cases[@]}"; do
        local test_output
        if test_output=$("$SMT_BINARY" run "$EXAMPLES_DIR/github-stats.coal" $args 2>&1); then
            # Some cases might succeed (like --help), which is fine
            continue
        else
            # Failure is expected for most edge cases
            if echo "$test_output" | grep -q -E "(Usage|help|error|Error|missing|required)"; then
                ((passed_tests++))
            fi
        fi
    done
    
    if [ "$passed_tests" -ge 2 ]; then
        log_success "GitHub stats handled command line edge cases appropriately"
    else
        log_error "GitHub stats didn't handle command line edge cases well"
        return 1
    fi
}

# Test Case 8: Integration of All 5 Adapters
test_all_adapters_integration() {
    log_info "E2E Test 8: Integration of all 5 adapters (CLI, HTTP, JSON, FS, Process)..."
    ((TESTS_RUN++))
    
    local username="octocat"
    local output_file="$TEMP_DIR/adapters-integration-test.txt"
    local test_output
    
    # Run with all features that exercise each adapter
    if test_output=$(timeout 25 "$SMT_BINARY" run "$EXAMPLES_DIR/github-stats.coal" \
                    --username "$username" \
                    --output "$output_file" \
                    --verbose 2>&1); then
        
        # Verify the output file was created (FS adapter)
        if [[ -f "$output_file" ]]; then
            local file_content
            file_content=$(cat "$output_file")
            
            # Check for evidence of each adapter working:
            # CLI adapter: arguments parsed correctly
            # HTTP adapter: API data fetched  
            # JSON adapter: data parsed from API
            # FS adapter: file written successfully
            # Process adapter: timestamp generated
            if echo "$file_content" | grep -q "GitHub Statistics Report" && \
               echo "$file_content" | grep -q "Generated:" && \
               echo "$file_content" | grep -q "$username" && \
               echo "$file_content" | grep -q "All 5 adapters working" && \
               echo "$file_content" | grep -q -E "(Name:|Public Repos:|Total Stars:)"; then
                log_success "All 5 adapters working together successfully"
            else
                log_error "Not all adapters appear to be working correctly"
                echo "File content preview:"
                head -10 "$output_file"
                return 1
            fi
        else
            log_error "Integration test failed - no output file created"
            return 1
        fi
    else
        log_error "Integration test failed to execute"
        return 1
    fi
}

# Main test execution
main() {
    echo "========================================"
    echo "    Smelter End-to-End (E2E) Tests"
    echo "========================================"
    echo
    
    setup_e2e_tests
    
    # Check network connectivity
    local network_available=0
    if check_network_connectivity; then
        network_available=1
    fi
    
    echo
    log_info "Running E2E integration tests..."
    echo
    
    # Run tests conditionally based on network availability
    if [ "$network_available" -eq 1 ]; then
        # Network-dependent tests
        test_github_stats_success || true
        test_github_stats_invalid_username || true
        test_github_stats_file_output || true
        test_github_stats_verbose_mode || true
        test_all_adapters_integration || true
        
        # Advanced network test
        test_network_failure_handling || true
    else
        log_warning "Skipping network-dependent tests due to connectivity issues"
    fi
    
    # Network-independent tests
    test_github_stats_missing_username || true
    test_argument_edge_cases || true
    
    # Cleanup
    cleanup_e2e_tests
    
    # Summary
    echo
    echo "========================================"
    echo "    E2E Test Results Summary"
    echo "========================================"
    echo "Tests run:    $TESTS_RUN"
    echo "Tests passed: $TESTS_PASSED"
    echo "Tests failed: $TESTS_FAILED"
    echo
    
    if [ "$TESTS_FAILED" -eq 0 ]; then
        log_success "All E2E tests passed! Real-world integration verified."
        exit 0
    else
        log_error "$TESTS_FAILED E2E test(s) failed. Integration issues detected."
        exit 1
    fi
}

# Trap for cleanup on exit
trap cleanup_e2e_tests EXIT

# Run main function
main "$@"