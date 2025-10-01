#!/bin/bash
# Smelter Smoke Tests
# Comprehensive verification that the smt binary works correctly

set -e  # Exit on any error

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Configuration
SMT_BINARY="./smt"
TEST_DIR="test"
EXAMPLES_DIR="examples"
TEMP_DIR="/tmp/smelter-test-$$"

# Counters
TESTS_RUN=0
TESTS_PASSED=0
TESTS_FAILED=0

# Helper functions
log_info() {
    echo -e "${BLUE}[INFO]${NC} $1"
}

log_success() {
    echo -e "${GREEN}[PASS]${NC} $1"
    ((TESTS_PASSED++)) || true
}

log_error() {
    echo -e "${RED}[FAIL]${NC} $1"
    ((TESTS_FAILED++)) || true
}

log_warning() {
    echo -e "${YELLOW}[WARN]${NC} $1"
}

run_test() {
    local test_name="$1"
    local test_command="$2"
    local expected_exit_code="${3:-0}"

    ((TESTS_RUN++)) || true
    
    log_info "Running test: $test_name"
    
    # Run the command and capture output and exit code
    local output
    local exit_code
    
    if output=$(eval "$test_command" 2>&1); then
        exit_code=0
    else
        exit_code=$?
    fi
    
    # Check exit code
    if [ "$exit_code" -eq "$expected_exit_code" ]; then
        log_success "$test_name"
        return 0
    else
        log_error "$test_name (exit code: $exit_code, expected: $expected_exit_code)"
        echo "Output: $output"
        return 1
    fi
}

run_test_with_output() {
    local test_name="$1"
    local test_command="$2"
    local expected_pattern="$3"

    ((TESTS_RUN++)) || true
    
    log_info "Running test: $test_name"
    
    local output
    if output=$(eval "$test_command" 2>&1); then
        if echo "$output" | grep -q "$expected_pattern"; then
            log_success "$test_name"
            return 0
        else
            log_error "$test_name (output doesn't match pattern: $expected_pattern)"
            echo "Actual output: $output"
            return 1
        fi
    else
        log_error "$test_name (command failed)"
        echo "Output: $output"
        return 1
    fi
}

# Setup
setup_tests() {
    log_info "Setting up smoke tests..."
    
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
    
    # Ensure examples directory exists
    if [ ! -d "$EXAMPLES_DIR" ]; then
        log_warning "Examples directory not found, creating minimal examples"
        mkdir -p "$EXAMPLES_DIR"
    fi
    
    # Ensure hello.coal exists
    if [ ! -f "$EXAMPLES_DIR/hello.coal" ]; then
        log_warning "hello.coal not found, creating minimal version"
        cat > "$EXAMPLES_DIR/hello.coal" << 'EOF'
#!/usr/bin/env smt run
(coalton-toplevel
  (declare test-add (Integer -> Integer -> Integer))
  (define (test-add x y) (+ x y)))

(defun main ()
  (format t "Hello from Smelter!~%")
  (format t "2 + 3 = ~A~%" (coalton:coalton (test-add 2 3))))
EOF
        chmod +x "$EXAMPLES_DIR/hello.coal"
    fi
    
    log_success "Test setup complete"
}

# Test functions
test_version() {
    run_test_with_output "Version command" "$SMT_BINARY --version" "Smelter"
}

test_help() {
    run_test_with_output "Help command" "$SMT_BINARY --help" "Usage:"
}

test_help_short() {
    run_test_with_output "Help command (short)" "$SMT_BINARY -h" "Usage:"
}

test_no_args() {
    run_test_with_output "No arguments (should show help)" "$SMT_BINARY" "Usage:"
}

test_invalid_command() {
    run_test "Invalid command" "$SMT_BINARY invalid-command" 1
}

test_eval_simple() {
    run_test_with_output "Simple eval" "$SMT_BINARY eval '(+ 2 3)'" "5"
}

test_eval_multiplication() {
    run_test_with_output "Eval multiplication" "$SMT_BINARY eval '(* 6 7)'" "42"
}

test_eval_missing_arg() {
    run_test "Eval without expression" "$SMT_BINARY eval" 1
}

test_run_hello() {
    run_test_with_output "Run hello.coal" "$SMT_BINARY run $EXAMPLES_DIR/hello.coal" "Hello from Smelter"
}

test_run_missing_file() {
    run_test "Run non-existent file" "$SMT_BINARY run /nonexistent/file.coal" 1
}

test_run_missing_arg() {
    run_test "Run without file argument" "$SMT_BINARY run" 1
}

test_shebang_execution() {
    if [ -x "$EXAMPLES_DIR/hello.coal" ]; then
        run_test_with_output "Shebang execution" "$EXAMPLES_DIR/hello.coal" "Hello from Smelter"
    else
        log_warning "Skipping shebang test - hello.coal not executable"
    fi
}

test_repl_batch() {
    log_info "Testing REPL with batch input"
    ((TESTS_RUN++))
    
    # Test REPL with echo input
    local output
    if output=$(echo '(+ 1 2 3)' | timeout 5 "$SMT_BINARY" repl 2>&1); then
        if echo "$output" | grep -q "6"; then
            log_success "REPL batch input"
        else
            log_error "REPL batch input (unexpected output)"
            echo "Output: $output"
        fi
    else
        log_error "REPL batch input (command failed or timed out)"
    fi
}

test_binary_properties() {
    log_info "Checking binary properties..."
    
    # Check binary size
    local size=$(stat -f%z "$SMT_BINARY" 2>/dev/null || stat -c%s "$SMT_BINARY" 2>/dev/null)
    local size_mb=$((size / 1024 / 1024))
    
    if [ "$size_mb" -lt 100 ]; then
        log_success "Binary size reasonable: ${size_mb}MB"
    else
        log_warning "Binary size large: ${size_mb}MB (expected <50MB)"
    fi
    
    # Check if binary is executable
    if [ -x "$SMT_BINARY" ]; then
        log_success "Binary is executable"
    else
        log_error "Binary is not executable"
    fi
    
    # Check startup time (basic measurement)
    log_info "Measuring startup time..."
    local start_time=$(date +%s%N)
    "$SMT_BINARY" --version > /dev/null
    local end_time=$(date +%s%N)
    local duration_ms=$(( (end_time - start_time) / 1000000 ))
    
    if [ "$duration_ms" -lt 1000 ]; then
        log_success "Startup time acceptable: ${duration_ms}ms"
    else
        log_warning "Startup time slow: ${duration_ms}ms (target <100ms)"
    fi
}

# Create additional test files
create_test_files() {
    log_info "Creating additional test files..."
    
    # Create a simple arithmetic test
    cat > "$TEMP_DIR/arithmetic.coal" << 'EOF'
(coalton-toplevel
  (declare square (Integer -> Integer))
  (define (square x) (* x x)))

(defun main ()
  (format t "~A~%" (coalton:coalton (square 8))))
EOF
    
    # Create a fibonacci test
    cat > "$TEMP_DIR/fibonacci.coal" << 'EOF'
(coalton-toplevel
  (declare fib (Integer -> Integer))
  (define (fib n)
    (if (<= n 1) n
        (+ (fib (- n 1)) (fib (- n 2))))))

(defun main ()
  (format t "~A~%" (coalton:coalton (fib 10))))
EOF
}

test_additional_scripts() {
    create_test_files
    
    run_test_with_output "Arithmetic script" "$SMT_BINARY run $TEMP_DIR/arithmetic.coal" "64"
    run_test_with_output "Fibonacci script" "$SMT_BINARY run $TEMP_DIR/fibonacci.coal" "55"
}

# Cleanup
cleanup_tests() {
    log_info "Cleaning up test files..."
    rm -rf "$TEMP_DIR"
    log_success "Cleanup complete"
}

# Main test execution
main() {
    echo "========================================"
    echo "    Smelter Smoke Tests"
    echo "========================================"
    echo
    
    setup_tests
    
    # Run all tests
    echo
    log_info "Running basic command tests..."
    test_version
    test_help
    test_help_short
    test_no_args
    test_invalid_command
    
    echo
    log_info "Running eval tests..."
    test_eval_simple
    test_eval_multiplication
    test_eval_missing_arg
    
    echo
    log_info "Running script execution tests..."
    test_run_hello
    test_run_missing_file
    test_run_missing_arg
    test_shebang_execution
    
    echo
    log_info "Running REPL tests..."
    test_repl_batch
    
    echo
    log_info "Running additional script tests..."
    test_additional_scripts
    
    echo
    log_info "Checking binary properties..."
    test_binary_properties
    
    # Cleanup
    cleanup_tests
    
    # Summary
    echo
    echo "========================================"
    echo "    Test Results Summary"
    echo "========================================"
    echo "Tests run:    $TESTS_RUN"
    echo "Tests passed: $TESTS_PASSED"
    echo "Tests failed: $TESTS_FAILED"
    echo
    
    if [ "$TESTS_FAILED" -eq 0 ]; then
        log_success "All tests passed! Smelter is working correctly."
        exit 0
    else
        log_error "$TESTS_FAILED test(s) failed. Please check the output above."
        exit 1
    fi
}

# Run main function
main "$@"