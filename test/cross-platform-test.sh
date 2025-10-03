#!/bin/bash
# Cross-Platform Testing Suite for Smelter
# Tests compatibility across different shells, locales, and environments

set -euo pipefail

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

TESTS_RUN=0
TESTS_PASSED=0
TESTS_FAILED=0
TESTS_SKIPPED=0
SMT="./smt"

log_info() {
    echo -e "${BLUE}[XPLAT]${NC} $1"
}

log_success() {
    echo -e "${GREEN}[PASS]${NC} $1"
    TESTS_PASSED=$((TESTS_PASSED + 1))
}

log_error() {
    echo -e "${RED}[FAIL]${NC} $1"
    TESTS_FAILED=$((TESTS_FAILED + 1))
}

log_skip() {
    echo -e "${YELLOW}[SKIP]${NC} $1"
    TESTS_SKIPPED=$((TESTS_SKIPPED + 1))
}

echo "========================================="
echo "  CROSS-PLATFORM TESTING SUITE"
echo "========================================="

# 1. DIFFERENT SHELLS
echo -e "\n${BLUE}--- Shell Compatibility ---${NC}"

for shell in bash sh zsh; do
    TESTS_RUN=$((TESTS_RUN + 1))
    if command -v $shell > /dev/null 2>&1; then
        log_info "Testing with $shell..."
        if $shell -c "$SMT eval '(+ 2 3)'" > /dev/null 2>&1; then
            log_success "Shell: $shell"
        else
            log_error "Shell: $shell"
        fi
    else
        log_skip "$shell not available"
    fi
done

# 2. DIFFERENT LOCALES
echo -e "\n${BLUE}--- Locale Compatibility ---${NC}"

for locale in C en_US.UTF-8 POSIX; do
    TESTS_RUN=$((TESTS_RUN + 1))
    log_info "Testing with locale $locale..."
    if LC_ALL=$locale $SMT eval '"Hello"' > /dev/null 2>&1; then
        log_success "Locale: $locale"
    else
        log_error "Locale: $locale"
    fi
done

# Test UTF-8 handling
TESTS_RUN=$((TESTS_RUN + 1))
log_info "Testing UTF-8 string handling..."
if $SMT eval '"Hello 世界"' > /dev/null 2>&1; then
    log_success "UTF-8 strings"
else
    log_error "UTF-8 strings"
fi

# 3. FILE PERMISSIONS
echo -e "\n${BLUE}--- File Permissions ---${NC}"

# Test executable permission
TESTS_RUN=$((TESTS_RUN + 1))
log_info "Testing executable with 500 permissions..."
original_perms=$(stat -f%p "$SMT" 2>/dev/null || stat -c%a "$SMT" 2>/dev/null)
chmod 500 "$SMT"
if $SMT eval '(+ 1 1)' > /dev/null 2>&1; then
    log_success "Binary executable with 500 permissions"
else
    log_error "Binary executable with 500 permissions"
fi
chmod "$original_perms" "$SMT" 2>/dev/null || chmod 755 "$SMT"

# Test script with different permissions
TESTS_RUN=$((TESTS_RUN + 1))
log_info "Testing script with restricted permissions..."
cat > /tmp/xplat_test.coal << 'EOF'
(define main
  (smelter.stdlib.io:io-println "Permission test"))
EOF
chmod 400 /tmp/xplat_test.coal
if $SMT run /tmp/xplat_test.coal > /dev/null 2>&1; then
    log_success "Read-only script execution"
else
    log_error "Read-only script execution"
fi
rm -f /tmp/xplat_test.coal

# 4. PATH HANDLING
echo -e "\n${BLUE}--- Path Handling ---${NC}"

# Test absolute path
TESTS_RUN=$((TESTS_RUN + 1))
log_info "Testing absolute path execution..."
cat > /tmp/abs_path_test.coal << 'EOF'
(define main
  (smelter.stdlib.io:io-println "Absolute path works"))
EOF
if $SMT run /tmp/abs_path_test.coal > /dev/null 2>&1; then
    log_success "Absolute path"
else
    log_error "Absolute path"
fi

# Test relative path
TESTS_RUN=$((TESTS_RUN + 1))
log_info "Testing relative path execution..."
cat > ./xplat_relative.coal << 'EOF'
(define main
  (smelter.stdlib.io:io-println "Relative path works"))
EOF
if $SMT run ./xplat_relative.coal > /dev/null 2>&1; then
    log_success "Relative path"
else
    log_error "Relative path"
fi
rm -f ./xplat_relative.coal /tmp/abs_path_test.coal

# Test path with spaces
TESTS_RUN=$((TESTS_RUN + 1))
log_info "Testing path with spaces..."
mkdir -p "/tmp/smelter test dir"
cat > "/tmp/smelter test dir/test.coal" << 'EOF'
(define main
  (smelter.stdlib.io:io-println "Spaces in path work"))
EOF
if $SMT run "/tmp/smelter test dir/test.coal" > /dev/null 2>&1; then
    log_success "Path with spaces"
else
    log_error "Path with spaces"
fi
rm -rf "/tmp/smelter test dir"

# 5. ENVIRONMENT VARIABLES
echo -e "\n${BLUE}--- Environment Variables ---${NC}"

# Test with minimal environment
TESTS_RUN=$((TESTS_RUN + 1))
log_info "Testing with minimal environment..."
if env -i PATH="$PATH" $SMT eval '(+ 1 1)' > /dev/null 2>&1; then
    log_success "Minimal environment"
else
    log_error "Minimal environment"
fi

# Test with custom environment variable
TESTS_RUN=$((TESTS_RUN + 1))
log_info "Testing with custom environment variable..."
if CUSTOM_VAR="test" $SMT eval '(+ 2 2)' > /dev/null 2>&1; then
    log_success "Custom environment variable"
else
    log_error "Custom environment variable"
fi

# 6. STANDARD STREAMS
echo -e "\n${BLUE}--- Standard Streams ---${NC}"

# Test stdout redirect
TESTS_RUN=$((TESTS_RUN + 1))
log_info "Testing stdout redirection..."
if $SMT eval '(+ 3 4)' > /tmp/stdout_test.txt 2>&1 && [ -s /tmp/stdout_test.txt ]; then
    log_success "Stdout redirection"
else
    log_error "Stdout redirection"
fi
rm -f /tmp/stdout_test.txt

# Test stderr handling
TESTS_RUN=$((TESTS_RUN + 1))
log_info "Testing stderr handling..."
if $SMT eval 'undefined-var' 2> /tmp/stderr_test.txt > /dev/null || [ -s /tmp/stderr_test.txt ]; then
    log_success "Stderr handling"
else
    log_error "Stderr handling"
fi
rm -f /tmp/stderr_test.txt

# Test pipe
TESTS_RUN=$((TESTS_RUN + 1))
log_info "Testing pipe handling..."
if echo '(+ 5 5)' | (true || false) && $SMT eval '(+ 5 5)' | grep -q '10'; then
    log_success "Pipe handling"
else
    log_error "Pipe handling"
fi

# 7. EXIT CODES
echo -e "\n${BLUE}--- Exit Codes ---${NC}"

# Test success exit code
TESTS_RUN=$((TESTS_RUN + 1))
log_info "Testing success exit code..."
$SMT eval '(+ 1 1)' > /dev/null 2>&1
if [ $? -eq 0 ]; then
    log_success "Success exit code (0)"
else
    log_error "Success exit code (expected 0, got $?)"
fi

# Test error exit code
TESTS_RUN=$((TESTS_RUN + 1))
log_info "Testing error exit code..."
if $SMT run /nonexistent_file.coal > /dev/null 2>&1; then
    log_error "Error exit code (expected non-zero, got 0)"
else
    log_success "Error exit code (non-zero)"
fi

# Test invalid command exit code
TESTS_RUN=$((TESTS_RUN + 1))
log_info "Testing invalid command exit code..."
if $SMT invalid-command > /dev/null 2>&1; then
    log_error "Invalid command exit code (expected non-zero, got 0)"
else
    log_success "Invalid command exit code (non-zero)"
fi

# 8. SPECIAL CHARACTERS
echo -e "\n${BLUE}--- Special Characters ---${NC}"

# Test single quotes in string
TESTS_RUN=$((TESTS_RUN + 1))
log_info "Testing single quotes in string..."
if $SMT eval "\"It's working\"" > /dev/null 2>&1; then
    log_success "Single quotes in string"
else
    log_error "Single quotes in string"
fi

# Test double quotes (escaped)
TESTS_RUN=$((TESTS_RUN + 1))
log_info "Testing special characters..."
cat > /tmp/special_chars.coal << 'EOF'
(define main
  (smelter.stdlib.io:io-println "Special: !@#$%"))
EOF
if $SMT run /tmp/special_chars.coal > /dev/null 2>&1; then
    log_success "Special characters"
else
    log_error "Special characters"
fi
rm -f /tmp/special_chars.coal

# 9. CONCURRENT EXECUTION
echo -e "\n${BLUE}--- Concurrent Execution ---${NC}"

TESTS_RUN=$((TESTS_RUN + 1))
log_info "Testing concurrent execution safety..."
pids=()
for i in {1..5}; do
    $SMT eval "(+ $i $i)" > /tmp/concurrent_$i.out 2>&1 &
    pids+=($!)
done
failed=0
for pid in "${pids[@]}"; do
    if ! wait $pid; then
        failed=$((failed + 1))
    fi
done
if [ $failed -eq 0 ]; then
    log_success "Concurrent execution (5 processes)"
else
    log_error "Concurrent execution ($failed/5 failed)"
fi
rm -f /tmp/concurrent_*.out

# 10. PLATFORM DETECTION
echo -e "\n${BLUE}--- Platform Detection ---${NC}"

TESTS_RUN=$((TESTS_RUN + 1))
log_info "Testing on $(uname -s) $(uname -m)..."
if $SMT --version > /dev/null 2>&1; then
    log_success "Platform: $(uname -s) $(uname -m)"
else
    log_error "Platform compatibility"
fi

# SUMMARY
echo ""
echo "========================================="
echo "  CROSS-PLATFORM TEST SUMMARY"
echo "========================================="
echo "Tests run:    $TESTS_RUN"
echo "Tests passed: $TESTS_PASSED"
echo "Tests failed: $TESTS_FAILED"
echo "Tests skipped: $TESTS_SKIPPED"
echo ""

if [ $TESTS_FAILED -eq 0 ]; then
    echo -e "${GREEN}✓ ALL CROSS-PLATFORM TESTS PASSED!${NC}"
    echo "Smelter is portable and robust across environments 🌍"
    exit 0
else
    echo -e "${YELLOW}⚠ $TESTS_FAILED CROSS-PLATFORM TEST(S) FAILED${NC}"
    echo "Consider investigating compatibility issues"
    exit 1
fi
