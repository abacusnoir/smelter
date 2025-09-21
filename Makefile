# Smelter (smt) - Self-contained Coalton CLI Runner
# Builds a single executable with embedded SBCL + Coalton

.PHONY: all build test clean install deps help

# Configuration
SBCL := sbcl --non-interactive --no-userinit --no-sysinit
COALTON_VERSION := 0.8.0
SMELTER_VERSION := 0.1.0
TARGET := smt

# Directories
SRC_DIR := src
BUILD_DIR := build
EXAMPLES_DIR := examples
TEST_DIR := test

# Platform detection
UNAME_S := $(shell uname -s)
UNAME_M := $(shell uname -m)

ifeq ($(UNAME_S),Darwin)
    PLATFORM := darwin
    ifeq ($(UNAME_M),arm64)
        ARCH := arm64
    else
        ARCH := x64
    endif
else ifeq ($(UNAME_S),Linux)
    PLATFORM := linux
    ARCH := x64
endif

BINARY_NAME := $(TARGET)-$(PLATFORM)-$(ARCH)

# Default target
all: build

# Install dependencies (Quicklisp setup)
deps:
	@echo "Setting up dependencies..."
	@if [ ! -d ~/quicklisp ]; then \
		echo "Installing Quicklisp..."; \
		curl -O https://beta.quicklisp.org/quicklisp.lisp; \
		$(SBCL) --load quicklisp.lisp \
		        --eval "(quicklisp-quickstart:install)" \
		        --eval "(ql:add-to-init-file)" \
		        --quit; \
		rm -f quicklisp.lisp; \
	fi
	@echo "Loading Coalton to cache..."
	@$(SBCL) --eval "(load \"~/quicklisp/setup.lisp\")" --eval "(ql:quickload :coalton)" --quit
	@echo "Dependencies ready!"

# Create the SBCL core with Coalton embedded
$(BUILD_DIR)/smelter.core: $(BUILD_DIR)/create-image.lisp deps
	@echo "Building Smelter core with embedded Coalton..."
	@mkdir -p $(BUILD_DIR)
	@$(SBCL) --load $(BUILD_DIR)/create-image.lisp

# Build the executable binary
$(TARGET): $(BUILD_DIR)/smelter.core $(SRC_DIR)/cli.lisp
	@echo "Creating executable binary..."
	@sbcl --core $(BUILD_DIR)/smelter.core \
	      --non-interactive --no-userinit --no-sysinit \
	      --eval "(load \"$(SRC_DIR)/cli.lisp\")" \
	      --eval "(smelter:save-executable \"$(TARGET)\")"
	@echo "Built: $(TARGET) ($(shell du -h $(TARGET) | cut -f1))"

# Main build target
build: $(TARGET)

# Compress binary (optional - requires UPX)
compress: $(TARGET)
	@if command -v upx >/dev/null 2>&1; then \
		echo "Compressing binary with UPX..."; \
		upx --best $(TARGET); \
		echo "Compressed: $(TARGET) ($(shell du -h $(TARGET) | cut -f1))"; \
	else \
		echo "UPX not found, skipping compression"; \
	fi

# Run smoke tests
test: $(TARGET) $(EXAMPLES_DIR)/hello.coal $(TEST_DIR)/smoke-test.sh
	@echo "Running smoke tests..."
	@chmod +x $(TEST_DIR)/smoke-test.sh
	@$(TEST_DIR)/smoke-test.sh

# Test individual commands
test-version: $(TARGET)
	@./$(TARGET) --version

test-help: $(TARGET)
	@./$(TARGET) --help

test-eval: $(TARGET)
	@./$(TARGET) eval '(+ 2 3)'

test-repl: $(TARGET)
	@echo '(+ 1 2 3)' | ./$(TARGET) repl

test-run: $(TARGET) $(EXAMPLES_DIR)/hello.coal
	@./$(TARGET) run $(EXAMPLES_DIR)/hello.coal

test-shebang: $(TARGET) $(EXAMPLES_DIR)/hello.coal
	@chmod +x $(EXAMPLES_DIR)/hello.coal
	@$(EXAMPLES_DIR)/hello.coal

# Comprehensive regression test suite
test-regression: $(TARGET)
	@echo "Running comprehensive regression tests..."
	@chmod +x $(TEST_DIR)/regression/run-regression-tests.sh
	@$(TEST_DIR)/regression/run-regression-tests.sh

# End-to-End integration test suite
test-e2e: $(TARGET)
	@echo "Running End-to-End integration tests..."
	@chmod +x $(TEST_DIR)/e2e-test.sh
	@$(TEST_DIR)/e2e-test.sh

# Install adapter dependencies
install-deps:
	@echo "Installing adapter dependencies..."
	@$(SBCL) --eval "(load \"~/quicklisp/setup.lisp\")" \
	         --eval "(ql:quickload '(drakma st-json split-sequence cl-ppcre flexi-streams uiop))" \
	         --quit

# Build Smelter with adapters included
build-adapters: install-deps
	@echo "Building Smelter with adapters..."
	@$(SBCL) --eval "(load \"~/quicklisp/setup.lisp\")" \
	         --load $(BUILD_DIR)/build-adapters.lisp \
	         --eval "(build-adapters-image)" \
	         --quit

# Test adapter functionality
test-adapters: build-adapters
	@echo "Testing adapter functionality..."
	@chmod +x $(TEST_DIR)/adapter-tests.coal
	@./$(TARGET) run $(TEST_DIR)/adapter-tests.coal

# Test GitHub stats example
test-example: build-adapters
	@echo "Testing GitHub stats example..."
	@chmod +x $(EXAMPLES_DIR)/github-stats.coal
	@./$(TARGET) run $(EXAMPLES_DIR)/github-stats.coal --username torvalds --verbose --output /tmp/test-stats.txt

# Skip build and run regression tests (for development)
test-regression-quick:
	@echo "Running regression tests (skipping rebuild)..."
	@chmod +x $(TEST_DIR)/regression/run-regression-tests.sh
	@SKIP_BUILD=1 $(TEST_DIR)/regression/run-regression-tests.sh

# Full test suite (smoke + regression + e2e)
test-all: test test-regression test-e2e test-csv

# CSV library tests
test-csv: $(TARGET)
	@echo "Running CSV library tests..."
	@./$(TARGET) run $(TEST_DIR)/csv-test.coal

test-csv-example: $(TARGET)
	@echo "Running CSV report generator example..."
	@mkdir -p examples/data
	@./$(TARGET) run $(EXAMPLES_DIR)/csv-report-generator.coal --input examples/data/sales_data.csv --output /tmp/sales_report.csv
	@echo "✅ Example run complete. Check the report at /tmp/sales_report.csv"

# Clean build artifacts
clean:
	@echo "Cleaning build artifacts..."
	@rm -f $(TARGET) $(TARGET)-*
	@rm -rf $(BUILD_DIR)/*.core
	@rm -rf $(BUILD_DIR)/*.tmp

# Install to /usr/local/bin (requires sudo)
install: $(TARGET)
	@echo "Installing $(TARGET) to /usr/local/bin..."
	@sudo cp $(TARGET) /usr/local/bin/
	@echo "Installed! Try: smt --version"

# Create release archive
release: $(TARGET) compress
	@echo "Creating release archive..."
	@mkdir -p releases
	@tar -czf releases/$(BINARY_NAME).tar.gz $(TARGET)
	@echo "Created: releases/$(BINARY_NAME).tar.gz"

# Performance-optimized builds
compile-all:
	@echo "Pre-compiling all sources to FASL..."
	@sbcl --no-userinit --no-sysinit \
	      --load build/fast-core.lisp \
	      --eval "(smelter-fast::compile-all-sources)" \
	      --quit

build-fast: compile-all
	@echo "Building fast Smelter binary..."
	@sbcl --no-userinit --no-sysinit \
	      --load build/fast-core.lisp \
	      --eval "(smelter-fast::build-fast-image)" \
	      --quit
	@echo "Optimizing binary..."
	@strip smt-fast
	@if command -v upx >/dev/null 2>&1; then \
		upx --best smt-fast; \
	fi
	@echo "Binary created: smt-fast"
	@ls -lh smt-fast
	@echo "Testing startup time..."
	@time ./smt-fast eval '(+ 1 2)'

build-minimal:
	@echo "Building minimal test binary..."
	@sbcl --no-userinit --no-sysinit \
	      --load build/fast-core-simple.lisp \
	      --eval "(build-minimal-test-image)" \
	      --quit
	@strip smt-minimal
	@upx --best smt-minimal 2>/dev/null || true
	@echo "Minimal binary created: smt-minimal"
	@ls -lh smt-minimal
	@echo "Testing minimal startup..."
	@time ./smt-minimal eval '(+ 1 2)'

test-performance: build-fast build-minimal
	@echo "=== PERFORMANCE COMPARISON ==="
	@printf "%-15s | %s\n" "Binary" "Startup Time (avg of 5 runs)"
	@printf "%-15s | %s\n" "---------------" "------------------------"
	@printf "%-15s | " "smt-original"
	@bash -c 'total=0; for i in {1..5}; do start=$$(date +%s%N); ./smt eval "(+ 1 2)" >/dev/null 2>&1; end=$$(date +%s%N); ms=$$(echo "scale=1; ($${end} - $${start}) / 1000000" | bc); total=$$(echo "$${total} + $${ms}" | bc); done; echo "scale=1; $${total} / 5" | bc'
	@printf "%-15s | " "smt-fast"
	@bash -c 'total=0; for i in {1..5}; do start=$$(date +%s%N); ./smt-fast eval "(+ 1 2)" >/dev/null 2>&1; end=$$(date +%s%N); ms=$$(echo "scale=1; ($${end} - $${start}) / 1000000" | bc); total=$$(echo "$${total} + $${ms}" | bc); done; echo "scale=1; $${total} / 5" | bc'
	@printf "%-15s | " "smt-minimal"
	@bash -c 'total=0; for i in {1..5}; do start=$$(date +%s%N); ./smt-minimal eval "(+ 1 2)" >/dev/null 2>&1; end=$$(date +%s%N); ms=$$(echo "scale=1; ($${end} - $${start}) / 1000000" | bc); total=$$(echo "$${total} + $${ms}" | bc); done; echo "scale=1; $${total} / 5" | bc'

# Development: quick rebuild for testing
dev: clean build test-version test-eval

# Show build info
info:
	@echo "Smelter Build Information"
	@echo "========================"
	@echo "Version: $(SMELTER_VERSION)"
	@echo "Platform: $(PLATFORM)-$(ARCH)"
	@echo "SBCL: $(shell sbcl --version 2>/dev/null || echo 'not found')"
	@echo "Target Binary: $(TARGET)"
	@echo ""
	@echo "Build Targets:"
	@echo "  make build              - Build the executable"
	@echo "  make test               - Run smoke tests"
	@echo "  make test-regression    - Run comprehensive regression tests"
	@echo "  make test-e2e           - Run End-to-End integration tests"
	@echo "  make test-regression-quick - Run regression tests (skip rebuild)"
	@echo "  make test-all           - Run all tests (smoke + regression + e2e)"
	@echo "  make install-deps       - Install adapter dependencies"
	@echo "  make build-adapters     - Build Smelter with adapters"
	@echo "  make test-adapters      - Test adapter functionality"
	@echo "  make test-example       - Test GitHub stats example"
	@echo "  make install            - Install to /usr/local/bin"
	@echo "  make clean              - Clean build artifacts"

# Help target
help: info

# Ensure required directories exist
$(BUILD_DIR) $(EXAMPLES_DIR) $(TEST_DIR):
	@mkdir -p $@

# Dependencies for directory creation
$(BUILD_DIR)/create-image.lisp: | $(BUILD_DIR)
$(EXAMPLES_DIR)/hello.coal: | $(EXAMPLES_DIR)
$(TEST_DIR)/smoke-test.sh: | $(TEST_DIR)