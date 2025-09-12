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
	@$(SBCL) --core $(BUILD_DIR)/smelter.core \
	         --non-interactive \
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
	@echo "  make build     - Build the executable"
	@echo "  make test      - Run all tests"
	@echo "  make install   - Install to /usr/local/bin"
	@echo "  make clean     - Clean build artifacts"

# Help target
help: info

# Ensure required directories exist
$(BUILD_DIR) $(EXAMPLES_DIR) $(TEST_DIR):
	@mkdir -p $@

# Dependencies for directory creation
$(BUILD_DIR)/create-image.lisp: | $(BUILD_DIR)
$(EXAMPLES_DIR)/hello.coal: | $(EXAMPLES_DIR)
$(TEST_DIR)/smoke-test.sh: | $(TEST_DIR)