# Smelter Makefile

.PHONY: all build test test-eval test-json clean clean-all deps install compress release release-build test-release release-all info help

# Version and build info
VERSION := $(shell git describe --tags --exact-match 2>/dev/null || echo "0.1.0")
BUILD_DATE := $(shell date -u +%Y-%m-%dT%H:%M:%S)
COMMIT := $(shell git rev-parse --short HEAD 2>/dev/null || echo "unknown")

# Platform detection
UNAME_S := $(shell uname -s)
UNAME_M := $(shell uname -m)

ifeq ($(UNAME_S),Darwin)
    PLATFORM := darwin
endif
ifeq ($(UNAME_S),Linux)
    PLATFORM := linux
endif

ifeq ($(UNAME_M),x86_64)
    ARCH := x64
endif
ifeq ($(UNAME_M),arm64)
    ARCH := arm64
endif
ifeq ($(UNAME_M),aarch64)
    ARCH := arm64
endif

# Binary naming
BINARY_NAME := smt-$(PLATFORM)-$(ARCH)
RELEASE_DIR := dist

SBCL := sbcl --non-interactive --no-userinit --no-sysinit
TARGET := smt

# Default target
all: build

# Install dependencies
deps:
	@echo "Installing Quicklisp and Coalton..."
	@if [ ! -d ~/quicklisp ]; then \
		curl -O https://beta.quicklisp.org/quicklisp.lisp; \
		$(SBCL) --load quicklisp.lisp --eval "(quicklisp-quickstart:install)" --eval "(ql:add-to-init-file)" --quit; \
		rm -f quicklisp.lisp; \
	fi
	@$(SBCL) --eval '(load "~/quicklisp/setup.lisp")' --eval "(ql:quickload :coalton)" --quit
	@echo "Dependencies ready."

# Build the smt executable
build: deps
	@echo "Building Smelter executable..."
	@$(SBCL) --load build/create-image.lisp
	@echo "✅ Build completed!"

# Run tests
test: $(TARGET)
	@echo "Running smoke tests..."
	@./test/smoke-test.sh
	@echo "Running eval regression tests..."
	@./test/eval-regression.sh

# Run eval regression tests
test-eval: $(TARGET)
	@echo "Running eval mode regression tests..."
	@./test/eval-regression.sh

# Run JSON regression tests
test-json: $(TARGET)
	@echo "Running JSON functionality tests..."
	@./test/json-regression.sh

# Clean build artifacts
clean:
	@echo "Cleaning build artifacts..."
	@rm -f $(TARGET)
	@rm -rf $(RELEASE_DIR)

# Clean everything including cache
clean-all: clean
	@echo "Cleaning all artifacts including cache..."
	@rm -rf ~/.cache/common-lisp/

# Development cycle: clean, build, test
dev: clean build test

# Install to /usr/local/bin
install: $(TARGET)
	@echo "Installing $(TARGET) to /usr/local/bin..."
	@cp $(TARGET) /usr/local/bin/
	@echo "✅ Installation completed!"

# Compress binary with UPX (optional)
compress: $(TARGET)
	@echo "Compressing binary with UPX..."
	@upx --best $(TARGET)
	@echo "✅ Compression completed!"

# Release build with proper naming
release-build:
	@echo "Creating release build for $(PLATFORM)-$(ARCH)..."
	@mkdir -p $(RELEASE_DIR)
	$(MAKE) build
	@cp $(TARGET) $(RELEASE_DIR)/$(BINARY_NAME)
	@echo "✅ Release binary created: $(RELEASE_DIR)/$(BINARY_NAME)"

# Test the release binary
test-release: release-build
	@echo "Testing release binary..."
	@$(RELEASE_DIR)/$(BINARY_NAME) --version
	@$(RELEASE_DIR)/$(BINARY_NAME) eval '1'
	@echo "✅ Release binary test passed!"

# Create distribution package
release: test-release
	@echo "Creating release package..."
	@cd $(RELEASE_DIR) && tar -czf $(BINARY_NAME).tar.gz $(BINARY_NAME)
	@cd $(RELEASE_DIR) && shasum -a 256 $(BINARY_NAME).tar.gz > $(BINARY_NAME).tar.gz.sha256
	@echo "✅ Release package ready:"
	@ls -la $(RELEASE_DIR)/$(BINARY_NAME)*

# Build for all supported platforms (requires cross-compilation setup)
release-all:
	@echo "Building for all platforms..."
	# Note: This requires platform-specific SBCL binaries
	# For now, build on each platform separately
	$(MAKE) release
	@echo "✅ Multi-platform build complete"

# Show build info
info:
	@echo "Smelter Build System"
	@echo "Version: $(VERSION)"
	@echo "Platform: $(PLATFORM)-$(ARCH)"
	@echo "Binary: $(BINARY_NAME)"
	@echo "SBCL: $(shell which sbcl)"
	@echo "Build Date: $(BUILD_DATE)"
	@echo "Commit: $(COMMIT)"

# Help
help:
	@echo "Smelter Build System"
	@echo "==================="
	@echo ""
	@echo "Main targets:"
	@echo "  make build      - Build the smt executable"
	@echo "  make test       - Run comprehensive tests (smoke + eval regression)"
	@echo "  make test-eval  - Run eval mode regression tests only"
	@echo "  make test-json  - Run JSON functionality tests only"
	@echo "  make clean      - Clean build artifacts"
	@echo "  make deps       - Install dependencies (Quicklisp and Coalton)"
	@echo ""
	@echo "Development:"
	@echo "  make dev        - Quick rebuild cycle (clean + build + test)"
	@echo "  make info       - Show build environment information"
	@echo ""
	@echo "Distribution:"
	@echo "  make install      - Install to /usr/local/bin"
	@echo "  make compress     - Compress binary with UPX (optional)"
	@echo "  make release-build - Create platform-specific release binary"
	@echo "  make test-release  - Test the release binary"
	@echo "  make release       - Create release package with checksum"
	@echo "  make release-all   - Build for all supported platforms"
	@echo ""
	@echo "Maintenance:"
	@echo "  make clean-all  - Clean all artifacts including cache"
	@echo "  make help       - Show this help"