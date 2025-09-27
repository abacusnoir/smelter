# Smelter Makefile

.PHONY: all build test test-eval clean clean-all deps install compress release help

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

# Clean build artifacts
clean:
	@echo "Cleaning build artifacts..."
	@rm -f $(TARGET)

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

# Create release archive
release: $(TARGET)
	@echo "Creating release archive..."
	@tar -czf smelter-$(shell ./$(TARGET) --version | head -1 | cut -d' ' -f2).tar.gz $(TARGET) README.md examples/
	@echo "✅ Release archive created!"

# Help
help:
	@echo "Smelter Build System"
	@echo "==================="
	@echo ""
	@echo "Main targets:"
	@echo "  make build     - Build the smt executable"
	@echo "  make test      - Run comprehensive tests (smoke + eval regression)"
	@echo "  make test-eval - Run eval mode regression tests only"
	@echo "  make clean     - Clean build artifacts"
	@echo "  make deps      - Install dependencies (Quicklisp and Coalton)"
	@echo ""
	@echo "Development:"
	@echo "  make dev       - Quick rebuild cycle (clean + build + test)"
	@echo ""
	@echo "Distribution:"
	@echo "  make install   - Install to /usr/local/bin"
	@echo "  make compress  - Compress binary with UPX (optional)"
	@echo "  make release   - Create release archive"
	@echo ""
	@echo "Maintenance:"
	@echo "  make clean-all - Clean all artifacts including cache"
	@echo "  make help      - Show this help"