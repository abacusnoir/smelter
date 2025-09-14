#!/bin/bash
# Smelter Release Script
# Creates production releases with tests, binary optimization, and packaging

set -e

VERSION=${1:-"0.1.0"}
PLATFORM=$(uname -s | tr '[:upper:]' '[:lower:]')
ARCH=$(uname -m)
RELEASE_NAME="smelter-v${VERSION}-${PLATFORM}-${ARCH}"
RELEASE_DIR="releases/${RELEASE_NAME}"

echo "🔥 Creating Smelter v${VERSION} release..."
echo "Platform: ${PLATFORM}-${ARCH}"

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
BLUE='\033[0;34m'
NC='\033[0m'

log_step() {
    echo -e "${BLUE}→${NC} $1"
}

log_success() {
    echo -e "${GREEN}✓${NC} $1"
}

log_error() {
    echo -e "${RED}✗${NC} $1"
    exit 1
}

# Step 1: Clean and build
log_step "Cleaning and building fresh binary..."
make clean
make build || log_error "Build failed"
log_success "Binary built successfully"

# Step 2: Run comprehensive tests
log_step "Running smoke tests..."
make test || log_error "Tests failed"
log_success "All tests passed"

# Step 3: Check binary size and performance
log_step "Measuring performance metrics..."
BINARY_SIZE=$(du -h smt | cut -f1)
START_TIME=$(date +%s%3N)
./smt eval '(+ 1 2)' > /dev/null
END_TIME=$(date +%s%3N)
STARTUP_TIME=$((END_TIME - START_TIME))

echo "  Binary size: ${BINARY_SIZE}"
echo "  Startup time: ${STARTUP_TIME}ms"
log_success "Performance metrics recorded"

# Step 4: Compress binary (if UPX available)
log_step "Optimizing binary size..."
if command -v upx &> /dev/null; then
    cp smt smt-original
    upx --best smt 2>/dev/null || true
    COMPRESSED_SIZE=$(du -h smt | cut -f1)
    echo "  Compressed size: ${COMPRESSED_SIZE}"
    log_success "Binary compressed with UPX"
else
    echo "  UPX not available, skipping compression"
fi

# Step 5: Create release directory
log_step "Creating release package..."
rm -rf releases
mkdir -p "${RELEASE_DIR}"

# Copy binary and documentation
cp smt "${RELEASE_DIR}/"
cp README.md "${RELEASE_DIR}/" 2>/dev/null || echo "# Smelter v${VERSION}" > "${RELEASE_DIR}/README.md"
cp -r examples "${RELEASE_DIR}/"
cp CLAUDE.md "${RELEASE_DIR}/ARCHITECTURE.md" 2>/dev/null || true

# Create quick start guide
cat > "${RELEASE_DIR}/QUICKSTART.md" << 'EOF'
# Smelter Quick Start

## Installation
```bash
# Make executable
chmod +x smt

# Add to PATH (optional)
sudo mv smt /usr/local/bin/
```

## First Steps
```bash
# Check version
./smt --version

# Try basic evaluation
./smt eval '(+ 2 3)'

# Run an example
./smt run examples/hello-world.smt

# Start REPL
./smt repl
```

## Examples
See the `examples/` directory for comprehensive demos.
EOF

# Step 6: Generate checksums
log_step "Generating checksums..."
cd releases
tar -czf "${RELEASE_NAME}.tar.gz" "${RELEASE_NAME}"
shasum -a 256 "${RELEASE_NAME}.tar.gz" > "${RELEASE_NAME}.tar.gz.sha256"
cd ..

log_success "Release package created: releases/${RELEASE_NAME}.tar.gz"

# Step 7: Display summary
echo ""
echo "🎉 Release Summary:"
echo "=================="
echo "Version: v${VERSION}"
echo "Binary size: ${BINARY_SIZE}"
echo "Startup time: ~${STARTUP_TIME}ms"
echo "Package: releases/${RELEASE_NAME}.tar.gz"
echo ""
echo "Ready for distribution! 🚀"

# Optional: Test the packaged binary
log_step "Testing packaged binary..."
cd "releases/${RELEASE_NAME}"
./smt eval '(* 6 7)' | grep -q "42" && log_success "Package test passed" || log_error "Package test failed"