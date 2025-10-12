#!/bin/bash
# Smelter installation script
# Automatically detects platform and installs the latest release

set -e

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Detect platform
OS="$(uname -s)"
ARCH="$(uname -m)"

echo "🔥 Installing Smelter..."
echo ""

# Determine download URL based on platform
case "$OS" in
    Darwin)
        case "$ARCH" in
            arm64|aarch64)
                PLATFORM="macos-arm64"
                ;;
            x86_64)
                PLATFORM="macos-x64"
                ;;
            *)
                echo -e "${RED}Error: Unsupported macOS architecture: $ARCH${NC}"
                exit 1
                ;;
        esac
        ;;
    Linux)
        case "$ARCH" in
            x86_64)
                PLATFORM="linux-x64"
                ;;
            *)
                echo -e "${RED}Error: Unsupported Linux architecture: $ARCH${NC}"
                echo "Currently only x86_64 is supported."
                exit 1
                ;;
        esac
        ;;
    *)
        echo -e "${RED}Error: Unsupported operating system: $OS${NC}"
        echo "Supported platforms: macOS (Intel/ARM), Linux (x86_64)"
        exit 1
        ;;
esac

echo "Detected platform: $PLATFORM"
echo ""

# Set download URL
REPO="abacusnoir/smelter"
RELEASE_URL="https://github.com/$REPO/releases/latest/download/smelter-$PLATFORM.tar.gz"

# Create temporary directory
TMP_DIR=$(mktemp -d)
trap "rm -rf $TMP_DIR" EXIT

# Download and extract
echo "Downloading Smelter..."
if curl -fsSL "$RELEASE_URL" -o "$TMP_DIR/smelter.tar.gz"; then
    echo -e "${GREEN}✓${NC} Download complete"
else
    echo -e "${RED}Error: Failed to download Smelter${NC}"
    echo "URL: $RELEASE_URL"
    exit 1
fi

echo "Extracting..."
tar -xzf "$TMP_DIR/smelter.tar.gz" -C "$TMP_DIR"

# Find the extracted binary
BINARY=$(find "$TMP_DIR" -name "smelter-*" -type f | head -n 1)
if [ -z "$BINARY" ]; then
    echo -e "${RED}Error: Could not find smelter binary in archive${NC}"
    exit 1
fi

# Determine install location
if [ -w "/usr/local/bin" ]; then
    INSTALL_DIR="/usr/local/bin"
    SUDO=""
elif [ -w "$HOME/.local/bin" ]; then
    INSTALL_DIR="$HOME/.local/bin"
    SUDO=""
    # Create directory if it doesn't exist
    mkdir -p "$INSTALL_DIR"
else
    INSTALL_DIR="/usr/local/bin"
    SUDO="sudo"
    echo -e "${YELLOW}Note: Installing to $INSTALL_DIR requires sudo${NC}"
fi

# Install binary
echo "Installing to $INSTALL_DIR/smt..."
if $SUDO mv "$BINARY" "$INSTALL_DIR/smt" && $SUDO chmod +x "$INSTALL_DIR/smt"; then
    echo -e "${GREEN}✓${NC} Installation complete!"
else
    echo -e "${RED}Error: Failed to install binary${NC}"
    exit 1
fi

# Verify installation
if command -v smt >/dev/null 2>&1; then
    VERSION=$(smt --version 2>&1 | head -n 1 || echo "unknown")
    echo ""
    echo -e "${GREEN}Success!${NC} Smelter is now installed."
    echo "Version: $VERSION"
    echo ""
    echo "Try it out:"
    echo "  smt eval '(+ 2 3)'"
    echo "  smt repl"
    echo "  smt --help"
else
    echo ""
    echo -e "${YELLOW}Warning: 'smt' command not found in PATH${NC}"
    echo "You may need to add $INSTALL_DIR to your PATH:"
    echo "  export PATH=\"$INSTALL_DIR:\$PATH\""
fi
