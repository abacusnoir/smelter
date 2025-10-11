#!/bin/bash
# Smelter Installation Script
# Automatically downloads and installs the latest Smelter binary

set -e

# Configuration
REPO_OWNER="abacusnoir"  # My Github organization
REPO_NAME="smelter"
INSTALL_DIR="$HOME/.local/bin"
BINARY_NAME="smt"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Helper functions
log_info() {
    echo -e "${BLUE}[INFO]${NC} $1"
}

log_success() {
    echo -e "${GREEN}[SUCCESS]${NC} $1"
}

log_error() {
    echo -e "${RED}[ERROR]${NC} $1"
}

log_warning() {
    echo -e "${YELLOW}[WARNING]${NC} $1"
}

# Detect platform and architecture
detect_platform() {
    local os=$(uname -s | tr '[:upper:]' '[:lower:]')
    local arch=$(uname -m)
    
    case "$os" in
        darwin)
            PLATFORM="darwin"
            ;;
        linux)
            PLATFORM="linux"
            ;;
        *)
            log_error "Unsupported operating system: $os"
            exit 1
            ;;
    esac
    
    case "$arch" in
        x86_64|amd64)
            ARCH="x64"
            ;;
        arm64|aarch64)
            ARCH="arm64"
            ;;
        *)
            log_error "Unsupported architecture: $arch"
            log_error "Supported architectures: x86_64, amd64, arm64, aarch64"
            exit 1
            ;;
    esac
    
    BINARY_TARGET="${BINARY_NAME}-${PLATFORM}-${ARCH}"
    log_info "Detected platform: $PLATFORM-$ARCH"
}

# Check prerequisites
check_prerequisites() {
    log_info "Checking prerequisites..."
    
    # Check if curl or wget is available
    if ! command -v curl >/dev/null 2>&1 && ! command -v wget >/dev/null 2>&1; then
        log_error "Neither curl nor wget found. Please install one of them."
        exit 1
    fi
    
    # Check if tar is available
    if ! command -v tar >/dev/null 2>&1; then
        log_error "tar not found. Please install tar."
        exit 1
    fi

    log_success "Prerequisites check passed"
}

# Get latest release version
get_latest_version() {
    log_info "Fetching latest release information..."
    
    local api_url="https://api.github.com/repos/${REPO_OWNER}/${REPO_NAME}/releases/latest"
    
    if command -v curl >/dev/null 2>&1; then
        LATEST_VERSION=$(curl -s "$api_url" | grep -o '"tag_name": "v[^"]*"' | cut -d'"' -f4)
    elif command -v wget >/dev/null 2>&1; then
        LATEST_VERSION=$(wget -qO- "$api_url" | grep -o '"tag_name": "v[^"]*"' | cut -d'"' -f4)
    fi
    
    if [ -z "$LATEST_VERSION" ]; then
        log_error "Failed to fetch latest version. Please check your internet connection."
        exit 1
    fi
    
    log_info "Latest version: $LATEST_VERSION"
}

# Download and install binary
download_and_install() {
    local download_url="https://github.com/${REPO_OWNER}/${REPO_NAME}/releases/download/${LATEST_VERSION}/${BINARY_TARGET}.tar.gz"
    local temp_dir=$(mktemp -d)
    local archive_path="$temp_dir/${BINARY_TARGET}.tar.gz"
    
    log_info "Downloading $BINARY_TARGET..."
    log_info "URL: $download_url"
    
    # Download the archive
    if command -v curl >/dev/null 2>&1; then
        if ! curl -L -o "$archive_path" "$download_url"; then
            log_error "Failed to download binary"
            rm -rf "$temp_dir"
            exit 1
        fi
    elif command -v wget >/dev/null 2>&1; then
        if ! wget -O "$archive_path" "$download_url"; then
            log_error "Failed to download binary"
            rm -rf "$temp_dir"
            exit 1
        fi
    fi
    
    log_success "Download completed"
    
    # Extract the archive
    log_info "Extracting archive..."
    if ! tar -xzf "$archive_path" -C "$temp_dir"; then
        log_error "Failed to extract archive"
        rm -rf "$temp_dir"
        exit 1
    fi
    
    # Find the binary in the extracted files
    local binary_path="$temp_dir/$BINARY_TARGET"
    if [ ! -f "$binary_path" ]; then
        # Try without the target suffix
        binary_path="$temp_dir/$BINARY_NAME"
        if [ ! -f "$binary_path" ]; then
            # List what's actually in the archive for debugging
            log_error "Binary not found in archive. Contents:"
            ls -la "$temp_dir/" || true
            log_error "Expected: $BINARY_TARGET or $BINARY_NAME"
            rm -rf "$temp_dir"
            exit 1
        fi
    fi
    
    # Make binary executable
    chmod +x "$binary_path"

    # Install the binary
    log_info "Installing to $INSTALL_DIR..."

    # Create install directory if it doesn't exist
    mkdir -p "$INSTALL_DIR"

    # Copy binary (no sudo needed for home directory)
    cp "$binary_path" "$INSTALL_DIR/$BINARY_NAME"
    
    # Cleanup
    rm -rf "$temp_dir"
    
    log_success "Installation completed"
}

# Verify installation
verify_installation() {
    log_info "Verifying installation..."

    if ! command -v "$BINARY_NAME" >/dev/null 2>&1; then
        log_warning "$BINARY_NAME not found in PATH."
        echo
        log_info "Add $INSTALL_DIR to your PATH:"
        echo

        # Detect shell and provide appropriate instructions
        if [ -n "$BASH_VERSION" ]; then
            echo "  echo 'export PATH=\"$INSTALL_DIR:\$PATH\"' >> ~/.bashrc"
            echo "  source ~/.bashrc"
        elif [ -n "$ZSH_VERSION" ]; then
            echo "  echo 'export PATH=\"$INSTALL_DIR:\$PATH\"' >> ~/.zshrc"
            echo "  source ~/.zshrc"
        else
            echo "  export PATH=\"$INSTALL_DIR:\$PATH\""
        fi

        echo
        log_info "Or run directly: $INSTALL_DIR/$BINARY_NAME"
        return 0
    fi

    local installed_version=$($BINARY_NAME --version 2>/dev/null | head -1 || echo "unknown")
    log_success "Installation verified: $installed_version"
}

# Print usage instructions
print_usage() {
    echo
    log_success "Smelter has been successfully installed!"
    echo
    echo "Quick start:"
    echo "  $BINARY_NAME --help                    # Show help"
    echo "  $BINARY_NAME --version                 # Show version"
    echo "  $BINARY_NAME eval '(+ 2 3)'           # Evaluate expression"
    echo "  $BINARY_NAME repl                     # Start REPL"
    echo
    echo "Create a script (hello.coal):"
    echo '  #!/usr/bin/env smt run'
    echo '  (coalton-toplevel'
    echo '    (declare greet (String -> String))'
    echo '    (define (greet name)'
    echo '      (concatenate "Hello, " name "!")))'
    echo '  '
    echo '  (defun main ()'
    echo '    (format t "~A~%" (coalton:coalton (greet "World"))))'
    echo
    echo "Run it:"
    echo "  chmod +x hello.coal"
    echo "  ./hello.coal"
    echo
    echo "Documentation: https://github.com/${REPO_OWNER}/${REPO_NAME}"
    echo "Issues: https://github.com/${REPO_OWNER}/${REPO_NAME}/issues"
}

# Handle command line arguments
show_help() {
    echo "Smelter Installation Script"
    echo
    echo "Usage: $0 [OPTIONS]"
    echo
    echo "Options:"
    echo "  -h, --help           Show this help message"
    echo "  -v, --version VER    Install specific version (e.g., v0.1.0)"
    echo "  -d, --dir DIR        Install to custom directory (default: $INSTALL_DIR)"
    echo "  --local PATH         Install from local binary file"
    echo "  --uninstall          Uninstall Smelter"
    echo
    echo "Examples:"
    echo "  $0                   # Install latest version"
    echo "  $0 -v v0.1.0         # Install specific version"
    echo "  $0 -d ~/.local/bin   # Install to custom directory"
    echo "  $0 --local ./smt     # Install from local binary"
    echo "  $0 --uninstall       # Remove Smelter"
}

# Local install option (for development)
install_local() {
    local binary_path="$1"

    if [ ! -f "$binary_path" ]; then
        log_error "Binary not found at $binary_path"
        exit 1
    fi

    log_info "Installing local binary to $INSTALL_DIR/$BINARY_NAME..."

    # Create install directory if it doesn't exist
    mkdir -p "$INSTALL_DIR"

    # Copy binary (no sudo needed for home directory)
    cp "$binary_path" "$INSTALL_DIR/$BINARY_NAME"
    chmod +x "$INSTALL_DIR/$BINARY_NAME"

    log_success "Local installation complete"
    verify_installation
}

# Uninstall function
uninstall() {
    log_info "Uninstalling Smelter..."

    local binary_path="$INSTALL_DIR/$BINARY_NAME"

    if [ ! -f "$binary_path" ]; then
        log_warning "Smelter not found at $binary_path"
        return 0
    fi

    # Remove binary (no sudo needed for home directory)
    rm -f "$binary_path"

    log_success "Smelter has been uninstalled"
}

# Parse command line arguments
while [[ $# -gt 0 ]]; do
    case $1 in
        -h|--help)
            show_help
            exit 0
            ;;
        -v|--version)
            LATEST_VERSION="$2"
            shift 2
            ;;
        -d|--dir)
            INSTALL_DIR="$2"
            shift 2
            ;;
        --local)
            if [ -z "$2" ]; then
                log_error "--local requires a binary path"
                echo "Usage: $0 --local /path/to/smt"
                exit 1
            fi
            install_local "$2"
            exit 0
            ;;
        --uninstall)
            uninstall
            exit 0
            ;;
        *)
            log_error "Unknown option: $1"
            show_help
            exit 1
            ;;
    esac
done

# Main installation process
main() {
    echo "========================================"
    echo "    Smelter Installation Script"
    echo "========================================"
    echo
    
    detect_platform
    check_prerequisites
    
    if [ -z "$LATEST_VERSION" ]; then
        get_latest_version
    else
        log_info "Using specified version: $LATEST_VERSION"
    fi
    
    download_and_install
    verify_installation
    print_usage
}

# Run main function
main "$@"