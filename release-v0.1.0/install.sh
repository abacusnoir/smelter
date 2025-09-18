#!/bin/bash
set -e

echo "🔥 Installing Smelter v0.1.0..."

# Download and install
curl -L https://github.com/YOURUSERNAME/smelter/releases/download/v0.1.0/smelter-0.1.0-darwin-arm64.tar.gz | tar xz
chmod +x smelter
sudo mv smelter /usr/local/bin/smt

echo "✅ Smelter installed! Try: smt eval '(+ 1 2)'"