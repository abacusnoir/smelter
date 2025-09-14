#!/bin/bash
# test-homebrew-local.sh - Test Homebrew formula locally

set -e

echo "🔥 Testing Smelter Homebrew Formula Locally"
echo "=========================================="

# Create local test tarball with proper structure
cd /Users/agam/Projects/smelter

# Create a proper tarball that matches what GitHub release would have
echo "Creating test tarball..."
mkdir -p test-release
cp releases/v0.1.0/smelter test-release/
cp README.md LICENSE test-release/ 2>/dev/null || true
if [ -d examples ]; then
    cp -r examples test-release/
fi

cd test-release
tar -czf ../smelter-0.1.0-darwin-arm64-local.tar.gz .
cd ..
rm -rf test-release

# Calculate actual checksum for local test
ACTUAL_SHA=$(shasum -a 256 smelter-0.1.0-darwin-arm64-local.tar.gz | cut -d' ' -f1)
echo "Local test tarball SHA256: $ACTUAL_SHA"

# Create a local version of the formula that uses file:// URL
cat > homebrew/smelter-local.rb << EOF
class SmelterLocal < Formula
  desc "Type-safe scripting language with 43ms startup"
  homepage "https://github.com/abacusnoir/smelter"
  version "0.1.0"
  license "MIT"

  # Use local file for testing
  url "file://$(pwd)/smelter-0.1.0-darwin-arm64-local.tar.gz"
  sha256 "$ACTUAL_SHA"

  def install
    bin.install "smelter" => "smt"
    
    # Install examples if they exist
    if Dir.exist?("examples")
      pkgshare.install "examples"
    end
    
    # Install documentation if present
    doc.install "README.md" if File.exist?("README.md")
    doc.install "LICENSE" if File.exist?("LICENSE")
  end

  test do
    # Test basic arithmetic
    assert_equal "3", shell_output("#{bin}/smt eval '(+ 1 2)'").strip
    
    # Test version command exists
    system "#{bin}/smt --version"
    
    # Verify startup performance (should be under 100ms)
    require "benchmark"
    time = Benchmark.realtime { system "#{bin}/smt", "eval", "'(+ 1 2)'", out: File::NULL }
    puts "Startup time: #{(time * 1000).round}ms"
    assert time < 0.15, "Startup time #{(time * 1000).round}ms exceeds 150ms"
  end

  def caveats
    <<~EOS
      🔥 Smelter installed successfully!
      
      Quick start:
        smt eval '(+ 1 2)'           # Evaluate expression
        smt repl                     # Start REPL
        smt run script.coal          # Run script file
        
      Smelter starts in ~43ms with full type safety!
    EOS
  end
end
EOF

echo "Created local test formula at homebrew/smelter-local.rb"

# Test that our binary works before trying Homebrew
echo "Testing binary directly..."
./releases/v0.1.0/smelter eval '(+ 1 2)' || {
    echo "❌ Binary doesn't work directly - fix before testing Homebrew"
    exit 1
}
echo "✅ Binary works directly"

echo ""
echo "To test with Homebrew, run:"
echo "  brew install --formula homebrew/smelter-local.rb"
echo "  smt eval '(+ 1 2)'"
echo "  brew test smelter-local"
echo "  brew uninstall smelter-local"