class SmelterLocal < Formula
  desc "Type-safe scripting language with 43ms startup"
  homepage "https://github.com/abacusnoir/smelter"
  version "0.1.0"
  license "MIT"

  # Use local file for testing - Update this path for your system
  url "file://#{ENV['SMELTER_LOCAL_PATH'] || '/path/to/smelter-0.1.0-darwin-arm64-local.tar.gz'}"
  sha256 "fac8f10cb71d1de26d3e94b77473da2ee9fb299bde6dd4ad62da910a01e40a53"

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
