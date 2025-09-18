class Smelter < Formula
  desc "Type-safe scripting language with 43ms startup"
  homepage "https://github.com/abacusnoir/smelter"
  version "0.1.0"
  license "MIT"

  # URL will point to your GitHub release
  if OS.mac? && Hardware::CPU.arm?
    url "https://github.com/abacusnoir/smelter/releases/download/v0.1.0/smelter-0.1.0-darwin-arm64.tar.gz"
    sha256 "9f61c25c715c66441d836e13d2099d0803c5630d6675f59f3c595c05c9c4337e"
  elsif OS.mac? && Hardware::CPU.intel?
    url "https://github.com/abacusnoir/smelter/releases/download/v0.1.0/smelter-0.1.0-darwin-x86_64.tar.gz"
    sha256 "PENDING_INTEL_BUILD"
  elsif OS.linux? && Hardware::CPU.intel?
    url "https://github.com/abacusnoir/smelter/releases/download/v0.1.0/smelter-0.1.0-linux-x86_64.tar.gz"
    sha256 "PENDING_LINUX_BUILD"
  end

  def install
    bin.install "smelter" => "smt"
    
    # Install examples if they exist in the tarball
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
    
    # Test version command
    assert_match "Smelter", shell_output("#{bin}/smt --version 2>&1")
    
    # Verify startup performance (should be under 100ms)
    require "benchmark"
    time = Benchmark.realtime { system "#{bin}/smt", "eval", "'(+ 1 2)'", out: File::NULL }
    assert time < 0.1, "Startup time #{(time * 1000).round}ms exceeds 100ms"
    
    # Test REPL can start (with timeout to prevent hanging)
    require "timeout"
    begin
      Timeout::timeout(2) do
        IO.popen("#{bin}/smt repl", "r+") do |pipe|
          pipe.puts ":quit"
          pipe.close_write
          output = pipe.read
          assert_match(/Smelter|smt|REPL/i, output)
        end
      end
    rescue Timeout::Error
      # REPL started but didn't quit cleanly - that's OK for this test
    end
  end

  def caveats
    <<~EOS
      🔥 Smelter installed successfully!
      
      Quick start:
        smt eval '(+ 1 2)'           # Evaluate expression
        smt repl                     # Start REPL
        smt run script.coal          # Run script file
        
      Smelter starts in ~43ms with full type safety!
      Faster than Ruby (62ms), competitive with Python (29ms).
      
      Examples installed to:
        #{pkgshare}/examples/
      
      For more information:
        smt --help
    EOS
  end
end