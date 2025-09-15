# Pre-Release Checklist

## Must Have (BLOCKING)
- [ ] Remove 9.7MB binary from git history (releases/v0.1.0/smelter)
- [ ] Fix hardcoded paths in homebrew/smelter-local.rb
- [ ] Fix hardcoded paths in test-homebrew-local.sh
- [ ] Verify GitHub token usage in workflows
- [ ] LICENSE file present ✅
- [ ] README with installation instructions ✅
- [ ] Performance claims verified ✅
- [ ] Binary builds successfully ✅

## Should Have
- [ ] Add CONTRIBUTING.md
- [ ] Add CHANGELOG.md
- [ ] Add license headers to source files
- [ ] Fix duplicate Makefile targets
- [ ] Review and clean commented code blocks
- [ ] .gitignore properly configured ✅
- [ ] Consistent code style ✅
- [ ] Examples working ✅

## Nice to Have
- [ ] CODE_OF_CONDUCT.md
- [ ] Issue templates
- [ ] CI/CD setup
- [ ] Logo/branding
- [ ] Website/docs site

## Security Verification
- [x] No API keys or tokens in source
- [ ] No hardcoded user paths (NEEDS FIX)
- [x] No external IP addresses
- [x] License compatibility checked

## Performance Verification
- [x] Startup time: 41.2ms (better than claimed 43ms!)
- [x] Binary size: 9.3MB (smt-minimal) vs 18MB (smt)

