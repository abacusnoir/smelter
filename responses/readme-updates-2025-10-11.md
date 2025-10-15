# README Updates - 2025-10-11

## Changes Made

### 1. Fixed Quick Install URL

**Before:**
```bash
curl -fsSL https://smelter-landing.pages.dev/install.sh | bash
```

**After:**
```bash
curl -fsSL https://raw.githubusercontent.com/abacusnoir/smelter/master/install.sh | bash
```

Updated both instances in the README (top quick start and installation section) to reference the GitHub repository instead of the external landing page.

### 2. Created install.sh Script

Created a new `install.sh` script at the repository root that:
- Automatically detects platform (macOS Intel/ARM, Linux x86_64)
- Downloads the appropriate release binary from GitHub releases
- Installs to `/usr/local/bin/smt` or `~/.local/bin/smt`
- Handles permissions appropriately (sudo if needed)
- Provides clear success/error messages with colored output
- Verifies installation and displays version

The script is:
- 117 lines
- Executable (`chmod +x install.sh`)
- Syntax validated
- Ready to be committed and pushed

### 3. Updated Contribution Stance

**Before (Contributing section):**
```markdown
## 🤝 Contributing

Contributions welcome! Fork the repo, make your changes with tests, run `make test-all`,
and open a pull request. See [CONTRIBUTING.md](CONTRIBUTING.md) for details.
```

**After (Source Code section):**
```markdown
## 🤝 Source Code

This project is open source under the MIT License. The source code is shared for
transparency and learning. Contributions will be welcomed in the future once the
project stabilizes further.
```

**Before (Footer):**
```markdown
**Questions?** Open an [issue](https://github.com/abacusnoir/smelter/issues) or
start a [discussion](https://github.com/abacusnoir/smelter/discussions).
```

**After (Footer):**
```markdown
**Questions?** For now, please explore the examples and documentation. Community
engagement will be enabled once the project matures further.
```

## Rationale

The changes reflect a "source available but not accepting contributions yet" stance, which is appropriate for a project in early public release. The messaging is:
- **Transparent**: Source code is available under MIT License
- **Welcoming but bounded**: Indicates contributions will be welcomed *later*
- **Professional**: Avoids closed-door language while setting clear expectations
- **User-focused**: Directs people to examples and docs rather than GitHub discussions

## Files Modified

1. `README.md` - Updated quick install URLs, contribution section, and footer
2. `install.sh` - New file, platform-detection install script

## Next Steps

Ready to commit these changes when you're ready to push them to the repository.
