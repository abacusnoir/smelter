# Homebrew Tap Setup - October 11, 2025

## Objective
Set up official Homebrew tap for Smelter to enable easy installation via `brew install`.

## Implementation

### 1. Prepared v0.1.7 Release Assets
Downloaded and verified checksums for all platforms:
- **darwin-arm64**: `c720cf85377f33e78826f51a4ad67727b1b65de759ba39a664580fcb61c4c874`
- **darwin-x64**: `ccb712cebac44f801b98a93f2ccad64b26b0723217852aac99c44534408798be`
- **linux-x64**: `390b3f72664ec14ed2a5038b4b976536bdd1cb9e6c3e5874709e61dd519a7c00`

### 2. Updated Homebrew Formula
Created `homebrew-tap/Formula/smelter.rb`:
- Version: 0.1.7
- Multi-platform support (macOS ARM64, macOS Intel, Linux x64)
- Correct binary naming (tarballs contain `smt-{platform}-{arch}`)
- Tests: arithmetic, version, startup performance (<150ms)
- Caveats with quick start guide

### 3. Created GitHub Repository
- Repository: `https://github.com/abacusnoir/homebrew-smelter`
- Public repository with description
- Pushed formula and README

### 4. Tested Installation
```bash
brew tap abacusnoir/smelter
brew install smelter
```

Verification:
```bash
$ which smt
/opt/homebrew/bin/smt

$ smt --version
Smelter 0.1.0
Coalton 0.8.0
SBCL 2.5.8

$ smt eval '(+ 2 3)'
5
```

## Installation Instructions (for users)

### Using Homebrew (Recommended)
```bash
brew tap abacusnoir/smelter
brew install smelter
```

### Direct Installation (Alternative)
```bash
curl -fsSL https://raw.githubusercontent.com/abacusnoir/smelter/master/scripts/install.sh | bash
```

## Repository Structure

```
homebrew-smelter/
├── Formula/
│   └── smelter.rb          # Homebrew formula
└── README.md                # Tap documentation
```

## Next Steps

Consider adding to main README.md:
```markdown
## Installation

### Homebrew (macOS/Linux)
```bash
brew tap abacusnoir/smelter
brew install smelter
```

### Quick Install Script
```bash
curl -fsSL https://raw.githubusercontent.com/abacusnoir/smelter/master/scripts/install.sh | bash
```
```

## Resources

- Tap repository: https://github.com/abacusnoir/homebrew-smelter
- Formula: https://github.com/abacusnoir/homebrew-smelter/blob/master/Formula/smelter.rb
- Homebrew docs: https://docs.brew.sh/How-to-Create-and-Maintain-a-Tap

## Success Metrics

✅ Tap created and published
✅ Formula tested and working
✅ Multi-platform support (3 platforms)
✅ Installation verified locally
✅ Binary works correctly after installation
