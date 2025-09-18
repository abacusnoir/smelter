# GitHub Workflows

This directory contains automated CI/CD workflows for Smelter.

## Security Note

All workflows use the automatic `GITHUB_TOKEN` provided by GitHub Actions:
- **No personal access tokens stored or required**
- Uses `${{ secrets.GITHUB_TOKEN }}` - automatically and securely provided by GitHub
- Safe for public repositories
- No manual token configuration needed

## Workflows

### `build.yml`
- Triggered on: Push and pull requests
- Actions: Build and test Smelter binary
- Environment: macOS latest with SBCL

### `release.yml` 
- Triggered on: Version tags (v*)
- Actions: Build optimized binary, create GitHub release
- Environment: macOS latest with SBCL and UPX compression

## Token Permissions

The automatic `GITHUB_TOKEN` has the following permissions by default:
- Read access to repository contents
- Write access to create releases and upload assets
- No access to other repositories or sensitive data

This is the recommended and secure approach for GitHub Actions.