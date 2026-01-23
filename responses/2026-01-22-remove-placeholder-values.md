# Remove Placeholder Username Values

**Date**: 2026-01-22
**Branch**: claude/remove-placeholder-values-5ELdK

## Summary

Replaced all instances of placeholder usernames (`yourusername`, `your-username`, `YOURUSERNAME`) with the actual GitHub username `abacusnoir`.

## Files Modified

1. **smelter.asd** (lines 6-8)
   - `:homepage` URL
   - `:bug-tracker` URL
   - `:source-control` URL

2. **create-release.sh** (line 113)
   - Documentation URL in generated QUICK_START.md

3. **CONTRIBUTING.md** (line 8)
   - Clone URL in Getting Started section

4. **release-v0.1.0/install.sh** (line 7)
   - Download URL for release tarball

5. **examples/showcase/README.md** (line 85)
   - Clone URL in "Try them yourself" section

## Detection Method

Used grep to search for patterns: `username`, `yourusername`, `your-username`, `YOURUSERNAME`, and `github.com/*/smelter` to find all instances and determine the correct username (`abacusnoir`) from existing correct references.

## Additional Findings

Also found other placeholder-related content that was intentionally not modified:
- `examples/process-demos.coal` - Contains placeholder stub functions (design pattern, not config)
- `docs/http-json-adapters.md` - Documents placeholder behavior for arrays/objects (technical documentation)
- Test files - Using `example.com` for test fixtures (appropriate for tests)
- The `responses/` directory references - Historical documentation

## Commit

```
c638160 fix: Replace placeholder usernames with actual GitHub username
```
