# 🚀 Smelter v0.1.0 Launch Checklist

## ✅ Prerequisites Complete
- [x] Release binary built (9.3MB, 35ms startup!)
- [x] SHA256 hash calculated: `9f61c25c715c66441d836e13d2099d0803c5630d6675f59f3c595c05c9c4337e`
- [x] Homebrew formula updated with real hash
- [x] All documentation complete
- [x] GitHub issue templates created
- [x] Homebrew tap repository prepared

## Step 1: GitHub Repository (Manual - YOU need to do this)
- [ ] Go to GitHub repo settings
- [ ] Make repository **PUBLIC** 
- [ ] Add topics: `lisp`, `programming-language`, `scripting`, `type-safety`, `coalton`
- [ ] Set description: "Type-safe scripting language with 35ms startup - faster than Ruby!"

## Step 2: Create GitHub Release (Manual)
- [ ] Go to Releases → Create New Release
- [ ] Tag: **v0.1.0** 
- [ ] Title: **"Smelter v0.1.0 - Type-Safe Scripting in 35ms"**
- [ ] Upload: `release-v0.1.0/smelter-0.1.0-darwin-arm64.tar.gz`
- [ ] Copy content from `RELEASE_NOTES.md`
- [ ] Update performance claim: **35ms** (not 41ms!)
- [ ] Publish release

## Step 3: Publish Homebrew Tap (Run these commands)
```bash
cd homebrew-tap
git remote add origin https://github.com/YOURUSERNAME/homebrew-smelter.git
git push -u origin main
cd ..
```

## Step 4: Test Installation End-to-End
```bash
# This will work once your GitHub release is public
brew tap YOURUSERNAME/smelter
brew install smelter
smt eval '(+ 1 2)'  # Should return 3 in ~35ms!
```

## Step 5: Announce to the World! 🎺
See `ANNOUNCE.md` for ready-to-post content.

---

## Performance Update! 📈
**IMPORTANT**: Our binary now starts in **35.1ms** - even faster than our 41ms claim!
Update all marketing materials to reflect this improvement.