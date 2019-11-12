## TODO

**Features**

- [x] Syntax highlight
- [x] Discovery of Markdown files
- [x] Static site generation (only Markdown files + tree structure)
- [x] README.md/readme.md file should be "index.html" of the directory (render
      ToC when directory is empty of files)
- [x] Rename to Martha !!!
- [ ] Proper CLI
- [ ] .gitignore aware (opt-out)
- [ ] Fuzzy finder
- [ ] ...

**Improvements / fixes**

- [x] Inline CSS =/
      ([Data.FileEmbed](http://hackage.haskell.org/package/file-embed-0.0.11/docs/Data-FileEmbed.html))
      :tada:
- [x] Move JS & CSS to files (no need to have them inline anymore)
- [ ] Highlight current file on ToC
- [ ] Responsiveness & layout (e.g. toggleable ToC)
- [ ] Support GitHub Flavored Markdown (emojis _et al_)
- [ ] Windows compatibility :tm: (paths handling probably broken)

**Code quality**

- [x] Cleanup arguments of `renderPath`
- [x] Cleanup unused dependencies from `package.yml`
- [ ] Minimize IO usage :smirk:
- [ ] Library vs. App
