## TODO

**Features** :space_invader:

- [x] Syntax highlight
- [x] Discovery of Markdown files
- [x] Static site generation (only Markdown files + tree structure)
- [x] README.md/readme.md file should be "index.html" of the directory (render
      ToC when directory has no files)
- [x] Rename to Martha !!!
- [ ] Proper CLI
- [ ] .gitignore aware (opt-out)
- [ ] Fuzzy finder
- [ ] Generate output in place (instead of in a separate directory)
- [ ] Deploy to Netlify/Now/.etc
- [ ] Plugin system: MDX support? Customize the ToC?
- [ ] ...

**Improvements / fixes** :bug:

- [x] Inline CSS =/
      ([Data.FileEmbed](http://hackage.haskell.org/package/file-embed-0.0.11/docs/Data-FileEmbed.html))
      :tada:
- [x] Move JS & CSS to files (no need to have them inline anymore)
- [x] Support GitHub Flavored Markdown
- [ ] Highlight current file on ToC
- [ ] Responsiveness & layout (e.g. toggleable ToC)
- [ ] Support emoji (`:pizza: -> 🍕`)
- [ ] Run `traverseDirectory`/`output` in parallel
- [ ] Windows compatibility :tm: (paths handling probably broken)
- [ ] Build step for JS/CSS (e.g. minify)
- [ ] Write to pwd (instead of the provided path)

**Code quality** :nail_care:

- [x] Cleanup arguments of `renderPath`
- [x] Cleanup unused dependencies from `package.yml`
- [ ] Tests
- [ ] Minimize IO usage :smirk:
- [ ] Library vs. App

**Repo** :snowboarder:

- [ ] Automate adding binaries to releases
- [ ] Support mainstream install methods: through brew / npm? / apt-get
