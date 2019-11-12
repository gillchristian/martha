# readme

A [serve](https://github.com/zeit/serve) & GitHub inspired file explorer focused
on [collocated Markdown documentation](#).

:warning: **WARNING Work in Progress** :warning:

## Installation

At the moment the only way to install is building from source.

```bash
git clone git@github.com:gillchristian/readme.git
cd readme
stack install .
```

_You probably can use cabal as well_.

## TODO

**Features**

- [x] Syntax highlight
- [x] Discovery of Markdown files
- [x] Static site generation (only Markdown files + tree structure)
- [ ] Proper CLI
- [ ] Rename to Marta !!!
- [ ] .gitignore aware (opt-out)
- [ ] README.md/readme.md file should be "index.html" of the directory (render
      ToC when directory is empty of files)
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

- [ ] Minimize IO usage :smirk:
- [ ] Library vs. App
- [ ] Code structure
- [ ] Cleanup arguments of `renderPath`
- [ ] Generate ToC only once (pass as argument) \*
- [ ] Cleanup unused dependencies from `package.yml`

<small>

\* Is that "optimization" needed? Shouldn't lazyness/compiler optimizations take
care of it.

</small>
