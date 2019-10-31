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

**Improvements / fixes**

- [x] Inline CSS =/
      ([Data.FileEmbed](http://hackage.haskell.org/package/file-embed-0.0.11/docs/Data-FileEmbed.html))
      :tada:
- [x] Render HTML views
- [ ] Render HTML views should be opt-out (I might want to see it as code
      instead, e.g. raw vs. rendered on GitHub)
- [ ] Serve files (e.g. CSS, JS) "normally" for HTML views !!!
- [ ] Handle 404s
- [ ] Handle media files (as HTML with their own standalone view)
- [ ] .gitignore aware (opt-out)
- [ ] Filter '..' on root
- [ ] Don't show logs (Servant logs some errors, like 404)

```
~/projects
$ readme
Serving http://localhost:7000
Users: getDirectoryContents:openDirStream: does not exist (No such file or directory)
favicon.ico: openFile: does not exist (No such file or directory)
sample: openFile: does not exist (No such file or directory)
favicon.ico: openFile: does not exist (No such file or directory)
```

**Code quality**

- [ ] 'Lift' stuff to setup function (e.g. `Dir.getCurrentDirectory`)
- [ ] Minimize IO usage :smirk:
- [ ] Library vs. app

**Features**

- [ ] Proper CLI
- [ ] History pannel (keep track of visited files)
- [ ] Syntax highlight
- [ ] Fuzzy finder
- [ ] Discovery of Markdown files
- [ ] Static site generation (only Markdown files + tree structure)
- [ ] Support GitHub flavored markdown (and emojis)
- [ ] ...
