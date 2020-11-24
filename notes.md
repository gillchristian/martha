## Notes 

```haskell
traverse :: (Traversable t, Applicative f) => (a -> f b) -> t a -> f (t b)
sequence :: (Traversable t, Monad m) => t (m a) -> m (t a)
```

```
traverse a_to_mb ta       ---> mtb
sequence $ fmap a_t_mb ta ---> mtb
```

## Things I don't want to delete =)

```haskell
type Breadcrumb = (FilePath, FilePath)

breadcrumbs :: Breadcrumb -> FilePath -> [Breadcrumb]
breadcrumbs root baseDir = root : go baseDir []
  where
    go :: FilePath -> [Breadcrumb] -> [(FilePath, FilePath)]
    go "." acc = acc
    go "/" acc = acc
    go base acc = go nextDir $ (base, file) : acc
      where
        (dir, file) = Path.splitFileName base
        nextDir = (Path.dropTrailingPathSeparator dir)

renderBreadcrumb :: Breadcrumb -> Html'
renderBreadcrumb (url, name) =
  a_
    [href_ $ Text.pack $ makeAbsolute url]
    $ toHtml
    $ Text.pack
    $ Path.addTrailingPathSeparator name
```

---

Implement gitignore library: https://git-scm.com/docs/gitignore
