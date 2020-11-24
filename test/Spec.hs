{-# LANGUAGE OverloadedStrings #-}

import Martha (isIgnored, parseGitignore)
import qualified System.FilePath.Glob as Glob
import Test.Hspec

(<=>) :: (Show a, Eq a) => a -> a -> Expectation
(<=>) = shouldBe

main :: IO ()
main = hspec $ do
  describe "Martha.parseGitignore" $ do
    it "skips empty lines" $ do
      parseGitignore "" <=> []
      (parseGitignore "pattern-1\n\npattern-2\n\n")
        <=> [Glob.compile "pattern-1", Glob.compile "pattern-2"]
    it "skips comment lines" $ do
      parseGitignore "# just a comment" <=> []
      parseGitignore "# just a comment\n# another comment" <=> []
      parseGitignore "#no space after #" <=> []
      (parseGitignore "pattern-1\n# a comment\npattern-2\n# another comment")
        <=> [Glob.compile "pattern-1", Glob.compile "pattern-2"]
    it "splits patterns" $ do
      (parseGitignore "pattern-1\n# a comment\npattern-2\n# another comment")
        <=> [Glob.compile "pattern-1", Glob.compile "pattern-2"]
  describe "Martha.isIgnored" $ do
    it "ignores '.'" $ do
      isIgnored [] "" "." <=> True
    it "ignores '..'" $ do
      isIgnored [] "" ".." <=> True
    it "ignores '.git'" $ do
      isIgnored [] "" ".git" <=> True
    it "ignores exact matches" $ do
      isIgnored [Glob.compile "dir"] "" "dir" <=> True
      isIgnored [Glob.compile "dir/dir"] "" "dir/dir" <=> True
      isIgnored [Glob.compile "dir/file.txt"] "" "dir/file.txt" <=> True
    it "ignores full path match" $ do
      isIgnored [Glob.compile "dir/dir"] "dir" "dir" <=> True
      isIgnored [Glob.compile "dir/file.txt"] "dir" "file.txt" <=> True
      isIgnored [Glob.compile "dir/dir/dir"] "dir/dir" "dir" <=> True
      isIgnored [Glob.compile "dir/dir/file.txt"] "dir/dir" "file.txt" <=> True
    it "ignores by name match" $ do
      isIgnored [Glob.compile "dir"] "base-dir" "dir" <=> True
      isIgnored [Glob.compile "file.txt"] "base-dir" "file.txt" <=> True
    describe "on pattern '*'" $ do
      it "ignores all files" $ do
        isIgnored [Glob.compile "*"] "" "file.txt" <=> True
        isIgnored [Glob.compile "*"] "base-dir" "file.txt" <=> True
      it "does not ignore dirs" $ do
        isIgnored [Glob.compile "*"] "base-dir" "dir/" <=> False
        -- TODO !!!!
        isIgnored [Glob.compile "base-dir/dir"] "base-dir" "dir/" <=> True
