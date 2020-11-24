It's a simple idea. Adding a README.md file with the documentation for module
(directory). If this becomes a practice that we all follow we could then render
those files as HTML to have a "pretty" version of the docs.

The rationale behind it is that the closer to the code the documentation is the
easier the discovery and maintainability becomes. The closer a piece of
documentation can be to the code is being part of it (a.k.a. comments). IMO
comments are really useful to give some short context or explanation but adding
more detailed documentation and examples requires a bit more. So the next place
a piece of documentation can be to still be close to the code is in a file in
the directory. By making it Markdown files we can add code snippets, formatting,
etc. And by making it a README it becomes the "index" of that directory in
GitHub.

# Put your docs where your code is

- Intro
- Documentation is importat, we all know that (give some examples, quote
  Alvaro's articles)
- Talk about the "ideals"
  - No docs needed (code is simple, follows conventions)
  - Literate programming, sadly not quite popular
    (https://en.wikipedia.org/wiki/WEB)
  - Doc in the code (godoc, haddoc, rustodc?, etc)
  - And then what? Wiki? Confluence? Google Drive?
- Describe the problems of those (easy to get out of sync, discovery is not
  always ideal)
- Describe the proposed solution
- Mention Martha (and ask for feedback)
- Conclusion

It is broadly accepted in the software development industry that good, up to
date documentation of the programs and software projects is important. But at
the same time we've all been in the situation where we are handled a new project
and when we go looking for the docs they are outdated, are hard to find or
aren't there at all.

< TODO: add a transition here >

In the ideal world one wouldn't need documentation at all. For several reasons.

When the code is simple and easy to follow. When we follow certain conventions.
Very often those conventions are brought by the frameworks we work with. There
are also conventions and patterns "proposed" by the team/department/company.
Which also need to be documented by the way. :wink:

But in the real word not all the code is simple, easy to follow, and follows all
the established patterns and good practices. That would be an uthopy (utopia?).
Although some people document their code like it was (and by document their code
I mean not having documentation.)

It's not only about the complexity of the code but also the size of a codebase.
At a certain point we cannot keep all the moving pieces of a system in our
heads. There are also contextual (context based) decisions that brought the
codebase to certain state that cannot be expressed in the code. Code usually
says the what/how but it cannot express the why.

There are great tools to write documentation in the code like godoc, haddoc,
rustdoc?. They are (always) language based, since they rely on the semantics of
the code to add certain type of comments. Which can then be extracted by the
tool and generate an static site as documentation.

There are several benefits about such "inline documentation" approach. The
biggest one is that being "in place" means it's more probable that it will be
kept up to date with the code when changes are made. But also that we can read
it while coding. Or grep ("find in directory" IDE features count as well) it to
find what we are looking for.

These "in place docs" are good for documenting functions/clases/types and
depending on the tool maybe modules as well.

IMO this is the preferred way of documenting things.

But is it not common for every language. For example in the JS/TS communities
(the ones I'm most familiar with) there's JSDoc (and some TS tool as well) but
they aren't widly used. Whethere's in Rust or Haskell every published library
has (good) "in place" docs.

There are also other things that need to be documented (like the patterns that I
mentioned earlier) that "aren't" code. Practices, integrations with 3rd party
libraries, ~hacks~, etc.

Usually for those things we either write some stuff in the README.md or in a
`doc/` directory. Or move away from the codebase completely either to the wiki
in GitHub or to company's Google Drive or JIRA's Confluence, or some other such
tool. And the only God knows where those file end up. I've seen this more than I
like in my short career.

Since we are moving the documentation far away from the code it's quite easy to
get it out of sync from each other. After all who is going to go and check the
Wiki (or Confluence) when they create a PR? (guilty as charged.)

Lately I've been doing something (a practice?) that I find to be a good tradeoff
between "in place docs" and "far away in some document system docs". And it is
writting Markdown files in the same directory as the code. More especifically
README.md files. This leverages the fact that GitHub show's the README.md (and
readme.md) as the "index" of a directory to render that Markdown nicely there
:sparks:

It might also be based on the fact that I love writing my documents in Markdown
(I built literate-avocado only to be able to see all my gists because I have
+150 already, which is not even a big number). Anway I like to think this came
more from the attempt to have good documentation than from the love of Markdown.

This turned out to be pretty handy. We get prety nicely formated docs, with
pictures, syntax-highlighted-snippets, links, etc. And the documents stay very
close to the code, thus reducing the chances of getting out of sync. I say
reducing because the only thing that cannot get out of sync with code is code
itself (well it can, that's how we introduce bugs, but you get the point.)

But it didn't solve the discovery problem to some extent. Yeah GitHub has "fuzzy
finder" (if you didn't know go to a repo and press "t"). But still seing all the
directories that have some README (or any other MD file for that matter) all at
once for a repo (or for all the repos of a company) would be much better.

So I went out and built Martha

`![marthaaa](gif).`

Yeah, that's the story behind the name.

At the moment is just an PoC/MVP. So give it a try and let me know what you
think.

## Conclusion

Write docs. Many. Keep your docs as close to the code as possible. If possible
inside the code, otherwise in the same directory or at least repository. And for
doing that give Martha a try (and let me know what you think and what is missing
to make it really useful.)

---

> In this article we are interested about those instances where knowledge cannot
> be immediately obtained from the source code, its tests and documentation, or
> any of the other artifacts produced by the code.

> [...] understanding code shouldn’t be a challenge, but a matter of
> collaboration.

> we fill in the blanks on a story using what’s available on our own
> encyclopedia

:point_up: (we can say that encyclopedia is our context)

> Code comments are probably one of the most important paratexts that can help
> other programmers understand our code. Keeping them in sync with the code is a
> whole different problem. Not even Cervantes escaped this fate: in Don Quixote,
> the original description for Chapter X doesn’t match the contents of the
> chapter!

:point_up: :point_up: :point_up:

[Lector In Codigo](http://alvaro-videla.com/2018/05/lector-in-codigo.html)

[Literate Programming](https://en.wikipedia.org/wiki/Literate_programming)
