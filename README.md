# `purescript-cst-rewrite`

ScalaFix for PureScript!

## tl;dr:

```
git clone --recurse-submodule git@github.com:jisantuc/purescript-cst-rewrite.git
cd purescript-cst-rewrite
stack build
find -name example-dir/ | xargs stack exec purescript-fix test/data/module-rename-single.diff
git diff
```

Then you can inspect the rule and the diff produced by applying it.

## Long version

[ScalaFix] is a tool for automatically rewriting Scala code based on
rules. These rules can be used for a few things:

- organizing imports based on some package name matching rules
- removing unused code
- changing import paths after an upgrade
- replacing deprecated method calls with non-deprecated calls

Pretty complicated automatic refactors are possible! For example,
`cats-effect` included Scalafix rules for the [`cats-effect` 3.0 release]
which overhauled the whole effect typeclass hierarchy. Users can apply
the rule and skip to the non-boring parts of the upgrade. It's neat!

I propose that a similar tool would be valuable for PureScript. The design
goals for this tool are:

- a simple CLI -- users shouldn't need to know about a lot of options, just locations
  of the rules and source code they want to use
- avoid reinventing the wheel -- there are already parsers and printers for PureScript
  CSTs, and those should be reused
- avoid hand-rolling new data formats -- developers everywhere use `git`, so borrow the
  `git` diff syntax with a tiny modification to keep things obvious

Design non-goals are:

- ensuring that resulting code compiles -- this might not always be possible, for example,
  if replacing a deprecated function call requires additional parameters. It also might not
  be desirable, for example, if filling in a `Nothing` where `Maybes` are expected changes
  behavior. The goal is just to automate the boring part, so that users can get to the
  point where they have to make choices more quickly.
- supporting every kind of rewrite imaginable _right now_. This is very proof-of-concept and I'm not going to go full steam ahead until there's some confirmation that this is
  a useful tool. The first type of rewrite I'll support is module renames, since they're an
  area where the existing language server tooling isn't super helpful.

With those in mind, an example invocation might look like this:

Suppose you have a PureScript project with code under `src/` and you've upgraded a library
version. The library author would publish a rule like:

```diff
# module rename
--- from
-import My.Package.Path.Foo
+++ to
+import My.Package.Foo
```

When upgrading, you would run:

```bash
purescript-cst-rewrite https://github.com/organization/library/tree/v2.0.0/rules/v2.0.0.diff src/
```

And all of the imports would be rewritten for you ✨

The short-term road map is:

- [x] automatic module renames
- [x] add the CLI
- [ ] rename imports (e.g. `s/import Foo (bar)/import Foo (baz)/`)

[ScalaFix]: https://scalacenter.github.io/scalafix/
[`cats-effect` 3.0 release]: https://github.com/typelevel/cats-effect/tree/series/3.x/scalafix/v3_0_0/input/src/main/scala/fix
