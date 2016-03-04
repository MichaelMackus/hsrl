# hsrl

Simple roguelike implemented in Haskell (pending more creative name). Inspired
by roguelikes like [rogue](https://archive.org/details/msdos_Rogue_1983) and
[nethack](http://www.nethack.org/).

Uses [VTY](http://hackage.haskell.org/package/vty) for virtual terminal
rendering/input. Will eventually move to
[LambdaHack](http://hackage.haskell.org/package/LambdaHack), but I'm currently
enjoying the basics of the VTY library.

# Dependencies

**Stack:**

The preferred way to install is with stack. `cd` into the directory, and
do a `stack build`.

**Manual Install:**

* GHC (`brew install ghc` or `apt-get install ghc`)
* Execute the following in a terminal (requires `apt-get install cabal-install`)

    cabal install monadrandom
    cabal install vty


# Run

Compile with `ghc -threaded roguelike.hs` or `stack build`. An executable `roguelike` should be created.

# TODOs:

- [ ] AI
- [ ] Dungeon generation
- [ ] Level navigation
- [ ] Items
- [ ] Basic story
- [ ] Save game
