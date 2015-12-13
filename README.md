# hsrl

Simple roguelike implemented in Haskell (pending more creative name). Inspired
by roguelikes like [rogue](https://archive.org/details/msdos_Rogue_1983) and
[nethack](http://www.nethack.org/).

Uses [VTY](http://hackage.haskell.org/package/vty) for virtual terminal
rendering/input. Will eventually move to
[LambdaHack](http://hackage.haskell.org/package/LambdaHack), but I'm currently
enjoying the basics of the VTY library.

# Run

**Dependencies**:

* GHC (`brew install ghc` or `apt-get install ghc`)
* VTY (`cabal install vty`)

Then, compile with `ghc -threaded roguelike.hs`. An executable should be created
(`roguelike` on unix).

# TODOs:

- [ ] AI
- [ ] Dungeon generation
- [ ] Level navigation
- [ ] Items
- [ ] Basic story
- [ ] Save game
