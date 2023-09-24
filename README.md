# hsrl

![hsrl](/res/screenshot.png?raw=true)

Simple roguelike implemented in Haskell (pending more creative name). Inspired
by roguelikes like [rogue](https://archive.org/details/msdos_Rogue_1983) and
[nethack](http://www.nethack.org/).

Uses [VTY](http://hackage.haskell.org/package/vty) for the terminal frontend
and [SDL2](https://www.libsdl.org/) for the graphics frontend.

There are Windows builds on the
[releases](https://github.com/MichaelMackus/hsrl/releases) page. Download the
hsrl.zip file, unzip it somewhere, and run the "hsrl" executable file.

# Build

[Stack](https://docs.haskellstack.org/en/stable/install_and_upgrade/) is the
supported way to build the game. You will also need the
[SDL2](https://www.libsdl.org/) library installed for the graphical (default)
frontend and `pkg-config` if it isn't already installed on your system.  After
you have the proper dependencies, run `stack build` to build or `stack run` to
build & run.

Mac installation:

```
brew install sdl2 pkg-config
curl -sSL https://get.haskellstack.org/ | sh
stack build
```

Ubuntu installation:

```
sudo apt-get install libsdl2-dev pkg-config
curl -sSL https://get.haskellstack.org/ | sh
stack build
```

If you would like the ability to run within a TTY (terminal) enable the vty
build flag: `stack build --flag hsrl:vty`. Unfortunately, I don't think the vty
library supports windows so if you want to run in a terminal you will have to
build on Mac or Linux.

# Run

`stack exec hsrl`

To run within a TTY (needs to be built with VTY support):

`stack exec hsrl -- --tty`
