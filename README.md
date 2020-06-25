# chip8

A chip8 emulator written in haskell, sdl2 library for graphics, sound and user input

## Usage

The executable should be placed in the same folder as the games, the sound.wav file and the SDL2 libraries.
The games, the sound file and the libraries for windows can be found in the resources folder.

```sh
$ chip8-exe TETRIS
```

## Build from source

Building requires having [stack](https://docs.haskellstack.org/en/stable/README/) and the [SDL2](https://www.libsdl.org/download-2.0.php) and [SDL2-mixer](https://www.libsdl.org/projects/SDL_mixer/installed) libraries installed.

```sh
$ stack build
```

## Resources
 - [SDL wiki](https://wiki.libsdl.org/)
 - [sdl2 haskell library documentation](https://hackage.haskell.org/package/sdl2-2.5.0.0)
 - [chip8 specs](http://devernay.free.fr/hacks/chip8/C8TECH10.HTM)
 - [chip8 specs2](http://chip8.sourceforge.net/chip8-1.1.pdf)
