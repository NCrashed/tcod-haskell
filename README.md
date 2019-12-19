# tcod-haskell

These are midlevel bindings for [libtcod](https://bitbucket.org/libtcod/libtcod).

# Compile

Needed system libraries:
- `autoconf`
- `make`
- `gcc`
- `gcc-c++`
- `alsa-lib-devel`
- `audiofile-devel`
- `mesa-libGL-devel`
- `mesa-libGLU-devel`
- `mesa-libEGL-devel`
- `mesa-libGLES-devel`
- `libXext-devel`
- `libX11-devel`
- `libXi-devel`
- `libXrandr-devel`
- `libXrender-devel`
- `dbus-devel`
- `libXScrnSaver-devel`
- `libusb-devel`
- `pulseaudio-libs-devel`
- `libXinerama-devel`
- `libXcursor-devel`
- `systemd-devel`
- `SDL2-devel`

Steps:
1. Install `stack` or `cabal`+`GHC` from https://www.haskell.org/downloads
2. Install `libtcod` in your system (version `>= 1.8.2`).
3. Run `stack install --flags tcod-haskell:examples` or `cabal sandbox new && cabal install --flags=examples`
4. Run example `tcod-haskell-sample01` in root folder of repo (or copy `terminal.png` to execution directory).

# Nix pipeline

Steps:
1. Install [Nix](https://nixos.org/nix/)
2. Run `nix-build`
3. For interactive shell `nis-shell`
4. Run example in shell `cabal new-run --flag examples tcod-haskell-sample01`

# Roadmap

Binded headers:

- [x] bresenham.h
- [x] bsp.h
- [x] color_types.h
- [x] color.h
- [x] console_types.h
- [x] console.h
- [x] fov_types.h
- [x] fov.h
- [x] heightmap.h
- [x] image.h
- [ ] lex.h - *not binding* as useless.
- [ ] libtcod_int.h - *not binding* as useless.
- [ ] libtcod_portability.h - *not binding* as useless.
- [ ] libtcod_utility.h - *not binding* as useless.
- [x] libtcod_version.h
- [ ] libtcod.h - *not binding* as useless.
- [x] list.h
- [x] mersenne_types.h
- [x] mersenne.h
- [x] mouse_types.h
- [x] mouse.h
- [x] namegen.h
- [x] noise_defaults.h
- [x] noise.h
- [x] path.h
- [ ] parser.h - *not binding* as useless.
- [x] sys.h
- [x] tree.h
- [ ] textfield.h - TODO
- [ ] wrappers.h - *not binding* as useless.
- [ ] zip.h - TODO
- [ ] gui/*.h - TODO
