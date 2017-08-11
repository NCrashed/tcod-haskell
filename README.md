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
2. Run `stack install --flags tcod-haskell:examples` or `cabal sandbox new && cabal install --flags=examples`
3. Copy `libtcod/build/autotools/.libs/libtcod.so.0.0.0` somewhere, where shared libraries can be found by executables (e.x. `/usr/local/lib/libtcod.so.0` and run `ldconfig`). Or install package with `libtcod` of version `1.6.3` for your system.
4. Run example `tcod-haskell-sample01` in root folder of repo (or copy `terminal.png` to execution directory).

# Roadmap

Binded headers:

- [x] color_types.h
- [x] color.h
- [x] console_types.h
- [x] console.h
- [x] list.h
- [x] impage.h
- [x] mouse_types.h
- [x] sys.h
- [x] bresenham.h
- [x] bsp.h
- [x] fov_types.h
- [x] fov.h
- [x] heightmap.h
- [x] libtcod_version.h
- [x] mersenne_types.h
- [x] mersenne.h
- [x] mouse.h
- [x] namegen.h
- [x] noise_defaults.h
- [x] noise.h
- [ ] path.h
- [x] tree.h
- [ ] textfield.h
- [ ] gui/*.h
