{ stdenv, fetchFromBitbucket, cmake, SDL2, libGLU_combined, upx, zlib
  , autoreconfHook, python, pkgconfig }:

stdenv.mkDerivation {

  pname = "libtcod";
  version = "1.8.2";

  src = fetchFromBitbucket {
    owner = "libtcod";
    repo = "libtcod";
    rev = "1.8.2";
    sha256 = "01qvk4i91mf3i5q7vsqy0660471xhs4wf66v7glicjif6wnzd3w6";
  };

  prePatch = ''
    cd build/autotools
    sed -i *.py \
      -e "s,#!/usr/bin/env python,#!${python}/bin/python,g"
  '';

  /* prePatch = ''
    sed -i CMakeLists.txt \
      -e "s,SET(ROOT_DIR.*,SET(ROOT_DIR $out),g" \
      -e "s,SET(INSTALL_DIR.*,SET(INSTALL_DIR $out),g"
    echo 'INSTALL(DIRECTORY include DESTINATION .)' >> CMakeLists.txt
  '';
  postInstall = ''
    mkdir -p $out/lib{,/pkgconfig}
    ${./create_pkgconfig_file.sh}
    cp libtcod.pc $out/lib/pkgconfig
    mv $out/*.so $out/lib

    mv $out/include{,_old}
    mkdir -p $out/include/libtcod
    mv $out/include_old/* $out/include/libtcod
    rm -rf $out/include_old
  '';

  cmakeFlags="-DLIBTCOD_SAMPLES=OFF"; */

  postInstall = ''
    mkdir -p $out/lib/pkgconfig
    ${./create_pkgconfig_file.sh}
    cp libtcod.pc $out/lib/pkgconfig
  '';

  nativeBuildInputs = [ autoreconfHook pkgconfig ];
  buildInputs = [ SDL2 libGLU_combined upx zlib ];

  meta = {
    description = "API for roguelike games";
    homepage = http://roguecentral.org/doryen/libtcod/;
    license = stdenv.lib.licenses.bsd3;
    platforms = stdenv.lib.platforms.linux;
    maintainers = [ stdenv.lib.maintainers.skeidel ];
  };
}
