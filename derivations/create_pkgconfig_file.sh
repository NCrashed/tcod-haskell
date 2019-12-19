cat <<EOF > libtcod.pc
prefix=$out
exec_prefix=\${prefix}
libdir=\${exec_prefix}/lib
includedir=\${prefix}/include
Name: libtcod
Description: API for roguelike games
Requires:
Version: 1.8.2
Cflags: -I\${includedir}/
EOF
