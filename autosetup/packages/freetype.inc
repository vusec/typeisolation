echo "building freetype"
cd "$PATHAUTOPACKSRC"
[ -f "$VERSIONFREETYPE.tar.bz2" ] || run wget "http://download.savannah.gnu.org/releases/freetype/$VERSIONFREETYPE.tar.bz2"
[ -d "$VERSIONFREETYPE" ] || run tar xf "$VERSIONFREETYPE.tar.bz2"
cd "$VERSIONFREETYPE"
[ -f "$VERSIONFREETYPE" ] ||  run ./configure --prefix="$PATHAUTOPREFIX"
run make -j"$JOBS"
run make install
