echo "building libgcrypt"
cd "$PATHAUTOPACKSRC"
[ -f "$VERSIONLIBGCRYPT.tar.bz2" ] || run wget "https://www.gnupg.org/ftp/gcrypt/libgcrypt/$VERSIONLIBGCRYPT.tar.bz2"
[ -d "$VERSIONLIBGCRYPT" ] || run tar xf "$VERSIONLIBGCRYPT.tar.bz2"
cd "$VERSIONLIBGCRYPT"
[ -f Makefile ] || run ./configure --prefix="$PATHAUTOPREFIX"
run make -j"$JOBS"
run make install
[ -h "$PATHAUTOPREFIX/lib/libgcrypt.so.11" ] || run ln -s libgcrypt.so "$PATHAUTOPREFIX/lib/libgcrypt.so.11" # Chrome needs this
