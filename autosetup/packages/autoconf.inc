echo "building autoconf"
cd "$PATHAUTOPACKSRC"
[ -f "$VERSIONAUTOCONF.tar.gz" ] || run wget "$GNUMIRROR/autoconf/$VERSIONAUTOCONF.tar.gz"
[ -d "$VERSIONAUTOCONF" ] || run tar xf "$VERSIONAUTOCONF.tar.gz"
run mkdir -p "$PATHAUTOPACKOBJ/$VERSIONAUTOCONF"
cd "$PATHAUTOPACKOBJ/$VERSIONAUTOCONF"
[ -f Makefile ] || run "$PATHAUTOPACKSRC/$VERSIONAUTOCONF/configure" --prefix="$PATHAUTOPREFIX"
run make -j"$JOBS"
run make install
