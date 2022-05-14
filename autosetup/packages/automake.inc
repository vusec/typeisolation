echo "building automake"
cd "$PATHAUTOPACKSRC"
[ -f "$VERSIONAUTOMAKE.tar.gz" ] || run wget "$GNUMIRROR/automake/$VERSIONAUTOMAKE.tar.gz"
[ -d "$VERSIONAUTOMAKE" ] || run tar xf "$VERSIONAUTOMAKE.tar.gz"
run mkdir -p "$PATHAUTOPACKOBJ/$VERSIONAUTOMAKE"
cd "$PATHAUTOPACKOBJ/$VERSIONAUTOMAKE"
[ -f Makefile ] || run "$PATHAUTOPACKSRC/$VERSIONAUTOMAKE/configure" --prefix="$PATHAUTOPREFIX"
run make -j"$JOBS"
run make install
