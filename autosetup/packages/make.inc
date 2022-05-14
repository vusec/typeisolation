echo "building make"
cd "$PATHAUTOPACKSRC"
[ -f "$VERSIONMAKE.tar.gz" ] || run wget "$GNUMIRROR/make/$VERSIONMAKE.tar.gz"
[ -d "$VERSIONMAKE" ] || run tar xf "$VERSIONMAKE.tar.gz"
run mkdir -p "$PATHAUTOPACKOBJ/$VERSIONMAKE"
cd "$PATHAUTOPACKOBJ/$VERSIONMAKE"
[ -f Makefile ] || run "$PATHAUTOPACKSRC/$VERSIONMAKE/configure" --prefix="$PATHAUTOPREFIX"
run make -j"$JOBS"
run make install
