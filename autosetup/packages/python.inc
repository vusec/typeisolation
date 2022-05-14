echo "building python"
cd "$PATHAUTOPACKSRC"
[ -f "Python-$VERSIONPYTHON.tgz" ] || run wget "https://www.python.org/ftp/python/$VERSIONPYTHON/Python-$VERSIONPYTHON.tgz"
[ -d "Python-$VERSIONPYTHON" ] || run tar xf "Python-$VERSIONPYTHON.tgz"
run mkdir -p "$PATHAUTOPACKOBJ/Python-$VERSIONPYTHON"
cd "$PATHAUTOPACKOBJ/Python-$VERSIONPYTHON"
[ -f Makefile ] || run "$PATHAUTOPACKSRC/Python-$VERSIONPYTHON/configure" --prefix="$PATHAUTOPREFIX"
run make -j"$JOBS"
run make install
