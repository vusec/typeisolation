echo "building wrk"
cd "$PATHAUTOPACKSRC"
[ -f "$VERSIONWRK.tar.gz" ] || run wget -O "$VERSIONWRK.tar.gz" "https://github.com/wg/wrk/archive/$VERSIONWRKURL.tar.gz"
[ -d "$VERSIONWRK" ] || run tar xf "$VERSIONWRK.tar.gz"
run cp -r "$PATHAUTOPACKSRC/$VERSIONWRK" "$PATHAUTOPACKOBJ"

cd "$PATHAUTOPACKOBJ/$VERSIONWRK"
run make -j"$JOBS" VER="$VERSIONWRKURL"
run cp wrk "$PATHAUTOPREFIX/bin"

