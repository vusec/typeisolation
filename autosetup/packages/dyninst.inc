echo "building dyninst"
dyninstdir="$PATHAUTOPACKSRC/$VERSIONDYNINST"
cd "$PATHAUTOPACKSRC"
[ -f "$VERSIONDYNINSTURL.tar.gz" ] || run wget "https://github.com/dyninst/dyninst/archive/$VERSIONDYNINSTURL.tar.gz"
[ -d "$VERSIONDYNINST" ] || run tar xf "$VERSIONDYNINSTURL.tar.gz"
run mkdir -p "$PATHAUTOPACKOBJ/$VERSIONDYNINST"
cd "$PATHAUTOPACKOBJ/$VERSIONDYNINST"
[ -f Makefile ] || run cmake -DCMAKE_INSTALL_PREFIX="$PATHAUTOPREFIX" -DCMAKE_BUILD_TYPE=Release "$dyninstdir"
run make -j"$JOBS" -k || true # dyninst does not work well with parallel builds
run make
run make install
echo "building di-opt"
cd "$PATHAUTOPACKSRC"
if [ ! -d di-opt/.git ]; then
    run git clone https://github.com/vvdveen/di-opt.git
fi
cd di-opt
DYNINST_INSTALL="$PATHAUTOPREFIX" run make 
DYNINST_INSTALL="$PATHAUTOPREFIX" run make install 
