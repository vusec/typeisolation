echo "building libomp"
openmpdir="$PATHAUTOPACKSRC/openmp-$VERSIONLLVMPATCH"

run mkdir -p "$openmpdir"
[ -f "$openmpdir/.gitignore" ] || (run curl "https://releases.llvm.org/$VERSIONLLVM/openmp-$VERSIONLLVM.src.tar.xz" --output "openmp-$VERSIONLLVM.src.tar.xz" && run tar -xvf "openmp-$VERSIONLLVM.src.tar.xz" -C "$openmpdir" --strip-components 1)

run mkdir -p "$PATHAUTOPACKOBJ/openmp-$VERSIONLLVMPATCH"
cd "$PATHAUTOPACKOBJ/openmp-$VERSIONLLVMPATCH"
[ -f Makefile ] || run cmake -DCMAKE_INSTALL_PREFIX="$PATHAUTOPREFIX" -DCMAKE_BUILD_TYPE=Release "$openmpdir"
run make -j"$JOBS"
run make install
