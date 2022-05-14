echo "building LLVM"
llvmdir="$PATHAUTOPACKSRC/llvm-$VERSIONLLVMPATCH"
run mkdir -p "$llvmdir"
[ -f "$llvmdir/.gitignore" ]                      || (run curl "https://releases.llvm.org/$VERSIONLLVM/llvm-$VERSIONLLVM.src.tar.xz"         --output "llvm-$VERSIONLLVM.src.tar.xz"         && run tar -xvf "llvm-$VERSIONLLVM.src.tar.xz"         -C "$llvmdir"                      --strip-components 1)

run mkdir -p "$llvmdir/tools/clang"
[ -f "$llvmdir/tools/clang/.gitignore" ]          || (run curl "https://releases.llvm.org/$VERSIONLLVM/cfe-$VERSIONLLVM.src.tar.xz"          --output "cfe-$VERSIONLLVM.src.tar.xz"          && run tar -xvf "cfe-$VERSIONLLVM.src.tar.xz"          -C "$llvmdir/tools/clang"          --strip-components 1)

run mkdir -p "$llvmdir/projects/compiler-rt"
[ -f "$llvmdir/projects/compiler-rt/.gitignore" ] || (run curl "https://releases.llvm.org/$VERSIONLLVM/compiler-rt-$VERSIONLLVM.src.tar.xz"  --output "compiler-rt-$VERSIONLLVM.src.tar.xz"  && run tar -xvf "compiler-rt-$VERSIONLLVM.src.tar.xz"  -C "$llvmdir/projects/compiler-rt" --strip-components 1)

cd "$llvmdir/projects/compiler-rt"
if [ ! -f .autosetup.patched-COMPILERRT-safestack ]; then
	run patch -p0 < "$PATHROOT/patches/COMPILERRT-safestack-$VERSIONLLVMPATCH.diff"
	touch .autosetup.patched-COMPILERRT-safestack
fi

cd "$llvmdir"
if [ ! -f .autosetup.patched-LLVM-gold-plugins ]; then
	run patch -p0 < "$PATHROOT/patches/LLVM-gold-plugins-$VERSIONLLVMPATCH.diff"
	touch .autosetup.patched-LLVM-gold-plugins
fi
if [ ! -f .autosetup.patched-LLVM-gold-Os ]; then
	run patch -p0 < "$PATHROOT/patches/LLVM-gold-Os-$VERSIONLLVMPATCH.diff"
	touch .autosetup.patched-LLVM-gold-Os
fi
if [ ! -f .autosetup.patched-LLVM-codegen ]; then
	run patch -p0 < "$PATHROOT/patches/LLVM-codegen-$VERSIONLLVMPATCH.diff"
	touch .autosetup.patched-LLVM-codegen
fi
if [ ! -f .autosetup.patched-LLVM-sanitizer ]; then
	pushd projects/compiler-rt
	run patch -p1 < "$PATHROOT/patches/LLVM-fix-sanitizer-build-with-modern-glibc-$VERSIONLLVMPATCH.diff"
	run patch -p1 < "$PATHROOT/patches/LLVM-fix-sanitizer-build-with-modern-glibc-2-$VERSIONLLVMPATCH.diff"
	popd
	touch .autosetup.patched-LLVM-sanitizer
fi
if [ ! -f .autosetup.patched-LLVM-orc ]; then
	run patch -p1 < "$PATHROOT/patches/LLVM-Fix-return-type-in-ORC-readMem-client-interface-$VERSIONLLVMPATCH.diff"
	touch .autosetup.patched-LLVM-orc
fi

if [ ! -f .autosetup.patch-LLVM-clang ]; then
    run patch -p0 < "$PATHROOT/patches/LLVM-clang-$VERSIONLLVMPATCH.diff"
    touch .autosetup.patch-LLVM-clang
fi

if [ ! -f .autosetup.patch-D75628 ]; then
    run patch -p2 < "$PATHROOT/patches/LLVM-D75628.diff"
    touch .autosetup.patch-D75628
fi

run mkdir -p "$PATHAUTOPACKOBJ/llvm-$VERSIONLLVMPATCH"
cd "$PATHAUTOPACKOBJ/llvm-$VERSIONLLVMPATCH"
[ -f Makefile ] || run cmake -DCMAKE_INSTALL_PREFIX="$PATHAUTOPREFIX" -DCMAKE_BUILD_TYPE=Release -DLLVM_ENABLE_ASSERTIONS=ON -DLLVM_BINUTILS_INCDIR="$PATHBINUTILS/include" "$llvmdir" -DLLVM_TEMPORARILY_ALLOW_OLD_TOOLCHAIN=1
run make -j"$JOBS"
run make install
