LIBUNWINDOBJDIR="$PATHAUTOTARGETOBJ/libunwind/$instancename"
LIBCXXABIOBJDIR="$PATHAUTOTARGETOBJ/libcxxabi/$instancename"
LIBCXXOBJDIR="$PATHAUTOTARGETOBJ/libcxx/$instancename"
LIBCXXABISRC="$PATHAUTOTARGETSRC/libcxxabi"

source $PATHROOT/scripts/linker-musl.inc

(
	export AR="$ar"
	export CC="$cc"
	export CFLAGS="$cflagsbl"
	export LDFLAGS="$ldflagsalways $ldflagsnolib -ldl"
	export LD_LIBRARY_PATH="$prefixlib:$PATHAUTOPREFIX/lib:$LD_LIBRARY_PATH"
	export NM="$nm"
	export RANLIB="$ranlib"
	export CONFIG_SHELL="$PATHAUTOPREFIX/bin/bash"

	run mkdir -p "$LIBUNWINDOBJDIR"
	run cd "$LIBUNWINDOBJDIR"
        run cmake "$PATHAUTOTARGETSRC/libunwind" \
        -DLLVM_PATH="$LLVMDIR" \
        -DLIBUNWIND_ENABLE_SHARED=OFF \
        -DCMAKE_BUILD_TYPE=Release \
        -DCMAKE_TRY_COMPILE_TARGET_TYPE=STATIC_LIBRARY \
        -DCMAKE_C_FLAGS="$cflagsbl" \
        -DCMAKE_CXX_FLAGS="$cflagsbl" \
        -DCMAKE_C_COMPILER="$cc" \
        -DCMAKE_CXX_COMPILER="$cxx" \
        -DCMAKE_C_LINK_FLAGS="$ldflagsalways $ldflagsnolib" \
        -DCMAKE_CXX_LINK_FLAGS="$ldflagsalways $ldflagsnolib" \
        -DCMAKE_AR="$ar" \
        -DCMAKE_RANLIB="$ranlib"

        # LIBCXXABI_SILENT_TERMINATE to avoid dragging in the demangling code
	run mkdir -p "$LIBCXXABIOBJDIR"
	run cd "$LIBCXXABIOBJDIR"
        run cmake "$PATHAUTOTARGETSRC/libcxxabi" \
        -DLLVM_PATH="$LLVMDIR" \
        -DLIBCXXABI_LIBCXX_PATH="$PATHAUTOTARGETSRC/libcxx" \
        -DLIBCXXABI_LIBUNWIND_PATH="$PATHAUTOTARGETSRC/libunwind" \
        -DLIBCXXABI_USE_LLVM_UNWINDER=ON \
        -DLIBCXXABI_ENABLE_SHARED=OFF \
        -DCMAKE_BUILD_TYPE=Release \
        -DCMAKE_TRY_COMPILE_TARGET_TYPE=STATIC_LIBRARY \
        -DCMAKE_C_FLAGS="$cflagsbl" \
        -DCMAKE_CXX_FLAGS="$cflagsbl" \
        -DCMAKE_C_COMPILER="$cc" \
        -DCMAKE_CXX_COMPILER="$cxx" \
        -DCMAKE_C_LINK_FLAGS="$ldflagsalways $ldflagsnolib" \
        -DCMAKE_CXX_LINK_FLAGS="$ldflagsalways $ldflagsnolib" \
        -DCMAKE_AR="$ar" \
        -DCMAKE_RANLIB="$ranlib" \
        -DLIBCXXABI_SILENT_TERMINATE=ON

	run mkdir -p "$LIBCXXOBJDIR"
	run cd "$LIBCXXOBJDIR"
        run cmake "$PATHAUTOTARGETSRC/libcxx" \
        -DLLVM_PATH="$LLVMDIR" \
        -DLIBCXX_CXX_ABI=libcxxabi \
        -DLIBCXX_CXX_ABI_INCLUDE_PATHS="$LIBCXXABISRC/include" \
        -DLIBCXX_CXX_ABI_LIBRARY_PATH="$LIBCXXABIOBJDIR/lib" \
        -DLIBCXX_ENABLE_SHARED=OFF \
        -DLIBCXX_ENABLE_EXPERIMENTAL_LIBRARY=OFF \
        -DLIBCXX_ENABLE_EXCEPTIONS=ON \
        -DLIBCXX_ENABLE_STATIC_ABI_LIBRARY=ON \
        -DLIBCXX_HAS_MUSL_LIBC=ON \
        -DCMAKE_BUILD_TYPE=Release \
        -DCMAKE_TRY_COMPILE_TARGET_TYPE=STATIC_LIBRARY \
        -DCMAKE_C_FLAGS="$cflagsbl $MUSLCHEADERS" \
        -DCMAKE_CXX_FLAGS="$cflagsbl $MUSLCXXHEADERS" \
        -DCMAKE_C_COMPILER="$cc" \
        -DCMAKE_CXX_COMPILER="$cxx" \
        -DCMAKE_C_LINK_FLAGS="$ldflagsalways $ldflagsnolib" \
        -DCMAKE_CXX_LINK_FLAGS="$ldflagsalways $ldflagsnolib" \
        -DCMAKE_AR="$ar" \
        -DCMAKE_RANLIB="$ranlib" \
        -DCMAKE_INSTALL_PREFIX="$PATHAUTOPREFIXBASE/libcxx-$instance"

)

(
	runscript_common_start
	echo "cd \"$LIBCXXABIOBJDIR\""
	echo "make -j"$JOBS""
	echo "cd \"$LIBCXXOBJDIR\""
	echo "make -j"$JOBS""
	echo "make install"
	echo "cd \"$LIBUNWINDOBJDIR\""
	echo "make -j"$JOBS""
	runscript_common_end
) > "$PATHAUTOSCRIPTSBUILD/build-libcxx-$instancename.sh"
run chmod u+x "$PATHAUTOSCRIPTSBUILD/build-libcxx-$instancename.sh"

