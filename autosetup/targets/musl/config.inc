MUSLOBJDIR="$PATHAUTOTARGETOBJ/musl-$VERSIONMUSL/$instancename"

(
	export AR="$ar"
	export CC="$cc"
	export CFLAGS="$cflagsbl"
	export LDFLAGS="$ldflagsalways $ldflagsnolib -ldl"
	export LD_LIBRARY_PATH="$prefixlib:$PATHAUTOPREFIX/lib:$LD_LIBRARY_PATH"
	export NM="$nm"
	export RANLIB="$ranlib"
	export CONFIG_SHELL="$PATHAUTOPREFIX/bin/bash"

	# in-tree because sigh
	run mkdir -p "$PATHAUTOTARGETOBJ/musl-$VERSIONMUSL"
	run rm -rf "$MUSLOBJDIR"
	cd "$PATHAUTOTARGETSRC"
	run tar xf "musl-$VERSIONMUSL.tar.gz"
	run mv "musl-$VERSIONMUSL" "$MUSLOBJDIR"
	cd "$MUSLOBJDIR"
        run patch -p1 < "$PATHROOT/patches/musl-symbol-hack.diff"
        run patch -R -p1 < "$PATHROOT/musl-hacks/musl-weaksyms.patch"
	run ./configure --disable-shared --prefix="$MUSLOBJDIR"
        run rm -rf src/malloc # we don't want musl's malloc
)

(
	runscript_common_start
	echo "cd \"$MUSLOBJDIR\""
	echo "make clean"
	echo -n "make -j"$JOBS""
	runscript_common_end
) > "$PATHAUTOSCRIPTSBUILD/build-musl-$instancename.sh"
run chmod u+x "$PATHAUTOSCRIPTSBUILD/build-musl-$instancename.sh"

