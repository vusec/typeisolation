if [ default != "$CONFIG_MALLOC" ]; then
	echo "downloading gperftools-$instance"
	cd "$PATHAUTOFRAMEWORKSRC"
	if [ ! -d "gperftools-$instance/.git" ]; then
		run git clone https://github.com/gperftools/gperftools.git "gperftools-$instance"
		cd "gperftools-$instance"
		run git checkout "$VERSIONGPERFTOOLS"
	fi
	cd "$PATHAUTOFRAMEWORKSRC/gperftools-$instance"
	#	if [ ! -f .autosetup.patched-gperftools-speedup ]; then
		#	run patch -p1 < "$PATHROOT/patches/GPERFTOOLS_SPEEDUP.patch"
		#touch .autosetup.patched-gperftools-speedup
		#fi
	if [ ! -f .autosetup.patched-gperftools-chrome_fork_hang ]; then
		run patch -p1 < "$PATHROOT/patches/GPERFTOOLS_chrome_fork_hang.patch"
		touch .autosetup.patched-gperftools-chrome_fork_hang
	fi
	# XXX START MUSL CHANGES XXX
	if [ ! -f .autosetup.patched-gperftools-hacky-build ]; then
		run patch -p1 < "$PATHROOT/patches/tcmalloc-test-build-hacks.txt"
		touch .autosetup.patched-gperftools-hacky-build
	fi
	if [ ! -f .autosetup.patched-gperftools-sbrk ]; then
		run patch -p1 < "$PATHROOT/patches/tcmalloc-sbrk-musl.patch"
		touch .autosetup.patched-gperftools-sbrk
	fi
	# XXX END MUSL CHANGES XXX
fi

case "$CONFIG_MALLOC" in
default)
	# no gperftools
	;;
tcmalloc)
	# standard tcmalloc
	echo "preparing gperftools-$instance"
	[ -f configure ] || run autoreconf -fi
	;;
tcmalloc-metalloc)
	# modified tcmalloc
	echo "preparing gperftools-$instance"
	cd "$PATHAUTOFRAMEWORKSRC/gperftools-$instance"
	if [ ! -f .autosetup.patched-gperftools-metalloc ]; then
		run patch -p1 < "$PATHROOT/patches/GPERFTOOLS_metalloc-2.5.patch"
		touch .autosetup.patched-gperftools-metalloc
	fi
	[ -f configure ] || run autoreconf -fi

	echo "building metapagetable-$instance"
	cd "$PATHROOT/metapagetable"
	export METALLOC_OPTIONS="-DFIXEDCOMPRESSION=$CONFIG_FIXEDCOMPRESSION -DMETADATABYTES=$CONFIG_METADATABYTES -DDEEPMETADATA=$CONFIG_DEEPMETADATA"
	[ "true" = "$CONFIG_DEEPMETADATA" ] && METALLOC_OPTIONS="$METALLOC_OPTIONS -DDEEPMETADATABYTES=$CONFIG_DEEPMETADATABYTES"
	[ -n "$CONFIG_ALLOC_SIZE_HOOK" ] && METALLOC_OPTIONS="$METALLOC_OPTIONS -DALLOC_SIZE_HOOK=$CONFIG_ALLOC_SIZE_HOOK"
	metapagetabledir="$PATHAUTOFRAMEWORKOBJ/metapagetable-$instance"
	run make OBJDIR="$metapagetabledir" GPERFTOOLSDIR="$PATHAUTOFRAMEWORKSRC/gperftools-$instance" config
	run make OBJDIR="$metapagetabledir" GPERFTOOLSDIR="$PATHAUTOFRAMEWORKSRC/gperftools-$instance" -j"$JOBS"

	export METAPAGETABLEDIR="$metapagetabledir"
	;;
tcmalloc-typed)
	# modified tcmalloc
	echo "preparing gperftools-$instance"
	cd "$PATHAUTOFRAMEWORKSRC/gperftools-$instance"
	if [ ! -f .autosetup.patched-gperftools-typed ]; then
		run patch -p1 < "$PATHROOT/patches/GPERFTOOLS_typed-2.5.patch"
		touch .autosetup.patched-gperftools-typed
	fi
	if [ ! -f .autosetup.patched-gperftools-typed-arenas ]; then
		run patch -p1 < "$PATHROOT/patches/GPERFTOOLS_typed-arenas-2.5.patch"
		touch .autosetup.patched-gperftools-typed-arenas
	fi
	[ -f configure ] || run autoreconf -fi
	;;
*)
	echo "error: pass $instance does not define CONFIG_MALLOC" >&2
	exit 1
	;;
esac

LIBCXXSRCDIR="$PATHAUTOTARGETSRC/libcxx"
LIBUNWINDSRCDIR="$PATHAUTOTARGETSRC/libunwind"

if [ default != "$CONFIG_MALLOC" ]; then
	(
	echo "building gperftools-$instance"
	# XXX START MUSL CHANGES XXX
	source $PATHROOT/autosetup/passes/helper/reset.inc
	# XXX END MUSL CHANGES XXX
	run mkdir -p "$PATHAUTOFRAMEWORKOBJ/gperftools-$instance"
	cd "$PATHAUTOFRAMEWORKOBJ/gperftools-$instance"
	# XXX START MUSL CHANGES XXX
	# [ -f Makefile ] || run "$PATHAUTOFRAMEWORKSRC/gperftools-$instance/configure" --prefix="$PATHAUTOPREFIXBASE/$instance"
	[ -f Makefile ] || CXX="$cxx" CXXFLAGS="-stdlib=libc++ -nostdinc++ -I$LIBCXXSRCDIR/include" run "$PATHAUTOFRAMEWORKSRC/gperftools-$instance/configure" --prefix="$PATHAUTOPREFIXBASE/$instance" --disable-libunwind --disable-backtrace --disable-shared
	# XXX END MUSL CHANGES XXX
	run make -j"$JOBS" METAPAGETABLEDIR="$METAPAGETABLEDIR"
	run make install METAPAGETABLEDIR="$METAPAGETABLEDIR"
	)
fi
unset METAPAGETABLEDIR
