source $PATHROOT/scripts/linker-musl.inc
MUSLSRCDIR="$PATHAUTOTARGETOBJ/musl-1.2.2/$instance"
OBJDIR="$PATHAUTOTARGETSRC/openssl-$VERSIONOPENSSL/$instance"

(
	export AR="$ar"
	export AS="as"
	export CC="$cc"
	export CXX="$cxx"
	export LD="$cc"
	export LD_LIBRARY_PATH="$prefixlib:$PATHAUTOPREFIX/lib:$LD_LIBRARY_PATH"
	export CFLAGS="-I$PATHAUTOPREFIX/include $cflagsbl -g -O2 $MUSLCHEADERS -D__STDC_NO_ATOMICS__"
	export CXXFLAGS="-I$PATHAUTOPREFIX/include $cflagsbl -g -O2 -no-c++-support"
	export LDFLAGS="$LINKERSTART $ldflagsalways $ldflagsnolib -v -O2"
	export LDLIBS="$LINKEREND"
	export NM="$nm"
	export RANLIB="$ranlib"
	

	run mkdir -p "$OBJDIR"
	cd "$OBJDIR"
	[ -f Configure ] || run tar -xf "$PATHAUTOTARGETSRC/$VERSIONOPENSSL.tar.gz" --strip-components=1
	#[ -f Makefile ] || run ./Configure --prefix="$OBJDIR/install" no-zlib no-asm no-shared linux-x86_64
	/usr/bin/perl ./Configure --prefix="$OBJDIR/install" no-zlib no-asm no-async no-shared no-tests linux-x86_64
	#--with-cc-opt="$cflagsbl" --with-ld-opt="$ldflagsalways $ldflagsnolib"
)

(
	runscript_common_start
	echo "export V=1"
	echo "export VERBOSE=1"
	echo "export PATH=\"$PATHAUTOPREFIX/bin:\$PATH\""
	echo "export LD_LIBRARY_PATH=\"$prefixlib:$PATHAUTOPREFIX/lib:\$LD_LIBRARY_PATH\""
	echo "cd \"$OBJDIR\""
	#echo "make -j$JOBS"
	echo "make -j4" # concurrent ld problems
	echo "make install"
	runscript_common_end
) > "$PATHAUTOSCRIPTSBUILD/build-openssl-$instancename.sh"
run chmod u+x "$PATHAUTOSCRIPTSBUILD/build-openssl-$instancename.sh"

(
	runscript_common_start
	echo "PATHAB=\"$PATHAB\""
	echo "PATHROOT=\"$PATHROOT\""
	echo "PATHSERVER=\"$OBJDIR/install/sbin/openlite\""
	echo "export LD_LIBRARY_PATH=\"$prefixlib:$PATHAUTOPREFIX/lib:\$LD_LIBRARY_PATH\""
	echo "source \"$PATHROOT/autosetup/targets/openssl/run.inc\""
	runscript_common_end
) > "$PATHAUTOSCRIPTSRUN/run-openssl-$instancename.sh"
run chmod u+x "$PATHAUTOSCRIPTSRUN/run-openssl-$instancename.sh"
