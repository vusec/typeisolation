source $PATHROOT/scripts/linker-musl.inc
MUSLSRCDIR="$PATHAUTOTARGETOBJ/musl-1.2.2/$instance"
OPENSSLINSTDIR="$PATHAUTOTARGETSRC/openssl-$VERSIONOPENSSL/$instance/install"

run mkdir -p "$PATHAUTOTARGETSRC/$VERSIONNGINX/$instance"
cd "$PATHAUTOTARGETSRC/$VERSIONNGINX/$instance"
[ -f "configure" ] || run tar -xf "$PATHAUTOTARGETSRC/$VERSIONNGINX.tar.gz" --strip-components=1
if [ ! -f .autosetup.patched-config ]; then
	run patch -p1 < "$PATHROOT/patches/nginx-ld-config.patch"
	touch .autosetup.patched-config
fi
(
	export CC="$cc $MUSLCHEADERS"
	export LD_LIBRARY_PATH="$OPENSSLINSTDIR/lib:$prefixlib:$PATHAUTOPREFIX/lib:$LD_LIBRARY_PATH"
	[ -f Makefile ] || run ./configure --prefix="$PATHAUTOTARGETOBJ/$VERSIONNGINX/$instance/include" --with-cc-opt="-I$OPENSSLINSTDIR/include $cflagsbl -O2 -fPIE -pie" --with-ld-begin="$LINKERSTART " --with-ld-end="$LINKEREND " --with-ld-opt="-L$OPENSSLINSTDIR/lib $ldflagsalways $ldflagsnolib -O2 -pthread -fPIE -pie" --without-http_geo_module --with-http_ssl_module --without-http_rewrite_module --without-http_gzip_module
)

(
	runscript_common_start
	echo "cd \"$PWD\""
	echo "make -j$JOBS -B"
	echo "make install"
	runscript_common_end
) > "$PATHAUTOSCRIPTSBUILD/build-nginx-$instancename.sh"
run chmod u+x "$PATHAUTOSCRIPTSBUILD/build-nginx-$instancename.sh"

(
	runscript_common_start
	echo "PATHAB=\"$PATHAB\""
	echo "PATHSERVER=\"$PATHAUTOTARGETOBJ/$VERSIONNGINX/$instance/install/sbin/nginx\""
	echo "source \"$PATHROOT/autosetup/targets/nginx/run.inc\""
	runscript_common_end
) > "$PATHAUTOSCRIPTSRUN/run-nginx-$instancename.sh"
run chmod u+x "$PATHAUTOSCRIPTSRUN/run-nginx-$instancename.sh"

