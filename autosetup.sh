#!/bin/bash

set -e

: ${PATHROOT:="$PWD"}
if [ ! -f "$PATHROOT/autosetup.sh" ]; then
	echo "Please execute from the root of the repository or set PATHROOT" >&2
	exit 1
fi

mkdir -p "$PATHROOT/musl-hacks/include"
ln -s /usr/include/linux "$PATHROOT/musl-hacks/include/" || true
ln -s /usr/include/asm "$PATHROOT/musl-hacks/include/" || true
ln -s /usr/include/asm-generic "$PATHROOT/musl-hacks/include/" || true

source "$PATHROOT/autosetup/config.inc"
source "$PATHROOT/autosetup/paths.inc"

corecount="`grep '^processor' /proc/cpuinfo|wc -l`"
[ "$corecount" -le "$JOBSMAX" ] || corecount="$JOBSMAX"
if uname -a | grep -q "aarch64"; then
    $aarch64=1
fi

: ${EXTRA_CFLAGS:=""}
: ${EXTRA_LDFLAGS:=""}
: ${FORCE_PACKAGES:=0}
: ${JOBS="$corecount"}
: ${KEEP_GOING:=0} # continue building even after errors
: ${NO_PACKAGES:=0}
: ${NO_GPERFTOOLS:=0}
: ${NO_PERL:=0}
: ${DISABLE_OPT:=0} # TODO: use in targets config other than SPEC
: ${THREADS:="$corecount"} # TODO: use in targets config other than SPEC
: ${COPIES:=$corecount} # SPEC specific
: ${PRUN:=0} # set to 1 to enable parallel build of spec

: ${LLVM:=9.0} # Short for DESIRED_LLVM_VERSION
: ${DESIRED_LLVM_VERSION:=$LLVM}
case "$DESIRED_LLVM_VERSION" in
4.0)
    : ${VERSIONLLVM:=RELEASE_400/final}
    : ${VERSIONLLVMPATCH:=4.0}
    ;;
6.0)
    : ${VERSIONLLVM:=RELEASE_600/final}
    : ${VERSIONLLVMPATCH:=6.0}
    ;;
9.0)
    : ${VERSIONLLVM:=9.0.0}
    : ${VERSIONLLVMPATCH:=9.0}
    ;;
*)
    echo "Unsupported LLVM version selected." >&2
    exit 1
    ;;
esac

# benchmarks
: ${VERSIONAB_APR=apr-1.6.3}
: ${VERSIONAB_APRUTIL=apr-util-1.6.1}
: ${VERSIONAB_HTTPD=httpd-2.4.29}
: ${VERSIONSYSBENCH=sysbench-0.4.12.14}

# framework
: ${VERSIONGPERFTOOLS=632de2975e63f89613af9ab99bc1603a4a6332aa}

# packages
: ${VERSIONAUTOCONF=autoconf-2.68}
: ${VERSIONAUTOMAKE=automake-1.16.1}
: ${VERSIONBASH=bash-4.3}
: ${VERSIONBINUTILS=binutils-2.30}
: ${VERSIONCMAKE=cmake-3.8.2}
: ${VERSIONCMAKEURL=v3.8}
: ${VERSIONCOREUTILS=coreutils-8.22}
: ${VERSIONDYNINST=dyninst-9.3.2}
: ${VERSIONDYNINSTURL=v9.3.2}
: ${VERSIONFREETYPE=freetype-2.6.5}
: ${VERSIONLIBGCRYPT=libgcrypt-1.7.0}
: ${VERSIONLIBTOOL=libtool-2.4.6}
: ${VERSIONLIBUNWIND=libunwind-1.3-stable}
: ${VERSIONLIBUNWINDURL=v1.3-stable}
: ${VERSIONM4=m4-1.4.18}
: ${VERSIONMAKE=make-4.3}
: ${VERSIONPERL=perl-5.8.8} # set to 'none' to avoid Perl install
: ${VERSIONPERLURL=5.0}
: ${VERSIONPYTHON=2.7.13}
: ${VERSIONWRK=wrk-4.1.0}
: ${VERSIONWRKURL=4.1.0}

# targets
: ${VERSIONAPACHE_APR=apr-1.5.2}
: ${VERSIONAPACHE_APRUTIL=apr-util-1.5.4}
: ${VERSIONAPACHE_HTTPD=httpd-2.4.25}
: ${VERSIONCHEROKEE=1.2.104}
: ${VERSIONCHROME=51.0.2704.106}
: ${VERSIONFIREFOX:=47.0}
: ${VERSIONLIGHTTPD:=lighttpd-1.4.45}
: ${VERSIONLIGHTTPDURL:=releases-1.4.x}
: ${VERSIONMONGODB=mongodb-src-r3.6.3}
: ${VERSIONMUSL=1.2.2}
: ${VERSIONMYSQL=mysql-5.7.21}
: ${VERSIONMYSQLURL=5.7}
: ${VERSIONNGINX:=nginx-1.18.0}
# WARNING! Node.js not tested with clang / LTO
: ${VERSIONNODEJS=node-v8.10.0}
: ${VERSIONNODEJSURL=v8.10.0}
: ${VERSIONOPENSSL:=OpenSSL_1_1_1h}
: ${VERSIONPARSEC:=parsec-3.0}
: ${VERSIONPARSECURL:=3.0}
: ${VERSIONPHP:=5.2.0}
: ${VERSIONPHP4:=4.4.6}
: ${VERSIONWIRESHARK:=wireshark-2.0.2}

PATHBINUTILS="$PATHAUTOPACKSRC/$VERSIONBINUTILS"
PATHLIBUNWIND="$PATHAUTOPACKSRC/$VERSIONLIBUNWIND"

export PATH="$PATHAUTOPREFIX/bin:$PATH"

logdir="$(dirname "$PATHLOG")"
[ -e "$logdir" ] || mkdir -p "$logdir"
exec 5> "$PATHLOG"

build_package()
{
	name="$1"
	if [ "(" "!" -f "$PATHAUTOPACKSTATUS/done-$name" ")" -o "(" "$FORCE_PACKAGES" -ne 0 ")" ]; then
		source "$PATHROOT/autosetup/packages/$name.inc"
		touch "$PATHAUTOPACKSTATUS/done-$name"
	fi
}

build_packages()
{
	# build bash to override the system's default shell
	build_package bash

	# build a sane version of coreutils
	build_package coreutils

	# build binutils to ensure we have gold
	build_package binutils-gold

	# build make
	build_package make

	# build m4
	build_package m4

	# build autoconf
	build_package autoconf

	# build automake
	build_package automake

	# build python
	build_package python

	# build libtool
	build_package libtool

	# build cmake, needed to build LLVM
	build_package cmake

	# gperftools requires libunwind
	build_package libunwind

	# we need a patched LLVM
	build_package llvm

	# SPEC2017speed requires libomp (openmp)
	build_package libomp

	if [ "$NEEDDYNINST" -ne 0 ]; then
		# build dyninst
		build_package dyninst
	fi

	# build wrk  (used to benchmark nodejs)
	build_package wrk

	if [ "$VERSIONPERL" != none ]; then
		# build Perl, needed for gperftools autoreconf and SPEC CPU2006
		build_package perl
	fi
}

run()
{
	echo -------------------------------------------------------------------------------- >&5
	echo "command:          $*"               >&5
	echo "\$PATH:            $PATH"            >&5
	echo "\$LD_LIBRARY_PATH: $LD_LIBRARY_PATH" >&5
	echo "working dir:      $PWD"             >&5
	echo -------------------------------------------------------------------------------- >&5
	success=0
	if [ "$logsuffix" = "" ]; then
		pathlog="$PATHLOG"
		"$@" >&5 2>&5 && success=1
	else
		pathlog="$PATHLOG.$logsuffix.txt"
		echo "logging to $pathlog" >&5
		"$@" > "$pathlog" 2>&1 && success=1
	fi
	if [ "$success" -ne 0 ]; then
		echo "[done]" >&5
	else
		echo "Command '$*' failed in directory $PWD with exit code $?, please check $pathlog for details" >&2
		[ "$KEEP_GOING" -ne 0 ] || exit 1
	fi
}

runscript_common_start()
{
	echo "#!/bin/bash"
	echo "set -e"
	echo "export LD_LIBRARY_PATH=\"$prefixlib:$PATHAUTOPREFIX/lib:\$LD_LIBRARY_PATH\""
	echo "export PATH=\"$PATHAUTOPREFIX/bin:\$PATH\""
	echo "export TCMALLOC_LARGE_ALLOC_REPORT_THRESHOLD=281474976710656"
	[ -z "$CONFIG_SAFESTACK_OPTIONS" ] || echo "export SAFESTACK_OPTIONS=$CONFIG_SAFESTACK_OPTIONS"
	echo "PATHROOT=\"$PATHROOT\""
	echo ": \${RUNSCRIPTVERBOSE=0}"
	echo ""
	echo "echo \"[autosetup-runscript] target=$target\""
	echo "echo \"[autosetup-runscript] instancename=$instancename\""
	echo "echo \"[autosetup-runscript] cmd=\$*\""
	echo "echo \"[autosetup-runscript] cwd=\`pwd\`\""
	echo "echo \"[autosetup-runscript] LD_LIBRARY_PATH=\$LD_LIBRARY_PATH\""
	echo "echo \"[autosetup-runscript] PATH=\$PATH\""
	echo "echo \"[autosetup-runscript] PATHROOT=$PATHROOT\""
	echo "echo \"[autosetup-runscript] commit=`git log -n1 --oneline`\""
	echo "echo \"[autosetup-runscript] kernel=\`uname -s\`\""
	echo "echo \"[autosetup-runscript] kernel-release=\`uname -r\`\""
	echo "echo \"[autosetup-runscript] kernel-version=\`uname -v\`\""
	echo "echo \"[autosetup-runscript] machine=\`uname -m\`\""
	echo "echo \"[autosetup-runscript] node=\`uname -n\`\""
	echo "if [ \"\$RUNSCRIPTVERBOSE\" -ne 0 ]; then"
	echo "echo \"[autosetup-runscript] meminfo-start\""
	echo "cat /proc/meminfo"
	echo "echo \"[autosetup-runscript] meminfo-end\""
	echo "echo \"[autosetup-runscript] cpuinfo-start\""
	echo "cat /proc/cpuinfo"
	echo "echo \"[autosetup-runscript] cpuinfo-end\""
	echo "fi"
	echo "echo \"[autosetup-runscript] date-start=\`date +%Y-%m-%dT%H:%M:%S\`\""
	echo ""
	[ -z "$run_wrapper" ] || echo "run_wrapper=\"$run_wrapper\""
	echo ""
}

runscript_common_end()
{
	echo ""
	echo "echo \"[autosetup-runscript] date-end=\`date +%Y-%m-%dT%H:%M:%S\`\""
}

echo "Creating directories"
run mkdir -p "$PATHAUTOBENCHSRC"
run mkdir -p "$PATHAUTOFRAMEWORKSRC"
run mkdir -p "$PATHAUTOPACKSRC"
run mkdir -p "$PATHAUTOPACKSTATUS"
run mkdir -p "$PATHAUTOSCRIPTSBUILD"
run mkdir -p "$PATHAUTOSCRIPTSRUN"
run mkdir -p "$PATHAUTOSTATE"
run mkdir -p "$PATHAUTOTARGETSRC"

export CFLAGS="-I$PATHAUTOPREFIX/include"
export CPPFLAGS="-I$PATHAUTOPREFIX/include"
export LDFLAGS="-L$PATHAUTOPREFIX/lib"

#export CPATH="$PATHAUTOPREFIX/include:/usr/include/aarch64-linux-gnu"
export CPATH="$PATHAUTOPREFIX/include"

if [ "$NO_PACKAGES" -eq 0 ]; then
	build_packages
fi

# We need to select the correct LLVM version. We switch versions by simply
# re-running make install in the LLVM build directory of the desired version. To
# avoid unnecessary re-installs, we check the current installed version first,
# and only run make install if this does not match the selected version.
INSTALLED_LLVM_VERSION=`clang --version | grep "clang version" | cut -d' ' -f3 | cut -d'.' -f1,2`

if [ "$INSTALLED_LLVM_VERSION" != "$DESIRED_LLVM_VERSION" ]; then
    echo "Switch required from LLVM $INSTALLED_LLVM_VERSION to LLVM $DESIRED_LLVM_VERSION"
    read -n 1 -s -r -p "Press any key to continue (or hit CTRL-C to break)"
    echo ""
    source "$PATHROOT/autosetup/packages/llvm_install.inc"
    NEW_LLVM_VERSION=`clang --version | grep "clang version" | cut -d' ' -f3 | cut -d'.' -f1,2`
    if [ "$NEW_LLVM_VERSION" == "$DESIRED_LLVM_VERSION" ]; then
        echo "Success!"
    else
        echo "Failed!"
        exit 1
    fi
else
    echo "Using LLVM $INSTALLED_LLVM_VERSION"
fi

for instance in $INSTANCES; do
	if [ ! -f "$PATHROOT/autosetup/passes/$instance.inc" ]; then
		echo "error: unknown pass: $instance" >&2
		exit 1
	fi
	source "$PATHROOT/autosetup/passes/$instance.inc"
	if [ "$NO_GPERFTOOLS" -eq 0 ]; then
		source "$PATHROOT/autosetup/framework/gperftools.inc"
	fi
	for lib in $CONFIG_STATICLIBS; do
		echo "building staticlib-$lib-$instance"
		cd "$PATHROOT/staticlib/$lib"
		run make OBJDIR="$PATHAUTOFRAMEWORKOBJ/staticlib-$lib-$instance" $CONFIG_STATICLIB_MAKE -j"$JOBS"
	done
done

echo "building llvm-plugins"
cd "$PATHROOT/llvm-plugins"
if [ "$INSTALLED_LLVM_VERSION" != "$DESIRED_LLVM_VERSION" ]; then
    run make clean
fi
run make -j"$JOBS" GOLDINSTDIR="$PATHAUTOPREFIX" TARGETDIR="$PATHLLVMPLUGINS" CC="$PATHAUTOPREFIX/bin/clang++"

echo "building clang-plugins"
cd "$PATHROOT/clang-plugins"
if [ "$INSTALLED_LLVM_VERSION" != "$DESIRED_LLVM_VERSION" ]; then
    run make clean
fi
run make -j"$JOBS" TARGETDIR="$PATHCLANGPLUGINS"

if [ "$NEEDDYNINST" -ne 0 ]; then
	echo "building dyninst-plugins"
	cd "$PATHROOT/dyninst-plugins"
	run make -j"$JOBS"
fi

echo "initializing targets"
for target in $TARGETS; do
	if [ ! -d "$PATHROOT/autosetup/targets/$target" ]; then
		echo "error: unknown target: $target" >&2
		exit 1
	fi
	if [ -f "$PATHROOT/autosetup/targets/$target/init.inc" ]; then
		source "$PATHROOT/autosetup/targets/$target/init.inc"
	fi
done

echo "building nothp"
cd "$PATHROOT/nothp"
run make

PATHSPECOUT=""
which prun > /dev/null && PATHSPECOUT="/local/$USER/cpu2006-output-root"

PATHSP17OUT=""
which prun > /dev/null && PATHSP17OUT="/local/$USER/cpu2017-output-root"

PATHSPEC00OUT=""
which prun > /dev/null && PATHSPEC00OUT="/local/$USER/cpu2000-output-root"

# Configure targets
for instance in $INSTANCES; do
	instancename="$instance$INSTANCESUFFIX"
	source "$PATHROOT/autosetup/passes/$instance.inc"

	cflagsbl="$cflags"
	[ "$blacklist" = "" ] || cflagsbl="$cflagsbl -fsanitize-blacklist=$blacklist"

	for target in $TARGETS; do
		echo "configuring $target-$instancename"

		if [ -f "$PATHROOT/autosetup/targets/$target/config.inc" ]; then
			source "$PATHROOT/autosetup/targets/$target/config.inc"
		fi
	done
done

# Build targets
for instance in $INSTANCES; do
	instancename="$instance$INSTANCESUFFIX"

	for target in $TARGETS; do
		echo "building $target-$instancename"

		if [ -f "$PATHROOT/autosetup/targets/$target/build.inc" ]; then
			source "$PATHROOT/autosetup/targets/$target/build.inc"
		fi
	done
done

echo done
