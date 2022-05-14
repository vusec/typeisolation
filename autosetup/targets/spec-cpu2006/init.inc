: ${CLEAN_SPEC:=0}

if [ -f "$PATHSPEC/shrc" ]; then
	echo "using existing SPEC CPU2006 installation"
else
	echo "installing SPEC CPU2006"
	PATHSPECCD="$PATHAUTOTARGETSRC/spec-cpu2006-cd"
  # [ -d "$PATHSPECCD/.git" ] || run git clone git@bitbucket.org:vusec/spec-cpu2006-cd.git "$PATHSPECCD"
	cd "$PATHSPECCD"
	run ./install.sh -f -d "$PATHSPEC"
fi

cd "$PATHSPEC"
if [ ! -f .autosetup.patched-dealII-stddef ]; then
	run patch -p0 < "$PATHROOT/patches/SPEC-CPU2006-dealII-stddef.patch"
	touch .autosetup.patched-dealII-stddef
fi

cd "$PATHSPEC"
if [ ! -f .autosetup.patched-dealII-pair ]; then
	run patch -p1 < "$PATHROOT/patches/SPEC-CPU2006-dealII-pair.patch"
	touch .autosetup.patched-dealII-pair
fi

cd "$PATHSPEC"
if [ ! -f .autosetup.patched-omnetpp-operators ]; then
	run patch -p1 < "$PATHROOT/patches/SPEC-CPU2006-omnetpp-operators.patch"
	touch .autosetup.patched-omnetpp-operators
fi

cd "$PATHSPEC/benchspec/CPU2006/403.gcc/src"
if [ ! -f .autosetup.patched-gcc ]; then
	run patch -p0 < "$PATHROOT/patches/SPEC-CPU2006-gcc-cse-ptrarith-r89543.patch"
	run patch -p2 < "$PATHROOT/patches/SPEC-CPU2006-gcc-reload1-r62672.patch"
	touch .autosetup.patched-gcc
fi
cd "$PATHSPEC"

if [ "$VERSIONPERL" != none ]; then
	# install perlbrew (needed by SPEC CPU2006), fixing its installer in the process
	source "$PATHROOT/autosetup/packages/perlbrew.inc"

	# install perl packages
	source "$PATHROOT/autosetup/packages/perl-packages.inc"
fi

# Clean existing SPEC builds and run directories
if [ "$CLEAN_SPEC" -ne 0 ]; then
	find "$PATHSPEC/benchspec/CPU2006" -name build -type d | xargs rm -rf
	find "$PATHSPEC/benchspec/CPU2006" -name exe   -type d | xargs rm -rf
	find "$PATHSPEC/benchspec/CPU2006" -name run   -type d | xargs rm -rf
fi
