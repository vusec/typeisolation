: ${CLEAN_SPEC00:=0}

if [ -f "$PATHSPEC00/shrc" ]; then
	echo "using existing SPEC CPU2000 installation"
else
	echo "installing SPEC CPU2000"
	PATHSPEC00CD="$PATHAUTOTARGETSRC/spec-cpu2000-cd"
	[ -d "$PATHSPEC00CD/.git" ] || run git clone git@bitbucket.org:vusec/spec-cpu2000-cd.git "$PATHSPEC00CD"
    mkdir "$PATHSPEC00"
	cd "$PATHSPEC00CD"
	run ./install.sh "linux-glibc22-x86_64" "$PATHSPEC00"
fi

cd "$PATHSPEC00"

if [ "$VERSIONPERL" != none ]; then
	# install perlbrew (needed by SPEC CPU2006), fixing its installer in the process
	source "$PATHROOT/autosetup/packages/perlbrew.inc"

	# install perl packages
	source "$PATHROOT/autosetup/packages/perl-packages.inc"
fi

# Clean existing SPEC builds and run directories
if [ "$CLEAN_SPEC00" -ne 0 ]; then
	find "$PATHSPEC00/benchspec/CPU2000" -name build -type d | xargs rm -rf
	find "$PATHSPEC00/benchspec/CPU2000" -name exe   -type d | xargs rm -rf
	find "$PATHSPEC00/benchspec/CPU2000" -name run   -type d | xargs rm -rf
fi
