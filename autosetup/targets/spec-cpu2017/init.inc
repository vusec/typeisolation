: ${CLEAN_SPEC:=0}

if [ -f "$PATHSP17/shrc" ]; then
	echo "using existing SPEC CPU2017 installation"
else
	echo "installing SPEC CPU2017"
    PATHSP17ISO="$PATHAUTOTARGETSRC/CPU2017.iso"
	# [ -f "$PATHSP17ISO" ] || wget -O "$PATHSP17ISO" "https://vvdveen.com/vusec-spec-cpu-2017.iso"
	PATHSP17CD="$PATHAUTOTARGETSRC/spec-cpu2017-cd"
    [ -d "$PATHSP17CD" ] || mkdir -p "$PATHSP17CD"

    if which prun > /dev/null 2>&1; then
        [ -f "$PATHSP17CD/install.sh" ] || fuseiso "$PATHSP17ISO" "$PATHSP17CD" 
	    cd "$PATHSP17CD"
    	run ./install.sh -f -d "$PATHSP17"
        fusermount -q -u "$PATHSP17CD"
    else
        [ -f "$PATHSP17CD/install.sh" ] || sudo mount -o loop,ro "$PATHSP17ISO" "$PATHSP17CD" 
	    cd "$PATHSP17CD"
    	run ./install.sh -f -d "$PATHSP17"
        # sudo umount "$PATHSP17CD"
    fi
fi

cd "$PATHSP17"
if [ ! -f .autosetup.patched-gcc ]; then
	run patch -p0 < "$PATHROOT/patches/SPEC-CPU2017-gcc-fix-obstack.patch"
	touch .autosetup.patched-gcc
fi

# Clean existing SPEC builds and run directories
if [ "$CLEAN_SPEC" -ne 0 ]; then
	find "$PATHSP17/benchspec/CPU" -name build -type d | xargs rm -rf
	find "$PATHSP17/benchspec/CPU" -name exe   -type d | xargs rm -rf
	find "$PATHSP17/benchspec/CPU" -name run   -type d | xargs rm -rf
fi
