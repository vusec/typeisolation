source $PATHROOT/scripts/linker-musl.inc
for conftype in build run; do
	configname="MetAlloc-$instancename-$conftype"
	(
		echo "tune        = base"
		echo "ext         = MetAlloc-$instancename"
		echo "reportable  = 0"
		echo "teeout      = 1"
		echo "teerunout   = 0"
		echo "makeflags   = -j$JOBS"
		[ "$conftype" = run -a -n "$PATHSPEC00OUT" ] && echo "output_root = $PATHSPEC00OUT"
		echo ""
		echo "default=default=default=default:"
		echo "CC          = $cc $cflagsbl $MUSLCHEADERS"
		echo "CXX         = $cxx $cflagsbl $MUSLCXXHEADERS"
		echo "FC          = `which false`"
		if [ "$DISABLE_OPT" -eq 1 ]; then
			echo "COPTIMIZE   = -O0 -std=gnu89"
			echo "CXXOPTIMIZE = -O0"
		else
			echo "COPTIMIZE   = -O2 -std=gnu89"
			echo "CXXOPTIMIZE = -O2"
		fi
		echo "CLD         = $cc $LINKERSTART $ldflagsalways $ldflagsnolib -fPIE -pie"
		echo "CXXLD       = $cxx $LINKERSTART $ldflagsalways $ldflagsnolib -fPIE -pie"
		echo "EXTRA_LIBS  = $LINKEREND"
		if [ ${#post_build_hooks[@]} -gt 0 ]; then
			echo ""
			echo -n "fdo_post1 = "
			echo -n "binary=\"\`pwd\`/\`echo \$commandexe |"
			echo -n " sed 's/_\\[a-z0-9\\]\\\\+\\\\.MetAlloc-$instancename\\\\\\$//'\`\"; "
			echo -n "export PYTHONPATH=\"$PYTHONPATH\";"
			echo -n "export PATH=\"$PATHAUTOPREFIX/bin:$PATH\";"
			for hook in "${post_build_hooks[@]}"; do
				echo -n " $hook \"\\\$binary\";"
			done
			echo ""
		fi
		if [ "$conftype" = run -a -n "$run_wrapper" ]; then
			echo ""
			echo "monitor_wrapper = $run_wrapper \$command"
		fi
		echo ""
		echo "default=base=default=default:"
		#echo "PORTABILITY    = -DSPEC_CPU_LP64"
		echo "PORTABILITY    = -DSPEC_CPU_LP64 -fno-builtin-ceil -fno-builtin-floor -fno-builtin-floorf -fno-builtin-round -fno-builtin-trunc"
        echo ""
        echo "186.crafty=default=default=default:"
        echo "CPORTABILITY = -DLINUX_i386"
        echo ""
        echo "252.eon=default=default=default:"
        #echo "CXXPORTABILITY = -DHAS_ERRLIST -DSPEC_CPU2000_LP64 -include cstring"
        echo "CXXPORTABILITY = -DSPEC_CPU2000_LP64"
        echo "srcalt = fmax_errno"
        echo ""
        echo "253.perlbmk=default=default=default:"
        #echo "CPORTABILITY = -DSPEC_CPU2000_LINUX_I386 -DSPEC_CPU2000_NEED_BOOL -DSPEC_CPU2000_GLIBC22 -DSPEC_CPU2000_LP64"
        echo "CPORTABILITY = -DSPEC_CPU2000_LINUX_I386 -DSPEC_CPU2000_NEED_BOOL -DSPEC_CPU2000_GLIBC22 -DSPEC_CPU2000_LP64 -DSPEC_CPU2000_NEED_TIME_H"
        echo ""
        echo "254.gap=default=default=default:"
        #echo "CPORTABILITY=-fno-strict-overflow -DSYS_IS_USG -DSYS_HAS_IOCTL_PROTO -DSYS_HAS_TIME_PROTO -DSYS_HAS_SIGNAL_PROTO -DSYS_HAS_ANSI -DSYS_HAS_CALLOC_PROTO -DSPEC_CPU2000_LP64"
        echo "CPORTABILITY=-fno-strict-overflow -fwrapv -DSYS_IS_USG -DSYS_HAS_IOCTL_PROTO -DSYS_HAS_TIME_PROTO -DSYS_HAS_SIGNAL_PROTO -DSYS_HAS_ANSI -DSYS_HAS_CALLOC_PROTO -DSPEC_CPU2000_LP64"
        echo ""
        echo "255.vortex=default=default=default:"
        echo "CPORTABILITY=-DSPEC_CPU2000_LP64"
        echo ""
        echo "300.twolf=default=default=default:"
        echo "CPORTABILITY=-DHAVE_SIGNED_CHAR -Wno-return-type"
        echo ""
	) > "$PATHSPEC00/config/$configname.cfg"

	if [ "$conftype" = run ]; then
		runtypes="runspec measuremem"
		scriptbasepath="$PATHAUTOSCRIPTSRUN"
	else
		runtypes="runspec"
		scriptbasepath="$PATHAUTOSCRIPTSBUILD"
	fi

	for runtype in $runtypes; do
		if [ "$runtype" = runspec ]; then
			scriptpath="$scriptbasepath/$conftype-spec-cpu2000-$instancename.sh"
		else
			scriptpath="$scriptbasepath/$conftype-$runtype-spec-cpu2000-$instancename.sh"
		fi
		(
			runscript_common_start
			if [ "$conftype" = run ]; then
				echo ": \${SPEC_FORCE_CPU=1}"
				echo "if [ \"\$SPEC_FORCE_CPU\" -eq 0 ]; then"
				echo "    SPEC_FORCE_CPU_CMD=\"\""
				echo "else"
				echo "    SPEC_FORCE_CPU_CMD=\"taskset -c \${SPEC_FORCE_CPUID-${SPEC_FORCE_CPUID-0}}\""
				echo "fi"
				echo ""
			fi
			echo "export PERLBREW_HOME=\"$PERLBREW_HOME\""
			echo "export PERLBREW_ROOT=\"$PERLBREW_ROOT\""
			echo "cd \"$PATHSPEC00\""
			echo "source \"$PERLBREW_ROOT/etc/bashrc\""
			echo "source \"$PATHSPEC00/shrc\""
			if [ "$conftype" = run -a -n "$PATHSPEC00OUT" ]; then
				echo "rm -rf \"$PATHSPEC00OUT\""
				echo "mkdir -p \"$PATHSPEC00OUT\""
				echo "mkdir -p \"$PATHSPEC00/result\""
				echo "ln -s \"$PATHSPEC00/result\" \"$PATHSPEC00OUT\""
				echo "for arg in \"\$@\"; do"
				echo "  if [ -d \"$PATHSPEC00/benchspec/CPU2000/\$arg/exe\" ]; then"
				echo "    mkdir -p \"$PATHSPEC00OUT/benchspec/CPU2000/\$arg\""
				echo "    cp -r \"$PATHSPEC00/benchspec/CPU2000/\$arg/exe\" \"$PATHSPEC00OUT/benchspec/CPU2000/\$arg\""
				echo "  fi"
				echo "done"
			fi
			if [ "$runtype" = measuremem ]; then
				echo "runspec --config=\"$configname\" --action=setup \"\$@\""
			fi
			#echo "source \"$PATHROOT/scripts/kill-tree-on-interrupt.inc\""
			#echo -n "killwrap_tree "
			[ "$conftype" = run ] && echo -n "\"$PATHROOT/nothp/nothp\" "
			[ "$conftype" = run ] && echo -n "\$SPEC_FORCE_CPU_CMD "
			if [ "$runtype" = measuremem ]; then
				if [ "$conftype" = run -a -n "$PATHSPEC00OUT" ]; then
					pathspecrun="$PATHSPEC00OUT"
				else
					pathspecrun="$PATHSPEC00"
				fi
				echo "$PATHROOT/autosetup/targets/spec-cpu2000/measuremem.py \"$pathspecrun\" \"$instancename\" \"\$@\""
			else
				echo -n "runspec --config=\"$configname\" \"\$@\""
				[ "$conftype" = build ] && echo -n " --action=build"
				if [ "$conftype" = run -a -n "$PATHSPEC00OUT" ]; then
					echo " | sed 's,$PATHSPEC00OUT/result/,$PATHSPEC00/result/,g'"
					echo "cp \`find \"$PATHSPEC00OUT\" -name simpleprof.*.txt\` \"$PATHSPEC00/result\" || true"
					echo "rm -rf \"$PATHSPEC00OUT\""
				else
					echo ""
				fi
			fi
			runscript_common_end
		) > "$scriptpath"
		run chmod u+x "$scriptpath"
	done
done
