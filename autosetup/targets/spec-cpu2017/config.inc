source $PATHROOT/scripts/linker-musl.inc
for conftype in build run; do
    configname="MetAlloc-$instancename-$conftype"
    (
        echo "#--------- Preprocessor -------------------------------------------------------"
        if [ -n "$aarch64" ]; then
            echo "%define model    -mabi=lp64"
        else
            echo "%define model    -m64"
        fi
		echo ""
        if [ "$DISABLE_OPT" -eq 1 ]; then
            echo "%define olevel   -O0"
		else
			echo "%define olevel   -O2"
		fi
        echo ""
        echo "#--------- Global Settings ----------------------------------------------------"
        echo "label                 = MetAlloc-$instancename"
        echo "makeflags             = -j$JOBS"
        [ "$conftype" = run -a -n "$PATHSP17OUT" ] && echo "output_root           = $PATHSP17OUT"
        echo "reportable            = no"
        echo "strict_rundir_verify  = no"
        echo "teeout                = yes"
        echo "tune                  = base"
        echo "submit = taskset -c 2 \${command}"
        echo ""
        echo "#--------- How Many CPUs? -----------------------------------------------------"
        echo "intrate,fprate:"
        echo "    copies            = $COPIES"
        echo "intspeed,fpspeed:"
        echo "    threads           = $THREADS"
        echo ""
        echo "#------- Compilers ------------------------------------------------------------"
        echo "default:"
      printf "    CC                 = %-75s        -std=c99     %%{model}\n" "$cc"
      printf "    CXX                = %-75s        -std=c++03   %%{model}\n" "$cxx"
        echo "    FC                 = `which false`"
        echo "    CC_VERSION_OPTION  = --version"
        echo "    CXX_VERSION_OPTION = --version"
        echo "    FC_VERSION_OPTION  = --version"
        echo ""
        echo "#--------- Portability --------------------------------------------------------"
        echo "default:"
        #echo "    EXTRA_PORTABILITY = -DSPEC_LP64"
        # XXX musl XXX
        echo "    EXTRA_PORTABILITY = -DSPEC_LP64 -DSPEC_NO_USE_STDIO_PTR -DSPEC_NO_USE_STDIO_BASE -DI_FCNTL -DSPEC_NEED_TIME_H -fno-builtin-ceil -fno-builtin-floor -fno-builtin-floorf -fno-builtin-round -fno-builtin-trunc"
        # XXX end musl XXX
        echo ""
        echo "500.perlbench_r,600.perlbench_s:"
        if [ -n "$aarch64" ]; then
            echo "    PORTABILITY   = -DSPEC_LINUX_AARCH64"
        else
            echo "    PORTABILITY   = -DSPEC_LINUX_X64"
        fi
        echo ""
        echo "523.xalancbmk_r,623.xalancbmk_s:"
        echo "    PORTABILITY   = -DSPEC_LINUX"
        echo ""
        echo "526.blender_r:"
        echo "    PORTABILITY   = -funsigned-char -DSPEC_LINUX"
        echo ""
        echo "#-------- Tuning Flags common to Base and Peak --------------------------------"
        echo "intspeed,fpspeed:"
        #echo "    EXTRA_OPTIMIZE = -fopenmp -DSPEC_OPENMP"
        echo "    EXTRA_OPTIMIZE = -DSPEC_SUPPRESS_OPENMP"
        echo ""
        echo "#--------  Baseline Tuning Flags ----------------------------------------------"
        echo "default=base:"
		if [ -n "$aarch64" ]; then
			echo "    # some testing is needed to determine what optimizations are best for armv8"
			echo "    # this is default:"
			echo "    OPTIMIZE        = -g %{olevel}                                 -fno-unsafe-math-optimizations -fno-tree-loop-vectorize"
			echo "    #"
			echo "    # does this work?"
			echo "   #OPTIMIZE        = -g %{olevel} -mcpu=native                    -fno-unsafe-math-optimizations -fno-tree-loop-vectorize"
			echo "    #"
			echo "    # is this faster?"
            echo "   #OPTIMIZE        = -g %{olevel} -target aarch64 -march=armv8    -fno-unsafe-math-optimizations -fno-tree-loop-vectorize"
			echo "    #"
			echo "    # use this for native 8.3a?"
            echo "   #OPTIMIZE        = -g %{olevel} -target aarch64 -march=armv8.3a -fno-unsafe-math-optimizations -fno-tree-loop-vectorize"
		else
	        #echo "    OPTIMIZE        = -g %{olevel}                 -march=native   -fno-unsafe-math-optimizations -fno-tree-loop-vectorize"
	        #echo "    OPTIMIZE        = -flto -g %{olevel}                 -march=native"
	        echo "    OPTIMIZE        = -flto -g %{olevel}  -fPIE -pie $MUSLCXXHEADERS"
		fi
        echo ""
        #echo "intrate,intspeed=base:"
        #echo "    EXTRA_COPTIMIZE = -fgnu89-inline -fno-strict-aliasing"
		echo ""
        echo "#--------  Custom Options -----------------------------------------------------"
        echo "default: "
        if [ "$conftype" = run -a -n "$run_wrapper" ]; then
            echo "    monitor_wrapper = $run_wrapper \$command"
        fi


      # TODO Set $cflagsbl to EXTRA_CFLAGS or to CC/CXX?
	  # TODO get rid of -O3 in cflags?
	  # TODO linkflags?


        echo "CLD         = $cc $LINKERSTART $ldflagsalways $ldflagsnolib  -fPIE -pie"
        echo "CXXLD       = $cxx $LINKERSTART $ldflagsalways $ldflagsnolib  -fPIE -pie"
        echo "EXTRA_LIBS = $LINKEREND"
        if [ "$conftype" = build -a ${#post_build_hooks[@]} -gt 0 ]; then
            echo ""
            echo -n "build_post_bench ="
            echo -n "binary=\"\`pwd\`/\`echo \$commandexe |"
            echo -n " sed 's/_\\[a-z0-9\\]\\\\+\\\\.MetAlloc-$instancename\\\\\\$//'\`\"; "
            for hook in "${post_build_hooks[@]}"; do
                echo -n " $hook \"\\\$binary\";"
            done
/           echo ""
        fi
        echo ""
    ) > "$PATHSP17/config/$configname.cfg"




    if [ "$conftype" = run ]; then
        runtypes="runspec measuremem"
        scriptbasepath="$PATHAUTOSCRIPTSRUN"
    else
        runtypes="runspec"
        scriptbasepath="$PATHAUTOSCRIPTSBUILD"
    fi



    
    for runtype in $runtypes; do
        if [ "$runtype" = runspec ]; then
            scriptpath="$scriptbasepath/$conftype-spec-cpu2017-$instancename.sh"
        else
            scriptpath="$scriptbasepath/$conftype-$runtype-spec-cpu2017-$instancename.sh"
        fi
        (
            runscript_common_start
            echo "cd \"$PATHSP17\""
            echo "source \"$PATHSP17/shrc\""
            if [ "$conftype" = run -a -n "$PATHSP17OUT" ]; then
                echo "rm -rf \"$PATHSP17OUT\""
                echo "mkdir -p \"$PATHSP17OUT\""
                echo "mkdir -p \"$PATHSP17/result\""
                echo "ln -s \"$PATHSP17/result\" \"$PATHSP17OUT\""
                echo "for arg in \"\$@\"; do"
                echo "  if [ -d \"$PATHSP17/benchspec/CPU/\$arg/exe\" ]; then"
                echo "    mkdir -p \"$PATHSP17OUT/benchspec/CPU/\$arg\""
                echo "    cp -r \"$PATHSP17/benchspec/CPU/\$arg/exe\" \"$PATHSP17OUT/benchspec/CPU/\$arg\""
                echo "  fi"
                echo "done"
            fi
            if [ "$runtype" = measuremem ]; then
                echo "runcpu --config=\"$configname\" --action=setup \"\$@\""
            fi
#           [ "$conftype" = run ] && echo -n "\"$PATHROOT/nothp/nothp\" "
            if [ "$runtype" = measuremem ]; then
                if [ "$conftype" = run -a -n "$PATHSP17OUT" ]; then
                    pathspecrun="$PATHSP17OUT"
                else
                    pathspecrun="$PATHSP17"
                fi
                echo "$PATHROOT/autosetup/targets/spec-cpu2017/measuremem.py \"$pathspecrun\" \"$instancename\" \"\$@\""
            else
                echo -n "runcpu --config=\"$configname\" \"\$@\""
                [ "$conftype" = build ] && echo -n " --action=build"
                [ "$conftype" = run ] && echo -n " --nobuild --use-submit-for-speed"
                if [ "$conftype" = run -a -n "$PATHSP17OUT" ]; then
                    echo " | sed 's,$PATHSP17OUT/result/,$PATHSP17/result/,g'"
#                   echo "cp \`find \"$PATHSP17OUT\" -name simpleprof.*.txt\` \"$PATHSP17/result\" || true"
                    echo "rm -rf \"$PATHSP17OUT\""
                else
                    echo ""
                fi
            fi
            runscript_common_end
        ) > "$scriptpath"
        run chmod u+x "$scriptpath"
    done
done
