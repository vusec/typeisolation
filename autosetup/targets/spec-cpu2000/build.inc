for benchmark in $BENCHMARKS_SPEC00; do
    allowbenchinstance || continue
    echo "building spec-cpu2000-$instancename $benchmark"
    logsuffix="spec-cpu2000-$instancename-$benchmark" run time "$PATHAUTOSCRIPTSBUILD/build-spec-cpu2000-$instancename.sh" "$benchmark"
done

echo "updating spec-cpu2000-$instancename MD5s"
configbuild="$PATHSPEC00/config/MetAlloc-$instancename-build.cfg"
configrun="$PATHSPEC00/config/MetAlloc-$instancename-run.cfg"
lineno="`grep -n __MD5__ "$configbuild" | cut -d: -f1`"
tail -n "+$lineno" "$configbuild" >> "$configrun"
