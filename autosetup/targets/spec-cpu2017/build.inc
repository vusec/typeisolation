: ${PATHOUTBASE:="$PATHROOT/results$pathoutsuffix"}
: ${PATHOUT:="$PATHOUTBASE/build-benchmarks-`date '+%Y%m%d-%H%M%S'`"}

for benchmark in $BENCHMARKS_SPEC; do
    allowbenchinstance || continue
    echo "building spec-cpu2017-$instancename $benchmark"
    buildscript="$PATHAUTOSCRIPTSBUILD/build-spec-cpu2017-$instancename.sh"

    if [ "$PRUN" -eq 1 ]; then
        outfile="$PATHLOG.spec-cpu2017-$instancename-$benchmark"
        rm -f $outfile.0
        touch $outfile.0
        eval prun -np 1 -o "$outfile" -1 -v "$PRUNOPTS" "$buildscript" "$benchmark" &
    else
        logsuffix="spec-cpu2017-$instancename-$benchmark" run time "$buildscript" "$benchmark"
    fi
done

if [ "$PRUN" -eq 1 ]; then
    for benchmark in $BENCHMARKS_SPEC; do
        allowbenchinstance || continue
        outfile="$PATHLOG.spec-cpu2017-$instancename-$benchmark.0"
        echo "waiting for spec-cpu2017-$instancename $benchmark build in $outfile"
        until grep -q "The log for this run is in" "$outfile"; do
            sleep 1
        done
        if grep -q "Error building" "$outfile"; then
            echo "Build failed for $benchmark. Please check $outfile"
        else
            echo "Build success for $benchmark"
        fi
    done
fi

echo "updating spec-cpu2017-$instancename MD5s"
configbuild="$PATHSP17/config/MetAlloc-$instancename-build.cfg"
configrun="$PATHSP17/config/MetAlloc-$instancename-run.cfg"
echo "configbuild: $configbuild"
lineno="`grep -a -n __HASH__ "$configbuild" | cut -d: -f1`"
tail -n "+$lineno" "$configbuild" >> "$configrun"

