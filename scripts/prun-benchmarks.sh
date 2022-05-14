#!/bin/bash

set -e

: ${PATHROOT:="$PWD"}
if [ ! -f "$PATHROOT/autosetup.sh" ]; then
	echo "Please execute from the root of the repository or set PATHROOT" >&2
	exit 1
fi

# autosetup/config.inc provides defaults for TARGETS, INSTANCES, and BENCHMARKS_*
source "$PATHROOT/autosetup/config.inc"
source "$PATHROOT/autosetup/paths.inc"

# script settings
: ${MEASUREMEM:=0}       # measure memory consumption
: ${PARALLELMAX:=1}      # number of instances to run in parallel
: ${PRUNITER:=1}         # number of times to invoke prun
: ${PRUNOPTS:=""}        # options to specify to prun (like "-t 30:00" to extend reservation duration)
: ${PRUNNODES:=16}       # number of nodes to use with prun
: ${PRUNPERNODE:=1}      # number of instances per node with prun
: ${RUNSCRIPTVERBOSE:=1} # include a dump of system info in run logs

if [ "$MEASUREMEM" -eq 0 ]; then
	pathoutsuffix=""
else
	pathoutsuffix="-mem"
fi

: ${PATHOUTBASE:="$PATHROOT/results$pathoutsuffix"}
: ${PATHOUT:="$PATHOUTBASE/benchmarks-`date '+%Y%m%d-%H%M%S'`"}

export RUNSCRIPTVERBOSE

sleep 0 &
if ! wait -n > /dev/null 2>&1; then
	echo "No support for wait -n, using newer bash version"
	exec "$PATHROOT/autosetup.dir/install/common/bin/bash" "$0" "$@"
	exit 1
fi

nopackages=0
for target in $TARGETS; do
	for instance in $INSTANCES; do
		if [ ! -f "$PATHAUTOSCRIPTSRUN/run-$target-$instance.sh" ]; then
			NO_PACKAGES="$nopackages" INSTANCES="$instance" TARGETS="$target" "$PATHROOT/autosetup.sh"
			nopackages=1
		fi
	done
done

echo "Running experiments with the following settings:"
echo "- TARGETS         = $TARGETS"
echo "- INSTANCES       = $INSTANCES"
echo "- PATHOUT         = $PATHOUT"
echo "- PRUNITER        = $PRUNITER"
echo "- PRUNOPTS        = $PRUNOPTS"
echo "- PRUNNODES       = $PRUNNODES"
echo "- PRUNPERNODE     = $PRUNPERNODE"
echo "- SPECITER        = $SPECITER"

doprun()
{
	if which prun > /dev/null 2>&1; then
		prun -np "$PRUNNODES" -o "$outfile" -"$PRUNPERNODE" -v $PRUNOPTS "$@"
	else
		for node in `seq 1 "$PRUNNODES"`; do
			if [ "$PRUNPERNODE" -gt 1 ]; then
				for pernode in `seq 1 "$PRUNPERNODE"`; do
					"$@" > "$outfile.$node.$pernode" 2>&1 &
				done
				wait
			else
				"$@" > "$outfile.$node" 2>&1
			fi
		done
	fi
}

run_nomeasuremem()
{
	if [ "$PARALLELMAX" -gt 1 ]; then
		while [ "$parallelcount" -ge "$PARALLELMAX" ]; do
			wait -n || echo "job failed; status=$?"
			parallelcount="`expr "$parallelcount" - 1`" || true
		done
		doprun "$@" &
		parallelcount="`expr "$parallelcount" + 1`"
	else
		doprun "$@" || echo "job failed; outfile=$outfile, status=$?"
	fi
}

run()
{
	if [ "$MEASUREMEM" -eq 0 ]; then
		run_nomeasuremem "$@"
	else
		run_nomeasuremem "`which time`" -v "$@"
	fi
}

parallelcount=0
for i in `seq 1 "$PRUNITER"`; do
	for target in $TARGETS; do
		[ -f "$PATHROOT/autosetup/targets/$target/prun-benchmarks.inc" ] || continue
		source "$PATHROOT/autosetup/targets/$target/prun-benchmarks.inc"

		for benchmark in $benchmarks; do
			for instance in $INSTANCES; do
				echo "`date +%FT%T`: iteration=$i, target=$target, benchmark=$benchmark, instance=$instance"
				pathout="$PATHOUT/$target/$instance/$i"
				mkdir -p "$pathout"

				runscript="$PATHAUTOSCRIPTSRUN/run-$target-$instance.sh"
				measurememscript="$PATHAUTOSCRIPTSRUN/run-measuremem-$target-$instance.sh"
				if [ "$MEASUREMEM" -eq 0 ]; then
					outfile="$pathout/out-$benchmark.txt"
				else
					outfile="$pathout/mem-$benchmark.txt"
				fi
				source "$PATHROOT/autosetup/targets/$target/prun-runcmd.inc"
			done
		done
	done
done
echo "`date +%FT%T`: done"
