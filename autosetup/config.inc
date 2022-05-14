BASELINEINSTANCES="
  baseline
  baseline-lto
  default
  default-lto
  "

LLVMINSTANCES="
  llvm-cfi-lto
  llvm-lvi-cfi-lto
  llvm-retpoline-lto
  llvm-retpoline-lvi-cfi-lto
  llvm-safestack-lto
  llvm-stackprotector-all-lto
  llvm-stackprotector-strong-lto
  "

DEFAULTINSTANCES="
  baseline-lto
  typedmalloc
  typedmalloc-inline
  typedmalloc-typeisolation
  typeisolation
  typeisolation-stores
  typesafestack
  typesafestack-typedmalloc
  typesafestack-typedmalloc-inline-typeisolation
  typesafestack-typedmalloc-typeisolation
  typesafestack-typeisolation
  "

: ${GNUMIRROR=http://ftp.gnu.org/gnu}
: ${INSTANCES="$DEFAULTINSTANCES"}
: ${INSTANCESUFFIX=}
: ${JOBSMAX=16}
: ${NEEDDYNINST=0}
: ${TARGETS=spec-cpu2006}
: ${THREADCOUNTS:="1 2 4 8 16 32 64"}

for target in $TARGETS; do
	if [ "$target" = "spec-cpu2000" ]; then
		source "$PATHROOT/autosetup/targets/spec-cpu2000/benchmarks.inc" # BENCHMARKS_SPEC for 2000
	fi
	if [ "$target" = "spec-cpu2006" ]; then
		source "$PATHROOT/autosetup/targets/spec-cpu2006/benchmarks.inc" # BENCHMARKS_SPEC for 2006
	fi
	if [ "$target" = "spec-cpu2017" ]; then
		source "$PATHROOT/autosetup/targets/spec-cpu2017/benchmarks.inc" # BENCHMARKS_SPEC for 2017
	fi
done

allowbenchinstance()
{
	[ "$benchmark" = 471.omnetpp -a "$instance" = caver ] && return 1
	return 0
}

