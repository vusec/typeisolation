#--------- How Many CPUs? -----------------------------------------------------
# Both SPECrate and SPECspeed can test multiple chips / cores / hw threads
#    - For SPECrate,  you set the number of copies.
#    - For SPECspeed, you set the number of threads.
# See: https://www.spec.org/cpu2017/Docs/system-requirements.html#MultipleCPUs
#
#    q. How many should I set?
#    a. Unknown, you will have to try it and see!
#
# To get you started, some suggestions:
#
#     copies - This config file defaults to testing only 1 copy.   You might
#              try changing it to match the number of cores on your system,
#              or perhaps the number of virtual CPUs as reported by:
#                     grep -c processor /proc/cpuinfo
#              Be sure you have enough memory.  See:
#              https://www.spec.org/cpu2017/Docs/system-requirements.html#memory
#
#     threads - This config file sets a starting point.  You could try raising
#               it.  A higher thread count is much more likely to be useful for
#               fpspeed than for intspeed.
#
#intrate,fprate:
#   copies           = 1   # EDIT to change number of copies (see above)
#intspeed,fpspeed:
#   threads          = 4   # EDIT to change number of OpenMP threads (see above)


# https://www.spec.org/cpu2017/Docs/system-requirements.html
# https://www.spec.org/cpu2017/Docs/system-requirements.html#MultipleCPUs
# https://www.spec.org/cpu2017/Docs/system-requirements.html#memory
# https://www.spec.org/cpu2017/Docs/index.html#intspeed




################### SPECspeed ####
# SPECspeed suites always run one copy of each benchmark. 
# Higher scores indicate that less time is needed. 
##################################
SPECspeed_C="
    600.perlbench_s
    602.gcc_s
    605.mcf_s
    619.lbm_s
    625.x264_s
    638.imagick_s
    644.nab_s
    657.xz_s
  "
SPECspeed_CPP="
    620.omnetpp_s
    623.xalancbmk_s
    631.deepsjeng_s
    641.leela_s
  "

SPECspeed="$SPECspeed_C $SPECspeed_CPP"

SPECspeed_int="
    600.perlbench_s
    602.gcc_s
    605.mcf_s
    620.omnetpp_s
    623.xalancbmk_s
    625.x264_s
    631.deepsjeng_s
    641.leela_s
    657.xz_s
  "
SPECspeed_fp="
    619.lbm_s
    638.imagick_s
    644.nab_s
  "



################### SPECrate ####
# SPECrate suites run multiple concurrent copies of each benchmark.
# The tester selects how many.
#################################
SPECrate_C="
    500.perlbench_r
    502.gcc_r
    505.mcf_r
    519.lbm_r
    525.x264_r
    538.imagick_r
    544.nab_r
    557.xz_r
  "
SPECrate_CPP="
    508.namd_r
    510.parest_r
    511.povray_r
    520.omnetpp_r
    523.xalancbmk_r
    531.deepsjeng_r
    541.leela_r
  "

SPECrate="$SPECrate_C $SPECrate_CPP"

SPECrate_int="
    500.perlbench_r
    502.gcc_r
    505.mcf_r
    520.omnetpp_r
    523.xalancbmk_r
    525.x264_r
    531.deepsjeng_r
    541.leela_r
    557.xz_r
  "

SPECrate_fp="
    508.namd_r
    510.parest_r
    511.povray_r
    519.lbm_r
    538.imagick_r
    544.nab_r
  "

SPECall="$SPECspeed $SPECrate"

################### Metalloc default ####
: ${BENCHMARKS_SPEC:="$SPECspeed"}

[ "$BENCHMARKS_SPEC" = "SPECspeed"     ] && BENCHMARKS_SPEC=$SPECspeed
[ "$BENCHMARKS_SPEC" = "SPECspeed_C"   ] && BENCHMARKS_SPEC=$SPECspeed_C
[ "$BENCHMARKS_SPEC" = "SPECspeed_CPP" ] && BENCHMARKS_SPEC=$SPECspeed_CPP
[ "$BENCHMARKS_SPEC" = "SPECspeed_int" ] && BENCHMARKS_SPEC=$SPECspeed_int
[ "$BENCHMARKS_SPEC" = "SPECspeed_fp"  ] && BENCHMARKS_SPEC=$SPECspeed_fp


[ "$BENCHMARKS_SPEC" = "SPECrate"     ] && BENCHMARKS_SPEC=$SPECrate
[ "$BENCHMARKS_SPEC" = "SPECrate_C"   ] && BENCHMARKS_SPEC=$SPECrate_C
[ "$BENCHMARKS_SPEC" = "SPECrate_CPP" ] && BENCHMARKS_SPEC=$SPECrate_CPP
[ "$BENCHMARKS_SPEC" = "SPECrate_int" ] && BENCHMARKS_SPEC=$SPECrate_int
[ "$BENCHMARKS_SPEC" = "SPECrate_fp"  ] && BENCHMARKS_SPEC=$SPECrate_fp

[ "$BENCHMARKS_SPEC" = "SPECall"      ] && BENCHMARKS_SPEC=$SPECall

echo -n "Selected the following benchmarks: "
echo $BENCHMARKS_SPEC

