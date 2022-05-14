source "$PATHROOT/autosetup/passes/helper/reset.inc"
source "$PATHROOT/autosetup/passes/helper/dyninst.inc"

# tcmalloc settings
CONFIG_MALLOC=default

# di-opt options first
add_dyninst_args -detach=0
#add_dyninst_args -debug
#add_dyninst_args -debug-pass=dummy
# then our passes + options
add_dyninst_args -dummy 
add_dyninst_args -dummy-opt1=\'custom option\'
add_dyninst_args -dummy-opt2=433
add_dyninst_args -dummy-opt3=1
add_dyninst_args -dummy-opt4=3.142348234

# This will
# - set the compiler to gcc
# - construct the di-opt arguments and set run_wrapper
source "$PATHROOT/autosetup/passes/helper/dyninst-post.inc"
