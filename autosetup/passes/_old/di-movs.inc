source "$PATHROOT/autosetup/passes/helper/reset.inc"
source "$PATHROOT/autosetup/passes/helper/dyninst.inc"

# tcmalloc settings
CONFIG_MALLOC=default

# di-opt options first
add_dyninst_args -detach=0
# then our passes + options
#add_dyninst_args -sleep
#add_dyninst_args -timeout=30
add_dyninst_args -movs

# This will
# - set the compiler to gcc
# - construct the di-opt arguments and set run_wrapper
source "$PATHROOT/autosetup/passes/helper/dyninst-post.inc"
