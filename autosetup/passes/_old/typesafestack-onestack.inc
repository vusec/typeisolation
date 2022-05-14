source "$PATHROOT/autosetup/passes/helper/reset.inc"
source "$PATHROOT/autosetup/passes/helper/lto.inc"

# tcmalloc settings
CONFIG_MALLOC=tcmalloc

# passes
add_lto_args_nolib -typesafe-stack
add_lto_args_nolib -typesafe-stack-onestack
add_lto_args_nolib -zeroinitstack

# static libraries
source "$PATHROOT/autosetup/passes/helper/typesafestack-staticlib.inc"

source "$PATHROOT/autosetup/passes/helper/tcmalloc.inc"
