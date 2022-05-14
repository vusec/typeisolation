source "$PATHROOT/autosetup/passes/helper/reset.inc"
source "$PATHROOT/autosetup/passes/helper/lto.inc"

# tcmalloc settings
CONFIG_MALLOC=tcmalloc

# passes
add_lto_args_nolib -typeisolation -typeisolation-new

source "$PATHROOT/autosetup/passes/helper/tcmalloc.inc"
