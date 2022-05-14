source "$PATHROOT/autosetup/passes/helper/reset.inc"
source "$PATHROOT/autosetup/passes/helper/clang-plugin.inc"
source "$PATHROOT/autosetup/passes/helper/lto.inc"
source "$PATHROOT/autosetup/passes/helper/debugsym.inc"

# tcmalloc settings
CONFIG_MALLOC=tcmalloc-typed

# passes
add_lto_args_nolib -canonicalizestuff
add_lto_args_nolib -typeisolation

source "$PATHROOT/autosetup/passes/helper/tcmalloc.inc"
