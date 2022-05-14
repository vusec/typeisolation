source "$PATHROOT/autosetup/passes/helper/reset.inc"
source "$PATHROOT/autosetup/passes/helper/clang-plugin.inc"
source "$PATHROOT/autosetup/passes/helper/lto.inc"
source "$PATHROOT/autosetup/passes/helper/debugsym.inc"

# tcmalloc settings
CONFIG_MALLOC=tcmalloc-typed

# passes
set_clang_plugin sizeof-types
set_clang_pass -decode-sizeof-types
add_lto_args -sizeof-types
add_lto_args -inline-malloc-wrappers
add_lto_args -typedmalloc

source "$PATHROOT/autosetup/passes/helper/tcmalloc.inc"
