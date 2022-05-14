source "$PATHROOT/autosetup/passes/helper/reset.inc"
source "$PATHROOT/autosetup/passes/helper/clang-plugin.inc"
source "$PATHROOT/autosetup/passes/helper/lto.inc"

# tcmalloc settings
CONFIG_MALLOC=tcmalloc

# passes
set_clang_plugin sizeof-types
set_clang_pass -decode-sizeof-types
add_lto_args -sizeof-types
add_lto_args -inline-malloc-wrappers
add_lto_args -dump-ir -debug-only=dump-ir

source "$PATHROOT/autosetup/passes/helper/tcmalloc.inc"
