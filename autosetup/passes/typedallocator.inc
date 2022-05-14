source "$PATHROOT/autosetup/passes/helper/reset.inc"
source "$PATHROOT/autosetup/passes/helper/clang-plugin.inc"
source "$PATHROOT/autosetup/passes/helper/lto.inc"
source "$PATHROOT/autosetup/passes/helper/debugsym.inc"

# tcmalloc settings
CONFIG_MALLOC=tcmalloc-typed

source "$PATHROOT/autosetup/passes/helper/tcmalloc.inc"
