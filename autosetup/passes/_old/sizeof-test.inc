source "$PATHROOT/autosetup/passes/helper/reset.inc"
source "$PATHROOT/autosetup/passes/helper/lto.inc"

# tcmalloc settings
CONFIG_MALLOC=tcmalloc

# no pass, just preprocessor magic
cflags="$cflags -include $PATHROOT/clang-plugins/HookSizeof.h"

source "$PATHROOT/autosetup/passes/helper/tcmalloc.inc"
