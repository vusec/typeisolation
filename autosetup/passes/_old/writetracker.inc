source "$PATHROOT/autosetup/passes/helper/reset.inc"
source "$PATHROOT/autosetup/passes/helper/lto.inc"
source "$PATHROOT/autosetup/passes/helper/metalloc.inc"

# safestack
cflags="$cflags -fsanitize=safe-stack"
ldflagsalways="$ldflagsalways -fsanitize=safe-stack"

# passes
add_lto_args -writetracker
add_lto_args -custominline

source "$PATHROOT/autosetup/passes/helper/tcmalloc.inc"
