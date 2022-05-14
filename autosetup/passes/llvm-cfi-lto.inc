source "$PATHROOT/autosetup/passes/helper/reset.inc"

# tcmalloc settings
CONFIG_MALLOC=tcmalloc

# safestack
cflags="$cflags -fsanitize=cfi-icall -fsanitize=cfi-vcall -fvisibility=default"
ldflagsalways="$ldflagsalways -fsanitize=cfi-icall -fsanitize=cfi-vcall -fvisibility=default"

source "$PATHROOT/autosetup/passes/helper/tcmalloc.inc"
