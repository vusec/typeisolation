source "$PATHROOT/autosetup/passes/helper/reset.inc"

# tcmalloc settings
CONFIG_MALLOC=tcmalloc

# compiler settings
cc="$CAVERPATH/build/bin/clang"
cxx="$CAVERPATH/build/bin/clang++"

# passes
cflags="$cflags -fsanitize=cver -fsanitize=cver-stack"
ldflagsalways="$ldflagsalways -fsanitize=cver -fsanitize=cver-stack"

source "$PATHROOT/autosetup/passes/helper/tcmalloc.inc"
