source "$PATHROOT/autosetup/passes/helper/reset.inc"

# tcmalloc settings
CONFIG_MALLOC=default

# compiler settings
cc="`which gcc`"
cxx="`which g++`"
cflags="$cflags -O3"
ldflagsalways="$ldflagsalways -O3"
ar="`which ar`"
ranlib="`which ranlib`"
