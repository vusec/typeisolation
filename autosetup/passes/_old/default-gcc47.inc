source "$PATHROOT/autosetup/passes/helper/reset.inc"

# tcmalloc settings
CONFIG_MALLOC=default

# compiler settings
cc="`which gcc-4.7`"
cxx="`which g++-4.7`"
cflags="$cflags -O2"
ldflagsalways="$ldflagsalways -O2"
