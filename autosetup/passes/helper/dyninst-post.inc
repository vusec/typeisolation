
# compiler settings
cc="`which gcc`"
cxx="`which g++`"
cflags="$cflags -O3"
ldflagsalways="$ldflagsalways -O3"

run_wrapper="$PATHAUTOPREFIX/bin/di-opt -load=$PATHDYNINSTPLUGINS/libplugins.so $diopts -args "

