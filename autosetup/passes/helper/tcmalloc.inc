prefix="$PATHAUTOPREFIXBASE/$instance"
prefixbin="$prefix/bin"
prefixlib="$prefix/lib"

ldflagsalways="$ldflagsalways -ltcmalloc -lpthread -lunwind"
ldflagschrome="$ldflagschrome $ldflagsalways -L$PATHAUTOPREFIXBASE/chrome-$instance/lib"
ldflagsalways="$ldflagsalways -L$prefix/lib -L$PATHAUTOPREFIX/lib"
