CONFIG_STATICLIBS=typesafestack
ldflagsalways="$ldflagsalways -ldl"
ldflagsnolib="$ldflagsnolib -L$PATHAUTOFRAMEWORKOBJ/staticlib-typesafestack-$instance"
ldflagsnolib="$ldflagsnolib -Wl,-whole-archive -l:libtypesafestack.a -Wl,-no-whole-archive"
