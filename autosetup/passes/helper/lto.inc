cflags="$cflags -flto"
ldflagsalways="$ldflagsalways -flto -Wl,-plugin-opt=-load=$PATHLLVMPLUGINS/libplugins.so"

if [ "$DISABLE_OPT" -eq 1 ]; then
    cflags="$cflags -g3 -O0"
    ldflagsalways="$ldflagsalways -g3 -O0 -Wl,-plugin-opt=-disable-opt"
fi

# helper for pass config files
add_lto_args() {
    for arg in "$@"; do
        ldflagsalways="$ldflagsalways -Wl,-plugin-opt=$arg"
    done
}
add_lto_args_nolib() {
    for arg in "$@"; do
        ldflagsnolib="$ldflagsnolib -Wl,-plugin-opt=$arg"
    done
}
