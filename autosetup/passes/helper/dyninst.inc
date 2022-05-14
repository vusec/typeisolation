cflags="$cflags"
ldflagsalways="$ldflagsalways"

# helper for pass config files
add_dyninst_args() {
    for arg in "$@"; do
        diopts="$diopts $arg"
    done
}
