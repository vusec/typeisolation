echo "downloading musl-${VERSIONMUSL}"

cd "$PATHAUTOTARGETSRC"
[ -f "musl-${VERSIONMUSL}.tar.gz" ] || run wget "https://musl.libc.org/releases/musl-${VERSIONMUSL}.tar.gz"
