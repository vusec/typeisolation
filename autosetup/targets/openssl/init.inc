#source "$PATHROOT/autosetup/benchmarks/ab/init.inc"

echo "downloading openssl-$VERSIONOPENSSL"
cd "$PATHAUTOTARGETSRC"
[ -f "$VERSIONOPENSSL.tar.gz" ] || run wget "https://github.com/openssl/openssl/archive/$VERSIONOPENSSL.tar.gz"
