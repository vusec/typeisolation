#source "$PATHROOT/autosetup/benchmarks/ab/init.inc"

echo "downloading $VERSIONNGINX"
cd "$PATHAUTOTARGETSRC"
[ -f "$VERSIONNGINX.tar.gz" ] || run wget "http://nginx.org/download/$VERSIONNGINX.tar.gz"
