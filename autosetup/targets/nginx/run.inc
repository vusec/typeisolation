source "$PATHROOT/autosetup/benchmarks/ab/run.inc"

rm -rf "$PATHTEMP"
mkdir -p "$PATHTEMP/files"
dd if=/dev/urandom of="$PATHTEMP/files/index.html" bs=1 count="$BENCHMARK_REQ_SIZE"

(
	echo "error_log $PATHTEMP/error.log;"
	echo "lock_file $PATHTEMP/nginx.lock;"
	echo "pid $PATHTEMP/nginx.pid;"
	echo "worker_processes $BENCHMARK_SERVER_WORKERS;"
	echo "events { }"
	echo "http {"
	echo "  server {"
	echo "    listen $BENCHMARK_SERVER_PORT;"
	echo "    server_name localhost;"
	echo "    location / {"
        echo "      root $PATHTEMP/files;"
	echo "    }"
	echo "  }"
	echo "}"
) > "$PATHTEMP/nginx.conf"

echo "starting nginx on port $SERVER_PORT"
eval "$run_wrapper" "$PATHSERVER" -c "$PATHTEMP/nginx.conf"


ab_run

echo "stopping nginx"
"$PATHSERVER" -c "$PATHTEMP/nginx.conf" -s quit

sleep 5
rm -rf "$PATHTEMP" || true
