if [ "$MEASUREMEM" -eq 0 ]; then
	run_nomeasuremem "$runscript" -n 1 "$benchmark"
else
	run_nomeasuremem "$measurememscript" "$benchmark"
fi
