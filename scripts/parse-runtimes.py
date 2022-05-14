#!/bin/python

import fileinput
import math
import re
import sys

def compute_mean(list):
	if len(list) < 1: return None
	sum = 0.0
	for value in list:
		sum += value
	return sum / len(list)

def compute_median(list):
	if len(list) < 1: return None
	list.sort()
	if len(list) % 2:
		return list[len(list) / 2]
	else:
		return (list[len(list) / 2 - 1] + list[len(list) / 2]) / 2

def compute_count(list):
	return len(list)

def compute_min(list):
	if len(list) < 1: return None
	list.sort()
	return list[0]

def compute_max(list):
	if len(list) < 1: return None
	list.sort()
	return list[len(list) - 1]

def compute_var(list):
	if len(list) < 2: return None
	sum = 0.0
	sum2 = 0.0
	for value in list:
		sum += value
		sum2 += value * value
	return (sum2 - sum * sum / len(list)) / (len(list) - 1)

def compute_stdev(list):
	return math.sqrt(compute_var(list))

def compute_stdev_pct(list):
	mean = compute_mean(list);
	if not mean: return None
	return 100.0 * compute_stdev(list) / mean

def compute_geomean(list):
	if len(list) < 1: return None
	sum = 0.0
	for value in list:
		sum += math.log(value)
	return math.exp(sum / len(list))

def sortset(s):
	l = list(s)
	l.sort()
	return l

def write_aggregate(aggregate, list):
	value = aggregate[2](list)
	if value is not None: sys.stdout.write(aggregate[1] % value)

aggregates = [
	("mean",   "%.2f", compute_mean),
	("median", "%.2f", compute_median),
	("min",    "%.2f", compute_min),
	("max",    "%.2f", compute_max),
	("count",  "%d",   compute_count),
	("stdev",  "%.2f", compute_stdev),
	("stdev%", "%.1f", compute_stdev_pct)];

benchmarks = set()
configs = set()
runtimes = {}
regex = re.compile(".*/([^/]*)-iter[0-9]+/[0-9]+/([^/]*)-out\.[0-9]+:runbench_secs = ([0-9.]+)")
for line in fileinput.input():
	match = regex.match(line)
	if not match:
		sys.stderr.write("Bad line: %s" % line)
		continue

	config = match.group(1)
	configs.add(config)
	
	benchmark = match.group(2)
	benchmarks.add(benchmark);

	index = (config, benchmark)
	if not runtimes.get(index):
		runtimes[index] = []
	
	runtime = float(match.group(3))
	runtimes[index].append(runtime)

benchmarks = sortset(benchmarks)
configs = sortset(configs)

for aggregate in aggregates:
	for config in configs:
		sys.stdout.write("\t%s" % aggregate[0])
sys.stdout.write("\n")

for aggregate in aggregates:
	for config in configs:
		sys.stdout.write("\t%s" % config)
sys.stdout.write("\n")

for benchmark in benchmarks:
	sys.stdout.write("%s" % benchmark)
	for aggregate in aggregates:
		for config in configs:
			sys.stdout.write("\t")
			write_aggregate(aggregate, runtimes[(config, benchmark)])
	sys.stdout.write("\n")

comparisons = [("baseline", "default", [])]
for config in configs:
	if config != "baseline" and config != "default":
		comparisons.append((config, "baseline", []))

for aggregate in aggregates[0:3]:
	sys.stdout.write("\n%s slowdown\nreference" % aggregate[0])
	for comparison in comparisons:
		sys.stdout.write("\t%s" % comparison[1])
	sys.stdout.write("\nconfig")
	for comparison in comparisons:
		sys.stdout.write("\t%s" % comparison[0])
	sys.stdout.write("\n")

	for benchmark in benchmarks:
		sys.stdout.write("%s" % benchmark)
		for comparison in comparisons:
			value = aggregate[2](runtimes[(comparison[0], benchmark)])
			refvalue = aggregate[2](runtimes[(comparison[1], benchmark)])
			ratio = value / refvalue
			sys.stdout.write("\t%.3f" % ratio)
			comparison[2].append(ratio)
		sys.stdout.write("\n")

	sys.stdout.write("geomean")
	for comparison in comparisons:
		sys.stdout.write("\t")
		value = compute_geomean(comparison[2])
		if value is not None: sys.stdout.write("%.3f" % value)
	sys.stdout.write("\n")

sys.stdout.write("\n")
sys.stdout.write("benchmark\tconfig")
for aggregate in aggregates:
	sys.stdout.write("\t%s" % aggregate[0])
sys.stdout.write("\t\truntimes\n")

for benchmark in benchmarks:
	for config in configs:
		sys.stdout.write("%s\t%s\t" % (benchmark, config))
		runtimelist = runtimes[(config, benchmark)]
		for aggregate in aggregates:
			write_aggregate(aggregate, runtimelist)
			sys.stdout.write("\t")
		runtimelist.sort()
		for runtime in runtimelist:
			sys.stdout.write("\t%.2f" % runtime)
		sys.stdout.write("\n")

