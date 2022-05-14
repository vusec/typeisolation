#!/usr/bin/env python3

import math
import os
import re
import sys

#import scipy.stats

nodeBlacklist = set()
#nodeBlacklist = { "node005", "node025", "node028", "node046", "node049", "node054", "node055", "node056", "node068" }

pathscripts = os.path.abspath(os.path.dirname(sys.argv[0]))
pathroot = os.path.join(pathscripts, "..")
pathspeccpu2006 = os.path.join(pathroot, "autosetup.dir", "targets", "src", "spec-cpu2006")
pathspeccpu2017 = os.path.join(pathroot, "autosetup.dir", "targets", "src", "spec-cpu2017")
resultBenchmarks = set()
resultInstances = set()
resultNodes = set()
resultTuples = dict()
resultTuplesNode = dict()
instanceRef = None
verbosity = 2

def grep(path, regexstr):
	regex = re.compile(regexstr)
	matches = list()
	f = open(path, "r")
	for line in f:
		match = regex.match(line)
		if match: matches.append(match)
	f.close()
	return matches

def grepone(path, regexstr):
	matches = grep(path, regexstr)
	if len(matches) != 1: return None
	return matches[0].groups()[0];

def addtuple(tuples, key, tuple):
	if key not in tuples: tuples[key] = list()
	tuples[key].append(tuple)

def getInstanceName(instance):
	if instance.startswith("MetAlloc-"): instance = instance[9:]
	if instance.endswith(".cfg"): instance = instance[:-4]
	if instance.endswith("-run"): instance = instance[:-4]
	return instance

def processresult(instance, benchmark, outcome, tuple, nodename):
	resultBenchmarks.add(benchmark)
	resultInstances.add(instance)
	resultNodes.add(nodename)
	if nodename in nodeBlacklist:
		return

	if outcome == "Success":
		addtuple(resultTuples, (instance, benchmark), tuple)
		addtuple(resultTuplesNode, (instance, benchmark, nodename), tuple)
	else:
		sys.stderr.write("error outcode \"%s\" for benchmark %s on instance %s\n" % (outcome, benchmark, instance))

	if verbosity >= 3:
		sys.stdout.write("%s\t%s\t%s\t%.3f\t%.0f\t%s\n" % (benchmark, getInstanceName(instance), outcome, tuple[0], tuple[1], nodename))

def processfile_ab(path, instance, nodename, target, memuse):
	threads = grepone(path, "- BENCHMARK_SERVER_WORKERS=([0-9]+)\n")
	if threads is None:
		sys.stderr.write("number of threads not in ab log %s\n" % (path))
		return None

	matchesresult = grep(path, "Requests per second: +([0-9]+\.[0-9]+) \[#/sec\] \(mean\)\n")
	if len(matchesresult) < 1:
		sys.stderr.write("no results in ab log %s\n" % (path))
		return None

	for match in matchesresult:
		reqPerSec = float(match.groups()[0])
		if reqPerSec == 0: continue
		runtime = 1000000 / reqPerSec
		processresult(instance, "%s%%%.3d" % (target, int(threads)), "Success", (runtime, memuse), nodename)

def processfile_parsec_getthreads(path):
	cmd = grepone(path, "\\[autosetup-runscript\\] cmd=(.*)\n")
	isthreads = False
	for part in cmd.split(" "):
		if isthreads: return part
		isthreads = (part == "-n")
	return None

def processfile_parsec(path, instance, nodename, memuse):
	threads = processfile_parsec_getthreads(path)
	if threads is None:
		sys.stderr.write("threads not specified in log file %s\n" % (path))
		return

	regexbenchmark = re.compile("\\[PARSEC\\] \\[=+ Running benchmark (.*) \[[0-9]+\] =+\\]\n")
	regexruntime = re.compile("real[ \t]+([0-9]+)m([0-9]+)\\.([0-9]+)s\n")

	benchmark = None
	found = False
	f = open(path, "r")
	for line in f:
		matchbenchmark = regexbenchmark.match(line)
		if matchbenchmark:
			benchmark = matchbenchmark.groups()[0]
			continue
		matchruntime = regexruntime.match(line)
		if matchruntime:
			if benchmark is None:
				sys.stderr.write("runtime without benchmark name in log file %s\n" % (path))
				continue
			timemin = int(matchruntime.groups()[0])
			timesec = int(matchruntime.groups()[1])
			timemsec = int(matchruntime.groups()[2])
			runtime = timemin * 60 + timesec + timemsec / 1000.0
			processresult(instance, "%s%%%.3d" % (benchmark, int(threads)), "Success", (runtime, memuse), nodename)
			found = True
	f.close()
	if not found:
		sys.stderr.write("runtime not found in log file %s\n" % (path))

def processlog_speccpu2006(path, logpath, instance, nodename):
	matchesresult = grep(logpath, " ([^ ]+) ([^ ]+) base ref ratio=([0-9.]+), runtime=([0-9.]+).*")
	if len(matchesresult) < 1:
		sys.stderr.write("no results in SPEC log %s referenced in %s\n" % (logpath, path))
		return

	for match in matchesresult:
		outcome = match.groups()[0]
		benchmark = match.groups()[1]
		runtime = float(match.groups()[3])
		processresult(instance, benchmark, outcome, (runtime, None), nodename)

def processlog_speccpu2017(path, logpath, instance, nodename):
	matchesresult = grep(logpath, " ([^ ]+) ([^ ]+) base ref[^ ]+ ratio=([0-9.]+), runtime=([0-9.]+).*")
	if len(matchesresult) < 1:
		sys.stderr.write("no results in SPEC log %s referenced in %s\n" % (logpath, path))
		return

	for match in matchesresult:
		outcome = match.groups()[0]
		benchmark = match.groups()[1]
		runtime = float(match.groups()[3])
		processresult(instance, benchmark, outcome, (runtime, None), nodename)

def parse_time_runtime(runtime):
	parts = runtime.split(":")
	if (len(parts) < 2) or (len(parts) > 3): return None
	runtimehour = 0
	if len(parts) > 2: runtimehour = int(parts[0])
	runtimemin = int(parts[len(parts) - 2])
	runtimesec = float(parts[len(parts) - 1])
	return (runtimehour * 60 + runtimemin) * 60 + runtimesec

def processlog_speccpu2006_time(path, instance, nodename, memuse):
	benchmark = grepone(path, "\\[autosetup-runscript\\] cmd=(.*)\n")
	matchesstatus = grep(path, "[ \t]*Exit status: ([-0-9]*)\n")
	matchesruntime = grep(path, "[ \t]*Elapsed \\(wall clock\\) time \\(h:mm:ss or m:ss\\): ([0-9:.]*)\n")
	if (benchmark is None) or (len(matchesstatus)) < 1 or (len(matchesruntime) < 1):
		sys.stderr.write("no results in SPEC time log %s\n" % (path))
		return

	if benchmark.find(" ") >= 0:
		sys.stderr.write("multiple benchmarks run SPEC time log %s, currently not supported\n" % (path))
		return

	outcome = "Success"
	for match in matchesstatus:
		status = int(match.groups()[0])
		if status != 0: outcome = "Error"

	runtimeSum = 0
	for match in matchesruntime:
		runtime = parse_time_runtime(match.groups()[0])
		if runtime is None:
			sys.stderr.write("bad runtime in run SPEC time log %s\n" % (path))
			return
		runtimeSum += runtime
	
	processresult(instance, benchmark, outcome, (runtimeSum, memuse), nodename)


def processlog_speccpu2017_time(path, instance, nodename, memuse):
	benchmark = grepone(path, "\\[autosetup-runscript\\] cmd=(.*)\n")
	matchesstatus = grep(path, "[ \t]*Exit status: ([-0-9]*)\n")
	matchesruntime = grep(path, "[ \t]*Elapsed \\(wall clock\\) time \\(h:mm:ss or m:ss\\): ([0-9:.]*)\n")
	if (benchmark is None) or (len(matchesstatus)) < 1 or (len(matchesruntime) < 1):
		sys.stderr.write("no results in SPEC time log %s\n" % (path))
		return

	if benchmark.find(" ") >= 0:
		sys.stderr.write("multiple benchmarks run SPEC time log %s, currently not supported\n" % (path))
		return

	outcome = "Success"
	for match in matchesstatus:
		status = int(match.groups()[0])
		if status != 0: outcome = "Error"

	runtimeSum = 0
	for match in matchesruntime:
		runtime = parse_time_runtime(match.groups()[0])
		if runtime is None:
			sys.stderr.write("bad runtime in run SPEC time log %s\n" % (path))
			return
		runtimeSum += runtime
	
	processresult(instance, benchmark, outcome, (runtimeSum, memuse), nodename)

def extractlogpaths_speccpu2006_fix(logpath, path):
	if os.path.isfile(logpath): return logpath
	altlogpath = os.path.join(pathspeccpu2006, "result", os.path.basename(logpath))
	if os.path.isfile(altlogpath): return altlogpath
	sys.stderr.write("log file %s referenced in %s does not exist\n" % (logpath, path))
	return None

def extractlogpaths_speccpu2017_fix(logpath, path):
	if os.path.isfile(logpath): return logpath
	altlogpath = os.path.join(pathspeccpu2017, "result", os.path.basename(logpath))
	if os.path.isfile(altlogpath): return altlogpath
	sys.stderr.write("log file %s referenced in %s does not exist\n" % (logpath, path))
	return None

def extractlogpaths_speccpu2006(path):
	matches = grep(path, "The log for this run is in (.*)\n")
	if len(matches) < 1: return None

	logpaths = list()
	for match in matches:
		logpath = extractlogpaths_speccpu2006_fix(match.groups()[0], path)
		if logpath is None: continue
		logpaths.append(logpath)

	return logpaths

def extractlogpaths_speccpu2017(path):
	matches = grep(path, "The log for this run is in (.*)\n")
	if len(matches) < 1: return None

	logpaths = list()
	for match in matches:
		logpath = extractlogpaths_speccpu2017_fix(match.groups()[0], path)
		if logpath is None: continue
		logpaths.append(logpath)

	return logpaths

def processlog_speccpu2000(path, logpath, instance, nodename):
	matchesresult = grep(logpath,
			"spec\.cpu2000\.results\.([^.]+)\.base\.[0-9]{3}\.reported_time: ([0-9.]+)")
	if len(matchesresult) < 1:
		sys.stderr.write("no results in SPEC log %s referenced in %s\n" % (logpath, path))
		return

	for match in matchesresult:
		benchmark = match.groups()[0]
		runtime = float(match.groups()[1])
		processresult(instance, benchmark, "Success", (runtime, None), nodename)

def processfile_speccpu2006(path, instance, nodename, memuse):
	if not (memuse is None):
		processlog_speccpu2006_time(path, instance, nodename, memuse)
	
	logpaths = extractlogpaths_speccpu2006(path)
	if not logpaths:
		sys.stderr.write("no log path for %s\n" % (path))
		return

	for logpath in logpaths:
		processlog_speccpu2006(path, logpath, instance, nodename)

def processfile_speccpu2017(path, instance, nodename, memuse):
	if not (memuse is None):
		processlog_speccpu2017_time(path, instance, nodename, memuse)
	
	logpaths = extractlogpaths_speccpu2017(path)
	if not logpaths:
		sys.stderr.write("no log path for %s\n" % (path))
		return

	for logpath in logpaths:
		processlog_speccpu2017(path, logpath, instance, nodename)

def extractlogpaths_speccpu2000(path):
	print("EXTRACT", path)
	matches = grep(path, ".*format: raw -> (.*)")
	if len(matches) < 1: return None

	logpaths = list()
	for match in matches:
		logpaths.append(match.groups()[0])

	return logpaths

def processfile_speccpu2000(path, instance, nodename, memuse):
	if memuse is not None:
		processlog_speccpu2000_time(path, instance, nodename, memuse)

	logpaths = extractlogpaths_speccpu2000(path)
	if not logpaths:
		sys.stderr.write("no log path for %s\n" % (path))
		return

	for logpath in logpaths:
		processlog_speccpu2000(path, logpath, instance, nodename)

def processfile_memuse(path):
	memuse = None
	matches = grep(path, "[ \t]*Maximum resident set size \\(kbytes\\): (.*)\n")
	for match in matches:
		memuseline = float(match.groups()[0])
		if (memuse is None) or (memuse < memuseline):
			memuse = memuseline

	return memuse

def processfile(path):
	target = grepone(path, "\\[autosetup-runscript\\] target=(.*)\n")
	instance = grepone(path, "\\[autosetup-runscript\\] instancename=(.*)\n")
	nodename = grepone(path, "\\[autosetup-runscript\\] node=(.*)\n")
	if (target is None) or (instance is None) or (nodename is None):
		sys.stderr.write("missing or duplicate header(s) in log file %s\n" % (path))
		return

	dateend = grepone(path, "\\[autosetup-runscript\\] date-end=(.*)\n")
	if dateend is None:
		sys.stderr.write("log file %s is incomplete\n" % (path))
		return

	memuse = processfile_memuse(path)
	if (target == "apache") or (target == "cherokee") or (target == "lighttpd") or (target == "nginx"):
		processfile_ab(path, instance, nodename, target, memuse)
	elif target == "parsec":
		processfile_parsec(path, instance, nodename, memuse)
	elif target == "spec-cpu2000":
		processfile_speccpu2000(path, instance, nodename, memuse)
	elif target == "spec-cpu2006":
		processfile_speccpu2006(path, instance, nodename, memuse)
	elif target == "spec-cpu2017":
		processfile_speccpu2017(path, instance, nodename, memuse)
	else:
		sys.stderr.write("target %s not supported in log file %s\n" % (target, path))

def processdir(path):
	files = os.listdir(path)
	files.sort()
	for file in files:
		processpath(os.path.join(path, file))

def processpath(path):
	if os.path.isfile(path):
		processfile(path)
	elif os.path.isdir(path):
		processdir(path)
	else:
		sys.stderr.write("path %s not found\n" % (path))

def extractvalues(tuples, index):
	if tuples is None: return None
	values = list()
	for tuple in tuples:
		value = tuple[index]
		if not (value is None):
			values.append(value)
	values.sort()
	return values

def compute_mean(values, index, benchmark):
	if len(values) < 1: return None
	sum_ = 0
	for value in values:
		sum_ += value
	return sum_ / len(values)

def compute_median(values, index, benchmark):
	if len(values) < 1: return None
	index = len(values) // 2
	if index * 2 == len(values):
		return (values[index - 1] + values[index]) / 2
	else:
		return values[index]

def compute_stdev(values, index, benchmark):
	if len(values) < 2: return None
	sum_ = 0
	sum2 = 0
	for value in values:
		sum_ += value
		sum2 += value * value
	var = (sum2 - sum_ * sum_ / len(values)) / (len(values) - 1)
	if (var < 0) and (var > -1e-12): var = 0
	return math.sqrt(var)

def compute_n(values, index, benchmark):
	return len(values)

def getvaluesref(index, benchmark):
	if instanceRef is None: return None
	keyRef = (instanceRef, benchmark)
	tuplesRef = resultTuples.get(keyRef)
	return extractvalues(tuplesRef, index)

def compute_oh_mean(values, index, benchmark):
	valuesRef = getvaluesref(index, benchmark)
	if valuesRef is None: return None
	value = compute_mean(values, index, benchmark)
	if value is None: return None
	valueRef = compute_mean(valuesRef, index, benchmark)
	if (valueRef is None) or (valueRef == 0): return None
	return (value / valueRef - 1.0) * 100.0

def compute_oh_median(values, index, benchmark):
	valuesRef = getvaluesref(index, benchmark)
	if valuesRef is None: return None
	value = compute_median(values, index, benchmark)
	if value is None: return None
	valueRef = compute_median(valuesRef, index, benchmark)
	if (valueRef is None) or (valueRef == 0): return None
	return (value / valueRef - 1.0) * 100.0

def computeresult(instance, benchmark, measure):
	key = (instance, benchmark)
	tuples = resultTuples.get(key)
	if tuples is None: return None
	return measure[2](extractvalues(tuples, measure[3]), measure[3], benchmark)

def printresult(measure, value):
	if value is None:
		valuestr = ""
	else:
		valuestr = measure[1] % (value)
	sys.stdout.write("\t%s" % (valuestr))

def showresult_basic(instance, benchmark, measure):
	value = computeresult(instance, benchmark, measure)
	printresult(measure, value)

def computegeomean(benchmarks, instance, measure):
	if not measure[4]: return None
	sum = 0
	n = 0
	for benchmark in benchmarks:
		value = computeresult(instance, benchmark, measure)
		if value is None: return None
		sum += math.log(value / 100 + 1)
		n += 1
	if n < 1: return None
	return (math.exp(sum / n) - 1) * 100

def getvalidbenchmarks(instance, benchmarks, measure):
	benchmarksValid = list()
	for benchmark in benchmarks:
		value = computeresult(instance, benchmark, measure)
		if not (value is None):
			benchmarksValid.append(benchmark)
	return benchmarksValid

def showresult_geomean(benchmarks, instance, measure):
	value = computegeomean(benchmarks, instance, measure)
	printresult(measure, value)

def showresults_basic_measures_row(benchmark, instances, measures):
	sys.stdout.write("%s" % (benchmark))
	for measure in measures:
		for instance in instances:
			showresult_basic(instance, benchmark, measure)
	sys.stdout.write("\n")

def showresults_basic_measures_geomean_row(benchmarks, instances, measures, instanceGeomean):
	if instanceGeomean is None:
		sys.stdout.write("geomean")
	else:
		sys.stdout.write("geomean-%s" % (instanceGeomean))

	for measure in measures:
		if instanceGeomean is None:
			benchmarksGeomean = benchmarks
		else:
			benchmarksGeomean = getvalidbenchmarks(instanceGeomean, benchmarks, measure)
		for instance in instances:
			showresult_geomean(benchmarksGeomean, instance, measure)
	sys.stdout.write("\n")

def showresults_basic_measures(benchmarks, instances, measures):
	for measure in measures:
		for instance in instances:
			sys.stdout.write("\t%s" % (measure[0]))
	sys.stdout.write("\n")
	for measure in measures:
		for instance in instances:
			sys.stdout.write("\t%s" % (getInstanceName(instance)))
	sys.stdout.write("\n")
	for benchmark in benchmarks:
		showresults_basic_measures_row(benchmark, instances, measures)

	showresults_basic_measures_geomean_row(benchmarks, instances, measures, None)
	for instanceGeomean in instances:
		showresults_basic_measures_geomean_row(benchmarks, instances, measures, instanceGeomean)

def showresults_basic(benchmarks, instances):
	measures = [
		("rt-oh",  "%.1f", compute_oh_median, 0, True),
		("rt-med", "%.3f", compute_median,    0, False),
		("rt-avg", "%.3f", compute_mean,      0, False),
		("rt-std", "%.3f", compute_stdev,     0, False),
		("rt-n",   "%d",   compute_n,         0, False)]
	measuresmem = [
		("rss-oh",  "%.1f", compute_oh_median, 1, True),
		("rss-med", "%.0f", compute_median,    1, False),
		("rss-avg", "%.0f", compute_mean,      1, False),
		("rss-std", "%.0f", compute_stdev,     1, False),
		("rss-n",   "%d",   compute_n,         1, False)]

	showresults_basic_measures(benchmarks, instances, measures)
	sys.stdout.write("\n")
	showresults_basic_measures(benchmarks, instances, measuresmem)

def showresults_all(benchmarks, instances):
	for index in [0, 1]:
		if index == 0:
			pfx = "runtime"
			fmt = "\t%.3f"
		else:
			pfx = "rss-max"
			fmt = "\t%.0f"

		for benchmark in benchmarks:
			for instance in instances:
				sys.stdout.write("%s\t%s\t%s" % (pfx, benchmark, getInstanceName(instance)))
				key = (instance, benchmark)
				tuples = resultTuples.get(key)
				values = extractvalues(tuples, index)
				if values is not None:
					for value in values:
						sys.stdout.write(fmt % (value))
				sys.stdout.write("\n")

def showresult_node(benchmark, instance, node, zscore, nodeStats):
	key = (instance, benchmark, node)
	tuples = resultTuplesNode.get(key)
	if tuples is None:
		sys.stdout.write("\t")
		return
	runtimes = extractvalues(tuples, 0)
	mean = compute_mean(runtimes, 0, benchmark)

	keyRef = (instance, benchmark)
	tuplesRef = resultTuples[keyRef]
	runtimesRef = extractvalues(tuplesRef, 0)
	meanRef = compute_mean(runtimesRef, 0, benchmark)
	if zscore:
		statIndex = 0
		stdevRef = compute_stdev(runtimesRef, 0, benchmark)
		if (stdevRef is None) or (stdevRef == 0):
			value = None
		else:
			value = (mean - meanRef) / stdevRef
	else:
		statIndex = 2
		if meanRef == 0:
			value = None
		else:
			value = mean / meanRef

	if value is None:
		sys.stdout.write("\t")
	else:
		sys.stdout.write("\t%.3f" % (value))
		nodeStats[node][statIndex] += len(runtimes)
		nodeStats[node][statIndex + 1] += value * len(runtimes)

def safediv(x, y):
	if y == 0:
		return None
	else:
		return x / y

def optfloat2str(x):
	if x is None:
		return ""
	else:
		return "%.3f" % (x)

def showresults_node(benchmarks, instances, nodes):
	nodeStats = dict() # node -> (z-n, z-sum, ratio-n, ratio-sum)
	for node in nodes: nodeStats[node] = [0, 0, 0, 0]

	sys.stdout.write("\t")
	for node in nodes: sys.stdout.write("\t%s-z" % (node))
	for node in nodes: sys.stdout.write("\t%s-ratio" % (node))
	sys.stdout.write("\n")
	for benchmark in benchmarks:
		for instance in instances:
			sys.stdout.write("%s\t%s" % (benchmark, getInstanceName(instance)))
			for node in nodes: showresult_node(benchmark, instance, node, True,  nodeStats)
			for node in nodes: showresult_node(benchmark, instance, node, False, nodeStats)
			sys.stdout.write("\n")

	nodeStatList = []
	for node in nodes:
		entry = nodeStats[node]
		nodeStatList.append((
			safediv(entry[1], entry[0]),
			safediv(entry[3], entry[2]),
			entry[0],
			entry[2],
			node))
	nodeStatList.sort()

	sys.stdout.write("\n")
	sys.stdout.write("node\tz-mean\tz-n\tratio-mean\tratio-n\n")
	for z_mean, ratio_mean, z_n, ratio_n, node in nodeStatList:
		sys.stdout.write("%s\t%s\t%d\t%s\t%d\n" %
			(node, optfloat2str(z_mean), z_n, optfloat2str(ratio_mean), ratio_n))

def selectInstanceRefMatch(instances, name):
	global instanceRef
	for instance in instances:
		if instance == name:
			instanceRef = instance
			return

def selectInstanceRef(instances):
	if instanceRef is None: selectInstanceRefMatch(instances, "baseline-lto")
	if instanceRef is None: selectInstanceRefMatch(instances, "baseline")
	if instanceRef is None: selectInstanceRefMatch(instances, "default-lto")
	if instanceRef is None: selectInstanceRefMatch(instances, "default")
	if instanceRef is None: selectInstanceRefMatch(instances, "safestack-lto")
	if instanceRef is None: selectInstanceRefMatch(instances, "safestack")
	if instanceRef is None:
		sys.stdout.write("no baseline, overheads not computed\n\n")
	else:
		sys.stdout.write("baseline for overheads: %s\n\n" % (instanceRef))

def showresults():
	benchmarks = list(resultBenchmarks)
	benchmarks.sort()
	instances = list(resultInstances)
	instances.sort()
	nodes = list(resultNodes)
	nodes.sort()

	selectInstanceRef(instances)
	if len(nodeBlacklist) > 0:
		sys.stdout.write("blacklisted nodes: %s\n\n" % (", ".join(nodeBlacklist)))

	showresults_basic(benchmarks, instances)
	if verbosity >= 1:
		sys.stdout.write("\n")
		showresults_all(benchmarks, instances)
	if verbosity >= 2:
		sys.stdout.write("\n")
		showresults_node(benchmarks, instances, nodes)

def findoutliers(values, alpha, mindelta):
	n = len(values)
	if n < 2: return None

	meandiffmax = 0
	meandiffindex = None
	for i in range(n // 2, n - 1):
		mean1 = compute_mean(values[0:i], None, None)
		mean2 = compute_mean(values[i:n], None, None)
		meandiff = mean2 - mean1
		if meandiffmax < meandiff:
			meandiffmax = meandiff
			meandiffindex = i

	mean = compute_mean(values, None, None)
	if mean == 0: return None

	delta = meandiffmax / mean
	if delta < mindelta: return None
	
#	(t, p) = scipy.stats.ttest_ind(values[0:meandiffindex], values[meandiffindex:n])
#	if p >= alpha: return None

	return meandiffindex

def selecttuples(tuples, runtimebelow):
	tuplesNew = list()
	for tuple in tuples:
		if tuple[0] < runtimebelow:
			tuplesNew.append(tuple)
	return tuplesNew

def dictadd(d, key, value):
	if key in d:
		d[key] = d[key] + value
	else:
		d[key] = value

def removeoutliersfor(benchmark, instance, outliersPerNode, totalsPerNode):
	key = (instance, benchmark)
	if not (key in resultTuples):
		return

	runtimes = list()
	for tuple in resultTuples[key]:
		runtimes.append(tuple[0])

	runtimes.sort()
	runtimeOutlierIndex = findoutliers(runtimes, 0.01, 0.05)
	if runtimeOutlierIndex is None: return
	runtimeOutlier = runtimes[runtimeOutlierIndex]

	tuples = resultTuples[key]
	tuplesNew = selecttuples(tuples, runtimeOutlier)
	resultTuples[key] = tuplesNew

	sys.stderr.write("warning: benchmark %s has outliers for instance %s: %d out of %d (cutoff runtime: %.1fs)\n" %
		(benchmark, instance, len(tuples) - len(tuplesNew), len(tuples), runtimeOutlier))

	if len(nodeBlacklist) > 0:
		return # do not remove outliers if a blacklist was already specified

	for node in resultNodes:
		keyNode = (instance, benchmark, node)
		if not (keyNode in resultTuplesNode): continue
		for tuple in resultTuplesNode[keyNode]:
			dictadd(totalsPerNode, node, 1)
			if tuple[0] >= runtimeOutlier: dictadd(outliersPerNode, node, 1)

def removeoutliers():
	benchmarks = list(resultBenchmarks)
	benchmarks.sort()
	instances = list(resultInstances)
	instances.sort()
	nodes = list(resultNodes)
	nodes.sort()

	outliersPerNode = dict()
	totalsPerNode = dict()
	for benchmark in benchmarks:
		for instance in instances:
			removeoutliersfor(benchmark, instance, outliersPerNode, totalsPerNode)

	for node in nodes:
		if node in outliersPerNode:
			sys.stderr.write("note: node %s generated %d outlier(s) out of %d result(s)\n" %
				(node, outliersPerNode[node], totalsPerNode[node]))

for path in sys.argv[1:]:
	processpath(path)

removeoutliers()
showresults()
