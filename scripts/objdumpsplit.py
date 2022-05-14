#!/usr/bin/python

import os
import re
import sys

regexmodule = re.compile("(.*): +file format .*$")
regexsection = re.compile("Disassembly of section (.*):$")
regexfunction = re.compile("[0-9a-f]+ <(.*)>:$")
regexinstr = re.compile(" +([0-9a-f]+):\t(?:[0-9a-f][0-9a-f] )*[0-9a-f][0-9a-f] *\t(.*)$")
regexinstr2 = re.compile(" +([0-9a-f]+):\t(?:[0-9a-f][0-9a-f] )*[0-9a-f][0-9a-f]$")
regexinstr3 = re.compile("\t\.\.\.$")
regexaddr = re.compile("([0-9a-f]+) <(.*)>")
regexriprel = re.compile("0x[0-9a-f]+\(%rip\)")

def die(msg):
	sys.stderr.write(msg)
	sys.exit(1)

def processfunction(outdir, function, instructionTuples):
	instaddrs = set()
	labeladdrs = set()
	for instructionTuple in instructionTuples:
		instaddr = int(instructionTuple[0], 16)
		instaddrs.add(instaddr)
		m = regexaddr.search(instructionTuple[1])
		if m:
			labeladdr = int(m.group(1), 16)
			labeladdrs.add(labeladdr)

	labeladdrs &= instaddrs
	labeladdrlist = list(labeladdrs)
	labeladdrlist.sort()
	labelnames = dict()
	labelindex = 0
	for labeladdr in labeladdrlist:
		labelnames[labeladdr] = "label" + str(labelindex)
		labelindex += 1

	outpath = os.path.join(outdir, function + ".txt")
	outfile = open(outpath, "w")
	for instructionTuple in instructionTuples:
		instaddr = int(instructionTuple[0], 16)
		if instaddr in labelnames:
			outfile.write("%s:\n" % (labelnames[instaddr]))
		instruction = instructionTuple[1]
		m = regexaddr.search(instruction)
		if m:
			labeladdr = int(m.group(1), 16)
			if labeladdr in labelnames:
				labelname = labelnames[labeladdr]
			else:
				labelname = m.group(2)
			instruction = instruction.replace(m.group(0), labelname)
		instruction = regexriprel.sub("0xXXX(%RIP)", instruction)
		outfile.write("%s\n" % (instruction))
	outfile.close()

def processfile(inpath, infile, outdir):
	module = None
	section = None
	function = None
	instructionTuples = None

	if not os.path.isdir(outdir): os.makedirs(outdir)

	for line in infile:
		sline = line.rstrip()
		if len(sline) == 0: continue

		m = regexmodule.match(sline)
		if m:
			if module is not None: die("multiple module lines in %s\n" % (inpath))
			module = m.group(1)
			continue

		m = regexsection.match(sline)
		if m:
			if module is None: die("section outside module in %s\n" % (inpath))
			if function is not None: processfunction(outdir, function, instructionTuples)
			section = m.group(1)
			function = None
			continue

		m = regexfunction.match(sline)
		if m:
			if section is None: die("function outside section in %s\n" % (inpath))
			if function is not None: processfunction(outdir, function, instructionTuples)
			function = m.group(1)
			instructionTuples = list()
			continue

		m = regexinstr.match(sline)
		if m:
			if function is None: die("instruction outside function in %s\n" % (inpath))
			instructionTuples.append((m.group(1), m.group(2)))
			continue

		if regexinstr2.match(sline): continue
		if regexinstr3.match(sline): continue

		die("line in %s not recognized: %s\n" % (inpath, sline))

	if function is not None: processfunction(outdir, function, instructionTuples)

def processpath(inpath, outdir):
	if inpath == "-":
		processfile(inpath, sys.stdin, outdir)
	else:
		infile = open(inpath, "r")
		processfile(inpath, infile, outdir)
		infile.close()

def main():
	outdir = "."
	if len(sys.argv) >= 2: outdir = sys.argv[1]
	if len(sys.argv) >= 3:
		for inpath in sys.argv[2:]: processpath(inpath, outdir)
	else:
		processpath("-", outdir)

main()
