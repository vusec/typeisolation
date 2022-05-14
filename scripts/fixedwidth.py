#!/usr/bin/env python3

# This script serves to format the output of analyze-logs.py to be easily
# readable on a terminal. Simply pipe the output through this script and
# it is formatted.

import re
import sys

def getColWidths(table):
	colWidths = list()
	for row in table:
		index = 0
		for field in row:
			colWidth = len(field)
			if index >= len(colWidths):
				colWidths.append(colWidth)
			elif colWidths[index] < colWidth:
				colWidths[index] = colWidth
			index = index + 1
	return colWidths

regexNumeric = re.compile("^-?[0-9]*\.?[0-9]*$")
def fieldIsNumeric(field):
	return regexNumeric.match(field) is not None

def printField(field, colWidth):
	padding = " " * (colWidth - len(field))
	if fieldIsNumeric(field):
		sys.stdout.write(padding)
		sys.stdout.write(field)
	else:
		sys.stdout.write(field)
		sys.stdout.write(padding)

firstLine = True
def printTable(table):
	global firstLine
	if table is None: return
	if firstLine:
		firstLine = False
	else:
		sys.stdout.write("\n")

	colWidths = getColWidths(table)
	for row in table:
		index = 0
		for field in row:
			if index > 0: sys.stdout.write(" ")
			printField(field, colWidths[index])
			index = index + 1
		sys.stdout.write("\n")

table = None
for line in sys.stdin:
	line = line.rstrip()
	if len(line) == 0:
		printTable(table)
		table = None
		continue
	if table is None: table = list()
	row = list()
	table.append(row)
	for field in line.split("\t"):
		row.append(field.strip())
	
printTable(table)
