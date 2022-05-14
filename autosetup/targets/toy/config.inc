source $PATHROOT/scripts/linker-musl.inc
run mkdir -p "$PATHAUTOTARGETOBJ/toy/$instance"
cd "$PATHAUTOTARGETOBJ/toy/$instance"
#[ -f "toy.c" ] || run cp "$PATHROOT/autosetup/targets/toy/toy.c" .
cp "$PATHROOT/autosetup/targets/toy/toy.c" .

(
	runscript_common_start
	echo "cd \"$PWD\""
	echo "$cc $cflagsbl -fPIE -pie $MUSLCHEADERS -c toy.c -o toy.o"
	echo "$cc $LINKERSTART $ldflagsalways $ldflagsnolib -fPIE -pie toy.o -o toy $LINKEREND"
	runscript_common_end
) > "$PATHAUTOSCRIPTSBUILD/build-toy-$instancename.sh"
run chmod u+x "$PATHAUTOSCRIPTSBUILD/build-toy-$instancename.sh"

(
	runscript_common_start
	echo "PATHSERVER=\"$PATHAUTOTARGETOBJ/toy/$instance/toy\""
	echo "source \"$PATHROOT/autosetup/targets/toy/run.inc\""
	runscript_common_end
) > "$PATHAUTOSCRIPTSRUN/run-toy-$instancename.sh"
run chmod u+x "$PATHAUTOSCRIPTSRUN/run-toy-$instancename.sh"

