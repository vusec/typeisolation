MUSLSRCDIR="$PATHAUTOTARGETOBJ/musl-1.2.2/$instance"
MUSLOBJDIR="$PATHAUTOTARGETOBJ/musl-$VERSIONMUSL/$instancename"
LIBUNWINDOBJDIR="$PATHAUTOTARGETOBJ/libunwind/$instancename"
LIBCXXABIOBJDIR="$PATHAUTOTARGETOBJ/libcxxabi/$instancename"
LIBCXXOBJDIR="$PATHAUTOTARGETOBJ/libcxx/$instancename"
LIBCXXSRCDIR="$PATHAUTOTARGETSRC/libcxx"
GCCPATHHACK="/usr/lib/gcc/x86_64-linux-gnu/7.5.0/"
#LINKERSTART="-nostdlib -Wl,--start-group -lc -lm -lc++abi -lc++ -Wl,-whole-archive -lunwind -Wl,-no-whole-archive -L$LIBUNWINDOBJDIR/lib -L$LIBCXXABIOBJDIR/lib -L$LIBCXXOBJDIR/lib $MUSLOBJDIR/lib/crt1.o $MUSLOBJDIR/lib/crti.o $MUSLOBJDIR/lib/crtn.o $GCCPATHHACK/crtbeginT.o $GCCPATHHACK/crtend.o"
#LINKEREND="-static --sysroot=$MUSLOBJDIR -L$MUSLOBJDIR/lib -static-libgcc -nostdinc -isystem $MUSLOBJDIR/include -L$GCCPATHHACK -Wl,--verbose -lc -lm -lunwind -Wl,--end-group"
LINKERSTART="-nostdlib -Wl,--start-group -Wl,-whole-archive -lunwind -Wl,-no-whole-archive -L$LIBUNWINDOBJDIR/lib -L$LIBCXXABIOBJDIR/lib -L$LIBCXXOBJDIR/lib $MUSLOBJDIR/lib/crt1.o $MUSLOBJDIR/lib/crti.o $MUSLOBJDIR/lib/crtn.o $GCCPATHHACK/crtbeginT.o $GCCPATHHACK/crtend.o"
LINKEREND="-static --sysroot=$MUSLOBJDIR -L$MUSLOBJDIR/lib -static-libgcc -nostdinc -isystem $MUSLOBJDIR/include -L$GCCPATHHACK -Wl,--verbose -lc -lm -lc++abi -lc++ -lunwind -Wl,--end-group"
MUSLCHEADERS="-nostdinc -isystem $MUSLSRCDIR/include -isystem $MUSLSRCDIR/obj/include -isystem $MUSLSRCDIR/arch/x86_64 -isystem $MUSLSRCDIR/arch/generic -isystem $PATHROOT/musl-hacks/include"
MUSLCXXHEADERS="-nostdinc -nostdinc++ -isystem $PATHAUTOPREFIXBASE/libcxx-$instancename/include/c++/v1 -isystem $MUSLSRCDIR/include -isystem $MUSLSRCDIR/obj/include -isystem $MUSLSRCDIR/arch/x86_64 -isystem $MUSLSRCDIR/arch/generic -isystem $PATHROOT/musl-hacks/include"
