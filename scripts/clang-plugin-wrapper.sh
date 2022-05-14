#!/usr/bin/env bash
cc=$1
plugin_lib=$2
plugin_name=$3
args="${@:4}"

# filter out file arguments and detect compiled language
cflags=()
infiles=()
lang=unknown
cpplang=unknown
linking=1
seen_outfile=0
for arg in "${@:4}"; do
    case "$arg" in
        *.c|*.i)
            if [ $lang = unknown ]; then
                lang=c
                cpplang=c
            fi
            infiles+=("$arg");;
        *.cpp|*.cc|*.cp|*.cxx|*.CPP|*.c++|*.C|*.ii)
            lang=c++
            cpplang=c++-cpp-output
            infiles+=("$arg");;
        *.h)
            if [ $lang = unknown ]; then
                lang=c-header
                cpplang=c-header
            fi
            infiles+=("$arg");;
        *.hh|*.H|*.hp|*.hxx|*.hpp|*.HPP|*.h++|*.tcc)
            if [ $lang = unknown ]; then
                lang=c++-header
                cpplang=c++-header
            fi
            infiles+=("$arg");;
        *.m|*.mi)
            lang=objective-c
            cpplang=objective-c-cpp-output
            infiles+=("$arg");;
        *.mm|*.M|*.mii)
            lang=objective-c++
            cpplang=objective-c++-cpp-output
            infiles+=("$arg");;
        *.s)
            lang=assembler
            cpplang=assembler
            linking=1 # HACK: alyssa: musl hack to avoid (unnecessary) wrapper for asm
            infiles+=("$arg");;
        *.S|*.sx)
            lang=assembler-with-cpp
            cpplang=assembler
            linking=1 # HACK: alyssa: musl hack to avoid (unnecessary) wrapper for asm
            infiles+=("$arg");;
        -c)
            linking=0
            cflags+=("$arg");;
        -o)
            seen_outfile=1
            cflags+=("$arg");;
        *)
            cflags+=("$arg");;
    esac
done
cflags="${cflags[@]}"

# exit on error
set -e

# for linking commands, execute normal behaviour
[ $linking -eq 1 ] && exec $cc $args

# add outfile flag if there is only one infile
outfile=""
if [ $seen_outfile -eq -0 -a ${#infiles[@]} -eq 1 ]; then
    outfile="-o $(basename "${infiles%.*}.o")"
fi

# print commands
set -x

# run the preprocessor to avoid multiple rewrite buffers
$cc $args -E -o - | \

# run the clang plugin
$cc $cflags -c -o /dev/null \
    -Xclang -load -Xclang $plugin_lib \
    -Xclang -plugin -Xclang $plugin_name \
    -x $cpplang - | \

# compile transformed source (-o option should be intact still)
$cc $cflags -x $lang - $outfile
