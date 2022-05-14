# helper for pass config files
#
# set a compiler wrapper for clang that performs a source-to-source
# transformation using the specified frontend plugin, saves the transformed
# source to a temporary file, and calls the original compiler on a 
#
# usage: add_clang_plugin plugin-name
set_clang_plugin() {
    cc="$PATHROOT/scripts/clang-plugin-wrapper.sh $cc $PATHCLANGPLUGINS/libplugins.so $1"
    cxx="$PATHROOT/scripts/clang-plugin-wrapper.sh $cxx $PATHCLANGPLUGINS/libplugins.so $1"
}

# usage: set_clang_pass pass-name
# ALYSSA HACK: make it a wrapper script, because libunwind, because reviewers complaining, etc
set_clang_pass() {
    cc="$PATHROOT/scripts/clang-pass-wrapper.sh $PATHCLANGPLUGINS/libplugins.so $1 $cc --"
    cxx="$PATHROOT/scripts/clang-pass-wrapper.sh $PATHCLANGPLUGINS/libplugins.so $1 $cxx --"
    #cflags="$cflags -Xclang -load -Xclang $PATHCLANGPLUGINS/libplugins.so -mllvm $1 "
    echo "#!/usr/bin/env bash" > $PATHROOT/scripts/wrapperhack-cc.sh
    echo "$cc \"\$@\"" >> $PATHROOT/scripts/wrapperhack-cc.sh
    echo "#!/usr/bin/env bash" > $PATHROOT/scripts/wrapperhack-cxx.sh
    echo "$cxx \"\$@\"" >> $PATHROOT/scripts/wrapperhack-cxx.sh
    chmod +x $PATHROOT/scripts/wrapperhack-cc.sh
    chmod +x $PATHROOT/scripts/wrapperhack-cxx.sh
    cc="$PATHROOT/scripts/wrapperhack-cc.sh"
    cxx="$PATHROOT/scripts/wrapperhack-cxx.sh"
}
