cd "$PATHAUTOTARGETSRC"
if [ ! -d libunwind ]; then
    run git clone https://github.com/llvm-mirror/libunwind
    cd libunwind
    run git checkout release_90
    sed -i 's/#define _LIBUNWIND_EXPORT               __attribute__((visibility("default")))/#define _LIBUNWIND_EXPORT               __attribute__((visibility("default"), used))/' src/config.h
    cd ..
fi
if [ ! -d libcxx ]; then
    run git clone https://github.com/llvm-mirror/libcxx.git
    cd libcxx
    run git checkout release_90
    run patch -p2 < "$PATHROOT/patches/libcxx-fix-init-order-D31413.diff"
    run patch -p2 < "$PATHROOT/patches/libcxx-sizeof-hacks.patch"
    cd ..
fi
if [ ! -d libcxxabi ]; then
    run git clone https://github.com/llvm-mirror/libcxxabi.git
    cd libcxxabi
    run git checkout release_90
    cd ..
fi

