This repository provides a prototype of the Type-based Data Isolation technique described in [Mitigating Information Leakage Vulnerabilities with Type-based Data Isolation](https://download.vusec.net/papers/tdi_sp22.pdf). Specifically, it contains:

* LLVM passes (primarily the TypeIsolation pass) which implement TDI.
* Ports and/or rewrites of several Type-after-Type passes for LLVM 9.
* tcmalloc patches to add support for arena-based allocation.
* Magical scripts to build and run CPU2006, CPU2017 and some other benchmarks.

Thanks to Jakob Koschel and Nikolaos Chalkiadakis for their help fixing the prototype repo for publication.

## Build system

The build system is based on Type-after-Type's build system. The possible 'instances' can be found in autosetup/passes/; they correspond to different configurations.
'baseline-lto' is the baseline we used.
'typesafestack' is the type-based stack allocation (based on SafeStack) and 'typedmalloc' is the type-based arena allocator (modified tcmalloc), both based on Type-after-Type.
'inline' inlines malloc wrappers for improved type detection. 'typeisolation' applies the TDI instrumentation. 'store' extends the instrumentation to cover stores.

'typesafestack-typedmalloc-inline-typeisolation-stores' is probably the most interesting instance.
You can select multiple instances by separating them with a space, e.g., `INSTANCES="baseline-lto typesafestack-typedmalloc-inline-typeisolation-stores"`.

Build output goes into autosetup.dir; `rm -rf autosetup.dir` to start over (you might want to keep at least the downloads, though).
Note that LTO builds use gold (not lld), and the .a files also contain non-bitcode. Finally, some benchmarks (such as CPU2017) may have very slow link times.

## Building

We tested this using an AWS instance running Ubuntu 22.04. On such an instance, you'll want to do this first:

    sudo apt-get update
    sudo apt-get install docker.io
    sudo adduser ubuntu docker

The recipe below should get you a toy example instrumented with TDI. The first build is very slow because it downloads and builds a lot of components, in particular LLVM. Note that sometimes you get weird build failures (typically late in the build process, wrk or perl); you can just run the command again.

    git clone https://github.com/vusec/typeisolation
    cd typeisolation
    docker build -t typeisolation .
    docker run -i -t -v $PWD:/home/user/typeisolation -u 1000:1000 typeisolation /bin/bash
    cd typeisolation
    NO_GPERFTOOLS=1 TARGETS=musl INSTANCES="typesafestack-typedmalloc-inline-typeisolation-stores" ./autosetup.sh
    NO_GPERFTOOLS=1 TARGETS=libcxx INSTANCES="typesafestack-typedmalloc-inline-typeisolation-stores" ./autosetup.sh
    # build the toy example
    TARGETS=toy INSTANCES="typesafestack-typedmalloc-inline-typeisolation-stores" ./autosetup.sh
    # run the toy example (source in , output in )
    autosetup.dir/scripts/run-toy-typesafestack-typedmalloc-inline-typeisolation-stores.sh

The source for the toy example is in autosetup/targets/toy/toy.c; it allocates three different types, prints the addresses (to confirm they're in different arenas), and then has some unsafe pointer arithmetic in the 'myprint' function. The output is in autosetup.dir. You may want to check the binary (targets/obj/toy/$INSTANCE/toy) to confirm the arithmetic is correctly masked. Otherwise, you could confirm this using the compiler debug output (logs/autosetup.txt.toy-$INSTANCE.txt):

    TypeIsolation: myprint - masked 1 arith insts (of 1), 0 GEP indexes

## Benchmarking

To benchmark SPEC, you'll need a copy of the relevant benchmark. For example:

    tar xvf spec2006.tar.gz -C autosetup.dir/targets/src
    mv autosetup.dir/targets/src/spec2006 autosetup.dir/targets/src/spec-cpu2006-cd
    tar xvf spec2017.tar.gz -C autosetup.dir/targets/src
    mv autosetup.dir/targets/src/spec2017 autosetup.dir/targets/src/spec-cpu2017-cd

Then you can build and run them like this:

    TARGETS=spec-cpu2006 INSTANCES="baseline-lto typesafestack-typedmalloc-inline-typeisolation" ./autosetup.sh
    TARGETS=spec-cpu2006 INSTANCES="baseline-lto typesafestack-typedmalloc-inline-typeisolation" scripts/prun-benchmarks.sh

You can also build nginx using the framework (build openssl first; both will be downloaded automatically), but the nginx benchmarks were done using VUSec's [more modern python-based infrastructure](https://github.com/vusec/instrumentation-infra). The 'nginx' directory has patches against 91aef25e. If you want to reproduce the hook experiment then you'll need to use the patched version with LD_PRELOAD hacked in.

Only full builds (including instrumentation of musl/libcxx) are supported by this release. If you want threading support (probably a good idea!), apply patches/musl-fix-thread-weak-symbols.patch to the relevant musl tree, run 'make', and then set the MUSL_HORROR defines in the staticlib/typesafestack/ files. (For nginx threading support, make sure to add --with-threads to autosetup/targets/nginx/config.inc)

## Notes

The TypeIsolation pass itself is chaotic; the code is the result of several rewrites. Much of the code is dedicated to trying to detect pointer arithmetic at LLVM IR level, which in hindsight is probably the wrong place. In practice it would be much easier (and less error-prone) to just instrument only the GEPs (which is what a previous higher-performance prototype did), and simply accept the reduced coverage of arithmetic. A practical implementation would probably also need to replace tcmalloc with a custom arena allocator, and fix up LLVM's register allocation so that multiple stacks don't end up being much slower than they should be. Unfortunately, this would make it impossible to benchmark against a reasonable baseline, so, uh, everything here is cursed. You're very welcome.

As discussed in the paper, there are various blacklists to make things work; the TaT sizeof pass adds hacks at AST (source, even) level, and that doesn't work correctly with templates, so there are some type-based blacklists there. The clang wrapper scripts in scripts/ blacklist several files (e.g. musl internals, and one SPEC wrapper) for which we disable flto and/or the sizeof pass. Finally, the TypeIsolation pass itself is configured to be a bit paranoid, and there are a few hacks in these for some cases (in particular with e.g., different sets of compilation flags to the ones we used) where we can't work out which side of arithmetic is a pointer; in the real world we'd just print a warning and ignore (as we do for a bunch of other cases actually), but here we check some known-problematic cases (where we hardcode the right result) and error out otherwise.

