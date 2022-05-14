#!/usr/bin/env bash
passlib="$1"
passname="$2"
cc="$3"
shift 3
while [ $# -gt 0 ]; do
    if [ "$1" == -- ]; then
        shift
        break
    fi
    cc+=" $1"
    shift
done

i_args=("$@")
o_args=("$@")
if [[ "$@" == *"spec_mem_io/spec_mem_io.c" ]] || [[ "$@" == *"env/__"* ]] || [[ "$@" == *"vdso.c"* ]] || [[ "$@" == *"ldso/dl_iterate_phdr.c"* ]] || [[ "$@" == *"cxa_personality.cpp"* ]]
then
  # hack by alyssa, see explanation in paper for spec_mem_io
  # (the env/__ is for musl __libc_init and similar, this is the simplest way to make we don't mess with them)
  # vdso.c/ldso is because musl, these directly manipulate ELF phdrs, so across arenas -> should mention in paper
  # cxa_personality.cpp is because libcxxabi, pointer arithmetic for unwinding -> should mention in paper

  echo "*** blacklisted for LTO! ***"
  o_args=()
  for arg in "${i_args[@]}"
  do
    o_args+=( "${arg/-flto/} ")
  done
fi

# HACK by alyssa: musl uses assembly, can't load clang plugin for TAT, I give up trying to find a nice fix
# (also: libunwind UnwindRegisters)
if [[ "$@" == *"/x86_64/"*".s"* ]] || [[ "$@" == *"UnwindRegisters"* ]]
then
  echo "*** SKIPPING CLANG PLUGIN FOR APPARENT ASSEMBLY ***"
  exec $cc "${o_args[@]}"
  exit 0
fi

# HACK by alyssa: TAT's sizeof stuff screws up some musl
if [[ "$@" == *"locale/dcngettext.c"* ]] || [[ "$@" == *"network/if_nameindex.c" ]]
then
  echo "*** MUSL HACK: SKIPPING CLANG PLUGIN ***"
  exec $cc "${o_args[@]}"
  exit 0
fi

exec $cc -Xclang -load -Xclang "$passlib" -mllvm $passname "${o_args[@]}"
