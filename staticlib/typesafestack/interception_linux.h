//===-- interception_linux.h ------------------------------------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file is a part of AddressSanitizer, an address sanity checker.
//
// Linux-specific interception methods.
//===----------------------------------------------------------------------===//

#if defined(__linux__) || defined(__FreeBSD__)

#if !defined(INCLUDED_FROM_INTERCEPTION_LIB)
# error "interception_linux.h should be included from interception library only"
#endif

#ifndef INTERCEPTION_LINUX_H
#define INTERCEPTION_LINUX_H

// returns true if a function with the given name was found.
bool GetRealFunctionAddress(const char *func_name, uptr *func_addr,
    uptr real, uptr wrapper);
void *GetFuncAddrVer(const char *func_name, const char *ver);

#define INTERCEPT_FUNCTION_LINUX_OR_FREEBSD(func)                          \
  GetRealFunctionAddress(                                \
      #func, (uptr *)&PTR_TO_REAL(func), \
      (uptr) & (func),                                   \
      (uptr) & WRAP(func))

#if !defined(__ANDROID__)  // android does not have dlvsym
#define INTERCEPT_FUNCTION_VER_LINUX_OR_FREEBSD(func, symver) \
  (real_##func = (func##_f)(                \
       unsigned long)GetFuncAddrVer(#func, symver))
#else
#define INTERCEPT_FUNCTION_VER_LINUX_OR_FREEBSD(func, symver) \
  INTERCEPT_FUNCTION_LINUX_OR_FREEBSD(func)
#endif  // !defined(__ANDROID__)

#endif  // INTERCEPTION_LINUX_H
#endif  // __linux__ || __FreeBSD__
