dnl Process this file with autoconf to produce a configure script.
AC_INIT(aplus, 4.0.0)
AC_CONFIG_SRCDIR([cmcfun.h])

# For automake.
VERSION=4.0.0
PACKAGE=aplis

dnl Checks for programs.
AC_PROG_CC
AC_PROG_CXX

dnl Checks for libraries.
AC_PROG_RANLIB

dnl Checks for header files.
AC_HEADER_STDC
AC_HEADER_TIME
AC_CHECK_HEADERS(malloc.h sys/time.h unistd.h)
AM_INIT_AUTOMAKE([subdir-objects])

dnl Checks for typedefs, structures, and compiler characteristics.
AC_C_CONST
AC_C_INLINE
AC_STRUCT_TM
AC_TYPE_SIZE_T
AC_EXEEXT
AC_OBJEXT
AX_WITH_CURSES
AX_WITH_CURSES_PANEL

dnl Checks for library functions.
AC_FUNC_ALLOCA
AC_FUNC_STRFTIME

AC_CONFIG_FILES([Makefile])
AC_OUTPUT
