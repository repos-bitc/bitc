dnl Confirm that libgc is present, with headers.
dnl
dnl Note that this does NOT add libgc to LIBS automatically. We
dnl don't want that for BitC build, and in any case the placement of
dnl the -lgc in the link line is position-sensitive.

AC_DEFUN([CHECK_LIBGC], [
  AC_CHECK_HEADERS([gc/gc.h], [], [
    AC_MSG_ERROR([Compiler output requires gc-devel or libgc-devel package]) ])
  check_libgc_save_libs="$LIBS"
  AC_CHECK_LIB(gc, GC_init, [], [
    AC_MSG_ERROR([Compiler output requires gc or libgc package]) ])
  dnl Do NOT add -lgc to our link line!
  LIBS="$check_libgc_save_libs"
])dnl
