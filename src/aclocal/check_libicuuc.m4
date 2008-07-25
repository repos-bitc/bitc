AC_DEFUN([CHECK_LIBICUUC], [
  AC_CHECK_HEADERS([unicode/uchar.h], [
    AC_MSG_RESULT([yes])], [
    AC_MSG_RESULT([no])
    AC_MSG_ERROR([Need to install libicu and possibly libicu-devel]) ])
  AC_MSG_CHECKING([to see if libicuuc is installed.])
  LIBS="-licuuc $LIBS"
  AC_LINK_IFELSE([
    AC_LANG_PROGRAM([[#include <unicode/uchar.h>]],
                    [[u_hasBinaryProperty(0,UCHAR_XID_START);]]) ],
    [ AC_MSG_RESULT([yes]) ],
    [ AC_MSG_RESULT([no])
      AC_MSG_ERROR([Need to install libicu and possibly libicu-devel]) ])
])dnl
