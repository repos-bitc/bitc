AC_DEFUN([CHECK_OSDOC], [
  AC_CHECK_FILE(/usr/share/osdoc)
  if test "$ac_cv_file__usr_share_osdoc" = "yes"; then
    AC_DEFINE(HAVE_OSDOC, 1,
              [Define if you have the OSDoc documentation tools])
    HAVE_OSDOC="yes"
  else
    HAVE_OSDOC="no"
    AC_MSG_WARN([Documentation cannot be built without OSDoc])
  fi
  AC_SUBST(HAVE_OSDOC)
])dnl
