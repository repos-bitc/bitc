dnl On 64-bit targets there are many conventions for what to do about
dnl libdir. On some systems, 64-bit libs go into /usr/lib64. Unfortunately,
dnl there is no general rule or convention that can be used to decide this,
dnl and when things are being cross-built, we cannot check.
dnl
dnl The attitude of the autoconf maintainers seems to be "only the user
dnl knows, so have them tell us". Which would be fine, except that a lot
dnl bunch of the autoconf macros fail in strange ways when checking for
dnl library presence. If the 32-bit libraries are co-installed, that
dnl masks the problem, but if they are not, then various things can go
dnl wrong -- notably for us in the boost library checks.
dnl
dnl The following is a god-awful, ugly, horrible hack that reduces the
dnl pain when we are NOT cross compiling by testing pointer size and
dnl then checking for existence of /usr/lib64.
dnl
dnl If you are crazy enough to use this, put it just below your call
dnl to AC_CANONICAL_TARGET, because that is what sets $cross_compiling,
dnl and you don't want anything later to get the wrong answer for libdir.
dnl A surprising number of autoconf macros check/use/fiddle that.

AC_DEFUN([CHECK_LIB64], [
  AC_CHECK_SIZEOF([void *])
  if test "$cross_compiling" = "no"; then
    AC_MSG_CHECKING([For 64-bit native build])
    if test "${ac_cv_sizeof_void_p}" = "8"; then
      AC_MSG_RESULT(yes)
      AC_MSG_CHECKING([  whether /usr/lib64 is present])
      if test -d "/usr/lib64"; then
        AC_MSG_RESULT(yes)
        if test "$libdir" = '${exec_prefix}/lib'; then
          AC_MSG_NOTICE([*** Whacking libdir to \${exec_prefix}/lib64 ***])
          libdir='${exec_prefix}/lib64';
        fi
      else
        AC_MSG_RESULT(no)
      fi
    else
      AC_MSG_RESULT([no])
    fi
  else
    if test "${ac_cv_sizeof_void_p}" = "8"; then
      AC_MSG_WARN([On some machines, it may be necessary to run configure with --libdir=/usr/lib64])
    fi
  fi
])dnl
