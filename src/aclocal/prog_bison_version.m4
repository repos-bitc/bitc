# ===========================================================================
#             http://autoconf-archive.cryp.to/ax_prog_bison.html
# ===========================================================================
#
# SYNOPSIS
#
#   AX_PROG_BISON(ACTION-IF-TRUE,ACTION-IF-FALSE)
#
# DESCRIPTION
#
#   Check whether bison is the parser generator. Run ACTION-IF-TRUE if
#   successful, ACTION-IF-FALSE otherwise
#
# LAST MODIFICATION
#
#   2008-04-12
#
# COPYLEFT
#
#   Copyright (c) 2008 Francesco Salvestrini <salvestrini@users.sourceforge.net>
#
#   This program is free software; you can redistribute it and/or modify it
#   under the terms of the GNU General Public License as published by the
#   Free Software Foundation; either version 2 of the License, or (at your
#   option) any later version.
#
#   This program is distributed in the hope that it will be useful, but
#   WITHOUT ANY WARRANTY; without even the implied warranty of
#   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General
#   Public License for more details.
#
#   You should have received a copy of the GNU General Public License along
#   with this program. If not, see <http://www.gnu.org/licenses/>.
#
#   As a special exception, the respective Autoconf Macro's copyright owner
#   gives unlimited permission to copy, distribute and modify the configure
#   scripts that are the output of Autoconf when processing the Macro. You
#   need not follow the terms of the GNU General Public License when using
#   or distributing such scripts, even though portions of the text of the
#   Macro appear in them. The GNU General Public License (GPL) does govern
#   all other use of the material that constitutes the Autoconf Macro.
#
#   This special exception to the GPL applies to versions of the Autoconf
#   Macro released by the Autoconf Macro Archive. When you make and
#   distribute a modified version of the Autoconf Macro, you may extend this
#   special exception to the GPL to apply to your modified version as well.

AC_DEFUN([PROG_BISON_VERSION], [
  AC_REQUIRE([AC_PROG_SED])
  AC_REQUIRE([AC_PROG_GREP])
  AC_CHECK_PROGS(BISON, bison)
  AS_IF([test -n "BISON"], [
      ax_bison_version="$1"

      AC_MSG_CHECKING([for bison version])
      changequote(<<,>>)
      bison_version=`$BISON -V 2>&1 | $GREP "^bison " | $SED -e 's/^.* \([0-9]*\.[0-9]*\.[0-9]*\)/\1/'`
      changequote([,])
      AC_MSG_RESULT($bison)

      AC_SUBST([BISON_VERSION],[$bison_version])

      AX_COMPARE_VERSION([$ax_bison_version],[le],[$bison_version],[
	    :
            $2
      ],[
	    :
            $3
      ])
  ], [
      AC_MSG_ERROR([bison not found and is required])
      $3
  ])
])
