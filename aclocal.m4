AC_DEFUN([EZ_MISSING_DEP],[
  if test "x$enable_dep_check" != "xno"; then
    AC_MSG_ERROR([$1])
  fi
])

AC_DEFUN([EZ_ERLANG_NEED_LIB],
	[AC_ERLANG_CHECK_LIB($1, [],
	  [EZ_MISSING_DEP([$1 was not found!])])])

AC_DEFUN([EZ_NEED_PROG],
	[AC_ARG_VAR([$1], [$3])dnl
if test -n "${$1}"; then
	AC_MSG_CHECKING([for $2])
	AC_MSG_RESULT([${$1}])
else
	AC_PATH_PROG([$1], [$2], [no])

  if test "x${$1}" = "xno"; then
	  EZ_MISSING_DEP([$2 not found but required])
  fi

fi
])
