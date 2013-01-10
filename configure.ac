AC_PREREQ([2.68])
AC_INIT([oacd_core], [2.0.0])

AC_ERLANG_NEED_ERLC

AC_ARG_VAR([REBAR], [Rebar command])dnl
if test -n "$REBAR"; then
	AC_MSG_CHECKING([for rebar])
	AC_MSG_RESULT([$REBAR])
else
	AC_PATH_PROG([REBAR],[rebar],[no])
	if test "x$REBAR" = "xno"; then
		AC_MSG_ERROR([rebar not found])
	fi
fi


EZ_ERLANG_NEED_LIB(gen_leader)
EZ_ERLANG_NEED_LIB(ejrpc2)
EZ_ERLANG_NEED_LIB(gproc)

AC_CONFIG_FILES([Makefile])
AC_OUTPUT
