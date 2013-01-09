AC_INIT([oacd_core], [2.0.0])

AC_ERLANG_NEED_ERLC
AC_PATH_PROG([REBAR],[rebar],[no])

if test "$REBAR" = "no"; then
	AC_MSG_ERROR([rebar not found])
fi

EZ_ERLANG_NEED_LIB(gen_leader)
EZ_ERLANG_NEED_LIB(ejrpc2)
EZ_ERLANG_NEED_LIB(gproc)

AC_CONFIG_FILES([Makefile])
AC_OUTPUT
