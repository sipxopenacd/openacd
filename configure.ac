AC_PREREQ([2.68])
AC_INIT([openacd], [2.0.0])

AC_ERLANG_NEED_ERLC
AC_ERLANG_NEED_ERL

AC_ERLANG_PATH_ERL

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

# Test dependencies
EZ_ERLANG_NEED_LIB(meck)
EZ_ERLANG_NEED_LIB(gen_server_mock)

AC_ERLANG_SUBST_INSTALL_LIB_SUBDIR(AC_PACKAGE_NAME, AC_PACKAGE_VERSION)

AC_ARG_ENABLE(user,
	[AS_HELP_STRING([--enable-user=USER], [user that will run openacd (default: no)])],
	[AS_CASE([$enableval],
		[yes], [ENABLEUSER=`whoami`],
		[no], [ENABLEUSER=""],
		[ENABLEUSER=$enableval])])

AS_IF([test "x$ENABLEUSER" != "x"], [AC_SUBST([runuser], [$ENABLEUSER])])

AC_CONFIG_FILES([Makefile])

AC_OUTPUT
