AC_PREREQ([2.68])
AC_INIT([openacd], [2.0.0])

AC_ERLANG_NEED_ERLC
AC_ERLANG_NEED_ERL

AC_ERLANG_PATH_ERL

EZ_NEED_PROG([REBAR], [rebar], [Rebar Erlang build tool [autodetected]])dnl
EZ_NEED_PROG([RUN_ERL], [run_erl], [Erlang/OTP interpreter runner [autodetected]])
EZ_NEED_PROG([TO_ERL], [to_erl], [Erlang/OTP interpreter runner [autodetected]])

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
