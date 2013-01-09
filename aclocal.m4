AC_DEFUN([EZ_ERLANG_NEED_LIB],
	[AC_ERLANG_CHECK_LIB($1, [],
		[AC_MSG_ERROR([$1 was not found!])])])