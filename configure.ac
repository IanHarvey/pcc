#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ(2.59)
AC_INIT(FULL-PACKAGE-NAME, VERSION, BUG-REPORT-ADDRESS)
AC_CONFIG_HEADER([config.h])

AC_PROG_MAKE_SET
AC_PROG_INSTALL
AC_CANONICAL_HOST

case "$host_os" in

    netbsd*)
	targos=netbsd
	case "$host_cpu" in
	    i386)
		targmach=x86 ;;
	esac
	;;
    *)
	targos=$host_os
	case "$host_cpu" in
	    m16c)
		targmach=m16c ;;
	esac
	;;	
 
esac

if test "X$targos" = X -o "X$targmach" = X ; then
	AC_MSG_ERROR(['$host' is not (yet) supported by pcc.])
fi

# Checks for programs.
AC_PROG_CC
AC_PROG_LEX
AC_PROG_YACC

AC_CHECK_PROG(strip,strip,yes,no)

# Checks for libraries.

# Checks for header files.
# AC_CHECK_HEADERS([fcntl.h stdlib.h string.h unistd.h])
AC_CHECK_HEADERS([string.h])

# Checks for library functions.
# AC_FUNC_ALLOCA
##  AC_FUNC_STRTOD
# AC_FUNC_VPRINTF
# AC_CHECK_FUNCS([memset strchr strdup strrchr strtol])
AC_CHECK_FUNCS(mkstemp)

AC_SUBST(targos)
AC_SUBST(targmach)
AC_SUBST(prefix)
AC_SUBST(exec_prefix)
AC_SUBST(libexecdir)
AC_SUBST(strip)

AC_DEFINE_UNQUOTED(TARGOS, $targos)

AC_CONFIG_FILES([Makefile
                 cc/Makefile
                 cpp/Makefile
                 mip/Makefile])
AC_OUTPUT
