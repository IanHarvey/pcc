#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ(2.59)
AC_INIT(FULL-PACKAGE-NAME, VERSION, BUG-REPORT-ADDRESS)
AC_CONFIG_HEADER([config.h])

AC_PROG_MAKE_SET
AC_PROG_INSTALL
AC_CANONICAL_TARGET


case "$target_os" in
    netbsd*)
	targos=netbsd
	case "$target_cpu" in
	    i?86) targmach=x86 ;;
	esac
	;;
    linux*)
	targos=linux
	case "$target_cpu" in
	    i?86) targmach=x86 ;;
	esac
	;;
    *)
        targos="$target_os"
	case "$target_cpu" in
	    m16c) targmach=m16c ;;
	esac
	;;
esac
   
if test "X$targos" = X -o "X$targmach" = X ; then
	AC_MSG_ERROR(['$target' is not (yet) supported by pcc.])
fi

# Checks for programs.
AC_PROG_CC
AC_PROG_LEX
AC_PROG_YACC

AC_CHECK_PROG(strip,strip,yes,no)

# Checks for libraries.

# Checks for header files.
# AC_CHECK_HEADERS([fcntl.h stdlib.h string.h unistd.h])
AC_CHECK_HEADERS([string.h alloca.h])

# Checks for library functions.
AC_FUNC_ALLOCA
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

#
# Try with some heuristics to find the largest alignment for this host.
# Used for the internal memory management.
#
AC_MSG_CHECKING(for host struct alignment)
AC_TRY_RUN([
struct a { long long b; long double c; };
main(){
	struct a *a;
	int b[24];
	a = (struct a *)&b[0]; a->b = 8; a->c = 8.0;
	a = (struct a *)&b[1]; a->b = 8; a->c = 8.0;
	a = (struct a *)&b[2]; a->b = 8; a->c = 8.0;
	a = (struct a *)&b[3]; a->b = 8; a->c = 8.0;
	return 0;
}], alignment=int, alignment=fail, alignment=CROSS])
if test "$alignment" = fail ; then
AC_TRY_RUN([
struct a { long long b; long double c; };
main(){
	struct a *a;
	long b[24];
	a = (struct a *)&b[0]; a->b = 8; a->c = 8.0;
	a = (struct a *)&b[1]; a->b = 8; a->c = 8.0;
	a = (struct a *)&b[2]; a->b = 8; a->c = 8.0;
	a = (struct a *)&b[3]; a->b = 8; a->c = 8.0;
	return 0;
}], alignment=long, alignment=fail, alignment=CROSS])
fi
if test "$alignment" = fail ; then
AC_TRY_RUN([
struct a { long long b; long double c; };
main(){
	struct a *a;
	long long b[24];
	a = (struct a *)&b[0]; a->b = 8; a->c = 8.0;
	a = (struct a *)&b[1]; a->b = 8; a->c = 8.0;
	a = (struct a *)&b[2]; a->b = 8; a->c = 8.0;
	a = (struct a *)&b[3]; a->b = 8; a->c = 8.0;
	return 0;
}], alignment="long long", alignment=fail, alignment=CROSS])
fi
if test "$alignment" = fail ; then
AC_TRY_RUN([
struct a { long long b; long double c; };
main(){
	struct a *a;
	long double b[24];
	a = (struct a *)&b[0]; a->b = 8; a->c = 8.0;
	a = (struct a *)&b[1]; a->b = 8; a->c = 8.0;
	a = (struct a *)&b[2]; a->b = 8; a->c = 8.0;
	a = (struct a *)&b[3]; a->b = 8; a->c = 8.0;
	return 0;
}], alignment="long double", alignment=fail, alignment=CROSS])
fi
if test "$alignment" = fail ; then
	AC_MSG_ERROR(can't find out host struct alignment)
fi
AC_MSG_RESULT($alignment)

AC_DEFINE_UNQUOTED(TARGOS, $targos)
AC_DEFINE_UNQUOTED(ALIGNMENT, $alignment)

AC_CONFIG_FILES([Makefile
		cc/Makefile
		cc/cc/Makefile
		cc/cpp/Makefile
		cc/ccom/Makefile
		f77/Makefile
		f77/f77/Makefile
		f77/fcom/Makefile
])
AC_OUTPUT
