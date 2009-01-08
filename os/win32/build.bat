rem @echo off

set TARGOS=win32
set MACH=i386
set LIBEXECDIR=""

set CC=cl.exe -D__MSC__
set CFLAGS=/nologo /MT
set CFLAGS2=/nologo /MD /Za /Wall

rem set PCCDIR=C:\pcc
rem set CC=%PCCDIR%\bin\pcc.exe -v
rem set CFLAGS=

set BASEDIR=..\..
set MIPDIR=%BASEDIR%\mip
set CPPDIR=%BASEDIR%\cc\cpp
set CCOMDIR=%BASEDIR%\cc\ccom
set CCDIR=%BASEDIR%\cc\cc
set OSDIR=%BASEDIR%\os\%TARGOS%
set MACHDIR=%BASEDIR%\arch\%MACH%
set BISON_SIMPLE=bison.simple
set CPPFLAGS=-DWIN32 -DGCC_COMPAT -DPCC_DEBUG -DCPP_DEBUG -DTARGOS=%TARGOS% -Dos_%TARGOS% -Dmach_%MACH% -DLIBEXECDIR=%LIBEXECDIR% -D_CRT_SECURE_NO_WARNINGS

%CC% -o pcc.exe %CPPFLAGS% %CFLAGS% -I%CCDIR% -I. -I%MACHDIR% -I%MIPDIR% %CCDIR%\cc.c %MIPDIR%\compat.c

bison -y -t -d --no-lines %CPPDIR%\cpy.y
flex %CPPDIR%\scanner.l
%CC% %CPPFLAGS% %CFLAGS% -I%CPPDIR% -I%OSDIR% -I%MACHDIR% -I%MIPDIR% %CPPDIR%\cpp.c %MIPDIR%\compat.c y.tab.c lex.yy.c "C:\Program Files\UnxUtils\usr\local\lib\libfl.lib"

%CC% -DMKEXT %CPPFLAGS% %CFLAGS% -I%CCOMDIR% -I%OSDIR% -I%MACHDIR% -I%MIPDIR% %MIPDIR%\mkext.c %MACHDIR%\table.c %MIPDIR%\common.c
mkext
bison -y -t -d --no-lines %CCOMDIR%\cgram.y
move y.tab.c cgram.c
move y.tab.h cgram.h
flex %CCOMDIR%\scan.l
move lex.yy.c scan.c

%CC% -o ccom.exe %CPPFLAGS% %CFLAGS% -I%CCOMDIR% -I%OSDIR% -I%MACHDIR% -I%MIPDIR% %CCOMDIR%\main.c %MIPDIR%\compat.c scan.c cgram.c external.c %CCOMDIR%\optim.c %CCOMDIR%\pftn.c %CCOMDIR%\trees.c %CCOMDIR%\inline.c %CCOMDIR%\symtabs.c %CCOMDIR%\gcc_compat.c %CCOMDIR%\init.c %MACHDIR%\local.c %MACHDIR%\code.c %CCOMDIR%\stabs.c %MIPDIR%\match.c %MIPDIR%\reader.c %MIPDIR%\optim2.c %MIPDIR%\regs.c %MACHDIR%\local2.c %MACHDIR%\order.c %MACHDIR%\table.c %MIPDIR%\common.c "C:\Program Files\UnxUtils\usr\local\lib\libfl.lib"

md "C:\pcc"
md "C:\pcc\bin"
md "C:\pcc\libexec"
copy pcc.exe "C:\pcc\bin"
copy cpp.exe "C:\pcc\libexec"
copy ccom.exe "C:\pcc\libexec"


set LIBPCCDIR=%BASEDIR%\..\pcc-libs\libpcc
set LIBPCCDESTDIR="C:\pcc\lib\i386-win32\0.9.9"
%CC% -c %CPPFLAGS% %CFLAGS2% %LIBPCCDIR%\adddi3.c
%CC% -c %CPPFLAGS% %CFLAGS2% %LIBPCCDIR%\anddi3.c
%CC% -c %CPPFLAGS% %CFLAGS2% %LIBPCCDIR%\ashldi3.c
%CC% -c %CPPFLAGS% %CFLAGS2% %LIBPCCDIR%\ashrdi3.c
%CC% -c %CPPFLAGS% %CFLAGS2% %LIBPCCDIR%\cmpdi2.c
%CC% -c %CPPFLAGS% %CFLAGS2% %LIBPCCDIR%\divdi3.c
%CC% -c %CPPFLAGS% %CFLAGS2% %LIBPCCDIR%\fixdfdi.c
%CC% -c %CPPFLAGS% %CFLAGS2% %LIBPCCDIR%\fixsfdi.c
%CC% -c %CPPFLAGS% %CFLAGS2% %LIBPCCDIR%\fixunsdfdi.c
%CC% -c %CPPFLAGS% %CFLAGS2% %LIBPCCDIR%\fixunssfdi.c
%CC% -c %CPPFLAGS% %CFLAGS2% %LIBPCCDIR%\floatdidf.c
%CC% -c %CPPFLAGS% %CFLAGS2% %LIBPCCDIR%\floatdisf.c
%CC% -c %CPPFLAGS% %CFLAGS2% %LIBPCCDIR%\floatunsdidf.c
%CC% -c %CPPFLAGS% %CFLAGS2% %LIBPCCDIR%\iordi3.c
%CC% -c %CPPFLAGS% %CFLAGS2% %LIBPCCDIR%\lshldi3.c
%CC% -c %CPPFLAGS% %CFLAGS2% %LIBPCCDIR%\lshrdi3.c
%CC% -c %CPPFLAGS% %CFLAGS2% %LIBPCCDIR%\moddi3.c
%CC% -c %CPPFLAGS% %CFLAGS2% %LIBPCCDIR%\muldi3.c
%CC% -c %CPPFLAGS% %CFLAGS2% %LIBPCCDIR%\negdi2.c
%CC% -c %CPPFLAGS% %CFLAGS2% %LIBPCCDIR%\notdi2.c
%CC% -c %CPPFLAGS% %CFLAGS2% %LIBPCCDIR%\qdivrem.c
%CC% -c %CPPFLAGS% %CFLAGS2% %LIBPCCDIR%\ssp.c
%CC% -c %CPPFLAGS% %CFLAGS2% %LIBPCCDIR%\subdi3.c
%CC% -c %CPPFLAGS% %CFLAGS2% %LIBPCCDIR%\ucmpdi2.c
%CC% -c %CPPFLAGS% %CFLAGS2% %LIBPCCDIR%\udivdi3.c
%CC% -c %CPPFLAGS% %CFLAGS2% %LIBPCCDIR%\umoddi3.c
%CC% -c %CPPFLAGS% %CFLAGS2% %LIBPCCDIR%\xordi3.c

lib /OUT:libpcc.a _alloca.obj adddi3.obj anddi3.obj ashldi3.obj ashrdi3.obj cmpdi2.obj divdi3.obj fixdfdi.obj fixsfdi.obj fixunsdfdi.obj fixunssfdi.obj floatdidf.obj floatdisf.obj floatunsdidf.obj iordi3.obj lshldi3.obj lshrdi3.obj moddi3.obj muldi3.obj negdi2.obj notdi2.obj qdivrem.obj ssp.obj subdi3.obj ucmpdi2.obj udivdi3.obj umoddi3.obj xordi3.obj

copy libpcc.a %LIBPCCDESTDIR%\lib
copy %LIBPCCDIR%\include\*.h %LIBPCCDESTDIR%\include
