rem @echo off

set TARGOS=win32
set MACH=i386
set LIBEXECDIR=""

rem set CC=cl.exe -D__MSC__
rem set CFLAGS=/nologo /Zi /MT
rem set CFLAGS2=/nologo /Zi /MD /Za /Wall /GS-
rem set OBJ=obj
rem set AR=lib
rem set AR_OUT=/OUT:libpcc.a

set CC=pcc.exe
set CFLAGS=
set CFLAGS2=-fno-stack-protector-all
set OBJ=o
set AR=ar
set AR_OUT=r libpcc.a

set BASEDIR=..\..
set MIPDIR=%BASEDIR%\mip
set CPPDIR=%BASEDIR%\cc\cpp
set CCOMDIR=%BASEDIR%\cc\ccom
set CCDIR=%BASEDIR%\cc\cc
set OSDIR=%BASEDIR%\os\%TARGOS%
set MACHDIR=%BASEDIR%\arch\%MACH%
set BISON_SIMPLE=bison.simple
set CPPFLAGS=-DWIN32 -DGCC_COMPAT -DPCC_DEBUG -DCPP_DEBUG -DTARGOS=%TARGOS% -Dos_%TARGOS% -Dmach_%MACH% -DLIBEXECDIR=%LIBEXECDIR% -D_CRT_SECURE_NO_WARNINGS

del *.obj *.o *.exe

%CC% -o pcc.exe %CPPFLAGS% %CFLAGS% -I%CCDIR% -I. -I%MACHDIR% -I%MIPDIR% %CCDIR%\cc.c %MIPDIR%\compat.c

bison -y -t -d --no-lines %CPPDIR%\cpy.y
flex %CPPDIR%\scanner.l
%CC% %CPPFLAGS% %CFLAGS% -I%CPPDIR% -I%OSDIR% -I%MACHDIR% -I%MIPDIR% %CPPDIR%\cpp.c %MIPDIR%\compat.c y.tab.c lex.yy.c "C:\Program Files\UnxUtils\usr\local\lib\libfl.lib"

%CC% -o mkext.exe -DMKEXT %CPPFLAGS% %CFLAGS% -I%CCOMDIR% -I%OSDIR% -I%MACHDIR% -I%MIPDIR% %MIPDIR%\mkext.c %MACHDIR%\table.c %MIPDIR%\common.c
mkext
bison -y -t -d --no-lines %CCOMDIR%\cgram.y
move y.tab.c cgram.c
move y.tab.h cgram.h
flex %CCOMDIR%\scan.l
move lex.yy.c scan.c

%CC% -o ccom.exe %CPPFLAGS% %CFLAGS% -I%CCOMDIR% -I%OSDIR% -I%MACHDIR% -I%MIPDIR% %CCOMDIR%\main.c %MIPDIR%\compat.c scan.c cgram.c external.c %CCOMDIR%\optim.c %CCOMDIR%\pftn.c %CCOMDIR%\trees.c %CCOMDIR%\inline.c %CCOMDIR%\symtabs.c %CCOMDIR%\init.c %MACHDIR%\local.c %MACHDIR%\code.c %CCOMDIR%\stabs.c %CCOMDIR%\gcc_compat.c %MIPDIR%\match.c %MIPDIR%\reader.c %MIPDIR%\optim2.c %MIPDIR%\regs.c %MACHDIR%\local2.c %MACHDIR%\order.c %MACHDIR%\table.c %MIPDIR%\common.c "C:\Program Files\UnxUtils\usr\local\lib\libfl.lib"

set PCCDIR=C:\Program Files\pcc
set LIBPCCDESTDIR=%PCCDIR%\lib\i386-win32\0.9.9
md "%PCCDIR%"
md "%PCCDIR%\bin"
md "%PCCDIR%\libexec"
md "%PCCDIR%\man"
md "%PCCDIR%\man\man1"
md "%LIBPCCDESTDIR%\lib"
md "%LIBPCCDESTDIR%\include"

set LIBPCCDIR=%BASEDIR%\..\pcc-libs\libpcc
%CC% -c %CPPFLAGS% %CFLAGS2% -I%LIBPCCDIR% %LIBPCCDIR%\_alloca.c
%CC% -c %CPPFLAGS% %CFLAGS2% -I%LIBPCCDIR% %LIBPCCDIR%\adddi3.c
%CC% -c %CPPFLAGS% %CFLAGS2% -I%LIBPCCDIR% %LIBPCCDIR%\anddi3.c
%CC% -c %CPPFLAGS% %CFLAGS2% -I%LIBPCCDIR% %LIBPCCDIR%\ashldi3.c
%CC% -c %CPPFLAGS% %CFLAGS2% -I%LIBPCCDIR% %LIBPCCDIR%\ashrdi3.c
%CC% -c %CPPFLAGS% %CFLAGS2% -I%LIBPCCDIR% %LIBPCCDIR%\cmpdi2.c
%CC% -c %CPPFLAGS% %CFLAGS2% -I%LIBPCCDIR% %LIBPCCDIR%\divdi3.c
%CC% -c %CPPFLAGS% %CFLAGS2% -I%LIBPCCDIR% %LIBPCCDIR%\fixdfdi.c
%CC% -c %CPPFLAGS% %CFLAGS2% -I%LIBPCCDIR% %LIBPCCDIR%\fixsfdi.c
%CC% -c %CPPFLAGS% %CFLAGS2% -I%LIBPCCDIR% %LIBPCCDIR%\fixunsdfdi.c
%CC% -c %CPPFLAGS% %CFLAGS2% -I%LIBPCCDIR% %LIBPCCDIR%\fixunssfdi.c
%CC% -c %CPPFLAGS% %CFLAGS2% -I%LIBPCCDIR% %LIBPCCDIR%\floatdidf.c
%CC% -c %CPPFLAGS% %CFLAGS2% -I%LIBPCCDIR% %LIBPCCDIR%\floatdisf.c
%CC% -c %CPPFLAGS% %CFLAGS2% -I%LIBPCCDIR% %LIBPCCDIR%\floatunsdidf.c
%CC% -c %CPPFLAGS% %CFLAGS2% -I%LIBPCCDIR% %LIBPCCDIR%\iordi3.c
%CC% -c %CPPFLAGS% %CFLAGS2% -I%LIBPCCDIR% %LIBPCCDIR%\lshldi3.c
%CC% -c %CPPFLAGS% %CFLAGS2% -I%LIBPCCDIR% %LIBPCCDIR%\lshrdi3.c
%CC% -c %CPPFLAGS% %CFLAGS2% -I%LIBPCCDIR% %LIBPCCDIR%\moddi3.c
%CC% -c %CPPFLAGS% %CFLAGS2% -I%LIBPCCDIR% %LIBPCCDIR%\muldi3.c
%CC% -c %CPPFLAGS% %CFLAGS2% -I%LIBPCCDIR% %LIBPCCDIR%\negdi2.c
%CC% -c %CPPFLAGS% %CFLAGS2% -I%LIBPCCDIR% %LIBPCCDIR%\notdi2.c
%CC% -c %CPPFLAGS% %CFLAGS2% -I%LIBPCCDIR% %LIBPCCDIR%\qdivrem.c
%CC% -c %CPPFLAGS% %CFLAGS2% -I%LIBPCCDIR% %LIBPCCDIR%\ssp.c
%CC% -c %CPPFLAGS% %CFLAGS2% -I%LIBPCCDIR% %LIBPCCDIR%\subdi3.c
%CC% -c %CPPFLAGS% %CFLAGS2% -I%LIBPCCDIR% %LIBPCCDIR%\ucmpdi2.c
%CC% -c %CPPFLAGS% %CFLAGS2% -I%LIBPCCDIR% %LIBPCCDIR%\udivdi3.c
%CC% -c %CPPFLAGS% %CFLAGS2% -I%LIBPCCDIR% %LIBPCCDIR%\umoddi3.c
%CC% -c %CPPFLAGS% %CFLAGS2% -I%LIBPCCDIR% %LIBPCCDIR%\xordi3.c

%CC% -c %CPPFLAGS% %CFLAGS2% -I%LIBPCCDIR% %LIBPCCDIR%\_ftol.c
rem ml -c %LIBPCCDIR%\_ftol.asm

%AR% %AR_OUT% _ftol.%OBJ% adddi3.%OBJ% anddi3.%OBJ% ashldi3.%OBJ% ashrdi3.%OBJ% cmpdi2.%OBJ% divdi3.%OBJ% fixdfdi.%OBJ% fixsfdi.%OBJ% fixunsdfdi.%OBJ% fixunssfdi.%OBJ% floatdidf.%OBJ% floatdisf.%OBJ% floatunsdidf.%OBJ% iordi3.%OBJ% lshldi3.%OBJ% lshrdi3.%OBJ% moddi3.%OBJ% muldi3.%OBJ% negdi2.%OBJ% notdi2.%OBJ% qdivrem.%OBJ% ssp.%OBJ% subdi3.%OBJ% ucmpdi2.%OBJ% udivdi3.%OBJ% umoddi3.%OBJ% xordi3.%OBJ%

copy pcc.exe "%PCCDIR%\bin"
copy cpp.exe "%PCCDIR%\libexec"
copy ccom.exe "%PCCDIR%\libexec"

copy libpcc.a "%LIBPCCDESTDIR%\lib"
copy "%LIBPCCDIR%\include\*.h" "%LIBPCCDESTDIR%\include"

copy "%CCDIR%\cc.1" "%PCCDIR%\man\man1"
copy "%CPPDIR%\cpp.1" "%PCCDIR%\man\man1"
copy "%CCOMDIR%\ccom.1" "%PCCDIR%\man\man1"

