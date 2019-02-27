@echo off  

REM mkhashfile
REM
REM This script takes a <name>.pem file
REM and converts it over to a <hashNumber>.0 file.
REM
REM This is to keep in line with industry standards
REM when dealing with chains of Certificate Authoritys (CAs)
REM


set SCRIPTNAME=mkhashfile

REM Progress home directory
REM
REM The following command is tailored during a Secure Client install 
REM giving DLC the correct value.
REM A WebClient customer can also use this script, that install does not
REM tailor, so default DLC to the WebClient location.  It will be tailored
REM correctly by a non-WebClient install.
if "%DLC%"==""    set DLC=C:\Program Files\Progress Software\WebClient

REM Certificate Directory
if "%CERTS%"==""   set CERTS=%DLC%\certs

REM Progress bin directory
if "%BIN%"==""   set BIN=%DLC%\bin


if "%1"== "" goto USAGE

echo.
type "%DLC%\version"
echo.

if exist .hashfile del .hashfile
cscript //nologo "%BIN%"\updatecerts.vbs %1 .hashfile
goto END

:USAGE
echo.
echo This script takes for input a "<name>.pem" file
echo and outputs a "<hashvalue>.0" file.
echo.
goto END


:END
if exist .hashfile del .hashfile

