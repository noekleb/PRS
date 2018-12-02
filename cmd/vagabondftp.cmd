rem @ECHO OFF
:: Puts file on ftpserver

d:
cd \appdir\skotex\export

:: Check if a filename was given
IF "%1"=="" GOTO Syntax

:: Create the temporary script file
> script.ftp ECHO open ftp.vagabond.com
>>script.ftp ECHO vb2062
>>script.ftp ECHO dg34rf
>>script.ftp ECHO prompt n
>>script.ftp ECHO put %1
>>script.ftp ECHO bye

:: >>script.ftp ECHO dir
:: >>script.ftp ECHO mget *.*

:: Use the temporary script for unattended FTP
:: Note: depending on your OS version you may have to add a '-n' switch

FTP -v -s:script.ftp 

:: For the paranoid: overwrite the temporary file before deleting it
TYPE NUL >script.ftp
DEL script.ftp
GOTO End

:Syntax
ECHO Usage: %0 filename

:End
exit
