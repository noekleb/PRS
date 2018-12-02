
:*********************************************************
:*  Standard PARAMETERS FOR STARTUP (do not change)      *
:*********************************************************
@ECHO OFF


:********************************************************
:* Set current directory to UP-ONE-LEVEL FROM bat file **
:********************************************************
call :setAbsPath ABS_PATH ..\

SET ENV=%1
SET DLC=%ABS_PATH%\WebClient
SET PROEXE=%DLC%\bin\_progres
SET SESSION=-cprcodeout undefined -h 10 -inp 8192 -tok 2048 -T %temp% -cpinternal UTF-8 -logthreshold 5000000 -numlogfiles 5
SET INI=%DLC%\stub.ini
SET PROPATH=.;%ABS_PATH%\src;%ABS_PATH%\ftpclient.pl
cd %ABS_PATH%
call :SETPARAMETERS
SET SESSIONSTART=%PROEXE% %SESSION% -basekey INI -ininame %INI% -b -p %BATCHPROGRAM% -param %ABS_PATH%\%CONFIGFILE%
echo %time% Startup on: %COMPUTERNAME%
echo %time% Startup Path: %ABS_PATH%
echo %time% DLC=%DLC%
echo %time% CONFIGFILE=%CONFIGFILE%
echo %time% PROCEDURE=%BATCHPROGRAM%
call %SESSIONSTART% 
echo %time% End...
goto END 

:END
SET DLC=

:setAbsPath
  SET local
  SET __absPath=%~f2
  endlocal && SET %1=%__absPath%
  goto :eof

:********************************************************
:* Configurable parameters for procedure and xml file   *
:********************************************************
:SETPARAMETERS

SET BATCHPROGRAM=FtpFileDownload.p
SET CONFIGFILE=config\FtpFileDownload.config.xml

