:*********************************************************
:* JobSchedule.bat                                       *
:* ---------------                                       *
:*                                                       *
:* Description :                                         *
:*     Batch file for executing progress session and     *
:*     execute of POS Links for upload and down-         *
:*     load of information from POS and external sys     *
:*                                                       *
:*     Executes section dependent on machine name        *
:*     Valid hosts: add hosts /nbco3                     *
:*                                                       *
:*                                                       *
:*  By: Curt H. Oldenborg (2011)                         *
:*********************************************************
@ECHO OFF
GOTO %COMPUTERNAME%


:********************************************************
:* NBCO3.progress.com Settings (DEVELOPMENT LOCAL)      *
:********************************************************
:MOBOBASE
SET DLC=C:\WebClient
SET APP=C:\appdir\se
SET ENV=%1
GOTO STARTUP


:********************************************************
:* Progress Batch Startup and Execute of links          *
:********************************************************
:STARTUP

SET PROEXE=%DLC%\bin\_progres
SET SESSION=-cprcodeout undefined -h 10 -inp 8192 -tok 2048 -T %temp% -cpinternal UTF-8 
SET INI=cmd\stub.ini
SET PROPATH=.;.\pl\ftpclient.pl

SET BATCHPROGRAM=FtpFileUpload.p
SET CONFIGFILE=cmd\MazeHitrate.config.xml
SET SESSIONSTART=%PROEXE% %SESSION% -basekey INI -ininame %INI% -b -p %BATCHPROGRAM% -param %CONFIGFILE%

cd %APP%

echo %time% Startup...
echo %time% Using Settins for: %COMPUTERNAME%
echo %time% DLC=%DLC%
echo %time% Starting in: %APP%
echo %time% %SESSIONSTART%
call %SESSIONSTART% 
echo %time% End...

goto END 

:END
SET DLC=

