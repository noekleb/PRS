@echo off
set DLC=C:\WebClient7_11
set WRKDIR=C:\Polygon\PRSRestCl\log
set PROMSGS=C:\WebClient7_11\promsgs
c:
cd C:\NSoft\Polygon\PRSRestCl
C:\WebClient7_11\bin\prowc.exe -ininame ini\PRSRestWebCl.ini -pf ini\start.pf -p winsrc\startPRSPosKlient.p -debugalert -assemblies C:\NSoft\JukeBox\assemblies -param "BUTIKK=%1,SELGER=%2"