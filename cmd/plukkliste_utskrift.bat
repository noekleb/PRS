@echo off
set DLC=C:\dlc
set WRKDIR=C:\OpenEdge\WRK
set OEM=C:\oemgmt
set OEMWRKDIR=C:\OpenEdge\wrk_oemgmt
c:
cd C:\appdir\se
c:\dlc\bin\prowin32.exe -ininame skotex.ini -pf db\start2.pf -p runplukkliste_utskrift.p -b
