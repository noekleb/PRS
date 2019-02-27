@echo off
set DLC=C:\dlc
set WRKDIR=C:\OpenEdge\WRK
set OEM=C:\oemgmt
set OEMWRKDIR=C:\OpenEdge\wrk_oemgmt
c:
cd C:\appdir\se
prowin32 -ininame skotex.ini -pf db\start2.pf -p eksporterSBudsjettTimeGrip_run.p -b -debugalert
