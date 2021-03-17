@echo off
set DLC=C:\dlc
set WRKDIR=C:\OpenEdge\WRK
set OEM=C:\oemgmt
set OEMWRKDIR=C:\OpenEdge\wrk_oemgmt
c:
cd C:\appdir\se
c:\dlc\bin\prowin32 -ininame skotex.ini -pf db\start2.pf -p plliste_gen_ordreforslag_run.p -param "BUTLST=16" -b