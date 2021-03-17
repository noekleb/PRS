@echo off
set DLC=eksportSBudsjettTimeGrip.bat
set WRKDIR=C:\OpenEdge10.2b\WRK
set OEM=C:\progress10.2b\oemgmt
set OEMWRKDIR=C:\OpenEdge10.2b\wrk_oemgmt
c:
cd C:\NSoft\Polygon\PRS
prowin32 -ininame skotex.ini -pf db\start2.pf -p cls\RFIDEtikett\runFtpSendfile.p -b -debugalert
