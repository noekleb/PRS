@echo off
set DLC=C:\Progress\OpenEdge
set WRKDIR=C:\polygon\prs
set OEM=C:\Progress\oemgmt
set OEMWRKDIR=C:\OpenEdge\wrk_oemgmt
set OPENSSL_CONF=%DLC%\keys\policy\pscpki.cnf
set BPSERVER_BIN=%DLC%\oebpm\server\bin

if not "%OS%"=="Windows_NT" set ENVSIZE=/E:5120

c:
cd c:\polygon\prs
C:\Progress\OpenEdge\bin\prowin -ininame skotex.ini -pf pf\clskotex.pf -p cls\Consignor\runHentPkSdlConsignorData.p -b -assemblies C:\Polygon\assemblies