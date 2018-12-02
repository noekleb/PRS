c:
cd \appdir\se
echo >> log\PRSStg_ImportRUN.log
echo %time% startup >> log\PRSStg_ImportRUN.log
echo %time% %COMPUTERNAME% >> log\PRSStg_ImportRUN.log
echo %time% Starter i c:\appdir\se >> log\PRSStg_ImportRUN.log
echo %time% %SESSIONSTART% >> log\PRSStg_ImportRUN.log
c:\dlc\bin\prowin32 -pf db\start.pf -pf db\PRSStg.pf -p prg\hkdatainnsamling.p -ininame skotex.ini %1 %2 -b
 >> log\PRSStg_ImportRUN.log
 