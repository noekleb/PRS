set DLC=C:\PROGRESS\OpenEdge
set DLCBIN=%DLC%\bin

DEL e:\bku\skotex.bku
call %dlcbin%\_mprshut.exe d:\Db\Sport1\skotex.db -C backup online e:\bku\skotex.bku
del e:\bku\data.bku
call %dlcbin%\_mprshut.exe d:\db\Sport1\data.db -C backup online e:\bku\data.bku
del e:\bku\vpi.bku
call %dlcbin%\_mprshut.exe d:\db\Sport1\vpi.db -C backup online e:\bku\vpi.bku



