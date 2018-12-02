set DLC=C:\PROGRESS\OpenEdge
set DLCBIN=%DLC%\bin
call %dlcbin%\dbman -stop prod-skotex
call %dlcbin%\dbman -stop prod-data
call %dlcbin%\dbman -stop prod-wr
call %dlcbin%\dbman -stop prod-vpi
call %dlcbin%\dbman -stop prod-temp-db



