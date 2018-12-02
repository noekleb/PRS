set DLC=C:\PROGRESS\OpenEdge
set DLCBIN=%DLC%\bin
call %dlcbin%\dbman -start prod-skotex
call %dlcbin%\dbman -start prod-data
call %dlcbin%\dbman -start prod-wr
call %dlcbin%\dbman -start prod-vpi
call %dlcbin%\dbman -start prod-temp-db



