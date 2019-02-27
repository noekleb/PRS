:Filkopiering f”r SkoDej V1.0
:Samlar in alla kvitton fr†n vald kassa, sl†r samman, l„gger till datain

:usage: samla1.cmd 

c:
cd \appdir\se\kom\in\kassafiler

:Kopiera filerna till respektive inkatalog 
for %%i IN (l:\pos\data\00??????.dat) do copy %%i 

:Ta bort de kopierad filerna fr†n kassan
for %%i IN (*.dat) do del l:\pos\data\%%i

:Addera filerna till EN datain-fil
for %%i IN (*.dat) do type %%i >> c:\appdir\se\kom\in\datain

:G”r en backup „ven till trbuffer
for %%i IN (*.dat) do type %%i >> c:\appdir\se\kom\in\trbuffer


:NU kan vi ta bort filerna med gott samvete
for %%i IN (*.dat) do del %%i
