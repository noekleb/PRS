:Filkopiering för PRS V1.0
:Samlar in alla kvitton fr†n vald kassa, 

:usage: samla1.cmd ip butnr

d:
cd \appdir\skotex\kom\in\kassafiler

:Kopiera filerna till respektive inkatalog 
for %%i IN (\\%1\kom\ut\PRS*.%2) do copy /Y %%i 

if errorlevel 1 goto felkopiering
:Ta bort de kopierad filerna fr†n kassan
:tabort
for %%i IN (PRS*.%2) do del /Q \\%1\kom\ut\\%%i

:säkerhetskopia
for %%i IN (PRS*.%2) do copy /Y %%i bku\%%i

:flytta till inläsning
for %%i IN (PRS*.%2) do copy /Y %%i ..\%%i
for %%i IN (PRS*.%2) do del /Q %%i 

:felkopiering
:gör någon skit...





