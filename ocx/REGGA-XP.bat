c:
cd \windows
copy q:\appdir\skotex\ocx\*.ocx c:\windows
cd \windows\system32
c:\windows\system32\regsvr32 comctl32.ocx
c:\windows\system32\regsvr32 msflxgrd.ocx
c:\windows\system32\regsvr32 oik32.ocx
c:\windows\system32\regsvr32 iemenu.ocx
c:\windows\system32\regsvr32 vsflex6d.ocx
c:\windows\system32\regsvr32 mschart.ocx
c:\windows\system32\regsvr32 mscomctl.ocx
c:\windows\system32\regsvr32 mschrt20.ocx
c:\windows\system32\regsvr32 Vsflex7L.ocx
rem copy q:\appdir\skotex\ocx\skotex32.lnk c:\windows\skrivbord

