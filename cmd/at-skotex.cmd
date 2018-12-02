: Kommandofil for oppstart av batch jobber via ProControl
: Sett inn kommentartegn for de jobbene som ikke skal startes.

At 20:00 /every:M†ndag,Tisdag,Onsdag,Torsdag,Fredag,L”rdag,S”ndag PCCmd OtherTask Start KlargjorPrisKo
At 20:30 /every:M†ndag,Tisdag,Onsdag,Torsdag,Fredag,L”rdag,S”ndag PCCmd OtherTask Start UtleggPrisFil
At 22:00 /interactive /every:M†ndag,Tisdag,Onsdag,Torsdag,Fredag,L”rdag,S”ndag cmd /C d:\appdir\skotex\kom\pgm\kasskom.cmd
At 23:00 /every:M†ndag,Tisdag,Onsdag,Torsdag,Fredag,L”rdag,S”ndag PCCmd OtherTask Start InnLesData
:At 00:02 PCCmd OtherTask Start  OppdLagerTrans
At 00:45 /every:M†ndag,Tisdag,Onsdag,Torsdag,Fredag,L”rdag,S”ndag PCCmd OtherTask Start LagerUt
At 01:00 /interactive /every:M†ndag,Tisdag,Onsdag,Torsdag,Fredag,L”rdag,S”ndag cmd /C d:\appdir\skotex\kom\pgm\lagerkom.cmd
At 02:00 /every:M†ndag,Tisdag,Onsdag,Torsdag,Fredag,L”rdag,S”ndag PCCmd OtherTask Start Backup
At 03:00 /interactive /every:M†ndag,Tisdag,Onsdag,Torsdag,Fredag,L”rdag,S”ndag cmd /C d:\appdir\skotex\kom\pgm\back.cmd
:At 04:00 /interactive /every:M†ndag,Tisdag,Onsdag,Torsdag,Fredag,L”rdag,S”ndag ntbackup backup c:\ e:\ f:\ hc:on /l "f:\skotex\backup.log" /e 

:cls
:at
