DEF VAR pi AS DEC FORMAT ">>>,>>>,>>>,>>>,>>>,>>9.99" NO-UNDO.

pause 0. Display "pfTenderRevenue Flipp 1".
pi = 0.
FOR EACH pfTenderRevenue: 
    pi = pi + 1.
    PAUSE 0.
    IF pi MODULO 500 = 0 THEN DISPLAY pi.
    DELETE pfTenderRevenue. 
END.

pause 0. Display "pfStatisticsRevenue Flipp 2".
pi = 0.
FOR EACH pfStatisticsRevenue: 
    pi = pi + 1.
    PAUSE 0.
    IF pi MODULO 500 = 0 THEN DISPLAY pi.
    DELETE pfStatisticsRevenue. 
END.

pause 0. Display "pfSlInfo Flipp 3".
FOR EACH pfSlInfo: 
    pi = pi + 1.
    PAUSE 0.
    IF pi MODULO 500 = 0 THEN DISPLAY pi.
    DELETE pfSlInfo. 
END.

pause 0. Display "pfMedias Flipp 4".
pi = 0.
FOR EACH pfMedias: 
    pi = pi + 1.
    PAUSE 0.
    IF pi MODULO 500 = 0 THEN DISPLAY pi.
    DELETE pfMedias. 
END.

pause 0. Display "pfManefacturer Flipp 5".
pi = 0.
FOR EACH pfManefacturer: 
    pi = pi + 1.
    PAUSE 0.
    IF pi MODULO 500 = 0 THEN DISPLAY pi.
    DELETE pfManefacturer. 
END.

pause 0. Display "pfItemSubGroup Flipp 6".
pi = 0.
FOR EACH pfItemSubGroup: 
    pi = pi + 1.
    PAUSE 0.
    IF pi MODULO 500 = 0 THEN DISPLAY pi.
    DELETE pfItemSubGroup. 
END.

pause 0. Display "pfDepartments Flipp 7".
pi = 0.
FOR EACH pfDepartments: 
    pi = pi + 1.
    PAUSE 0.
    IF pi MODULO 500 = 0 THEN DISPLAY pi.
    DELETE pfDepartments. 
END.

pause 0. Display "pfDaySales_HourExt Flipp 8".
pi = 0.
FOR EACH pfDaySales_HourExt: 
    pi = pi + 1.
    PAUSE 0.
    IF pi MODULO 500 = 0 THEN DISPLAY pi.
    DELETE pfDaySales_HourExt. 
END.

pause 0. Display "pfDaySales Flipp 9".
pi = 0.
FOR EACH pfDaySales: 
    pi = pi + 1.
    PAUSE 0.
    IF pi MODULO 500 = 0 THEN DISPLAY pi.
    DELETE pfDaySales. 
END.

pause 0. Display "pbVendor Flipp 10".
pi = 0.
FOR EACH pbVendor: 
    pi = pi + 1.
    PAUSE 0.
    IF pi MODULO 500 = 0 THEN DISPLAY pi.
    DELETE pbVendor. 
END.

pause 0. Display "pfItemGroup Flipp 11".
pi = 0.
FOR EACH pfItemGroup: 
    pi = pi + 1.
    PAUSE 0.
    IF pi MODULO 500 = 0 THEN DISPLAY pi.
    DELETE pfItemGroup. 
END.

pause 0. Display "pfItemInfo Flipp 12".
pi = 0.
FOR EACH pfItemInfo: 
    pi = pi + 1.
    PAUSE 0.
    IF pi MODULO 500 = 0 THEN DISPLAY pi.
    DELETE pfItemInfo. 
END.

