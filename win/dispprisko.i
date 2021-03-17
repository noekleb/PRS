/************************************************************
    Program:  dispprisko.i
    Created:  TN   20 Mar 100
Description:

Last change:  TN   26 Apr 100    2:23 pm
************************************************************/

IF AVAILABLE {&Tabell} THEN
  DO WITH FRAME FRAME-PrisInfo:

    IF CAN-DO("2,3",STRING({&Tabell}.Type)) THEN
    ASSIGN
      CB-Tilbud:SCREEN-VALUE = "2".      
    IF CAN-DO("5,6",STRING({&Tabell}.Type)) THEN
    ASSIGN
      CB-Tilbud:SCREEN-VALUE = "5".      
    ELSE
        ASSIGN
            CB-Tilbud:SCREEN-VALUE = IF {&Tabell}.Type = 1
                                       THEN "1"
                                       ELSE "3".
    APPLY "VALUE-CHANGED":U TO CB-Tilbud IN FRAME FRAME-PrisInfo.

    IF AVAILABLE {&Tabell} THEN
      DO:
        ASSIGN
          FI-ValPris:screen-value  = STRING({&Tabell}.ValPris)
          FI-InnPris:screen-value  = STRING({&Tabell}.InnkjopsPris)
          FI-Rab1:screen-value     = STRING({&Tabell}.Rab1Kr)
          FI-Rab1%:screen-value    = STRING({&Tabell}.Rab1%)
          FI-Rab2:screen-value     = STRING({&Tabell}.Rab2Kr)
          FI-Rab2%:screen-value    = STRING({&Tabell}.Rab2%)
          FI-Frakt:screen-value    = STRING({&Tabell}.Frakt)
          FI-Frakt%:screen-value   = STRING({&Tabell}.Frakt%)
          FI-DivKost:screen-value  = STRING({&Tabell}.DivKostKr)
          FI-DivKost%:screen-value = STRING({&Tabell}.DivKost%)
          FI-Rab3:screen-value     = STRING({&Tabell}.Rab3Kr)
          FI-Rab3%:screen-value    = STRING({&Tabell}.Rab3%)
          FI-VareKost:screen-value = STRING({&Tabell}.VareKost)
          FI-Mva:screen-value      = STRING({&Tabell}.MvaKr)
          FI-Mva%:screen-value     = STRING({&Tabell}.Mva%)
          FI-DB:screen-value       = STRING({&Tabell}.DBKr)
          FI-DB%:screen-value      = STRING({&Tabell}.DB%)
          FI-Pris:screen-value     = STRING({&Tabell}.Pris)
          FI-EUPris:screen-value   = STRING({&Tabell}.EuroPris).
        IF (CB-Tilbud <> 2 AND CB-Tilbud <> 5) THEN
        /*if INPUT T-Tilbud = false then*/
          ASSIGN
            FI-NAktFra:screen-value = STRING({&Tabell}.AktiveresDato)
            FI-NTid:SCREEN-VALUE    = SUBSTRING(STRING({&Tabell}.AktiveresTid,"HH:MM"),1,2) + "," + 
                                      SUBstring(STRING({&Tabell}.AktiveresTid,"HH:MM"),4,2)
            FI-Txt1:HIDDEN   = TRUE
            FI-AktFra:HIDDEN = TRUE 
            FI-Tid1:HIDDEN   = TRUE 
            FI-AktTil:HIDDEN = TRUE 
            FI-Tid2:HIDDEN   = TRUE 
            .
        ELSE
          ASSIGN
            FI-Txt2:HIDDEN    = TRUE
            FI-NAktFra:HIDDEN = TRUE 
            FI-NTid:HIDDEN    = TRUE 
            FI-AktFra:screen-value  = STRING({&Tabell}.AktiveresDato)
            FI-Tid1:SCREEN-VALUE    = SUBSTRING(STRING({&Tabell}.AktiveresTid,"HH:MM"),1,2) + "," + 
                                      SUBstring(STRING({&Tabell}.AktiveresTid,"HH:MM"),4,2)
            FI-AktTil:screen-value  = STRING({&Tabell}.GyldigTilDato)
            FI-Tid2:SCREEN-VALUE    = SUBSTRING(STRING({&Tabell}.GyldigTilTid,"HH:MM"),1,2) + "," + 
                                      SUBstring(STRING({&Tabell}.GyldigtilTid,"HH:MM"),4,2)
            .
            /*FI-AktTil:screen-value  = string({&Tabell}.GyldigTilDato).*/
          END.
  END.
/*
if AVAILABLE {&Tabell} then
  browse BROWSE-{&Tabell}:deselect-focused-row().
*/

