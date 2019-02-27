/************************************************************
    Program:  dispprisko.i
    Created:  TN   20 Mar 100
Description:

Last change:  TN   26 Apr 100    2:23 pm
************************************************************/

IF AVAILABLE {&Tabell} THEN
  DO WITH FRAME FRAME-PrisInfo:

    ASSIGN
      CB-Tilbud:SCREEN-VALUE = "1".      
    APPLY "VALUE-CHANGED":U TO CB-Tilbud IN FRAME FRAME-PrisInfo.

    IF AVAILABLE {&Tabell} THEN
      DO:
        ASSIGN
          FI-ValPris:screen-value  = STRING({&Tabell}.ValPris[1])
          FI-InnPris:screen-value  = STRING({&Tabell}.InnkjopsPris[1])
          FI-Rab1:screen-value     = STRING({&Tabell}.Rab1Kr[1])
          FI-Rab1%:screen-value    = STRING({&Tabell}.Rab1%[1])
          FI-Rab2:screen-value     = STRING({&Tabell}.Rab2Kr[1])
          FI-Rab2%:screen-value    = STRING({&Tabell}.Rab2%[1])
          FI-Frakt:screen-value    = STRING({&Tabell}.Frakt[1])
          FI-Frakt%:screen-value   = STRING({&Tabell}.Frakt%[1])
          FI-DivKost:screen-value  = STRING({&Tabell}.DivKostKr[1])
          FI-DivKost%:screen-value = STRING({&Tabell}.DivKost%[1])
          FI-Rab3:screen-value     = STRING({&Tabell}.Rab3Kr[1])
          FI-Rab3%:screen-value    = STRING({&Tabell}.Rab3%[1])
          FI-VareKost:screen-value = STRING({&Tabell}.VareKost[1])
          FI-Mva:screen-value      = STRING({&Tabell}.MvaKr[1])
          FI-Mva%:screen-value     = STRING({&Tabell}.Mva%[1])
          FI-DB:screen-value       = STRING({&Tabell}.DBKr[1])
          FI-DB%:screen-value      = STRING({&Tabell}.DB%[1])
          FI-Pris:screen-value     = STRING({&Tabell}.Pris[1])
          FI-EUPris:screen-value   = STRING({&Tabell}.EuroPris[1]).
        ASSIGN
          FI-Txt2:HIDDEN    = TRUE
          FI-NAktFra:HIDDEN = TRUE 
          FI-NTid:HIDDEN    = TRUE 
          FI-AktFra:screen-value  = STRING({&Tabell}.AktivFraDato)
          FI-Tid1:SCREEN-VALUE    = SUBSTRING(STRING({&Tabell}.AktivFraTid,"HH:MM"),1,2) + "," + 
                                    SUBstring(STRING({&Tabell}.AktivFraTid,"HH:MM"),4,2)
          FI-AktTil:screen-value  = ''
          FI-Tid2:SCREEN-VALUE    = '' 
          .
      END.
  END.

