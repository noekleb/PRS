TRIGGER PROCEDURE FOR DELETE OF StLinje.
IF StLinje.StTypeId = "ARTIKKEL" AND DECI(StLinje.DataObjekt) < 8500000 THEN
    RETURN.
/* Vi använder CHR(2) i stf CHR(1), Vissa ststistiker innhåller */
/* CHR(1) i DataObjekt ex. SELGER-VG */

DEF VAR btrgHk    AS LOG  NO-UNDO.
DEFINE VARIABLE cTekst AS CHARACTER NO-UNDO.

{syspara.i 1 1 18 cTekst}
IF CAN-DO("ja,1,true,yes",cTekst) THEN
    btrgHK = TRUE.
ELSE
    btrgHK = FALSE.


IF btrgHK THEN
DO:
    FIND ELogg WHERE 
         ELogg.TabellNavn     = "StLinje" AND
         ELogg.EksterntSystem = "HK"    AND
         ELogg.Verdier        = StLinje.StTypeId      + CHR(2) + 
                                Stlinje.PerId         + CHR(2) + 
                                StLinje.DataObjekt    + CHR(2) + 
                                StLinje.Diverse       + CHR(2) + 
                                STRING(StLinje.Butik) + CHR(2) + 
                                STRING(StLinje.Aar)   + CHR(2) + 
                                STRING(StLinje.PerLinNr) NO-ERROR.
    IF NOT AVAIL Elogg THEN DO:
        CREATE Elogg.
        ASSIGN ELogg.TabellNavn     = "StLinje"
               ELogg.EksterntSystem = "HK"   
               ELogg.Verdier        = StLinje.StTypeId      + CHR(2) + 
                                      Stlinje.PerId         + CHR(2) + 
                                      StLinje.DataObjekt    + CHR(2) + 
                                      StLinje.Diverse       + CHR(2) + 
                                      STRING(StLinje.Butik) + CHR(2) + 
                                      STRING(StLinje.Aar)   + CHR(2) + 
                                      STRING(StLinje.PerLinNr) NO-ERROR.
    END.
    ASSIGN ELogg.EndringsType = 3
           ELogg.Behandlet    = FALSE.
END.
RELEASE ELogg.


