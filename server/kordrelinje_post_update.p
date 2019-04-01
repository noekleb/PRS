/* Kjøres etter oppdatering av pris,rabatt eller antall, se prosedyre MySaveBrowseFillIn i KOrdreLinje.w  
   NB: Beste pris er et kalkulert felt som sendes med tilbake hit for å gjøre riktig rabattberegning
   Endret:  22.04.13 av Brynjar
          - Kundens mva kode / mvafritt overstyrer mva-beregningen 
   Endret:  14.05.13 av Brynjar
          - Rettet slik at felt-parameter blir riktig ("NettoPris") når funksjonen kalles fra kordrelinje_kalkpakke.p
   Endret 27/3-19 TN
          - Oppdaterer nå også LinjeSum feltet (NettoLinjeSum + MVAKr).
-----------------------------------------------------------------------------------------*/   
DEF INPUT  PARAM ihBuffer       AS HANDLE NO-UNDO.  /* Handle to current buffer. Her: KOrdreLinje */
DEF INPUT  PARAM icAction       AS CHAR   NO-UNDO.  /* Create or Update */
DEF INPUT  PARAM icSessionId    AS CHAR   NO-UNDO.
DEF OUTPUT PARAM ocValue        AS CHAR   NO-UNDO.  /* Error message. If <> blank the transaction is backed out */

DEF VAR cField        AS CHAR  NO-UNDO.  /* Last modified field */
DEF VAR iCl           AS INT   NO-UNDO.
DEF VAR fOrdreRabPris AS DEC   NO-UNDO.
DEF VAR cFieldParam   AS CHAR  NO-UNDO.     

DEFINE BUFFER bufKOrdreHode FOR KOrdreHode.

cField = DYNAMIC-FUNCTION("getCurrentValueFields" IN SOURCE-PROCEDURE) NO-ERROR.
IF NOT ERROR-STATUS:ERROR THEN
  cField = ENTRY(1,cField).
         
cFieldParam = DYNAMIC-FUNCTION("getInputParam" IN SOURCE-PROCEDURE) NO-ERROR.
IF cFieldParam NE "" AND (cField = ? OR cField = "") THEN
  cField = cFieldParam.

FIND KOrdreHode WHERE KOrdreHode.KOrdre_id = DEC(ihBuffer:BUFFER-FIELD("KOrdre_id"):BUFFER-VALUE) NO-LOCK NO-ERROR.
IF NOT AVAIL KOrdreHode THEN DO:
  ocValue = "Hopp i havet. Ordren mangler :)".
  RETURN.
END.

IF ihBuffer:BUFFER-FIELD("VareNr"):BUFFER-VALUE = "" THEN
  ASSIGN ihBuffer:BUFFER-FIELD("BruttoPris"):BUFFER-VALUE = ihBuffer:BUFFER-FIELD("NettoPris"):BUFFER-VALUE
         ihBuffer:BUFFER-FIELD("Pris"):BUFFER-VALUE = ihBuffer:BUFFER-FIELD("NettoPris"):BUFFER-VALUE
         ihBuffer:BUFFER-FIELD("VareKost"):BUFFER-VALUE   = ihBuffer:BUFFER-FIELD("BruttoPris"):BUFFER-VALUE * 0.65.

ASSIGN fOrdreRabPris = ihBuffer:BUFFER-FIELD("Pris"):BUFFER-VALUE - ihBuffer:BUFFER-FIELD("BruttoPris"):BUFFER-VALUE * KOrdreHode.TotalRabatt% / 100
       ihBuffer:BUFFER-FIELD("OrdreRabattKr"):BUFFER-VALUE = (ihBuffer:BUFFER-FIELD("BruttoPris"):BUFFER-VALUE - fOrdreRabPris) * ihBuffer:BUFFER-FIELD("Antall"):BUFFER-VALUE          
       .
         
IF cField = "LinjeRab%" THEN 
DO:
  IF ihBuffer:BUFFER-FIELD("LinjeRab%"):BUFFER-VALUE NE 0 THEN
    ihBuffer:BUFFER-FIELD("NettoPris"):BUFFER-VALUE = fOrdreRabPris - fOrdreRabPris * ihBuffer:BUFFER-FIELD("LinjeRab%"):BUFFER-VALUE / 100.
  ELSE
    ihBuffer:BUFFER-FIELD("NettoPris"):BUFFER-VALUE = fOrdreRabPris - fOrdreRabPris * ihBuffer:BUFFER-FIELD("KundeRab%"):BUFFER-VALUE / 100.
END.
ELSE IF cField = "NettoPris" THEN 
  ASSIGN ihBuffer:BUFFER-FIELD("LinjeRab%"):BUFFER-VALUE = (1 - ihBuffer:BUFFER-FIELD("NettoPris"):BUFFER-VALUE / fOrdreRabPris) * 100
         .

IF ihBuffer:BUFFER-FIELD("LinjeRab%"):BUFFER-VALUE = ? THEN ihBuffer:BUFFER-FIELD("LinjeRab%"):BUFFER-VALUE = 0.

ASSIGN ihBuffer:BUFFER-FIELD("NettoLinjesum"):BUFFER-VALUE = ihBuffer:BUFFER-FIELD("NettoPris"):BUFFER-VALUE * ihBuffer:BUFFER-FIELD("Antall"):BUFFER-VALUE
       ihBuffer:BUFFER-FIELD("LinjeRabattKr"):BUFFER-VALUE = (fOrdreRabPris - ihBuffer:BUFFER-FIELD("NettoPris"):BUFFER-VALUE) * ihBuffer:BUFFER-FIELD("Antall"):BUFFER-VALUE
       ihBuffer:BUFFER-FIELD("KundeRabattKr"):BUFFER-VALUE = (ihBuffer:BUFFER-FIELD("Pris"):BUFFER-VALUE - ihBuffer:BUFFER-FIELD("BruttoPris"):BUFFER-VALUE) * ihBuffer:BUFFER-FIELD("Antall"):BUFFER-VALUE
       .
         
FIND FIRST Moms WHERE Moms.MomsKod = INT(ihBuffer:BUFFER-FIELD("MomsKod"):BUFFER-VALUE) NO-LOCK NO-ERROR.

IF AVAIL Moms THEN
  ASSIGN ihBuffer:BUFFER-FIELD("Mva%"):BUFFER-VALUE     = Moms.MomsProc
         ihBuffer:BUFFER-FIELD("MvaKr"):BUFFER-VALUE    = (IF Moms.MomsProc = 0 THEN 0 ELSE ihBuffer:BUFFER-FIELD("NettoLinjeSum"):BUFFER-VALUE - ihBuffer:BUFFER-FIELD("NettoLinjeSum"):BUFFER-VALUE / (1 + Moms.MomsProc / 100))
         ihBuffer:BUFFER-FIELD("Linjesum"):BUFFER-VALUE = ihBuffer:BUFFER-FIELD("NettoLinjesum"):BUFFER-VALUE + ihBuffer:BUFFER-FIELD("MvaKr"):BUFFER-VALUE          
         .
ELSE
  ASSIGN ihBuffer:BUFFER-FIELD("Mva%"):BUFFER-VALUE     = 0
         ihBuffer:BUFFER-FIELD("MvaKr"):BUFFER-VALUE    = 0
         .

IF ihBuffer:BUFFER-FIELD("VareKost"):BUFFER-VALUE NE 0 THEN 
  ihBuffer:BUFFER-FIELD("DbKr"):BUFFER-VALUE = ihBuffer:BUFFER-FIELD("NettoLinjeSum"):BUFFER-VALUE - 
                                               ihBuffer:BUFFER-FIELD("MvaKr"):BUFFER-VALUE - 
                                               ihBuffer:BUFFER-FIELD("Varekost"):BUFFER-VALUE * ihBuffer:BUFFER-FIELD("Antall"):BUFFER-VALUE.
ELSE
  ihBuffer:BUFFER-FIELD("DbKr"):BUFFER-VALUE = ihBuffer:BUFFER-FIELD("NettoLinjeSum"):BUFFER-VALUE * 0.35.

ihBuffer:BUFFER-FIELD("Db%"):BUFFER-VALUE  = ihBuffer:BUFFER-FIELD("DbKr"):BUFFER-VALUE / 
                                            (ihBuffer:BUFFER-FIELD("NettoLinjeSum"):BUFFER-VALUE 
                                             - ihBuffer:BUFFER-FIELD("MvaKr"):BUFFER-VALUE) * 100.
IF ihBuffer:BUFFER-FIELD("Db%"):BUFFER-VALUE = ? THEN
  ihBuffer:BUFFER-FIELD("Db%"):BUFFER-VALUE = 0.

/* Oppdaterer KORdreHode totaler. */

IF KOrdreHode.Opphav = 10 THEN 
DO FOR bufKOrdreHode:
    FIND bufKOrdreHode EXCLUSIVE-LOCK WHERE 
        RECID(bufKORdreHode) = RECID(KORdreHode) NO-ERROR.
    IF AVAILABLE bufKOrdreHode THEN 
    DO:
        ASSIGN 
            bufKOrdreHode.Totalt = 0
            bufKOrdrEHode.Mva    = 0
            .
        FOR EACH KOrdreLinje OF bufKOrdreHode NO-LOCK: 
            FIND ArtBas NO-LOCK WHERE 
                ArtBas.ArtikkelNr = DEC(KOrdreLinje.VareNr) NO-ERROR.
            IF AVAILABLE ArtBas THEN 
                ASSIGN 
                    bufKOrdreHode.Totalt = bufKOrdreHode.Totalt + KOrdreLinje.NettoLinjesum
                    bufKOrdrEHode.Mva    = bufKOrdrEHode.Mva    + KOrdreLinje.MvaKr        
                    .    
        END.
        FIND CURRENT bufKOrdreHode NO-LOCK.
        
        FIND FIRST KOrdreLinje OF bufKOrdrEHode EXCLUSIVE-LOCK WHERE 
            KOrdreLinje.VareNr = 'BETALT' NO-ERROR.
        IF AVAILABLE KOrdreLinje THEN 
        DO:
            ASSIGN 
                KOrdreLinje.NettoLinjesum = bufKOrdreHode.Totalt * -1  
                KOrdreLinje.NettoPris     = bufKOrdreHode.Totalt * -1 
                KOrdreLinje.BruttoPris    = bufKOrdreHode.Totalt * -1 
                KOrdreLinje.Pris          = bufKOrdreHode.Totalt * -1 
                KOrdreLinje.Linjesum      = bufKOrdreHode.Totalt * -1 
                .
        END.
        RELEASE bufKOrdreHode.
    END.    
END.