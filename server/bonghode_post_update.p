/* Kjøres etter oppdatering merknad. Se prosedyre MySaveBrowseFillIn i InnUtloggingLager.w  
   
-----------------------------------------------------------------------------------------*/   
DEF INPUT  PARAM ihBuffer       AS HANDLE NO-UNDO.  /* Handle to current buffer. Her: Bonghode */
DEF INPUT  PARAM icAction       AS CHAR   NO-UNDO.  /* Create or Update */
DEF INPUT  PARAM icSessionId    AS CHAR   NO-UNDO.
DEF OUTPUT PARAM ocValue        AS CHAR   NO-UNDO.  /* Error message. If <> blank the transaction is backed out */

DEF VAR cField        AS CHAR  NO-UNDO.  /* Last modified field */
DEF VAR cFieldParam   AS CHAR  NO-UNDO.   
DEFINE VARIABLE cLogg AS CHARACTER NO-UNDO.

ASSIGN 
  cLogg = 'bonghode_post_update' + REPLACE(STRING(TODAY),'/','')
  .  

cField = DYNAMIC-FUNCTION("getCurrentValueFields" IN SOURCE-PROCEDURE) NO-ERROR.
IF NOT ERROR-STATUS:ERROR THEN
  cField = ENTRY(1,cField).

RUN bibl_loggDbFri(cLogg,'cField: ' + cField).
         
cFieldParam = DYNAMIC-FUNCTION("getInputParam" IN SOURCE-PROCEDURE) NO-ERROR.
IF cFieldParam NE "" AND (cField = ? OR cField = "") THEN
  cField = cFieldParam.

RUN bibl_loggDbFri(cLogg,'cFieldParam: ' + cFieldParam).

FIND BongHode WHERE BongHode.b_id = DEC(ihBuffer:BUFFER-FIELD("B_Id"):BUFFER-VALUE) NO-LOCK NO-ERROR.
IF NOT AVAIL BongHode THEN DO:
  ocValue = "Hopp i havet. Bongen mangler :)".
  RUN bibl_loggDbFri(cLogg,ocValue).
  RETURN.
END.
ELSE DO:
  FIND FIRST BongLinje EXCLUSIVE-LOCK WHERE 
    BongLinje.b_id = BongHode.b_id NO-ERROR.

  RUN bibl_loggDbFri(cLogg,'Funnet bonglinje: ' + STRING(AVAILABLE BongLinje)).

  IF AVAILABLE BongLinje THEN 
    DO:
      ASSIGN 
        BongLinje.RefTekst = ihBuffer:BUFFER-FIELD("Logg"):BUFFER-VALUE.
      RELEASE BongLinje.
      FIND CURRENT BongHode EXCLUSIVE-LOCK.
      ASSIGN 
        BongHode.eDato = TODAY
        BongHode.eTid  = TIME
        .
      FIND CURRENT BongHode NO-LOCK.
    END.  
END.

/*IF ihBuffer:BUFFER-FIELD("VareNr"):BUFFER-VALUE = "" THEN                                                                                                                                                                         */
/*  ASSIGN ihBuffer:BUFFER-FIELD("BruttoPris"):BUFFER-VALUE = ihBuffer:BUFFER-FIELD("NettoPris"):BUFFER-VALUE                                                                                                                       */
/*         ihBuffer:BUFFER-FIELD("Pris"):BUFFER-VALUE = ihBuffer:BUFFER-FIELD("NettoPris"):BUFFER-VALUE                                                                                                                             */
/*         ihBuffer:BUFFER-FIELD("VareKost"):BUFFER-VALUE   = ihBuffer:BUFFER-FIELD("BruttoPris"):BUFFER-VALUE * 0.65.                                                                                                              */
/*                                                                                                                                                                                                                                  */
/*ASSIGN fOrdreRabPris = ihBuffer:BUFFER-FIELD("Pris"):BUFFER-VALUE - ihBuffer:BUFFER-FIELD("BruttoPris"):BUFFER-VALUE * KOrdreHode.TotalRabatt% / 100                                                                              */
/*       ihBuffer:BUFFER-FIELD("OrdreRabattKr"):BUFFER-VALUE = (ihBuffer:BUFFER-FIELD("BruttoPris"):BUFFER-VALUE - fOrdreRabPris) * ihBuffer:BUFFER-FIELD("Antall"):BUFFER-VALUE                                                    */
/*       .                                                                                                                                                                                                                          */
/*                                                                                                                                                                                                                                  */
/*IF cField = "LinjeRab%" THEN                                                                                                                                                                                                      */
/*DO:                                                                                                                                                                                                                               */
/*  IF ihBuffer:BUFFER-FIELD("LinjeRab%"):BUFFER-VALUE NE 0 THEN                                                                                                                                                                    */
/*    ihBuffer:BUFFER-FIELD("NettoPris"):BUFFER-VALUE = fOrdreRabPris - fOrdreRabPris * ihBuffer:BUFFER-FIELD("LinjeRab%"):BUFFER-VALUE / 100.                                                                                      */
/*  ELSE                                                                                                                                                                                                                            */
/*    ihBuffer:BUFFER-FIELD("NettoPris"):BUFFER-VALUE = fOrdreRabPris - fOrdreRabPris * ihBuffer:BUFFER-FIELD("KundeRab%"):BUFFER-VALUE / 100.                                                                                      */
/*END.                                                                                                                                                                                                                              */
/*ELSE IF cField = "NettoPris" THEN                                                                                                                                                                                                 */
/*  ASSIGN ihBuffer:BUFFER-FIELD("LinjeRab%"):BUFFER-VALUE = (1 - ihBuffer:BUFFER-FIELD("NettoPris"):BUFFER-VALUE / fOrdreRabPris) * 100                                                                                            */
/*         .                                                                                                                                                                                                                        */
/*                                                                                                                                                                                                                                  */
/*IF ihBuffer:BUFFER-FIELD("LinjeRab%"):BUFFER-VALUE = ? THEN ihBuffer:BUFFER-FIELD("LinjeRab%"):BUFFER-VALUE = 0.                                                                                                                  */
/*                                                                                                                                                                                                                                  */
/*ASSIGN ihBuffer:BUFFER-FIELD("NettoLinjesum"):BUFFER-VALUE = ihBuffer:BUFFER-FIELD("NettoPris"):BUFFER-VALUE * ihBuffer:BUFFER-FIELD("Antall"):BUFFER-VALUE                                                                       */
/*       ihBuffer:BUFFER-FIELD("LinjeRabattKr"):BUFFER-VALUE = (fOrdreRabPris - ihBuffer:BUFFER-FIELD("NettoPris"):BUFFER-VALUE) * ihBuffer:BUFFER-FIELD("Antall"):BUFFER-VALUE                                                     */
/*       ihBuffer:BUFFER-FIELD("KundeRabattKr"):BUFFER-VALUE = (ihBuffer:BUFFER-FIELD("Pris"):BUFFER-VALUE - ihBuffer:BUFFER-FIELD("BruttoPris"):BUFFER-VALUE) * ihBuffer:BUFFER-FIELD("Antall"):BUFFER-VALUE                       */
/*       .                                                                                                                                                                                                                          */
/*                                                                                                                                                                                                                                  */
/*FIND FIRST Moms WHERE Moms.MomsKod = INT(ihBuffer:BUFFER-FIELD("MomsKod"):BUFFER-VALUE) NO-LOCK NO-ERROR.                                                                                                                         */
/*                                                                                                                                                                                                                                  */
/*IF AVAIL Moms THEN                                                                                                                                                                                                                */
/*  ASSIGN ihBuffer:BUFFER-FIELD("Mva%"):BUFFER-VALUE     = Moms.MomsProc                                                                                                                                                           */
/*         ihBuffer:BUFFER-FIELD("MvaKr"):BUFFER-VALUE    = (IF Moms.MomsProc = 0 THEN 0 ELSE ihBuffer:BUFFER-FIELD("NettoLinjeSum"):BUFFER-VALUE - ihBuffer:BUFFER-FIELD("NettoLinjeSum"):BUFFER-VALUE / (1 + Moms.MomsProc / 100))*/
/*         ihBuffer:BUFFER-FIELD("Linjesum"):BUFFER-VALUE = ihBuffer:BUFFER-FIELD("NettoLinjesum"):BUFFER-VALUE + ihBuffer:BUFFER-FIELD("MvaKr"):BUFFER-VALUE                                                                       */
/*         .                                                                                                                                                                                                                        */
/*ELSE                                                                                                                                                                                                                              */
/*  ASSIGN ihBuffer:BUFFER-FIELD("Mva%"):BUFFER-VALUE     = 0                                                                                                                                                                       */
/*         ihBuffer:BUFFER-FIELD("MvaKr"):BUFFER-VALUE    = 0                                                                                                                                                                       */
/*         .                                                                                                                                                                                                                        */
/*                                                                                                                                                                                                                                  */
/*IF ihBuffer:BUFFER-FIELD("VareKost"):BUFFER-VALUE NE 0 THEN                                                                                                                                                                       */
/*  ihBuffer:BUFFER-FIELD("DbKr"):BUFFER-VALUE = ihBuffer:BUFFER-FIELD("NettoLinjeSum"):BUFFER-VALUE -                                                                                                                              */
/*                                               ihBuffer:BUFFER-FIELD("MvaKr"):BUFFER-VALUE -                                                                                                                                      */
/*                                               ihBuffer:BUFFER-FIELD("Varekost"):BUFFER-VALUE * ihBuffer:BUFFER-FIELD("Antall"):BUFFER-VALUE.                                                                                     */
/*ELSE                                                                                                                                                                                                                              */
/*  ihBuffer:BUFFER-FIELD("DbKr"):BUFFER-VALUE = ihBuffer:BUFFER-FIELD("NettoLinjeSum"):BUFFER-VALUE * 0.35.                                                                                                                        */
/*                                                                                                                                                                                                                                  */
/*ihBuffer:BUFFER-FIELD("Db%"):BUFFER-VALUE  = ihBuffer:BUFFER-FIELD("DbKr"):BUFFER-VALUE /                                                                                                                                         */
/*                                            (ihBuffer:BUFFER-FIELD("NettoLinjeSum"):BUFFER-VALUE                                                                                                                                  */
/*                                             - ihBuffer:BUFFER-FIELD("MvaKr"):BUFFER-VALUE) * 100.                                                                                                                                */
/*IF ihBuffer:BUFFER-FIELD("Db%"):BUFFER-VALUE = ? THEN                                                                                                                                                                             */
/*  ihBuffer:BUFFER-FIELD("Db%"):BUFFER-VALUE = 0.                                                                                                                                                                                  */

