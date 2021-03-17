/* Kjøres etter oppdatering. Se prosedyre MySaveBrowseFillIn i kallende rutine  
   
-----------------------------------------------------------------------------------------*/   
DEF INPUT  PARAM ihBuffer       AS HANDLE NO-UNDO.  /* Handle to current buffer. Her: Ovbuffer */
DEF INPUT  PARAM icAction       AS CHAR   NO-UNDO.  /* Delete, Create or Update */
DEF INPUT  PARAM icSessionId    AS CHAR   NO-UNDO.
DEF OUTPUT PARAM ocValue        AS CHAR   NO-UNDO.  /* Error message. If <> blank the transaction is backed out */

DEF VAR cFields       AS CHAR  NO-UNDO.  /* Last modified field */
DEF VAR cFieldValues  AS CHAR  NO-UNDO.   
DEFINE VARIABLE iLoop AS INTEGER NO-UNDO.
DEFINE VARIABLE cFelt AS CHARACTER NO-UNDO.
DEFINE VARIABLE cVerdi AS CHARACTER NO-UNDO.

cFields      = DYNAMIC-FUNCTION("getCurrentValueFields" IN SOURCE-PROCEDURE).
cFieldValues = DYNAMIC-FUNCTION("getCurrentValues" IN SOURCE-PROCEDURE).

IF icAction = 'DELETE' THEN 
  RETURN.
  
IF NOT ihBuffer:AVAIL OR cFields = '' THEN 
DO:
  ocValue = '**Finner ikke bokføringsbilaget, eller ingen endring er gjort.'.
END.
ELSE DO:
  FIND LAST BokforingsVisning NO-LOCK WHERE
    BokforingsVisning.bokforingsId = ihBuffer:BUFFER-FIELD("BokforingsId"):BUFFER-VALUE AND  
    BokforingsVisning.Tekst BEGINS 'Kasse diff' NO-ERROR.
  IF AVAILABLE BokforingsVisning AND DEC(BokforingsVisning.Belop) <> 0 THEN 
  DO:
    ocValue = '**Det er avvik på oppgjøret. Avvik må være lik 0 for at bokføringsbilaget skal kunne godkjennes.'.
    RETURN. 
  END. 
  
  DO iLoop = 1 TO NUM-ENTRIES(cFields):
    ASSIGN 
      cFelt  = ENTRY(iLoop,cFields)
      cVerdi = ENTRY(iLoop,cFieldValues,'|')
      .
    CASE cFelt:
      WHEN 'GodkjentDato'  THEN ihBuffer:BUFFER-FIELD("GodkjentDato"):BUFFER-VALUE = DATE(cVerdi).
      WHEN 'GodkjentTid'   THEN ihBuffer:BUFFER-FIELD("GodkjentTid"):BUFFER-VALUE = INT(cVerdi).
      WHEN 'GodkjentAv'    THEN ihBuffer:BUFFER-FIELD("GodkjentAv"):BUFFER-VALUE = cVerdi.
      WHEN 'GodkjentFlagg' THEN ihBuffer:BUFFER-FIELD("GodkjentFlagg"):BUFFER-VALUE = (IF cVerdi = 'TRUE' THEN TRUE ELSE FALSE).
      
    END CASE.    
    /* Her er bilaget godkjent selv om ikke EOD er kommet inn. */
    IF ihBuffer:BUFFER-FIELD("GodkjentFlagg"):BUFFER-VALUE = TRUE AND ihBuffer:BUFFER-FIELD("EODMottatt"):BUFFER-VALUE = FALSE THEN 
    DO:
      ASSIGN 
        ihBuffer:BUFFER-FIELD("EODMottatt"):BUFFER-VALUE        = TRUE
        ihBuffer:BUFFER-FIELD("EODDato"):BUFFER-VALUE           = TODAY 
        ihBuffer:BUFFER-FIELD("EODDatoTidMottatt"):BUFFER-VALUE = NOW
        .
      CREATE BokforingsKorrBilag.
      ASSIGN 
        BokforingsKorrBilag.BokforingsID = ihBuffer:BUFFER-FIELD("BokforingsId"):BUFFER-VALUE
        BokforingsKorrBilag.TTId         = 144
        BokforingsKorrBilag.TBId         = 1
        BokforingsKorrBilag.Belop        = 0
        BokforingsKorrBilag.Merknad      = 'Godkjent uten at EOD var mottatt'
        BokforingsKorrBilag.BrukerId     = ihBuffer:BUFFER-FIELD("GodkjentAv"):BUFFER-VALUE
        BokforingsKorrBilag.EBrukerId    = ihBuffer:BUFFER-FIELD("GodkjentAv"):BUFFER-VALUE
        .
    END.
  END.
END.
