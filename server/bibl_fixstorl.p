/* jukebox/server/template/runproc usage.p
   Template for server procedure invoked by the "runproc" function:
   DYNAMIC-FUNCTION("runproc","myproc.p","<parameter string>",temp-table handle).
   
   If a temp-table is passed from the client the corresponding default buffer is input param here.
   Cleanup for buffer and TT is taken care of by the parent of this procedure which is jbserv_runproc.p
   
   Similar procedure can be used for the ProcessQuery method

  run bibl_fixstorl.p ('XL',?,'',output ocReturn,output obOk).

-----------------------------------------------------------------------------------*/
DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.

DEF VAR wStorl AS CHAR NO-UNDO.

DEF VAR wDecimaler AS CHAR NO-UNDO.

{syspara.i 1 1 16 wDecimaler}

ASSIGN
   wStorl = ENTRY(1,icParam,"|")
   wStorl = trim(wStorl)
   wStorl = caps(wStorl)
   wStorl = if (length(wStorl) = 1 OR
                length(wStorl) = 3
                )
               then " " + wStorl
               else wStorl.

/* Bytter ut eventuelle comma med punkt. */
IF index(wStorl,",") <> 0 THEN
  OVERLAY(wStorl, index(wStorl,","), 1, "CHARACTER") = ".".

/* Sjekker om det er benyttet gyldige tegn i halvnummer. */
/* Er det ikke det, tas halvnummeret bort.               */
/* TN 14/4-11 Skal ikke gjøres lenger.
IF NUM-ENTRIES(wStorl,".") = 2 THEN
  DO:
    IF NOT CAN-DO(wDecimaler,ENTRY(2,wStorl,".")) THEN
      wStorl = ENTRY(1,wStorl,".").
  END.
*/

ASSIGN
    ocReturn = wStorl
    obOk     = TRUE.

