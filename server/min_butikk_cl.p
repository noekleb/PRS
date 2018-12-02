/* Finn CL fra en liste over butikker 
   Parametere:  Butikkliste
   
   Opprettet: 08.05.06 av BHa                  
-----------------------------------------------------------------------------------*/
DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.

DEF VAR ix AS INT NO-UNDO.

DO ix = 1 TO NUM-ENTRIES(icParam):
  IF ocReturn = "" THEN DO:
    FIND Butiker NO-LOCK
         WHERE Butiker.Butik = INT(ENTRY(ix,icParam))
         NO-ERROR.
    IF AVAIL Butiker AND Butiker.clButikkNr NE 0 THEN
      ocReturn = STRING(Butiker.clButikkNr).
  END.
END.
IF ocReturn = "" THEN
  ocReturn = "Listen over tilgjengelige butikker for bruker inneholder ingen referanse til et sentrallager:" + CHR(10) + icParam.
ELSE obOk = YES.
