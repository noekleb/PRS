/************************************************************
    Program:  ovordre.p
    Created:  TN    8 Dec 99
Description:

    run ovordre.p (input ipModus, output FI-OvOrdreId, output wOk).

Last change:  TN    3 Jan 100   11:55 pm
************************************************************/

DEF INPUT  PARAMETER wModus     as INT  NO-UNDO.
DEF INPUT  PARAMETER wDelimiter as CHAR NO-UNDO.
DEF INPUT  PARAMETER wData      as CHAR NO-UNDO.
DEF OUTPUT PARAMETER wOvOrdreId as DEC  NO-UNDO.
DEF OUTPUT PARAMETER wOk        as CHAR NO-UNDO.

MESSAGE
  "wModus:"      wModus skip
  "wDelimiter:"  wDelimiter skip
  "wData:"       wData skip
  "wOvOrdreId:"  wOvOrdreId skip
  "wOk:"         wOk SKIP
VIEW-AS ALERT-BOX.

/* Default delimiter */
IF wDelimiter = "" then
  wDelimiter = "£".

CASE wModus:
  WHEN 1 THEN RUN NyOvOrdre.
  WHEN 2 THEN RUN EndreOvOrdre.
  WHEN 3 THEN RUN KobleOvOrdre.
  OTHERWISE . /* Gj›r ingenting. */
END CASE.

assign
  wOk = "OK".

/* Oppretter ny OvOrdre. Det eneste som settes er OvOrdreId. */
PROCEDURE NyOvOrdre:
  DEF BUFFER ibOvOrdre FOR OvOrdre.

  do FOR ibOvOrdre TRANSACTION:
    FIND LAST ibOvOrdre USE-INDEX OvOrdre NO-LOCK NO-ERROR.
    IF AVAILABLE ibOvOrdre then
      wOvOrdreId = ibOvOrdre.OvOrdreId + 1.
    ELSE
      wOvOrdreId = 1.
    CREATE ibOvOrdre.
    assign
      ibOvOrdre.OvOrdreId = wOvOrdreId.
    IF wData <> "" then
      assign
        ibOvOrdre.Overfores = if NUM-ENTRIES(wData,wDelimiter) > 0  then  DATE(ENTRY(1,wData,wDelimiter)) ELSE ?
        ibOvOrdre.Merknad   = if NUM-ENTRIES(wData,wDelimiter) > 1  then  ENTRY(2,wData,wDelimiter) ELSE ""
        ibOvOrdre.Notat     = if NUM-ENTRIES(wData,wDelimiter) > 2  then  ENTRY(3,wData,wDelimiter) ELSE "".
  END.

END PROCEDURE.

/* LAgrer endringer i OvORdre */
PROCEDURE EndreOvOrdre:

END procedure.

/* Koble til OvOrdre */
PROCEDURE KobleOvOrdre:

END PROCEDURE.


