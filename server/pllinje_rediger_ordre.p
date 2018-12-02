/* Nullstill bekreftet antall for plukkliste-linje
   Parametere: Buffer for query
   
   Opprettet: 21.08.09 av BHa                  
-----------------------------------------------------------------------------------*/
DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.

DEF VAR hQuery AS HANDLE NO-UNDO.
DEFINE VARIABLE lplListeId AS DECIMAL NO-UNDO.

CREATE QUERY hQuery.
hQuery:SET-BUFFERS(ihBuffer).
hQuery:QUERY-PREPARE("FOR EACH " + ihBuffer:NAME).
hQuery:QUERY-OPEN().
hQuery:GET-FIRST().

LOOPEN:
REPEAT WHILE NOT hQuery:QUERY-OFF-END:
  FIND PlListeLinje EXCLUSIVE-LOCK WHERE 
      ROWID(PlListeLinje) = TO-ROWID(STRING(ihBuffer:BUFFER-FIELD("RowIdent1"):BUFFER-VALUE))
      NO-WAIT NO-ERROR.
  IF AVAIL PlListeLinje THEN DO:
    FIND plListeHode NO-LOCK OF plListeLinje NO-ERROR.
    IF AVAILABLE PlListeHode THEN
    DO:
        lplListeId = plListeLinje.plListeId.
        LEAVE LOOPEN.
    END.
  END.
  LEAVE LOOPEN.
END. /* LOOPEN */

FIND plListeHode NO-LOCK WHERE
  plListeHode.PlListeId = lplListeId NO-ERROR. 

IF NOT AVAILABLE plListeHode THEN 
DO:
    ocReturn = 'Ukjent ordre.'.
    obOk = ocReturn = "".
    RETURN.
END.

IF plListeHode.plListeStatus > 1 THEN 
DO:
    ocReturn = 'Ordrenstatus er endret tidligere.'.
    obOk = ocReturn = "".
    RETURN.
END.

DO TRANSACTION:
    FIND CURRENT plListeHode EXCLUSIVE-LOCK.
    ASSIGN
      PlListeHode.OverfortDato  = TODAY
      plListeHode.plListeStatus = 20
      lplListeId                = plListeLinje.plListeId.
    RELEASE plListeHode.
END. /* TRANSACTION */

obOk = ocReturn = "".
/* **********************  Internal Procedures  *********************** */


/* ************************  Function Implementations ***************** */



