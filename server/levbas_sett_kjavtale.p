/* Sett flagg..
   Parameter:  0 eller "antlevert"
   Opprettet: 09.08.07 av BHa              
-----------------------------------------------------------------------------------*/
DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.

DEF VAR hQuery AS HANDLE NO-UNDO.

CREATE QUERY hQuery.
hQuery:SET-BUFFERS(ihBuffer).
hQuery:QUERY-PREPARE("FOR EACH " + ihBuffer:NAME).
hQuery:QUERY-OPEN().

hQuery:GET-FIRST().

REPEAT WHILE NOT hQuery:QUERY-OFF-END:

  FIND LevBas EXCLUSIVE-LOCK 
       WHERE ROWID(LevBas) = TO-ROWID(STRING(ihBuffer:BUFFER-FIELD("RowIdent1"):BUFFER-VALUE))
       NO-ERROR.
  IF NOT AVAIL LevBas THEN DO:
    ocReturn = "Pakkseddel-linje ikke tilgjengelig for oppdatering".
    UNDO, LEAVE.
  END.
  LevBas.KjedeAvtale = LOGICAL(icParam).
  hQuery:GET-NEXT().
END.

DELETE OBJECT hQuery NO-ERROR.

obOk = ocReturn = "".
