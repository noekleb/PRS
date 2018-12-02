/* Sett utpris på artikler
   Parameter:  pris
   Opprettet: 01.04.08 av brynjar@chemistry.no              
-----------------------------------------------------------------------------------*/
DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.

DEF VAR hQuery AS HANDLE NO-UNDO.

CREATE QUERY hQuery.
hQuery:SET-BUFFERS(ihBuffer).
hQuery:QUERY-PREPARE("FOR EACH " + ihBuffer:NAME + " WHERE MottaksId = 0").
hQuery:QUERY-OPEN().

hQuery:GET-FIRST().

REPEAT WHILE NOT hQuery:QUERY-OFF-END:

  FIND PkSdlPris EXCLUSIVE-LOCK 
       WHERE ROWID(PkSdlPris) = TO-ROWID(STRING(ihBuffer:BUFFER-FIELD("RowIdent1"):BUFFER-VALUE))
       NO-ERROR.
  IF NOT AVAIL PkSdlPris THEN DO:
    ocReturn = "Pakkseddel-pris ikke tilgjengelig for oppdatering".
    UNDO, LEAVE.
  END.
  PkSdlPris.NyPris = DECIMAL(icParam).
  PkSdlPris.OverstyrPris = PkSdlPris.NyPris NE PkSdlPris.Pris OR
                           PkSdlPris.NyVarekost NE PkSdlPris.VareKost
                           .
     
  hQuery:GET-NEXT().
END.

DELETE OBJECT hQuery NO-ERROR.

obOk = ocReturn = "".
