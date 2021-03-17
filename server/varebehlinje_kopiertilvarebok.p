/* Kopier varehåndteringsbok til varebok
   Parametere:  Vareboknr
   
   Opprettet: 02.05.11 av BHa 
-----------------------------------------------------------------------------------*/
DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.

DEF VAR hQuery AS HANDLE NO-UNDO.

FIND FIRST VareBokHode NO-LOCK
     WHERE VareBokHode.VareBokNr = INTEGER(icParam)
     NO-ERROR.
IF NOT AVAIL VareBokHode THEN DO:
  ocReturn = "Finner ikke varebok " + icParam + CHR(10) + "(Er vareboknr satt for suppleringsboken?)".
  RETURN.
END.

CREATE QUERY hQuery.
hQuery:SET-BUFFERS(ihBuffer).
hQuery:QUERY-PREPARE("FOR EACH " + ihBuffer:NAME).
hQuery:QUERY-OPEN().
hQuery:GET-FIRST().

DO TRANSACTION ON ERROR UNDO, LEAVE:
  REPEAT WHILE NOT hQuery:QUERY-OFF-END:
    FIND FIRST VareBokLinje EXCLUSIVE-LOCK
         WHERE VareBokLinje.VareBokNr = VareBokHode.VareBokNr
           AND VareBokLinje.ArtikkelNr = DECIMAL(ihBuffer:BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE)
         NO-ERROR.
    IF NOT AVAIL VareBokLinje THEN DO:
      CREATE VareBokLinje.
      VareBokLinje.VareBokNr  = VareBokHode.VareBokNr.
    END.
    BUFFER VareBokLinje:HANDLE:BUFFER-COPY(ihBuffer).
    hQuery:GET-NEXT().
  END.
END.

DELETE OBJECT hQuery.

obOK = ocReturn = "".

