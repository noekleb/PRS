/* Registrer Slett tellelinje record
   Parameter:  
   Opprettet: 25.11.2007             
-----------------------------------------------------------------------------------*/
DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.


DEF VAR hQuery          AS HANDLE NO-UNDO.

CREATE QUERY hQuery.
hQuery:SET-BUFFERS(ihBuffer).
hQuery:QUERY-PREPARE("FOR EACH " + ihBuffer:NAME + " NO-LOCK").
hQuery:QUERY-OPEN().

hQuery:GET-FIRST().
REPEAT WHILE NOT hQuery:QUERY-OFF-END:
  ASSIGN 
    ocReturn = ''
    obOk     = TRUE
  .

  DO TRANSACTION:

    FIND FIRST TelleLinje WHERE TelleLinje.TelleNr    = INT(ihBuffer:BUFFER-FIELD('TelleNr'):BUFFER-VALUE)
                            AND TelleLinje.ArtikkelNr = dec(ihBuffer:BUFFER-FIELD('ArtikkelNr'):BUFFER-VALUE)
                            AND TelleLinje.Butik      = INT(ihBuffer:BUFFER-FIELD('Butik'):BUFFER-VALUE)
                            AND TelleLinje.Storl      = STRING(ihBuffer:BUFFER-FIELD('Storl'):BUFFER-VALUE)
                         EXCLUSIVE-LOCK NO-ERROR.

    IF AVAIL Tellelinje THEN
    DO:
        assign
            TelleLinje.AntallTalt = 0
            TelleLinje.AntallDiff = TelleLinje.AntallPar - TelleLinje.AntallTalt
            TelleLinje.OpptVerdi  = TelleLinje.AntallTalt * TelleLinje.VVareKost
            TelleLinje.VerdiDiff  = TelleLinje.AntallDiff * TelleLinje.VVareKost
            NO-ERROR.
      obOk = NOT ERROR-STATUS:ERROR.
      IF NOT obOk THEN
      DO:
        ocReturn = ERROR-STATUS:GET-MESSAGE(1).
        LEAVE.
      END.
    END.
  END.
  IF AVAIL TelleLinje THEN RELEASE Tellelinje.
  hQuery:GET-NEXT().
END.

