/* Registrer Slett tellelinje record
   Parameter:  
   Opprettet: 25.11.2007             
-----------------------------------------------------------------------------------*/
DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.


DEFINE VARIABLE hQuery     AS HANDLE  NO-UNDO.
DEFINE VARIABLE dVVarekost AS DECIMAL NO-UNDO.
DEFINE VARIABLE iCl        AS INTEGER NO-UNDO.

DEFINE BUFFER clButiker FOR Butiker.

{syspara.i 5 1 1 iCL INT}
FIND clButiker NO-LOCK WHERE
  clButiker.Butik = iCL NO-ERROR.
IF NOT AVAILABLE clButiker THEN 
  DO:
    ASSIGN 
      ocReturn = 'Ukjent sentrallager (' + STRING(iCL) + ').'
      obOk     = FALSE. 
    RETURN.  
  END.

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
    KORR_LINJE:
    DO:
        FIND ArtBas NO-LOCK WHERE
          ArtBas.ArtikkelNr = TelleLinje.ArtikkelNr NO-ERROR.
        IF NOT AVAILABLE ArtBas THEN 
          LEAVE KORR_LINJE.
        FIND Lager NO-LOCK WHERE
          Lager.ArtikkelNr = ArtBas.ArtikkelNr AND
          Lager.Butik      = TelleLinje.Butik NO-ERROR.
        IF AVAILABLE Lager THEN
          dVVAreKost = Lager.VVareKost.
        IF dVVareKost <= 0 OR dVVareKost = ? THEN 
          DO:
            IF AVAILABLE ArtPris THEN RELEASE ArtPris.
            FIND Butiker NO-LOCK WHERE
              Butiker.Butik = Tellelinje.Butik NO-ERROR.
            IF AVAILABLE Butiker THEN 
              FIND ArtPris OF ArtBas NO-LOCK WHERE
                ArtPris.ProfilNr = Butiker.ProfilNr NO-ERROR.
            IF NOT AVAILABLE ArtPris THEN 
              FIND ArtPris OF ArtBas NO-LOCK WHERE 
                ArtPris.ProfilNr = clButiker.ProfilNr NO-ERROR.
            IF AVAILABLE ArtPris 
              THEN dVVAreKost = ArtPris.VareKost[1].
            IF dVVareKost <= 0 OR dVVareKost = ? 
              THEN dVVareKost = 0.
          END.
    
        ASSIGN
            TelleLinje.VVarekost  = dVVarekost
            Tellelinje.OpprVerdi  = TelleLinje.AntallPar  * TelleLinje.VVareKost
            TelleLinje.OpptVerdi  = TelleLinje.AntallTalt * TelleLinje.VVareKost
            TelleLinje.VerdiDiff  = TelleLinje.AntallDiff * TelleLinje.VVareKost
            NO-ERROR.
      obOk = NOT ERROR-STATUS:ERROR.
      IF NOT obOk THEN
      DO:
        ocReturn = ERROR-STATUS:GET-MESSAGE(1).
        LEAVE.
      END.
    END. /* KORR_LINJE */
  END.
  IF AVAIL TelleLinje THEN RELEASE Tellelinje.
  hQuery:GET-NEXT().
END.

