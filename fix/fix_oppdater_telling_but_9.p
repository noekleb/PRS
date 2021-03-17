

DEF VAR iTelleNr AS INT NO-UNDO.
DEF VAR iLevNr AS INT NO-UNDO.
DEF VAR iButNr AS INT NO-UNDO.
DEF VAR cRecord AS CHAR NO-UNDO.
DEF VAR iAntTalt AS INT NO-UNDO.
DEF VAR lArtikkelNr AS DEC FORMAT ">>>>>>>>>>>>>9" NO-UNDO.
DEF VAR cStorl AS CHAR NO-UNDO.

DEF VAR cFil AS CHAR NO-UNDO.

DEF STREAM Inn.

CURRENT-WINDOW:WIDTH = 350.

ASSIGN 
    iLevNr   = 99 /* Leverandør hvor tellelinjer ikke skal røres. */
    itelleNr = 29029
    ibutNr   = 9
    cFil     = 'konv\GantTellefil\Varetellingbut9september2019.csv'
    .


FIND TelleHode NO-LOCK WHERE 
    TelleHode.TelleNr = itelleNr AND 
    TelleHode.ButikkListe = STRING(iButNr) NO-ERROR.
IF TelleHode.Oppdatert <> ? THEN
DO:
  MESSAGE 'Telleliste er allerede oppdatert. Kan ikke endres. '
      VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
  RETURN.
END.

INPUT STREAM Inn FROM VALUE(cFil).
REPEAT:
    IMPORT STREAM Inn UNFORMATTED
        cRecord.
    ASSIGN 
        lArtikkelNr = DEC(ENTRY(2,cRecord,';'))
        iAntTalt = INT(ENTRY(9,cRecord,';'))
        cStorl   = TRIM(ENTRY(7,cRecord,';'))
        NO-ERROR.
    IF ERROR-STATUS:ERROR THEN
        NEXT.
    FIND FIRST TelleLinje EXCLUSIVE-LOCK WHERE 
        TelleLinje.TelleNr = TelleHode.TelleNr AND 
        TelleLinje.ArtikkelNr = lArtikkelNr AND 
        TelleLinje.butik = iButNr AND 
        TRIM(TelleLinje.Storl) = cStorl NO-ERROR.

    IF AVAILABLE TelleLinje THEN 
        FIND ArtBas NO-LOCK WHERE 
            ArtBas.ArtikkelNr = TelleLinje.ArtikkelNr NO-ERROR.
    /* Skal ikke med Lev 99 */
    IF AVAILABLE ArtBas AND ArtBas.LevNr = iLevNr THEN
        NEXT.
    IF AVAILABLE TelleLinje THEN
    DO:
        IF (TelleLinje.AntallTalt - iAntTalt) <> 0 THEN
            DISPLAY
                cRecord 
                lArtikkelNr
                ArtBas.LevNr WHEN AVAILABLE ArtBas
                iAntTalt
                cStorl
                '|'
                TelleLinje.VVareKost WHEN AVAILABLE TelleLinje
                TelleLinje.Storl WHEN AVAILABLE TelleLinje
                TelleLinje.AntallTalt WHEN AVAILABLE TelleLinje
                (TelleLinje.AntallTalt - iAntTalt) WHEN AVAILABLE TelleLinje
            WITH WIDTH 350.
        /* Korriger */
     ASSIGN
        TelleLinje.AntallTalt    = iAntTalt
        TelleLinje.OpptVerdi     = TelleLinje.AntallTalt * TelleLinje.VVarekost
        TelleLinje.AntallDiff    = TelleLinje.AntallPar - TelleLinje.AntallTalt
        TelleLinje.VerdiDiff     = TelleLinje.AntallDiff * TelleLinje.VVareKost
        .                              
    END.
    ELSE DO:
        DISPLAY
            cRecord WITH WIDTH 350.
    END.
END.
INPUT STREAM Inn CLOSE.


