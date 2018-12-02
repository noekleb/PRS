/* Kopier artikkel 
   Parametere: Aksjon (new|edit|copy),artikkelnr,prisprofil
               temp-tabell med overstyrte verdier for ny artikkel
   
   Opprettet: 17.11.04 av BHa                  
-----------------------------------------------------------------------------------*/
DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.

DEF VAR fEANSerieId   AS INT    NO-UNDO.
DEF VAR piLoop        AS INT    NO-UNDO.
DEF VAR cKode         AS CHAR   NO-UNDO.

ASSIGN 
    fEANSerieId = int(ENTRY(1,icParam)).

FIND EANNrSerie NO-LOCK WHERE
    EANNrSerie.EANSerieId = fEANSerieId NO-ERROR.
IF NOT AVAILABLE EANNrSerie THEN
DO:
    obOk = FALSE.
    ocReturn = '** Finner ikke EANNrSerie ' + STRING(fEANSerieId) + '.'.
    RETURN.
END.
IF (EANNrSerie.TilEANArtikkelNr <= EANNrSerie.FraEANArtikkelNr) OR 
   (EANNrSerie.TilEANArtikkelNr + EANNrSerie.FraEANArtikkelNr = 0) THEN
DO:
    obOk = FALSE.
    ocReturn = '** Feil angivelse av FRA/TIL artikkelnr.'.
    RETURN.
END.
IF (EANNrSerie.EANType = 0) THEN
DO:
    obOk = FALSE.
    ocReturn = '** Feil EANType.'.
    RETURN.
END.
IF (EANNrSerie.EANLandkode = 0) THEN
DO:
    obOk = FALSE.
    ocReturn = '** Feil Landkode.'.
    RETURN.
END.
/* Oppretter EAN koder for EANNrSerie. */
ELSE DO piLoop = EANNrSerie.FraEANArtikkelNr TO EANNrSerie.TilEANArtikkelNr:
    IF EANNrSerie.EANType = 13 THEN
        ASSIGN
            cKode = STRING(EANNrSerie.EANLandKode,'99') +
                    FILL('0',EANNrSerie.AntSifferILevNr - LENGTH(STRING(EANNrSerie.EANLevNr))) + STRING(EANNrSerie.EANLevNr) +
                    FILL('0',10 - EANNrSerie.AntSifferILevNr - LENGTH(STRING(piLoop))) + STRING(piLoop) +
                    '0'.
    ELSE IF EANNrSerie.EANType = 8 THEN DO:
        IF EANNrSerie.AntSifferILevNr > 0 THEN
            ASSIGN
                cKode = '00000' +
                    STRING(EANNrSerie.EANLandKode,'99') +
                    FILL('0',EANNrSerie.AntSifferILevNr - LENGTH(STRING(EANNrSerie.EANLevNr))) + 
                    STRING(EANNrSerie.EANLevNr) +
                    STRING(piLoop) +
                    '0'.
        ELSE
            ASSIGN
                cKode = '00000' +
                    STRING(EANNrSerie.EANLandKode,'99') +
                    FILL('0',5 - LENGTH(STRING(piLoop))) + STRING(piLoop) +
                    '0'.
    END.

    /* Setter på korrekt sjekksiffer */
    RUN bibl_chkean(INPUT-OUTPUT cKode).

    IF NOT CAN-FIND(FIRST EANNrListe WHERE
                    EANNrListe.EANKode = cKode) THEN
    LOOPEN:
    DO TRANSACTION:
        CREATE EANNrListe.
        ASSIGN
            EANNrListe.EANSErieId = fEANSerieId
            EANNrListe.EANKode    = cKode
            EANNrListe.ArtikkelNr = 0
            .

        /* Sjekker om strekkode er tatt i bruk tidligere */
        FIND Strekkode NO-LOCK WHERE
            Strekkode.Kode = EANNrListe.EANKode NO-ERROR.
        IF AVAILABLE Strekkode THEN
            EANNrListe.ArtikkelNr = Strekkode.ArtikkelNr.

        /* Sjekker i VPI registeret */
        IF EANNrListe.ArtikkelNr = 0 THEN
        VPISJEKK:
        FOR EACH EkstVPILev NO-LOCK WHERE
            EkstVPILev.EkstVPILevNr >= 1 AND
            EkstVPILev.EkstVPILevNr <= 999999:
            IF CAN-FIND(VPIStrekkode WHERE
                        VPISTrekkode.EkstVPILevNr = EkstVPILev.EkstVPILevNr AND
                        VPISTrekkode.Kode         = EANNrListe.EANKode) THEN
            DO:
                FIND VPIStrekkode WHERE
                     VPISTrekkode.EkstVPILevNr = EkstVPILev.EkstVPILevNr AND
                     VPISTrekkode.Kode         = EANNrListe.EANKode NO-ERROR.
                IF AVAILABLE VPISTrekkode THEN
                    EANNrListe.ArtikkelNr = dec(VPIStrekkode.VareNr).
                LEAVE VPISJEKK.
            END.
        END. /* VPISJEKK */
    END. /* LOOPEN */

END.
IF CAN-FIND(FIRST EANNrListe OF EANNrSerie) THEN
DO TRANSACTION:
    FIND CURRENT EANNrSerie EXCLUSIVE-LOCK.
    ASSIGN
        EANNrSerie.EANSerieAktiv = TRUE
        obOK = TRUE.
    FIND CURRENT EANNrSerie NO-LOCK.
END.
