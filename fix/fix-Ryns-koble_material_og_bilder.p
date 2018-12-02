/* Kobling av Ryns artikkelregsiter 
   - Materialkode
   - Bildefil
*/

DEF VAR cFilNavn AS CHAR NO-UNDO.
DEF VAR pcLinje AS CHAR NO-UNDO.
DEF VAR cVg AS CHAR NO-UNDO.
DEF VAR cLopNr AS CHAR NO-UNDO.
DEF VAR cTekst AS CHAR NO-UNDO.
DEF VAR lDec AS DEC NO-UNDO.
DEF VAR piAntFeil AS INT NO-UNDO.
DEF VAR iMatKod AS INT NO-UNDO.
DEF VAR iFarg AS INT NO-UNDO.
DEF VAR iLevNr AS INT NO-UNDO.
DEF VAR cBilde AS CHAR NO-UNDO.

CURRENT-WINDOW:WIDTH = 300.

ASSIGN
    cFilNavn = 'C:\Polygon\PRS\kom\in\PRSFargeMaterialBilder.csv'.

DEF STREAM Inn.

INPUT STREAM Inn FROM VALUE(cFilNavn) NO-ECHO.
LESERLINJER:
REPEAT:

    IMPORT STREAM Inn UNFORMATTED pcLinje.

    /* Skipper tomme linjer */
    IF pcLinje = "" THEN
        NEXT LESERLINJER.
        
    /* Skipper overskriftslinje fra mal. */
    IF pcLinje BEGINS "1;2;3;4" THEN
        NEXT LESERLINJER.
    
    /* Skipper overskrift og linjer med feil. */
    ASSIGN lDec = DECIMAL(ENTRY(1,pcLinje,';')) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN 
        NEXT LESERLINJER.

    /* Tomme linjer fra Excel */
    IF pcLinje BEGINS ";;;;;:" THEN
        NEXT LESERLINJER.
    /* OVerskriftsrad 2 */
    IF pcLinje BEGINS "ID" THEN
        NEXT LESERLINJER.

    IF NUM-ENTRIES(pcLinje,";") < 10 THEN
    DO:
      ASSIGN
        piAntFeil = piAntFeil + 1
        .
      NEXT LESERLINJER.
    END.


    ASSIGN
        cTekst = STRING(INT(ENTRY(3,pcLinje,';')),"99999")
        NO-ERROR.
    IF cTekst = ? OR ERROR-STATUS:ERROR THEN
        NEXT.
    ASSIGN lDec = DEC(cTekst) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN
        NEXT.

    ASSIGN
        cVg    = ''
        cLopNr = ''.
    IF INT(cTekst) > 0 AND INT(cTekst) <= 99999 THEN
    DO:
      ASSIGN
          cVg    = SUBSTRING(STRING(INT(cTekst),"99999"),1,2)
          cLopNr = SUBSTRING(STRING(INT(cTekst),"99999"),3,3)
          NO-ERROR.
      IF ERROR-STATUS:ERROR THEN
          NEXT.
    END.
    ASSIGN
        iFarg   = INT(ENTRY(8,pcLinje,';'))
        iMatKod = INT(ENTRY(9,pcLinje,';'))
        NO-ERROR.
    IF ERROR-STATUS:ERROR THEN
        NEXT.

    FIND Farg NO-LOCK WHERE
        Farg.Farg = iFarg.
    FIND Material NO-LOCK WHERE
        Material.MatKod = iMatKod.

    cBilde = LEFT-TRIM(TRIM(ENTRY(10,pcLinje,';')),'\').

    IF (cVg <> '' AND cLopNr <> '') THEN
    FIND ArtBas EXCLUSIVE-LOCK WHERE
        ArtBas.Vg = INT(cVg) AND
        ArtBas.LopNr = INT(cLopNr) NO-ERROR.
    ELSE 
    DO:
        FIND FIRST ArtBas EXCLUSIVE-LOCK WHERE
            ArtBas.LevNr = INT(ENTRY(6,pcLinje,';')) AND
            ArtBas.LevKod = TRIM(ENTRY(7,pcLinje,';')) AND 
            ArtBas.Farg   = iFarg
            NO-ERROR.
        IF ERROR-STATUS:ERROR THEN
            NEXT.
    END.
    
    IF NOT AVAILABLE ArtBas THEN
        NEXT.
    ASSIGN
        ArtBas.VPIBildeKode = cBilde
        ArtBas.MatKod       = iMatKod
        ArtBas.Farg         = iFarg
        ArtBas.LevFargKod   = STRING(iFarg) + '/' + Farg.FarBeskr.


    IF ArtBas.ArtikkelNr = 26991 THEN
    DO:
        DISPLAY
            cTekst
            cVg
            cLopNr
            Farg.Farg
            Farg.FarBeskr
            '|'
            Material.MatKod
            ArtBas.MatKod
            Material.MatBeskr
            AVAILABLE ArtBas
            ArtBas.ArtikkelNr
            ArtBAs.LevKo
            ArtBas.Beskr
            ArtBas.Farg
            ArtBAs.LevFargKod SKIP
            WITH WIDTH 300.
        MESSAGE pcLinje
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
    END.
    IF AVAILABLE ArtBas THEN RELEASE ArtBAs.
    IF AVAILABLE Farg THEN RELEASE Farg.
    IF AVAILABLE Material THEN RELEASE material.


END. /* LESERLINJER */
INPUT STREAM Inn CLOSE.

