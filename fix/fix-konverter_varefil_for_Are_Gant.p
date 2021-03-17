
DEF VAR cInnFil AS CHAR NO-UNDO.
DEF VAR cUtFil  AS CHAR NO-UNDO.
DEF VAR cRecord AS CHAR FORMAT "x(40)" NO-UNDO.

DEF VAR cLevKod AS CHAR FORMAT "x(20)" NO-UNDO.
DEF VAR cBeskr  AS CHAR FORMAT "x(40)" NO-UNDO.
DEF VAR cLevFargKod AS CHAR FORMAT "x(20)" NO-UNDO.
DEF VAR cFarge  AS CHAR FORMAT "x(20)" NO-UNDO.
DEF VAR cStorl  AS CHAR NO-UNDO.
DEF VAR cEan    AS CHAR FORMAT "x(15)" NO-UNDO.
DEF VAR cGNVpi  AS CHAR NO-UNDO.
DEF VAR cWrk AS CHAR NO-UNDO.
DEF VAR cHeader AS CHAR NO-UNDO.
DEF VAR cTekst  AS CHAR NO-UNDO.

DEF STREAM Inn.
DEF STREAM Ut.

CURRENT-WINDOW:WIDTH = 350.

ASSIGN
    cHeader = 'LevModellNr;EANnr;VareTekst;FargeKode;Str1;Varemerke;VeilPris og  MarkedsPris;LevPrisEngros og nettoForh;VareGruppe;Sesong;forhRab%;Leverandør nr'
    cGNVpi  = ';;;;;;;;;;;'
    cInnFil = 'C:\tmp\tn\GantVpi08092017\Kopi-eankoder F16.csv'
    cUtFil  = REPLACE(cInnFil,'Kopi','GNVPIKopi')
    .

INPUT STREAM Inn FROM VALUE(cInnFil) NO-ECHO.
OUTPUT STREAM Ut TO VALUE(cUtFil) NO-ECHO.
    
PUT STREAM Ut UNFORMATTED
        cHeader
        SKIP.

REPEAT:
    IMPORT STREAM inn UNFORMATTED
        cRecord.
    IF cRecord BEGINS 'Item' THEN
        NEXT.
    IF TRIM(cRecord) = '' THEN
        NEXT.
    IF NUM-ENTRIES(cRecord,';') < 7 THEN
        NEXT.

    ASSIGN
        cWrk        = cGNVpi
        cLevKod     = TRIM(ENTRY(1,cRecord,';'))
        cBeskr      = TRIM(ENTRY(2,cRecord,';'))
        cTekst      = TRIM(ENTRY(3,cRecord,';'))
        cLevFargKod = TRIM(ENTRY(4,cRecord,';'))
        cFarge      = TRIM(ENTRY(5,cRecord,';'))
        cStorl      = TRIM(ENTRY(6,cRecord,';'))        
        cEan        = TRIM(ENTRY(7,cRecord,';'))
        NO-ERROR.
    IF ERROR-STATUS:ERROR THEN
    DO:
        MESSAGE cRecord
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        NEXT.
    END.
    RUN bibl_chkean.p (INPUT-OUTPUT cEAN).

    FIND Strekkode NO-LOCK WHERE 
        Strekkode.Kode = cEan NO-ERROR.
    IF AVAILABLE Strekkode THEN
    DO:
        FIND ArtBas OF Strekkode NO-LOCK NO-ERROR.
        FIND FIRST ArtPris NO-LOCK WHERE
             ArtPris.ArtikkelNr = Strekkode.ArtikkelNr AND
             ArtPris.ProfilNr   = 1 NO-ERROR.
         IF AVAILABLE ArtBas THEN
         DO:
             ASSIGN
                 ENTRY( 1,cWrk,';') = cLevKod
                 ENTRY( 2,cWrk,';') = cEan
                 ENTRY( 3,cWrk,';') = cBeskr
                 ENTRY( 4,cWrk,';') = (IF cTekst <> '' THEN cTekst ELSE ArtBas.LevFargKod)
                 ENTRY( 5,cWrk,';') = cStorl
                 ENTRY( 6,cWrk,';') = STRING(ArtBas.VmId)
                 ENTRY( 7,cWrk,';') = STRING(ArtBas.AnbefaltPris)
                 ENTRY( 8,cWrk,';') = STRING(ArtPris.InnkjopsPris[1])
                 ENTRY( 9,cWrk,';') = STRING(ArtBas.Vg)
                 ENTRY(10,cWrk,';') = STRING(ArtBas.Sasong)
                 ENTRY(11,cWrk,';') = ''
                 ENTRY(12,cWrk,';') = STRING(ArtBas.LevNr)
                 .
             PUT STREAM Ut UNFORMATTED
                 cWrk
                 SKIP.
         END.
    END.
    ELSE DO:
        ASSIGN
            ENTRY( 1,cWrk,';') = cLevKod
            ENTRY( 2,cWrk,';') = cEan
            ENTRY( 3,cWrk,';') = cBeskr
            ENTRY( 4,cWrk,';') = (IF cTekst <> '' THEN cTekst ELSE cFarge)
            ENTRY( 5,cWrk,';') = cStorl
            ENTRY( 6,cWrk,';') = ''
            ENTRY( 7,cWrk,';') = ''
            ENTRY( 8,cWrk,';') = ''
            ENTRY( 9,cWrk,';') = ''
            ENTRY(10,cWrk,';') = ''
            ENTRY(11,cWrk,';') = ''
            ENTRY(12,cWrk,';') = ''
            .
        PUT STREAM Ut UNFORMATTED
            cWrk
            SKIP.
    END.
    /*
    DISPLAY
        cLevKod     
        cBeskr      
        cLevFargKod 
        cFarge      
        cStorl              
        cEan        
    WITH WIDTH 350.
    */
END.
OUTPUT STREAM Ut CLOSE.
INPUT STREAM Inn CLOSE.
