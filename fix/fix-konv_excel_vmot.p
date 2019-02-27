
DEF VAR cFil        AS CHAR NO-UNDO.
DEF VAR cUtFil      AS CHAR NO-UNDO.
DEF VAR cLinje      AS CHAR FORMAT "x(180)" NO-UNDO.
DEF VAR cOrgRecord  AS CHAR NO-UNDO.
DEF VAR cRecord     AS CHAR NO-UNDO.
DEF VAR lArtikkelNr AS DEC  FORMAT ">>>>>>>>>>>>9" NO-UNDO.
DEF VAR iStrKode    AS INT  NO-UNDO.
DEF VAR iButNr      AS INT  NO-UNDO.
DEF VAR cKode       AS CHAR NO-UNDO.
DEF VAR cStorl      AS CHAR NO-UNDO.

DEF STREAM Inn.
DEF STREAM Ut.

CURRENT-WINDOW:WIDTH = 350.


ASSIGN
    iButNr     = 9
    cOrgRecord = "@ArtikkelNr;@Antall;'2014-01-02 06:23:20.936';@ButNr;@EAN;System;;606.54;sone 24;@ButNr;8;123;30;"
    cFil       = 'konv\varetelling_Tønsberg_2017.csv'
    cUtFil     = 'konv\VMOT' + REPLACE(STRING(TODAY,"99/99/9999"),'/','')  + '.' + STRING(iButNr)
    .

INPUT STREAM Inn FROM VALUE(cFil).
OUTPUT STREAM Ut TO VALUE(cUtFil).
REPEAT:
    IMPORT STREAM Inn UNFORMATTED
        cLinje.

    IF cLinje BEGINS '1;2;3;4;5;6;7;8;9;10' THEN NEXT.
    IF cLinje BEGINS 'Butikk;Artikkelnr;Vg/' THEN NEXT.
    IF TRIM(cLinje) = '' THEN NEXT.
    
    IF AVAILABLE Strekkode THEN
        RELEASE Strekkode.

    ASSIGN
        lArtikkelNr = DEC(ENTRY(2,cLinje,';'))
        cStorl      = TRIM(ENTRY(7,cLinje,';'))
        .
    FIND FIRST StrKonv NO-LOCK WHERE
        TRIM(StrKonv.Storl) = cStorl NO-ERROR. 
    IF AVAILABLE StrKonv THEN
        FIND FIRST Strekkode NO-LOCK WHERE
            Strekkode.ArtikkelNr = lArtikkelNr AND
            Strekkode.StrKode    = StrKonv.StrKode NO-ERROR. 

    /*
    DISPLAY
        lArtikkelNr
        cStorl
        ENTRY(9,cLinje,';') 
        '|'
        '*Ukjent' WHEN NOT AVAILABLE Strekkode
        Strekkode.Kode WHEN AVAILABLE Strekkode
        ENTRY(2,cLinje,';')
        ENTRY(7,cLinje,';') 
        cLinje
    WITH WIDTH 350.
    */

    IF AVAILABLE Strekkode THEN
    DO:
        cRecord = REPLACE(cOrgRecord,'@ButNr',STRING(iButNr)).
        cRecord = REPLACE(cRecord,'@EAN',Strekkode.Kode).
        cRecord = REPLACE(cRecord,'@ArtikkelNr',STRING(lArtikkelNr)).
        cRecord = REPLACE(cRecord,'@Antall',ENTRY(9,cLinje,';')).

        PUT STREAM Ut UNFORMATTED
            cRecord
            SKIP.
    END.
END.
OUTPUT STREAM Ut CLOSE.
INPUT STREAM inn CLOSE.


