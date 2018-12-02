CURRENT-WINDOW:WIDTH = 300.

DEF VAR cLinje AS CHAR NO-UNDO.
DEF VAR iLoop AS INT NO-UNDO.
DEF VAR cFilNavn AS CHAR NO-UNDO.
DEF VAR cUtFil   AS CHAR NO-UNDO.
DEF VAR cEAN AS CHAR FORMAT "x(20)" NO-UNDO.
DEF VAR cVVareKost AS CHAR NO-UNDO.
DEF VAR lVVareKost AS DEC FORMAT "->>>>>>>>>9.99" NO-UNDO.
DEF VAR iButikkNr AS INT NO-UNDO.
DEF VAR cNavn AS CHAR NO-UNDO.
DEF VAR iAnt AS INT NO-UNDO.
DEF VAR cTelleLinje AS CHAR NO-UNDO.
DEF VAR cTellefil AS CHAR NO-UNDO.
DEF VAR cAnders AS CHAR NO-UNDO.
DEF VAR cArtikkelNr AS CHAR NO-UNDO.

ASSIGN
    cNavn     = 'HasleLagerverdiEtterTellin_Anders'
    cFilNavn  = 'c:\home\lindbak\ankommet\' + cNavn + '.csv'
    cUtfil    = 'c:\home\lindbak\ankommet\' + cNavn + '_log.csv'
    cTellefil = 'c:\home\lindbak\ankommet\varetranHasle.550'
    cAnders   = 'c:\home\lindbak\ankommet\' + cNavn + 'Anders.csv'
    iButikkNr = 550
    .
DEF STREAM Inn.
DEF STREAM Ut.
DEF STREAM TelleFil.
DEF STREAM Anders.

INPUT STREAM Inn FROM VALUE(cFilNavn) NO-ECHO. 
OUTPUT STREAM Ut TO VALUE(cUtFil) NO-ECHO.
OUTPUT STREAM Tellefil TO VALUE(cTelleFil) NO-ECHO.
OUTPUT STREAM Anders TO VALUE(cAnders) NO-ECHO.

LOOPEN:
REPEAT :
    IMPORT STREAM Inn UNFORMATTED cLinje .

    /*
    IF NUM-ENTRIES(cLinje,';') <> 2 THEN
    DO:
        RUN ForKast.
        NEXT LOOPEN.
    END.
    */
    IF TRIM(ENTRY(3,cLinje,';')) = '' THEN
    DO:
        RUN ForKast.
        NEXT LOOPEN.
    END.

    ASSIGN
        cEAN       = TRIM(ENTRY(3,cLinje,';'))
        cVVareKost = TRIM(ENTRY(23,cLinje,';'))
        .
    ASSIGN
        lVVAreKost = ROUND(DEC(cVVareKost),2)         
        NO-ERROR.
    IF ERROR-STATUS:ERROR OR lVVareKost <= 0 THEN
    DO:
        lVVAreKost = 0.
        /*
        RUN ForKast.
        NEXT LOOPEN.
        */
    END.

    RUN bibl_chkean(INPUT-OUTPUT cEAN).
    FIND Strekkode NO-LOCK WHERE
        Strekkode.Kode = cEAN NO-ERROR.
    IF NOT AVAILABLE Strekkode THEN
    DO:
        RUN ForKast.
        NEXT LOOPEN.
    END.
    FIND ArtBas OF Strekkode NO-LOCK NO-ERROR.
    IF NOT AVAILABLE ArtBas THEN
    DO:
        RUN ForKast.
        NEXT LOOPEN.
    END.

    cArtikkelNr = STRING(ArtBAs.ArtikkelNr).

    DO FOR Lager TRANSACTION:
        FIND Lager EXCLUSIVE-LOCK WHERE
            Lager.ArtikkelNr = ArtBas.ArtikkelNr AND
            Lager.Butik      = iButikkNr NO-ERROR.
        IF NOT AVAILABLE Lager THEN
        DO:
            CREATE Lager.
            ASSIGN
                Lager.ArtikkelNr = ArtBas.ArtikkelNr 
                Lager.Butik      = iButikkNr 
                Lager.VVareKost  = lVVareKost
                .
            RELEASE Lager.
        END.
        ELSE Lager.VVareKost  = lVVareKost.
        iAnt = iant + 1.

        ASSIGN
            cTelleLinje = '550 ' +
                           cEAN + ' ' +
                           '12/06/22 ' +
                           '0 ' + 
                           '0 ' +
                           '7 ' + 
                           'Tekst ' + /*'"' + ENTRY(5,cLinje,';') + '" ' +*/
                           'Gurre ' + 
                           ENTRY(8,cLinje,';') + ' ' +
                           REPLACE(STRING(lVVAreKost),',','.') + ' ' +
                           '0 ' + 
                           '0 ' + 
                           '0 ' + 
                           '0 ' + 
                           '0 ' + 
                           '0 ' + 
                           '0 ' 
                           .
        PUT STREAM TelleFil UNFORMATTED cTelleLinje SKIP.

        PUT STREAM Anders UNFORMATTED 
            cArtikkelNr ';'
            cLinje SKIP.
    END. /* TRANSACTION */

    /*
    DISPLAY
        cLinje FORMAT "x(60)"
        cEAN
        cVVareKost
        lVVareKost
        WITH WIDTH 300.
    */
END. /* LOOPEN */

OUTPUT STREAM TelleFil CLOSE.
OUTPUT STREAM Ut CLOSE.
INPUT STREAM Inn CLOSE.

MESSAGE iant
    VIEW-AS ALERT-BOX INFO BUTTONS OK.

PROCEDURE ForKast:
    PUT STREAM Ut UNFORMATTED
        cLinje SKIP.
END.
