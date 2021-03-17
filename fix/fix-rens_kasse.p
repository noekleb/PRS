/*
176 24777371521 620 "Duraflex 300M sufix" no 0 1 0 189 no 25 yes "DURAFLEX 300M SUFIX" 35.75 ? 0 0 no 0 no ? "" ? "" 0 no ? "" ? "" 0 "" "" 0 no no 0 "B000000.jpg" no 0 0 1 2 0 0 2001074 "6909" 0 620003000010 0 no
176 24777371545 620 "Duraflex 300M sufix" no 0 1 0 189 no 25 yes "DURAFLEX 300M SUFIX" 35.75 ? 0 0 no 0 no ? "" ? "" 0 no ? "" ? "" 0 "" "" 0 no no 0 "B000000.jpg" no 0 0 1 2 0 0 2001075 "6909" 0 620003200010 0 no
*/

CURRENT-WINDOW:WIDTH = 300.

DEF VAR cInnFil AS CHAR NO-UNDO.
DEF VAR cUtFil  AS CHAR NO-UNDO.

DEF VAR cButNr AS CHAR NO-UNDO.
DEF VAR cStat  AS CHAR NO-UNDO.

DEF VAR cEAN   AS CHAR NO-UNDO.
DEF VAR c2EAN  AS CHAR NO-UNDO.
DEF VAR cLinje AS CHAR NO-UNDO.
DEF VAR cOrgLinje AS CHAR NO-UNDO.
DEF VAR lDec   AS DEC  NO-UNDO.

ASSIGN 
    cOrgLinje = '620 "Varetekst" no 0 9 0 189 no 25 yes "Varetekst" 35.75 ? 0 0 no 0 no ? "" ? "" 0 no ? "" ? "" 0 "" "" 0 no no 0 " " no 0 0 1 2 0 0 0 "0" 0 0 0 no'
    cButNr    = '176'
    cInnFil   = 'C:\ArkivDokument\Kunder\MxSport\plufile.txt'
    cUtFil    = 'C:\ArkivDokument\Kunder\MxSport\vare.'
    .
DEF STREAM Inn.
DEF STREAM Ut.

INPUT STREAM Inn FROM VALUE(cInnFil).


REPEAT:
    IMPORT STREAM Inn UNFORMATTED cEAN.
    cEAN = TRIM(cEAN).
    c2EAN = cEAN.
    RUN bibl_chkean (INPUT-OUTPUT c2EAN).
    IF CAN-FIND(Strekkode WHERE
                    Strekkode.Kode = c2EAN) THEN NEXT.
    lDec = 0.
    ASSIGN lDec = DEC(cEAN) NO-ERROR.
    IF DEC(lDec) = 0 THEN NEXT.

    BUTIKK:
    FOR EACH Butiker NO-LOCK WHERE 
        Butiker.harButikksystem = TRUE AND
        Butiker.ApningsDato < TODAY AND
        Butiker.NEdlagtDato = ?:

        cButNr = STRING(Butiker.Butik).
        OUTPUT STREAM Ut TO VALUE(cUtFil + cButNr) APPEND.
        ASSIGN
            cLinje = cButNr + ' ' + cEAN + ' ' + cOrgLinje.

        PUT STREAM Ut UNFORMATTED
            cLinje SKIP.
        OUTPUT STREAM Ut CLOSE.
    END. /* BUTIKK */
END.

INPUT STREAM Inn CLOSE.

