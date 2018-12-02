/* fix-utlegg_av_lager_for_butikk.p */

DEF VAR iButikkNr AS INT  NO-UNDO.
DEF VAR cFilNavn  AS CHAR NO-UNDO.
DEF VAR iAnt      AS INT  NO-UNDO.
DEF VAR cStrekkodeListe AS CHAR NO-UNDO.
DEF VAR iStrKode  AS INT  NO-UNDO.

DEF STREAM Ut.


{syspara.i 5 1 1 iButikkNr INT}

UPDATE 
    iButikkNr LABEL 'Angi butikknr:' SKIP
    .
ASSIGN
    cFilNavn = 'Lager_for_butikk.' + STRING(iButikkNr).

OUTPUT STREAM Ut TO VALUE(cFilNavn).

EXPORT STREAM Ut DELIMITER ';'
    'ArtLag.Butik;'
    'ArtBas.ArtikkelNr;'
    'ArtBas.LevKod;'
    'ArtBas.Beskr;'
    'ArtBas.LevFargKod;'
    'ArtLag.StrKode;'
    'StrKonv.Storl;'
    'ArtLag.LagAnt;'
    'Lager.VVareKost;'
    'ArtPris.Pris[1];'
    'StrekkodeListe;'
    'ArtBas.Vg;'
    'ArtBas.LevNr;'
    'LevBas.LevNamn;'
    'ArtBas.Sasong;'
    'ArtBas.Farg;'
    'Farg.FarBeskr;'
    'ArtBas.VmId;'
    'Varemerke.Beskrivelse;'
    'ArtBas.Salgsenhet;'
    'ArtBas.WebButikkArtikkel;'
    'ArtBas.RegistrertDato;'
    'Lager.AntSolgt;'
    'Lager.VerdiSolgt;'
    'Lager.SVK'
    SKIP.

UTLEGG_LAGER:
FOR EACH Lager NO-LOCK WHERE Lager.Butik = iButikkNr,
    FIRST ArtBas NO-LOCK WHERE ArtBas.ArtikkelNr = Lager.ArtikkelNr,
    FIRST ArtPris NO-LOCK WHERE
          ArtPris.ArtikkelNr = Lager.ArtikkelNr,
    EACH ArtLag NO-LOCK WHERE
         ArtLag.ArtikkelNr = Lager.ArtikkelNr AND
         ArtLag.Butik      = Lager.Butik:

    FIND LevBas OF ArtBas NO-LOCK NO-ERROR.
    FIND Farg OF ArtBas NO-LOCK NO-ERROR.
    FIND Varemerke OF ArtBas NO-LOCK NO-ERROR.
    FIND FIRST StrKonv NO-LOCK WHERE
        StrKonv.StrKode = ArtLag.StrKode NO-ERROR.

    cStrekkodeListe = ''.
    iStrKode        = 0.
    FOR EACH Strekkode NO-LOCK WHERE
        Strekkode.ArtikkelNr = Lager.ArtikkelNr AND
        Strekkode.StrKode    = ArtLag.StrKode:
        ASSIGN
          iStrKode        = Strekkode.StrKode
          cStrekkodeListe = cStrekkodeListe + 
                          (IF cStrekkodeListe = '' THEN '' ELSE ',') + 
                          TRIM(Strekkode.Kode).
    END.

    iAnt = IAnt + 1.

    EXPORT STREAM Ut DELIMITER ';'
        ArtLag.Butik
        ArtBas.ArtikkelNr
        REPLACE(ArtBas.LevKod,';',',')
        REPLACE(ArtBas.Beskr,';',',')
        REPLACE(ArtBas.LevFargKod,';',',')
        iStrKode 
        (IF AVAILABLE StrKonv THEN StrKonv.Storl ELSE '') 
        ArtLag.LagAnt 
        Lager.VVareKost 
        ArtPris.Pris[1] 
        cStrekkodeListe
        ArtBas.Vg
        ArtBas.LevNr
        (IF AVAILABLE levbas THEN LevBas.LevNamn ELSE '') 
        ArtBas.Sasong
        ArtBas.Farg
        (IF AVAILABLE Farg THEN Farg.FarBeskr ELSE '')
        ArtBas.VmId
        (IF AVAILABLE Varemerke THEN Varemerke.Beskrivelse ELSE '')
        ArtBas.Salgsenhet
        (IF ArtBas.WebButikkArtikkel THEN '*' ELSE '')
        ArtBas.RegistrertDato        
        Lager.AntSolgt
        Lager.VerdiSolgt
        Lager.SVK
        SKIP.

END. /* UTLEGG_LAGER */
OUTPUT STREAM Ut CLOSE.

MESSAGE 'Lager for butikk ' iButikkNr 'er eksportert til:' SKIP 
    'Fil: ' cFilNavn SKIP
    'Det er eksportert ' iAnt ' lagerposter.'
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
