
DEF VAR iantArt AS INT NO-UNDO.
DEF VAR cSasongLst AS CHAR NO-UNDO.
DEF VAR cArtNrLst AS CHAR FORMAT "x(100)" NO-UNDO.
DEF VAR cUtFil AS CHAR NO-UNDO.
DEF VAR bHarEan AS LOG NO-UNDO.
DEF VAR bHarTrans AS LOG NO-UNDO.
DEF VAR iAntTrans AS INT NO-UNDO.
DEF VAR iAntEan AS INT NO-UNDO.
DEF VAR cAntLst AS CHAR NO-UNDO.
DEF VAR lSanertTil LIKE ArtBas.ArtikkelNr NO-UNDO.
DEF VAR iMaksAnt AS INT NO-UNDO.
DEF VAR cVmLst AS CHAR NO-UNDO.
DEF VAR iantLinjer AS INT NO-UNDO.
DEF VAR cReturn-value AS CHAR NO-UNDO.

DEF STREAM Ut.

DEF TEMP-TABLE ttArtikkel
    FIELD ArtikkelNr LIKE ArtBas.ArtikkelNr
    FIELD AntEan AS INT
    .

ASSIGN
    iMaksAnt   = 0
    cSasongLst = '201901'
    cVmLst     = ''
    cUtFil     = 'konv\ArtiklerUtenTransEllerPkSdl' + REPLACE(STRING(TODAY),'/','') + '.csv'
    .

OUTPUT STREAM Ut TO VALUE(cUtFil).
PUT STREAM Ut UNFORMATTED
    'iantArt;'
    'ArtikkelNr;'
    'Varetekst;'
    'LevKod;' 
    'Sasong;' 
    'LevFargKod;'
    'Varemerke;'
    'VaremerkeNavn;'
    'HarEan;'
    'HarTrans;'
    'RegistrertDato;'
    'SanertDato;'
    'EDato;'
    'lSanertTil;'
    'cArtNrLst;' 
    'cAntLst'
    SKIP.


BLOKKEN:
FOR EACH ArtBas NO-LOCK WHERE 
    NOT CAN-FIND(FIRST TransLogg WHERE 
                 TransLogg.ArtikkelNr = ArtBas.ArtikkelNr) AND
    NOT CAN-DO(cSasongLst,STRING(ArtBas.Sasong)):

  /* Artikler med ikke mottatte pakkseddler slettes ikke. */
  cReturn-value = ''.
  PkSdlSJEKK:
  FOR EACH PkSdlHode NO-LOCK WHERE 
    PkSdlHode.PkSdlStatus = 10,
    EACH PkSdlLinje OF PkSdlHode NO-LOCK WHERE 
        PkSdlLinje.ArtikkelNr = ArtBas.ArtikkelNr:
        cReturn-value = "Det ikke innleverte pakkseddler på artikkelen "  + STRING(ArtBas.ArtikkelNr) + ".".
        LEAVE PkSdlSJEKK.            
  END. /* PkSdlSJEKK */
  IF cReturn-Value <> '' THEN 
    NEXT.


    ASSIGN 
        iAntEan   = 0
        iantTrans = 0
        iAntArt   = iAntArt + 1
        .
    IF bHarEan = FALSE AND 
        CAN-FIND(FIRST Strekkode OF ArtBas) THEN
        bHarEAN = TRUE.

    IF bHarTrans = FALSE AND 
        CAN-FIND(FIRST TransLogg WHERE 
                 TransLogg.ArtikkelNr = ArtBas.ArtikkelNr) THEN
        bHarTrans = TRUE.

    FOR EACH Strekkode OF ArtBas:
        iAntEan = iAntEan + 1.
    END.
    
    DO:
        FIND Varemerke OF ArtBas NO-LOCK NO-ERROR.
        iAntLinjer = iAntLinjer + 1.
        FOR EACH ttArtikkel
            BREAK BY ttArtikkel.AntEan:
            lSanertTil = ttArtikkel.ArtikkelNr.
        END.
        PUT STREAM Ut UNFORMATTED
            iantArt ';'
            ArtBas.ArtikkelNr ';'
            ArtBas.Beskr ';'
            ArtBas.LevKod ';' 
            ArtBas.Sasong ';' 
            ArtBas.LevFargKod ';'
            ArtBas.VMId ';'
            (IF AVAILABLE Varemerke THEN Varemerke.Beskrivelse ELSE '') ';'
            bHarEAN ';'
            bHarTrans ';'
            ArtBas.RegistrertDato ';' 
            ArtBas.SanertDato ';'
            ArtBas.EDato ';'
            lSanertTil ';'
            cArtNrLst ';'
            cAntLst
            SKIP.
        
        ASSIGN 
            cArtNrLst = ''
            cAntLst   = ''
            iAntArt   = 0
            bHarEan   = FALSE
            bHarTrans = FALSE
            .
        
        RUN slettartbasbatch.w (INPUT ArtBas.ArtikkelNr).
    
    END.
END. /* BLOKKEN */ 
OUTPUT STREAM Ut CLOSE.


