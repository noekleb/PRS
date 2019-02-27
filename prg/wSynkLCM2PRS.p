DEFINE VARIABLE iProfnr   AS INTEGER  INIT 1   NO-UNDO.
DEFINE VARIABLE cEan8     AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cEan      AS CHARACTER   NO-UNDO.
DEFINE VARIABLE dVarekost AS DECIMAL     NO-UNDO.
DEFINE VARIABLE dMva      AS DECIMAL     NO-UNDO.
FOR EACH vare WHERE aktiv = TRUE AND vare.nonsale = FALSE NO-LOCK:
    FIND pris WHERE pris.ean = vare.ean AND
                    pris.profnr = 1 AND
                    pris.butnr = 0 NO-LOCK NO-ERROR.
    IF NOT AVAIL pris OR pris.utprisn = 0 THEN
        NEXT.
    IF pris.engrosn = 0 THEN DO:
        FIND vargr WHERE vargr.vg = vare.hgr NO-LOCK NO-ERROR.
        IF NOT AVAIL vargr THEN
            NEXT.
    END.
    FIND artbas WHERE artbas.artikkelnr = vare.ean NO-LOCK NO-ERROR.
    /* se om ean-et finns på annan artikel, i så fall ta bort */
    IF NOT AVAIL artbas THEN DO:
        /* tänk på opris, negvare mm */
        CREATE artbas.
        ASSIGN artbas.artikkelnr   = vare.ean
               artbas.vg           = vare.hgr.
        IF LENGTH(STRING(vare.ean)) > 7 THEN
            cEan = FILL("0",13 - LENGTH(STRING(vare.ean))) + STRING(vare.ean).
        ELSE
            cEan = STRING(vare.ean).
        FIND strekkode WHERE strekkode.kode = cEan NO-ERROR.
        IF AVAIL strekkode THEN
            DELETE strekkode.
        CREATE strekkode.
        ASSIGN strekkode.artikkelnr = artbas.artikkelnr
               strekkode.kode       = cEan
               strekkode.Bestillingsnummer = STRING(pris.bestnr).
    END.
    IF artbas.vg <> vargr.vg THEN
        ASSIGN artbas.vg = vargr.vg
               artbas.lopnr = ?.
    ASSIGN artbas.hg           = vargr.hg
           artbas.levnr        = pris.levnr
           artbas.levkod       = STRING(pris.bestnr)
           artbas.ikasse       = vare.aktiv
           artbas.bongtekst    = vare.bong
           artbas.beskr        = vare.varetekst
           artbas.ProdNr       = vare.prodnr
           artbas.lager        = FALSE
           artbas.opris        = vare.opris
           ArtBas.LinkVareNr   = vare.link
           artbas.strtypeid    = 2
           artbas.storrelser   = TRUE
/*            artbas.varetype     = */
           artbas.negvare      = vare.ean >= 550 AND vare.ean <= 559
           artbas.kunderabatt  = vare.krabatt
           ArtBas.Mengde       = vare.mengde
           artbas.hkstyrt      = pris.hkstyrt
           ArtBas.Etikettekst1 = vare.etitekst1
           ArtBas.Etikettekst2 = vare.etitekst2.
        /* mva hämtas från vargr */
    FIND moms OF vargr NO-LOCK.
    FIND artpris WHERE artpris.artikkelnr = artbas.artikkelnr AND
                       artpris.profilnr   = 1.
    IF NOT AVAIL artpris THEN DO:
        CREATE artpris.
        ASSIGN artpris.artikkelnr      = artbas.artikkelnr
               artpris.profilnr        = 1.
    END.
    ASSIGN artpris.ValPris[1]      = pris.engrosn
           artpris.InnkjopsPris[1] = pris.engrosn
           artpris.Varekost[1]     = pris.engrosn
           artpris.pris[1]         = pris.utprisn
           artpris.mva%[1]         = moms.momsproc
           artpris.mvakr[1]        = IF moms.momsproc = 0 THEN 0 ELSE round(artpris.pris[1] * moms.momsproc / (100 + moms.momsproc),2)
           artpris.dbkr[1]         = artpris.pris[1] - artpris.mvakr[1] - artpris.Varekost[1]
           artpris.db%[1]          = ROUND(artpris.dbkr[1] / (artpris.pris[1] - artpris.mvakr[1]),2).
    /* strekkode */
    DO:
        FOR EACH tandem WHERE tandem.ean = vare.ean NO-LOCK:
            IF LENGTH(STRING(tandem.tandemean)) > 7 THEN
                cEan = FILL("0",13 - LENGTH(STRING(tandem.tandemean))) + STRING(tandem.tandemean).
            ELSE
                cEan = STRING(tandem.tandemean).
            FIND strekkode WHERE strekkode.kode = cEan NO-ERROR.
            IF AVAIL strekkode AND strekkode.artikkelnr <> artbas.artikkelnr THEN
                DELETE Strekkode.
            IF NOT AVAIL Strekkode THEN DO:
                CREATE strekkode.
                ASSIGN strekkode.artikkelnr = artbas.artikkelnr
                       strekkode.kode       = cEan.
            END.
            ASSIGN strekkode.bestillingsnummer = STRING(pris.bestnr)
                   strekkode.ikasse            = tandem.aktiv.
        END.
    END.
END.
