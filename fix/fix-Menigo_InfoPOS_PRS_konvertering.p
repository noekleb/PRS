&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : 
    Purpose     :

    Syntax      :

    Description :

    Author(s)   :
    Created     :
    Notes       :
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE VARIABLE iButikkNr AS INTEGER NO-UNDO.
DEFINE VARIABLE cFilNAvn AS CHARACTER NO-UNDO.
DEFINE VARIABLE cVarefeil AS CHARACTER NO-UNDO.
DEFINE VARIABLE itotAntall AS INTEGER NO-UNDO.

DEFINE BUFFER bArtBas FOR ArtBas.

DEFINE TEMP-TABLE tt_vare NO-UNDO
    FIELD ean AS DECI
    FIELD pris AS DECI
    INDEX ean IS PRIMARY UNIQUE ean.
    
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Procedure
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: CODE-ONLY COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS 

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Procedure ASSIGN
         HEIGHT             = 15
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */
/* CONNECT -db infopos -H 10.125.250.27 -S 8010 -N tcp */

iButikkNr = 0.
UPDATE 
  'Angi butikk det skal hentes ut vare og prisinformasjon for: ' iButikkNr NO-LABELS  
  WITH FRAME G OVERLAY.
ASSIGN iButikkNr = INPUT iButikkNr.
PAUSE 0.
IF iButikkNr = 0 THEN 
  RETURN.    

ASSIGN
  cFilNavn  = "plufile_" + string(iButikkNr) + ".rpt"
  cVareFeil = "MenigoVarerMedFeil_" + string(iButikkNr) + ".txt"
  .
  
IF SEARCH(cFilNavn) = ? THEN 
DO:
  MESSAGE 'Kan ikke finne varefil fra kassen i butikk ' + string(iButikkNr) + '.' SKIP 
          'Filnavn: ' cFilNavn
  VIEW-AS ALERT-BOX.
  RETURN.
END.
    
RUN OppdaterMva.     /* Moms */

RUN OppdaterGruppe.  /* Avdeling, HuvGr, VarGr, VgKat */

RUN OppdaterLev.     /* Levbas, Produsent */

RUN OppdaterFylke.   /* Fylke, Kommune */

RUN OppdaterPost.    /* Post */

RUN OppdaterButiker. /* Butiker */

/*RUN OppdaterArtiklar.*/  /* 6/5-11 Denne kobles ut og erstattes med den nye oppdateringsrutinen */

RUN oppdaterArtiklerFraVarefilKasse.

RUN FixUtvidetSok.

RUN OppdatKunder.

RUN OppdaterKasserere.

RUN systemfix.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-FixUtvidetSok) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE FixUtvidetSok Procedure 
PROCEDURE FixUtvidetSok :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    FOR EACH Artbas:
        Artbas.notat = TRIM(Artbas.notat) + " ".
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-OppdaterArtiklar) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OppdaterArtiklar Procedure 
PROCEDURE OppdaterArtiklar :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

/* -------

DEFINE VARIABLE iProfnr   AS INTEGER  INIT 1   NO-UNDO.
DEFINE VARIABLE cEan8     AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cEan      AS CHARACTER   NO-UNDO.
DEFINE VARIABLE dVarekost AS DECIMAL     NO-UNDO.
DEFINE VARIABLE dMva      AS DECIMAL     NO-UNDO.
DEFINE VARIABLE iLopnr    AS INTEGER     NO-UNDO.
DEFINE VARIABLE iTeller   AS INTEGER     NO-UNDO.
DEFINE VARIABLE dProfNr   AS INTEGER     NO-UNDO.
DEFINE VARIABLE dEngrosn  AS DECIMAL     NO-UNDO.
ON CREATE OF artbas OVERRIDE DO: END.
ON WRITE OF artbas  OVERRIDE DO: END.
ON CREATE OF artpris OVERRIDE DO: END.
ON WRITE OF artpris  OVERRIDE DO: END.
ON CREATE OF strekkode OVERRIDE DO: END.
ON WRITE OF strekkode  OVERRIDE DO: END.

FOR EACH vare NO-LOCK:
    iTeller = iTeller + 1.
    IF iTeller MOD 100 = 0 THEN DO:
        DISPLAY iTeller WITH FRAME a.
        PAUSE 0 NO-MESSAGE.
    END.
    FIND pris WHERE pris.ean = vare.ean AND
                    pris.profnr = 1 AND
                    pris.butnr = 0 NO-LOCK NO-ERROR.
    /* det ligger noen artikler som ikke har riser på hovedprofilen, bare lokale priser. */
    /* Før å få med disse gjør vi dette. Da får hovedprofilen den første lokale prisen.  */
    IF NOT AVAILABLE Pris THEN 
      FIND FIRST pris WHERE pris.ean = vare.ean AND
                    pris.profnr = 1 AND
                    pris.butnr > 0 NO-LOCK NO-ERROR.
    IF NOT AVAIL pris OR pris.utprisn = 0 THEN
        NEXT.
        
    FIND vargr WHERE vargr.vg = vare.hgr NO-LOCK NO-ERROR.
    IF NOT AVAIL vargr THEN
        NEXT.
    FIND artbas WHERE artbas.artikkelnr = vare.ean NO-ERROR.
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
               strekkode.Bestillingsnummer = IF pris.bestnr = ? OR pris.bestnr = 0 THEN STRING(vare.nklnr) ELSE STRING(pris.bestnr)
               strekkode.Bestillingsnummer = IF strekkode.Bestillingsnummer = "0" OR 
                                                strekkode.Bestillingsnummer = ?   OR 
                                                strekkode.Bestillingsnummer = "?" THEN "" ELSE strekkode.Bestillingsnummer 
               strekkode.kodetype   = IF LENGTH(cEan) = 13 THEN 1 ELSE 0
               strekkode.strkode    = 1.
    END.
    FIND Jamforenhet WHERE JamforEnhet.JamforEnhId = vare.enhet NO-LOCK NO-ERROR.
    IF artbas.vg <> vargr.vg THEN
        ASSIGN artbas.vg = vargr.vg
               artbas.lopnr = ?.
    ASSIGN artbas.hg           = vargr.hg
           artbas.levnr        = pris.levnr
           artbas.levkod       = IF pris.bestnr > 0 THEN STRING(pris.bestnr) ELSE ""
           artbas.levkod       = IF artbas.levkod = ? OR artbas.levkod = "?" THEN "" ELSE artbas.levkod
           artbas.ikasse       = vare.aktiv
           artbas.bongtekst    = vare.bong
           artbas.beskr        = vare.varetekst
           artbas.ProdNr       = vare.prodnr
           artbas.lager        = FALSE
           artbas.opris        = vare.opris
           artbas.artslag      = IF pris.vekt = 1 THEN 1 ELSE artbas.artslag
           ArtBas.LinkVareNr   = vare.link
           artbas.strtypeid    = 2
           artbas.storrelser   = TRUE
/*            artbas.varetype     = */
           artbas.NON_Sale     = vare.nonsale
           artbas.etikett      = IF pris.etiant = 1 THEN 2 ELSE 0
           ArtBas.JamforEnhet  = IF AVAIL jamforenhet THEN JamforEnhet.JamforEnhet ELSE ""
           artbas.negvare      = vare.ean >= 550 AND vare.ean <= 559
           artbas.kunderabatt  = vare.krabatt
           ArtBas.Mengde       = vare.mengde
           artbas.hkstyrt      = pris.hkstyrt
           ArtBas.Etikettekst1 = vare.etitekst1
           ArtBas.Etikettekst2 = vare.etitekst2
           ArtBas.SalgsEnhet   = "Stk".
        /* mva hämtas från vargr */
    FIND moms OF vargr NO-LOCK.
    FIND artpris WHERE artpris.artikkelnr = artbas.artikkelnr AND
                       artpris.profilnr   = 1 NO-ERROR.
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
           artpris.mvakr[1]        = IF moms.momsproc = 0 THEN 0 ELSE ROUND(artpris.pris[1] * moms.momsproc / (100 + moms.momsproc),2)
           artpris.dbkr[1]         = artpris.pris[1] - artpris.mvakr[1] - artpris.Varekost[1]
           artpris.db%[1]          = ROUND(artpris.dbkr[1] / (artpris.pris[1] - artpris.mvakr[1]) * 100,2)
           ArtPris.AktivFraDato    = pris.dato.
/*            ArtPris.AktivFraTid     = 0 */

    /* strekkode */
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
        ASSIGN strekkode.Bestillingsnummer = IF pris.bestnr = ? OR pris.bestnr = 0 THEN STRING(vare.nklnr) ELSE STRING(pris.bestnr)
               strekkode.Bestillingsnummer = IF strekkode.Bestillingsnummer = "0" OR 
                                                strekkode.Bestillingsnummer = ?   OR 
                                                strekkode.Bestillingsnummer = "?" THEN "" ELSE strekkode.Bestillingsnummer 
               strekkode.ikasse            = tandem.aktiv
               strekkode.kodetype   = IF LENGTH(cEan) = 13 THEN 1 ELSE 0
               strekkode.strkode    = 1.
    END.
    IF artbas.lopnr = ? THEN DO:
        RUN SettLopNr.p (artbas.vg,"F",OUTPUT iLopnr).
        ASSIGN artbas.lopnr = iLopnr.
    END.
    /* Skapa lokala priser */
    IF artbas.negvare = FALSE AND artbas.opris = FALSE AND Artbas.pant = FALSE THEN DO:
        dEngrosn = pris.engrosn.
        FOR EACH pris WHERE pris.ean = vare.ean AND
                            pris.profnr = 1 AND
                            pris.butnr = 404
                            /*pris.butnr > 0*/ NO-LOCK.
            IF NOT CAN-FIND(skotex.butiker WHERE skotex.butiker.butik = pris.butnr) THEN
                NEXT.
            dProfNr = 1000000 + pris.butnr.
            FIND artpris WHERE artpris.artikkelnr = artbas.artikkelnr AND
                               artpris.profilnr   = dProfNr NO-ERROR.
            IF NOT AVAIL artpris THEN DO:
                CREATE artpris.
                ASSIGN artpris.artikkelnr      = artbas.artikkelnr
                       artpris.profilnr        = dProfnr.
            END.

            ASSIGN artpris.ValPris[1]      = dEngrosn
                   artpris.InnkjopsPris[1] = dEngrosn
                   artpris.Varekost[1]     = dEngrosn
                   artpris.pris[1]         = pris.utprisn
                   artpris.mva%[1]         = moms.momsproc
                   artpris.mvakr[1]        = IF moms.momsproc = 0 THEN 0 ELSE ROUND(artpris.pris[1] * moms.momsproc / (100 + moms.momsproc),2)
                   artpris.dbkr[1]         = artpris.pris[1] - artpris.mvakr[1] - artpris.Varekost[1]
                   artpris.db%[1]          = ROUND(artpris.dbkr[1] / (artpris.pris[1] - artpris.mvakr[1]) * 100,2)
                   ArtPris.AktivFraDato    = pris.dato.
        END.
    END.
END.
---------------------------*/
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

 
&IF DEFINED(EXCLUDE-oppdaterArtiklerFraVarefilKasse) = 0 &THEN
        
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE oppdaterArtiklerFraVarefilKasse Procedure
PROCEDURE oppdaterArtiklerFraVarefilKasse:
    /*------------------------------------------------------------------------------
            Purpose:                                                                      
            Notes:                                                                        
    ------------------------------------------------------------------------------*/
DEFINE VARIABLE cc   AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cEan AS CHARACTER   NO-UNDO.
DEFINE VARIABLE iTot AS INTEGER     NO-UNDO.
DEFINE VARIABLE iAntVare    AS INTEGER     NO-UNDO.
DEFINE VARIABLE iAntHittade AS INTEGER     NO-UNDO.
DEFINE VARIABLE iBaraTandem AS INTEGER     NO-UNDO.
DEFINE VARIABLE cPris       AS CHARACTER   NO-UNDO.
DEFINE VARIABLE iNonOpris AS INTEGER     NO-UNDO.
DEFINE VARIABLE iVareUtanPris AS INTEGER     NO-UNDO.
DEFINE VARIABLE iBaraBut0 AS INTEGER     NO-UNDO.
DEFINE VARIABLE iBaraBut14 AS INTEGER     NO-UNDO.
DEFINE VARIABLE iBaada AS INTEGER     NO-UNDO.
DEFINE VARIABLE iSaknas AS INTEGER     NO-UNDO.
DEFINE VARIABLE dPris AS DECIMAL     NO-UNDO.
DEFINE VARIABLE cKode AS CHARACTER   NO-UNDO.

ON CREATE OF artbas OVERRIDE DO: END.
ON WRITE OF artbas  OVERRIDE DO: END.
ON CREATE OF artpris OVERRIDE DO: END.
ON WRITE OF artpris  OVERRIDE DO: END.
ON CREATE OF strekkode OVERRIDE DO: END.
ON WRITE OF strekkode  OVERRIDE DO: END.

RUN TellOppLinjer.

FIND Butikk NO-LOCK WHERE
  Butikk.ButNr = iButikkNr NO-ERROR.

DISPLAY 
iantVare LABEL 'Antall behandlet' SKIP 
itotAntall LABEL 'Totalt antall varer' WITH FRAME g.
PAUSE 0.

INPUT FROM value(cFilNavn).
OUTPUT TO value(cVareFeil).

VARERAD:
REPEAT TRANSACTION:
    /* Leser linje fra varefil og trimmer bort ledende og avsluttende space. */
    IMPORT UNFORMATTED cc.
    cc = TRIM(cc).
    IF cc = "" THEN
        NEXT.
        
    ASSIGN 
      iTot = iTot + 1
      cEan = ENTRY(1,cc,' ').

    /* Strekkoder med feil */
    /*
    IF cEan BEGINS "7388" AND LENGTH(cEan) <> 13 THEN
        NEXT.
    IF cEan BEGINS "7388" AND SUBSTR(cEan,13) <> "0" THEN
        NEXT.
    */    
    IF AVAILABLE Tandem THEN 
      RELEASE tandem.
    
    /* Sjekker vare og tandem om EAN finnes */
    FIND vare WHERE vare.ean = DECI(cEan) NO-LOCK NO-ERROR.
    IF NOT AVAIL vare THEN 
    DO:
        FIND tandem WHERE tandem.tandemean = DECI(cEan) NO-LOCK NO-ERROR.
        IF AVAIL tandem THEN
            FIND vare WHERE vare.ean = tandem.ean NO-LOCK NO-ERROR.
    END.
    
    /* Logger ukjente EAN koder som kommer fra kassens varefil. */
    IF NOT AVAILABLE Vare THEN 
    DO:
        PUT UNFORMATTED "Ukjent EAN på denne vare: " cc SKIP.
        NEXT VARERAD.
    END.
    
    /* Vare finnes og overføres til PRS. */
    ELSE IF AVAIL vare THEN 
    DO:
        /* Henter prisen fra varefilen */
        cPris = REPLACE(TRIM(SUBSTR(cc,72)),".",",").
        dPris = DECI(cPris).

        FIND Jamforenhet WHERE JamforEnhet.JamforEnhId = vare.enhet NO-LOCK NO-ERROR.
        FIND vargr WHERE vargr.vg = vare.hgr NO-LOCK NO-ERROR.
        FIND moms OF vargr NO-LOCK.
        FIND FIRST pris WHERE pris.ean = vare.ean AND
                        Pris.LevNr > 0 AND 
                        pris.profnr    = Butikk.ProfNr NO-ERROR.
        IF NOT AVAILABLE Pris THEN 
          FIND FIRST pris WHERE pris.ean = vare.ean NO-LOCK NO-ERROR.

        IF NOT AVAIL pris THEN 
        DO:
            PUT UNFORMATTED "Finner ingen pris på artikkelen. Oprettet med pris fra kasse og 0 i varekost: " cc SKIP.
            /*NEXT VARERAD.*/
        END.
    
        FIND Artbas WHERE artbas.artikkelnr = vare.ean EXCLUSIVE-LOCK NO-ERROR.
        IF NOT AVAIL artbas THEN 
        DO:
            CREATE artbas.
            ASSIGN artbas.artikkelnr   = vare.ean
                   artbas.vg           = vare.hgr.
        END.
        ASSIGN artbas.hg           = vargr.hg
               artbas.levnr        = IF AVAILABLE Pris THEN pris.levnr ELSE 999999
               artbas.levkod       = IF (AVAILABLE Pris AND pris.bestnr > 0) THEN STRING(pris.bestnr) ELSE ""
               artbas.levkod       = IF artbas.levkod = ? OR artbas.levkod = "?" THEN "" ELSE artbas.levkod
               artbas.ikasse       = vare.aktiv
               artbas.bongtekst    = vare.bong
               artbas.beskr        = vare.varetekst
               ArtBas.Pant         = IF ArtBas.Beskr BEGINS 'Pant' THEN TRUE ELSE FALSE 
               artbas.ProdNr       = vare.prodnr
               artbas.lager        = FALSE
               artbas.opris        = vare.opris
               artbas.artslag      = IF (AVAILABLE Pris AND pris.vekt = 1) THEN 1 ELSE 0
               artbas.artslag      = IF (LENGTH(cEan) >= 8 AND artbas.artslag > 0) THEN 0 ELSE artbas.artslag
               ArtBas.LinkVareNr   = vare.link
               artbas.strtypeid    = 2
               artbas.storrelser   = TRUE
    /*            artbas.varetype     = */
               /*artbas.NON_Sale     = vare.nonsale*/
               artbas.etikett      = IF (AVAILABLE Pris AND pris.etiant = 1) THEN 2 ELSE 0
               ArtBas.JamforEnhet  = IF AVAIL jamforenhet THEN JamforEnhet.JamforEnhet ELSE ""
/*                    artbas.negvare      = vare.ean >= 550 AND vare.ean <= 559 */
               artbas.kunderabatt  = vare.krabatt
               ArtBas.Mengde       = vare.mengde
               artbas.hkstyrt      = IF AVAILABLE Pris THEN pris.hkstyrt ELSE FALSE 
               ArtBas.Etikettekst1 = vare.etitekst1
               ArtBas.Etikettekst2 = vare.etitekst2
               ArtBas.SalgsEnhet   = IF artbas.artslag = 1 THEN "Kg" ELSE "Stk"
               ArtBas.AnbefaltPris = IF AVAILABLE Pris THEN Pris.VeilPris ELSE ArtBas.AnbefaltPris.

        FIND ArtPris EXCLUSIVE-LOCK WHERE
             ArtPris.ArtikkelNr = ArtBas.ArtikkelNr AND 
             ArtPris.ProfilNr = 1 NO-ERROR.
        IF NOT AVAILABLE ArtPris THEN 
        DO:
          CREATE artpris.
          ASSIGN artpris.artikkelnr      = artbas.artikkelnr
                 artpris.profilnr        = 1.
        END.
        ASSIGN artpris.ValPris[1]      = IF AVAILABLE Pris THEN pris.engrosn ELSE artpris.ValPris[1]
               artpris.InnkjopsPris[1] = IF AVAILABLE Pris THEN pris.engrosn ELSE artpris.InnkjopsPris[1]
               artpris.Varekost[1]     = IF AVAILABLE Pris THEN pris.engrosn ELSE artpris.Varekost[1]
               artpris.pris[1]         = IF AVAILABLE Pris THEN pris.UtprisN ELSE ArtPris.Pris[1]
               artpris.mva%[1]         = moms.momsproc
               ArtPris.MomsKod         = Moms.MomsKod
               artpris.mvakr[1]        = IF moms.momsproc = 0 THEN 0 ELSE ROUND(artpris.pris[1] * moms.momsproc / (100 + moms.momsproc),2)
               artpris.dbkr[1]         = artpris.pris[1] - artpris.mvakr[1] - artpris.Varekost[1]
               artpris.db%[1]          = ROUND(artpris.dbkr[1] / (artpris.pris[1] - artpris.mvakr[1]) * 100,2)
               ArtPris.AktivFraDato    = IF AVAILABLE Pris THEN pris.dato ELSE TODAY.
        IF ArtBas.LopNr = ? THEN 
          RUN settLopNr.p (ArtBas.Vg, 'F', OUTPUT ArtBas.LopNr). 

        IF LENGTH(cEan) < 8 THEN
            cKode = cEan.
        ELSE
            cKode = FILL("0",13 - LENGTH(cEan)) + cEan.
        FIND strekkode WHERE strekkode.kode = cKode NO-LOCK NO-ERROR.
        IF NOT AVAIL strekkode THEN DO:
            CREATE strekkode.
            ASSIGN strekkode.artikkelnr = artbas.artikkelnr
                   strekkode.kodetype   = IF LENGTH(cKode) = 13 THEN 1 ELSE 0
                   strekkode.kode       = cKode
                   strekkode.strkode    = 1
                   strekkode.Bestillingsnummer = Artbas.levkod.
        END.

        /* om tandem är tillgänglig så skall vi skapa ean från vare.ean */
        IF AVAIL tandem THEN DO:
            cKode = STRING(vare.ean).
            IF LENGTH(cEan) < 8 THEN
                cKode = cEan.
            ELSE
                cKode = FILL("0",13 - LENGTH(cEan)) + cEan.
            FIND strekkode WHERE strekkode.kode = cKode NO-LOCK NO-ERROR.
            IF NOT AVAIL strekkode THEN DO:
                CREATE strekkode.
                ASSIGN strekkode.artikkelnr = artbas.artikkelnr
                       strekkode.kodetype   = IF LENGTH(cKode) < 8 THEN 0 ELSE 1
                       strekkode.kode       = cKode
                       strekkode.strkode    = 1
                       strekkode.Bestillingsnummer = Artbas.levkod.
            END.
        END.
    END.

END. /* TRANSACTION */
INPUT CLOSE.
OUTPUT CLOSE.

END PROCEDURE.
    
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF



&IF DEFINED(EXCLUDE-OppdaterButiker) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OppdaterButiker Procedure 
PROCEDURE OppdaterButiker :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   FOR EACH Infopos.Butikk NO-LOCK WHERE
     InfoPOS.Butikk.ButNr = iButikkNr.
     
       FIND SkoTex.Butiker WHERE SkoTex.Butiker.butik = Infopos.butikk.butnr NO-LOCK NO-ERROR.
       FIND skotex.post WHERE skotex.post.postnr = STRING(Infopos.butikk.postnr) NO-LOCK NO-ERROR.
       
       IF NOT AVAIL SkoTex.Butiker THEN 
       DO:
           CREATE SkoTex.Butiker.
           ASSIGN SkoTex.Butiker.Butik       = Infopos.butikk.butnr
                  SkoTex.Butiker.ProfilNr    = Infopos.butikk.profnr
                  SkoTex.Butiker.ApningsDato = DATE(01,01,2005)
                  SkoTex.Butiker.BuAdr       = Infopos.butikk.adresse
                  SkoTex.Butiker.BuKon       = Infopos.butikk.kontakt
                  SkoTex.Butiker.BuPadr      = IF AVAIL Skotex.Post THEN SkoTex.Post.Beskrivelse ELSE ""
                  SkoTex.Butiker.BuPonr      = STRING(Infopos.butikk.postnr)
                  SkoTex.Butiker.BuTel       = Infopos.butikk.telefon
                  SkoTex.Butiker.Butik       = Infopos.butikk.butnr
                  SkoTex.Butiker.ProfilNr    = IF infopos.butikk.butnr > 100 THEN 1000000 + infopos.butikk.butnr ELSE 1
                  SkoTex.Butiker.ButNamn     = REPLACE(Infopos.butikk.navn,","," ")
                  SkoTex.Butiker.KortNavn    = STRING(Infopos.butikk.butnr) NO-ERROR.
           IF ERROR-STATUS:ERROR THEN
               DELETE SkoTex.Butiker.
       END.
       ELSE IF SkoTex.Butiker.BuAdr    <> Infopos.butikk.adresse        OR
               SkoTex.Butiker.BuKon    <> Infopos.butikk.kontakt        OR
               SkoTex.Butiker.BuPonr   <> STRING(Infopos.butikk.postnr) OR
               SkoTex.Butiker.BuTel    <> Infopos.butikk.telefon        OR
               SkoTex.Butiker.Butik    <> Infopos.butikk.butnr          OR
               SkoTex.Butiker.ButNamn  <> REPLACE(Infopos.butikk.navn,","," ") OR
               SkoTex.Butiker.KortNavn <> STRING(Infopos.butikk.butnr)   THEN 
       DO:
           FIND CURRENT SkoTex.Butiker EXCLUSIVE NO-WAIT.
           IF AVAIL SkoTex.Butiker THEN DO:
               ASSIGN SkoTex.Butiker.ProfilNr = IF infopos.butikk.butnr > 100 THEN 1000000 + infopos.butikk.butnr ELSE 1
                      SkoTex.Butiker.BuAdr    = Infopos.butikk.adresse       
                      SkoTex.Butiker.BuKon    = Infopos.butikk.kontakt       
                      SkoTex.Butiker.BuPonr   = STRING(Infopos.butikk.postnr)
                      SkoTex.Butiker.BuPadr   = IF AVAIL Skotex.Post THEN SkoTex.Post.Beskrivelse ELSE ""
                      SkoTex.Butiker.BuTel    = Infopos.butikk.telefon       
                      SkoTex.Butiker.ButNamn  = REPLACE(Infopos.butikk.navn,","," ")
                      SkoTex.Butiker.KortNavn = STRING(Infopos.butikk.butnr) NO-ERROR.
           END.
       END.
       RUN kopierKasse2Time.p (Skotex.Butiker.Butik) NO-ERROR.
       RELEASE SkoTex.Butiker.
   END.
/* SkoTex.Butiker.BankKonto               = */
/* SkoTex.Butiker.Butiker.ButLand         = */
/* SkoTex.Butiker.Butiker.ePostAdresse    = */
/* SkoTex.Butiker.Butiker.harButikksystem = */
/* SkoTex.Butiker.Butiker.LevAdresse1     = */
/* SkoTex.Butiker.Butiker.LevAdresse2     = */
/* SkoTex.Butiker.Butiker.LevKontakt      = */
/* SkoTex.Butiker.Butiker.LevMerknad      = */
/* SkoTex.Butiker.Butiker.LevPostBoks     = */
/* SkoTex.Butiker.Butiker.LevPostNr       = */
/* SkoTex.Butiker.Butiker.LevTelefon      = */
/* SkoTex.Butiker.Butiker.OrganisasjonsNr = */
/* SkoTex.Butiker.Butiker.Postgiro        = */
/* SkoTex.Butiker.Butiker.ProfilNr        = */
/* SkoTex.Butiker.Butiker.StdVeksel       = */
/* SkoTex.Butiker.Butiker.Telefaks        = */
/* SkoTex.Butiker.Butiker.URLAdresse      = */
/* SkoTex.Butiker.Butiker.VaarREf         = */
/* SkoTex.Butiker.Butiker.FaktTekstNr     = */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-OppdaterFylke) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OppdaterFylke Procedure 
PROCEDURE OppdaterFylke :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    IF NOT CAN-FIND(kommune WHERE kommune.kommnr = "") THEN DO:
        CREATE kommune.
        ASSIGN kommune.kommnr = "" NO-ERROR.
        IF ERROR-STATUS:ERROR THEN
            DELETE kommune.
    END.
    IF NOT CAN-FIND(fylke WHERE fylke.fylkesnr = "") THEN DO:
        CREATE fylke.
        ASSIGN fylke.fylkesnr = "" NO-ERROR.
        IF ERROR-STATUS:ERROR THEN
            DELETE fylke.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-OppdaterGruppe) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OppdaterGruppe Procedure 
PROCEDURE OppdaterGruppe :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
FOR EACH InfoPOS.Avdeling NO-LOCK:
    FIND skotex.Avdeling WHERE 
        skotex.Avdeling.avdelingnr = infopos.avdeling.avd NO-LOCK NO-ERROR.
    IF NOT AVAIL skotex.Avdeling THEN 
    DO:
        CREATE skotex.Avdeling.
        ASSIGN SkoTex.Avdeling.AvdelingNr   = infopos.avdeling.avd
               SkoTex.Avdeling.AvdelingNavn = infopos.avdeling.avdtekst NO-ERROR.
        IF ERROR-STATUS:ERROR THEN
            DELETE skotex.Avdeling.
    END.
    ELSE IF SkoTex.Avdeling.AvdelingNavn <> infopos.avdeling.avdtekst THEN 
    DO:
        FIND CURRENT SkoTex.Avdeling EXCLUSIVE NO-WAIT.
        IF AVAIL SkoTex.Avdeling THEN
            ASSIGN SkoTex.Avdeling.AvdelingNavn = infopos.avdeling.avdtekst.
    END.
    RELEASE skotex.Avdeling.



    FIND skotex.HuvGr WHERE 
        skotex.HuvGr.Hg = infopos.avdeling.avd NO-LOCK NO-ERROR.
    IF NOT AVAIL skotex.huvgr THEN 
    DO:
        CREATE skotex.huvgr.
        ASSIGN SkoTex.HuvGr.AvdelingNr = infopos.avdeling.avd
               SkoTex.HuvGr.Hg         = infopos.avdeling.avd
               SkoTex.HuvGr.HgBeskr    = infopos.avdeling.avdtekst NO-ERROR.
        IF ERROR-STATUS:ERROR THEN
            DELETE skotex.HuvGr.
    END.
    ELSE IF SkoTex.HuvGr.AvdelingNr <> infopos.avdeling.avd OR
            SkoTex.HuvGr.HgBeskr    <> infopos.avdeling.avdtekst THEN 
    DO:
        FIND CURRENT SkoTex.HuvGr EXCLUSIVE NO-WAIT.
        ASSIGN SkoTex.HuvGr.AvdelingNr = infopos.avdeling.avd
               SkoTex.HuvGr.HgBeskr    = infopos.avdeling.avdtekst NO-ERROR.
    END.
    RELEASE Skotex.HuvGr.

    VAREGRUPPE:
    FOR EACH Infopos.hovedgr NO-LOCK WHERE
        Infopos.hovedgr.Avd = InfoPOS.Avdeling.Avd:

        FIND skotex.VarGr WHERE skotex.Vargr.vg = Infopos.hovedgr.hgr NO-LOCK NO-ERROR.
        FIND infopos.hgrprof WHERE infopos.hgrprof.profnr = 1 AND 
                                   Infopos.hgrprof.hgr    = Infopos.hovedgr.hgr NO-LOCK NO-ERROR.
        IF NOT AVAIL skotex.VarGr THEN 
        DO:
            CREATE skotex.VarGr.
            ASSIGN SkoTex.VarGr.Hg        = Infopos.hovedgr.Avd
                   SkoTex.VarGr.Kost_Proc = IF AVAIL Infopos.hgrprof THEN 100 - Infopos.hgrprof.brutto% ELSE 0
                   SkoTex.VarGr.MomsKod   = Infopos.hovedgr.mvagr
                   SkoTex.VarGr.Vg        = Infopos.hovedgr.hgr     
                   SkoTex.VarGr.VgBeskr   = Infopos.hovedgr.hgrtekst NO-ERROR.
            IF ERROR-STATUS:ERROR THEN DO:
                DELETE skotex.vargr.
                NEXT.
            END.
            CREATE skotex.vgkat.
            ASSIGN SkoTex.VgKat.KatNr = 1
                   SkoTex.VgKat.Vg    = Infopos.hovedgr.hgr
                   SkoTex.VgKat.VgKat = 1 NO-ERROR.
            IF ERROR-STATUS:ERROR THEN
                DELETE skotex.VgKat.
        END.
        ELSE IF SkoTex.VarGr.Hg        <> Infopos.hovedgr.Avd OR
                SkoTex.VarGr.Kost_Proc <> (IF AVAIL Infopos.hgrprof THEN 100 - Infopos.hgrprof.brutto% ELSE 0) OR
                SkoTex.VarGr.MomsKod   <> Infopos.hovedgr.mvagr OR
                SkoTex.VarGr.VgBeskr   <> Infopos.hovedgr.hgrtekst THEN 
        DO:
            FIND CURRENT SkoTex.VarGr EXCLUSIVE NO-WAIT.
            ASSIGN SkoTex.VarGr.Hg        = Infopos.hovedgr.Avd
                   SkoTex.VarGr.Kost_Proc = IF AVAIL Infopos.hgrprof THEN 100 - Infopos.hgrprof.brutto% ELSE 0
                   SkoTex.VarGr.MomsKod   = Infopos.hovedgr.mvagr
                   SkoTex.VarGr.VgBeskr   = Infopos.hovedgr.hgrtekst NO-ERROR.
        END.
        RELEASE SkoTex.VarGr.


    END. /* VAREGRUPPE */
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-OppdaterLev) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OppdaterLev Procedure 
PROCEDURE OppdaterLev :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DO TRANSACTION:
      IF NOT CAN-FIND(LevBas WHERE
                      LevBas.LevNr = 999999) THEN 
      DO:
        CREATE LevBas.
        ASSIGN
            LevBas.LevNr = 999999
            LevBas.LevNamn = 'DIV. Lev.'.
        RELEASE LevBas.                
      END.
    END.
    
    FOR EACH infopos.lev NO-LOCK:
        FIND skotex.levbas WHERE 
             skotex.levbas.levnr = Int(infopos.lev.levnr) NO-LOCK NO-ERROR.
        IF NOT AVAIL skotex.levbas THEN 
        DO:
            CREATE skotex.levbas.
            ASSIGN SkoTex.LevBas.levnr        = infopos.lev.levnr
                   SkoTex.LevBas.levnamn      = infopos.lev.navn
                   SkoTex.LevBas.E_MailLev    = infopos.lev.emailadr
                   SkoTex.LevBas.levadr       = infopos.lev.adresse
                   SkoTex.LevBas.levkon       = infopos.lev.kontakt
                   SkoTex.LevBas.levpadr      = infopos.lev.poststed
                   SkoTex.LevBas.levponr      = STRING(infopos.lev.postnr)
                   SkoTex.LevBas.levtel       = infopos.lev.telefon
                   SkoTex.LevBas.telefax      = infopos.lev.faxnr NO-ERROR.
            IF ERROR-STATUS:ERROR THEN
                DELETE skotex.levbas.
        END.
        ELSE IF SkoTex.LevBas.E_MailLev <> infopos.lev.emailadr       OR
                SkoTex.LevBas.levadr    <> infopos.lev.adresse        OR
                SkoTex.LevBas.levkon    <> infopos.lev.kontakt        OR
                SkoTex.LevBas.levnamn   <> infopos.lev.navn           OR
                SkoTex.LevBas.levpadr   <> infopos.lev.poststed       OR
                SkoTex.LevBas.levponr   <> STRING(infopos.lev.postnr) OR
                SkoTex.LevBas.levtel    <> infopos.lev.telefon        OR
                SkoTex.LevBas.telefax   <> infopos.lev.faxnr THEN 
        DO:
            FIND CURRENT SkoTex.LevBas EXCLUSIVE NO-WAIT.
            IF AVAIL SkoTex.LevBas THEN DO:
                ASSIGN SkoTex.LevBas.E_MailLev = infopos.lev.emailadr
                       SkoTex.LevBas.levadr    = infopos.lev.adresse
                       SkoTex.LevBas.levkon    = infopos.lev.kontakt
                       SkoTex.LevBas.levnamn   = infopos.lev.navn
                       SkoTex.LevBas.levpadr   = infopos.lev.poststed
                       SkoTex.LevBas.levponr   = STRING(infopos.lev.postnr)
                       SkoTex.LevBas.levtel    = infopos.lev.telefon
                       SkoTex.LevBas.telefax   = infopos.lev.faxnr.
                RELEASE Skotex.LevBas.
            END.
        END.
        RELEASE SkoTex.LevBas.
    END.
    FOR EACH Infopos.produsent NO-LOCK.
        FIND skotex.produsent WHERE skotex.produsent.ProdNr = infopos.produsent.prodnr NO-LOCK NO-ERROR.
        IF NOT AVAIL skotex.produsent THEN 
        DO:
            CREATE skotex.produsent.
            ASSIGN SkoTex.Produsent.ProdNr      = infopos.produsent.prodnr
                   SkoTex.Produsent.Beskrivelse = infopos.produsent.navn 
                   SkoTex.Produsent.Adresse1    = infopos.produsent.adresse
                   SkoTex.Produsent.Kontakt     = infopos.produsent.kontakt 
                   SkoTex.Produsent.PostNr      = STRING(infopos.produsent.postnr)
                   SkoTex.Produsent.Telefon     = infopos.produsent.telefon NO-ERROR.
            IF ERROR-STATUS:ERROR THEN
                DELETE skotex.produsent.
        END.
        ELSE IF SkoTex.Produsent.Beskrivelse <> infopos.produsent.navn           OR
                   SkoTex.Produsent.Adresse1 <> infopos.produsent.adresse        OR
                   SkoTex.Produsent.Kontakt  <> infopos.produsent.kontakt        OR
                   SkoTex.Produsent.PostNr   <> STRING(infopos.produsent.postnr) OR
                   SkoTex.Produsent.Telefon  <> infopos.produsent.telefon THEN 
        DO:
            FIND CURRENT SkoTex.Produsent EXCLUSIVE NO-WAIT.
            IF AVAIL skotex.produsent THEN DO:
                ASSIGN SkoTex.Produsent.Beskrivelse = infopos.produsent.navn 
                       SkoTex.Produsent.Adresse1    = infopos.produsent.adresse
                       SkoTex.Produsent.Kontakt     = infopos.produsent.kontakt 
                       SkoTex.Produsent.PostNr      = STRING(infopos.produsent.postnr)
                       SkoTex.Produsent.Telefon     = infopos.produsent.telefon NO-ERROR.
                RELEASE skotex.produsent.
            END.
        END.
        RELEASE skotex.produsent.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-OppdaterMva) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OppdaterMva Procedure 
PROCEDURE OppdaterMva :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    FOR EACH Infopos.mva NO-LOCK.
        FIND skotex.moms WHERE 
        skotex.moms.momskod = Infopos.mva.mvagr NO-LOCK NO-ERROR.
        
        IF NOT AVAIL skotex.moms THEN 
        DO:
            CREATE skotex.moms.
            ASSIGN skotex.moms.MomsKod     = infopos.mva.mvagr
                   skotex.moms.Beskrivelse = infopos.mva.mvatekst
                   skotex.moms.MomsProc    = infopos.mva.mva NO-ERROR.
            IF ERROR-STATUS:ERROR THEN
                DELETE skotex.moms.
        END.
        
        ELSE IF skotex.moms.Beskrivelse <> infopos.mva.mvatekst OR
                skotex.moms.MomsProc    <> infopos.mva.mva THEN 
        DO:
            FIND CURRENT skotex.moms EXCLUSIVE NO-WAIT.
            IF AVAIL skotex.moms THEN DO:
                ASSIGN skotex.moms.Beskrivelse = infopos.mva.mvatekst
                       skotex.moms.MomsProc    = infopos.mva.mva NO-ERROR.
                RELEASE skotex.moms.
            END.
        END.
        RELEASE skotex.moms.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-OppdaterPost) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OppdaterPost Procedure 
PROCEDURE OppdaterPost :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    FOR EACH infopos.post NO-LOCK:
        FIND skotex.post WHERE 
             skotex.pos.postnr = STRING(infopos.post.postnr) NO-LOCK NO-ERROR.
        IF NOT AVAIL skotex.post THEN 
        DO:
            CREATE Skotex.post.
            ASSIGN Skotex.post.PostNr      = STRING(infopos.post.postnr)
                   Skotex.post.Beskrivelse = infopos.post.poststed NO-ERROR.
            IF ERROR-STATUS:ERROR THEN
                DELETE Skotex.post.
        END.
        ELSE IF SkoTex.Post.Beskrivelse <> infopos.post.poststed THEN 
        DO:
            FIND CURRENT Skotex.post EXCLUSIVE NO-WAIT.
            IF AVAIL Skotex.post THEN
                ASSIGN Skotex.post.Beskrivelse = infopos.post.poststed NO-ERROR.
        END.
        RELEASE Skotex.post.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

 
 
 
&IF DEFINED(EXCLUDE-OppdatertKasserere) = 0 &THEN
		
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OppdatertKasserere Procedure
PROCEDURE OppdaterKasserere:
	/*------------------------------------------------------------------------------
			Purpose:  																	  
			Notes:  																	  
	------------------------------------------------------------------------------*/

FOR EACH Kasserer NO-LOCK:
    CREATE Forsalj.
    ASSIGN
        Forsalj.FoAnstNr       = Kasserer.AnsNr
        Forsalj.AnsattNr       = STRING(Kasserer.AnsNr)        
        Forsalj.AnsattDato     = Kasserer.AnsDato                       
        forsalj.FoNamn         = Kasserer.Fornavn + ' ' +  Kasserer.EtterNavn
        
        Forsalj.FoAdr          = Kasserer.Adresse
        Forsalj.FoPoNr         = STRING(Kasserer.PostNr)
        
        Forsalj.NavnIKasse     = Kasserer.KassTekst
        Forsalj.FodtDato       = Kasserer.fDato
        
        forsalj.Rabatt         = Kasserer.GiRabatt
        Forsalj.Prisendring    = Kasserer.EndrePris
        Forsalj.Retur          = Kasserer.TaRetur
        Forsalj.SlettTidligere = Kasserer.Slettidl
        Forsalj.SletteForste   = Kasserer.Slettforst
        Forsalj.Butik          = ibutikkNr
        NO-ERROR.
    IF ERROR-STATUS:ERROR THEN
    DO:
        DELETE ButikkForsalj.
        NEXT.
    END.

   ASSIGN Forsalj.Passord = int(Kasserer.Passord) NO-ERROR.

   CREATE ButikkForsalj.
   ASSIGN
       ButikkForsalj.butik      = Kasserer.ButNr
       ButikkForsalj.ForsNr     = Forsalj.ForsNr
       ButikkForsalj.KassererId = Kasserer.KassNr
       NO-ERROR.
   IF ERROR-STATUS:ERROR THEN
       DELETE ButikkForsalj.
END.

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF



&IF DEFINED(EXCLUDE-oppdatKunder) = 0 &THEN
		
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE oppdatKunder Procedure
PROCEDURE oppdatKunder:
	/*------------------------------------------------------------------------------
			Purpose:  																	  
			Notes:  																	  
	------------------------------------------------------------------------------*/

DEF VAR iAnt AS INT NO-UNDO.
DEF VAR wRecid AS RECID NO-UNDO.

DEF BUFFER bufKunde FOR Skotex.Kunde.

FOR EACH InfoPOS.Kund NO-LOCK
  BREAK BY InfoPOS.Kund.KundeNr:
  
  IF FIRST-OF(InfoPOS.Kund.KundeNr) THEN 
  DO:
    iAnt = 1.
  END.
  ELSE iAnt = iant + 1.
  
  FIND FIRST kundekort NO-LOCK WHERE 
      KundeKort.KortNr = STRING(InfoPOS.Kund.KundeNr) NO-ERROR.
  
  /* TN Her opprettes kunder som ikke finnes fra før. */
  IF NOT CAN-FIND(FIRST KundeKort WHERE
                  KundeKort.KortNr = STRING(InfoPOS.Kund.KundeNr)) THEN 
  DO:
    FIND FIRST KundeGruppe NO-LOCK WHERE
       KundeGruppe.GruppeId > 0 NO-ERROR.
    CREATE SkoTex.Kunde.
    FIND FIRST SkoTex.KundeKort OF SkoTex.Kunde EXCLUSIVE-LOCK NO-ERROR.
    IF AVAILABLE SkoTex.KundeKort THEN DELETE  SkoTex.KundeKort.
  END.
  ELSE DO:
     FIND FIRST SkoTex.KundeKort WHERE
                SkoTex.KundeKort.KortNr = STRING(InfoPOS.Kund.KundeNr).
     FIND SkoTex.Kunde OF SkoTex.KundeKort.
  END.

  ASSIGN
     SkoTex.Kunde.Navn            = TRIM(TRIM(InfoPOS.Kund.navn,','))
     SkoTex.Kunde.BetType         = 2
     SkoTex.Kunde.ButikkNr        = iButikkNr
     SkoTex.Kunde.GruppeId        = KundeGr
     SkoTex.Kunde.SamleFaktura    = FALSE  
     SkoTex.Kunde.Fakturagebyr    = FALSE 
     SkoTex.Kunde.Purregebyr      = TRUE 
     SkoTex.Kunde.EksterntKundeNr = STRING(InfoPOS.Kund.KundeNr)
     /*SkoTex.Kunde.ePostAdresse    = InfoPOS.Kund.emailadr*/
     SkoTex.Kunde.Adresse1        = InfoPOS.Kund.adresse                 
     SkoTex.Kunde.PostNr          = STRING(InfoPOS.Kund.postnr)  
     SkoTex.Kunde.Telefon         = InfoPOS.Kund.Telefon1     
     SkoTex.Kunde.Telefaks        = InfoPOS.Kund.faxnr      
     SkoTex.Kunde.MobilTlf        = InfoPOS.Kund.Telefon2  
     SkoTex.Kunde.Aktiv           = NOT InfoPOS.Kund.Sperret
     SkoTex.Kunde.MaksKredit      = InfoPOS.Kund.limit
     SkoTex.Kunde.LevAdresse1     = InfoPOS.Kund.levadresse    
     SkoTex.Kunde.LevPostNr       = STRING(InfoPOS.Kund.LevPostNr)
     /*SkoTex.Kunde.FaktAdresse1    = InfoPOS.Kund.faktadresse */  
     /*SkoTex.Kunde.FaktPostNr      = STRING(InfoPOS.Kund.faktpostnr) */    
     .
    IF NOT CAN-FIND(FIRST SkoTex.KundeKort OF SkoTex.Kunde) THEN 
      RUN createKundekort.p (INPUT SkoTex.Kunde.Kundenr, INPUT STRING(InfoPOS.Kund.KundeNr), INPUT 720, OUTPUT wRecid).
END.

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF



 
&IF DEFINED(EXCLUDE-systemfix) = 0 &THEN
		
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE systemfix Procedure
PROCEDURE systemfix:
	/*------------------------------------------------------------------------------
			Purpose:  																	  
			Notes:  																	  
	------------------------------------------------------------------------------*/
  DO TRANSACTION:
    FIND SysPara EXCLUSIVE-LOCK WHERE
       SysPara.SysHId = 5 AND
       SysPara.SysGr  = 1 AND
       SysPara.ParaNr = 1 NO-ERROR.
    IF AVAILABLE SysPara THEN
      ASSIGN SysPara.Parameter1 = STRING(ibutikkNr).
    IF AVAILABLE SysPara THEN RELEASE syspara.
  END.

  FOR EACH jBoxCompanyUser:
    jBoxCompanyUser.iJBoxCompanyId = iButikkNr NO-ERROR.
  END.
  
  FOR EACH Butiker EXCLUSIVE-LOCK WHERE
    Butiker.Butik <> iButikkNr:
    FOR EACH Kasse WHERE 
      Kasse.butikkNr = Butiker.butik:
      DELETE Kasse.
    END.
    DELETE Butiker.
  END.
  FOR EACH Butiker WHERE Butiker.butik = ibutikkNr:
    FIND FIRST Prisprofil NO-LOCK NO-ERROR.
    ASSIGN
      Butiker.clButikkNr   = iButikkNr
      Butiker.Sentrallager = TRUE 
      Butiker.ProfilNr     = PrisProfil.ProfilNr
      .
  END.
  
  /* Sjekker at panteflagg er satt på alle pantevarer. */
  FOR EACH ArtBas EXCLUSIVE-LOCK WHERE
    ArtBas.LinkVare > 0:
    FIND bArtBas EXCLUSIVE-LOCK WHERE
      bArtBas.ArtikkelNr = ArtBas.LinkVareNr NO-ERROR.
    IF AVAILABLE bArtBas THEN 
      bArtBas.Pant = TRUE.
  END.
  
  DO TRANSACTION:
    FIND FIRST JBoxCompany EXCLUSIVE-LOCK NO-ERROR.
      IF AVAILABLE JBoxCompany THEN
      ASSIGN
        jBoxCompany.iJBoxCompanyId = iButikkNr
        jBoxCompany.Cl        = iButikkNr.
      IF AVAILABLE jBoxCompany THEN 
        RELEASE jBoxCompany.
        
    FIND LAST JboxCompany NO-LOCK NO-ERROR.
    IF AVAIL JboxCompany THEN CURRENT-VALUE(seqJboxCompanyId) = JBoxCompany.iJboxCompanyId.
    
    FIND LAST JboxFunctionAccess NO-LOCK NO-ERROR.
    IF AVAIL JboxFunctionAccess THEN CURRENT-VALUE(seqJboxFunctionAccessId)
    = JBoxFunctionAccess.iJboxFunctionAccessId.
    
    FIND LAST JboxFunction NO-LOCK NO-ERROR.
    IF AVAIL JboxFunction THEN CURRENT-VALUE(seqJboxFunctionId) = JBoxFunction.iJboxFunctionId.
    
    FIND LAST JboxGenCode NO-LOCK NO-ERROR.
    IF AVAIL JboxGenCode THEN CURRENT-VALUE(seqJboxGenCodeId) = JBoxGenCode.iJboxGenCodeId.
    
    FIND LAST JboxGenCodeType NO-LOCK NO-ERROR.
    IF AVAIL JboxGenCodeType THEN CURRENT-VALUE(seqJboxGenCodeTypeId) = JBoxGenCodeType.iJboxGenCodeTypeId.
    
    FIND LAST JboxMenu NO-LOCK NO-ERROR.
    IF AVAIL JboxMenu THEN CURRENT-VALUE(seqJboxMenuId) = JBoxMenu.iJboxMenuId.
    
    FIND LAST JboxTranslation NO-LOCK NO-ERROR.
    IF AVAIL JboxTranslation THEN CURRENT-VALUE(seqJboxTranslationId) = JBoxTranslation.iJboxTranslationId.
    
    FIND LAST JboxUserGroup NO-LOCK NO-ERROR.
    IF AVAIL JboxUserGroup THEN CURRENT-VALUE(seqJboxUserGroupId) = JBoxUserGroup.iJboxUserGroupId.
    
    FIND LAST JboxUserGroupMembers NO-LOCK NO-ERROR.
    IF AVAIL JboxUserGroupMembers THEN
    CURRENT-VALUE(seqJboxUserGroupMembersId) = JBoxUserGroupMembers.iJboxUserGroupMembersId.
    
    FIND LAST JboxUserMenu NO-LOCK NO-ERROR.
    IF AVAIL JboxUserMenu THEN CURRENT-VALUE(seqJboxUserMenuId) = JBoxUserMenu.iJboxUserMenuId.
    
    FIND LAST JboxUserSetting NO-LOCK NO-ERROR.
    IF AVAIL JboxUserSetting THEN CURRENT-VALUE(seqJboxUserSettingId) = JBoxUserSetting.iJboxUserSettingId.        
  END.
  
FOR EACH Butikkkobling:
    ASSIGN Butikkkobling.Butik = ibutikkNr NO-ERROR.
END.
FOR EACH ButikkForsalj:
    ASSIGN ButikkForsalj.Butik = ibutikkNr NO-ERROR.
END.
FOR EACH ButikkSelger:
    ASSIGN ButikkSelger.ButikkNr = iButikkNr NO-ERROR.
END.
FOR EACH ButikkTilgang:
    ASSIGN ButikkTilgang.Butik = iButikkNr NO-ERROR.
END.
FOR EACH SkoTex.Bruker:
    ASSIGN SkoTex.Bruker.ButikkNr = iButikkNr NO-ERROR.
END.
  
END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF



&IF DEFINED(EXCLUDE-TellOppLinjer) = 0 &THEN
		
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE TellOppLinjer Procedure
PROCEDURE TellOppLinjer:
	/*------------------------------------------------------------------------------
			Purpose:  																	  
			Notes:  																	  
	------------------------------------------------------------------------------*/
DEFINE VARIABLE cLinje AS CHARACTER NO-UNDO.

iTotAntall = 0.

INPUT FROM value(cFilNavn).
REPEAT:
  IMPORT cLinje.
  itotAntall = itotAntall + 1.
END.
INPUT CLOSE.

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF


