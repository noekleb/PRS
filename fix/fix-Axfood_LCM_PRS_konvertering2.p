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

DEFINE TEMP-TABLE tt_vare NO-UNDO
    FIELD ean AS DECI
    FIELD pris AS DECI
    INDEX ean IS PRIMARY UNIQUE ean.
    
def buffer bArtBas for ArtBas.    
    
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
  cVareFeil = "LCMVarerMedFeil_" + string(iButikkNr) + ".txt"
  .
  
IF SEARCH(cFilNavn) = ? THEN 
DO:
  MESSAGE 'Kan ikke finne varefil fra kassen i butikk ' + string(iButikkNr) + '.' SKIP 
          'Filnavn: ' cFilNavn
  VIEW-AS ALERT-BOX.
  RETURN.
END.
    
RUN oppdaterArtiklerFraTranslogg.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */
&IF DEFINED(EXCLUDE-oppdaterArtiklerFraTranslogg) = 0 &THEN
        
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE oppdaterArtiklerFraTranslogg Procedure
PROCEDURE oppdaterArtiklerFraTranslogg:
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

DEFINE BUFFER bufTransLogg FOR TransLogg.

RUN TellOppLinjer.


DISPLAY 
iantVare LABEL 'Antall behandlet' SKIP 
itotAntall LABEL 'Totalt antall varer' WITH FRAME g.
PAUSE 0.

FIND Butikk NO-LOCK WHERE
  Butikk.ButNr = iButikkNr NO-ERROR.

VARERAD:
FOR EACH TransLogg NO-LOCK WHERE
  TransLogg.ArtikkelNr = 0 AND 
  TransLogg.Postert = FALSE :
        
    ASSIGN 
      iTot = iTot + 1
      cEan = TransLogg.Kode.

    FIND Strekkode NO-LOCK WHERE
      Strekkode.Kode = cEAN NO-ERROR.
    IF AVAILABLE Strekkode THEN 
      DO:
        IF CAN-FIND(ArtBas WHERE 
                    ArtBas.ArtikkelNr = Strekkode.ArtikkelNr) THEN 
                    NEXT VARERAD.
      END.

    /* Strekkoder med feil */
    IF cEan BEGINS "7388" AND LENGTH(cEan) <> 13 THEN
        NEXT.
    IF cEan BEGINS "7388" AND SUBSTR(cEan,13) <> "0" THEN
        NEXT.
        
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
    AVAIL_VARE:
    DO:
        /* Henter prisen fra varefilen */
        cPris = REPLACE(TRIM(SUBSTR(cc,72)),".",",").
        dPris = DECI(cPris).

        FIND vargr WHERE vargr.vg = vare.hgr NO-LOCK NO-ERROR.
        FIND moms OF vargr NO-LOCK.
        FIND Jamforenhet WHERE JamforEnhet.JamforEnhId = vare.enhet NO-LOCK NO-ERROR.
        FIND FIRST pris WHERE pris.ean = vare.ean AND
                        Pris.LevNr > 0 AND 
                        pris.profnr    = Butikk.ProfNr NO-LOCK NO-ERROR.
        IF NOT AVAIL pris THEN
        FIND FIRST pris WHERE pris.ean = vare.ean AND
                        Pris.LevNr > 0 AND 
                        pris.profnr    = iButikkNr NO-LOCK NO-ERROR.
        IF NOT AVAIL pris THEN
            FIND FIRST pris WHERE pris.ean = vare.ean AND
                       Pris.LevNr > 0 NO-LOCK NO-ERROR.
        IF NOT AVAIL pris THEN 
        DO:
            PUT UNFORMATTED "Finner ingen pris på artikkelen. Oprettet med pris fra kasse og 0 i varekost: " cc SKIP.
            leave AVAIL_VARE.
        END.
    
        FIND Artbas WHERE artbas.artikkelnr = vare.ean EXCLUSIVE-LOCK NO-ERROR.
        IF NOT AVAIL artbas THEN 
        DO:
            CREATE artbas.
            ASSIGN artbas.artikkelnr   = vare.ean
                   artbas.vg           = vare.hgr.
        END.
        
  find Fabrikat of Vare no-lock no-error.

  if available Fabrikat then 
  do:
    if not can-find(VareMerke where VareMErke.VMId = Vare.FabrikatNr) then 
    do:
      create VareMerke.
      assign
      VareMerke.VmId = Vare.FabrikatNr
      VareMerke.Beskrivelse = Fabrikat.FabrikatNavn.
    end.
  end.
        
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
               artbas.NON_Sale     = vare.nonsale
               artbas.etikett      = IF (AVAILABLE Pris AND pris.etiant = 1) THEN 2 ELSE 0
               ArtBas.JamforEnhet  = IF AVAIL jamforenhet THEN JamforEnhet.JamforEnhet ELSE ""
/*                    artbas.negvare      = vare.ean >= 550 AND vare.ean <= 559 */
               artbas.kunderabatt  = vare.krabatt
               ArtBas.Mengde       = vare.mengde
               artbas.hkstyrt      = IF AVAILABLE Pris THEN pris.hkstyrt ELSE FALSE 
               ArtBas.Etikettekst1 = vare.etitekst1
               ArtBas.Etikettekst2 = vare.etitekst2
               ArtBas.SalgsEnhet   = IF artbas.artslag = 1 THEN "Kg" ELSE "Stk"
               ArtBas.AnbefaltPris = IF AVAILABLE Pris THEN Pris.VeilPris ELSE ArtBas.AnbefaltPris
               ArtBas.AntIPakn     = Vare.AntPkn
               ArtBas.Anbrekk      = Vare.Anbrekk
               ArtBas.VmId         = Vare.Fabrikat
               ArtBas.LokPris      = TRUE 
               .

        FIND bufTransLogg WHERE
          RECID(bufTransLogg) = RECID(Translogg) EXCLUSIVE-LOCK NO-ERROR.
        IF AVAILABLE bufTransLogg THEN 
        DO:
          ASSIGN bufTransLogg.ArtikkelNr = ArtBas.ArtikkelNr.
          RELEASE bufTransLogg.
        END.

        FIND ArtPris EXCLUSIVE-LOCK WHERE
          ArtPris.ArtikkelNr = ArtBas.ArtikkelNr AND 
          ArtPris.ProfilNr   = 1 NO-ERROR.
        IF NOT AVAILABLE ArtPris THEN 
        DO:
          CREATE artpris.
          ASSIGN artpris.artikkelnr      = artbas.artikkelnr
                 artpris.profilnr        = 1.
        END.
        ASSIGN artpris.ValPris[1]      = IF AVAILABLE Pris THEN pris.engrosn ELSE artpris.ValPris[1]
               artpris.InnkjopsPris[1] = IF AVAILABLE Pris THEN pris.engrosn ELSE artpris.InnkjopsPris[1]
               artpris.Varekost[1]     = IF AVAILABLE Pris THEN pris.engrosn ELSE artpris.Varekost[1]
               artpris.pris[1]         = IF AVAILABLE Pris THEN pris.utprisn ELSE artpris.pris[1]
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


