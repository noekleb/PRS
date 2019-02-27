&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        :  xinnn9boseljournal.p
    Purpose     :  Innlesning av xml kvitteringsfil fra Nucleus/Wayne kasse.

    Syntax      :

    Description :  

    Author(s)   :  Tom Nøkleby
    Created     :  10/06-06
    Notes       :
    
    
main
    initTT_Kasse   - Skapar inne och utekassor i TT
    xmlReadn9bos.p - Läser IN kvittofil i TT
    GetOldShifts   - Ser om skift finns sedan tidigare och kopplar mot gammalt skift
    InnLesFil      - Behandling av TT, inläsning gjord
        SjekkKasseNr
        CreateTmpBonghode
        CreateTmpBongLinje
    BehandlaData
        OppdaterFiler
        Skift_Bokfdag
        OpprettDatasett
        LagreBonger
        OpprettDatasettForPro
    

  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEF INPUT  PARAMETER lFilId      AS DEC    NO-UNDO.
DEF INPUT  PARAMETER h_Parent    AS HANDLE NO-UNDO.
DEF INPUT  PARAMETER h_Telleverk AS HANDLE NO-UNDO.
DEF OUTPUT PARAMETER iAntLinjer  AS INT    NO-UNDO.

DEF VAR cError          AS CHAR NO-UNDO.
DEF VAR piLoop1         AS INT  NO-UNDO.
DEF VAR cLinje          AS CHAR NO-UNDO.
DEF VAR cFilNavn        AS CHAR NO-UNDO.
DEF VAR cBongFil        AS CHAR NO-UNDO.

DEF VAR h_dproclib      AS HANDLE NO-UNDO.

/* Konvertering av transaksjonskoder */
DEF VAR cBONGLst        AS CHAR NO-UNDO.
DEF VAR cTTIDLst        AS CHAR NO-UNDO.
DEF VAR cPOSLst         AS CHAR NO-UNDO.
DEF VAR cPOS            AS CHAR NO-UNDO.
DEF VAR cTTID           AS CHAR NO-UNDO.

/* Felt for å holde info fra record Receipt Type */
DEF VAR iOperatorId        AS INT  NO-UNDO.
DEF VAR iTerminalNo        AS INT  NO-UNDO.
DEF VAR iOperatorShiftNo   AS INT  NO-UNDO.
DEF VAR iReceiptNumber     AS INT  NO-UNDO.
DEF VAR lReceiptTotAmaount AS DEC  NO-UNDO.
DEF VAR dReceiptDateTime   AS CHAR NO-UNDO.

DEF VAR pcBongLinje AS CHAR NO-UNDO.
DEF VAR pcPrefix    AS CHAR NO-UNDO.
DEF VAR pcRecord    AS CHAR NO-UNDO.
DEF VAR pcBeskr     AS CHAR NO-UNDO.
DEF VAR cStrekKode  AS CHAR NO-UNDO.
DEF VAR cOrgStrekkode AS CHAR NO-UNDO.
DEF VAR cTekst      AS CHAR NO-UNDO.
DEF VAR piHuvGr     AS INT  NO-UNDO.
DEF VAR plEnhPris   AS DEC  FORMAT "->>>>>>>>9.99" NO-UNDO.
DEF VAR plQuantity  AS DEC  NO-UNDO.
DEF VAR plAmount    AS DEC  FORMAT "->>>>>>>>9.99" NO-UNDO.
DEF VAR piMvaKode   AS INT  NO-UNDO.
DEF VAR plMva%      AS DEC  FORMAT "->>9.99"       NO-UNDO.
DEF VAR plMvaKr     AS DEC  FORMAT "->>>>>>>>9.99" NO-UNDO.
DEF VAR plDiscount  AS DEC  FORMAT "->>>>>>>>9.99" NO-UNDO.
DEF VAR plSubTotal  AS DEC  FORMAT "->>>>>>>>9.99" NO-UNDO.
DEF VAR pbDropp     AS LOG  NO-UNDO.
DEF VAR bMakulert   AS LOG  NO-UNDO.
DEF VAR piCardMode  AS INT  NO-UNDO.

DEF VAR piAntBonger AS INT NO-UNDO.
DEF VAR plPrisprsalgsenhet AS DECI NO-UNDO.

DEF VAR lArtikkelNr     AS DEC  FORMAT ">>>>>>>>>>>>9" NO-UNDO.
DEF VAR lEAN            AS DEC  FORMAT ">>>>>>>>>>>>9" NO-UNDO.
DEF VAR iButikkNr       AS INT  NO-UNDO.
DEF VAR iGruppeNr       AS INT  NO-UNDO.
DEF VAR iKasseNr        AS INT  NO-UNDO.
DEF VAR iKassererNr     AS INT  NO-UNDO.
DEF VAR cInnKvittering  AS CHAR NO-UNDO.
DEF VAR iTotAntLinjer   AS INT  NO-UNDO.
DEF VAR cDatoListe      AS CHAR NO-UNDO.
DEF VAR cKontrolltabell AS CHARACTER  NO-UNDO. /* MottaksKontroll av vilken data vi skall testa mot */
DEF VAR iSequenceNbr    AS INT  NO-UNDO.
DEF VAR cReceiptType    AS CHAR NO-UNDO.
DEF VAR cKonvArt        AS CHAR NO-UNDO.
DEF VAR iSchemaFelAar   AS INTE NO-UNDO. /* Om vi får fel i getOKschema(dato,output iSchemaFelAar)  vet vi vilket år */
DEF VAR lSchemaOK       AS LOG NO-UNDO.
DEF VAR cOutputDir      AS CHAR NO-UNDO.
DEFINE VARIABLE lKampanjFinns AS LOGICAL     NO-UNDO.
DEFINE VARIABLE cReturVerdi AS CHARACTER  NO-UNDO.
DEFINE VARIABLE iAntBonger AS INTEGER    NO-UNDO.
DEFINE VARIABLE cReturnStatus AS CHARACTER   NO-UNDO. /* från xml-läsning */
DEFINE VARIABLE lFirstBetallinje AS LOGICAL    NO-UNDO.  /* skall användas för att läggas i 'bonglinje.forkonvertering' */
                                                         /* första betallinjen får JA för att vi skall kunna hantera riktigt */
                                                         /* när vi uppdaterar KD_Data. Om 'JA' och kort skall vi ta hand om volym på kort */

DEF STREAM InnFil.
DEF STREAM Bong.


DEF TEMP-TABLE tmpDataDato NO-UNDO
    FIELD dato AS DATE
    FIELD doDatasett AS LOGICAL
    INDEX dato dato.

DEF BUFFER bufButiker FOR Butiker.
DEF TEMP-TABLE tmpBongHode  NO-UNDO LIKE BongHode.
DEF TEMP-TABLE tmpBongLinje NO-UNDO LIKE BongLinje
    FIELD cPOS AS CHAR.

DEF TEMP-TABLE TT_kasse NO-UNDO
    FIELD uteinne AS CHAR
    FIELD kassenr AS INTE
    INDEX uk IS PRIMARY UNIQUE uteinne kassenr.


DEF BUFFER btmpBongHode FOR tmpBongHode.

DEFINE TEMP-TABLE TT_Error NO-UNDO
    FIELD BongNr  AS INTE
    FIELD cError  AS CHAR.

DEFINE TEMP-TABLE tt_tilbud NO-UNDO
    FIELD kampid               AS DECIMAL
    FIELD kamptilbid           AS INTEGER
    FIELD ProdFamId            AS DECIMAL
    FIELD KampTilbArtMinAntall AS DECI
    FIELD Solgtantall          AS DECI
    FIELD Namn                 AS CHAR
    FIELD Pris                 AS DECI /* egentligen hela tillbudets pris, shortcut */
    INDEX kamp IS PRIMARY kampid kamptilbid prodfamid.

DEFINE TEMP-TABLE tt_tilbudart NO-UNDO
    FIELD ProdFamId            AS DECIMAL
    FIELD Artikkelnr           AS DECI
    INDEX art IS PRIMARY UNIQUE ProdFamId Artikkelnr.

{xmln9bos.i &NEW=NEW}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&IF DEFINED(EXCLUDE-finnesPROdatasett) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD finnesPROdatasett Procedure 
FUNCTION finnesPROdatasett RETURNS LOGICAL
  ( INPUT dStartDato AS DATE, OUTPUT dCreateDato AS DATE)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-fixChkEAN) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fixChkEAN Procedure 
FUNCTION fixChkEAN RETURNS CHARACTER
    ( INPUT cKode AS CHARACTER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getOKschema) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getOKschema Procedure 
FUNCTION getOKschema RETURNS LOGICAL
  ( INPUT bongdato AS DATE, OUTPUT iAr AS INTEGER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF


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
         HEIGHT             = 21.52
         WIDTH              = 77.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

ASSIGN
    cBONGLst  = "0102,1202,1302,1402,1502" 
    cPOSLst   = ""
    cTTIDLst  = ""
    cKonvArt  = "1,2,70,71,72,73,74,75,76,77,78,79,80,81,82,84,85,86,87,88,89"
    .

RUN NyFilLogg IN h_Parent (INPUT lFilId, STRING(TODAY) + " " + 
          STRING(TIME,"HH:MM:SS") + " " + USERID("skotex") + 
         " - xinnn9boseljournal.p startet.").

FIND Filer NO-LOCK WHERE
    Filer.FilId = lFilId NO-ERROR.
IF NOT AVAILABLE Filer THEN
DO:
    RUN NyFilLogg IN h_Parent (INPUT lFilId, STRING(TODAY) + " " + 
              STRING(TIME,"HH:MM:SS") + " " + USERID("skotex") + 
             " -  Ukjent post 'filer' (" + STRING(lFilId) + ")." + 
             CHR(1) + "1") NO-ERROR.

    RETURN " ** Ukjent Filer post (" + STRING(lFilId) + ").".
END.
ASSIGN
    cFilNavn  = Filer.Katalog + "~\" + Filer.FilNavn
    iButikkNr = int(SUBSTRING(ENTRY(2,Filer.FilNavn,"_"),1,5))
    NO-ERROR.
FIND bufButiker NO-LOCK WHERE
    bufButiker.Butik = iButikkNr NO-ERROR.
IF ERROR-STATUS:ERROR THEN
DO:
    RUN NyFilLogg IN h_Parent (INPUT lFilId, STRING(TODAY) + " " + 
              STRING(TIME,"HH:MM:SS") + " " + USERID("skotex") + 
             " -  Ukjent butikknummer " + ENTRY(2,Filer.FilNavn,"_") + " | " + Filer.FilNavn + ")." + 
             CHR(1) + "1") NO-ERROR.

    RETURN " ** Ukjent butikknr " + ENTRY(2,Filer.FilNavn,"_") + " | " + Filer.FilNavn + ").".
END.
RUN initTT_Kasse.

/* Leser inn fil og legger linjene inn i tmpFiles.                   */
/* Videre behandling av bongene gjøres med utgangspunkt i tmpFilene. */

RUN KonverterN9XML.p (INPUT cFilNavn) NO-ERROR.

RUN xmlReadn9bos.p (INPUT iButikkNr, INPUT cFilNavn).
ASSIGN cReturnStatus = RETURN-VALUE.
{syspara.i 1 1 25 cKontrolltabell}
IF NOT CAN-DO("1,2",cKontrolltabell) THEN
    ASSIGN cKontrolltabell = "1".

/* vi kör inte tellopplinjer!! */
/* RUN TellOppLinjer. */
IF NOT VALID-HANDLE(h_dproclib) THEN
    RUN dproclib PERSISTENT SET h_dproclib.
RUN GetOldShifts.

RUN InnLesFil.    /* El-Journal. */

IF cReturnStatus = "OK" AND NOT CAN-FIND(FIRST tt_receipt) THEN
    RUN TomDag(cFilNavn).

/* Kontrollera mot öppetschema */
FIND FIRST tmpDataDato NO-ERROR.
IF AVAIL tmpDataDato THEN
    ASSIGN lSchemaOK = getOKschema(tmpDataDato.dato,OUTPUT iSchemaFelAar).
IF AVAIL tmpDataDato AND lSchemaOK THEN DO:
    RUN BehandlaData.
END.
ELSE IF AVAIL tmpDataDato THEN DO:
    ASSIGN cReturVerdi = "Data saknas i filmottak".
/*     MESSAGE cReturverdi                    */
/*         VIEW-AS ALERT-BOX INFO BUTTONS OK. */
/*     MESSAGE tmpDataDato.dato SKIP lSchemaOK SKIP iSchemaFelAar */
/*         VIEW-AS ALERT-BOX INFO BUTTONS OK.                     */
END.
ELSE DO:
/*     MESSAGE "Ingen tmpdata"                */
/*         VIEW-AS ALERT-BOX INFO BUTTONS OK. */
    ASSIGN cReturVerdi = "". /* tomma filer backuppas */
END.
IF CAN-FIND(FIRST TT_Error) THEN
    RUN SaveError.
IF VALID-HANDLE(h_dproclib) THEN
    DELETE PROCEDURE h_dproclib.

RETURN cReturVerdi.
.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-Article) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Article Procedure 
PROCEDURE Article :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cItemIdTxt AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cTmp7388   AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE iPrisEntry AS INTEGER     NO-UNDO.
  ON CREATE OF strekkode OVERRIDE DO: END.
  ON DELETE OF strekkode OVERRIDE DO: END.
  ON WRITE OF strekkode  OVERRIDE DO: END.  
  IF TRUE THEN
      pcBeskr = "Ukjent drivstoff artikkel".
  ELSE
      pcBeskr = "Ukjent artikkel".

  IF AVAILABLE ArtBas THEN
      RELEASE ArtBas.

  /* Trekker ut informasjon fra strengen */
  ASSIGN 
      piHuvGr       = TT_ItemLine.EFTCodeId
      lArtikkelNr   = TT_ItemLine.ItemId
      lEAN          = TT_ItemLine.BarCode /* den finns bara om varan scannats */
      plEnhPris     =  0
      plQuantity    = TT_ItemLine.Quantity
      plAmount      = TT_ItemLine.Amount
      piMvaKode     = IF TT_ItemLine.TaxClassId > 9 THEN 9 ELSE TT_ItemLine.TaxClassId
      plMvaKr       = TT_ItemLine.TaxAmount
      plDiscount    = TT_ItemLine.Man_Discount + TT_ItemLine.Prom_Discount + TT_ItemLine.Pack_discount
      cItemIdTxt    = IF TT_ItemLine.ItemId >= 1 AND TT_ItemLine.ItemId <= 20 THEN "ITEMID=" + STRING(TT_ItemLine.ItemId) ELSE ""
/*       cItemIdTxt    = IF TT_ItemLine.ItemId >= 1 AND TT_ItemLine.ItemId <= 20 AND CAN-DO("1,2,3,4,5,6,7,8,9,10,11,12,13,20",STRING(TT_ItemLine.ItemId)) THEN "ITEMID=" + STRING(TT_ItemLine.ItemId) ELSE "" */
      cItemIdTxt    = IF cItemIdTxt = "" AND CAN-DO("70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89",STRING(piHuvGr)) THEN "ITEMID=" + STRING(TT_ItemLine.ItemId) ELSE cItemIdTxt
/*       cItemIdTxt    = IF TT_ItemLine.ItemId >= 1 AND TT_ItemLine.ItemId <= 10 THEN "ITEMID=" + STRING(TT_ItemLine.ItemId) ELSE ""                                                           */
/*       cItemIdTxt    = IF cItemIdTxt = "" AND CAN-DO("70,71,72,73,74,75,76,77,78,79,80,81,82,84,85,86,87,88,89",STRING(piHuvGr)) THEN "ITEMID=" + STRING(TT_ItemLine.ItemId) ELSE cItemIdTxt */
      cItemIdTxt    = IF TRIM(TT_Receipt.receiptType) = "void" OR plAmount + plDiscount = 0 THEN "" ELSE cItemIdTxt
          .
  /*
    cKonvArt  = "1,2,70,71,72,73,74,75,76,77,78,79,80,81,82,84,85,86,87,88,89"  
  */
  IF CAN-DO(cKonvArt,STRING(piHuvGr)) THEN
      lEAN = dec("9000" + string(piHuvGr,"99")).
  ELSE IF (lEAN > 900000 AND lEAN < 900200) OR (lArtikkelNr > 900000 AND lArtikkelNr < 900200) THEN DO:
      ASSIGN lEAN = 0
             lArtikkelNr = 0.
  END.
  RELEASE StrekKode.
  RELEASE ArtBas.

  /* Beregner Mva% */
  ASSIGN
      plMva% = ROUND(((abs(plAmount) / (abs(plAmount) - abs(plMvaKr))) - 1) * 100,0)
      plMva% = IF plMva% = ? THEN 0 ELSE plMva%
      .
  
  /* Henter artikkel på grunnlag av strekkoden */
  IF lEAN > 0 THEN  
  DO:
      ASSIGN cStrekkode = STRING(lEAN).
      IF LENGTH(cStrekkode) > 7 AND LENGTH(cStrekkode) < 13 THEN
        cStrekkode = FILL("0",13 - LENGTH(cStrekkode)) + cStrekkode.
      FIND StrekKode WHERE StrekKode.kode = cStrekKode NO-LOCK NO-ERROR.
      IF NOT AVAIL StrekKode THEN DO:
          IF cStrekKode BEGINS "0" THEN DO:
            cStrekkode = LEFT-TRIM(cStrekkode,"0").
            FIND StrekKode WHERE StrekKode.Kode = cStrekKode NO-LOCK NO-ERROR.
          END.
          IF NOT AVAIL StrekKode AND  (cStrekkode BEGINS "7388" OR cStrekkode BEGINS "20") AND LENGTH(cStrekkode) = 13 THEN DO:
              IF cStrekkode BEGINS "7388" THEN DO:
                  cStrekkode = SUBSTR(cStrekKode,1,8) + "00000".
                  cTmp7388   = cStrekkode.
                  cStrekkode = DYNAMIC-FUNCTION('fixChkEAN':U,INPUT SUBSTR(cStrekkode,1,12)).
                  IF NOT CAN-FIND(strekkode WHERE strekkode.kode = cStrekkode) THEN
                      cStrekkode = cTmp7388.
              END.
              ELSE DO:
                  ASSIGN cStrekKode = SUBSTR(cStrekKode,1,8) + "00000"
                         cStrekkode = DYNAMIC-FUNCTION('fixChkEAN':U,INPUT SUBSTR(cStrekkode,1,12)).
              END.
              FIND StrekKode WHERE StrekKode.kode = cStrekKode NO-LOCK NO-ERROR.
          END.
      END.
      IF AVAILABLE Strekkode THEN
          FIND ArtBas OF Strekkode NO-LOCK NO-ERROR.
  END.

  /* EAN kode ikke angitt, eller ukjent. Da skal koden hentes på artikkelnummer */
  IF lEAN = 0 OR NOT AVAILABLE ArtBas THEN
  DO:
      FIND ArtBas NO-LOCK WHERE
          ArtBas.ArtikkelNr = lArtikkelNr NO-ERROR.
      IF AVAILABLE ArtBas THEN
          FIND FIRST Strekkode OF ArtBas NO-LOCK NO-ERROR.
      IF AVAILABLE Strekkode THEN 
          ASSIGN
              lEAN       = DEC(Strekkode.Kode)
              cStrekkode = Strekkode.Kode.
      ELSE /* Ukjent strekkode. Da setter vi artikkelnummeret inn i strekkodefeltet */
          ASSIGN cStrekkode = IF lEAN <> 0 THEN STRING(lEAN) ELSE STRING(lArtikkelNr)
                 pcBeskr    = (IF lEAN <> 0 THEN "**" + pcBeskr + " - " + "Art.nr istedenfor EAN" ELSE pcBeskr).
      IF AVAILABLE Strekkode THEN
          FIND ArtBas OF Strekkode NO-LOCK NO-ERROR.
  END.
  
  /* Er varen ukjent - skal den legges på samleartikkel. */
  IF NOT AVAILABLE ArtBas THEN
  POSTER-PA-SAMLEARTIKKEL:
  DO:
      IF piHuvGr > 99 OR NOT CAN-FIND(HuvGr WHERE HuvGr.Hg = piHuvGr) THEN
          ASSIGN piHuvGr = 24.
      IF TT_ItemLine.BarCode = 0 THEN
          lEAN = DEC("9000" + string(piHuvGr,"99")). /* PLU vare */
      ELSE 
          lEAN = DEC("9001" + string(piHuvGr,"99")). /* Scanning vare */
      ASSIGN   
          cStrekkode = STRING(lEAN)
          .
      /* Sjekker rått. */
      FIND Strekkode NO-LOCK WHERE Strekkode.Kode = cStrekkode NO-ERROR.
      IF AVAILABLE Strekkode THEN
          FIND ArtBas OF Strekkode NO-LOCK NO-ERROR.
      /* Logg strekkoden på samleartikkelen. Plu skal ikke logges. */
      IF AVAILABLE ArtBas AND TT_ItemLine.BarCode > 0 THEN
      LOGG-STREKKODE:
      DO:
          cStrekkode = STRING(TT_ItemLine.BarCode).
          IF LENGTH(cStrekkode) > 7 THEN
            cStrekkode =  FILL("0",13 - LENGTH(cStrekkode)) + cStrekkode.
          IF cStrekkode BEGINS "7388" AND LENGTH(cStrekkode) > 7 THEN DO:
              cStrekkode = SUBSTR(cStrekKode,1,8) + "00000".
              cStrekkode = DYNAMIC-FUNCTION('fixChkEAN':U,INPUT SUBSTR(cStrekkode,1,12)).
          END.
          CREATE Strekkode.
          ASSIGN
              Strekkode.ArtikkelNr = ArtBas.ArtikkelNr
              Strekkode.Kode       = cStrekkode
              Strekkode.StrKode    = 1
              Strekkode.KodeType   = 1
/*               Strekkode.VareId     = ArtBas.ArtikkelNr */
              Strekkode.IKasse     = FALSE
              Strekkode.RegistrertDato = TODAY 
              Strekkode.RegistrertTid  = TIME 
              Strekkode.RegistrertAv   = USERID('SkoTex')
              Strekkode.EDato          = TODAY
              Strekkode.ETid           = TIME 
              Strekkode.BrukerId       = USERID('SkoTex')
              NO-ERROR.
          IF ERROR-STATUS:ERROR AND AVAILABLE Strekkode THEN
              DELETE STrekkode.
          IF AVAILABLE Strekkode THEN
              RELEASE Strekkode.
      END. /* LOGG-STREKKODE */
  END. /* POSTER-PA-SAMLEARTIKKEL */
  /* FEIL - Ukjent artikkel */
  IF lEAN = 0 THEN
  DO:
      RUN NyFilLogg IN h_parent (INPUT lFilId, STRING(TODAY) + " " + 
                      STRING(TIME,"HH:MM:SS") + " " + USERID("skotex") + 
                     " - Ukjent artikkel på linje " + STRING(iAntLinjer + 1) + ". ArtNr/EAN: " + string(lArtikkelNr) + "/" + string(lEAN) + "." + 
                     CHR(1) + "2").
  END.

  /* Plukker frisk info fra artikkelen */
  RELEASE VarGr.
  RELEASE HuvGr.
  IF AVAILABLE ArtBas THEN
  DO:
/*       FIND ArtPris NO-LOCK WHERE                             */
/*           ArtPris.ArtikkelNr = ArtBas.ArtikkelNr AND         */
/*           ArtPris.ProfilNr   = bufButiker.ProfilNr NO-ERROR. */
      FIND ArtPris NO-LOCK WHERE
          ArtPris.ArtikkelNr = ArtBas.ArtikkelNr AND
          ArtPris.ProfilNr   = 1 NO-ERROR.  /* 17/11-15 ken1 */
      IF NOT AVAILABLE ArtPris THEN
          FIND FIRST ArtPris WHERE
          ArtPris.ArtikkelNr = ArtBas.ArtikkelNr NO-LOCK NO-ERROR.
      iPrisEntry = 1.
      IF AVAIL Artpris AND artpris.tilbud = TRUE THEN DO:   /* 17/11-15 ken1 trippelkoll att den är aktiv */
          IF artpris.tilbudfradato <> ? AND artpris.tilbudfradato <= tmpBonglinje.dato AND ArtPris.TilbudTilDato <> ? AND ArtPris.TilbudTilDato >= tmpBongLinje.Dato THEN
              iPrisEntry = 2.
      END.
      ASSIGN
      /*piHuvGr   = ArtBas.Vg*/
/*       plEnhPris = IF AVAILABLE ArtPris THEN ArtPris.Varekost[1] ELSE 0 */
      plEnhPris = IF AVAILABLE ArtPris THEN ArtPris.Varekost[iPrisEntry] ELSE 0   /* 17/11-15 ken1 */
      pcBeskr   = TRIM(ArtBas.Beskr)
      .
      FIND VarGr OF ArtBas NO-LOCK NO-ERROR.
/*       IF AVAIL VarGr THEN       */
/*           FIND HuvGr OF ArtBas. */
      FIND HuvGr WHERE HuvGr.Hg = piHuvGr NO-LOCK NO-ERROR.
  END.
  ELSE
      ASSIGN
          plEnhPris = 0
          .

  /* Kommer salget fra en seddelautomat skal solgt beløp alltid avrundes til */
  /* nærmeste hele krone alltid nedover.                                     */
/*   IF substring(tmpFilLinjer.Tekst,6,2) = "13" THEN */
/*       plAmount = TRUNCATE(plAmount,0).             */

  /* PANT                                                                  */
  /* Er antall positivt, men beløp negativt, er det PANT.                  */
  /* RETUR legges ut med både antall og beløp negativt på varelinjen.      */
  IF plAmount < 0 AND plQuantity > 0 THEN
  BETALING-MED-PANT:
  DO:
      ASSIGN
          pcBeskr    = "PANT" + STRING(plAmount)
          plAmount   = plAmount * -1
          plQuantity = plQuantity * -1
          .
  END. /* BETALING-MED-PANT */
  FIND Moms WHERE moms.momskod = piMvaKode NO-LOCK NO-ERROR.
  ASSIGN
    tmpBongLinje.ArtikkelNr = IF AVAIL ArtBas THEN STRING(ArtBas.ArtikkelNr) ELSE STRING(TT_ItemLine.ItemId)
    tmpBongLinje.Storrelse  = "1"
    tmpBongLinje.Antall     = TT_ItemLine.Quantity
    /* tmpBongLinje.TBId */
    tmpBongLinje.VareGr     = IF AVAIL VarGr THEN VarGr.Vg ELSE IF AVAIL ArtBas THEN ArtBas.Vg ELSE TT_ItemLine.EFTCodeId
    tmpBongLinje.VareGruppeNavn = IF AVAIL VarGr THEN VarGr.Vgbeskr ELSE ""
    tmpBongLinje.BongTekst  = IF AVAIL ArtBas THEN ArtBas.Bongtekst ELSE "*Ukjent"
    tmpBongLinje.LinjeRab   = plDiscount
    tmpBongLinje.SubtotalRab = 0
    tmpBongLinje.MvaGr       = piMvaKode
    tmpBongLinje.MvaGruppeNavn = IF AVAIL Moms THEN Moms.beskrivelse ELSE STRING(plMva%,">>9.99") + "%"
    tmpBongLinje.Mva%     = IF AVAIL Moms THEN Moms.MomsProc ELSE plMva%
    tmpBongLinje.MvaKr    = plMvaKr
    tmpBongLinje.BongPris = TT_ItemLine.Amount + plDiscount
    tmpBongLinje.LinjeSum = tmpBongLinje.BongPris
    tmpBongLinje.LopeNr   = IF AVAIL ArtBas THEN ArtBas.Lopnr ELSE tmpBongLinje.Lopenr
    tmpBongLinje.Makulert = TRIM(TT_Receipt.receiptType) = "void"
/*     tmpBongLinje.HovedGr  = IF AVAIL HuvGr THEN HuvGr.hg ELSE TT_ItemLine.EFTCodeId */
    tmpBongLinje.HovedGr            = piHuvGr
    tmpBongLinje.HovedGrBeskrivelse = IF AVAIL HuvGr THEN HuvGr.hgbeskr ELSE "*Ukjent*"
    tmpBongLinje.VVarekost = IF plEnhPris = 0 THEN plEnhPris ELSE IF AVAIL ArtBas AND ArtBas.OPris = FALSE THEN plEnhPris * ABS(tmpBongLinje.Antall) ELSE IF AVAIL ArtBas THEN  
                                  ROUND((TT_ItemLine.Amount - plMvaKr) * (plEnhPris / 100),2) ELSE plEnhPris
    tmpBongLinje.Strekkode      = cStrekKode
    tmpBongLinje.GenerellRabatt = plDiscount
    tmpBongLinje.PrisPrSalgsenhet = TT_ItemLine.UnitPrice
    tmpBongLinje.KampId       = TT_ItemLine.Prom_CampaignId
    tmpBongLinje.KampTilbId   = TT_ItemLine.Prom_PromotionId
    tmpBongLinje.KampEierId   = IF TT_ItemLine.Prom_CampaignId <> 0 AND TT_ItemLine.Prom_CampaignOwnerId = 0 THEN bufButiker.butik ELSE TT_ItemLine.Prom_CampaignOwnerId
    tmpBongLinje.OriginalData = cItemIdTxt
        .

    IF cItemIdTxt <> "" AND AVAIL tmpBongHode AND tmpBongHode.Eksportertdato <> ? THEN
        tmpBongHode.Eksportertdato = ?.
    IF tmpBongLinje.KampEierId = TT_ItemLine.Prom_CampaignOwnerId THEN
        ASSIGN lKampanjFinns = TRUE. /* vi flaggar om tillslag på kampanj i TT_ItemLine */

    /* tmpBongLinje.RefTekst ?? */
    /* tmpBongLinje.RefNr    ?? */

/*   ENTRY(1,pcRecord,";") = IF tmpFilLinjer.Makulert THEN "1003" ELSE "3". /* Varesalg eller makulering */  */
/*   ENTRY(2,pcRecord,";") = trim(cStrekkode). /* EAN koden */                                               */
/*   ENTRY(3,pcRecord,";") = string(piVarGr). /* Varegruppen */                                              */
/*   ENTRY(4,pcRecord,";") = '"' + pcBeskr + '"'. /* Varetekst */                                            */
/*   ENTRY(5,pcRecord,";") = trim(replace(string(plQuantity,"->>>>>>>>9.999"),",",".")). /* Antall/mengde */ */
/*   ENTRY(6,pcRecord,";") = trim(replace(string(plAmount,"->>>>>>>>9.99"),",",".")). /* Beløp */            */
/*   ENTRY(7,pcRecord,";") = trim(replace(string(plEnhPris,"->>>>>>>>9.99"),",",".")). /* Kostpris */        */
/*   ENTRY(8,pcRecord,";") = trim(replace(string(plMva%,"->>>9.99"),",",".")). /* Mva% */                    */
/*   ENTRY(9,pcRecord,";") = trim(replace(string(plMvaKr,"->>>>>>>>9.99"),",",".")). /* MvaKr */             */
/*   ENTRY(10,pcRecord,";") = "0". /* Feilkode */                                                            */
/*   ENTRY(11,pcRecord,";") = "0". /* Tiltakskode */                                                         */
/*   ENTRY(12,pcRecord,";") = IF plDiscount = 0 THEN "0" ELSE "9". /* Salgstype - Linjerabatt*/              */
/*   ENTRY(13,pcRecord,";") = trim(replace(string(plDiscount,"->>>>>>>>9.99"),",",".")). /* Avslag */        */
/*   ENTRY(14,pcRecord,";") = "0". /* Salgstype */                                                           */
/*   ENTRY(15,pcRecord,";") = "0". /* Avslag */                                                              */
/*   ENTRY(16,pcRecord,";") = "0". /* Salgstype */                                                           */
/*   ENTRY(17,pcRecord,";") = "0". /* Avslag */                                                              */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-BehandlaData) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE BehandlaData Procedure 
PROCEDURE BehandlaData :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF VAR lSlettBFdagar   AS LOG NO-UNDO.
    DEF VAR cDatasettIDlist AS CHAR NO-UNDO.
    DEF VAR dDato AS DATE NO-UNDO.
    FOR EACH TT_Period BREAK BY TT_Period.TermDatoTid: /* skall bara kunna vara 1 !!! */
        IF LAST-OF(TT_Period.TermDatoTid) THEN DO:
            dDato = TT_Period.Dato.
        END.
    END.
    IF NOT AVAIL bufButiker THEN
        FIND bufButiker WHERE bufButiker.butik = iButikknr NO-LOCK.
/*     RUN OppdaterFiler. Flyttat längre ner */
    /* här skall vi testa om vi är i kommissionsläge */
    IF bufButiker.kommisjonsdato <> ? THEN DO:
        IF dDato = ? OR dDato >= bufButiker.kommisjonsdato THEN DO:
            RUN Skift_Bokfdag.
            IF dDato = bufButiker.kommisjonsdato THEN
                ASSIGN lSlettBFdagar = TRUE.
        END.
    END.
    /* nästa 2 rader skall tas bort när vi kör igång det som står oven */
/*     IF bufButiker.EODRapporter = TRUE THEN */
/*         RUN Skift_Bokfdag.                 */
    RUN OpprettDatasett.
    RUN LagreBonger(OUTPUT cDatasettIDlist).
    RUN OppdaterFiler. /* Flyttat hit */
    RUN OppdaterPRdata (INPUT cDatasettIDlist).
/*     RUN OpprettDatasettForPro. */
/* Detta skall in när vi börja det nya */
    IF lSlettBFdagar = TRUE AND CAN-FIND(FIRST Bokforingsdag WHERE Bokforingsdag.ButikkNr = iButikkNr AND
                                         Bokforingsdag.Dato = bufButiker.kommisjonsdato) THEN
        RUN SlettBokforingsdag (bufButiker.kommisjonsdato).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ByggTilbud) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ByggTilbud Procedure 
PROCEDURE ByggTilbud :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER dKampid     AS DECIMAL     NO-UNDO.
    DEFINE INPUT  PARAMETER iKampTilbid AS INTEGER     NO-UNDO.
    DEFINE OUTPUT PARAMETER lOK         AS LOGICAL     NO-UNDO.
    DEFINE        VARIABLE  dPris       AS DECIMAL     NO-UNDO.
    DEFINE        VARIABLE  lRet        AS LOGICAL     NO-UNDO.
    DEFINE        VARIABLE  cReturn     AS CHARACTER   NO-UNDO.
    
    FIND kampanjetilbud WHERE kampanjetilbud.kampid     = dkampid AND
                              kampanjetilbud.kamptilbid = iKampTilbid NO-LOCK NO-ERROR.
    IF NOT AVAIL kampanjetilbud THEN
            lOK = FALSE.
    ELSE DO:
/*         CREATE tt_kampanjer.                                        */
/*         ASSIGN tt_kampanjer.kampid     = preemanalysembr.kampid     */
/*                tt_kampanjer.kamptilbid = preemanalysembr.kamptilbid */
/*                tt_kampanjer.radnr      = preemanalysembr.radnr.     */
        ASSIGN dPris = KampanjeTilbud.KampTilbBelop.
        IF dPris = 0 THEN DO:
            RUN getkampanjeinfo.p (STRING(dkampid) + "|" + STRING(iKampTilbid),?,"",OUTPUT cReturn,OUTPUT lRet).
            IF lRet THEN
                dPris = DECI(ENTRY(3,cReturn,"|")).
        END.
        FOR EACH KampanjeTilbArtikkel OF kampanjetilbud NO-LOCK:
            IF NOT CAN-FIND(tt_tilbud WHERE tt_tilbud.kampid = KampanjeTilbArtikkel.kampid   AND
                            tt_tilbud.kamptilbid           = KampanjeTilbArtikkel.kamptilbid /*AND
                            tt_tilbud.ProdFamId            = KampanjeTilbArtikkel.ProdFamId*/) THEN DO:
                CREATE tt_tilbud.
                ASSIGN tt_tilbud.kampid               = KampanjeTilbArtikkel.kampid
                       tt_tilbud.kamptilbid           = KampanjeTilbArtikkel.kamptilbid
                       tt_tilbud.ProdFamId            = KampanjeTilbArtikkel.ProdFamId
                       tt_tilbud.KampTilbArtMinAntall = KampanjeTilbArtikkel.KampTilbArtMinAntall
                       tt_tilbud.Namn                 = KampanjeTilbud.KampTilbNavn
                       tt_tilbud.Pris                 = dPris.
            END.
            IF KampanjeTilbArtikkel.KampTilbArtId <> 0 THEN DO:
                IF NOT CAN-FIND(tt_tilbudart WHERE tt_tilbudart.prodfamid  = KampanjeTilbArtikkel.ProdFamId AND
                                                   tt_tilbudart.artikkelnr = KampanjeTilbArtikkel.KampTilbArtId) THEN DO:
                    CREATE tt_tilbudart.
                    ASSIGN tt_tilbudart.prodfamid  = KampanjeTilbArtikkel.ProdFamId
                           tt_tilbudart.artikkelnr = KampanjeTilbArtikkel.KampTilbArtId.
                END.
            END.
            ELSE DO:
                FOR EACH ProduktFamMedlem WHERE ProduktFamMedlem.Prodfamid = KampanjeTilbArtikkel.ProdFamId NO-LOCK:
                    IF NOT CAN-FIND(tt_tilbudart WHERE tt_tilbudart.prodfamid  = KampanjeTilbArtikkel.ProdFamId    AND
                                                       tt_tilbudart.artikkelnr = ProduktFamMedlem.ProdFamArtikkelNr) THEN DO:
                        CREATE tt_tilbudart.
                        ASSIGN tt_tilbudart.prodfamid  = KampanjeTilbArtikkel.ProdFamId
                               tt_tilbudart.artikkelnr = ProduktFamMedlem.ProdFamArtikkelNr.
                    END.
                END.
            END.
        END.
        lOK = TRUE.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-CreateTmpBonghode) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CreateTmpBonghode Procedure 
PROCEDURE CreateTmpBonghode :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER lFeil AS LOGICAL  NO-UNDO.
    DEFINE VARIABLE ii AS INTEGER    NO-UNDO.
    DEFINE VARIABLE cCreateError AS CHARACTER  NO-UNDO.
    cReturverdi = "".
    IF CAN-FIND(btmpBongHode WHERE
                btmpBongHode.ButikkNr   = iButikkNr AND
                btmpBongHode.GruppeNr   = 1         AND
                btmpBongHode.KasseNr    = TT_Receipt.TerminalId  AND
                btmpBongHode.Dato       = TT_Receipt.Dato        AND
                btmpBongHode.BongNr     = INT(TT_Receipt.ReceiptNumber)) THEN DO:
/*         CREATE TT_Error.                                   */
/*         ASSIGN TT_Error.BongNr = TT_Receipt.ReceiptNumber  */
/*                TT_Error.cError = "Dubblett".               */
/*                .                                           */
/*         RELEASE TT_Error.                                  */

        ASSIGN lFeil = TRUE.
        RETURN.
    END.

    FIND LAST btmpBongHode NO-LOCK USE-INDEX B_Id NO-ERROR.
    CREATE tmpBongHode.
    ASSIGN /* Setter indeksfeltene */
        tmpBongHode.ButikkNr   = iButikkNr 
        tmpBongHode.GruppeNr   = 1
        tmpBongHode.KasseNr    = TT_Receipt.TerminalId  
        tmpBongHode.Dato       = TT_Receipt.Dato
        tmpBongHode.EksportertDato = TT_Receipt.Dato - 1
        tmpBongHode.BongStatus = 5
        tmpBongHode.BongNr     = TT_Receipt.ReceiptNumber
        tmpBongHode.Skiftnr    = TT_Receipt.ShiftId
        tmpBongHode.B_Id       = IF AVAILABLE btmpBongHode
                                   THEN btmpBongHode.B_Id + 1
                                   ELSE 1
        tmpBongHode.Makulert   = IF TRIM(TT_Receipt.receiptType) = "void" THEN 2 ELSE 0
/*         tmpBongHode.BongStatus = 1 /* Under klargjøring */                           */
/*         tmpBongHode.pfFlagg    = 4                          /* Vad är riktigt???  */ */
        iAntBonger             = iAntBonger + 1.
/*         NO-ERROR.                                                                                                     */
/*     IF ERROR-STATUS:ERROR THEN DO:                                                                                    */
/*         MESSAGE TT_receipt.receiptnumber                                                                              */
/*             VIEW-AS ALERT-BOX INFO BUTTONS OK.                                                                        */
/*         IF ERROR-STATUS:NUM-MESSAGES > 0 THEN DO:                                                                     */
/*             DO ii = 1 TO ERROR-STATUS:NUM-MESSAGES:                                                                   */
/*                 cCreateError = cCreateError + (IF cCreateError = "" THEN "" ELSE ",") + ERROR-STATUS:GET-MESSAGE(ii). */
/*             END.                                                                                                      */
/*         END.                                                                                                          */
/*         ELSE                                                                                                          */
/*             cCreateError = "Okänt fel".                                                                               */
/*         CREATE TT_Error.                                                                                              */
/*         ASSIGN TT_Error.BongNr = TT_Receipt.ReceiptNumber                                                             */
/*                TT_Error.cError = cCreateError.                                                                        */
/*                .                                                                                                      */
/*         RELEASE TT_Error.                                                                                             */
/*         DELETE tmpBongHode.                                                                                           */
/*         PROCESS EVENTS.                                                                                               */
/*         RETURN "FEIL".                                                                                                */
/*     END.                                                                                                              */
BONGHODEINFO:
DO:
    ASSIGN
        tmpBongHode.KassererNr      = 0
        tmpBongHode.SelgerNr        = 0
        tmpBongHode.Belop           = TT_Receipt.Amount /* det görs i create linje TT_Receipt.Amount */
        tmpBongHode.KortType        = 1 /* (1-Ingen, 2-Kunde, 3-Medlem) */
/*         tmpBongHode.KundeKort       = trim(trim(ENTRY(24,tmpFilLinjer.StorTekst),"+"),"-") */
        tmpBongHode.flBetalingskort = TT_Receipt.cardamount <> 0
        tmpBongHode.flBankkort      = tmpBongHode.flBetalingskort 
        tmpBongHode.flKreditkort    = tmpBongHode.flBetalingskort 
        .
    /* Setter tid. */
        tmpBongHode.Tid = TT_receipt.tid.
    /* Kobler til kasserer */
/*     FIND ButikkForsalj NO-LOCK WHERE                                     */
/*         ButikkForsalj.Butik      = tmpBongHode.ButikkNr AND              */
/*         ButikkForsalj.KassererId = int(tmpBongHode.KassererNr) NO-ERROR. */
/*     IF AVAILABLE ButikkForsalj THEN                                      */
/*     DO:                                                                  */
/*         tmpBongHode.KassererNr = ButikkForsalj.ForsNr.                   */
/*         FIND Forsalj NO-LOCK where                                       */
/*             Forsalj.ForsNr = ButikkForsalj.ForsNr NO-ERROR.              */
/*         IF AVAILABLE Forsalj THEN                                        */
/*             tmpBongHode.KassererNavn = Forsalj.FoNamn.                   */
/*                                                                          */
/*     END.                                                                 */
    /* Kobler til kunde */
    FIND FIRST KundeKort NO-LOCK WHERE
        KundeKort.KortNr = tmpBongHode.KundeKort NO-ERROR.
    IF AVAILABLE KundeKort THEN
    DO:
        FIND Kunde NO-LOCK WHERE
            Kunde.KundeNr = KundeKort.KundeNr NO-ERROR.
        ASSIGN
            tmpBongHode.KundeNr   = KundeKort.KundeNr
            tmpBongHode.KundeNavn = IF AVAILABLE Kunde
                                      THEN Kunde.Navn
                                      ELSE ""
            .
    END.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-CreateTmpBongLinje) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CreateTmpBongLinje Procedure 
PROCEDURE CreateTmpBongLinje :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       13/6-07 TT_TenderLine.Amount blir TT_TenderLine.domesticValue
------------------------------------------------------------------------------*/
   DEFINE INPUT  PARAMETER iType AS INTEGER  NO-UNDO.
   DEFINE INPUT-OUTPUT  PARAMETER piLinjeNr AS INTEGER    NO-UNDO.
   DEFINE VARIABLE iTTId AS INTEGER    NO-UNDO.

  /* Här skapar vi en bonglinje för salg ellse tendering */
  /* I vissa fall skapar vi en extra bonglinje för ex. inbetaling/utbetaling*/
  RUN NyTmpBongLinje (piLinjeNr).

  IF iType = 1 OR iType = 10 THEN DO: /* Itemline */
     ASSIGN tmpBonglinje.TTId = IF TRIM(TT_Receipt.receiptType) = "refund" THEN 10 ELSE iType. /* om corr på rad kommer vi in med 10 */
     RUN Article.
/*      ASSIGN tmpBongHode.Belop = tmpBongHode.Belop + IF tmpBongLinje.Makulert THEN 0 ELSE tmpBongLinje.Linjesum                                                      */
/*             tmpBongHode.Belop = tmpBongHode.Belop - tmpBongLinje.GenerellRabatt                                                                                     */
/*             tmpBongHode.Belop = IF tmpBongLinje.Makulert THEN tmpBongHode.Belop ELSE IF tmpBongLinje.Antall < 0 THEN -1 * tmpBongHode.Belop ELSE tmpBongHode.Belop. */
/*                            ELSE IF tmpBongLinje.Antall < 0 THEN -1 * tmpBongLinje.Linjesum ELSE tmpBongLinje.Linjesum */
/*             tmpBongHode.Belop = tmpBongHode.Belop - tmpBongLinje.GenerellRabatt.                                      */
                                
  END.
  ELSE IF iType = 2 THEN DO: /* Tender */
      CASE TT_TenderLine.paymentType:
          WHEN "cash" THEN DO:
/*               ASSIGN tmpBonglinje.TTId = IF TT_TenderLine.domesticValue < 0 AND TT_Receipt.cardamount >= ABS(TT_TenderLine.domesticValue) */
              ASSIGN tmpBonglinje.TTId = IF TT_TenderLine.domesticValue < 0 AND TT_Receipt.cardamount > 0
                                            THEN 67  /* cashback */
                                    ELSE IF TT_TenderLine.domesticValue < 0 THEN 70 ELSE 50 /* växel/kontant */
                     /* Det kan finnas obalans vi utetankningar ie mer pengar. enl. samtal Anna Per Åke 29/6 */
                     tmpBonglinje.BongPris = IF TT_Receipt.utecash = TRUE THEN TT_receipt.Amount ELSE (TT_TenderLine.domesticValue * IF TT_Receipt.receiptType = "drop" THEN -1 ELSE 1)
                     tmpBonglinje.Linjesum = tmpBonglinje.BongPris.
               IF TT_Receipt.receiptType = "drop" THEN DO:
                    /* Här skall vi skapa ny bonglinje med */
                   ASSIGN piLinjeNr = piLinjeNr + 1.
                   RUN NyTmpBongLinje (piLinjeNr).
                   ASSIGN tmpBonglinje.TTId = 59
                          tmpBonglinje.BongPris = TT_TenderLine.domesticValue
                          tmpBonglinje.Linjesum = TT_TenderLine.domesticValue
                          tmpBongLinje.Storrelse = "".
               END.
               ELSE IF CAN-DO("inpayment,outpayment",TT_Receipt.receiptType) THEN DO:
                    /* Här skall vi skapa ny bonglinje med */
                   ASSIGN piLinjeNr = piLinjeNr + 1.
                   RUN NyTmpBongLinje (piLinjeNr).
                   ASSIGN tmpBonglinje.TTId = IF TT_Receipt.receiptType = "inpayment" THEN 61 ELSE 62
                          tmpBonglinje.BongPris = TT_TenderLine.domesticValue
                          tmpBonglinje.Linjesum = TT_TenderLine.domesticValue * -1
                          tmpBongLinje.Storrelse = TRIM(STRING(TT_Receipt.ptypetargetId,">99")).
               END.
               ELSE IF CAN-DO("currencySell,currencyBuy",TT_Receipt.receiptType) THEN DO:
                    /* Här skall vi skapa ny bonglinje med */
/*                    ASSIGN piLinjeNr = piLinjeNr + 1.                                      */
/*                    RUN NyTmpBongLinje (piLinjeNr).                                        */
/*                    ASSIGN tmpBonglinje.TTId = IF TT_TenderLine.domesticValue < 0 THEN 70 ELSE 50 */
/*                           tmpBonglinje.BongPris = TT_Receipt.Amount * -1                  */
/*                           tmpBonglinje.Linjesum = ABS(TT_TenderLine.domesticValue) * -1.         */
                  ASSIGN piLinjeNr = piLinjeNr + 1.
                  RUN NyTmpBongLinje (piLinjeNr).
                  ASSIGN tmpBonglinje.TTId = 60
                         tmpBonglinje.BongPris  = TT_TenderLine.domesticValue * -1
                         tmpBonglinje.Linjesum  = TT_TenderLine.domesticValue * -1.
                         tmpBongLinje.Bongtekst = TT_Receipt.currency + " " + STRING(TT_Receipt.currencyAmount)
                         .
               END.
          END.
          WHEN "card" THEN DO: 
              ASSIGN tmpBonglinje.TTId = IF TT_TenderLine.subPaymentId = 13 THEN 58 ELSE 52
                     tmpBonglinje.BongPris = TT_TenderLine.domesticValue
                     tmpBonglinje.Linjesum = TT_TenderLine.domesticValue
                     tmpBongLinje.Antall   = TT_TenderLine.subPaymentId.
              IF CAN-DO("inpayment,outpayment",TT_Receipt.receiptType) THEN DO:
                   /* Här skall vi skapa ny bonglinje */
                  ASSIGN piLinjeNr = piLinjeNr + 1.
                  RUN NyTmpBongLinje (piLinjeNr).
                  ASSIGN tmpBonglinje.TTId = IF TT_Receipt.receiptType = "inpayment" THEN 61 ELSE 62
                         tmpBonglinje.BongPris = TT_TenderLine.domesticValue * -1
                         tmpBonglinje.Linjesum = TT_TenderLine.domesticValue * -1
                         tmpBongLinje.Storrelse = TRIM(STRING(TT_Receipt.ptypetargetId,">99"))
                      .
              END.
              ELSE IF CAN-DO("currencySell,currencyBuy",TT_Receipt.receiptType) THEN DO:
                    /* Här skall vi skapa ny bonglinje med */
                  ASSIGN piLinjeNr = piLinjeNr + 1.
                  RUN NyTmpBongLinje (piLinjeNr).
                  ASSIGN tmpBonglinje.TTId = 60
                         tmpBonglinje.BongPris  = TT_TenderLine.domesticValue * -1
                         tmpBonglinje.Linjesum  = TT_TenderLine.domesticValue * -1.
                         tmpBongLinje.Bongtekst = TT_Receipt.currency + " " + STRING(TT_Receipt.currencyAmount)
                         .
              END.
          END.
/*           WHEN "voucher" THEN  /* subtypehantering */        */
/*               ASSIGN tmpBonglinje.TTId =                     */
/*                                                              */
          WHEN "CustomerAccountTenderLine" THEN DO: /* subtypehantering */
              ASSIGN tmpBonglinje.TTId = 65
                     tmpBonglinje.BongPris = TT_Receipt.Amount
                     tmpBonglinje.Linjesum = TT_TenderLine.domesticValue
/*                      tmpBonglinje.Linjesum = ABS(TT_TenderLine.domesticValue) */
                     tmpBongLinje.Storrelse = "0612"
                     tmpBongLinje.BongTekst = TT_Receipt.receiptType.
          END.
          WHEN "other" OR WHEN "voucher" THEN DO: /* subtypehantering */
              ASSIGN tmpBonglinje.TTId = IF TT_TenderLine.subPaymentId = 1 AND TT_TenderLine.paymentType = "other" THEN 65 ELSE 53
                     tmpBonglinje.BongPris = TT_Receipt.Amount
                     tmpBonglinje.Linjesum = TT_TenderLine.domesticValue.
              IF TT_TenderLine.paymentType = "other" THEN
                  ASSIGN tmpBongLinje.Storrelse = TRIM(STRING(TT_TenderLine.subPaymentId,">99"))
                         tmpBongLinje.BongTekst = IF TT_TenderLine.subPaymentId = 1 THEN "Manuell slip"
                                             ELSE IF TT_TenderLine.subPaymentId = 3 THEN "Preem Presentkort"
                                             ELSE IF TT_TenderLine.subPaymentId = 2 THEN "Preem Bonuscheck" ELSE "".
              ELSE tmpBongLinje.Storrelse = TRIM(STRING((IF TT_TenderLine.subPaymentId < 6 THEN 5 ELSE 0) + TT_TenderLine.subPaymentId,">99")).
              IF CAN-DO("inpayment,outpayment",TT_Receipt.receiptType) THEN DO:
                   /* Här skall vi skapa ny bonglinje */
                  ASSIGN piLinjeNr = piLinjeNr + 1.
                  RUN NyTmpBongLinje (piLinjeNr).
                  ASSIGN tmpBonglinje.TTId = IF TT_Receipt.receiptType = "inpayment" THEN 61 ELSE 62
                         tmpBonglinje.BongPris = TT_TenderLine.domesticValue * -1
                         tmpBonglinje.Linjesum = TT_TenderLine.domesticValue * -1
                         tmpBongLinje.Storrelse = TRIM(STRING(TT_Receipt.ptypetargetId,">99"))
                      .
              END.
          END.
          WHEN "driveoff"        THEN
              ASSIGN tmpBonglinje.TTId = 54 /* vi använder check */
                     tmpBonglinje.BongPris = TT_Receipt.Amount
                     tmpBonglinje.Linjesum = TT_TenderLine.domesticValue.
          WHEN "rounding"        THEN DO:
              ASSIGN tmpBonglinje.TTId     = 78
                     tmpBonglinje.BongPris = TT_Receipt.Amount
                     tmpBonglinje.Linjesum = TT_TenderLine.domesticValue.
          END.

      END CASE.
      IF TRIM(TT_Receipt.receiptType) = "CustomerAccountInPayReceipt" THEN DO:
          ASSIGN piLinjeNr = piLinjeNr + 1.
          RUN NyTmpBongLinje (piLinjeNr).
          ASSIGN tmpBonglinje.TTId = 65
                 tmpBonglinje.BongPris = TT_Receipt.Amount
                 tmpBonglinje.Linjesum = TT_TenderLine.domesticValue
                 tmpBongLinje.Storrelse = "0610"
                 tmpBongLinje.BongTekst = TT_Receipt.receiptType.
      END.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-GetOldShifts) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetOldShifts Procedure 
PROCEDURE GetOldShifts :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    FOR EACH TT_BongShift:
        IF NOT CAN-FIND(FIRST TT_Shift WHERE TT_Shift.skiftNr = TT_BongShift.ShiftId) THEN DO:
            FIND FIRST Skift WHERE Skift.ButikkNr = iButikkNr AND
/*                                    skift.BokforingsId = 0 AND */
                                   skift.SkiftNr = TT_BongShift.ShiftId NO-LOCK NO-ERROR.
            IF AVAIL Skift THEN DO:
                CREATE TT_Shift.
                ASSIGN TT_Shift.Butikknr = iButikknr
                       TT_Shift.Dato     = Skift.dato
                       TT_Shift.SkiftNr  = Skift.Skiftnr
                       TT_Shift.sequenceNumber = Skift.n9SkiftNr
                       TT_Shift.terminert = Skift.terminert.
            END.
            ELSE DO:
                CREATE TT_Shift.
                ASSIGN TT_Shift.Butikknr = iButikknr
                       TT_Shift.Dato     = TODAY
                       TT_Shift.SkiftNr  = TT_BongShift.ShiftId.
            END.
        END.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-initTT_Kasse) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initTT_Kasse Procedure 
PROCEDURE initTT_Kasse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    FOR EACH Kasse WHERE Kasse.Butikknr = iButikkNr AND kasse.gruppe = 1 NO-LOCK:
        CREATE TT_kasse.
        ASSIGN TT_kasse.uteinne = IF Kasse.kassenr < 11 THEN "I" ELSE "U"
               TT_Kasse.kassenr = Kasse.KasseNr NO-ERROR.
        IF ERROR-STATUS:ERROR THEN
            DELETE TT_Kasse.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-InnLesFil) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InnLesFil Procedure 
PROCEDURE InnLesFil :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
  
  iButikkNr 
  iGruppeNr 
  iKasseNr  
  cFilNavn  
  lFilId    
  iAntLinjer
------------------------------------------------------------------------------*/
  DEF VAR piLinjeNr    AS INT   NO-UNDO.
  DEF VAR pcLinje      AS CHAR  NO-UNDO.
  DEF VAR iTerminalid  AS INTEGER NO-UNDO.
  DEF VAR lHarCorr AS LOGICAL    NO-UNDO.
  DEF VAR lFirstBetSatt AS LOGICAL NO-UNDO.
  DEFINE VARIABLE lFeil AS LOGICAL    NO-UNDO.
/*--------
  DEF VAR pcOLinje     AS CHAR  NO-UNDO.
  DEF VAR piSettNr     AS INT   NO-UNDO.
  DEF VAR pdDato       AS DATE  NO-UNDO.
  DEF VAR pcDato       AS CHAR  NO-UNDO.
  DEF VAR plDataSettId AS DEC   NO-UNDO.
  DEF VAR pcSokMaske   AS CHAR  NO-UNDO.
  DEF VAR pcOSokMaske  AS CHAR  NO-UNDO.
  DEF VAR pbKoblet     AS LOG   NO-UNDO.
  DEF VAR prRowId      AS ROWID NO-UNDO.
  DEF VAR piAntISett   AS INT   NO-UNDO.
  DEF VAR piLoop       AS INT   NO-UNDO.
  DEF VAR pcTekst      AS CHAR  NO-UNDO.
  DEF VAR pcButKasLst  AS CHAR  NO-UNDO.
  DEF VAR pc2Tekst     AS CHAR  NO-UNDO.
  DEF VAR piBongLinje  AS INT   NO-UNDO.
  DEF VAR piBongNr     AS INT   NO-UNDO.
  DEF VAR piSkiftNr    AS INT   NO-UNDO.
  
  DEF VAR d31Dec2002   AS DATE NO-UNDO.
  DEF VAR dSkiftnrTmp  AS DECI DECIMALS 0 NO-UNDO.

  DEFINE VARIABLE cOldSekvens AS CHARACTER  NO-UNDO.
-------------*/

  DEF VAR prBongRecid AS RECID NO-UNDO.

  DEF BUFFER bSkift FOR Skift.

  ASSIGN
      iantLinjer  = 0
      .

  RUN Telleverk IN h_Parent ("Leser og konverterer data. Vent litt... ") NO-ERROR.

  LESINNBUFFER:
  FOR EACH TT_Receipt:
      IF TT_Receipt.Dato = ? OR TT_Receipt.Tid = ? THEN DO:
          CREATE TT_Error.
          ASSIGN TT_Error.BongNr = TT_Receipt.ReceiptNumber
                 TT_Error.cError = "Okänt dato / tid".
                 .
          RELEASE TT_Error.
          NEXT.
      END.
      IF NOT CAN-FIND(FIRST tmpDataDato WHERE tmpDataDato.dato = TT_Receipt.Dato) THEN DO:
          CREATE tmpDataDato.
          ASSIGN tmpDataDato.dato = TT_Receipt.Dato
                 tmpDataDato.doDatasett = TRUE.
      END.
      IF TRIM(TT_Receipt.receiptType) = "FinancialReceipt" THEN
          RUN KontrollerBalansFinancial.
      ASSIGN lHarCorr = FALSE.
      IF NOT CAN-DO("sale,refund,void,inpayment,outpayment,drop,currencySell,currencyBuy,FinancialReceipt,CustomerAccountInPayReceipt",TRIM(TT_Receipt.receiptType)) THEN
          NEXT LESINNBUFFER.
       ASSIGN piLinjeNr = 0.
      /* vi ändrar terminalId i TT_receipt innan vi gör något mer */
       ASSIGN TT_Receipt.terminalId = IF TT_Receipt.TerminalType = "opt" THEN TT_Receipt.terminalId + 10
                                 ELSE IF TT_Receipt.TerminalType = "attended"  THEN TT_Receipt.terminalId + 30
                                 ELSE TT_Receipt.terminalId.
       RUN SjekkKasseNr (TT_Receipt.terminalId).
       /* detta skall bort enligt telemøte med Anna och Per Åke 29/6 */
/*       IF TT_Receipt.utecash = TRUE THEN                                          */
/*           TT_Receipt.Amount = ROUND(TT_Receipt.Amount,0). /* vi skall avrunda */ */
      ASSIGN lFeil = FALSE.
      RUN CreateTmpBonghode (OUTPUT lFeil).
      IF lFeil = TRUE THEN
          NEXT.
      /* Hopper over allt som ikke skal inn.                           
      IF NOT can-do("",trim(TT_Receipt.receiptType)) THEN
          NEXT LESINNBUFFER.
      */
      /* Leser alle varelinjene */
      ASSIGN lKampanjFinns = FALSE. /* vi flaggar om tillslag på kampanj i TT_ItemLine */
      VARELINJE:
      FOR EACH TT_ItemLine OF TT_Receipt:
        /* TN 13082011 Har lagt inn linjeteller. Den var kommentert bort sammen med lHarCorr. */
        ASSIGN piLinjeNr = piLinjeNr + 1.                        
      
      /* 
         IF CAN-DO("normal,corr",TRIM(TT_ItemLine.lineType)) THEN DO: 
             ASSIGN lHarCorr = TRUE.                                  
             ASSIGN piLinjeNr = piLinjeNr + 1.                        
         END.     
      */
                                                          
       /* detta skall bort enligt telemøte med Anna och Per Åke 29/6 */
/*           IF TT_Receipt.utecash = TRUE THEN                                            */
/*               TT_ItemLine.Amount = ROUND(TT_ItemLine.Amount,0). /* vi skall avrunda */ */
          RUN CreateTmpBongLinje (IF CAN-DO("normal,corr",TRIM(TT_ItemLine.lineType)) THEN 1 ELSE 10,INPUT-OUTPUT piLinjeNr).
      END. /* VARELINJE */
      /* Leser betalingslinjene */
      ASSIGN lFirstBetallinje = FALSE.
      ASSIGN lFirstBetSatt    = FALSE.
      BETALINGSLINJE:
      FOR EACH TT_TenderLine OF TT_Receipt:
          IF lFirstBetSatt = FALSE AND TT_TenderLine.paymentType <> "rounding" THEN DO:
              IF (tmpBongHode.Belop > 0 AND TT_Tenderline.domesticvalue > 0) OR (tmpBongHode.Belop < 0 AND TT_Tenderline.domesticvalue < 0) THEN DO:
                  ASSIGN lFirstBetallinje = TRUE
                         lFirstBetSatt = TRUE.
              END.
          END.
          ASSIGN piLinjeNr = piLinjeNr + 1.
          RUN CreateTmpBongLinje (2,INPUT-OUTPUT piLinjeNr).
          ASSIGN lFirstBetallinje = FALSE.
      END. /* BETALINGSLINJE */

/*       IF lHarCorr = TRUE THEN */
/*           RUN KorrigerCorr.   */
/*       TAXKONTROLL */
      RUN TaxKontroll.
      
      IF lKampanjFinns = TRUE THEN
          RUN LoggKampanj (INPUT piLinjeNr) NO-ERROR.
  END. /* LESINNBUFFE */

  /* Stempler posten som innlest. */
  /* Detta gör vi i OppdaterFiler */

/*   DO TRANSACTION:                                */
/*       FIND CURRENT Filer EXCLUSIVE-LOCK.         */
/*       ASSIGN                                     */
/*           Filer.Innlest = TRUE                   */
/*           Filer.InnlestDato = TODAY              */
/*           Filer.InnlestKl   = TIME               */
/*           Filer.InnlestAv   = USERID("SkoTex")   */
/*           Filer.Oppdatert   = TRUE               */
/*           Filer.OppdatertKl   = TIME             */
/*           Filer.OppdatertAv   = USERID("SkoTex") */
/*           .                                      */
/*       FIND CURRENT Filer NO-LOCK.                */
/*   END.                                           */

  /* Nullstiller telleverket */
  RUN Telleverk IN h_Telleverk (" ") NO-ERROR.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-KontrollerBalansFinancial) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE KontrollerBalansFinancial Procedure 
PROCEDURE KontrollerBalansFinancial :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE dKontrollSum AS DECIMAL    NO-UNDO.
    DEFINE BUFFER bufTT_TenderLine FOR TT_TenderLine.

    FOR EACH bufTT_TenderLine OF TT_Receipt:
        dKontrollSum = dKontrollSum + bufTT_TenderLine.domesticValue.
    END.
    IF dKontrollsum <> 0 THEN DO:
        /* Vi gør om till  */
        ASSIGN TT_Receipt.receipttype   = IF dKontrollsum < 0 THEN "outpayment" ELSE "inpayment"
               TT_Receipt.ptypetargetId = 20.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-KorrigerCorr) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE KorrigerCorr Procedure 
PROCEDURE KorrigerCorr :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE BUFFER btmpBongLinje FOR tmpBongLinje.
    FOR EACH tmpBongLinje WHERE tmpBongLinje.b_id = tmpBongHode.b_id AND tmpBongLinje.ttId = 10 AND tmpbonglinje.makulert = FALSE:
        FIND FIRST btmpBongLinje     WHERE btmpBongLinje.b_id = tmpBongHode.b_id AND btmpBongLinje.ttid = 1 AND btmpBongLinje.makulert = FALSE AND
                                           btmpBongLinje.strekkode = tmpBonglinje.strekkode AND btmpBongLinje.antall = (-1 * tmpBonglinje.antall) AND
                                           btmpbonglinje.linjesum = tmpbonglinje.linjesum AND btmpbonglinje.generellrabatt = tmpbonglinje.generellrabatt NO-ERROR.
        IF NOT AVAIL btmpBongLinje THEN
            FIND FIRST btmpBongLinje WHERE btmpBongLinje.b_id = tmpBongHode.b_id AND btmpBongLinje.ttid = 1 AND btmpBongLinje.makulert = FALSE AND
                                           btmpBongLinje.strekkode = tmpBonglinje.strekkode AND ABS(btmpBongLinje.antall) = ABS(tmpBonglinje.antall) AND
                                           ABS(btmpbonglinje.linjesum) = ABS(tmpbonglinje.linjesum) AND ABS(btmpbonglinje.generellrabatt) = ABS(tmpbonglinje.generellrabatt) NO-ERROR.
        IF AVAIL btmpBongLinje THEN
            ASSIGN tmpbonglinje.makulert = TRUE
                  btmpbonglinje.makulert = TRUE.
        RELEASE btmpbonglinje.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-LagreBonger) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LagreBonger Procedure 
PROCEDURE LagreBonger :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER cDatasettIDlist AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cDatasettId AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE ii AS INTEGER    NO-UNDO.
    DEFINE BUFFER bufBongHode FOR Bonghode.
    FOR EACH tmpBongHode TRANSACTION:
        IF CAN-FIND(BongHode WHERE BongHode.ButikkNr = tmpBongHode.ButikkNr AND
                                   BongHode.gruppenr = 1                    AND
                                   BongHode.Kassenr  = tmpBongHode.Kassenr  AND
                                   BongHode.Dato     = tmpBongHode.Dato     AND
                                   BongHode.BongNr   = tmpBongHode.BongNr) THEN
            NEXT.
        RELEASE BongHode.
        BUFFER-COPY tmpBongHode EXCEPT tmpBonghode.b_id TO BongHode.
        IF NOT CAN-DO(cDatasettId,STRING(Bonghode.datasettid)) THEN
            ASSIGN cDatasettId = cDatasettId + (IF cDatasettId <> "" THEN "," ELSE "") + STRING(BongHode.DatasettId).
/*         IF ERROR-STATUS:ERROR AND AVAIL BongHode THEN DO: */
/*             DELETE BongHode.                              */
/*             NEXT.                                         */
/*         END.                                              */
        FOR EACH tmpBongLinje WHERE tmpBongLinje.b_id = tmpBongHode.b_id:
            RELEASE BongLinje.
            BUFFER-COPY tmpBongLinje EXCEPT tmpBongLinje.b_id TO BongLinje
                ASSIGN BongLinje.b_id = BongHode.b_id.
        END.
    END.
   DO ii = 1 TO NUM-ENTRIES(cDataSettId) TRANSACTION:
       FIND datasett EXCLUSIVE WHERE datasett.datasettid = DECI(ENTRY(ii,cDatasettid)) NO-ERROR.
       IF AVAIL datasett THEN
           datasett.behandlet = 3.
       RELEASE datasett.
   END.
/*    IF AVAIL datasett THEN             */
/*        FIND CURRENT datasett NO-LOCK. */
    ASSIGN cDatasettIDlist = cDataSettId.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-LoggKampanj) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LoggKampanj Procedure 
PROCEDURE LoggKampanj :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER piLinjeNr AS INTEGER     NO-UNDO.
    
    DEFINE VARIABLE lOK           AS LOGICAL    NO-UNDO.
    DEFINE VARIABLE iFirstAntal   AS INTEGER    NO-UNDO.
    DEFINE VARIABLE iLastAntal    AS INTEGER    NO-UNDO.
    DEFINE VARIABLE iAntTilbud    AS INTEGER    NO-UNDO.
    DEFINE VARIABLE dVarekost     AS DECIMAL    NO-UNDO.
    
    /* Logger tilbud.                                        */
    /* Bygger også tilbudet hvis det ikke er logget fra før. */
    POSTER_SOLGT_ANTALL:
    FOR EACH tmpBongLinje WHERE 
        tmpBongLinje.b_id = tmpBongHode.b_id       AND 
        tmpBongLinje.KampId >  0                   AND
        tmpBongLinje.KampTilbId > 0                AND  
        CAN-DO("1,3,10",STRING(tmpBongLinje.ttId)) AND 
        tmpbonglinje.makulert = FALSE
        BREAK BY tmpBongLinje.b_id
              BY tmpBongLinje.KampId
              BY tmpBongLinje.KampTilbId:
        
        /* Bygger tilbudet hvis det ikke finnes fra før. */
        FIND tt_tilbud WHERE 
          tt_tilbud.kampid     = tmpBonglinje.kampid AND
          tt_tilbud.kamptilbid = tmpBonglinje.kamptilbid NO-ERROR.
        IF NOT AVAIL tt_tilbud THEN
            RUN ByggTilbud (tmpbonglinje.kampid,tmpbonglinje.kamptilbid, OUTPUT lOK).
        IF NOT lOK THEN
            NEXT.
        
        /* Logger antall solgte artikler på tilbudet */    
        FOR EACH tt_tilbud WHERE 
            tt_tilbud.kampid     = tmpbonglinje.kampid AND
            tt_tilbud.kamptilbid = tmpbonglinje.kamptilbid:
            
            /*
            IF CAN-FIND(tt_tilbudart WHERE 
                tt_tilbudart.prodfamid  = tt_tilbud.prodfamid AND
                tt_tilbudart.artikkelnr = DECI(tmpbonglinje.artikkelnr)) THEN 
            DO:
                ASSIGN tt_tilbud.solgtantall = tt_tilbud.solgtantall + tmpbonglinje.antall.
            END.
            */
            ASSIGN tt_tilbud.solgtantall = tt_tilbud.solgtantall + tmpbonglinje.antall.
/*            
MESSAGE 'Legger inn antall solgt på tilbudet' tt_tilbud.kampid tt_tilbud.kamptilbid SKIP
'Solgt:' tt_tilbud.solgtantall
VIEW-AS ALERT-BOX.
*/            
        END.
    END. /* POSTER_SOLGT_ANTALL */
    
    FOR EACH tt_tilbud 
        BREAK BY tt_tilbud.kampid 
              BY tt_tilbud.kamptilbId:

        IF FIRST-OF(tt_tilbud.kamptilbid) AND tt_tilbud.Solgtantall > 0 THEN 
        DO:
            ASSIGN iFirstAntal = TRUNCATE(tt_tilbud.Solgtantall / tt_tilbud.KampTilbArtMinAntall,0).
        END.
        
        IF LAST-OF(tt_tilbud.kamptilbid) AND tt_tilbud.Solgtantall > 0 THEN 
        DO:
            ASSIGN iLastAntal = TRUNCATE(tt_tilbud.Solgtantall / tt_tilbud.KampTilbArtMinAntall,0).
            iAntTilbud = MIN(iFirstAntal,iLastAntal).
            
            ASSIGN piLinjeNr = piLinjeNr + 1.
            RUN NyTmpBongLinje (piLinjeNr).
            
            ASSIGN tmpbonglinje.TTId       = 109
                   tmpBonglinje.antall     = iAntTilbud                   
                   tmpBongLinje.LinjeSum   = tt_tilbud.Pris * iAntTilbud
                   tmpBongLinje.BongPris   = tt_tilbud.Pris * iAntTilbud
                   tmpBongLinje.KampId     = tt_tilbud.kampid
                   tmpBongLinje.KampTilbId = tt_tilbud.kamptilbid
                   /*tmpBongLinje.KampEier   = */
                   .              
                   tmpbonglinje.bongtekst = STRING(tt_tilbud.kampid) + "|" + STRING(tt_tilbud.kamptilbid) + "|" + STRING(tt_tilbud.pris)  + "|" + tt_tilbud.Namn.
            ASSIGN iFirstAntal = 0
                   iLastAntal  = 0
                   iAntTilbud  = 0
                   dVarekost   = 0.
        END.
        
        ASSIGN tt_tilbud.Solgtantall = 0.
    END.

/* 
DEFINE TEMP-TABLE tt_tilbud NO-UNDO
    FIELD kampid               AS DECIMAL
    FIELD kamptilbid           AS INTEGER
    FIELD ProdFamId            AS DECIMAL
    FIELD KampTilbArtMinAntall AS DECI
    FIELD Solgtantall          AS DECI
    FIELD radnr      AS INTEGER
    INDEX kamp IS PRIMARY kampid kamptilbid.
    
DEFINE TEMP-TABLE tt_tilbudart NO-UNDO
    FIELD ProdFamId            AS DECIMAL
    FIELD Artikkelnr           AS DECI
    INDEX art IS PRIMARY UNIQUE ProdFamId Artikkelnr.
    
 */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-NyTmpBongLinje) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE NyTmpBongLinje Procedure 
PROCEDURE NyTmpBongLinje :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER iLinjeNr AS INTEGER    NO-UNDO.
    CREATE tmpBongLinje. /* */
    ASSIGN
        tmpBongLinje.B_Id         = tmpBongHode.B_Id
        tmpBongLinje.ButikkNr     = tmpBongHode.ButikkNr 
        tmpBongLinje.GruppeNr     = tmpBongHode.GruppeNr 
        tmpBongLinje.KasseNr      = tmpBongHode.KasseNr  
        tmpBongLinje.Dato         = tmpBongHode.Dato     
        tmpBonglinje.Transdato    = tmpBongHode.Dato
        tmpBonglinje.Transtid     = tmpBongHode.Tid
        tmpBongLinje.BongNr       = tmpBongHode.BongNr   
        tmpBongLinje.LinjeNr      = iLinjeNr /*BongLinje*/
        tmpBongLinje.Forkonvertering = IF lFirstBetallinje = TRUE THEN "JA" ELSE "".
        .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-OppdaterFiler) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OppdaterFiler Procedure 
PROCEDURE OppdaterFiler :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DO TRANSACTION:
      FIND CURRENT Filer EXCLUSIVE-LOCK.
      ASSIGN
          Filer.Innlest = TRUE
          Filer.InnlestDato = TODAY
          Filer.InnlestKl   = TIME
          Filer.InnlestAv   = USERID("SkoTex")
          Filer.Oppdatert   = TRUE
          Filer.OppdatertDato = TODAY
          Filer.OppdatertKl   = TIME
          Filer.OppdatertAv   = USERID("SkoTex")
          . 
      IF bufButiker.StatistikkOppdatering = FALSE THEN DO:
          ASSIGN Filer.Overfort     = TRUE
                 Filer.OverfortDato = Filer.OppdatertDato
                 Filer.OverfortTid  = Filer.OppdatertKl
                 Filer.OverfortAv   = Filer.OppdatertAv.
      END.
      FIND CURRENT Filer NO-LOCK.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-OppdaterPRdata) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OppdaterPRdata Procedure 
PROCEDURE OppdaterPRdata :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER cDataSettId AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE ii AS INTEGER     NO-UNDO.
    DO ii = 1 TO NUM-ENTRIES(cDataSettId):
        FIND datasett WHERE datasett.datasettid = DECI(ENTRY(ii,cDatasettid)) NO-LOCK NO-ERROR.
        IF AVAIL datasett THEN
            RUN oppdaterPRdata.p (datasett.datasettid).
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-OpprettDatasett) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OpprettDatasett Procedure 
PROCEDURE OpprettDatasett :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
  
  iButikkNr 
  iGruppeNr 
  iKasseNr  
  cFilNavn  
  lFilId    
  iAntLinjer
------------------------------------------------------------------------------*/
  DEF VAR piLinjeNr    AS INT   NO-UNDO.
  DEF VAR piSettNr     AS INT   NO-UNDO.
  DEF VAR pdDato       AS DATE  NO-UNDO.
  DEF VAR plDataSettId AS DEC   NO-UNDO.
  DEF VAR pbKoblet     AS LOG   NO-UNDO.
  DEF VAR prRowId      AS ROWID NO-UNDO.
  DEF VAR piAntISett   AS INT   NO-UNDO.
  DEF VAR d31DecFgAr AS DATE NO-UNDO.
  DEF VAR lKontrollert AS LOG NO-UNDO.
  piLinjeNr = 1.

  LESERLINJER:
  FOR EACH tmpBongHode
      BREAK BY tmpBongHode.KasseNr BY tmpBongHode.dato:
/*     piLinjeNr = piLinjeNr + 1. */
      IF FIRST-OF(tmpBongHode.dato) THEN
        OPPRETTDATASETT:
        DO:
          /* Ferdigstempler den vi hold på med. */
          IF prRowId <> ? THEN
          DO:
              FIND DataSett EXCLUSIVE-LOCK WHERE
                  ROWID(Datasett) = prRowid.
              ASSIGN
                DataSett.AntallLinjer = DataSett.AntallLinjer + piAntISett
                DataSett.SettStatus   = (IF DataSett.SettNr > 1
                                          THEN 3 /* Ekstra  */
                                          ELSE 2 /* Mottatt */)
                DataSett.SettStatus   = (IF DataSett.ButikkNr <> 0
                                          THEN DataSett.SettStatus
                                          ELSE 9  /* Ikke koblet */)
                piAntISett            = 0
                prRowId               = ?
                .
              /* Åpningsskjemahantering */
/*               IF cKontrolltabell = "1" AND lKontrollert = FALSE THEN DO:                            */
/*                   FIND ApnSkjema WHERE ApnSkjema.ButikkNr = DataSett.ButikkNr AND                   */
/*                                        ApnSkjema.Ar       = YEAR(DataSett.Dato) NO-ERROR.           */
/*                   IF AVAIL ApnSkjema THEN DO:                                                       */
/*                       ASSIGN d31DecFgAr = DATE(12,31,ApnSkjema.Ar - 1)                              */
/*                              ENTRY(DataSett.Dato - d31DecFgAr,ApnSkjema.OpenClosed) = "4" NO-ERROR. */
/*                   END.                                                                              */
/*                   ASSIGN lKontrollert = TRUE.                                                       */
/*               END.                                                                                  */
              /* Åpnings.... SLUTT      */
              RELEASE DataSett.
          END.
          ASSIGN iGruppeNr = 1 /*tmpFilLinjer.GruppeNr*/
                 iKasseNr  = tmpBongHode.KasseNr
                 pbKoblet = TRUE.

          /* Setter transaksjonsdato dd/mm/yy */
          ASSIGN pdDato = tmpBongHode.dato.

          /* Finner neste DataSettId */
          FIND LAST DataSett NO-LOCK
              USE-INDEX DataSettId NO-ERROR.
          IF AVAILABLE DataSett THEN
              plDataSettId = DataSett.DataSettId + 1.
          ELSE
              plDataSettId = 1.
          /* Finner neste SettNr */
          FIND LAST Datasett NO-LOCK WHERE
              Datasett.ButikkNr = iButikkNr AND
              Datasett.GruppeNr = iGruppeNr AND
              Datasett.KasseNr  = iKasseNr  AND
              Datasett.Dato     = pdDato    AND
              DataSett.FilType  = 1 /* EL-Journal */
              USE-INDEX DataSett NO-ERROR.
          IF AVAILABLE DataSett THEN
              piSettNr = DataSett.SettNr + 1.
          ELSE DO:
              piSettNr = 1.
          END.

          /* Alle kvitteringer som kommer inn på samme dag skal kobles  */
          /* til det samme datasettet. Forutsetning er at settet ikke   */
          /* har behandlingsstatus > 1.                                 */
/*           IF AVAILABLE DataSett THEN                             */
/*           DO:                                                    */
/*             IF DataSett.SettNr <= 3 AND DataSett.SettStatus <= 2 */
/*                 AND DataSett.Behandlet <= 1 THEN                 */
/*                 ASSIGN                                           */
/*                   plDataSettId = DataSett.DataSettId             */
/*                   piSettNr     = DataSett.SettNr                 */
/*                   .                                              */
/*             ELSE                                                 */
/*                 RELEASE DataSett. /* Ny post skal skapes. */     */
/*           END.                                                   */
          IF AVAIL DataSett THEN
              RELEASE DataSett. /* Ny post skal skapes. */
          IF NOT AVAILABLE DataSett THEN
          DO:
            CREATE DataSett.
            ASSIGN
                DataSett.DataSettId = plDataSettId
                DataSett.SettStatus = 8 /* Innlesning avbrutt */
                DataSett.pfFlagg    = 98.
            ASSIGN lKontrollert = FALSE.
          END.
          ELSE  /* Bruker det vi fant. */
          DO:
              FIND CURRENT DataSett EXCLUSIVE-LOCK.
          END.
          ASSIGN
            prRowId             = ROWID(DataSett)
            DataSett.ButikkNr   = iButikkNr 
            DataSett.GruppeNr   = iGruppeNr
            DataSett.KasseNr    = iKasseNr
            DataSett.Dato       = pdDato
            DataSett.SettNr     = piSettNr
            DataSett.Tid        = 0
            DataSett.FilId      = lFilId
            DataSett.FilType    = 1 /* EL-Journal */
            .
          /* Åpningsskjemahantering */
          IF cKontrolltabell = "1" AND lKontrollert = FALSE THEN DO:
              FIND ApnSkjema WHERE ApnSkjema.ButikkNr = DataSett.ButikkNr AND
                                   ApnSkjema.Ar       = YEAR(DataSett.Dato) NO-ERROR.
              IF AVAIL ApnSkjema THEN DO:
                  ASSIGN d31DecFgAr = DATE(12,31,ApnSkjema.Ar - 1)
                         ENTRY(DataSett.Dato - d31DecFgAr,ApnSkjema.OpenClosed) = "4" NO-ERROR.
              END.
              ASSIGN lKontrollert = TRUE.
          END.
          /* Åpnings.... SLUTT      */
    END. /* OPPRETTDATASETT */
    tmpBongHode.DataSettId = DataSett.DatasettId.
    piAntISett = piAntISett + 1.

    IF iAntLinjer MODULO 25 = 0 THEN
    DO:
      RUN Telleverk IN h_Telleverk 
          ("Fil: " + Filer.Katalog + "\" + Filer.Filnavn +  
           " Leser " + STRING(iAntLinjer) +
           " av " + STRING(iTotAntLinjer) + ".") NO-ERROR.
    END.

  END. /* LESERLINJER */

  RUN Telleverk IN h_Telleverk (" ") NO-ERROR.

  /* Stempler posten som innlest. */
  IF AVAILABLE DataSett THEN
  DO TRANSACTION:
      FIND CURRENT DataSett EXCLUSIVE-LOCK.
      ASSIGN
          DataSett.AntallLinjer = DataSett.AntallLinjer + piAntISett
          DataSett.SettStatus   = (IF DataSett.SettNr > 1
                                    THEN 3 /* Ekstra  */
                                    ELSE 2 /* Mottatt */)
          DataSett.SettStatus   = (IF DataSett.ButikkNr <> 0
                                    THEN DataSett.SettStatus
                                    ELSE 9  /* Ikke koblet */)
          .
/*       FIND CURRENT Filer EXCLUSIVE-LOCK.       */
/*       ASSIGN                                   */
/*           Filer.Innlest = TRUE                 */
/*           Filer.InnlestDato = TODAY            */
/*           Filer.InnlestKl   = TIME             */
/*           Filer.InnlestAv   = USERID("SkoTex") */
/*           .                                    */
      /* Åpningsskjemahantering */

      /* Görs i Opprettdatasettforpro */

      /* Åpnings.... SLUTT      */
  END.
  IF AVAILABLE DataSett THEN
      FIND CURRENT DataSett NO-LOCK.
  IF AVAILABLE Filer THEN
      FIND CURRENT Filer    NO-LOCK.

/*   RUN Telleverk IN h_Parent ("Kontrollerer at alle datasett er mottatt. Vent litt... ") NO-ERROR. */
/*   RUN sjekkdatasett.p (INPUT lFilId, INPUT cDatoListe).                                           */
  RUN Telleverk IN h_Parent (" ") NO-ERROR.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-OpprettDatasettForPro) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OpprettDatasettForPro Procedure 
PROCEDURE OpprettDatasettForPro :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/*   DEF VAR piLinjeNr    AS INT   NO-UNDO. */
  DEF VAR piSettNr     AS INT   NO-UNDO.
  DEF VAR dsDato AS DATE NO-UNDO.
  DEF VAR d31DecFgAr AS DATE NO-UNDO.
  DEFINE VARIABLE dFilid AS DECIMAL    NO-UNDO.
  DEF VAR plDataSettId AS DEC   NO-UNDO.
  DEFINE VARIABLE CreateDato AS DATE       NO-UNDO.


/*     FOR EACH tmpDataDato:                                                                       */
/*         FOR EACH TT_Kasse:                                                                      */
/*             IF CAN-FIND(FIRST Bonghode WHERE Bonghode.butikknr = iButikkNr AND                  */
/*                                              Bonghode.GruppeNr = 1         AND                  */
/*                                              Bonghode.KasseNr  = TT_Kasse.Kassenr AND           */
/*                                              BongHode.dato     = tmpDataDato.dato - 1) THEN DO: */
/*                 tmpDataDato.doDatasett = TRUE.                                                  */
/*                 LEAVE.                                                                          */
/*             END.                                                                                */
/*         END.                                                                                    */
/*     END.                                                                                        */

    FOR EACH tmpDataDato WHERE tmpDataDato.doDatasett = TRUE BY tmpDataDato.dato:
        IF finnesPROdatasett(tmpDataDato.dato,OUTPUT CreateDato) = FALSE AND CreateDato <> ? THEN DO:
            ASSIGN dsDato = CreateDato.
            FIND LAST DataSett NO-LOCK
                USE-INDEX DataSettId NO-ERROR.
            IF AVAILABLE DataSett THEN
                plDataSettId = DataSett.DataSettId + 1.
            ELSE
                plDataSettId = 1.
            /* Finner neste SettNr */
            FIND LAST Datasett NO-LOCK WHERE
                Datasett.ButikkNr = iButikkNr AND
                Datasett.GruppeNr = 1 AND
                Datasett.KasseNr  = 1  AND
                Datasett.Dato     = dsDato    AND
                DataSett.FilType  = 1 /* EL-Journal */
                USE-INDEX DataSett NO-ERROR.
            IF AVAILABLE DataSett THEN
                ASSIGN piSettNr = DataSett.SettNr + 1
                       dFilid   = DataSett.FilId.
            ELSE DO:
                ASSIGN dFilid   = lFilId 
                       piSettNr = 1.
            END.
            RELEASE DataSett. /* Ny post skal skapes. */
            CREATE DataSett.
            ASSIGN DataSett.DataSettId = plDataSettId
                   DataSett.pfFlagg    = 1
                   DataSett.ButikkNr   = iButikkNr 
                   DataSett.GruppeNr   = iGruppeNr
                   DataSett.KasseNr    = iKasseNr
                   DataSett.Dato       = dsDato
                   DataSett.SettNr     = piSettNr
                   DataSett.Tid        = 0
                   DataSett.FilId      = dFilId
                   DataSett.FilType    = 1 /* EL-Journal */
                   DataSett.SettStatus = 3
                   DataSett.Behandlet  = 3 /* Ekstra  */.
        END.
        IF cKontrolltabell = "1" THEN DO:
            FIND ApnSkjema WHERE ApnSkjema.ButikkNr = iButikkNr AND
                                 ApnSkjema.Ar       = YEAR(tmpDataDato.dato) NO-ERROR.  
            IF AVAIL ApnSkjema THEN DO:
                ASSIGN d31DecFgAr = DATE(12,31,ApnSkjema.Ar - 1).
                ENTRY(tmpDataDato.dato - d31DecFgAr,ApnSkjema.OpenClosed) = "4" NO-ERROR.
                RELEASE ApnSkjema.
            END.
        END.
    END. /* OPPRETTDATASETT */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-OpprettDatasettORG) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OpprettDatasettORG Procedure 
PROCEDURE OpprettDatasettORG :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
  
  iButikkNr 
  iGruppeNr 
  iKasseNr  
  cFilNavn  
  lFilId    
  iAntLinjer
------------------------------------------------------------------------------*/
  DEF VAR piLinjeNr    AS INT   NO-UNDO.
  DEF VAR piSettNr     AS INT   NO-UNDO.
  DEF VAR pdDato       AS DATE  NO-UNDO.
  DEF VAR plDataSettId AS DEC   NO-UNDO.
  DEF VAR pbKoblet     AS LOG   NO-UNDO.
  DEF VAR prRowId      AS ROWID NO-UNDO.
  DEF VAR piAntISett   AS INT   NO-UNDO.
  DEF VAR d31DecFgAr AS DATE NO-UNDO.
  DEF VAR lKontrollert AS LOG NO-UNDO.
  piLinjeNr = 1.

  LESERLINJER:
  FOR EACH tmpBongHode
      BREAK BY tmpBongHode.KasseNr:
/*     piLinjeNr = piLinjeNr + 1. */
      IF FIRST-OF(tmpBongHode.KasseNr) THEN
        OPPRETTDATASETT:
        DO:
          /* Ferdigstempler den vi hold på med. */
          IF prRowId <> ? THEN
          DO:
              FIND DataSett EXCLUSIVE-LOCK WHERE
                  ROWID(Datasett) = prRowid.
              ASSIGN
                DataSett.AntallLinjer = DataSett.AntallLinjer + piAntISett
                DataSett.SettStatus   = (IF DataSett.SettNr > 1
                                          THEN 3 /* Ekstra  */
                                          ELSE 2 /* Mottatt */)
                DataSett.SettStatus   = (IF DataSett.ButikkNr <> 0
                                          THEN DataSett.SettStatus
                                          ELSE 9  /* Ikke koblet */)
                piAntISett            = 0
                prRowId               = ?
                .
              /* Åpningsskjemahantering */
/*               IF cKontrolltabell = "1" AND lKontrollert = FALSE THEN DO:                            */
/*                   FIND ApnSkjema WHERE ApnSkjema.ButikkNr = DataSett.ButikkNr AND                   */
/*                                        ApnSkjema.Ar       = YEAR(DataSett.Dato) NO-ERROR.           */
/*                   IF AVAIL ApnSkjema THEN DO:                                                       */
/*                       ASSIGN d31DecFgAr = DATE(12,31,ApnSkjema.Ar - 1)                              */
/*                              ENTRY(DataSett.Dato - d31DecFgAr,ApnSkjema.OpenClosed) = "4" NO-ERROR. */
/*                   END.                                                                              */
/*                   ASSIGN lKontrollert = TRUE.                                                       */
/*               END.                                                                                  */
              /* Åpnings.... SLUTT      */
              RELEASE DataSett.
          END.
          ASSIGN iGruppeNr = 1 /*tmpFilLinjer.GruppeNr*/
                 iKasseNr  = tmpBongHode.KasseNr
                 pbKoblet = TRUE.

          /* Setter transaksjonsdato dd/mm/yy */
          ASSIGN pdDato = tmpBongHode.dato.

          /* Finner neste DataSettId */
          FIND LAST DataSett NO-LOCK
              USE-INDEX DataSettId NO-ERROR.
          IF AVAILABLE DataSett THEN
              plDataSettId = DataSett.DataSettId + 1.
          ELSE
              plDataSettId = 1.
          /* Finner neste SettNr */
          FIND LAST Datasett NO-LOCK WHERE
              Datasett.ButikkNr = iButikkNr AND
              Datasett.GruppeNr = iGruppeNr AND
              Datasett.KasseNr  = iKasseNr  AND
              Datasett.Dato     = pdDato    AND
              DataSett.FilType  = 1 /* EL-Journal */
              USE-INDEX DataSett NO-ERROR.
          IF AVAILABLE DataSett THEN
              piSettNr = DataSett.SettNr + 1.
          ELSE DO:
              piSettNr = 1.
          END.

          /* Alle kvitteringer som kommer inn på samme dag skal kobles  */
          /* til det samme datasettet. Forutsetning er at settet ikke   */
          /* har behandlingsstatus > 1.                                 */
/*           IF AVAILABLE DataSett THEN                             */
/*           DO:                                                    */
/*             IF DataSett.SettNr <= 3 AND DataSett.SettStatus <= 2 */
/*                 AND DataSett.Behandlet <= 1 THEN                 */
/*                 ASSIGN                                           */
/*                   plDataSettId = DataSett.DataSettId             */
/*                   piSettNr     = DataSett.SettNr                 */
/*                   .                                              */
/*             ELSE                                                 */
/*                 RELEASE DataSett. /* Ny post skal skapes. */     */
/*           END.                                                   */
          IF AVAIL DataSett THEN
              RELEASE DataSett. /* Ny post skal skapes. */
          IF NOT AVAILABLE DataSett THEN
          DO:
            CREATE DataSett.
            ASSIGN
                DataSett.DataSettId = plDataSettId
                DataSett.SettStatus = 8 /* Innlesning avbrutt */
                DataSett.pfFlagg    = 98.
            ASSIGN lKontrollert = FALSE.
          END.
          ELSE  /* Bruker det vi fant. */
          DO:
              FIND CURRENT DataSett EXCLUSIVE-LOCK.
          END.
          ASSIGN
            prRowId             = ROWID(DataSett)
            DataSett.ButikkNr   = iButikkNr 
            DataSett.GruppeNr   = iGruppeNr
            DataSett.KasseNr    = iKasseNr
            DataSett.Dato       = pdDato
            DataSett.SettNr     = piSettNr
            DataSett.Tid        = 0
            DataSett.FilId      = lFilId
            DataSett.FilType    = 1 /* EL-Journal */
            .
          /* Åpningsskjemahantering */
          IF cKontrolltabell = "1" AND lKontrollert = FALSE THEN DO:
              FIND ApnSkjema WHERE ApnSkjema.ButikkNr = DataSett.ButikkNr AND
                                   ApnSkjema.Ar       = YEAR(DataSett.Dato) NO-ERROR.
              IF AVAIL ApnSkjema THEN DO:
                  ASSIGN d31DecFgAr = DATE(12,31,ApnSkjema.Ar - 1)
                         ENTRY(DataSett.Dato - d31DecFgAr,ApnSkjema.OpenClosed) = "4" NO-ERROR.
              END.
              ASSIGN lKontrollert = TRUE.
          END.
          /* Åpnings.... SLUTT      */
    END. /* OPPRETTDATASETT */
    tmpBongHode.DataSettId = DataSett.DatasettId.
    piAntISett = piAntISett + 1.

    IF iAntLinjer MODULO 25 = 0 THEN
    DO:
      RUN Telleverk IN h_Telleverk 
          ("Fil: " + Filer.Katalog + "\" + Filer.Filnavn +  
           " Leser " + STRING(iAntLinjer) +
           " av " + STRING(iTotAntLinjer) + ".") NO-ERROR.
    END.

  END. /* LESERLINJER */

  RUN Telleverk IN h_Telleverk (" ") NO-ERROR.

  /* Stempler posten som innlest. */
  IF AVAILABLE DataSett THEN
  DO TRANSACTION:
      FIND CURRENT DataSett EXCLUSIVE-LOCK.
      ASSIGN
          DataSett.AntallLinjer = DataSett.AntallLinjer + piAntISett
          DataSett.SettStatus   = (IF DataSett.SettNr > 1
                                    THEN 3 /* Ekstra  */
                                    ELSE 2 /* Mottatt */)
          DataSett.SettStatus   = (IF DataSett.ButikkNr <> 0
                                    THEN DataSett.SettStatus
                                    ELSE 9  /* Ikke koblet */)
          .
      FIND CURRENT Filer EXCLUSIVE-LOCK.
      ASSIGN
          Filer.Innlest = TRUE
          Filer.InnlestDato = TODAY
          Filer.InnlestKl   = TIME
          Filer.InnlestAv   = USERID("SkoTex")
          .
      /* Åpningsskjemahantering */

      /* Görs i Opprettdatasettforpro */

      /* Åpnings.... SLUTT      */
  END.
  IF AVAILABLE DataSett THEN
      FIND CURRENT DataSett NO-LOCK.
  IF AVAILABLE Filer THEN
      FIND CURRENT Filer    NO-LOCK.

/*   RUN Telleverk IN h_Parent ("Kontrollerer at alle datasett er mottatt. Vent litt... ") NO-ERROR. */
/*   RUN sjekkdatasett.p (INPUT lFilId, INPUT cDatoListe).                                           */
  RUN Telleverk IN h_Parent (" ") NO-ERROR.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SaveError) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SaveError Procedure 
PROCEDURE SaveError :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    FIND Syspara WHERE SysPara.SysHId         = 210 AND
                               SysPara.SysGr  = 252 AND
                               SysPara.ParaNr = 1   NO-LOCK NO-ERROR.
    IF AVAIL SysPara THEN DO:
        ASSIGN cOutputDir = SysPara.Parameter1
               cOutputDir = RIGHT-TRIM(cOutputDir,"\") + "\".
        OUTPUT TO VALUE(cOutputDir + "ERROR_" + Filer.FilNavn + ".xls").
        PUT UNFORMATTED "Kvittonr" CHR(9) "Feltext" SKIP.
        FOR EACH TT_Error:
            PUT UNFORMATTED TT_Error.Bongnr CHR(9) TT_Error.cError SKIP.
        END.
        OUTPUT CLOSE.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SjekkKasseNr) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SjekkKasseNr Procedure 
PROCEDURE SjekkKasseNr :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       RUN SjekkKasseNr (INPUT-OUTPUT iTerminalNr).
------------------------------------------------------------------------------*/
DEF INPUT PARAMETER piTerminalNr AS INT  NO-UNDO.

DEF BUFFER bKasse FOR Kasse.

IF NOT CAN-FIND(Kasse WHERE
                Kasse.ButikkNr = ibutikkNr AND
                Kasse.GruppeNr = 1  AND
                Kasse.KasseNr  = piTerminalNr) THEN
  DO FOR bKasse TRANSACTION:
    /* Kasse 1 skal ALLTID være lagt opp på alle butikker. */
    FIND Kasse NO-LOCK WHERE
        Kasse.ButikkNr = iButikkNr AND
        Kasse.Gruppe   = 1 AND
        Kasse.KasseNr  = 1 NO-ERROR.
    IF AVAILABLE Kasse THEN
    DO:
        CREATE bKasse.
        BUFFER-COPY Kasse TO bKasse
            ASSIGN
            bKasse.KasseNr      = piTerminalNr
            bKasse.Navn         = "Kasse " + string(piTerminalNr) + " - Butikk " + string(iButikkNr)
/*             bKasse.ElJournal[2] = STRING(ibutikkNr)                                    */
/*             bKasse.ElJournalId  = STRING(ibutikkNr) + ";" + string(piTerminalNr) + ";" */
            .
    END.

    IF AVAILABLE bKasse THEN
        RELEASE bKasse.
  END. /* TRANSACTION */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Skift_Bokfdag) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Skift_Bokfdag Procedure 
PROCEDURE Skift_Bokfdag :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE iBokTermTid AS INTEGER    NO-UNDO.
    DEFINE BUFFER bSkift FOR Skift.
    DEFINE BUFFER bTT_Period FOR TT_Period.
    DATOLOOP:
    FOR EACH bTT_Period BREAK BY bTT_Period.dato:
/*         OUTPUT TO "c:\tmp\axe.txt" APPEND.                                                                                      */
/*         PUT UNFORMATTED                                                                                                         */
/*             substr(string(bTT_period.termdatotid),1,8) + "-" + string(int(substr(string(bTT_period.termdatotid),9)),"HH:MM:SS") */
/*             SKIP.                                                                                                               */
/*         EXPORT bTT_Period.                                                                                                      */
/*         OUTPUT CLOSE.                                                                                                           */
        IF FIRST-OF(bTT_Period.dato) THEN DO:
        /* detta gör vi först för att kunna sätta bokföringsid på skift vi terminerar */
            FOR EACH TT_Period WHERE TT_Period.dato = bTT_Period.dato BREAK BY TT_Period.TermDatoTid: /* skall bara kunna vara 1 !!! */
                IF LAST-OF(TT_Period.TermDatoTid) THEN DO:
                    IF CAN-FIND(Bokforingsdag WHERE Bokforingsdag.ButikkNr = iButikkNr AND
                                Bokforingsdag.Dato     = TT_Period.Dato AND
                                Bokforingsdag.datetime = TT_Period.TermDatoTid) THEN
                        NEXT DATOLOOP.
                    CREATE Bokforingsdag.
                    ASSIGN
                        Bokforingsdag.ButikkNr = iButikkNr
                        Bokforingsdag.Dato     = TT_Period.Dato
                        Bokforingsdag.GruppeNr = 1
                        Bokforingsdag.datetime = TT_Period.TermDatoTid
                        iBokTermTid            = TT_Period.TermTid NO-ERROR.
                    IF ERROR-STATUS:ERROR THEN DO:
                        DELETE Bokforingsdag.
                        NEXT.
                    END.

                    FOR EACH Skift WHERE Skift.ButikkNr = iButikkNr AND Skift.BokforingsId = 0 AND Skift.Terminert = TRUE AND Skift.datetime <= Bokforingsdag.datetime:
                        ASSIGN Skift.BokforingsId = Bokforingsdag.BokforingsId.
    /*                     FOR EACH TT_Shift WHERE TT_Shift.ButikkNr = iButikknr AND TT_Shift.SkiftNr = Skift.SkiftNr: */
    /*                         DELETE TT_Shift.                                                                        */
    /*                     END.                                                                                        */
                    END.
                    FOR EACH TT_Shift WHERE TT_shift.terminert = TRUE AND TT_Shift.TermDatoTid > 0 AND TT_Shift.TermDatoTid <= Bokforingsdag.datetime
                                                 AND TT_Shift.BokforingsId = 0:
                        TT_Shift.BokforingsId = Bokforingsdag.BokforingsId.
                    END.
                    RELEASE BokForingsdag.
                END.
            END.
        END.
    END.
    FOR EACH TT_Shift:
        FIND FIRST Skift WHERE Skift.ButikkNr     = iButikkNr AND
                               skift.BokforingsId = 0         AND 
                               skift.SkiftNr      = TT_Shift.SkiftNr NO-ERROR.
        IF NOT AVAIL Skift THEN DO:
            CREATE Skift.
            ASSIGN
                Skift.ButikkNr       = TT_Shift.ButikkNr
                Skift.Aar            = YEAR(TT_Shift.Dato) 
                Skift.SkiftNr        = TT_Shift.SkiftNr
                Skift.KassererNr     = 0
                Skift.Dato           = TT_Shift.Dato
                Skift.n9SkiftNr      = TT_Shift.sequenceNumber 
                Skift.BokforingsId   = TT_Shift.BokforingsId
                Skift.terminert      = TT_Shift.terminert
                Skift.datetime       = TT_Shift.Termdatotid NO-ERROR.
            IF ERROR-STATUS:ERROR THEN DO:
                DELETE Skift.
                NEXT.
            END.
        END.
        ELSE DO:
            ASSIGN Skift.BokforingsId   = TT_Shift.BokforingsId
                   Skift.datetime       = IF Skift.terminert THEN Skift.datetime ELSE TT_Shift.Termdatotid
                   Skift.terminert      = IF Skift.terminert THEN Skift.terminert ELSE TT_Shift.terminert.
        END.
        FOR EACH tmpBongHode WHERE tmpBongHode.Skiftnr = skift.skiftnr:
            tmpbonghode.skiftid = skift.skiftid.
        END.
    END.
/*     FOR EACH TT_Shift:                                                      */
/*         FIND FIRST Skift WHERE Skift.ButikkNr = iButikkNr AND               */
/* /*                                            skift.BokforingsId = 0 AND */ */
/*                                skift.SkiftNr = TT_Shift.SkiftNr NO-ERROR.   */
/*         IF NOT AVAIL Skift THEN DO:                                         */
/*             CREATE Skift.                                                   */
/*             ASSIGN                                                          */
/*                 Skift.ButikkNr       = TT_Shift.ButikkNr                    */
/*                 Skift.Aar            = year(TT_Shift.Dato)                  */
/*                 Skift.SkiftNr        = TT_Shift.SkiftNr                     */
/*                 Skift.KassererNr     = 0                                    */
/*                 Skift.Dato           = TT_Shift.Dato                        */
/*                 Skift.n9SkiftNr      = TT_Shift.sequenceNumber              */
/*                 Skift.BokforingsId   = TT_Shift.BokforingsId                */
/*                 Skift.terminert      = TT_Shift.terminert                   */
/*                 Skift.datetime       = TT_Shift.Termdatotid NO-ERROR.       */
/*             IF ERROR-STATUS:ERROR THEN DO:                                  */
/*                 DELETE Skift.                                               */
/*                 NEXT.                                                       */
/*             END.                                                            */
/*         END.                                                                */
/*         ELSE DO:                                                            */
/*             ASSIGN Skift.BokforingsId   = TT_Shift.BokforingsId             */
/*                    Skift.terminert      = TT_Shift.terminert                */
/*                    Skift.datetime       = TT_Shift.Termdatotid.             */
/*         END.                                                                */
/*         FOR EACH tmpBongHode WHERE tmpBongHode.Skiftnr = skift.skiftnr:     */
/*             tmpbonghode.skiftid = skift.skiftid.                            */
/*         END.                                                                */
/*     END.                                                                    */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Skift_BokfdagOld) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Skift_BokfdagOld Procedure 
PROCEDURE Skift_BokfdagOld :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE iBokTermTid AS INTEGER    NO-UNDO.
    DEFINE BUFFER bSkift FOR Skift.
    DEFINE BUFFER bTT_Period FOR TT_Period.
    FOR EACH bTT_Period BREAK BY bTT_Period.dato:
        IF FIRST-OF(bTT_Period.dato) THEN DO:
        /* detta gör vi först för att kunna sätta bokföringsid på skift vi terminerar */
            FOR EACH TT_Period WHERE TT_Period.dato = bTT_Period.dato BREAK BY TT_Period.TermDatoTid: /* skall bara kunna vara 1 !!! */
                IF LAST-OF(TT_Period.TermDatoTid) THEN DO:
                    CREATE Bokforingsdag.
                    ASSIGN
                        Bokforingsdag.ButikkNr = iButikkNr
                        Bokforingsdag.Dato     = TT_Period.Dato
                        Bokforingsdag.GruppeNr = 1
                        Bokforingsdag.datetime = TT_Period.TermDatoTid
                        iBokTermTid            = TT_Period.TermTid.
                END.
            END.
            IF AVAIL BokForingsdag THEN DO:
                FOR EACH Skift WHERE Skift.ButikkNr = iButikkNr AND Skift.BokforingsId = 0 AND Skift.Terminert = TRUE:
                    ASSIGN Skift.BokforingsId = Bokforingsdag.BokforingsId.
                END.
                FOR EACH tmpBongHode WHERE tmpBongHode.Skiftnr = skift.skiftnr:
                     tmpbonghode.skiftid = skift.skiftid.
                END.
        
            END.
            FOR EACH TT_Shift:
                IF TT_Shift.terminert = FALSE THEN DO:
                    FIND FIRST Skift WHERE Skift.ButikkNr = iButikkNr AND
                                           skift.BokforingsId = 0 AND
                                           skift.SkiftNr = TT_Shift.SkiftNr NO-LOCK NO-ERROR.
                    IF NOT AVAIL Skift THEN DO:
                        CREATE Skift.
                        ASSIGN
                            Skift.ButikkNr       = TT_Shift.ButikkNr
                            Skift.Aar            = YEAR(TT_Shift.Dato) 
                            Skift.SkiftNr        = TT_Shift.SkiftNr
                            Skift.KassererNr     = 0
                            Skift.Dato           = TT_Shift.Dato
                            Skift.n9SkiftNr      = TT_Shift.sequenceNumber NO-ERROR.
                        IF ERROR-STATUS:ERROR THEN DO:
                            DELETE Skift.
                            NEXT.
                        END.
                    END.
                    FOR EACH tmpBongHode WHERE tmpBongHode.Skiftnr = skift.skiftnr:
                        tmpbonghode.skiftid = skift.skiftid.
                    END.
                END.
                ELSE DO:
                    FIND Skift WHERE Skift.ButikkNr  = iButikkNr AND
                                     Skift.terminert = FALSE     AND
                                     Skift.SkiftNr   = TT_Shift.SkiftNr NO-ERROR.
                    IF NOT AVAIL Skift THEN DO:
                        CREATE Skift.
                        ASSIGN
                            Skift.ButikkNr       = TT_Shift.ButikkNr
                            Skift.Aar            = YEAR(TT_Shift.Dato) 
                            Skift.SkiftNr        = TT_Shift.SkiftNr
                            Skift.KassererNr     = 0
                            Skift.Dato           = TT_Shift.Dato
                            Skift.n9SkiftNr      = TT_Shift.sequenceNumber.
                    END.
                    ASSIGN Skift.terminert = TRUE.
                    IF TT_Shift.TermTid <= iBokTermTid AND AVAIL Bokforingsdag THEN
                           Skift.BokforingsId = Bokforingsdag.BokforingsId
                           .
                    FOR EACH tmpBongHode WHERE tmpBongHode.Skiftnr = skift.skiftnr:
                        tmpbonghode.skiftid = skift.skiftid.
                    END.
                END.
            END.
            RELEASE BokForingsdag.
        END.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SlettBokforingsdag) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SlettBokforingsdag Procedure 
PROCEDURE SlettBokforingsdag :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER dSlettDato AS DATE       NO-UNDO.

  FOR EACH bokforingsdag WHERE bokforingsdag.butikknr = iButikkNr AND
                               bokforingsdag.dato = dSlettDato:
      FOR EACH skift WHERE skift.bokforingsid = bokforingsdag.bokforingsid:
          DELETE skift.
      END.
      DELETE bokforingsdag.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-TaxKontroll) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE TaxKontroll Procedure 
PROCEDURE TaxKontroll :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE dTaxSum AS DECIMAL    NO-UNDO.
    DEFINE VARIABLE dKorrBelop AS DECIMAL    NO-UNDO.
/*     OUTPUT TO "C:\tmp\tax.txt" APPEND. */
    FOR EACH TT_TaxSum WHERE TT_TaxSum.ReceiptSeq = TT_Receipt.ReceiptSeq:
        IF TT_Receipt.Receipttype = "void" THEN
            NEXT.
        ASSIGN dTaxSum = 0.
        FOR EACH tmpBongLinje WHERE tmpBongLinje.b_Id = tmpBongHode.b_id AND tmpBongLinje.Makulert = FALSE AND tmpBongLinje.ttId < 11 AND tmpBongLinje.MvaGr = TT_TaxSum.taxClassId:
            ASSIGN dTaxSum = dTaxSum + ((IF tmpBongLinje.antall < 0 THEN -1 ELSE 1) * tmpBongLinje.MvaKr).
        END.
        IF dTaxSum <> TT_taxSum.taxAmount THEN DO:
            dKorrBelop = TT_taxSum.taxAmount - dTaxSum.
/*             IF tmpBongHode.dato = DATE(10,5,2006) THEN                                           */
/*             EXPORT DELIMITER ";" TT_taxsum.receiptnumber TT_taxSum.taxAmount dTaxSum dKorrbelop. */
            FOR EACH tmpBongLinje WHERE tmpBongLinje.b_Id = tmpBongHode.b_id AND tmpBongLinje.Makulert = FALSE AND 
                                   tmpBongLinje.ttId < 11 AND tmpBongLinje.MvaGr = TT_TaxSum.taxClassId BREAK BY tmpBongLinje.LinjeSum:
                IF TT_taxSum.taxAmount < 0 AND FIRST-OF(tmpBongLinje.LinjeSum) THEN DO:
                    ASSIGN tmpBongLinje.MvaKr = tmpBongLinje.MvaKr + dKorrBelop.
                    LEAVE.
                END.
                ELSE IF TT_taxSum.taxAmount > 0 AND LAST-OF(tmpBongLinje.LinjeSum) THEN DO:
                    ASSIGN tmpBongLinje.MvaKr = tmpBongLinje.MvaKr + dKorrBelop.
                    LEAVE.
                END.
            END.
        END.
    END.
/*     OUTPUT CLOSE. */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-TellOppLinjer) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE TellOppLinjer Procedure 
PROCEDURE TellOppLinjer :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  ASSIGN
      iTotAntLinjer = 0
      .
  INPUT STREAM InnFil FROM VALUE(Filer.Katalog + "~\" + Filer.FilNavn) NO-ECHO.
  REPEAT:
    IMPORT STREAM InnFil UNFORMATTED cLinje.
    ASSIGN
        iTotAntLinjer = iTotAntLinjer + 1
        .
  END.
  INPUT STREAM InnFil CLOSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-TomDag) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE TomDag Procedure 
PROCEDURE TomDag :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER cFilnamn AS CHARACTER   NO-UNDO.
DEFINE VARIABLE dDato AS DATE    NO-UNDO.
DEFINE VARIABLE cDato AS CHARACTER   NO-UNDO.
DEF VAR d31DecFgAr AS DATE NO-UNDO.
    ASSIGN cFilnamn = ENTRY(NUM-ENTRIES(cFilnamn,"\"),cFilnamn,"\")
           cDato = SUBSTR(ENTRY(2,cFilnamn,"_"),6,8)
           dDato = DATE(INT(SUBSTR(cDato,5,2)),INT(SUBSTR(cDato,7)),INT(SUBSTR(cDato,1,4))) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN
        RETURN.
    IF cKontrolltabell = "1" THEN DO:
      FIND ApnSkjema WHERE ApnSkjema.ButikkNr = iButikkNr AND
                           ApnSkjema.Ar       = YEAR(dDato) NO-ERROR.  
      IF AVAIL ApnSkjema THEN DO:
          ASSIGN d31DecFgAr = DATE(12,31,ApnSkjema.Ar - 1).
          ENTRY(dDato - d31DecFgAr,ApnSkjema.OpenClosed) = "4" NO-ERROR.
          RELEASE ApnSkjema.
      END.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

/* ************************  Function Implementations ***************** */

&IF DEFINED(EXCLUDE-finnesPROdatasett) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION finnesPROdatasett Procedure 
FUNCTION finnesPROdatasett RETURNS LOGICAL
  ( INPUT dStartDato AS DATE, OUTPUT dCreateDato AS DATE) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE dTmpDato   AS DATE       NO-UNDO.
  DEFINE VARIABLE ii         AS INTEGER    NO-UNDO.
  DEFINE VARIABLE d31decfgar AS DATE       NO-UNDO.
  
/*   MESSAGE "STARTDATO" dStartdato         */
/*       VIEW-AS ALERT-BOX INFO BUTTONS OK. */
/*                                          */
/*                                          */

  /* finn första tidigare godkända dag i åpningsskjema */
  ASSIGN d31decfgar = DATE(12,31,YEAR(dStartDato) - 1).

  FIND ApnSkjema WHERE ApnSkjema.ButikkNr = iButikkNr AND ApnSkjema.Ar = YEAR(dStartDato) NO-LOCK NO-ERROR.
  IF NOT AVAIL Apnskjema THEN
      RETURN FALSE.

  DO ii = dStartDato - 1 - d31decfgar TO 1 BY -1:
/*       IF ii > 365 THEN DO:                       */
/*           MESSAGE "first part" SKIP              */
/*               dStartdato SKIP                    */
/*               d31decfgar                         */
/*               VIEW-AS ALERT-BOX INFO BUTTONS OK. */
/*       END.                                       */
       IF ENTRY(ii,ApnSkjema.OpenClosed) = "4" THEN DO:
           ASSIGN dCreateDato = d31decfgar + ii.
           LEAVE.
       END.
  END.
  IF dCreateDato <> ? THEN DO:
      FIND FIRST DataSett WHERE DataSett.ButikkNr = iButikkNr AND 
                               Datasett.Dato     = dCreateDato AND
                               DataSett.pfFlagg  < 98 USE-INDEX Apningsskjema NO-LOCK NO-ERROR.
      IF AVAIL DataSett AND dCreateDato = Datasett.dato THEN
          RETURN TRUE.
      ELSE DO:
          RETURN FALSE.
      END.
  END.
  ELSE DO:
/*NY*/ ASSIGN dStartDato = DATE(12,31,YEAR(dStartDato) - 1). 
/*NY*/ FIND ApnSkjema WHERE ApnSkjema.ButikkNr = iButikkNr AND ApnSkjema.Ar = YEAR(dStartDato) NO-LOCK NO-ERROR.
/*       FIND ApnSkjema WHERE ApnSkjema.ButikkNr = iButikkNr AND ApnSkjema.Ar = YEAR(dStartDato) - 1 NO-LOCK NO-ERROR. */
      IF NOT AVAIL Apnskjema THEN
          RETURN FALSE.
      ASSIGN d31decfgar = DATE(12,31,ApnSkjema.Ar - 1).
/*       ASSIGN d31decfgar = DATE(12,31,ApnSkjema.Ar). */
      DO ii = dStartDato - 1 - d31decfgar TO 1 BY -1:
/*           IF ii > 365 THEN DO:                       */
/*               MESSAGE "second part" SKIP             */
/*                   dStartdato SKIP                    */
/*                   d31decfgar                         */
/*                   VIEW-AS ALERT-BOX INFO BUTTONS OK. */
/*           END.                                       */
           IF ENTRY(ii,ApnSkjema.OpenClosed) = "4" THEN DO:
               ASSIGN dCreateDato = d31decfgar + ii.
               LEAVE.
           END.
      END.
      IF dCreateDato <> ? THEN DO:
          FIND FIRST DataSett WHERE DataSett.ButikkNr = iButikkNr AND 
                                   Datasett.Dato     = dCreateDato AND
                                   DataSett.pfFlagg  < 98 USE-INDEX Apningsskjema NO-LOCK NO-ERROR.
          IF AVAIL DataSett AND dCreateDato = Datasett.dato THEN
              RETURN TRUE.
          ELSE DO:
              RETURN FALSE.
          END.
      END.
      ELSE
          RETURN FALSE.
  END.
  /* Är det inom samma år? */
/*   IF YEAR(dStartDato) = YEAR(dTmpDato) THEN DO:                                                             */
/*       /* finner vi en dag innan startdat som det skall skapas datasett för */                               */
/*                                                                                                             */
/*   END.                                                                                                      */
/*   FIND ApnSkjema WHERE ApnSkjema.ButikkNr = iButikkNr AND ApnSkjema.Ar = YEAR(dStartDato) NO-LOCK NO-ERROR. */

  RETURN FALSE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-fixChkEAN) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fixChkEAN Procedure 
FUNCTION fixChkEAN RETURNS CHARACTER
    ( INPUT cKode AS CHARACTER ) :
  /*------------------------------------------------------------------------------
    Purpose:  Räknar ut checksiffra för ean EAN-kod - parameter utan chksiffra
              i.e 12 lång
      Notes:  
  ------------------------------------------------------------------------------*/
      DEF VAR iCount1 AS INTE NO-UNDO.
      DEF VAR iMulti  AS INTE INIT 1 NO-UNDO.
      DEF VAR iSum AS INTE NO-UNDO.
      DO iCount1 = LENGTH(cKode) TO 1 BY -1:  
          ASSIGN iMulti = IF iMulti = 1 THEN 3 ELSE 1
                 iSum = iSum + INT(SUBSTR(cKode,iCount1,1)) * iMulti.
      END.
      RETURN cKode + string((10 - iSum MODULO 10) MODULO 10).
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getOKschema) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getOKschema Procedure 
FUNCTION getOKschema RETURNS LOGICAL
  ( INPUT bongdato AS DATE, OUTPUT iAr AS INTEGER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE d31decFgar  AS DATE       NO-UNDO.
  DEFINE VARIABLE cOpenclosed AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE iDaynum     AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iVentet     AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iExtra      AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iFirstForventet AS INTEGER    NO-UNDO.

  ASSIGN d31decFgar = DATE(12,31,YEAR(bongdato) - 1).
         iDayNum    = bongdato - d31decFgar.
  FIND ApnSkjema WHERE ApnSkjema.ButikkNr = iButikkNr AND ApnSkjema.Ar = YEAR(bongdato) - 1 NO-LOCK NO-ERROR.
  IF AVAIL ApnSkjema AND (CAN-DO(ApnSkjema.OpenClosed,"1") OR CAN-DO(ApnSkjema.OpenClosed,"2")) THEN DO:
      ASSIGN iAr = ApnSkjema.Ar.
      RETURN FALSE.
  END.
  FIND ApnSkjema WHERE ApnSkjema.ButikkNr = iButikkNr AND ApnSkjema.Ar = YEAR(bongdato) NO-LOCK NO-ERROR.
  IF AVAIL ApnSkjema THEN DO:
      ASSIGN iVentet  = LOOKUP("1",ApnSkjema.OpenClosed)
             iExtra   = LOOKUP("2",ApnSkjema.OpenClosed).
      iFirstForventet = IF iVentet = 0 AND iExtra = 0 THEN iDaynum
                   ELSE IF iVentet = 0 THEN iExtra
                   ELSE IF iExtra = 0 THEN iVentet
                   ELSE MIN(iVentet,iExtra).
  END.
  IF NOT AVAIL ApnSkjema THEN DO:
      iAr = 0.
      RETURN FALSE.
  END.
  ELSE IF AVAIL ApnSkjema AND iFirstForventet < iDaynum THEN DO:
      ASSIGN iAr = ApnSkjema.Ar.
      RETURN FALSE.
  END.
  ELSE DO:
      iAr = ApnSkjema.Ar.
      RETURN TRUE.
  END.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

