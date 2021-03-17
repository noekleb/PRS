&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        :  xinnprseljournal.p
    Purpose     :  Innlesning av kvitteringsfil fra PRSPos.

    Syntax      :

    Description :  

    Author(s)   :  Kenneth Olausseon
    Created     :  
    Notes       :
    
    
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEF INPUT  PARAMETER lFilId      AS DEC    NO-UNDO.
DEF INPUT  PARAMETER h_Parent    AS HANDLE NO-UNDO.
DEF INPUT  PARAMETER h_Telleverk AS HANDLE NO-UNDO.
DEF OUTPUT PARAMETER iAntLinjer  AS INT    NO-UNDO.

DEFINE VARIABLE bOk AS LOG NO-UNDO.
DEFINE VARIABLE ocReturn AS CHARACTER NO-UNDO.
DEFINE VARIABLE bBrukTBId2 AS LOG NO-UNDO.

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

DEFINE VARIABLE iGantAktiv             AS INTEGER   NO-UNDO. 
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
DEF VAR cCl         AS CHAR NO-UNDO.
DEF VAR iCl         AS INTE NO-UNDO.
DEFINE VARIABLE lUseB_id AS LOGICAL     NO-UNDO.

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
DEF VAR i0%Momskod      LIKE Moms.MomsKod NO-UNDO.
DEFINE VARIABLE cGavekortArtikler AS CHARACTER NO-UNDO.
DEFINE VARIABLE bTest   AS LOG NO-UNDO.
DEFINE VARIABLE lInnloser AS LOG NO-UNDO.
DEFINE VARIABLE cReturVerdi AS CHARACTER  NO-UNDO.
DEFINE VARIABLE iAntBonger AS INTEGER    NO-UNDO.
DEFINE VARIABLE cReturnStatus AS CHARACTER   NO-UNDO. /* från xml-läsning */
DEFINE VARIABLE lFirstBetallinje AS LOGICAL    NO-UNDO.  /* skall användas för att läggas i 'bonglinje.forkonvertering' */
                                                         /* första betallinjen får JA för att vi skall kunna hantera riktigt */
                                                         /* när vi uppdaterar KD_Data. Om 'JA' och kort skall vi ta hand om volym på kort */
DEFINE VARIABLE lB_id_satt AS LOGICAL     NO-UNDO.
DEF STREAM InnFil.
DEF STREAM Bong.


DEF TEMP-TABLE tmpDataDato NO-UNDO
    FIELD dato AS DATE
    FIELD doDatasett AS LOGICAL
    INDEX dato dato.

DEFINE BUFFER bufFiler FOR Filer.
DEF BUFFER bufButiker FOR Butiker.
DEF BUFFER clButiker  FOR Butiker.
DEF TEMP-TABLE tmpBongHode  NO-UNDO LIKE BongHode
  FIELD SelgerId AS INTEGER FORMAT ">>>9"
  .
DEF TEMP-TABLE tmpBongLinje NO-UNDO LIKE BongLinje
    FIELD cPOS AS CHAR.
DEF BUFFER tmpMixBongLinje FOR tmpBongLinje.

DEF TEMP-TABLE TT_kasse NO-UNDO
    FIELD uteinne AS CHAR
    FIELD kassenr AS INTE
    INDEX uk IS PRIMARY UNIQUE uteinne kassenr.


DEF BUFFER btmpBongHode FOR tmpBongHode.

DEFINE TEMP-TABLE TT_Error NO-UNDO
    FIELD BongNr  AS INTE
    FIELD cError  AS CHAR.

{xmln9bos.i &NEW=NEW}
/* Bruke TBId = 2 for VArer på vei ved overføringer */
{syspara.i 11 6 1 cTekst}
IF cTekst = '1' THEN 
    bBrukTBId2 = TRUE.

/* TN 13/8-20 */
{syspara.i 210 100 8 iGantAktiv INT}
IF iGantAktiv = 1 THEN 
  ASSIGN
    cGavekortArtikler = '9001,9002'
    . 
    
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&IF DEFINED(EXCLUDE-getMixMvaKr) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getMixMvaKr Procedure 
FUNCTION getMixMvaKr RETURNS DECIMAL
        (  ) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getMixVVarekost) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getMixVVarekost Procedure 
FUNCTION getMixVVarekost RETURNS DECIMAL
        (  ) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getRabatt) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getRabatt Procedure 
FUNCTION getRabatt RETURNS DECIMAL
        (  ) FORWARD.

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
         HEIGHT             = 29.48
         WIDTH              = 64.2.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

ASSIGN
    cBONGLst  = "0102,1202,1302,1402,1502" 
    cPOSLst   = ""
    cTTIDLst  = ""
    cKonvArt  = "1,2,70,71,72,73,74,75,76,77,78,79,80,81,82"
    bTest     = TRUE 
    .

RUN NyFilLogg IN h_Parent (INPUT lFilId, STRING(TODAY) + " " + 
          STRING(TIME,"HH:MM:SS") + " " + USERID("skotex") + 
         " - xinnprseljournal.p startet.").

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
    iButikkNr = INT(ENTRY(NUM-ENTRIES(cFilNavn,"."),cFilNavn,".")) NO-ERROR.
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
{syspara.i 5 1 1 cCL}
iCl = INT(cCl) NO-ERROR.
IF NOT ERROR-STATUS:ERROR THEN DO:
    FIND clButiker WHERE clButiker.butik = iCl NO-LOCK NO-ERROR.
END.

/* Leser inn fil og legger linjene inn i tmpFiles.                   */
/* Videre behandling av bongene gjøres med utgangspunkt i tmpFilene. */

{syspara.i 1 1 25 cKontrolltabell}
IF NOT CAN-DO("1,2",cKontrolltabell) THEN
    ASSIGN cKontrolltabell = "1".

{syspara.i 50 20 10 cTekst}
IF NOT CAN-DO("1,2",cTekst) THEN
    ASSIGN lInnloser = TRUE.

IF NOT VALID-HANDLE(h_dproclib) THEN
    RUN dproclib PERSISTENT SET h_dproclib.

FIND LAST Moms WHERE Moms.MomsProc = 0 NO-LOCK NO-ERROR.
IF AVAIL Moms THEN
    i0%Momskod = Moms.MomsKod.
ELSE
    i0%Momskod = ?.

RUN InnLesFil.    /* El-Journal. */

/* Kontrollera mot öppetschema */
IF CAN-FIND(FIRST tmpBonghode) THEN DO:
    RUN BehandlaData.
END.
ELSE IF AVAIL tmpDataDato THEN DO:
    ASSIGN cReturVerdi = "Data saknas i filmottak".
END.
ELSE DO:
    ASSIGN cReturVerdi = "". /* tomma filer backuppas */
END.
IF CAN-FIND(FIRST TT_Error) THEN
    RUN SaveError.
IF VALID-HANDLE(h_dproclib) THEN
    DELETE PROCEDURE h_dproclib.
    
IF AVAILABLE Filer THEN 
DO FOR bufFiler TRANSACTION:
  FIND bufFiler EXCLUSIVE-LOCK WHERE 
    ROWID(bufFiler) = ROWID(Filer) NO-ERROR.
  IF AVAILABLE bufFiler THEN 
  DO:
    ASSIGN 
      bufFiler.AntLinjer = iTotantLinjer
      .
    RELEASE bufFiler.
  END.
END.

RETURN cReturVerdi.
.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-BehandlaData) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE BehandlaData Procedure 
PROCEDURE BehandlaData :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    IF NOT AVAIL bufButiker THEN
        FIND bufButiker WHERE bufButiker.butik = iButikknr NO-LOCK.
    RUN OpprettDatasett.
    RUN LagreBonger.
    RUN OppdaterFiler.

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
    DEFINE INPUT  PARAMETER cData AS CHARACTER   NO-UNDO.
    DEFINE OUTPUT PARAMETER lFeil AS LOGICAL  NO-UNDO.
    DEFINE OUTPUT PARAMETER rRowId AS ROWID NO-UNDO.
    
    DEFINE VARIABLE ii AS INTEGER    NO-UNDO.
    DEFINE VARIABLE cDato AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE dDato AS DATE    NO-UNDO.
    DEFINE VARIABLE cCreateError AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE dTMPb_id AS DECIMAL     NO-UNDO.
    DEFINE VARIABLE iInt AS DECIMAL NO-UNDO.
    
    cReturverdi = "".
    /* workaround */
    IF ENTRY(10,cData,";") = "no" THEN DO:
        ASSIGN ENTRY(10,cData,";") = ""
               ENTRY(11,cData,";") = "".
    END.
    IF NUM-ENTRIES(cData,";") > 15 THEN 
    DO:
        ASSIGN dTMPb_id = DECI(ENTRY(16,cData,";")) NO-ERROR.
        IF ERROR-STATUS:ERROR THEN
            lB_id_satt = FALSE.
        ELSE
            lB_id_satt = TRUE.
    END.
    ELSE lB_id_satt = FALSE.
    cDato = ENTRY(5,cData,";").
    dDato = DATE(INT(SUBSTR(cDato,5,2)),INT(SUBSTR(cDato,7,2)),INT(SUBSTR(cDato,1,4))).
    IF CAN-FIND(btmpBongHode WHERE
                btmpBongHode.ButikkNr   = INT(ENTRY(2,cData,";")) AND
                btmpBongHode.GruppeNr   = INT(ENTRY(3,cData,";")) AND
                btmpBongHode.KasseNr    = INT(ENTRY(4,cData,";")) AND
                btmpBongHode.Dato       = dDato                   AND
                btmpBongHode.BongNr     = INT(ENTRY(6,cData,";"))) THEN DO:
        ASSIGN lFeil = TRUE.
        RETURN.
/*         IF NUM-ENTRIES(cData,";") < 14 THEN. */
    END.
    IF lB_id_satt = FALSE THEN DO:
        FIND LAST btmpBongHode NO-LOCK USE-INDEX B_Id NO-ERROR.
        dTMPb_id = IF AVAIL btmpBongHode THEN btmpBongHode.B_Id + 1 ELSE 1.
    END.
    ELSE
        lUseB_id = TRUE.
    CREATE tmpBongHode.
    ASSIGN /* Setter indeksfeltene */
      tmpBongHode.B_Id           = dTMPb_id
      tmpBongHode.butikknr       = INT(ENTRY(2,cData,";"))
      tmpBongHode.gruppenr       = INT(ENTRY(3,cData,";"))
      tmpBongHode.kassenr        = INT(ENTRY(4,cData,";"))
      tmpbonghode.Dato           = dDato
      tmpBongHode.BongNr         = INT(ENTRY(6,cData,";"))
      tmpBongHode.Tid            = INT(ENTRY(7,cData,";"))
      tmpBongHode.Belop          = DECI(ENTRY(8,cData,";"))
      tmpBongHode.KassererNr     = INT(ENTRY(9,cData,";"))
      tmpBongHode.KundeKort      = ENTRY(10,cData,";")
      tmpBongHode.MedlemsKort    = ENTRY(11,cData,";")
      tmpBongHode.MedlemsNr      = DECI(ENTRY(12,cData,";"))
      tmpBongHode.SelgerNr       = INT(ENTRY(13,cData,";"))
      tmpBongHode.Makulert       = INT(ENTRY(14,cData,";"))
      tmpBongHode.BongStatus     = 5
      rRowId                     = ROWID(tmpBongHode)
      .
    IF NUM-ENTRIES(cData,";") > 14 THEN
        tmpBongHode.kundenr = DECI(ENTRY(15,cData,";")).
      iAntBonger             = iAntBonger + 1.

BONGHODEINFO:
DO:
    ASSIGN
        tmpBongHode.KortType        = 1. /* (1-Ingen, 2-Kunde, 3-Medlem) */
/*         tmpBongHode.flBetalingskort = TT_Receipt.cardamount <> 0  */
/*         tmpBongHode.flBankkort      = tmpBongHode.flBetalingskort */
/*         tmpBongHode.flKreditkort    = tmpBongHode.flBetalingskort */
    /* Kobler til kasserer */
   .
   IF TRIM(tmpBongHode.MedlemsKort) <> '' THEN 
   DO:
       FIND FIRST Medlemskort NO-LOCK WHERE MedlemsKort.KortNr = tmpBongHode.MedlemsKort NO-ERROR.
       /* Deretter med ledende nuller */
       IF NOT AVAILABLE MedlemsKort AND LENGTH(tmpBongHode.Medlemskort) <= 6 THEN
           FIND FIRST MedlemsKort NO-LOCK WHERE MedlemsKort.KortNr = STRING(INT(tmpBongHode.Medlemskort),"999999") NO-ERROR.
       ELSE IF NOT AVAILABLE MedlemsKort AND LENGTH(tmpBongHode.Medlemskort) > 6 THEN
           FIND FIRST MedlemsKort NO-LOCK WHERE MedlemsKort.KortNr = tmpBongHode.Medlemskort NO-ERROR.
       /* Vi har funnet et medlemskort, altså er dette et medlem. */
       IF AVAILABLE MedlemsKort THEN MEDLEMSKORT: 
       DO:
           tmpBongHode.KortType = 3.
           FIND Medlem OF MedlemsKort NO-LOCK NO-ERROR.
           IF AVAILABLE Medlem THEN 
           DO:
               ASSIGN tmpBongHode.MedlemsNr  = Medlem.MedlemsNr
                      tmpBongHode.MedlemNavn = Medlem.Fornavn + " " + Medlem.EtterNavn.
               /* Er medlemmet koblet til en kunde, skal også kundenummer settes i transaksjonen. */
               IF tmpBongHode.Kundekort = "" AND Medlem.KundeNr <> 0 THEN 
               DO:
                 FIND Kunde NO-LOCK WHERE
                     Kunde.KundeNr = Medlem.KundeNr NO-ERROR.
                 ASSIGN
                   tmpBongHode.KundeNr   = Medlem.KundeNr
                   tmpBongHode.KundeNavn = IF AVAILABLE Kunde
                                  THEN Kunde.Navn
                                  ELSE "*Ukjent*".
               END.
           END.
       END.
       ELSE
           tmpBongHode.MedlemsKort = "".
   END.
    IF TRIM(tmpBongHode.KundeKort) <> '' THEN 
    DO:
       FIND FIRST KundeKort NO-LOCK WHERE KundeKort.KortNr = tmpBongHode.Kundekort NO-ERROR.
       IF NOT AVAILABLE KundeKort AND LENGTH(tmpBongHode.Kundekort) <= 6 THEN
           FIND FIRST KundeKort NO-LOCK WHERE KundeKort.KortNr = STRING(INT(tmpBongHode.Kundekort),"999999") NO-ERROR.
       ELSE IF NOT AVAILABLE KundeKort AND LENGTH(tmpBongHode.Kundekort) > 6 THEN
           FIND FIRST KundeKort NO-LOCK WHERE KundeKort.KortNr = tmpBongHode.Kundekort NO-ERROR.
       IF AVAILABLE KundeKort THEN 
       DO:
         FIND Kunde OF KundeKort NO-LOCK NO-ERROR.
             ASSIGN tmpBongHode.KundeNr   = KundeKort.KundeNr
                    tmpBongHode.KundeNavn = IF AVAIL kunde THEN Kunde.Navn ELSE "Ukjent".
       END.
    END.
    FIND ButikkForsalj NO-LOCK WHERE ButikkForsalj.Butik = tmpBongHode.ButikkNr AND
        ButikkForsalj.KassererId = int(tmpBongHode.KassererNr) NO-ERROR.
    IF AVAILABLE ButikkForsalj THEN 
    DO:
        tmpBongHode.KassererNr = ButikkForsalj.ForsNr.
        FIND Forsalj NO-LOCK WHERE Forsalj.ForsNr = ButikkForsalj.ForsNr NO-ERROR.
        IF AVAILABLE Forsalj THEN
            tmpBongHode.KassererNavn = Forsalj.FoNamn.
    END.
    FIND ButikkSelger WHERE 
      ButikkSelger.ButikkNr = tmpBongHode.ButikkNr AND 
      Butikkselger.selgerid = tmpBongHode.Selgernr NO-LOCK NO-ERROR.
    IF AVAILABLE ButikkSelger THEN 
      FIND Selger NO-LOCK WHERE 
        Selger.SelgerNr = ButikkSelger.SelgerNr NO-ERROR.
    IF NOT AVAILABLE Selger THEN 
      FIND Selger NO-LOCK WHERE 
        Selger.SelgerNr = tmpBongHode.Selgernr NO-ERROR.
    ASSIGN 
        tmpBongHode.SelgerId   = INT(tmpBongHode.Selgernr)
        tmpBongHode.SelgerNr   = (IF AVAILABLE ButikkSelger THEN ButikkSelger.SelgerNr ELSE tmpBongHode.SelgerNr) 
        tmpBongHode.SelgerNavn = (IF AVAILABLE Selger THEN Selger.Navn ELSE 'Ukjent selger')
        .

    /* Inn/utlogging av selger */
    IF AVAILABLE Selger THEN 
    DO:
        CREATE tmpBongLinje.
        ASSIGN 
        tmpBongLinje.b_id       = tmpBongHode.b_id
        tmpBongLinje.ButikkNr   = tmpBonghode.ButikkNr
        tmpBongLinje.GruppeNr   = tmpBonghode.GruppeNr
        tmpBongLinje.KasseNr    = tmpBonghode.KasseNr 
        tmpBongLinje.BongNr     = tmpBonghode.BongNr  
        tmpBongLinje.Dato       = tmpBonghode.Dato
        tmpBongLinje.TTId       = 146
        tmpBongLinje.Antall     = 0
        tmpBongLinje.BongTekst  = STRING(tmpBongHode.SelgerNr)
        tmpBongLinje.LinjeNr    = 99999 /* här måste vi ta i */
        tmpBongLinje.RefNr      = 0
        tmpBongLinje.TransDato  = tmpBongLinje.Dato
        tmpBongLinje.TransTid   = tmpBongHode.Tid
        tmpBongLinje.reftekst   = '' NO-ERROR.
        IF ERROR-STATUS:ERROR THEN
            DELETE tmpBongLinje.
    END.        
END.

IF bTest THEN
  TEMP-TABLE  tmpBongHode:WRITE-JSON ("file", 'log\' + 'tmpBongHode.' + REPLACE(STRING(TODAY),'/','') + 'JSon',TRUE).
  
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
   DEFINE INPUT  PARAMETER cData AS CHAR  NO-UNDO.
   
   DEFINE VARIABLE iKoeff AS INTEGER     NO-UNDO.
   DEFINE VARIABLE dArtikkelnr AS DECIMAL     NO-UNDO.
   DEFINE VARIABLE dKostProcent AS DECIMAL     NO-UNDO.
   DEFINE VARIABLE dLinjesum_u_mva AS DECIMAL     NO-UNDO.
   DEFINE VARIABLE cFakturaRef AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE iTestKort AS INTEGER     NO-UNDO.
   DEFINE VARIABLE iButnr    AS INTEGER     NO-UNDO.

   DEFINE BUFFER buftmpBongLinje FOR tmpBongLinje.

  /* Här skapar vi en bonglinje för salg eller tendering */
  /* I vissa fall skapar vi en extra bonglinje för ex. inbetaling/utbetaling*/
    CREATE tmpBongLinje.
    ASSIGN 
    /* Unik index 1 */
    tmpBongLinje.b_id       = tmpBongHode.b_id
    
    /* Unik index 2 */
    tmpBongLinje.ButikkNr   = tmpBonghode.ButikkNr
    tmpBongLinje.GruppeNr   = tmpBonghode.GruppeNr
    tmpBongLinje.KasseNr    = tmpBonghode.KasseNr 
    tmpBongLinje.Dato       = tmpBonghode.Dato
    tmpBongLinje.BongNr     = tmpBonghode.BongNr  
    tmpBongLinje.LinjeNr    = INT(ENTRY(11,cData,";"))
    NO-ERROR .
/*    
MESSAGE 'Bonglinje' SKIP
    tmpBonghode.ButikkNr
    tmpBonghode.GruppeNr
    tmpBonghode.KasseNr 
    tmpBonghode.Dato
    tmpBonghode.BongNr      
    INT(ENTRY(11,cData,";"))
    ENTRY(10,cData,";")
VIEW-AS ALERT-BOX.    
*/    
    ASSIGN 
    /* Øvrige data */
    tmpBongLinje.Antall     = DECI(ENTRY(7,cData,";"))
    tmpBongLinje.ArtikkelNr = ENTRY(8,cData,";")
    tmpBongLinje.BongPris   = DECI(ENTRY(9,cData,";"))
    tmpBongLinje.BongTekst  = ENTRY(10,cData,";")
    tmpBongLinje.LopeNr     = INT(ENTRY(12,cData,";"))
    tmpBongLinje.LinjeRab   = DECI(ENTRY(13,cData,";"))
    tmpBongLinje.LinjeSum   = DECI(ENTRY(14,cData,";"))
    tmpBongLinje.Makulert   = ENTRY(15,cData,";") = "J"
    tmpBongLinje.MButikkNr  = INT(ENTRY(16,cData,";"))
    tmpBongLinje.Mva%       = DECI(ENTRY(17,cData,";"))
    tmpBongLinje.MvaGr      = INT(ENTRY(18,cData,";"))
    tmpBongLinje.MvaKr      = DECI(ENTRY(19,cData,";"))
    tmpBongLinje.PrisPrSalgsenhet = DECI(ENTRY(20,cData,";"))
    tmpBongLinje.RefNr            = INT(ENTRY(21,cData,";"))
    tmpBongLinje.ReturButikk      = INT(ENTRY(22,cData,";"))
    tmpBongLinje.ReturKasserer    = INT(ENTRY(23,cData,";"))
    tmpBongLinje.Storrelse        = ENTRY(24,cData,";")
    tmpBongLinje.Strekkode        = ENTRY(25,cData,";")
    tmpBongLinje.SubtotalRab      = DECI(ENTRY(26,cData,";"))
    tmpBongLinje.TTId             = INT(ENTRY(27,cData,";"))
    tmpBongLinje.TBId             = IF NUM-ENTRIES(cData,";") >= 38 THEN INT(ENTRY(38,cData,";")) ELSE tmpBongLinje.TBId
    tmpBongLinje.VareGr           = INT(ENTRY(28,cData,";"))
    tmpBongLinje.OrgVareGr        = tmpBongLinje.VareGr
    tmpBongLinje.VVarekost        = DECI(ENTRY(29,cData,";"))
    tmpBongLinje.TransDato        = tmpBongLinje.Dato
    tmpBongLinje.TransTid         = tmpBongHode.Tid.
    IF NUM-ENTRIES(cData,";") > 29 THEN
        ASSIGN tmpBongLinje.Feilkode         = INT(ENTRY(30,cData,";"))
               tmpBongLinje.FeilkodeTekst    = ENTRY(31,cData,";")
               tmpBongLinje.Notatkode        = INT(ENTRY(32,cData,";"))
               tmpBongLinje.NotatkodeTekst   = ENTRY(33,cData,";").
    IF  NUM-ENTRIES(cData,";") > 33 THEN
        tmpBongLinje.originaldata   = ENTRY(34,cData,";").
    IF  NUM-ENTRIES(cData,";") > 34 THEN
        tmpBongLinje.reftekst       = ENTRY(35,cData,";").
    IF  NUM-ENTRIES(cData,";") > 35 THEN
        tmpBongLinje.KampId         = DEC(ENTRY(36,cData,";")).
    IF  NUM-ENTRIES(cData,";") > 36 THEN
        tmpBongLinje.KampTilbId     = INT(ENTRY(37,cData,";")).
    IF  NUM-ENTRIES(cData,";") > 38 THEN
        tmpBongLinje.Normalpris     = DEC(ENTRY(39,cData,";")).
    /* 38 längre upp */
    /* Dobbelsjekker Artikkel ut fra strekkode.                           */
    IF CAN-DO('1,3,10',STRING(tmpBongLinje.TTId)) THEN 
    DO:
        FIND Strekkode NO-LOCK WHERE
          Strekkode.Kode = tmpBongLinje.Strekkode NO-ERROR.
        FIND ArtBas OF Strekkode NO-LOCK NO-ERROR. /* vi måste trigga non_sale */
        IF AVAIL artbas AND ArtBas.NON_Sale THEN DO:
            tmpBonglinje.ProduktType = IF ArtBas.NegVare THEN 9 ELSE 8.
            IF tmpBongLinje.Mva% > 0 AND i0%Momskod <> ? THEN DO:
                ASSIGN
                    tmpBongLinje.Mva%       = 0
                    tmpBongLinje.MvaGr      = i0%Momskod
                    tmpBongLinje.MvaKr      = 0.
            END.
        END.

        /* Dette TRIGGER bare når kassens register ikke er i synk med bakrom. */
        IF AVAILABLE Strekkode AND Strekkode.ArtikkelNr <> DEC(tmpBongLinje.ArtikkelNr) THEN 
          DO:
/*               FIND ArtBas OF Strekkode NO-LOCK NO-ERROR. Flyttat upp för att vi skall kunna hantera non_sale */
              FIND StrKonv OF Strekkode NO-LOCK NO-ERROR.
              IF AVAILABLE ArtBas THEN
                ASSIGN
                  tmpBongLinje.ArtikkelNr = STRING(ArtBas.ArtikkelNr)
                  tmpBongLinje.VareGr     = ArtBas.Vg
                  tmpBongLinje.LopeNr     = ArtBas.LopNr
                  tmpBongLinje.Storrelse  = IF AVAILABLE StrKonv THEN StrKonv.Storl ELSE tmpBongLinje.Storrelse.
/*  Produkttype används vi SIEexport */
                  . 
              IF AVAILABLE Moms THEN RELEASE Moms.
              IF AVAILABLE ArtBas THEN 
                FIND VarGr OF ArtBas NO-LOCK NO-ERROR.
              IF AVAILABLE VarGr THEN 
                FIND Moms OF VarGr NO-LOCK NO-ERROR.
              IF AVAILABLE Moms AND NOT artbas.non_sale THEN
                  ASSIGN
                      tmpBongLinje.Mva%       = Moms.MomsProc
                      tmpBongLinje.MvaGr      = Moms.MomsKod
                      tmpBongLinje.MvaKr      = tmpBongLinje.LinjeSum - (tmpBongLinje.LinjeSum / (1 + (Moms.MomsProc / 100)))
                      . 
          END.
          IF tmpBonglinje.Linjerab > 0 OR tmpBonglinje.SubtotalRab > 0 THEN
              tmpBongHode.flRabatt = TRUE.
    END.
    RUN bibl_fixstorl.p (tmpBongLinje.Storrelse,?,'',OUTPUT ocReturn,OUTPUT bOk).
    tmpBongLinje.Storrelse = ocReturn.
    
    /* Salg av elektronisk gavekort */
    /*
    if tmpBongLinje.TTId = 1 AND tmpBongLinje.BongTekst BEGINS 'Gavekort' THEN 
      ASSIGN
      tmpBongLinje.Strekkode = (IF NUM-ENTRIES(tmpBonglinje.BongTekst,'-') >= 2 THEN ENTRY(2,tmpBongLinje.BongTekst,'-')  ELSE tmpBongLinje.BongTekst) /* Gavekort ID */
      tmpBongLinje.BongTekst = STRING(DATE(TODAY + (355 * 3))) 
      tmpBongLinje.TTId = 134.
    */
    
    /* TN 13/8-20 Salg av elektronisk gavekort, er ikke varesalg, og skal legges ut som gavekort. */
    /* Ved manuelle gavekort, artikkel 9001, leverer kassen 134 i TTId.                           */
    /* NB: cGavekortArtikler er bare satt nå riGantAktiv = 1.                                     */
    /*
    VARESALG:             BONGLINJE;10;1;1;262528;20200118;1;9828713;140;VAREPRØVER ACC                     ;4;24;40; 140;N;0;25;1;20; 140;0;0;0;ONESIZE;7321360874265;0;  1;700730;24,5;0;;0;;Str ONESIZE 410;;0;0;0;140
    Manuelt gavekort:     BONGLINJE;10;1;1;262528;20200118;1;9001;1500;Manuelle gavekort                     ;5; 1; 0;1500;N;0; 0;0; 0;1500;0;0;0;       ;         7991;0;134;  9001;0;0;;0;;Str  ;;0;0;0;1500
    Elektronisk gavekort: BONGLINJE;2;1;4;12832;20200514;1;9002;150;Elektronisk gavekort -9578411004203851424;1; 1; 0; 150;N;0; 0;0; 0; 150;0;0;0;       ;         9002;0;  1;  9002;0;0;;0;;Str  ;;0;0;0;150
    */
    IF tmpBongLinje.TTId = 1 AND CAN-DO(cGaveKortArtikler,tmpBongLinje.ArtikkelNr) THEN
    DO: 
      tmpBongLinje.TTId = 134.
      IF NUM-ENTRIES(tmpBongLinje.BongTekst,'-') = 2 AND tmpBongLinje.BongTekst BEGINS 'Elektronisk' THEN 
        tmpBongLinje.Strekkode = ENTRY(2,tmpBongLinje.BongTekst,'-').
    END.
    IF iGantAktiv = 1 AND tmpBongLinje.TTId = 95 AND LENGTH(tmpBongLinje.BongTekst) >= 19 THEN 
      DO:
        FIND FIRST buftmpBongLinje WHERE 
          buftmpBongLinje.b_id = tmpBongLinje.b_id AND 
          buftmpBongLinje.ttid = 52 AND 
          buftmpBongLinje.tbid = 38 AND 
          buftmpBongLinje.Strekkode = '' NO-ERROR.
        IF AVAILABLE buftmpBongLinje THEN 
          buftmpBongLinje.Strekkode = TRIM(tmpBongLinje.BongTekst).
      END. 
    /* TN 13/8-20 Slutt endring. */
    
    
    /* Kuponger fra Kuponginnløen */
    IF CAN-DO('205',STRING(tmpBongLinje.TTId)) AND tmpBongLinje.TBId > 0 THEN 
    DO:
        FIND TransBeskr NO-LOCK WHERE
          TransBeskr.TTId = tmpBongLinje.TTId AND 
          TransBeskr.TBId = tmpBongLinje.TBId NO-ERROR.
        IF AVAILABLE TransBeskr THEN 
          tmpBongLinje.BongTekst = TransBeskr.Beskrivelse + '|' + tmpBongLinje.Originaldata.
    END.
    
    /* Overføringer */
    IF tmpBongLinje.TTId = 6 AND bBrukTBId2 = TRUE THEN
    OVERFORBLOKK:
    DO: 
      /* Setter TBID for postering av overføringer. */
      tmpBongLinje.TBId = 2.
    END. /* OVERFORBLOKK */
    
    /* Etikett Batch */
    IF tmpBongLinje.TTId = 25 THEN 
      tmpBongLinje.LinjeSum = DEC(tmpBongLinje.Antall) / 10.
    
    /* Akonto innbetaling (Kunde er angitt) skal ha transkode 89. */
    IF tmpBongLinje.TTID = 61 AND (tmpBongHode.KundeKort <> '' OR tmpBongHode.KundeNr > 0) THEN 
      tmpBongLinje.TTId = 89.
    
    IF tmpBongLinje.TTId = 109 THEN 
    DO:
      IF NUM-ENTRIES(tmpBongLinje.BongTekst,'|') >= 4 THEN 
      DO:
        FIND KampanjeTilbud NO-LOCK WHERE 
          KampanjeTilbud.KampId     = DEC(ENTRY(1,tmpBonglinje.BongTekst,'|')) AND 
          KampanjeTilbud.KampTilbId = INT(ENTRY(2,tmpBonglinje.BongTekst,'|')) NO-ERROR.
        IF AVAILABLE KampanjeTilbud THEN 
          FIND KampanjeMixMatch OF KampanjeTilbud NO-LOCK NO-ERROR.
        IF AVAILABLE KampanjeTilbud AND AVAILABLE KampanjeMixMatch THEN
        DO: 
          ASSIGN
            tmpBongLinje.KampEierId = KampanjeMixMatch.KampEierId
            tmpBongLinje.KampId     = DEC(ENTRY(1,tmpBonglinje.BongTekst,'|'))
            tmpBongLinje.KampTilbId = INT(ENTRY(2,tmpBonglinje.BongTekst,'|'))
            tmpBongLinje.LinjeSum   = tmpBongLinje.Antall * DEC(ENTRY(3,tmpBonglinje.BongTekst,'|')).
          /* 109 transaksjonen kommer alltid i slutten av bongen derfor kan vi summere linjene i bongen. */
          ASSIGN tmpBongLinje.MvaKr     = DEC(getMixMvaKr()) / tmpBongLinje.Antall.  
          IF tmpBongLinje.MvaKr = ? THEN tmpBongLinje.MvaKr = 0.
          ASSIGN tmpBongLinje.VVareKost = DEC(getMixVVarekost()) / tmpBongLinje.Antall.  
          IF tmpBongLinje.VVareKost = ? THEN tmpBongLinje.VVareKost = 0.
          ASSIGN tmpBongLinje.LinjeRab = DEC(getRabatt()).  
          IF tmpBongLinje.LinjeRab = ? THEN tmpBongLinje.LinjeRab = 0.
        END.    
      END.
    END.
    
    /* Betaling med manuelle gavekort er merket spesielt i bongteksten ;1,Andre,178,0; */
    IF tmpBongLinje.TTId = 53 THEN 
    DO:
        IF NUM-ENTRIES(tmpBongLinje.BongTekst)>= 3 THEN DO:
            IF LENGTH(tmpBonglinje.strekkode) = 22 THEN DO:
                iButnr = INT(SUBSTR(tmpBonglinje.strekkode,7,6)) NO-ERROR.
                IF ERROR-STATUS:ERROR THEN
                    iButnr = 0.
            END.
            ASSIGN tmpBongLinje.Antall = IF iButNr > 0 THEN iButNr ELSE DEC(ENTRY(3,tmpBongLinje.BongTekst)) /* Butikknr utstedende butikk */
                .
        END.
    END.

    IF tmpBongLinje.TTId = 66 THEN 
    DO:
        IF ENTRY(35,cData,";") = "EXTERN" THEN DO:
            ASSIGN tmpBongLinje.Antall    = DEC(ENTRY(1,ENTRY(25,cData,";"),"-")) /* Butikknr utstedende butikk */
                   tmpBongLinje.BongTekst = "EXTERN".
        END.
        ELSE IF LENGTH(ENTRY(25,cData,";")) = 22 THEN 
        ASSIGN
    /* Betaling med tilgodelapper skal merkes spesielt i bongteksten ;1311120001780200000624; */
            tmpBongLinje.Antall    = DEC(SUBSTRING(ENTRY(25,cData,";"),7,6)) /* Butikknr utstedende butikk */
            tmpBongLinje.BongTekst = IF tmpBongLinje.ButikkNr = INT(tmpBongLinje.Antall) 
                                       THEN "0,Eget," + STRING(tmpBongLinje.ButikkNr) + ",0" 
                                       ELSE "1,Andre," + STRING(INT(tmpBongLinje.Antall)) + ",0" 
            tmpBongLinje.MButikk   = INT(tmpBongLinje.Antall).
            .
    END.
    
    IF tmpBongLinje.TTId = 52 THEN 
      KORTBETALING:
      DO:
        IF tmpBongLinje.Antall = 0 THEN 
            tmpBongLinje.Antall = 1.
        tmpBongHode.flBetalingsKort = TRUE.
        tmpBongHode.flBankKort      = TRUE.

        /* Här ska test mot korttyp läggas in i TBId för senare rätt kontering 20120330/ghg */
        iTestKort = INT(tmpBongLinje.BongTekst) NO-ERROR.
        IF ERROR-STATUS:ERROR THEN
        DO:
          IF lInnloser THEN 
            FIND FIRST TransBeskr WHERE
              TransBeskr.TTId = 52 AND
              TransBeskr.Innloser = tmpBongLinje.BongTekst NO-LOCK NO-ERROR.
          ELSE DO: 
            FIND FIRST TransBeskr WHERE
              TransBeskr.TTId = 52 AND
              TransBeskr.Beskrivelse = tmpBongLinje.BongTekst NO-LOCK NO-ERROR.
          END.
          IF AVAILABLE TransBeskr THEN
          DO:
            ASSIGN tmpBongLinje.TBId   = TransBeskr.TBId
                   tmpBongLinje.Antall = TransBeskr.TBId.
          END.
        END.
        /* Fanger opp blandt annet Gavekort Kjede */
        ELSE IF CAN-FIND(FIRST TransBeskr WHERE 
                               TransBeskr.TTId = 52 AND 
                               TransBeskr.TBId = iTestKort) THEN 
        DO:
          ASSIGN 
            tmpBongLinje.TBId   = iTestKort
            tmpBongLinje.Antall = tmpBongLinje.TBId.
        END.
        ELSE DO:
          /* Dette er Netx bankkort typer. */
          IF CAN-DO('0,1,2,9,10,17,20,30',STRING(iTestKort)) THEN
            tmpBongLinje.TTId = 58. 
          ASSIGN 
            tmpBongLinje.TBId   = (IF iTestKort = 0 THEN 1 ELSE iTestKort)
            tmpBongLinje.Antall = tmpBongLinje.TBId.
        END.
      END. /* KORTBETALING */
    ELSE IF tmpBongLinje.TTId = 62 THEN 
    UTBETALING:
    DO:
        ASSIGN
            tmpBongLinje.BongPris   = tmpBongLinje.BongPris * -1 
            tmpBongLinje.LinjeRab   = tmpBongLinje.LinjeRab * -1
            tmpBongLinje.LinjeSum   = tmpBongLinje.LinjeSum * -1
            .
    END. /* UTBETALING */
    
    ELSE IF tmpBongLinje.TTId = 65 THEN DO:
          IF tmpBongLinje.RefNr > 0 OR tmpBongLinje.reftekst <> "" THEN DO:
            cFakturaRef = STRING(tmpBongLinje.refnr) + CHR(1) + tmpBongLinje.reftekst.
            CREATE tmpBongLinje.
            ASSIGN 
            tmpBongLinje.b_id       = tmpBongHode.b_id
            tmpBongLinje.ButikkNr   = tmpBonghode.ButikkNr
            tmpBongLinje.GruppeNr   = tmpBonghode.GruppeNr
            tmpBongLinje.KasseNr    = tmpBonghode.KasseNr 
            tmpBongLinje.BongNr     = tmpBonghode.BongNr  
            tmpBongLinje.Dato       = tmpBonghode.Dato
            tmpBongLinje.TTId       = 88
            tmpBongLinje.Antall     = INT(ENTRY(1,cFakturaRef,CHR(1)))
            tmpBongLinje.BongTekst  = ENTRY(2,cFakturaRef,CHR(1))
            tmpBongLinje.LinjeNr    = 99998 /* här måste vi ta i */
            tmpBongLinje.RefNr      = tmpBongLinje.Antall
            tmpBongLinje.TransDato  = tmpBongLinje.Dato
            tmpBongLinje.TransTid   = tmpBongHode.Tid
            tmpBongLinje.reftekst   = tmpBongLinje.BongTekst NO-ERROR.
            IF ERROR-STATUS:ERROR THEN
                DELETE tmpBongLinje.
        END.
    END.
    ELSE
        dArtikkelnr = DECI(tmpbonglinje.artikkelnr) NO-ERROR.
    IF dArtikkelnr > 0 THEN DO:

        FIND Artbas WHERE Artbas.artikkelnr = dArtikkelnr NO-LOCK NO-ERROR.
        IF NOT AVAIL ArtBas THEN
            FIND Artbas WHERE artbas.vg = tmpBonglinje.varegr AND
                              artbas.lopnr = tmpbonglinje.lopenr NO-LOCK NO-ERROR.
        IF AVAIL ArtBas THEN DO:
            FIND artpris WHERE artpris.artikkelnr = artbas.artikkelnr AND artpris.profilnr = bufbutiker.profilnr NO-LOCK NO-ERROR.
            IF NOT AVAIL artpris THEN DO:
                FIND artpris WHERE artpris.artikkelnr = artbas.artikkelnr AND artpris.profilnr = clButiker.profilnr NO-LOCK NO-ERROR.
                IF NOT AVAIL artpris THEN
                    FIND FIRST artpris WHERE artpris.artikkelnr = artbas.artikkelnr.
            END.

            IF ArtBas.Opris = TRUE AND tmpBonglinje.vvarekost = 0 THEN DO:
                IF AVAIL artpris AND artpris.varekost[1] > 0 AND artpris.pris[1] - artpris.mvakr[1] > artpris.varekost[1] THEN DO:
                    dKostprocent = ROUND(artpris.varekost[1] / (artpris.pris[1] - artpris.mvakr[1]) * 100,2).
                END.
                ELSE DO:
                    FIND vargr WHERE vargr.vg = artbas.vg NO-LOCK NO-ERROR.
                    IF AVAIL vargr THEN
                        dKostprocent = VarGr.Kost_Proc.
                END.
                IF dKostProcent > 0 THEN DO:
                    dLinjesum_u_mva = tmpBonglinje.linjesum - ROUND(tmpBonglinje.linjesum * (ArtPris.Mva%[1] / (ArtPris.Mva%[1] + 100)),2).
                    tmpBonglinje.vvarekost = dLinjesum_u_mva * dKostProcent / 100.
                END.
            END.
            IF ArtBas.Opris = FALSE AND AVAIL artpris THEN
                tmpBongLinje.Normalpris = artpris.pris[1].
            ASSIGN tmpBongLinje.HovedGr = artbas.hg
                   tmpBongLinje.Levnr   = artbas.levnr.
            FIND Lager WHERE Lager.Butik      = tmpBonglinje.ButikkNr AND
                             Lager.Artikkelnr = DECI(tmpbonglinje.artikkelnr) NO-LOCK NO-ERROR.
            IF AVAIL Lager AND Lager.Lagant > 0 AND Lager.VVarekost > 0 THEN DO:

                iKoeff = IF tmpBonglinje.TTId = 10 THEN -1 ELSE 1.
                tmpBongLinje.VVarekost = tmpBongLinje.Antall * Lager.Vvarekost * iKoeff.                
            END.
            /* Setter varekost også der hvor lager ikke finnes. */
            IF AVAILABLE ArtPris AND tmpBongLinje.VVareKost = 0 THEN 
                tmpBongLinje.VVareKost = tmpBongLinje.Antall * ArtPris.VareKost[1].   
            /* For å sikre innlesning fra gamle versjoner av PRS kassen. */
            /* Koden kvar med tillägg av negvare för att vi får fel där  */
            IF tmpBongLinje.VVareKost < 0 AND Artbas.negvare = FALSE THEN 
              tmpBongLinje.VVareKost = tmpBongLinje.VVareKost * -1.
              
        END.
    END.

    IF bTest THEN
      TEMP-TABLE  tmpBongLinje:WRITE-JSON ("file", 'log\' + 'tmpBongLinje.' + REPLACE(STRING(TODAY),'/','') + 'JSon',TRUE).

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
  DEFINE VARIABLE rRowId AS ROWID NO-UNDO.

  DEFINE VARIABLE cRad AS CHARACTER   NO-UNDO.

  DEFINE VARIABLE dDato AS DATE       NO-UNDO.
  DEFINE VARIABLE dB_id AS DECIMAL     NO-UNDO.
/*---------------------*/

  ASSIGN
      iantLinjer    = 0
      iTotAntLinjer = 0
      .

  RUN Telleverk IN h_Parent ("Leser og konverterer data fra fil: " + cFilNavn + ". Vent litt... (" + SEARCH(cFilNavn) + ")") NO-ERROR.
  IF SEARCH(cFilNavn) = ? THEN
  DO:
      RUN Telleverk IN h_Parent ("FEILINNLES Avbryter.") NO-ERROR.
      RETURN "FEILINNLES".
  END.
  INPUT FROM VALUE(cFilNavn).
  LESINNBUFFER:
  REPEAT:
      iTotAntLinjer = iTotAntLinjer + 1.
      IMPORT UNFORMATTED cRad.
      cRad = TRIM(cRad).
      
      IF bTest THEN
        RUN Telleverk IN h_Parent ("  Rad: " + cRad) NO-ERROR.
      
      IF cRad = "" THEN
          NEXT.
      IF ENTRY(1,cRad,";") = "BONGHODE" THEN 
      DO:
          RELEASE tmpBongHode.
          RUN CreateTmpBonghode (cRad,OUTPUT lFeil, OUTPUT rRowId).
          FIND tmpbongHode WHERE 
            ROWID(tmpBongHode) = rRowId.
      END.
      ELSE IF ENTRY(1,cRad,";") = "BONGLINJE" THEN 
      DO:
          IF AVAIL tmpBongHode THEN
              RUN CreateTmpBongLinje (cRad).
      END.
      cRad = ''.
/*       IF lFeil = TRUE THEN */
/*           NEXT.            */
  END. /* LESINNBUFFE */

  /* Nullstiller telleverket */
  RUN Telleverk IN h_Telleverk (" ") NO-ERROR.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Kassereroppgjor) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Kassereroppgjor Procedure 
PROCEDURE Kassereroppgjor :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE dOpptaltinveksel         AS DECIMAL     NO-UNDO.
DEFINE VARIABLE dOpptaltkontanter        AS DECIMAL     NO-UNDO.
DEFINE VARIABLE dOpptaltdropp            AS DECIMAL     NO-UNDO.
DEFINE VARIABLE dOpptalttillgodo         AS DECIMAL     NO-UNDO.
DEFINE VARIABLE dOpptalttillgodoandra    AS DECIMAL     NO-UNDO.
DEFINE VARIABLE dOpptaltpresentkort      AS DECIMAL     NO-UNDO.
DEFINE VARIABLE dOpptaltrikspresentkort  AS DECIMAL     NO-UNDO.
DEFINE VARIABLE dOpptaltkupong2          AS DECIMAL     NO-UNDO.
DEFINE VARIABLE dOpptaltbilag            AS DECIMAL     NO-UNDO.
DEFINE VARIABLE dOpptaltgavekortut       AS DECIMAL     NO-UNDO.
DEFINE VARIABLE dOpptalttilgodeut        AS DECIMAL     NO-UNDO.
DEFINE VARIABLE dOpptaltveksel           AS DECIMAL     NO-UNDO.
DEFINE VARIABLE dOpptaltlevertbank       AS DECIMAL     NO-UNDO.
DEFINE VARIABLE cPoseNr                  AS CHARACTER   NO-UNDO.
DEFINE VARIABLE iKassererNr              AS INTEGER     NO-UNDO.
DEFINE VARIABLE iZNr                     AS INTEGER     NO-UNDO.
DEFINE VARIABLE cOppjDatoTmp             AS CHARACTER   NO-UNDO.
DEFINE VARIABLE dOppjDato                AS DATE        NO-UNDO.

    IF NUM-ENTRIES(Bonglinje.originaldata,":") <> 14 THEN
        RETURN.
    ASSIGN dOpptaltinveksel        = DECI(ENTRY(1,Bonglinje.originaldata,":"))
           dOpptaltkontanter       = DECI(ENTRY(2,Bonglinje.originaldata,":"))
           dOpptaltdropp           = DECI(ENTRY(3,Bonglinje.originaldata,":"))
           dOpptalttillgodo        = DECI(ENTRY(4,Bonglinje.originaldata,":"))
           dOpptalttillgodoandra   = DECI(ENTRY(5,Bonglinje.originaldata,":"))
           dOpptaltpresentkort     = DECI(ENTRY(6,Bonglinje.originaldata,":"))
           dOpptaltrikspresentkort = DECI(ENTRY(7,Bonglinje.originaldata,":"))
           dOpptaltkupong2         = DECI(ENTRY(8,Bonglinje.originaldata,":"))
           dOpptaltbilag           = DECI(ENTRY(9,Bonglinje.originaldata,":"))
           dOpptaltgavekortut      = DECI(ENTRY(10,Bonglinje.originaldata,":"))
           dOpptalttilgodeut       = DECI(ENTRY(11,Bonglinje.originaldata,":"))
           dOpptaltveksel          = DECI(ENTRY(12,Bonglinje.originaldata,":"))
           dOpptaltlevertbank      = DECI(ENTRY(13,Bonglinje.originaldata,":"))
           cPoseNr                 = ENTRY(14,Bonglinje.originaldata,":").
    
    iKassererNr = BongHode.Kasserernr.    
    
    FIND ButikkForsalj NO-LOCK WHERE
        ButikkForsalj.Butik      = Bonghode.ButikkNr AND
        ButikkForsalj.KassererId = INT(BongHode.kasserernr) NO-ERROR.
    IF AVAILABLE ButikkForsalj THEN
        iKassererNr = ButikkForsalj.ForsNr.
    ELSE DO:
        FIND FIRST forsalj WHERE forsalj.butikknr = bonghode.butikknr NO-LOCK NO-ERROR.
        IF AVAIL forsalj THEN
            iKassererNr = forsalj.forsnr.
    END.

    cOppjDatoTmp = TRIM(bonglinje.bongtekst).
    IF cOppjDatoTmp <> "" THEN DO:
        dOppjDato = DATE(INT(SUBSTR(cOppjDatoTmp,5,2)),INT(SUBSTR(cOppjDatoTmp,7,2)),INT(SUBSTR(cOppjDatoTmp,1,4))) NO-ERROR.
        IF ERROR-STATUS:ERROR THEN
            dOppjDato = bonghode.dato.
    END.
    ELSE
        dOppjDato = bonghode.dato.

    Z-NUMMER:
    DO iZNr = 1 TO 99:
      IF NOT CAN-FIND(KassererOppgj WHERE 
          KassererOppgj.Dato       = dOppjDato          AND
          KassererOppgj.ButikkNr   = BongLinje.ButikkNr AND
          KassererOppgj.KassererNr = iKassererNr        AND
          KassererOppgj.Z_Nummer   = iZNr) THEN 
        LEAVE Z-NUMMER. 
    END. /* Z-NUMMER */

    FIND KassererOppgj EXCLUSIVE-LOCK WHERE 
        KassererOppgj.Dato       = dOppjDato          AND
        KassererOppgj.ButikkNr   = BongLinje.ButikkNr AND
        KassererOppgj.KassererNr = iKassererNr        AND
        KassererOppgj.Z_Nummer   = iZNr
        NO-ERROR.
    
    IF NOT AVAILABLE KassererOppgj THEN
    OPPRETT-OPPGJOR:
    DO:
      FIND Butiker NO-LOCK WHERE
          Butiker.Butik = BongLinje.ButikkNr NO-ERROR.
      CREATE KassererOppgj.
      ASSIGN
          KassererOppgj.Dato       = dOppjDato 
          KassererOppgj.ButikkNr   = BongLinje.ButikkNr 
          KassererOppgj.KassererNr = iKassererNr 
          KassererOppgj.Z_Nummer   = iZNr
          KassererOppgj.SelgerNr   = BongHode.SelgerNr 
          KassererOppgj.SelgerId   = tmpBongHode.SelgerId
          .
    END. /* OPPRETT-OPPGJOR */

/*    ASSIGN                                                             */
/*      KassererOppgj.OpptaltInnVeksel = KassererOppgj.OpptaltInnVeksel +*/
/*                                       IF AVAILABLE Butiker            */
/*                                         THEN Butiker.StdVeksel        */
/*                                         ELSE 0.                       */
/* TN 28/5-20 Det er bare første avlesning av inngående veksel som skal gjelde den dagen. */
/* Hos Gant logger de inn etter at de har tatt kassereroppgjøret for å stemple seg ut.    */
/* Når de gjør dette, er det mange som også angir ingående veksel på nytt.                */
    ASSIGN
      KassererOppgj.OpptaltInnVeksel = KassererOppgj.OpptaltInnVeksel + 
                                       (IF KassererOppgj.OpptaltInnVeksel = 0 THEN 
                                          dOpptaltinveksel
                                        ELSE 
                                          0).

    /* Default opprettelse av relaterte poster. */
    /* Oppretter bilag */
    IF NOT CAN-FIND(FIRST KassererBilag WHERE KassererBilag.ButikkNr     = BongHode.ButikkNr AND
                                              KassererBilag.Dato         = dOppjDato     AND
                                              KassererBilag.KassererNr   = iKasserernr       AND
                                              KassererBilag.Z_Nummer     = iZNr) THEN DO:
        CREATE KassererBilag.
        ASSIGN KassererBilag.ButikkNr     = BongHode.ButikkNr 
               KassererBilag.Dato         = dOppjDato 
               KassererBilag.KassererNr   = iKassererNr 
               KassererBilag.Z_Nummer     = iZNr
               KassererBilag.BilagsNr     = 1.
    END.
    FIND FIRST KassererBilag EXCLUSIVE-LOCK WHERE KassererBilag.ButikkNr     = BongHode.ButikkNr AND
                                                  KassererBilag.Dato         = dOppjDato     AND
                                                  KassererBilag.KassererNr   = iKassererNr       AND
                                                  KassererBilag.Z_Nummer     = iZNr.
/*     /* Legger inn default fra kasse */                                             */
/*     ASSIGN KassererBilag.Belop  = KassererBilag.Belop + KassererOppgj.OpptaltBilag */
/*            KassererBilag.Meknad = "Kasse".                                         */
 
    /* Oppretter valører */
    IF NOT CAN-FIND(FIRST KassererKontanter WHERE
                KassererKontanter.ButikkNr     = BongHode.ButikkNr AND
                KassererKontanter.Dato         = dOppjDato     AND
                KassererKontanter.KassererNr   = iKassererNr       AND
                KassererKontanter.Z_Nummer     = iZNr) THEN
    DO:
        CREATE KassererKontanter.
        ASSIGN
            KassererKontanter.ButikkNr     = BongHode.ButikkNr 
            KassererKontanter.Dato         = dOppjDato 
            KassererKontanter.KassererNr   = iKassererNr 
            KassererKontanter.Z_Nummer     = iZNr
            .
    END.
    FIND FIRST KassererKontanter EXCLUSIVE-LOCK WHERE
        KassererKontanter.ButikkNr     = BongHode.ButikkNr AND
        KassererKontanter.Dato         = dOppjDato AND
        KassererKontanter.KassererNr   = iKassererNr AND
        KassererKontanter.Z_Nummer     = iZNr.
    /* Beløp fra kasse legges inn i KRONE valøren */
/*     ASSIGN                                                                                                    */
/*         KassererKontanter.Belop[2]        = KassererKontanter.Belop[2]       + KassererOppgj.OpptaltKontanter */
/*         KassererKontanter.AntallValor[2]  = KassererKontanter.AntallValor[2] + KassererOppgj.OpptaltKontanter */
/*         .                                                                                                     */
     /* Gammelt record layout - Opp til og med 14 entries.*/
  ASSIGN KassererOppgj.OpptaltKontanter        = dOpptaltkontanter + dOpptaltdrOpp
/*         KassererOppgj.OpptaltSjekk      = DEC(ENTRY(  9,Bonglinje.Originaldata,";")) / 100 */
/*         KassererOppgj.OpptaltReserve    = DEC(ENTRY( 10,Bonglinje.Originaldata,";")) / 100 */
         KassererOppgj.OpptaltGavekort         = dOpptaltpresentkort
         KassererOppgj.OpptaltBilag            = dOpptaltbilag
         KassererOppgj.OpptaltTilgode          = dOpptalttillgodo
         KassererOppgj.OpptaltLevertBank       = dOpptaltlevertbank
/*          KassererOppgj.OpptaltGavekortAndre    = dOpptaltrikspresentkort + dOpptaltkupong2 */
         KassererOppgj.OpptaltKupong           = dOpptaltrikspresentkort + dOpptaltkupong2
         KassererOppgj.OpptaltGavekortUtlevert = dOpptaltgavekortut
         KassererOppgj.OpptaltTilgodeAndre     = dOpptalttillgodoandra
         KassererOppgj.OpptaltTilgodeUtlevert  = dOpptalttilgodeut
         KassererOppgj.PoseNr                  = cPoseNr.
 
/* 
  /* Oppretter valuta */
  FOR EACH KasValuta NO-LOCK WHERE
      KasValuta.ValAktiv = TRUE AND
      KasValuta.EgenValuta = FALSE AND
      KasValuta.KasseValkurs <> 0:

      FIND KassererValuta EXCLUSIVE-LOCK WHERE
          KassererValuta.ButikkNr     = BongHode.ButikkNr AND
          KassererValuta.Dato         = dOppjDato AND
          KassererValuta.KassererNr   = piForsNr AND
          KassererValuta.Z_Nummer     = 1 AND
          KassererValuta.ValKod       = KasValuta.ValKod NO-ERROR.
      IF NOT AVAILABLE KassererValuta THEN
      DO:
          CREATE KassererValuta.
          ASSIGN
              /* Nøkkel */
              KassererValuta.ButikkNr     = BongHode.ButikkNr 
              KassererValuta.Dato         = dOppjDato 
              KassererValuta.KassererNr   = piForsNr 
              KassererValuta.Z_Nummer     = 1
              KassererValuta.ValKod       = KasValuta.ValKod
              .
      END.
      ASSIGN
          KassererValuta.KasseValkurs = KasValuta.KasseValkurs / KasValuta.Indeks
          KassererValuta.KasseValkurs = IF KassererValuta.KasseValkurs = ?
                                          THEN 0
                                          ELSE KassererValuta.KasseValkurs
          .
  END.
 */
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
    DEFINE VARIABLE cDatasettId AS CHARACTER NO-UNDO.
    DEFINE VARIABLE ii          AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iLinjeNr    AS INTEGER   NO-UNDO.
    
    DEFINE BUFFER bufBongHode FOR Bonghode.
    
    RUN NyFilLogg IN h_Parent (INPUT lFilId,"      Start LagreBonger.").
    
    BONGHODE:
    FOR EACH tmpBongHode:
        iLinjeNr = 0. /* Skal nulles for hver bong */
        IF CAN-FIND(BongHode WHERE BongHode.ButikkNr = tmpBongHode.ButikkNr AND
                                   BongHode.gruppenr = 1                    AND
                                   BongHode.Kassenr  = tmpBongHode.Kassenr  AND
                                   BongHode.Dato     = tmpBongHode.Dato     AND
                                   BongHode.BongNr   = tmpBongHode.BongNr) THEN
        DO:
            /* Kassereroppgjør kommer på bongnr = 0 ??? og skal legges inn på egne linjer på samme bong. */
            IF tmpBongHode.BongNr > 0 OR 
               NOT CAN-FIND(FIRST tmpBongLinje WHERE 
                                  tmpBongLinje.B_Id = tmpBongHode.B_Id AND 
                                  tmpBonglinje.TTId = 150) THEN
            DO:
              IF bTest AND tmpBongHode.BongNr > 0 THEN 
                RUN NyFilLogg IN h_Parent (INPUT lFilId,"      **Feil - Dublettbong(1): " + STRING(tmpBongHode.B_Id) + ").").
               
              NEXT. /* BONGHODE */
            END.
            /* Dagsoppgjørene legges inn som nye linjer på samme bong :) */
            ELSE DO:
              FIND BongHode NO-LOCK WHERE 
                BongHode.ButikkNr = tmpBongHode.ButikkNr AND
                BongHode.gruppenr = 1                    AND
                BongHode.Kassenr  = tmpBongHode.Kassenr  AND
                BongHode.Dato     = tmpBongHode.Dato     AND
                BongHode.BongNr   = tmpBongHode.BongNr NO-ERROR.
              IF AVAILABLE BongHode THEN 
                FIND LAST BongLinje NO-LOCK WHERE
                  BongLinje.B_Id = BongHode.B_id USE-INDEX b_Id NO-ERROR.
                IF AVAILABLE BongLinje
                  THEN iLinjeNr = BongLinje.LinjeNr + 1.
                  ELSE iLinjeNr = 1.
            END.
        END.
        ELSE DO:
          IF AVAILABLE BongHode THEN RELEASE BongHode.
 
          IF lUseB_id = TRUE AND CAN-FIND(FIRST BongHode WHERE 
                      BongHode.b_id = tmpBonghode.b_id) THEN
          DO: 
            IF bTest THEN 
              RUN NyFilLogg IN h_Parent (INPUT lFilId,"      **Feil - Dublettbong(2): " + STRING(tmpBongHode.b_id) + ").").
            NEXT. /* BONGHODE */
          END.
            
          ELSE DO:
            BUFFER-COPY tmpBongHode 
              EXCEPT tmpBonghode.b_id 
              TO BongHode.
            IF lUseB_id = TRUE THEN
                ASSIGN BongHode.b_id = tmpBonghode.b_id.
          END.
        END.
            
        IF NOT CAN-DO(cDatasettId,STRING(Bonghode.datasettid)) THEN
            ASSIGN cDatasettId = cDatasettId + (IF cDatasettId <> "" THEN "," ELSE "") + STRING(BongHode.DatasettId).
/*         IF ERROR-STATUS:ERROR AND AVAIL BongHode THEN DO: */
/*             DELETE BongHode.                              */
/*             NEXT.                                         */
/*         END.                                              */
        
        BONGLINJE:
        FOR EACH tmpBongLinje WHERE tmpBongLinje.b_id = tmpBongHode.b_id:
            iAntLinjer = iAntLinjer + 1.
            IF AVAILABLE BongLinje THEN RELEASE BongLinje.
            IF iLinjeNr > 0 AND tmpBonglinje.TTId = 150 THEN 
            DO:
                BUFFER-COPY tmpBongLinje EXCEPT tmpBongLinje.b_id tmpBongLinje.LinjeNr TO BongLinje
                ASSIGN BongLinje.b_id    = BongHode.b_id
                       BongLinje.LinjeNr = iLinjeNr.
            END.
            ELSE DO:
                BUFFER-COPY tmpBongLinje EXCEPT tmpBongLinje.b_id TO BongLinje
                ASSIGN BongLinje.b_id = BongHode.b_id.
            END.
            IF Bonglinje.TTId = 150 AND BongLinje.originaldata <> "" THEN DO:
                RUN Kassereroppgjor.
            END.
        END. /* BONGLINJE*/
    END. /* BONGHODE */
    
    DO ii = 1 TO NUM-ENTRIES(cDataSettId):
        FIND datasett EXCLUSIVE WHERE datasett.datasettid = DECI(ENTRY(ii,cDatasettid)) NO-ERROR.
        IF AVAIL datasett THEN
            datasett.behandlet = 3.
    END.
    IF AVAIL datasett THEN
        FIND CURRENT datasett NO-LOCK.
        
    RUN NyFilLogg IN h_Parent (INPUT lFilId,"      Ferdig (Antall linjer: " + STRING(iAntLinjer) + ").").
        
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
          Filer.Innlest       = TRUE
          Filer.InnlestDato   = TODAY
          Filer.InnlestKl     = TIME
          Filer.InnlestAv     = USERID("SkoTex")
          Filer.Oppdatert     = TRUE
          Filer.OppdatertDato = TODAY
          Filer.OppdatertKl   = TIME
          Filer.OppdatertAv   = USERID("SkoTex")
          . 
      /* Dette gjelder Preem. Bongene skal leses inn, men ikke posteres i translogg. */    
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
            PUBLISH 'bongFeilVedImport' ( STRING(TT_Error.Bongnr) + ' **' + TT_Error.cError).
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
/* ************************  Function Implementations ***************** */

&IF DEFINED(EXCLUDE-getMixMvaKr) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getMixMvaKr Procedure 
FUNCTION getMixMvaKr RETURNS DECIMAL
        (  ):
        /*------------------------------------------------------------------------------
                        Purpose: Summerer MVaKr på de bonglinjene som inngår i en MixMatch.                                                                                                                                       
                        Notes:                                                                                                                                            
        ------------------------------------------------------------------------------*/

                DEFINE VARIABLE dMvaKr AS DECIMAL NO-UNDO.

    FOR EACH tmpMixBongLinje WHERE
      tmpMixBongLinje.b_Id       = tmpBongLinje.b_Id AND 
      tmpMixBongLinje.KampId     = tmpBongLinje.KampId AND 
      tmpMixBongLinje.KampTilbId = tmpBongLinje.KampTilbId:
        
      dMvaKr = dMvaKr + tmpMixbongLinje.MvaKr.  
    END. 
                RETURN dMvaKr.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getMixVVarekost) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getMixVVarekost Procedure 
FUNCTION getMixVVarekost RETURNS DECIMAL
        (  ):
        /*------------------------------------------------------------------------------
                        Purpose:                                                                                                                                          
                        Notes:                                                                                                                                            
        ------------------------------------------------------------------------------*/

    DEFINE VARIABLE dVVareKost AS DECIMAL NO-UNDO.

    FOR EACH tmpMixBongLinje WHERE
      tmpMixBongLinje.b_Id       = tmpBongLinje.b_Id AND 
      tmpMixBongLinje.KampId     = tmpBongLinje.KampId AND 
      tmpMixBongLinje.KampTilbId = tmpBongLinje.KampTilbId:
        
      dVVareKost = dVVareKost + tmpMixbongLinje.VVareKost.  
    END. 
    RETURN dVVAreKost.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getRabatt) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getRabatt Procedure 
FUNCTION getRabatt RETURNS DECIMAL
        (  ):
        /*------------------------------------------------------------------------------
                        Purpose:                                                                                                                                          
                        Notes:                                                                                                                                            
        ------------------------------------------------------------------------------*/

                DEFINE VARIABLE result AS DECIMAL NO-UNDO.

    DEFINE VARIABLE dRabatt AS DECIMAL NO-UNDO.

    FOR EACH tmpMixBongLinje WHERE
      tmpMixBongLinje.b_Id       = tmpBongLinje.b_Id AND 
      tmpMixBongLinje.KampId     = tmpBongLinje.KampId AND 
      tmpMixBongLinje.KampTilbId = tmpBongLinje.KampTilbId:
        
      dRabatt = dRabatt + (tmpMixbongLinje.LinjeRab + tmpMixbongLinje.subtotalRab).  
    END. 
    RETURN dRabatt.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

