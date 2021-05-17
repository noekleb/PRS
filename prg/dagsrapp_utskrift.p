&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
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


DEFINE INPUT  PARAMETER pcRappType AS CHARACTER                NO-UNDO.
DEFINE INPUT  PARAMETER piButNr    AS INTEGER                  NO-UNDO.
DEFINE INPUT  PARAMETER pdFraDato  AS DATE                     NO-UNDO.
DEFINE INPUT  PARAMETER pdTilDato  AS DATE                     NO-UNDO.
DEFINE INPUT  PARAMETER lBatch     AS LOGICAL                  NO-UNDO.
DEFINE OUTPUT PARAMETER pcFilNavn  AS CHARACTER                NO-UNDO.

DEFINE VARIABLE lOresavrunding     AS DECIMAL   NO-UNDO.
DEFINE VARIABLE wOmsKommisjon      AS DECIMAL NO-UNDO.
DEFINE VARIABLE wAndelKommisjon    AS DECIMAL NO-UNDO.
DEFINE VARIABLE lKortGebyr         AS DECIMAL   FORMAT "->>,>>>,>>9.99" NO-UNDO.
DEFINE VARIABLE iAktiv             AS INTEGER   NO-UNDO. 
DEFINE VARIABLE cSprak             AS CHARACTER NO-UNDO.
DEFINE VARIABLE cBelopp            AS CHARACTER NO-UNDO.
DEFINE VARIABLE dColPosBF          AS DECIMAL   EXTENT 5 NO-UNDO.
DEFINE VARIABLE dColPosFR          AS DECIMAL   EXTENT 6 NO-UNDO.
DEFINE VARIABLE dULstartFR         AS DECIMAL   EXTENT 6 NO-UNDO.
DEFINE VARIABLE dColPosMVA         AS DECIMAL   EXTENT 4 NO-UNDO.
DEFINE VARIABLE dULstartMVA        AS DECIMAL   EXTENT 4 NO-UNDO.
DEFINE VARIABLE iLineSpace         AS INTEGER   INITIAL 11 NO-UNDO.
DEFINE VARIABLE wBruttoOmsetning   AS DECIMAL   NO-UNDO.
DEFINE VARIABLE wNettoOmsetning    AS DECIMAL   NO-UNDO.
DEFINE VARIABLE dMvaGrunnlag       AS DECIMAL   NO-UNDO.
DEFINE VARIABLE dMvaBelop          AS DECIMAL   NO-UNDO.
DEFINE VARIABLE iAntKunder         AS INTEGER   NO-UNDO.
DEFINE VARIABLE dTotaltInslaget    AS DECIMAL   NO-UNDO.
DEFINE VARIABLE lKortSum           AS DECIMAL   NO-UNDO.
DEFINE VARIABLE lDagensKontStrom   AS DECIMAL   NO-UNDO.
DEFINE VARIABLE lKasseSlutt        AS DECIMAL   NO-UNDO.
DEFINE VARIABLE lKasseEndring      AS DECIMAL   NO-UNDO.
DEFINE VARIABLE lKasseDiff         AS DECIMAL   NO-UNDO.
DEFINE VARIABLE cBokfNr            AS CHARACTER FORMAT "x(20)" NO-UNDO.
DEFINE VARIABLE iCl                AS INTEGER   NO-UNDO.
DEFINE VARIABLE cTekst             AS CHARACTER NO-UNDO.
DEFINE VARIABLE cHKInst            AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFirstButik        AS CHARACTER NO-UNDO.
DEFINE VARIABLE lDirekte           AS LOGICAL   NO-UNDO.
DEFINE VARIABLE cButBatchPrinter   AS CHARACTER NO-UNDO.
DEFINE VARIABLE iAntallUtbetBonger AS INTEGER   NO-UNDO.
DEFINE VARIABLE lVerdiUtbetBonger  AS DECIMAL   NO-UNDO.
DEFINE VARIABLE cLogo              AS CHARACTER NO-UNDO.
DEFINE VARIABLE cTittel            AS CHARACTER FORMAT "x(25)" NO-UNDO.
DEFINE VARIABLE iAntiGive          AS INTEGER   FORMAT "->>,>>9" NO-UNDO.
DEFINE VARIABLE lVerdiiGive        AS DECIMAL   FORMAT "->>>,>>>,>>9.99" NO-UNDO.
DEFINE VARIABLE cigiveArtNr        AS CHARACTER NO-UNDO.
/* DEF VAR cSubTittel1        AS CHAR FORMAT "x(70)" NO-UNDO. */
/* DEF VAR cSubTittel2        AS CHAR FORMAT "x(70)" NO-UNDO. */
/* DEF VAR cKrit1             AS CHAR                NO-UNDO. */
/* DEF VAR cKrit2             AS CHAR                NO-UNDO. */
DEFINE VARIABLE cFirma             AS CHARACTER FORMAT "x(40)" NO-UNDO.
DEFINE VARIABLE cDato              AS CHARACTER NO-UNDO.
DEFINE VARIABLE cClInfo            AS CHARACTER FORMAT "x(70)" NO-UNDO.
DEFINE VARIABLE pcOldLst           AS CHARACTER NO-UNDO.
DEFINE VARIABLE iTjHG              AS INTEGER   NO-UNDO.
DEFINE VARIABLE iLinjeNr           AS INTEGER   NO-UNDO.
DEFINE VARIABLE cLagraBlob         AS CHARACTER NO-UNDO.
DEFINE VARIABLE cButikkTxt         AS CHARACTER NO-UNDO.
DEFINE VARIABLE lBelop             AS DECIMAL   NO-UNDO.
DEFINE VARIABLE lKorrBrutto        AS DECIMAL   NO-UNDO.
DEFINE VARIABLE lKorrNetto         AS DECIMAL   NO-UNDO.
DEFINE VARIABLE lKorrMva           AS DECIMAL   NO-UNDO.
DEFINE VARIABLE lKorr              AS DECIMAL   NO-UNDO.
DEFINE VARIABLE cBrukerId          AS CHARACTER NO-UNDO.

DEFINE VARIABLE lOpptaltMorgen     AS DECIMAL   FORMAT "->>,>>>,>>9.99" NO-UNDO.
DEFINE VARIABLE lExOpptaltMorgen   AS DECIMAL   FORMAT "->>,>>>,>>9.99" NO-UNDO.
DEFINE VARIABLE lExOpptaltKveld    AS DECIMAL   FORMAT "->>,>>>,>>9.99" NO-UNDO.
DEFINE VARIABLE lExDropp           AS DECIMAL   FORMAT "->>,>>>,>>9.99" NO-UNDO.
DEFINE VARIABLE lExInnbet          AS DECIMAL   FORMAT "->>,>>>,>>9.99" NO-UNDO.
DEFINE VARIABLE lExUtBet           AS DECIMAL   FORMAT "->>,>>>,>>9.99" NO-UNDO.
DEFINE VARIABLE lLagtIPose         AS DECIMAL   FORMAT "->>,>>>,>>9.99" NO-UNDO.
DEFINE VARIABLE lKort_Spes         AS DECIMAL   FORMAT "->>,>>>,>>9.99" NO-UNDO.
DEFINE VARIABLE lOpptaltInnVeksel  AS DECIMAL   FORMAT "->>,>>>,>>9.99" NO-UNDO.
DEFINE VARIABLE lSumElementer      AS DECIMAL   FORMAT "->>,>>>,>>9.99" NO-UNDO.
DEFINE VARIABLE l52KorrBelop       AS DECIMAL   FORMAT "->>,>>>,>>9.99" NO-UNDO.
DEFINE VARIABLE l58KorrBelop       AS DECIMAL   FORMAT "->>,>>>,>>9.99" NO-UNDO.
DEFINE VARIABLE iGantAktiv         AS INTEGER   NO-UNDO.

DEFINE TEMP-TABLE TT_KontHg NO-UNDO
  FIELD Hg  AS INTEGER
  FIELD Txt AS CHARACTER
  FIELD Bel AS DECIMAL
  INDEX Hg IS PRIMARY UNIQUE Hg.

DEFINE TEMP-TABLE TT_Vg NO-UNDO
  FIELD vg     AS INTEGER
  FIELD txt    AS CHARACTER
  FIELD antal  AS INTEGER
  FIELD belopp AS DECIMAL
  FIELD mva    AS LOGICAL
  INDEX vgm IS PRIMARY UNIQUE vg mva.
DEFINE TEMP-TABLE TT_ArtHG NO-UNDO
  FIELD artobjekt AS CHARACTER
  FIELD txt       AS CHARACTER
  FIELD antal     AS INTEGER
  FIELD belopp    AS DECIMAL
  FIELD mva       AS LOGICAL
  INDEX vgm IS PRIMARY UNIQUE artobjekt mva.

DEFINE TEMP-TABLE TT_KTL NO-UNDO
  FIELD ButikkNr       AS INT 
  FIELD SalgsDato      AS DATE 
  FIELD KTypeNr        AS INT
  FIELD KupBeskrivelse AS CHAR 
  FIELD Antall         AS INT 
  FIELD BelopKnd       AS DEC 
  FIELD Belopbut       AS DEC 
  INDEX KTypeNr ButikkNr SalgsDato KTypeNr.

DEFINE TEMP-TABLE ttEksport NO-UNDO SERIALIZE-NAME 'Dagsoppjor'
  FIELD ButNr       AS INTEGER   FORMAT ">>>>>9"
  FIELD ButNamn     AS CHARACTER FORMAT "x(30)"
  FIELD Avdeling    AS CHARACTER FORMAT "x(10)"
  FIELD Dato        AS DATE      FORMAT "99/99/9999"
  FIELD LinjeNr     AS INTEGER   FORMAT ">>9"
  FIELD Tekst       AS CHARACTER FORMAT "x(50)"
  FIELD KontoPrefix AS CHARACTER FORMAT "x"
  FIELD KontoNr     AS INTEGER   FORMAT ">>>>9"
  FIELD MvaKontoNr  AS INTEGER   FORMAT ">>>>9"
  FIELD Belop       AS DECIMAL   FORMAT "->>,>>>,>>>,>>9.99"
  FIELD Mva         AS DECIMAL   FORMAT "->>,>>>,>>>,>>9.99"
  FIELD BelopUMva   AS DECIMAL   FORMAT "->>,>>>,>>>,>>9.99"
  INDEX idxEksport ButNr Dato LinjeNr.

{syspara.i 55 10 1 iAktiv INT}
{syspara.i 210 100 8 iGantAktiv INT}
{ttOverfor.i}
{ttRabattertSalg.i}
  
DEFINE TEMP-TABLE tt_Non_sale_spes NO-UNDO LIKE NON_Sale_Spes.
{tmpKort_spes.i &NEW = NEW &SHARED = SHARED}
{tmpKas_Rap.i &NEW = NEW &SHARED = SHARED}
DEFINE BUFFER btmpKas_rap FOR tmpKas_rap.
DEFINE BUFFER bufButiker  FOR Butiker.

/* {xPrint.i} */
{runlib.i}
{ pdf_inc.i "THIS-PROCEDURE"}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&IF DEFINED(EXCLUDE-bredd) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD bredd Procedure 
FUNCTION bredd RETURNS DECIMAL
  ( INPUT cText AS CHARACTER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getDBKr) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getDBKr Procedure 
FUNCTION getDBKr RETURNS DECIMAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getKonto) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getKonto Procedure 
FUNCTION getKonto RETURNS CHARACTER
  ( INPUT gruppe AS INTEGER, INPUT syspara AS INTEGER, INPUT dBelop AS DECIMAL )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getMvaGrunnlag) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getMvaGrunnlag Procedure 
FUNCTION getMvaGrunnlag RETURNS DECIMAL
  (  ) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getTjenesteOms) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getTjenesteOms Procedure 
FUNCTION getTjenesteOms RETURNS DECIMAL
  (  ) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getTransName) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getTransName Procedure 
FUNCTION getTransName RETURNS CHARACTER
  ( INPUT iTTId AS INTE )  FORWARD.

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
         HEIGHT             = 27.81
         WIDTH              = 82.4.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

FIND Butiker WHERE Butiker.Butik = piButNr NO-LOCK NO-ERROR.
IF NOT AVAIL Butiker THEN
DO:
  RETURN "FEIL".
END.

{syspara.i  20 5 1 iTjHG INT}
IF iTjHG = ? OR iTjHG = 0 THEN
  ASSIGN iTjHG = 13.

cBrukerId = ''.
PUBLISH 'bokforingsbilag_bruker' (OUTPUT cBrukerId).
IF cBrukerId <> '' THEN 
  FIND bruker WHERE bruker.brukerid = cBrukerId NO-LOCK NO-ERROR.
IF NOT AVAILABLE Bruker THEN 
  FIND bruker WHERE bruker.brukerid = USERID("skotex") NO-LOCK NO-ERROR.
IF AVAIL bruker THEN
  cSprak = TRIM(Bruker.Lng).
RUN ValiderKriterier.
IF RETURN-VALUE <> "OK" THEN
DO:
  RETURN "FEIL".
END.

IF CAN-DO(pcRappType,'99') AND pdFraDato = pdtilDato THEN 
DO:
  /* T�mmer visningstabellen. */
  FOR EACH BokforingsBilag NO-LOCK WHERE 
    BokforingsBilag.ButikkNr = pibutNr AND 
    Bokforingsbilag.Aar      = YEAR(pdFraDato) AND 
    BokforingsBilag.OmsetningsDato = pdFraDato:
    FOR EACH BokforingsVisning OF BokforingsBilag EXCLUSIVE-LOCK:
      DELETE Bokforingsvisning.
    END.  
  END.
END.

RUN ByggFinansRapport.

IF RETURN-VALUE = "AVBRYT" THEN
DO:
  RETURN "FEIL".
END.    

RUN ByggKortSpes.
RUN Bygg_NONS_sale.
RUN Bygg_KupongSpes.
RUN Bygg_tjanster.
RUN ByggAntKunder.

IF CAN-FIND(FIRST tmpKas_rap) THEN 
DO:
  RUN PDFSamling.

  IF CAN-DO(pcRappType,'99') AND CAN-FIND(FIRST ttEksport)THEN
  DO: 
    FOR EACH ttEksport:
      FIND FIRST ImpKonv NO-LOCK WHERE 
        ImpKonv.EDB-System = 'Gant Global' AND 
        ImpKonv.Tabell     = 'Regnskapsavd' AND 
        ImpKonv.InterntID = STRING(ttEksport.ButNr) NO-ERROR.
      IF AVAILABLE ImpKonv THEN
      DO: 
        ASSIGN
          ttEksport.Avdeling = ImpKonv.eksterntId 
          .
      END.
        
    END.
  /*      TEMP-TABLE ttEksport:WRITE-JSON('file', 'konv\dagsoppgjor' + '_But' + STRING(piButNr) + '_' + REPLACE(STRING(TODAY),'/','') + '_' + REPLACE(STRING(TIME,"HH:MM:SS"),':','') + '.json', TRUE).*/
  END.
  RETURN "OK".
END.
ELSE 
DO:
  RETURN "FEIL".
END.
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-ButikRubrik) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ButikRubrik Procedure 
PROCEDURE ButikRubrik :
  /*------------------------------------------------------------------------------
    Purpose:     
    Parameters:  <none>
    Notes:       
  ------------------------------------------------------------------------------*/
  DEFINE VARIABLE cString AS CHARACTER NO-UNDO.
    
  cButikkTxt = IF CAN-DO("SVE,SE",TRIM(cSprak)) THEN "Butik" ELSE "Butikk".
  RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold",12).
  IF AVAIL Butiker THEN
  DO:
    RUN pdf_text_xy_dec ("Spdf",cButikkTxt + " " + STRING(Butiker.Butik) + " " + Butiker.ButNamn + " " + cString,pdf_LeftMargin ("Spdf"),pdf_PageHeight("Spdf") - 88).
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ByggAntKunder) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ByggAntKunder Procedure 
PROCEDURE ByggAntKunder :
  /*------------------------------------------------------------------------------
    Purpose:     
    Parameters:  <none>
    Notes:       
  ------------------------------------------------------------------------------*/
  DEFINE VARIABLE pdDato AS DATE NO-UNDO.
  DO pdDato = pdFraDato TO pdTilDato:
    /* Henter postene */
    FOR EACH Akt_Rapp NO-LOCK WHERE Akt_Rapp.Dato = pdDato AND Akt_Rapp.Butik = piButNr:
      iAntKunder = iAntKunder + akt_rapp.ant_kunder.
    END.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ByggFinansRapport) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ByggFinansRapport Procedure 
PROCEDURE ByggFinansRapport :
  /*------------------------------------------------------------------------------
    Purpose:     
    Parameters:  <none>
    Notes:       
  ------------------------------------------------------------------------------*/
  DEF    VAR      pdDato    AS DATE    NO-UNDO.
  DEFINE VARIABLE lFlereDar AS LOGICAL NO-UNDO.

  DO:
    ASSIGN
      /*       T-Kuntotal */
      lFlereDar = pdFraDato <> pdTilDato
      .
  END.
  /* T�mmer temp-table */
  EMPTY TEMP-TABLE tmpKas_Rap NO-ERROR.

  /* Bygger opp tabell */
  BYGG:
  DO pdDato = pdFraDato TO pdTilDato:
    /* Henter postene */
    KASSELOOP:
    FOR EACH Kas_Rap NO-LOCK WHERE Kas_Rap.Dato = pdDato AND
      Kas_Rap.Butikk = piButNr BREAK BY Kas_Rap.Dato:
      /* Sumerer opp bonger med utbetaling */
      ASSIGN
        iAntallUtbetBonger = 0
        lVerdiUtbetBonger  = 0
        .
      FOR EACH BongHode NO-LOCK WHERE
        BongHode.ButikkNr = Kas_Rap.Butikk AND
        BongHode.GruppeNr = 1 AND
        BongHode.KasseNr  = Kas_Rap.Kasse AND
        BongHode.Dato     = pdFraDato AND          /* ????? pdDato ghg */
        BongHode.Belop    < 0:
        IF CAN-FIND(FIRST BongLinje WHERE
          BongLinje.B_Id = BongHode.B_Id AND
          BongLinje.TTId = 50) THEN
          ASSIGN
            iAntallUtbetBonger = iAntallUtbetBonger + 1
            lVerdiUtbetBonger  = lVerdiUtbetBonger  + BongHode.Belop
            .
      END.
      BUTIKKTOTAL: /* Bygger butikktotaler */
      DO:
        FIND tmpKas_Rap WHERE tmpKas_Rap.Dato     = pdFraDato AND /* Kas_Rap.Dato AND */
          tmpKas_Rap.Butikk   = Kas_Rap.Butikk AND
          tmpKas_Rap.Kasse    = -9999 AND
          tmpKas_Rap.Z_Nummer = 0 NO-ERROR.
        IF NOT AVAILABLE tmpKas_Rap THEN
        DO:
          CREATE tmpKas_Rap.
          ASSIGN 
            tmpKas_Rap.Dato     = pdFraDato /* Kas_Rap.Dato  */
            tmpKas_Rap.Butikk   = Kas_Rap.Butikk 
            tmpKas_Rap.Kasse    = -9999
            tmpKas_Rap.Z_Nummer = 0.
        END.
        IF FIRST-OF(Kas_Rap.Dato) THEN
          RUN HentKassererRapport.
        RUN SummerPost.
      END. /* BUTIKKTOTAL */
    END. /* KASSELOOP */

  END. /* BYGG */

  STATUS DEFAULT "".
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ByggKortSpes) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ByggKortSpes Procedure 
PROCEDURE ByggKortSpes :
  /*------------------------------------------------------------------------------
    Purpose:     
    Parameters:  <none>
    Notes:       
  ------------------------------------------------------------------------------*/
  DEF    VAR      pdDato    AS DATE    NO-UNDO.
  DEFINE VARIABLE lFlereDar AS LOGICAL NO-UNDO.
  /*  
    DEF INPUT PARAMETER piButNr    AS INT  NO-UNDO.
    DEF INPUT PARAMETER pdFraDato  AS DATE NO-UNDO.
    DEF INPUT PARAMETER pdTilDato  AS DATE NO-UNDO.
    */
  DO WITH FRAME Default-frame:
    ASSIGN 
      lFlereDar = pdFraDato <> pdTilDato.
  END.
  /* T�mmer temp-table */
  EMPTY TEMP-TABLE tmpKort_Spes NO-ERROR.
  /* Bygger opp tabell */
  BYGG:
  DO pdDato = pdFraDato TO pdTilDato:

    /* Henter postene */
    KASSELOOP:
    FOR EACH Kort_Spes NO-LOCK WHERE Kort_Spes.Dato   = pdDato AND
      Kort_Spes.butikk = piButNr 
      BREAK BY Kort_Spes.Dato:
      /* Legger opp kassetotaler */
      /*           IF NOT T-Kuntotal THEN                                                           */
      /*           DO:                                                                              */
      /*               CREATE tmpKort_Spes.                                                         */
      /*               BUFFER-COPY Kort_Spes TO tmpKort_Spes.                                       */
      /*               ASSIGN tmpKort_Spes.Sortering = IF lFlereDar THEN tmpKort_Spes.Kasse ELSE 0. */
      /*               RELEASE tmpKort_Spes.                                                        */
      /*           END.                                                                             */

      /* Bygger kassetotaler */
      /*           IF T-Kasse AND lFlereDar THEN */
      IF lFlereDar THEN
      KASSETOTAL:
      DO:
        FIND tmpKort_Spes WHERE
          tmpKort_Spes.Dato      = pdFraDato AND /* Kort_Spes.Dato AND */
          tmpKort_Spes.Butikk    = Kort_Spes.Butikk AND
          tmpKort_Spes.Kasse     = Kort_Spes.Kasse AND
          tmpKort_Spes.KortType  = Kort_Spes.KortType AND
          tmpKort_Spes.Z_Nummer  = 0 AND
          tmpKort_Spes.Sortering = 0 NO-ERROR.
        IF NOT AVAILABLE tmpKort_Spes THEN
        DO:
          CREATE tmpKort_Spes.
          ASSIGN
            tmpKort_Spes.Dato      = pdFraDato /* Kort_Spes.Dato  */
            tmpKort_Spes.Butikk    = Kort_Spes.Butikk 
            tmpKort_Spes.Kasse     = Kort_Spes.Kasse
            tmpKort_Spes.KortType  = Kort_Spes.KortType
            tmpKort_Spes.Z_Nummer  = 0 
            tmpKort_Spes.Sortering = 0 
            .
        END.
        IF tmpKort_Spes.KortType = 0 THEN
          tmpKort_Spes.KortType = 50.
        IF tmpKort_Spes.cKortNavn = "" THEN 
        DO:
          FIND Syspara WHERE Syspara.SysHId = 20 AND
            Syspara.SysGr  = 3  AND
            SysPara.ParaNr = tmpKort_Spes.KortType NO-LOCK NO-ERROR.
          ASSIGN 
            tmpKort_Spes.cKortNavn = IF AVAIL SysPara THEN SysPara.parameter1 ELSE (IF CAN-DO("SE,SVE",cSprak) THEN "Ok�nt" ELSE "Ukjent")
            tmpKort_Spes.cKonto    = IF AVAIL SysPara THEN SysPara.parameter2 ELSE "".
        END.
        RUN SummerKortSpes.
      END. /* KASSETOTAL */
          
      BUTIKKTOTAL:
      DO:
        FIND tmpKort_Spes WHERE
          tmpKort_Spes.Dato     = pdFraDato AND /* Kort_Spes.Dato AND */
          tmpKort_Spes.Butikk   = Kort_Spes.Butikk AND
          tmpKort_Spes.Kasse    = -9999 AND
          tmpKort_Spes.KortType = Kort_Spes.KortType AND
          tmpKort_Spes.Z_Nummer = 0 NO-ERROR.
        IF NOT AVAILABLE tmpKort_Spes THEN
        DO:
          CREATE tmpKort_Spes.
          ASSIGN
            tmpKort_Spes.Dato     = pdFraDato /* Kort_Spes.Dato  */
            tmpKort_Spes.Butikk   = Kort_Spes.Butikk 
            tmpKort_Spes.Kasse    = -9999
            tmpKort_Spes.KortType = Kort_Spes.KortType
            tmpKort_Spes.Z_Nummer = 0
            .
        END.
        IF tmpKort_Spes.KortType = 0 THEN
          tmpKort_Spes.KortType = 50.
        RUN SummerKortSpes.
      END. /* BUTIKKTOTAL */

    /* Bygger Total */
    END. /* KASSELOOP */

  END. /* BYGG */

  STATUS DEFAULT "".
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Bygg_KupongSpes) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Bygg_KupongSpes Procedure 
PROCEDURE Bygg_KupongSpes :
  /*------------------------------------------------------------------------------
    Purpose:     
    Parameters:  <none>
    Notes:  
    
    DEFINE TEMP-TABLE TT_KTL NO-UNDO
      field ButikkNr       as int 
      field SalgsDato      as date 
      FIELD KTypeNr        AS INT
      FIELD KupBeskrivelse AS CHAR 
      field Antall         as int 
      FIELD BelopKnd       AS DEC 
      FIELD Belopbut       AS DEC 
      INDEX KTypeNr KTypeNr.
  
  ------------------------------------------------------------------------------*/

  DEF    VAR      pdDato    AS DATE    NO-UNDO.
  DEFINE VARIABLE lFlereDar AS LOGICAL NO-UNDO.
  
  DO WITH FRAME Default-frame:
    ASSIGN 
      lFlereDar = pdFraDato <> pdTilDato.
  END.
  
  /* T�mmer temp-table */
  EMPTY TEMP-TABLE tt_KTL NO-ERROR.
  
  /* Bygger opp tabell */
  BYGG:
  DO pdDato = pdFraDato TO pdTilDato:
    FOR EACH KupongTransLogg NO-LOCK WHERE
      KupongTransLogg.butikkNr  = piButNr AND
      KupongTransLogg.SalgsDato = pdDato:

      FIND FIRST TT_KTL WHERE
        TT_KTL.ButikkNr  = KupongTransLogg.ButikkNr  AND 
        TT_KTL.SalgsDato = KupongTransLogg.SalgsDato AND 
        TT_KTL.KTypeNr   = KupongTransLogg.KTypeNr
        NO-ERROR.
      IF NOT AVAILABLE TT_KTL THEN
      DO:
        CREATE TT_KTL.
        ASSIGN
          TT_KTL.ButikkNr       = KupongTransLogg.ButikkNr   
          TT_KTL.SalgsDato      = KupongTransLogg.SalgsDato  
          TT_KTL.KTypeNr        = KupongTransLogg.KTypeNr
          TT_KTL.KupBeskrivelse = KupongTransLogg.KupBeskrivelse
          .
      END.
      ASSIGN
        TT_KTL.Antall   = TT_KTL.Antall   + 1
        TT_KTL.BelopKnd = TT_KTL.BelopKnd + KupongTransLogg.BelopKnd
        TT_KTL.BelopBut = TT_KTL.BelopBut + KupongTransLogg.BelopBut
        .
    END.
  END. /* BYGG */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Bygg_NONS_sale) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Bygg_NONS_sale Procedure 
PROCEDURE Bygg_NONS_sale :
  /*------------------------------------------------------------------------------
    Purpose:     
    Parameters:  <none>
    Notes:       
  ------------------------------------------------------------------------------*/
  DEFINE VARIABLE dDato AS DATE NO-UNDO.
  FOR EACH kasse WHERE kasse.butikknr = piButNr NO-LOCK:
    DO dDato = pdFraDato TO pdTilDato:
      FOR EACH NON_Sale_spes WHERE NON_Sale_spes.butikk = kasse.butikknr AND
        NON_Sale_spes.kasse  = kasse.kassenr  AND
        NON_Sale_spes.dato   = dDato NO-LOCK:
        FIND tt_NON_Sale_spes WHERE tt_NON_Sale_spes.butikk        = 0 AND
          tt_NON_Sale_spes.kasse         = 0 AND
          tt_NON_Sale_spes.dato          = pdFraDato AND
          tt_NON_Sale_spes.Non_Sale_Type = NON_Sale_spes.Non_Sale_Type AND
          tt_NON_Sale_Spes.Kode          = NON_Sale_Spes.Kode NO-ERROR.
        IF NOT AVAIL tt_NON_Sale_spes THEN 
        DO:
          CREATE tt_NON_Sale_Spes.
          ASSIGN 
            tt_NON_Sale_spes.butikk        = 0
            tt_NON_Sale_spes.kasse         = 0
            tt_NON_Sale_spes.dato          = pdFraDato
            tt_NON_Sale_spes.Non_Sale_Type = NON_Sale_spes.Non_Sale_Type
            tt_NON_Sale_Spes.Kode          = NON_Sale_Spes.Kode.
        END.
        ASSIGN 
          tt_NON_Sale_Spes.NON_SaleAntall = tt_NON_Sale_Spes.NON_SaleAntall + NON_Sale_Spes.NON_SaleAntall
          tt_NON_Sale_Spes.NON_SaleVerdi  = tt_NON_Sale_Spes.NON_SaleVerdi  + NON_Sale_Spes.NON_SaleVerdi.
      END.
    END.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Bygg_tjanster) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Bygg_tjanster Procedure 
PROCEDURE Bygg_tjanster :
  /*------------------------------------------------------------------------------
    Purpose:     
    Parameters:  <none>
    Notes:       
  ------------------------------------------------------------------------------*/
  DEFINE VARIABLE dDato        AS DATE    NO-UNDO.
  DEFINE VARIABLE iAarPerLinNr AS INTEGER NO-UNDO.
  DEFINE VARIABLE dFgAr31Dec   AS DATE    NO-UNDO.
  /* DEFINE TEMP-TABLE TT_Vg NO-UNDO         */
  /*     FIELD vg     AS INTE                */
  /*     FIELD txt    AS CHAR                */
  /*     FIELD antal  AS INTE                */
  /*     FIELD belopp AS DECI                */
  /*     FIELD mva    AS LOG                 */
  /*     INDEX vgm IS PRIMARY UNIQUE vg mva. */
    
  FOR EACH vargr WHERE vargr.hg = iTjHG NO-LOCK.
    FIND moms OF vargr NO-LOCK NO-ERROR.
    CREATE tt_vg.
    ASSIGN 
      tt_vg.vg  = vargr.vg
      tt_vg.txt = vargr.vgbeskr
      tt_vg.mva = AVAIL moms AND Moms.MomsProc > 0.
  END.
  FOR EACH tt_vg:
    DO dDato = pdFraDato TO pdTilDato:
      dFgAr31Dec = DATE(12,31,YEAR(dDato) - 1).
      iAarperlinnr = INT(STRING(YEAR(dDato),"9999") + STRING(dDato - dFgAr31Dec,"999")). 
      FOR EACH Stlinje WHERE Stlinje.butik       = piButNr      AND Stlinje.Sttypeid = "VAREGR" AND Stlinje.PerId = "DAG" AND
        StLinje.Aarperlinnr = iAarperlinnr AND StLinje.Dataobjekt = STRING(tt_vg.vg,"999999") NO-LOCK:
        ASSIGN 
          tt_vg.antal  = tt_vg.antal  + StLinje.AntSolgt
          TT_vg.belopp = TT_vg.belopp + StLinje.VerdiSolgt. /* + StLinje.MvaVerdi. */
      END.
    END.
  END.
  FOR EACH tt_vg WHERE tt_vg.antal = 0 AND tt_vg.belopp = 0:
    DELETE tt_vg.
  END.
  FOR EACH artbas WHERE artbas.hg = iTjHG NO-LOCK.
    FIND vargr WHERE vargr.hg = artbas.hg NO-LOCK NO-ERROR.
    IF NOT AVAIL vargr THEN
      NEXT.
    FIND moms OF vargr NO-LOCK NO-ERROR.
    CREATE tt_ArtHG.
    ASSIGN 
      tt_ArtHG.artobjekt = STRING(artbas.artikkelnr,"9999999999999")
      tt_ArtHG.txt       = Artbas.beskr
      tt_ArtHG.mva       = AVAIL moms AND Moms.MomsProc > 0.
  END.
  FOR EACH tt_ArtHG:
    DO dDato = pdFraDato TO pdTilDato:
      dFgAr31Dec = DATE(12,31,YEAR(dDato) - 1).
      iAarperlinnr = INT(STRING(YEAR(dDato),"9999") + STRING(dDato - dFgAr31Dec,"999")). 
      FOR EACH Stlinje WHERE Stlinje.butik       = piButNr      AND Stlinje.Sttypeid = "ARTIKKEL" AND Stlinje.PerId = "DAG" AND
        StLinje.Aarperlinnr = iAarperlinnr AND StLinje.Dataobjekt = STRING(tt_ArtHG.artobjekt,"999999") NO-LOCK:
        ASSIGN 
          tt_ArtHG.antal  = tt_ArtHG.antal  + StLinje.AntSolgt
          tt_ArtHG.belopp = tt_ArtHG.belopp + StLinje.VerdiSolgt. /* + StLinje.MvaVerdi. */
      END.
    END.
  END.
  FOR EACH tt_ArtHG WHERE tt_ArtHG.antal = 0 AND tt_ArtHG.belopp = 0:
    DELETE tt_ArtHG.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF 

&IF DEFINED(EXCLUDE-HentKassererRapport) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE HentKassererRapport Procedure 
PROCEDURE HentKassererRapport :
  /*------------------------------------------------------------------------------
    Purpose:     Adderer inn kassereroppgj�ret i butikktotalen n�r den f�rste 
                 gang opprettes.
    Parameters:  <none>
    Notes:       
  ------------------------------------------------------------------------------*/
  /* Adderer opp opptalte verdier. */
  OPPGJOR:
  FOR EACH KassererOppgj NO-LOCK WHERE
    KassererOppgj.Dato     = Kas_Rap.Dato AND
    KassererOppgj.Butikk   = Kas_Rap.Butikk:
    ASSIGN
      tmpKas_Rap.OpptaltVeksel           = tmpKas_Rap.OpptaltVeksel           + KassererOppgj.OpptaltVeksel   
      tmpKas_Rap.OpptaltKontanter        = tmpKas_Rap.OpptaltKontanter        + KassererOppgj.OpptaltKontanter
      tmpKas_Rap.OpptaltSjekk            = tmpKas_Rap.OpptaltSjekk            + KassererOppgj.OpptaltSjekk    
      tmpKas_Rap.OpptaltReserve          = tmpKas_Rap.OpptaltReserve          + KassererOppgj.OpptaltReserve  
      tmpKas_Rap.OpptaltGavekort         = tmpKas_Rap.OpptaltGavekort         + KassererOppgj.OpptaltGavekort 
      tmpKas_Rap.OpptaltTilgode          = tmpKas_Rap.OpptaltTilgode          + KassererOppgj.OpptaltTilgode  
      tmpKas_Rap.OpptaltGaveKortAndre    = tmpKas_Rap.OpptaltGaveKortAndre    + KassererOppgj.OpptaltGaveKortAndre  
      tmpKas_Rap.OpptaltGavekortUtlevert = tmpKas_Rap.OpptaltGavekortUtlevert + KassererOppgj.OpptaltGavekortUtlevert  
      tmpKas_Rap.OpptaltTilgodeAndre     = tmpKas_Rap.OpptaltTilgodeAndre     + KassererOppgj.OpptaltTilgodeAndre  
      tmpKas_Rap.OpptaltTilgodeUtlevert  = tmpKas_Rap.OpptaltTilgodeUtlevert  + KassererOppgj.OpptaltTilgodeUtlevert  
      tmpKas_Rap.OpptaltInnVeksel        = tmpKas_Rap.OpptaltInnVeksel        + KassererOppgj.OpptaltInnVeksel  
      tmpKas_Rap.OpptaltValuta           = tmpKas_Rap.OpptaltValuta           + KassererOppgj.OpptaltValuta  
      tmpKas_Rap.OpptaltLevertBank       = tmpKas_Rap.OpptaltLevertBank       + KassererOppgj.OpptaltLevertBank  
      tmpKas_Rap.OpptaltBilag            = tmpKas_Rap.OpptaltBilag            + KassererOppgj.OpptaltBilag  
      tmpKas_Rap.OpptaltFinansiering     = tmpKas_Rap.OpptaltFinansiering     + KassererOppgj.OpptaltFinansiering
      tmpKas_Rap.OpptaltUtbetalt         = tmpKas_Rap.OpptaltUtbetalt         + KassererOppgj.OpptaltUtbetalt
      tmpKas_Rap.OpptaltKupong           = tmpKas_Rap.OpptaltKupong           + KassererOppgj.OpptaltKupong
      .
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-LagraBlob) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LagraBlob Procedure 
PROCEDURE LagraBlob :
  /*------------------------------------------------------------------------------
    Purpose:     
    Parameters:  <none>
    Notes:       
  ------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER cFilnamn AS CHARACTER   NO-UNDO.
  CREATE DistributeFile. 
  ASSIGN 
    DistributeFile.id       = GUID(GENERATE-UUID)
    DistributeFile.DATE     = NOW
    DistributeFile.Butikk   = piButNr
    DistributeFile.FILENAME = cFilnamn. 
  COPY-LOB FILE cFilnamn TO DistributeFile.FileObject NO-CONVERT NO-ERROR. 
  IF ERROR-STATUS:ERROR THEN
    DELETE DistributeFile.
  ELSE 
  DO:
    CREATE DistributeFileReceiver. 
    ASSIGN 
      DistributeFileReceiver.id               = GUID(GENERATE-UUID)
      DistributeFileReceiver.DistributeFileid = DistributeFile.id 
      DistributeFileReceiver.KasseNr          = 1.
  END.
  RELEASE DistributeFile.
  RELEASE DistributeFileReceiver.
  OS-DELETE VALUE(cFilnamn).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-opprettBokforingsvisning) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE opprettBokforingsvisning Procedure
PROCEDURE opprettBokforingsvisning:
  /*------------------------------------------------------------------------------
   Purpose:
   Notes:
                  ttEksport.ButNr     = tmpKas_Rap.Butikk
                  ttEksport.ButNamn   = cButikkTxt
                  ttEksport.Dato      = pdFraDato
                  ttEksport.LinjeNr   = iLinjeNr
                  ttEksport.Tekst     = pcTekst
                  ttEksport.KontoPrefix = ENTRY(1,cKonto,' ')
                  ttEksport.KontoNr     = INT(ENTRY(2,ENTRY(1,cKonto,'/'),' '))
                  ttEksport.MvaKontoNr  = INT(ENTRY(2,cKonto,'/'))
                  ttEksport.Belop     = tmpKas_rap.MvaGrunnlag[piLoop] + tmpKas_rap.MvaBelop[piLoop]
                  ttEksport.Mva       = tmpKas_rap.MvaBelop[piLoop]
                  ttEksport.BelopUMva = tmpKas_rap.MvaGrunnlag[piLoop]
     
  ------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER piType AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER pcTekst AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER piTTId AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER piTBId AS INTEGER NO-UNDO.
  
  DEFINE VARIABLE pcChar  AS CHARACTER NO-UNDO.
  DEFINE VARIABLE pcKonto AS CHARACTER NO-UNDO.
  
  IF AVAILABLE Bokforingsbilag THEN
  DO TRANSACTION:
    IF CAN-DO('3,4,5,6',STRING(piType)) THEN 
      iLinjeNr = iLinjeNr + 1.

    FIND Bokforingsvisning EXCLUSIVE-LOCK WHERE 
      BokforingsVisning.BokforingsID = Bokforingsbilag.BokforingsId AND 
      BokforingsVisning.LinjeNr      = iLinjeNr NO-ERROR.
    IF NOT AVAILABLE Bokforingsvisning THEN 
    OPPSTANDELSEN:
    DO:
        
      CREATE Bokforingsvisning.
      ASSIGN 
        BokforingsVisning.BokforingsID = Bokforingsbilag.BokforingsId  
        BokforingsVisning.LinjeNr      = iLinjeNr 
        .

      IF pitype = 1 THEN 
      DO:
          
        ASSIGN 
          BokforingsVisning.Konto = FILL(' ',8 - LENGTH(STRING(ttEksport.KontoNr))) + STRING(ttEksport.KontoNr)
          BokforingsVisning.Tekst = IF pcTekst <> '' THEN pcTekst ELSE ttEksport.Tekst
          BokforingsVisning.Belop = FILL(' ',12 - LENGTH(STRING(ttEksport.Belop))) + STRING(ttEksport.Belop) 
          .  
        IF piTTId > 0 AND piTBId > 0 AND NUM-ENTRIES(BokforingsVisning.Teks,'/') = 1 THEN   
          BokforingsVisning.Tekst =  BokforingsVisning.Tekst + '(' + STRING(piTTId) + '/' + STRING(piTBId) + ')'.
      END.       
      ELSE IF pitype = 2 THEN 
        DO:
          IF (piTTId = 800 AND piTBId = 2) THEN
            ASSIGN 
              BokforingsVisning.Tekst = pcTekst 
              .  
          ELSE 
            ASSIGN 
              BokforingsVisning.Tekst = '.' + CAPS(pcTekst) 
              .  
        END.       
      ELSE IF pitype = 3 THEN 
        DO:
          .
          ASSIGN 
            BokforingsVisning.Konto = FILL(' ',8 - LENGTH(STRING(ttEksport.MvaKontoNr))) + STRING(ttEksport.MvaKontoNr)
            BokforingsVisning.Tekst = 'Mva'
            BokforingsVisning.Belop = FILL(' ',12 - LENGTH(STRING(ttEksport.Mva))) + STRING(ttEksport.Mva) 
            .  
        END.       
      ELSE IF pitype = 4 THEN 
        DO:
          ASSIGN 
            BokforingsVisning.Konto = FILL(' ',8 - LENGTH(STRING(ttEksport.KontoNr))) + STRING(ttEksport.KontoNr)
            BokforingsVisning.Tekst = 'BelopUMva'
            BokforingsVisning.Belop = FILL(' ',12 - LENGTH(STRING(ttEksport.BelopUMva))) + STRING(ttEksport.BelopUMva) 
            .  
        END.       
      ELSE IF pitype = 5 THEN 
        DO:
    
          ASSIGN 
            BokforingsVisning.Konto       = FILL(' ',8 - LENGTH(STRING(ttEksport.KontoNr))) + STRING(ttEksport.KontoNr)
            BokforingsVisning.Tekst       = ttEksport.Tekst + ' (' + STRING(piTTId) + '/' + STRING(piTBId) + ')'
            BokforingsVisning.Belop       = FILL(' ',12 - LENGTH(STRING(ttEksport.Belop))) + STRING(ttEksport.Belop) 
            BokforingsVisning.KorrTillatt = TRUE /* �pner for � kunne korrigere omsetning. Ref. Str�mmen 28/5-20 med sub.tot.rab. p� bong med varesalg og retur. */
            .  
        END.       

      ELSE IF pitype = 6 THEN 
        DO:
  
          ASSIGN 
            BokforingsVisning.Konto = FILL(' ',8 - LENGTH(STRING(ttEksport.KontoNr))) + STRING(ttEksport.KontoNr)
            BokforingsVisning.Belop = FILL(' ',12 - LENGTH(STRING(ttEksport.Belop))) + STRING(ttEksport.Belop) 
            .
          IF piTTId = 61 THEN 
          DO:  
            RUN settTekstOgKonto(BokforingsBilag.ButikkNr,61,1,0,'',OUTPUT pcChar, OUTPUT pcKonto). 
            ASSIGN   
              BokforingsVisning.Tekst = 'Innbetalingsbilag(' + STRING(piTTId) + '/' + STRING(piTBId) + ')'
              BokforingsVisning.Konto = pcKonto
              .
          END.
          ELSE IF piTTId = 62 THEN 
            DO: 
              RUN settTekstOgKonto(BokforingsBilag.ButikkNr,62,1,0,'',OUTPUT pcChar, OUTPUT pcKonto).
              ASSIGN 
                BokforingsVisning.Tekst = 'Utbetalingsbilag(' + STRING(piTTId) + '/' + STRING(piTBId) + ')'
                BokforingsVisning.Konto = pcKonto
                .
            END.
        END.       

      ELSE IF pitype = 7 THEN 
        DO:
          RUN settTekstOgKonto(BokforingsBilag.ButikkNr,piTTId,piTBId,0,pcTekst,OUTPUT pcChar, OUTPUT pcKonto). 
          ASSIGN   
            BokforingsVisning.Tekst = IF ttEksport.Tekst <> '' THEN ttEksport.Tekst ELSE pcChar
            BokforingsVisning.Konto = pcKonto
            BokforingsVisning.Belop = FILL(' ',12 - LENGTH(STRING(ttEksport.Belop))) + STRING(ttEksport.Belop) 
            .
        END.       
        
      IF CAN-DO('52,58,59,61,62,800,900',STRING(piTTId)) AND piTBId > 0 THEN
      DO:
        IF piTTId = 800 AND piTBId <> 2 THEN. /* Gj�r ingenting */
        ELSE IF piTTId = 900 THEN 
          DO:
            IF CAN-DO('28,29',STRING(piTBId)) THEN
              BokforingsVisning.KorrTillatt = TRUE. 
          END.
          ELSE    
            BokforingsVisning.KorrTillatt = TRUE.
      END.
      /*        IF CAN-DO('61,62,800',STRING(piTTId)) AND piTBId > 0 THEN*/
      ASSIGN  
        BokforingsVisning.TTId = piTTId
        Bokforingsvisning.TBId = piTBId
        .
          
    END. /* OPPSTANDELSEN */
  END. /* TRANSACTION */


END PROCEDURE.
  
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


&IF DEFINED(EXCLUDE-PageFooter) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PageFooter Procedure 
PROCEDURE PageFooter :
  /*------------------------------------------------------------------------------
    Purpose:  Procedure to Print Page Footer -- on all pages.
    Parameters:  <none>
    Notes:       
  ------------------------------------------------------------------------------*/
  DEFINE VARIABLE cSidTxt AS CHARACTER NO-UNDO.
  RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica",8).
  RUN pdf_set_dash IN h_PDFinc ("Spdf",1,0).
  RUN pdf_line IN h_PDFinc  ("Spdf", pdf_LeftMargin ("Spdf"), pdf_BottomMargin ("Spdf"), pdf_PageWidth("Spdf") - pdf_LeftMargin ("Spdf") , pdf_BottomMargin ("Spdf"), 0.5).

  RUN pdf_text_xy_dec ("Spdf",TRIM(STRING(TODAY)) + " " + STRING(TIME,"HH:MM:SS"),pdf_LeftMargin ("Spdf"),pdf_BottomMargin ("Spdf") - 14).
  cSidTxt = TRIM("Sida: " + STRING(pdf_page("Spdf")) + " (" + pdf_TotalPages("Spdf") + ")").
  RUN pdf_text_xy_dec ("Spdf",cSidTxt,pdf_Pagewidth ("Spdf") - pdf_LeftMargin ("Spdf") - 50,pdf_BottomMargin ("Spdf") - 14).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-PageHeader) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PageHeader Procedure 
PROCEDURE PageHeader :
  /*------------------------------------------------------------------------------
    Purpose:     
    Parameters:  <none>
    Notes:       
  ------------------------------------------------------------------------------*/
  
  RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold",20).
  RUN pdf_text_xy_dec ("Spdf",cTittel,pdf_LeftMargin ("Spdf"),pdf_PageHeight("Spdf") - 45).
  RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold",10).
  RUN pdf_text_xy_dec ("Spdf",cFirma,pdf_LeftMargin ("Spdf"),pdf_PageHeight("Spdf") - 61).
  
  RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica",8).
  RUN pdf_text_xy_dec ("Spdf",cDato,pdf_LeftMargin ("Spdf"),pdf_PageHeight("Spdf") - 72).
  
  RUN pdf_line IN h_PDFinc  ("Spdf", pdf_LeftMargin ("Spdf"), pdf_PageHeight ("Spdf") - 74, pdf_PageWidth("Spdf") - pdf_LeftMargin ("Spdf") , pdf_PageHeight ("Spdf") - 74, 0.5).
/*   RUN xx. */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-PDFSamling) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PDFSamling Procedure 
PROCEDURE PDFSamling :
  /*------------------------------------------------------------------------------
    Purpose:     
    Parameters:  <none>
    Notes:       
  ------------------------------------------------------------------------------*/
  DEF    VAR      piLoop    AS INT       NO-UNDO.
  DEF    VAR      pcOverskr AS CHAR      NO-UNDO.
  DEFINE VARIABLE dY        AS DECIMAL   NO-UNDO.
  DEFINE VARIABLE wOK       AS LOGICAL   NO-UNDO.
  DEFINE VARIABLE cPrinter  AS CHARACTER NO-UNDO.
  DEFINE VARIABLE dYspara   AS DECIMAL   NO-UNDO.
  DEFINE VARIABLE cEmail    AS CHARACTER NO-UNDO.
  DEFINE VARIABLE lKorrOms  AS DECIMAL   NO-UNDO.

  FIND FIRST Bokforingsbilag WHERE
    Bokforingsbilag.ButikkNr = piButNr AND
    Bokforingsbilag.OmsetningsDato = pdFraDato NO-ERROR.
  IF NOT AVAILABLE Bokforingsbilag THEN 
  DO:
    RETURN.
  END.          

  /* Summerer opp det som er lagt ekstra i pose. */
  POSESUM:
  DO:
    lLagtIPose = 0.
    FOR EACH KassererOppgj NO-LOCK WHERE
      KassererOppgj.Dato     = pdFraDato AND
      KassererOppgj.ButikkNr = piButNr.
      IF KassererOppgj.OpptaltLevertBank > 0 THEN
        lLagtIPose = lLagtIPose + KassererOppgj.OpptaltLevertBank.
    END.
    /* Legger opp eventyelle poser som er registrert ved korreksjon */
    FOR EACH BokforingsKorr NO-LOCK WHERE 
      BokforingsKorr.BokforingsID = BokforingsBilag.BokforingsID AND 
      BokforingsKorr.TTId = 800 AND 
      BokforingsKorr.TbId = 2:
      ASSIGN 
        lLagtIPose = lLagtIPose + BokforingsKorrBilag.Belop
        .
    END.
  END. /* POSESUM */

  /* Summerer ekstra dropp. */
  lExDropp = 0.
  FOR EACH BokforingsKorrBilag NO-LOCK WHERE
    BokforingsKorrBilag.BokforingsID = BokforingsBilag.BokforingsID AND
    BokforingsKorrBilag.TTId = 59 AND
    BokforingsKorrBilag.TbId = 1:
    ASSIGN
      lExDropp = lExDropp + BokforingsKorrBilag.Belop
      .
  END.

  /*Opptalt morgen - Henter fra trans 108 p� bonglinje.       */
  /* TN 29/5-20. Bare f�rste registrering for dagen skal med. */
  lOpptaltMorgen = 0.
  FOR EACH Kasse NO-LOCK WHERE 
    Kasse.ButikkNr = BokforingsBilag.ButikkNr AND 
    Kasse.GruppeNr = 1 AND 
    Kasse.KasseNr <= 90,
    FIRST bongHode NO-LOCK WHERE 
    BongHode.ButikkNr = Kasse.butikkNr AND 
    BongHode.GruppeNr = Kasse.GruppeNr AND          
    BongHode.KasseNr = Kasse.KasseNr AND
    BongHode.Dato = BokforingsBilag.OmsetningsDato AND
    CAN-FIND(FIRST BongLinje WHERE 
    BongLinje.B_Id = BongHode.B_Id AND 
    BongLinje.TTId = 108),
    FIRST BongLinje NO-LOCK WHERE 
    BongLinje.ButikkNr = Kasse.ButikkNr AND 
    BongLinje.GruppeNr = Kasse.GruppeNr AND 
    BongLinje.KasseNr = Kasse.KasseNr AND
    BongLinje.Dato = BongHode.Dato AND 
    BongLinje.TTId = 108:
    ASSIGN
      lOpptaltMorgen = lOpptaltMorgen + BongLinje.LinjeSum
      .
  END.

  /* Extra opptalt morgen */
  lExOpptaltMorgen = 0.
  FOR EACH BokforingsKorrBilag NO-LOCK WHERE
    BokforingsKorrBilag.BokforingsID = BokforingsBilag.BokforingsID AND
    BokforingsKorrBilag.TTId = 900 AND
    BokforingsKorrBilag.TbId = 28:
    ASSIGN
      lExOpptaltMorgen = lExOpptaltMorgen + BokforingsKorr.Belop
      .
  END.

  /* Extra opptalt kveld */
  lExOpptaltKveld = 0.
  FOR EACH BokforingsKorrBilag NO-LOCK WHERE
    BokforingsKorrBilag.BokforingsID = BokforingsBilag.BokforingsID AND
    BokforingsKorrBilag.TTId = 900 AND
    BokforingsKorrBilag.TbId = 29:
    ASSIGN
      lExOpptaltKveld = lExOpptaltKveld + BokforingsKorr.Belop
      .
  END.

  /* Legger opp eventyelle innbetalingsbilag som er registrert ved korreksjon */
  lExInnbet = 0.
  FOR EACH BokforingsKorrBilag NO-LOCK WHERE
    BokforingsKorrBilag.BokforingsID = BokforingsBilag.BokforingsID AND
    BokforingsKorrBilag.TTId = 61 AND
    BokforingsKorrBilag.TbId = 1:
    ASSIGN
      lExInnbet = lExInnbet + BokforingsKorr.Belop
      .
  END.

  /* Legger opp eventyelle utbetalingsbilag som er registrert ved korreksjon */
  lExUtBet = 0.
  FOR EACH BokforingsKorrBilag NO-LOCK WHERE
    BokforingsKorrBilag.BokforingsID = BokforingsBilag.BokforingsID AND
    BokforingsKorrBilag.TTId = 62 AND
    BokforingsKorrBilag.TbId = 1:
    ASSIGN
      lExUtBet = lExUtBet + BokforingsKorr.Belop 
      .
  END.
  
  /* NB: TN 3/5-20 Dette p�virker dagens kontantstr�m i motsetning.  Er ogs� lagt til omsetning under Saml_2_mva subrutinen. */
  lKorrOms = 0.
  FOR EACH BokforingsKorrBilag NO-LOCK WHERE
    BokforingsKorrBilag.BokforingsID = BokforingsBilag.BokforingsID AND
    BokforingsKorrBilag.TTId = 1: /* Summerer her for alle korttyper. */
    ASSIGN
      lKorrOms = lKorrOms + BokforingsKorr.Belop
      .
  END.
  
  ASSIGN 
    iAntigive   = 0
    lVerdiiGive = 0
    ciGiveArtNr = '9002'
    .
  FOR EACH Kasse NO-LOCK WHERE 
    Kasse.ButikkNr = BokforingsBilag.ButikkNr AND 
    Kasse.GruppeNr = 1 AND 
    Kasse.KasseNr <= 90,
    EACH bongHode NO-LOCK WHERE 
    BongHode.ButikkNr = Kasse.butikkNr AND 
    BongHode.GruppeNr = Kasse.GruppeNr AND          
    BongHode.KasseNr = Kasse.KasseNr AND
    BongHode.Dato = BokforingsBilag.OmsetningsDato AND
    CAN-FIND(FIRST BongLinje WHERE 
    BongLinje.B_Id = BongHode.B_Id AND 
    BongLinje.TTId = 1),
    EACH BongLinje NO-LOCK WHERE 
    BongLinje.ButikkNr = Kasse.ButikkNr AND 
    BongLinje.GruppeNr = Kasse.GruppeNr AND 
    BongLinje.KasseNr = Kasse.KasseNr AND
    BongLinje.Dato = BongHode.Dato AND 
    BongLinje.TTId = 1 AND 
    BongLinje.ArtikkelNr = cigiveArtNr:
    ASSIGN
      iAntigive   = iAntiGive   + 1
      lVerdiiGive = lVerdiiGive + BongLinje.LinjeSum
      .
  END.

  ASSIGN
    lKort_Spes        = 0
    lDagensKontStrom  = 0
    lKasseEndring     = 0
    lKasseSlutt       = 0
    wBruttoOmsetning  = 0
    lOpptaltInnVeksel = 0
    lSumElementer     = 0
    .

  FOR EACH tmpKort_Spes NO-LOCK WHERE
    tmpKort_Spes.Butikk   = piButNr AND
    tmpKort_Spes.Kasse    = -9999:
    ASSIGN 
      lKort_Spes = lKort_Spes + tmpKort_Spes.Belop
      .
  END.
  KAS_RAP_SUMMER:
  FOR EACH tmpKas_Rap WHERE  
    tmpKas_Rap.butikk = piButNr AND 
    tmpKas_Rap.Kasse = -9999:

    ASSIGN                                                          
      lKasseSlutt       = lKasseSlutt  
                    /* + tmpKas_Rap.OpptaltVeksel /* Ved dagens slutt */*/ 
                    + tmpKas_Rap.OpptaltKontanter /* Opptalte kontanter er allt som ligger i kassen inklusive inng�ende veksel. */                 
                    /*+ tmpKas_Rap.OpptaltSjekk TN 21/4-20 Sjekk er tatt i bruk for Vips */                       
                    + tmpKas_Rap.OpptaltReserve                     
                    + tmpKas_Rap.OpptaltValuta
      lOpptaltInnVeksel = lOpptaltInnVeksel + tmpKas_Rap.OpptaltInnVeksel
      .                      
    DO piLoop = 1 TO 10:
      IF tmpKas_rap.MvaGrunnlag[piLoop] <> 0 THEN
      DO:
        wBruttoOmsetning = wBruttoOmsetning + 
          tmpKas_rap.MvaGrunnlag[piLoop] + 
          tmpKas_rap.MvaBelop[piLoop].
      END.
    END.
    ASSIGN 
      lKasseSlutt = lKasseslutt + lExOpptaltKveld
      . 
    
    
    /* TN 21/4-20 Dette l� tidligere nede i kas_rap loopen. Det overstyrte brutto omsetning beregnet over. Hvorfor?   */
    /*    DO piLoop = 1 TO 10:                                                                                        */
    /*        ASSIGN wNettoOmsetning = wNettoOmsetning + tmpKas_rap.MvaGrunnlag[piLoop] + tmpKas_rap.MvaBelop[piLoop].*/
    /*    END.                                                                                                        */
    /*    wBruttoOmsetning = wNettoOmsetning +                                                                        */
    /*                       tmpKas_Rap.Retur  +                                                                      */
    /*                       tmpKas_Rap.Reklamasjon  +                                                                */
    /*                       tmpKas_Rap.GenerellRabatt +                                                              */
    /*                       tmpKas_Rap.Kunderabatt +                                                                 */
    /*                       tmpKas_Rap.Personalrabatt +                                                              */
    /*                       tmpKas_Rap.Medlemsrabatt +                                                               */
    /*                       tmpKas_Rap.Pakkerabatt.                                                                  */

    ASSIGN 
      lSumElementer = lSumElementer +
                      ( 
                        - tmpKas_Rap.TilgodeInn
                        - tmpKas_rap.TilgodeAndre
                        - tmpKas_Rap.Kupong1
                        - tmpKas_Rap.Kupong2
                        + tmpKas_Rap.TilgodeUt
                        + tmpKas_Rap.GavekortUt
                        + tmpKas_Rap.Non_SalePos
                        - tmpKas_Rap.GavekortInn
                        - tmpKas_Rap.Kredit
                        + tmpKas_Rap.InnbetaltKunde
                        - (tmpKas_Rap.kont_ut - tmpKas_Rap.kont_in)
                        - (tmpKas_Rap.Bank + tmpKas_Rap.CashBack)
                        - tmpKas_Rap.Reservelosning
                        - tmpKas_Rap.Non_SaleNeg
                        + tmpKas_Rap.LayAway_Ut
                        - tmpKas_Rap.LayAway_Inn
                        - tmpKas_Rap.Dropp
                      ).
  END. /* KAS_RAP_SUMMER */


  ASSIGN 
    /*    lKasseEndring = lKasseSlutt   - (lOpptaltInnVeksel + lExOpptaltMorgen)*/
    lKasseEndring = lKasseSlutt   - (lOpptaltMorgen + lExOpptaltMorgen)  
    .

  /*MESSAGE 'TEST Kasseendring'           */
  /*'lKasseEndring:' lKasseEndring        */
  /*'lKasseSlutt:' lKasseSlutt            */
  /*'lOpptaltInnVeksel:' lOpptaltInnVeksel*/
  /*'lExOpptaltMorgen:' lExOpptaltMorgen  */
  /*'lExOpptaltKveld:' lExOpptaltKveld    */
  /*VIEW-AS ALERT-BOX.                    */

  ASSIGN 
    lDagensKontStrom = wBruttoOmsetning 
                       + lSumElementer 
                       - lExDropp
                       + lExInnBet
                       - lExUtbet
                       - lKort_Spes
                       + lKorrOms
    .
  /* NB: Er det gjort korreksjoner p� betalingsmiddlene, blir ikke disse lagt p� f�r kassediffen skrives ut p� rapporten. */
  /* �rsaken er at l52KorrBelop og l58KorrBelop f�rst blir initiert litt senere.                                          */
  ASSIGN 
    lKasseDiff = lKasseEndring - lDagensKontStrom
    .
 
  {syspara.i  1 1 100 cFirma}
  cDato = (IF CAN-DO("SE,SVE",cSprak) THEN "Datum: " ELSE "Dato: ") + STRING(pdFraDato).
  IF pdTilDato > pdFraDato THEN
    cDato = cDato + " - " + STRING(pdTilDato).
 
  ASSIGN 
    pcFilNavn = SESSION:TEMP-DIR + "Samlingsrapport" + "_" + STRING(DAY(TODAY)) +  STRING(TIME) + ".pdf".
  /* �pner stream til skriverfil. */

  DO:
    ASSIGN 
      cButBatchPrinter = TRIM(Butiker.RAPPrinter).
    {syspara.i  210 271 piButnr cEmail}
    {syspar2.i  210 270 piButnr cLagraBlob}

    RUN pdf_new ("Spdf",pcFilNavn).
    /*   pdf_PageHeader ("Spdf",THIS-PROCEDURE:HANDLE,"PageFooter"). */
    pdf_PageFooter ("Spdf",THIS-PROCEDURE:HANDLE,"PageFooter").
    RUN pdf_set_PaperType ("Spdf","A4").
    RUN pdf_set_LeftMargin ("Spdf", 40).
    RUN pdf_set_BottomMargin ("Spdf", 60).
    RUN pdf_set_VerticalSpace IN h_PDFinc ("Spdf",13).
    RUN pdf_set_Orientation ("Spdf","portrait").

    RUN SetPositioner.
  
    ASSIGN 
      wBruttoOmsetning = 0.
    /* Leser temp-table postene */
    TEMP-TABLEN:
    /*       FOR EACH tmpKas_Rap WHERE tmpKas_Rap.Sortering = 0: */
    FOR EACH tmpKas_Rap WHERE  
      tmpKas_Rap.butikk = piButNr AND 
      tmpKas_Rap.Kasse = -9999
      BREAK BY tmpKas_Rap.Dato 
      BY tmpKas_Rap.butikk 
      BY tmpKas_Rap.Kasse 
      BY tmpKas_Rap.Z_Nummer:
              
      /* TN 30/9-19 Preparerer underlag for eksport til navision. */
      IF CAN-DO(pcRappType,"99") THEN 
      DO:   
        cButikkTxt = IF CAN-DO("SVE,SE",TRIM(cSprak)) THEN "Butik" ELSE "Butikk".
        IF AVAIL Butiker THEN
          cButikkTxt = cButikkTxt + " " + STRING(Butiker.Butik) + " " + Butiker.ButNamn.
  
        RUN pdf_new_page ("Spdf").
        RUN ButikRubrik.
        cTittel     = IF CAN-DO("SE,SVE",cSprak) THEN "Bokf�ringsrapport" ELSE "Bokf�ringsbilag".
        RUN PageHeader.
        dY = pdf_PageHeight ("Spdf") - 110.
        RUN Saml_2_mva(INPUT-OUTPUT dY).
        dYSpara = dY.
        RUN Saml_2_betalat(INPUT-OUTPUT dY).
        RUN Saml_2_utbetalt(INPUT-OUTPUT dY).
        RUN Saml_2_kredit(INPUT-OUTPUT dY).
        RUN Saml_2_in_ut(INPUT-OUTPUT dY).
        RUN Saml_2_Dagensstrom(INPUT-OUTPUT dY).
        RUN Saml_2_PoseNr.
        dY = dYSpara.
      END.

      /*     IF piRappType = 1 OR piRappType = 5 THEN DO: */
      IF CAN-DO(pcRappType,"1") THEN 
      DO:
        /* Sida 1 */
        /* column 1 */
        RUN pdf_new_page ("Spdf").
        RUN ButikRubrik.
        cTittel     = IF CAN-DO("SE,SVE",cSprak)THEN "Samlingsrapport" ELSE "Samlerapport".
        RUN PageHeader.
        dY = pdf_PageHeight ("Spdf") - 110.
        RUN Saml_1_oms(INPUT-OUTPUT dY).
        RUN Saml_1_bank(INPUT-OUTPUT dY).
        RUN Saml_1_Kontant(INPUT-OUTPUT dY).
        RUN Saml_1_mva(INPUT-OUTPUT dY).
        RUN Saml_1_justering(INPUT-OUTPUT dY).
        RUN Saml_1_ovrigt(INPUT-OUTPUT dY).
        RUN Saml_1_nyckeltal(INPUT-OUTPUT dY).
        /*     /* column 2 */                            */
        dY = pdf_PageHeight ("Spdf") - 110.
        RUN Saml_1_kassa_inslaget(INPUT-OUTPUT dY).
        RUN Saml_1_in_ut(INPUT-OUTPUT dY).
        RUN Saml_1_Kassor_talt(INPUT-OUTPUT dY).
      END.

      /*     IF piRappType = 2 OR piRappType = 5 THEN DO: */
      IF CAN-DO(pcRappType,"2") THEN 
      DO:
        /* Sida 2 */
        RUN pdf_new_page ("Spdf").
        RUN ButikRubrik.
        cTittel     = IF CAN-DO("SE,SVE",cSprak) THEN "Bokf�ringsrapport" ELSE "Bokf�ringsbilag".
        RUN PageHeader.
        dY = pdf_PageHeight ("Spdf") - 110.
        RUN Saml_2_mva(INPUT-OUTPUT dY).
        dYSpara = dY.
        RUN Saml_2_betalat(INPUT-OUTPUT dY).
        RUN Saml_2_utbetalt(INPUT-OUTPUT dY).
        RUN Saml_2_kredit(INPUT-OUTPUT dY).
        RUN Saml_2_in_ut(INPUT-OUTPUT dY).
        RUN Saml_2_Dagensstrom(INPUT-OUTPUT dY).
        RUN Saml_2_PoseNr.
        dY = dYSpara.
      /*         RUN Saml_2_BilagSpes(INPUT-OUTPUT dY). */
      /*         RUN Saml_2_Diverse(INPUT-OUTPUT dY). */
      END.
      /*     IF piRappType = 3 OR piRappType = 5 THEN DO: */
      IF CAN-DO(pcRappType,"3") THEN 
      DO:
        /* Sida 3 */
        RUN pdf_new_page ("Spdf").
        RUN ButikRubrik.
        cTittel     = IF CAN-DO("SE,SVE",cSprak) THEN "Detaljerad specifikation" ELSE "Detaljert spesifikasjon".
        RUN PageHeader.
        dY = pdf_PageHeight ("Spdf") - 110.
        RUN Saml_3_Detaljspec(INPUT-OUTPUT dY).
      END.
      /*     IF piRappType = 4 OR piRappType = 5 THEN DO: */
      IF CAN-DO(pcRappType,"4") THEN 
      DO:
        /* Sida 4 */
        RUN pdf_new_page ("Spdf").
        RUN ButikRubrik.
        cTittel     = IF CAN-DO("SE,SVE",cSprak) THEN "Konteringsrapport per huvudgrupp" ELSE "Konteringsrapport per hovedgruppe".
        RUN PageHeader.
        dY = pdf_PageHeight ("Spdf") - 110.
        RUN Saml_4_Kont_hg(INPUT-OUTPUT dY).
      END.
    END. /* TEMP-TABLEN */
    IF CAN-DO(pcRappType,"5") THEN 
    DO:
      /* Sida 4 */
      RUN pdf_new_page ("Spdf").
      RUN ButikRubrik.
      cTittel     = IF CAN-DO("SE,SVE",cSprak) THEN "Tj�nster per artikel" ELSE "Tjenester per artikkel".
      RUN PageHeader.
      dY = pdf_PageHeight ("Spdf") - 110.
      RUN Saml_5_DetalArtHG13(INPUT-OUTPUT dY).
    END.
    IF CAN-DO(pcRappType,"6") THEN 
    DO:
      /* Sida 6 */
      RUN pdf_new_page ("Spdf").
      RUN ButikRubrik.
      cTittel     = IF CAN-DO("SE,SVE",cSprak) THEN "Detaljerad specifikation tilbakebetaling" ELSE "Detaljert spesifikasjon tilbakebetaling".
      RUN PageHeader.
      dY = pdf_PageHeight ("Spdf") - 110.
      RUN Saml_6_Detaljspec(INPUT-OUTPUT dY).
    END.
    IF CAN-DO(pcRappType,"7") THEN 
    DO:
      IF CAN-FIND(FIRST BokforingsKorrBilag WHERE 
        BokforingsKorrBilag.BokforingsID = BokforingsBilag.BokforingsID) THEN
      DO: 
        /* Sida 7 */
        RUN pdf_new_page ("Spdf").
        RUN ButikRubrik.
        cTittel     = IF CAN-DO("SE,SVE",cSprak) THEN "Spesifikasjon korreksjonsbilag" ELSE "Spesifikasjon korreksjonsbilag".
        RUN PageHeader.
        dY = pdf_PageHeight ("Spdf") - 110.
        RUN Saml_7_SpesKorrBilag(INPUT-OUTPUT dY).
      END.
    END.
    IF CAN-DO(pcRappType,"8") THEN 
    DO:
      /* Teller opp og logger rabatter fra butikken p� den aktuelle dagen. */
      RUN dagsrapp_rabattertsalg.p (FALSE, /* Append - False t�mmer tabellen f�rst. */
        BokforingsBilag.ButikkNr,
        BokforingsBilag.OmsetningsDato,
        INPUT-OUTPUT TABLE ttRabattertSalg).
      IF CAN-FIND(FIRST ttRabattertSalg) THEN 
      DO:    
        /* Sida 8 */
        RUN pdf_new_page ("Spdf").
        RUN ButikRubrik.
        cTittel     = IF CAN-DO("SE,SVE",cSprak) THEN "Spesifikasjon rabatter" ELSE "Spesifikasjon rabatter".
        RUN PageHeader.
        dY = pdf_PageHeight ("Spdf") - 110.
        RUN Saml_8_SpesRabatter(INPUT-OUTPUT dY).
      END.
    END.
    IF CAN-DO(pcRappType,"9") THEN 
    DO:
      /* Teller opp og logger overf�ringer til/fra butikken p� den aktuelle dagen. */
      RUN dagsrapp_overfor.p (FALSE, /* Append - False t�mmer tabellen f�rst. */
        BokforingsBilag.ButikkNr,
        BokforingsBilag.OmsetningsDato,
        INPUT-OUTPUT TABLE ttOverfor).
      IF CAN-FIND(FIRST ttOverfor) THEN 
      DO:
        /* Sida 9 */
        RUN pdf_new_page ("Spdf").
        RUN ButikRubrik.
        cTittel     = IF CAN-DO("SE,SVE",cSprak) THEN "Spesifikasjon utg�ende overf�ringer" ELSE "Spesifikasjon utg�ende overf�ringer".
        RUN PageHeader.
        dY = pdf_PageHeight ("Spdf") - 110.
        RUN Saml_9_SpesOverforinger(INPUT-OUTPUT dY).
      END.
    END.
    IF CAN-DO(pcRappType,"10") THEN 
    DO:
      /* Teller opp og logger overf�ringer til/fra butikken p� den aktuelle dagen. */
      RUN dagsrapp_overfor.p (FALSE, /* Append - False t�mmer tabellen f�rst. */
        BokforingsBilag.ButikkNr,
        BokforingsBilag.OmsetningsDato,
        INPUT-OUTPUT TABLE ttOverfor).
      IF CAN-FIND(FIRST ttOverfor) THEN 
      DO:  
        /* Sida 10 */
        RUN pdf_new_page ("Spdf").
        RUN ButikRubrik.
        cTittel     = IF CAN-DO("SE,SVE",cSprak) THEN "Spesifikasjon innkommende overf�ringer" ELSE "Spesifikasjon innkommende overf�ringer".
        RUN PageHeader.
        dY = pdf_PageHeight ("Spdf") - 110.
        RUN Saml_10_SpesOverforinger(INPUT-OUTPUT dY).
      END.
    END.
  
    IF NOT CAN-DO(pcRappType,'99') THEN 
    DO:
      RUN pdf_close ("Spdf").
      IF cEmail <> "" THEN 
      DO:
        RUN sendEmail IN THIS-PROCEDURE (cEmail,pcFilnavn).
      END.
      ELSE IF lBatch = TRUE AND cLagraBlob = "1" THEN 
        DO:
          RUN LagraBlob (pcFilnavn).
        END.
        ELSE IF cButBatchPrinter <> "" THEN 
          DO:
            IF SEARCH("cmd\PrintPdf.cmd") <> ? THEN 
            DO:
              OS-COMMAND SILENT VALUE(SEARCH("cmd\PrintPdf.cmd") + " " + pcFilnavn + " " + '"' + cButBatchPrinter + '"').
            END.
          END.
    END.
    ELSE 
    DO:
      RUN pdf_close ("Spdf") NO-ERROR.
      ASSIGN 
        cButBatchPrinter = TRIM(Butiker.RAPPrinter).
      IF lBatch = FALSE AND cButBatchPrinter <> "" THEN 
      DO:

        IF SEARCH(pcFilNavn) <> ? THEN
          OS-COMMAND SILENT VALUE('cmd\foxitreader.exe  /t ' + QUOTER(pcFilNavn) + ' ' + QUOTER(cButBatchPrinter)).
      END.
    END.
    /*   RUN browse2pdf\viewxmldialog.w (pcFilNavn,"Rapport"). */

    STATUS DEFAULT " ".
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Saml_10_SpesOverforinger) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Saml_10_SpesOverforinger Procedure
PROCEDURE Saml_10_SpesOverforinger:
  /*------------------------------------------------------------------------------
   Purpose:
   Notes:
  ------------------------------------------------------------------------------*/
  DEFINE INPUT-OUTPUT PARAMETER dY AS DECIMAL     NO-UNDO.
  DEFINE VARIABLE cOverskr     AS CHARACTER EXTENT 6 NO-UNDO.
  DEFINE VARIABLE cLabel       AS CHARACTER EXTENT 2 NO-UNDO.
  DEFINE VARIABLE cTxt         AS CHARACTER NO-UNDO.
  DEFINE VARIABLE dSum_pos     AS DECIMAL   NO-UNDO.
  DEFINE VARIABLE dSum_neg     AS DECIMAL   NO-UNDO.
  DEFINE VARIABLE dColPos_S3   AS DECIMAL   EXTENT 5 NO-UNDO.
  DEFINE VARIABLE dULstart_S3  AS DECIMAL   EXTENT 5 NO-UNDO.
  DEFINE VARIABLE cTjensterTxt AS CHARACTER NO-UNDO.
  DEFINE VARIABLE dSum         AS DECIMAL   NO-UNDO.
  DEFINE VARIABLE cSumTxt      AS CHARACTER EXTENT 2 NO-UNDO.
  DEFINE VARIABLE cOrdreRef    AS CHARACTER NO-UNDO.

  IF CAN-DO("SE,SVE",cSprak) THEN 
  DO:
    ASSIGN 
      cOverskr[1]  = "Strekkode"
      cOverskr[3]  = "Lev.art.nr/Lev.fargekode/St�rrelse"
      cOverskr[4]  = "Antall"
      cOverskr[5]  = "Bel�p"
      cOverskr[6]  = "Dato"
      cTjensterTxt = "Innkommende overf�ringer"
      cSumTxt[1]   = "Totalt"
      .
  END.
  ELSE 
  DO:
    ASSIGN 
      cOverskr[1]  = "Strekkode"
      cOverskr[3]  = "Lev.art.nr/Lev.fargekode/St�rrelse"
      cOverskr[4]  = "Antall"
      cOverskr[5]  = "Bel�p"
      cOverskr[6]  = "Dato"
      cTjensterTxt = "Innkommende overf�ringer"
      cSumTxt[1]   = "Totalt"
      .
  END.
  ASSIGN 
    dColPos_S3[1] = pdf_LeftMargin ("Spdf") + 45
    /*           dColPos_S3[2] = dColPos_S3[1] + 10*/
    dColPos_S3[2] = pdf_LeftMargin ("Spdf") + 50
    dColPos_S3[3] = 340
    dColPos_S3[4] = 420
    dColPos_S3[5] = 553
    .
  ASSIGN 
    dULstart_S3[1] = pdf_LeftMargin ("Spdf")
    dULstart_S3[2] = pdf_LeftMargin ("Spdf") + 50
    dULstart_S3[3] = 290
    dULstart_S3[4] = 350
    dULstart_S3[5] = 430
    .

  IF NOT CAN-FIND(FIRST ttOverfor) THEN
    RETURN.
        
  /* RUBRIK */
  RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold",12).
  RUN pdf_text_xy_dec ("Spdf",cTjensterTxt,dULStart_S3[1],dY).
  dY = dY - 4.
  RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica",8).
  RUN pdf_line IN h_PDFinc  ("Spdf", pdf_LeftMargin ("Spdf"), dY, dULStart_S3[1], dY, 0.5).
  dY = dY - iLineSpace.
  /* Kolonnrubrik */
  RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold",8).
  RUN pdf_text_xy_dec ("Spdf",cOverskr[1],dULStart_S3[1],dY).
  RUN pdf_text_xy_dec ("Spdf",cOverskr[3],dULStart_S3[2],dY).
  RUN pdf_text_xy_dec ("Spdf",cOverskr[4],dColPos_S3[3] - bredd(cOverskr[4]),dY).
  RUN pdf_text_xy_dec ("Spdf",cOverskr[5],dColPos_S3[4] - bredd(cOverskr[5]),dY).
  RUN pdf_text_xy_dec ("Spdf",cOverskr[6],dColPos_S3[5] - bredd(cOverskr[6]),dY).
  dY = dY - 4.
  RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica",8).
  RUN pdf_line IN h_PDFinc  ("Spdf", dULstart_S3[1], dY, dColPos_S3[1], dY, 0.5).
  RUN pdf_line IN h_PDFinc  ("Spdf", dULstart_S3[2], dY, dULStart_S3[3] - 10, dY, 0.5).
  RUN pdf_line IN h_PDFinc  ("Spdf", dULstart_S3[3], dY, dColPos_S3[3], dY, 0.5).
  RUN pdf_line IN h_PDFinc  ("Spdf", dULstart_S3[4], dY, dColPos_S3[4], dY, 0.5).
  RUN pdf_line IN h_PDFinc  ("Spdf", dULstart_S3[5], dY, dColPos_S3[5], dY, 0.5).
           
  DO:
    FOR EACH ttOverfor NO-LOCK WHERE 
      /*          ttOverfor.ButNr = BokforingsBilag.ButikkNr AND*/
      ttOverfor.OvType = 1
      BREAK BY ttOverfor.OvType
      BY ttOverfor.ButNr
      BY ttOverfor.ArtikkelNr
      BY ttOverfor.Storl:
              
      ASSIGN 
        dSum = dSum + (ttOverfor.Verdi)
        dY   = dY - iLineSpace
        .
      cTekst = ttOverfor.Kode.
      RUN pdf_text_xy_dec ("Spdf",cTekst,dColPos_S3[1] - bredd(cTekst),dY).
      cTekst = ttOverfor.LevKod + '/' + ttOverfor.LevFargKod + '/' + ttOverfor.Storl.  
      RUN pdf_text_xy_dec ("Spdf",cTekst,dColPos_S3[2],dY).
      cTekst = STRING(ttOverfor.Antall,"->>>>>9").
      RUN pdf_text_xy_dec ("Spdf",cTekst,dColPos_S3[3] - bredd(cTekst),dY).
      cBelopp = TRIM(STRING(ttOverfor.Verdi,"->>,>>>,>>9.99")).
      RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPos_S3[4] - bredd(cBelopp),dY).
      cTekst = STRING(ttOverfor.Dato,"99/99/99").
      RUN pdf_text_xy_dec ("Spdf",cTekst,dColPos_S3[5] - bredd(cTekst),dY).
      IF DY <= 86 THEN 
      DO:
        RUN pdf_new_page ("Spdf").
        RUN ButikRubrik.
        RUN PageHeader.
        dY = pdf_PageHeight ("Spdf") - 110.
      END.
    END.
    IF dSum <> 0 THEN 
    DO:
      dY = dY - 4.
      RUN pdf_line IN h_PDFinc  ("Spdf", dULstart_S3[4], dY, dColPos_S3[4], dY, 0.5).
      dY = dY - iLineSpace.
      RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold",8).
      RUN pdf_text_xy_dec ("Spdf",cSumTxt[1],dULStart_S3[1],dY).
      cBelopp = TRIM(STRING(dSum,"->>>,>>>,>>9.99")).
      RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPos_S3[4] - bredd(cBelopp),dY).
      dSum = 0.
      dY = dY - iLineSpace.
    END.
  END.
  RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica",8).
  EMPTY TEMP-TABLE ttOverfor.

END PROCEDURE.
  
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


&IF DEFINED(EXCLUDE-Saml_1_bank) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Saml_1_bank Procedure 
PROCEDURE Saml_1_bank PRIVATE :
  /*------------------------------------------------------------------------------
    Purpose:     
    Parameters:  <none>
    Notes:       
  ------------------------------------------------------------------------------*/
  DEFINE INPUT-OUTPUT PARAMETER dY AS DECIMAL     NO-UNDO.
  DEFINE VARIABLE cOverskr AS CHARACTER EXTENT 3 NO-UNDO.
  DEFINE VARIABLE cLabel   AS CHARACTER EXTENT 3 NO-UNDO.
  DEFINE VARIABLE cTxt     AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cChar    AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cKonto   AS CHARACTER NO-UNDO.
  DEFINE VARIABLE iIndex   AS INTEGER   NO-UNDO.
  DEFINE VARIABLE dTest    AS DECIMAL   NO-UNDO.
  IF CAN-DO("SE,SVE",cSprak) THEN 
  DO:
    ASSIGN 
      cOverskr[1] = "Bank"
      cOverskr[2] = "Antal"
      cOverskr[3] = "Belopp".
    ASSIGN 
      cLabel[1] = "F�rs�ljning bankkort"      
      cLabel[2] = "Cash Back"                
      cLabel[3] = "Totalt bank".
  END.
  ELSE 
  DO:
    ASSIGN 
      cOverskr[1] = "Bank"
      cOverskr[2] = "Antall"
      cOverskr[3] = "Bel�p".
    ASSIGN 
      cLabel[1] = "Salg bankkort"
      cLabel[2] = "Cash Back"
      cLabel[3] = "Totalt bank".
  END.

  /* 
        "<C16><RIGHT=C+6>" + STRING(tmpKas_Rap.AntBank,"->>,>>9")    +
        "<C24><RIGHT=C+11>" + STRING(tmpKas_Rap.Bank,"->>>,>>>,>>9.99") SKIP        
*/

  dY = dY - 20.
  /* RUBRIK */
  RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold",12).
  RUN pdf_text_xy_dec ("Spdf",cOverskr[1],dColPosFR[1],dY).
  dY = dY - 4.
  RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica",8).
  RUN pdf_line IN h_PDFinc  ("Spdf", pdf_LeftMargin ("Spdf"), dY, dColPosFR[3], dY, 0.5).
  dY = dY - iLineSpace.
  /* Kolonnrubrtik */
  RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold",8).
  RUN pdf_text_xy_dec ("Spdf",cOverskr[2],dColPosFR[2] - bredd(cOverskr[2]),dY).
  RUN pdf_text_xy_dec ("Spdf",cOverskr[3],dColPosFR[3] - bredd(cOverskr[3]),dY).
  dY = dY - 4.
  RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica",8).
  RUN pdf_line IN h_PDFinc  ("Spdf", dULstartFR[2], dY, dColPosFR[2], dY, 0.5).
  RUN pdf_line IN h_PDFinc  ("Spdf", dULstartFR[3], dY, dColPosFR[3], dY, 0.5).
  /* 1 Bank */
  /*    dY = dY - iLineSpace.
      RUN pdf_text_xy_dec ("Spdf",cLabel[1],dColPosFR[1],dY).*/

  FOR EACH tmpKort_Spes WHERE 
    tmpKort_Spes.Dato      = tmpKas_Rap.Dato AND 
    tmpKort_Spes.Butikk    = tmpKas_Rap.Butikk AND  
    tmpKort_Spes.Kasse     = -9999 AND 
    tmpKort_Spes.KortType >= 0 AND 
    tmpKort_Spes.Z_Nummer  = 0:
    RUN settTekstOgKonto(tmpKas_Rap.Butikk,52,tmpKort_Spes.KortType,tmpKort_Spes.Belop,'D',OUTPUT cChar, OUTPUT cKonto).      

    dY = dY - iLineSpace.
    RUN pdf_text_xy_dec ("Spdf",cCHAR,dColPosFR[1],dY).
    /*      RUN pdf_text_xy_dec ("Spdf",cLabel[1],dColPosFR[1],dY).*/
   

    cTxt = TRIM(STRING(tmpKort_Spes.AntKort,"->>,>>9")).
    /*    cTxt = TRIM(STRING(tmpKas_Rap.AntBank + tmpKas_Rap.AntKort,"->>,>>9")).*/
    RUN pdf_text_xy_dec ("Spdf",cTxt,dColPosFR[2] - bredd(cTxt),dY).
    cBelopp = TRIM(STRING(tmpKort_Spes.Belop,"->>>,>>>,>>9.99")).
    /*    cBelopp = TRIM(STRING(tmpKas_Rap.Bank + tmpKas_Rap.Kort,"->>>,>>>,>>9.99")).*/
    RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPosFR[3] - bredd(cBelopp),dY).
  END.
  IF tmpKas_Rap.Bank <> 0 THEN
  DO:
    dY = dY - iLineSpace.
    RUN pdf_text_xy_dec ("Spdf",cLabel[1],dColPosFR[1],dY).
    cTxt = TRIM(STRING(tmpKas_Rap.AntBank,"->>,>>9")).
    RUN pdf_text_xy_dec ("Spdf",cTxt,dColPosFR[2] - bredd(cTxt),dY).
    cBelopp = TRIM(STRING((tmpKas_Rap.Bank),"->>>,>>>,>>9.99")).
    RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPosFR[3] - bredd(cBelopp),dY).
  END.

  ASSIGN 
    dTest = 0.
  /*    DO iIndex = 1 TO 9:
        dY = dY - iLineSpace.
        ASSIGN cChar = "Kort " + STRING(iIndex).
        RUN pdf_text_xy_dec ("Spdf",cCHAR,dColPosFR[1],dY).
        cTxt = TRIM(STRING(dTest,"->>,>>9")).
        RUN pdf_text_xy_dec ("Spdf",cTxt,dColPosFR[2] - bredd(cTxt),dY).
        cBelopp = TRIM(STRING(dTest,"->>>,>>>,>>9.99")).
        RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPosFR[3] - bredd(cBelopp),dY).
      END.*/
  /* 2 Cash back */
  dY = dY - iLineSpace.
  RUN pdf_text_xy_dec ("Spdf","+",dColPosFR[1] - 5,dY).
  RUN pdf_text_xy_dec ("Spdf",cLabel[2],dColPosFR[1],dY).
  cTxt = TRIM(STRING(tmpKas_Rap.AntCashback,"->>,>>9")).
  RUN pdf_text_xy_dec ("Spdf",cTxt,dColPosFR[2] - bredd(cTxt),dY).
  cBelopp = TRIM(STRING((tmpKas_Rap.Cashback),"->>>,>>>,>>9.99")).
  RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPosFR[3] - bredd(cBelopp),dY).
  /* line */
  dY = dY - 4.
  RUN pdf_line IN h_PDFinc  ("Spdf", dColPosFR[1], dY, dColPosFR[3], dY, 0.5).
  /* 3 Totalt bank */
  dY = dY - iLineSpace.
  RUN pdf_text_xy_dec ("Spdf","=",dColPosFR[1] - 5,dY).
  RUN pdf_text_xy_dec ("Spdf",cLabel[3],dColPosFR[1],dY).
  cBelopp = TRIM(STRING(tmpKas_Rap.Bank + tmpKas_Rap.Cashback + tmpKas_Rap.Kort,"->>>,>>>,>>9.99")).
  RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPosFR[3] - bredd(cBelopp),dY).
  /* line */
  dY = dY - 4.
  RUN pdf_line IN h_PDFinc  ("Spdf", dColPosFR[1], dY, dColPosFR[3], dY, 0.5).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Saml_1_in_ut) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Saml_1_in_ut Procedure 
PROCEDURE Saml_1_in_ut :
  /*------------------------------------------------------------------------------
    Purpose:     
    Parameters:  <none>
    Notes:       
  ------------------------------------------------------------------------------*/
  DEFINE INPUT-OUTPUT PARAMETER dY AS DECIMAL     NO-UNDO.
  DEFINE VARIABLE cOverskr AS CHARACTER EXTENT 3 NO-UNDO.
  DEFINE VARIABLE cLabel   AS CHARACTER EXTENT 11 NO-UNDO.

  IF CAN-DO("SE,SVE",cSprak) THEN 
  DO:
    ASSIGN 
      cOverskr[1] = "In- och utbetalningar"
      cOverskr[2] = "Antal"
      cOverskr[3] = "Belopp".
    ASSIGN 
      cLabel[1]  = "Utbetalt"      
      cLabel[2]  = "Inbetalt"                
      cLabel[3]  = "Nonsale + (Inbetalt)"        
      cLabel[4]  = "Nonsale - (Utbetalt)"             
      cLabel[5]  = "Tillgodo in"         
      cLabel[6]  = "Tillgodo ut"          
      cLabel[7]  = "Dep in"       
      cLabel[8]  = "Dep ut"       
      cLabel[9]  = "Presentkort in"             
      cLabel[10] = "Presentkort ut"
      cLabel[11] = "Presentkortsrabatt".
  END.
  ELSE 
  DO:
    ASSIGN 
      cOverskr[1] = "Inn- og utbetalinger"
      cOverskr[2] = "Antall"
      cOverskr[3] = "Bel�p".
    ASSIGN 
      cLabel[1]  = "Utbetalt"      
      cLabel[2]  = "Innbetalt"                
      cLabel[3]  = "Nonsale + (Innbetalt)"        
      cLabel[4]  = "Nonsale - (Utbetalt)"             
      cLabel[5]  = "Tilode inn"         
      cLabel[6]  = "Tilgode ut"          
      cLabel[7]  = "Dep in"       
      cLabel[8]  = "Dep ut"       
      cLabel[9]  = "Gavekort in"             
      cLabel[10] = "Gavekort ut"
      cLabel[11] = "Gavekortsrabatt".
  END.
  dY = dY - 20.
  RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold",12).
  RUN pdf_text_xy_dec ("Spdf",cOverskr[1],dColPosFR[4],dY).
  dY = dY - 4.
  RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica",8).
  RUN pdf_line IN h_PDFinc  ("Spdf", dColPosFR[4], dY, dColPosFR[6], dY, 0.5).
  dY = dY - iLineSpace.
  RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold",8).
  RUN pdf_text_xy_dec ("Spdf",cOverskr[2],dColPosFR[5] - bredd(cOverskr[2]),dY).
  RUN pdf_text_xy_dec ("Spdf",cOverskr[3],dColPosFR[6] - bredd(cOverskr[3]),dY).
  dY = dY - 4.
  RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica",8).
  RUN pdf_line IN h_PDFinc  ("Spdf", dULstartFR[5], dY, dColPosFR[5], dY, 0.5).
  RUN pdf_line IN h_PDFinc  ("Spdf", dULstartFR[6], dY, dColPosFR[6], dY, 0.5).
  dY = dY - iLineSpace. /* 1 Utbet */
  RUN pdf_text_xy_dec ("Spdf",cLabel[1],dColPosFR[4],dY).
  cBelopp = TRIM(STRING(tmpKas_Rap.AntKont_ut,"->>,>>9")).
  RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPosFR[5] - bredd(cBelopp),dY).
  cBelopp = TRIM(STRING(tmpKas_Rap.Kont_Ut,"->>>,>>>,>>9.99")).
  RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPosFR[6] - bredd(cBelopp),dY).
  dY = dY - iLineSpace. /* 2 inbetalt */
  RUN pdf_text_xy_dec ("Spdf",cLabel[2],dColPosFR[4],dY).
  cBelopp = TRIM(STRING(tmpKas_Rap.AntKont_inn,"->>,>>9")).
  RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPosFR[5] - bredd(cBelopp),dY).
  cBelopp = TRIM(STRING(tmpKas_Rap.Kont_Inn,"->>>,>>>,>>9.99")).
  RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPosFR[6] - bredd(cBelopp),dY).
    
  dY = dY - iLineSpace. /* 3 Nonsale pos */
  RUN pdf_text_xy_dec ("Spdf",cLabel[3],dColPosFR[4],dY).
  cBelopp = TRIM(STRING(tmpKas_Rap.Non_SalePosAnt,"->>,>>9")).
  RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPosFR[5] - bredd(cBelopp),dY).
  cBelopp = TRIM(STRING(tmpKas_Rap.Non_SalePos,"->>>,>>>,>>9.99")).
  RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPosFR[6] - bredd(cBelopp),dY).
    
  dY = dY - iLineSpace. /* 4 Nonsale negativ */
  RUN pdf_text_xy_dec ("Spdf",cLabel[4],dColPosFR[4],dY).
  cBelopp = TRIM(STRING(tmpKas_Rap.Non_SaleNegAnt,"->>,>>9")).
  RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPosFR[5] - bredd(cBelopp),dY).
  cBelopp = TRIM(STRING(tmpKas_Rap.Non_SaleNeg,"->>>,>>>,>>9.99")).
  RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPosFR[6] - bredd(cBelopp),dY).
    
  dY = dY - iLineSpace. /* 5 tilg in */
  RUN pdf_text_xy_dec ("Spdf",cLabel[5],dColPosFR[4],dY).
  cBelopp = TRIM(STRING(tmpKas_Rap.AntTilgodeInn + tmpKas_Rap.AntTilgodeAndre,"->>,>>9")).
  RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPosFR[5] - bredd(cBelopp),dY).
  cBelopp = TRIM(STRING(tmpKas_Rap.TilgodeInn + tmpKas_Rap.TilgodeAndre,"->>>,>>>,>>9.99")).
  RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPosFR[6] - bredd(cBelopp),dY).
    
  dY = dY - iLineSpace. /* 6 Tilg ut */
  RUN pdf_text_xy_dec ("Spdf",cLabel[6],dColPosFR[4],dY).
  cBelopp = TRIM(STRING(tmpKas_Rap.AntTilgodeUt,"->>,>>9")).
  RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPosFR[5] - bredd(cBelopp),dY).
  cBelopp = TRIM(STRING(tmpKas_Rap.TilgodeUt,"->>>,>>>,>>9.99")).
  RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPosFR[6] - bredd(cBelopp),dY).
    
  dY = dY - iLineSpace. /* 7 dep in */
  RUN pdf_text_xy_dec ("Spdf",cLabel[7],dColPosFR[4],dY).
  cBelopp = TRIM(STRING(tmpKas_Rap.AntLayAway_Inn,"->>,>>9")).
  RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPosFR[5] - bredd(cBelopp),dY).
  cBelopp = TRIM(STRING(tmpKas_Rap.LayAway_Inn,"->>>,>>>,>>9.99")).
  RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPosFR[6] - bredd(cBelopp),dY).
  dY = dY - iLineSpace. /* 8 Dep ut */
  RUN pdf_text_xy_dec ("Spdf",cLabel[8],dColPosFR[4],dY).
  cBelopp = TRIM(STRING(tmpKas_Rap.AntLayAway_Ut,"->>,>>9")).
  RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPosFR[5] - bredd(cBelopp),dY).
  cBelopp = TRIM(STRING(tmpKas_Rap.LayAway_Ut,"->>>,>>>,>>9.99")).
  RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPosFR[6] - bredd(cBelopp),dY).
  dY = dY - iLineSpace. /* 9 Gavekort in */
  RUN pdf_text_xy_dec ("Spdf",cLabel[9],dColPosFR[4],dY).
  cBelopp = TRIM(STRING(tmpKas_Rap.AntGavekortInn,"->>,>>9")).
  RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPosFR[5] - bredd(cBelopp),dY).
  cBelopp = TRIM(STRING(tmpKas_Rap.GavekortInn,"->>>,>>>,>>9.99")).
  RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPosFR[6] - bredd(cBelopp),dY).
  dY = dY - iLineSpace. /* 10 Gavekort ut */
  RUN pdf_text_xy_dec ("Spdf",cLabel[10],dColPosFR[4],dY).
  cBelopp = TRIM(STRING(tmpKas_Rap.AntGavekortUt,"->>,>>9")).
  RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPosFR[5] - bredd(cBelopp),dY).
  cBelopp = TRIM(STRING(tmpKas_Rap.GavekortUt,"->>>,>>>,>>9.99")).
  RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPosFR[6] - bredd(cBelopp),dY).
  dY = dY - iLineSpace. /* 11 Gavekortrabatt */
  RUN pdf_text_xy_dec ("Spdf",cLabel[11],dColPosFR[4],dY).
  cBelopp = TRIM(STRING(tmpKas_Rap.AntGavekortRabUt,"->>,>>9")).
  RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPosFR[5] - bredd(cBelopp),dY).
  cBelopp = TRIM(STRING(tmpKas_Rap.GavekortRabatt,"->>>,>>>,>>9.99")).
  RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPosFR[6] - bredd(cBelopp),dY).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Saml_1_justering) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Saml_1_justering Procedure 
PROCEDURE Saml_1_justering :
  /*------------------------------------------------------------------------------
    Purpose:     
    Parameters:  <none>
    Notes:       
  ------------------------------------------------------------------------------*/
  DEFINE INPUT-OUTPUT PARAMETER dY AS DECIMAL     NO-UNDO.
  DEFINE VARIABLE cOverskr AS CHARACTER EXTENT 3 NO-UNDO.
  DEFINE VARIABLE cLabel   AS CHARACTER EXTENT 7 NO-UNDO.
  DEFINE VARIABLE cTxt     AS CHARACTER NO-UNDO.
  IF CAN-DO("SE,SVE",cSprak) THEN 
  DO:
    ASSIGN 
      cOverskr[1] = "Justeringar"
      cOverskr[2] = "Antal"
      cOverskr[3] = "Belopp".
    ASSIGN 
      cLabel[1] = "Ing�ende interna �verf�ringar"      
      cLabel[2] = "Utg�ende interna �verf�ringar"                
      cLabel[3] = "Varumottagningar"        
      cLabel[4] = "Lagerjusteringar"             
      cLabel[5] = "Kassation"         
      cLabel[6] = "Intern f�rbrukning"          
      cLabel[7] = "Rensad varuf�rs�ljning".       
  END.
  ELSE 
  DO:
    ASSIGN 
      cOverskr[1] = "Justeringer"
      cOverskr[2] = "Antall"
      cOverskr[3] = "Bel�p".
    ASSIGN 
      cLabel[1] = "Inng�ende interne overf�ringer"      
      cLabel[2] = "Utg�ende interne overf�ringer"                
      cLabel[3] = "Varemottak"        
      cLabel[4] = "Lagerjusteringer"             
      cLabel[5] = "Brekkasje"         
      cLabel[6] = "Internt forbruk"          
      cLabel[7] = "Renset varesalg".       
  END.
  ASSIGN 
    dY = 301.
  /*    dY = dY - 65.*/
  /* RUBRIK */
  RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold",12).
  RUN pdf_text_xy_dec ("Spdf",cOverskr[1],dColPosFR[1],dY).
  dY = dY - 4.
  RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica",8).
  RUN pdf_line IN h_PDFinc  ("Spdf", pdf_LeftMargin ("Spdf"), dY, dColPosFR[3], dY, 0.5).
  dY = dY - iLineSpace.
  /* Kolonnrubrtik */
  RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold",8).
  RUN pdf_text_xy_dec ("Spdf",cOverskr[2],dColPosFR[2] - bredd(cOverskr[2]),dY).
  RUN pdf_text_xy_dec ("Spdf",cOverskr[3],dColPosFR[3] - bredd(cOverskr[3]),dY).
  dY = dY - 4.
  RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica",8).
  RUN pdf_line IN h_PDFinc  ("Spdf", dULstartFR[2], dY, dColPosFR[2], dY, 0.5).
  RUN pdf_line IN h_PDFinc  ("Spdf", dULstartFR[3], dY, dColPosFR[3], dY, 0.5).
  /* 1 Ing�ende interna �verf */
  dY = dY - iLineSpace.
  RUN pdf_text_xy_dec ("Spdf",cLabel[1],dColPosFR[1],dY).
  cBelopp = TRIM(STRING(tmpKas_Rap.AntOverfortInn,"->>,>>9")).
  RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPosFR[2] - bredd(cBelopp),dY).
  cBelopp = TRIM(STRING(tmpKas_Rap.OverfortInn,"->>>,>>>,>>9.99")).
  RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPosFR[3] - bredd(cBelopp),dY).
  /* 2 Utg�ende interna �verf */
  dY = dY - iLineSpace.
  RUN pdf_text_xy_dec ("Spdf","+",dColPosFR[1] - 5,dY).
  RUN pdf_text_xy_dec ("Spdf",cLabel[2],dColPosFR[1],dY).
  cBelopp = TRIM(STRING(tmpKas_Rap.AntOverfortUt,"->>,>>9")).
  RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPosFR[2] - bredd(cBelopp),dY).
  cBelopp = TRIM(STRING(tmpKas_Rap.OverfortUt,"->>>,>>>,>>9.99")).
  RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPosFR[3] - bredd(cBelopp),dY).
  /* 3 varumottag */
  dY = dY - iLineSpace.
  RUN pdf_text_xy_dec ("Spdf","-",dColPosFR[1] - 5,dY).
  RUN pdf_text_xy_dec ("Spdf",cLabel[3],dColPosFR[1],dY).
  cBelopp = TRIM(STRING(tmpKas_Rap.AntVaremottak,"->>,>>9")).
  RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPosFR[2] - bredd(cBelopp),dY).
  cBelopp = TRIM(STRING(tmpKas_Rap.Varemottak,"->>>,>>>,>>9.99")).
  RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPosFR[3] - bredd(cBelopp),dY).
  /* 4 Lagerjusteringer */
  dY = dY - iLineSpace.
  RUN pdf_text_xy_dec ("Spdf","-",dColPosFR[1] - 5,dY).
  RUN pdf_text_xy_dec ("Spdf",cLabel[4],dColPosFR[1],dY).
  cBelopp = TRIM(STRING(tmpKas_Rap.AntLagerjustering,"->>,>>9")).
  RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPosFR[2] - bredd(cBelopp),dY).
  cBelopp = TRIM(STRING(tmpKas_Rap.Lagerjustering,"->>>,>>>,>>9.99")).
  RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPosFR[3] - bredd(cBelopp),dY).
  /* 5 Kassation */
  dY = dY - iLineSpace.
  RUN pdf_text_xy_dec ("Spdf","-",dColPosFR[1] - 5,dY).
  RUN pdf_text_xy_dec ("Spdf",cLabel[5],dColPosFR[1],dY).
  cBelopp = TRIM(STRING(tmpKas_Rap.AntBrekkasje,"->>,>>9")).
  RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPosFR[2] - bredd(cBelopp),dY).
  cBelopp = TRIM(STRING(tmpKas_Rap.Brekkasje,"->>>,>>>,>>9.99")).
  RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPosFR[3] - bredd(cBelopp),dY).
  /* 6 Internt f�rbruk */
  dY = dY - iLineSpace.
  RUN pdf_text_xy_dec ("Spdf","-",dColPosFR[1] - 5,dY).
  RUN pdf_text_xy_dec ("Spdf",cLabel[6],dColPosFR[1],dY).
  cBelopp = TRIM(STRING(tmpKas_Rap.AntInterntForbruk,"->>,>>9")).
  RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPosFR[2] - bredd(cBelopp),dY).
  cBelopp = TRIM(STRING(tmpKas_Rap.InterntForbruk,"->>>,>>>,>>9.99")).
  RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPosFR[3] - bredd(cBelopp),dY).
  /* 7 Rensad f�rs�ljning */
  dY = dY - iLineSpace.
  RUN pdf_text_xy_dec ("Spdf","-",dColPosFR[1] - 5,dY).
  RUN pdf_text_xy_dec ("Spdf",cLabel[7],dColPosFR[1],dY).
  cBelopp = TRIM(STRING(0,"->>,>>9")).
  RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPosFR[2] - bredd(cBelopp),dY).
  cBelopp = TRIM(STRING(0,"->>>,>>>,>>9.99")).
  RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPosFR[3] - bredd(cBelopp),dY).
  /* line */
  dY = dY - 4.
  RUN pdf_line IN h_PDFinc  ("Spdf", dColPosFR[1], dY, dColPosFR[3], dY, 0.5).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Saml_1_kassa_inslaget) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Saml_1_kassa_inslaget Procedure 
PROCEDURE Saml_1_kassa_inslaget :
  /*------------------------------------------------------------------------------
    Purpose:     
    Parameters:  <none>
    Notes:       
  ------------------------------------------------------------------------------*/
  DEFINE INPUT-OUTPUT PARAMETER dY AS DECIMAL     NO-UNDO.
  DEFINE VARIABLE cOverskr AS CHARACTER EXTENT 3 NO-UNDO.
  DEFINE VARIABLE cLabel   AS CHARACTER EXTENT 12 NO-UNDO.

  IF CAN-DO("SE,SVE",cSprak) THEN 
  DO:
    ASSIGN 
      cOverskr[1] = "Kassaredovisning (inslaget)"
      cOverskr[2] = "Antal"
      cOverskr[3] = "Belopp".
    ASSIGN 
      cLabel[1]  = "Kontant"      
      cLabel[2]  = getTransName(54) /* "Check/trasig sedel CG" */
      cLabel[3]  = "Bank"        
      cLabel[4]  = "Kredit"             
      cLabel[5]  = "Off-line betalning"         
      cLabel[6]  = "Presentkort ut"          
      cLabel[7]  = "Presentkort in"       
      cLabel[8]  = "Tillgodo"       
      cLabel[9]  = "Kort"             
      cLabel[10] = "Kupong 1"
      cLabel[11] = "Kupong 2"
      cLabel[12] = "Totalt i kassa (inslaget)".
  END.
  ELSE 
  DO:
    ASSIGN 
      cOverskr[1] = "Kasseregnskap (innslaget)"
      cOverskr[2] = "Antall"
      cOverskr[3] = "Bel�p".
    ASSIGN 
      cLabel[1]  = "Kontant"      
      cLabel[2]  = getTransName(54) /* "Check/" */
      cLabel[3]  = "Bank"        
      cLabel[4]  = "Kreditt"             
      cLabel[5]  = "Off-line betaling"         
      cLabel[6]  = "Gavekort ut"          
      cLabel[7]  = "Gavekort in"       
      cLabel[8]  = "Tilgode"       
      cLabel[9]  = "Kort"             
      cLabel[10] = "Kupong 1"
      cLabel[11] = "Kupong 2"
      cLabel[12] = "Totalt i kasse (innslaget)".
  END.
  /*     dY = dY - 20. */
  RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold",12).
  RUN pdf_text_xy_dec ("Spdf",cOverskr[1],dColPosFR[4],dY).
  dY = dY - 4.
  RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica",8).
  RUN pdf_line IN h_PDFinc  ("Spdf", dColPosFR[4], dY, dColPosFR[6], dY, 0.5).
  dY = dY - iLineSpace.
  RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold",8).
  RUN pdf_text_xy_dec ("Spdf",cOverskr[2],dColPosFR[5] - bredd(cOverskr[2]),dY).
  RUN pdf_text_xy_dec ("Spdf",cOverskr[3],dColPosFR[6] - bredd(cOverskr[3]),dY).
  dY = dY - 4.
  RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica",8).
  RUN pdf_line IN h_PDFinc  ("Spdf", dULstartFR[5], dY, dColPosFR[5], dY, 0.5).
  RUN pdf_line IN h_PDFinc  ("Spdf", dULstartFR[6], dY, dColPosFR[6], dY, 0.5).
  dY = dY - iLineSpace. /* 1 kontant */
  RUN pdf_text_xy_dec ("Spdf",cLabel[1],dColPosFR[4],dY).
  cBelopp = TRIM(STRING(tmpKas_Rap.AntKontant,"->>,>>9")).
  RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPosFR[5] - bredd(cBelopp),dY).
  /*     cBelopp = TRIM(STRING(tmpKas_Rap.Kontant + tmpKas_Rap.Layaway_inn - tmpKas_rap.Non_SaleNeg,"->>>,>>>,>>9.99")). */
  cBelopp = TRIM(STRING(tmpKas_Rap.KontantBeholdning + tmpKas_Rap.dropp,"->>>,>>>,>>9.99")).
  RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPosFR[6] - bredd(cBelopp),dY).
  dY = dY - iLineSpace. /* 2 sjekk */
  RUN pdf_text_xy_dec ("Spdf","+",dColPosFR[4] - 5,dY).
  RUN pdf_text_xy_dec ("Spdf",cLabel[2],dColPosFR[4],dY).
  cBelopp = TRIM(STRING(tmpKas_Rap.AntSjekk,"->>,>>9")).
  RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPosFR[5] - bredd(cBelopp),dY).
  cBelopp = TRIM(STRING(tmpKas_Rap.SjekkBeholdning,"->>>,>>>,>>9.99")).
  RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPosFR[6] - bredd(cBelopp),dY).
  dY = dY - iLineSpace. /* 3 bank */
  RUN pdf_text_xy_dec ("Spdf","+",dColPosFR[4] - 5,dY).
  RUN pdf_text_xy_dec ("Spdf",cLabel[3],dColPosFR[4],dY).
  /*    cBelopp = TRIM(STRING(tmpKas_Rap.AntBank + tmpKas_Rap.AntKort,"->>,>>9")).*/
  cBelopp = TRIM(STRING(tmpKas_Rap.AntBank + tmpKas_Rap.AntCashBack,"->>,>>9")).
  RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPosFR[5] - bredd(cBelopp),dY).
  cBelopp = TRIM(STRING(tmpKas_Rap.Bank + tmpKas_Rap.Cashback,"->>>,>>>,>>9.99")).
  RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPosFR[6] - bredd(cBelopp),dY).
  dY = dY - iLineSpace. /* 4 kredit */
  RUN pdf_text_xy_dec ("Spdf","+",dColPosFR[4] - 5,dY).
  RUN pdf_text_xy_dec ("Spdf",cLabel[4],dColPosFR[4],dY).
  cBelopp = TRIM(STRING(tmpKas_Rap.AntKredit,"->>,>>9")).
  RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPosFR[5] - bredd(cBelopp),dY).
  cBelopp = TRIM(STRING(tmpKas_Rap.Kredit,"->>>,>>>,>>9.99")).
  RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPosFR[6] - bredd(cBelopp),dY).
  dY = dY - iLineSpace. /* 5 reserv bank */
  RUN pdf_text_xy_dec ("Spdf","+",dColPosFR[4] - 5,dY).
  RUN pdf_text_xy_dec ("Spdf",cLabel[5],dColPosFR[4],dY).
  cBelopp = TRIM(STRING(tmpKas_Rap.AntReservelosning,"->>,>>9")).
  RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPosFR[5] - bredd(cBelopp),dY).
  cBelopp = TRIM(STRING(tmpKas_Rap.Reservelosning,"->>>,>>>,>>9.99")).
  RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPosFR[6] - bredd(cBelopp),dY).
  dY = dY - iLineSpace. /* 6 Gavekort ut */
  RUN pdf_text_xy_dec ("Spdf","-",dColPosFR[4] - 5,dY).
  RUN pdf_text_xy_dec ("Spdf",cLabel[6],dColPosFR[4],dY).
  cBelopp = TRIM(STRING(tmpKas_Rap.GavekortUt,"->>>,>>>,>>9.99")).
  RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPosFR[6] - bredd(cBelopp),dY).
  dY = dY - iLineSpace. /* 7 Gavekort in */
  RUN pdf_text_xy_dec ("Spdf","+",dColPosFR[4] - 5,dY).
  RUN pdf_text_xy_dec ("Spdf",cLabel[7],dColPosFR[4],dY).
  cBelopp = TRIM(STRING(tmpKas_Rap.GavekortInn,"->>>,>>>,>>9.99")).
  RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPosFR[6] - bredd(cBelopp),dY).
  dY = dY - iLineSpace. /* 8 tilgode */
  RUN pdf_text_xy_dec ("Spdf","+",dColPosFR[4] - 5,dY).
  RUN pdf_text_xy_dec ("Spdf",cLabel[8],dColPosFR[4],dY).
  cBelopp = TRIM(STRING(tmpKas_Rap.AntTilgode,"->>,>>9")).
  RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPosFR[5] - bredd(cBelopp),dY).
  cBelopp = TRIM(STRING(tmpKas_Rap.Tilgode,"->>>,>>>,>>9.99")).
  RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPosFR[6] - bredd(cBelopp),dY).
  dY = dY - iLineSpace. /* 9 kort */
  RUN pdf_text_xy_dec ("Spdf","+",dColPosFR[4] - 5,dY).
  RUN pdf_text_xy_dec ("Spdf",cLabel[9],dColPosFR[4],dY).
  cBelopp = TRIM(STRING(tmpKas_Rap.AntKort,"->>,>>9")).
  RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPosFR[5] - bredd(cBelopp),dY).
  cBelopp = TRIM(STRING(tmpKas_Rap.Kort,"->>>,>>>,>>9.99")).
  RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPosFR[6] - bredd(cBelopp),dY).
  dY = dY - iLineSpace. /* 10 kupopong 1 */
  RUN pdf_text_xy_dec ("Spdf","+",dColPosFR[4] - 5,dY).
  RUN pdf_text_xy_dec ("Spdf",cLabel[10],dColPosFR[4],dY).
  cBelopp = TRIM(STRING(tmpKas_Rap.AntKupong1,"->>,>>9")).
  RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPosFR[5] - bredd(cBelopp),dY).
  cBelopp = TRIM(STRING(tmpKas_Rap.Kupong1,"->>>,>>>,>>9.99")).
  RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPosFR[6] - bredd(cBelopp),dY).
  dY = dY - iLineSpace. /* 11 kupong 2 */
  RUN pdf_text_xy_dec ("Spdf","+",dColPosFR[4] - 5,dY).
  RUN pdf_text_xy_dec ("Spdf",cLabel[11],dColPosFR[4],dY).
  cBelopp = TRIM(STRING(tmpKas_Rap.AntKupong2,"->>,>>9")).
  RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPosFR[5] - bredd(cBelopp),dY).
  cBelopp = TRIM(STRING(tmpKas_Rap.Kupong2,"->>>,>>>,>>9.99")).
  RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPosFR[6] - bredd(cBelopp),dY).

  dY = dY - 4.
  RUN pdf_line IN h_PDFinc  ("Spdf", dColPosFR[4], dY, dColPosFR[6], dY, 0.5).
  dY = dY - iLineSpace. /* sum */
  RUN pdf_text_xy_dec ("Spdf","=",dColPosFR[4] - 5,dY).
  RUN pdf_text_xy_dec ("Spdf",cLabel[12],dColPosFR[4],dY).
  /*     cBelopp = STRING(tmpKas_Rap.Kontant     + tmpKas_Rap.Sjekk      + tmpKas_Rap.Bank    + tmpKas_Rap.Kredit      + tmpKas_Rap.Reservelosning +              */
  /*                      tmpKas_Rap.GavekortInn - tmpKas_Rap.GavekortUt + tmpKas_Rap.Tilgode + tmpKas_Rap.Kort       + tmpKas_Rap.Kupong1 + tmpKas_Rap.Kupong2 + */
  /*                         tmpKas_Rap.Layaway_inn - tmpKas_rap.Non_SaleNeg,"->>>,>>>,>>9.99").                                                                  */
  dTotaltInslaget = tmpKas_Rap.KontantBeholdning + tmpKas_Rap.SjekkBeholdning + tmpKas_Rap.Bank + tmpKas_Rap.Cashback +
  tmpKas_Rap.Kredit + tmpKas_Rap.Reservelosning /* - tmpKas_Rap.GavekortUt */ + tmpKas_Rap.GavekortInn + tmpKas_Rap.Tilgode + 
    tmpKas_Rap.Kort + tmpKas_Rap.Kupong1 + tmpKas_Rap.Kupong2 + tmpKas_Rap.dropp.
  cBelopp = STRING(dTotaltInslaget,"->>>,>>>,>>9.99").
  RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPosFR[6] - bredd(cBelopp),dY).
  dY = dY - 4.
  RUN pdf_line IN h_PDFinc  ("Spdf", dColPosFR[4], dY, dColPosFR[6], dY, 0.5).

  /* h�r justerar vi totaltinslaget att j�mf�ras med kassereroppg�r */
  dTotaltInslaget = dTotaltInslaget - tmpKas_Rap.Bank - tmpKas_Rap.Cashback - tmpKas_Rap.Kredit - tmpKas_Rap.Reservelosning - tmpKas_Rap.Kort - tmpKas_Rap.Reservelosning.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Saml_1_Kassor_talt) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Saml_1_Kassor_talt Procedure 
PROCEDURE Saml_1_Kassor_talt :
  /*------------------------------------------------------------------------------
    Purpose:     
    Parameters:  <none>
    Notes:       
  ------------------------------------------------------------------------------*/
  DEFINE INPUT-OUTPUT PARAMETER dY AS DECIMAL     NO-UNDO.
  DEFINE VARIABLE cOverskr    AS CHARACTER EXTENT 3 NO-UNDO.
  DEFINE VARIABLE cLabel      AS CHARACTER EXTENT 13 NO-UNDO.
  DEFINE VARIABLE dSumOpptalt AS DECIMAL   NO-UNDO.
  DEFINE VARIABLE dWrk        AS DECIMAL   NO-UNDO.
  IF CAN-DO("SE,SVE",cSprak) THEN 
  DO:
    ASSIGN 
      cOverskr[1] = "Uppr�knat enligt kass�r"
      cOverskr[2] = "Antal"
      cOverskr[3] = "Belopp".
    ASSIGN 
      cLabel[1]  = "Kontant beh�llning"      
      cLabel[2]  = "V�xelbeh�llning"                
      cLabel[3]  = getTransName(54) /* "Check/trasig sedel CG" */
      cLabel[4]  = "Dropp"             
      cLabel[5]  = "Reserv bank"         
      cLabel[6]  = "Rekvisition"          
      cLabel[7]  = "Presentkort"
      cLabel[8]  = "Tillgodokvitton"
      cLabel[9]  = "Kupong"
      cLabel[10] = "Valuta"
      cLabel[11] = "Totalt uppr�knat"       
      cLabel[12] = "Totalt inslaget"
      cLabel[13] = "Differens kassa"
      .
  END.
  ELSE 
  DO:
    ASSIGN 
      cOverskr[1] = "Beholdning"
      cOverskr[2] = "Antall"
      cOverskr[3] = "Bel�p".
    ASSIGN 
      cLabel[1]  = "Kontant beholdning"      
      cLabel[2]  = "Veksel"                
      cLabel[3]  = getTransName(54) /* "Check" */
      cLabel[4]  = "Dropp"             
      cLabel[5]  = "Reserve bank"         
      cLabel[6]  = "Rekvisisjon"          
      cLabel[7]  = "Gavekort"
      cLabel[8]  = "Tilgodelapper"
      cLabel[9]  = "Kupong"
      cLabel[10] = "Valuta"       
      cLabel[11] = "Sum beholdning"       
      cLabel[12] = "Totalt innslaget"
      cLabel[13] = "Differens kasse"
      .
  END.
  dY = dY - 20.
  RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold",12).
  RUN pdf_text_xy_dec ("Spdf",cOverskr[1],dColPosFR[4],dY).
  dY = dY - 4.
  RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica",8).
  RUN pdf_line IN h_PDFinc  ("Spdf", dColPosFR[4], dY, dColPosFR[6], dY, 0.5).
  dY = dY - iLineSpace.
  RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold",8).
  /*     RUN pdf_text_xy_dec ("Spdf",cOverskr[2],dColPosFR[5] - bredd(cOverskr[2]),dY). */
  RUN pdf_text_xy_dec ("Spdf",cOverskr[3],dColPosFR[6] - bredd(cOverskr[3]),dY).
  dY = dY - 4.
  RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica",8).
  /*     RUN pdf_line IN h_PDFinc  ("Spdf", dULstartFR[5], dY, dColPosFR[5], dY, 0.5). */
  RUN pdf_line IN h_PDFinc  ("Spdf", dULstartFR[6], dY, dColPosFR[6], dY, 0.5).

  dY = dY - iLineSpace. /* 1 Kontantbeholding */
  RUN pdf_text_xy_dec ("Spdf",cLabel[1],dColPosFR[4],dY).
  cBelopp = TRIM(STRING(tmpKas_Rap.OpptaltKontanter - tmpKas_Rap.Dropp,"->>>,>>>,>>9.99")). /* Lagt till - tmpKas_Rap.Dropp IGEN 20140205 ghg */
  RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPosFR[6] - bredd(cBelopp),dY).
    
  dY = dY - iLineSpace. /* 2 Vekselbeholdning */
  RUN pdf_text_xy_dec ("Spdf",cLabel[2],dColPosFR[4],dY).
  cBelopp = TRIM(STRING(tmpKas_Rap.OpptaltVeksel,"->>>,>>>,>>9.99")).
    
  RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPosFR[6] - bredd(cBelopp),dY).
  dY = dY - iLineSpace. /* 3 Sjekkbeholdning */
  RUN pdf_text_xy_dec ("Spdf",cLabel[3],dColPosFR[4],dY).
  cBelopp = TRIM(STRING(tmpKas_Rap.OpptaltSjekk,"->>>,>>>,>>9.99")).
  RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPosFR[6] - bredd(cBelopp),dY).
  dY = dY - iLineSpace. /* 4 Dropp */
  RUN pdf_text_xy_dec ("Spdf",cLabel[4],dColPosFR[4],dY).
  cBelopp = TRIM(STRING(tmpKas_Rap.Dropp,"->>>,>>>,>>9.99")).
  RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPosFR[6] - bredd(cBelopp),dY).
  dY = dY - iLineSpace. /* 5 Reserv bank */
  RUN pdf_text_xy_dec ("Spdf",cLabel[5],dColPosFR[4],dY).
  cBelopp = TRIM(STRING(tmpKas_Rap.OpptaltReserve,"->>>,>>>,>>9.99")).
  RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPosFR[6] - bredd(cBelopp),dY).
  dY = dY - iLineSpace. /* 6 Rekvisisjon */
  RUN pdf_text_xy_dec ("Spdf",cLabel[6],dColPosFR[4],dY).
  cBelopp = TRIM(STRING(tmpKas_Rap.Rekvisisasjon,"->>>,>>>,>>9.99")).
  RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPosFR[6] - bredd(cBelopp),dY).
  dY = dY - iLineSpace. /* 7 Gavekort */
  RUN pdf_text_xy_dec ("Spdf",cLabel[7],dColPosFR[4],dY).
  cBelopp = TRIM(STRING(tmpKas_Rap.OpptaltGavekort + tmpKas_Rap.OpptaltGavekortAndre,"->>>,>>>,>>9.99")).
  RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPosFR[6] - bredd(cBelopp),dY).
  dY = dY - iLineSpace. /* 8 Tilgode */
  RUN pdf_text_xy_dec ("Spdf",cLabel[8],dColPosFR[4],dY).
  /*    ASSIGN dWrk = (tmpKas_Rap.OpptaltTilgode + tmpKas_Rap.OpptaltTilgodeAndre) * -1. */ /* ghg 20131120 */
  cBelopp = TRIM(STRING(tmpKas_Rap.OpptaltTilgode + tmpKas_Rap.OpptaltTilgodeAndre,"->>>,>>>,>>9.99")).
  /*     cBelopp = TRIM(STRING(dWrk,"->>>,>>>,>>9.99")). */
  RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPosFR[6] - bredd(cBelopp),dY).
  dY = dY - iLineSpace. /* 9 Kupong */
  RUN pdf_text_xy_dec ("Spdf",cLabel[9],dColPosFR[4],dY).
  cBelopp = TRIM(STRING(tmpKas_Rap.OpptaltKupong,"->>>,>>>,>>9.99")).
  RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPosFR[6] - bredd(cBelopp),dY).
  dY = dY - iLineSpace. /* 10 Valuta */
  RUN pdf_text_xy_dec ("Spdf",cLabel[10],dColPosFR[4],dY).
  cBelopp = TRIM(STRING(tmpKas_Rap.OpptaltValuta,"->>>,>>>,>>9.99")).
  RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPosFR[6] - bredd(cBelopp),dY).

  ASSIGN 
    dSumOpptalt = tmpKas_Rap.OpptaltKontanter + tmpKas_Rap.OpptaltVeksel + tmpKas_Rap.OpptaltSjekk +
                         /*tmpKas_Rap.Dropp +*/ tmpKas_Rap.OpptaltReserve + tmpKas_Rap.Rekvisisasjon +
                         tmpKas_Rap.OpptaltGavekort + tmpKas_Rap.OpptaltGavekortAndre + 
                         /*dWrk + */ /* ghg 20131120 */
                         tmpKas_Rap.OpptaltTilgode + tmpKas_Rap.OpptaltTilgodeAndre + 
                         tmpKas_Rap.OpptaltKupong + tmpKas_Rap.OpptaltValuta.

  dY = dY - 4.
  RUN pdf_line IN h_PDFinc  ("Spdf", dColPosFR[4], dY, dColPosFR[6], dY, 0.5).
  dY = dY - iLineSpace. /* 11 Totalt uppr�knat */
  RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold",8).
  RUN pdf_text_xy_dec ("Spdf",cLabel[11],dColPosFR[4],dY).
  cBelopp = TRIM(STRING(dSumOpptalt,"->>>,>>>,>>9.99")).
  RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPosFR[6] - bredd(cBelopp),dY).
  dY = dY - 4.
  RUN pdf_line IN h_PDFinc  ("Spdf", dColPosFR[4], dY, dColPosFR[6], dY, 0.5).
  RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica",8).

  dY = dY - iLineSpace. /* 12 Totalt inslaget */
  RUN pdf_text_xy_dec ("Spdf",cLabel[12],dColPosFR[4],dY).
  RUN pdf_text_xy_dec ("Spdf","-",dColPosFR[4] - 5,dY).
  cBelopp = TRIM(STRING(dTotaltInslaget,"->>>,>>>,>>9.99")).
  RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPosFR[6] - bredd(cBelopp),dY).

  dY = dY - 4.
  RUN pdf_line IN h_PDFinc  ("Spdf", dColPosFR[4], dY, dColPosFR[6], dY, 0.5).
  RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold",8).
  dY = dY - iLineSpace. /* 13 Differens */
  RUN pdf_text_xy_dec ("Spdf",cLabel[13],dColPosFR[4],dY).
  cBelopp = TRIM(STRING(dSumOpptalt - dTotaltInslaget,"->>>,>>>,>>9.99")).
  RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPosFR[6] - bredd(cBelopp),dY).
  dY = dY - 4.
  RUN pdf_line IN h_PDFinc  ("Spdf", dColPosFR[4], dY, dColPosFR[6], dY, 0.5).

  RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica",8).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Saml_1_Kontant) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Saml_1_Kontant Procedure 
PROCEDURE Saml_1_Kontant :
  /*------------------------------------------------------------------------------
    Purpose:     
    Parameters:  <none>
    Notes:       Oppstilling Kontant midt p� venstre side av samlingsrapporten.
  ------------------------------------------------------------------------------*/
  DEFINE INPUT-OUTPUT PARAMETER dY AS DECIMAL     NO-UNDO.
  DEFINE VARIABLE cOverskr AS CHARACTER EXTENT 3 NO-UNDO.
  DEFINE VARIABLE cLabel   AS CHARACTER EXTENT 3 NO-UNDO.
  DEFINE VARIABLE cTxt     AS CHARACTER NO-UNDO.
  IF CAN-DO("SE,SVE",cSprak) THEN 
  DO:
    ASSIGN 
      cOverskr[1] = "Kontant"
      cOverskr[2] = "Antal"
      cOverskr[3] = "Belopp".
    ASSIGN 
      cLabel[1] = "Kontant"      
      cLabel[2] = getTransName(54) /* "Check/Trasig sedel CG" */
      cLabel[3] = "Totalt kontant".
  END.
  ELSE 
  DO:
    ASSIGN 
      cOverskr[1] = "Kontant"
      cOverskr[2] = "Antall"
      cOverskr[3] = "Bel�p".
    ASSIGN 
      cLabel[1] = "Kontant"
      cLabel[2] = "Sjekk/"
      cLabel[3] = "Totalt bank".
  END.
  /*    dY = dY - 120.  */
  ASSIGN 
    dY = 391.

  /* RUBRIK */
  RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold",12).
  RUN pdf_text_xy_dec ("Spdf",cOverskr[1],dColPosFR[1],dY).
  dY = dY - 4.
  RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica",8).
  RUN pdf_line IN h_PDFinc  ("Spdf", pdf_LeftMargin ("Spdf"), dY, dColPosFR[3], dY, 0.5).
  dY = dY - iLineSpace.

  /* Kolonnrubrtik */
  RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold",8).
  RUN pdf_text_xy_dec ("Spdf",cOverskr[2],dColPosFR[2] - bredd(cOverskr[2]),dY).
  RUN pdf_text_xy_dec ("Spdf",cOverskr[3],dColPosFR[3] - bredd(cOverskr[3]),dY).
  dY = dY - 4.
  RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica",8).
  RUN pdf_line IN h_PDFinc  ("Spdf", dULstartFR[2], dY, dColPosFR[2], dY, 0.5).
  RUN pdf_line IN h_PDFinc  ("Spdf", dULstartFR[3], dY, dColPosFR[3], dY, 0.5).

  /* 1 Kontant */
  dY = dY - iLineSpace.
  RUN pdf_text_xy_dec ("Spdf",cLabel[1],dColPosFR[1],dY).
  /*     cBelopp = TRIM(STRING(tmpKas_Rap.KontantBeholdning + tmpKas_Rap.Layaway_inn - tmpKas_rap.Non_SaleNeg,"->>>,>>>,>>9.99")). */
  cBelopp = TRIM(STRING(tmpKas_Rap.KontantBeholdning + tmpkas_rap.dropp ,"->>>,>>>,>>9.99")).
  RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPosFR[3] - bredd(cBelopp),dY).

  /* 2 Check - Trasig seddel*/
  dY = dY - iLineSpace.
  RUN pdf_text_xy_dec ("Spdf","+",dColPosFR[1] - 5,dY).
  RUN pdf_text_xy_dec ("Spdf",cLabel[2],dColPosFR[1],dY).
  cTxt = TRIM(STRING(tmpKas_Rap.AntSjekk,"->>>,>>>,>>9.99")).
  RUN pdf_text_xy_dec ("Spdf",cTxt,dColPosFR[2] - bredd(cTxt),dY).
  cBelopp = TRIM(STRING(tmpKas_Rap.SjekkBeholdning,"->>>,>>>,>>9.99")).
  RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPosFR[3] - bredd(cBelopp),dY).

  /* line */
  dY = dY - 4.
  RUN pdf_line IN h_PDFinc  ("Spdf", dColPosFR[1], dY, dColPosFR[3], dY, 0.5).

  /* 3 Totalt kontant */
  dY = dY - iLineSpace.
  RUN pdf_text_xy_dec ("Spdf","=",dColPosFR[1] - 5,dY).
  RUN pdf_text_xy_dec ("Spdf",cLabel[3],dColPosFR[1],dY).
  /*  cBelopp = TRIM(STRING(tmpKas_Rap.KontantBeholdning + kas_rap.dropp + tmpKas_Rap.SjekkBeholdning,"->>>,>>>,>>9.99")).*/
  cBelopp = TRIM(STRING(tmpKas_Rap.KontantBeholdning + tmpkas_rap.dropp + tmpKas_Rap.SjekkBeholdning,"->>>,>>>,>>9.99")).
  RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPosFR[3] - bredd(cBelopp),dY).
    
  /* line */
  dY = dY - 4.
  RUN pdf_line IN h_PDFinc  ("Spdf", dColPosFR[1], dY, dColPosFR[3], dY, 0.5).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Saml_1_mva) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Saml_1_mva Procedure 
PROCEDURE Saml_1_mva :
  /*------------------------------------------------------------------------------
    Purpose:     
    Parameters:  <none>
    Notes:       
  ------------------------------------------------------------------------------*/
  DEFINE INPUT-OUTPUT PARAMETER dY AS DECIMAL     NO-UNDO.
  DEFINE VARIABLE cOverskr AS CHARACTER EXTENT 5 NO-UNDO.
  DEFINE VARIABLE cLabel   AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cTxt     AS CHARACTER NO-UNDO.
  DEFINE VARIABLE dY2      AS DECIMAL   NO-UNDO.
  DEFINE VARIABLE iCount   AS INTEGER   NO-UNDO.
  /* Mva regnskap totalt varesalg */
  IF CAN-DO("SE,SVE",cSprak) THEN 
  DO:
    ASSIGN 
      cOverskr[1] = "Moms f�rs�ljning"
      cOverskr[2] = "Grupp"
      cOverskr[3] = "Underlag"
      cOverskr[4] = "Belopp"
      cOverskr[5] = "Summa".
    ASSIGN 
      cLabel = "Totalt".
  END.
  ELSE 
  DO:
    ASSIGN 
      cOverskr[1] = "Mvaregnskap"
      cOverskr[2] = "Gruppe"
      cOverskr[3] = "Grunnlag"
      cOverskr[4] = "Bel�p"
      cOverskr[5] = "Sum".
    ASSIGN 
      cLabel = "Totalt".
  END.
  /*   ASSIGN dColPosMVA[1] = 80
            dColPosMVA[2] = 140
            dColPosMVA[3] = 190
            dColPosMVA[4] = dColPosFR[3].*/
  ASSIGN 
    dColPosMVA[1] = 355
    dColPosMVA[2] = 140 + 275
    dColPosMVA[3] = 190 + 275
    dColPosMVA[4] = 250 + 305.
  /*   ASSIGN dULstartMVA[1] = pdf_LeftMargin ("Spdf")
            dULstartMVA[2] = 90
            dULstartMVA[3] = 150
            dULstartMVA[4] = 200.*/
  ASSIGN 
    dULstartMVA[1] = 315
    dULstartMVA[2] = 90 + 235
    dULstartMVA[3] = 150 + 235
    dULstartMVA[4] = 200 + 265.

  /*        PUT UNFORMATTED                                                                                */
  /*            "<P12><R+1><C6><B>"  ENTRY(1,pcOverskr,CHR(1)) SKIP "</B><C6><FROM><C35><LINE>" SKIP       */
  /*            "<P8>"                                                                                     */
  /*            "<C6><RIGHT=C+6>" ENTRY(2,pcOverskr,CHR(1))                                                */
  /*            "<C13><RIGHT=C+7>" ENTRY(3,pcOverskr,CHR(1))                                               */
  /*            "<C21><RIGHT=C+6>" ENTRY(4,pcOverskr,CHR(1))                                               */
  /*            "<C28><RIGHT=C+7>" ENTRY(5,pcOverskr,CHR(1)) SKIP                                          */
  /*            "<C6><FROM><C12><LINE><C13><FROM><C20><LINE><C21><FROM><C27><LINE><C28><FROM><C35><LINE>". */

  ASSIGN 
    dY2 = 195.
  dY = dY - 20.
  RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold",12).
  RUN pdf_text_xy_dec ("Spdf",cOverskr[1],dColPosFR[4],dY2).
  /*   RUN pdf_text_xy_dec ("Spdf",cOverskr[1],dColPosFR[1],dY).*/
  RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica",8).
  dY = dY - 4.
  dy2 = dY2 - 4.
  /*   RUN pdf_line IN h_PDFinc  ("Spdf", pdf_LeftMargin ("Spdf"), dY, dColPosFR[3], dY, 0.5).*/
  RUN pdf_line IN h_PDFinc  ("Spdf", dColPosFR[4], dY2, dColPosFR[6], dY2, 0.5).
  dY = dY - iLineSpace.
  dy2 = dy2 - iLineSpace.
  RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold",8).
  RUN pdf_text_xy_dec ("Spdf",cOverskr[2],dColPosMVA[1] - bredd(cOverskr[5]),dY2).
  RUN pdf_text_xy_dec ("Spdf",cOverskr[3],dColPosMVA[2] - bredd(cOverskr[3]),dY2).
  RUN pdf_text_xy_dec ("Spdf",cOverskr[4],dColPosMVA[3] - bredd(cOverskr[4]),dY2).
  RUN pdf_text_xy_dec ("Spdf",cOverskr[5],dColPosMVA[4] - bredd(cOverskr[5]),dY2).
  dY = dY - 4.
  dY2 = dY2 - 4.
  RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica",8).
  RUN pdf_line IN h_PDFinc  ("Spdf", dULstartMVA[1], dY2, dColPosMVA[1], dY2, 0.5).
  RUN pdf_line IN h_PDFinc  ("Spdf", dULstartMVA[2], dY2, dColPosMVA[2], dY2, 0.5).
  RUN pdf_line IN h_PDFinc  ("Spdf", dULstartMVA[3], dY2, dColPosMVA[3], dY2, 0.5).
  RUN pdf_line IN h_PDFinc  ("Spdf", dULstartMVA[4], dY2, dColPosMVA[4], dY2, 0.5).


  DO iCount = 1 TO 10:
    IF tmpKas_rap.MvaGrunnlag[iCount] <> 0 THEN 
    DO:
      dY = dY - iLineSpace.
      dY2 = dY2 - iLineSpace.
      FIND moms WHERE moms.momskod = tmpKas_rap.MvaGrp[iCount] NO-LOCK NO-ERROR.
      cTxt = IF AVAIL moms THEN STRING(moms.momsproc,">9.99") + "%"  ELSE "%".
      cBelopp = TRIM(STRING(tmpKas_rap.MvaGrp[iCount],">9")).
      RUN pdf_text_xy_dec ("Spdf",cBelopp,dULstartMVA[1],dY2).
      RUN pdf_text_xy_dec ("Spdf",cTxt,dColPosMVA[1] - bredd(cTxt),dY2).
      cBelopp = TRIM(STRING(tmpKas_rap.MvaGrunnlag[iCount],"->>>,>>>,>>9.99")).
      RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPosMVA[2] - bredd(cBelopp),dY2).
      cBelopp = TRIM(STRING(tmpKas_rap.MvaBelop[iCount],"->>>,>>>,>>9.99")).
      RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPosMVA[3] - bredd(cBelopp),dY2).
      cBelopp = TRIM(STRING(tmpKas_rap.MvaGrunnlag[iCount] + tmpKas_rap.MvaBelop[iCount],"->>>,>>>,>>9.99")).
      RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPosMVA[4] - bredd(cBelopp),dY2).
       
      ASSIGN 
        dMvaGrunnlag = dMvaGrunnlag + tmpKas_rap.MvaGrunnlag[iCount]
        dMvaBelop    = dMvaBelop    + tmpKas_rap.MvaBelop[iCount].
    END.
  END.
  dY = dY - 4.
  dY2 = dY2 - 4.
  RUN pdf_line IN h_PDFinc  ("Spdf", dULstartMVA[2], dY2, dColPosMVA[2], dY2, 0.5).
  RUN pdf_line IN h_PDFinc  ("Spdf", dULstartMVA[3], dY2, dColPosMVA[3], dY2, 0.5).
  RUN pdf_line IN h_PDFinc  ("Spdf", dULstartMVA[4], dY2, dColPosMVA[4], dY2, 0.5).
  dY = dY - iLineSpace.
  dY2 = dY2 - iLineSpace.
  RUN pdf_text_xy_dec ("Spdf",cLabel,dColPosMVA[1] - bredd(cBelopp),dY2).
  cBelopp = TRIM(STRING(dMvaGrunnlag,"->>>,>>>,>>9.99")).
  RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPosMVA[2] - bredd(cBelopp),dY2).
  cBelopp = TRIM(STRING(dMvaBelop,"->>>,>>>,>>9.99")).
  RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPosMVA[3] - bredd(cBelopp),dY2).
  cBelopp = TRIM(STRING(dMvaGrunnlag + dMvaBelop,"->>>,>>>,>>9.99")).
  RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPosMVA[4] - bredd(cBelopp),dY2).


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Saml_1_nyckeltal) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Saml_1_nyckeltal Procedure 
PROCEDURE Saml_1_nyckeltal :
  /*------------------------------------------------------------------------------
    Purpose:     
    Parameters:  <none>
    Notes:       
  ------------------------------------------------------------------------------*/
  DEFINE INPUT-OUTPUT PARAMETER dY AS DECIMAL     NO-UNDO.

  DEFINE VARIABLE cOverskr      AS CHARACTER EXTENT 7 NO-UNDO.
  DEFINE VARIABLE cLabel        AS CHARACTER EXTENT 4 NO-UNDO.
  DEFINE VARIABLE dColPos       AS DECIMAL   EXTENT 8 NO-UNDO.
  DEFINE VARIABLE dULStart      AS DECIMAL   EXTENT 8 NO-UNDO.
  DEFINE VARIABLE dFsgsum_kr    AS DECIMAL   NO-UNDO.
  DEFINE VARIABLE dPerKunde     AS DECIMAL   NO-UNDO.
  DEFINE VARIABLE dTB           AS DECIMAL   NO-UNDO.
  DEFINE VARIABLE dTB%          AS DECIMAL   NO-UNDO.
  DEFINE VARIABLE dTjanstoms    AS DECIMAL   NO-UNDO.
  DEFINE VARIABLE plMvaGrunnlag AS DECIMAL   NO-UNDO.
  DEFINE VARIABLE plMvaBelop    AS DECIMAL   NO-UNDO.

  /* Hentes fra statistikk */
  dTjanstoms    = DYNAMIC-FUNCTION('getTjenesteOms':U).
  plMvaGrunnlag = DYNAMIC-FUNCTION('getMvaGrunnlag':U).

  ASSIGN 
    dFsgsum_kr = dMvaGrunnlag + dMvaBelop
    dTB        = DYNAMIC-FUNCTION('getDBKr':U)
    dTB%       = ROUND((dTb / dMvaGrunnlag) * 100,2).
  ASSIGN 
    dPerKunde = ROUND(dFsgsum_kr / iAntKunder,2) NO-ERROR.
  IF dPerKunde = ? THEN
    dPerKunde = 0.
  IF CAN-DO("SE,SVE",cSprak) THEN 
  DO:
    ASSIGN 
      cOverskr[1] = "Nyckeltal"
      cOverskr[2] = "TB i kr"
      cOverskr[3] = "TB i %"
      cOverskr[4] = "K�p/kund"
      cOverskr[5] = "Antal kunder"
      cOverskr[6] = "Varuoms."
      cOverskr[7] = "Tj�nsteoms.".
  END.
  ELSE 
  DO:
    ASSIGN 
      cOverskr[1] = "N�kkeltall"
      cOverskr[2] = "DB kr"
      cOverskr[3] = "DB %"
      cOverskr[4] = "Kj�p/kunde"
      cOverskr[5] = "Andel kampanje %"
      cOverskr[6] = "Vareslag"
      cOverskr[7] = "Tjenester".
  END.
  ASSIGN 
    dColPos[1] = pdf_LeftMargin ("Spdf")
    dColPos[2] = dColPos[1] + 73
    dColPos[3] = dColPos[2] + 73
    dColPos[4] = dColPos[3] + 73
    dColPos[5] = dColPos[4] + 93
    dColPos[6] = dColPos[5] + 73
    dColPos[7] = dColPos[6] + 120
    dColPos[8] = dColPos[7]. /* + 73.*/
  ASSIGN 
    dULStart[1] = pdf_LeftMargin ("Spdf")
    dULStart[2] = dColPos[2] + 4
    dULStart[3] = dColPos[3] + 4
    dULStart[4] = dColPos[4] + 4
    dULStart[5] = dColPos[5] + 4
    dULStart[6] = dColPos[6] + 44
    dULStart[7] = dColPos[7] + 4
    dULStart[8] = dColPos[8] - 44.


  ASSIGN 
    dY = 98.
  /*    dY = dY - 65.*/
  RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold",12).
  RUN pdf_text_xy_dec ("Spdf",cOverskr[1],dColPos[1],dY).
  dY = dY - 4.
  RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica",8).
  RUN pdf_line IN h_PDFinc  ("Spdf", dColPos[1], dY, dColPos[8], dY, 0.5).
  dY = dY - iLineSpace.
  RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold",8).
  RUN pdf_text_xy_dec ("Spdf",cOverskr[2],dColPos[2] - bredd(cOverskr[2]),dY).
  RUN pdf_text_xy_dec ("Spdf",cOverskr[3],dColPos[3] - bredd(cOverskr[3]),dY).
  IF CAN-DO("SE,SVE",cSprak) THEN
    RUN pdf_text_xy_dec ("Spdf",cOverskr[4],dColPos[4] - bredd("K�p/kund"),dY).
  ELSE
    RUN pdf_text_xy_dec ("Spdf",cOverskr[4],dColPos[4] - bredd("Kj�p/kunde"),dY).
  RUN pdf_text_xy_dec ("Spdf",cOverskr[5],dColPos[5] - bredd(cOverskr[5]),dY).
  RUN pdf_text_xy_dec ("Spdf",cOverskr[6],dColPos[6] - bredd(cOverskr[6]),dY).
  RUN pdf_text_xy_dec ("Spdf",cOverskr[7],dColPos[7] - bredd(cOverskr[7]),dY).
  /*     RUN pdf_text_xy_dec ("Spdf",cOverskr[8],dColPos[8] - bredd(cOverskr[8]),dY). */
  dY = dY - 4.
  RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica",8).
  RUN pdf_line IN h_PDFinc  ("Spdf", dULStart[1], dY, dColPos[2], dY, 0.5).
  RUN pdf_line IN h_PDFinc  ("Spdf", dULStart[2], dY, dColPos[3], dY, 0.5).
  RUN pdf_line IN h_PDFinc  ("Spdf", dULStart[3], dY, dColPos[4], dY, 0.5).
  RUN pdf_line IN h_PDFinc  ("Spdf", dULStart[4], dY, dColPos[5], dY, 0.5).
  RUN pdf_line IN h_PDFinc  ("Spdf", dULStart[5], dY, dColPos[6], dY, 0.5).
  RUN pdf_line IN h_PDFinc  ("Spdf", dULStart[6], dY, dColPos[7], dY, 0.5).
  /*     RUN pdf_line IN h_PDFinc  ("Spdf", dULStart[7], dY, dColPos[8], dY, 0.5). */

  dY = dY - iLineSpace. /* 1 F�rs�ljning */
  /* 1 TB */
  cBelopp = TRIM(STRING(dTB,"->>>,>>>,>>9.99")).
  RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPos[2] - bredd(cBelopp),dY).
  /* 2 TB% */
  cBelopp = TRIM(STRING(dTB%,"->>>,>>>,>>9.99")).
  RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPos[3] - bredd(cBelopp),dY).
  /* 3 Per kund */
  cBelopp = TRIM(STRING(dPerKunde,"->>>,>>>,>>9.99")).
  RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPos[4] - bredd(cBelopp),dY).
  /* 4 Antal kunder */
  cBelopp = TRIM(STRING(iAntKunder,"->>>,>>>,>>9.99")).
  RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPos[5] - bredd(cBelopp),dY).
  /* 5 Varuoms */
  cBelopp = TRIM(STRING(dFsgsum_kr - dTjanstoms,"->>>,>>>,>>9.99")).
  RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPos[6] - bredd(cBelopp),dY).
  /* 2 Kamapanjfsg */
  cBelopp = TRIM(STRING(dTjanstoms,"->>>,>>>,>>9.99")).
  RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPos[7] - bredd(cBelopp),dY).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Saml_1_oms) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Saml_1_oms Procedure 
PROCEDURE Saml_1_oms :
  /*------------------------------------------------------------------------------
    Purpose:     
    Parameters:  <none>
    Notes:       
  ------------------------------------------------------------------------------*/
  DEFINE INPUT-OUTPUT PARAMETER dY AS DECIMAL     NO-UNDO.
  DEFINE VARIABLE cOverskr AS CHARACTER EXTENT 3 NO-UNDO.
  DEFINE VARIABLE cLabel   AS CHARACTER EXTENT 10 NO-UNDO.
  DEFINE VARIABLE cTxt     AS CHARACTER NO-UNDO.

  IF CAN-DO("SE,SVE",cSprak) THEN 
  DO:
    ASSIGN 
      cOverskr[1] = "Oms�ttning"
      cOverskr[2] = "Antal"
      cOverskr[3] = "Belopp".
    ASSIGN 
      cLabel[1]  = "Brutto oms�ttning"      
      cLabel[2]  = "Returer"                
      cLabel[3]  = "Generell rabatt"        
      cLabel[4]  = "Kundrabatt"             
      cLabel[5]  = "Personalrabatt"         
      cLabel[6]  = "Medlemsrabatt"          
      cLabel[7]  = "Presentkort ut"       
      cLabel[8]  = "Netto oms�ttning"       
      cLabel[9]  = "Avrundning"             
      cLabel[10] = "Registrerad oms�ttning".
  END.
  ELSE 
  DO:
    ASSIGN 
      cOverskr[1] = "Kasse totalt"
      cOverskr[2] = "Antall"
      cOverskr[3] = "Bel�p".
    ASSIGN 
      cLabel[1]  = "Brutto omsetning"
      cLabel[2]  = "Neg. vare"
      cLabel[3]  = "Gen. rabatt"
      cLabel[4]  = "Kunderabatt"
      cLabel[5]  = "Personalrabatt"
      cLabel[6]  = "Medlemsrabatt"
      cLabel[7]  = "Gavekort ut"
      cLabel[8]  = "Netto omsetning"
      cLabel[9]  = "Avrunding"
      cLabel[10] = "Registrert omsetning".
  END.
  /* RUBRIK */
  RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold",12).
  RUN pdf_text_xy_dec ("Spdf",cOverskr[1],dColPosFR[1],dY).
  dY = dY - 4.
  RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica",8).
  RUN pdf_line IN h_PDFinc  ("Spdf", pdf_LeftMargin ("Spdf"), dY, dColPosFR[3], dY, 0.5).
  dY = dY - iLineSpace.
  /* Kolonnrubrik */
  RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold",8).
  /*     RUN pdf_text_xy_dec ("Spdf",cOverskr[2],dColPosFR[2] - bredd(cOverskr[2]),dY). */
  RUN pdf_text_xy_dec ("Spdf",cOverskr[3],dColPosFR[3] - bredd(cOverskr[3]),dY).
  dY = dY - 4.
  RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica",8).
  /*     RUN pdf_line IN h_PDFinc  ("Spdf", dULstartFR[2], dY, dColPosFR[2], dY, 0.5). */
  RUN pdf_line IN h_PDFinc  ("Spdf", dULstartFR[3], dY, dColPosFR[3], dY, 0.5).
    
  /* 1 Brutto */
  dY = dY - iLineSpace.
  RUN pdf_text_xy_dec ("Spdf",cLabel[1],dColPosFR[1],dY).
  cBelopp = TRIM(STRING(wBruttoOmsetning,"->>>,>>>,>>9.99")).
  RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPosFR[3] - bredd(cBelopp),dY).
    
  /* 2 Returer */
  dY = dY - iLineSpace.
  RUN pdf_text_xy_dec ("Spdf","+",dColPosFR[1] - 5,dY).
  RUN pdf_text_xy_dec ("Spdf",cLabel[2],dColPosFR[1],dY).
  cBelopp = TRIM(STRING((tmpKas_Rap.Retur + tmpKas_Rap.Reklamasjon) * -1,"->>>,>>>,>>9.99")).
  RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPosFR[3] - bredd(cBelopp),dY).
    
  /* 3 generell rabatt */
  dY = dY - iLineSpace.
  RUN pdf_text_xy_dec ("Spdf","-",dColPosFR[1] - 5,dY).
  RUN pdf_text_xy_dec ("Spdf",cLabel[3],dColPosFR[1],dY).
  /*     cBelopp = TRIM(STRING(tmpKas_Rap.AntGenerellRabatt,"->>,>>9")).        */
  /*     RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPosFR[2] - bredd(cBelopp),dY). */
  cBelopp = TRIM(STRING(tmpKas_Rap.GenerellRabatt,"->>>,>>>,>>9.99")).
  RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPosFR[3] - bredd(cBelopp),dY).
    
  /* 4 kundrabatt */
  dY = dY - iLineSpace.
  RUN pdf_text_xy_dec ("Spdf","-",dColPosFR[1] - 5,dY).
  RUN pdf_text_xy_dec ("Spdf",cLabel[4],dColPosFR[1],dY).
  /*     cBelopp = TRIM(STRING(tmpKas_Rap.AntKunderabatt,"->>,>>9")).           */
  /*     RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPosFR[2] - bredd(cBelopp),dY). */
  cBelopp = TRIM(STRING(tmpKas_Rap.Kunderabatt,"->>>,>>>,>>9.99")).
  RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPosFR[3] - bredd(cBelopp),dY).
    
  /* 5 personalrabatt */
  dY = dY - iLineSpace.
  RUN pdf_text_xy_dec ("Spdf","-",dColPosFR[1] - 5,dY).
  RUN pdf_text_xy_dec ("Spdf",cLabel[5],dColPosFR[1],dY).
  /*     cBelopp = TRIM(STRING(tmpKas_Rap.AntPersonalrabatt,"->>,>>9")).        */
  /*     RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPosFR[2] - bredd(cBelopp),dY). */
  cBelopp = TRIM(STRING(tmpKas_Rap.Personalrabatt,"->>>,>>>,>>9.99")).
  RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPosFR[3] - bredd(cBelopp),dY).
    
  /* 6 Medlemsrabatt */
  dY = dY - iLineSpace.
  RUN pdf_text_xy_dec ("Spdf","-",dColPosFR[1] - 5,dY).
  RUN pdf_text_xy_dec ("Spdf",cLabel[6],dColPosFR[1],dY).
  /*     cBelopp = TRIM(STRING(tmpKas_Rap.AntMedlemsrabatt,"->>,>>9")).         */
  /*     RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPosFR[2] - bredd(cBelopp),dY). */
  cBelopp = TRIM(STRING(tmpKas_Rap.Medlemsrabatt,"->>>,>>>,>>9.99")).
  RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPosFR[3] - bredd(cBelopp),dY).
    
  /* 7 Presentkort */
  dY = dY - iLineSpace.
  RUN pdf_text_xy_dec ("Spdf","-",dColPosFR[1] - 5,dY).
  RUN pdf_text_xy_dec ("Spdf",cLabel[7],dColPosFR[1],dY).
  /*     cBelopp = TRIM(STRING(tmpKas_Rap.AntGavekortUt,"->>,>>9")).            */
  /*     RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPosFR[2] - bredd(cBelopp),dY). */
  cBelopp = TRIM(STRING(tmpKas_Rap.GavekortUt,"->>>,>>>,>>9.99")).
  RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPosFR[3] - bredd(cBelopp),dY).
  /* line */
  dY = dY - 4.
  RUN pdf_line IN h_PDFinc  ("Spdf", dColPosFR[1], dY, dColPosFR[3], dY, 0.5).
    
  /* 8 netto oms */
  dY = dY - iLineSpace.
  RUN pdf_text_xy_dec ("Spdf","=",dColPosFR[1] - 5,dY).
  RUN pdf_text_xy_dec ("Spdf",cLabel[8],dColPosFR[1],dY).
  cBelopp = TRIM(STRING(wNettoOmsetning,"->>>,>>>,>>9.99")).
  RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPosFR[3] - bredd(cBelopp),dY).
    
  /* 9 Avrundning */
  dY = dY - iLineSpace.
  RUN pdf_text_xy_dec ("Spdf","+",dColPosFR[1] - 5,dY).
  RUN pdf_text_xy_dec ("Spdf",cLabel[9],dColPosFR[1],dY).
  cBelopp = TRIM(STRING(tmpKas_Rap.Avrunding * -1,"->>>,>>>,>>9.99")).
  RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPosFR[3] - bredd(cBelopp),dY).
  /* line */
  dY = dY - 4.
  RUN pdf_line IN h_PDFinc  ("Spdf", dColPosFR[1], dY, dColPosFR[3], dY, 0.5).
    
  /* 10 Registrerad oms */
  dY = dY - iLineSpace.
  RUN pdf_text_xy_dec ("Spdf","=",dColPosFR[1] - 5,dY).
  RUN pdf_text_xy_dec ("Spdf",cLabel[10],dColPosFR[1],dY).
  cBelopp = TRIM(STRING(wNettoOmsetning - tmpKas_Rap.Avrunding,"->>>,>>>,>>9.99")).
  RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPosFR[3] - bredd(cBelopp),dY).
    
  /* line */
  dY = dY - 4.
  RUN pdf_line IN h_PDFinc  ("Spdf", dColPosFR[1], dY, dColPosFR[3], dY, 0.5).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Saml_1_ovrigt) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Saml_1_ovrigt Procedure 
PROCEDURE Saml_1_ovrigt :
  /*------------------------------------------------------------------------------
    Purpose:     
    Parameters:  <none>
    Notes:       
  ------------------------------------------------------------------------------*/
  DEFINE INPUT-OUTPUT PARAMETER dY AS DECIMAL     NO-UNDO.
  DEFINE VARIABLE cOverskr AS CHARACTER EXTENT 3 NO-UNDO.
  DEFINE VARIABLE cLabel   AS CHARACTER EXTENT 4 NO-UNDO.

  IF CAN-DO("SE,SVE",cSprak) THEN 
  DO:
    ASSIGN 
      cOverskr[1] = "�vrigt"
      cOverskr[2] = "Antal"
      cOverskr[3] = "Belopp".
    ASSIGN 
      cLabel[1] = "Medlemsf�rs�ljning"      
      cLabel[2] = "Inbetalt kunde"                
      cLabel[3] = "V�xel"        
      cLabel[4] = "Returer".
  END.
  ELSE 
  DO:
    ASSIGN 
      cOverskr[1] = "�vrigt"
      cOverskr[2] = "Antall"
      cOverskr[3] = "Bel�p".
    ASSIGN 
      cLabel[1] = "Medlemssalg"      
      cLabel[2] = "Innbetalt kunde"                
      cLabel[3] = "Veksel"        
      cLabel[4] = "Returer".
  END.
  ASSIGN 
    dY = 181.
  /*    dY = dY - 65.*/
  RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold",12).
  RUN pdf_text_xy_dec ("Spdf",cOverskr[1],dColPosFR[1],dY).
  dY = dY - 4.
  RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica",8).
  RUN pdf_line IN h_PDFinc  ("Spdf", dColPosFR[1], dY, dColPosFR[3], dY, 0.5).
  dY = dY - iLineSpace.
  RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold",8).
  RUN pdf_text_xy_dec ("Spdf",cOverskr[2],dColPosFR[2] - bredd(cOverskr[2]),dY).
  RUN pdf_text_xy_dec ("Spdf",cOverskr[3],dColPosFR[3] - bredd(cOverskr[3]),dY).
  dY = dY - 4.
  RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica",8).
  RUN pdf_line IN h_PDFinc  ("Spdf", dULstartFR[2], dY, dColPosFR[2], dY, 0.5).
  RUN pdf_line IN h_PDFinc  ("Spdf", dULstartFR[3], dY, dColPosFR[3], dY, 0.5).
  dY = dY - iLineSpace. /* 1 Medlemssalg */
  RUN pdf_text_xy_dec ("Spdf",cLabel[1],dColPosFR[1],dY).
  cBelopp = TRIM(STRING(tmpKas_Rap.AntMedlemssalg,"->>,>>9")).
  RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPosFR[2] - bredd(cBelopp),dY).
  cBelopp = TRIM(STRING(tmpKas_Rap.Medlemssalg,"->>>,>>>,>>9.99")).
  RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPosFR[3] - bredd(cBelopp),dY).
  dY = dY - iLineSpace. /* 2 Inbetalt kund */
  RUN pdf_text_xy_dec ("Spdf",cLabel[2],dColPosFR[1],dY).
  cBelopp = TRIM(STRING(tmpKas_Rap.AntInnbetaltKunde,"->>,>>9")).
  RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPosFR[2] - bredd(cBelopp),dY).
  cBelopp = TRIM(STRING(tmpKas_Rap.InnbetaltKunde,"->>>,>>>,>>9.99")).
  RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPosFR[3] - bredd(cBelopp),dY).
  dY = dY - iLineSpace. /* 3 Veksel */
  RUN pdf_text_xy_dec ("Spdf",cLabel[3],dColPosFR[1],dY).
  cBelopp = TRIM(STRING(tmpKas_Rap.AntVeksel,"->>,>>9")).
  RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPosFR[2] - bredd(cBelopp),dY).
  cBelopp = TRIM(STRING(tmpKas_Rap.Veksel,"->>>,>>>,>>9.99")).
  RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPosFR[3] - bredd(cBelopp),dY).
    
  dY = dY - iLineSpace. /* 4 Returer */
  RUN pdf_text_xy_dec ("Spdf",cLabel[4],dColPosFR[1],dY).
  cBelopp = TRIM(STRING(tmpKas_Rap.AntRetur,"->>,>>9")).
  RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPosFR[2] - bredd(cBelopp),dY).
  cBelopp = TRIM(STRING(tmpKas_Rap.Retur,"->>>,>>>,>>9.99")).
  RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPosFR[3] - bredd(cBelopp),dY).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Saml_2_betalat) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Saml_2_betalat Procedure 
PROCEDURE Saml_2_betalat :
  /*------------------------------------------------------------------------------
    Purpose:     
    Parameters:  <none>
    Notes:       
  ------------------------------------------------------------------------------*/
  DEFINE INPUT-OUTPUT PARAMETER dY AS DECIMAL   NO-UNDO.
  DEFINE VARIABLE pcOverskr  AS CHARACTER NO-UNDO.
  DEFINE VARIABLE pcLabel    AS CHARACTER NO-UNDO.
  /* DEFINE VARIABLE cLabel    AS CHARACTER EXTENT  NO-UNDO. */
  DEFINE VARIABLE cLabel     AS CHARACTER EXTENT 12 NO-UNDO.
  DEFINE VARIABLE iKontoPara AS INTEGER   EXTENT 12 NO-UNDO.
  DEFINE VARIABLE pcKonto    AS CHARACTER NO-UNDO.
  DEFINE VAR      piLoop     AS INT       NO-UNDO.
  DEFINE VAR      pcBank     AS CHAR      NO-UNDO.
  DEFINE VAR      piInt      AS INT       NO-UNDO.
  DEFINE VARIABLE ii         AS INTEGER   NO-UNDO.
  DEFINE VAR      pcTekst    AS CHAR      NO-UNDO.
  DEFINE VARIABLE cChar      AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cCharTmp   AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cKonto     AS CHARACTER NO-UNDO.


  ASSIGN 
    l52KorrBelop = 0
    l58KorrBelop = 0
    .

  ASSIGN 
    pcKonto = "K 0000  0%" + CHR(1) +
                 "K 3000 24%" + chr(1) +
                 "K 3001 12%" + chr(1) +
                 "K 0000  0%" + chr(1) +
                 "K 0000  0%" + chr(1) +
                 "K 0000  0%" + chr(1) +
                 "K 0000  0%" + chr(1) +
                 "K 0000  0%" + chr(1) +
                 "K 0000  0%" + chr(1) +
                 "K 0000  0%".
  ASSIGN 
    iKontoPara[1]  = 70
    iKontoPara[2]  = 90
    iKontoPara[3]  = 1
    iKontoPara[4]  = 13
    iKontoPara[5]  = 2
    iKontoPara[6]  = 3
    iKontoPara[7]  = 5
    iKontoPara[8]  = 4
    iKontoPara[9]  = 5
    iKontoPara[10] = 10
    iKontoPara[11] = 6
    iKontoPara[12] = 8.

  IF CAN-DO("SE,SVE",cSprak) THEN 
  DO:
    /*     ASSIGN cLabel[1] = */
    /*            cLabel[2] = */
    ASSIGN
      pcOverskr  = "Text" + CHR(1) + 
                  "Konto" + CHR(1) + 
                  "Belopp" + CHR(1) +
                  "Mva"   + CHR(1) +
                  "Belopp u/mva"
      pcLabel    = "F�rs�ljning" + CHR(1) + 
                  "Betalat med:" + CHR(1) + 
                  "Bankkort" + CHR(1) + 
                  "Reservl�sning"
      pcBank     = "D 0000"
      cLabel[1]  = "Kontant"
      cLabel[2]  = "Kassadifferans"
      cLabel[3]  = "Bankkort"
      cLabel[4]  = "Off-line betalning"
      cLabel[5]  = "Tillgodokvitton egna"
      cLabel[6]  = "Tillgodokvitton andras"
      cLabel[7]  = "Presentkort Center"
      cLabel[8]  = "Presentkort egna"
      cLabel[9]  = "Presentkort andras"
      cLabel[10] = "Deposition"
      cLabel[11] = "Kupong 1"
      cLabel[12] = "Kupong 2"
      .
  END.
  ELSE 
  DO:
    ASSIGN
      pcOverskr  = "Tekst" + CHR(1) + 
                  "Konto" + CHR(1) + 
                  "Bel�p" + CHR(1) +
                  "Mva"   + CHR(1) +
                  "Bel�p u/mva"
      pcLabel    = "Varesalg" + CHR(1) + 
                  "Betalt med:" + CHR(1) + 
                  "Bankkort" + CHR(1) + 
                  "Reservel�sning"
      pcBank     = "D 2380"
      cLabel[1]  = "Kontant"
      cLabel[2]  = "Kassedifferens"
      cLabel[3]  = "Bankkort"
      cLabel[4]  = "Reservel�sning"
      cLabel[5]  = "Tilgodeseddler egne"
      cLabel[6]  = "Tilgodeseddler andre"
      cLabel[7]  = "Gavekort Senter"
      cLabel[8]  = "Gavekort egna"
      cLabel[9]  = "Gavekort andre"
      cLabel[10] = "Deponering"
      cLabel[11] = "Kupong 1"
      cLabel[12] = "Kupong 2"
      .
  END.                     
  /*    IF pcRappType <> '99' THEN*/
  DO:
    dY = dY - 24.
    RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold",8).
    RUN pdf_text_xy_dec ("Spdf",ENTRY(2,pcLabel,CHR(1)),dColPosBF[1],dY).
    dY = dY - 4.
    RUN pdf_line IN h_PDFinc  ("Spdf", pdf_LeftMargin ("Spdf"), dY, dColPosBF[2] - 15 ,dY, 0.5).
    /* ghg fr */
  
    /* KONTANTBEHOLDNING */
    dY = dY - 14.
    RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica",8).
    cBelopp = TRIM(STRING(tmpKas_Rap.KontantBeholdning + tmpKas_Rap.SjekkBeholdning,"->>>,>>>,>>9.99")).
    RUN settTekstOgKonto(tmpKas_Rap.Butikk,50,1,DEC(cBelopp),'D',OUTPUT cChar, OUTPUT cKonto).      
    RUN pdf_text_xy_dec ("Spdf",cChar,dColPosBF[1],dY).
    RUN pdf_text_xy_dec ("Spdf",cKonto,dColPosBF[2],dY).
    RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPosBF[3] - bredd(cBelopp),dY).
    RUN pdf_text_xy_dec ("Spdf","  -",dColPosBF[3],dY).
  END.
  DO:
    iLinjeNr = iLinjeNr + 1.
    cBelopp = TRIM(STRING(tmpKas_Rap.KontantBeholdning + tmpKas_Rap.SjekkBeholdning,"->>>,>>>,>>9.99")).
    RUN settTekstOgKonto(tmpKas_Rap.Butikk,50,1,DEC(cBelopp),'D',OUTPUT cChar, OUTPUT cKonto).      
    CREATE ttEksport.
    ASSIGN 
      ttEksport.ButNr       = tmpKas_Rap.Butikk
      ttEksport.ButNamn     = cButikkTxt
      ttEksport.Dato        = pdFraDato
      ttEksport.LinjeNr     = iLinjeNr
      ttEksport.Tekst       = cChar
      ttEksport.KontoPrefix = ENTRY(1,cKonto,' ')
      ttEksport.KontoNr     = INT(ENTRY(2,cKonto,' '))
      ttEksport.Belop       = tmpKas_Rap.KontantBeholdning + tmpKas_Rap.SjekkBeholdning
      .
    RUN opprettBokforingsvisning(2,"BETALT MED",0,0).
    iLinjeNr = iLinjeNr + 1.
    RUN opprettBokforingsvisning(1,"",50,1).
  END.
    
  /* TN 11/10-13 Bank kommer i kort spesifikasjonen
  /* Bank med cashBack presenteres uten Kort. Kort spesifiseres p� de underliggende linjene. */
  */
    
  /* Her spesifiserer vi kreditkortene. */
  KREDITKORTPSES:
  FOR EACH tmpKort_Spes WHERE 
    tmpKort_Spes.Dato      = tmpKas_Rap.Dato AND 
    tmpKort_Spes.Butikk    = tmpKas_Rap.Butikk AND  
    tmpKort_Spes.Kasse     = -9999 AND 
    tmpKort_Spes.KortType >= 0 AND 
    tmpKort_Spes.Z_Nummer  = 0:
        
    DO:  
      dY = dY - 14.
      RUN settTekstOgKonto(tmpKas_Rap.Butikk,52,tmpKort_Spes.KortType,tmpKort_Spes.Belop,'D',OUTPUT cChar, OUTPUT cKonto).      
      RUN pdf_text_xy_dec ("Spdf",(cChar),dColPosBF[1],dY).
      RUN pdf_text_xy_dec ("Spdf",cKonto,dColPosBF[2],dY).
      lBelop = tmpKort_Spes.Belop. 
      FOR EACH BokforingsKorrBilag NO-LOCK WHERE
        BokforingsKorrBilag.BokforingsID = BokforingsBilag.BokforingsID AND
        BokforingsKorrBilag.TTId = 52 AND
        BokforingsKorrBilag.TbId = tmpKort_Spes.KortType:
        /* TN 27/8-20 Her skal ikke iGive med. */
        IF BokforingsKorrBilag.TbId = 38 THEN 
          NEXT.
        ASSIGN
          lBelop       = lBelop + BokforingsKorrBilag.Belop
          l52KorrBelop = l52KorrBelop + BokforingsKorrBilag.Belop
          .
      END.
      cBelopp = TRIM(STRING(STRING(lBelop,"->>>,>>>,>>9.99"))).
          
      RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPosBF[3] - bredd(cBelopp),dY).
      RUN pdf_text_xy_dec ("Spdf","  -",dColPosBF[3],dY).
    END.
    DO:
      iLinjeNr = iLinjeNr + 1.
      RUN settTekstOgKonto(tmpKas_Rap.Butikk,52,tmpKort_Spes.KortType,DEC(cBelopp),'D',OUTPUT cChar, OUTPUT cKonto).      
      CREATE ttEksport.
      ASSIGN 
        ttEksport.ButNr       = tmpKas_Rap.Butikk
        ttEksport.ButNamn     = cButikkTxt
        ttEksport.Dato        = pdFraDato
        ttEksport.LinjeNr     = iLinjeNr
        ttEksport.Tekst       = cChar
        ttEksport.KontoPrefix = ENTRY(1,cKonto,' ')
        ttEksport.KontoNr     = INT(ENTRY(2,cKonto,' '))
        ttEksport.Belop       = DEC(cBelopp)
        .
      RUN opprettBokforingsvisning(1,"",52,tmpKort_Spes.KortType).
    END.
  END. /* KREDITKORTPSES */

  /* BANK */
  DO:
    DO:
      dY = dY - 14.
      RUN settTekstOgKonto(tmpKas_Rap.Butikk,58,1,tmpKas_Rap.Bank,'D',OUTPUT cChar, OUTPUT cKonto).      
      RUN pdf_text_xy_dec ("Spdf",(cChar),dColPosBF[1],dY).
      RUN pdf_text_xy_dec ("Spdf",cKonto,dColPosBF[2],dY).
      lBelop = tmpKas_Rap.Bank + tmpKas_Rap.CashBack. 

      FOR EACH BokforingsKorrBilag NO-LOCK WHERE
        BokforingsKorrBilag.BokforingsID = BokforingsBilag.BokforingsID AND
        BokforingsKorrBilag.TTId = 58 AND
        BokforingsKorrBilag.TbId = 1:
        ASSIGN
          lBelop       = lBelop + BokforingsKorrBilag.Belop
          l58KorrBelop = l58KorrBelop + BokforingsKorrBilag.Belop 
          .
      END.
      cBelopp = TRIM(STRING(STRING(lBelop ,"->>>,>>>,>>9.99"))).

      RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPosBF[3] - bredd(cBelopp),dY).
      RUN pdf_text_xy_dec ("Spdf","  -",dColPosBF[3],dY).
    END.
    DO:
      iLinjeNr = iLinjeNr + 1.
      RUN settTekstOgKonto(tmpKas_Rap.Butikk,58,1,DEC(cBelopp),'D',OUTPUT cChar, OUTPUT cKonto).      
      CREATE ttEksport.
      ASSIGN 
        ttEksport.ButNr       = tmpKas_Rap.Butikk
        ttEksport.ButNamn     = cButikkTxt
        ttEksport.Dato        = pdFraDato
        ttEksport.LinjeNr     = iLinjeNr
        ttEksport.Tekst       = cChar
        ttEksport.KontoPrefix = ENTRY(1,cKonto,' ')
        ttEksport.KontoNr     = INT(ENTRY(2,cKonto,' '))
        ttEksport.Belop       = DEC(cBelopp)
        .
      RUN opprettBokforingsvisning(1,"",58,1).
    END.
  END.
    
  /* RESERVEL�SNING */
  IF tmpKas_Rap.Reservelosning <> 0 THEN
  DO:
    dY = dY - 14.
    RUN settTekstOgKonto(tmpKas_Rap.Butikk,59,1,tmpKas_Rap.Reservelosning,'D',OUTPUT cChar, OUTPUT cKonto). 
    cChar = 'Reservel�sning(59/1)'.     
    RUN pdf_text_xy_dec ("Spdf",(cChar),dColPosBF[1],dY).
    RUN pdf_text_xy_dec ("Spdf",cKonto,dColPosBF[2],dY).
    cBelopp = TRIM(STRING(STRING(Reservelosning,"->>>,>>>,>>9.99"))).
    RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPosBF[3] - bredd(cBelopp),dY).
    RUN pdf_text_xy_dec ("Spdf","  -",dColPosBF[3],dY).
    iLinjeNr = iLinjeNr + 1.
    RUN settTekstOgKonto(tmpKas_Rap.Butikk,59,1,tmpKas_Rap.Reservelosning,'D',OUTPUT cChar, OUTPUT cKonto). 
    cChar = 'Reservel�sning(59/1)'.     
    CREATE ttEksport.
    ASSIGN 
      ttEksport.ButNr       = tmpKas_Rap.Butikk
      ttEksport.ButNamn     = cButikkTxt
      ttEksport.Dato        = pdFraDato
      ttEksport.LinjeNr     = iLinjeNr
      ttEksport.Tekst       = cChar
      ttEksport.KontoPrefix = ENTRY(1,cKonto,' ')
      ttEksport.KontoNr     = INT(ENTRY(2,cKonto,' '))
      ttEksport.Belop       = Reservelosning
      .
    RUN opprettBokforingsvisning(1,"",59,0).
  END.
    
  /*    IF pcRappType <> '99' THEN*/
  DO:
    dY = dY - iLineSpace. /* 5 tilgode inn */
    RUN settTekstOgKonto(tmpKas_Rap.Butikk,66,1,tmpKas_Rap.TilgodeInn,'D',OUTPUT cChar, OUTPUT cKonto). 
    RUN pdf_text_xy_dec ("Spdf",cChar,dColPosBF[1],dY).
    RUN pdf_text_xy_dec ("Spdf",cKonto,dColPosBF[2],dY).
    cBelopp = TRIM(STRING(STRING(TilgodeInn,"->>>,>>>,>>9.99"))).
    RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPosBF[3] - bredd(cBelopp),dY).
    RUN pdf_text_xy_dec ("Spdf","  -",dColPosBF[3],dY).
  END.
  DO:
    iLinjeNr = iLinjeNr + 1.
    RUN settTekstOgKonto(tmpKas_Rap.Butikk,66,1,tmpKas_Rap.TilgodeInn,'D',OUTPUT cChar, OUTPUT cKonto). 
    CREATE ttEksport.
    ASSIGN 
      ttEksport.ButNr       = tmpKas_Rap.Butikk
      ttEksport.ButNamn     = cButikkTxt
      ttEksport.Dato        = pdFraDato
      ttEksport.LinjeNr     = iLinjeNr
      ttEksport.Tekst       = cChar
      ttEksport.KontoPrefix = ENTRY(1,cKonto,' ')
      ttEksport.KontoNr     = INT(ENTRY(2,cKonto,' '))
      ttEksport.Belop       = TilgodeInn
      .
    RUN opprettBokforingsvisning(1,"",66,0).
  END.
    
  /*    IF pcRappType <> '99' THEN*/
  DO:
    dY = dY - iLineSpace. /* 6 tilgode inn andre */
    RUN settTekstOgKonto(tmpKas_Rap.Butikk,900,16,tmpKas_Rap.TilgodeAndre,'D',OUTPUT cChar, OUTPUT cKonto). 
    RUN pdf_text_xy_dec ("Spdf",(cChar),dColPosBF[1],dY).
    RUN pdf_text_xy_dec ("Spdf",cKonto,dColPosBF[2],dY).
    cBelopp = TRIM(STRING(STRING(TilgodeAndre,"->>>,>>>,>>9.99"))).
    RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPosBF[3] - bredd(cBelopp),dY).
    RUN pdf_text_xy_dec ("Spdf","  -",dColPosBF[3],dY).
  END.
  DO:
    iLinjeNr = iLinjeNr + 1.
    RUN settTekstOgKonto(tmpKas_Rap.Butikk,900,16,tmpKas_Rap.TilgodeAndre,'D',OUTPUT cChar, OUTPUT cKonto). 
    CREATE ttEksport.
    ASSIGN 
      ttEksport.ButNr       = tmpKas_Rap.Butikk
      ttEksport.ButNamn     = cButikkTxt
      ttEksport.Dato        = pdFraDato
      ttEksport.LinjeNr     = iLinjeNr
      ttEksport.Tekst       = cChar
      ttEksport.KontoPrefix = ENTRY(1,cKonto,' ')
      ttEksport.KontoNr     = INT(ENTRY(2,cKonto,' '))
      ttEksport.Belop       = TilgodeAndre
      .
    RUN opprettBokforingsvisning(1,"",900,16).
  END.
 
  /*    IF pcRappType <> '99' THEN*/
  DO:
    dY = dY - iLineSpace. /* 8 Gavekort in */
    RUN settTekstOgKonto(tmpKas_Rap.Butikk,53,1,tmpKas_Rap.GavekortInn,'D',OUTPUT cChar, OUTPUT cKonto). 
    RUN pdf_text_xy_dec ("Spdf",(cChar),dColPosBF[1],dY).
    RUN pdf_text_xy_dec ("Spdf",cKonto,dColPosBF[2],dY).
    cBelopp = TRIM(STRING(STRING(tmpKas_Rap.GavekortInn - tmpKas_Rap.GavekortAndreInn,"->>>,>>>,>>9.99"))).
    RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPosBF[3] - bredd(cBelopp),dY).
    RUN pdf_text_xy_dec ("Spdf","  -",dColPosBF[3],dY).
  END.
  DO:
    iLinjeNr = iLinjeNr + 1.
    RUN settTekstOgKonto(tmpKas_Rap.Butikk,53,1,tmpKas_Rap.GavekortInn,'D',OUTPUT cChar, OUTPUT cKonto). 
    CREATE ttEksport.
    ASSIGN 
      ttEksport.ButNr       = tmpKas_Rap.Butikk
      ttEksport.ButNamn     = cButikkTxt
      ttEksport.Dato        = pdFraDato
      ttEksport.LinjeNr     = iLinjeNr
      ttEksport.Tekst       = cChar
      ttEksport.KontoPrefix = ENTRY(1,cKonto,' ')
      ttEksport.KontoNr     = INT(ENTRY(2,cKonto,' '))
      ttEksport.Belop       = tmpKas_Rap.GavekortInn - tmpKas_Rap.GavekortAndreInn
      .
    RUN opprettBokforingsvisning(1,"",53,1).
  END.
    
  dY = dY - iLineSpace. /* 9 Gavekort andre in */
  RUN settTekstOgKonto(tmpKas_Rap.Butikk,900,19,tmpKas_Rap.GavekortAndreInn,'D',OUTPUT cChar, OUTPUT cKonto).
  IF tmpKas_Rap.GavekortAndreInn <> 0 AND cKonto MATCHES "*0000" THEN 
  DO:
    cCharTmp = cChar.
    RUN settTekstOgKonto(tmpKas_Rap.Butikk,53,1,tmpKas_Rap.GavekortAndreInn,'D',OUTPUT cChar, OUTPUT cKonto).
    cChar = cCharTmp.
  END.


  IF tmpKas_Rap.GavekortAndreInn <> 0 THEN
  DO:
    RUN pdf_text_xy_dec ("Spdf",(cChar),dColPosBF[1],dY).
    RUN pdf_text_xy_dec ("Spdf",cKonto,dColPosBF[2],dY).
    cBelopp = TRIM(STRING(STRING(tmpKas_Rap.GavekortAndreInn,"->>>,>>>,>>9.99"))).
    RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPosBF[3] - bredd(cBelopp),dY).
    RUN pdf_text_xy_dec ("Spdf","  -",dColPosBF[3],dY).
    iLinjeNr = iLinjeNr + 1.
    CREATE ttEksport.
    ASSIGN 
      ttEksport.ButNr       = tmpKas_Rap.Butikk
      ttEksport.ButNamn     = cButikkTxt
      ttEksport.Dato        = pdFraDato
      ttEksport.LinjeNr     = iLinjeNr
      ttEksport.Tekst       = cChar
      ttEksport.KontoPrefix = ENTRY(1,cKonto,' ')
      ttEksport.KontoNr     = INT(ENTRY(2,cKonto,' '))
      ttEksport.Belop       = tmpKas_Rap.GavekortAndreInn
      .
    RUN opprettBokforingsvisning(1,"",900,0).
  END.
        
  IF tmpKas_Rap.LayAway_Inn <> 0 THEN
  DO:
    dY = dY - iLineSpace. /* 10 deponering in */
    RUN settTekstOgKonto(tmpKas_Rap.Butikk,72,1,tmpKas_Rap.LayAway_Inn,'D',OUTPUT cChar, OUTPUT cKonto). 
    RUN pdf_text_xy_dec ("Spdf",(cChar),dColPosBF[1],dY).
    RUN pdf_text_xy_dec ("Spdf",cKonto,dColPosBF[2],dY).
    cBelopp = TRIM(STRING(STRING(tmpKas_Rap.LayAway_Inn,"->>>,>>>,>>9.99"))).
    RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPosBF[3] - bredd(cBelopp),dY).
    RUN pdf_text_xy_dec ("Spdf","  -",dColPosBF[3],dY).
    iLinjeNr = iLinjeNr + 1.
    RUN settTekstOgKonto(tmpKas_Rap.Butikk,72,1,tmpKas_Rap.LayAway_Inn,'D',OUTPUT cChar, OUTPUT cKonto). 
    CREATE ttEksport.
    ASSIGN 
      ttEksport.ButNr       = tmpKas_Rap.Butikk
      ttEksport.ButNamn     = cButikkTxt
      ttEksport.Dato        = pdFraDato
      ttEksport.LinjeNr     = iLinjeNr
      ttEksport.Tekst       = cChar
      ttEksport.KontoPrefix = ENTRY(1,cKonto,' ')
      ttEksport.KontoNr     = INT(ENTRY(2,cKonto,' '))
      ttEksport.Belop       = tmpKas_Rap.LayAway_Inn
      .
    RUN opprettBokforingsvisning(1,"",72,0).
  END.
        
  IF tmpKas_Rap.kupong1 <> 0 THEN
  DO:
    dY = dY - iLineSpace. /* 11 Kupong 1 */
    RUN settTekstOgKonto(tmpKas_Rap.Butikk,56,1,tmpKas_Rap.kupong1,'D',OUTPUT cChar, OUTPUT cKonto). 
    RUN pdf_text_xy_dec ("Spdf",(cChar),dColPosBF[1],dY).
    RUN pdf_text_xy_dec ("Spdf",cKonto,dColPosBF[2],dY).
    cBelopp = TRIM(STRING(STRING(tmpKas_Rap.kupong1,"->>>,>>>,>>9.99"))).
    RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPosBF[3] - bredd(cBelopp),dY).
    RUN pdf_text_xy_dec ("Spdf","  -",dColPosBF[3],dY).
    iLinjeNr = iLinjeNr + 1.
    RUN settTekstOgKonto(tmpKas_Rap.Butikk,56,1,tmpKas_Rap.kupong1,'D',OUTPUT cChar, OUTPUT cKonto). 
    CREATE ttEksport.
    ASSIGN 
      ttEksport.ButNr       = tmpKas_Rap.Butikk
      ttEksport.ButNamn     = cButikkTxt
      ttEksport.Dato        = pdFraDato
      ttEksport.LinjeNr     = iLinjeNr
      ttEksport.Tekst       = cChar
      ttEksport.KontoPrefix = ENTRY(1,cKonto,' ')
      ttEksport.KontoNr     = INT(ENTRY(2,cKonto,' '))
      ttEksport.Belop       = tmpKas_Rap.kupong1
      .
    RUN opprettBokforingsvisning(1,"",56,0).
  END.
        
  IF tmpKas_Rap.kupong2 <> 0 THEN
  DO:
    dY = dY - iLineSpace. /* 12 Kupong 12 */
    RUN settTekstOgKonto(tmpKas_Rap.Butikk,71,1,tmpKas_Rap.kupong2,'D',OUTPUT cChar, OUTPUT cKonto). 
    RUN pdf_text_xy_dec ("Spdf",(cChar),dColPosBF[1],dY).
    RUN pdf_text_xy_dec ("Spdf",cKonto,dColPosBF[2],dY).
    cBelopp = TRIM(STRING(STRING(tmpKas_Rap.kupong2,"->>>,>>>,>>9.99"))).
    RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPosBF[3] - bredd(cBelopp),dY).
    RUN pdf_text_xy_dec ("Spdf","  -",dColPosBF[3],dY).
    iLinjeNr = iLinjeNr + 1.
    RUN settTekstOgKonto(tmpKas_Rap.Butikk,71,1,tmpKas_Rap.kupong2,'D',OUTPUT cChar, OUTPUT cKonto). 
    CREATE ttEksport.
    ASSIGN 
      ttEksport.ButNr       = tmpKas_Rap.Butikk
      ttEksport.ButNamn     = cButikkTxt
      ttEksport.Dato        = pdFraDato
      ttEksport.LinjeNr     = iLinjeNr
      ttEksport.Tekst       = cChar
      ttEksport.KontoPrefix = ENTRY(1,cKonto,' ')
      ttEksport.KontoNr     = INT(ENTRY(2,cKonto,' '))
      ttEksport.Belop       = tmpKas_Rap.kupong2
      .
    RUN opprettBokforingsvisning(1,"",71,0).
  END.
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Saml_2_BilagSpes) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Saml_2_BilagSpes Procedure 
PROCEDURE Saml_2_BilagSpes :
  /*------------------------------------------------------------------------------
    Purpose:     
    Parameters:  <none>
    Notes:     
  
      END.
  
  ------------------------------------------------------------------------------*/
  DEFINE INPUT-OUTPUT PARAMETER dY AS DECIMAL     NO-UNDO.
  DEFINE VARIABLE pcOverskr  AS CHARACTER NO-UNDO.
  DEFINE VARIABLE pcLabel    AS CHARACTER NO-UNDO.
  DEFINE VARIABLE pcKonto    AS CHARACTER NO-UNDO.
  DEFINE VARIABLE plSum      AS DEC       NO-UNDO.
  DEFINE VARIABLE lKun1Dag   AS LOGICAL   NO-UNDO.
  DEFINE VARIABLE dLeftCol   AS DECIMAL   INIT 385 NO-UNDO.
  DEFINE VARIABLE dLeftCol2  AS DECIMAL   INIT 420 NO-UNDO.
  DEFINE VARIABLE dLineStart AS DECIMAL   INIT 484 NO-UNDO.

  DO:
    /* BILAG ??????????????? */
    dY = dY - 28.

    RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica",8).
    NON_Sale_Spes:
    FOR EACH NON_Sale_Spes NO-LOCK WHERE
      NON_Sale_Spes.Butikk    = piButNr AND
      NON_Sale_Spes.kasse     > 0 AND 
      NON_Sale_Spes.Dato     >= pdFraDato AND
      NON_Sale_Spes.Dato     <= pdTilDato AND
      NON_Sale_Spes.Non_Sale_Type = 2
      BREAK BY NON_Sale_Spes.Butikk:
      /*         IF FIRST-OF(NON_Sale_Spes.butikk) THEN                                      */
      /*         DO:                                                                         */
      /*             RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold",8).              */
      /*             dY = dY - 14.                                                           */
      /*             RUN pdf_text_xy_dec ("Spdf",'NonSale (Negativ)',dLeftCol,dY).           */
      /*             dY = dY - 4.                                                            */
      /*             RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica",8).                   */
      /*             RUN pdf_line IN h_PDFinc  ("Spdf", dLeftCol, dY, dColPosBF[5],dY, 0.5). */
      /*         END.                                                                        */
      FIND StrekKode NO-LOCK WHERE
        StrekKode.Kode = NON_Sale_Spes.Kode NO-ERROR.
      IF AVAILABLE StrekKode THEN FIND ArtBas OF StrekKode NO-LOCK NO-ERROR.  
      dY = dY - 14.
      RUN pdf_text_xy_dec ("Spdf",(IF AVAILABLE ArtBas THEN SUBSTRING(ArtBas.Beskr,1,10) ELSE ''),dLeftCol,dY).
      RUN pdf_text_xy_dec ("Spdf",TRIM(SUBSTRING(NON_Sale_Spes.Kode,1,13)),dColPosBF[4] - bredd(TRIM(SUBSTRING(NON_Sale_Spes.Kode,1,13))),dY).
      cBelopp = TRIM(STRING(NON_Sale_Spes.NON_SaleVerdi,"->>,>>9.99")).
      RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPosBF[5] - bredd(cBelopp),dY).
    END. /* NON_SALE_SPES */




    RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica",8).
    IF tmpKas_Rap.AntRetur <> 0 THEN
    ANTALL_RETURER:
    DO:
      dY = dY - 14.
      cBelopp = " (" + TRIM(STRING(tmpkas_rap.AntRetur,"->>>>9")) + ")".
      RUN pdf_text_xy_dec ("Spdf","Returer" + cBelopp,dLeftCol,dY).
      /*           RUN pdf_text_xy_dec ("Spdf",STRING(tmpKas_Rap.AntRetur,"->>,>>9"),dLeftCol2,dY). */
      cBelopp = TRIM(STRING(tmpKas_Rap.Retur,"->>,>>9.99")).
      RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPosBF[5] - bredd(cBelopp),dY).
    END. /* ANTALL_RETURER */
    IF tmpKas_Rap.AntReklamasjon <> 0 THEN
    ANTALL_REKLAMASJONER:
    DO:
      dY = dY - 14.
      cBelopp = " (" + TRIM(STRING(tmpKas_Rap.AntReklamasjon,"->>,>>9")) + ")".
      RUN pdf_text_xy_dec ("Spdf",IF CAN-DO("SE,SVE",cSprak) THEN "Reklamationer" + cBelopp ELSE "Reklamasjoner" + cBelopp,dLeftCol,dY).
      /*           RUN pdf_text_xy_dec ("Spdf",STRING(tmpKas_Rap.AntReklamasjon,"->>,>>9"),dLeftCol2,dY). */
      cBelopp = TRIM(STRING(tmpKas_Rap.Reklamasjon,"->>,>>9.99")).
      RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPosBF[5] - bredd(cBelopp),dY).
    END. /* ANTALL_REKLAMASJONER */
    IF tmpKas_Rap.AntallUtbetBonger <> 0 THEN
    ANTALL_BONGER_MED_UTBETALING:
    DO:
      dY = dY - 14.
      cBelopp = " (" + TRIM(STRING(tmpKas_Rap.AntallUtbetBonger,"->>,>>9")) + ")".
      RUN pdf_text_xy_dec ("Spdf",IF CAN-DO("SE,SVE",cSprak) THEN "Kvitton m utbet" + cBelopp ELSE "Bonger med utbetaling" + cBelopp,dLeftCol,dY).
      /*           RUN pdf_text_xy_dec ("Spdf",STRING(tmpKas_Rap.AntallUtbetBonger,"->>,>>9"),dLeftCol2,dY). */
      cBelopp = TRIM(STRING(tmpKas_Rap.VerdiUtbetBonger,"->>,>>9.99")).
      RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPosBF[5] - bredd(cBelopp),dY).
    END. /* ANTALL_BONGER_MED_UTBETALING */

  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Saml_2_Dagensstrom) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Saml_2_Dagensstrom Procedure 
PROCEDURE Saml_2_Dagensstrom :
  /*------------------------------------------------------------------------------
    Purpose:     
    Parameters:  <none>
    Notes:       
  ------------------------------------------------------------------------------*/
  DEFINE INPUT-OUTPUT  PARAMETER dY AS DECIMAL     NO-UNDO.
  DEFINE VARIABLE cLabel  AS CHARACTER EXTENT 5 NO-UNDO.
  DEFINE VARIABLE plSum   AS DEC       NO-UNDO.
  DEFINE VARIABLE pcTekst AS CHAR      NO-UNDO.
  DEFINE VARIABLE piLoop  AS INT       NO-UNDO.
  DEFINE VARIABLE cChar   AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cKonto  AS CHARACTER NO-UNDO.

  IF CAN-DO("SE,SVE",cSprak) THEN 
  DO:
    ASSIGN 
      cLabel[1] = "Dagens kontantstr�m"
      cLabel[2] = "Kassa vid dagens b�rjan"
      cLabel[3] = "Kassa vid dagens slut, uppr�knat"
      cLabel[4] = "�ndring kassa"
      cLabel[5] = "Differens".
  END.
  ELSE 
  DO:
    ASSIGN 
      cLabel[1] = "Dagens kontantstr�m"
      cLabel[2] = "Opptalt morgen"
      cLabel[3] = "Opptalt kveld"
      cLabel[4] = "Endring kasse"
      cLabel[5] = "Differanse".
  END.

  dY = dY - 24.
  RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold",8).
  RUN pdf_text_xy_dec ("Spdf",cLabel[1],dColPosBF[1],dY).
  cBelopp = TRIM(STRING(lDagensKontStrom,"->>>,>>>,>>9.99")).
  RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPosBF[3] - bredd(cBelopp),dY).
  RUN pdf_text_xy_dec ("Spdf","  A",dColPosBF[3],dY).

  dY = dY - 4.
  RUN pdf_line IN h_PDFinc  ("Spdf", pdf_LeftMargin ("Spdf"), dY, dColPosBF[2] - 15 ,dY, 0.5).
  RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica",8).

  dY = dY - 24.
  RUN pdf_text_xy_dec ("Spdf",cLabel[2],dColPosBF[1],dY).
  /*    cBelopp = TRIM(STRING(tmpKas_Rap.OpptaltInnVeksel + lExOpptaltMorgen,"->>>,>>>,>>9.99")).*/
  cBelopp = TRIM(STRING(lOpptaltMorgen + lExOpptaltMorgen,"->>>,>>>,>>9.99")).
  RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPosBF[3] - bredd(cBelopp),dY).
  RUN pdf_text_xy_dec ("Spdf","  B",dColPosBF[3],dY).

  DO:
    iLinjeNr = iLinjeNr + 1.
    CREATE ttEksport.
    ASSIGN 
      ttEksport.ButNr       = tmpKas_Rap.Butikk
      ttEksport.ButNamn     = cButikkTxt
      ttEksport.Dato        = pdFraDato
      ttEksport.LinjeNr     = iLinjeNr
      ttEksport.Tekst       = cLabel[1]
      ttEksport.KontoPrefix = ''
      ttEksport.KontoNr     = 0
      ttEksport.Belop       = lDagensKontStrom
      .
    RUN opprettBokforingsvisning(2,"KONTANTSTR�M",0,0).
    iLinjeNr = iLinjeNr + 1.
    RUN opprettBokforingsvisning(1,"Dagens kontantstr�m",0,0).
      
    iLinjeNr = iLinjeNr + 1.
    CREATE ttEksport.
    ASSIGN 
      ttEksport.ButNr       = tmpKas_Rap.Butikk
      ttEksport.ButNamn     = cButikkTxt
      ttEksport.Dato        = pdFraDato
      ttEksport.LinjeNr     = iLinjeNr
      ttEksport.Tekst       = cLabel[2]
      ttEksport.KontoPrefix = ''
      ttEksport.KontoNr     = 0
      /*        ttEksport.Belop       = tmpKas_Rap.OpptaltInnVeksel + lExOpptaltMorgen*/
      ttEksport.Belop       = lOpptaltMorgen + lExOpptaltMorgen        
      .
    RUN opprettBokforingsvisning(1,"",900,28).
  END. 
    
  /* Kasse ved dagens slutt */
  /*    IF pcRappType <> '99' THEN*/
  DO:
    /* Ekstra Opptalt kveld. */
    dY = dY - 14.
    RUN pdf_text_xy_dec ("Spdf",cLabel[3],dColPosBF[1],dY).
    cBelopp = TRIM(STRING(lKasseSlutt - lExDropp,"->>>,>>>,>>9.99")).
    RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPosBF[3] - bredd(cBelopp),dY).
    RUN pdf_text_xy_dec ("Spdf","  C",dColPosBF[3],dY).
  END.
  DO:
    iLinjeNr = iLinjeNr + 1.
    CREATE ttEksport.
    ASSIGN 
      ttEksport.ButNr       = tmpKas_Rap.Butikk
      ttEksport.ButNamn     = cButikkTxt
      ttEksport.Dato        = pdFraDato
      ttEksport.LinjeNr     = iLinjeNr
      ttEksport.Tekst       = cLabel[3]
      ttEksport.KontoPrefix = ''
      ttEksport.KontoNr     = 0
      ttEksport.Belop       = lKasseSlutt - lExDropp
      .
    RUN opprettBokforingsvisning(1,"",900,29).
  END. 
    
  /* Fast veksel */
  FIND bufbutiker NO-LOCK WHERE 
    bufButiker.butik = tmpKas_Rap.Butikk NO-ERROR.
  IF AVAILABLE bufButiker AND bufButiker.StdVeksel > 0 THEN 
  VEKSEL:
  DO:
    dY = dY - 14.
    cBelopp = TRIM(STRING(STRING(bufbutiker.StdVeksel ,"->>>,>>>,>>9.99"))).
    /*      RUN settTekstOgKonto(tmpKas_Rap.Butikk,70,1,bufbutiker.StdVeksel,'K',OUTPUT cChar, OUTPUT cKonto).*/
    RUN settTekstOgKonto(tmpKas_Rap.Butikk,900,30,bufbutiker.StdVeksel,'K',OUTPUT cChar, OUTPUT cKonto).
    cChar = 'Fast veksel'. 
    RUN pdf_text_xy_dec ("Spdf",(cChar),dColPosBF[1],dY).
    RUN pdf_text_xy_dec ("Spdf",cKonto,dColPosBF[2],dY).
    RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPosBF[3] - bredd(cBelopp),dY).
    /*      RUN pdf_text_xy_dec ("Spdf","  -",dColPosBF[3],dY).*/
    iLinjeNr = iLinjeNr + 1.
    CREATE ttEksport.
    ASSIGN 
      ttEksport.ButNr       = tmpKas_Rap.Butikk
      ttEksport.ButNamn     = cButikkTxt
      ttEksport.Dato        = pdFraDato
      ttEksport.LinjeNr     = iLinjeNr
      ttEksport.Tekst       = cChar
      ttEksport.KontoPrefix = ''
      ttEksport.KontoNr     = DEC(ENTRY(2,cKonto,' '))
      ttEksport.Belop       = bufbutiker.StdVeksel
      .
    RUN opprettBokforingsvisning(1,cChar,900,30).


    /* Legger ut en linje med det som er lagt i pose for � f� korrigert for mye/lite veksel. */
    dY = dY - 14.
    cBelopp = TRIM(STRING(STRING(lLagtIPose ,"->>>,>>>,>>9.99"))).
    /*      RUN settTekstOgKonto(tmpKas_Rap.Butikk,70,1,bufbutiker.StdVeksel,'K',OUTPUT cChar, OUTPUT cKonto).*/
    RUN settTekstOgKonto(tmpKas_Rap.Butikk,900,30,bufbutiker.StdVeksel,'K',OUTPUT cChar, OUTPUT cKonto).
    cChar = 'Lagt i pose'. 
    RUN pdf_text_xy_dec ("Spdf",(cChar),dColPosBF[1],dY).
    RUN pdf_text_xy_dec ("Spdf",cKonto,dColPosBF[2],dY).
    RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPosBF[3] - bredd(cBelopp),dY).
    /*      RUN pdf_text_xy_dec ("Spdf","  -",dColPosBF[3],dY).*/
    iLinjeNr = iLinjeNr + 1.
    CREATE ttEksport.
    ASSIGN 
      ttEksport.ButNr       = tmpKas_Rap.Butikk
      ttEksport.ButNamn     = cButikkTxt
      ttEksport.Dato        = pdFraDato
      ttEksport.LinjeNr     = iLinjeNr
      ttEksport.Tekst       = cChar
      ttEksport.KontoPrefix = ''
      ttEksport.KontoNr     = DEC(ENTRY(2,cKonto,' '))
      ttEksport.Belop       = lLagtIPose
      .
    RUN opprettBokforingsvisning(1,cChar,900,30).
    /* slutt vekselkorr. */ 

    /*MESSAGE 'Test2'                             */
    /*'lKasseSlutt:' lKasseSlutt                  */
    /*'lExDropp:' lExDropp                        */
    /*'bufbutiker.StdVeksel:' bufbutiker.StdVeksel*/
    /*'lExOpptaltKveld:' lExOpptaltKveld          */
    /*'lLagtIPose:' lLagtIPose                    */
    /*VIEW-AS ALERT-BOX.                          */

    dY = dY - 14.
    cBelopp = TRIM(STRING(STRING(lKasseSlutt - lExDropp - bufbutiker.StdVeksel - lLagtIPose,"->>>,>>>,>>9.99"))).
    /*      RUN settTekstOgKonto(tmpKas_Rap.Butikk,70,1,bufbutiker.StdVeksel,'K',OUTPUT cChar, OUTPUT cKonto).*/
    RUN settTekstOgKonto(tmpKas_Rap.Butikk,900,30,bufbutiker.StdVeksel,'K',OUTPUT cChar, OUTPUT cKonto).
    cChar = 'Veksel for mye/lite'. 
    RUN pdf_text_xy_dec ("Spdf",(cChar),dColPosBF[1],dY).
    RUN pdf_text_xy_dec ("Spdf",cKonto,dColPosBF[2],dY).
    RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPosBF[3] - bredd(cBelopp),dY).
    /*      RUN pdf_text_xy_dec ("Spdf","  -",dColPosBF[3],dY).*/
    iLinjeNr = iLinjeNr + 1.
    CREATE ttEksport.
    ASSIGN 
      ttEksport.ButNr       = tmpKas_Rap.Butikk
      ttEksport.ButNamn     = cButikkTxt
      ttEksport.Dato        = pdFraDato
      ttEksport.LinjeNr     = iLinjeNr
      ttEksport.Tekst       = cChar
      ttEksport.KontoPrefix = ''
      ttEksport.KontoNr     = DEC(ENTRY(2,cKonto,' '))
      ttEksport.Belop       = lKasseSlutt - lExDropp - bufbutiker.StdVeksel - lLagtIPose
      .
    RUN opprettBokforingsvisning(1,cChar,900,30).
      
  END. /* VEKSEL */.
    
  /* Endring kasse */
  /*    IF pcRappType <> '99' THEN*/
  DO:
    RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold",8).
    dY = dY - 24.
    RUN settTekstOgKonto(tmpKas_Rap.Butikk,900,26,lKasseEndring,'D',OUTPUT cChar, OUTPUT cKonto). 
    RUN pdf_text_xy_dec ("Spdf",(cChar),dColPosBF[1],dY).
    RUN pdf_text_xy_dec ("Spdf",cKonto,dColPosBF[2],dY).
    cBelopp = TRIM(STRING(STRING(lKasseEndring - lExDropp,"->>>,>>>,>>9.99"))).
    RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPosBF[3] - bredd(cBelopp),dY).
    RUN pdf_text_xy_dec ("Spdf","  -",dColPosBF[3],dY).
  END.
  DO:
    iLinjeNr = iLinjeNr + 1.
    RUN settTekstOgKonto(tmpKas_Rap.Butikk,900,26,lKasseEndring,'D',OUTPUT cChar, OUTPUT cKonto). 
    CREATE ttEksport.
    ASSIGN 
      ttEksport.ButNr       = tmpKas_Rap.Butikk
      ttEksport.ButNamn     = cButikkTxt
      ttEksport.Dato        = pdFraDato
      ttEksport.LinjeNr     = iLinjeNr
      ttEksport.Tekst       = cChar
      ttEksport.KontoPrefix = ''
      ttEksport.KontoNr     = INT(ENTRY(2,cKonto,' '))
      ttEksport.Belop       = lKasseEndring - lExDropp
      .
    RUN opprettBokforingsvisning(1,"",900,26).  
  END. 
    
  /* Differanse */
  /*    IF pcRappType <> '99' THEN*/
  DO:
    dY = dY - 14.
    RUN settTekstOgKonto(tmpKas_Rap.Butikk,900,27,lKasseDiff + l52KorrBelop + l58KorrBelop,'D',OUTPUT cChar, OUTPUT cKonto). 
    RUN pdf_text_xy_dec ("Spdf",(cChar),dColPosBF[1],dY).
    RUN pdf_text_xy_dec ("Spdf",cKonto,dColPosBF[2],dY).
    cBelopp = TRIM(STRING(STRING(lKasseDiff + l52KorrBelop + l58KorrBelop,"->>>,>>>,>>9.99"))).
    RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPosBF[3] - bredd(cBelopp),dY).
    RUN pdf_text_xy_dec ("Spdf","  -",dColPosBF[3],dY).
  END.
  DO:
    iLinjeNr = iLinjeNr + 1.
    CREATE ttEksport.
    ASSIGN 
      ttEksport.ButNr       = tmpKas_Rap.Butikk
      ttEksport.ButNamn     = cButikkTxt
      ttEksport.Dato        = pdFraDato
      ttEksport.LinjeNr     = iLinjeNr
      ttEksport.Tekst       = cChar
      ttEksport.KontoPrefix = ''
      ttEksport.KontoNr     = INT(ENTRY(2,cKonto,' '))
      ttEksport.Belop       = lKasseDiff + l52KorrBelop + l58KorrBelop
      .
    RUN opprettBokforingsvisning(1,"",900,27).
  END. 
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Saml_2_Diverse) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Saml_2_Diverse Procedure 
PROCEDURE Saml_2_Diverse :
  /*------------------------------------------------------------------------------
    Purpose:     
    Parameters:  <none>
    Notes:       
  ------------------------------------------------------------------------------*/
  DEFINE INPUT-OUTPUT PARAMETER dY AS DECIMAL     NO-UNDO.
  DEFINE VARIABLE pcOverskr AS CHARACTER NO-UNDO.
  DEFINE VARIABLE pcLabel   AS CHARACTER NO-UNDO.

  IF CAN-DO("SE,SVE",cSprak) THEN 
  DO:
    ASSIGN 
      pcOverskr = "Diverse" + CHR(1) + "Antal" + CHR(1) + "Belopp".
    ASSIGN 
      pcLabel = "Utbetalt" + CHR(1) + "Inbetalt" + CHR(1) + "Tiligode in" + CHR(1) + "Tilgodo ut" + CHR(1) + 
                         "Dep-in" + CHR(1) + "Dep-ut" + CHR(1) + "Presentkort in" + CHR(1) + 
                         "Presentkort ut" + CHR(1) + "Dropp" + CHR(1) + "Ing�ende interna �verf�ringar" + CHR(1) + 
                         "Utg�ende interna �verf�ringar" + CHR(1) + "Varumottag" + CHR(1) + "Lagerjusteringar" + CHR(1) +
                         "Kassation" + CHR(1) + "Intern f�rbrukning" + CHR(1) + "Reklamation" + CHR(1) + 
                         "Medlemsf�rs�ljning" + CHR(1) + "Inbetalt kund" + CHR(1) + "V�xel" + CHR(1) + "Returer" + CHR(1) + "G�vekortsrabatt" + CHR(1) + "NonSale(Negativ)".
  END.
  ELSE 
  DO:
    ASSIGN 
      pcOverskr = "Diverse" + CHR(1) + "Antall" + CHR(1) + "Bel�p".
    ASSIGN 
      pcLabel = "Utbetalt" + CHR(1) + "Innbetalt" + CHR(1) + "Tilgode inn" + CHR(1) + "Tilgode ut" + CHR(1) + 
                         "Dep-in" + CHR(1) + "Dep-ut" + CHR(1) + "Gavekort inn" + CHR(1) + 
                         "Gavekort ut" + CHR(1) + "Dropp" + CHR(1) + "Inng�ende interne overf�ringer" + CHR(1) + 
                         "Utg�ende interne overf�ringer" + CHR(1) + "Varemottak" + CHR(1) + "Lagerjustering" + CHR(1) +
                         "Brekkasje" + CHR(1) + "Internt forbruk" + CHR(1) + "Reklamasjon" + CHR(1) + 
                         "Medlemssalg" + CHR(1) + "Innbetalt kunde" + CHR(1) + "Veksel" + CHR(1) + "Returer" + CHR(1) + "Gavekortrabatt" + CHR(1) + "NonSale(Negativ)".
  END.
    
  dY = dY - 20.
  RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold",12).
  RUN pdf_text_xy_dec ("Spdf",ENTRY(1,pcOverskr,CHR(1)),dColPosFR[4],dY).
  dY = dY - 4.
  RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica",8).
  RUN pdf_line IN h_PDFinc  ("Spdf", dColPosFR[4], dY, dColPosFR[6], dY, 0.5).
    
  dY = dY - iLineSpace.
  RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold",8).
  RUN pdf_text_xy_dec ("Spdf",ENTRY(2,pcOverskr,CHR(1)),dColPosFR[5] - bredd(ENTRY(2,pcOverskr,CHR(1))),dY).
  RUN pdf_text_xy_dec ("Spdf",ENTRY(3,pcOverskr,CHR(1)),dColPosFR[6] - bredd(ENTRY(3,pcOverskr,CHR(1))),dY).
  dY = dY - 4.
  RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica",8).
  RUN pdf_line IN h_PDFinc  ("Spdf", dULstartFR[5], dY, dColPosFR[5], dY, 0.5).
  RUN pdf_line IN h_PDFinc  ("Spdf", dULstartFR[6], dY, dColPosFR[6], dY, 0.5).


  dY = dY - iLineSpace. /* Utbet */
  RUN pdf_text_xy_dec ("Spdf",ENTRY(1,pcLabel,CHR(1)),dColPosFR[4],dY).
  cBelopp = TRIM(STRING(tmpKas_Rap.AntKont_ut,"->>,>>9")).
  RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPosFR[5] - bredd(cBelopp),dY).
  cBelopp = TRIM(STRING(tmpKas_Rap.Kont_Ut,"->>>,>>>,>>9.99")).
  RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPosFR[6] - bredd(cBelopp),dY).
  dY = dY - iLineSpace. /* inbetalt */
  RUN pdf_text_xy_dec ("Spdf",ENTRY(2,pcLabel,CHR(1)),dColPosFR[4],dY).
  cBelopp = TRIM(STRING(tmpKas_Rap.AntKont_inn,"->>,>>9")).
  RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPosFR[5] - bredd(cBelopp),dY).
  cBelopp = TRIM(STRING(tmpKas_Rap.Kont_Inn,"->>>,>>>,>>9.99")).
  RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPosFR[6] - bredd(cBelopp),dY).
  dY = dY - iLineSpace. /* tilg in */
  RUN pdf_text_xy_dec ("Spdf",ENTRY(3,pcLabel,CHR(1)),dColPosFR[4],dY).
  cBelopp = TRIM(STRING(tmpKas_Rap.AntTilgodeInn,"->>,>>9")).
  RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPosFR[5] - bredd(cBelopp),dY).
  cBelopp = TRIM(STRING(tmpKas_Rap.TilgodeInn,"->>>,>>>,>>9.99")).
  RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPosFR[6] - bredd(cBelopp),dY).
  dY = dY - iLineSpace. /* Tilg ut */
  RUN pdf_text_xy_dec ("Spdf",ENTRY(4,pcLabel,CHR(1)),dColPosFR[4],dY).
  cBelopp = TRIM(STRING(tmpKas_Rap.AntTilgodeUt,"->>,>>9")).
  RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPosFR[5] - bredd(cBelopp),dY).
  cBelopp = TRIM(STRING(tmpKas_Rap.TilgodeUt,"->>>,>>>,>>9.99")).
  RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPosFR[6] - bredd(cBelopp),dY).
  dY = dY - iLineSpace. /* dep in */
  RUN pdf_text_xy_dec ("Spdf",ENTRY(5,pcLabel,CHR(1)),dColPosFR[4],dY).
  cBelopp = TRIM(STRING(tmpKas_Rap.AntLayAway_Inn,"->>,>>9")).
  RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPosFR[5] - bredd(cBelopp),dY).
  cBelopp = TRIM(STRING(tmpKas_Rap.LayAway_Inn,"->>>,>>>,>>9.99")).
  RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPosFR[6] - bredd(cBelopp),dY).
  dY = dY - iLineSpace. /* Dep ut */
  RUN pdf_text_xy_dec ("Spdf",ENTRY(6,pcLabel,CHR(1)),dColPosFR[4],dY).
  cBelopp = TRIM(STRING(tmpKas_Rap.AntLayAway_Ut,"->>,>>9")).
  RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPosFR[5] - bredd(cBelopp),dY).
  cBelopp = TRIM(STRING(tmpKas_Rap.LayAway_Ut,"->>>,>>>,>>9.99")).
  RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPosFR[6] - bredd(cBelopp),dY).
  dY = dY - iLineSpace. /* Gavekort in */
  RUN pdf_text_xy_dec ("Spdf",ENTRY(7,pcLabel,CHR(1)),dColPosFR[4],dY).
  cBelopp = TRIM(STRING(tmpKas_Rap.AntGavekortInn,"->>,>>9")).
  RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPosFR[5] - bredd(cBelopp),dY).
  cBelopp = TRIM(STRING(tmpKas_Rap.GavekortInn,"->>>,>>>,>>9.99")).
  RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPosFR[6] - bredd(cBelopp),dY).
  dY = dY - iLineSpace. /* Gavekort ut */
  RUN pdf_text_xy_dec ("Spdf",ENTRY(8,pcLabel,CHR(1)),dColPosFR[4],dY).
  cBelopp = TRIM(STRING(tmpKas_Rap.AntGavekortUt,"->>,>>9")).
  RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPosFR[5] - bredd(cBelopp),dY).
  cBelopp = TRIM(STRING(tmpKas_Rap.GavekortUt,"->>>,>>>,>>9.99")).
  RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPosFR[6] - bredd(cBelopp),dY).
  dY = dY - iLineSpace. /* Gavekortrabatt */
  RUN pdf_text_xy_dec ("Spdf",ENTRY(21,pcLabel,CHR(1)),dColPosFR[4],dY).
  cBelopp = TRIM(STRING(tmpKas_Rap.AntGavekortRabUt,"->>,>>9")).
  RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPosFR[5] - bredd(cBelopp),dY).
  cBelopp = TRIM(STRING(tmpKas_Rap.GavekortRabatt,"->>>,>>>,>>9.99")).
  RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPosFR[6] - bredd(cBelopp),dY).
  dY = dY - iLineSpace. /* Dropp */
  RUN pdf_text_xy_dec ("Spdf",ENTRY(9,pcLabel,CHR(1)),dColPosFR[4],dY).
  cBelopp = TRIM(STRING(tmpKas_Rap.AntDropp,"->>,>>9")).
  RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPosFR[5] - bredd(cBelopp),dY).
  cBelopp = TRIM(STRING(tmpKas_Rap.Dropp,"->>>,>>>,>>9.99")).
  RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPosFR[6] - bredd(cBelopp),dY).
  dY = dY - iLineSpace. /* Ing in overf */
  RUN pdf_text_xy_dec ("Spdf",ENTRY(10,pcLabel,CHR(1)),dColPosFR[4],dY).
  cBelopp = TRIM(STRING(tmpKas_Rap.AntOverfortInn,"->>,>>9")).
  RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPosFR[5] - bredd(cBelopp),dY).
  cBelopp = TRIM(STRING(tmpKas_Rap.OverfortInn,"->>>,>>>,>>9.99")).
  RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPosFR[6] - bredd(cBelopp),dY).
  dY = dY - iLineSpace. /* Utg int overf */
  RUN pdf_text_xy_dec ("Spdf",ENTRY(11,pcLabel,CHR(1)),dColPosFR[4],dY).
  cBelopp = TRIM(STRING(tmpKas_Rap.AntOverfortUt,"->>,>>9")).
  RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPosFR[5] - bredd(cBelopp),dY).
  cBelopp = TRIM(STRING(tmpKas_Rap.OverfortUt,"->>>,>>>,>>9.99")).
  RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPosFR[6] - bredd(cBelopp),dY).
  dY = dY - iLineSpace. /* Varumottag */
  RUN pdf_text_xy_dec ("Spdf",ENTRY(12,pcLabel,CHR(1)),dColPosFR[4],dY).
  cBelopp = TRIM(STRING(tmpKas_Rap.AntVaremottak,"->>,>>9")).
  RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPosFR[5] - bredd(cBelopp),dY).
  cBelopp = TRIM(STRING(tmpKas_Rap.Varemottak,"->>>,>>>,>>9.99")).
  RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPosFR[6] - bredd(cBelopp),dY).
  dY = dY - iLineSpace. /* Lagerjustering */
  RUN pdf_text_xy_dec ("Spdf",ENTRY(13,pcLabel,CHR(1)),dColPosFR[4],dY).
  cBelopp = TRIM(STRING(tmpKas_Rap.AntLagerjustering,"->>,>>9")).
  RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPosFR[5] - bredd(cBelopp),dY).
  cBelopp = TRIM(STRING(tmpKas_Rap.Lagerjustering,"->>>,>>>,>>9.99")).
  RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPosFR[6] - bredd(cBelopp),dY).
  dY = dY - iLineSpace. /* Kassation */
  RUN pdf_text_xy_dec ("Spdf",ENTRY(14,pcLabel,CHR(1)),dColPosFR[4],dY).
  cBelopp = TRIM(STRING(tmpKas_Rap.AntBrekkasje,"->>,>>9")).
  RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPosFR[5] - bredd(cBelopp),dY).
  cBelopp = TRIM(STRING(tmpKas_Rap.Brekkasje,"->>>,>>>,>>9.99")).
  RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPosFR[6] - bredd(cBelopp),dY).
  dY = dY - iLineSpace. /* Internt forbruk */
  RUN pdf_text_xy_dec ("Spdf",ENTRY(15,pcLabel,CHR(1)),dColPosFR[4],dY).
  cBelopp = TRIM(STRING(tmpKas_Rap.AntInterntForbruk,"->>,>>9")).
  RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPosFR[5] - bredd(cBelopp),dY).
  cBelopp = TRIM(STRING(tmpKas_Rap.InterntForbruk,"->>>,>>>,>>9.99")).
  RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPosFR[6] - bredd(cBelopp),dY).
    
  dY = dY - iLineSpace. /* Reklamasjon */
  RUN pdf_text_xy_dec ("Spdf",ENTRY(16,pcLabel,CHR(1)),dColPosFR[4],dY).
  cBelopp = TRIM(STRING(tmpKas_Rap.AntReklamasjon,"->>,>>9")).
  RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPosFR[5] - bredd(cBelopp),dY).
  cBelopp = TRIM(STRING(tmpKas_Rap.Reklamasjon,"->>>,>>>,>>9.99")).
  RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPosFR[6] - bredd(cBelopp),dY).
    
  dY = dY - iLineSpace. /* Medlemssalg */
  RUN pdf_text_xy_dec ("Spdf",ENTRY(17,pcLabel,CHR(1)),dColPosFR[4],dY).
  cBelopp = TRIM(STRING(tmpKas_Rap.AntMedlemssalg,"->>,>>9")).
  RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPosFR[5] - bredd(cBelopp),dY).
  cBelopp = TRIM(STRING(tmpKas_Rap.Medlemssalg,"->>>,>>>,>>9.99")).
  RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPosFR[6] - bredd(cBelopp),dY).
    
  dY = dY - iLineSpace. /* Innbetalt kunde */
  RUN pdf_text_xy_dec ("Spdf",ENTRY(18,pcLabel,CHR(1)),dColPosFR[4],dY).
  cBelopp = TRIM(STRING(tmpKas_Rap.AntInnbetaltKunde,"->>,>>9")).
  RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPosFR[5] - bredd(cBelopp),dY).
  cBelopp = TRIM(STRING(tmpKas_Rap.InnbetaltKunde,"->>>,>>>,>>9.99")).
  RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPosFR[6] - bredd(cBelopp),dY).
    
  dY = dY - iLineSpace. /* Veksel */
  RUN pdf_text_xy_dec ("Spdf",ENTRY(19,pcLabel,CHR(1)),dColPosFR[4],dY).
  cBelopp = TRIM(STRING(tmpKas_Rap.AntVeksel,"->>,>>9")).
  RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPosFR[5] - bredd(cBelopp),dY).
  cBelopp = TRIM(STRING(tmpKas_Rap.Veksel,"->>>,>>>,>>9.99")).
  RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPosFR[6] - bredd(cBelopp),dY).
    
  dY = dY - iLineSpace. /* Returer */
  RUN pdf_text_xy_dec ("Spdf",ENTRY(20,pcLabel,CHR(1)),dColPosFR[4],dY).
  cBelopp = TRIM(STRING(tmpKas_Rap.AntRetur,"->>,>>9")).
  RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPosFR[5] - bredd(cBelopp),dY).
  cBelopp = TRIM(STRING(tmpKas_Rap.Retur,"->>>,>>>,>>9.99")).
  RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPosFR[6] - bredd(cBelopp),dY).
    
  dY = dY - iLineSpace. /* Nonsale negativ */
  RUN pdf_text_xy_dec ("Spdf",ENTRY(22,pcLabel,CHR(1)),dColPosFR[4],dY).
  cBelopp = TRIM(STRING(tmpKas_Rap.Non_SaleNegAnt,"->>,>>9")).
  RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPosFR[5] - bredd(cBelopp),dY).
  cBelopp = TRIM(STRING(tmpKas_Rap.Non_SaleNeg,"->>>,>>>,>>9.99")).
  RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPosFR[6] - bredd(cBelopp),dY).
    
  dY = dY - 4.
  RUN pdf_line IN h_PDFinc  ("Spdf", dColPosFR[4], dY, dColPosFR[6], dY, 0.5).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Saml_2_in_ut) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Saml_2_in_ut Procedure 
PROCEDURE Saml_2_in_ut :
  /*------------------------------------------------------------------------------
    Purpose:     
    Parameters:  <none>
    Notes:       
  ------------------------------------------------------------------------------*/
  DEFINE INPUT-OUTPUT  PARAMETER dY AS DECIMAL     NO-UNDO.
  DEFINE VARIABLE cOverskr   AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cLabel     AS CHARACTER EXTENT 5 NO-UNDO.
  DEFINE VARIABLE iKontoPara AS INTEGER   EXTENT 5 NO-UNDO.
  DEFINE VARIABLE plSum      AS DEC       NO-UNDO.
  DEFINE VARIABLE pcTekst    AS CHAR      NO-UNDO.
  DEFINE VARIABLE piLoop     AS INT       NO-UNDO.
  DEFINE VARIABLE cChar      AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cKonto     AS CHARACTER NO-UNDO.

  ASSIGN 
    iKontoPara[1] = 59
    iKontoPara[2] = 60
    iKontoPara[3] = 59
    iKontoPara[4] = 60
    iKontoPara[5] = 61.
    
  IF CAN-DO("SE,SVE",cSprak) THEN 
  DO:
    ASSIGN 
      cOverskr  = "In / Utbetalningar"
      cLabel[1] = "Inbetalt"
      cLabel[2] = "Utbetalt"
      cLabel[3] = "Nonsale inbet"
      cLabel[4] = "Nonsale utbet"
      cLabel[5] = "Dropp".
  END.
  ELSE 
  DO:
    ASSIGN 
      cOverskr  = "In / Utbetalningar"
      cLabel[1] = "Innbetalt"
      cLabel[2] = "Utbetalt"
      cLabel[3] = "Nonsale inbet"
      cLabel[4] = "Nonsale utbet"
      cLabel[5] = "Dropp".
  END.

  /* Overskrift */
  /*    IF pcRappType <> '99' THEN*/
  DO:
    dY = dY - 24.
    RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold",8).
    RUN pdf_text_xy_dec ("Spdf",cOverskr,dColPosBF[1],dY).
    dY = dY - 4.
    RUN pdf_line IN h_PDFinc  ("Spdf", pdf_LeftMargin ("Spdf"), dY, dColPosBF[2] - 15 ,dY, 0.5).
    RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica",8).
  
    /* Innbetalt */
    dY = dY - 14.
    RUN settTekstOgKonto(tmpKas_Rap.Butikk,61,1,tmpKas_Rap.kont_in,'D',OUTPUT cChar, OUTPUT cKonto). 
    RUN pdf_text_xy_dec ("Spdf",(cChar),dColPosBF[1],dY).
    RUN pdf_text_xy_dec ("Spdf",cKonto,dColPosBF[2],dY).
    cBelopp = TRIM(STRING(STRING(tmpKas_Rap.kont_in,"->>>,>>>,>>9.99"))).
    RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPosBF[3] - bredd(cBelopp),dY).
    RUN pdf_text_xy_dec ("Spdf","  -",dColPosBF[3],dY).
  END.
  DO:
    iLinjeNr = iLinjeNr + 1.
    RUN settTekstOgKonto(tmpKas_Rap.Butikk,61,1,tmpKas_Rap.kont_in,'D',OUTPUT cChar, OUTPUT cKonto). 
    CREATE ttEksport.
    ASSIGN 
      ttEksport.ButNr       = tmpKas_Rap.Butikk
      ttEksport.ButNamn     = cButikkTxt
      ttEksport.Dato        = pdFraDato
      ttEksport.LinjeNr     = iLinjeNr
      ttEksport.Tekst       = cChar
      ttEksport.KontoPrefix = ENTRY(1,cKonto,' ')
      ttEksport.KontoNr     = INT(ENTRY(2,cKonto,' '))
      ttEksport.Belop       = tmpKas_Rap.kont_in
      .
    RUN opprettBokforingsvisning(2,"INN/UTBETALINGER",0,0).
    iLinjeNr = iLinjeNr + 1.
    RUN opprettBokforingsvisning(1,"",0,0).

    CREATE ttEksport.
    ASSIGN
      iLinjeNr              = iLinjeNr + 1
      ttEksport.ButNr       = tmpKas_Rap.Butikk
      ttEksport.ButNamn     = cButikkTxt
      ttEksport.Dato        = pdFraDato
      ttEksport.LinjeNr     = iLinjeNr
      ttEksport.Tekst       = ''
      ttEksport.KontoPrefix = 'Innbetallilngsbilag'
      ttEksport.KontoNr     = 0
      ttEksport.MvaKontoNr  = 0
      ttEksport.Belop       = 0
      ttEksport.Mva         = 0
      ttEksport.BelopUMva   = 0
      .
      
    /* Legger opp eventyelle innbetalingsbilag som er registrert ved korreksjon */
    FOR EACH BokforingsKorrBilag NO-LOCK WHERE 
      BokforingsKorrBilag.BokforingsID = BokforingsBilag.BokforingsID AND 
      BokforingsKorrBilag.TTId = 61 AND 
      BokforingsKorrBilag.TbId = 1:
      ASSIGN
        ttEksport.Belop = ttEksport.Belop + DEC(BokforingsKorr.Belop)
        .
    END.
      
    iLinjeNr = iLinjeNr + 1.
    RUN opprettBokforingsvisning(6,"",61,1).
  END. 
    
  /* Utbetalt */
  /*    IF pcRappType <> '99' THEN*/
  DO:
    dY = dY - 14.
    RUN settTekstOgKonto(tmpKas_Rap.Butikk,62,1,tmpKas_Rap.kont_ut,'K',OUTPUT cChar, OUTPUT cKonto). 
    RUN pdf_text_xy_dec ("Spdf",(cChar),dColPosBF[1],dY).
    RUN pdf_text_xy_dec ("Spdf",cKonto,dColPosBF[2],dY).
    cBelopp = TRIM(STRING(STRING(tmpKas_Rap.kont_ut,"->>>,>>>,>>9.99"))).
    RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPosBF[3] - bredd(cBelopp),dY).
    RUN pdf_text_xy_dec ("Spdf","  -",dColPosBF[3],dY).
  END.
  DO:
    iLinjeNr = iLinjeNr + 1.
    RUN settTekstOgKonto(tmpKas_Rap.Butikk,62,1,tmpKas_Rap.kont_ut,'K',OUTPUT cChar, OUTPUT cKonto). 
    CREATE ttEksport.
    ASSIGN 
      ttEksport.ButNr       = tmpKas_Rap.Butikk
      ttEksport.ButNamn     = cButikkTxt
      ttEksport.Dato        = pdFraDato
      ttEksport.LinjeNr     = iLinjeNr
      ttEksport.Tekst       = cChar
      ttEksport.KontoPrefix = ENTRY(1,cKonto,' ')
      ttEksport.KontoNr     = INT(ENTRY(2,cKonto,' '))
      ttEksport.Belop       = tmpKas_Rap.kont_ut
      .
    RUN opprettBokforingsvisning(1,"",62,0).
      
    CREATE ttEksport.
    ASSIGN
      iLinjeNr              = iLinjeNr + 1
      ttEksport.ButNr       = tmpKas_Rap.Butikk
      ttEksport.ButNamn     = cButikkTxt
      ttEksport.Dato        = pdFraDato
      ttEksport.LinjeNr     = iLinjeNr
      ttEksport.Tekst       = ''
      ttEksport.KontoPrefix = 'Utbetallilngsbilag'
      ttEksport.KontoNr     = 0
      ttEksport.MvaKontoNr  = 0
      ttEksport.Belop       = 0
      ttEksport.Mva         = 0
      ttEksport.BelopUMva   = 0
      .
      
    /* Legger opp eventyelle utbetalingsbilag som er registrert ved korreksjon */
    FOR EACH BokforingsKorrBilag NO-LOCK WHERE 
      BokforingsKorrBilag.BokforingsID = BokforingsBilag.BokforingsID AND 
      BokforingsKorrBilag.TTId = 62 AND 
      BokforingsKorrBilag.TbId = 1:
      ASSIGN
        ttEksport.Belop = ttEksport.Belop + DEC(BokforingsKorr.Belop)
        .
    END.
    /* Snur fortegn p� utbetaling. */
    ttEksport.Belop = ttEksport.Belop * -1.
      
    iLinjeNr = iLinjeNr + 1.      
    RUN opprettBokforingsvisning(6,"",62,1).
  END. 
    
  /* NonSale positiv */
  IF tmpKas_Rap.Non_SalePos <> 0 THEN
  DO:
    dY = dY - 14.
    RUN settTekstOgKonto(tmpKas_Rap.Butikk,900,24,tmpKas_Rap.Non_SalePos,'D',OUTPUT cChar, OUTPUT cKonto). 
    RUN pdf_text_xy_dec ("Spdf",(cChar),dColPosBF[1],dY).
    RUN pdf_text_xy_dec ("Spdf",cKonto,dColPosBF[2],dY).
    cBelopp = TRIM(STRING(STRING(tmpKas_Rap.Non_SalePos,"->>>,>>>,>>9.99"))).
    RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPosBF[3] - bredd(cBelopp),dY).
    RUN pdf_text_xy_dec ("Spdf","  -",dColPosBF[3],dY).
    iLinjeNr = iLinjeNr + 1.
    RUN settTekstOgKonto(tmpKas_Rap.Butikk,900,24,tmpKas_Rap.Non_SalePos,'D',OUTPUT cChar, OUTPUT cKonto). 
    CREATE ttEksport.
    ASSIGN 
      ttEksport.ButNr       = tmpKas_Rap.Butikk
      ttEksport.ButNamn     = cButikkTxt
      ttEksport.Dato        = pdFraDato
      ttEksport.LinjeNr     = iLinjeNr
      ttEksport.Tekst       = cChar
      ttEksport.KontoPrefix = ENTRY(1,cKonto,' ')
      ttEksport.KontoNr     = INT(ENTRY(2,cKonto,' '))
      ttEksport.Belop       = tmpKas_Rap.Non_SalePos
      .
    RUN opprettBokforingsvisning(1,"",900,0).
  END. 
    
  /* NonSale negativ */
  IF tmpKas_Rap.Non_SaleNeg <> 0 THEN
  DO:
    dY = dY - 14.
    RUN settTekstOgKonto(tmpKas_Rap.Butikk,900,25,tmpKas_Rap.Non_SaleNeg,'K',OUTPUT cChar, OUTPUT cKonto). 
    RUN pdf_text_xy_dec ("Spdf",(cChar),dColPosBF[1],dY).
    RUN pdf_text_xy_dec ("Spdf",cKonto,dColPosBF[2],dY).
    cBelopp = TRIM(STRING(STRING(tmpKas_Rap.Non_SaleNeg,"->>>,>>>,>>9.99"))).
    RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPosBF[3] - bredd(cBelopp),dY).
    RUN pdf_text_xy_dec ("Spdf","  -",dColPosBF[3],dY).
    iLinjeNr = iLinjeNr + 1.
    RUN settTekstOgKonto(tmpKas_Rap.Butikk,900,25,tmpKas_Rap.Non_SaleNeg,'K',OUTPUT cChar, OUTPUT cKonto). 
    CREATE ttEksport.
    ASSIGN 
      ttEksport.ButNr       = tmpKas_Rap.Butikk
      ttEksport.ButNamn     = cButikkTxt
      ttEksport.Dato        = pdFraDato
      ttEksport.LinjeNr     = iLinjeNr
      ttEksport.Tekst       = cChar
      ttEksport.KontoPrefix = ENTRY(1,cKonto,' ')
      ttEksport.KontoNr     = INT(ENTRY(2,cKonto,' '))
      ttEksport.Belop       = tmpKas_Rap.Non_SaleNeg
      .
    RUN opprettBokforingsvisning(1,"",900,0).
  END. 
    
  /* Dropp */
  /*    IF pcRappType <> '99' THEN*/
  DO:
    dY = dY - 14.
    cBelopp = TRIM(STRING(STRING(tmpKas_Rap.Dropp ,"->>>,>>>,>>9.99"))).
    RUN settTekstOgKonto(tmpKas_Rap.Butikk,59,1,tmpKas_Rap.Dropp,'K',OUTPUT cChar, OUTPUT cKonto). 
    RUN pdf_text_xy_dec ("Spdf",(cChar),dColPosBF[1],dY).
    RUN pdf_text_xy_dec ("Spdf",cKonto,dColPosBF[2],dY).
    RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPosBF[3] - bredd(cBelopp),dY).
    RUN pdf_text_xy_dec ("Spdf","  -",dColPosBF[3],dY).
    iLinjeNr = iLinjeNr + 1.
    RUN settTekstOgKonto(tmpKas_Rap.Butikk,59,1,tmpKas_Rap.Dropp,'K',OUTPUT cChar, OUTPUT cKonto). 
    CREATE ttEksport.
    ASSIGN 
      ttEksport.ButNr       = tmpKas_Rap.Butikk
      ttEksport.ButNamn     = cButikkTxt
      ttEksport.Dato        = pdFraDato
      ttEksport.LinjeNr     = iLinjeNr
      ttEksport.Tekst       = cChar
      ttEksport.KontoPrefix = ENTRY(1,cKonto,' ')
      ttEksport.KontoNr     = INT(ENTRY(2,cKonto,' '))
      ttEksport.Belop       = tmpKas_Rap.Dropp
      .
    RUN opprettBokforingsvisning(1,"",59,0).
      
    dY = dY - 14.
    cBelopp = TRIM(STRING(STRING(lExDropp ,"->>>,>>>,>>9.99"))).
    /*      RUN settTekstOgKonto(tmpKas_Rap.Butikk,59,1,lBelop,'K',OUTPUT cChar, OUTPUT cKonto).*/
    RUN settTekstOgKonto(tmpKas_Rap.Butikk,900,30,lBelop,'K',OUTPUT cChar, OUTPUT cKonto).
    cChar = "Ekstra dropp". 
    RUN pdf_text_xy_dec ("Spdf",(cChar),dColPosBF[1],dY).
    RUN pdf_text_xy_dec ("Spdf",cKonto,dColPosBF[2],dY).
    RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPosBF[3] - bredd(cBelopp),dY).
    RUN pdf_text_xy_dec ("Spdf","  -",dColPosBF[3],dY).
    iLinjeNr = iLinjeNr + 1.
    CREATE ttEksport.
    ASSIGN 
      ttEksport.ButNr       = tmpKas_Rap.Butikk
      ttEksport.ButNamn     = cButikkTxt
      ttEksport.Dato        = pdFraDato
      ttEksport.LinjeNr     = iLinjeNr
      ttEksport.Tekst       = cChar
      ttEksport.KontoPrefix = ENTRY(1,cKonto,' ')
      ttEksport.KontoNr     = INT(ENTRY(2,cKonto,' '))
      ttEksport.Belop       = lExDropp
      .
    /*      RUN opprettBokforingsvisning(1,cChar,59,1).*/
    RUN opprettBokforingsvisning(1,cChar,900,30).
      
  END. 


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Saml_2_kredit) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Saml_2_kredit Procedure 
PROCEDURE Saml_2_kredit :
  /*------------------------------------------------------------------------------
    Purpose:     
    Parameters:  <none>
    Notes:       
  ------------------------------------------------------------------------------*/
  DEFINE INPUT-OUTPUT PARAMETER dY AS DECIMAL   NO-UNDO.
  DEFINE VARIABLE pcOverskr  AS CHARACTER NO-UNDO.
  DEFINE VARIABLE pcLabel    AS CHARACTER NO-UNDO.
  /* DEFINE VARIABLE cLabel    AS CHARACTER EXTENT  NO-UNDO. */
  DEFINE VARIABLE pcKonto    AS CHARACTER NO-UNDO.
  DEFINE VAR      piLoop     AS INT       NO-UNDO.
  DEFINE VAR      pcBank     AS CHAR      NO-UNDO.
  DEFINE VAR      piInt      AS INT       NO-UNDO.
  DEFINE VARIABLE ii         AS INTEGER   NO-UNDO.
  DEFINE VAR      pcTekst    AS CHAR      NO-UNDO.
  DEFINE VARIABLE iKontoPara AS INTEGER   EXTENT 2 NO-UNDO.
  DEFINE VARIABLE cChar      AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cKonto     AS CHARACTER NO-UNDO.

  ASSIGN 
    iKontoPara[1] = 40
    iKontoPara[2] = 41.
  IF CAN-DO("SE,SVE",cSprak) THEN 
  DO:
    /*     ASSIGN cLabel[1] = */
    /*            cLabel[2] = */
    ASSIGN
      pcOverskr = "Text" + CHR(1) + 
                  "Konto" + CHR(1) + 
                  "Belopp" + CHR(1) +
                  "Mva"   + CHR(1) +
                  "Belopp u/mva"
      pcLabel   = "Kreditf�rs�ljning" + CHR(1) +
                  "Fakturerat" + CHR(1) + 
                  "Inbetalt konto"
      pcKonto   = "K 0000  0%" + CHR(1) +
                  "K 3000 24%" + chr(1) +
                  "K 3001 12%" + chr(1) +
                  "K 0000  0%" + chr(1) +
                  "K 0000  0%" + chr(1) +
                  "K 0000  0%" + chr(1) +
                  "K 0000  0%" + chr(1) +
                  "K 0000  0%" + chr(1) +
                  "K 0000  0%" + chr(1) +
                  "K 0000  0%"
      pcBank    = "D 2380"
      .
  END.
  ELSE 
  DO:
    ASSIGN
      pcOverskr = "Tekst" + CHR(1) + 
                  "Konto" + CHR(1) + 
                  "Bel�p" + CHR(1) +
                  "Mva"   + CHR(1) +
                  "Bel�p u/mva"
      pcLabel   = "Kredittsalg" + CHR(1) +
                  "Fakturert" + CHR(1) + 
                  "Innbetalt konto"
      pcKonto   = "K 0000  0%" + CHR(1) +
                  "K 3000 24%" + chr(1) +
                  "K 3001 12%" + chr(1) +
                  "K 0000  0%" + chr(1) +
                  "K 0000  0%" + chr(1) +
                  "K 0000  0%" + chr(1) +
                  "K 0000  0%" + chr(1) +
                  "K 0000  0%" + chr(1) +
                  "K 0000  0%" + chr(1) +
                  "K 0000  0%"
      pcBank    = "D 2380"
      .
  END.                                                                               

  /*    IF pcRappType <> '99' THEN*/
  DO:
    dY = dY - 24.
    RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold",8).
    RUN pdf_text_xy_dec ("Spdf",ENTRY(1,pcLabel,CHR(1)),dColPosBF[1],dY).
    dY = dY - 4.
    RUN pdf_line IN h_PDFinc  ("Spdf", pdf_LeftMargin ("Spdf"), dY, dColPosBF[2] - 15 ,dY, 0.5).
    RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica",8).
    /* Fakturerat */
    dY = dY - 14.
    RUN settTekstOgKonto(tmpKas_Rap.Butikk,65,1,tmpKas_Rap.Kredit,'D',OUTPUT cChar, OUTPUT cKonto). 
    RUN pdf_text_xy_dec ("Spdf",(cChar),dColPosBF[1],dY).
    RUN pdf_text_xy_dec ("Spdf",cKonto,dColPosBF[2],dY).
    cBelopp = TRIM(STRING(STRING(tmpKas_Rap.Kredit,"->>>,>>>,>>9.99"))).
    RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPosBF[3] - bredd(cBelopp),dY).
    RUN pdf_text_xy_dec ("Spdf","  -",dColPosBF[3],dY).
  END.
  DO:
    iLinjeNr = iLinjeNr + 1.
    RUN settTekstOgKonto(tmpKas_Rap.Butikk,65,1,tmpKas_Rap.Kredit,'D',OUTPUT cChar, OUTPUT cKonto). 
    CREATE ttEksport.
    ASSIGN 
      ttEksport.ButNr       = tmpKas_Rap.Butikk
      ttEksport.ButNamn     = cButikkTxt
      ttEksport.Dato        = pdFraDato
      ttEksport.LinjeNr     = iLinjeNr
      ttEksport.Tekst       = cChar
      ttEksport.KontoPrefix = ENTRY(1,cKonto,' ')
      ttEksport.KontoNr     = INT(ENTRY(2,cKonto,' '))
      ttEksport.Belop       = tmpKas_Rap.Kredit
      .
    RUN opprettBokforingsvisning(2,"KREDITSALG",0,0).
    RUN opprettBokforingsvisning(1,"",0,0).
  END. 
    

  /* Inbetalt konto */
  /*    IF pcRappType <> '99' THEN*/
  DO:
    dY = dY - 14.
    RUN settTekstOgKonto(tmpKas_Rap.Butikk,61,4,tmpKas_Rap.Kont_Inn,'K',OUTPUT cChar, OUTPUT cKonto). 
    RUN pdf_text_xy_dec ("Spdf",(cChar),dColPosBF[1],dY).
    RUN pdf_text_xy_dec ("Spdf",cKonto,dColPosBF[2],dY).
    cBelopp = TRIM(STRING(STRING(tmpKas_Rap.Kont_Inn,"->>>,>>>,>>9.99"))).
    RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPosBF[3] - bredd(cBelopp),dY).
    RUN pdf_text_xy_dec ("Spdf","  -",dColPosBF[3],dY).
  END.
  DO:
    iLinjeNr = iLinjeNr + 1.
    RUN settTekstOgKonto(tmpKas_Rap.Butikk,61,4,tmpKas_Rap.Kont_Inn,'K',OUTPUT cChar, OUTPUT cKonto). 
    CREATE ttEksport.
    ASSIGN 
      ttEksport.ButNr       = tmpKas_Rap.Butikk
      ttEksport.ButNamn     = cButikkTxt
      ttEksport.Dato        = pdFraDato
      ttEksport.LinjeNr     = iLinjeNr
      ttEksport.Tekst       = cChar
      ttEksport.KontoPrefix = ENTRY(1,cKonto,' ')
      ttEksport.KontoNr     = INT(ENTRY(2,cKonto,' '))
      ttEksport.Belop       = tmpKas_Rap.Kont_Inn
      .
    RUN opprettBokforingsvisning(1,"",61,0).
  END. 

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Saml_2_mva) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Saml_2_mva Procedure 
PROCEDURE Saml_2_mva :
  /*------------------------------------------------------------------------------
    Purpose:     
    Parameters:  <none>
    Notes:       
  ------------------------------------------------------------------------------*/
  DEFINE INPUT-OUTPUT PARAMETER dY AS DECIMAL   NO-UNDO.
  DEFINE VARIABLE pcOverskr      AS CHARACTER NO-UNDO.
  DEFINE VARIABLE pcLabel        AS CHARACTER NO-UNDO.
  DEFINE VARIABLE pcKonto        AS CHARACTER NO-UNDO.
  DEFINE VAR      piLoop         AS INT       NO-UNDO.
  DEFINE VAR      pcBank         AS CHAR      NO-UNDO.
  DEFINE VAR      piInt          AS INT       NO-UNDO.
  DEFINE VARIABLE cKonto         AS CHARACTER NO-UNDO.
  DEFINE VARIABLE ii             AS INTEGER   NO-UNDO.
  DEFINE VAR      pcTekst        AS CHAR      NO-UNDO.
  DEFINE VARIABLE cChar          AS CHARACTER NO-UNDO.

  ASSIGN 
    lKorrBrutto    = 0
    lKorrNetto     = 0
    lKorrMva       = 0
    lOresavrunding = 0
    .

  /* Legger opp eventyelle ekstra kortsalg som er registrert ved korreksjon */
  FOR EACH BokforingsKorrBilag NO-LOCK WHERE
    BokforingsKorrBilag.BokforingsID = BokforingsBilag.BokforingsID AND
    BokforingsKorrBilag.TTId = 1 AND 
    BokforingsKorrBilag.TBId = 1:
    ASSIGN
      lOresavrunding = lOresavrunding + BokforingsKorr.Belop
      .
  END.

  /* Legger opp eventyelle ekstra kortsalg som er registrert ved korreksjon */
  /* TN 27/8-20 Her skal ikke igive tas med.                                */
  FOR EACH BokforingsKorrBilag NO-LOCK WHERE
    BokforingsKorrBilag.BokforingsID = BokforingsBilag.BokforingsID AND
    BokforingsKorrBilag.TTId = 52: /* Summerer her for alle korttyper. */
    /* Ikke iGive her */
    IF BokforingsKorrBilag.TBId = 38 THEN 
      NEXT.
    ASSIGN
      lKorrBrutto = lKorrBrutto + BokforingsKorr.Belop
      .
  END.

  /* Legger opp eventyelle ekstra Banksalg som er registrert ved korreksjon */
  FOR EACH BokforingsKorrBilag NO-LOCK WHERE
    BokforingsKorrBilag.BokforingsID = BokforingsBilag.BokforingsID AND
    BokforingsKorrBilag.TTId = 58: /* Summerer her for alle korttyper. */
    ASSIGN
      lKorrBrutto = lKorrBrutto + BokforingsKorr.Belop
      .
  END.

  /* Legger opp eventyelle ekstra omstning registrert ved korreksjon */
  /* NB: TN 3/5-20 Dette p�virker dagens kontantstr�m i motsetning.  */
  FOR EACH BokforingsKorrBilag NO-LOCK WHERE
    BokforingsKorrBilag.BokforingsID = BokforingsBilag.BokforingsID AND
    BokforingsKorrBilag.TTId = 1: /* Summerer her for alle korttyper. */
    ASSIGN
      lKorrBrutto = lKorrBrutto + BokforingsKorr.Belop
      .
  END.

  ASSIGN 
    lKorrMva   = ROUND((lKorrBrutto * 20) / 100,2)
    lKorrNetto = lKorrBrutto - lKorrMva
    . 


  IF CAN-DO("SE,SVE",cSprak) THEN 
  DO:
    ASSIGN
      pcOverskr = "Text" + CHR(1) + 
                  "Konto" + CHR(1) + 
                  "Belopp" + CHR(1) +
                  "Mva"   + CHR(1) +
                  "Belopp u/mva"
      pcLabel   = "Varuf�rs�ljning momsgrupp"
      pcKonto   = "K 0000  0%" + CHR(1) +
                  "K 3000 24%" + chr(1) +
                  "K 3001 12%" + chr(1) +
                  "K 0000  0%" + chr(1) +
                  "K 0000  0%" + chr(1) +
                  "K 0000  0%" + chr(1) +
                  "K 0000  0%" + chr(1) +
                  "K 0000  0%" + chr(1) +
                  "K 0000  0%" + chr(1) +
                  "K 0000  0%"
      pcBank    = "D 2380"
      .
  END.
  ELSE 
  DO:
    ASSIGN
      pcOverskr = "Tekst" + CHR(1) + 
                  "Konto" + CHR(1) + 
                  "Bel�p" + CHR(1) +
                  "Mva"   + CHR(1) +
                  "Bel�p u/mva"
      pcLabel   = "Varesalg mva gruppe"
      pcKonto   = "K 0000  0%" + CHR(1) +
                  "K 3000 24%" + chr(1) +
                  "K 3001 12%" + chr(1) +
                  "K 0000  0%" + chr(1) +
                  "K 0000  0%" + chr(1) +
                  "K 0000  0%" + chr(1) +
                  "K 0000  0%" + chr(1) +
                  "K 0000  0%" + chr(1) +
                  "K 0000  0%" + chr(1) +
                  "K 0000  0%"
      pcBank    = "D 2380"
      .
  END.
  /*    IF pcRappType <> '99' THEN*/
  DO:                                                                           
    RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold",8).
    RUN pdf_text_xy_dec ("Spdf",ENTRY(1,pcOverskr,CHR(1)),dColPosBF[1],dY).
    RUN pdf_text_xy_dec ("Spdf",ENTRY(2,pcOverskr,CHR(1)),dColPosBF[2],dY).
    RUN pdf_text_xy_dec ("Spdf",ENTRY(3,pcOverskr,CHR(1)),dColPosBF[3] - bredd(ENTRY(3,pcOverskr,CHR(1))),dY).
    RUN pdf_text_xy_dec ("Spdf",ENTRY(4,pcOverskr,CHR(1)),dColPosBF[4] - bredd(ENTRY(4,pcOverskr,CHR(1))),dY).
    RUN pdf_text_xy_dec ("Spdf",ENTRY(5,pcOverskr,CHR(1)),dColPosBF[5] - bredd(ENTRY(5,pcOverskr,CHR(1))),dY).
    dY = dY - 4.
    RUN pdf_line IN h_PDFinc  ("Spdf", pdf_LeftMargin ("Spdf"), dY, dColPosBF[2] - 15 , dY, 0.5).
    RUN pdf_line IN h_PDFinc  ("Spdf", dColPosBF[2], dY, 275 , dY, 0.5).
    RUN pdf_line IN h_PDFinc  ("Spdf", 290, dY, dColPosBF[3] , dY, 0.5).
    RUN pdf_line IN h_PDFinc  ("Spdf", 385, dY, dColPosBF[4] , dY, 0.5).
    RUN pdf_line IN h_PDFinc  ("Spdf", 484, dY, dColPosBF[5] , dY, 0.5).
    RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica",8).
  END.
    
  /* Legger ut mva regnskapet. */
  DO piLoop = 1 TO 10:
    IF piLoop = 2 OR tmpKas_rap.MvaGrunnlag[piLoop] <> 0 THEN
    DO:
      dY = dY - 14.
      ASSIGN
        piInt   = tmpKas_Rap.MvaGrp[piLoop] + 1
        pcTekst = ENTRY(piLoop,pcKonto,CHR(1))
        .
      IF tmpKas_rap.MvaGrunnlag[piLoop] > 0 THEN
      {syspara.i 20 1 piInt pcTekst}
      ELSE
      {syspar2.i 20 1 piInt pcTekst}
      cKonto  = pcTekst.
      FIND Moms NO-LOCK WHERE
        Moms.MomsKod = tmpKas_rap.MvaGrp[piLoop] NO-ERROR.
      pcTekst = ENTRY(1,pcLabel,CHR(1)).

      IF AVAILABLE Moms THEN
        pcTekst = pcTekst + " " + STRING(tmpKas_Rap.MvaGrp[piLoop]) + " " + STRING(Moms.MomsProc) + "%".
      ELSE 
        pcTekst = pcTekst + " " + string(ROUND((tmpKas_rap.MvaBelop[piLoop] / tmpKas_rap.MvaGrunnlag[piLoop]) * 100,0)) + "%".

      /*            IF pcRappType <> '99' THEN*/
      DO:
        RUN pdf_text_xy_dec ("Spdf",ENTRY(1,pcTekst,CHR(1)),dColPosBF[1],dY).
              
        cBelopp = TRIM(STRING(tmpKas_rap.MvaGrunnlag[piLoop] + tmpKas_rap.MvaBelop[piLoop] + lKorrBrutto - lOresavrunding,"->>>,>>>,>>9.99")).              
        RUN settTekstOgKonto(tmpKas_Rap.Butikk,1,1,DEC(cBelopp),'D',OUTPUT cChar, OUTPUT cKonto).
        RUN settMvaKonto(tmpKas_Rap.Butikk,1,INPUT-OUTPUT cKonto).
        RUN pdf_text_xy_dec ("Spdf",cKonto,dColPosBF[2],dY).
        RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPosBF[3] - bredd(cBelopp),dY).
              
        cBelopp = TRIM(STRING(tmpKas_rap.MvaBelop[piLoop] + lKorrMva,"->>>,>>>,>>9.99")).
        RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPosBF[4] - bredd(cBelopp),dY).
              
        cBelopp = TRIM(STRING(tmpKas_rap.MvaGrunnlag[piLoop] + lKorrNetto,"->>>,>>>,>>9.99")).
        RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPosBF[5] - bredd(cBelopp),dY).
      END.
      DO:
        iLinjeNr = iLinjeNr + 1.
        RUN settTekstOgKonto(tmpKas_Rap.Butikk,1,1,DEC(cBelopp),'D',OUTPUT cChar, OUTPUT cKonto).
        RUN settMvaKonto(tmpKas_Rap.Butikk,1,INPUT-OUTPUT cKonto).
        CREATE ttEksport.
        ASSIGN 
          ttEksport.ButNr       = tmpKas_Rap.Butikk
          ttEksport.ButNamn     = cButikkTxt
          ttEksport.Dato        = pdFraDato
          ttEksport.LinjeNr     = iLinjeNr
          ttEksport.Tekst       = pcTekst
          ttEksport.KontoPrefix = ENTRY(1,cKonto,' ')
          ttEksport.KontoNr     = INT(ENTRY(2,ENTRY(1,cKonto,'/'),' '))
          ttEksport.MvaKontoNr  = INT(ENTRY(2,cKonto,'/'))
          ttEksport.Belop       = tmpKas_rap.MvaGrunnlag[piLoop] + tmpKas_rap.MvaBelop[piLoop] + lKorrBrutto
          ttEksport.Mva         = tmpKas_rap.MvaBelop[piLoop] + lKorrMva
          ttEksport.BelopUMva   = tmpKas_rap.MvaGrunnlag[piLoop] + lKorrNetto
          .
        RUN opprettBokforingsvisning(2,"OMSETNING",0,0).
        RUN opprettBokforingsvisning(5,"",1,1). 
        RUN opprettBokforingsvisning(3,"",1,1). 
        RUN opprettBokforingsvisning(4,"",1,1). 
      END. 
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Saml_2_PoseNr) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Saml_2_PoseNr Procedure 
PROCEDURE Saml_2_PoseNr :
  /*------------------------------------------------------------------------------
    Purpose:     
    Parameters:  <none>
    Notes:       
  ------------------------------------------------------------------------------*/
  DEFINE VARIABLE dY2           AS DECIMAL   NO-UNDO.
  DEFINE VARIABLE dTotBank      AS DECIMAL   NO-UNDO.
  DEFINE VARIABLE cLabel        AS CHARACTER EXTENT 15 NO-UNDO.
  DEFINE VARIABLE lKasserFinns  AS LOGICAL   NO-UNDO.
  DEFINE VARIABLE pcTekst       AS CHARACTER NO-UNDO.
  DEFINE VARIABLE piLoop        AS INTEGER   NO-UNDO.
  DEFINE VARIABLE piOldLinjeNr  AS INTEGER   NO-UNDO.
  DEFINE VARIABLE lSumAnt       AS DECIMAL   FORMAT "->>,>>>,>>9" NO-UNDO.
  DEFINE VARIABLE lSumVerdi     AS DECIMAL   FORMAT "->>,>>>,>>9" NO-UNDO.
  DEFINE VARIABLE lLCVerdi      AS DECIMAL   FORMAT "->>,>>>,>>9" NO-UNDO.
  DEFINE VARIABLE iLCAnt        AS INTEGER   NO-UNDO.
  DEFINE VARIABLE obOk          AS LOG       NO-UNDO.
  DEFINE VARIABLE ocReturn      AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cKommisjonLST AS CHARACTER NO-UNDO.
          
  DEFINE BUFFER bufButiker FOR Butiker.
          
  /* Visning av informasjon om kommisjonsbutikker skal bare vises for GANT butikker. */        
  IF iGantAktiv = 1 THEN         
  KOMMISJONSBUTIKKER:
  DO:
    cKommisjonLST = ''.
    FOR EACH bufButiker WHERE 
      bufButiker.Butik >= 10000 AND 
      bufButiker.butik <= 10999:
      cKommisjonLST = cKommisjonLST + 
        (IF cKommisjonLST = '' THEN '' ELSE ',') + 
        STRING(bufButiker.Butik).  
    END. 
  /* TN TEST Ta bort i PROD*/
  /*    IF SEARCH('tnc.txt') <> ? THEN cKommisjonLST = cKommisjonLST + ',11'.*/
  END. /* KOMMISJONSBUTIKKER */
   
  IF CAN-DO("SE,SVE",cSprak) THEN 
  DO:
    ASSIGN 
      cLabel[1]  = "Bankp�sar"
      cLabel[2]  = "P�sNr"
      cLabel[3]  = "Belopp"
      cLabel[4]  = "Totalt"
      cLabel[5]  = "Returer"
      cLabel[6]  = "Reklamationer"
      cLabel[7]  = "Utbetalning"
      cLabel[8]  = "Innk. overf�ringer"
      cLabel[9]  = "Utg. overf�ringer"
      cLabel[10] = "Rab. HK pris"
      cLabel[11] = "Rab. Outlet pris"
      cLabel[12] = "Godkjent av"
      cLabel[13] = "Utskrevet av"
      cLabel[14] = "Oppgj�r"
      cLabel[15] = "Oppt.morgen"
      .
  END.
  ELSE 
  DO:
    ASSIGN 
      cLabel[1]  = "Bankposer"
      cLabel[2]  = "PoseNr"
      cLabel[3]  = " Bel�p"
      cLabel[4]  = "Totalt"
      cLabel[5]  = "Returer"
      cLabel[6]  = "Reklamasjoner"
      cLabel[7]  = "Utbetaling"
      cLabel[8]  = "Innk. overf�ringer"
      cLabel[9]  = "Utg. overf�ringer"
      cLabel[10] = "Rabatt mot HK pris"
      cLabel[11] = "Outlet rabatt"
      cLabel[12] = "Godkjent av"
      cLabel[13] = "Utskrevet av"
      cLabel[14] = "Oppgj�r"
      cLabel[15] = "Oppt.morgen"
      .

  END.
  
  IF AVAILABLE bokforingsbilag THEN 
  DO:
    /* Teller opp og logger overf�ringer til/fra butikken p� den aktuelle dagen. */
    RUN dagsrapp_overfor.p (FALSE, /* Append - False t�mmer tabellen f�rst. */
      BokforingsBilag.ButikkNr,
      BokforingsBilag.OmsetningsDato,
      INPUT-OUTPUT TABLE ttOverfor).
  
    /* Teller opp og logger rabatter fra butikken p� den aktuelle dagen. */
    RUN dagsrapp_rabattertsalg.p (FALSE, /* Append - False t�mmer tabellen f�rst. */
      BokforingsBilag.ButikkNr,
      BokforingsBilag.OmsetningsDato,
      INPUT-OUTPUT TABLE ttRabattertSalg).
  END.
  
  ASSIGN 
    piOldLinjeNr = iLinjeNr
    iLinjeNr     = 99
    dY2          = 610
    .
  
  ASSIGN 
    lKasserFinns = FALSE.
  FOR EACH KassererOppgj NO-LOCK WHERE
    KassererOppgj.Dato     = pdFraDato AND
    KassererOppgj.ButikkNr = piButNr.
    IF KassererOppgj.OpptaltLevertBank > 0 THEN
      ASSIGN lKasserFinns = TRUE.
  END.
  
  /*  IF pcRappType <> '99' THEN*/
  DO:
    IF lKasserFinns = TRUE THEN
    KASSEFINANS:
    DO:
      ASSIGN 
        dTotBank = 0.
      RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold",12).
      RUN pdf_text_xy_dec ("Spdf",cLabel[1],400,dY2).
  
      ASSIGN 
        dY2 = 600.
      RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold",8).
      RUN pdf_text_xy_dec ("Spdf",cLabel[2],400,dY2).
      RUN pdf_text_xy_dec ("Spdf",cLabel[3],505,dY2).
      dY2 = dY2 - 4.
      RUN pdf_line IN h_PDFinc  ("Spdf", 400, dY2, 530 ,dY2, 0.5).
      RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica",8).
      piLoop = 0.
      
      FOR EACH KassererOppgj NO-LOCK WHERE
        KassererOppgj.Dato     = pdFraDato AND
        KassererOppgj.ButikkNr = piButNr.
        IF KassererOppgj.OpptaltLevertBank > 0 THEN
        DO:
          piLoop = piLoop + 1.
          dY2 = dY2 - 14.
          RUN pdf_text_xy_dec ("Spdf",KassererOppgj.PoseNr,400,dY2).
          ASSIGN 
            cBelopp = TRIM(STRING(KassererOppgj.OpptaltLevertBank,"->>>,>>>,>>9.99")).
          RUN pdf_text_xy_dec ("Spdf",cBelopp,530 - bredd(cBelopp),dY2).
          ASSIGN 
            dTotBank = dTotBank + KassererOppgj.OpptaltLevertBank
            .
            
          CREATE ttEksport.
          ASSIGN 
            iLinjeNr              = iLinjeNr + 1
            ttEksport.ButNr       = tmpKas_Rap.Butikk
            ttEksport.ButNamn     = cButikkTxt
            ttEksport.Dato        = pdFraDato
            ttEksport.LinjeNr     = iLinjeNr
            ttEksport.Tekst       = cLabel[2] + '(800/0): ' + TRIM(KassererOppgj.PoseNr)
            ttEksport.KontoPrefix = ''
            ttEksport.KontoNr     = 0
            ttEksport.MvaKontoNr  = 0
            ttEksport.Belop       = KassererOppgj.OpptaltLevertBank
            ttEksport.Mva         = 0
            ttEksport.BelopUMva   = 0
            .

          IF piLoop = 1 THEN
          DO:
            RUN opprettBokforingsvisning(2,"BANKPOSER",0,0).
            iLinjeNr = iLinjeNr + 1.
            RUN opprettBokforingsvisning(2,"Ny bankpose(800/2)",800,2). 
            iLinjeNr = iLinjeNr + 1.
          END.
          RUN opprettBokforingsvisning(1,"",800,1). 
            
          /* Legger opp eventyelle poser som er registrert ved korreksjon */
          FOR EACH BokforingsKorr NO-LOCK WHERE 
            BokforingsKorr.BokforingsID = BokforingsBilag.BokforingsID AND 
            BokforingsKorr.TTId = 800 AND 
            BokforingsKorr.TbId = 2:

            piLoop = piLoop + 1.
            dY2 = dY2 - 14.
            RUN pdf_text_xy_dec ("Spdf",BokforingsKorr.Merknad,400,dY2).
            ASSIGN 
              cBelopp = TRIM(STRING(BokforingsKorr.Belop,"->>>,>>>,>>9.99")).
            RUN pdf_text_xy_dec ("Spdf",cBelopp,530 - bredd(cBelopp),dY2).
            ASSIGN 
              dTotBank = dTotBank + BokforingsKorr.Belop
              .
    
            CREATE ttEksport.
            ASSIGN
              iLinjeNr              = iLinjeNr + 1
              ttEksport.ButNr       = tmpKas_Rap.Butikk
              ttEksport.ButNamn     = cButikkTxt
              ttEksport.Dato        = pdFraDato
              ttEksport.LinjeNr     = iLinjeNr
              ttEksport.Tekst       = 'PoseNr(800/2): ' + BokforingsKorr.Merknad
              ttEksport.KontoPrefix = ''
              ttEksport.KontoNr     = 0
              ttEksport.MvaKontoNr  = 0
              ttEksport.Belop       = DEC(BokforingsKorr.Belop)
              ttEksport.Mva         = 0
              ttEksport.BelopUMva   = 0
              .
    
            RUN opprettBokforingsvisning(1,"",800,1). 
            iLinjeNr = iLinjeNr + 1.
          END.
        END.        
      END.
      dY2 = dY2 - 4.
      RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold",8).
      RUN pdf_line IN h_PDFinc  ("Spdf", 490, dY2, 530 ,dY2, 0.5).
      dY2 = dY2 - 14.
      RUN pdf_text_xy_dec ("Spdf",cLabel[4],400,dY2).
      ASSIGN 
        cBelopp = TRIM(STRING(dTotBank,"->>>,>>>,>>9.99")).
      RUN pdf_text_xy_dec ("Spdf",cBelopp,530 - bredd(cBelopp),dY2).
      RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica",8).
    END. /* KASSEFINANS */
    ELSE  
    KLARGJORBANKPOSE: 
    DO:
      /* Klargj�re for bankpose registrering */
      CREATE ttEksport.
      ASSIGN
        iLinjeNr              = iLinjeNr + 1
        ttEksport.ButNr       = tmpKas_Rap.Butikk
        ttEksport.ButNamn     = cButikkTxt
        ttEksport.Dato        = pdFraDato
        ttEksport.LinjeNr     = iLinjeNr
        ttEksport.Tekst       = cLabel[2] + '(800/000): '
        ttEksport.KontoPrefix = ''
        ttEksport.KontoNr     = 0
        ttEksport.MvaKontoNr  = 0
        ttEksport.Belop       = 0
        ttEksport.Mva         = 0
        ttEksport.BelopUMva   = 0
        .
      RUN opprettBokforingsvisning(2,"BANKPOSER",0,0).
      iLinjeNr = iLinjeNr + 1.
      RUN opprettBokforingsvisning(2,"Ny bankpose(800/2)",800,2). 

      IF CAN-FIND(FIRST BokforingsKorr NO-LOCK WHERE 
        BokforingsKorr.BokforingsID = BokforingsBilag.BokforingsID AND 
        BokforingsKorr.TTId = 800 AND 
        BokforingsKorr.TbId = 2) THEN
      DO:
        ASSIGN 
          dTotBank = 0.
        RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold",12).
        RUN pdf_text_xy_dec ("Spdf",cLabel[1],400,dY2).
    
        ASSIGN 
          dY2 = 600.
        RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold",8).
        RUN pdf_text_xy_dec ("Spdf",cLabel[2],400,dY2).
        RUN pdf_text_xy_dec ("Spdf",cLabel[3],505,dY2).
        dY2 = dY2 - 4.
        RUN pdf_line IN h_PDFinc  ("Spdf", 400, dY2, 530 ,dY2, 0.5).
        RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica",8).
        piLoop = 0.
      END.

      /* Legger opp eventyelle poser som er registrert ved korreksjon */
      FOR EACH BokforingsKorr NO-LOCK WHERE 
        BokforingsKorr.BokforingsID = BokforingsBilag.BokforingsID AND 
        BokforingsKorr.TTId = 800 AND 
        BokforingsKorr.TbId = 2:

        piLoop = piLoop + 1.
        dY2 = dY2 - 14.
        RUN pdf_text_xy_dec ("Spdf",BokforingsKorr.Merknad,400,dY2).
        ASSIGN 
          cBelopp = TRIM(STRING(BokforingsKorr.Belop,"->>>,>>>,>>9.99")).
        RUN pdf_text_xy_dec ("Spdf",cBelopp,530 - bredd(cBelopp),dY2).
        ASSIGN 
          dTotBank = dTotBank + BokforingsKorr.Belop
          .

        CREATE ttEksport.
        ASSIGN
          iLinjeNr              = iLinjeNr + 1
          ttEksport.ButNr       = tmpKas_Rap.Butikk
          ttEksport.ButNamn     = cButikkTxt
          ttEksport.Dato        = pdFraDato
          ttEksport.LinjeNr     = iLinjeNr
          ttEksport.Tekst       = 'PoseNr(800/2): ' + BokforingsKorr.Merknad
          ttEksport.KontoPrefix = ''
          ttEksport.KontoNr     = 0
          ttEksport.MvaKontoNr  = 0
          ttEksport.Belop       = DEC(BokforingsKorr.Belop)
          ttEksport.Mva         = 0
          ttEksport.BelopUMva   = 0
          .

        RUN opprettBokforingsvisning(1,"",800,1). 
        iLinjeNr = iLinjeNr + 1.
      END.

      IF CAN-FIND(FIRST BokforingsKorr NO-LOCK WHERE 
        BokforingsKorr.BokforingsID = BokforingsBilag.BokforingsID AND 
        BokforingsKorr.TTId = 800 AND 
        BokforingsKorr.TbId = 2) THEN
      DO:
        dY2 = dY2 - 4.
        RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold",8).
        RUN pdf_line IN h_PDFinc  ("Spdf", 490, dY2, 530 ,dY2, 0.5).
        dY2 = dY2 - 14.
        RUN pdf_text_xy_dec ("Spdf",cLabel[4],400,dY2).
        ASSIGN 
          cBelopp = TRIM(STRING(dTotBank,"->>>,>>>,>>9.99")).
        RUN pdf_text_xy_dec ("Spdf",cBelopp,530 - bredd(cBelopp),dY2).
        RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica",8).
      END.
    END.

    /* Her var vi med linjetellingen. */ 
    iLinjeNr = piOldLinjeNr.
    dY2 = dY2 - 14. /* Tom linje f�r resten av infoen kommer. */
  
    IF tmpKas_Rap.AntRetur <> 0 THEN
    DO:
      dY2 = dY2 - 14.
      RUN pdf_text_xy_dec ("Spdf",cLabel[5],400,dY2).
      ASSIGN 
        cBelopp = TRIM(STRING(tmpKas_Rap.AntRetur,"->>,>>9")).
      RUN pdf_text_xy_dec ("Spdf",cBelopp,480 - bredd(cBelopp),dY2).
      ASSIGN 
        cBelopp = TRIM(STRING(tmpKas_Rap.Retur,"->>>,>>>,>>9.99")).
      RUN pdf_text_xy_dec ("Spdf",cBelopp,530 - bredd(cBelopp),dY2).
    END. /* ANTALL_RETURER */
  
    IF tmpKas_Rap.AntReklamasjon <> 0 THEN
    DO:
      dY2 = dY2 - 14.
      RUN pdf_text_xy_dec ("Spdf",cLabel[6],400,dY2).
      ASSIGN 
        cBelopp = TRIM(STRING(tmpKas_Rap.AntReklamasjon,"->>,>>9")).
      RUN pdf_text_xy_dec ("Spdf",cBelopp,480 - bredd(cBelopp),dY2).
      ASSIGN 
        cBelopp = TRIM(STRING(tmpKas_Rap.Reklamasjon,"->>>,>>>,>>9.99")).
      RUN pdf_text_xy_dec ("Spdf",cBelopp,530 - bredd(cBelopp),dY2).
    END. /* ANTALL_REKLAMASJONER */
  
    IF tmpKas_Rap.AntKont_ut <> 0 THEN 
    DO:  /* i st f blocket ovanf�r */
      dY2 = dY2 - 14.
      RUN pdf_text_xy_dec ("Spdf",cLabel[7],400,dY2).
      ASSIGN 
        cBelopp = TRIM(STRING(tmpKas_Rap.AntKont_ut,"->>,>>9")).
      RUN pdf_text_xy_dec ("Spdf",cBelopp,480 - bredd(cBelopp),dY2).
      ASSIGN 
        cBelopp = TRIM(STRING(tmpKas_Rap.Kont_Ut,"->>>,>>>,>>9.99")).
      RUN pdf_text_xy_dec ("Spdf",cBelopp,530 - bredd(cBelopp),dY2).
    END.
    
    /* Antall og bel�p iGive gavekort. */
    /* TN iGive                        */
    IF iAntigive > 0 THEN 
    DO:  
      dY2 = dY2 - 14.
      RUN pdf_text_xy_dec ("Spdf","iGive",400,dY2).
      ASSIGN 
        cBelopp = TRIM(STRING(iAntigive,"->>,>>9")).
      RUN pdf_text_xy_dec ("Spdf",cBelopp,480 - bredd(cBelopp),dY2).
      ASSIGN 
        cBelopp = TRIM(STRING(lVerdiiGive,"->>>,>>>,>>9.99")).
      RUN pdf_text_xy_dec ("Spdf",cBelopp,530 - bredd(cBelopp),dY2).
    END.
     
    /* Innkommende overf�ringer. */
    ASSIGN 
      lSumant   = 0
      lsumVerdi = 0
      .
    FOR EACH ttOverfor WHERE 
      ttOverfor.Ovtype = 1 AND 
      ttOverfor.ButNr = BokforingsBilag.ButikkNr:
      ASSIGN 
        lSumant   = lSumAnt + ttOverfor.Antall      
        lsumVerdi = lSumVerdi + ttOverfor.Verdi
        .
    END.
    IF lSumant <> 0 THEN 
    DO:  
      dY2 = dY2 - 14.
      RUN pdf_text_xy_dec ("Spdf",cLabel[8],400,dY2).
      ASSIGN 
        cBelopp = TRIM(STRING(lSumAnt,"->>,>>9")).
      RUN pdf_text_xy_dec ("Spdf",cBelopp,480 - bredd(cBelopp),dY2).
      ASSIGN 
        cBelopp = TRIM(STRING(lSumverdi,"->>>,>>>,>>9.99")).
      RUN pdf_text_xy_dec ("Spdf",cBelopp,530 - bredd(cBelopp),dY2).
    END.
    
    /* Utg�ende overf�ringer. */
    ASSIGN 
      lSumant   = 0
      lsumVerdi = 0
      .
    FOR EACH ttOverfor WHERE 
      ttOverfor.Ovtype = 2 AND 
      ttOverfor.ButNr = BokforingsBilag.ButikkNr: 
      ASSIGN 
        lSumant   = lSumAnt + ttOverfor.Antall      
        lsumVerdi = lSumVerdi + ttOverfor.Verdi
        .
    END.
    IF lSumant <> 0 THEN 
    DO:  
      dY2 = dY2 - 14.
      RUN pdf_text_xy_dec ("Spdf",cLabel[9],400,dY2).
      ASSIGN 
        cBelopp = TRIM(STRING(lSumant,"->>,>>9")).
      RUN pdf_text_xy_dec ("Spdf",cBelopp,480 - bredd(cBelopp),dY2).
      ASSIGN 
        cBelopp = TRIM(STRING(lSumVerdi,"->>>,>>>,>>9.99")).
      RUN pdf_text_xy_dec ("Spdf",cBelopp,530 - bredd(cBelopp),dY2).
    END.

    /* Rabatter mot HK pris. */
    ASSIGN 
      lSumant   = 0
      lsumVerdi = 0
      .
    FOR EACH ttRabattertSalg WHERE 
      ttRabattertSalg.ButNr = BokforingsBilag.ButikkNr: 
      ASSIGN 
        lSumant   = lSumAnt   + ttRabattertSalg.Antall      
        lsumVerdi = lSumVerdi + ttRabattertSalg.Rabatt
        .
    END.
    IF lsumVerdi <> 0 THEN 
    DO:  
      dY2 = dY2 - 14.
      RUN pdf_text_xy_dec ("Spdf",cLabel[10],400,dY2).
      ASSIGN 
        cBelopp = TRIM(STRING(lSumant,"->>,>>9")).
      RUN pdf_text_xy_dec ("Spdf",cBelopp,480 - bredd(cBelopp),dY2).
      ASSIGN 
        cBelopp = TRIM(STRING(lSumVerdi,"->>>,>>>,>>9.99")).
      RUN pdf_text_xy_dec ("Spdf",cBelopp,530 - bredd(cBelopp),dY2).
    END.

    /* Rabatter mot Outlet pris. */
    ASSIGN 
      lSumant   = 0
      lsumVerdi = 0
      .
    FOR EACH ttRabattertSalg WHERE 
      ttRabattertSalg.ButNr = BokforingsBilag.ButikkNr: 
      ASSIGN 
        lSumant   = lSumAnt   + ttRabattertSalg.Antall      
        lsumVerdi = lSumVerdi + ttRabattertSalg.RabattOutlet
        .
    END.
    IF lSumVerdi <> 0 THEN 
    DO:  
      dY2 = dY2 - 14.
      RUN pdf_text_xy_dec ("Spdf",cLabel[11],400,dY2).
      ASSIGN 
        cBelopp = TRIM(STRING(lSumant,"->>,>>9")).
      RUN pdf_text_xy_dec ("Spdf",cBelopp,480 - bredd(cBelopp),dY2).
      ASSIGN 
        cBelopp = TRIM(STRING(lSumVerdi,"->>>,>>>,>>9.99")).
      RUN pdf_text_xy_dec ("Spdf",cBelopp,530 - bredd(cBelopp),dY2).
    END.

    IF CAN-DO(cKommisjonLST,STRING(BokforingsBilag.ButikkNr)) THEN
    KOMMISJONSBUTIKKER: 
    DO:
      /* Varemottak kommisjonsbutikker. */
      ASSIGN 
        lSumant   = 0
        lsumVerdi = 0
        .
      FOR EACH PksdlHode NO-LOCK WHERE 
        PkSdlHode.PkSdlStatus = 20 AND
        PkSdlHode.ButikkNr = BokforingsBilag.ButikkNr AND   
        CAN-FIND(FIRST PkSdlMottak OF PkSdlHode WHERE 
        PkSdlMottak.MottattDato = BokforingsBilag.OmsetningsDato):
        
        IF PkSdlHode.LandedCost > 0 THEN
        DO: 
          iLCAnt = 0.
          lLCVerdi = PkSdlHode.LandedCost.
          FOR EACH PkSdlLinje OF PkSdlHode NO-LOCK:
            iLCAnt = iLCAnt + PkSdlLinje.AntLevert.
          END.
        END.
        ELSE 
        DO:
          RUN PkSdlSetLandedCost.p (PkSdlHode.PkSdlId, ?, '', OUTPUT ocReturn, OUTPUT obOk).
          ASSIGN 
            lLCVerdi = DEC(ENTRY(1,ocReturn,'|'))
            iLCAnt   = INT(ENTRY(2,ocReturn,'|'))
            NO-ERROR.  
        END.
        
        ASSIGN 
          lSumant   = lSumAnt   + iLCAnt       
          lsumVerdi = lSumVerdi + lLCVerdi
          .
      END.
      IF lSumVerdi <> 0 THEN 
      DO:  
        lSumVerdi = ROUND(lSumVerdi,0).
        dY2 = dY2 - 14.
        RUN pdf_text_xy_dec ("Spdf","Varekj�p LC (5/1)",400,dY2).
        ASSIGN 
          cBelopp = TRIM(STRING(lSumant,"->>,>>9")).
        RUN pdf_text_xy_dec ("Spdf",cBelopp,480 - bredd(cBelopp),dY2).
        ASSIGN 
          cBelopp = TRIM(STRING(lSumVerdi,"->>>,>>>,>>9.99")).
        RUN pdf_text_xy_dec ("Spdf",cBelopp,530 - bredd(cBelopp),dY2).
  
        CREATE ttEksport.
        ASSIGN
          iLinjeNr              = iLinjeNr + 1
          ttEksport.ButNr       = tmpKas_Rap.Butikk
          ttEksport.ButNamn     = cButikkTxt
          ttEksport.Dato        = pdFraDato
          ttEksport.LinjeNr     = iLinjeNr
          ttEksport.Tekst       = 'Varekj�p LC(5/1)' /* + BokforingsKorr.Merknad */
          ttEksport.KontoPrefix = ''
          ttEksport.KontoNr     = 0
          ttEksport.MvaKontoNr  = 0
          ttEksport.Belop       = lSumVerdi
          ttEksport.Mva         = 0
          ttEksport.BelopUMva   = 0
          .
        RUN opprettBokforingsvisning(2,"KOMMISJON",0,0).
          
        iLinjeNr = iLinjeNr + 1.
        RUN opprettBokforingsvisning(7,"",5,1). 
      END.
      
      /* Overf�ringer til overskuddslager. */
      ASSIGN 
        lSumant   = 0
        lsumVerdi = 0
        .
      FOR EACH Translogg NO-LOCK WHERE
        TransLogg.Dato    = BokforingsBilag.OmsetningsDato AND 
        Translogg.TTId    = 6 AND /* Overf�ring */ 
        TransLogg.Butik   = BokforingsBilag.ButikkNr AND 
        TransLogg.OvButik = 20: /* Til overskuddslager. */
        
        ASSIGN 
          lSumant   = lSumAnt   + TransLogg.Antall       
          lsumVerdi = lSumVerdi + (TransLogg.Antall * TransLogg.Pris).
        .
      END.
      IF lSumVerdi <> 0 THEN 
      DO:  
        lSumVerdi = ROUND(lSumVerdi,0).
        dY2 = dY2 - 14.
        RUN pdf_text_xy_dec ("Spdf","Overf�rt LC(6/2)",400,dY2).
        ASSIGN 
          cBelopp = TRIM(STRING(lSumant,"->>,>>9")).
        RUN pdf_text_xy_dec ("Spdf",cBelopp,480 - bredd(cBelopp),dY2).
        ASSIGN 
          cBelopp = TRIM(STRING(lSumVerdi,"->>>,>>>,>>9.99")).
        RUN pdf_text_xy_dec ("Spdf",cBelopp,530 - bredd(cBelopp),dY2).
  
        CREATE ttEksport.
        ASSIGN
          iLinjeNr              = iLinjeNr + 1
          ttEksport.ButNr       = tmpKas_Rap.Butikk
          ttEksport.ButNamn     = cButikkTxt
          ttEksport.Dato        = pdFraDato
          ttEksport.LinjeNr     = iLinjeNr
          ttEksport.Tekst       = 'Overf�rt LC(6/2)'
          ttEksport.KontoPrefix = ''
          ttEksport.KontoNr     = 0
          ttEksport.MvaKontoNr  = 0
          ttEksport.Belop       = lSumVerdi
          ttEksport.Mva         = 0
          ttEksport.BelopUMva   = 0
          .
        RUN opprettBokforingsvisning(7,"",6,2). 
      END.

      /* Overf�ringer til overskuddslager. */
      ASSIGN 
        lSumant   = 0
        lsumVerdi = 0
        .
      FOR EACH Translogg NO-LOCK WHERE
        TransLogg.Dato    = BokforingsBilag.OmsetningsDato AND 
        Translogg.TTId    = 9 AND /* Svinn */ 
        TransLogg.Butik   = BokforingsBilag.ButikkNr:
        
        ASSIGN 
          lSumant   = lSumAnt   + TransLogg.Antall       
          lsumVerdi = lSumVerdi + (TransLogg.Antall * TransLogg.Pris).
        .
      END.
      IF lSumVerdi <> 0 THEN 
      DO:  
        lSumVerdi = ROUND(lSumVerdi,0).
        dY2 = dY2 - 14.
        RUN pdf_text_xy_dec ("Spdf","Svinn LC(9/1)",400,dY2).
        ASSIGN 
          cBelopp = TRIM(STRING(lSumant,"->>,>>9")).
        RUN pdf_text_xy_dec ("Spdf",cBelopp,480 - bredd(cBelopp),dY2).
        ASSIGN 
          cBelopp = TRIM(STRING(lSumVerdi,"->>>,>>>,>>9.99")).
        RUN pdf_text_xy_dec ("Spdf",cBelopp,530 - bredd(cBelopp),dY2).
  
        CREATE ttEksport.
        ASSIGN
          iLinjeNr              = iLinjeNr + 1
          ttEksport.ButNr       = tmpKas_Rap.Butikk
          ttEksport.ButNamn     = cButikkTxt
          ttEksport.Dato        = pdFraDato
          ttEksport.LinjeNr     = iLinjeNr
          ttEksport.Tekst       = 'Svinn LC(9/1)'
          ttEksport.KontoPrefix = ''
          ttEksport.KontoNr     = 0
          ttEksport.MvaKontoNr  = 0
          ttEksport.Belop       = lSumVerdi
          ttEksport.Mva         = 0
          ttEksport.BelopUMva   = 0
          .
        RUN opprettBokforingsvisning(7,"",9,1). 
      END.

      /* Beregner kortgebyr for kommisjonsbutikker inkl. mva. Brutto omsetning m� regnes opp p� nytt her. */
      IF iAktiv = 1 THEN
      KORTGEBYR: 
      DO:
        wOmsKommisjon   = 0.
        wAndelKommisjon = 0.
        /* Summerer opp registrert omsetning. */
        DO piLoop = 1 TO 10:
          IF tmpKas_rap.MvaGrunnlag[piLoop] <> 0 THEN
          DO:
            wOmsKommisjon = wOmsKommisjon + 
              tmpKas_rap.MvaGrunnlag[piLoop] + 
              tmpKas_rap.MvaBelop[piLoop].
          END.
        END.
/*        FOR EACH BongLinje NO-LOCK WHERE                     */
/*          BongLinje.ButikkNr = tmpKas_Rap.butikk AND         */
/*          BongLinje.GruppeNr = 1 AND                         */
/*          BongLinje.KasseNr  >= 0 AND                        */
/*          BongLinje.Dato = tmpKas_Rap.Dato:                  */
/*          wOmsKommisjon = wOmsKommisjon + BongLinje.LinjeRab.*/
/*        END.                                                 */

        /* Finner verdi av totalsalg til kunde ink mva. */
        wOmsKommisjon = ROUND(((wOmsKommisjon * 100) / 45),1).
        dY2 = dY2 - 14.
        RUN pdf_text_xy_dec ("Spdf","Tot. kommisjonssalg ",400,dY2).
        ASSIGN 
          cBelopp = "".
        RUN pdf_text_xy_dec ("Spdf",cBelopp,480 - bredd(cBelopp),dY2).
        ASSIGN 
          cBelopp = TRIM(STRING(wOmsKommisjon,"->>>,>>>,>>9.99")).
        RUN pdf_text_xy_dec ("Spdf",cBelopp,530 - bredd(cBelopp),dY2).

        /* Finner illums andel som er 55% av ovenst�ende. */
        wOmsKommisjon = ROUND(((wOmsKommisjon * 55) / 100),1).
        dY2 = dY2 - 14.
        RUN pdf_text_xy_dec ("Spdf","Kommisjonsandel ",400,dY2).
        ASSIGN 
          cBelopp = "".
        RUN pdf_text_xy_dec ("Spdf",cBelopp,480 - bredd(cBelopp),dY2).
        ASSIGN 
          cBelopp = TRIM(STRING(wOmsKommisjon,"->>>,>>>,>>9.99")).
        RUN pdf_text_xy_dec ("Spdf",cBelopp,530 - bredd(cBelopp),dY2).
        
        /* Kortgebyr av Illums andel. */
        lKortGebyr = ROUND(((wOmsKommisjon * 1) / 100),1).
        IF lKortGebyr = ? THEN 
          lKortGebyr = 0.
        IF lKortGebyr <> 0 THEN 
        DO:  
          dY2 = dY2 - 14.
          RUN pdf_text_xy_dec ("Spdf","Kortgebyr (98/2)",400,dY2).
          ASSIGN 
            cBelopp = "".
          RUN pdf_text_xy_dec ("Spdf",cBelopp,480 - bredd(cBelopp),dY2).
          ASSIGN 
            cBelopp = TRIM(STRING(lKortGebyr,"->>>,>>>,>>9.99")).
          RUN pdf_text_xy_dec ("Spdf",cBelopp,530 - bredd(cBelopp),dY2).
    
          CREATE ttEksport.
          ASSIGN
            iLinjeNr              = iLinjeNr + 1
            ttEksport.ButNr       = tmpKas_Rap.Butikk
            ttEksport.ButNamn     = cButikkTxt
            ttEksport.Dato        = pdFraDato
            ttEksport.LinjeNr     = iLinjeNr
            ttEksport.Tekst       = 'Kortgebyr (98/2)'
            ttEksport.KontoPrefix = ''
            ttEksport.KontoNr     = 0
            ttEksport.MvaKontoNr  = 0
            ttEksport.Belop       = lKortGebyr * -1
            ttEksport.Mva         = 0
            ttEksport.BelopUMva   = 0
            .
          RUN opprettBokforingsvisning(7,"",98,2).
          
          CREATE ttEksport.
          ASSIGN
            iLinjeNr              = iLinjeNr + 1
            ttEksport.ButNr       = tmpKas_Rap.Butikk
            ttEksport.ButNamn     = cButikkTxt
            ttEksport.Dato        = pdFraDato
            ttEksport.LinjeNr     = iLinjeNr
            ttEksport.Tekst       = 'Kortgebyr (98/3)'
            ttEksport.KontoPrefix = ''
            ttEksport.KontoNr     = 0
            ttEksport.MvaKontoNr  = 0
            ttEksport.Belop       = lKortGebyr
            ttEksport.Mva         = 0
            ttEksport.BelopUMva   = 0
            .
          RUN opprettBokforingsvisning(7,"",98,3). 
           
        END.
      END. /* KORTGEBYR */
      /* Lager */
      ASSIGN 
        lSumant   = 0
        lsumVerdi = 0
        .
      FOR EACH Lager NO-LOCK WHERE 
        Lager.Butik = tmpKas_Rap.Butikk AND 
        Lager.Lagant > 0:
      
        lSumant = lSumant + Lager.LagAnt.
        IF Lager.VVareKost <> ? THEN
          lsumVerdi = lsumVerdi + (Lager.LagAnt * Lager.VVareKost).
      END.
      IF lsumVerdi <> 0 OR lSumant <> 0 THEN 
      DO:  
        lSumVerdi = ROUND(lSumVerdi,0).
        dY2 = dY2 - 14.
        RUN pdf_text_xy_dec ("Spdf","Lager",400,dY2).
        ASSIGN 
          cBelopp = TRIM(STRING(lSumant,"->>,>>9")).
        RUN pdf_text_xy_dec ("Spdf",cBelopp,480 - bredd(cBelopp),dY2).
        ASSIGN 
          cBelopp = TRIM(STRING(lSumVerdi,"->>>,>>>,>>9.99")).
        RUN pdf_text_xy_dec ("Spdf",cBelopp,530 - bredd(cBelopp),dY2).
      END.
    END. /* KOMMISJONSBUTIKKER*/
    
    /* Hvem som har talt opp kassene p� morgenen */
    FOR EACH Kasse NO-LOCK WHERE 
      Kasse.ButikkNr = BokforingsBilag.ButikkNr AND 
      Kasse.GruppeNr = 1 AND 
      Kasse.KasseNr <= 90,
      EACH bongHode NO-LOCK WHERE 
      BongHode.ButikkNr = Kasse.butikkNr AND 
      BongHode.GruppeNr = Kasse.GruppeNr AND          
      BongHode.KasseNr = Kasse.KasseNr AND
      BongHode.Dato = BokforingsBilag.OmsetningsDato AND
      CAN-FIND(FIRST BongLinje WHERE 
      BongLinje.B_Id = BongHode.B_Id AND 
      BongLinje.TTId = 108),
      FIRST BongLinje NO-LOCK WHERE 
      BongLinje.ButikkNr = Kasse.ButikkNr AND 
      BongLinje.GruppeNr = Kasse.GruppeNr AND 
      BongLinje.KasseNr = Kasse.KasseNr AND
      BongLinje.Dato = BongHode.Dato AND 
      BongLinje.TTId = 108:
      FIND Selger NO-LOCK WHERE 
        Selger.SelgerNr = BongHode.SelgerNr NO-ERROR.

      dY2 = dY2 - 14.
      RUN pdf_text_xy_dec ("Spdf",cLabel[15] + '(' + STRING(BongHode.KasseNr) + ')',400,dY2).
      cTekst = STRING(BongLinje.Linjesum,"->>>,>>>,>>9.99") + ' ' + 
        (IF AVAILABLE Selger THEN Selger.ForNavn + ' ' + Selger.Navn
        ELSE BongHode.SelgerNavn).
      RUN pdf_text_xy_dec ("Spdf",cTekst,450,dY2).
    END.

    /* Hvem som har gjort opp kassene */
    DO:
      FOR EACH KassererOppgj NO-LOCK WHERE
        KassererOppgj.Dato     = Kas_Rap.Dato AND
        KassererOppgj.Butikk   = Kas_Rap.Butikk:
        FIND Selger NO-LOCK WHERE 
          Selger.SelgerNr = KassererOppgj.SelgerNr NO-ERROR.
        dY2 = dY2 - 14.
        RUN pdf_text_xy_dec ("Spdf",cLabel[14] + '(' + STRING(KassererOppgj.SelgerId) + ')',400,dY2).
        cTekst = STRING(KassererOppgj.OpptaltKontanter,"->>>,>>>,>>9.99") + ' ' +  
          (IF AVAILABLE Selger THEN Selger.ForNavn + ' ' + Selger.Navn ELSE 'Ukjent selger').
        RUN pdf_text_xy_dec ("Spdf",cTekst,450,dY2).
      END.
    END.
    
    /* Signatur godkjenning */
    DO:
      dY2 = dY2 - 14.
      RUN pdf_text_xy_dec ("Spdf",cLabel[12],400,dY2).
      ASSIGN 
        cTekst = IF BokforingsBilag.GodkjentDato <> ? THEN 
                          STRING(BokforingsBilag.GodkjentDato) + ' ' +  
                          STRING(BokforingsBilag.GodkjentTid,"HH:MM:SS") + ' ' + 
                          BokforingsBilag.GodkjentAv
                        ELSE 
                          'IKKE GODKJENT'.
      RUN pdf_text_xy_dec ("Spdf",cTekst,450,dY2).
    END.

    /* Hvem som har skrevet ut bilaget */
    DO:
      dY2 = dY2 - 14.
      RUN pdf_text_xy_dec ("Spdf",cLabel[13],400,dY2).
      ASSIGN 
        cTekst = cBrukerId + ' ' + STRING(NOW,"99/99/9999 HH:MM:SS").
      RUN pdf_text_xy_dec ("Spdf",cTekst,450,dY2).
    END.
    
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Saml_2_utbetalt) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Saml_2_utbetalt Procedure 
PROCEDURE Saml_2_utbetalt :
  /*------------------------------------------------------------------------------
    Purpose:     
    Parameters:  <none>
    Notes:       
  ------------------------------------------------------------------------------*/
  DEFINE INPUT-OUTPUT PARAMETER dY AS DECIMAL   NO-UNDO.
  DEFINE VARIABLE pcOverskr  AS CHARACTER NO-UNDO.
  DEFINE VARIABLE pcLabel    AS CHARACTER NO-UNDO.
  /* DEFINE VARIABLE cLabel    AS CHARACTER EXTENT  NO-UNDO. */
  DEFINE VARIABLE pcKonto    AS CHARACTER NO-UNDO.
  DEFINE VAR      piLoop     AS INT       NO-UNDO.
  DEFINE VAR      pcBank     AS CHAR      NO-UNDO.
  DEFINE VAR      piInt      AS INT       NO-UNDO.
  DEFINE VARIABLE ii         AS INTEGER   NO-UNDO.
  DEFINE VAR      pcTekst    AS CHAR      NO-UNDO.
  DEFINE VARIABLE iKontoPara AS INTEGER   EXTENT 3 NO-UNDO.
  DEFINE VARIABLE cChar      AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cKonto     AS CHARACTER NO-UNDO.

  ASSIGN 
    iKontoPara[1] = 20
    iKontoPara[2] = 21
    iKontoPara[3] = 11.

  IF CAN-DO("SE,SVE",cSprak) THEN 
  DO:
    /*     ASSIGN cLabel[1] = */
    /*            cLabel[2] = */
    ASSIGN
      pcOverskr = "Text" + CHR(1) + 
                  "Konto" + CHR(1) + 
                  "Belopp" + CHR(1) +
                  "Mva"   + CHR(1) +
                  "Belopp u/mva"
      pcLabel   = "Utbetalt" + CHR(1) +
                  "Tillgodokvitton ut" + CHR(1) + 
                  "Presentkort ut" + CHR(1) + 
                  "Deposition ut"
      pcKonto   = "K 0000  0%" + CHR(1) +
                  "K 3000 24%" + chr(1) +
                  "K 3001 12%" + chr(1) +
                  "K 0000  0%" + chr(1) +
                  "K 0000  0%" + chr(1) +
                  "K 0000  0%" + chr(1) +
                  "K 0000  0%" + chr(1) +
                  "K 0000  0%" + chr(1) +
                  "K 0000  0%" + chr(1) +
                  "K 0000  0%"
      pcBank    = "D 2380"
      .
  END.
  ELSE 
  DO:
    ASSIGN
      pcOverskr = "Tekst" + CHR(1) + 
                  "Konto" + CHR(1) + 
                  "Bel�p" + CHR(1) +
                  "Mva"   + CHR(1) +
                  "Bel�p u/mva"
      pcLabel   = "Utbetalt" + CHR(1) +
                  "Tillgodesedler ut" + CHR(1) + 
                  "Gavekort ut" + CHR(1) + 
                  "Deponering ut"
      pcKonto   = "K 0000  0%" + CHR(1) +
                  "K 3000 24%" + chr(1) +
                  "K 3001 12%" + chr(1) +
                  "K 0000  0%" + chr(1) +
                  "K 0000  0%" + chr(1) +
                  "K 0000  0%" + chr(1) +
                  "K 0000  0%" + chr(1) +
                  "K 0000  0%" + chr(1) +
                  "K 0000  0%" + chr(1) +
                  "K 0000  0%"
      pcBank    = "D 2380"
      .
  END.                                                                               

  /*    IF pcRappType <> '99' THEN*/
  DO:
    dY = dY - 24.
    RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold",8).
    RUN pdf_text_xy_dec ("Spdf",ENTRY(1,pcLabel,CHR(1)),dColPosBF[1],dY).
    dY = dY - 4.
    RUN pdf_line IN h_PDFinc  ("Spdf", pdf_LeftMargin ("Spdf"), dY, dColPosBF[2] - 15 ,dY, 0.5).
    RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica",8).
      
    /* Tilgodesedler ut */
    dY = dY - 14.
    RUN settTekstOgKonto(tmpKas_Rap.Butikk,69,1,tmpKas_Rap.TilgodeUt,'K',OUTPUT cChar, OUTPUT cKonto). 
    RUN pdf_text_xy_dec ("Spdf",(cChar),dColPosBF[1],dY).
    RUN pdf_text_xy_dec ("Spdf",cKonto,dColPosBF[2],dY).
    cBelopp = TRIM(STRING(STRING(tmpKas_Rap.TilgodeUt,"->>>,>>>,>>9.99"))).
    RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPosBF[3] - bredd(cBelopp),dY).
    RUN pdf_text_xy_dec ("Spdf","  -",dColPosBF[3],dY).
  END.
  DO:
    iLinjeNr = iLinjeNr + 1.
    RUN settTekstOgKonto(tmpKas_Rap.Butikk,69,1,tmpKas_Rap.TilgodeUt,'K',OUTPUT cChar, OUTPUT cKonto). 
    CREATE ttEksport.
    ASSIGN 
      ttEksport.ButNr       = tmpKas_Rap.Butikk
      ttEksport.ButNamn     = cButikkTxt
      ttEksport.Dato        = pdFraDato
      ttEksport.LinjeNr     = iLinjeNr
      ttEksport.Tekst       = cChar
      ttEksport.KontoPrefix = ENTRY(1,cKonto,' ')
      ttEksport.KontoNr     = INT(ENTRY(2,cKonto,' '))
      ttEksport.Belop       = tmpKas_Rap.TilgodeUt * -1
      .
    RUN opprettBokforingsvisning(2,"UTBETALT",0,0).
    iLinjeNr = iLinjeNr + 1.
    RUN opprettBokforingsvisning(1,"",69,0).
  /* GURRE */
  END.

  /* Gavekort ut */
  /*    IF pcRappType <> '99' THEN*/
  DO:
    dY = dY - 14.
    RUN settTekstOgKonto(tmpKas_Rap.Butikk,134,1,tmpKas_Rap.GavekortUt - tmpKas_Rap.GavekortRabatt,'K',OUTPUT cChar, OUTPUT cKonto). 
    RUN pdf_text_xy_dec ("Spdf",(cChar),dColPosBF[1],dY).
    RUN pdf_text_xy_dec ("Spdf",cKonto,dColPosBF[2],dY).
    cBelopp = TRIM(STRING(STRING(tmpKas_Rap.GavekortUt - tmpKas_Rap.GavekortRabatt,"->>>,>>>,>>9.99"))).
    RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPosBF[3] - bredd(cBelopp),dY).
    RUN pdf_text_xy_dec ("Spdf","  -",dColPosBF[3],dY).
  END.
  DO:
    iLinjeNr = iLinjeNr + 1.
    RUN settTekstOgKonto(tmpKas_Rap.Butikk,134,1,tmpKas_Rap.GavekortUt - tmpKas_Rap.GavekortRabatt,'K',OUTPUT cChar, OUTPUT cKonto). 
    CREATE ttEksport.
    ASSIGN 
      ttEksport.ButNr       = tmpKas_Rap.Butikk
      ttEksport.ButNamn     = cButikkTxt
      ttEksport.Dato        = pdFraDato
      ttEksport.LinjeNr     = iLinjeNr
      ttEksport.Tekst       = cChar
      ttEksport.KontoPrefix = ENTRY(1,cKonto,' ')
      ttEksport.KontoNr     = INT(ENTRY(2,cKonto,' '))
      ttEksport.Belop       = (tmpKas_Rap.GavekortUt - tmpKas_Rap.GavekortRabatt) * -1
      .
    RUN opprettBokforingsvisning(1,"",134,1).

  END.
    
  /* Depositum ut */
  IF tmpKas_Rap.Layaway_Ut <> 0 THEN
  DO:
    dY = dY - 14.
    RUN settTekstOgKonto(tmpKas_Rap.Butikk,900,23,tmpKas_Rap.Layaway_Ut,'K',OUTPUT cChar, OUTPUT cKonto). 
    RUN pdf_text_xy_dec ("Spdf",(cChar),dColPosBF[1],dY).
    RUN pdf_text_xy_dec ("Spdf",cKonto,dColPosBF[2],dY).
    cBelopp = TRIM(STRING(STRING(tmpKas_Rap.Layaway_Ut,"->>>,>>>,>>9.99"))).
    RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPosBF[3] - bredd(cBelopp),dY).
    RUN pdf_text_xy_dec ("Spdf","  -",dColPosBF[3],dY).
    iLinjeNr = iLinjeNr + 1.
    RUN settTekstOgKonto(tmpKas_Rap.Butikk,900,23,tmpKas_Rap.Layaway_Ut,'K',OUTPUT cChar, OUTPUT cKonto). 
    CREATE ttEksport.
    ASSIGN 
      ttEksport.ButNr       = tmpKas_Rap.Butikk
      ttEksport.ButNamn     = cButikkTxt
      ttEksport.Dato        = pdFraDato
      ttEksport.LinjeNr     = iLinjeNr
      ttEksport.Tekst       = cChar
      ttEksport.KontoPrefix = ENTRY(1,cKonto,' ')
      ttEksport.KontoNr     = INT(ENTRY(2,cKonto,' '))
      ttEksport.Belop       = tmpKas_Rap.Layaway_Ut
      .
    RUN opprettBokforingsvisning(1,"",900,0).
  END.
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Saml_3_Detaljspec) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Saml_3_Detaljspec Procedure 
PROCEDURE Saml_3_Detaljspec :
  /*------------------------------------------------------------------------------
    Purpose:     
    Parameters:  <none>
    Notes:       
  ------------------------------------------------------------------------------*/
  DEFINE INPUT-OUTPUT PARAMETER dY AS DECIMAL     NO-UNDO.
  DEFINE VARIABLE cOverskr     AS CHARACTER EXTENT 7 NO-UNDO.
  DEFINE VARIABLE cLabel       AS CHARACTER EXTENT 2 NO-UNDO.
  DEFINE VARIABLE cTxt         AS CHARACTER NO-UNDO.
  DEFINE VARIABLE dSum_pos     AS DECIMAL   NO-UNDO.
  DEFINE VARIABLE dSum_neg     AS DECIMAL   NO-UNDO.
  DEFINE VARIABLE dColPos_S3   AS DECIMAL   EXTENT 5 NO-UNDO.
  DEFINE VARIABLE dULstart_S3  AS DECIMAL   EXTENT 5 NO-UNDO.
  DEFINE VARIABLE cTjensterTxt AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cKupongTxt   AS CHAR      NO-UNDO.
  DEFINE VARIABLE dSum         AS DECIMAL   NO-UNDO.
  DEFINE VARIABLE dSum1        AS DECIMAL   NO-UNDO.
  DEFINE VARIABLE dSum2        AS DECIMAL   NO-UNDO.
  DEFINE VARIABLE cSumTxt      AS CHARACTER EXTENT 3 NO-UNDO.

  IF CAN-DO("SE,SVE",cSprak) THEN 
  DO:
    ASSIGN 
      cOverskr[1]  = "Nonsale"
      cOverskr[2]  = "Plu"
      cOverskr[3]  = "Text"
      cOverskr[4]  = "Antal"
      cOverskr[5]  = "Belopp"
      cOverskr[6]  = "Utnyttjad belopp"
      cOverskr[7]  = "Kupong belopp"
      cLabel[1]    = "Totalt positiv nonsale"
      cLabel[2]    = "Totalt negativ nonsale"
      cTjensterTxt = "Tj�nster avd " + STRING(iTjHG) + " per varugrupp (Vgr)"
      cKupongTxt   = "Kupong"
      cSumTxt[1]   = "Totalt tj�nster utan moms"
      cSumTxt[2]   = "Totalt tj�nster med moms"
      cSumTxt[3]   = "Totalt kupong"
      .
  END.
  ELSE 
  DO:
    ASSIGN 
      cOverskr[1]  = "Nonsale"
      cOverskr[2]  = "Plu"
      cOverskr[3]  = "Tekst"
      cOverskr[4]  = "Antall"
      cOverskr[5]  = "Bel�p"
      cOverskr[6]  = "Utnyttet bel�p"
      cOverskr[7]  = "Kupong bel�p"
      cLabel[1]    = "Totalt positiv nonsale"
      cLabel[2]    = "Totalt negativ nonsale"
      cTjensterTxt = "Tjenester avd " + STRING(iTjHG) + " per varegruppe (Vgr)"
      cKupongTxt   = "Kupong"
      cSumTxt[1]   = "Totalt tjenester uten moms"
      cSumTxt[2]   = "Totalt tjenester med moms"
      cSumTxt[3]   = "Totalt kuponger"
      .
  END.
  ASSIGN 
    dColPos_S3[1] = pdf_LeftMargin ("Spdf") + 20
    dColPos_S3[2] = dColPos_S3[1] + 10
    dColPos_S3[3] = 340
    dColPos_S3[4] = 420
    dColPos_S3[5] = 500
    .
  ASSIGN 
    dULstart_S3[1] = pdf_LeftMargin ("Spdf")
    dULstart_S3[2] = dColPos_S3[2]
    dULstart_S3[3] = 290
    dULstart_S3[4] = 350
    dULstart_S3[5] = 430
    .

  /* nonsale */
  IF CAN-FIND(FIRST tt_NON_sale_spes) THEN 
  DO:
    /* RUBRIK */
    RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold",12).
    RUN pdf_text_xy_dec ("Spdf",cOverskr[1],dULStart_S3[1],dY).
    dY = dY - 4.
    RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica",8).
    RUN pdf_line IN h_PDFinc  ("Spdf", pdf_LeftMargin ("Spdf"), dY, dULStart_S3[1], dY, 0.5).
    dY = dY - iLineSpace.
    /* Kolonnrubrik */
    RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold",8).
    RUN pdf_text_xy_dec ("Spdf",cOverskr[2],dULStart_S3[1],dY).
    RUN pdf_text_xy_dec ("Spdf",cOverskr[3],dULStart_S3[2],dY).
    RUN pdf_text_xy_dec ("Spdf",cOverskr[4],dColPos_S3[3] - bredd(cOverskr[4]),dY).
    RUN pdf_text_xy_dec ("Spdf",cOverskr[5],dColPos_S3[4] - bredd(cOverskr[5]),dY).
    dY = dY - 4.
    RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica",8).
    RUN pdf_line IN h_PDFinc  ("Spdf", dULstart_S3[1], dY, dColPos_S3[1], dY, 0.5).
    RUN pdf_line IN h_PDFinc  ("Spdf", dULstart_S3[2], dY, dULStart_S3[3] - 10, dY, 0.5).
    RUN pdf_line IN h_PDFinc  ("Spdf", dULstart_S3[3], dY, dColPos_S3[3], dY, 0.5).
    RUN pdf_line IN h_PDFinc  ("Spdf", dULstart_S3[4], dY, dColPos_S3[4], dY, 0.5).
    
    FOR EACH tt_NON_sale_spes BY tt_NON_Sale_Spes.kode:
      IF tt_NON_Sale_spes.Non_Sale_Type = 1 THEN
        ASSIGN dSum_pos = dSum_pos + tt_NON_Sale_Spes.NON_SaleVerdi.
      ELSE
        ASSIGN dSum_neg = dSum_neg + tt_NON_Sale_Spes.NON_SaleVerdi.
      RELEASE artbas.
      FIND strekkode WHERE strekkode.kode = tt_NON_Sale_Spes.Kode NO-LOCK NO-ERROR.
      IF AVAIL strekkode THEN
        FIND artbas OF strekkode NO-LOCK NO-ERROR.
      dY = dY - iLineSpace.
      cTxt = IF AVAIL artbas THEN artbas.beskr ELSE "--".
      RUN pdf_text_xy_dec ("Spdf",tt_NON_Sale_Spes.Kode,dColPos_S3[1] - bredd(tt_NON_Sale_Spes.Kode),dY).
      RUN pdf_text_xy_dec ("Spdf",cTxt,dColPos_S3[2],dY).
      cBelopp = TRIM(STRING(tt_NON_Sale_Spes.NON_SaleAntall,"->,>>9")).
      RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPos_S3[3] - bredd(cBelopp),dY).
      cBelopp = TRIM(STRING(tt_NON_Sale_Spes.NON_SaleVerdi,"->>>,>>>,>>9.99")).
      RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPos_S3[4] - bredd(cBelopp),dY).
    END.
    dY = dY - 4.
    RUN pdf_line IN h_PDFinc  ("Spdf", dULstart_S3[4], dY, dColPos_S3[4], dY, 0.5).
    IF dSUm_pos <> 0 THEN 
    DO:
      dY = dY - iLineSpace.
      RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold",8).
      RUN pdf_text_xy_dec ("Spdf",cLabel[1],dULStart_S3[1],dY).
      cBelopp = TRIM(STRING(dSum_pos,"->>>,>>>,>>9.99")).
      RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPos_S3[4] - bredd(cBelopp),dY).
    END.
    IF dSum_neg <> 0 THEN 
    DO:
      dY = dY - iLineSpace.
      RUN pdf_text_xy_dec ("Spdf",cLabel[2],dULStart_S3[1],dY).
      cBelopp = TRIM(STRING(dSum_neg,"->>>,>>>,>>9.99")).
      RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPos_S3[4] - bredd(cBelopp),dY).
    END.
    dY = dY - 24.
  END. /* nonsale */
    
  IF CAN-FIND(FIRST TT_vg) THEN
  VGSPES:
  DO:
    /* RUBRIK */
    RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold",12).
    RUN pdf_text_xy_dec ("Spdf",cTjensterTxt,dULStart_S3[1],dY).
    dY = dY - 4.
    RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica",8).
    RUN pdf_line IN h_PDFinc  ("Spdf", pdf_LeftMargin ("Spdf"), dY, dULStart_S3[1], dY, 0.5).
    dY = dY - iLineSpace.
    /* Kolonnrubrik */
    RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold",8).
    RUN pdf_text_xy_dec ("Spdf","Vgr",dULStart_S3[1],dY).
    RUN pdf_text_xy_dec ("Spdf",cOverskr[3],dULStart_S3[2],dY).
    RUN pdf_text_xy_dec ("Spdf",cOverskr[4],dColPos_S3[3] - bredd(cOverskr[4]),dY).
    RUN pdf_text_xy_dec ("Spdf",cOverskr[5],dColPos_S3[4] - bredd(cOverskr[5]),dY).
    dY = dY - 4.
    RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica",8).
    RUN pdf_line IN h_PDFinc  ("Spdf", dULstart_S3[1], dY, dColPos_S3[1], dY, 0.5).
    RUN pdf_line IN h_PDFinc  ("Spdf", dULstart_S3[2], dY, dULStart_S3[3] - 10, dY, 0.5).
    RUN pdf_line IN h_PDFinc  ("Spdf", dULstart_S3[3], dY, dColPos_S3[3], dY, 0.5).
    RUN pdf_line IN h_PDFinc  ("Spdf", dULstart_S3[4], dY, dColPos_S3[4], dY, 0.5).
    IF CAN-FIND(FIRST TT_Vg WHERE tt_vg.mva = FALSE AND TT_Vg.belopp <> 0) THEN 
    DO:
      FOR EACH TT_Vg WHERE tt_vg.mva = FALSE AND TT_Vg.belopp <> 0: 
        dSum = dSum + tt_vg.belopp.
        dY = dY - iLineSpace.
        RUN pdf_text_xy_dec ("Spdf",STRING(tt_vg.vg),dColPos_S3[1] - bredd(STRING(tt_vg.vg)),dY).
        RUN pdf_text_xy_dec ("Spdf",tt_vg.txt,dColPos_S3[2],dY).
        cBelopp = TRIM(STRING(tt_vg.antal,"->,>>9")).
        RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPos_S3[3] - bredd(cBelopp),dY).
        cBelopp = TRIM(STRING(tt_vg.belopp,"->>>,>>>,>>9.99")).
        RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPos_S3[4] - bredd(cBelopp),dY).
      END.
      IF dSum <> 0 THEN 
      DO:
        dY = dY - 4.
        RUN pdf_line IN h_PDFinc  ("Spdf", dULstart_S3[4], dY, dColPos_S3[4], dY, 0.5).
        dY = dY - iLineSpace.
        RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold",8).
        RUN pdf_text_xy_dec ("Spdf",cSumTxt[1],dULStart_S3[1],dY).
        cBelopp = TRIM(STRING(dSum,"->>>,>>>,>>9.99")).
        RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPos_S3[4] - bredd(cBelopp),dY).
        dSum = 0.
        dY = dY - iLineSpace.
      END.
    END.
    RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica",8).
    IF CAN-FIND(FIRST TT_Vg WHERE tt_vg.mva = TRUE AND tt_Vg.belopp <> 0) THEN 
    DO:
      FOR EACH TT_Vg WHERE tt_vg.mva = TRUE AND TT_Vg.belopp <> 0: 
        dSum = dSum + tt_vg.belopp.
        dY = dY - iLineSpace.
        RUN pdf_text_xy_dec ("Spdf",STRING(tt_vg.vg),dColPos_S3[1] - bredd(STRING(tt_vg.vg)),dY).
        RUN pdf_text_xy_dec ("Spdf",tt_vg.txt,dColPos_S3[2],dY).
        cBelopp = TRIM(STRING(tt_vg.antal,"->,>>9")).
        RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPos_S3[3] - bredd(cBelopp),dY).
        cBelopp = TRIM(STRING(tt_vg.belopp,"->>>,>>>,>>9.99")).
        RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPos_S3[4] - bredd(cBelopp),dY).
      END.
      IF dSum <> 0 THEN 
      DO:
        dY = dY - 4.
        RUN pdf_line IN h_PDFinc  ("Spdf", dULstart_S3[4], dY, dColPos_S3[4], dY, 0.5).
        dY = dY - iLineSpace.
        RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold",8).
        RUN pdf_text_xy_dec ("Spdf",cSumTxt[2],dULStart_S3[1],dY).
        cBelopp = TRIM(STRING(dSum,"->>>,>>>,>>9.99")).
        RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPos_S3[4] - bredd(cBelopp),dY).
        dY = dY - iLineSpace.
      END.
    END.
  END. /* VGSPES*/

  /* ----------------- */
  IF CAN-FIND(FIRST TT_KTL) THEN
  KUPONGSPES:
  DO:
    dSum1 = 0.
    dSum2 = 0.
    dY = dY - 4.
    /* RUBRIK */
    RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold",12).
    RUN pdf_text_xy_dec ("Spdf",cKupongTxt,dULStart_S3[1],dY).
    dY = dY - 4.
    RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica",8).
    RUN pdf_line IN h_PDFinc  ("Spdf", pdf_LeftMargin ("Spdf"), dY, dULStart_S3[1], dY, 0.5).
    dY = dY - iLineSpace.
    /* Kolonnrubrik */
    RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold",8).
    RUN pdf_text_xy_dec ("Spdf","KTyp",dULStart_S3[1],dY).
    RUN pdf_text_xy_dec ("Spdf",cOverskr[3],dULStart_S3[2],dY).
    RUN pdf_text_xy_dec ("Spdf",cOverskr[4],dColPos_S3[3] - bredd(cOverskr[4]),dY).
    RUN pdf_text_xy_dec ("Spdf",cOverskr[6],dColPos_S3[4] - bredd(cOverskr[6]),dY).
    RUN pdf_text_xy_dec ("Spdf",cOverskr[7],dColPos_S3[5] - bredd(cOverskr[6]),dY).
    dY = dY - 4.
    /* Strek over kolonnene */
    RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica",8).
    RUN pdf_line IN h_PDFinc  ("Spdf", dULstart_S3[1], dY, dColPos_S3[1], dY, 0.5).
    RUN pdf_line IN h_PDFinc  ("Spdf", dULstart_S3[2], dY, dULStart_S3[3] - 10, dY, 0.5).
    RUN pdf_line IN h_PDFinc  ("Spdf", dULstart_S3[3], dY, dColPos_S3[3], dY, 0.5).
    RUN pdf_line IN h_PDFinc  ("Spdf", dULstart_S3[4], dY, dColPos_S3[4], dY, 0.5).
    RUN pdf_line IN h_PDFinc  ("Spdf", dULstart_S3[5], dY, dColPos_S3[5], dY, 0.5).

    /*Legger ut en sumlinje pr. kupongtype. */
    FOR EACH TT_KTL: 
      dSum1 = dSum1 + TT_KTL.BelopKnd.
      dSum2 = dSum2 + TT_KTL.BelopBut.
      dY = dY - iLineSpace.
      RUN pdf_text_xy_dec ("Spdf",STRING(TT_KTL.KTypeNr),dColPos_S3[1] - bredd(STRING(TT_KTL.KTypeNr)),dY).
      RUN pdf_text_xy_dec ("Spdf",TT_KTL.KupBeskrivelse,dColPos_S3[2],dY).
      cBelopp = TRIM(STRING(TT_KTL.Antall,"->,>>9")).
      RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPos_S3[3] - bredd(cBelopp),dY).
      cBelopp = TRIM(STRING(TT_KTL.BelopKnd,"->>>,>>>,>>9.99")).
      RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPos_S3[4] - bredd(cBelopp),dY).
      cBelopp = TRIM(STRING(TT_KTL.BelopBut,"->>>,>>>,>>9.99")).
      RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPos_S3[5] - bredd(cBelopp),dY).
    END.
        
    IF dSum1 <> 0 OR dSum2 <> 0 THEN 
    DO:
      dY = dY - 4.
      RUN pdf_line IN h_PDFinc  ("Spdf", dULstart_S3[4], dY, dColPos_S3[4], dY, 0.5).
      RUN pdf_line IN h_PDFinc  ("Spdf", dULstart_S3[5], dY, dColPos_S3[5], dY, 0.5).
      dY = dY - iLineSpace.
      RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold",8).
      RUN pdf_text_xy_dec ("Spdf",cSumTxt[3],dULStart_S3[1],dY).
      cBelopp = TRIM(STRING(dSum1,"->>>,>>>,>>9.99")).
      RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPos_S3[4] - bredd(cBelopp),dY).
      cBelopp = TRIM(STRING(dSum2,"->>>,>>>,>>9.99")).
      RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPos_S3[5] - bredd(cBelopp),dY).
      dSum1 = 0.
      dSum2 = 0.
      dY = dY - iLineSpace.
    END.
  END. /* KUPONGSPES */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Saml_4_Kont_hg) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Saml_4_Kont_hg Procedure 
PROCEDURE Saml_4_Kont_hg :
  /*------------------------------------------------------------------------------
    Purpose:     
    Parameters:  <none>
    Notes:       
  ------------------------------------------------------------------------------*/
  DEFINE INPUT-OUTPUT PARAMETER dY AS DECIMAL     NO-UNDO.
  DEFINE VARIABLE cOverskr     AS CHARACTER EXTENT 4 NO-UNDO.
  DEFINE VARIABLE cLabel       AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cTxt         AS CHARACTER NO-UNDO.
  DEFINE VARIABLE dSum         AS DECIMAL   NO-UNDO.
  DEFINE VARIABLE dColPos_S4   AS DECIMAL   EXTENT 3 NO-UNDO.
  DEFINE VARIABLE dULstart_S4  AS DECIMAL   EXTENT 3 NO-UNDO.
  DEFINE VARIABLE dDato        AS DATE      NO-UNDO.
  DEFINE VARIABLE dFgAr31Dec   AS DATE      NO-UNDO.
  DEFINE VARIABLE iAarperlinnr AS INTEGER   NO-UNDO.
  DEFINE VARIABLE iPerLoop     AS INTEGER   NO-UNDO.
  IF CAN-DO("SE,SVE",cSprak) THEN 
  DO:
    ASSIGN 
      cOverskr[1] = "Huvudgruppsredovisning"
      cOverskr[2] = "Hgr"
      cOverskr[3] = "Text"
      cOverskr[4] = "Belopp u moms"
      cLabel      = "Totalt".
  END.
  ELSE 
  DO:
    ASSIGN 
      cOverskr[1] = "Hovedgrupperegnskap"
      cOverskr[2] = "Hgr"
      cOverskr[3] = "Tekst"
      cOverskr[4] = "Bel�p u mva"
      clabel      = "Totalt".
  END.
  ASSIGN 
    dColPos_S4[1] = pdf_LeftMargin ("Spdf") + 15
    dColPos_S4[2] = dColPos_S4[1] + 10
    dColPos_S4[3] = 320.
  ASSIGN 
    dULstart_S4[1] = pdf_LeftMargin ("Spdf")
    dULstart_S4[2] = dColPos_S4[2]
    dULstart_S4[3] = 250.

  /* 
    
     FI-Butiker              = STRING(iButik)
     pdFraDato = dRappDat
     pdTilDato = dRappDat
  pdFraDato
  pdTilDato
*/
  DO dDato = pdFraDato TO pdTilDato:
    dFgAr31Dec = DATE(12,31,YEAR(dDato) - 1).
    iAarperlinnr = INT(STRING(YEAR(dDato),"9999") + STRING(dDato - dFgAr31Dec,"999")). 
    FOR EACH Stlinje WHERE Stlinje.butik = piButNr AND Stlinje.Sttypeid = "HOVEDGR" AND Stlinje.PerId = "DAG" AND
      StLinje.Aarperlinnr = iAarperlinnr NO-LOCK:
      FIND TT_KontHg WHERE TT_KontHg.Hg = INT(StLinje.Dataobjekt) NO-ERROR.
      IF NOT AVAIL TT_KontHg THEN 
      DO:
        FIND HuvGr WHERE HuvGr.Hg = INT(StLinje.Dataobjekt) NO-LOCK NO-ERROR.
        CREATE TT_KontHg.
        ASSIGN 
          TT_kontHg.Hg  = INT(StLinje.Dataobjekt)
          TT_kontHg.Txt = IF AVAIL HuvGr THEN Huvgr.hgbeskr ELSE "---".
      END.
      TT_KontHg.Bel = TT_kontHg.Bel + StLinje.VerdiSolgt.
    END.
  END.

  /* RUBRIK */
  RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold",12).
  RUN pdf_text_xy_dec ("Spdf",cOverskr[1],dULStart_S4[1],dY).
  dY = dY - 4.
  RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica",8).
  RUN pdf_line IN h_PDFinc  ("Spdf", pdf_LeftMargin ("Spdf"), dY, dULStart_S4[1], dY, 0.5).
  dY = dY - iLineSpace.
  /* Kolonnrubrik */
  RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold",8).
  RUN pdf_text_xy_dec ("Spdf",cOverskr[2],dULStart_S4[1],dY).
  RUN pdf_text_xy_dec ("Spdf",cOverskr[3],dULStart_S4[2],dY).
  RUN pdf_text_xy_dec ("Spdf",cOverskr[4],dColPos_S4[3] - bredd(cOverskr[4]),dY).
  dY = dY - 4.
  RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica",8).
  RUN pdf_line IN h_PDFinc  ("Spdf", dULstart_S4[1], dY, dColPos_S4[1], dY, 0.5).
  RUN pdf_line IN h_PDFinc  ("Spdf", dULstart_S4[2], dY, dULStart_S4[3] - 10, dY, 0.5).
  RUN pdf_line IN h_PDFinc  ("Spdf", dULstart_S4[3], dY, dColPos_S4[3], dY, 0.5).
  FOR EACH TT_KontHg:
  END.
  FOR EACH TT_KontHg:
    dSum = dSum + TT_KontHg.Bel.
    dY = dY - iLineSpace.
    cTxt = STRING(TT_KontHg.Hg).
    RUN pdf_text_xy_dec ("Spdf",cTxt,dColPos_S4[1] - bredd(ctxt),dY).
    RUN pdf_text_xy_dec ("Spdf",TT_KontHg.Txt,dColPos_S4[2],dY).
    cBelopp = TRIM(STRING(TT_KontHg.Bel,"->>>,>>>,>>9.99")).
    RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPos_S4[3] - bredd(cBelopp),dY).
  END.
  dY = dY - 4.
  RUN pdf_line IN h_PDFinc  ("Spdf", dULstart_S4[3], dY, dColPos_S4[3], dY, 0.5).
  dY = dY - iLineSpace.
  RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold",8).
  RUN pdf_text_xy_dec ("Spdf",cLabel,dULStart_S4[1],dY).
  cBelopp = TRIM(STRING(dSum,"->>>,>>>,>>9.99")).
  RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPos_S4[3] - bredd(cBelopp),dY).
/* line */
/*     dY = dY - 4.                                                                   */
/*     RUN pdf_line IN h_PDFinc  ("Spdf", dColPos_S4[1], dY, dColPos_S4[3], dY, 0.5). */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Saml_5_DetalArtHG13) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Saml_5_DetalArtHG13 Procedure 
PROCEDURE Saml_5_DetalArtHG13 :
  /*------------------------------------------------------------------------------
    Purpose:     
    Parameters:  <none>
    Notes:       
  ------------------------------------------------------------------------------*/
  DEFINE INPUT-OUTPUT PARAMETER dY AS DECIMAL     NO-UNDO.
  DEFINE VARIABLE cOverskr     AS CHARACTER EXTENT 5 NO-UNDO.
  DEFINE VARIABLE cLabel       AS CHARACTER EXTENT 2 NO-UNDO.
  DEFINE VARIABLE cTxt         AS CHARACTER NO-UNDO.
  DEFINE VARIABLE dSum_pos     AS DECIMAL   NO-UNDO.
  DEFINE VARIABLE dSum_neg     AS DECIMAL   NO-UNDO.
  DEFINE VARIABLE dColPos_S3   AS DECIMAL   EXTENT 4 NO-UNDO.
  DEFINE VARIABLE dULstart_S3  AS DECIMAL   EXTENT 4 NO-UNDO.
  DEFINE VARIABLE cTjensterTxt AS CHARACTER NO-UNDO.
  DEFINE VARIABLE dSum         AS DECIMAL   NO-UNDO.
  DEFINE VARIABLE cSumTxt      AS CHARACTER EXTENT 2 NO-UNDO.
  IF CAN-DO("SE,SVE",cSprak) THEN 
  DO:
    ASSIGN 
      cOverskr[1]  = "Nonsale"
      cOverskr[2]  = "Plu"
      cOverskr[3]  = "Text"
      cOverskr[4]  = "Antal"
      cOverskr[5]  = "Belopp"
      cLabel[1]    = "Totalt positiv nonsale"
      cLabel[2]    = "Totalt negativ nonsale"
      cTjensterTxt = "Tj�nster avd 13 per artikel"
      cSumTxt[1]   = "Totalt tj�nster utan moms"
      cSumTxt[2]   = "Totalt tj�nster med moms".
  END.
  ELSE 
  DO:
    ASSIGN 
      cOverskr[1]  = "Nonsale"
      cOverskr[2]  = "Plu"
      cOverskr[3]  = "Tekst"
      cOverskr[4]  = "Antall"
      cOverskr[5]  = "Bel�p"
      cLabel[1]    = "Totalt positiv nonsale"
      cLabel[2]    = "Totalt negativ nonsale"
      cTjensterTxt = "Tjenester avd 13 per artikkel"
      cSumTxt[1]   = "Totalt tjenester uten moms"
      cSumTxt[2]   = "Totalt tjenester med moms".
  END.
  ASSIGN 
    dColPos_S3[1] = pdf_LeftMargin ("Spdf") + 50
    dColPos_S3[2] = dColPos_S3[1] + 10
    dColPos_S3[3] = 340
    dColPos_S3[4] = 420.
  ASSIGN 
    dULstart_S3[1] = pdf_LeftMargin ("Spdf")
    dULstart_S3[2] = dColPos_S3[2]
    dULstart_S3[3] = 290
    dULstart_S3[4] = 350.

  IF NOT CAN-FIND(FIRST tt_ArtHG) THEN
    RETURN.
  /* RUBRIK */
  RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold",12).
  RUN pdf_text_xy_dec ("Spdf",cTjensterTxt,dULStart_S3[1],dY).
  dY = dY - 4.
  RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica",8).
  RUN pdf_line IN h_PDFinc  ("Spdf", pdf_LeftMargin ("Spdf"), dY, dULStart_S3[1], dY, 0.5).
  dY = dY - iLineSpace.
  /* Kolonnrubrik */
  RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold",8).
  RUN pdf_text_xy_dec ("Spdf","Artnr",dULStart_S3[1],dY).
  RUN pdf_text_xy_dec ("Spdf",cOverskr[3],dULStart_S3[2],dY).
  RUN pdf_text_xy_dec ("Spdf",cOverskr[4],dColPos_S3[3] - bredd(cOverskr[4]),dY).
  RUN pdf_text_xy_dec ("Spdf",cOverskr[5],dColPos_S3[4] - bredd(cOverskr[5]),dY).
  dY = dY - 4.
  RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica",8).
  RUN pdf_line IN h_PDFinc  ("Spdf", dULstart_S3[1], dY, dColPos_S3[1], dY, 0.5).
  RUN pdf_line IN h_PDFinc  ("Spdf", dULstart_S3[2], dY, dULStart_S3[3] - 10, dY, 0.5).
  RUN pdf_line IN h_PDFinc  ("Spdf", dULstart_S3[3], dY, dColPos_S3[3], dY, 0.5).
  RUN pdf_line IN h_PDFinc  ("Spdf", dULstart_S3[4], dY, dColPos_S3[4], dY, 0.5).
  IF CAN-FIND(FIRST tt_ArtHG WHERE tt_ArtHG.mva = FALSE AND tt_ArtHG.belopp <> 0) THEN 
  DO:
    FOR EACH tt_ArtHG WHERE tt_ArtHG.mva = FALSE AND tt_ArtHG.belopp <> 0: 
      dSum = dSum + tt_ArtHG.belopp.
      dY = dY - iLineSpace.
      RUN pdf_text_xy_dec ("Spdf",STRING(LEFT-TRIM(tt_ArtHG.artobjekt,"0")),dColPos_S3[1] - bredd(STRING(LEFT-TRIM(tt_ArtHG.artobjekt,"0"))),dY).
      RUN pdf_text_xy_dec ("Spdf",tt_ArtHG.txt,dColPos_S3[2],dY).
      cBelopp = TRIM(STRING(tt_ArtHG.antal,"->,>>9")).
      RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPos_S3[3] - bredd(cBelopp),dY).
      cBelopp = TRIM(STRING(tt_ArtHG.belopp,"->>>,>>>,>>9.99")).
      RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPos_S3[4] - bredd(cBelopp),dY).
    END.
    IF dSum <> 0 THEN 
    DO:
      dY = dY - 4.
      RUN pdf_line IN h_PDFinc  ("Spdf", dULstart_S3[4], dY, dColPos_S3[4], dY, 0.5).
      dY = dY - iLineSpace.
      RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold",8).
      RUN pdf_text_xy_dec ("Spdf",cSumTxt[1],dULStart_S3[1],dY).
      cBelopp = TRIM(STRING(dSum,"->>>,>>>,>>9.99")).
      RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPos_S3[4] - bredd(cBelopp),dY).
      dSum = 0.
      dY = dY - iLineSpace.
    END.
  END.
  RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica",8).
  IF CAN-FIND(FIRST tt_ArtHG WHERE tt_ArtHG.mva = TRUE AND tt_ArtHG.belopp <> 0) THEN 
  DO:
    FOR EACH tt_ArtHG WHERE tt_ArtHG.mva = TRUE AND tt_ArtHG.belopp <> 0: 
      dSum = dSum + tt_ArtHG.belopp.
      dY = dY - iLineSpace.
      RUN pdf_text_xy_dec ("Spdf",STRING(LEFT-TRIM(tt_ArtHG.artobjekt,"0")),dColPos_S3[1] - bredd(STRING(LEFT-TRIM(tt_ArtHG.artobjekt,"0"))),dY).
      RUN pdf_text_xy_dec ("Spdf",tt_ArtHG.txt,dColPos_S3[2],dY).
      cBelopp = TRIM(STRING(tt_ArtHG.antal,"->,>>9")).
      RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPos_S3[3] - bredd(cBelopp),dY).
      cBelopp = TRIM(STRING(tt_ArtHG.belopp,"->>>,>>>,>>9.99")).
      RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPos_S3[4] - bredd(cBelopp),dY).
    END.
    IF dSum <> 0 THEN 
    DO:
      dY = dY - 4.
      RUN pdf_line IN h_PDFinc  ("Spdf", dULstart_S3[4], dY, dColPos_S3[4], dY, 0.5).
      dY = dY - iLineSpace.
      RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold",8).
      RUN pdf_text_xy_dec ("Spdf",cSumTxt[2],dULStart_S3[1],dY).
      cBelopp = TRIM(STRING(dSum,"->>>,>>>,>>9.99")).
      RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPos_S3[4] - bredd(cBelopp),dY).
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Saml_6_Detaljspec) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Saml_6_Detaljspec Procedure
PROCEDURE Saml_6_Detaljspec:
  /*------------------------------------------------------------------------------
   Purpose:
   Notes:
  ------------------------------------------------------------------------------*/
  DEFINE INPUT-OUTPUT PARAMETER dY AS DECIMAL     NO-UNDO.
  DEFINE VARIABLE cOverskr     AS CHARACTER EXTENT 6 NO-UNDO.
  DEFINE VARIABLE cLabel       AS CHARACTER EXTENT 2 NO-UNDO.
  DEFINE VARIABLE cTxt         AS CHARACTER NO-UNDO.
  DEFINE VARIABLE dSum_pos     AS DECIMAL   NO-UNDO.
  DEFINE VARIABLE dSum_neg     AS DECIMAL   NO-UNDO.
  DEFINE VARIABLE dColPos_S3   AS DECIMAL   EXTENT 5 NO-UNDO.
  DEFINE VARIABLE dULstart_S3  AS DECIMAL   EXTENT 5 NO-UNDO.
  DEFINE VARIABLE cTjensterTxt AS CHARACTER NO-UNDO.
  DEFINE VARIABLE dSum         AS DECIMAL   NO-UNDO.
  DEFINE VARIABLE cSumTxt      AS CHARACTER EXTENT 2 NO-UNDO.
  DEFINE VARIABLE cOrdreRef    AS CHARACTER NO-UNDO.

  IF CAN-DO("SE,SVE",cSprak) THEN 
  DO:
    ASSIGN 
      cOverskr[1]  = "Ordr/LnNr"
      cOverskr[2]  = "Plu"
      cOverskr[3]  = "Text"
      cOverskr[4]  = "Antal"
      cOverskr[5]  = "Belopp"
      cOverskr[6]  = "Bet. referens"
      cLabel[1]    = "Totalt positiv nonsale"
      cLabel[2]    = "Totalt negativ nonsale"
      cTjensterTxt = "Spesifikasjon pr.ordre/ordrelinje"
      cSumTxt[1]   = "Sum tilbakebetalt"
      .
  END.
  ELSE 
  DO:
    ASSIGN 
      cOverskr[1]  = "Ordr/LnNr"
      cOverskr[2]  = "Plu"
      cOverskr[3]  = "Tekst"
      cOverskr[4]  = "Antall"
      cOverskr[5]  = "Bel�p"
      cOverskr[6]  = "Bet. referanse"
      cLabel[1]    = "Totalt positiv nonsale"
      cLabel[2]    = "Totalt negativ nonsale"
      cTjensterTxt = "Spesifikasjon pr.ordre/ordrelinje"
      cSumTxt[1]   = "Sum tilbakebetalt"
      .
  END.
  ASSIGN 
    dColPos_S3[1] = pdf_LeftMargin ("Spdf") + 50
    /*           dColPos_S3[2] = dColPos_S3[1] + 10*/
    dColPos_S3[2] = pdf_LeftMargin ("Spdf") + 50
    dColPos_S3[3] = 340
    dColPos_S3[4] = 420
    dColPos_S3[5] = 553
    .
  ASSIGN 
    dULstart_S3[1] = pdf_LeftMargin ("Spdf")
    dULstart_S3[2] = pdf_LeftMargin ("Spdf") + 50
    dULstart_S3[3] = 290
    dULstart_S3[4] = 350
    dULstart_S3[5] = 430
    .

  IF NOT CAN-FIND(FIRST KOrdreHode WHERE 
    KOrdreHode.LevStatus = '50' AND 
    DATE(KOrdreHode.ShipmentSendt) >= pdFraDato AND 
    DATE(KOrdreHode.ShipmentSendt) <= pdTilDato) THEN 
    RETURN.
        
  /* RUBRIK */
  RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold",12).
  RUN pdf_text_xy_dec ("Spdf",cTjensterTxt,dULStart_S3[1],dY).
  dY = dY - 4.
  RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica",8).
  RUN pdf_line IN h_PDFinc  ("Spdf", pdf_LeftMargin ("Spdf"), dY, dULStart_S3[1], dY, 0.5).
  dY = dY - iLineSpace.
  /* Kolonnrubrik */
  RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold",8).
  RUN pdf_text_xy_dec ("Spdf",cOverskr[1],dULStart_S3[1],dY).
  RUN pdf_text_xy_dec ("Spdf",cOverskr[3],dULStart_S3[2],dY).
  RUN pdf_text_xy_dec ("Spdf",cOverskr[4],dColPos_S3[3] - bredd(cOverskr[4]),dY).
  RUN pdf_text_xy_dec ("Spdf",cOverskr[5],dColPos_S3[4] - bredd(cOverskr[5]),dY).
  RUN pdf_text_xy_dec ("Spdf",cOverskr[6],dColPos_S3[5] - bredd(cOverskr[6]),dY).
  dY = dY - 4.
  RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica",8).
  RUN pdf_line IN h_PDFinc  ("Spdf", dULstart_S3[1], dY, dColPos_S3[1], dY, 0.5).
  RUN pdf_line IN h_PDFinc  ("Spdf", dULstart_S3[2], dY, dULStart_S3[3] - 10, dY, 0.5).
  RUN pdf_line IN h_PDFinc  ("Spdf", dULstart_S3[3], dY, dColPos_S3[3], dY, 0.5).
  RUN pdf_line IN h_PDFinc  ("Spdf", dULstart_S3[4], dY, dColPos_S3[4], dY, 0.5).
  RUN pdf_line IN h_PDFinc  ("Spdf", dULstart_S3[5], dY, dColPos_S3[5], dY, 0.5).
    
  DO:
    FOR EACH KOrdreHode NO-LOCK WHERE 
      KOrdreHode.LevStatus = '50' AND 
      DATE(KOrdreHode.ShipmentSendt) >= pdFraDato AND 
      DATE(KOrdreHode.ShipmentSendt) <= pdTilDato,
      EACH KOrdreLinje OF KOrdreHode WHERE 
      KOrdreLinje.BetRef > '':
              
      IF KOrdreLinje.VareNr = 'BETALT' THEN 
        NEXT.
      IF KOrdreLinje.Linjesum = KOrdreLinje.OrgLinjeSum THEN 
        NEXT.
                  
      ASSIGN 
        dSum      = dSum + (KOrdreLinje.OrgLinjeSum - KOrdreLinje.Linjesum)
        dY        = dY - iLineSpace
        cOrdreRef = TRIM(KOrdreHode.EkstOrdreNr + '/' + STRING(KOrdreLinje.KOrdreLinjeNr)) 
        .
      RUN pdf_text_xy_dec ("Spdf",cOrdreRef,dColPos_S3[1] - bredd(cOrdreRef),dY).
      RUN pdf_text_xy_dec ("Spdf",KOrdreLinje.Varetekst,dColPos_S3[2],dY).
      cBelopp = TRIM(STRING(KOrdreLinje.Antall,"->,>>9")).
      RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPos_S3[3] - bredd(cBelopp),dY).
      cBelopp = TRIM(STRING(KOrdreLinje.OrgLinjesum - KOrdreLinje.Linjesum,"->>>,>>>,>>9.99")).
      RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPos_S3[4] - bredd(cBelopp),dY).
      RUN pdf_text_xy_dec ("Spdf",SUBSTRING(KOrdreLinje.BetRef,1,30),dColPos_S3[5] - bredd(SUBSTRING(KOrdreLinje.BetRef,1,30)),dY).
    END.
    IF dSum <> 0 THEN 
    DO:
      dY = dY - 4.
      RUN pdf_line IN h_PDFinc  ("Spdf", dULstart_S3[4], dY, dColPos_S3[4], dY, 0.5).
      dY = dY - iLineSpace.
      RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold",8).
      RUN pdf_text_xy_dec ("Spdf",cSumTxt[1],dULStart_S3[1],dY).
      cBelopp = TRIM(STRING(dSum,"->>>,>>>,>>9.99")).
      RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPos_S3[4] - bredd(cBelopp),dY).
      dSum = 0.
      dY = dY - iLineSpace.
    END.
  END.
  RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica",8).
END PROCEDURE.
    
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


&IF DEFINED(EXCLUDE-Saml_7_SpesKorrBilag) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Saml_7_SpesKorrBilag Procedure
PROCEDURE Saml_7_SpesKorrBilag:
  /*------------------------------------------------------------------------------
   Purpose:
   Notes:
  ------------------------------------------------------------------------------*/
  DEFINE INPUT-OUTPUT PARAMETER dY AS DECIMAL     NO-UNDO.
  DEFINE VARIABLE cOverskr     AS CHARACTER EXTENT 6 NO-UNDO.
  DEFINE VARIABLE cLabel       AS CHARACTER EXTENT 2 NO-UNDO.
  DEFINE VARIABLE cTxt         AS CHARACTER NO-UNDO.
  DEFINE VARIABLE dSum_pos     AS DECIMAL   NO-UNDO.
  DEFINE VARIABLE dSum_neg     AS DECIMAL   NO-UNDO.
  DEFINE VARIABLE dColPos_S3   AS DECIMAL   EXTENT 5 NO-UNDO.
  DEFINE VARIABLE dULstart_S3  AS DECIMAL   EXTENT 5 NO-UNDO.
  DEFINE VARIABLE cTjensterTxt AS CHARACTER NO-UNDO.
  DEFINE VARIABLE dSum         AS DECIMAL   NO-UNDO.
  DEFINE VARIABLE cSumTxt      AS CHARACTER EXTENT 2 NO-UNDO.
  DEFINE VARIABLE cOrdreRef    AS CHARACTER NO-UNDO.

  IF CAN-DO("SE,SVE",cSprak) THEN 
  DO:
    ASSIGN 
      cOverskr[1]  = "LinjeNr"
      cOverskr[3]  = "Tekst"
      cOverskr[4]  = "TTId/TBId"
      cOverskr[5]  = "Bel�p"
      cOverskr[6]  = "Dato/Tid"
      cTjensterTxt = "Spesifikasjon pr.korreksjonstype"
      cSumTxt[1]   = "Totalt"
      .
  END.
  ELSE 
  DO:
    ASSIGN 
      cOverskr[1]  = "LinjeNr"
      cOverskr[3]  = "Tekst"
      cOverskr[4]  = "TTId/TBId"
      cOverskr[5]  = "Bel�p"
      cOverskr[6]  = "Dato/Tid"
      cTjensterTxt = "Spesifikasjon pr.korreksjonstype"
      cSumTxt[1]   = "Totalt"
      .
  END.
  ASSIGN 
    dColPos_S3[1] = pdf_LeftMargin ("Spdf") + 28
    /*           dColPos_S3[2] = dColPos_S3[1] + 10*/
    dColPos_S3[2] = pdf_LeftMargin ("Spdf") + 50
    dColPos_S3[3] = 340
    dColPos_S3[4] = 420
    dColPos_S3[5] = 553
    .
  ASSIGN 
    dULstart_S3[1] = pdf_LeftMargin ("Spdf")
    dULstart_S3[2] = pdf_LeftMargin ("Spdf") + 50
    dULstart_S3[3] = 290
    dULstart_S3[4] = 350
    dULstart_S3[5] = 430
    .
  IF NOT AVAILABLE BokforingsBilag THEN 
    RETURN.
  IF NOT CAN-FIND(FIRST BokforingsKorrBilag WHERE 
    BokforingsKorrBilag.BokforingsID = BokforingsBilag.BokforingsID) THEN 
    RETURN.
        
  /* RUBRIK */
  RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold",12).
  RUN pdf_text_xy_dec ("Spdf",cTjensterTxt,dULStart_S3[1],dY).
  dY = dY - 4.
  RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica",8).
  RUN pdf_line IN h_PDFinc  ("Spdf", pdf_LeftMargin ("Spdf"), dY, dULStart_S3[1], dY, 0.5).
  dY = dY - iLineSpace.
  /* Kolonnrubrik */
  RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold",8).
  RUN pdf_text_xy_dec ("Spdf",cOverskr[1],dULStart_S3[1],dY).
  RUN pdf_text_xy_dec ("Spdf",cOverskr[3],dULStart_S3[2],dY).
  RUN pdf_text_xy_dec ("Spdf",cOverskr[4],dColPos_S3[3] - bredd(cOverskr[4]),dY).
  RUN pdf_text_xy_dec ("Spdf",cOverskr[5],dColPos_S3[4] - bredd(cOverskr[5]),dY).
  RUN pdf_text_xy_dec ("Spdf",cOverskr[6],dColPos_S3[5] - bredd(cOverskr[6]),dY).
  dY = dY - 4.
  RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica",8).
  RUN pdf_line IN h_PDFinc  ("Spdf", dULstart_S3[1], dY, dColPos_S3[1], dY, 0.5).
  RUN pdf_line IN h_PDFinc  ("Spdf", dULstart_S3[2], dY, dULStart_S3[3] - 10, dY, 0.5).
  RUN pdf_line IN h_PDFinc  ("Spdf", dULstart_S3[3], dY, dColPos_S3[3], dY, 0.5).
  RUN pdf_line IN h_PDFinc  ("Spdf", dULstart_S3[4], dY, dColPos_S3[4], dY, 0.5).
  RUN pdf_line IN h_PDFinc  ("Spdf", dULstart_S3[5], dY, dColPos_S3[5], dY, 0.5).
    
  DO:
    FOR EACH BokforingsKorrBilag NO-LOCK WHERE 
      BokforingsKorrBilag.BokforingsID = BokforingsBilag.BokforingsID
      BREAK BY BokforingsKorrBilag.BokforingsID
      BY BokforingsKorrBilag.TTId
      BY BokforingsKorrBilag.TBId
      BY BokforingsKorrBilag.LinjeNr:
              
      ASSIGN 
        dSum = dSum + (BokforingsKorrBilag.Belop)
        dY   = dY - iLineSpace
        .
      cTekst = STRING(BokforingsKorrBilag.LinjeNr,">>9").
      RUN pdf_text_xy_dec ("Spdf",cTekst,dColPos_S3[1] - bredd(cTekst),dY).
      RUN pdf_text_xy_dec ("Spdf",BokforingsKorrBilag.Merknad,dColPos_S3[2],dY).
      cTekst = STRING(BokforingsKorrBilag.TTID) + '/' + STRING(BokforingsKorrBilag.TBID).
      RUN pdf_text_xy_dec ("Spdf",cTekst,dColPos_S3[3] - bredd(cTekst),dY).
      cBelopp = TRIM(STRING(BokforingsKorrBilag.Belop,"->>>,>>>,>>9.99")).
      RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPos_S3[4] - bredd(cBelopp),dY).
      cTekst = STRING(BokforingsKorrBilag.DatoTid,"99/99/99 HH:MM:SS") + " " + BokforingsKorrBilag.BrukerId.
      RUN pdf_text_xy_dec ("Spdf",cTekst,dColPos_S3[5] - bredd(cTekst),dY).
    END.
    IF dSum <> 0 THEN 
    DO:
      dY = dY - 4.
      RUN pdf_line IN h_PDFinc  ("Spdf", dULstart_S3[4], dY, dColPos_S3[4], dY, 0.5).
      dY = dY - iLineSpace.
      RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold",8).
      RUN pdf_text_xy_dec ("Spdf",cSumTxt[1],dULStart_S3[1],dY).
      cBelopp = TRIM(STRING(dSum,"->>>,>>>,>>9.99")).
      RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPos_S3[4] - bredd(cBelopp),dY).
      dSum = 0.
      dY = dY - iLineSpace.
    END.
  END.
  RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica",8).


END PROCEDURE.
  
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


&IF DEFINED(EXCLUDE-Saml_8_SpesRabatter) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Saml_8_SpesRabatter Procedure
PROCEDURE Saml_8_SpesRabatter:
  /*------------------------------------------------------------------------------
   Purpose:
   Notes:
  ------------------------------------------------------------------------------*/
  DEFINE INPUT-OUTPUT PARAMETER dY AS DECIMAL     NO-UNDO.
  DEFINE VARIABLE cOverskr     AS CHARACTER EXTENT 6 NO-UNDO.
  DEFINE VARIABLE cLabel       AS CHARACTER EXTENT 2 NO-UNDO.
  DEFINE VARIABLE cTxt         AS CHARACTER NO-UNDO.
  DEFINE VARIABLE dSum_pos     AS DECIMAL   NO-UNDO.
  DEFINE VARIABLE dSum_neg     AS DECIMAL   NO-UNDO.
  DEFINE VARIABLE dColPos_S3   AS DECIMAL   EXTENT 5 NO-UNDO.
  DEFINE VARIABLE dULstart_S3  AS DECIMAL   EXTENT 5 NO-UNDO.
  DEFINE VARIABLE cTjensterTxt AS CHARACTER NO-UNDO.
  DEFINE VARIABLE dSum         AS DECIMAL   NO-UNDO.
  DEFINE VARIABLE cSumTxt      AS CHARACTER EXTENT 2 NO-UNDO.
  DEFINE VARIABLE cOrdreRef    AS CHARACTER NO-UNDO.

  IF CAN-DO("SE,SVE",cSprak) THEN 
  DO:
    ASSIGN 
      cOverskr[1]  = "Strekkode"
      cOverskr[3]  = "Lev.art.nr/Lev.fargekode/St�rrelse"
      cOverskr[4]  = "Antall"
      cOverskr[5]  = "Bel�p"
      cOverskr[6]  = "Dato"
      cTjensterTxt = "Spesifikasjon av rabatter"
      cSumTxt[1]   = "Totalt"
      .
  END.
  ELSE 
  DO:
    ASSIGN 
      cOverskr[1]  = "Strekkode"
      cOverskr[3]  = "Lev.art.nr/Lev.fargekode/St�rrelse"
      cOverskr[4]  = "Antall"
      cOverskr[5]  = "Bel�p"
      cOverskr[6]  = "Dato"
      cTjensterTxt = "Spesifikasjon av rabatter"
      cSumTxt[1]   = "Totalt"
      .
  END.
  ASSIGN 
    dColPos_S3[1] = pdf_LeftMargin ("Spdf") + 45
    /*           dColPos_S3[2] = dColPos_S3[1] + 10*/
    dColPos_S3[2] = pdf_LeftMargin ("Spdf") + 50
    dColPos_S3[3] = 340
    dColPos_S3[4] = 420
    dColPos_S3[5] = 553
    .
  ASSIGN 
    dULstart_S3[1] = pdf_LeftMargin ("Spdf")
    dULstart_S3[2] = pdf_LeftMargin ("Spdf") + 50
    dULstart_S3[3] = 290
    dULstart_S3[4] = 350
    dULstart_S3[5] = 430
    .

  IF NOT CAN-FIND(FIRST ttRabattertSalg) THEN
    RETURN.
        
  /* RUBRIK */
  RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold",12).
  RUN pdf_text_xy_dec ("Spdf",cTjensterTxt,dULStart_S3[1],dY).
  dY = dY - 4.
  RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica",8).
  RUN pdf_line IN h_PDFinc  ("Spdf", pdf_LeftMargin ("Spdf"), dY, dULStart_S3[1], dY, 0.5).
  dY = dY - iLineSpace.
  /* Kolonnrubrik */
  RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold",8).
  RUN pdf_text_xy_dec ("Spdf",cOverskr[1],dULStart_S3[1],dY).
  RUN pdf_text_xy_dec ("Spdf",cOverskr[3],dULStart_S3[2],dY).
  RUN pdf_text_xy_dec ("Spdf",cOverskr[4],dColPos_S3[3] - bredd(cOverskr[4]),dY).
  RUN pdf_text_xy_dec ("Spdf",cOverskr[5],dColPos_S3[4] - bredd(cOverskr[5]),dY).
  RUN pdf_text_xy_dec ("Spdf",cOverskr[6],dColPos_S3[5] - bredd(cOverskr[6]),dY).
  dY = dY - 4.
  RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica",8).
  RUN pdf_line IN h_PDFinc  ("Spdf", dULstart_S3[1], dY, dColPos_S3[1], dY, 0.5).
  RUN pdf_line IN h_PDFinc  ("Spdf", dULstart_S3[2], dY, dULStart_S3[3] - 10, dY, 0.5).
  RUN pdf_line IN h_PDFinc  ("Spdf", dULstart_S3[3], dY, dColPos_S3[3], dY, 0.5).
  RUN pdf_line IN h_PDFinc  ("Spdf", dULstart_S3[4], dY, dColPos_S3[4], dY, 0.5).
  RUN pdf_line IN h_PDFinc  ("Spdf", dULstart_S3[5], dY, dColPos_S3[5], dY, 0.5).
           
  DO:
    FOR EACH ttRabattertSalg NO-LOCK WHERE
      ttRabattertSalg.ButNr = BokforingsBilag.ButikkNr
      BREAK BY ttRabattertSalg.ButNr
      BY ttRabattertSalg.ArtikkelNr
      BY ttRabattertSalg.Storl:

      ASSIGN
        dSum = dSum + (ttRabattertSalg.VerdiBut)
        dY   = dY - iLineSpace
        .
      cTekst = ttRabattertSalg.Kode.
      RUN pdf_text_xy_dec ("Spdf",cTekst,dColPos_S3[1] - bredd(cTekst),dY).
      cTekst = ttRabattertSalg.LevKod + '/' + ttRabattertSalg.LevFargKod + '/' + ttRabattertSalg.Storl.
      RUN pdf_text_xy_dec ("Spdf",cTekst,dColPos_S3[2],dY).
      cTekst = STRING(ttRabattertSalg.Antall,"->>>>>9").
      RUN pdf_text_xy_dec ("Spdf",cTekst,dColPos_S3[3] - bredd(cTekst),dY).
      cBelopp = TRIM(STRING(ttRabattertSalg.Rabatt,"->>,>>>,>>9.99")).
      RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPos_S3[4] - bredd(cBelopp),dY).
      cTekst = STRING(ttRabattertSalg.Dato,"99/99/99") + " " + ttRabattertSalg.FoNamn.
      RUN pdf_text_xy_dec ("Spdf",cTekst,dColPos_S3[5] - bredd(cTekst),dY).
      IF DY <= 86 THEN 
      DO:
        RUN pdf_new_page ("Spdf").
        RUN ButikRubrik.
        RUN PageHeader.
        dY = pdf_PageHeight ("Spdf") - 110.
      END.
    END.
    IF dSum <> 0 THEN 
    DO:
      dY = dY - 4.
      RUN pdf_line IN h_PDFinc  ("Spdf", dULstart_S3[4], dY, dColPos_S3[4], dY, 0.5).
      dY = dY - iLineSpace.
      RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold",8).
      RUN pdf_text_xy_dec ("Spdf",cSumTxt[1],dULStart_S3[1],dY).
      cBelopp = TRIM(STRING(dSum,"->>>,>>>,>>9.99")).
      RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPos_S3[4] - bredd(cBelopp),dY).
      dSum = 0.
      dY = dY - iLineSpace.
    END.
  END.
  RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica",8).
  EMPTY TEMP-TABLE ttRabattertSalg.
    
END PROCEDURE.
  
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


&IF DEFINED(EXCLUDE-Saml_9_SpesOverforinger) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Saml_9_SpesOverforinger Procedure
PROCEDURE Saml_9_SpesOverforinger:
  /*------------------------------------------------------------------------------
   Purpose:
   Notes:
  ------------------------------------------------------------------------------*/
  DEFINE INPUT-OUTPUT PARAMETER dY AS DECIMAL     NO-UNDO.
  DEFINE VARIABLE cOverskr     AS CHARACTER EXTENT 6 NO-UNDO.
  DEFINE VARIABLE cLabel       AS CHARACTER EXTENT 2 NO-UNDO.
  DEFINE VARIABLE cTxt         AS CHARACTER NO-UNDO.
  DEFINE VARIABLE dSum_pos     AS DECIMAL   NO-UNDO.
  DEFINE VARIABLE dSum_neg     AS DECIMAL   NO-UNDO.
  DEFINE VARIABLE dColPos_S3   AS DECIMAL   EXTENT 5 NO-UNDO.
  DEFINE VARIABLE dULstart_S3  AS DECIMAL   EXTENT 5 NO-UNDO.
  DEFINE VARIABLE cTjensterTxt AS CHARACTER NO-UNDO.
  DEFINE VARIABLE dSum         AS DECIMAL   NO-UNDO.
  DEFINE VARIABLE cSumTxt      AS CHARACTER EXTENT 2 NO-UNDO.
  DEFINE VARIABLE cOrdreRef    AS CHARACTER NO-UNDO.

  IF CAN-DO("SE,SVE",cSprak) THEN 
  DO:
    ASSIGN 
      cOverskr[1]  = "Strekkode"
      cOverskr[3]  = "Lev.art.nr/Lev.fargekode/St�rrelse"
      cOverskr[4]  = "Antall"
      cOverskr[5]  = "Bel�p"
      cOverskr[6]  = "Dato"
      cTjensterTxt = "Utg�ende overf�ringer"
      cSumTxt[1]   = "Totalt"
      .
  END.
  ELSE 
  DO:
    ASSIGN 
      cOverskr[1]  = "Strekkode"
      cOverskr[3]  = "Lev.art.nr/Lev.fargekode/St�rrelse"
      cOverskr[4]  = "Antall"
      cOverskr[5]  = "Bel�p"
      cOverskr[6]  = "Dato"
      cTjensterTxt = "Utg�ende overf�ringer"
      cSumTxt[1]   = "Totalt"
      .
  END.
  ASSIGN 
    dColPos_S3[1] = pdf_LeftMargin ("Spdf") + 45
    /*           dColPos_S3[2] = dColPos_S3[1] + 10*/
    dColPos_S3[2] = pdf_LeftMargin ("Spdf") + 50
    dColPos_S3[3] = 340
    dColPos_S3[4] = 420
    dColPos_S3[5] = 553
    .
  ASSIGN 
    dULstart_S3[1] = pdf_LeftMargin ("Spdf")
    dULstart_S3[2] = pdf_LeftMargin ("Spdf") + 50
    dULstart_S3[3] = 290
    dULstart_S3[4] = 350
    dULstart_S3[5] = 430
    .

  IF NOT CAN-FIND(FIRST ttOverfor) THEN
    RETURN.
        
  /* RUBRIK */
  RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold",12).
  RUN pdf_text_xy_dec ("Spdf",cTjensterTxt,dULStart_S3[1],dY).
  dY = dY - 4.
  RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica",8).
  RUN pdf_line IN h_PDFinc  ("Spdf", pdf_LeftMargin ("Spdf"), dY, dULStart_S3[1], dY, 0.5).
  dY = dY - iLineSpace.
  /* Kolonnrubrik */
  RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold",8).
  RUN pdf_text_xy_dec ("Spdf",cOverskr[1],dULStart_S3[1],dY).
  RUN pdf_text_xy_dec ("Spdf",cOverskr[3],dULStart_S3[2],dY).
  RUN pdf_text_xy_dec ("Spdf",cOverskr[4],dColPos_S3[3] - bredd(cOverskr[4]),dY).
  RUN pdf_text_xy_dec ("Spdf",cOverskr[5],dColPos_S3[4] - bredd(cOverskr[5]),dY).
  RUN pdf_text_xy_dec ("Spdf",cOverskr[6],dColPos_S3[5] - bredd(cOverskr[6]),dY).
  dY = dY - 4.
  RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica",8).
  RUN pdf_line IN h_PDFinc  ("Spdf", dULstart_S3[1], dY, dColPos_S3[1], dY, 0.5).
  RUN pdf_line IN h_PDFinc  ("Spdf", dULstart_S3[2], dY, dULStart_S3[3] - 10, dY, 0.5).
  RUN pdf_line IN h_PDFinc  ("Spdf", dULstart_S3[3], dY, dColPos_S3[3], dY, 0.5).
  RUN pdf_line IN h_PDFinc  ("Spdf", dULstart_S3[4], dY, dColPos_S3[4], dY, 0.5).
  RUN pdf_line IN h_PDFinc  ("Spdf", dULstart_S3[5], dY, dColPos_S3[5], dY, 0.5).
           
  DO:
    FOR EACH ttOverfor NO-LOCK WHERE 
      /*          ttOverfor.ButNr = BokforingsBilag.ButikkNr AND*/
      ttOverfor.OvType = 2
      BREAK BY ttOverfor.OvType
      BY ttOverfor.ButNr
      BY ttOverfor.ArtikkelNr
      BY ttOverfor.Storl:
              
      ASSIGN 
        dSum = dSum + (ttOverfor.Verdi)
        dY   = dY - iLineSpace
        .
      cTekst = ttOverfor.Kode.
      RUN pdf_text_xy_dec ("Spdf",cTekst,dColPos_S3[1] - bredd(cTekst),dY).
      cTekst = ttOverfor.LevKod + '/' + ttOverfor.LevFargKod + '/' + ttOverfor.Storl.  
      RUN pdf_text_xy_dec ("Spdf",cTekst,dColPos_S3[2],dY).
      cTekst = STRING(ttOverfor.Antall,"->>>>>9").
      RUN pdf_text_xy_dec ("Spdf",cTekst,dColPos_S3[3] - bredd(cTekst),dY).
      cBelopp = TRIM(STRING(ttOverfor.Verdi,"->>,>>>,>>9.99")).
      RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPos_S3[4] - bredd(cBelopp),dY).
      cTekst = STRING(ttOverfor.Dato,"99/99/99").
      RUN pdf_text_xy_dec ("Spdf",cTekst,dColPos_S3[5] - bredd(cTekst),dY).
      IF DY <= 86 THEN 
      DO:
        RUN pdf_new_page ("Spdf").
        RUN ButikRubrik.
        RUN PageHeader.
        dY = pdf_PageHeight ("Spdf") - 110.
      END.
    END.
    IF dSum <> 0 THEN 
    DO:
      dY = dY - 4.
      RUN pdf_line IN h_PDFinc  ("Spdf", dULstart_S3[4], dY, dColPos_S3[4], dY, 0.5).
      dY = dY - iLineSpace.
      RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold",8).
      RUN pdf_text_xy_dec ("Spdf",cSumTxt[1],dULStart_S3[1],dY).
      cBelopp = TRIM(STRING(dSum,"->>>,>>>,>>9.99")).
      RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPos_S3[4] - bredd(cBelopp),dY).
      dSum = 0.
      dY = dY - iLineSpace.
    END.
  END.
  RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica",8).
  EMPTY TEMP-TABLE ttOverfor.
    
END PROCEDURE.
  
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


&IF DEFINED(EXCLUDE-sendEmail) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE sendEmail Procedure 
PROCEDURE sendEmail :
  /*------------------------------------------------------------------------------
    Purpose:     
    Parameters:  <none>
    Notes:       
  ------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER cMailTo   AS CHARACTER   NO-UNDO.
  DEFINE INPUT  PARAMETER cFileName AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE cMailhub   AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cDoAUTH    AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cAuthType  AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cUser      AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cPassword  AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cEmailCC   AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cEmailFrom AS CHARACTER NO-UNDO.

  DEFINE VARIABLE lMailOK    AS LOGICAL   NO-UNDO.
  DEFINE VARIABLE cMessage   AS CHARACTER NO-UNDO.

  {syspara.i 50 50 1 cMailhub }
  {syspara.i 50 50 2 cDoAUTH  }
  {syspara.i 50 50 3 cAuthType}
  {syspara.i 50 50 4 cUser    }
  {syspara.i 50 50 5 cPassword}
  {syspar2.i 50 50 20 cEmailCC}
  {syspara.i 50 50 40 cEmailFrom}


  RUN prssmtpmailv5_7a.p (
  /*mailhub    */   cMailhub,
  /*EmailTo    */   cMailTo,
  /*EmailFrom  */   cEmailFrom,
  /*EmailCC    */   "",
  /*Attachments*/   ENTRY(NUM-ENTRIES(cFileName,"\"),cFileName,"\"),
  /*LocalFiles */   cFileName,
  /*Subject    */   "Samlingsrapport",
  /*Body       */   "",
  /*MIMEHeader */   "CharSet=iso8859-1",
  /*BodyType   */   "",
  /*Importance */   0,
  /*L_DoAUTH   */   cDoAUTH,
  /*C_AuthType */   cAuthType,
  /*C_User     */   cUser,
  /*C_Password */   cPassword,
  /*oSuccessful*/  OUTPUT lMailOK,
  /*vMessage   */  OUTPUT cMessage) NO-ERROR.
/*         IF cFileName <> "" THEN         */
/*             OS-DELETE VALUE(cFileName). */
/*         IF lMailOK = FALSE THEN                    */
/*             MESSAGE cMessage                       */
/*                 VIEW-AS ALERT-BOX INFO BUTTONS OK. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SetPositioner) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SetPositioner Procedure 
PROCEDURE SetPositioner :
  /*------------------------------------------------------------------------------
    Purpose:     
    Parameters:  <none>
    Notes:       
  ------------------------------------------------------------------------------*/
  ASSIGN 
    dColPosBF[1] = pdf_LeftMargin ("Spdf")
    dColPosBF[2] = 210
    dColPosBF[3] = 370
    dColPosBF[4] = 470
    dColPosBF[5] = pdf_PageWidth ("Spdf") - pdf_LeftMargin ("Spdf").
  ASSIGN 
    dColPosFR[1] = pdf_LeftMargin ("Spdf")
    dColPosFR[2] = 160
    dColPosFR[3] = 250
    dColPosFR[4] = 315
    dColPosFR[5] = 455
    dColPosFR[6] = pdf_PageWidth ("Spdf") - pdf_LeftMargin ("Spdf").
  ASSIGN 
    dULstartFR[1] = 0
    dULstartFR[2] = 110
    dULstartFR[3] = 170
    dULstartFR[4] = 0
    dULstartFR[5] = 410
    dULstartFR[6] = 470.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-settMvaKonto) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE settMvaKonto Procedure
PROCEDURE settMvaKonto:
  /*------------------------------------------------------------------------------
   Purpose:
   Notes: RUN settMvaKonto(tmpKas_Rap.Butikk,1,1,INPUT-OUTPUT cKonto).
  ------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER piButikkNr AS INTEGER   NO-UNDO.
  DEFINE INPUT  PARAMETER piMomsKod  AS INTEGER   NO-UNDO.
  DEFINE INPUT-OUTPUT PARAMETER pcKonto    AS CHARACTER NO-UNDO. 
    
  FIND SIEMoms NO-LOCK WHERE 
    SIEMoms.ButikkNr = piButikkNr AND 
    SIEMoms.MomsKod  = piMomsKod NO-ERROR.
  IF NOT AVAILABLE SIEMoms THEN 
    FIND SIEMoms NO-LOCK WHERE 
      SIEMoms.ButikkNr = 0 AND 
      SIEMoms.MomsKod  = piMomsKod NO-ERROR.
  IF AVAILABLE SIEMoms THEN 
    pcKonto = pcKonto + '/' + STRING(SIEMoms.KontoNrMva).
  ELSE 
    pcKonto = pcKonto + '/0000'.
END PROCEDURE.
    
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


&IF DEFINED(EXCLUDE-settTekstOgKonto) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE settTekstOgKonto Procedure 
PROCEDURE settTekstOgKonto :
  /*------------------------------------------------------------------------------
                          Purpose:                                                                                                                                          
                          Notes:  RUN settTekstOgKonto(tmpKas_Rap.Butikk,52,tmpKort_Spes.KortType,lBelop,output cChar, OUTPUT cKonto).      
                                                                                                                                                                    
          ------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER piButikkNr AS INTEGER   NO-UNDO.
  DEFINE INPUT  PARAMETER piTTId     AS INTEGER   NO-UNDO.
  DEFINE INPUT  PARAMETER piTBId     AS INTEGER   NO-UNDO.
  DEFINE INPUT  PARAMETER plBelop    AS DECIMAL   NO-UNDO.
  DEFINE INPUT  PARAMETER cType      AS CHARACTER NO-UNDO.
  DEFINE OUTPUT PARAMETER pcChar     AS CHARACTER NO-UNDO. 
  DEFINE OUTPUT PARAMETER pcKonto    AS CHARACTER NO-UNDO. 

  /*
  MESSAGE 'settTekstOgKonto:' piButikkNr piTTId piTBId plBelop SKIP 
  'pcChar' pcChar SKIP
  'pcKonto' pcKonto 
  VIEW-AS ALERT-BOX.
  */

  ASSIGN
    pcChar  = ''
    pcKonto = ''.
  /*   IF plBelop = 0 THEN */
  /*     RETURN.           */

  /* Sjekker SIETransType for butikken. */
  FIND SIETransType NO-LOCK WHERE 
    SIETRansType.ButikkNr = piButikkNr AND 
    SIETransType.TTId     = piTTId AND 
    SIETransType.TBId     = piTBId NO-ERROR.
  IF AVAILABLE SIETRansType THEN 
    ASSIGN 
      pcChar  = SIETransType.Beskrivelse + '(' + STRING(piTTId) + '/' + STRING(piTBId) +  ')'
      pcKonto = cType + ' ' + STRING(SIETRansType.KontoNr,"9999").
  
  /* Sjekker SIETransType for standardbutikk = 0. */
  IF (pcChar = '' AND pcKonto = '') THEN
  DO:
    FIND SIETransType NO-LOCK WHERE 
      SIETRansType.ButikkNr = 0 AND 
      SIETransType.TTId     = piTTId AND 
      SIETransType.TBId     = piTBId NO-ERROR.
    IF AVAILABLE SIETRansType THEN 
      ASSIGN 
        pcChar  = SIETRansType.Beskrivelse + '(' + STRING(piTTId) + '/' + STRING(piTBId) + ')'
        pcKonto = cType + ' ' + STRING(SIETRansType.KontoNr,"9999").
  END.
           
  /* Standard kontering. */
  IF (pcChar = '' AND pcKonto = '') THEN
  DO:
    FIND TransBeskr NO-LOCK WHERE
      TransBeskr.TTId = piTTId AND 
      TransBeskr.TBId = piTBId NO-ERROR.
    IF AVAILABLE TransBeskr THEN 
      ASSIGN 
        pcChar  = TransBeskr.Beskrivelse + '(' + STRING(piTTId) + '/' + STRING(piTBId) + ')'
        pcKonto = cType + ' ' + STRING(TransBeskr.KontoNr,"9999").
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SummerKortSpes) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SummerKortSpes Procedure 
PROCEDURE SummerKortSpes :
  /*------------------------------------------------------------------------------
    Purpose:     
    Parameters:  <none>
    Notes:       
  ------------------------------------------------------------------------------*/
  ASSIGN 
    tmpKort_Spes.AntKort = tmpKort_Spes.AntKort + Kort_Spes.AntKort
    tmpKort_Spes.Belop   = tmpKort_Spes.Belop   + Kort_Spes.Belop.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SummerPost) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SummerPost Procedure 
PROCEDURE SummerPost :
  /*------------------------------------------------------------------------------
    Purpose:     
    Parameters:  <none>
    Notes:      
  ------------------------------------------------------------------------------*/
  ASSIGN
    tmpKas_Rap.Kontant           = tmpKas_Rap.Kontant       + Kas_Rap.Kontant
    tmpKas_Rap.Sjekk             = tmpKas_Rap.Sjekk         + Kas_Rap.Sjekk
    tmpKas_Rap.Kort              = tmpKas_Rap.Kort          + Kas_Rap.Kort
    tmpKas_Rap.Kredit            = tmpKas_Rap.Kredit        + Kas_Rap.Kredit
    tmpKas_Rap.Kupong1           = tmpKas_Rap.Kupong1       + Kas_Rap.Kupong1
    tmpKas_Rap.Kupong2           = tmpKas_Rap.Kupong2       + Kas_Rap.Kupong2
    tmpKas_Rap.Tilgode           = tmpKas_Rap.Tilgode       + Kas_Rap.Tilgode
    tmpKas_Rap.Layaway_inn       = tmpKas_Rap.Layaway_inn   + Kas_Rap.Layaway_inn
    tmpKas_Rap.Layaway_ut        = tmpKas_Rap.Layaway_ut    + Kas_Rap.Layaway_ut
    tmpKas_Rap.Kont_inn          = tmpKas_Rap.Kont_inn      + Kas_Rap.Kont_inn
    tmpKas_Rap.Kont_ut           = tmpKas_Rap.Kont_ut       + Kas_Rap.Kont_ut
    tmpKas_Rap.Gavekort          = tmpKas_Rap.Gavekort      + Kas_Rap.Gavekort

    tmpKas_Rap.Rekvisisasjon     = tmpKas_Rap.Rekvisisasjon + Kas_Rap.Rekvisisasjon
    tmpKas_Rap.Pant              = tmpKas_Rap.Pant          + Kas_Rap.Pant
    tmpKas_Rap.Bank              = tmpKas_Rap.Bank          + Kas_Rap.Bank
    tmpKas_Rap.Dropp             = tmpKas_Rap.Dropp         + Kas_Rap.Dropp
    tmpKas_Rap.AntVaremottak     = tmpKas_Rap.AntVaremottak     + Kas_Rap.AntVaremottak    
    tmpKas_Rap.AntLagerjustering = tmpKas_Rap.AntLagerjustering + Kas_Rap.AntLagerjustering
    tmpKas_Rap.AntBrekkasje      = tmpKas_Rap.AntBrekkasje      + Kas_Rap.AntBrekkasje     
    tmpKas_Rap.AntInterntForbruk = tmpKas_Rap.AntInterntForbruk + Kas_Rap.AntInterntForbruk
    tmpKas_Rap.Varemottak        = tmpKas_Rap.Varemottak        + Kas_Rap.Varemottak    
    tmpKas_Rap.Lagerjustering    = tmpKas_Rap.Lagerjustering    + Kas_Rap.Lagerjustering
    tmpKas_Rap.Brekkasje         = tmpKas_Rap.Brekkasje         + Kas_Rap.Brekkasje     
    tmpKas_Rap.InterntForbruk    = tmpKas_Rap.InterntForbruk    + Kas_Rap.InterntForbruk
    .

  ASSIGN
    tmpKas_Rap.Overfort          = tmpKas_Rap.Overfort          + Kas_Rap.Overfort
    tmpKas_Rap.CashBack          = tmpKas_Rap.CashBack          + Kas_Rap.CashBack
    tmpKas_Rap.Veksel            = tmpKas_Rap.Veksel            + Kas_Rap.Veksel
    tmpKas_Rap.Avrunding         = tmpKas_Rap.Avrunding         + Kas_Rap.Avrunding
    tmpKas_Rap.Reklamasjon       = tmpKas_Rap.Reklamasjon       + Kas_Rap.Reklamasjon
    tmpKas_Rap.Retur             = tmpKas_Rap.Retur             + Kas_Rap.Retur
    tmpKas_Rap.InnbetaltKunde    = tmpKas_Rap.InnbetaltKunde    + Kas_Rap.InnbetaltKunde
    tmpKas_Rap.Medlemssalg       = tmpKas_Rap.Medlemssalg       + Kas_Rap.Medlemssalg
    tmpKas_Rap.AntCashBack       = tmpKas_Rap.AntCashBack       + Kas_Rap.AntCashBack
    tmpKas_Rap.AntMedlemssalg    = tmpKas_Rap.AntMedlemssalg    + Kas_Rap.AntMedlemssalg
    tmpKas_Rap.AntInnbetaltKunde = tmpKas_Rap.AntInnbetaltKunde + Kas_Rap.AntInnbetaltKunde
    tmpKas_Rap.AntRetur          = tmpKas_Rap.AntRetur          + Kas_Rap.AntRetur
    tmpKas_Rap.AntKontant        = tmpKas_Rap.AntKontant        + Kas_Rap.AntKontant
    tmpKas_Rap.AntSjekk          = tmpKas_Rap.AntSjekk          + Kas_Rap.AntSjekk
    tmpKas_Rap.SjekkBeholdning   = tmpKas_Rap.SjekkBeholdning   + Kas_Rap.Sjekk
    tmpKas_Rap.AntKort           = tmpKas_Rap.AntKort           + Kas_Rap.AntKort
    tmpKas_Rap.AntKredit         = tmpKas_Rap.AntKredit         + Kas_Rap.AntKredit
    tmpKas_Rap.AntKupong1        = tmpKas_Rap.AntKupong1        + Kas_Rap.AntKupong1
    tmpKas_Rap.AntKupong2        = tmpKas_Rap.AntKupong2        + Kas_Rap.AntKupong2
    tmpKas_Rap.AntTilgode        = tmpKas_Rap.AntTilgode        + Kas_Rap.AntTilgode
    tmpKas_Rap.AntBank           = tmpKas_Rap.AntBank           + kas_rap.AntBank
    tmpKas_Rap.Non_SalePosAnt    = tmpKas_Rap.Non_SalePosAnt    + Kas_Rap.Non_SalePosAnt
    tmpKas_Rap.Non_SalePos       = tmpKas_Rap.Non_SalePos       + kas_rap.Non_SalePos
    tmpKas_Rap.Non_SaleNegAnt    = tmpKas_Rap.Non_SaleNegAnt    + Kas_Rap.Non_SaleNegAnt
    tmpKas_Rap.Non_SaleNeg       = tmpKas_Rap.Non_SaleNeg       + kas_rap.Non_SaleNeg
    .

  ASSIGN
    tmpKas_Rap.GaveKortRabatt      = tmpKas_Rap.GaveKortRabatt    + Kas_Rap.GaveKortRabatt
    tmpKas_Rap.AntGaveKortRabUt    = tmpKas_Rap.AntGaveKortRabUt  + Kas_Rap.AntGaveKortRabUt
    tmpKas_Rap.AntGavekort         = tmpKas_Rap.AntGavekort       + Kas_Rap.AntGavekort
    tmpKas_Rap.AntRekvisisjon      = tmpKas_Rap.AntRekvisisjon    + Kas_Rap.AntRekvisisjon
    tmpKas_Rap.AntVeksel           = tmpKas_Rap.AntVeksel         + Kas_Rap.AntVeksel
    tmpKas_Rap.AntAvrunding        = tmpKas_Rap.AntAvrunding      + Kas_Rap.AntAvrunding
    tmpKas_Rap.AntDropp            = tmpKas_Rap.AntDropp          + Kas_Rap.AntDropp
    tmpKas_Rap.AntOverfort         = tmpKas_Rap.AntOverfort       + Kas_Rap.AntOverfort
    tmpKas_Rap.AntKont_Inn         = tmpKas_Rap.AntKont_Inn       + Kas_Rap.AntKont_Inn
    tmpKas_Rap.AntKont_Ut          = tmpKas_Rap.AntKont_Ut        + Kas_Rap.AntKont_Ut
    tmpKas_Rap.AntLayAway_Inn      = tmpKas_Rap.AntLayAway_Inn    + Kas_Rap.AntLayAway_Inn
    tmpKas_Rap.AntLayAway_Ut       = tmpKas_Rap.AntLayAway_Ut     + Kas_Rap.AntLayAway_Ut
    tmpKas_Rap.AntReturer          = tmpKas_Rap.AntReturer        + Kas_Rap.AntReturer
    tmpKas_Rap.TilgodeInn          = tmpKas_Rap.TilgodeInn        + Kas_Rap.TilgodeInn
    tmpKas_Rap.TilgodeAndre        = tmpKas_Rap.TilgodeAndre      + Kas_Rap.TilgodeAndre
    tmpKas_Rap.TilgodeUt           = tmpKas_Rap.TilgodeUt         + Kas_Rap.TilgodeUt
    tmpKas_Rap.AntTilgodeInn       = tmpKas_Rap.AntTilgodeInn     + Kas_Rap.AntTilgodeInn
    tmpKas_Rap.AntTilgodeAndre     = tmpKas_Rap.AntTilgodeAndre   + Kas_Rap.AntTilgodeAndre
    tmpKas_Rap.AntTilgodeUt        = tmpKas_Rap.AntTilgodeUt      + Kas_Rap.AntTilgodeUt
    tmpKas_Rap.GavekortUt          = tmpKas_Rap.GavekortUt        + Kas_Rap.GavekortUt
    tmpKas_Rap.GavekortInn         = tmpKas_Rap.GavekortInn       + Kas_Rap.GavekortInn
    tmpKas_Rap.GavekortAndreInn    = tmpKas_Rap.GavekortAndreInn  + Kas_Rap.GavekortAndreInn
    tmpKas_Rap.AntGavekortUt       = tmpKas_Rap.AntGavekortUt     + Kas_Rap.AntGavekortUt
    tmpKas_Rap.AntGavekortInn      = tmpKas_Rap.AntGavekortInn    + Kas_Rap.AntGavekortInn
    tmpKas_Rap.AntGavekortAndreInn = tmpKas_Rap.AntGavekortAndreInn + Kas_Rap.AntGavekortAndreInn
    tmpKas_Rap.Medlemsrabatt       = tmpKas_Rap.Medlemsrabatt     + Kas_Rap.Medlemsrabatt.

  ASSIGN
    tmpKas_Rap.Kunderabatt       = tmpKas_Rap.Kunderabatt       + Kas_Rap.Kunderabatt
    tmpKas_Rap.Personalrabatt    = tmpKas_Rap.Personalrabatt    + Kas_Rap.Personalrabatt
    tmpKas_Rap.GenerellRabatt    = tmpKas_Rap.GenerellRabatt    + Kas_Rap.GenerellRabatt
    tmpKas_Rap.AntPersonalrabatt = tmpKas_Rap.AntPersonalrabatt + Kas_Rap.AntPersonalrabatt
    tmpKas_Rap.AntMedlemsrabatt  = tmpKas_Rap.AntMedlemsrabatt  + Kas_Rap.AntMedlemsrabatt
    tmpKas_Rap.AntKunderabatt    = tmpKas_Rap.AntKunderabatt    + Kas_Rap.AntKunderabatt
    tmpKas_Rap.AntGenerellRabatt = tmpKas_Rap.AntGenerellRabatt + Kas_Rap.AntGenerellRabatt
    tmpKas_Rap.OverfortInn       = tmpKas_Rap.OverfortInn       + Kas_Rap.OverfortInn
    tmpKas_Rap.OverfortUt        = tmpKas_Rap.OverfortUt        + Kas_Rap.OverfortUt
    tmpKas_Rap.AntOverfortInn    = tmpKas_Rap.AntOverfortInn    + Kas_Rap.AntOverfortInn
    tmpKas_Rap.AntOverfortUt     = tmpKas_Rap.AntOverfortUt     + Kas_Rap.AntOverfortUt.
  ASSIGN
    tmpKas_Rap.MvaGrp[ 1] = 0  /* Kas_Rap.MvaGrp[ 1]  */      
    tmpKas_Rap.MvaGrp[ 2] = 1  /* Kas_Rap.MvaGrp[ 2]  */      
    tmpKas_Rap.MvaGrp[ 3] = 2  /* Kas_Rap.MvaGrp[ 3]  */      
    tmpKas_Rap.MvaGrp[ 4] = 3  /* Kas_Rap.MvaGrp[ 4]  */      
    tmpKas_Rap.MvaGrp[ 5] = 4  /* Kas_Rap.MvaGrp[ 5]  */      
    tmpKas_Rap.MvaGrp[ 6] = 5  /* Kas_Rap.MvaGrp[ 6]  */      
    tmpKas_Rap.MvaGrp[ 7] = 6  /* Kas_Rap.MvaGrp[ 7]  */      
    tmpKas_Rap.MvaGrp[ 8] = 7  /* Kas_Rap.MvaGrp[ 8]  */      
    tmpKas_Rap.MvaGrp[ 9] = 8  /* Kas_Rap.MvaGrp[ 9]  */
    tmpKas_Rap.MvaGrp[10] = 9. /*  Kas_Rap.MvaGrp[10].*/

  ASSIGN
    tmpKas_Rap.MvaGrunnlag[ 1] = tmpKas_Rap.MvaGrunnlag[ 1]   + Kas_Rap.MvaGrunnlag[ 1]
    tmpKas_Rap.MvaGrunnlag[ 2] = tmpKas_Rap.MvaGrunnlag[ 2]   + Kas_Rap.MvaGrunnlag[ 2]
    tmpKas_Rap.MvaGrunnlag[ 3] = tmpKas_Rap.MvaGrunnlag[ 3]   + Kas_Rap.MvaGrunnlag[ 3]
    tmpKas_Rap.MvaGrunnlag[ 4] = tmpKas_Rap.MvaGrunnlag[ 4]   + Kas_Rap.MvaGrunnlag[ 4]
    tmpKas_Rap.MvaGrunnlag[ 5] = tmpKas_Rap.MvaGrunnlag[ 5]   + Kas_Rap.MvaGrunnlag[ 5]
    tmpKas_Rap.MvaGrunnlag[ 6] = tmpKas_Rap.MvaGrunnlag[ 6]   + Kas_Rap.MvaGrunnlag[ 6]
    tmpKas_Rap.MvaGrunnlag[ 7] = tmpKas_Rap.MvaGrunnlag[ 7]   + Kas_Rap.MvaGrunnlag[ 7]
    tmpKas_Rap.MvaGrunnlag[ 8] = tmpKas_Rap.MvaGrunnlag[ 8]   + Kas_Rap.MvaGrunnlag[ 8]
    tmpKas_Rap.MvaGrunnlag[ 9] = tmpKas_Rap.MvaGrunnlag[ 9]   + Kas_Rap.MvaGrunnlag[ 9]
    tmpKas_Rap.MvaGrunnlag[10] = tmpKas_Rap.MvaGrunnlag[10]   + Kas_Rap.MvaGrunnlag[10]
    tmpKas_Rap.MvaBelop[ 1]    = tmpKas_Rap.MvaBelop[ 1]      + Kas_Rap.MvaBelop[ 1]
    tmpKas_Rap.MvaBelop[ 2]    = tmpKas_Rap.MvaBelop[ 2]      + Kas_Rap.MvaBelop[ 2]
    tmpKas_Rap.MvaBelop[ 3]    = tmpKas_Rap.MvaBelop[ 3]      + Kas_Rap.MvaBelop[ 3]
    tmpKas_Rap.MvaBelop[ 4]    = tmpKas_Rap.MvaBelop[ 4]      + Kas_Rap.MvaBelop[ 4]
    tmpKas_Rap.MvaBelop[ 5]    = tmpKas_Rap.MvaBelop[ 5]      + Kas_Rap.MvaBelop[ 5]
    tmpKas_Rap.MvaBelop[ 6]    = tmpKas_Rap.MvaBelop[ 6]      + Kas_Rap.MvaBelop[ 6]
    tmpKas_Rap.MvaBelop[ 7]    = tmpKas_Rap.MvaBelop[ 7]      + Kas_Rap.MvaBelop[ 7]
    tmpKas_Rap.MvaBelop[ 8]    = tmpKas_Rap.MvaBelop[ 8]      + Kas_Rap.MvaBelop[ 8]
    tmpKas_Rap.MvaBelop[ 9]    = tmpKas_Rap.MvaBelop[ 9]      + Kas_Rap.MvaBelop[ 9].

  ASSIGN
    tmpKas_Rap.MvaBelop[10]      = tmpKas_Rap.MvaBelop[10]      + Kas_Rap.MvaBelop[10]
    tmpKas_Rap.AntReklamasjoner  = tmpKas_Rap.AntReklamasjoner  + Kas_Rap.AntReklamasjoner
    tmpKas_Rap.Reservelosning    = tmpKas_Rap.Reservelosning    + Kas_Rap.Reservelosning
    tmpKas_Rap.AntReservelosning = tmpKas_Rap.AntReservelosning + Kas_Rap.AntReservelosning
    tmpKas_Rap.AntPakkerabatt    = tmpKas_Rap.AntPakkerabatt    + Kas_Rap.AntPakkerabatt
    tmpKas_Rap.Pakkerabatt       = tmpKas_Rap.Pakkerabatt       + Kas_Rap.Pakkerabatt
    tmpKas_Rap.KontantBeholdning = tmpKas_Rap.KontantBeholdning + Kas_Rap.KontantBeholdning
    tmpKas_Rap.VekselBeholdning  = tmpKas_Rap.VekselBeholdning  + Kas_Rap.VekselBeholdning
    tmpKas_Rap.AntallUtbetBonger = tmpKas_Rap.AntallUtbetBonger + iAntallUtbetBonger
    tmpKas_Rap.VerdiUtbetBonger  = tmpKas_Rap.VerdiUtbetBonger  + lVerdiUtbetBonger
    .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ValiderKriterier) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ValiderKriterier Procedure 
PROCEDURE ValiderKriterier :
  /*------------------------------------------------------------------------------
    Purpose:     
    Parameters:  <none>
    Notes:       
  ------------------------------------------------------------------------------*/
  IF pdFraDato > TODAY OR pdFraDato = ? THEN
    RETURN "AVBRYT".
  IF pdTilDato < pdFraDato OR pdTilDato = ? THEN
    pdTilDato = pdFraDato.
  RETURN "OK".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

/* ************************  Function Implementations ***************** */

&IF DEFINED(EXCLUDE-bredd) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION bredd Procedure 
FUNCTION bredd RETURNS DECIMAL
  ( INPUT cText AS CHARACTER ) :
  /*------------------------------------------------------------------------------
    Purpose:  
      Notes:  
  ------------------------------------------------------------------------------*/

  RETURN pdf_text_widthdec ("Spdf",cText).   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getDBKr) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getDBKr Procedure 
FUNCTION getDBKr RETURNS DECIMAL
  ( /* parameter-definitions */ ) :
  /*------------------------------------------------------------------------------
    Purpose:  
      Notes:  
  ------------------------------------------------------------------------------*/
  DEFINE VARIABLE dSum             AS DECIMAL NO-UNDO.

  /*
  FOR EACH dags_rap NO-LOCK WHERE butik = piButNr AND 
           dato >= pdFraDato AND
           dato <= pdTilDato:
      dSum = dSum + dags_rap.tb1 + 
                    dags_rap.tb2 +
                    dags_rap.tb3 +
                    dags_rap.tb4 +
                    dags_rap.tb5 +
                    dags_rap.tb6 +
                    dags_rap.tb7 +
                    dags_rap.tb8 +
                    dags_rap.tb9 + 
                    Dags_rap.tb10.
  END.
  */

  DEF    VAR      pcPerId          AS CHAR    NO-UNDO.
  DEF    VAR      pcStTypeId       AS CHAR    NO-UNDO.
  DEF    VAR      piFraAarPerLinNr AS INT     NO-UNDO.
  DEF    VAR      piTilAarPerLinNr AS INT     NO-UNDO.

  ASSIGN
    pcPerId          = 'DAG'
    pcStTypeId       = 'AVDELING'
    piFraAarPerLinNr = INT(
                         STRING(YEAR(pdFraDato),"9999") + 
                         STRING(pdFraDato - DATE(12 , 31, YEAR(pdFraDato) - 1),"999")
                        )
    piTilAarPerLinNr = INT(
                         STRING(YEAR(pdTilDato),"9999") + 
                         STRING(pdTilDato - DATE(12 , 31, YEAR(pdTilDato) - 1),"999")
                        )
    .

  FOR EACH StLinje NO-LOCK WHERE
    StLinje.Butik    = piButNr AND
    StLinje.StTypeId = pcStTypeId AND
    StLinje.PerId    = pcPerId AND
    StLinje.AarPerLinNr >= piFraAarPerLinNr AND
    StLinje.AarPerLinNr <= piTilAarPerLinNr 
    :

    ASSIGN
      dSum = dSum + (StLinje.VerdiSolgt - StLinje.VVareKost)
      .
  END.

  RETURN dSum.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getKonto) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getKonto Procedure 
FUNCTION getKonto RETURNS CHARACTER
  ( INPUT gruppe AS INTEGER, INPUT syspara AS INTEGER, INPUT dBelop AS DECIMAL ) :
  /*------------------------------------------------------------------------------
    Purpose:  
      Notes:  
  ------------------------------------------------------------------------------*/
  DEFINE VARIABLE pcTekst AS CHARACTER NO-UNDO.
  IF dBelop = 0 THEN
    pcTekst = "".
  ELSE IF dBelop > 0 THEN
    {syspara.i 20 gruppe syspara pcTekst}
    ELSE
    {syspar2.i 20 gruppe syspara pcTekst}

  RETURN pcTekst.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getMvaGrunnlag) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getMvaGrunnlag Procedure 
FUNCTION getMvaGrunnlag RETURNS DECIMAL
  (  ):
  /*------------------------------------------------------------------------------
                  Purpose:                                                                                                                                          
                  Notes:                                                                                                                                            
  ------------------------------------------------------------------------------*/
  DEFINE VARIABLE dSum             AS DECIMAL NO-UNDO.

  DEF    VAR      pcPerId          AS CHAR    NO-UNDO.
  DEF    VAR      pcStTypeId       AS CHAR    NO-UNDO.
  DEF    VAR      piFraAarPerLinNr AS INT     NO-UNDO.
  DEF    VAR      piTilAarPerLinNr AS INT     NO-UNDO.

  ASSIGN
    pcPerId          = 'DAG'
    pcStTypeId       = 'AVDELING'
    piFraAarPerLinNr = INT(
                         STRING(YEAR(pdFraDato),"9999") + 
                         STRING(pdFraDato - DATE(12 , 31, YEAR(pdFraDato) - 1),"999")
                        )
    piTilAarPerLinNr = INT(
                         STRING(YEAR(pdTilDato),"9999") + 
                         STRING(pdTilDato - DATE(12 , 31, YEAR(pdTilDato) - 1),"999")
                        )
    .

  FOR EACH StLinje NO-LOCK WHERE
    StLinje.Butik    = piButNr AND
    StLinje.StTypeId = pcStTypeId AND
    StLinje.PerId    = pcPerId AND
    StLinje.AarPerLinNr >= piFraAarPerLinNr AND
    StLinje.AarPerLinNr <= piTilAarPerLinNr 
    :

    ASSIGN
      dSum = dSum + StLinje.VerdiSolgt
      .
  END.

  RETURN dSum.   /* Function return value. */
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getTjenesteOms) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getTjenesteOms Procedure 
FUNCTION getTjenesteOms RETURNS DECIMAL
  (  ):
  /*------------------------------------------------------------------------------
                  Purpose:                                                                                                                                          
                  Notes:                                                                                                                                            
  ------------------------------------------------------------------------------*/
  DEFINE VARIABLE dSum             AS DECIMAL NO-UNDO.

  DEF    VAR      pcPerId          AS CHAR    NO-UNDO.
  DEF    VAR      pcStTypeId       AS CHAR    NO-UNDO.
  DEF    VAR      piFraAarPerLinNr AS INT     NO-UNDO.
  DEF    VAR      piTilAarPerLinNr AS INT     NO-UNDO.

  ASSIGN
    pcPerId          = 'DAG'
    pcStTypeId       = 'AVDELING'
    piFraAarPerLinNr = INT(
                         STRING(YEAR(pdFraDato),"9999") + 
                         STRING(pdFraDato - DATE(12 , 31, YEAR(pdFraDato) - 1),"999")
                        )
    piTilAarPerLinNr = INT(
                         STRING(YEAR(pdTilDato),"9999") + 
                         STRING(pdTilDato - DATE(12 , 31, YEAR(pdTilDato) - 1),"999")
                        )
    .

  FOR EACH StLinje NO-LOCK WHERE
    StLinje.Butik    = piButNr AND
    StLinje.StTypeId = pcStTypeId AND
    StLinje.PerId    = pcPerId AND
    StLinje.AarPerLinNr >= piFraAarPerLinNr AND
    StLinje.AarPerLinNr <= piTilAarPerLinNr 
    :

    IF iTjHG = INT(STLinje.DataObjekt) THEN 
      ASSIGN
        dSum = dSum + StLinje.VerdiSolgt
        .
  END.

  RETURN dSum.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getTransName) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getTransName Procedure 
FUNCTION getTransName RETURNS CHARACTER
  ( INPUT iTTId AS INTE ) :
  /*------------------------------------------------------------------------------
    Purpose:  
      Notes:  
  ------------------------------------------------------------------------------*/


  FIND transtype WHERE transtype.ttid = iTTId NO-LOCK NO-ERROR.
  RETURN IF AVAIL transtype THEN transtype.beskrivelse ELSE "Unknown".   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

FINALLY:
  EMPTY TEMP-TABLE ttOverfor.
         
END FINALLY.
