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

DEFINE INPUT PARAMETER pcRappType  AS CHARACTER                NO-UNDO.
DEFINE INPUT PARAMETER piButNr     AS INTEGER                  NO-UNDO.
DEFINE INPUT PARAMETER pikasserer  AS INTEGER                  NO-UNDO.
DEFINE INPUT PARAMETER pdFraDato   AS DATE                     NO-UNDO.
DEFINE INPUT PARAMETER pdTilDato   AS DATE                     NO-UNDO.
DEFINE INPUT PARAMETER lBatch      AS LOGICAL                  NO-UNDO.
DEFINE OUTPUT PARAMETER pcFilNavn  AS CHARACTER                NO-UNDO.

DEFINE VARIABLE cSprak             AS CHARACTER                NO-UNDO.
DEFINE VARIABLE cBelopp            AS CHARACTER                NO-UNDO.
DEFINE VARIABLE dColPosBF          AS DECIMAL EXTENT 5         NO-UNDO.
DEFINE VARIABLE dColPosFR          AS DECIMAL EXTENT 6         NO-UNDO.
DEFINE VARIABLE dULstartFR         AS DECIMAL EXTENT 6         NO-UNDO.
DEFINE VARIABLE dColPosMVA         AS DECIMAL EXTENT 4         NO-UNDO.
DEFINE VARIABLE dULstartMVA        AS DECIMAL EXTENT 4         NO-UNDO.
DEFINE VARIABLE iLineSpace         AS INTEGER INIT 11          NO-UNDO.
DEFINE VARIABLE wBruttoOmsetning   AS DECIMAL                  NO-UNDO.
DEFINE VARIABLE wNettoOmsetning    AS DECIMAL                  NO-UNDO.
DEFINE VARIABLE dMvaGrunnlag       AS DECIMAL                  NO-UNDO.
DEFINE VARIABLE dMvaBelop          AS DECIMAL                  NO-UNDO.
DEFINE VARIABLE iAntKunder         AS INTEGER                  NO-UNDO.
DEFINE VARIABLE dTotaltInslaget    AS DECIMAL                  NO-UNDO.
DEFINE VARIABLE lKortSum           AS DECIMAL                  NO-UNDO.
DEFINE VARIABLE lDagensKontStrom   AS DECIMAL                  NO-UNDO.
DEFINE VARIABLE lKasseSlutt        AS DECIMAL                  NO-UNDO.
DEFINE VARIABLE lKasseEndring      AS DECIMAL                  NO-UNDO.
DEFINE VARIABLE lKasseDiff         AS DECIMAL                  NO-UNDO.
DEFINE VARIABLE cBokfNr            AS CHARACTER FORMAT "x(20)" NO-UNDO.
DEFINE VARIABLE iCl                AS INTEGER                  NO-UNDO.
DEFINE VARIABLE cTekst             AS CHARACTER                NO-UNDO.
DEFINE VARIABLE cHKInst            AS CHARACTER                NO-UNDO.
DEFINE VARIABLE cFirstButik        AS CHARACTER                NO-UNDO.
DEFINE VARIABLE lDirekte           AS LOGICAL                  NO-UNDO.
DEFINE VARIABLE cButBatchPrinter   AS CHARACTER                NO-UNDO.
DEFINE VARIABLE iAntallUtbetBonger AS INTEGER                  NO-UNDO.
DEFINE VARIABLE lVerdiUtbetBonger  AS DECIMAL                  NO-UNDO.
DEFINE VARIABLE cLogo              AS CHARACTER                NO-UNDO.
DEFINE VARIABLE cTittel            AS CHARACTER FORMAT "x(25)" NO-UNDO.
DEFINE VARIABLE cFirma             AS CHARACTER FORMAT "x(40)" NO-UNDO.
DEFINE VARIABLE cDato              AS CHARACTER                NO-UNDO.
DEFINE VARIABLE cClInfo            AS CHARACTER FORMAT "x(70)" NO-UNDO.
DEFINE VARIABLE pcOldLst           AS CHARACTER                NO-UNDO.
DEFINE VARIABLE iTjHG              AS INTEGER                  NO-UNDO.
DEFINE VARIABLE lFirstKasserer     AS LOGICAL                  NO-UNDO.

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
    FIELD txt    AS CHARACTER
    FIELD antal  AS INTEGER
    FIELD belopp AS DECIMAL
    FIELD mva    AS LOGICAL
    INDEX vgm IS PRIMARY UNIQUE artobjekt mva.
DEFINE TEMP-TABLE TT_Kasserer NO-UNDO
    FIELD TT_KassererNr AS INTEGER
    INDEX TT_KasIndex IS PRIMARY UNIQUE TT_KassererNr.
  
DEFINE TEMP-TABLE tt_Non_sale_spes NO-UNDO LIKE NON_Sale_Spes.
{tmpKort_spes.i &NEW = NEW &SHARED = SHARED}
{tmpKas_Rap.i &NEW = NEW &SHARED = SHARED}
    DEFINE BUFFER btmpKas_rap FOR tmpKas_rap.

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
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

FIND Butiker WHERE Butiker.Butik = piButNr NO-LOCK NO-ERROR.
IF NOT AVAIL Butiker THEN
    RETURN "FEIL".

{syspara.i  20 5 1 iTjHG INT}
IF iTjHG = ? OR iTjHG = 0 THEN
    ASSIGN iTjHG = 13.


FIND bruker WHERE bruker.brukerid = USERID("skotex") NO-LOCK NO-ERROR.
IF AVAIL bruker THEN
    cSprak = TRIM(Bruker.Lng).
  RUN ValiderKriterier.
  IF RETURN-VALUE <> "OK" THEN
      RETURN "FEIL".
RUN GetKasserer.

ASSIGN lFirstKasserer = TRUE.

FOR EACH TT_Kasserer NO-LOCK:
  RUN DelTT_Tables.
  FIND Forsalj WHERE Forsalj.ForsNr = TT_Kasserer.TT_KassererNr NO-LOCK NO-ERROR.
  
    RUN ByggFinansRapport.
  
    IF RETURN-VALUE = "AVBRYT" THEN
      RETURN "FEIL".    

    RUN ByggKortSpes.
    RUN Bygg_NONS_sale.
    RUN Bygg_tjanster.
    RUN ByggAntKunder.
    IF CAN-FIND(FIRST tmpKas_rap) THEN DO:
/*       OUTPUT TO "CLIPBOARD".  */
/*       FOR EACH btmpKas_rap:   */
/*           EXPORT btmpkas_rap. */
/*       END.                    */
/*       OUTPUT CLOSE.           */
        RUN PDFSamling.
    END.
END.
RUN pdf_close ("Spdf").
    RETURN "OK".

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
    DEFINE VARIABLE cString AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cButikkTxt AS CHARACTER   NO-UNDO.
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
  DEFINE VARIABLE pdDato AS DATE    NO-UNDO.
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
  DEF VAR pdDato       AS DATE NO-UNDO.
  DEFINE VARIABLE lFlereDar AS LOGICAL    NO-UNDO.

  DO:
    ASSIGN
/*       T-Kuntotal */
      lFlereDar  = pdFraDato <> pdTilDato
      .
  END.
  /* Tømmer temp-table */
  EMPTY TEMP-TABLE tmpKas_Rap NO-ERROR.

  /* Bygger opp tabell */
  BYGG:
  DO pdDato = pdFraDato TO pdTilDato:
      /* Henter postene */
      KASSELOOP:
      FOR EACH Kas_Rap NO-LOCK WHERE Kas_Rap.Dato = pdDato AND
                                     Kas_Rap.Butikk = piButNr AND
                                     Kas_Rap.KassererNr = TT_Kasserer.TT_KassererNr
                                     BREAK BY Kas_Rap.Dato:

          /* Sumerer opp bonger med utbetaling */
          ASSIGN
              iAntallUtbetBonger = 0
              lVerdiUtbetBonger  = 0.

          FOR EACH BongHode NO-LOCK WHERE
              BongHode.ButikkNr   = Kas_Rap.Butikk AND
              BongHode.GruppeNr   = 1 AND
              BongHode.KasseNr    = Kas_Rap.Kasse AND
              BongHode.Dato       = pdDato AND        
              BongHode.KassererNr = Kas_Rap.KassererNr AND
              BongHode.Belop      < 0:
              IF CAN-FIND(FIRST BongLinje WHERE
                          BongLinje.B_Id = BongHode.B_Id AND
                          BongLinje.TTId = 50) THEN
              ASSIGN
                  iAntallUtbetBonger = iAntallUtbetBonger + 1
                  lVerdiUtbetBonger  = lVerdiUtbetBonger  + BongHode.Belop.
          END.

          BUTIKKTOTAL: /* Bygger Kassörs-totaler */
          DO:
/*              FIND tmpKas_Rap WHERE tmpKas_Rap.Dato       = pdDato AND  /* Kas_Rap.Dato AND */    ghg 2014-06-04 */
              FIND tmpKas_Rap WHERE tmpKas_Rap.Dato       = pdFraDato AND  /* Kas_Rap.Dato AND */
                                    tmpKas_Rap.Butikk     = Kas_Rap.Butikk AND
                                    tmpKas_Rap.Kasse      = -9999 AND
                                    tmpKas_Rap.KassererNr = Kas_Rap.KassererNr AND
                                    tmpKas_Rap.Z_Nummer   = 0 NO-ERROR.
              IF NOT AVAILABLE tmpKas_Rap THEN
              DO:
                  CREATE tmpKas_Rap.
/*                  ASSIGN tmpKas_Rap.Dato       = pdDato /* Kas_Rap.Dato  */    ghg 2014-06-04 */
                  ASSIGN tmpKas_Rap.Dato       = pdFraDato /* Kas_Rap.Dato  */
                         tmpKas_Rap.Butikk     = Kas_Rap.Butikk 
                         tmpKas_Rap.Kasse      = -9999
                         tmpKas_Rap.KassererNr = Kas_Rap.KassererNr
                         tmpKas_Rap.Z_Nummer   = 0.
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
  DEF VAR pdDato       AS DATE NO-UNDO.
  DEFINE VARIABLE lFlereDar AS LOGICAL    NO-UNDO.
/*  
  DEF INPUT PARAMETER piButNr    AS INT  NO-UNDO.
  DEF INPUT PARAMETER pdFraDato  AS DATE NO-UNDO.
  DEF INPUT PARAMETER pdTilDato  AS DATE NO-UNDO.
  */
  DO WITH FRAME Default-frame:
    ASSIGN lFlereDar  = pdFraDato <> pdTilDato.
  END.
  /* Tømmer temp-table */
  EMPTY TEMP-TABLE tmpKort_Spes NO-ERROR.
/* Bygger opp tabell */
  BYGG:
  DO pdDato = pdFraDato TO pdTilDato:

      /* Henter postene */
      KASSELOOP:
      FOR EACH Kort_Spes NO-LOCK WHERE Kort_Spes.Dato = pdDato AND
                                       Kort_Spes.butikk = piButNr AND
                                       Kort_Spes.KassererNr = TT_Kasserer.TT_KassererNr
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
                   tmpKort_Spes.Dato       = pdDato AND /* Kort_Spes.Dato AND */
                   tmpKort_Spes.Butikk     = Kort_Spes.Butikk AND
                   tmpKort_Spes.Kasse      = Kort_Spes.Kasse AND
                   tmpKort_Spes.KortType   = Kort_Spes.KortType AND
                   tmpKort_Spes.Z_Nummer   = 0 AND
                   tmpKort_Spes.KassererNr = Kort_Spes.KassererNr AND
                   tmpKort_Spes.Sortering  = 0 NO-ERROR.
              IF NOT AVAILABLE tmpKort_Spes THEN
              DO:
                  CREATE tmpKort_Spes.
                  ASSIGN
                      tmpKort_Spes.Dato       = pdDato /* Kort_Spes.Dato  */
                      tmpKort_Spes.Butikk     = Kort_Spes.Butikk 
                      tmpKort_Spes.Kasse      = Kort_Spes.Kasse
                      tmpKort_Spes.KortType   = Kort_Spes.KortType
                      tmpKort_Spes.Z_Nummer   = 0 
                      tmpKort_Spes.KassererNr = Kort_Spes.KassererNr
                      tmpKort_Spes.Sortering  = 0 
                      .
              END.
              IF tmpKort_Spes.KortType = 0 THEN
                  tmpKort_Spes.KortType = 50.
              IF tmpKort_Spes.cKortNavn = "" THEN DO:
                  FIND Syspara WHERE Syspara.SysHId = 20 AND
                                     Syspara.SysGr  = 3  AND
                                     SysPara.ParaNr = tmpKort_Spes.KortType NO-LOCK NO-ERROR.
                  ASSIGN tmpKort_Spes.cKortNavn = IF AVAIL SysPara THEN SysPara.parameter1 ELSE (IF CAN-DO("SE,SVE",cSprak) THEN "Okänt" ELSE "Ukjent")
                         tmpKort_Spes.cKonto    = IF AVAIL SysPara THEN SysPara.parameter2 ELSE "".
              END.
              RUN SummerKortSpes.
          END. /* KASSETOTAL */
          
          BUTIKKTOTAL:
          DO:
              FIND tmpKort_Spes WHERE
                   tmpKort_Spes.Dato       = pdDato AND /* Kort_Spes.Dato AND */
                   tmpKort_Spes.Butikk     = Kort_Spes.Butikk AND
                   tmpKort_Spes.Kasse      = -9999 AND
                   tmpKort_Spes.KortType   = Kort_Spes.KortType AND
                   tmpKort_Spes.KassererNr = Kort_Spes.KassererNr AND
                   tmpKort_Spes.Z_Nummer   = 0 NO-ERROR.
              IF NOT AVAILABLE tmpKort_Spes THEN
              DO:
                  CREATE tmpKort_Spes.
                  ASSIGN
                      tmpKort_Spes.Dato       = pdFraDato /* Kort_Spes.Dato  */
                      tmpKort_Spes.Butikk     = Kort_Spes.Butikk 
                      tmpKort_Spes.Kasse      = -9999
                      tmpKort_Spes.KortType   = Kort_Spes.KortType
                      tmpKort_Spes.KassererNr = Kort_Spes.KassererNr
                      tmpKort_Spes.Z_Nummer   = 0.
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

&IF DEFINED(EXCLUDE-Bygg_NONS_sale) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Bygg_NONS_sale Procedure 
PROCEDURE Bygg_NONS_sale :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE dDato AS DATE    NO-UNDO.
    FOR EACH kasse WHERE kasse.butikknr = piButNr NO-LOCK:
        DO dDato = pdFraDato TO pdTilDato:
            FOR EACH NON_Sale_spes WHERE NON_Sale_spes.butikk = kasse.butikknr AND
                                         NON_Sale_spes.kasse  = kasse.kassenr  AND
                                         NON_Sale_spes.dato   = dDato AND
                                         NON_Sale_spes.KassererNr = TT_Kasserer.TT_KassererNr NO-LOCK:
                FIND tt_NON_Sale_spes WHERE tt_NON_Sale_spes.butikk        = 0 AND
                                            tt_NON_Sale_spes.kasse         = 0 AND
                                            tt_NON_Sale_spes.dato          = dDato AND
                                            tt_NON_Sale_spes.KassererNr    = NON_Sale_spes.KassererNr AND
                                            tt_NON_Sale_spes.Non_Sale_Type = NON_Sale_spes.Non_Sale_Type AND
                                            tt_NON_Sale_Spes.Kode          = NON_Sale_Spes.Kode NO-ERROR.
                IF NOT AVAIL tt_NON_Sale_spes THEN DO:
                    CREATE tt_NON_Sale_Spes.
                    ASSIGN tt_NON_Sale_spes.butikk        = 0
                           tt_NON_Sale_spes.kasse         = 0
                           tt_NON_Sale_spes.dato          = dDato
                           tt_NON_Sale_spes.KassererNr    = NON_Sale_spes.KassererNr 
                           tt_NON_Sale_spes.Non_Sale_Type = NON_Sale_spes.Non_Sale_Type
                           tt_NON_Sale_Spes.Kode          = NON_Sale_Spes.Kode.
                END.
                ASSIGN tt_NON_Sale_Spes.NON_SaleAntall = tt_NON_Sale_Spes.NON_SaleAntall + NON_Sale_Spes.NON_SaleAntall
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
   DEFINE VARIABLE dDato        AS DATE        NO-UNDO.
   DEFINE VARIABLE iAarPerLinNr AS INTEGER     NO-UNDO.
   DEFINE VARIABLE dFgAr31Dec   AS DATE        NO-UNDO.
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
        ASSIGN tt_vg.vg  = vargr.vg
               tt_vg.txt = vargr.vgbeskr
               tt_vg.mva = AVAIL moms AND Moms.MomsProc > 0.
    END.
    FOR EACH tt_vg:
        DO dDato = pdFraDato TO pdTilDato:
            dFgAr31Dec = DATE(12,31,YEAR(dDato) - 1).
            iAarperlinnr = INT(STRING(YEAR(dDato),"9999") + STRING(dDato - dFgAr31Dec,"999")). 
            FOR EACH Stlinje WHERE Stlinje.butik       = piButNr      AND Stlinje.Sttypeid = "VAREGR" AND Stlinje.PerId = "DAG" AND
                                   StLinje.Aarperlinnr = iAarperlinnr AND StLinje.Dataobjekt = STRING(tt_vg.vg,"999999") NO-LOCK:
                ASSIGN tt_vg.antal  = tt_vg.antal  + StLinje.AntSolgt
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
        ASSIGN tt_ArtHG.artobjekt = STRING(artbas.artikkelnr,"9999999999999")
               tt_ArtHG.txt = Artbas.beskr
               tt_ArtHG.mva = AVAIL moms AND Moms.MomsProc > 0.
    END.
    FOR EACH tt_ArtHG:
        DO dDato = pdFraDato TO pdTilDato:
            dFgAr31Dec = DATE(12,31,YEAR(dDato) - 1).
            iAarperlinnr = INT(STRING(YEAR(dDato),"9999") + STRING(dDato - dFgAr31Dec,"999")). 
            FOR EACH Stlinje WHERE Stlinje.butik       = piButNr      AND Stlinje.Sttypeid = "ARTIKKEL" AND Stlinje.PerId = "DAG" AND
                                   StLinje.Aarperlinnr = iAarperlinnr AND StLinje.Dataobjekt = STRING(tt_ArtHG.artobjekt,"999999") NO-LOCK:
                ASSIGN tt_ArtHG.antal  = tt_ArtHG.antal  + StLinje.AntSolgt
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

&IF DEFINED(EXCLUDE-DelTT_TABLES) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DelTT_TABLES Procedure 
PROCEDURE DelTT_TABLES :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
EMPTY TEMP-TABLE TT_KontHg NO-ERROR.

EMPTY TEMP-TABLE TT_vg NO-ERROR.

EMPTY TEMP-TABLE TT_ArtHg NO-ERROR.

EMPTY TEMP-TABLE tmpKas_Rap NO-ERROR.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-GetKasserer) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetKasserer Procedure 
PROCEDURE GetKasserer :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE pdDato2      AS DATE NO-UNDO.
  DO pdDato2 = pdFraDato TO pdTilDato:
      /* Henter postene */
      FOR EACH Kas_Rap NO-LOCK WHERE Kas_Rap.Dato = pdDato2 AND
                                     Kas_Rap.Butikk = piButNr
                                     BREAK BY Kas_Rap.Dato:
       FIND TT_Kasserer WHERE TT_Kasserer.TT_KassererNr = Kas_rap.KassererNr NO-LOCK NO-ERROR.
       IF NOT AVAILABLE TT_Kasserer THEN
       DO:
         CREATE TT_Kasserer.
         ASSIGN TT_Kasserer.TT_KassererNr = Kas_rap.KassererNr.
       END.

      END. /* KASSELOOP */

  END. /* BYGG */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-HentKassererRapport) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE HentKassererRapport Procedure 
PROCEDURE HentKassererRapport :
/*------------------------------------------------------------------------------
  Purpose:     Adderer inn kassereroppgjøret i butikktotalen når den første 
               gang opprettes.
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  /* Adderer opp opptalte verdier. */
  OPPGJOR:
  FOR EACH KassererOppgj NO-LOCK WHERE
    KassererOppgj.Dato     = Kas_Rap.Dato AND
    KassererOppgj.Butikk   = Kas_Rap.Butikk AND
    KassererOppgj.KassererNr = Kas_Rap.KassererNr:
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

&IF DEFINED(EXCLUDE-PageFooter) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PageFooter Procedure 
PROCEDURE PageFooter :
/*------------------------------------------------------------------------------
  Purpose:  Procedure to Print Page Footer -- on all pages.
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cSidTxt AS CHARACTER   NO-UNDO.
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
  RUN pdf_text_xy_dec ("Spdf",cTittel + "        Kassör: " + STRING(TT_Kasserer.TT_KassererNr) + " " + Forsalj.navnikasse,pdf_LeftMargin ("Spdf"),pdf_PageHeight("Spdf") - 45).
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
  DEF VAR piLoop           AS INT    NO-UNDO.
  DEF VAR pcOverskr        AS CHAR    NO-UNDO.
  DEFINE VARIABLE dY       AS DECIMAL     NO-UNDO.
  DEFINE VARIABLE wOK      AS LOGICAL    NO-UNDO.
  DEFINE VARIABLE cPrinter AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE dYspara  AS DECIMAL     NO-UNDO.
  DEFINE VARIABLE cEmail   AS CHARACTER NO-UNDO.
  ASSIGN
      lDagensKontStrom = 0
      lKasseEndring    = 0
      lKasseSlutt      = 0.
 DO:
/*      ASSIGN                                                          */
/*        lKasseSlutt = tmpKas_Rap.OpptaltVeksel /* Ved dagens slutt */ */
/*                      + tmpKas_Rap.OpptaltKontanter                   */
/*                      + tmpKas_Rap.OpptaltSjekk                       */
/*                      + tmpKas_Rap.OpptaltReserve                     */
/*                      + tmpKas_Rap.OpptaltValuta                      */
/*          .                                                           */
/*      ASSIGN                                                             */
/*        lDagensKontStrom = wBruttoOmsetning                              */
/*                           - tmpKas_Rap.TilgodeInn                       */
/*                           /*- tmpKas_Rap.Gavekort */                    */
/*                           - 0 /* Gavekort Universal */                  */
/*                           - tmpKas_Rap.Kupong1                          */
/*                           - tmpKas_Rap.Kupong2                          */
/*                           + tmpKas_Rap.TilgodeUt                        */
/*                           + tmpKas_Rap.GavekortUt                       */
/*                           - tmpKas_Rap.GavekortInn                      */
/*                           - tmpKas_Rap.Kredit                           */
/*                           + tmpKas_Rap.InnbetaltKunde                   */
/*                           - (tmpKas_Rap.kont_ut - (tmpKas_Rap.kont_in)) */
/*                           - (tmpKas_Rap.Bank + tmpKas_Rap.CashBack)     */
/*                           - tmpKas_Rap.Reservelosning                   */
/*                           + tmpKas_Rap.LayAway_Ut                       */
/*                           - tmpKas_Rap.LayAway_Inn                      */
/*                           - tmpKas_Rap.Dropp                            */
/*        .                                                                */
    {syspara.i  1 1 100 cFirma}
    cDato = (IF CAN-DO("SE,SVE",cSprak) THEN "Datum: " ELSE "Dato: ") + STRING(pdFraDato).
    IF pdTilDato > pdFraDato THEN
        cDato = cDato + " - " + STRING(pdTilDato).
    /* Leser alle kortspesifikasjoner */
    FOR EACH tmpKort_Spes NO-LOCK WHERE tmpKort_Spes.Butikk   = piButNr AND tmpKort_Spes.Kasse    = -9999:
      ASSIGN lDagensKontStrom = lDagensKontStrom - tmpKort_Spes.Belop.
    END.
    ASSIGN lKasseEndring    = lKasseSlutt   - tmpKas_Rap.OpptaltInnVeksel
           lKasseDiff       = lKasseEndring - lDagensKontStrom.
 
  IF lFirstKasserer = TRUE THEN
    ASSIGN pcFilNavn = SESSION:TEMP-DIR + "Samlingsrapport" + "_" + STRING(piKasserer) + "_" + STRING(TIME) + ".pdf".
   /* Åpner stream til skriverfil. */

/*  IF lBatch = TRUE THEN DO:
      ASSIGN cButBatchPrinter = TRIM(Butiker.RAPPrinter).
      {syspara.i  210 271 piButnr cEmail}
  END.*/
  
  IF lFirstKasserer = TRUE THEN
  DO:
    RUN pdf_new ("Spdf",pcFilNavn).
    ASSIGN lFirstKasserer = FALSE.
  END.
/*   pdf_PageHeader ("Spdf",THIS-PROCEDURE:HANDLE,"PageFooter"). */
  pdf_PageFooter ("Spdf",THIS-PROCEDURE:HANDLE,"PageFooter").
  RUN pdf_set_PaperType ("Spdf","A4").
  RUN pdf_set_LeftMargin ("Spdf", 40).
  RUN pdf_set_BottomMargin ("Spdf", 60).
  RUN pdf_set_VerticalSpace IN h_PDFinc ("Spdf",13).
  RUN pdf_set_Orientation ("Spdf","portrait").

  RUN SetPositioner.

  ASSIGN wBruttoOmsetning = 0
         wNettoOmsetning = 0.
  /* Leser temp-table postene */
  TEMP-TABLEN:
/*       FOR EACH tmpKas_Rap WHERE tmpKas_Rap.Sortering = 0: */
    FOR EACH tmpKas_Rap WHERE  tmpKas_Rap.butikk = piButNr AND tmpKas_Rap.Kasse = -9999 AND tmpKas_Rap.KassererNr = TT_Kasserer.TT_KassererNr
                        BREAK BY tmpKas_Rap.Dato BY tmpKas_Rap.butikk BY tmpKas_Rap.Kasse BY tmpKas_Rap.Z_Nummer:
        wBruttoOmsetning = 0.
        DO piLoop = 1 TO 10:       
            ASSIGN wNettoOmsetning = wNettoOmsetning + tmpKas_rap.MvaGrunnlag[piLoop] + tmpKas_rap.MvaBelop[piLoop].             
        END.                       

        wBruttoOmsetning = wNettoOmsetning + 
                           tmpKas_Rap.Retur  + 
                           tmpKas_Rap.Reklamasjon  +
                           tmpKas_Rap.GenerellRabatt +
                           tmpKas_Rap.Kunderabatt +   
                           tmpKas_Rap.Personalrabatt +
                           tmpKas_Rap.Medlemsrabatt + 
                           tmpKas_Rap.Pakkerabatt.
/*            +                                     */
/*                            tmpKas_Rap.Avrunding. */


/*         ASSIGN wBruttoOmsetning = tmpKas_Rap.kontant +                                                      */
/*                                   tmpKas_Rap.sjekk +                                                        */
/*                                   tmpKas_Rap.kort +                                                         */
/*                                   tmpKas_Rap.kredit +                                                       */
/*                                   tmpKas_Rap.kupong1 +                                                      */
/*                                   tmpKas_Rap.kupong2 +                                                      */
/*                                   tmpKas_Rap.Tilgode +                                                      */
/*                                   tmpKas_Rap.Bank - tmpKas_Rap.InnbetaltKunde /* - tmpKas_Rap.Cashback */ + */
/*                                   tmpKas_Rap.Reservelosning -                                               */
/*                                   /*tmpKas_Rap.Gavekort - */                                                */
/*                                   /*tmpKas_Rap.GavekortUt*/                                                 */
/*                                   tmpKas_Rap.Non_SalePos +                                                  */
/*                                   tmpKas_Rap.GavekortInn +                                                  */
/*                                   tmpKas_Rap.Avrunding -                                                    */
/*                                   (tmpKas_Rap.Kont_Inn -                                                    */
/*                                    tmpKas_Rap.Kont_Ut                                                       */
/*                                   ) +                                                                       */
/*                                   tmpKas_Rap.GenerellRabatt +                                               */
/*                                   tmpKas_Rap.Kunderabatt +                                                  */
/*                                   tmpKas_Rap.Personalrabatt +                                               */
/*                                   tmpKas_Rap.Medlemsrabatt +                                                */
/*                                   tmpKas_Rap.Pakkerabatt +                                                  */
/*                                   tmpKas_Rap.layaway_inn -                                                  */
/*                                   tmpKas_Rap.layaway_Ut +                                                   */
/*                                   (tmpKas_Rap.Retur + tmpKas_Rap.Reklamasjon)                               */
/*                                   .                                                                         */


/*     IF piRappType = 1 OR piRappType = 5 THEN DO: */
    IF CAN-DO(pcRappType,"1") THEN DO:
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
    IF CAN-DO(pcRappType,"2") THEN DO:
        /* Sida 2 */
        RUN pdf_new_page ("Spdf").
        RUN ButikRubrik.
        cTittel     = IF CAN-DO("SE,SVE",cSprak) THEN "Bokföringsrapport" ELSE "Bokføringsbilag".
        RUN PageHeader.
        dY = pdf_PageHeight ("Spdf") - 110.
        RUN Saml_2_mva(INPUT-OUTPUT dY).
        dYSpara = dY.
        RUN Saml_2_betalat(INPUT-OUTPUT dY).
        RUN Saml_2_utbetalt(INPUT-OUTPUT dY).
        RUN Saml_2_kredit(INPUT-OUTPUT dY).
        RUN Saml_2_in_ut(INPUT-OUTPUT dY).
        RUN Saml_2_Dagensstrom(INPUT-OUTPUT dY).
        dY = dYSpara.
/*         RUN Saml_2_BilagSpes(INPUT-OUTPUT dY). */
/*         RUN Saml_2_Diverse(INPUT-OUTPUT dY). */
    END.
/*     IF piRappType = 3 OR piRappType = 5 THEN DO: */
    IF CAN-DO(pcRappType,"3") THEN DO:
        /* Sida 3 */
        RUN pdf_new_page ("Spdf").
        RUN ButikRubrik.
        cTittel     = IF CAN-DO("SE,SVE",cSprak) THEN "Detaljerad specifikation" ELSE "Detaljert spesifikasjon".
        RUN PageHeader.
        dY = pdf_PageHeight ("Spdf") - 110.
        RUN Saml_3_Detaljspec(INPUT-OUTPUT dY).
    END.
/*     IF piRappType = 4 OR piRappType = 5 THEN DO: */
    IF CAN-DO(pcRappType,"4") THEN DO:
        /* Sida 4 */
        RUN pdf_new_page ("Spdf").
        RUN ButikRubrik.
        cTittel     = IF CAN-DO("SE,SVE",cSprak) THEN "Konteringsrapport per huvudgrupp" ELSE "Konteringsrapport per hovedgruppe".
        RUN PageHeader.
        dY = pdf_PageHeight ("Spdf") - 110.
        RUN Saml_4_Kont_hg(INPUT-OUTPUT dY).
    END.
  END. /* TEMP-TABLEN */
  IF CAN-DO(pcRappType,"5") THEN DO:
      /* Sida 4 */
      RUN pdf_new_page ("Spdf").
      RUN ButikRubrik.
      cTittel     = IF CAN-DO("SE,SVE",cSprak) THEN "Tjänster per artikel" ELSE "Tjenester per artikkel".
      RUN PageHeader.
      dY = pdf_PageHeight ("Spdf") - 110.
      RUN Saml_5_DetalArtHG13(INPUT-OUTPUT dY).
  END.

/*  RUN pdf_close ("Spdf").*/
/*  IF cEmail <> "" THEN DO:
      RUN sendEmail IN THIS-PROCEDURE (cEmail,pcFilnavn).
  END.
  ELSE IF cButBatchPrinter <> "" THEN DO:
      IF SEARCH(".\cmd\PrintPdf.cmd") <> ? THEN DO:
          OS-COMMAND SILENT VALUE(".\cmd\PrintPdf.cmd" + " " + pcFilnavn + " " + '"' + cButBatchPrinter + '"').
      END.
  END.
/*   RUN browse2pdf\viewxmldialog.w (pcFilNavn,"Rapport"). */

  STATUS DEFAULT " ".*/
END.
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
DEFINE VARIABLE cLabel  AS CHARACTER EXTENT 3  NO-UNDO.
DEFINE VARIABLE cTxt    AS CHARACTER   NO-UNDO.
    IF CAN-DO("SE,SVE",cSprak) THEN DO:
        ASSIGN cOverskr[1] = "Bank"
               cOverskr[2] = "Antal"
               cOverskr[3] = "Belopp".
        ASSIGN cLabel[1] = "Försäljning bankkort"      
               cLabel[2] = "Cash Back"                
               cLabel[3] = "Totalt bank".
    END.
    ELSE DO:
        ASSIGN cOverskr[1] = "Bank"
               cOverskr[2] = "Antall"
               cOverskr[3] = "Beløp".
        ASSIGN cLabel[1] = "Salg bankkort"
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
    dY = dY - iLineSpace.
    RUN pdf_text_xy_dec ("Spdf",cLabel[1],dColPosFR[1],dY).
    cTxt = TRIM(STRING(tmpKas_Rap.AntBank + tmpKas_Rap.AntKort,"->>,>>9")).
    RUN pdf_text_xy_dec ("Spdf",cTxt,dColPosFR[2] - bredd(cTxt),dY).
    cBelopp = TRIM(STRING(tmpKas_Rap.Bank + tmpKas_Rap.Kort,"->>>,>>>,>>9.99")).
    RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPosFR[3] - bredd(cBelopp),dY).
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
DEFINE VARIABLE cOverskr AS CHARACTER EXTENT 3  NO-UNDO.
DEFINE VARIABLE cLabel   AS CHARACTER EXTENT 11 NO-UNDO.

    IF CAN-DO("SE,SVE",cSprak) THEN DO:
        ASSIGN cOverskr[1] = "In- och utbetalningar"
               cOverskr[2] = "Antal"
               cOverskr[3] = "Belopp".
        ASSIGN cLabel[1] = "Utbetalt"      
               cLabel[2] = "Inbetalt"                
               cLabel[3] = "Nonsale + (Inbetalt)"        
               cLabel[4] = "Nonsale - (Utbetalt)"             
               cLabel[5] = "Tillgodo in"         
               cLabel[6] = "Tillgodo ut"          
               cLabel[7] = "Dep in"       
               cLabel[8] = "Dep ut"       
               cLabel[9] = "Presentkort in"             
               cLabel[10] = "Presentkort ut"
               cLabel[11] = "Presentkortsrabatt".
    END.
    ELSE DO:
        ASSIGN cOverskr[1] = "Inn- og utbetalinger"
               cOverskr[2] = "Antall"
               cOverskr[3] = "Beløp".
        ASSIGN cLabel[1]  = "Utbetalt"      
               cLabel[2]  = "Innbetalt"                
               cLabel[3]  = "Nonsale + (Innbetalt)"        
               cLabel[4]  =  "Nonsale - (Utbetalt)"             
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
    cBelopp = TRIM(STRING(tmpKas_Rap.AntTilgodeInn,"->>,>>9")).
    RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPosFR[5] - bredd(cBelopp),dY).
    cBelopp = TRIM(STRING(tmpKas_Rap.TilgodeInn,"->>>,>>>,>>9.99")).
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
DEFINE VARIABLE cLabel  AS CHARACTER EXTENT  7  NO-UNDO.
DEFINE VARIABLE cTxt    AS CHARACTER   NO-UNDO.
    IF CAN-DO("SE,SVE",cSprak) THEN DO:
        ASSIGN cOverskr[1] = "Justeringar"
               cOverskr[2] = "Antal"
               cOverskr[3] = "Belopp".
        ASSIGN cLabel[1] = "Ingående interna överföringar"      
               cLabel[2] = "Utgående interna överföringar"                
               cLabel[3] = "Varumottagningar"        
               cLabel[4] = "Lagerjusteringar"             
               cLabel[5] = "Kassation"         
               cLabel[6] = "Intern förbrukning"          
               cLabel[7] = "Rensad varuförsäljning".       
    END.
    ELSE DO:
        ASSIGN cOverskr[1] = "Justeringer"
               cOverskr[2] = "Antall"
               cOverskr[3] = "Beløp".
        ASSIGN cLabel[1] = "Inngående interne overføringer"      
               cLabel[2] = "Utgående interne overføringer"                
               cLabel[3] = "Varemottak"        
               cLabel[4] = "Lagerjusteringer"             
               cLabel[5] = "Brekkasje"         
               cLabel[6] = "Internt forbruk"          
               cLabel[7] = "Renset varesalg".       
    END.
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
    /* 1 Ingående interna överf */
    dY = dY - iLineSpace.
    RUN pdf_text_xy_dec ("Spdf",cLabel[1],dColPosFR[1],dY).
    cBelopp = TRIM(STRING(tmpKas_Rap.AntOverfortInn,"->>,>>9")).
    RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPosFR[2] - bredd(cBelopp),dY).
    cBelopp = TRIM(STRING(tmpKas_Rap.OverfortInn,"->>>,>>>,>>9.99")).
    RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPosFR[3] - bredd(cBelopp),dY).
    /* 2 Utgående interna överf */
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
    /* 6 Internt förbruk */
    dY = dY - iLineSpace.
    RUN pdf_text_xy_dec ("Spdf","-",dColPosFR[1] - 5,dY).
    RUN pdf_text_xy_dec ("Spdf",cLabel[6],dColPosFR[1],dY).
    cBelopp = TRIM(STRING(tmpKas_Rap.AntInterntForbruk,"->>,>>9")).
    RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPosFR[2] - bredd(cBelopp),dY).
    cBelopp = TRIM(STRING(tmpKas_Rap.InterntForbruk,"->>>,>>>,>>9.99")).
    RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPosFR[3] - bredd(cBelopp),dY).
    /* 7 Rensad försäljning */
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
DEFINE VARIABLE cOverskr AS CHARACTER EXTENT 3  NO-UNDO.
DEFINE VARIABLE cLabel   AS CHARACTER EXTENT 12 NO-UNDO.

    IF CAN-DO("SE,SVE",cSprak) THEN DO:
        ASSIGN cOverskr[1] = "Kassaredovisning (inslaget)"
               cOverskr[2] = "Antal"
               cOverskr[3] = "Belopp".
        ASSIGN cLabel[1] = "Kontant"      
               cLabel[2] = "Check/trasig sedel CG"                
               cLabel[3] = "Bank"        
               cLabel[4] = "Kredit"             
               cLabel[5] = "Off-line betalning"         
               cLabel[6] = "Presentkort ut"          
               cLabel[7] = "Presentkort in"       
               cLabel[8] = "Tillgodo"       
               cLabel[9] = "Kort"             
               cLabel[10] = "Kupong 1"
               cLabel[11] = "Kupong 2"
               cLabel[12] = "Totalt i kassa (inslaget)".
    END.
    ELSE DO:
        ASSIGN cOverskr[1] = "Kasseregnskap (innslaget)"
               cOverskr[2] = "Antall"
               cOverskr[3] = "Beløp".
        ASSIGN cLabel[1] = "Kontant"      
               cLabel[2] = "Check/"                
               cLabel[3] = "Bank"        
               cLabel[4] = "Kreditt"             
               cLabel[5] = "Off-line betaling"         
               cLabel[6] = "Gavekort ut"          
               cLabel[7] = "Gavekort in"       
               cLabel[8] = "Tilgode"       
               cLabel[9] = "Kort"             
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
    cBelopp = TRIM(STRING(tmpKas_Rap.AntBank + tmpKas_Rap.AntKort,"->>,>>9")).
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
                     tmpKas_Rap.Kredit + tmpKas_Rap.Reservelosning - tmpKas_Rap.GavekortUt + tmpKas_Rap.GavekortInn + tmpKas_Rap.Tilgode +
                     tmpKas_Rap.Kort + tmpKas_Rap.Kupong1 + tmpKas_Rap.Kupong2 + tmpKas_Rap.dropp.
    cBelopp = STRING(dTotaltInslaget,"->>>,>>>,>>9.99").
    RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPosFR[6] - bredd(cBelopp),dY).
    dY = dY - 4.
    RUN pdf_line IN h_PDFinc  ("Spdf", dColPosFR[4], dY, dColPosFR[6], dY, 0.5).

    /* här justerar vi totaltinslaget att jämföras med kassereroppgör */
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
DEFINE VARIABLE cOverskr         AS CHARACTER EXTENT 3  NO-UNDO.
DEFINE VARIABLE cLabel           AS CHARACTER EXTENT 13 NO-UNDO.
DEFINE VARIABLE dSumOpptalt      AS DECIMAL     NO-UNDO.
    IF CAN-DO("SE,SVE",cSprak) THEN DO:
        ASSIGN cOverskr[1] = "Uppräknat enligt kassör"
               cOverskr[2] = "Antal"
               cOverskr[3] = "Belopp".
        ASSIGN cLabel[1] = "Kontant behållning"      
               cLabel[2] = "Växelbehållning"                
               cLabel[3] = "Check/trasig sedel CG"        
               cLabel[4] = "Dropp"             
               cLabel[5] = "Reserv bank"         
               cLabel[6] = "Rekvisition"          
               cLabel[7] = "Presentkort"
               cLabel[8] = "Tillgodokvitton"
               cLabel[9] = "Kupong"
               cLabel[10] = "Valuta"
               cLabel[11] = "Totalt uppräknat"       
               cLabel[12] = "Totalt inslaget"
               cLabel[13] = "Differens kassa"
            .
    END.
    ELSE DO:
        ASSIGN cOverskr[1] = "Beholdning"
               cOverskr[2] = "Antall"
               cOverskr[3] = "Beløp".
        ASSIGN cLabel[1]  = "Kontant beholdning"      
               cLabel[2]  = "Veksel"                
               cLabel[3]  = "Check"        
               cLabel[4]  =  "Dropp"             
               cLabel[5]  = "Reserve bank"         
               cLabel[6]  = "Rekvisisjon"          
               cLabel[7]  = "Gavekort"
               cLabel[8]  = "Tilgodelapper"
               cLabel[9]  = "Kupong"
               cLabel[10]  = "Valuta"       
               cLabel[11]  = "Sum beholdning"       
               cLabel[12]  = "Totalt innslaget"
               cLabel[13]  = "Differens kasse"
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
    cBelopp = TRIM(STRING(tmpKas_Rap.OpptaltKontanter,"->>>,>>>,>>9.99")).
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
    cBelopp = TRIM(STRING(tmpKas_Rap.OpptaltTilgode + tmpKas_Rap.OpptaltTilgodeAndre,"->>>,>>>,>>9.99")).
    RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPosFR[6] - bredd(cBelopp),dY).
    dY = dY - iLineSpace. /* 9 Kupong */
    RUN pdf_text_xy_dec ("Spdf",cLabel[9],dColPosFR[4],dY).
    cBelopp = TRIM(STRING(tmpKas_Rap.OpptaltKupong,"->>>,>>>,>>9.99")).
    RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPosFR[6] - bredd(cBelopp),dY).
    dY = dY - iLineSpace. /* 10 Valuta */
    RUN pdf_text_xy_dec ("Spdf",cLabel[10],dColPosFR[4],dY).
    cBelopp = TRIM(STRING(tmpKas_Rap.OpptaltValuta,"->>>,>>>,>>9.99")).
    RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPosFR[6] - bredd(cBelopp),dY).

    ASSIGN dSumOpptalt = tmpKas_Rap.OpptaltKontanter + tmpKas_Rap.OpptaltVeksel + tmpKas_Rap.OpptaltSjekk +
                         tmpKas_Rap.Dropp + tmpKas_Rap.OpptaltReserve + tmpKas_Rap.Rekvisisasjon +
                         tmpKas_Rap.OpptaltGavekort + tmpKas_Rap.OpptaltGavekortAndre + 
                         tmpKas_Rap.OpptaltTilgode + tmpKas_Rap.OpptaltTilgodeAndre +
                         tmpKas_Rap.OpptaltKupong + tmpKas_Rap.OpptaltValuta.

    dY = dY - 4.
    RUN pdf_line IN h_PDFinc  ("Spdf", dColPosFR[4], dY, dColPosFR[6], dY, 0.5).
    dY = dY - iLineSpace. /* 11 Totalt uppräknat */
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
  Notes:       Oppstilling Kontant midt på venstre side av samlingsrapporten.
------------------------------------------------------------------------------*/
DEFINE INPUT-OUTPUT PARAMETER dY AS DECIMAL     NO-UNDO.
DEFINE VARIABLE cOverskr AS CHARACTER EXTENT 3 NO-UNDO.
DEFINE VARIABLE cLabel  AS CHARACTER EXTENT 3  NO-UNDO.
DEFINE VARIABLE cTxt    AS CHARACTER   NO-UNDO.
    IF CAN-DO("SE,SVE",cSprak) THEN DO:
        ASSIGN cOverskr[1] = "Kontant"
               cOverskr[2] = "Antal"
               cOverskr[3] = "Belopp".
        ASSIGN cLabel[1] = "Kontant"      
               cLabel[2] = "Check/Trasig sedel CG"                
               cLabel[3] = "Totalt kontant".
    END.
    ELSE DO:
        ASSIGN cOverskr[1] = "Kontant"
               cOverskr[2] = "Antall"
               cOverskr[3] = "Beløp".
        ASSIGN cLabel[1] = "Kontant"
               cLabel[2] = "Sjekk/"
               cLabel[3] = "Totalt bank".
    END.
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
       DEFINE VARIABLE cOverskr AS CHARACTER EXTENT 5  NO-UNDO.
       DEFINE VARIABLE cLabel  AS CHARACTER            NO-UNDO.
       DEFINE VARIABLE cTxt    AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE iCount    AS INTEGER    NO-UNDO.
       /* Mva regnskap totalt varesalg */
   IF CAN-DO("SE,SVE",cSprak) THEN DO:
       ASSIGN cOverskr[1] = "Moms försäljning"
              cOverskr[2] = "Grupp"
              cOverskr[3] = "Underlag"
              cOverskr[4] = "Belopp"
              cOverskr[5] = "Summa".
       ASSIGN cLabel   = "Totalt".
   END.
   ELSE DO:
       ASSIGN cOverskr[1] = "Mvaregnskap"
              cOverskr[2] = "Gruppe"
              cOverskr[3] = "Grunnlag"
              cOverskr[4] = "Beløp"
              cOverskr[5] = "Sum".
       ASSIGN cLabel   = "Totalt".
   END.
   ASSIGN dColPosMVA[1] = 80
          dColPosMVA[2] = 140
          dColPosMVA[3] = 190
          dColPosMVA[4] = dColPosFR[3].
   ASSIGN dULstartMVA[1] = pdf_LeftMargin ("Spdf")
          dULstartMVA[2] = 90
          dULstartMVA[3] = 150
          dULstartMVA[4] = 200.

/*        PUT UNFORMATTED                                                                                */
/*            "<P12><R+1><C6><B>"  ENTRY(1,pcOverskr,CHR(1)) SKIP "</B><C6><FROM><C35><LINE>" SKIP       */
/*            "<P8>"                                                                                     */
/*            "<C6><RIGHT=C+6>" ENTRY(2,pcOverskr,CHR(1))                                                */
/*            "<C13><RIGHT=C+7>" ENTRY(3,pcOverskr,CHR(1))                                               */
/*            "<C21><RIGHT=C+6>" ENTRY(4,pcOverskr,CHR(1))                                               */
/*            "<C28><RIGHT=C+7>" ENTRY(5,pcOverskr,CHR(1)) SKIP                                          */
/*            "<C6><FROM><C12><LINE><C13><FROM><C20><LINE><C21><FROM><C27><LINE><C28><FROM><C35><LINE>". */

   dY = dY - 20.
   RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold",12).
   RUN pdf_text_xy_dec ("Spdf",cOverskr[1],dColPosFR[1],dY).
   RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica",8).
   dY = dY - 4.
   RUN pdf_line IN h_PDFinc  ("Spdf", pdf_LeftMargin ("Spdf"), dY, dColPosFR[3], dY, 0.5).
   dY = dY - iLineSpace.
   RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold",8).

   RUN pdf_text_xy_dec ("Spdf",cOverskr[2],dColPosMVA[1] - bredd(cOverskr[5]),dY).
   RUN pdf_text_xy_dec ("Spdf",cOverskr[3],dColPosMVA[2] - bredd(cOverskr[3]),dY).
   RUN pdf_text_xy_dec ("Spdf",cOverskr[4],dColPosMVA[3] - bredd(cOverskr[4]),dY).
   RUN pdf_text_xy_dec ("Spdf",cOverskr[5],dColPosMVA[4] - bredd(cOverskr[5]),dY).
   dY = dY - 4.
   RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica",8).
   RUN pdf_line IN h_PDFinc  ("Spdf", dULstartMVA[1], dY, dColPosMVA[1], dY, 0.5).
   RUN pdf_line IN h_PDFinc  ("Spdf", dULstartMVA[2], dY, dColPosMVA[2], dY, 0.5).
   RUN pdf_line IN h_PDFinc  ("Spdf", dULstartMVA[3], dY, dColPosMVA[3], dY, 0.5).
   RUN pdf_line IN h_PDFinc  ("Spdf", dULstartMVA[4], dY, dColPosMVA[4], dY, 0.5).


   DO iCount = 1 TO 10:
       IF tmpKas_rap.MvaGrunnlag[iCount] <> 0 THEN DO:
       dY = dY - iLineSpace.
       FIND moms WHERE moms.momskod = tmpKas_rap.MvaGrp[iCount] NO-LOCK NO-ERROR.
       cTxt = IF AVAIL moms THEN STRING(moms.momsproc,">9.99") + "%"  ELSE "%".
       cBelopp = TRIM(STRING(tmpKas_rap.MvaGrp[iCount],">9")).
       RUN pdf_text_xy_dec ("Spdf",cBelopp,dULstartMVA[1],dY).
       RUN pdf_text_xy_dec ("Spdf",cTxt,dColPosMVA[1] - bredd(cTxt),dY).
       cBelopp = TRIM(STRING(tmpKas_rap.MvaGrunnlag[iCount],"->>>,>>>,>>9.99")).
       RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPosMVA[2] - bredd(cBelopp),dY).
       cBelopp = TRIM(STRING(tmpKas_rap.MvaBelop[iCount],"->>>,>>>,>>9.99")).
       RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPosMVA[3] - bredd(cBelopp),dY).
       cBelopp = TRIM(STRING(tmpKas_rap.MvaGrunnlag[iCount] + tmpKas_rap.MvaBelop[iCount],"->>>,>>>,>>9.99")).
       RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPosMVA[4] - bredd(cBelopp),dY).
       
       ASSIGN dMvaGrunnlag = dMvaGrunnlag + tmpKas_rap.MvaGrunnlag[iCount]
              dMvaBelop    = dMvaBelop    + tmpKas_rap.MvaBelop[iCount].
       END.
   END.
   dY = dY - 4.
   RUN pdf_line IN h_PDFinc  ("Spdf", dULstartMVA[2], dY, dColPosMVA[2], dY, 0.5).
   RUN pdf_line IN h_PDFinc  ("Spdf", dULstartMVA[3], dY, dColPosMVA[3], dY, 0.5).
   RUN pdf_line IN h_PDFinc  ("Spdf", dULstartMVA[4], dY, dColPosMVA[4], dY, 0.5).
   dY = dY - iLineSpace.
   RUN pdf_text_xy_dec ("Spdf",cLabel,dColPosMVA[1] - bredd(cBelopp),dY).
   cBelopp = TRIM(STRING(dMvaGrunnlag,"->>>,>>>,>>9.99")).
   RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPosMVA[2] - bredd(cBelopp),dY).
   cBelopp = TRIM(STRING(dMvaBelop,"->>>,>>>,>>9.99")).
   RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPosMVA[3] - bredd(cBelopp),dY).
   cBelopp = TRIM(STRING(dMvaGrunnlag + dMvaBelop,"->>>,>>>,>>9.99")).
   RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPosMVA[4] - bredd(cBelopp),dY).


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

DEFINE VARIABLE cOverskr    AS CHARACTER EXTENT 7  NO-UNDO.
DEFINE VARIABLE cLabel      AS CHARACTER EXTENT 4 NO-UNDO.
DEFINE VARIABLE dColPos     AS DECIMAL EXTENT 8    NO-UNDO.
DEFINE VARIABLE dULStart    AS DECIMAL EXTENT 8    NO-UNDO.
DEFINE VARIABLE dFsgsum_kr  AS DECIMAL     NO-UNDO.
DEFINE VARIABLE dPerKunde   AS DECIMAL     NO-UNDO.
DEFINE VARIABLE dTB         AS DECIMAL     NO-UNDO.
DEFINE VARIABLE dTB%        AS DECIMAL     NO-UNDO.
DEFINE VARIABLE dTjanstoms  AS DECIMAL     NO-UNDO.
DEFINE VARIABLE plMvaGrunnlag AS DECIMAL NO-UNDO.
DEFINE VARIABLE plMvaBelop    AS DECIMAL NO-UNDO.

    /* Hentes fra statistikk */
    dTjanstoms    = DYNAMIC-FUNCTION('getTjenesteOms':U).
    plMvaGrunnlag = DYNAMIC-FUNCTION('getMvaGrunnlag':U).

    ASSIGN dFsgsum_kr = dMvaGrunnlag + dMvaBelop
           dTB        = DYNAMIC-FUNCTION('getDBKr':U)
           dTB%       = ROUND((dTb / dMvaGrunnlag) * 100,2).
    ASSIGN dPerKunde  = ROUND(dFsgsum_kr / iAntKunder,2) NO-ERROR.
    IF dPerKunde = ? THEN
        dPerKunde = 0.
    IF CAN-DO("SE,SVE",cSprak) THEN DO:
        ASSIGN cOverskr[1] = "Nyckeltal"
               cOverskr[2] = "TB i kr"
               cOverskr[3] = "TB i %"
               cOverskr[4] = "Köp/kund"
               cOverskr[5] = "Antal kunder"
               cOverskr[6] = "Varuoms."
               cOverskr[7] = "Tjänsteoms.".
    END.
    ELSE DO:
        ASSIGN cOverskr[1] = "Nøkkeltall"
               cOverskr[2] = "DB kr"
               cOverskr[3] = "DB %"
               cOverskr[4] = "Kjøp/kunde"
               cOverskr[5] = "Andel kampanje %"
               cOverskr[6] = "Vareslag"
               cOverskr[7] = "Tjenester".
    END.
    ASSIGN dColPos[1] = pdf_LeftMargin ("Spdf")
           dColPos[2] = dColPos[1] + 73
           dColPos[3] = dColPos[2] + 73
           dColPos[4] = dColPos[3] + 73
           dColPos[5] = dColPos[4] + 73
           dColPos[6] = dColPos[5] + 73
           dColPos[7] = dColPos[6] + 73
           dColPos[8] = dColPos[7] + 73.
    ASSIGN dULStart[1] = pdf_LeftMargin ("Spdf")
           dULStart[2] = dColPos[2] + 4
           dULStart[3] = dColPos[3] + 4
           dULStart[4] = dColPos[4] + 4
           dULStart[5] = dColPos[5] + 4
           dULStart[6] = dColPos[6] + 4
           dULStart[7] = dColPos[7] + 4
           dULStart[8] = dColPos[8] + 4.


    dY = dY - 20.
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
        RUN pdf_text_xy_dec ("Spdf",cOverskr[4],dColPos[4] - bredd("Köp/kund"),dY).
    ELSE
        RUN pdf_text_xy_dec ("Spdf",cOverskr[4],dColPos[4] - bredd("Kjøp/kunde"),dY).
    RUN pdf_text_xy_dec ("Spdf",cOverskr[5],dColPos[5] - bredd(cOverskr[5]),dY).
    RUN pdf_text_xy_dec ("Spdf",cOverskr[6],dColPos[6] - bredd(cOverskr[6]),dY).
    RUN pdf_text_xy_dec ("Spdf",cOverskr[7],dColPos[7] - bredd("Tjänsteoms."),dY).
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

    dY = dY - iLineSpace. /* 1 Försäljning */
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
DEFINE VARIABLE cLabel  AS CHARACTER EXTENT 10  NO-UNDO.
DEFINE VARIABLE cTxt    AS CHARACTER   NO-UNDO.
    IF CAN-DO("SE,SVE",cSprak) THEN DO:
        ASSIGN cOverskr[1] = "Omsättning"
               cOverskr[2] = "Antal"
               cOverskr[3] = "Belopp".
        ASSIGN cLabel[1] = "Brutto omsättning"      
               cLabel[2] = "Returer"                
               cLabel[3] = "Generell rabatt"        
               cLabel[4] = "Kundrabatt"             
               cLabel[5] = "Personalrabatt"         
               cLabel[6] = "Medlemsrabatt"          
               cLabel[7] = "Presentkort ut"       
               cLabel[8] = "Netto omsättning"       
               cLabel[9] = "Avrundning"             
               cLabel[10] = "Registrerad omsättning".
    END.
    ELSE DO:
        ASSIGN cOverskr[1] = "Kasse totalt"
               cOverskr[2] = "Antall"
               cOverskr[3] = "Beløp".
        ASSIGN cLabel[1] = "Brutto omsetning"
               cLabel[2] = "Neg. vare"
               cLabel[3] = "Gen. rabatt"
               cLabel[4] = "Kunderabatt"
               cLabel[5] = "Personalrabatt"
               cLabel[6] = "Medlemsrabatt"
               cLabel[7] = "Gavekort ut"
               cLabel[8] = "Netto omsetning"
               cLabel[9] = "Avrunding"
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
DEFINE VARIABLE cOverskr AS CHARACTER EXTENT 3  NO-UNDO.
DEFINE VARIABLE cLabel   AS CHARACTER EXTENT 4 NO-UNDO.

    IF CAN-DO("SE,SVE",cSprak) THEN DO:
        ASSIGN cOverskr[1] = "Övrigt"
               cOverskr[2] = "Antal"
               cOverskr[3] = "Belopp".
        ASSIGN cLabel[1] = "Medlemsförsäljning"      
               cLabel[2] = "Inbetalt kunde"                
               cLabel[3] = "Växel"        
               cLabel[4] = "Returer".
    END.
    ELSE DO:
        ASSIGN cOverskr[1] = "Øvrigt"
               cOverskr[2] = "Antall"
               cOverskr[3] = "Beløp".
        ASSIGN cLabel[1]  = "Medlemssalg"      
               cLabel[2]  = "Innbetalt kunde"                
               cLabel[3]  = "Veksel"        
               cLabel[4]  = "Returer".
    END.
    dY = dY - 20.
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
DEFINE VARIABLE pcOverskr AS CHARACTER  NO-UNDO.
DEFINE VARIABLE pcLabel   AS CHARACTER  NO-UNDO.
/* DEFINE VARIABLE cLabel    AS CHARACTER EXTENT  NO-UNDO. */
DEFINE VARIABLE cLabel    AS CHARACTER EXTENT 12  NO-UNDO.
DEFINE VARIABLE iKontoPara AS INTEGER EXTENT 12  NO-UNDO.
DEFINE VARIABLE pcKonto   AS CHARACTER  NO-UNDO.
DEFINE VAR      piLoop    AS INT        NO-UNDO.
DEFINE VAR      pcBank    AS CHAR       NO-UNDO.
DEFINE VAR      piInt     AS INT        NO-UNDO.
DEFINE VARIABLE ii AS INTEGER     NO-UNDO.
DEFINE VAR      pcTekst   AS CHAR       NO-UNDO.
ASSIGN pcKonto = "K 0000  0%" + CHR(1) +
                 "K 3000 24%" + chr(1) +
                 "K 3001 12%" + chr(1) +
                 "K 0000  0%" + chr(1) +
                 "K 0000  0%" + chr(1) +
                 "K 0000  0%" + chr(1) +
                 "K 0000  0%" + chr(1) +
                 "K 0000  0%" + chr(1) +
                 "K 0000  0%" + chr(1) +
                 "K 0000  0%".
ASSIGN iKontoPara[1]  = 70
       iKontoPara[2]  = 90
       iKontoPara[3]  =  1
       iKontoPara[4]  = 13
       iKontoPara[5]  =  2
       iKontoPara[6]  =  3
       iKontoPara[7]  =  5
       iKontoPara[8]  =  4
       iKontoPara[9]  =  5
       iKontoPara[10] = 10
       iKontoPara[11] =  6
       iKontoPara[12] =  8.

IF CAN-DO("SE,SVE",cSprak) THEN DO:
/*     ASSIGN cLabel[1] = */
/*            cLabel[2] = */
    ASSIGN
      pcOverskr = "Text" + CHR(1) + 
                  "Konto" + CHR(1) + 
                  "Belopp" + CHR(1) +
                  "Mva"   + CHR(1) +
                  "Belopp u/mva"
      pcLabel   = "Försäljning" + CHR(1) + 
                  "Betalat med:" + CHR(1) + 
                  "Bankkort" + CHR(1) + 
                  "Reservlösning"
      pcBank    = "D 0000"
        cLabel[1] = "Kontant"
        cLabel[2] = "Kassadifferans"
        cLabel[3] = "Bankkort"
        cLabel[4] = "Off-line betalning"
        cLabel[5] = "Tillgodokvitton egna"
        cLabel[6] = "Tillgodokvitton andras"
        cLabel[7] = "Presentkort Center"
        cLabel[8] = "Presentkort egna"
        cLabel[9] = "Presentkort andras"
        cLabel[10] = "Deposition"
        cLabel[11] = "Kupong 1"
        cLabel[12] = "Kupong 2"
      .
END.
ELSE DO:
    ASSIGN
      pcOverskr = "Tekst" + CHR(1) + 
                  "Konto" + CHR(1) + 
                  "Beløp" + CHR(1) +
                  "Mva"   + CHR(1) +
                  "Beløp u/mva"
      pcLabel   = "Varesalg" + CHR(1) + 
                  "Betalt med:" + CHR(1) + 
                  "Bankkort" + CHR(1) + 
                  "Reserveløsning"
      pcBank    = "D 2380"
        cLabel[1] = "Kontant"
        cLabel[2] = "Kassedifferens"
        cLabel[3] = "Bankkort"
        cLabel[4] = "Reserveløsning"
        cLabel[5] = "Tilgodeseddler egne"
        cLabel[6] = "Tilgodeseddler andres"
        cLabel[7] = "Gavekort Senter"
        cLabel[8] = "Gavekort egna"
        cLabel[9] = "Gavekort andre"
        cLabel[10] = "Deponering"
        cLabel[11] = "Kupong 1"
        cLabel[12] = "Kupong 2"
      .
END.                                                                               
    dY = dY - 24.
    RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold",8).
    RUN pdf_text_xy_dec ("Spdf",ENTRY(2,pcLabel,CHR(1)),dColPosBF[1],dY).
    dY = dY - 4.
    RUN pdf_line IN h_PDFinc  ("Spdf", pdf_LeftMargin ("Spdf"), dY, dColPosBF[2] - 15 ,dY, 0.5).
/* ghg fr */
    dY = dY - 14.
    RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica",8).
    RUN pdf_text_xy_dec ("Spdf",cLabel[1],dColPosBF[1],dY).
    pcBank = getKonto(2,iKontoPara[1],tmpKas_Rap.KontantBeholdning + tmpKas_Rap.SjekkBeholdning).
    RUN pdf_text_xy_dec ("Spdf",pcBank,dColPosBF[2],dY).
    cBelopp = TRIM(STRING(tmpKas_Rap.KontantBeholdning + tmpKas_Rap.SjekkBeholdning,"->>>,>>>,>>9.99")).
    RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPosBF[3] - bredd(cBelopp),dY).
    RUN pdf_text_xy_dec ("Spdf","  -",dColPosBF[3],dY).
/* ghg hit */
/*    dY = dY - 14.
    RUN pdf_text_xy_dec ("Spdf",cLabel[2],dColPosBF[1],dY).
    pcBank = getKonto(2,iKontoPara[2],DECI(0)).
    RUN pdf_text_xy_dec ("Spdf",pcBank,dColPosBF[2],dY).
    cBelopp = TRIM(STRING(0,"->>>,>>>,>>9.99")).
    RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPosBF[3] - bredd(cBelopp),dY).
    RUN pdf_text_xy_dec ("Spdf","  -",dColPosBF[3],dY).*/
    dY = dY - 14.
    RUN pdf_text_xy_dec ("Spdf",cLabel[3],dColPosBF[1],dY).
    pcBank = getKonto(2,iKontoPara[3],tmpKas_Rap.Bank + tmpKas_Rap.Kort + tmpKas_Rap.Cashback).
    RUN pdf_text_xy_dec ("Spdf",pcBank,dColPosBF[2],dY).
    cBelopp = TRIM(STRING(tmpKas_Rap.Bank + tmpKas_Rap.Cashback,"->>>,>>>,>>9.99")).
    RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPosBF[3] - bredd(cBelopp),dY).
    RUN pdf_text_xy_dec ("Spdf","  -",dColPosBF[3],dY).
    dY = dY - 14.
    RUN pdf_text_xy_dec ("Spdf",cLabel[4],dColPosBF[1],dY).
    pcBank = getKonto(2,iKontoPara[4],Reservelosning).
    RUN pdf_text_xy_dec ("Spdf",pcBank,dColPosBF[2],dY).
    cBelopp = TRIM(STRING(STRING(Reservelosning,"->>>,>>>,>>9.99"))).
    RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPosBF[3] - bredd(cBelopp),dY).
    RUN pdf_text_xy_dec ("Spdf","  -",dColPosBF[3],dY).
    dY = dY - iLineSpace. /* 5 tilg in */
    RUN pdf_text_xy_dec ("Spdf",cLabel[5],dColPosBF[1],dY).
    pcBank = getKonto(2,iKontoPara[5],tmpKas_Rap.TilgodeInn).
    RUN pdf_text_xy_dec ("Spdf",pcBank,dColPosBF[2],dY).
    cBelopp = TRIM(STRING(tmpKas_Rap.TilgodeInn,"->>>,>>>,>>9.99")).
    RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPosBF[3] - bredd(cBelopp),dY).
    RUN pdf_text_xy_dec ("Spdf","  -",dColPosBF[3],dY).
    dY = dY - iLineSpace. /* 6 tilg in andre */
    RUN pdf_text_xy_dec ("Spdf",cLabel[6],dColPosBF[1],dY).
    pcBank = getKonto(2,iKontoPara[6],tmpkas_rap.TilgodeAndre).
    RUN pdf_text_xy_dec ("Spdf",pcBank,dColPosBF[2],dY).
    cBelopp = TRIM(STRING(tmpkas_rap.TilgodeAndre,"->>>,>>>,>>9.99")).
    RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPosBF[3] - bredd(cBelopp),dY).
    RUN pdf_text_xy_dec ("Spdf","  -",dColPosBF[3],dY).
 
    dY = dY - iLineSpace. /* 8 Gavekort in */
    RUN pdf_text_xy_dec ("Spdf",cLabel[8],dColPosBF[1],dY).
    pcBank = getKonto(2,iKontoPara[8],tmpKas_Rap.GavekortInn).
    RUN pdf_text_xy_dec ("Spdf",pcBank,dColPosBF[2],dY).
    cBelopp = TRIM(STRING(tmpKas_Rap.GavekortInn,"->>>,>>>,>>9.99")).
    RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPosBF[3] - bredd(cBelopp),dY).
    RUN pdf_text_xy_dec ("Spdf","  -",dColPosBF[3],dY).

    dY = dY - iLineSpace. /* 9 Gavekort andre in */
    RUN pdf_text_xy_dec ("Spdf",cLabel[9],dColPosBF[1],dY).
    pcBank = getKonto(2,iKontoPara[9],tmpKas_Rap.GavekortAndreInn).
    RUN pdf_text_xy_dec ("Spdf",pcBank,dColPosBF[2],dY).
    cBelopp = TRIM(STRING(tmpKas_Rap.GavekortAndreInn,"->>>,>>>,>>9.99")).
    RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPosBF[3] - bredd(cBelopp),dY).
    RUN pdf_text_xy_dec ("Spdf","  -",dColPosBF[3],dY).
    
    dY = dY - iLineSpace. /* 10 dep in */
    RUN pdf_text_xy_dec ("Spdf",cLabel[10],dColPosBF[1],dY).
    pcBank = getKonto(2,iKontoPara[10],tmpKas_Rap.LayAway_Inn).
    RUN pdf_text_xy_dec ("Spdf",pcBank,dColPosBF[2],dY).
    cBelopp = TRIM(STRING(tmpKas_Rap.LayAway_Inn,"->>>,>>>,>>9.99")).
    RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPosBF[3] - bredd(cBelopp),dY).
    RUN pdf_text_xy_dec ("Spdf","  -",dColPosBF[3],dY).
    
    dY = dY - iLineSpace. /* 11 Kupong 1 */
    RUN pdf_text_xy_dec ("Spdf",cLabel[11],dColPosBF[1],dY).
    pcBank = getKonto(2,iKontoPara[11],tmpkas_rap.kupong1).
    RUN pdf_text_xy_dec ("Spdf",pcBank,dColPosBF[2],dY).
    cBelopp = TRIM(STRING(tmpkas_rap.kupong1,"->>>,>>>,>>9.99")).
    RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPosBF[3] - bredd(cBelopp),dY).
    RUN pdf_text_xy_dec ("Spdf","  -",dColPosBF[3],dY).
    
    dY = dY - iLineSpace. /* 12 Kupong 12 */
    RUN pdf_text_xy_dec ("Spdf",cLabel[12],dColPosBF[1],dY).
    pcBank = getKonto(2,iKontoPara[12],tmpkas_rap.kupong2).
    RUN pdf_text_xy_dec ("Spdf",pcBank,dColPosBF[2],dY).
    cBelopp = TRIM(STRING(tmpkas_rap.kupong2,"->>>,>>>,>>9.99")).
    RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPosBF[3] - bredd(cBelopp),dY).
    RUN pdf_text_xy_dec ("Spdf","  -",dColPosBF[3],dY).

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
DEFINE VARIABLE pcOverskr AS CHARACTER  NO-UNDO.
DEFINE VARIABLE pcLabel   AS CHARACTER  NO-UNDO.
DEFINE VARIABLE pcKonto   AS CHARACTER  NO-UNDO.
DEFINE VARIABLE plSum     AS DEC        NO-UNDO.
DEFINE VARIABLE lKun1Dag  AS LOGICAL    NO-UNDO.
DEFINE VARIABLE dLeftCol   AS DECIMAL  INIT 385  NO-UNDO.
DEFINE VARIABLE dLeftCol2  AS DECIMAL  INIT 420  NO-UNDO.
DEFINE VARIABLE dLineStart AS DECIMAL  INIT 484  NO-UNDO.

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

&IF DEFINED(EXCLUDE-Saml_2_BilagSpesORG) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Saml_2_BilagSpesORG Procedure 
PROCEDURE Saml_2_BilagSpesORG :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

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
DEFINE VARIABLE cLabel    AS CHARACTER EXTENT 5  NO-UNDO.
DEFINE VARIABLE plSum     AS DEC        NO-UNDO.
DEFINE VARIABLE pcTekst   AS CHAR       NO-UNDO.
DEFINE VARIABLE piLoop    AS INT        NO-UNDO.
    IF CAN-DO("SE,SVE",cSprak) THEN DO:
        ASSIGN cLabel[1] = "Dagens kontantström"
               cLabel[2] = "Kassa vid dagens början"
               cLabel[3] = "Kassa vid dagens slut, uppräknat"
               cLabel[4] = "Ändring kassa"
               cLabel[5] = "Differens".
    END.
    ELSE DO:
        ASSIGN cLabel[1] = "Dagens kontantstrøm"
               cLabel[2] = "Kasse ved dagens begynnelse"
               cLabel[3] = "Kasse ved dagens slutt, opptalt"
               cLabel[4] = "Endring kasse"
               cLabel[5] = "Differanse".
    END.
    dY = dY - 24.
    RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold",8).
    RUN pdf_text_xy_dec ("Spdf",cLabel[1],dColPosBF[1],dY).
    cBelopp = TRIM(STRING(lDagensKontStrom,"->>>,>>>,>>9.99")).
/*     cBelopp = TRIM(STRING(tmpKas_Rap.KontantBeholdning + tmpKas_Rap.SjekkBeholdning,"->>>,>>>,>>9.99")). */
    
    RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPosBF[3] - bredd(cBelopp),dY).
    RUN pdf_text_xy_dec ("Spdf","  A",dColPosBF[3],dY).
    RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica",8).
    
    /* Kasse ved dagens start */
    dY = dY - 24.
    RUN pdf_text_xy_dec ("Spdf",cLabel[2],dColPosBF[1],dY).
    cBelopp = TRIM(STRING(tmpKas_Rap.OpptaltInnVeksel,"->>>,>>>,>>9.99")).
    RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPosBF[3] - bredd(cBelopp),dY).
    RUN pdf_text_xy_dec ("Spdf","  B",dColPosBF[3],dY).
    
    /* Kasse ved dagens slutt */
    dY = dY - 14.
    RUN pdf_text_xy_dec ("Spdf",cLabel[3],dColPosBF[1],dY).
    cBelopp = TRIM(STRING(lKasseSlutt,"->>>,>>>,>>9.99")).
    RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPosBF[3] - bredd(cBelopp),dY).
    RUN pdf_text_xy_dec ("Spdf","  C",dColPosBF[3],dY).
    
    /* Endring kasse */
    RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold",8).
    dY = dY - 24.
    RUN pdf_text_xy_dec ("Spdf",cLabel[4],dColPosBF[1],dY).
    RUN pdf_text_xy_dec ("Spdf",pcTekst,dColPosBF[2],dY).
    cBelopp = TRIM(STRING(lKasseEndring,"->>>,>>>,>>9.99")).
    RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPosBF[3] - bredd(cBelopp),dY).
    RUN pdf_text_xy_dec ("Spdf","  D=C-B",dColPosBF[3],dY).
    
    /* Differanse */
    dY = dY - 14.
    RUN pdf_text_xy_dec ("Spdf",cLabel[5],dColPosBF[1],dY).
    RUN pdf_text_xy_dec ("Spdf",pcTekst,dColPosBF[2],dY).
    cBelopp = TRIM(STRING(lKasseDiff,"->>>,>>>,>>9.99")).
    RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPosBF[3] - bredd(cBelopp),dY).
    RUN pdf_text_xy_dec ("Spdf","  =D-A",dColPosBF[3],dY).

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
DEFINE VARIABLE pcOverskr AS CHARACTER  NO-UNDO.
DEFINE VARIABLE pcLabel AS CHARACTER  NO-UNDO.
  IF CAN-DO("SE,SVE",cSprak) THEN DO:
      ASSIGN pcOverskr = "Diverse" + CHR(1) + "Antal" + CHR(1) + "Belopp".
      ASSIGN pcLabel   = "Utbetalt" + CHR(1) + "Inbetalt" + CHR(1) + "Tiligode in" + CHR(1) + "Tilgodo ut" + CHR(1) + 
                         "Dep-in" + CHR(1) + "Dep-ut" + CHR(1) + "Presentkort in" + CHR(1) + 
                         "Presentkort ut" + CHR(1) + "Dropp" + CHR(1) + "Ingående interna överföringar" + CHR(1) + 
                         "Utgående interna överføringar" + CHR(1) + "Varumottag" + CHR(1) + "Lagerjusteringar" + CHR(1) +
                         "Kassation" + CHR(1) + "Intern förbrukning" + CHR(1) + "Reklamation" + CHR(1) + 
                         "Medlemsförsäljning" + CHR(1) + "Inbetalt kund" + CHR(1) + "Växel" + CHR(1) + "Returer" + CHR(1) + "Gåvekortsrabatt" + CHR(1) + "NonSale(Negativ)".
  END.
  ELSE DO:
      ASSIGN pcOverskr = "Diverse" + CHR(1) + "Antall" + CHR(1) + "Beløp".
      ASSIGN pcLabel   = "Utbetalt" + CHR(1) + "Innbetalt" + CHR(1) + "Tilgode inn" + CHR(1) + "Tilgode ut" + CHR(1) + 
                         "Dep-in" + CHR(1) + "Dep-ut" + CHR(1) + "Gavekort inn" + CHR(1) + 
                         "Gavekort ut" + CHR(1) + "Dropp" + CHR(1) + "Inngående interne overføringer" + CHR(1) + 
                         "Utgående interne overføringer" + CHR(1) + "Varemottak" + CHR(1) + "Lagerjustering" + CHR(1) +
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
DEFINE VARIABLE cOverskr AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cLabel    AS CHARACTER EXTENT 5  NO-UNDO.
DEFINE VARIABLE iKontoPara AS INTEGER  EXTENT 5  NO-UNDO.
DEFINE VARIABLE plSum     AS DEC        NO-UNDO.
DEFINE VARIABLE pcTekst   AS CHAR       NO-UNDO.
DEFINE VARIABLE piLoop    AS INT        NO-UNDO.
ASSIGN iKontoPara[1]  = 59
       iKontoPara[2]  = 60
       iKontoPara[3]  = 59
       iKontoPara[4]  = 60
       iKontoPara[5]  = 61.
    
   IF CAN-DO("SE,SVE",cSprak) THEN DO:
        ASSIGN cOverskr  = "In / Utbetalningar"
               cLabel[1] = "Inbetalt"
               cLabel[2] = "Utbetalt"
               cLabel[3] = "Nonsale inbet"
               cLabel[4] = "Nonsale utbet"
               cLabel[5] = "Dropp".
    END.
    ELSE DO:
        ASSIGN cOverskr  = "In / Utbetalningar"
               cLabel[1] = "Inbetalt"
               cLabel[2] = "Utbetalt"
               cLabel[3] = "Nonsale inbet"
               cLabel[4] = "Nonsale utbet"
               cLabel[5] = "Dropp".
    END.

    /* Overskrift */
    dY = dY - 24.
    RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold",8).
    RUN pdf_text_xy_dec ("Spdf",cOverskr,dColPosBF[1],dY).
    dY = dY - 4.
    RUN pdf_line IN h_PDFinc  ("Spdf", pdf_LeftMargin ("Spdf"), dY, dColPosBF[2] - 15 ,dY, 0.5).
    RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica",8).

    /* Innbetalt */
    dY = dY - 14.
    RUN pdf_text_xy_dec ("Spdf",cLabel[1],dColPosBF[1],dY).
    pcTekst = getKonto(2,iKontoPara[1],tmpKas_Rap.kont_in).
    RUN pdf_text_xy_dec ("Spdf",pcTekst,dColPosBF[2],dY).
    cBelopp = TRIM(STRING(tmpKas_Rap.kont_in,"->>>,>>>,>>9.99")).
    RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPosBF[3] - bredd(cBelopp),dY).
    RUN pdf_text_xy_dec ("Spdf","  +",dColPosBF[3],dY).

    /* Utbetalt */
    dY = dY - 14.
    RUN pdf_text_xy_dec ("Spdf",cLabel[2],dColPosBF[1],dY).
    pcTekst = getKonto(2,iKontoPara[2],tmpKas_Rap.kont_ut).
    RUN pdf_text_xy_dec ("Spdf",pcTekst,dColPosBF[2],dY).
    cBelopp = TRIM(STRING(tmpKas_Rap.kont_ut,"->>>,>>>,>>9.99")).
    RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPosBF[3] - bredd(cBelopp),dY).
    RUN pdf_text_xy_dec ("Spdf","  -",dColPosBF[3],dY).

    /* NonSale positiv */
    dY = dY - 14.
    RUN pdf_text_xy_dec ("Spdf",cLabel[3],dColPosBF[1],dY).
    pcTekst = getKonto(2,iKontoPara[3],tmpKas_Rap.Non_SalePos).
    RUN pdf_text_xy_dec ("Spdf",pcTekst,dColPosBF[2],dY).
    cBelopp = TRIM(STRING(tmpKas_Rap.Non_SalePos,"->>>,>>>,>>9.99")).
    RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPosBF[3] - bredd(cBelopp),dY).
    RUN pdf_text_xy_dec ("Spdf","  +",dColPosBF[3],dY).

    /* NonSale negativ */
    dY = dY - 14.
    RUN pdf_text_xy_dec ("Spdf",cLabel[4],dColPosBF[1],dY).
    pcTekst = getKonto(2,iKontoPara[4],tmpKas_Rap.Non_SaleNeg).
    RUN pdf_text_xy_dec ("Spdf",pcTekst,dColPosBF[2],dY).
    cBelopp = TRIM(STRING(tmpKas_Rap.Non_SaleNeg,"->>>,>>>,>>9.99")).
    RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPosBF[3] - bredd(cBelopp),dY).
    RUN pdf_text_xy_dec ("Spdf","  -",dColPosBF[3],dY).

    /* Dropp */
    dY = dY - 14.
    RUN pdf_text_xy_dec ("Spdf",cLabel[5],dColPosBF[1],dY).
    pcTekst = getKonto(2,iKontoPara[5],tmpKas_Rap.Dropp).
    RUN pdf_text_xy_dec ("Spdf",pcTekst,dColPosBF[2],dY).
    cBelopp = TRIM(STRING(tmpKas_Rap.Dropp,"->>>,>>>,>>9.99")).
    RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPosBF[3] - bredd(cBelopp),dY).
    RUN pdf_text_xy_dec ("Spdf","  -",dColPosBF[3],dY).


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
DEFINE VARIABLE pcOverskr AS CHARACTER  NO-UNDO.
DEFINE VARIABLE pcLabel   AS CHARACTER  NO-UNDO.
/* DEFINE VARIABLE cLabel    AS CHARACTER EXTENT  NO-UNDO. */
DEFINE VARIABLE pcKonto   AS CHARACTER  NO-UNDO.
DEFINE VAR      piLoop    AS INT        NO-UNDO.
DEFINE VAR      pcBank    AS CHAR       NO-UNDO.
DEFINE VAR      piInt     AS INT        NO-UNDO.
DEFINE VARIABLE ii AS INTEGER     NO-UNDO.
DEFINE VAR      pcTekst   AS CHAR       NO-UNDO.
DEFINE VARIABLE iKontoPara AS INTEGER  EXTENT 2   NO-UNDO.
ASSIGN iKontoPara[1]  = 40
       iKontoPara[2]  = 41.
IF CAN-DO("SE,SVE",cSprak) THEN DO:
/*     ASSIGN cLabel[1] = */
/*            cLabel[2] = */
    ASSIGN
      pcOverskr = "Text" + CHR(1) + 
                  "Konto" + CHR(1) + 
                  "Belopp" + CHR(1) +
                  "Mva"   + CHR(1) +
                  "Belopp u/mva"
      pcLabel   = "Kreditförsäljning" + CHR(1) +
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
ELSE DO:
    ASSIGN
      pcOverskr = "Tekst" + CHR(1) + 
                  "Konto" + CHR(1) + 
                  "Beløp" + CHR(1) +
                  "Mva"   + CHR(1) +
                  "Beløp u/mva"
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

    dY = dY - 24.
    RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold",8).
    RUN pdf_text_xy_dec ("Spdf",ENTRY(1,pcLabel,CHR(1)),dColPosBF[1],dY).
    dY = dY - 4.
    RUN pdf_line IN h_PDFinc  ("Spdf", pdf_LeftMargin ("Spdf"), dY, dColPosBF[2] - 15 ,dY, 0.5).
    RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica",8).
    /* Fakturerat */
      dY = dY - 14.
      RUN pdf_text_xy_dec ("Spdf",ENTRY(2,pcLabel,CHR(1)),dColPosBF[1],dY).
      pcTekst = getKonto(2,iKontoPara[1],tmpKas_Rap.Kredit).
      RUN pdf_text_xy_dec ("Spdf",pcTekst,dColPosBF[2],dY).
      cBelopp = TRIM(STRING(tmpKas_Rap.Kredit,"->>>,>>>,>>9.99")).
      RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPosBF[3] - bredd(cBelopp),dY).
      RUN pdf_text_xy_dec ("Spdf","  -",dColPosBF[3],dY).

    /* Inbetalt konto */
      dY = dY - 14.
      RUN pdf_text_xy_dec ("Spdf",ENTRY(3,pcLabel,CHR(1)),dColPosBF[1],dY).
      pcTekst = getKonto(2,iKontoPara[2],tmpKas_Rap.Kont_Inn).
      RUN pdf_text_xy_dec ("Spdf",pcTekst,dColPosBF[2],dY).
      cBelopp = TRIM(STRING(tmpKas_Rap.Kont_Inn,"->>>,>>>,>>9.99")).
      RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPosBF[3] - bredd(cBelopp),dY).
      RUN pdf_text_xy_dec ("Spdf","  +",dColPosBF[3],dY).


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
DEFINE VARIABLE pcOverskr AS CHARACTER  NO-UNDO.
DEFINE VARIABLE pcLabel   AS CHARACTER  NO-UNDO.
DEFINE VARIABLE pcKonto   AS CHARACTER  NO-UNDO.
DEFINE VAR      piLoop    AS INT        NO-UNDO.
DEFINE VAR      pcBank    AS CHAR       NO-UNDO.
DEFINE VAR      piInt     AS INT        NO-UNDO.
DEFINE VARIABLE cKonto AS CHARACTER   NO-UNDO.
DEFINE VARIABLE ii AS INTEGER     NO-UNDO.
DEFINE VAR      pcTekst   AS CHAR       NO-UNDO.
IF CAN-DO("SE,SVE",cSprak) THEN DO:
    ASSIGN
      pcOverskr = "Text" + CHR(1) + 
                  "Konto" + CHR(1) + 
                  "Belopp" + CHR(1) +
                  "Mva"   + CHR(1) +
                  "Belopp u/mva"
      pcLabel   = "Varuförsäljning momsgrupp"
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
ELSE DO:
    ASSIGN
      pcOverskr = "Tekst" + CHR(1) + 
                  "Konto" + CHR(1) + 
                  "Beløp" + CHR(1) +
                  "Mva"   + CHR(1) +
                  "Beløp u/mva"
      pcLabel   = "Varesalg momsgruppe"
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
    /* Legger ut mva regnskapet. */
    DO piLoop = 1 TO 10:
        IF tmpKas_rap.MvaGrunnlag[piLoop] <> 0 THEN
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
/* /* !!! */   pcKonto = "K 3000". */
            IF AVAILABLE Moms THEN
                pcTekst = pcTekst + " " + STRING(tmpKas_Rap.MvaGrp[piLoop]) + " " + STRING(Moms.MomsProc) + "%".
            ELSE 
                pcTekst = pcTekst + " " + string(ROUND((tmpKas_rap.MvaBelop[piLoop] / tmpKas_rap.MvaGrunnlag[piLoop]) * 100,0)) + "%".

            RUN pdf_text_xy_dec ("Spdf",ENTRY(1,pcTekst,CHR(1)),dColPosBF[1],dY).
            RUN pdf_text_xy_dec ("Spdf",cKonto,dColPosBF[2],dY).
            cBelopp = TRIM(STRING(tmpKas_rap.MvaGrunnlag[piLoop] + tmpKas_rap.MvaBelop[piLoop],"->>>,>>>,>>9.99")).
            RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPosBF[3] - bredd(cBelopp),dY).
            cBelopp = TRIM(STRING(tmpKas_rap.MvaBelop[piLoop],"->>>,>>>,>>9.99")).
            RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPosBF[4] - bredd(cBelopp),dY).
            cBelopp = TRIM(STRING(tmpKas_rap.MvaGrunnlag[piLoop],"->>>,>>>,>>9.99")).
            RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPosBF[5] - bredd(cBelopp),dY).
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
DEFINE VARIABLE pcOverskr AS CHARACTER  NO-UNDO.
DEFINE VARIABLE pcLabel   AS CHARACTER  NO-UNDO.
/* DEFINE VARIABLE cLabel    AS CHARACTER EXTENT  NO-UNDO. */
DEFINE VARIABLE pcKonto   AS CHARACTER  NO-UNDO.
DEFINE VAR      piLoop    AS INT        NO-UNDO.
DEFINE VAR      pcBank    AS CHAR       NO-UNDO.
DEFINE VAR      piInt     AS INT        NO-UNDO.
DEFINE VARIABLE ii AS INTEGER     NO-UNDO.
DEFINE VAR      pcTekst   AS CHAR       NO-UNDO.
DEFINE VARIABLE iKontoPara AS INTEGER EXTENT 3    NO-UNDO.
ASSIGN iKontoPara[1]  = 20
       iKontoPara[2]  = 21
       iKontoPara[3]  = 11.

IF CAN-DO("SE,SVE",cSprak) THEN DO:
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
ELSE DO:
    ASSIGN
      pcOverskr = "Tekst" + CHR(1) + 
                  "Konto" + CHR(1) + 
                  "Beløp" + CHR(1) +
                  "Mva"   + CHR(1) +
                  "Beløp u/mva"
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

    dY = dY - 24.
    RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold",8).
    RUN pdf_text_xy_dec ("Spdf",ENTRY(1,pcLabel,CHR(1)),dColPosBF[1],dY).
    dY = dY - 4.
    RUN pdf_line IN h_PDFinc  ("Spdf", pdf_LeftMargin ("Spdf"), dY, dColPosBF[2] - 15 ,dY, 0.5).
    RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica",8).
    /* Tilgodesedler ut */
      pcTekst = getKonto(2,iKontoPara[1],tmpKas_Rap.TilgodeUt).
      dY = dY - 14.
      RUN pdf_text_xy_dec ("Spdf",ENTRY(2,pcLabel,CHR(1)),dColPosBF[1],dY).
      RUN pdf_text_xy_dec ("Spdf",pcTekst,dColPosBF[2],dY).
      cBelopp = TRIM(STRING(tmpKas_Rap.TilgodeUt,"->>>,>>>,>>9.99")).
      RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPosBF[3] - bredd(cBelopp),dY).
      RUN pdf_text_xy_dec ("Spdf","  +",dColPosBF[3],dY).

    /* Gavekort ut */
      pcTekst = getKonto(2,iKontoPara[2],tmpKas_Rap.GavekortUt).
      dY = dY - 14.
      RUN pdf_text_xy_dec ("Spdf",ENTRY(3,pcLabel,CHR(1)),dColPosBF[1],dY).
      RUN pdf_text_xy_dec ("Spdf",pcTekst,dColPosBF[2],dY).
      cBelopp = TRIM(STRING(tmpKas_Rap.GavekortUt - tmpKas_Rap.GavekortRabatt,"->>>,>>>,>>9.99")).
      RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPosBF[3] - bredd(cBelopp),dY).
      RUN pdf_text_xy_dec ("Spdf","  +",dColPosBF[3],dY).

    /* Depositum ut */
      pcTekst = getKonto(2,iKontoPara[3],tmpKas_Rap.Layaway_Ut).
      dY = dY - 14.
      RUN pdf_text_xy_dec ("Spdf",ENTRY(4,pcLabel,CHR(1)),dColPosBF[1],dY).
      RUN pdf_text_xy_dec ("Spdf",pcTekst,dColPosBF[2],dY).
      cBelopp = TRIM(STRING(tmpKas_Rap.Layaway_Ut,"->>>,>>>,>>9.99")).
      RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPosBF[3] - bredd(cBelopp),dY).
      RUN pdf_text_xy_dec ("Spdf","  +",dColPosBF[3],dY).

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
DEFINE VARIABLE cOverskr AS CHARACTER EXTENT 5 NO-UNDO.
DEFINE VARIABLE cLabel   AS CHARACTER EXTENT 2  NO-UNDO.
DEFINE VARIABLE cTxt     AS CHARACTER   NO-UNDO.
DEFINE VARIABLE dSum_pos AS DECIMAL     NO-UNDO.
DEFINE VARIABLE dSum_neg AS DECIMAL     NO-UNDO.
DEFINE VARIABLE dColPos_S3  AS DECIMAL  EXTENT 4   NO-UNDO.
DEFINE VARIABLE dULstart_S3 AS DECIMAL  EXTENT 4   NO-UNDO.
DEFINE VARIABLE cTjensterTxt AS CHARACTER   NO-UNDO.
DEFINE VARIABLE dSum AS DECIMAL     NO-UNDO.
DEFINE VARIABLE cSumTxt AS CHARACTER EXTENT 2  NO-UNDO.
    IF CAN-DO("SE,SVE",cSprak) THEN DO:
        ASSIGN cOverskr[1] = "Nonsale"
               cOverskr[2] = "Plu"
               cOverskr[3] = "Text"
               cOverskr[4] = "Antal"
               cOverskr[5] = "Belopp"
               cLabel[1]   = "Totalt positiv nonsale"
               cLabel[2]   = "Totalt negativ nonsale"
              cTjensterTxt = "Tjänster avd " + STRING(iTjHG) + " per varugrupp (Vgr)"
              cSumTxt[1]   = "Totalt tjänster utan moms"
              cSumTxt[2]   = "Totalt tjänster med moms".
    END.
    ELSE DO:
        ASSIGN cOverskr[1] = "Nonsale"
               cOverskr[2] = "Plu"
               cOverskr[3] = "Tekst"
               cOverskr[4] = "Antall"
               cOverskr[5] = "Beløp"
               cLabel[1]   = "Totalt positiv nonsale"
               cLabel[2]   = "Totalt negativ nonsale"
              cTjensterTxt = "Tjenester avd " + STRING(iTjHG) + " per varegruppe (Vgr)"
              cSumTxt[1]   = "Totalt tjenester uten moms"
              cSumTxt[2]   = "Totalt tjenester med moms".
    END.
    ASSIGN dColPos_S3[1] = pdf_LeftMargin ("Spdf") + 20
           dColPos_S3[2] = dColPos_S3[1] + 10
           dColPos_S3[3] = 340
           dColPos_S3[4] = 420.
    ASSIGN dULstart_S3[1] = pdf_LeftMargin ("Spdf")
           dULstart_S3[2] = dColPos_S3[2]
           dULstart_S3[3] = 290
           dULstart_S3[4] = 350.

    /* nonsale */
    IF CAN-FIND(FIRST tt_NON_sale_spes) THEN DO:
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
        IF dSUm_pos <> 0 THEN DO:
            dY = dY - iLineSpace.
            RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold",8).
            RUN pdf_text_xy_dec ("Spdf",cLabel[1],dULStart_S3[1],dY).
            cBelopp = TRIM(STRING(dSum_pos,"->>>,>>>,>>9.99")).
            RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPos_S3[4] - bredd(cBelopp),dY).
        END.
        IF dSum_neg <> 0 THEN DO:
            dY = dY - iLineSpace.
            RUN pdf_text_xy_dec ("Spdf",cLabel[2],dULStart_S3[1],dY).
            cBelopp = TRIM(STRING(dSum_neg,"->>>,>>>,>>9.99")).
            RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPos_S3[4] - bredd(cBelopp),dY).
        END.
        dY = dY - 24.
    END.
    IF NOT CAN-FIND(FIRST TT_vg) THEN
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
    IF CAN-FIND(FIRST TT_Vg WHERE tt_vg.mva = FALSE AND TT_Vg.belopp <> 0) THEN DO:
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
        IF dSum <> 0 THEN DO:
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
    IF CAN-FIND(FIRST TT_Vg WHERE tt_vg.mva = TRUE AND tt_Vg.belopp <> 0) THEN DO:
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
        IF dSum <> 0 THEN DO:
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

&IF DEFINED(EXCLUDE-Saml_4_Kont_hg) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Saml_4_Kont_hg Procedure 
PROCEDURE Saml_4_Kont_hg :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT-OUTPUT PARAMETER dY AS DECIMAL     NO-UNDO.
DEFINE VARIABLE cOverskr AS CHARACTER EXTENT 4 NO-UNDO.
DEFINE VARIABLE cLabel   AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cTxt     AS CHARACTER   NO-UNDO.
DEFINE VARIABLE dSum     AS DECIMAL     NO-UNDO.
DEFINE VARIABLE dColPos_S4  AS DECIMAL  EXTENT 3   NO-UNDO.
DEFINE VARIABLE dULstart_S4 AS DECIMAL  EXTENT 3   NO-UNDO.
DEFINE VARIABLE dDato      AS DATE        NO-UNDO.
DEFINE VARIABLE dFgAr31Dec AS DATE        NO-UNDO.
DEFINE VARIABLE iAarperlinnr AS INTEGER     NO-UNDO.
DEFINE VARIABLE iPerLoop   AS INTEGER     NO-UNDO.
    IF CAN-DO("SE,SVE",cSprak) THEN DO:
        ASSIGN cOverskr[1] = "Huvudgruppsredovisning"
               cOverskr[2] = "Hgr"
               cOverskr[3] = "Text"
               cOverskr[4] = "Belopp u moms"
               cLabel      = "Totalt".
    END.
    ELSE DO:
        ASSIGN cOverskr[1] = "Hovedgrupperegnskap"
               cOverskr[2] = "Hgr"
               cOverskr[3] = "Tekst"
               cOverskr[4] = "Beløp u mva"
               clabel      = "Totalt".
    END.
    ASSIGN dColPos_S4[1] = pdf_LeftMargin ("Spdf") + 15
           dColPos_S4[2] = dColPos_S4[1] + 10
           dColPos_S4[3] = 320.
    ASSIGN dULstart_S4[1] = pdf_LeftMargin ("Spdf")
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
            IF NOT AVAIL TT_KontHg THEN DO:
                FIND HuvGr WHERE HuvGr.Hg = INT(StLinje.Dataobjekt) NO-LOCK NO-ERROR.
                CREATE TT_KontHg.
                ASSIGN TT_kontHg.Hg = INT(StLinje.Dataobjekt)
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
DEFINE VARIABLE cOverskr AS CHARACTER EXTENT 5 NO-UNDO.
DEFINE VARIABLE cLabel   AS CHARACTER EXTENT 2  NO-UNDO.
DEFINE VARIABLE cTxt     AS CHARACTER   NO-UNDO.
DEFINE VARIABLE dSum_pos AS DECIMAL     NO-UNDO.
DEFINE VARIABLE dSum_neg AS DECIMAL     NO-UNDO.
DEFINE VARIABLE dColPos_S3  AS DECIMAL  EXTENT 4   NO-UNDO.
DEFINE VARIABLE dULstart_S3 AS DECIMAL  EXTENT 4   NO-UNDO.
DEFINE VARIABLE cTjensterTxt AS CHARACTER   NO-UNDO.
DEFINE VARIABLE dSum AS DECIMAL     NO-UNDO.
DEFINE VARIABLE cSumTxt AS CHARACTER EXTENT 2  NO-UNDO.
    IF CAN-DO("SE,SVE",cSprak) THEN DO:
        ASSIGN cOverskr[1] = "Nonsale"
               cOverskr[2] = "Plu"
               cOverskr[3] = "Text"
               cOverskr[4] = "Antal"
               cOverskr[5] = "Belopp"
               cLabel[1]   = "Totalt positiv nonsale"
               cLabel[2]   = "Totalt negativ nonsale"
              cTjensterTxt = "Tjänster avd 13 per artikel"
              cSumTxt[1]   = "Totalt tjänster utan moms"
              cSumTxt[2]   = "Totalt tjänster med moms".
    END.
    ELSE DO:
        ASSIGN cOverskr[1] = "Nonsale"
               cOverskr[2] = "Plu"
               cOverskr[3] = "Tekst"
               cOverskr[4] = "Antall"
               cOverskr[5] = "Beløp"
               cLabel[1]   = "Totalt positiv nonsale"
               cLabel[2]   = "Totalt negativ nonsale"
              cTjensterTxt = "Tjenester avd 13 per artikkel"
              cSumTxt[1]   = "Totalt tjenester uten moms"
              cSumTxt[2]   = "Totalt tjenester med moms".
    END.
    ASSIGN dColPos_S3[1] = pdf_LeftMargin ("Spdf") + 50
           dColPos_S3[2] = dColPos_S3[1] + 10
           dColPos_S3[3] = 340
           dColPos_S3[4] = 420.
    ASSIGN dULstart_S3[1] = pdf_LeftMargin ("Spdf")
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
    IF CAN-FIND(FIRST tt_ArtHG WHERE tt_ArtHG.mva = FALSE AND tt_ArtHG.belopp <> 0) THEN DO:
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
        IF dSum <> 0 THEN DO:
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
    IF CAN-FIND(FIRST tt_ArtHG WHERE tt_ArtHG.mva = TRUE AND tt_ArtHG.belopp <> 0) THEN DO:
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
        IF dSum <> 0 THEN DO:
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
    DEFINE VARIABLE cMailhub  AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cDoAUTH   AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cAuthType AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cUser     AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cPassword AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cEmailCC  AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cEmailFrom AS CHARACTER   NO-UNDO.

    DEFINE VARIABLE lMailOK    AS LOGICAL     NO-UNDO.
    DEFINE VARIABLE cMessage   AS CHARACTER   NO-UNDO.

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
  ASSIGN dColPosBF[1] = pdf_LeftMargin ("Spdf")
         dColPosBF[2] = 210
         dColPosBF[3] = 370
         dColPosBF[4] = 470
         dColPosBF[5] = pdf_PageWidth ("Spdf") - pdf_LeftMargin ("Spdf").
  ASSIGN dColPosFR[1] = pdf_LeftMargin ("Spdf")
         dColPosFR[2] = 160
         dColPosFR[3] = 250
         dColPosFR[4] = 315
         dColPosFR[5] = 455
         dColPosFR[6] = pdf_PageWidth ("Spdf") - pdf_LeftMargin ("Spdf").
  ASSIGN dULstartFR[1] = 0
         dULstartFR[2] = 110
         dULstartFR[3] = 170
         dULstartFR[4] = 0
         dULstartFR[5] = 410
         dULstartFR[6] = 470.

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
      ASSIGN tmpKort_Spes.AntKort  = tmpKort_Spes.AntKort + Kort_Spes.AntKort
             tmpKort_Spes.Belop    = tmpKort_Spes.Belop   + Kort_Spes.Belop.

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
                tmpKas_Rap.Kontant           =  tmpKas_Rap.Kontant       + Kas_Rap.Kontant
                tmpKas_Rap.Sjekk             =  tmpKas_Rap.Sjekk         + Kas_Rap.Sjekk
                tmpKas_Rap.Kort              =  tmpKas_Rap.Kort          + Kas_Rap.Kort
                tmpKas_Rap.Kredit            =  tmpKas_Rap.Kredit        + Kas_Rap.Kredit
                tmpKas_Rap.Kupong1           =  tmpKas_Rap.Kupong1       + Kas_Rap.Kupong1
                tmpKas_Rap.Kupong2           =  tmpKas_Rap.Kupong2       + Kas_Rap.Kupong2
                tmpKas_Rap.Tilgode           =  tmpKas_Rap.Tilgode       + Kas_Rap.Tilgode
                tmpKas_Rap.Layaway_inn       =  tmpKas_Rap.Layaway_inn   + Kas_Rap.Layaway_inn
                tmpKas_Rap.Layaway_ut        =  tmpKas_Rap.Layaway_ut    + Kas_Rap.Layaway_ut
                tmpKas_Rap.Kont_inn          =  tmpKas_Rap.Kont_inn      + Kas_Rap.Kont_inn
                tmpKas_Rap.Kont_ut           =  tmpKas_Rap.Kont_ut       + Kas_Rap.Kont_ut
                tmpKas_Rap.Gavekort          =  tmpKas_Rap.Gavekort      + Kas_Rap.Gavekort

                tmpKas_Rap.Rekvisisasjon     =  tmpKas_Rap.Rekvisisasjon + Kas_Rap.Rekvisisasjon
                tmpKas_Rap.Pant              =  tmpKas_Rap.Pant          + Kas_Rap.Pant
                tmpKas_Rap.Bank              =  tmpKas_Rap.Bank          + Kas_Rap.Bank
                tmpKas_Rap.Dropp             =  tmpKas_Rap.Dropp         + Kas_Rap.Dropp
                tmpKas_Rap.AntVaremottak     =  tmpKas_Rap.AntVaremottak     + Kas_Rap.AntVaremottak    
                tmpKas_Rap.AntLagerjustering =  tmpKas_Rap.AntLagerjustering + Kas_Rap.AntLagerjustering
                tmpKas_Rap.AntBrekkasje      =  tmpKas_Rap.AntBrekkasje      + Kas_Rap.AntBrekkasje     
                tmpKas_Rap.AntInterntForbruk =  tmpKas_Rap.AntInterntForbruk + Kas_Rap.AntInterntForbruk
                tmpKas_Rap.Varemottak        =  tmpKas_Rap.Varemottak        + Kas_Rap.Varemottak    
                tmpKas_Rap.Lagerjustering    =  tmpKas_Rap.Lagerjustering    + Kas_Rap.Lagerjustering
                tmpKas_Rap.Brekkasje         =  tmpKas_Rap.Brekkasje         + Kas_Rap.Brekkasje     
                tmpKas_Rap.InterntForbruk    =  tmpKas_Rap.InterntForbruk    + Kas_Rap.InterntForbruk.

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
                tmpKas_Rap.Non_SaleNeg       = tmpKas_Rap.Non_SaleNeg       + kas_rap.Non_SaleNeg.

              ASSIGN
                tmpKas_Rap.GaveKortRabatt    = tmpKas_Rap.GaveKortRabatt    + Kas_Rap.GaveKortRabatt
                tmpKas_Rap.AntGaveKortRabUt  = tmpKas_Rap.AntGaveKortRabUt  + Kas_Rap.AntGaveKortRabUt
                tmpKas_Rap.AntGavekort       = tmpKas_Rap.AntGavekort       + Kas_Rap.AntGavekort
                tmpKas_Rap.AntRekvisisjon    = tmpKas_Rap.AntRekvisisjon    + Kas_Rap.AntRekvisisjon
                tmpKas_Rap.AntVeksel         = tmpKas_Rap.AntVeksel         + Kas_Rap.AntVeksel
                tmpKas_Rap.AntAvrunding      = tmpKas_Rap.AntAvrunding      + Kas_Rap.AntAvrunding
                tmpKas_Rap.AntDropp          = tmpKas_Rap.AntDropp          + Kas_Rap.AntDropp
                tmpKas_Rap.AntOverfort       = tmpKas_Rap.AntOverfort       + Kas_Rap.AntOverfort
                tmpKas_Rap.AntKont_Inn       = tmpKas_Rap.AntKont_Inn       + Kas_Rap.AntKont_Inn
                tmpKas_Rap.AntKont_Ut        = tmpKas_Rap.AntKont_Ut        + Kas_Rap.AntKont_Ut
                tmpKas_Rap.AntLayAway_Inn    = tmpKas_Rap.AntLayAway_Inn    + Kas_Rap.AntLayAway_Inn
                tmpKas_Rap.AntLayAway_Ut     = tmpKas_Rap.AntLayAway_Ut     + Kas_Rap.AntLayAway_Ut
                tmpKas_Rap.AntReturer        = tmpKas_Rap.AntReturer        + Kas_Rap.AntReturer
                tmpKas_Rap.TilgodeInn        = tmpKas_Rap.TilgodeInn        + Kas_Rap.TilgodeInn
                tmpKas_Rap.TilgodeAndre      = tmpKas_Rap.TilgodeAndre      + Kas_Rap.TilgodeAndre
                tmpKas_Rap.TilgodeUt         = tmpKas_Rap.TilgodeUt         + Kas_Rap.TilgodeUt
                tmpKas_Rap.AntTilgodeInn     = tmpKas_Rap.AntTilgodeInn     + Kas_Rap.AntTilgodeInn
                tmpKas_Rap.AntTilgodeUt      = tmpKas_Rap.AntTilgodeUt      + Kas_Rap.AntTilgodeUt
                tmpKas_Rap.GavekortUt        = tmpKas_Rap.GavekortUt        + Kas_Rap.GavekortUt
                tmpKas_Rap.GavekortInn       = tmpKas_Rap.GavekortInn       + Kas_Rap.GavekortInn
                tmpKas_Rap.GavekortAndreInn  = tmpKas_Rap.GavekortAndreInn  + Kas_Rap.GavekortAndreInn
                tmpKas_Rap.AntGavekortUt     = tmpKas_Rap.AntGavekortUt     + Kas_Rap.AntGavekortUt
                tmpKas_Rap.AntGavekortInn    = tmpKas_Rap.AntGavekortInn    + Kas_Rap.AntGavekortInn
                tmpKas_Rap.AntGavekortAndreInn = tmpKas_Rap.AntGavekortAndreInn + Kas_Rap.AntGavekortAndreInn
                tmpKas_Rap.Medlemsrabatt     = tmpKas_Rap.Medlemsrabatt     + Kas_Rap.Medlemsrabatt.

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
                tmpKas_Rap.AntOverfortUt     = tmpKas_Rap.AntOverfortUt     + Kas_Rap.AntOverfortUt
                tmpKas_Rap.MvaGrp[ 1]        = Kas_Rap.MvaGrp[ 1]        
                tmpKas_Rap.MvaGrp[ 2]        = Kas_Rap.MvaGrp[ 2]        
                tmpKas_Rap.MvaGrp[ 3]        = Kas_Rap.MvaGrp[ 3]        
                tmpKas_Rap.MvaGrp[ 4]        = Kas_Rap.MvaGrp[ 4]        
                tmpKas_Rap.MvaGrp[ 5]        = Kas_Rap.MvaGrp[ 5]        
                tmpKas_Rap.MvaGrp[ 6]        = Kas_Rap.MvaGrp[ 6]        
                tmpKas_Rap.MvaGrp[ 7]        = Kas_Rap.MvaGrp[ 7]        
                tmpKas_Rap.MvaGrp[ 8]        = Kas_Rap.MvaGrp[ 8]        
                tmpKas_Rap.MvaGrp[ 9]        = Kas_Rap.MvaGrp[ 9].

              ASSIGN
                tmpKas_Rap.MvaGrp[10]        = Kas_Rap.MvaGrp[10]
                tmpKas_Rap.MvaGrunnlag[ 1]   = tmpKas_Rap.MvaGrunnlag[ 1]   + Kas_Rap.MvaGrunnlag[ 1]
                tmpKas_Rap.MvaGrunnlag[ 2]   = tmpKas_Rap.MvaGrunnlag[ 2]   + Kas_Rap.MvaGrunnlag[ 2]
                tmpKas_Rap.MvaGrunnlag[ 3]   = tmpKas_Rap.MvaGrunnlag[ 3]   + Kas_Rap.MvaGrunnlag[ 3]
                tmpKas_Rap.MvaGrunnlag[ 4]   = tmpKas_Rap.MvaGrunnlag[ 4]   + Kas_Rap.MvaGrunnlag[ 4]
                tmpKas_Rap.MvaGrunnlag[ 5]   = tmpKas_Rap.MvaGrunnlag[ 5]   + Kas_Rap.MvaGrunnlag[ 5]
                tmpKas_Rap.MvaGrunnlag[ 6]   = tmpKas_Rap.MvaGrunnlag[ 6]   + Kas_Rap.MvaGrunnlag[ 6]
                tmpKas_Rap.MvaGrunnlag[ 7]   = tmpKas_Rap.MvaGrunnlag[ 7]   + Kas_Rap.MvaGrunnlag[ 7]
                tmpKas_Rap.MvaGrunnlag[ 8]   = tmpKas_Rap.MvaGrunnlag[ 8]   + Kas_Rap.MvaGrunnlag[ 8]
                tmpKas_Rap.MvaGrunnlag[ 9]   = tmpKas_Rap.MvaGrunnlag[ 9]   + Kas_Rap.MvaGrunnlag[ 9]
                tmpKas_Rap.MvaGrunnlag[10]   = tmpKas_Rap.MvaGrunnlag[10]   + Kas_Rap.MvaGrunnlag[10]
                tmpKas_Rap.MvaBelop[ 1]      = tmpKas_Rap.MvaBelop[ 1]      + Kas_Rap.MvaBelop[ 1]
                tmpKas_Rap.MvaBelop[ 2]      = tmpKas_Rap.MvaBelop[ 2]      + Kas_Rap.MvaBelop[ 2]
                tmpKas_Rap.MvaBelop[ 3]      = tmpKas_Rap.MvaBelop[ 3]      + Kas_Rap.MvaBelop[ 3]
                tmpKas_Rap.MvaBelop[ 4]      = tmpKas_Rap.MvaBelop[ 4]      + Kas_Rap.MvaBelop[ 4]
                tmpKas_Rap.MvaBelop[ 5]      = tmpKas_Rap.MvaBelop[ 5]      + Kas_Rap.MvaBelop[ 5]
                tmpKas_Rap.MvaBelop[ 6]      = tmpKas_Rap.MvaBelop[ 6]      + Kas_Rap.MvaBelop[ 6]
                tmpKas_Rap.MvaBelop[ 7]      = tmpKas_Rap.MvaBelop[ 7]      + Kas_Rap.MvaBelop[ 7]
                tmpKas_Rap.MvaBelop[ 8]      = tmpKas_Rap.MvaBelop[ 8]      + Kas_Rap.MvaBelop[ 8]
                tmpKas_Rap.MvaBelop[ 9]      = tmpKas_Rap.MvaBelop[ 9]      + Kas_Rap.MvaBelop[ 9].

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
                tmpKas_Rap.VerdiUtbetBonger  = tmpKas_Rap.VerdiUtbetBonger  + lVerdiUtbetBonger.

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
DEFINE VARIABLE dSum AS DECIMAL     NO-UNDO.

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

DEF VAR pcPerId AS CHAR NO-UNDO.
DEF VAR pcStTypeId AS CHAR NO-UNDO.
DEF VAR piFraAarPerLinNr AS INT NO-UNDO.
DEF VAR piTilAarPerLinNr AS INT NO-UNDO.

ASSIGN
  pcPerId    = 'DAG'
  pcStTypeId = 'AVDELING'
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
  DEFINE VARIABLE pcTekst AS CHARACTER   NO-UNDO.
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
DEFINE VARIABLE dSum AS DECIMAL     NO-UNDO.

DEF VAR pcPerId AS CHAR NO-UNDO.
DEF VAR pcStTypeId AS CHAR NO-UNDO.
DEF VAR piFraAarPerLinNr AS INT NO-UNDO.
DEF VAR piTilAarPerLinNr AS INT NO-UNDO.

ASSIGN
  pcPerId    = 'DAG'
  pcStTypeId = 'AVDELING'
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
DEFINE VARIABLE dSum AS DECIMAL     NO-UNDO.

DEF VAR pcPerId AS CHAR NO-UNDO.
DEF VAR pcStTypeId AS CHAR NO-UNDO.
DEF VAR piFraAarPerLinNr AS INT NO-UNDO.
DEF VAR piTilAarPerLinNr AS INT NO-UNDO.

ASSIGN
  pcPerId    = 'DAG'
  pcStTypeId = 'AVDELING'
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

