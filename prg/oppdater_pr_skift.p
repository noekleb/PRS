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

DEFINE VARIABLE iKoeff        AS INTEGER    NO-UNDO.
DEFINE VARIABLE dSalgssumTmp  AS DECIMAL    NO-UNDO.
DEFINE VARIABLE dMvaKr            AS DECIMAL    NO-UNDO.
DEFINE VARIABLE dKreditkr     AS DECIMAL    NO-UNDO.
DEFINE VARIABLE iKreditant    AS INTEGER    NO-UNDO.
DEFINE VARIABLE cBetTTId      AS CHARACTER  NO-UNDO.

DEFINE VARIABLE deDatotid      AS DECIMAL    NO-UNDO.
DEFINE VARIABLE cDatoTid       AS CHARACTER  NO-UNDO.

DEFINE TEMP-TABLE TT_prBMData   NO-UNDO LIKE prBMData.  /* Betalingsmiddler                 */
DEFINE TEMP-TABLE TT_prKDData   NO-UNDO LIKE prKDData.  /* Solgte volumer drivstoff på kort */
DEFINE TEMP-TABLE TT_prKD2Data   NO-UNDO LIKE prKD2Data.  /* Solgte volumer drivstoff på kort hg, belopp och mva*/
DEFINE TEMP-TABLE TT_prKSData   NO-UNDO LIKE prKSData.  /* SUM kreditsalg (stasjonskreditt) */
DEFINE TEMP-TABLE TT_prPGData   NO-UNDO LIKE prPGData.  /* varegruppesalg                   */
DEFINE TEMP-TABLE TT_prn9HGData   NO-UNDO LIKE prn9HGData.  /* varegruppesalg                   */

DEFINE BUFFER bBokforingsdag FOR Bokforingsdag.

DEFINE TEMP-TABLE tt_kotroll NO-UNDO
    FIELD bokforingsid LIKE bokforingsdag.bokforingsid
    FIELD butikknr AS INTE
    FIELD antalTid AS INTE
    FIELD volumTid AS INTE
    FIELD todbTid  AS INTE
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

&IF DEFINED(EXCLUDE-getHGKonto) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getHGKonto Procedure 
FUNCTION getHGKonto RETURNS INTEGER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getKonto) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getKonto Procedure 
FUNCTION getKonto RETURNS INTEGER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getOrgNr) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getOrgNr Procedure 
FUNCTION getOrgNr RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getSubtypeName) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getSubtypeName Procedure 
FUNCTION getSubtypeName RETURNS CHARACTER
  ( INPUT iSubType AS INTEGER )  FORWARD.

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
         HEIGHT             = 27.67
         WIDTH              = 98.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */
ASSIGN cBetTTId = "50,51,52,53,54,55,56,57,58,59,60,61,62,64,65,66,67,69,70,71,73,78,79,89".
/* 63=subtotalrab inte med
   64=overföring inte med
   68=reservert  inte med
   72=deponering inte med
   73=betaling av deponering inte med
*/
RUN BFDagLoop.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-BFDagLoop) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE BFDagLoop Procedure 
PROCEDURE BFDagLoop :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    FOR EACH bBokforingsdag WHERE bBokforingsdag.pfFlagg = 1 NO-LOCK:
        cDatoTid = STRING(YEAR(TODAY),"9999") + 
                   STRING(MONTH(TODAY),"99")  +
                   STRING(DAY(TODAY),"99")    +
                   REPLACE(STRING(TIME,"HH:MM:SS"),":","").
        deDatoTid = DECI(cDatoTid).
        RUN Bokforingsdag.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Bokforingsdag) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Bokforingsdag Procedure 
PROCEDURE Bokforingsdag :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE lBongdataONskift AS LOGICAL    NO-UNDO.
    BOKFDAG:
    DO TRANSACTION:
/*     FOR EACH bokforingsdag WHERE Bokforingsdag.pfFlagg = 1 AND Bokforingsdag.ButikkNr = Butiker.Butik: */
        FIND bokforingsdag WHERE Bokforingsdag.bokforingsid = bBokforingsdag.bokforingsid NO-ERROR.
        IF NOT AVAIL bokforingsdag THEN
            LEAVE BOKFDAG.
        ASSIGN lBongdataONskift = FALSE.
        FOR EACH skift WHERE skift.bokforingsid = bokforingsdag.bokforingsid AND skift.pfFlagg = 1 NO-LOCK:
            IF CAN-FIND(FIRST Bonghode WHERE BongHode.skiftid = skift.skiftid) THEN DO:
                ASSIGN lBongdataONskift = TRUE.
                LEAVE.
            END.
        END.
        IF lBongdataONskift = FALSE THEN
            LEAVE BOKFDAG.
        FOR EACH skift WHERE skift.bokforingsid = bokforingsdag.bokforingsid AND skift.pfFlagg = 1:
            EMPTY TEMP-TABLE TT_prBMData.
            EMPTY TEMP-TABLE TT_prKSData.
            EMPTY TEMP-TABLE TT_prPGData.
            EMPTY TEMP-TABLE TT_prn9HGData.
            FOR EACH bonghode WHERE Bonghode.skiftid = skift.skiftid AND
                                    bonghode.pfFlagg = 1 USE-INDEX skiftid:
                EMPTY TEMP-TABLE TT_prKDData.
                EMPTY TEMP-TABLE TT_prKD2Data.
                IF bonghode.makulert <> 2 THEN /* makulerad */ DO:
                    RUN NullStillVars.
                    ASSIGN dSalgssumTmp = 0.
                    
                    FOR EACH Bonglinje WHERE bonglinje.b_id = bonghode.b_id NO-LOCK BY bonglinje.TTId:
                      IF CAN-DO("1,3,10",STRING(BongLinje.TTId)) AND BongLinje.Antall <> 0 AND Bonglinje.Makulert = FALSE THEN SALG: DO:
                        FIND Artbas WHERE ArtBas.ArtikkelNr = DECI(Bonglinje.Artikkelnr) NO-LOCK NO-ERROR.
                        FIND HuvGr WHERE HuvGr.Hg = Bonglinje.HovedGr NO-LOCK NO-ERROR.
                        ASSIGN iKoeff       = IF BongLinje.Antall = 0 THEN 0 ELSE IF BongLinje.Antall > 0 THEN 1 ELSE -1
/*                                iMvaGr       = IF BongLinje.MvaGr = 0 OR BongLinje.MvaGr > 9 THEN 9 ELSE BongLinje.MvaGr */
                               dSalgssumTmp = BongLinje.LinjeSum - BongLinje.Linjerab - BongLinje.SubtotalRab
/*                                dKostPris = dKostPris + iKoeff * BongLinje.VVarekost */
                            .
                        prPGData:
                        DO:
                            RUN Update_TT_prPGData.
                        END.
                        prn9HGData:
                        DO:
                            RUN Update_TT_prn9HGData.
                        END.
                        /* test för att få med kontanter: IF AVAIL HuvGr AND HuvGr.Avdelingnr = 1 THEN prKDData: */
/*                         IF BongHode.flBankkort = TRUE AND AVAIL HuvGr AND HuvGr.Avdelingnr = 1 THEN prKDData: */
                        IF (BongHode.flBankkort = TRUE AND AVAIL HuvGr AND HuvGr.Avdelingnr = 1) OR 
                           (BongHode.flBankkort = TRUE AND Bonglinje.varegr = 8398) THEN prKDData:
                        DO:
                            RUN Update_TT_prKDData ("VOLUM").
                        END.
                        IF BongHode.flBankkort = TRUE  THEN prKD2Data:
                        DO:
                            RUN Update_TT_prKD2Data ("VOLUM").
                        END.
                      END. /* SALG: */
                      IF BongLinje.Makulert = FALSE THEN DO:
                          IF CAN-DO(cBetTTId,STRING(bongLinje.TTId)) THEN DO:
                              /* test för att få med kontanter: IF bongLinje.TTId = 52 OR (bongLinje.TTId = 58 AND bonglinje.antall = 13) OR bonglinje.TTid = 50 THEN */
                              IF bongLinje.TTId = 52 OR (bongLinje.TTId = 58 AND bonglinje.antall = 13) THEN DO:
/*                               IF (bongLinje.TTId = 52 OR (bongLinje.TTId = 58 AND bonglinje.antall = 13)) AND bonglinje.forkonvertering = "JA" THEN DO: */
                                  RUN Update_TT_prKDData ("KORT").
                                  RUN Update_TT_prKD2Data ("KORT").
                              END.
                              RUN Update_TT_prBMData.
                          END.
                          IF bongLinje.TTId = 65 /* kredit */ AND Bonglinje.linjesum <> 0 AND BongHode.KundeNr <> 0 THEN DO:
                              RUN Update_TT_prKSData.
                          END.
                      END.
                    END. /* Bonglinjer slut */
                END.
                RUN TT_KD_toDB.
                RUN TT_KD2_toDB.
                ASSIGN BongHode.pfFlagg = 3.
            END.
            RUN TT_BM_toDB.
            RUN TT_KS_toDB.
            RUN TT_PG_toDB.
            RUN TT_n9HG_toDB.
            ASSIGN Skift.pfFlagg = 3.
        END.
        ASSIGN bokforingsdag.pfFlagg = 3.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ButikkLoop) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ButikkLoop Procedure 
PROCEDURE ButikkLoop :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    FOR EACH Butiker NO-LOCK:
        FOR EACH bBokforingsdag WHERE bBokforingsdag.pfFlagg = 1 AND bBokforingsdag.ButikkNr = Butiker.Butik NO-LOCK:
            RUN Bokforingsdag.
        END.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-NullStillVars) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE NullStillVars Procedure 
PROCEDURE NullStillVars :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    ASSIGN dSalgssumtmp = 0
           dKreditkr    = 0
           iKreditant   = 0

        .
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ORGUpdate_TT_prBMData) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ORGUpdate_TT_prBMData Procedure 
PROCEDURE ORGUpdate_TT_prBMData :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       Betalingsmiddler
------------------------------------------------------------------------------*/
/* 
      ASSIGN dKreditkr   = dKreditkr   + IF bongLinje.TTId = 65 THEN BongLinje.linjesum ELSE 0
             iKreditant  = iKreditant  + IF bongLinje.TTId = 65 THEN 1 ELSE 0
             iKontantant = iKontantant + IF bongLinje.TTId = 50 THEN IF bonglinje.linjesum < 0 THEN -1 ELSE 1 ELSE 0
             dKontantkr  = dKontantkr  + IF bongLinje.TTId = 50 THEN BongLinje.linjesum ELSE 0
             dKontantkr  = dKontantkr  + IF bongLinje.TTId = 70 THEN BongLinje.linjesum ELSE 0
             dVeksel     = dVeksel     + IF bongLinje.TTId = 70 THEN BongLinje.linjesum ELSE 0
             dBankkr     = dBankkr     + IF bongLinje.TTId = 52 OR bongLinje.TTId = 58 THEN Bonglinje.LinjeSum ELSE 0
             iBankAnt    = iBankAnt    + IF bongLinje.TTId = 52 OR bongLinje.TTId = 58 THEN 1 ELSE 0
             dBankkr     = dBankkr     + IF bongLinje.TTId = 67 THEN Bonglinje.LinjeSum ELSE 0
             dKupongkr   = dKupongkr   + IF bongLinje.TTId = 56 OR bongLinje.TTId = 72 THEN Bonglinje.LinjeSum ELSE 0
             dCashBack   = dCashBack   + IF bongLinje.TTId = 67 THEN Bonglinje.LinjeSum ELSE 0
             iCashBackAnt = iCashBackAnt + IF bongLinje.TTId = 67 THEN 1 ELSE 0
             dDriveOff    = dDriveOff    + IF bongLinje.TTId = 54 THEN Bonglinje.LinjeSum ELSE 0
             iDriveOffAnt = iDriveOffAnt + IF bongLinje.TTId = 54 THEN 1 ELSE 0
             dAvrundKr   = dAvrundKr   + IF bonglinje.TTId = 78 THEN Bonglinje.LinjeSum ELSE 0
             iAvrundAnt  = iAvrundAnt  + IF bongLinje.TTId = 78 THEN 1 ELSE 0
             dReservBank    = dReservBank    + IF bonglinje.TTId = 79 THEN Bonglinje.LinjeSum ELSE 0
             iReservBankAnt = iReservBankAnt + IF bonglinje.TTId = 79 THEN 1 ELSE 0                 
             dGavekortIn    = dGavekortIn    + IF bonglinje.TTId = 53 THEN Bonglinje.LinjeSum ELSE 0
             iGavekortInAnt = iGavekortInAnt + IF bonglinje.TTId = 53 THEN 1 ELSE 0
             dGavekortUt    = dGavekortUt    + IF bonglinje.TTId = 134 THEN Bonglinje.LinjeSum ELSE 0
             dGavekortUtAnt = dGavekortUtAnt + IF bonglinje.TTId = 134 THEN Bonglinje.Antall ELSE 0 
             dTilGodeInn    = dTilGodeInn + IF bonglinje.TTId = 66 THEN Bonglinje.LinjeSum ELSE 0
             dTilGodeInnAnt = dTilGodeInnAnt + IF bonglinje.TTId = 66 THEN 1 ELSE 0
             dTilGodeUt     = dTilGodeUt    + IF bonglinje.TTId = 69 THEN -1 * Bonglinje.LinjeSum ELSE 0
             dTilGodeUtAnt  = dTilGodeUtAnt + IF bonglinje.TTId = 69 THEN 1 ELSE 0
             dInOutKr       = dInOutKr      +  IF bonglinje.TTId = 59 THEN -1 * Bonglinje.LinjeSum ELSE 0
             iInOutAnt      = iInOutAnt     +  IF bonglinje.TTId = 59 THEN 1 ELSE 0
             dInOutKr       = dInOutKr      +  IF bonglinje.TTId = 61 THEN Bonglinje.LinjeSum ELSE 0
             iInOutAnt      = iInOutAnt     +  IF bonglinje.TTId = 61 THEN 1 ELSE 0
             dInOutKr       = dInOutKr      +  IF bonglinje.TTId = 62 THEN -1 * Bonglinje.LinjeSum ELSE 0
             iInOutAnt      = iInOutAnt     +  IF bonglinje.TTId = 62 THEN 1 ELSE 0
                 .
 
 */
    DEFINE VARIABLE iBetalingsType AS INTEGER    NO-UNDO.
    DEFINE VARIABLE iSubType       AS INTEGER    NO-UNDO.
    DEFINE VARIABLE iSubSubType    AS INTEGER    NO-UNDO.
    DEFINE VARIABLE iKonto         AS INTEGER    NO-UNDO.
    DEFINE VARIABLE iTest          AS INTEGER    NO-UNDO.
    DEFINE VARIABLE dLinjesum      AS DECIMAL    NO-UNDO.
    IF BongLinje.TTId = 65 AND NOT CAN-DO("01,0610,0612",BongLinje.Storrelse) THEN
        RETURN.
    ELSE IF BongLinje.TTId = 53 THEN DO:
        iTest = INT(BongLinje.Storrelse) NO-ERROR.
        IF iTest = ? OR iTest < 1 OR iTest > 20 THEN
            RETURN.
    END.
    ASSIGN dLinjesum = Bonglinje.Linjesum.
    ASSIGN iSubSubType = ?.
    CASE Bonglinje.TTId:
        WHEN 50 OR WHEN 60 THEN /* Kontant or Valuta */
            ASSIGN iBetalingsType = 1
                   iSubType       = ?
                   iKonto         = 1912. /*1930.*/
        WHEN 70 THEN /* Växel */
            ASSIGN iBetalingsType = 1
                   iSubType       = ?
                   iKonto         = 1912. /*1930.*/
        WHEN 52 THEN /* Kreditkort */
            ASSIGN iBetalingsType = 2
                   iSubType       = Bonglinje.antall
                   iKonto         = 2445. /*1520.*/
        WHEN 53 THEN /* Gavekort */
            ASSIGN iBetalingsType = 3
                   iSubType       = INT(BongLinje.Storrelse)
                   iKonto         = IF iSubType = 2 THEN 2445 ELSE IF iSubType = 3 THEN 2445 ELSE 1518. /* 1600 */
/*                    iKonto         = IF iSubType = 2 THEN 1531 ELSE IF iSubType = 3 THEN 1532 ELSE 1533. */
        WHEN 54 THEN /* check -> drive off */
            ASSIGN iBetalingsType = 4
                   iSubType       = ?
                   iKonto         = 6385. /*3727.*/
        WHEN 58 THEN DO: /* Bank */
            IF Bonglinje.antall = 13 THEN
                ASSIGN iBetalingsType = 2
                       iSubType       = Bonglinje.antall
                       iKonto         = 2445. /*1520.*/
            ELSE
                ASSIGN iBetalingsType = 58
                       iSubType       = ?
                       iKonto         = ?.
        END.
        WHEN 59 THEN /* Dropp */
            ASSIGN iBetalingsType = 5
                   iSubType       = 2
                   iKonto         = 1930. /*06-2007 ændrat från ?.*/
        WHEN 61 THEN /* Inbetalning */
            ASSIGN iBetalingsType = 5
                   iSubType       = 0
                   iSubSubType    = IF TRIM(Bonglinje.storrelse) <> "" THEN INT(Bonglinje.storrelse) ELSE iSubSubType
                   iKonto         = IF iSubSubType = 1 THEN 2421 ELSE IF iSubSubType = 2 THEN 1696 /* 06-2007 ændrat från 2990 */
/*                                     IF iSubSubType = 3 THEN 1691 ELSE 2990. /*1535.*/ */
                               ELSE IF iSubSubType = 3 THEN 1691  /*1535.*/
                               ELSE IF iSubSubType = 4 THEN 1518 ELSE 2990. /*1600.*/
        WHEN 62 THEN /* Utbetalning */
            ASSIGN iBetalingsType = 5
                   iSubType       = 1
                   iSubSubType    = IF TRIM(Bonglinje.storrelse) <> "" THEN INT(Bonglinje.storrelse) ELSE iSubSubType
                   iKonto         = IF iSubSubType = 1 THEN 2445 ELSE IF iSubSubType = 2 THEN 2445 
                               ELSE IF iSubSubType = 3 THEN 1693 ELSE 1699. /* 06-2007 ændrat från 2990.*/
        WHEN 65 THEN DO: /* Kredit */
            IF TRIM(Bonglinje.storrelse) = "0610" OR TRIM(Bonglinje.storrelse) = "0612" THEN DO:
                ASSIGN iBetalingsType = 6
                       iSubType       = 1
                       iSubSubtype    = INT(SUBSTR(TRIM(Bonglinje.storrelse),4))
                       iKonto         = 1518. /* 1600 */ 
/*                        dLinjesum      = dLinjesum * IF TRIM(Bonglinje.storrelse) = "0610" THEN -1 ELSE 1. */
            END.
            ELSE DO:
                ASSIGN iBetalingsType = 3
                       iSubType       = 1
                       iKonto         = 2445. /*1530.*/
            END.
        END.
        WHEN 67 THEN /* Cashback */
            ASSIGN iBetalingsType = 9
                   iSubType       = ?
                   iKonto         = ?.    /*ingen ändring*/
        WHEN 78 THEN /* Öresavrundning */
            ASSIGN iBetalingsType = 7
                   iSubType       = ?
                   iKonto         = 3790. /*ingen ändring*/
    END CASE.
    FIND TT_prBMData WHERE TT_prBMData.ButikkNr      = Bonglinje.ButikkNr AND
                           TT_prBMData.dato          = bokforingsdag.dato  AND   /* växel skall korrigera kontant */
                           TT_prBMData.Betalingstype = iBetalingsType AND 
                           TT_prBMData.SUBTYPE       = iSubType AND 
                           TT_prBMData.subsubtype    = iSubSubtype NO-ERROR.

    IF NOT AVAIL TT_prBMData THEN DO:
        CREATE TT_prBMData.
        ASSIGN TT_prBMData.ButikkNr      = Bonglinje.ButikkNr
               TT_prBMData.dato          = bokforingsdag.dato
               TT_prBMData.Betalingstype = iBetalingsType 
               TT_prBMData.SUBTYPE       = iSubType
               TT_prBMData.subsubtype    = iSubSubType
               TT_prBMData.Konto         = iKonto
               TT_prBMData.datotid       = deDatoTid
/*                TT_prBMData.BettypeBeskrivelse = cBettypeBeskrivelse */
/*                TT_prBMData.subtypenavn        = cSubtypenavn        */
              .
    END.
    ASSIGN TT_prBMData.Belop = TT_prBMData.Belop + dLinjesum
/*            TT_prBMData.Kassadiff = TT_prBMData.Kassadiff + */
          .
/*     IF bonglinje.TTId = 61 OR Bonglinje.TTId = 62 THEN DO: /* Inbetalin / utbetaling -> kontant korrigeras */ */
/*         FIND TT_prBMData WHERE TT_prBMData.ButikkNr      = Bonglinje.ButikkNr AND                             */
/*                                TT_prBMData.dato          = bokforingsdag.dato  AND                            */
/*                                TT_prBMData.Betalingstype = 1 AND                                              */
/*                                TT_prBMData.SUBTYPE       = ? AND                                              */
/*                                TT_prBMData.subsubtype    = ? NO-ERROR.                                        */
/*                                                                                                               */
/*         IF NOT AVAIL TT_prBMData THEN DO:                                                                     */
/*             CREATE TT_prBMData.                                                                               */
/*             ASSIGN TT_prBMData.ButikkNr      = Bonglinje.ButikkNr                                             */
/*                    TT_prBMData.dato          = bokforingsdag.dato                                             */
/*                    TT_prBMData.Betalingstype = 1                                                              */
/*                    TT_prBMData.SUBTYPE       = ?                                                              */
/*                    TT_prBMData.subsubtype    = ?                                                              */
/*                    TT_prBMData.Konto         = 1930. /*1912.*/                                                */
/*         END.                                                                                                  */
/*         ASSIGN TT_prBMData.Belop = TT_prBMData.Belop + (dLinjesum * IF Bonglinje.TTId = 61 THEN -1 ELSE 1).   */
/*     END.                                                                                                      */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-TT_BM_toDB) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE TT_BM_toDB Procedure 
PROCEDURE TT_BM_toDB :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    FOR EACH TT_prBMData:
        FIND prBMData WHERE prBMData.ButikkNr      = TT_prBMData.ButikkNr      AND
                            prBMData.dato          = TT_prBMData.dato          AND
                            prBMData.Betalingstype = TT_prBMData.Betalingstype AND
                            prBMData.SUBTYPE       = TT_prBMData.SUBTYPE       AND
                            prBMData.subsubtype    = TT_prBMData.subsubtype    NO-ERROR.
        IF AVAIL prBMData THEN DO:
            ASSIGN prBMData.Belop = prBMData.Belop + TT_prBMData.Belop.
            IF prBMData.datotid <> TT_prBMData.datotid THEN
                ASSIGN prBMData.datotid = TT_prBMData.datotid.
                /* TT_prBMData.Kassadiff = TT_prBMData.Kassadiff + */
        END.
        ELSE DO:
            CREATE prBMData.
            BUFFER-COPY TT_prBMData TO prBMData NO-ERROR.
            
    /*                     ASSIGN prBMData.ButikkNr      = TT_prBMData.ButikkNr      */
    /*                            prBMData.dato          = TT_prBMData.dato          */
    /*                            prBMData.Betalingstype = TT_prBMData.Betalingstype */
    /*                            prBMData.SUBTYPE       = TT_prBMData.SUBTYPE       */
    /*                            prBMData.subsubtype    = TT_prBMData.subsubtype    */
    /*                            prBMData.Belop         = TT_prBMData.Belop.        */
            IF ERROR-STATUS:ERROR = TRUE THEN
                    DELETE prBMData.
        END.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-TT_KD2_toDB) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE TT_KD2_toDB Procedure 
PROCEDURE TT_KD2_toDB :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    FOR EACH TT_prKD2Data WHERE TT_prKD2Data.Subtype <> 0:
        FIND prKD2Data WHERE prKD2Data.ButikkNr = TT_prKD2Data.ButikkNr AND
                            prKD2Data.dato     = TT_prKD2Data.dato     AND
                            prKD2Data.hg       = TT_prKD2Data.hg       AND
                            prKD2Data.SUBTYPE  = TT_prKD2Data.SUBTYPE  NO-ERROR.
        IF AVAIL prKD2Data THEN DO:
            ASSIGN prKD2Data.Volum = prKD2Data.Volum   + TT_prKD2Data.Volum
                   prKD2Data.Antall = prKD2Data.Antall + TT_prKD2Data.Antall
                   prKD2Data.Belopp = prKD2Data.Belopp + TT_prKD2Data.Belopp
                   prKD2Data.Mva    = prKD2Data.Mva    + TT_prKD2Data.Mva.
            IF prKD2Data.datotid <> TT_prKD2Data.datotid THEN
                ASSIGN prKD2Data.datotid = TT_prKD2Data.datotid.
        END.
        ELSE DO:
            CREATE prKD2Data.
            BUFFER-COPY TT_prKD2Data TO prKD2Data NO-ERROR.
            IF ERROR-STATUS:ERROR = TRUE THEN
                DELETE prKD2Data.
        END.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-TT_KD_toDB) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE TT_KD_toDB Procedure 
PROCEDURE TT_KD_toDB :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    FOR EACH TT_prKDData WHERE TT_prKDData.Subtype <> 0:
        FIND prKDData WHERE prKDData.ButikkNr = TT_prKDData.ButikkNr AND
                            prKDData.dato     = TT_prKDData.dato     AND
                            prKDData.SUBTYPE  = TT_prKDData.SUBTYPE  AND
                            prKDData.vg       = TT_prKDData.vg       NO-ERROR.
        IF AVAIL prKDData THEN DO:
            ASSIGN prKDData.Volum = prKDData.Volum   + TT_prKDData.Volum
                   prKDData.Antall = prKDData.Antall + TT_prKDData.Antall.
            IF prKDData.datotid <> TT_prKDData.datotid THEN
                ASSIGN prKDData.datotid = TT_prKDData.datotid.
        END.
        ELSE DO:
            CREATE prKDData.
            BUFFER-COPY TT_prKDData TO prKDData NO-ERROR.
            IF ERROR-STATUS:ERROR = TRUE THEN
                DELETE prKDData.
        END.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-TT_KS_toDB) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE TT_KS_toDB Procedure 
PROCEDURE TT_KS_toDB :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    FOR EACH TT_prKSData:
        FIND prKSData WHERE prKSData.ButikkNr = TT_prKSData.ButikkNr AND
                            prKSData.GruppeNr = TT_prKSData.GruppeNr AND
                            prKSData.dato     = TT_prKSData.dato     AND
                            prKSData.KundeNr  = TT_prKSData.KundeNr  NO-ERROR.
        IF AVAIL prKSData THEN DO:
            ASSIGN prKSData.SumKreditsalg    = prKSData.SumKreditsalg    + TT_prKSData.SumKreditsalg   
                   prKSData.AntallKreditsalg = prKSData.AntallKreditsalg + TT_prKSData.AntallKreditsalg.
            IF prKSData.datotid <> TT_prKSData.datotid THEN
                ASSIGN prKSData.datotid = TT_prKSData.datotid.
        END.
        ELSE DO:
            CREATE prKSData.
            BUFFER-COPY TT_prKSData TO prKSData NO-ERROR.
            IF ERROR-STATUS:ERROR THEN
                DELETE prKSData.
        END.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-TT_n9HG_toDB) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE TT_n9HG_toDB Procedure 
PROCEDURE TT_n9HG_toDB :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    FOR EACH TT_prn9HGData:
        FIND prn9HGData WHERE prn9HGData.hg     = TT_prn9HGData.hg   AND
                            prn9HGData.ButikkNr = TT_prn9HGData.ButikkNr AND
                            prn9HGData.dato     = TT_prn9HGData.dato      NO-ERROR.
        IF AVAIL prn9HGData THEN DO:
            ASSIGN prn9HGData.SumVaresalg    = prn9HGData.SumVaresalg    + TT_prn9HGData.SumVaresalg
                   prn9HGData.MvaKr          = prn9HGData.MvaKr          + TT_prn9HGData.MvaKr
                   prn9HGData.SumVolumAntall = prn9HGData.SumVolumAntall + TT_prn9HGData.SumVolumAntall
                   prn9HGData.SumBruttoFsg   = prn9HGData.SumBruttoFsg   + TT_prn9HGData.SumBruttoFsg
                   prn9HGData.SumRab         = prn9HGData.SumRab         + TT_prn9HGData.SumRab.
            IF prn9HGData.datotid <> TT_prn9HGData.datotid THEN
                ASSIGN prn9HGData.datotid = TT_prn9HGData.datotid.
        END.
        ELSE DO:
            CREATE prn9HGData.
            BUFFER-COPY TT_prn9HGData TO prn9HGData NO-ERROR.
            IF ERROR-STATUS:ERROR THEN
                DELETE prn9HGData.
        END.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-TT_PG_toDB) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE TT_PG_toDB Procedure 
PROCEDURE TT_PG_toDB PRIVATE :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    FOR EACH TT_prPGData:
        FIND prPGData WHERE prPGData.Varegr   = TT_prPGData.Varegr   AND
                            prPGData.ButikkNr = TT_prPGData.ButikkNr AND
                            prPGData.dato     = TT_prPGData.dato      NO-ERROR.
        IF AVAIL prPGData THEN DO:
            ASSIGN prPGData.SumVaresalg    = prPGData.SumVaresalg    + TT_prPGData.SumVaresalg
                   prPGData.MvaKr          = prPGData.MvaKr          + TT_prPGData.MvaKr
                   prPGData.SumVolumAntall = prPGData.SumVolumAntall + TT_prPGData.SumVolumAntall
                   prPGData.SumBruttoFsg   = prPGData.SumBruttoFsg   + TT_prPGData.SumBruttoFsg
                   prPGData.SumRab         = prPGData.SumRab         + TT_prPGData.SumRab.
            IF prPGData.datotid <> TT_prPGData.datotid THEN
                ASSIGN prPGData.datotid = TT_prPGData.datotid.
        END.
        ELSE DO:
            CREATE prPGData.
            BUFFER-COPY TT_prPGData TO prPGData NO-ERROR.
            IF ERROR-STATUS:ERROR THEN
                DELETE prPGData.
        END.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Update_TT_prBMData) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Update_TT_prBMData Procedure 
PROCEDURE Update_TT_prBMData :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       Betalingsmiddler
------------------------------------------------------------------------------*/
/* 
      ASSIGN dKreditkr   = dKreditkr   + IF bongLinje.TTId = 65 THEN BongLinje.linjesum ELSE 0
             iKreditant  = iKreditant  + IF bongLinje.TTId = 65 THEN 1 ELSE 0
             iKontantant = iKontantant + IF bongLinje.TTId = 50 THEN IF bonglinje.linjesum < 0 THEN -1 ELSE 1 ELSE 0
             dKontantkr  = dKontantkr  + IF bongLinje.TTId = 50 THEN BongLinje.linjesum ELSE 0
             dKontantkr  = dKontantkr  + IF bongLinje.TTId = 70 THEN BongLinje.linjesum ELSE 0
             dVeksel     = dVeksel     + IF bongLinje.TTId = 70 THEN BongLinje.linjesum ELSE 0
             dBankkr     = dBankkr     + IF bongLinje.TTId = 52 OR bongLinje.TTId = 58 THEN Bonglinje.LinjeSum ELSE 0
             iBankAnt    = iBankAnt    + IF bongLinje.TTId = 52 OR bongLinje.TTId = 58 THEN 1 ELSE 0
             dBankkr     = dBankkr     + IF bongLinje.TTId = 67 THEN Bonglinje.LinjeSum ELSE 0
             dKupongkr   = dKupongkr   + IF bongLinje.TTId = 56 OR bongLinje.TTId = 72 THEN Bonglinje.LinjeSum ELSE 0
             dCashBack   = dCashBack   + IF bongLinje.TTId = 67 THEN Bonglinje.LinjeSum ELSE 0
             iCashBackAnt = iCashBackAnt + IF bongLinje.TTId = 67 THEN 1 ELSE 0
             dDriveOff    = dDriveOff    + IF bongLinje.TTId = 54 THEN Bonglinje.LinjeSum ELSE 0
             iDriveOffAnt = iDriveOffAnt + IF bongLinje.TTId = 54 THEN 1 ELSE 0
             dAvrundKr   = dAvrundKr   + IF bonglinje.TTId = 78 THEN Bonglinje.LinjeSum ELSE 0
             iAvrundAnt  = iAvrundAnt  + IF bongLinje.TTId = 78 THEN 1 ELSE 0
             dReservBank    = dReservBank    + IF bonglinje.TTId = 79 THEN Bonglinje.LinjeSum ELSE 0
             iReservBankAnt = iReservBankAnt + IF bonglinje.TTId = 79 THEN 1 ELSE 0                 
             dGavekortIn    = dGavekortIn    + IF bonglinje.TTId = 53 THEN Bonglinje.LinjeSum ELSE 0
             iGavekortInAnt = iGavekortInAnt + IF bonglinje.TTId = 53 THEN 1 ELSE 0
             dGavekortUt    = dGavekortUt    + IF bonglinje.TTId = 134 THEN Bonglinje.LinjeSum ELSE 0
             dGavekortUtAnt = dGavekortUtAnt + IF bonglinje.TTId = 134 THEN Bonglinje.Antall ELSE 0 
             dTilGodeInn    = dTilGodeInn + IF bonglinje.TTId = 66 THEN Bonglinje.LinjeSum ELSE 0
             dTilGodeInnAnt = dTilGodeInnAnt + IF bonglinje.TTId = 66 THEN 1 ELSE 0
             dTilGodeUt     = dTilGodeUt    + IF bonglinje.TTId = 69 THEN -1 * Bonglinje.LinjeSum ELSE 0
             dTilGodeUtAnt  = dTilGodeUtAnt + IF bonglinje.TTId = 69 THEN 1 ELSE 0
             dInOutKr       = dInOutKr      +  IF bonglinje.TTId = 59 THEN -1 * Bonglinje.LinjeSum ELSE 0
             iInOutAnt      = iInOutAnt     +  IF bonglinje.TTId = 59 THEN 1 ELSE 0
             dInOutKr       = dInOutKr      +  IF bonglinje.TTId = 61 THEN Bonglinje.LinjeSum ELSE 0
             iInOutAnt      = iInOutAnt     +  IF bonglinje.TTId = 61 THEN 1 ELSE 0
             dInOutKr       = dInOutKr      +  IF bonglinje.TTId = 62 THEN -1 * Bonglinje.LinjeSum ELSE 0
             iInOutAnt      = iInOutAnt     +  IF bonglinje.TTId = 62 THEN 1 ELSE 0
                 .
 
 */
    DEFINE VARIABLE iBetalingsType AS INTEGER    NO-UNDO.
    DEFINE VARIABLE iSubType       AS INTEGER    NO-UNDO.
    DEFINE VARIABLE iSubSubType    AS INTEGER    NO-UNDO.
    DEFINE VARIABLE iKonto         AS INTEGER    NO-UNDO.
    DEFINE VARIABLE iTest          AS INTEGER    NO-UNDO.
    DEFINE VARIABLE dLinjesum      AS DECIMAL    NO-UNDO.
    DEFINE VARIABLE cOrgNr AS CHARACTER   NO-UNDO.

    IF BongLinje.TTId = 65 AND NOT CAN-DO("01,0610,0612",BongLinje.Storrelse) THEN
        RETURN.
    ELSE IF BongLinje.TTId = 53 THEN DO:
        iTest = INT(BongLinje.Storrelse) NO-ERROR.
        IF iTest = ? OR iTest < 1 OR iTest > 20 THEN
            RETURN.
    END.
    ASSIGN dLinjesum = Bonglinje.Linjesum.
    ASSIGN iSubSubType = ?.
    CASE Bonglinje.TTId:
        WHEN 50 OR WHEN 60 THEN /* Kontant or Valuta */
            ASSIGN iBetalingsType = 1
                   iSubType       = ?
                   iKonto         = 1912. /*1930.*/
        WHEN 70 THEN /* Växel */
            ASSIGN iBetalingsType = 1
                   iSubType       = ?
                   iKonto         = 1912. /*1930.*/
        WHEN 52 THEN /* Kreditkort */
            ASSIGN iBetalingsType = 2
                   iSubType       = Bonglinje.antall
/*                    iKonto         = 2445. /*1520.*/ */
            iKonto         = IF iSubType =  1 THEN 2445 ELSE 
                             IF iSubType =  2 THEN 2444 ELSE 
                             IF iSubType =  3 THEN 2444 ELSE
                             IF iSubType =  4 THEN 2444 ELSE
                             IF iSubType =  5 THEN 2445 ELSE
                             IF iSubType =  6 THEN 2444 ELSE 
                             IF iSubType =  7 THEN 2446 ELSE
                             IF iSubType =  8 THEN 2445 ELSE 
                             IF iSubType =  9 THEN 2445 ELSE 
                             IF iSubType = 10 THEN 2444 ELSE 
                             IF iSubType = 11 THEN 2444 ELSE
                             IF iSubType = 12 THEN 2444 ELSE 
                             IF iSubType = 13 THEN 2445 ELSE 
                             IF iSubType = 14 THEN 2445 ELSE
                             IF iSubType = 15 THEN 2445 ELSE
                             IF iSubType = 16 THEN 2445 ELSE 
                             IF iSubType = 17 THEN 2444 ELSE
                             IF iSubType = 18 THEN 2445 ELSE 
                             IF iSubType = 19 THEN 2445 ELSE 
                             IF iSubType = 20 THEN 2444 ELSE 
                             IF iSubType = 21 THEN 2444 ELSE
                             IF iSubType = 22 THEN 2444 ELSE 
                             IF iSubType = 23 THEN 2444 ELSE 
                             IF iSubType = 24 THEN 2444 ELSE      2445. /* 1600 */





        WHEN 53 THEN /* Gavekort */ DO:
            ASSIGN iBetalingsType = 3
                   iSubType       = INT(BongLinje.Storrelse)
                   iKonto         = IF iSubType =  1 THEN 2445 ELSE 
                                    IF iSubType =  2 THEN 1699 ELSE 
                                    IF iSubType =  3 THEN 1699 ELSE
                                    IF iSubType =  4 THEN 1699 ELSE
                                    IF iSubType =  5 THEN 1699 ELSE
                                    IF iSubType =  6 THEN 1680 ELSE 
                                    IF iSubType =  7 THEN 1683 ELSE
                                    IF iSubType =  8 THEN 2422 ELSE 
                                    IF iSubType =  9 THEN 1693 ELSE 
                                    IF iSubType = 10 THEN 1912 ELSE 
                                    IF iSubType = 11 THEN 2446 ELSE
                                    IF iSubType = 12 THEN 2446 ELSE 
                                    IF iSubType = 13 THEN 1699 ELSE 
                                    IF iSubType = 14 THEN 2446 ELSE      1699. /* 1600 */
/*                    iKonto         = IF iSubType =  2 THEN 2445 ELSE IF iSubType = 3 THEN 2445 ELSE                        */
/*                                     IF iSubType =  6 THEN 1680 ELSE IF iSubType = 7 THEN 1683 ELSE                        */
/*                                     IF iSubType =  8 THEN 2422 ELSE IF iSubType = 9 THEN 1693 ELSE                        */
/*                                     IF iSubType = 10 THEN 1912 ELSE IF iSubType = 11 THEN 2446 ELSE                       */
/*                                     IF iSubType = 12 THEN 2446 ELSE IF iSubType = 14 THEN 2446 ELSE      1518. /* 1600 */ */
        END.
        WHEN 54 THEN /* check -> drive off */
            ASSIGN iBetalingsType = 4
                   iSubType       = ?
                   iKonto         = 6385. /*3727.*/
        WHEN 58 THEN DO: /* Bank */
            IF Bonglinje.antall = 13 THEN
                ASSIGN iBetalingsType = 2
                       iSubType       = Bonglinje.antall
                       iKonto         = 2445. /*1520.*/
            ELSE
                ASSIGN iBetalingsType = 58
                       iSubType       = ?
                       iKonto         = ?.
        END.
        WHEN 59 THEN /* Dropp */
            ASSIGN iBetalingsType = 5
                   iSubType       = 2
                   iKonto         = 1930. /*06-2007 ændrat från ?.*/
        WHEN 61 THEN /* Inbetalning */ DO:
            ASSIGN iBetalingsType = 5
                   iSubType       = 0
                   iSubSubType    = IF TRIM(Bonglinje.storrelse) <> "" THEN INT(Bonglinje.storrelse) ELSE iSubSubType
                       iKonto         = IF iSubSubType =  1 THEN 1699 ELSE IF iSubSubType =  2 THEN 1696 ELSE
                                        IF iSubSubType =  3 THEN 1691 ELSE IF iSubSubType =  4 THEN 1518 ELSE
                                        IF iSubSubType =  5 THEN 1921 ELSE IF iSubSubType =  6 THEN 1697 ELSE
                                        IF iSubSubType =  7 THEN 2422 ELSE IF iSubSubType =  8 THEN 1681 ELSE
                                        IF iSubSubType =  9 THEN 1692 ELSE IF iSubSubType = 10 THEN 1693 ELSE
                                        IF iSubSubType = 11 THEN 1694 ELSE IF iSubSubType = 12 THEN 1686 ELSE
                                        IF iSubSubType = 13 THEN 1912 ELSE IF iSubSubType = 14 THEN 2446 /* 2423 */ ELSE
                                        IF iSubSubType = 15 THEN 1674 ELSE IF iSubSubType = 16 THEN 1675 ELSE
                                        IF iSubSubType = 17 THEN 1676 ELSE IF iSubSubType = 18 THEN 1677 ELSE
                                        IF iSubSubType = 19 THEN 1678 ELSE IF iSubSubType = 20 THEN 1679 ELSE  1699. /*1600.*/
/*                    iKonto         = IF iSubSubType =  1 THEN 2421 ELSE IF iSubSubType =  2 THEN 1696 ELSE                  */
/*                                     IF iSubSubType =  3 THEN 1691 ELSE IF iSubSubType =  4 THEN 1518 ELSE                  */
/*                                     IF iSubSubType =  5 THEN 1921 ELSE IF iSubSubType =  6 THEN 1697 ELSE                  */
/*                                     IF iSubSubType =  7 THEN 2422 ELSE IF iSubSubType =  8 THEN 1681 ELSE                  */
/*                                     IF iSubSubType =  9 THEN 1692 ELSE IF iSubSubType = 10 THEN 1693 ELSE                  */
/*                                     IF iSubSubType = 11 THEN 1694 ELSE IF iSubSubType = 12 THEN 1686 ELSE                  */
/*                                     IF iSubSubType = 13 THEN 1930 ELSE IF iSubSubType = 14 THEN 2423 /* 1673 */ ELSE       */
/*                                     IF iSubSubType = 15 THEN 1674 ELSE IF iSubSubType = 16 THEN 1675 ELSE                  */
/*                                     IF iSubSubType = 17 THEN 1676 ELSE IF iSubSubType = 18 THEN 1677 ELSE                  */
/*                                     IF iSubSubType = 19 THEN 1678 ELSE IF iSubSubType = 20 THEN 1679 ELSE  2990. /*1600.*/ */
        END.
        WHEN 62 THEN /* Utbetalning */ DO:
            ASSIGN iBetalingsType = 5
                   iSubType       = 1
                   iSubSubType    = IF TRIM(Bonglinje.storrelse) <> "" THEN INT(Bonglinje.storrelse) ELSE iSubSubType
                   iKonto         = IF iSubSubType =  1 THEN 1699 ELSE IF iSubSubType =  2 THEN 1699 ELSE
                                    IF iSubSubType =  3 THEN 1693 ELSE IF iSubSubType =  4 THEN 1699 ELSE
                                    IF iSubSubType =  5 THEN 1921 ELSE IF iSubSubType =  6 THEN 2422 ELSE
                                    IF iSubSubType =  7 THEN 1686 ELSE IF iSubSubType =  8 THEN 1695 ELSE
                                    IF iSubSubType =  9 THEN 2446 ELSE IF iSubSubType = 10 THEN 1692 ELSE
                                    IF iSubSubType = 11 THEN 1673 ELSE IF iSubSubType = 12 THEN 1674 ELSE
                                    IF iSubSubType = 13 THEN 1675 ELSE IF iSubSubType = 14 THEN 1676 ELSE
                                    IF iSubSubType = 15 THEN 1677 ELSE IF iSubSubType = 16 THEN 1678 ELSE
                                    IF iSubSubType = 17 THEN 1679 ELSE IF iSubSubType = 18 THEN 1699 ELSE
                                    IF iSubSubType = 19 THEN 1699 ELSE IF iSubSubType = 20 THEN 1699 ELSE 1699. /* 06-2007 ændrat från 2990.*/
/*                    iKonto         = IF iSubSubType =  1 THEN 2445 ELSE IF iSubSubType =  2 THEN 2445 ELSE                                      */
/*                                     IF iSubSubType =  3 THEN 1693 ELSE IF iSubSubType =  4 THEN 1699 ELSE                                      */
/*                                     IF iSubSubType =  5 THEN 1921 ELSE IF iSubSubType =  6 THEN 2422 ELSE                                      */
/*                                     IF iSubSubType =  7 THEN 1686 ELSE IF iSubSubType =  8 THEN 1695 ELSE                                      */
/*                                     IF iSubSubType =  9 THEN 2446 ELSE IF iSubSubType = 10 THEN 1692 ELSE                                      */
/*                                     IF iSubSubType = 11 THEN 1673 ELSE IF iSubSubType = 12 THEN 1674 ELSE                                      */
/*                                     IF iSubSubType = 13 THEN 1675 ELSE IF iSubSubType = 14 THEN 1676 ELSE                                      */
/*                                     IF iSubSubType = 15 THEN 1677 ELSE IF iSubSubType = 16 THEN 1678 ELSE                                      */
/*                                     IF iSubSubType = 17 THEN 1679 ELSE IF iSubSubType = 18 THEN 1699 ELSE                                      */
/*                                     IF iSubSubType = 19 THEN 1699 ELSE IF iSubSubType = 20 THEN 1699 ELSE 1699. /* 06-2007 ændrat från 2990.*/ */
        END.
        WHEN 65 THEN DO: /* Kredit */
            IF TRIM(Bonglinje.storrelse) = "0610" OR TRIM(Bonglinje.storrelse) = "0612" THEN DO:
                ASSIGN iBetalingsType = 6
                       iSubType       = 1
                       iSubSubtype    = INT(SUBSTR(TRIM(Bonglinje.storrelse),4))
                       iKonto         = 1518. /* 1600 */ 
/*                        dLinjesum      = dLinjesum * IF TRIM(Bonglinje.storrelse) = "0610" THEN -1 ELSE 1. */
            END.
            ELSE DO:
                ASSIGN iBetalingsType = 3     /* Central deb */
                       iSubType       = 1
                       iKonto         = 2445. /*1530.*/
            END.
        END.
        WHEN 67 THEN /* Cashback */
            ASSIGN iBetalingsType = 9
                   iSubType       = ?
                   iKonto         = ?.    /*ingen ändring*/
        WHEN 78 THEN /* Öresavrundning */
            ASSIGN iBetalingsType = 7
                   iSubType       = ?
                   iKonto         = 3740. /*3790*/
    END CASE.
    FIND TT_prBMData WHERE TT_prBMData.ButikkNr      = Bonglinje.ButikkNr AND
                           TT_prBMData.dato          = bokforingsdag.dato  AND   /* växel skall korrigera kontant */
                           TT_prBMData.Betalingstype = iBetalingsType AND 
                           TT_prBMData.SUBTYPE       = iSubType AND 
                           TT_prBMData.subsubtype    = iSubSubtype NO-ERROR.

    IF NOT AVAIL TT_prBMData THEN DO:
        cOrgNr = getOrgNr().
        CREATE TT_prBMData.
        ASSIGN TT_prBMData.ButikkNr      = Bonglinje.ButikkNr
               TT_prBMData.dato          = bokforingsdag.dato
               TT_prBMData.Betalingstype = iBetalingsType 
               TT_prBMData.SUBTYPE       = iSubType
               TT_prBMData.subsubtype    = iSubSubType
               TT_prBMData.Konto         = iKonto
               TT_prBMData.datotid       = deDatoTid
               TT_prBMData.Organisationsnr = cOrgNr
/*                TT_prBMData.BettypeBeskrivelse = cBettypeBeskrivelse */
/*                TT_prBMData.subtypenavn        = cSubtypenavn        */
              .
    END.
    ASSIGN TT_prBMData.Belop = TT_prBMData.Belop + dLinjesum
/*            TT_prBMData.Kassadiff = TT_prBMData.Kassadiff + */
          .
/*     IF bonglinje.TTId = 61 OR Bonglinje.TTId = 62 THEN DO: /* Inbetalin / utbetaling -> kontant korrigeras */ */
/*         FIND TT_prBMData WHERE TT_prBMData.ButikkNr      = Bonglinje.ButikkNr AND                             */
/*                                TT_prBMData.dato          = bokforingsdag.dato  AND                            */
/*                                TT_prBMData.Betalingstype = 1 AND                                              */
/*                                TT_prBMData.SUBTYPE       = ? AND                                              */
/*                                TT_prBMData.subsubtype    = ? NO-ERROR.                                        */
/*                                                                                                               */
/*         IF NOT AVAIL TT_prBMData THEN DO:                                                                     */
/*             CREATE TT_prBMData.                                                                               */
/*             ASSIGN TT_prBMData.ButikkNr      = Bonglinje.ButikkNr                                             */
/*                    TT_prBMData.dato          = bokforingsdag.dato                                             */
/*                    TT_prBMData.Betalingstype = 1                                                              */
/*                    TT_prBMData.SUBTYPE       = ?                                                              */
/*                    TT_prBMData.subsubtype    = ?                                                              */
/*                    TT_prBMData.Konto         = 1930. /*1912.*/                                                */
/*         END.                                                                                                  */
/*         ASSIGN TT_prBMData.Belop = TT_prBMData.Belop + (dLinjesum * IF Bonglinje.TTId = 61 THEN -1 ELSE 1).   */
/*     END.                                                                                                      */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Update_TT_prKD2Data) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Update_TT_prKD2Data Procedure 
PROCEDURE Update_TT_prKD2Data :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       Solgte volumer drivstoff på kort
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipType AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE iKoeff AS INTEGER     NO-UNDO.
/*     summera volym per kvalitet (hg)                               */
/*     när alla bonglinjer är genomgångna skall vi köra UpdateKDData */
    IF ipType = "VOLUM" THEN DO:
        FIND TT_prKD2Data WHERE TT_prKD2Data.ButikkNr = Bonglinje.ButikkNr AND
                            TT_prKD2Data.dato        = bokforingsdag.dato AND
                            TT_prKD2Data.SUBTYPE     = 0                  AND
                            TT_prKD2Data.hg          = Bonglinje.hovedgr NO-ERROR.
        IF NOT AVAIL TT_prKD2Data THEN DO:
            CREATE TT_prKD2Data.
            ASSIGN TT_prKD2Data.ButikkNr    = Bonglinje.ButikkNr
                   TT_prKD2Data.dato        = bokforingsdag.dato
                   TT_prKD2Data.SUBTYPE     = 0
                   TT_prKD2Data.hg          = Bonglinje.hovedgr
                   TT_prKD2Data.datotid     = deDatoTid
                   .
        END.

        iKoeff = IF Bonglinje.Antall = 0 THEN 0 ELSE IF Bonglinje.Antall > 0 THEN 1 ELSE -1.
        ASSIGN TT_prKD2Data.Volum  = TT_prKD2Data.Volum + BongLinje.Antall
               TT_prKD2Data.Belopp = TT_prKD2Data.Belopp + iKoeff * (BongLinje.Linjesum - Bonglinje.Linjerab - Bonglinje.SubtotalRab)
               TT_prKD2Data.mva    = TT_prKD2Data.mva + iKoeff * BongLinje.MvaKr
               TT_prKD2Data.Antall = TT_prKD2Data.Antall + 1.

    END.
    ELSE IF ipType = "KORT" THEN DO:
        FOR EACH TT_prKD2Data WHERE TT_prKD2Data.SUBTYPE = 0:
            ASSIGN TT_prKD2Data.SUBTYPE     = BongLinje.Antall
                   TT_prKD2Data.subtypenavn = getSubtypeName(INT(BongLinje.Antall)).
        END.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Update_TT_prKDData) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Update_TT_prKDData Procedure 
PROCEDURE Update_TT_prKDData :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       Solgte volumer drivstoff på kort
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipType AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE iKoeff AS INTEGER     NO-UNDO.
/*     summera volym per kvalitet (hg)                               */
/*     när alla bonglinjer är genomgångna skall vi köra UpdateKDData */
    IF ipType = "VOLUM" THEN DO:
        FIND TT_prKDData WHERE TT_prKDData.ButikkNr = Bonglinje.ButikkNr AND
                            TT_prKDData.dato        = bokforingsdag.dato AND
                            TT_prKDData.SUBTYPE     = 0                  AND
                            TT_prKDData.vg          = Bonglinje.varegr NO-ERROR.
        IF NOT AVAIL TT_prKDData THEN DO:
            CREATE TT_prKDData.
            ASSIGN TT_prKDData.ButikkNr    = Bonglinje.ButikkNr
                   TT_prKDData.dato        = bokforingsdag.dato
                   TT_prKDData.SUBTYPE     = 0
                   TT_prKDData.vg          = Bonglinje.varegr
                   TT_prKDData.datotid     = deDatoTid
                   .
        END.
        iKoeff = IF Bonglinje.Antall = 0 THEN 0 ELSE IF Bonglinje.Antall > 0 THEN 1 ELSE -1.
        ASSIGN TT_prKDData.Volum  = TT_prKDData.Volum + BongLinje.Antall
               TT_prKDData.Belopp = TT_prKDData.Belopp + iKoeff * (BongLinje.Linjesum - Bonglinje.Linjerab - Bonglinje.SubtotalRab).
            .
    END.
    ELSE IF ipType = "KORT" THEN DO:
        FOR EACH TT_prKDData WHERE TT_prKDData.SUBTYPE = 0:
/* kommenterad text har använts för test för att få med kontanter */
/* ASSIGN TT_prKDData.SUBTYPE     = IF Bonglinje.TTid = 50 THEN 50 ELSE BongLinje.Antall                             */
/*        TT_prKDData.subtypenavn = IF Bonglinje.TTid = 50 THEN "KONTANT" ELSE getSubtypeName(INT(BongLinje.Antall)) */
/*        TT_prKDData.Antall      = TT_prKDData.Antall + 1.                                                          */
            ASSIGN TT_prKDData.SUBTYPE     = BongLinje.Antall
                   TT_prKDData.subtypenavn = getSubtypeName(INT(BongLinje.Antall))
                   TT_prKDData.Antall      = TT_prKDData.Antall + 1
                   TT_prKDData.Belopp      = TT_prKDData.Belopp + BongLinje.Linjesum.
        END.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Update_TT_prKSData) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Update_TT_prKSData Procedure 
PROCEDURE Update_TT_prKSData :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       SUM kreditsalg (stasjonskreditt)
------------------------------------------------------------------------------*/
    FIND TT_prKSData WHERE TT_prKSData.ButikkNr = BongHode.ButikkNr  AND
                           TT_prKSData.GruppeNr = BongHode.GruppeNr  AND
                           TT_prKSData.dato     = bokforingsdag.dato AND
                           TT_prKSData.KundeNr  = BongHode.Kundenr   NO-ERROR.
    IF NOT AVAIL TT_prKSData THEN DO:
        CREATE TT_prKSData.
        ASSIGN TT_prKSData.ButikkNr = BongHode.ButikkNr
               TT_prKSData.GruppeNr = BongHode.GruppeNr
               TT_prKSData.dato     = bokforingsdag.dato
               TT_prKSData.KundeNr  = BongHode.Kundenr
               TT_prKSData.datotid  = deDatoTid
               .
    END.
    ASSIGN TT_prKSData.SumKreditsalg    = TT_prKSData.SumKreditsalg    + Bonglinje.linjesum
           TT_prKSData.AntallKreditsalg = TT_prKSData.AntallKreditsalg + 1.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Update_TT_prn9HGData) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Update_TT_prn9HGData Procedure 
PROCEDURE Update_TT_prn9HGData :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       varegruppesalg   
------------------------------------------------------------------------------*/
    DEFINE VARIABLE iKonto AS INTEGER    NO-UNDO.
    FIND TT_prn9HGData WHERE TT_prn9HGData.Hg   = Bonglinje.hovedgr   AND
                           TT_prn9HGData.ButikkNr = Bonglinje.ButikkNr AND
                           TT_prn9HGData.dato     = bokforingsdag.dato     NO-ERROR.
    IF NOT AVAIL TT_prn9HGData THEN DO:
        /* !!! finn konto */
        ASSIGN iKonto = getHGKonto().
        CREATE TT_prn9HGData.
        ASSIGN TT_prn9HGData.hg       = Bonglinje.hovedgr
               TT_prn9HGData.ButikkNr = Bonglinje.ButikkNr
               TT_prn9HGData.dato     = bokforingsdag.dato
               TT_prn9HGData.Konto    = iKonto
               TT_prn9HGData.datotid  = deDatoTid
            .
    END.
    ASSIGN TT_prn9HGData.SumVaresalg    = TT_prn9HGData.SumVaresalg    + (iKoeff * dSalgssumTmp)
           TT_prn9HGData.MvaKr          = TT_prn9HGData.MvaKr          + (iKoeff * BongLinje.MvaKr)
           TT_prn9HGData.SumVolumAntall = TT_prn9HGData.SumVolumAntall + BongLinje.Antall
          TT_prn9HGData.SumBruttoFsg    = TT_prn9HGData.SumBruttoFsg   + (iKoeff * BongLinje.Linjesum)
          TT_prn9HGData.SumRab          = TT_prn9HGData.SumRab         + (iKoeff * (BongLinje.LinjeRab + BongLinje.SubtotalRab))
        .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Update_TT_prPGData) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Update_TT_prPGData Procedure 
PROCEDURE Update_TT_prPGData :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       varegruppesalg   
------------------------------------------------------------------------------*/
    DEFINE VARIABLE iKonto AS INTEGER    NO-UNDO.
    DEFINE VARIABLE cOrgNr AS CHARACTER   NO-UNDO.
    FIND TT_prPGData WHERE TT_prPGData.Varegr   = Bonglinje.varegr   AND
                           TT_prPGData.ButikkNr = Bonglinje.ButikkNr AND
                           TT_prPGData.dato     = bokforingsdag.dato     NO-ERROR.
    IF NOT AVAIL TT_prPGData THEN DO:
        /* !!! finn konto */
        ASSIGN iKonto = getKonto().
        ASSIGN cOrgNr = getOrgNr().
        CREATE TT_prPGData.
        ASSIGN TT_prPGData.Varegr   = Bonglinje.varegr
               TT_prPGData.ButikkNr = Bonglinje.ButikkNr
               TT_prPGData.dato     = bokforingsdag.dato
               TT_prPGData.Konto    = iKonto
               TT_prPGData.datotid  = deDatoTid
               TT_prPGData.Organisationsnr = cOrgNr.
            .
    END.
    ASSIGN TT_prPGData.SumVaresalg    = TT_prPGData.SumVaresalg    + (iKoeff * dSalgssumTmp)
           TT_prPGData.MvaKr          = TT_prPGData.MvaKr          + (iKoeff * BongLinje.MvaKr)
           TT_prPGData.SumVolumAntall = TT_prPGData.SumVolumAntall + BongLinje.Antall
          TT_prPGData.SumBruttoFsg    = TT_prPGData.SumBruttoFsg   + (iKoeff * BongLinje.Linjesum)
          TT_prPGData.SumRab          = TT_prPGData.SumRab         + (iKoeff * (BongLinje.LinjeRab + BongLinje.SubtotalRab))
        .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

/* ************************  Function Implementations ***************** */

&IF DEFINED(EXCLUDE-getHGKonto) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getHGKonto Procedure 
FUNCTION getHGKonto RETURNS INTEGER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  FIND FIRST konto WHERE konto.butik = 0 AND
                         konto.dato  = DATE(1,1,2000) AND
                         konto.vg    = bonglinje.hovedgr NO-LOCK NO-ERROR.
/*   IF NOT AVAIL konto THEN                                                      */
/*       FIND FIRST konto WHERE konto.butik = 0 AND                               */
/*                              konto.dato  = DATE(1,1,2000) AND                  */
/*                              konto.vg    = bonglinje.hovedgr NO-LOCK NO-ERROR. */
  RETURN IF AVAIL konto THEN konto.kontonummer ELSE 1699.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getKonto) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getKonto Procedure 
FUNCTION getKonto RETURNS INTEGER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  FIND FIRST konto WHERE konto.butik = 0 AND
                         konto.dato  = DATE(1,1,2000) AND
                         konto.vg    = bonglinje.varegr NO-LOCK NO-ERROR.
  IF NOT AVAIL konto THEN DO:
      FIND Vargr WHERE Vargr.vg = bonglinje.varegr NO-LOCK NO-ERROR.
      IF AVAIL VarGr THEN DO:
          FIND huvgr OF vargr NO-LOCK NO-ERROR.
          FIND FIRST konto WHERE konto.butik = 0 AND
                                 konto.dato  = DATE(1,1,2000) AND
                                 konto.vg    = IF AVAIL huvgr THEN HuvGr.hg ELSE bonglinje.hovedgr NO-LOCK NO-ERROR.
      END.
  END.
  RETURN IF AVAIL konto THEN konto.kontonummer ELSE 1699.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getOrgNr) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getOrgNr Procedure 
FUNCTION getOrgNr RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
-----------------------------------------------------------------------------*/
  FIND butiker WHERE butiker.butik = BongLinje.ButikkNr NO-LOCK NO-ERROR.
  IF AVAIL butiker THEN
      RETURN Butiker.OrganisasjonsNr.
  ELSE
      RETURN "".   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getSubtypeName) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getSubtypeName Procedure 
FUNCTION getSubtypeName RETURNS CHARACTER
  ( INPUT iSubType AS INTEGER ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
   DEFINE VARIABLE cSubtypeName AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE cSubTypeNr   AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE cReturNamn   AS CHARACTER  NO-UNDO.
   ASSIGN cSubtypeNr   = "1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25"
          cSubtypename = "PREEM,SIV,PREEM SÅIFA,LOGPAY,UNO-X NO,UNO-X DK,RETAIN24,VOLVO,NESTE,DKV,PREEM FTG,FUNDINS,BANKKORT,VAKANT,VAKANT,VAKANT,UTA,BONUSKORT,OKDK,E100,EDC,TP24,ARIS,Eurowag,".
   IF iSubType = 0 THEN DO:
     cReturNamn = "RETAIN24".
   END.
   ELSE IF CAN-DO(cSubtypeNr,STRING(iSubType)) THEN
       ASSIGN cReturNamn = ENTRY(iSubType,cSubTypename).
   IF cReturNamn = "" THEN
       ASSIGN cReturNamn = "OKÄNT".
   RETURN cReturNamn.   /* Function return value. */

/* 1   PREEM      */
/* 2   PREEM VISA */
/* 3   SÅIFA      */
/* 4   TEPAR      */
/* 5   HY/TEX NO  */
/* 6   HY/TEX DK  */
/* 7   SAAB/OPEL  */
/* 8   VOLVO      */
/* 9   NESTE      */
/* 10  DKV        */
/* 11  OK         */
/* 12  UNO-X      */
/* 13  BANKKORT   */
/* 14  AMEX       */
/* 15  DINERS     */
/* 16  FINAX      */
/* 17  UTA        */
/* 18  BONUSKORT  */
/* 19  CAMPING    */
/* 20             */
/* 21             */
/* 22             */
/* 23             */
/* 24             */
/* 25             */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

