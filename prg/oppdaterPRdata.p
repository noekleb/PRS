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

DEFINE INPUT  PARAMETER dDatasettid AS DECIMAL     NO-UNDO.
/* DEFINE INPUT  PARAMETER iButikkNr AS INTEGER    NO-UNDO. */
/* DEFINE INPUT  PARAMETER dDato     AS DATE       NO-UNDO. */

DEFINE VARIABLE deDatotid      AS DECIMAL    NO-UNDO.
DEFINE VARIABLE cDatoTid       AS CHARACTER  NO-UNDO.

DEFINE VARIABLE dMvaKr            AS DECIMAL    NO-UNDO.
DEFINE VARIABLE dMvaDrivm         AS DECIMAL    NO-UNDO.
DEFINE VARIABLE dSalgssum         AS DECIMAL    NO-UNDO.
DEFINE VARIABLE iKoeff            AS INTEGER    NO-UNDO.
/**/ DEFINE VARIABLE iKontantant AS INTEGER    NO-UNDO.
/**/ DEFINE VARIABLE dKontantkr  AS DECIMAL    NO-UNDO.
/**/ DEFINE VARIABLE iBankAnt    AS INTEGER    NO-UNDO.
/**/ DEFINE VARIABLE dBankkr     AS DECIMAL    NO-UNDO.
/**/ DEFINE VARIABLE dAvrundKr   AS DECIMAL    NO-UNDO.
/**/ DEFINE VARIABLE iAvrundAnt  AS INTEGER    NO-UNDO.
/**/ DEFINE VARIABLE dKreditkr   AS DECIMAL    NO-UNDO.
/**/ DEFINE VARIABLE iKreditant  AS INTEGER    NO-UNDO.
/**/ DEFINE VARIABLE dGavekortIn AS DECIMAL    NO-UNDO.
/**/ DEFINE VARIABLE iGavekortInAnt AS INTEGER    NO-UNDO.
/**/ DEFINE VARIABLE dCashBack      AS DECIMAL    NO-UNDO.
/**/ DEFINE VARIABLE iCashBackAnt AS INTEGER    NO-UNDO.
/**/ DEFINE VARIABLE dDriveOff    AS DECIMAL    NO-UNDO.
/**/ DEFINE VARIABLE iDriveOffAnt AS INTEGER    NO-UNDO.
/**/ DEFINE VARIABLE dInOutKr     AS DECIMAL    NO-UNDO.
/**/ DEFINE VARIABLE iInOutAnt    AS INTEGER    NO-UNDO.
DEFINE VARIABLE dSalgssumTmp    AS DECIMAL    NO-UNDO.
DEFINE VARIABLE lHarSalg        AS LOGICAL    NO-UNDO.
DEFINE VARIABLE cPRFSdagar AS CHARACTER   NO-UNDO.
DEFINE TEMP-TABLE TT_prBSData   NO-UNDO LIKE prBSData.
DEFINE TEMP-TABLE TT_prCHData   NO-UNDO LIKE prCHData.
DEFINE TEMP-TABLE TT_prFSData   NO-UNDO LIKE prFSData.

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
cDatoTid = STRING(YEAR(TODAY),"9999") + 
           STRING(MONTH(TODAY),"99")  +
           STRING(DAY(TODAY),"99")    +
           REPLACE(STRING(TIME,"HH:MM:SS"),":","").
deDatoTid = DECI(cDatoTid).

RUN FillTempData.
RUN OppdaterPrData.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-FillTempData) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE FillTempData Procedure 
PROCEDURE FillTempData :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE iMvaGr AS INTEGER    NO-UNDO.
DEFINE VARIABLE dManuell    AS DECIMAL    NO-UNDO.
DEFINE VARIABLE dLokal      AS DECIMAL    NO-UNDO.
DEFINE VARIABLE dCentralOk  AS DECIMAL    NO-UNDO.
DEFINE VARIABLE dCentralNok AS DECIMAL    NO-UNDO.
DEFINE VARIABLE dManVol   AS DECIMAL    NO-UNDO.
DEFINE VARIABLE dLokalVol AS DECIMAL    NO-UNDO.
DEFINE VARIABLE dCOkVol   AS DECIMAL    NO-UNDO.
DEFINE VARIABLE dCNOkVol  AS DECIMAL    NO-UNDO.
DEFINE VARIABLE lDrivmedelInne AS LOGICAL     NO-UNDO.
DEFINE VARIABLE lVarorInne     AS LOGICAL     NO-UNDO.
DEFINE VARIABLE lUteFsg    AS LOGICAL     NO-UNDO.
DEFINE VARIABLE iAntBlandet      AS INTEGER     NO-UNDO.
DEFINE VARIABLE iAntKunDrivstoff AS INTEGER     NO-UNDO.
DEFINE VARIABLE iAntKunVare      AS INTEGER     NO-UNDO.
    
  FOR EACH BongHode WHERE data.BongHode.DataSettId = dDatasettid  NO-LOCK:
    IF BongHode.Makulert = 2 THEN DO:
      NEXT.
    END.
    RUN NullStillVars.
    ASSIGN lDrivmedelInne = FALSE
           lVarorInne     = FALSE
           lUteFsg        = FALSE
           iAntBlandet      = 0
           iAntKunDrivstoff = 0
           iAntKunVare      = 0.

    FOR EACH Bonglinje WHERE bonglinje.b_id = bonghode.b_id NO-LOCK:
      IF CAN-DO("1,3,10",STRING(BongLinje.TTId)) AND BongLinje.Antall <> 0 AND Bonglinje.Makulert = FALSE THEN SALG: DO:
        FIND Artbas WHERE ArtBas.ArtikkelNr = DECI(Bonglinje.Artikkelnr) NO-LOCK NO-ERROR.
        FIND VarGr OF Artbas NO-LOCK NO-ERROR.
        IF AVAIL VarGr THEN
            FIND HuvGr OF VarGr NO-LOCK NO-ERROR.
        ELSE
            RELEASE HuvGr.
            IF BongLinje.KasseNr > 10 THEN
                lUteFsg = TRUE.
        ASSIGN 
          iKoeff       = IF BongLinje.Antall > 0 THEN 1 ELSE -1
          iMvaGr       = IF BongLinje.MvaGr = 0 OR BongLinje.MvaGr > 9 THEN 9 ELSE BongLinje.MvaGr
          dSalgssumTmp = BongLinje.LinjeSum - BongLinje.Linjerab - BongLinje.SubtotalRab
          dMvaKr    = dMvaKr    + iKoeff * BongLinje.MvaKr
          dSalgssum = dSalgssum + (iKoeff * dSalgssumTmp)
          lHarSalg  = TRUE.
        prBSData:
        DO:
          FIND FIRST TT_prBSData WHERE TT_prBSData.butikknr = Bonghode.butikknr AND 
                                       TT_prBSData.dato     = Bonghode.dato    NO-ERROR.
          IF NOT AVAIL TT_prBSData THEN DO:
            CREATE TT_prBSData.
            ASSIGN TT_prBSData.butikknr = Bonghode.butikknr
                   TT_prBSData.dato     = Bonghode.dato
                   TT_prBSData.datotid  = deDatoTid.
          END.
          ASSIGN 
            TT_prBSData.TotSalg  = TT_prBSData.TotSalg + (iKoeff * dSalgssumTmp)
            TT_prBSData.MvaKr    = TT_prBSData.MvaKr   + (iKoeff * BongLinje.MvaKr)
            TT_prBSData.TotDrvm  = TT_prBSData.TotDrvm + (IF AVAIL HuvGr AND HuvGr.avdelingNr = 1 THEN (iKoeff * dSalgssumTmp) ELSE 0)
            TT_prBSData.DrivmInne = TT_prBSData.DrivmInne + (IF BongLinje.KasseNr < 11 AND AVAIL HuvGr AND HuvGr.avdelingNr = 1 THEN (iKoeff * dSalgssumTmp) ELSE 0)
            TT_prBSData.MvaDrm    = TT_prBSData.MvaDrm + (IF AVAIL HuvGr AND HuvGr.avdelingNr = 1 THEN iKoeff * BongLinje.MvaKr ELSE 0).
            IF lUteFsg = FALSE THEN DO:
                IF AVAIL HuvGr AND HuvGr.avdelingNr = 1 THEN
                    lDrivmedelInne = TRUE.
                ELSE 
                    lVarorInne     = TRUE.
            END.
        END.
        IF BongLinje.HovedGr = 44 THEN prCHData: DO:
          FIND FIRST TT_prCHData WHERE TT_prCHData.butikknr   = Bonglinje.ButikkNr AND TT_prCHData.dato = Bonglinje.dato AND
                                       TT_prCHData.hg         = Bonglinje.HovedGr  AND TT_prCHData.vg   = Bonglinje.VareGr AND 
                                       TT_prCHData.Artikkelnr = Bonglinje.ArtikkelNr NO-ERROR.
          IF NOT AVAIL TT_prCHData THEN DO:
            CREATE TT_prCHData.
            ASSIGN TT_prCHData.butikknr = Bonglinje.ButikkNr
                   TT_prCHData.dato     = Bonglinje.dato   
                   TT_prCHData.hg       = Bonglinje.HovedGr  
                   TT_prCHData.vg       = Bonglinje.VareGr
                   TT_prCHData.artikkelnr = Bonglinje.ArtikkelNr 
                   TT_prCHData.datotid  = deDatoTid NO-ERROR.
            IF ERROR-STATUS:ERROR THEN DO:
              DELETE TT_prCHData.
              LEAVE prCHData.
            END.
          END.
          ASSIGN TT_prCHData.AntSolgt  = TT_prCHData.AntSolgt + BongLinje.Antall.
        END.
        prFSData:
        DO:
         IF AVAIL Artbas AND AVAIL HuvGr THEN DO:
           dManuell    = 0.
           dManVol     = 0.
           dLokal      = 0.
           dLokalVol   = 0.
           dCentralNok = 0.
           dCNOkVol   = 0.
           dCentralOk = 0.
           dCOkVol    = 0.
           IF HuvGr.AvdelingNr = 1 AND linjerab <> 0 THEN DO:
             IF Bonglinje.KampEierId = 0  THEN DO:
               /* manuell rabatt */
               ASSIGN dManuell = Bonglinje.linjerab * iKoeff
                      dManVol  = Bonglinje.antall.
             END.
             ELSE IF Bonglinje.KampEierId = Bonglinje.butikknr THEN DO:
               /* lokal kampanj */
               ASSIGN dLokal    = Bonglinje.linjerab * iKoeff
                      dLokalVol = Bonglinje.Antall.
             END.
             ELSE DO:
               /* om vi finner preemkampanj, se på giltigt datum */
               FIND kampanjemixmatch NO-LOCK WHERE KampanjeMixMatch.KampEierId = bonglinje.kampeierid AND
                                                   KampanjeMixMatch.KampId     = bonglinje.kampid NO-ERROR.
               IF NOT AVAIL kampanjemixmatch THEN DO:
                 /* Lokal rabatt ? */
                 ASSIGN dCentralNOK = Bonglinje.linjerab * iKoeff
                        dCNOkVol    = Bonglinje.Antall.
               END.
               ELSE DO:
                 /* kontroll av datum och tid är OK, Det kan hända att man ändra tid och datum på central kampanj */
                 IF bonglinje.dato < KampanjeMixMatch.KampStartDato OR bonglinje.dato = KampanjeMixMatch.KampStartDato AND 
                                     bonglinje.transtid < KampanjeMixMatch.KampStartTid THEN DO:
                     ASSIGN dCentralNOK = Bonglinje.linjerab * iKoeff
                            dCNOkVol = Bonglinje.Antall.
                 END.
                 ELSE IF bonglinje.dato > KampanjeMixMatch.KampSluttDato OR bonglinje.dato = KampanjeMixMatch.KampSluttDato AND 
                         bonglinje.transtid > KampanjeMixMatch.KampSluttTid THEN DO:
                     ASSIGN dCentralNOK = Bonglinje.linjerab * iKoeff
                            dCNOkVol    = Bonglinje.Antall.
                 END.
                 ELSE DO:
                     ASSIGN dCentralOK = Bonglinje.linjerab * iKoeff
                            dCOkVol    = Bonglinje.Antall.
                 END.
               END.
             END.
           END.
           FIND FIRST TT_prFSData WHERE TT_prFSData.butikknr = Bonglinje.ButikkNr AND
                                        TT_prFSData.dato     = Bonglinje.dato     AND TT_prFSData.avdelingsnr = HuvGr.Avdelingnr AND
                                        TT_prFSData.hg       = Bonglinje.HovedGr  AND TT_prFSData.vg = Bonglinje.VareGr  NO-ERROR.
           IF NOT AVAIL TT_prFSData THEN DO:
               CREATE TT_prFSData.
               ASSIGN TT_prFSData.butikknr    = Bonglinje.ButikkNr
                      TT_prFSData.dato        = Bonglinje.dato   
                      TT_prFSData.avdelingsnr = HuvGr.Avdelingnr
                      TT_prFSData.hg          = Bonglinje.HovedGr  
                      TT_prFSData.vg          = Bonglinje.VareGr
                      TT_prFSData.datotid     = deDatoTid.
               IF NOT CAN-DO(cPRFSdagar,STRING(Bonglinje.dato)) THEN
                   cPRFSdagar = cPRFSdagar + (IF cPRFSdagar = "" THEN "" ELSE ",") + STRING(Bonglinje.dato).
           END.
           ASSIGN TT_prFSData.AntSolgtNto  = TT_prFSData.AntSolgtNto + BongLinje.Antall
                  TT_prFSData.AntSolgtPlu  = TT_prFSData.AntSolgtPlu + (IF Artbas.Opris = TRUE THEN BongLinje.Antall ELSE 0)
                  TT_prFSData.fsgnto       = TT_prFSData.fsgnto + (iKoeff * (dSalgssumTmp - BongLinje.MvaKr))
                  TT_prFSData.dbkr         = TT_prFSData.dbkr   + (iKoeff * (dSalgssumTmp - BongLinje.MvaKr - BongLinje.VVarekost))
                  TT_prFSData.mvakr        = TT_prFSData.mvakr  + (iKoeff * BongLinje.MvaKr)
                  TT_prFSData.ManRabatter           = TT_prFSData.ManRabatter           + dManuell   
                  TT_prFSData.ManRabatter_vol       = TT_prFSData.ManRabatter_vol       + dManVol    
                  TT_prFSData.LokKampRabatt         = TT_prFSData.LokKampRabatt         + dLokal     
                  TT_prFSData.LokKampRabatt_vol     = TT_prFSData.LokKampRabatt_vol     + dLokalVol  
                  TT_prFSData.CentKampRabattNOK     = TT_prFSData.CentKampRabattNOK     + dCentralNok
                  TT_prFSData.CentKampRabattNOK_vol = TT_prFSData.CentKampRabattNOK_vol + dCNOkVol   
                  TT_prFSData.CentKampRabattOK      = TT_prFSData.CentKampRabattOK      + dCentralOk 
                  TT_prFSData.CentKampRabattOK_vol  = TT_prFSData.CentKampRabattOK_vol  + dCOkVol.    
         END.
        END.
      END. /* SALG */
/* vid uppdat av finansdag och cTTIdString bara innehåller "95" uppdateras inte btovareknd */
      IF BongLinje.Makulert = FALSE THEN
      ASSIGN 
/*        dRingcountant = dRingcountant + IF bongLinje.TTId = 93 THEN BongLinje.SubtotalRab ELSE 0 */
       dKreditkr   = dKreditkr   + IF bongLinje.TTId = 65 THEN BongLinje.linjesum ELSE 0
       iKreditant  = iKreditant  + IF bongLinje.TTId = 65 THEN 1 ELSE 0
       iKontantant = iKontantant + IF bongLinje.TTId = 50 THEN IF bonglinje.linjesum < 0 THEN -1 ELSE 1 ELSE 0
       dKontantkr  = dKontantkr  + IF bongLinje.TTId = 50 THEN BongLinje.linjesum ELSE 0
       dKontantkr  = dKontantkr  + IF bongLinje.TTId = 70 THEN BongLinje.linjesum ELSE 0
       dBankkr     = dBankkr   + IF bongLinje.TTId = 52 OR bongLinje.TTId = 58 THEN Bonglinje.LinjeSum ELSE 0
       iBankAnt    = iBankAnt  + IF bongLinje.TTId = 52 OR bongLinje.TTId = 58 THEN 1 ELSE 0
       dBankkr     = dBankkr   + IF bongLinje.TTId = 67 THEN Bonglinje.LinjeSum ELSE 0
       dCashBack   = dCashBack + IF bongLinje.TTId = 67 THEN Bonglinje.LinjeSum ELSE 0
       iCashBackAnt = iCashBackAnt + IF bongLinje.TTId = 67 THEN 1 ELSE 0
       dDriveOff    = dDriveOff    + IF bongLinje.TTId = 54 THEN Bonglinje.LinjeSum ELSE 0
       iDriveOffAnt = iDriveOffAnt + IF bongLinje.TTId = 54 THEN 1 ELSE 0
       dAvrundKr   = dAvrundKr  + IF bonglinje.TTId = 78 THEN Bonglinje.LinjeSum ELSE 0
       iAvrundAnt  = iAvrundAnt + IF bongLinje.TTId = 78 THEN 1 ELSE 0
       dGavekortIn    = dGavekortIn    + IF bonglinje.TTId = 53 THEN Bonglinje.LinjeSum ELSE 0
       iGavekortInAnt = iGavekortInAnt + IF bonglinje.TTId = 53 THEN 1 ELSE 0
       dInOutKr  = dInOutKr  + IF bonglinje.TTId = 59 THEN -1 * Bonglinje.LinjeSum ELSE 0
       iInOutAnt = iInOutAnt + IF bonglinje.TTId = 59 THEN 1 ELSE 0
       dInOutKr  = dInOutKr  + IF bonglinje.TTId = 61 THEN Bonglinje.LinjeSum ELSE 0
       iInOutAnt = iInOutAnt + IF bonglinje.TTId = 61 THEN 1 ELSE 0
       dInOutKr  = dInOutKr  + IF bonglinje.TTId = 62 THEN -1 * Bonglinje.LinjeSum ELSE 0
       iInOutAnt = iInOutAnt + IF bonglinje.TTId = 62 THEN 1 ELSE 0.
    END.
    IF lHarSalg = TRUE THEN prBSData2: DO:
        IF lUteFsg = FALSE THEN DO:
            IF lDrivmedelInne = TRUE AND lVarorInne = TRUE THEN
                iAntBlandet = 1.
            ELSE IF lDrivmedelInne = TRUE THEN
                iAntKunDrivstoff = 1.
            ELSE IF lVarorInne     = TRUE THEN
                iAntKunVare = 1.
        END.
        FIND FIRST TT_prBSData WHERE TT_prBSData.butikknr = Bonghode.butikknr AND 
                                     TT_prBSData.dato     = Bonghode.dato    NO-ERROR.
      IF NOT AVAIL TT_prBSData THEN DO:
        CREATE TT_prBSData.
        ASSIGN TT_prBSData.butikknr = Bonghode.butikknr
               TT_prBSData.dato     = Bonghode.dato
               TT_prBSData.datotid  = deDatoTid.
      END.
      ASSIGN TT_prBSData.AntBlandet      = TT_prBSData.AntBlandet      + iAntBlandet
             TT_prBSData.AntKunDrivstoff = TT_prBSData.AntKunDrivstoff + iAntKunDrivstoff
             TT_prBSData.AntKunVare      = TT_prBSData.AntKunVare      + iAntKunVare.
      RUN Update_TT_PrBS.
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
    ASSIGN dMvaKr    = 0
           dMvaDrivm = 0
           dSalgssum = 0
           dSalgssumtmp = 0
           iKontantant = 0
           dKontantkr  = 0
           iBankAnt    = 0
           dBankkr     = 0
           dAvrundKr   = 0
           iAvrundAnt  = 0
           dKreditkr   = 0
           iKreditant  = 0
           dGavekortIn = 0
           iGavekortInAnt = 0
           dCashBack      = 0
           iCashBackAnt   = 0
           dDriveOff      = 0
           iDriveOffAnt   = 0
           dInOutKr       = 0
           iInOutAnt      = 0
           lHarSalg     = FALSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-OppdaterPrData) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OppdaterPrData Procedure 
PROCEDURE OppdaterPrData :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE iButikkNr AS INTEGER     NO-UNDO.
    DEFINE VARIABLE ii AS INTEGER     NO-UNDO.
    DEFINE VARIABLE dDato AS DATE        NO-UNDO.
    DEFINE BUFFER bufPRFS FOR prFSData.
    PRBSDATA:
    FOR EACH TT_prBSData:
        FIND FIRST prBSData WHERE prBSData.butikknr  = TT_prBSData.butikknr AND 
                                  prBSData.dato      = TT_prBSData.dato     NO-ERROR.
        IF NOT AVAIL prBSData THEN DO:
            CREATE prBSData.
            ASSIGN prBSData.butikknr  = TT_prBSData.butikknr
                   prBSData.dato      = TT_prBSData.dato NO-ERROR.
            IF ERROR-STATUS:ERROR THEN DO:
                DELETE prBSdata.
                NEXT PRBSDATA.
            END.
        END.
        ASSIGN prBSData.AntKvittoInne = prBSData.AntKvittoInne + TT_prBSData.AntKvittoInne
               prBSData.AntKvittoUte  = prBSData.AntKvittoUte  + TT_prBSData.AntKvittoUte 
               prBSData.bm01          = prBSData.bm01          + TT_prBSData.bm01         
               prBSData.bm01Ant       = prBSData.bm01Ant       + TT_prBSData.bm01Ant      
               prBSData.bm02          = prBSData.bm02          + TT_prBSData.bm02         
               prBSData.bm02ant       = prBSData.bm02ant       + TT_prBSData.bm02ant      
               prBSData.bm03          = prBSData.bm03          + TT_prBSData.bm03         
               prBSData.bm03ant       = prBSData.bm03ant       + TT_prBSData.bm03ant      
               prBSData.bm04          = prBSData.bm04          + TT_prBSData.bm04         
               prBSData.bm04and       = prBSData.bm04and       + TT_prBSData.bm04and      
               prBSData.bm05          = prBSData.bm05          + TT_prBSData.bm05         
               prBSData.bm05ant       = prBSData.bm05ant       + TT_prBSData.bm05ant      
               prBSData.bm06          = prBSData.bm06          + TT_prBSData.bm06         
               prBSData.bm06ant       = prBSData.bm06ant       + TT_prBSData.bm06ant      
               prBSData.bm07          = prBSData.bm07          + TT_prBSData.bm07         
               prBSData.bm07ant       = prBSData.bm07ant       + TT_prBSData.bm07ant      
               prBSData.bm08          = prBSData.bm08          + TT_prBSData.bm08         
               prBSData.bm08ant       = prBSData.bm08ant       + TT_prBSData.bm08ant      
               prBSData.bm09          = prBSData.bm09          + TT_prBSData.bm09         
               prBSData.bm09ant       = prBSData.bm09ant       + TT_prBSData.bm09ant      
               prBSData.bm10          = prBSData.bm10          + TT_prBSData.bm10         
               prBSData.bm10ant       = prBSData.bm10ant       + TT_prBSData.bm10ant      
               prBSData.bm11          = prBSData.bm11          + TT_prBSData.bm11         
               prBSData.bm11ant       = prBSData.bm11ant       + TT_prBSData.bm11ant      
               prBSData.bm12          = prBSData.bm12          + TT_prBSData.bm12         
               prBSData.bm12ant       = prBSData.bm12ant       + TT_prBSData.bm12ant      
               prBSData.bm13          = prBSData.bm13          + TT_prBSData.bm13         
               prBSData.bm13ant       = prBSData.bm13ant       + TT_prBSData.bm13ant      
               prBSData.bm14          = prBSData.bm14          + TT_prBSData.bm14         
               prBSData.bm14ant       = prBSData.bm14ant       + TT_prBSData.bm14ant      
               prBSData.bm15          = prBSData.bm15          + TT_prBSData.bm15         
               prBSData.bm15ant       = prBSData.bm15ant       + TT_prBSData.bm15ant      
               prBSData.bm16          = prBSData.bm16          + TT_prBSData.bm16         
               prBSData.bm16ant       = prBSData.bm16ant       + TT_prBSData.bm16ant      
               prBSData.bm17          = prBSData.bm17          + TT_prBSData.bm17         
               prBSData.bm17ant       = prBSData.bm17ant       + TT_prBSData.bm17ant      
               prBSData.bm18          = prBSData.bm18          + TT_prBSData.bm18         
               prBSData.bm18ant       = prBSData.bm18ant       + TT_prBSData.bm18ant      
               prBSData.bm19          = prBSData.bm19          + TT_prBSData.bm19         
               prBSData.bm19ant       = prBSData.bm19ant       + TT_prBSData.bm19ant      
               prBSData.bm20          = prBSData.bm20          + TT_prBSData.bm20         
               prBSData.bm20ant       = prBSData.bm20ant       + TT_prBSData.bm20ant      
               prBSData.DrivmInne     = prBSData.DrivmInne     + TT_prBSData.DrivmInne    
               prBSData.MvaDrm        = prBSData.MvaDrm        + TT_prBSData.MvaDrm       
               prBSData.MvaKr         = prBSData.MvaKr         + TT_prBSData.MvaKr        
               prBSData.TotDrvm       = prBSData.TotDrvm       + TT_prBSData.TotDrvm      
               prBSData.TotSalg       = prBSData.TotSalg       + TT_prBSData.TotSalg      
               prBSData.AntBlandet      = prBSData.AntBlandet      + TT_prBSData.AntBlandet
               prBSData.AntKunDrivstoff = prBSData.AntKunDrivstoff + TT_prBSData.AntKunDrivstoff
               prBSData.AntKunVare      = prBSData.AntKunVare      + TT_prBSData.AntKunVare
               prBSData.datotid       = TT_prBSData.datotid.
    END.
    CHDATA:
    FOR EACH TT_prCHData:
        FIND FIRST prCHData WHERE prCHData.butikknr   = TT_prCHData.butikknr    AND 
                                  prCHData.dato       = TT_prCHData.dato        AND
                                  prCHData.hg         = TT_prCHData.hg          AND 
                                  prCHData.vg         = TT_prCHData.vg          AND 
                                  prCHData.Artikkelnr = TT_prCHData.Artikkelnr  NO-ERROR.
        IF NOT AVAIL prCHData THEN DO:
            CREATE prCHData.
            ASSIGN prCHData.butikknr   = TT_prCHData.butikknr  
                   prCHData.dato       = TT_prCHData.dato      
                   prCHData.hg         = TT_prCHData.hg        
                   prCHData.vg         = TT_prCHData.vg        
                   prCHData.artikkelnr = TT_prCHData.artikkelnr NO-ERROR.
            IF ERROR-STATUS:ERROR THEN DO:
                DELETE prCHData.
                NEXT CHDATA.
            END.
        END.
        ASSIGN prCHData.AntSolgt = prCHData.AntSolgt + TT_prCHData.AntSolgt
               prCHData.datotid  = TT_prCHData.datotid.
    END.
    FIND FIRST TT_prFSData NO-ERROR.
    IF AVAIL TT_prFSData THEN
        iButikkNr = TT_prFSData.butikkNr.
    FSDATA:
    FOR EACH TT_prFSData:
        FIND FIRST prFSData WHERE prFSData.butikknr    = TT_prFSData.butikknr    AND
                                  prFSData.dato        = TT_prFSData.dato        AND 
                                  prFSData.avdelingsnr = TT_prFSData.avdelingsnr AND
                                  prFSData.hg          = TT_prFSData.hg          AND 
                                  prFSData.vg          = TT_prFSData.vg          NO-ERROR.
        IF NOT AVAIL prFSData THEN DO:
            CREATE prFSData.
            ASSIGN prFSData.butikknr    = TT_prFSData.butikknr   
                   prFSData.dato        = TT_prFSData.dato       
                   prFSData.avdelingsnr = TT_prFSData.avdelingsnr
                   prFSData.hg          = TT_prFSData.hg          
                   prFSData.vg          = TT_prFSData.vg NO-ERROR.
            IF ERROR-STATUS:ERROR THEN DO:
                DELETE prFSData.
                NEXT FSDATA.
            END.
        END.
        ASSIGN   prFSData.AntSolgtNto           = prFSData.AntSolgtNto           + TT_prFSData.AntSolgtNto          
                 prFSData.AntSolgtPlu           = prFSData.AntSolgtPlu           + TT_prFSData.AntSolgtPlu          
                 prFSData.fsgnto                = prFSData.fsgnto                + TT_prFSData.fsgnto               
                 prFSData.dbkr                  = prFSData.dbkr                  + TT_prFSData.dbkr                 
                 prFSData.mvakr                 = prFSData.mvakr                 + TT_prFSData.mvakr                
                 prFSData.ManRabatter           = prFSData.ManRabatter           + TT_prFSData.ManRabatter          
                 prFSData.ManRabatter_vol       = prFSData.ManRabatter_vol       + TT_prFSData.ManRabatter_vol      
                 prFSData.LokKampRabatt         = prFSData.LokKampRabatt         + TT_prFSData.LokKampRabatt        
                 prFSData.LokKampRabatt_vol     = prFSData.LokKampRabatt_vol     + TT_prFSData.LokKampRabatt_vol    
                 prFSData.CentKampRabattNOK     = prFSData.CentKampRabattNOK     + TT_prFSData.CentKampRabattNOK    
                 prFSData.CentKampRabattNOK_vol = prFSData.CentKampRabattNOK_vol + TT_prFSData.CentKampRabattNOK_vol
                 prFSData.CentKampRabattOK      = prFSData.CentKampRabattOK      + TT_prFSData.CentKampRabattOK     
                 prFSData.CentKampRabattOK_vol  = prFSData.CentKampRabattOK_vol  + TT_prFSData.CentKampRabattOK_vol 
                 prFSData.datotid               = TT_prFSData.datotid.

    END.
    RELEASE prFSData.
    IF iButikkNr <> 0 THEN DO:
        DO ii = 1 TO NUM-ENTRIES(cPRFSdagar):
            dDato = DATE(ENTRY(ii,cPRFSdagar)) NO-ERROR.
            IF ERROR-STATUS:ERROR THEN
                NEXT.
            FOR EACH bufPRFS WHERE bufPRFS.butikknr = iButikkNr AND bufPRFS.dato = dDato NO-LOCK.
                IF bufPRFS.datotid <> deDatoTid THEN DO:
                    FIND prFSData WHERE ROWID(prFSData) = ROWID(bufPRFS) EXCLUSIVE NO-WAIT NO-ERROR.
                    IF AVAIL prFSData THEN
                        prFSData.datotid = deDatoTid.
                    RELEASE prFSData.
                END.
            END.
        END.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Update_TT_PrBS) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Update_TT_PrBS Procedure 
PROCEDURE Update_TT_PrBS :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    ASSIGN TT_prBSData.AntKvittoInne = TT_prBSData.AntKvittoInne + IF Bonghode.Kassenr < 11 THEN 1 ELSE 0
           TT_prBSData.AntKvittoUte  = TT_prBSData.AntKvittoUte  + IF Bonghode.Kassenr > 10 THEN 1 ELSE 0
           TT_prBSData.bm01          = TT_prBSData.bm01    + dKontantkr 
           TT_prBSData.bm01Ant       = TT_prBSData.bm01Ant + iKontantant
           TT_prBSData.bm02          = TT_prBSData.bm02    + dBankkr + (-1 * dCashBack) /* cashback är negativt och fråndraget 1 gång */
           TT_prBSData.bm02ant       = TT_prBSData.bm02ant + iBankAnt
           TT_prBSData.bm03          = TT_prBSData.bm03    + dGavekortIn    /* other credit */
           TT_prBSData.bm03ant       = TT_prBSData.bm03ant + iGavekortInAnt
           TT_prBSData.bm04          = TT_prBSData.bm04    + dDriveOff   
           TT_prBSData.bm04and       = TT_prBSData.bm04and + iDriveOffAnt
           TT_prBSData.bm05          = TT_prBSData.bm05    + dInOutKr 
           TT_prBSData.bm05ant       = TT_prBSData.bm05ant + iInOutAnt
           TT_prBSData.bm06          = TT_prBSData.bm06    + dKreditkr 
           TT_prBSData.bm06ant       = TT_prBSData.bm06ant + iKreditant
           TT_prBSData.bm07          = TT_prBSData.bm07    + dAvrundKr 
           TT_prBSData.bm07ant       = TT_prBSData.bm07ant + iAvrundAnt
/*            TT_prBSData.bm08          = TT_prBSData.bm08    + */
/*            TT_prBSData.bm08ant       = TT_prBSData.bm08ant + */
           TT_prBSData.bm09          = TT_prBSData.bm09    + dCashBack   
           TT_prBSData.bm09ant       = TT_prBSData.bm09ant + iCashBackAnt
                .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

