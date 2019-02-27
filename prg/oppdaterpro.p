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

DEFINE INPUT  PARAMETER iButikkNr AS INTEGER    NO-UNDO.
DEFINE INPUT  PARAMETER dDato     AS DATE       NO-UNDO.
DEFINE INPUT  PARAMETER lFinansPro   AS LOGICAL    NO-UNDO.
DEFINE INPUT  PARAMETER lFinansPreem AS LOGICAL    NO-UNDO.
DEFINE INPUT  PARAMETER cOutputDir     AS CHARACTER  NO-UNDO.

DEFINE VARIABLE deDatotid      AS DECIMAL    NO-UNDO.
DEFINE VARIABLE cDatoTid       AS CHARACTER  NO-UNDO.

DEFINE VARIABLE dKostPris         AS DECIMAL    NO-UNDO.
DEFINE VARIABLE dMvaKr            AS DECIMAL    NO-UNDO.
DEFINE VARIABLE dMvaDrivm         AS DECIMAL    NO-UNDO.
DEFINE VARIABLE dMvaGrpKr      AS DECIMAL EXTENT 10   NO-UNDO.
DEFINE VARIABLE dMvaGrLag      AS DECIMAL EXTENT 10   NO-UNDO.
DEFINE VARIABLE dSalgssum         AS DECIMAL    NO-UNDO.
DEFINE VARIABLE dGenrabkr         AS DECIMAL    NO-UNDO.
DEFINE VARIABLE dGenrabant        AS DECIMAL    NO-UNDO.
DEFINE VARIABLE dkunrabkr         AS DECIMAL    NO-UNDO.
DEFINE VARIABLE dmedrabkr         AS DECIMAL    NO-UNDO.
DEFINE VARIABLE dkunderabatt      AS DECIMAL    NO-UNDO.
DEFINE VARIABLE dkunderabant      AS INTEGER    NO-UNDO.
DEFINE VARIABLE dperrabkr         AS DECIMAL    NO-UNDO.
DEFINE VARIABLE iKoeff            AS INTEGER    NO-UNDO.
DEFINE VARIABLE iVareAnt          AS INTEGER    NO-UNDO.
DEFINE VARIABLE iTimme            AS INTEGER    NO-UNDO.
DEFINE VARIABLE iKontantant AS INTEGER    NO-UNDO.
DEFINE VARIABLE dKontantkr  AS DECIMAL    NO-UNDO.
DEFINE VARIABLE iBankAnt    AS INTEGER    NO-UNDO.
DEFINE VARIABLE dBankkr     AS DECIMAL    NO-UNDO.
DEFINE VARIABLE dBehBankkr  AS DECIMAL    NO-UNDO.
DEFINE VARIABLE iReturant   AS INTEGER    NO-UNDO.
DEFINE VARIABLE dReturkr    AS DECIMAL    NO-UNDO.
DEFINE VARIABLE dNegativkr  AS DECIMAL    NO-UNDO.
DEFINE VARIABLE dNegvarekr  AS DECIMAL    NO-UNDO.
DEFINE VARIABLE dRegminkr   AS DECIMAL    NO-UNDO.
DEFINE VARIABLE iRegminant  AS INTEGER    NO-UNDO.
DEFINE VARIABLE dAvrundKr   AS DECIMAL    NO-UNDO.
DEFINE VARIABLE iAvrundAnt  AS INTEGER    NO-UNDO.
DEFINE VARIABLE dTellkontkr AS DECIMAL    NO-UNDO.
DEFINE VARIABLE dKreditkr   AS DECIMAL    NO-UNDO.
DEFINE VARIABLE iKreditant  AS INTEGER    NO-UNDO.
DEFINE VARIABLE iScannant   AS INTEGER    NO-UNDO.
DEFINE VARIABLE iPluant     AS INTEGER    NO-UNDO.
DEFINE VARIABLE dGrandtot   AS DECIMAL    NO-UNDO.
DEFINE VARIABLE dVeksel     AS DECIMAL    NO-UNDO.
DEFINE VARIABLE dReservBank    AS DECIMAL    NO-UNDO.
DEFINE VARIABLE iReservBankAnt AS INTEGER    NO-UNDO.
DEFINE VARIABLE dGavekortIn AS DECIMAL    NO-UNDO.
DEFINE VARIABLE iGavekortInAnt AS INTEGER    NO-UNDO.
DEFINE VARIABLE dGavekortUt    AS DECIMAL    NO-UNDO.
DEFINE VARIABLE dGavekortUtAnt AS INTEGER   NO-UNDO.
DEFINE VARIABLE dTilGodeInn    AS DECIMAL    NO-UNDO.
DEFINE VARIABLE dTilGodeInnAnt AS INTEGER    NO-UNDO.
DEFINE VARIABLE dTilGodeUt     AS DECIMAL    NO-UNDO.
DEFINE VARIABLE dKupongkr      AS DECIMAL    NO-UNDO.
DEFINE VARIABLE dTilGodeUtAnt  AS INTEGER    NO-UNDO.
DEFINE VARIABLE dCashBack      AS DECIMAL    NO-UNDO.
DEFINE VARIABLE iCashBackAnt AS INTEGER    NO-UNDO.
DEFINE VARIABLE dDriveOff    AS DECIMAL    NO-UNDO.
DEFINE VARIABLE iDriveOffAnt AS INTEGER    NO-UNDO.
DEFINE VARIABLE dInOutKr     AS DECIMAL    NO-UNDO.
DEFINE VARIABLE iInOutAnt    AS INTEGER    NO-UNDO.
DEFINE VARIABLE dSalgssumTmp    AS DECIMAL    NO-UNDO.
DEFINE VARIABLE dRingcountant   AS INTEGER    NO-UNDO.
DEFINE VARIABLE cOrgNumFormat   AS CHARACTER  NO-UNDO.
DEFINE VARIABLE lHarSalg        AS LOGICAL    NO-UNDO.
DEFINE VARIABLE lHarMakulert    AS LOGICAL    NO-UNDO.
DEFINE VARIABLE lIkkeKunde      AS LOGICAL    NO-UNDO.
DEFINE VARIABLE lMedlemsSalg    AS LOGICAL    NO-UNDO.
DEFINE VARIABLE cTTIdString     AS CHARACTER  NO-UNDO.
DEFINE TEMP-TABLE TT_finansdag  NO-UNDO LIKE finansdag.
DEFINE TEMP-TABLE TT_hgrdag     NO-UNDO LIKE hgrdag.
DEFINE TEMP-TABLE TT_kassdag    NO-UNDO LIKE kassdag.
DEFINE TEMP-TABLE TT_timedag    NO-UNDO LIKE timedag.
DEFINE TEMP-TABLE TT_varedag    NO-UNDO LIKE varedag.
DEFINE TEMP-TABLE TT_prBSData   NO-UNDO LIKE prBSData.
DEFINE TEMP-TABLE TT_prCHData   NO-UNDO LIKE prCHData.
DEFINE TEMP-TABLE TT_prFSData   NO-UNDO LIKE prFSData.

DEFINE TEMP-TABLE TT_prBMData   NO-UNDO LIKE prBMData.  /* Betalingsmiddler                 */
DEFINE TEMP-TABLE TT_prKDData   NO-UNDO LIKE prKDData.  /* Solgte volumer drivstoff på kort */
DEFINE TEMP-TABLE TT_prKSData   NO-UNDO LIKE prKSData.  /* SUM kreditsalg (stasjonskreditt) */
DEFINE TEMP-TABLE TT_prPGData   NO-UNDO LIKE prPGData.  /* varegruppesalg                   */

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
/* IF lFinansPro = TRUE THEN DO:                                */
/*     ASSIGN cOrgNumformat = SESSION:NUMERIC-FORMAT.           */
/*            cOutputDir    = RIGHT-TRIM(cOutputDir,"\") + "\". */
/*                                                              */
/* /*     RUN EksporterTT. */                                   */
/*     SESSION:NUMERIC-FORMAT = cOrgNumformat.                  */
/* END.                                                         */
IF lFinansPreem = TRUE THEN DO:
    RUN OppdaterPrData.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-EksporterTT) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE EksporterTT Procedure 
PROCEDURE EksporterTT :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE cFilNavn       AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cFilNavnTMP    AS CHARACTER  NO-UNDO.

SESSION:NUMERIC-FORMAT = "American".

/* hgrdag */
ASSIGN cFilNavn    = cOutputdir + "hgrdag." + STRING(iButikkNr)
       cFilNavnTMP = cFilNavn + "TMP".
IF SEARCH(cFilNavn) <> ? THEN
    OS-RENAME VALUE(cFilNavn) VALUE(cFilNavnTMP).
OUTPUT TO VALUE(cFilNavnTMP) APPEND.
FOR EACH TT_hgrdag:
    EXPORT TT_hgrdag.
END.
OUTPUT CLOSE.
OS-RENAME VALUE(cFilNavnTMP) VALUE(cFilNavn).
/* varedag */
ASSIGN cFilNavn    = cOutputdir + "varesalg." + STRING(iButikkNr)
       cFilNavnTMP = cFilNavn + "TMP".
IF SEARCH(cFilNavn) <> ? THEN
    OS-RENAME VALUE(cFilNavn) VALUE(cFilNavnTMP).
OUTPUT TO VALUE(cFilNavnTMP) APPEND.
FOR EACH TT_varedag:
    EXPORT TT_varedag.
END.
OUTPUT CLOSE.
OS-RENAME VALUE(cFilNavnTMP) VALUE(cFilNavn).

/* timedag */
ASSIGN cFilNavn    = cOutputdir + "timedag." + STRING(iButikkNr)
       cFilNavnTMP = cFilNavn + "TMP".
IF SEARCH(cFilNavn) <> ? THEN
    OS-RENAME VALUE(cFilNavn) VALUE(cFilNavnTMP).
OUTPUT TO VALUE(cFilNavnTMP) APPEND.
FOR EACH TT_timedag:
    EXPORT TT_timedag.
END.
OUTPUT CLOSE.
OS-RENAME VALUE(cFilNavnTMP) VALUE(cFilNavn).

ASSIGN cFilNavn    = cOutputdir + "finansda." + STRING(iButikkNr)
       cFilNavnTMP = cFilNavn + "TMP".
IF SEARCH(cFilNavn) <> ? THEN
    OS-RENAME VALUE(cFilNavn) VALUE(cFilNavnTMP).
OUTPUT TO VALUE(cFilNavnTMP) APPEND.
FOR EACH TT_finansdag:
    EXPORT TT_finansdag.
END.
OUTPUT CLOSE.
OS-RENAME VALUE(cFilNavnTMP) VALUE(cFilNavn).

ASSIGN cFilNavn    = cOutputdir + "kassdag." + STRING(iButikkNr)
       cFilNavnTMP = cFilNavn + "TMP".
IF SEARCH(cFilNavn) <> ? THEN
    OS-RENAME VALUE(cFilNavn) VALUE(cFilNavnTMP).
OUTPUT TO VALUE(cFilNavnTMP) APPEND.
FOR EACH TT_kassdag:
    EXPORT TT_kassdag.
END.
OUTPUT CLOSE.
OS-RENAME VALUE(cFilNavnTMP) VALUE(cFilNavn).

/* hgrdag.    */
/* varedag.   */
/* timedag.   */
/* kassedag.  */
/* finansdag. */
/*            */
/* OS-RENAME  */

SESSION:NUMERIC-FORMAT = cOrgNumformat.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

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
FOR EACH Kasse WHERE Kasse.ButikkNr = iButikkNr AND Kasse.GruppeNr = 1 NO-LOCK:
  FOR EACH BongHode WHERE BongHode.ButikkNr = Kasse.ButikkNr AND BongHode.GruppeNr = Kasse.GruppeNr AND
                          BongHode.KasseNr  = Kasse.KasseNr  AND BongHode.Dato     = dDato          NO-LOCK:
    IF BongHode.Makulert = 2 THEN DO:
      NEXT.
    END.
    RUN NullStillVars.
    FOR EACH Bonglinje WHERE bonglinje.b_id = bonghode.b_id NO-LOCK:
      IF CAN-DO("1,3,10",STRING(BongLinje.TTId)) AND BongLinje.Antall <> 0 AND Bonglinje.Makulert = FALSE THEN SALG: DO:
        FIND Artbas WHERE ArtBas.ArtikkelNr = DECI(Bonglinje.Artikkelnr) NO-LOCK NO-ERROR.
        FIND VarGr OF Artbas NO-LOCK NO-ERROR.
        IF AVAIL VarGr THEN
            FIND HuvGr OF VarGr NO-LOCK NO-ERROR.
        ELSE
            RELEASE HuvGr.
        ASSIGN 
          iKoeff       = IF BongLinje.Antall > 0 THEN 1 ELSE -1
          iMvaGr       = IF BongLinje.MvaGr = 0 OR BongLinje.MvaGr > 9 THEN 9 ELSE BongLinje.MvaGr
          dSalgssumTmp = BongLinje.LinjeSum - BongLinje.Linjerab - BongLinje.SubtotalRab
          dKostPris = dKostPris + iKoeff * BongLinje.VVarekost
          dMvaKr    = dMvaKr    + iKoeff * BongLinje.MvaKr
          dMvaGrpKr[iMvaGr] = dMvaGrpKr[iMvaGr] + iKoeff * BongLinje.MvaKr
          dMvaGrLag[iMvaGr] = dMvaGrLag[iMvaGr] + (iKoeff * dSalgssumTmp) - iKoeff * BongLinje.MvaKr
          dSalgssum = dSalgssum + (iKoeff * dSalgssumTmp)
          dGenrabkr = dGenrabkr + (iKoeff * (BongLinje.SubtotalRab + Bonglinje.LinjeRab))
          dGenrabant = dGenrabant + (IF BongLinje.SubtotalRab + Bonglinje.LinjeRab - bonglinje.kunderabatt <> 0 THEN iKoeff ELSE 0)
          dkunderabatt = dkunderabatt + (iKoeff * bonglinje.kunderabatt)
          dkunderabant = dkunderabant + (IF bonglinje.kunderabatt <> 0 THEN iKoeff ELSE 0)
          dmedrabkr = dmedrabkr + iKoeff * BongLinje.Medlemsrabatt
          dperrabkr = dperrabkr + iKoeff * BongLinje.Personalrabatt
          iVareAnt  = iVareAnt  + (IF ROUND(BongLinje.Antall,0) = BongLinje.Antall THEN BongLinje.Antall ELSE iKoeff * 1)
          dNegativkr = dNegativkr + (IF bonglinje.ttid = 3 THEN dSalgsSum ELSE 0)
          iReturant  = iReturant  + (IF bonglinje.ttid = 3 OR bonglinje.ttid = 10 THEN ABS(BongLinje.Antall) ELSE 0)
          dReturkr   = dReturkr   + (IF bonglinje.ttid = 3 OR bonglinje.ttid = 10 THEN dSalgsSumTmp ELSE 0)
          dNegvarekr = dNegvarekr + (IF bonglinje.ttid = 3 OR bonglinje.ttid = 10 THEN iKoeff * dSalgsSumTmp ELSE 0)
          dRegminkr  = dRegminkr  + (IF bonglinje.ttid = 3 OR bonglinje.ttid = 10 THEN dSalgsSumTmp ELSE 0)
          iRegminant = iRegminant + (IF bonglinje.ttid = 3 OR bonglinje.ttid = 10 THEN ABS(iVareAnt) ELSE 0)
          iScannant  = iScannant + IF AVAIL Artbas AND ArtBas.Opris = FALSE THEN ABS((IF ROUND(BongLinje.Antall,0) = BongLinje.Antall THEN BongLinje.Antall ELSE iKoeff * 1)) ELSE 0
          iPluAnt    = iPluAnt   + IF AVAIL Artbas AND ArtBas.Opris = TRUE  THEN ABS((IF ROUND(BongLinje.Antall,0) = BongLinje.Antall THEN BongLinje.Antall ELSE iKoeff * 1)) ELSE 0
          lHarSalg  = TRUE.
        prBSData:
        DO:
/*           FIND FIRST TT_prBSData NO-ERROR.                                                                                                                           */
/*           IF NOT AVAIL TT_prBSData THEN DO:                                                                                                                          */
/*             CREATE TT_prBSData.                                                                                                                                      */
/*             ASSIGN TT_prBSData.butikknr = iButikkNr                                                                                                                  */
/*                    TT_prBSData.dato     = dDato                                                                                                                      */
/*                    TT_prBSData.datotid  = deDatoTid.                                                                                                                 */
/*           END.                                                                                                                                                       */
/*           ASSIGN                                                                                                                                                     */
/*             TT_prBSData.TotSalg  = TT_prBSData.TotSalg + (iKoeff * dSalgssumTmp)                                                                                     */
/*             TT_prBSData.MvaKr    = TT_prBSData.MvaKr   + (iKoeff * BongLinje.MvaKr)                                                                                  */
/*             TT_prBSData.TotDrvm  = TT_prBSData.TotDrvm + (IF AVAIL HuvGr AND HuvGr.avdelingNr = 1 THEN (iKoeff * dSalgssumTmp) ELSE 0)                               */
/*             TT_prBSData.DrivmInne = TT_prBSData.DrivmInne + (IF BongLinje.KasseNr < 11 AND AVAIL HuvGr AND HuvGr.avdelingNr = 1 THEN (iKoeff * dSalgssumTmp) ELSE 0) */
/*             TT_prBSData.MvaDrm    = TT_prBSData.MvaDrm + (IF AVAIL HuvGr AND HuvGr.avdelingNr = 1 THEN iKoeff * BongLinje.MvaKr ELSE 0).                             */
        END.
/*         IF BongLinje.HovedGr = 44 THEN prCHData: DO:                                                                           */
/*           FIND FIRST TT_prCHData WHERE TT_prCHData.butikknr   = Bonglinje.ButikkNr AND TT_prCHData.dato = Bonglinje.dato AND   */
/*                                        TT_prCHData.hg         = Bonglinje.HovedGr  AND TT_prCHData.vg   = Bonglinje.VareGr AND */
/*                                        TT_prCHData.Artikkelnr = Bonglinje.ArtikkelNr NO-ERROR.                                 */
/*           IF NOT AVAIL TT_prCHData THEN DO:                                                                                    */
/*             CREATE TT_prCHData.                                                                                                */
/*             ASSIGN TT_prCHData.butikknr = Bonglinje.ButikkNr                                                                   */
/*                    TT_prCHData.dato     = Bonglinje.dato                                                                       */
/*                    TT_prCHData.hg       = Bonglinje.HovedGr                                                                    */
/*                    TT_prCHData.vg       = Bonglinje.VareGr                                                                     */
/*                    TT_prCHData.artikkelnr = Bonglinje.ArtikkelNr                                                               */
/*                    TT_prCHData.datotid  = deDatoTid NO-ERROR.                                                                  */
/*             IF ERROR-STATUS:ERROR THEN DO:                                                                                     */
/*               DELETE TT_prCHData.                                                                                              */
/*               LEAVE prCHData.                                                                                                  */
/*             END.                                                                                                               */
/*           END.                                                                                                                 */
/*           ASSIGN TT_prCHData.AntSolgt  = TT_prCHData.AntSolgt + BongLinje.Antall.                                              */
/*         END.                                                                                                                   */
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
                      TT_prFSData.dato = Bonglinje.dato AND TT_prFSData.avdelingsnr = HuvGr.Avdelingnr AND
                      TT_prFSData.hg   = Bonglinje.HovedGr AND TT_prFSData.vg = Bonglinje.VareGr  NO-ERROR.
           IF NOT AVAIL TT_prFSData THEN DO:
               CREATE TT_prFSData.
               ASSIGN TT_prFSData.butikknr    = Bonglinje.ButikkNr
                      TT_prFSData.dato        = Bonglinje.dato   
                      TT_prFSData.avdelingsnr = HuvGr.Avdelingnr
                      TT_prFSData.hg          = Bonglinje.HovedGr  
                      TT_prFSData.vg          = Bonglinje.VareGr
                      TT_prFSData.datotid     = deDatoTid.
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
/*       IF bonglinje.TTId = 1 AND bonglinje.makulert = TRUE THEN                                                 */
/*           ASSIGN lHarMakulert = TRUE.                                                                          */
/*       IF CAN-DO("92,93,96,97,150",STRING(bonglinje.TTId)) THEN                                                 */
/*           ASSIGN lIkkeKunde = TRUE.                                                                            */
/*       IF bonglinje.TTId = 94 THEN                                                                              */
/*           ASSIGN lMedlemsSalg = TRUE.                                                                          */
/* /* vid uppdat av finansdag och cTTIdString bara innehåller "95" uppdateras inte btovareknd */                  */
/*       IF NOT CAN-DO(cTTIdString,STRING(bonglinje.TTId)) THEN                                                   */
/*           ASSIGN cTTIdString = cTTIdString + (IF cTTIdString <> "" THEN "," ELSE "") + STRING(bonglinje.TTId). */
/*       IF BongLinje.Makulert = FALSE THEN                                                                       */
/*       ASSIGN                                                                                                   */
/*        dRingcountant = dRingcountant + IF bongLinje.TTId = 93 THEN BongLinje.SubtotalRab ELSE 0                */
/*        dKreditkr   = dKreditkr   + IF bongLinje.TTId = 65 THEN BongLinje.linjesum ELSE 0                       */
/*        iKreditant  = iKreditant  + IF bongLinje.TTId = 65 THEN 1 ELSE 0                                        */
/*        iKontantant = iKontantant + IF bongLinje.TTId = 50 THEN IF bonglinje.linjesum < 0 THEN -1 ELSE 1 ELSE 0 */
/*        dKontantkr  = dKontantkr  + IF bongLinje.TTId = 50 THEN BongLinje.linjesum ELSE 0                       */
/*        dKontantkr  = dKontantkr  + IF bongLinje.TTId = 70 THEN BongLinje.linjesum ELSE 0                       */
/*        dVeksel     = dVeksel     + IF bongLinje.TTId = 70 THEN BongLinje.linjesum ELSE 0                       */
/*        dTellkontkr = dTellkontkr + IF bongLinje.TTId = 150 THEN BongLinje.linjesum ELSE 0                      */
/*        dBankkr     = dBankkr   + IF bongLinje.TTId = 52 OR bongLinje.TTId = 58 THEN Bonglinje.LinjeSum ELSE 0  */
/*        iBankAnt    = iBankAnt  + IF bongLinje.TTId = 52 OR bongLinje.TTId = 58 THEN 1 ELSE 0                   */
/*        dBankkr     = dBankkr   + IF bongLinje.TTId = 67 THEN Bonglinje.LinjeSum ELSE 0                         */
/*        dKupongkr   = dKupongkr + IF bongLinje.TTId = 56 OR bongLinje.TTId = 72 THEN Bonglinje.LinjeSum ELSE 0  */
/*        dCashBack   = dCashBack + IF bongLinje.TTId = 67 THEN Bonglinje.LinjeSum ELSE 0                         */
/*        iCashBackAnt = iCashBackAnt + IF bongLinje.TTId = 67 THEN 1 ELSE 0                                      */
/*        dDriveOff    = dDriveOff    + IF bongLinje.TTId = 54 THEN Bonglinje.LinjeSum ELSE 0                     */
/*        iDriveOffAnt = iDriveOffAnt + IF bongLinje.TTId = 54 THEN 1 ELSE 0                                      */
/*        dBehBankkr  = dBehBankkr + IF bongLinje.TTId = 58 THEN Bonglinje.LinjeSum ELSE 0                        */
/*        dAvrundKr   = dAvrundKr  + IF bonglinje.TTId = 78 THEN Bonglinje.LinjeSum ELSE 0                        */
/*        iAvrundAnt  = iAvrundAnt + IF bongLinje.TTId = 78 THEN 1 ELSE 0                                         */
/*        dGrandtot   = dGrandtot  + IF bonglinje.TTId = 93 THEN Bonglinje.LinjeSum ELSE 0                        */
/*        dReservBank    = dReservBank    + IF bonglinje.TTId = 79 THEN Bonglinje.LinjeSum ELSE 0                 */
/*        iReservBankAnt = iReservBankAnt + IF bonglinje.TTId = 79 THEN 1 ELSE 0                                  */
/*        dGavekortIn    = dGavekortIn    + IF bonglinje.TTId = 53 THEN Bonglinje.LinjeSum ELSE 0                 */
/*        iGavekortInAnt = iGavekortInAnt + IF bonglinje.TTId = 53 THEN 1 ELSE 0                                  */
/*        dGavekortUt    = dGavekortUt    + IF bonglinje.TTId = 134 THEN Bonglinje.LinjeSum ELSE 0                */
/*        dGavekortUtAnt = dGavekortUtAnt + IF bonglinje.TTId = 134 THEN Bonglinje.Antall ELSE 0                  */
/*        dTilGodeInn    = dTilGodeInn + IF bonglinje.TTId = 66 THEN Bonglinje.LinjeSum ELSE 0                    */
/*        dTilGodeInnAnt = dTilGodeInnAnt + IF bonglinje.TTId = 66 THEN 1 ELSE 0                                  */
/*        dTilGodeUt     = dTilGodeUt    + IF bonglinje.TTId = 69 THEN -1 * Bonglinje.LinjeSum ELSE 0             */
/*        dTilGodeUtAnt  = dTilGodeUtAnt + IF bonglinje.TTId = 69 THEN 1 ELSE 0                                   */
/*        dInOutKr  = dInOutKr  + IF bonglinje.TTId = 59 THEN -1 * Bonglinje.LinjeSum ELSE 0                      */
/*        iInOutAnt = iInOutAnt + IF bonglinje.TTId = 59 THEN 1 ELSE 0                                            */
/*        dInOutKr  = dInOutKr  + IF bonglinje.TTId = 61 THEN Bonglinje.LinjeSum ELSE 0                           */
/*        iInOutAnt = iInOutAnt + IF bonglinje.TTId = 61 THEN 1 ELSE 0                                            */
/*        dInOutKr  = dInOutKr  + IF bonglinje.TTId = 62 THEN -1 * Bonglinje.LinjeSum ELSE 0                      */
/*        iInOutAnt = iInOutAnt + IF bonglinje.TTId = 62 THEN 1 ELSE 0.                                           */
    END.
/*     IF lHarSalg = TRUE THEN prBSData2: DO:       */
/*       FIND FIRST TT_prBSData NO-ERROR.           */
/*       IF NOT AVAIL TT_prBSData THEN DO:          */
/*         CREATE TT_prBSData.                      */
/*         ASSIGN TT_prBSData.butikknr = iButikkNr  */
/*                TT_prBSData.dato     = dDato      */
/*                TT_prBSData.datotid  = deDatoTid. */
/*       END.                                       */
/*       RUN Update_TT_PrBS.                        */
/*     END.                                         */
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
    ASSIGN dKostPris = 0
           dMvaKr    = 0
           dMvaDrivm = 0
           dMvaGrpKr  = 0
           dMvaGrLag = 0
           dSalgssum = 0
           dSalgssumtmp = 0
           dGenrabkr = 0
           dGenrabant = 0
           dkunderabatt = 0
           dkunderabant = 0
           dmedrabkr = 0
           dperrabkr = 0
           iVareAnt  = 0
           iKontantant = 0
           dKontantkr  = 0
           iBankAnt    = 0
           dBankkr     = 0
           dBehBankkr  = 0
           iReturant   = 0
           dReturkr    = 0
           dNegativkr  = 0
           dNegvarekr  = 0
           dRegminkr   = 0
           iRegminant  = 0
           dAvrundKr   = 0
           iAvrundAnt  = 0
           dTellkontkr = 0
           dKreditkr   = 0
           iKreditant  = 0
           iScannant   = 0
           iPluant     = 0
           dGrandtot   = 0
           dVeksel     = 0
           dReservBank    = 0
           iReservBankAnt = 0
           dGavekortIn = 0
           iGavekortInAnt = 0
           dGavekortUt    = 0
           dGavekortUtAnt = 0
           dTilGodeInn    = 0
           dTilGodeInnAnt = 0
           dTilGodeUt     = 0
           dTilGodeUtAnt  = 0
           dCashBack      = 0
           iCashBackAnt   = 0
           dDriveOff      = 0
           iDriveOffAnt   = 0
           dRingcountant  = 0
           dInOutKr       = 0
           iInOutAnt      = 0
           dKupongkr      = 0
           lMedlemsSalg   = FALSE
           lHarSalg     = FALSE
           lHarMakulert = FALSE
           lIkkeKunde   = FALSE
           cTTIdString  = "".

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

/*     IF can-find(FIRST prBSData WHERE prBSData.ButikkNr = iButikkNr AND prBSData.Dato = dDato) THEN DO: */
/*         FOR EACH prBSData WHERE prBSData.ButikkNr = iButikkNr AND prBSData.Dato = dDato:               */
/*             DELETE prBSData.                                                                           */
/*         END.                                                                                           */
/*     END.                                                                                               */
/*     IF can-find(FIRST prCHData WHERE prCHData.ButikkNr = iButikkNr AND prCHData.Dato = dDato) THEN DO: */
/*         FOR EACH prCHData WHERE prCHData.ButikkNr = iButikkNr AND prCHData.Dato = dDato:               */
/*             DELETE prCHData.                                                                           */
/*         END.                                                                                           */
/*     END.                                                                                               */
    IF can-find(FIRST prFSData WHERE prFSData.ButikkNr = iButikkNr AND prFSData.Dato = dDato) THEN DO:
        FOR EACH prFSData WHERE prFSData.ButikkNr = iButikkNr AND prFSData.Dato = dDato:
            DELETE prFSData.
        END.
    END.
/*     FOR EACH TT_prBSData:                             */
/*         CREATE prBSData.                              */
/*         BUFFER-COPY TT_prBSData TO prBSData NO-ERROR. */
/*         IF ERROR-STATUS:ERROR THEN                    */
/*             DELETE prBSData.                          */
/*     END.                                              */
/*     FOR EACH TT_prCHData:                             */
/*         CREATE prCHData.                              */
/*         BUFFER-COPY TT_prCHData TO prCHData NO-ERROR. */
/*         IF ERROR-STATUS:ERROR THEN                    */
/*             DELETE prCHData.                          */
/*     END.                                              */
    FOR EACH TT_prFSData:
        CREATE prFSData.
        BUFFER-COPY TT_prFSData TO prFSData NO-ERROR.
        IF ERROR-STATUS:ERROR THEN
            DELETE prFSData.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Update_TT_Finansdag) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Update_TT_Finansdag Procedure 
PROCEDURE Update_TT_Finansdag :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
ASSIGN TT_finansdag.btovarerkr = TT_finansdag.btovarerkr + dSalgssum
       TT_finansdag.btovareant = TT_finansdag.btovareant + iVareAnt
       TT_finansdag.btovareknd = TT_finansdag.btovareknd + IF (lHarSalg = FALSE AND lIkkeKunde = TRUE) OR cTTIdString = "95" THEN 0 ELSE 1
       TT_finansdag.Kontantant = TT_finansdag.Kontantant + iKontantant    
       TT_finansdag.Kontantkr  = TT_finansdag.Kontantkr  + dKontantkr
       TT_finansdag.Kontantkr  = TT_finansdag.Kontantkr  - (IF dGavekortIn > 0 AND dGavekortIn > dSalgssum THEN dVeksel ELSE 0)
       TT_finansdag.Kreditkr   = TT_finansdag.Kreditkr   + dKreditkr 
       TT_finansdag.Kreditant  = TT_finansdag.Kreditant  + iKreditant
       TT_finansdag.Avrundkr   = TT_finansdag.avrundkr   + dAvrundkr
       TT_finansdag.Tellkontkr = TT_finansdag.Tellkontkr + dTellkontkr
       TT_finansdag.Varekr     = TT_finansdag.Varekr     + dSalgsSum + dReturkr
       TT_finansdag.bankant    = TT_finansdag.bankant    + iBankAnt       
       TT_finansdag.bankkr     = TT_finansdag.bankkr     + dBankkr
       TT_finansdag.BehBankkr  = TT_finansdag.BehBankkr  + dBehBankkr
       TT_finansdag.BehSjekk   = TT_finansdag.BehSjekk   + dDriveOff
       TT_finansdag.Negativkr  = TT_finansdag.Negativkr + IF lHarSalg = TRUE AND bonghode.belop < 0 THEN bonghode.belop - dAvrundkr ELSE 0
       TT_finansdag.Returant   = TT_finansdag.Returant  + iReturant 
       TT_finansdag.Returkr    = TT_finansdag.Returkr   + dReturkr  
       TT_finansdag.Negvarekr  = TT_finansdag.Negvarekr + dNegvarekr
       TT_finansdag.kupongkr   = TT_finansdag.kupongkr + dKupongkr
       TT_finansdag.nettokr    = TT_finansdag.nettokr   + dSalgssum
       TT_finansdag.bruttokr   = TT_finansdag.bruttokr  + dSalgssum
       TT_finansdag.ikkekredkr = TT_finansdag.ikkekredkr + dSalgssum - dKreditkr - dAvrundkr
       TT_finansdag.behkontkr = TT_finansdag.behkontkr + dKontantkr - ABS(dCashBack)
       TT_finansdag.behkredkr = TT_finansdag.behkredkr + dKreditkr
       TT_finansdag.behmed3kr  = TT_finansdag.behmed3kr + dReservBank
       TT_finansdag.media3kr   = TT_finansdag.media3kr  + dReservBank   
       TT_finansdag.media3ant  = TT_finansdag.media3ant + iReservBankAnt
       TT_finansdag.behmed4kr  = TT_finansdag.behmed4kr + dGavekortIn
       TT_finansdag.media4kr   = TT_finansdag.media4kr  + dGavekortIn   
       TT_finansdag.media4kr   = TT_finansdag.media4kr  + (IF dGavekortIn > 0 AND dGavekortIn > dSalgssum THEN dVeksel - dTilGodeUt ELSE 0)
       TT_finansdag.media4ant  = TT_finansdag.media4ant + iGavekortInAnt
       TT_finansdag.Regminkr   = TT_finansdag.Regminkr  + dRegminkr 
       TT_finansdag.Regminant  = TT_finansdag.Regminant + iRegminant
       TT_finansdag.kortkr[1]  = TT_finansdag.kortkr[1]  + dBehBankkr
       TT_finansdag.kortant[1]  = TT_finansdag.kortant[1]  + iBankant
       TT_finansdag.mvagrlag[1]  = TT_finansdag.mvagrlag[1] + dMvaGrLag[1]
       TT_finansdag.mvagrlag[2]  = TT_finansdag.mvagrlag[2] + dMvaGrLag[2]
       TT_finansdag.mvagrlag[3]  = TT_finansdag.mvagrlag[3] + dMvaGrLag[3]
       TT_finansdag.mvagrlag[4]  = TT_finansdag.mvagrlag[4] + dMvaGrLag[4]
       TT_finansdag.mvagrlag[5]  = TT_finansdag.mvagrlag[5] + dMvaGrLag[5]
       TT_finansdag.mvagrlag[6]  = TT_finansdag.mvagrlag[6] + dMvaGrLag[6]
       TT_finansdag.mvagrlag[7]  = TT_finansdag.mvagrlag[7] + dMvaGrLag[7]
       TT_finansdag.mvagrlag[8]  = TT_finansdag.mvagrlag[8] + dMvaGrLag[8]
       TT_finansdag.mvagrlag[9]  = TT_finansdag.mvagrlag[9] + dMvaGrLag[9]
       TT_finansdag.mvakr[1]     = TT_finansdag.mvakr[1]    + dMvaGrpKr[1]
       TT_finansdag.mvakr[2]     = TT_finansdag.mvakr[2]    + dMvaGrpKr[2]
       TT_finansdag.mvakr[3]     = TT_finansdag.mvakr[3]    + dMvaGrpKr[3]
       TT_finansdag.mvakr[4]     = TT_finansdag.mvakr[4]    + dMvaGrpKr[4]
       TT_finansdag.mvakr[5]     = TT_finansdag.mvakr[5]    + dMvaGrpKr[5]
       TT_finansdag.mvakr[6]     = TT_finansdag.mvakr[6]    + dMvaGrpKr[6]
       TT_finansdag.mvakr[7]     = TT_finansdag.mvakr[7]    + dMvaGrpKr[7]
       TT_finansdag.mvakr[8]     = TT_finansdag.mvakr[8]    + dMvaGrpKr[8]
       TT_finansdag.mvakr[9]     = TT_finansdag.mvakr[9]    + dMvaGrpKr[9].
ASSIGN
       TT_finansdag.rabattkr[2]  = TT_finansdag.rabattkr[2]  + dkunderabatt
       TT_finansdag.rabattant[2] = TT_finansdag.rabattant[2] + dkunderabant
       TT_finansdag.rabattkr[4]  = TT_finansdag.rabattkr[4]  + dGenrabkr - dkunderabatt
       TT_finansdag.rabattant[4] = TT_finansdag.rabattant[4] + dGenrabant
       TT_finansdag.gavekortutkr  = TT_finansdag.gavekortutkr  + dGavekortUt
       TT_finansdag.gavekortutant = TT_finansdag.gavekortutant + dGavekortUtAnt
       TT_finansdag.tilgodeinnkr  = TT_finansdag.tilgodeinnkr  + dTilGodeInn - (IF bonghode.belop >= 0 AND dTilGodeInn = 0 
                                                                                THEN 0 ELSE dTilGodeUt) - 
                                                        (IF dTilGodeInn > 0 AND dTilGodeInn > Bonghode.belop THEN (ABS(dKontantkr) + ABS(dVeksel)) ELSE 0)
       TT_finansdag.tilgodeinnant = TT_finansdag.tilgodeinnant + dTilGodeInnAnt
       TT_finansdag.tilgodeutkr   = TT_finansdag.tilgodeutkr  + dTilGodeUt
       TT_finansdag.tilgodeutant  = TT_finansdag.tilgodeutant + dTilGodeUtAnt
       TT_finansdag.behtilgodekr  = TT_finansdag.behtilgodekr  + dTilGodeInn
       TT_finansdag.nullant       = TT_finansdag.nullant + IF lHarSalg = FALSE AND lHarMakulert = TRUE THEN 1 ELSE 0
       TT_finansdag.medant[2]     = TT_finansdag.medant[2] + IF lMedlemsSalg = TRUE THEN 1 ELSE 0
       TT_finansdag.medkr[2]     = TT_finansdag.medkr[2] + IF lMedlemsSalg = TRUE THEN dSalgsSum + dReturkr ELSE 0
       TT_finansdag.medmva[1]     = TT_finansdag.medmva[1] + IF lMedlemsSalg = FALSE THEN dMvaKr ELSE 0
       TT_finansdag.medmva[2]     = TT_finansdag.medmva[2] + IF lMedlemsSalg = TRUE THEN dMvaKr ELSE 0
       TT_finansdag.Scannant      = TT_finansdag.Scannant  + iScannant
       TT_finansdag.Pluant        = TT_finansdag.pluant    + iPluant
       TT_finansdag.paragonant    = TT_finansdag.paragonant + iPluant
       TT_finansdag.Ringcountant  = TT_finansdag.Ringcountant + dRingcountant
/*                    Grandtot   = Grandtot  + dGrandtot */
           .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Update_TT_Kassdag) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Update_TT_Kassdag Procedure 
PROCEDURE Update_TT_Kassdag :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
ASSIGN 
/*         TT_kassdag.btovarerkr = TT_kassdag.btovarerkr + dSalgssum */
TT_kassdag.vareant = TT_kassdag.vareant + iVareAnt
TT_kassdag.kundeant = TT_kassdag.kundeant + IF (lHarSalg = FALSE AND lIkkeKunde = TRUE) OR cTTIdString = "95" THEN 0 ELSE 1
TT_kassdag.Kontantant = TT_kassdag.Kontantant + iKontantant    
TT_kassdag.Kontantkr  = TT_kassdag.Kontantkr  + dKontantkr
TT_kassdag.Kontantkr  = TT_kassdag.Kontantkr  - (IF dGavekortIn > 0 AND dGavekortIn > dSalgssum THEN dVeksel ELSE 0)
TT_kassdag.Kreditkr   = TT_kassdag.Kreditkr   + dKreditkr 
TT_kassdag.Kreditant  = TT_kassdag.Kreditant  + iKreditant
/*        TT_kassdag.Avrundkr   = TT_kassdag.avrundkr   + dAvrundkr            */
/*        TT_kassdag.Tellkontkr = TT_kassdag.Tellkontkr + dTellkontkr          */
/*        TT_kassdag.Varekr     = TT_kassdag.Varekr     + dSalgsSum + dReturkr */
TT_kassdag.bankant    = TT_kassdag.bankant    + iBankAnt       
TT_kassdag.bankkr     = TT_kassdag.bankkr     + dBankkr
TT_kassdag.BehBankkr  = TT_kassdag.BehBankkr  + dBehBankkr
/*        TT_kassdag.BehSjekk   = TT_kassdag.BehSjekk   + dDriveOff                                                                       */
/*        TT_kassdag.Negativkr  = TT_kassdag.Negativkr + IF lHarSalg = TRUE AND bonghode.belop < 0 THEN bonghode.belop - dAvrundkr ELSE 0 */
TT_kassdag.Returant   = TT_kassdag.Returant  + iReturant 
TT_kassdag.Returkr    = TT_kassdag.Returkr   + dReturkr  
/*        TT_kassdag.Negvarekr  = TT_kassdag.Negvarekr + dNegvarekr */
/*        TT_kassdag.kupongkr   = TT_kassdag.kupongkr + dKupongkr   */
TT_kassdag.nettokr    = TT_kassdag.nettokr   + dSalgssum
/*        TT_kassdag.bruttokr   = TT_kassdag.bruttokr  + dSalgssum                          */
/*        TT_kassdag.ikkekredkr = TT_kassdag.ikkekredkr + dSalgssum - dKreditkr - dAvrundkr */
TT_kassdag.behkontkr = TT_kassdag.behkontkr + dKontantkr - ABS(dCashBack)
TT_kassdag.behkredkr = TT_kassdag.behkredkr + dKreditkr
TT_kassdag.behmed3kr  = TT_kassdag.behmed3kr + dReservBank
TT_kassdag.media3kr   = TT_kassdag.media3kr  + dReservBank   
TT_kassdag.media3ant  = TT_kassdag.media3ant + iReservBankAnt
TT_kassdag.behmed4kr  = TT_kassdag.behmed4kr + dGavekortIn
TT_kassdag.media4kr   = TT_kassdag.media4kr  + dGavekortIn   
TT_kassdag.media4kr   = TT_kassdag.media4kr  + (IF dGavekortIn > 0 AND dGavekortIn > dSalgssum THEN dVeksel - dTilGodeUt ELSE 0)
TT_kassdag.media4ant  = TT_kassdag.media4ant + iGavekortInAnt
/*        TT_kassdag.Regminkr   = TT_kassdag.Regminkr  + dRegminkr  */
/*        TT_kassdag.Regminant  = TT_kassdag.Regminant + iRegminant */
TT_kassdag.kortkr[1]  = TT_kassdag.kortkr[1]  + dBehBankkr
TT_kassdag.kortant[1]  = TT_kassdag.kortant[1]  + iBankant
TT_kassdag.mvagrlag[1]  = TT_kassdag.mvagrlag[1] + dMvaGrLag[1]
TT_kassdag.mvagrlag[2]  = TT_kassdag.mvagrlag[2] + dMvaGrLag[2]
TT_kassdag.mvagrlag[3]  = TT_kassdag.mvagrlag[3] + dMvaGrLag[3]
TT_kassdag.mvagrlag[4]  = TT_kassdag.mvagrlag[4] + dMvaGrLag[4]
TT_kassdag.mvagrlag[5]  = TT_kassdag.mvagrlag[5] + dMvaGrLag[5]
TT_kassdag.mvagrlag[6]  = TT_kassdag.mvagrlag[6] + dMvaGrLag[6]
TT_kassdag.mvagrlag[7]  = TT_kassdag.mvagrlag[7] + dMvaGrLag[7]
TT_kassdag.mvagrlag[8]  = TT_kassdag.mvagrlag[8] + dMvaGrLag[8]
TT_kassdag.mvagrlag[9]  = TT_kassdag.mvagrlag[9] + dMvaGrLag[9]
TT_kassdag.mvakr[1]     = TT_kassdag.mvakr[1]    + dMvaGrpKr[1]
TT_kassdag.mvakr[2]     = TT_kassdag.mvakr[2]    + dMvaGrpKr[2]
TT_kassdag.mvakr[3]     = TT_kassdag.mvakr[3]    + dMvaGrpKr[3]
TT_kassdag.mvakr[4]     = TT_kassdag.mvakr[4]    + dMvaGrpKr[4]
TT_kassdag.mvakr[5]     = TT_kassdag.mvakr[5]    + dMvaGrpKr[5]
TT_kassdag.mvakr[6]     = TT_kassdag.mvakr[6]    + dMvaGrpKr[6]
TT_kassdag.mvakr[7]     = TT_kassdag.mvakr[7]    + dMvaGrpKr[7]
TT_kassdag.mvakr[8]     = TT_kassdag.mvakr[8]    + dMvaGrpKr[8]
TT_kassdag.mvakr[9]     = TT_kassdag.mvakr[9]    + dMvaGrpKr[9].
ASSIGN
TT_kassdag.rabattkr[2]  = TT_kassdag.rabattkr[2]  + dkunderabatt
TT_kassdag.rabattant[2] = TT_kassdag.rabattant[2] + dkunderabant
TT_kassdag.rabattkr[4]  = TT_kassdag.rabattkr[4]  + dGenrabkr - dkunderabatt
TT_kassdag.rabattant[4] = TT_kassdag.rabattant[4] + dGenrabant
TT_kassdag.gavekortutkr  = TT_kassdag.gavekortutkr  + dGavekortUt
TT_kassdag.gavekortutant = TT_kassdag.gavekortutant + dGavekortUtAnt
TT_kassdag.tilgodeinnkr  = TT_kassdag.tilgodeinnkr  + dTilGodeInn - (IF bonghode.belop >= 0 AND dTilGodeInn = 0 
                                                                                THEN 0 ELSE dTilGodeUt) - 
                                                        (IF dTilGodeInn > 0 AND dTilGodeInn > Bonghode.belop THEN (ABS(dKontantkr) + ABS(dVeksel)) ELSE 0)
TT_kassdag.tilgodeinnant = TT_kassdag.tilgodeinnant + dTilGodeInnAnt
TT_kassdag.tilgodeutkr   = TT_kassdag.tilgodeutkr  + dTilGodeUt
TT_kassdag.tilgodeutant  = TT_kassdag.tilgodeutant + dTilGodeUtAnt
TT_kassdag.behtilgodekr  = TT_kassdag.behtilgodekr  + dTilGodeInn
TT_kassdag.nullant       = TT_kassdag.nullant + IF lHarSalg = FALSE AND lHarMakulert = TRUE THEN 1 ELSE 0
TT_kassdag.medant[2]     = TT_kassdag.medant[2] + IF lMedlemsSalg = TRUE THEN 1 ELSE 0
TT_kassdag.medkr[2]     = TT_kassdag.medkr[2] + IF lMedlemsSalg = TRUE THEN dSalgsSum + dReturkr ELSE 0
TT_kassdag.medmva[1]     = TT_kassdag.medmva[1] + IF lMedlemsSalg = FALSE THEN dMvaKr ELSE 0
TT_kassdag.medmva[2]     = TT_kassdag.medmva[2] + IF lMedlemsSalg = TRUE THEN dMvaKr ELSE 0
TT_kassdag.Scannant      = TT_kassdag.Scannant  + iScannant
TT_kassdag.Pluant        = TT_kassdag.pluant    + iPluant
/*                    Grandtot   = Grandtot  + dGrandtot */
           .


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

