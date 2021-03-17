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

DEF INPUT  PARAMETER rArtBasRecid          AS   RECID          NO-UNDO.
DEF INPUT  PARAMETER dBestillingsDato      AS   DATE           NO-UNDO.
DEF INPUT  PARAMETER dLevDato              AS   DATE           NO-UNDO.
DEF INPUT  PARAMETER lLaptop               AS   LOGICAL        NO-UNDO.
DEF INPUT  PARAMETER iCentralLager         LIKE Butiker.Butik  NO-UNDO.
DEF INPUT  PARAMETER cStorrelser           AS   CHAR           NO-UNDO.
DEF INPUT  PARAMETER iBrGrpNr              LIKE Bruker.BrGrpNr NO-UNDO.
DEF INPUT  PARAMETER cButikkTeam           AS   CHAR           NO-UNDO.
DEF OUTPUT PARAMETER rBestHodeRecid        AS   RECID          NO-UNDO.

DEF VAR wStrTypeLst AS CHAR NO-UNDO.
DEF VAR lNyDirektexIn  AS LOG    NO-UNDO.
DEFINE VARIABLE cTekst   AS CHARACTER NO-UNDO.

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
{syspara.i 5 4 2 wStrTypeLst}

FIND ArtBas WHERE RECID(ArtBas) = rArtBasRecid NO-LOCK NO-ERROR.


{syspara.i 5 4 12 cTekst}
IF CAN-DO('Ja,Yes,1,true',cTekst) THEN
    lNyDirektexIn = TRUE.
ELSE
    lNyDirektexIn = FALSE.

/* Kontrollerer størrelsestypen */
IF AVAILABLE ArtBas THEN
DO:
    IF CAN-DO(wStrTypeLst,STRING(ArtBas.StrTypeID)) THEN
        RETURN.
END.

IF ArtBas.Pakke = FALSE THEN DO:
    IF iCentralLager = ? OR iCentralLager = 0 THEN
        {syspara.i 5 1 1 iCentralLager INT}
    IF cStorrelser = "" THEN
        RUN InitStorrelser.
    RUN SkapeBestilling.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-InitStorrelser) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitStorrelser Procedure 
PROCEDURE InitStorrelser :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    FIND StrType OF ArtBas NO-LOCK NO-ERROR.
    FOR EACH StrTstr OF StrType NO-LOCK:
        ASSIGN cStorrelser = cStorrelser + 
                             (IF cStorrelser = "" THEN "" ELSE " ") + 
                             left-trim(StrTStr.SoStorl).
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SkapeBestilling) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SkapeBestilling Procedure 
PROCEDURE SkapeBestilling :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VAR iTillbudID    AS INTE NO-UNDO.
  DO TRANSACTION:
      
     CREATE BestHode.
     ASSIGN BestHode.ArtikkelNr      = ArtBas.ArtikkelNr
            BestHode.BestStat        = 1
            BestHode.StrTypeID       = ArtBas.StrTypeID
            BestHode.LevFargKod      = ArtBas.LevFargKod
            BestHode.LevKod          = ArtBas.LevKod
            BestHode.LevNr           = ArtBas.LevNr
            BestHode.LevDato         = dLevDato
            BestHode.OrdreNr         = 0
            BestHode.BestillingsDato = IF dBestillingsDato = ? THEN TODAY ELSE dBestillingsDato
            BestHode.LapTop          = lLapTop
            BestHode.DirekteLev      = lNyDirektexIn
            rBestHodeRecid           = RECID(BestHode).
     
     CREATE BestSort.
     ASSIGN BestSort.BestNr     = BestHode.BestNr
            BestSort.SortID     = "FRI"
            BestSort.Fri        = TRUE
            BestSort.Storrelser = cStorrelser.
     FIND Butiker WHERE Butiker.Butik = iCentralLager NO-LOCK.
     CREATE BestPris.
     ASSIGN BestPris.BestNr   = BestHode.BestNr
            BestPris.BestStat = BestHode.BestStat
            BestPris.ProfilNr = Butiker.ProfilNr. /* CENTRALLAGER */
     FIND ArtPris WHERE ArtPris.ArtikkelNr = ArtBas.ArtikkelNr AND
                        ArtPris.ProfilNr  = Butiker.ProfilNr NO-LOCK NO-ERROR.
     IF AVAIL ArtPris THEN
/*          Vi lagrar ordinarie kalkyl på best */
/*          ASSIGN iTillBudID            = IF ArtPris.Tilbud = TRUE THEN 2 ELSE 1 */
         ASSIGN iTillBudID            = 1
                BestPris.DB%          = ArtPris.DB%[iTillBudID]
                BestPris.DBKr         = ArtPris.DBKr[iTillBudID]
                BestPris.DivKost%     = ArtPris.DivKost%[iTillBudID]
                BestPris.DivKostKr    = ArtPris.DivKostKr[iTillBudID]
                BestPris.EuroManuel   = ArtPris.EuroManuel
                BestPris.EuroPris     = ArtPris.EuroPris[iTillBudID]
                BestPris.Frakt        = ArtPris.Frakt[iTillBudID]
                BestPris.Frakt%       = ArtPris.Frakt%[iTillBudID]
                BestPris.InnkjopsPris = ArtPris.InnkjopsPris[iTillBudID]
                BestPris.Pris         = ArtPris.Pris[iTillBudID]
                BestPris.Rab1%        = ArtPris.Rab1%[iTillBudID]
                BestPris.Rab1Kr       = ArtPris.Rab1Kr[iTillBudID]
                BestPris.Rab2%        = ArtPris.Rab2%[iTillBudID]
                BestPris.Rab2Kr       = ArtPris.Rab2Kr[iTillBudID]
                BestPris.Rab3%        = ArtPris.Rab3%[iTillBudID]
                BestPris.Rab3Kr       = ArtPris.Rab3Kr[iTillBudID]
                BestPris.MVAKr        = ArtPris.MVAKr[iTillBudID]
                BestPris.MVA%         = ArtPris.MVA%[iTillBudID]
                BestPris.ValPris      = ArtPris.ValPris[iTillBudID]
                BestPris.VareKost     = ArtPris.VareKost[iTillBudID]
                .

     IF cButikkTeam = "" THEN DO:
         FOR EACH ButikkTilgang WHERE ButikkTilgang.BrGrpNr = iBrGrpNr.
             IF NOT CAN-FIND(FIRST ButikkKobling WHERE ButikkKobling.BrGrpNr = iBrGrpNr AND
                                                       ButikkKobling.Butik   = ButikkTilgang.butik AND
                                                       ButikkKobling.TeamTypeId = 1) THEN
                 NEXT.
             CREATE BestLinje.
             ASSIGN BestLinje.BestNr = BestHode.BestNr
                    BestLinje.Butik  = ButikkTilgang.Butik.
         END.
     END.
     ELSE DO:
         FIND ButikkTeam WHERE ButikkTeam.BrGrpNr     = iBrGrpNr AND
                               ButikkTeam.TeamTypeId  = 1 AND
                               ButikkTeam.Beskrivelse = cButikkTeam NO-LOCK NO-ERROR.
         IF AVAIL ButikkTeam THEN DO:
             FOR EACH ButikkKobling OF ButikkTeam NO-LOCK:
                 CREATE BestLinje.
                 ASSIGN BestLinje.BestNr = BestHode.BestNr
                        BestLinje.Butik  = ButikkKobling.Butik.
             END.
         END.
     END.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

