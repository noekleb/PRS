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
DEFINE INPUT  PARAMETER cButik      AS CHARACTER  NO-UNDO.
DEFINE INPUT  PARAMETER dArtikkelnr LIKE ArtBas.ArtikkelNr  NO-UNDO.
DEFINE INPUT  PARAMETER iTTId       AS INTEGER    NO-UNDO.
DEFINE INPUT  PARAMETER cStorrelser AS CHARACTER  NO-UNDO.
DEFINE INPUT  PARAMETER cAntall     AS CHARACTER  NO-UNDO.
DEFINE INPUT  PARAMETER dVVarekost  AS DECIMAL    NO-UNDO.

DEFINE VARIABLE iBatchNr AS INTEGER    NO-UNDO.
DEFINE VARIABLE iCount   AS INTEGER    NO-UNDO.
DEFINE VARIABLE iCl      AS INTEGER    NO-UNDO.

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

{syspara.i 5 1 1 iCl INT}

FIND ArtBas WHERE 
    ArtBas.ArtikkelNr = dArtikkelNr NO-LOCK NO-ERROR.

IF iTTId = 7 THEN
  run batchlogg.w (program-name(1), 
                   "Lagerjustering - ArtikkelNr: " + STRING(dArtikkelNr),
                    output iBatchNr).
ELSE IF iTTId = 8 THEN
DO:
    ASSIGN /* Nedskrivning skal bare ha en trans pr. butikk. */
        cAntall = ENTRY(1,cAntall)
        .
    run batchlogg.w (program-name(1), 
                     "Nedskrivning - ArtikkelNr: " + STRING(dArtikkelNr),
                      output iBatchNr).
END.
ELSE IF iTTId = 9 THEN
  run batchlogg.w (program-name(1), 
                   "Varetelling - ArtikkelNr: " + STRING(dArtikkelNr),
                    output iBatchNr).

DO  iCount = 1 TO NUM-ENTRIES(cAntall):
    IF TRIM(ENTRY(iCount,cAntall)) <> "" THEN
        RUN SkapaTransLogg (ENTRY(iCount,cStorrelser,";"),INT(ENTRY(iCount,cAntall))).
END.
run batchstatus.p (iBatchNr, 2).

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-SkapaTranslogg) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SkapaTranslogg Procedure 
PROCEDURE SkapaTranslogg :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER cStorl   AS CHARACTER  NO-UNDO.
  DEFINE INPUT  PARAMETER iAntall  AS INTEGER    NO-UNDO.
  DEFINE        VARIABLE  iTransNr AS INTEGER    NO-UNDO.
  DEFINE BUFFER bufTransLogg for TransLogg.

  DO FOR bufTransLogg:
    /* Henter lager */
    FIND Lager NO-LOCK where
         Lager.ArtikkelNr = dArtikkelNr AND
         Lager.Butik = INT(cButik) NO-ERROR.
            
    /* Transaksjonsnummer for butikken. */
    find last bufTransLogg no-lock where
      bufTransLogg.Butik = INT(cButik) use-index TransLogg no-error.
    if available bufTransLogg then
      iTransNr = bufTransLogg.TransNr + 1.
    else 
      iTransNr = 1.

    /* Oppretter transaksjon */
    LAG_TRANS:
    do:
      /* Sjekker at transnr er ledig */
      if can-find(bufTransLogg where
                  bufTransLogg.Butik   = INT(cButik) and
                  bufTransLogg.TransNr = iTransNr AND
                  bufTransLogg.SeqNr   = 1) then
      NESTE_NR:
      do while true:
        iTransNr = iTransNr + 1.
        if can-find(bufTransLogg where
                    bufTransLogg.Butik   = INT(cButik) and
                    bufTransLogg.TransNr = iTransNr AND
                    bufTransLogg.SeqNr   = 1) then
          next NESTE_NR.
       else
          leave NESTE_NR.
      end. /* NESTE_NR */

      create bufTransLogg.
      assign bufTransLogg.Butik        = INT(cButik)
             bufTransLogg.TransNr      = iTransNr
             bufTransLogg.SeqNr        = 1.
      assign bufTransLogg.BatchNr      = iBatchNr
             bufTransLogg.KundNr       = 0
             bufTransLogg.TTId         = iTTId
             bufTransLogg.TBId         = 1
             bufTransLogg.ArtikkelNr   = dArtikkelNr
             bufTransLogg.LevNr        = ArtBas.LevNr
             bufTransLogg.BongId       = 0
             bufTransLogg.BongLinjeNr  = 0
             bufTransLogg.KassaNr      = 0
             bufTransLogg.Vg           = ArtBas.Vg
             bufTransLogg.LopNr        = ArtBas.LopNr
             bufTransLogg.Storl        = cStorl
             bufTransLogg.Dato         = TODAY
             bufTransLogg.Tid          = time
             bufTransLogg.BestNr       = 0
             bufTransLogg.Postert      = false.                                             
             
      /* Tillatte transaksjonstyper 1,2,3,5,7,8,9,10,11 */       
      case ITTId:
        when 7 OR WHEN 9 /* Lagerjustering  */ then 
        DO:
            /* Butikkens pris */
            find Butiker no-lock where
              Butiker.Butik = int(cButik) no-error.
            if available Butiker then
              find ArtPris no-lock where
                ArtPris.ArtikkelNr = ArtBas.ArtikkelNr and
                ArtPris.ProfilNr   = Butiker.ProfilNr no-error.
            /* Sentrallagerets pris, hvis det ikke er pris for butikken. */
            if not available ArtPris then
              do:
                find Butiker no-lock where
                  Butiker.Butik = iCl no-error.
                find ArtPris no-lock where
                  ArtPris.ArtikkelNr = ArtBAs.ArtikkelNr and
                  ArtPris.ProfilNr   = Butiker.ProfilNr no-error.
              end.

            if available Lager AND ArtBas.Lager = TRUE then
            DO:
                ASSIGN
                    bufTransLogg.VVareKost = Lager.VVareKost
                    bufTransLogg.Pris      = Lager.VVareKost
                    .
                IF Lager.VVareKost <= 0 AND AVAILABLE ArtPris THEN
                    ASSIGN
                        bufTransLogg.VVareKost = ArtPris.VareKost[if ArtPris.Tilbud then 2 else 1]
                        bufTransLogg.Pris      = ArtPris.VareKost[if ArtPris.Tilbud then 2 else 1]
                        .
            END.
            else   
                ASSIGN
                    bufTransLogg.VVareKost = ArtPris.VareKost[if ArtPris.Tilbud then 2 else 1]
                    bufTransLogg.Pris      = ArtPris.VareKost[if ArtPris.Tilbud then 2 else 1]
                    .

            assign
              bufTransLogg.Plukket       = true /* Skal ikke ut på plukkliste */
              bufTransLogg.Antall        = iAntall
              bufTransLogg.SattVVareKost = TRUE 
              bufTransLogg.RabKr         = 0
              bufTransLogg.Mva           = 0
              .
        END.
          when 8  /* Nedskriving     */ then 
            assign
              bufTransLogg.Plukket       = true /* Skal ikke ut på plukkliste */
              bufTransLogg.Antall        = iAntall
              bufTransLogg.Pris          = dVVarekost
              bufTransLogg.VVarekost     = dVVarekost
              bufTransLogg.SattVVareKost = TRUE
              bufTransLogg.RabKr         = 0
              bufTransLogg.Mva           = 0.
      end case.
    end. /* LAG_TRANS */
    
  if available bufTransLogg then
    release bufTransLogg.    
  END. /* TRANSLOGG */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

