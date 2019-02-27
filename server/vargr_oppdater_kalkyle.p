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
DEF INPUT PARAMETER  cParameter        AS CHAR   NO-UNDO. /* Input parametre til programmet.           */
DEF INPUT PARAMETER  httBuffer         AS HANDLE NO-UNDO. /* Handle til temptable buffer - hvis det benyttes. */
DEF INPUT  PARAMETER icSessionId       AS CHAR   NO-UNDO.
DEF OUTPUT PARAMETER cReturn           AS CHAR   NO-UNDO. /* Retur veri til kallende program.          */
DEF OUTPUT PARAMETER lOk               AS LOG    NO-UNDO. /* Vellykket utføring.                       */

DEF VAR iAntArtPris  AS INT NO-UNDO.
DEF VAR iAntPrisKo   AS INT NO-UNDO.
DEF VAR iAntBestHode AS INT NO-UNDO.
DEF VAR iAntVpiPris  AS INT NO-UNDO.

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

IF httBuffer = ? THEN
  RUN lagreKobling.
ELSE 
  RUN LimInnKobling.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-lagreKobling) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE lagreKobling Procedure 
PROCEDURE lagreKobling :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR piVg               AS INT  NO-UNDO.

  ASSIGN
      piVg = int(ENTRY(1,cParameter,"|"))
      .
  MVAKORR:
  DO:
      FOR EACH ArtBas NO-LOCK WHERE
          ArtBas.Vg    = piVg:
          RUN kalkyle_mva_korr.p (ArtBas.ArtikkelNr, INPUT-OUTPUT iAntArtPris, INPUT-OUTPUT iAntPrisKo, INPUT-OUTPUT iAntBestHode, INPUT-OUTPUT iAntVpiPris).
      END.
      lOk = TRUE.
  END. /* MVAKORR */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-LimInnKobling) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LimInnKobling Procedure 
PROCEDURE LimInnKobling :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR ix     AS INT NO-UNDO.

KOBLING:
DO:
  DO ix = 1 TO NUM-ENTRIES(cParameter,"|"):
    FOR EACH ArtBas NO-LOCK WHERE
        ArtBas.Vg = INT(ENTRY(ix,cParameter,"|")):
        RUN kalkyle_mva_korr.p (ArtBas.ArtikkelNr, INPUT-OUTPUT iAntArtPris, INPUT-OUTPUT iAntPrisKo, INPUT-OUTPUT iAntBestHode, INPUT-OUTPUT iAntVpiPris).
    END.
  END.
  lOk = TRUE.
END. /* KOBLING */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

