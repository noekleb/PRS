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
DEF INPUT PARAMETER  htmpHandle        AS HANDLE NO-UNDO. /* Handle til temptable - hvis det benyttes. */
DEF INPUT PARAMETER  cSessionId        AS CHAR   NO-UNDO.
DEF OUTPUT PARAMETER cReturn           AS CHAR   NO-UNDO. /* Retur veri til kallende program.          */
DEF OUTPUT PARAMETER lOk               AS LOG    NO-UNDO. /* Vellykket utføring.                       */

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

  RUN genererPrisprofil.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-genererPrisprofil) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE genererPrisprofil Procedure 
PROCEDURE genererPrisprofil :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR piProfilNr        AS INT  NO-UNDO.
  DEF VAR piLoop            AS INT  NO-UNDO.

  ASSIGN
      piProfilNr = int(cParameter)
      .
  
  GEN_PR_BUTIKK:
  FOR EACH Butiker EXCLUSIVE-LOCK:
    lOk = TRUE.
    
    /* Butikker som allerede har egen profil, berøres ikke. */
    IF Butiker.ProfilNr > 999999 THEN 
      NEXT.
    /* Sentrallager skal ikke ha generert egen profil. */  
    IF Butiker.Cl = Butiker.Butik THEN 
      NEXT.
    /* Butikken må være åpen. */
    IF Butiker.ApningsDato = ? THEN 
      NEXT.
    /* butikken må være satt opp med butikksystem */
    IF Butiker.harButikksystem = FALSE THEN 
      NEXT.
      
    ASSIGN
      Butiker.ProfilNr = 1000000 + Butiker.Butik
      Butiker.KortNavn = (IF Butiker.KortNavn = '' THEN 'But ' + string(Butiker.Butik) ELSE Butiker.KortNavn)
      .

    /* Legger opp profil for butikken. */
    IF NOT CAN-FIND(PrisProfil WHERE
      PrisProfil.ProfilNr = 1000000 + Butiker.Butik) THEN 
    DO:
      CREATE PrisProfil.
      ASSIGN
        PrisProfil.ProfilNr    = Butiker.ProfilNr
        PrisProfil.Beskrivelse = 'Profil: ' + string(Butiker.Butik) + ' ' + Butiker.ButNamn
        PrisProfil.Merknad     = ''
        PrisProfil.Notat       = '' 
        PrisProfil.KortNavn    = Butiker.KortNavn
        .
    END. 
  END. /* GEN_PR_BUTIKK */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

