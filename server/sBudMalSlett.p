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

  RUN slettSBudMalHode.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-slettSBudMalHode) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE slettSBudMalHode Procedure 
PROCEDURE slettSBudMalHode :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR piMalNr         AS INT  NO-UNDO.
  DEF VAR piLoop            AS INT  NO-UNDO.

  DEF BUFFER trgTelleLinje   FOR TelleLinje.
  DEF BUFFER tgtLokSBudMalHode FOR SBudMalHode.
  DEF BUFFER bufLokSBudMalHode FOR SBudMalHode.

  ASSIGN
      piMalNr = INTEGER(cParameter)
      .
  FIND SBudMalHode NO-LOCK WHERE
    SBudMalHode.MalId = piMalNr NO-ERROR.



  IF NOT AVAILABLE SBudMalHode THEN 
    DO:
      lOk = FALSE.
      RETURN.
    END.

  /* Sletter tellelinjene */
  FOR EACH SBudMalManed WHERE 
      SBudMalManed.MalId = SBudMalHode.MalId EXCLUSIVE-LOCK:
      FOR EACH SBudMalDag WHERE 
          SBudMalDag.MalId  = SBudMalManed.MalId AND 
          SBudMalDag.AarMnd = SBudMalManed.AarMnd:
          DELETE SBudMalDag.    
      END.    
      DELETE SBudMalManed.
  END.

  /* Sletter SBudMalHode. */
  DO TRANSACTION:
    /* Sletter SBudMalHode for lokasjonslisten. */
    FIND CURRENT SBudMalHode EXCLUSIVE-LOCK.
    DELETE SBudMalHode.
  
    lOk = TRUE.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

