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

  RUN slettTelleLinjer.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-slettButikkTilgang) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE slettTelleLinjer Procedure 
PROCEDURE slettTelleLinjer :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR piTelleNr         AS INT  NO-UNDO.
  DEF VAR piLoop            AS INT  NO-UNDO.

  DEF BUFFER trgTelleLinje   FOR TelleLinje.
  DEF BUFFER tgtLokTelleHode FOR TelleHode.
  DEF BUFFER bufLokTelleHode FOR TelleHode.

  ASSIGN
      piTelleNr = int(cParameter)
      .
  FIND TelleHode NO-LOCK WHERE
    TelleHode.TelleNr = piTelleNr NO-ERROR.



  IF NOT AVAILABLE TelleHode THEN 
    DO:
      lOk = FALSE.
      RETURN.
    END.

  /* Sletter tellelinjene */
  FOR EACH TelleLinje OF TelleHode EXCLUSIVE-LOCK:
      DELETE TelleLinje.
  END.

  /* Sletter fillinjene for listen.  */
  IF TelleHode.FilId <> 0 THEN
  FOR EACH VPIFilLinje EXCLUSIVE-LOCK WHERE
    VPIFilLinje.FilId = TelleHode.FilId:
    DELETE VPIFilLinje.
  END.

  /* Sletter filhode for listen. */
  DO TRANSACTION:
    FIND VPIFilHode EXCLUSIVE-LOCK WHERE
      VPIFilHode.FilId = TelleHode.FilId NO-ERROR.
    IF AVAILABLE VPIFilHode THEN
        DELETE VPIFilHode.
  END.

  /* Er det en telleliste, skal også de koblede lokasjonslistene slettes. */
  IF TelleHode.TelleType = 1 THEN
  DO:
    FOR EACH tgtLokTelleHode NO-LOCK WHERE
        tgtLokTelleHode.KobletTilTelleNr = TelleHode.TelleNr:
        /* Sletter linjene på lokasjonslisten. */
        FOR EACH trgTelleLinje OF tgtLokTelleHode EXCLUSIVE-LOCK:
            DELETE trgTelleLinje.
        END.
        /* Sletter fillinjene for listen.  */
        IF tgtLokTelleHode.FilId <> 0 THEN
        FOR EACH VPIFilLinje EXCLUSIVE-LOCK WHERE
            VPIFilLinje.FilId = tgtLokTelleHode.FilId:
            DELETE VPIFilLinje.
        END.
        /* Sletter filhode for listen. */
        DO TRANSACTION:
          FIND VPIFilHode EXCLUSIVE-LOCK WHERE
              VPIFilHode.FilId = tgtLokTelleHode.FilId NO-ERROR.
          IF AVAILABLE VPIFilHode THEN
              DELETE VPIFilHode.
    
          /* Sletter tellehode for lokasjonslisten. */
          FIND bufLokTelleHode EXCLUSIVE-LOCK WHERE 
            RECID(bufLokTelleHode) = RECID(tgtLokTelleHode).
          DELETE bufLokTelleHode.
        END.
    END.
  END.

  /* Sletter gamle lister hvis det er gamle telleister som tas bort. */
  FOR EACH HT-FilHode EXCLUSIVE-LOCK WHERE
    HT-FilHode.TelleNr = TelleHode.TelleNr:
    FOR EACH HT-FilLinje OF HT-FilHode EXCLUSIVE-LOCK:
        DELETE HT-FilLinje.
    END.
    DELETE HT-FilHode.
  END.

  /* Sletter tellehode. */
  DO TRANSACTION:
    /* Sletter tellehode for lokasjonslisten. */
    FIND CURRENT TelleHode EXCLUSIVE-LOCK.
    DELETE TelleHode.
  
    lOk = TRUE.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

