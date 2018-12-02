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

DEF OUTPUT PARAMETER iMixNr AS INT NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&IF DEFINED(EXCLUDE-getMixNr) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getMixNr Procedure 
FUNCTION getMixNr RETURNS INTEGER
  ( /* parameter-definitions */ )  FORWARD.

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
         HEIGHT             = 15
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

iMixNr = getMixNr().

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Implementations ***************** */

&IF DEFINED(EXCLUDE-getMixNr) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getMixNr Procedure 
FUNCTION getMixNr RETURNS INTEGER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE iHentetValue AS INTEGER    NO-UNDO.

  ASSIGN 
      iMixNr       = NEXT-VALUE(Pakkenr)
      iHentetValue = iMixNr
      .
  IF CAN-FIND(ArtBas WHERE ArtBas.PakkeNr = iMixNr) THEN 
  FINNLEDIGT: 
  DO:
      DO WHILE iMixNr < 9999:
          ASSIGN iMixNr = iMixNr + 1.
          IF (NOT CAN-FIND(FIRST ArtBas WHERE ArtBas.PakkeNr = iMixNr) OR
              NOT CAN-FIND(MixMatchHode WHERE MixMatchHode.MixNr = iMixNr)) THEN 
          DO:
              CURRENT-VALUE(Pakkenr) = iMixNr.
              LEAVE FINNLEDIGT.
          END.
      END.
      ASSIGN iMixNr = 0.
      DO WHILE iMixNr < iHentetValue:
          ASSIGN iMixNr = iMixNr + 1.
          IF NOT CAN-FIND(ArtBas WHERE ArtBas.PakkeNr = iMixNr) THEN
              LEAVE FINNLEDIGT.
      END.
      ASSIGN iMixNr = ?.
  END. /* FINNLEDIGT */

  RETURN iMixNr.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

