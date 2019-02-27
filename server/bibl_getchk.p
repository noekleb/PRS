&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
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

DEF INPUT-OUTPUT PARAMETER lDec AS DEC FORMAT ">>>>>>>>>>>>9" NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&IF DEFINED(EXCLUDE-getchk) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getchk Procedure 
FUNCTION getchk RETURNS CHARACTER
  ( INPUT dInput AS DECIMAL )  FORWARD.

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

  DEFINE VARIABLE cChk AS CHARACTER   NO-UNDO.
  
  cChk = getchk(lDec).
  lDec = dec(cChk).

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Implementations ***************** */

&IF DEFINED(EXCLUDE-getchk) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getchk Procedure 
FUNCTION getchk RETURNS CHARACTER
  ( INPUT dInput AS DECIMAL ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cInput AS CHARACTER       NO-UNDO.
  DEFINE VARIABLE iSum AS INTEGER           NO-UNDO.
  DEFINE VARIABLE iToggle AS INTEGER INIT 2 NO-UNDO.
  DEFINE VARIABLE ii AS INTEGER             NO-UNDO.
  DEFINE VARIABLE iTmp AS INTEGER           NO-UNDO.

  cInput = STRING(dInput).
  DO ii = LENGTH(cInput) TO 1 BY -1:
      iTmp = iToggle * INT(SUBSTR(cInput,ii,1)).
      IF iTmp < 10 THEN
          iSum = iSum + iTmp.
      ELSE
          iSum = iSum + INT(SUBSTR(STRING(iTmp),1,1)) + INT(SUBSTR(STRING(iTmp),2,1)).
      iToggle = IF iToggle = 2 THEN 1 ELSE 2.
  END.
  RETURN IF SUBSTR(STRING(iSum),LENGTH(STRING(iSum))) = "0" THEN "0" ELSE
      STRING(10 - INT(SUBSTR(STRING(iSum),LENGTH(STRING(iSum))))).   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

