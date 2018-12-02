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

DEFINE INPUT  PARAMETER lcData       AS LONGCHAR     NO-UNDO.
DEFINE OUTPUT PARAMETER lOK          AS LOGICAL     NO-UNDO.
DEFINE OUTPUT PARAMETER cRetur       AS CHARACTER   NO-UNDO.
DEFINE TEMP-TABLE tt_no_image NO-UNDO
    FIELD article AS DEC DECIMALS 0
    FIELD dirname AS CHAR.

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


RUN sendmail_tsl.p ("NOIMAGE","Bild saknas i webshop","",STRING(lcData) ,"","") NO-ERROR.
IF ERROR-STATUS:ERROR THEN
    ASSIGN lOK = FALSE
           cRetur = "Error run mail".
ELSE
    ASSIGN lOK = TRUE
           cRetur = "Mail sänt" + " " + STRING(lcData).

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


