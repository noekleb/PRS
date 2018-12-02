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

DEFINE OUTPUT PARAMETER TABLE-HANDLE ttBlob.
DEFINE VARIABLE iAnt AS INTEGER     NO-UNDO.
DEFINE BUFFER bufSend FOR sendtowoocomm.

DEFINE TEMP-TABLE TT_blob NO-UNDO
FIELD BatchNr   AS INTE
FIELD blobdata  AS BLOB
FIELD butikknr  AS INTE
FIELD typ       AS CHAR
FIELD extraparam AS CHAR
    INDEX batchnr IS PRIMARY batchnr.

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

FOR EACH sendtowoocomm WHERE sendtowoocomm.fetched = FALSE NO-LOCK.
    IF sendtowoocomm.skapad = ? THEN
        NEXT.
    CREATE TT_blob.
    BUFFER-COPY sendtowoocomm TO TT_blob.
    FIND bufSend WHERE ROWID(bufSend) = ROWID(sendtowoocomm).
    ASSIGN bufSend.fetched   = TRUE
           bufSend.fetcheddt = NOW.
    RELEASE bufSend.
    iAnt = iAnt + 1.
    IF iAnt = 5 THEN
        LEAVE.
END.
ttBlob = TEMP-TABLE TT_blob:HANDLE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


