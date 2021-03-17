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
                  Input parameter kan være en kommasparert liste. 
                  
                  cFilename = 'c:\temp\runtimeParam.zip,' + 
                              'c:\temp\runtimeExample.p,' +
                              'c:\temp\reports.prl,'      +
                              'c:\temp\sports.lg'.

  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE INPUT PARAMETER cFileName AS CHAR NO-UNDO.

DEFINE VARIABLE mStruct   AS MEMPTR     NO-UNDO. /* SHFileOpStruct */
DEFINE VARIABLE mStrFrom  AS MEMPTR     NO-UNDO. /* pFrom */

DEFINE VARIABLE iReturn   AS INTEGER    NO-UNDO.
DEFINE VARIABLE iCount    AS INTEGER    NO-UNDO.
DEFINE VARIABLE iLength   AS INTEGER    NO-UNDO.

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

/* Initialize 'pFrom' structure */
SET-SIZE(mStrFrom) = LENGTH(cFilename) + (4 * NUM-ENTRIES(cFilename)).

/* Initialize SHFileOpStruct structure */
SET-SIZE(mStruct)  = 28 + LENGTH(cFilename) + 5.

/* Place the filename(s) as a NULL separated list into the 'pFrom' 
structure */
DO iCount = 1 TO NUM-ENTRIES(cFilename):
  PUT-STRING(mStrFrom,
             (IF iCount = 1 THEN 1 ELSE iLength + 1)) =
              ENTRY(iCount,cFilename).
  IF NUM-ENTRIES(cFilename) >= (iCount + 1) THEN DO:
    PUT-BYTE(mStrFrom,25) = 0. /* If there are more entries in the
                                  list, separate them with a NULL
                                  character (zero 0) */
    iLength = iLength + 1. /* Add the length of the NULL to the 
                              length */
  END.
  iLength = iLength + LENGTH(ENTRY(iCount,cFilename)).
END.

/* Build the SHFileOpStruct structure */
PUT-LONG(mStruct,1) = CURRENT-WINDOW:HWND. /*HWND - controlling win*/
PUT-LONG(mStruct,5) = 3. /* Function */
PUT-LONG(mStruct,9) = GET-POINTER-VALUE(mStrFrom). /*Filelist         
                                            structure - From 
                                                   */
PUT-LONG(mStruct,13) = 0. /* Filelist structure - To (move/copy ops)*/
PUT-LONG(mStruct,17) = 64. /* Flags (64 for move to recycle bin)*/
PUT-LONG(mStruct,21) = 0. /* Any Operations Aborted - Not Used here*/
PUT-LONG(mStruct,25) = 0. /* Name Mappings - Not used here */
PUT-LONG(mStruct,29) = 0. /* Title structure for progress indicator*/

RUN SHFileOperationA ( INPUT mStruct,
                       OUTPUT iReturn ).

/* Reset memory for all pointers to zero */
SET-SIZE(mStruct)   = 0.
SET-SIZE(mStrFrom)  = 0.

/* External Procedure definition */
PROCEDURE SHFileOperationA EXTERNAL "shell32.dll":
  DEFINE INPUT PARAMETER mStruct AS MEMPTR.
  DEFINE RETURN PARAMETER iReturn AS LONG.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


