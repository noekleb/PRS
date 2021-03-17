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

{windows.i}
 
DEFINE INPUT  PARAMETER DialogTitle AS CHAR NO-UNDO.
DEFINE OUTPUT PARAMETER FolderName  AS CHAR NO-UNDO.
DEFINE OUTPUT PARAMETER Canceled    AS LOGICAL NO-UNDO.
 
DEF VAR MAX_PATH       AS INTEGER INITIAL 260.
DEF VAR lpbi           AS MEMPTR.  /* pointer to BROWSEINFO structure */
DEF VAR pszDisplayName AS MEMPTR.
DEF VAR lpszTitle      AS MEMPTR.
DEF VAR lpItemIDList   AS INTEGER NO-UNDO.
DEF VAR ReturnValue    AS INTEGER NO-UNDO.
 
SET-SIZE(lpbi)           = 32.
SET-SIZE(pszDisplayName) = MAX_PATH.
SET-SIZE(lpszTitle)      = LENGTH(DialogTitle) + 1.
 
PUT-STRING(lpszTitle,1)  = DialogTitle.
 
PUT-LONG(lpbi, 1) = 0.  /* hwnd for parent */
PUT-LONG(lpbi, 5) = 0.
PUT-LONG(lpbi, 9) = GET-POINTER-VALUE(pszDisplayName).
PUT-LONG(lpbi,13) = GET-POINTER-VALUE(lpszTitle).
PUT-LONG(lpbi,17) = 1. /* BIF_RETURNONLYFSDIRS = only accept a file system directory */
PUT-LONG(lpbi,21) = 0. /* lpfn, callback function */
PUT-LONG(lpbi,25) = 0. /* lParam for lpfn */
PUT-LONG(lpbi,29) = 0.

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

RUN SHBrowseForFolder IN hpApi ( INPUT  GET-POINTER-VALUE(lpbi), 
                                 OUTPUT lpItemIDList ).
 
/* parse the result: */
IF lpItemIDList=0 THEN DO:
   Canceled   = YES.
   FolderName = "".
END.
ELSE DO:
   Canceled = NO.
   FolderName = FILL(" ", MAX_PATH).
   RUN SHGetPathFromIDList IN hpApi(lpItemIDList, 
                                    OUTPUT FolderName,
                                    OUTPUT ReturnValue).
   FolderName = TRIM(FolderName).
END.   
 
/* free memory: */
SET-SIZE(lpbi)=0.
SET-SIZE(pszDisplayName)=0.
SET-SIZE(lpszTitle)=0.
RUN CoTaskMemFree (lpItemIDList).

PROCEDURE CoTaskMemFree EXTERNAL "ole32.dll" :
  DEFINE INPUT PARAMETER lpVoid AS LONG.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-BrowseForFolder) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE BrowseForFolder Procedure 
PROCEDURE BrowseForFolder :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 
 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

