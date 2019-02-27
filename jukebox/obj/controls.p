&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*--------------------------------------------------------------------------
    File        : 
    Purpose     :

    Syntax      :

    Description :

    Author(s)   :
    Created     :
    Notes       :
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/*{TreeView.i}*/
{Controls.i NEW}
{JukeBoxControlsGeneral.i}
/*{CVControls.i}*/



DEFINE VARIABLE hControlsLibrary AS INTEGER INITIAL ? NO-UNDO.
DEFINE VARIABLE hProgBarLibrary  AS INTEGER INITIAL ? NO-UNDO.

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
   Other Settings: CODE-ONLY
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Procedure ASSIGN
         HEIGHT             = 21.91
         WIDTH              = 40.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */
ON CLOSE OF THIS-PROCEDURE DO:
  /*RUN FreeLibrary(hControlsLibrary).*/
  MESSAGE ".".
END.

DEF VAR cVirtualStore AS CHAR NO-UNDO.
DEF VAR cVirtualRoot  AS CHAR NO-UNDO.
DEF VAR cInstallDir   AS CHAR NO-UNDO.
DEF VAR cControls     AS CHAR NO-UNDO.

IF hControlsLibrary = ? AND SEARCH("controls.dll") NE ? THEN DO:
  FILE-INFO:FILE-NAME = SEARCH("controls.dll").  
  RUN LoadLibraryA(FILE-INFO:FULL-PATHNAME, OUTPUT hControlsLibrary).
  IF hControlsLibrary = 0 THEN DO:
    FILE-INFO:FILE-NAME = ".".
    cInstallDir = FILE-INFO:FULL-PATHNAME.
    cControls = SEARCH("controls.dll").
    IF OS-GETENV("localappdata") NE ? THEN DO:
      cVirtualRoot = OS-GETENV("localappdata").
      cVirtualStore = cVirtualRoot + "\VirtualStore" + SUBSTR(cInstallDir,INDEX(cInstallDir,"\")).
    END.
    IF SEARCH(cVirtualStore + SUBSTR(cControls,INDEX(cControls,"\"))) NE ? THEN
      RUN LoadLibraryA(cVirtualStore + SUBSTR(cControls,INDEX(cControls,"\")), OUTPUT hControlsLibrary).      
    ELSE DO:
      IF OS-GETENV("allusersprofile") NE ? THEN DO:
        cVirtualRoot = OS-GETENV("allusersprofile").
        cVirtualStore = cVirtualRoot + "\VirtualStore" + SUBSTR(cInstallDir,INDEX(cInstallDir,"\")).
      END.
      IF SEARCH(cVirtualStore + SUBSTR(cControls,INDEX(cControls,"\"))) NE ? THEN 
        RUN LoadLibraryA(cVirtualStore + SUBSTR(cControls,INDEX(cControls,"\")), OUTPUT hControlsLibrary).      
    END.
    IF hControlsLibrary = 0 THEN
      MESSAGE "Failed to load Controls.dll" SKIP
              VIEW-AS ALERT-BOX ERROR.
  END.
END.

IF hProgBarLibrary = ? AND SEARCH("ThreadProgBar.dll") NE ? THEN
  RUN LoadLibraryA(SEARCH("ThreadProgBar.dll"), OUTPUT hProgBarLibrary).

SUBSCRIBE TO "ReleaseProgressBar" ANYWHERE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-LoadControlsDll) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LoadControlsDll Procedure 
PROCEDURE LoadControlsDll :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
IF hControlsLibrary = ? THEN
            RUN LoadLibraryA(SEARCH("controls.dll"), OUTPUT hControlsLibrary).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ReleaseDll) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ReleaseDll Procedure 
PROCEDURE ReleaseDll :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
     RUN FreeLibrary(hControlsLibrary).
     ASSIGN hControlsLibrary = ?.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ReleaseProgressBar) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ReleaseProgressBar Procedure 
PROCEDURE ReleaseProgressBar :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
RUN FreeLibrary(hProgBarLibrary).
hProgBarLibrary = ?.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

