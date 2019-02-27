&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Include 
/*------------------------------------------------------------------------
    File        : JukeBoxRichEditor.i 
    Purpose     : This file contains DLL function Declarations  

    Syntax      :

    Description :

    Author(s)   : James Williams
    Created     : June 8,2008
    Notes       :
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

&GLOBAL-DEFINE TEXT_ALIGN_LEFT              1
&GLOBAL-DEFINE TEXT_ALIGN_CENTER        2
&GLOBAL-DEFINE TEXT_ALIGN_RIGHT         3

&GLOBAL-DEFINE PAR_BULLETS        1
&GLOBAL-DEFINE PAR_NOBULLETS      0

&GLOBAL-DEFINE PAR_VAL_ALIGNMENT     1
&GLOBAL-DEFINE PAR_VAL_FIRSTINDENT   2
&GLOBAL-DEFINE PAR_VAL_LEFTINDENT    4
&GLOBAL-DEFINE PAR_VAL_RIGHTINDENT   8
&GLOBAL-DEFINE PAR_VAL_NUMBERING    16

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Include
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: INCLUDE-ONLY
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Include ASSIGN
         HEIGHT             = 15
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Include 


/* ***************************  Main Block  *************************** */

/*Contains all of the DLL declarations.*/
  PROCEDURE RichEditorCreate EXTERNAL "controls.dll":u:
    DEFINE INPUT  PARAMETER RE_hFrameHWD          AS LONG.
    DEFINE RETURN PARAMETER RE_hObject            AS LONG.
  END PROCEDURE.

  PROCEDURE RichEditorSetFont EXTERNAL "CONTROLS.DLL":U:
      DEFINE INPUT  PARAMETER RE_hObject          AS LONG.
      DEFINE INPUT  PARAMETER RE_cFontName        AS CHAR.
      DEFINE INPUT  PARAMETER RE_iFontSize        AS LONG.
      DEFINE INPUT  PARAMETER RE_iStyle           AS LONG.
      DEFINE INPUT  PARAMETER RE_iColor           AS LONG.
      DEFINE INPUT  PARAMETER RE_iValidValues     AS LONG.
      DEFINE RETURN PARAMETER RE_iSuccess         AS LONG.
  END PROCEDURE.

  PROCEDURE RichEditorGetRichText EXTERNAL "controls.dll":U:
      DEFINE INPUT  PARAMETER RE_hObject          AS LONG.
      DEFINE OUTPUT PARAMETER RE_cData            AS CHAR.
      DEFINE INPUT  PARAMETER RE_iDataSize        AS LONG.
      DEFINE RETURN PARAMETER RE_iSuccess         AS LONG.
  END PROCEDURE.

  PROCEDURE RichEditorGetPlainText EXTERNAL "controls.dll":U:
      DEFINE INPUT  PARAMETER RE_hObject          AS LONG.
      DEFINE OUTPUT PARAMETER RE_cData            AS CHAR.
      DEFINE INPUT  PARAMETER RE_iDataSize        AS LONG.
      DEFINE RETURN PARAMETER RE_iSuccess         AS LONG.
  END PROCEDURE.

  PROCEDURE RichEditorSetParagraph  EXTERNAL "controls.dll":U:
      DEFINE INPUT  PARAMETER RE_hObject            AS LONG.
      DEFINE INPUT  PARAMETER RE_iAlignment         AS LONG.
      DEFINE INPUT  PARAMETER RE_iFirstIndent       AS LONG.
      DEFINE INPUT  PARAMETER RE_iLeftIndent        AS LONG.
      DEFINE INPUT  PARAMETER RE_iRightIndent       AS LONG.
      DEFINE INPUT  PARAMETER RE_iBullets           AS LONG.
      DEFINE INPUT  PARAMETER RE_iValidParameters   AS LONG.
      DEFINE RETURN PARAMETER RE_iSuccess           AS LONG.
  END PROCEDURE.

  PROCEDURE RichEditorPrint EXTERNAL "CONTROLS.DLL":U:
      DEFINE INPUT  PARAMETER RE_hObject            AS LONG.
      DEFINE INPUT  PARAMETER RE_cTile              AS CHAR.
      DEFINE RETURN PARAMETER RE_iSuccess           AS LONG.
  END PROCEDURE.

  PROCEDURE RichEditorPaste EXTERNAL "CONTROLS.DLL":U:
      DEFINE INPUT  PARAMETER RE_hObject            AS LONG.
      DEFINE RETURN PARAMETER RE_iSuccess           AS LONG.
  END PROCEDURE.

  PROCEDURE RichEditorPasteSpecial EXTERNAL "CONTROLS.DLL":U:
      DEFINE INPUT  PARAMETER RE_hObject            AS LONG.
      DEFINE RETURN PARAMETER RE_iSuccess           AS LONG.
  END PROCEDURE.

  PROCEDURE RichEditorCopyToClipBoard EXTERNAL "CONTROLS.DLL":U:
      DEFINE INPUT  PARAMETER RE_hObject            AS LONG.
      DEFINE RETURN PARAMETER RE_iSuccess           AS LONG.
  END PROCEDURE.

  PROCEDURE RichEditorCutToClipBoard EXTERNAL "CONTROLS.DLL":U:
      DEFINE INPUT  PARAMETER RE_hObject            AS LONG.
      DEFINE RETURN PARAMETER RE_iSuccess           AS LONG.
  END PROCEDURE.

  PROCEDURE RichEditorUndo EXTERNAL "CONTROLS.DLL":U:
      DEFINE INPUT  PARAMETER RE_hObject            AS LONG.
      DEFINE RETURN PARAMETER RE_iSuccess           AS LONG.
  END PROCEDURE.

  PROCEDURE RichEditorOpenFile EXTERNAL "CONTROLS.DLL":U:
      DEFINE INPUT  PARAMETER RE_hObject            AS LONG.
      DEFINE INPUT  PARAMETER RE_cFile              AS CHAR.
      DEFINE RETURN PARAMETER RE_iSuccess           AS LONG.
  END PROCEDURE.

  PROCEDURE RichEditorSaveFile EXTERNAL "CONTROLS.DLL":U:
      DEFINE INPUT  PARAMETER RE_hObject            AS LONG.
      DEFINE INPUT  PARAMETER RE_cFile              AS CHAR.
      DEFINE RETURN PARAMETER RE_iSuccess           AS LONG.
  END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


