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

DEF INPUT PARAM ioJBoxRtfTextBox AS JBoxRtfTextBox NO-UNDO.
DEF INPUT PARAM ihWinProc        AS HANDLE NO-UNDO.
DEF INPUT PARAM ihPlaceHolder    AS HANDLE NO-UNDO.
DEF INPUT PARAM ihFieldMap       AS HANDLE NO-UNDO.
DEF INPUT PARAM icField          AS CHAR   NO-UNDO.
DEF INPUT PARAM icMenuType       AS CHAR   NO-UNDO.


DEF VAR oJBoxContextMenu AS JBoxContextMenu NO-UNDO.
DEF VAR oMSTextBox       AS System.Windows.Forms.RichTextBox NO-UNDO.
DEF VAR hSourceProc      AS HANDLE NO-UNDO.
DEF VAR ix               AS INT NO-UNDO.

DEF VAR cFontList           AS CHAR   NO-UNDO
    INIT "Arial,Arial Black,Comic Sans MS,Comic Sans MS Bold Courier,Courier New,Estrangelo Edessa,Franklin Gothic Medium,Italic,Georgia,Lucida Console,Lucida Sans Unicode,MS Sans Serif,Modern MS Sans Serif,MS Serif,Mv Boli,Palatino Linotype,Roman,Tahoma,Tahoma Bold,Times New Roman,Trebuchet MS,Tunga,Verdana". 
DEF VAR cFontSizeList       AS CHAR   NO-UNDO
    INIT "8,9,10,12,14,16,18,24,28".
DEF VAR cColorList          AS CHAR   NO-UNDO
    INIT "Aqua,Black,Blue,Cream,Darkgray,Fuchsia,Gray,Green,Limegreen,Lightgray,Maroon,Mediumgray,Mintgreen,Navyblue,Olive,Purple,Red,Silver,Skyblue,Teal,White,Yellow".
DEF VAR cColorNumList       AS CHAR   NO-UNDO
    INIT "16776960,0,16711680,15793151,8421504,16711935,8421504,32768,65280,12632256,128,10789024,12639424,8388608,32896,8388736,255,12632256,15780518,8421376,16777215,65535".

DEF VAR iDefaultFontSize    AS INT  NO-UNDO.
DEF VAR cDefaultFont        AS CHAR NO-UNDO.
DEF VAR cDefaultFontStyle   AS CHAR NO-UNDO.

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
         HEIGHT             = 15
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

hSourceProc = SOURCE-PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-BulletsRtfText) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE BulletsRtfText Procedure 
PROCEDURE BulletsRtfText :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
oMSTextBox:SelectionBullet = YES.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-CopyRtfText) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CopyRtfText Procedure 
PROCEDURE CopyRtfText :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
oMSTextBox:Copy().

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-dotNetDisplay) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE dotNetDisplay Procedure 
PROCEDURE dotNetDisplay :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
IF ihFieldMap:AVAIL THEN
  oMSTextBox:Text = ihFieldMap:BUFFER-FIELD(icField):BUFFER-VALUE.
ELSE
  oMSTextBox:Text = "".
oMSTextBox:ReadOnly = NOT ihFieldMap:AVAIL.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-dotNetUndo) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE dotNetUndo Procedure 
PROCEDURE dotNetUndo :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
ASSIGN
       ioJBoxRtfTextBox:Font       = NEW System.Drawing.Font(cDefaultFont,iDefaultFontSize)
/*        ioJBoxRtfTextBox:Font:Size  = iDefaultFontSize                                */
/*        ioJBoxRtfTextBox:Font:Style = NEW System.Drawing.FontStyle(cDefaultFontStyle) */
       .
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-FontRtfText) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE FontRtfText Procedure 
PROCEDURE FontRtfText :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR cLabel AS CHAR NO-UNDO.

cLabel = oJBoxContextMenu:getSelectedMenuLabel().
IF cLabel NE "" THEN DO:
  IF oMSTextBox:SelectedText NE "" THEN 
    oMSTextBox:SelectionFont = NEW System.Drawing.Font(cLabel,ioJBoxRtfTextBox:Font:Size).
  ELSE
    oMSTextBox:Font = NEW System.Drawing.Font(cLabel,ioJBoxRtfTextBox:Font:Size).
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-InitializeRtfText) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitializeRtfText Procedure 
PROCEDURE InitializeRtfText :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR cMenu AS CHAR NO-UNDO.

ASSIGN cDefaultFont      = ioJBoxRtfTextBox:Font:Name
       iDefaultFontSize  = ioJBoxRtfTextBox:Font:Size
       cDefaultFontStyle = STRING(ioJBoxRtfTextBox:Font:Style)
       .

/* MESSAGE ioJBoxRtfTextBox:getFontHeight() SKIP  */
/*         ioJBoxRtfTextBox:Font:Size             */
/*     VIEW-AS ALERT-BOX INFO BUTTONS OK.         */

IF VALID-HANDLE(ihFieldMap) THEN 
  DYNAMIC-FUNCTION("CreateDotNetDisplayLink",ihFieldMap,ihPlaceHolder,icField).

oMSTextBox = ioJBoxRtfTextBox:getDotNetWidget().

oJBoxContextMenu = NEW JBoxContextMenu(THIS-PROCEDURE,hSourceProc,ihWinProc,ihPlaceHolder,"").

oJBoxContextMenu:NewDotNetMenuBand (?,
                   "Undo"
                 + ",rule"
                 + ",Cut"
                 + ",Copy"
                 + ",Paste"
                 + ",Bullets"
                 + ",PasteSpecial;Paste special"
                 + ",rule"
                 + ",|Font"
                 + ",|Font size"
                 + ",|Font style"
                 + ",|Font Color"
                 + ",ResetFontAndStyle;Reset Font Settings"
                 + ",rule"
                 + ",|Paragraph"
                 + ",rule"
                 + ",InsertFromFile;Load file"
/*                  + ",|Append file"  */
                 + ",Print"
                 ).

oMSTextBox:ContextMenu = oJBoxContextMenu:contextMenu1.

/* MESSAGE oJBoxContextMenu:getMenuItem("Paste"):Tag */
/*     VIEW-AS ALERT-BOX INFO BUTTONS OK.            */

cMenu = "".
DO ix = 1 TO NUM-ENTRIES(cFontList):
  cMenu = cMenu + (IF cMenu NE "" THEN "," ELSE "") + "Font" + ";" + ENTRY(ix,cFontList).
END.
oJBoxContextMenu:NewDotNetMenuBand (oJBoxContextMenu:getMenuItem("Font"),cMenu).


/*


  DYNAMIC-FUNCTION("NewMenuBand",FRAME rtf-edit-frame:HANDLE
                  ,"Undo"
                 + ",rule"
                 + ",Cut"
                 + ",Copy"
                 + ",Paste"
                 + ",PasteSpecial;Paste special"
                 + ",rule"
                 + ",|Font"
                 + ",|Font size"
                 + ",|Font style"
                 + ",|Font Color"
                 + ",ResetFontAndStyle;Reset Font Settings"
                 + ",rule"
                 + ",|Paragraph"
                 + ",rule"
                 + ",InsertFromFile;Load file"
/*                  + ",|Append file"  */
                 + ",Print"
                  ,"").

  cMenu = "".
  DO ix = 1 TO NUM-ENTRIES(cFontList):
    cMenu = cMenu + (IF cMenu NE "" THEN "," ELSE "") + "Font;" + ENTRY(ix,cFontList).
  END.
  DYNAMIC-FUNCTION("NewMenuBand",WIDGET-HANDLE(DYNAMIC-FUNCTION("getAttribute",FRAME rtf-edit-frame:HANDLE,"placeholder1"))
                  ,cMenu
                  ,"").

  cMenu = "".
  DO ix = 1 TO NUM-ENTRIES(cFontSizeList):
    cMenu = cMenu + (IF cMenu NE "" THEN "," ELSE "") + "FontSize;" + ENTRY(ix,cFontSizeList).
  END.
  DYNAMIC-FUNCTION("NewMenuBand",WIDGET-HANDLE(DYNAMIC-FUNCTION("getAttribute",FRAME rtf-edit-frame:HANDLE,"placeholder2"))
                  ,cMenu
                  ,"").

  DYNAMIC-FUNCTION("NewMenuBand",WIDGET-HANDLE(DYNAMIC-FUNCTION("getAttribute",FRAME rtf-edit-frame:HANDLE,"placeholder3"))
                ,"FontStyle;Normal"
               + ",FontStyle;Bold"
               + ",FontStyle;Bold Italic"
               + ",FontStyle;Bold Underline"
               + ",FontStyle;Bold Strikeout"
               + ",FontStyle;Bold Italic Underline"
               + ",FontStyle;Bold Italic Strikeout"
               + ",FontStyle;Bold Underline Strikeout"
               + ",FontStyle;Bold Italic Underline Strikeout"
               + ",FontStyle;Italic"
               + ",FontStyle;Italic Underline"
               + ",FontStyle;Italic Strikeout"
               + ",FontStyle;Italic Underline Strikeout"
               + ",FontStyle;Underline"
               + ",FontStyle;Underline Strikeout"
               + ",FontStyle;Strikeout"
                ,"").

  cMenu = "".
  DO ix = 1 TO NUM-ENTRIES(cColorList):
    cMenu = cMenu + (IF cMenu NE "" THEN "," ELSE "") + "Color;" + ENTRY(ix,cColorList).
  END.
  DYNAMIC-FUNCTION("NewMenuBand",WIDGET-HANDLE(DYNAMIC-FUNCTION("getAttribute",FRAME rtf-edit-frame:HANDLE,"placeholder4"))
                  ,cMenu
                  ,"").


  DYNAMIC-FUNCTION("NewMenuBand",WIDGET-HANDLE(DYNAMIC-FUNCTION("getAttribute",FRAME rtf-edit-frame:HANDLE,"placeholder5"))
                ,"Align;Left Justified"
               + ",Align;Centered"
               + ",Align;Right Justified"
               + ",Align;Left Indent"
               + ",Bullets;Bullets"
               + ",NoBullets;No Bullets"
                ,"").

/*   DYNAMIC-FUNCTION("NewMenuBand",WIDGET-HANDLE(DYNAMIC-FUNCTION("getAttribute",FRAME rtf-edit-frame:HANDLE,"placeholder6")) */
/*                 ,"AppendFirst;First"                                                                                        */
/*                + ",AppendLast;Last"                                                                                         */
/*                 ,"").                                                                                                       */
/*                                                                                                                             */

  GET-KEY-VALUE SECTION "StartUp" KEY "DefaultFont" VALUE cDefaultFont.
  IF INDEX(cDefaultFont,"size=") > 0 THEN DO:
    iDefaultFontSize = INT(SUBSTR(cDefaultFont,INDEX(cDefaultFont,"size=") + 5)) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN iDefaultFontSize = 8.
  END.





*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-TextChangedRtfText) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE TextChangedRtfText Procedure 
PROCEDURE TextChangedRtfText :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR hToolbar AS HANDLE NO-UNDO.

IF VALID-HANDLE(ihFieldMap) THEN DO:
  hToolbar = DYNAMIC-FUNCTION("GetLinkedObject",ihFieldMap,"toolbar","from").
  IF VALID-HANDLE(hToolbar) THEN
    DYNAMIC-FUNCTION("setToolbar",hToolbar,"modified").
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-UndoRtfText) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE UndoRtfText Procedure 
PROCEDURE UndoRtfText :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
oMSTextBox:Undo().
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

