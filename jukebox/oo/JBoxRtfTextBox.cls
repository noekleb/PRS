USING Progress.Lang.*.
USING Progress.Windows.UserControl.
USING Progress.Windows.Form.

ROUTINE-LEVEL ON ERROR UNDO, THROW.

CLASS JBoxRtfTextBox INHERITS UserControl: 
	
	DEFINE PRIVATE VARIABLE components      AS System.ComponentModel.IContainer NO-UNDO.
  DEFINE PUBLIC PROPERTY textBox1         AS System.Windows.Forms.RichTextBox NO-UNDO GET. SET.
  DEFINE PUBLIC VARIABLE hWinSuper        AS HANDLE NO-UNDO.
  DEFINE PUBLIC VARIABLE hWinProc         AS HANDLE NO-UNDO.
  DEFINE PUBLIC VARIABLE hWindow          AS HANDLE NO-UNDO.
  DEFINE PUBLIC VARIABLE hPlaceHolder     AS HANDLE NO-UNDO.
  DEFINE PUBLIC VARIABLE hbUserControls   AS HANDLE NO-UNDO. /* Buffer to temp-table for user controls in window super */
  DEFINE PUBLIC VARIABLE oForm            AS Form   NO-UNDO.
  DEFINE PUBLIC VARIABLE hFieldMap        AS HANDLE NO-UNDO.
  DEFINE PUBLIC VARIABLE cFieldMapDisplay AS CHAR   NO-UNDO.
  DEFINE PUBLIC VARIABLE oJBoxContextMenu AS JBoxContextMenu NO-UNDO. /* See last method: InitializeMenu */

  DEF VAR iDefaultFontSize    AS INT  NO-UNDO.
  DEF VAR cDefaultFont        AS CHAR NO-UNDO.

  DEFINE PRIVATE PROPERTY bDisableTextChanged AS LOG NO-UNDO GET. SET.

	CONSTRUCTOR PUBLIC JBoxRtfTextBox ( INPUT ihWinSuper    AS HANDLE, /* Window super proc - unknown if object created from window */
                                      INPUT ihWinProc     AS HANDLE,
                                      INPUT ihPlaceHolder AS HANDLE ): /* Design widget (rectangle, field, etc) */
	 
    SUPER().
    ASSIGN hWinSuper    = IF VALID-HANDLE(ihWinSuper) THEN ihWinSuper ELSE DYNAMIC-FUNCTION("getWindowSuper" IN ihWinProc)        
           hWinProc     = ihWinProc
           hWindow      = ihWinProc:CURRENT-WINDOW
           hPlaceHolder = ihPlaceHolder
           cDefaultFont      = THIS-OBJECT:Font:Name
           iDefaultFontSize  = THIS-OBJECT:Font:Size
           .
    InitializeComponent().
        
    CATCH e AS Progress.Lang.Error:
       UNDO, THROW e.
    END CATCH.

  END CONSTRUCTOR.

	CONSTRUCTOR PUBLIC JBoxRtfTextBox ( INPUT ihWinProc     AS HANDLE,
                                      INPUT ihPlaceHolder AS HANDLE ): /* Design widget (rectangle, field, etc) */
	 
    THIS-OBJECT(?,ihWinProc,ihPlaceHolder).
        
    CATCH e AS Progress.Lang.Error:
       UNDO, THROW e.
    END CATCH.

  END CONSTRUCTOR.

	METHOD PUBLIC VOID InitializeComponent(  ):
    THIS-OBJECT:textBox1 = NEW System.Windows.Forms.RichTextBox().
    THIS-OBJECT:SuspendLayout().
    THIS-OBJECT:textBox1:Location = NEW System.Drawing.Point(1, 1).
    THIS-OBJECT:textBox1:Size = NEW System.Drawing.Size(hPlaceHolder:WIDTH-PIXELS,hPlaceHolder:HEIGHT-PIXELS).
    THIS-OBJECT:textBox1:Name = "textBox1".
    THIS-OBJECT:textBox1:TabIndex = 0.

    THIS-OBJECT:textBox1:TextChanged:Subscribe(THIS-OBJECT:handler_TextChanged).

    THIS-OBJECT:Controls:Add(THIS-OBJECT:textBox1).
    THIS-OBJECT:Location = NEW System.Drawing.Point(DYNAMIC-FUNCTION("getAbsPosition",hPlaceHolder,"x"),DYNAMIC-FUNCTION("getAbsPosition",hPlaceHolder,"y")).  
    THIS-OBJECT:Size = NEW System.Drawing.Size(hPlaceHolder:WIDTH-PIXELS,hPlaceHolder:HEIGHT-PIXELS).
    THIS-OBJECT:Name = "JBoxRtfTextBox".
    THIS-OBJECT:ResumeLayout(FALSE).
    THIS-OBJECT:PerformLayout().
    CATCH e AS Progress.Lang.Error:
        UNDO, THROW e.
    END CATCH.

  END METHOD.
 
  /* Mandatory methods: */

  METHOD PUBLIC System.Windows.Forms.RichTextBox getDotNetWidget():
    RETURN textbox1.
  END METHOD.

  METHOD PUBLIC CHARACTER getDotNetWidgetType():
    RETURN STRING(textbox1:GetType()).
  END METHOD.

  METHOD PUBLIC LOGICAL RegisterWithJukeBox(INPUT ibView AS LOG):

    hbUserControls = DYNAMIC-FUNCTION("getUserControlsBuffer" IN hWinProc).
    oForm = CAST(DYNAMIC-FUNCTION("getWindowForm" IN hWinProc),"Progress.Windows.Form").

    oForm:Controls:Add(THIS-OBJECT).

    IF ibView THEN THIS-OBJECT:BringToFront().
    ELSE THIS-OBJECT:SendToBack().

    DYNAMIC-FUNCTION("NewObject",hWindow,hPlaceHolder,STRING(THIS-OBJECT:GetType())).

    hbUserControls:BUFFER-CREATE().
    ASSIGN hbUserControls::oUserControl     = CAST(THIS-OBJECT,"Progress.Lang.Object")
           hbUserControls::cUserControlType = STRING(THIS-OBJECT:GetType())
           hbUserControls::oWidget          = CAST(THIS-OBJECT:getDotNetWidget(),"Progress.Lang.Object")
           hbUserControls::cWidgetType      = THIS-OBJECT:getDotNetWidgetType()
           hbUserControls::hPlaceHolder     = hPlaceHolder
           hbUserControls::hTabFolder       = DYNAMIC-FUNCTION("getCurrTabFolder" IN hWinProc)
           hbUserControls::iTabPage         = DYNAMIC-FUNCTION("getCurrTabPage" IN hWinProc)
           hbUserControls::hSourceProc      = hWinProc
           hbUserControls::cMethodSuffix    = "RtfText"
           .

  END METHOD.

  METHOD PUBLIC LOGICAL CreateDisplayLink(INPUT ihFieldMap AS HANDLE,INPUT icDisplay AS CHAR):
    ASSIGN hFieldMap        = ihFieldMap
           cFieldMapDisplay = icDisplay
           .
    DYNAMIC-FUNCTION("CreateDotNetDisplayLink",hFieldMap,hPlaceHolder,cFieldMapDisplay).
  END METHOD.

  METHOD PUBLIC CHARACTER DEFAULT():
    MESSAGE PROGRAM-NAME(2)
      VIEW-AS ALERT-BOX INFO BUTTONS OK.
    RETURN DYNAMIC-INVOKE(THIS-OBJECT,ENTRY(1,PROGRAM-NAME(2)," ")).
  END METHOD.

  /* Methods invoked from jukebox (via window super): */

  METHOD PUBLIC CHARACTER dotNetUndo():
/*     THIS-OBJECT:Font = NEW System.Drawing.Font(cDefaultFont,iDefaultFontSize). */
    RETURN "".
  END.

  METHOD PUBLIC CHARACTER dotNetDisplay():
    bDisableTextChanged = YES.
    textbox1:Font = NEW System.Drawing.Font(cDefaultFont,iDefaultFontSize,System.Drawing.FontStyle:Regular).

    IF hFieldMap:AVAIL THEN DO:
      textbox1:Text = hFieldMap:BUFFER-FIELD(cFieldMapDisplay):BUFFER-VALUE.
    END.
    ELSE
      textbox1:Text = "".
    textbox1:ReadOnly = NOT hFieldMap:AVAIL.
    bDisableTextChanged = NO.
    RETURN "".
  END.

  METHOD PUBLIC CHARACTER dotNetSave():
    DEF VAR cFileName AS CHAR NO-UNDO.

    cFileName = DYNAMIC-FUNCTION("getUniqueFileName").
    cFileName = REPLACE(cFileName,".tmp",".rtf").
    textbox1:SaveFile(cFileName).
/*     textbox1:SaveFile(cFileName,System.Windows.Forms.RichTextBoxStreamType.RichText). */
    FILE-INFO:FILE-NAME = cFileName.
    MESSAGE FILE-INFO:FILE-SIZE SKIP
            textbox1:TextLength
        VIEW-AS ALERT-BOX INFO BUTTONS OK.

    RETURN "".
  END.

  /* (Semi-)class specific methods - event handlers: */
  METHOD PRIVATE VOID handler_TextChanged (sender AS CLASS System.Object, e AS CLASS System.EventArgs):
    IF NOT bDisableTextChanged THEN 
      RUN InvokeMethod IN hWinSuper (hPlaceHolder,"TextChanged").
    bDisableTextChanged = NO.
  END METHOD. 

  /* Class specific methods: */

  METHOD PUBLIC VOID DisableTextChanged (ibDisableTextChanged AS LOG):
    IF textbox1:Parent:Parent:VISIBLE THEN
      bDisableTextChanged = ibDisableTextChanged.
  END METHOD.

  METHOD PUBLIC INTEGER getFontHeight():
    RETURN THIS-OBJECT:FontHeight.
  END METHOD.


  METHOD PUBLIC CHARACTER BulletsRtfText():
    textbox1:SelectionBullet = YES.
    RETURN "".
  END.

  METHOD PUBLIC CHARACTER CopyRtfText():
    textbox1:Copy().
    RETURN "".
  END METHOD.

  METHOD PUBLIC CHARACTER FontRtfText ():
    DEF VAR cLabel AS CHAR NO-UNDO.
  
    cLabel = oJBoxContextMenu:getSelectedMenuLabel().
    IF cLabel NE "" THEN DO:
      IF textbox1:SelectedText NE "" THEN 
        textbox1:SelectionFont = NEW System.Drawing.Font(cLabel,THIS-OBJECT:Font:Size).
      ELSE
        textbox1:Font = NEW System.Drawing.Font(cLabel,THIS-OBJECT:Font:Size).
    END.
  END METHOD.

  METHOD PUBLIC CHARACTER FontSizeRtfText ():
    DEF VAR iFontSize AS INT NO-UNDO.
  
    iFontSize = INTEGER(oJBoxContextMenu:getSelectedMenuLabel()) NO-ERROR.
    IF NOT ERROR-STATUS:ERROR AND iFontSize > 0 THEN DO:
      IF textbox1:SelectedText NE "" THEN 
        textbox1:SelectionFont = NEW System.Drawing.Font("",iFontSize).
      ELSE
        textbox1:Font = NEW System.Drawing.Font("",iFontSize).
    END.
  END METHOD.

  METHOD PUBLIC CHARACTER FontStyleRtfText ():
    DEF VAR cFontStyle AS CHAR NO-UNDO.
    DEF VAR oFontStyle AS CLASS System.Drawing.FontStyle NO-UNDO.
    DEF VAR cl_style   AS System.Enum NO-UNDO.

    cFontStyle = oJBoxContextMenu:getSelectedMenuLabel().
    IF NOT ERROR-STATUS:ERROR AND cFontStyle NE "" THEN DO:
      cl_style = mFontStyle(cFontStyle).

      oFontStyle = CAST(cl_style,System.Drawing.FontStyle).
      IF textbox1:SelectedText NE "" THEN 
        textbox1:SelectionFont = NEW System.Drawing.Font("",textbox1:Font:Size,oFontStyle).
      ELSE
        textbox1:Font = NEW System.Drawing.Font("",textbox1:Font:Size,oFontStyle).
    END.
  END METHOD.

  METHOD PUBLIC CHARACTER TextChangedRtfText ():
    DEF VAR hToolbar AS HANDLE NO-UNDO.
  
    IF VALID-HANDLE(hFieldMap) THEN DO:
      hToolbar = DYNAMIC-FUNCTION("GetLinkedObject",hFieldMap,"toolbar","from").
      IF VALID-HANDLE(hToolbar) THEN
        DYNAMIC-FUNCTION("setToolbar",hToolbar,"modified").
    END.
    RETURN "".
  END METHOD.

  METHOD PUBLIC CHARACTER UndoRtfText():
    textbox1:UNDO().
    RETURN "".
  END.

  METHOD PUBLIC CHARACTER InitializeMenu (icMenu AS CHAR):
    DEF VAR cFontList           AS CHAR   NO-UNDO
        INIT "Arial,Arial Black,Comic Sans MS,Comic Sans MS Bold Courier,Courier New,Estrangelo Edessa,Franklin Gothic Medium,Italic,Georgia,Lucida Console,Lucida Sans Unicode,MS Sans Serif,Modern MS Sans Serif,MS Serif,Mv Boli,Palatino Linotype,Roman,Tahoma,Tahoma Bold,Times New Roman,Trebuchet MS,Tunga,Verdana". 
    DEF VAR cFontSizeList       AS CHAR   NO-UNDO
        INIT "8,9,10,12,14,16,18,24,28".
    DEF VAR cFontStyleList      AS CHAR   NO-UNDO
        INIT "Bold,Italic,Regular,Strikeout,Underline".
    DEF VAR cColorList          AS CHAR   NO-UNDO
        INIT "Aqua,Black,Blue,Cream,Darkgray,Fuchsia,Gray,Green,Limegreen,Lightgray,Maroon,Mediumgray,Mintgreen,Navyblue,Olive,Purple,Red,Silver,Skyblue,Teal,White,Yellow".
    DEF VAR cColorNumList       AS CHAR   NO-UNDO
        INIT "16776960,0,16711680,15793151,8421504,16711935,8421504,32768,65280,12632256,128,10789024,12639424,8388608,32896,8388736,255,12632256,15780518,8421376,16777215,65535".
      
    DEF VAR cMenu AS CHAR NO-UNDO.
    DEF VAR ix    AS INT  NO-UNDO.

    oJBoxContextMenu = NEW JBoxContextMenu(hWinSuper,hWinProc,hPlaceHolder,"").

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
                     + ",InsertFile;Load file"
    /*                  + ",|Append file"  */
                     + ",Print"
                     ).

    textbox1:ContextMenu = oJBoxContextMenu:contextMenu1.

    cMenu = "".
    DO ix = 1 TO NUM-ENTRIES(cFontList):
      cMenu = cMenu + (IF cMenu NE "" THEN "," ELSE "") + "Font;" + ENTRY(ix,cFontList).
    END.
    oJBoxContextMenu:NewDotNetMenuBand (oJBoxContextMenu:getMenuItem("Font"),cMenu).

    cMenu = "".
    DO ix = 1 TO NUM-ENTRIES(cFontSizeList):
      cMenu = cMenu + (IF cMenu NE "" THEN "," ELSE "") + "Font size;" + ENTRY(ix,cFontSizeList) + ";FontSize".
    END.
    oJBoxContextMenu:NewDotNetMenuBand (oJBoxContextMenu:getMenuItem("Font size"),cMenu).

    cMenu = "".
    DO ix = 1 TO NUM-ENTRIES(cFontStyleList):
      cMenu = cMenu + (IF cMenu NE "" THEN "," ELSE "") + "Font style;" + ENTRY(ix,cFontStyleList) + ";FontStyle".
    END.
    oJBoxContextMenu:NewDotNetMenuBand (oJBoxContextMenu:getMenuItem("Font style"),cMenu).


  END METHOD.

  METHOD PRIVATE System.Enum mFontStyle(cl_styleClick AS CHARACTER):
    DEFINE VARIABLE cl_style as System.Enum NO-UNDO.
    DEFINE VARIABLE cl_Enum  as System.Enum NO-UNDO.

    cl_style = THIS-OBJECT:textbox1:SelectionFont:Style.
    cl_Enum = cl_style.
    
    CASE cl_styleClick:
      WHEN "Bold" THEN DO:
        IF (textbox1:SelectedText NE "" AND NOT THIS-OBJECT:textbox1:SelectionFont:Bold) OR
           NOT THIS-OBJECT:textbox1:Font:Bold THEN DO:
            cl_Enum = Progress.Util.EnumHelper:Add(cl_style, System.Drawing.FontStyle:Bold).
            cl_style = cl_Enum.    
        END.
        ELSE DO:
          cl_Enum = Progress.Util.EnumHelper:Subtract(cl_style, System.Drawing.FontStyle:Bold).
          cl_style = cl_Enum.    
        END.
      END.
    END CASE.

   /* same thing for other Font Styles */
    RETURN cl_style.

  END METHOD.

	DESTRUCTOR PUBLIC JBoxRtfTextBox ( ):

		IF VALID-OBJECT(components) THEN DO:
			CAST(components, System.IDisposable):Dispose().
		END.

	END DESTRUCTOR.
END CLASS.
