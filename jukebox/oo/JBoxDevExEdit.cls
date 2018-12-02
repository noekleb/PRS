USING Progress.Lang.*.
USING Progress.Windows.UserControl.
USING Progress.Windows.Form.

ROUTINE-LEVEL ON ERROR UNDO, THROW.

CLASS JBoxDevExEdit INHERITS UserControl: 
	
	DEFINE PRIVATE VARIABLE components      AS System.ComponentModel.IContainer NO-UNDO.
  DEFINE PUBLIC PROPERTY textBox1         AS DevExpress.XtraRichEdit.RichEditControl NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY cDocLoadContext  AS CHARACTER NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY cDocLoadIdFields AS CHARACTER NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY cDocLoadFormat   AS CHARACTER NO-UNDO INIT "rtf" GET. SET.
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

/*   DEFINE PRIVATE PROPERTY bDisableContentChanged AS LOG NO-UNDO INIT YES GET. SET. */
  DEFINE VARIABLE  bDisableContentChanged AS LOG NO-UNDO INIT YES.

	CONSTRUCTOR PUBLIC JBoxDevExEdit ( INPUT ihWinSuper    AS HANDLE, /* Window super proc - unknown if object created from window */
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
        
    THIS-OBJECT:textbox1:Options:HorizontalRuler:Visibility = DevExpress.XtraRichEdit.RichEditRulerVisibility:Hidden.
    THIS-OBJECT:textbox1:Options:VerticalRuler:Visibility = DevExpress.XtraRichEdit.RichEditRulerVisibility:Hidden.
    THIS-OBJECT:textbox1:Margin = NEW System.Windows.Forms.Padding(0).
    THIS-OBJECT:textbox1:ActiveViewType = DevExpress.XtraRichEdit.RichEditViewType:Simple.        
        
    CATCH e AS Progress.Lang.Error:
       UNDO, THROW e.
    END CATCH.

  END CONSTRUCTOR.

	CONSTRUCTOR PUBLIC JBoxDevExEdit ( INPUT ihWinProc     AS HANDLE,
                                     INPUT ihPlaceHolder AS HANDLE ): /* Design widget (rectangle, field, etc) */
	 
    THIS-OBJECT(?,ihWinProc,ihPlaceHolder).
        
    CATCH e AS Progress.Lang.Error:
       UNDO, THROW e.
    END CATCH.

  END CONSTRUCTOR.

	METHOD PUBLIC VOID InitializeComponent(  ):
    THIS-OBJECT:textBox1 = NEW DevExpress.XtraRichEdit.RichEditControl().
    THIS-OBJECT:SuspendLayout().
    THIS-OBJECT:textBox1:Location = NEW System.Drawing.Point(1, 1).
    THIS-OBJECT:textBox1:Size = NEW System.Drawing.Size(hPlaceHolder:WIDTH-PIXELS,hPlaceHolder:HEIGHT-PIXELS).
    THIS-OBJECT:textBox1:Name = "textBox1".
    THIS-OBJECT:textBox1:TabIndex = 0.

    THIS-OBJECT:textBox1:ContentChanged:Subscribe(THIS-OBJECT:handler_ContentChanged).

    THIS-OBJECT:Controls:Add(THIS-OBJECT:textBox1).
    THIS-OBJECT:Location = NEW System.Drawing.Point(DYNAMIC-FUNCTION("getAbsPosition",hPlaceHolder,"x"),DYNAMIC-FUNCTION("getAbsPosition",hPlaceHolder,"y")).  
    THIS-OBJECT:Size = NEW System.Drawing.Size(hPlaceHolder:WIDTH-PIXELS,hPlaceHolder:HEIGHT-PIXELS).
    THIS-OBJECT:Name = "JBoxDevExEdit".
    THIS-OBJECT:ResumeLayout(FALSE).
    THIS-OBJECT:PerformLayout().
    CATCH e AS Progress.Lang.Error:
        UNDO, THROW e.
    END CATCH.

  END METHOD.
 
  /* Mandatory methods: */

  METHOD PUBLIC DevExpress.XtraRichEdit.RichEditControl getDotNetWidget():
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
           hbUserControls::cMethodSuffix    = "DevExEdit"
           .

  END METHOD.

  METHOD PUBLIC LOGICAL CreateDisplayLink(INPUT ihFieldMap AS HANDLE,INPUT icDisplay AS CHAR):
    ASSIGN hFieldMap        = ihFieldMap
           cFieldMapDisplay = icDisplay
           .
    DYNAMIC-FUNCTION("CreateDotNetDisplayLink",hFieldMap,hPlaceHolder,cFieldMapDisplay).
  END METHOD.

  METHOD PUBLIC CHARACTER DEFAULT():
    RETURN DYNAMIC-INVOKE(THIS-OBJECT,ENTRY(1,PROGRAM-NAME(2)," ")).
  END METHOD.

  /* Methods invoked from jukebox (via window super): */

  METHOD PUBLIC CHARACTER dotNetUndo():
/*     THIS-OBJECT:Font = NEW System.Drawing.Font(cDefaultFont,iDefaultFontSize). */
    RETURN "".
  END.

  METHOD PUBLIC CHARACTER dotNetFocus():
    THIS-OBJECT:ActiveControl = textbox1.
    textbox1:Focus().
  END METHOD.

  METHOD PUBLIC CHARACTER dotNetDisplay():
    DEF VAR cEntityId AS CHAR NO-UNDO.
    DEF VAR ix        AS INT  NO-UNDO.
    DEF VAR cFileName AS CHAR NO-UNDO.

    bDisableContentChanged = YES.
    textbox1:Font = NEW System.Drawing.Font(cDefaultFont,iDefaultFontSize,System.Drawing.FontStyle:Regular).

    IF hFieldMap:AVAIL THEN DO:
      IF cDocLoadContext NE "" THEN DO:
        DO ix = 1 TO NUM-ENTRIES(cDocLoadIdFields):
          cEntityId = cEntityId + (IF ix > 1 THEN CHR(1) ELSE "") 
                    + (IF hFieldMap:BUFFER-FIELD(ENTRY(ix,cDocLoadIdFields)):BUFFER-VALUE NE ? THEN
                         STRING(hFieldMap:BUFFER-FIELD(ENTRY(ix,cDocLoadIdFields)):BUFFER-VALUE)
                       ELSE "").
        END.
        IF DYNAMIC-FUNCTION("ViewDocs",cDocLoadContext,cEntityId,NO,"") THEN DO:
          cFileName = DYNAMIC-FUNCTION("getTmpDocFileNames").
          textbox1:LoadDocument(cFileName).
        END.
        ELSE textbox1:Text = hFieldMap:BUFFER-FIELD(cFieldMapDisplay):BUFFER-VALUE.
      END.
      ELSE textbox1:Text = hFieldMap:BUFFER-FIELD(cFieldMapDisplay):BUFFER-VALUE.
    END.
    ELSE
      textbox1:Text = "".
    textbox1:ReadOnly = NOT hFieldMap:AVAIL.
    RETURN "".
  END.

  METHOD PUBLIC CHARACTER dotNetNew():
    IF CAN-QUERY(hPlaceholder,"initial") THEN
      textbox1:Text = hPlaceholder:INITIAL.
    ELSE textbox1:Text = hFieldMap:BUFFER-FIELD(cFieldMapDisplay):INITIAL.
  END METHOD.
  
  METHOD PUBLIC CHARACTER dotNetPreSave():
         /* Invoked from jboxuilib.SaveRecord before record i saved so input data from the dot.net control
            may be added to the dynamic save operation. */
    DEF VAR hWidget          AS HANDLE NO-UNDO.
    DEF VAR cScreenUpdFields AS CHAR   NO-UNDO.

    cScreenUpdFields = DYNAMIC-FUNCTION("getAttribute",hFieldMap,"screenUpdateFields").

    IF CAN-DO(cScreenUpdFields,cFieldMapDisplay) THEN DO:
      hWidget = WIDGET-HANDLE(ENTRY(LOOKUP(cFieldMapDisplay,cScreenUpdFields),DYNAMIC-FUNCTION("getAttribute",hFieldMap,"screenUpdateWidgets"))) NO-ERROR.
      IF VALID-HANDLE(hWidget) THEN hWidget:SCREEN-VALUE = textbox1:Text.
    END.

  END METHOD.

  METHOD PUBLIC CHARACTER dotNetSave():
    DEF VAR cFileName  AS CHAR NO-UNDO.
    DEF VAR oDocFormat AS CLASS DevExpress.XtraRichEdit.DocumentFormat NO-UNDO.
    DEF VAR iFormat    AS INT  NO-UNDO.
    DEF VAR cEntityId  AS CHAR NO-UNDO.
    DEF VAR ix         AS INT  NO-UNDO.

    /* 1: text, 2: rtf, 3: html */
    iFormat = IF cDocLoadFormat = "rtf" THEN 2 ELSE IF cDocLoadFormat = "html" THEN 3 ELSE 1.

    IF textbox1:MODIFIED AND iFormat > 1 THEN DO:

      cFileName = DYNAMIC-FUNCTION("getUniqueFileName").
      cFileName = REPLACE(cFileName,".tmp","." + cDocLoadFormat).
      oDocFormat = NEW DevExpress.XtraRichEdit.DocumentFormat(iFormat).
      textbox1:SaveDocument(cFileName,oDocFormat).
      DO ix = 1 TO NUM-ENTRIES(cDocLoadIdFields):
        cEntityId = cEntityId + (IF ix > 1 THEN CHR(1) ELSE "") 
                  + (IF hFieldMap:BUFFER-FIELD(ENTRY(ix,cDocLoadIdFields)):BUFFER-VALUE NE ? THEN
                       STRING(hFieldMap:BUFFER-FIELD(ENTRY(ix,cDocLoadIdFields)):BUFFER-VALUE)
                     ELSE "").
      END.
      DYNAMIC-FUNCTION("setDocLoadParam","replacedoc").
      DYNAMIC-FUNCTION("LoadDocs",cFileName,cDocLoadContext,cEntityId,cDocLoadContext).
    END.
    RETURN "".
  END.

  /* (Semi-)class specific methods - event handlers: */
  METHOD PRIVATE VOID handler_ContentChanged (sender AS CLASS System.Object, e AS CLASS System.EventArgs):
    IF NOT bDisableContentChanged THEN 
      RUN InvokeMethod IN hWinSuper (hPlaceHolder,"ContentChanged").
    bDisableContentChanged = NO.
  END METHOD. 

  /* Class specific methods: */

  METHOD PUBLIC VOID setDocLoadContext (icDocLoadContext AS CHAR):
    cDocLoadContext = icDocLoadContext.
  END METHOD.

  METHOD PUBLIC VOID setDocLoadType (icDocLoadFormat AS CHAR):
    cDocLoadFormat = icDocLoadFormat.
  END METHOD.

  METHOD PUBLIC VOID setDocLoadIdFields (icDocLoadIdFields AS CHAR):
    cDocLoadIdFields = icDocLoadIdFields.
  END METHOD.

  METHOD PUBLIC VOID DisableContentChanged (ibDisableContentChanged AS LOG):
    IF textbox1:Parent:Parent:VISIBLE THEN
      bDisableContentChanged = ibDisableContentChanged.
  END METHOD.

  METHOD PUBLIC INTEGER getFontHeight():
    RETURN THIS-OBJECT:FontHeight.
  END METHOD.

  METHOD PUBLIC CHARACTER BulletsDevExEdit():
/*     textbox1:SelectionBullet = YES. */
    RETURN "".
  END.

  METHOD PUBLIC CHARACTER CopyDevExEdit():
    textbox1:Copy().
    RETURN "".
  END METHOD.

  METHOD PUBLIC LOGICAL Print():
/*    MESSAGE valid-object(textbox1) skip*/
/*            textbox1:ToString()        */
/*    VIEW-AS ALERT-BOX.                 */
/*    textbox1:Print().                  */
    RETURN YES.
  END METHOD.

  METHOD PUBLIC CHARACTER FontDevExEdit():
    DEF VAR cLabel AS CHAR NO-UNDO.
  
/*     cLabel = oJBoxContextMenu:getSelectedMenuLabel().                                   */
/*     IF cLabel NE "" THEN DO:                                                            */
/*       IF textbox1:SelectedText NE "" THEN                                               */
/*         textbox1:SelectionFont = NEW System.Drawing.Font(cLabel,THIS-OBJECT:Font:Size). */
/*       ELSE                                                                              */
/*         textbox1:Font = NEW System.Drawing.Font(cLabel,THIS-OBJECT:Font:Size).          */
/*     END.                                                                                */

    RETURN "".

  END METHOD.

  METHOD PUBLIC CHARACTER FontSizeDevExEdit():
    DEF VAR iFontSize AS INT NO-UNDO.
  
    /*
    iFontSize = INTEGER(oJBoxContextMenu:getSelectedMenuLabel()) NO-ERROR.
    IF NOT ERROR-STATUS:ERROR AND iFontSize > 0 THEN DO:
      IF textbox1:SelectedText NE "" THEN 
        textbox1:SelectionFont = NEW System.Drawing.Font("",iFontSize).
      ELSE
        textbox1:Font = NEW System.Drawing.Font("",iFontSize).
    END.
    */
    RETURN "".
  END METHOD.

  METHOD PUBLIC CHARACTER FontStyleDevExEdit():
    DEF VAR cFontStyle AS CHAR NO-UNDO.
    DEF VAR oFontStyle AS CLASS System.Drawing.FontStyle NO-UNDO.
    DEF VAR cl_style   AS System.Enum NO-UNDO.

    /*
    cFontStyle = oJBoxContextMenu:getSelectedMenuLabel().
    IF NOT ERROR-STATUS:ERROR AND cFontStyle NE "" THEN DO:
      cl_style = mFontStyle(cFontStyle).

      oFontStyle = CAST(cl_style,System.Drawing.FontStyle).
      IF textbox1:SelectedText NE "" THEN 
        textbox1:SelectionFont = NEW System.Drawing.Font("",textbox1:Font:Size,oFontStyle).
      ELSE
        textbox1:Font = NEW System.Drawing.Font("",textbox1:Font:Size,oFontStyle).
    END.
    */
    RETURN "".
  END METHOD.

  METHOD PUBLIC CHARACTER ContentChangedDevExEdit():
    DEF VAR hToolbar AS HANDLE NO-UNDO.
  
    IF VALID-HANDLE(hFieldMap) AND textbox1:Modified THEN DO:
      hToolbar = DYNAMIC-FUNCTION("GetLinkedObject",hFieldMap,"toolbar","from").
      IF VALID-HANDLE(hToolbar) THEN
        DYNAMIC-FUNCTION("setToolbar",hToolbar,"modified").
    END.

    bDisableContentChanged = NO.
    RETURN "".
  END METHOD.

  METHOD PUBLIC CHARACTER UndoDevExEdit():
/*     textbox1:UNDO(). */
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

    /*
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
   */
   /* same thing for other Font Styles */
    RETURN cl_style.

  END METHOD.

	DESTRUCTOR PUBLIC JBoxDevExEdit ( ):

		IF VALID-OBJECT(components) THEN DO:
			CAST(components, System.IDisposable):Dispose().
		END.

	END DESTRUCTOR.
END CLASS.
