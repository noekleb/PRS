 
 /*------------------------------------------------------------------------
    File        : Telerik rtf editor
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Brynjar
    Created     : Fri Jan 17 14:46:33 CEST 2017
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.
USING Progress.Windows.UserControl.
USING Progress.Windows.Form.
USING uc.* FROM PROPATH.


ROUTINE-LEVEL ON ERROR UNDO, THROW.

CLASS uc.JBoxRadRtfEdit INHERITS uc.JBoxUCbase: 

  DEFINE PRIVATE VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.
  DEFINE PRIVATE VARIABLE radRichTextEditor1 AS Telerik.WinControls.UI.RadRichTextEditor NO-UNDO.
  DEFINE PRIVATE VARIABLE richTextEditorRibbonBar1 AS Telerik.WinControls.UI.RichTextEditorRibbonBar NO-UNDO.
  DEF VAR format1 AS Telerik.WinForms.Documents.FormatProviders.Txt.TxtFormatProvider NO-UNDO.
  DEF VAR document1 AS Telerik.WinControls.UI.RadPrintDocument NO-UNDO.
  DEF VAR stream1 AS System.IO.FileStream NO-UNDO.
  DEF VAR font1   AS System.Drawing.Font NO-UNDO.
  DEF VAR edit1 AS CLASS Telerik.WinForms.Documents.Model.RadDocumentEditor NO-UNDO.
  DEF VAR marg1 AS CLASS Telerik.WinForms.Documents.Layout.Padding NO-UNDO.
    		
  CONSTRUCTOR PUBLIC JBoxRadRtfEdit (  INPUT ihWinSuper    AS HANDLE, /* Window super proc - unknown if object created from window */
                                    INPUT ihWinProc     AS HANDLE,
                                    INPUT ihPlaceHolder AS HANDLE, /* Design widget (rectangle, field, etc) */
                                    INPUT iiDeltaWidth  AS INTEGER):
     SUPER(ihWinSuper,ihWinProc,ihPlaceHolder,iiDeltaWidth).
   
  END CONSTRUCTOR.
  
  
  CONSTRUCTOR PUBLIC JBoxRadRtfEdit ( INPUT ihWinProc     AS HANDLE,
                                      INPUT ihPlaceHolder AS HANDLE ): /* Design widget (rectangle, field, etc) */
                                   
     SUPER(?,ihWinProc,ihPlaceHolder,0).
   
  END CONSTRUCTOR.


METHOD PUBLIC OVERRIDE VOID InitializeComponent(  ):
		
    /* NOTE: The following method is automatically generated.
    
    We strongly suggest that the contents of this method only be modified using the
    Visual Designer to avoid any incompatible modifications.
    
    Modifying the contents of this method using a code editor will invalidate any support for this file. */
    THIS-OBJECT:radRichTextEditor1 = NEW Telerik.WinControls.UI.RadRichTextEditor().
    THIS-OBJECT:richTextEditorRibbonBar1 = NEW Telerik.WinControls.UI.RichTextEditorRibbonBar().
    CAST(THIS-OBJECT:radRichTextEditor1, System.ComponentModel.ISupportInitialize):BeginInit().
    CAST(THIS-OBJECT:richTextEditorRibbonBar1, System.ComponentModel.ISupportInitialize):BeginInit().
    THIS-OBJECT:SuspendLayout().
    /*  */
    /* radRichTextEditor1 */
    /*  */
    THIS-OBJECT:radRichTextEditor1:BorderColor = System.Drawing.Color:FromArgb(System.Convert:ToInt32(System.Convert:ToByte(156)), System.Convert:ToInt32(System.Convert:ToByte(189)), System.Convert:ToInt32(System.Convert:ToByte(232))).
    THIS-OBJECT:radRichTextEditor1:CaretWidth = System.Single:NaN.
    THIS-OBJECT:radRichTextEditor1:Location = NEW System.Drawing.Point(0, 176).
    THIS-OBJECT:radRichTextEditor1:Name = "radRichTextEditor1".
    THIS-OBJECT:radRichTextEditor1:SelectionFill = System.Drawing.Color:FromArgb(System.Convert:ToInt32(System.Convert:ToByte(128)), System.Convert:ToInt32(System.Convert:ToByte(78)), System.Convert:ToInt32(System.Convert:ToByte(158)), System.Convert:ToInt32(System.Convert:ToByte(255))).
    THIS-OBJECT:radRichTextEditor1:Size = NEW System.Drawing.Size(833, 401).
    THIS-OBJECT:radRichTextEditor1:TabIndex = 0.
    /*  */
    /* richTextEditorRibbonBar1 */
    /*  */
    THIS-OBJECT:richTextEditorRibbonBar1:ApplicationMenuStyle = Telerik.WinControls.UI.ApplicationMenuStyle:BackstageView.
    THIS-OBJECT:richTextEditorRibbonBar1:AssociatedRichTextEditor = THIS-OBJECT:radRichTextEditor1.
    THIS-OBJECT:richTextEditorRibbonBar1:BuiltInStylesVersion = Telerik.WinForms.Documents.Model.Styles.BuiltInStylesVersion:Office2013.
    /*  */
    /*  */
    /*  */
    THIS-OBJECT:richTextEditorRibbonBar1:ExitButton:Text = "Exit".
    THIS-OBJECT:richTextEditorRibbonBar1:Location = NEW System.Drawing.Point(0, 0).
    THIS-OBJECT:richTextEditorRibbonBar1:Name = "richTextEditorRibbonBar1".
    /*  */
    /*  */
    /*  */
    THIS-OBJECT:richTextEditorRibbonBar1:OptionsButton:Text = "Options".
    THIS-OBJECT:richTextEditorRibbonBar1:Size = NEW System.Drawing.Size(833, 174).
    THIS-OBJECT:richTextEditorRibbonBar1:TabIndex = 1.
    THIS-OBJECT:richTextEditorRibbonBar1:TabStop = FALSE.
    THIS-OBJECT:richTextEditorRibbonBar1:Text = "PrintDoc".
    /*  */
    /* PrintDoc */
    /*  */
    THIS-OBJECT:ClientSize = NEW System.Drawing.Size(833, 577).
    THIS-OBJECT:Controls:Add(THIS-OBJECT:richTextEditorRibbonBar1).
    THIS-OBJECT:Controls:Add(THIS-OBJECT:radRichTextEditor1).
    THIS-OBJECT:Name = "PrintDoc".
    THIS-OBJECT:Text = "PrintDoc".
    CAST(THIS-OBJECT:radRichTextEditor1, System.ComponentModel.ISupportInitialize):EndInit().
    CAST(THIS-OBJECT:richTextEditorRibbonBar1, System.ComponentModel.ISupportInitialize):EndInit().
    THIS-OBJECT:ResumeLayout(FALSE).
    CATCH e AS Progress.Lang.Error:
      UNDO, THROW e.
    END CATCH.
END METHOD.

/*
	/*------------------------------------------------------------------------------
			Purpose:  																	  
			Notes:  																	  
	------------------------------------------------------------------------------*/
	METHOD PRIVATE VOID webBrowser1_DocumentCompleted( INPUT sender AS System.Object, INPUT e AS System.Windows.Forms.WebBrowserDocumentCompletedEventArgs ):
        /* MESSAGE sender e:URL VIEW-AS ALERT-BOX. */
        DYNAMIC-FUNCTION('WebDocumentLoaded') NO-ERROR. 
		RETURN.
	END METHOD.


	METHOD PUBLIC VOID setURI(INPUT ipcURI AS CHAR ):
        THIS-OBJECT:webBrowser1:Url = NEW System.Uri(ipcURI, System.UriKind:Absolute).
  END. 
*/

/* Mandatory methods: */
  METHOD PUBLIC Telerik.WinControls.UI.RadRichTextEditor getDotNetWidget():
    RETURN radRichTextEditor1.
  END METHOD.

  METHOD PUBLIC CHARACTER getDotNetWidgetType():
    RETURN STRING(radRichTextEditor1:GetType()).
  END METHOD.

  METHOD PUBLIC CHARACTER DEFAULT():
    RETURN DYNAMIC-INVOKE(THIS-OBJECT,ENTRY(1,PROGRAM-NAME(2)," ")).
  END METHOD.


	DESTRUCTOR PUBLIC JBoxRadRtfEdit ( ):

		IF VALID-OBJECT(components) THEN DO:
			CAST(components, System.IDisposable):Dispose().
		END.

	END DESTRUCTOR.

END CLASS.
