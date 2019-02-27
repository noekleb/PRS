 
 /*------------------------------------------------------------------------
    File        : PrintDoc
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Brynjar
    Created     : Thu Jan 05 13:24:14 CET 2017
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.
USING Progress.Windows.Form.


CLASS PrintDoc INHERITS Form: 
	
	DEFINE PRIVATE VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.
  DEF VAR PageSettings1 AS System.Drawing.Printing.PageSettings NO-UNDO.
  DEFINE PRIVATE VARIABLE radRichTextEditor1 AS Telerik.WinControls.UI.RadRichTextEditor NO-UNDO.
  DEFINE PRIVATE VARIABLE richTextEditorRibbonBar1 AS Telerik.WinControls.UI.RichTextEditorRibbonBar NO-UNDO.
  DEF VAR format1 AS Telerik.WinForms.Documents.FormatProviders.Txt.TxtFormatProvider NO-UNDO.
  DEF VAR document1 AS Telerik.WinControls.UI.RadPrintDocument NO-UNDO.
  DEF VAR stream1 AS System.IO.FileStream NO-UNDO.
  DEF VAR font1   AS System.Drawing.Font NO-UNDO.
  DEF VAR edit1 AS CLASS Telerik.WinForms.Documents.Model.RadDocumentEditor NO-UNDO.
  DEF VAR marg1 AS CLASS Telerik.WinForms.Documents.Layout.Padding NO-UNDO.
		
	CONSTRUCTOR PUBLIC PrintDoc (  ):
		
		
    SUPER().
    InitializeComponent().
    THIS-OBJECT:ComponentsCollection:ADD(THIS-OBJECT:components).
    
    format1 = NEW Telerik.WinForms.Documents.FormatProviders.Txt.TxtFormatProvider().
    stream1 = NEW System.IO.FileStream("t:\prowrk\feste\data\FI-12358-kvittliste.txt",System.IO.FileMode:Open).
/*    stream1 = NEW System.IO.FileStream("t:\prowrk\feste\data\FI-11843-kvittliste.txt",System.IO.FileMode:Open).*/
    
    font1 = NEW System.Drawing.Font("Courier New", 12).
    marg1 = NEW Telerik.WinForms.Documents.Layout.Padding(20,20,20,20).
    document1 = NEW Telerik.WinControls.UI.RadPrintDocument().
    document1:PrinterSettings:DefaultPageSettings:Landscape = true.
    document1:Landscape = true.

    radRichTextEditor1:BeginInit().

    radRichTextEditor1:Document = format1:Import(stream1).

    edit1 = NEW Telerik.WinForms.Documents.Model.RadDocumentEditor(radRichTextEditor1:Document).
    radRichTextEditor1:Document:Selection:SelectAll().
    edit1:ChangeFontFamily(new Telerik.WinControls.RichTextEditor.UI.FontFamily("Courier New")).
    edit1:ChangeFontSize(12).
    edit1:ChangeSectionActualPageMargin(marg1).
    edit1:ChangeSectionPageOrientation(Telerik.WinForms.Documents.Model.PageOrientation:Landscape).
    radRichTextEditor1:Document:Selection:Clear().

/*    radRichTextEditor1:RichTextBoxElement:ChangeFontFamily(new Telerik.WinControls.RichTextEditor.UI.FontFamily("Courier New")).*/
/*    radRichTextEditor1:RichTextBoxElement:ChangeFontSize(new Telerik.WinControls.RichTextBox.Model.Unit.PointToDip(12)).*/
/*    radRichTextEditor1:ChangeFontStyle(Telerik.WinControls.RichTextEditor.UI.FontStyles.Italic).*/
/*    radRichTextEditor1:ChangeFontWeight(Telerik.WinControls.RichTextEditor.UI.FontWeights.Bold).*/

/*    radRichTextEditor1:DocumentInheritsDefaultStyleSettings = true.*/


/*    radRichTextEditor1:Font = font1.*/

    
    radRichTextEditor1:EndInit().

    document1:AssociatedObject = radRichTextEditor1.
    
    
    radRichTextEditor1:PrintPreview(document1).
    
    CATCH e AS Progress.Lang.Error:
      UNDO, THROW e.
    END CATCH.

	END CONSTRUCTOR.

	METHOD PRIVATE VOID InitializeComponent(  ):
		
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
    THIS-OBJECT:richTextEditorRibbonBar1:Size = NEW System.Drawing.Size(833, 144).
    THIS-OBJECT:richTextEditorRibbonBar1:TabIndex = 1.
    THIS-OBJECT:richTextEditorRibbonBar1:TabStop = FALSE.
    THIS-OBJECT:richTextEditorRibbonBar1:Text = "PrintDoc".
    CAST(THIS-OBJECT:richTextEditorRibbonBar1:GetChildAt(0):GetChildAt(2), Telerik.WinControls.UI.RadQuickAccessToolBar):Enabled = FALSE.
    CAST(THIS-OBJECT:richTextEditorRibbonBar1:GetChildAt(0):GetChildAt(2), Telerik.WinControls.UI.RadQuickAccessToolBar):Visibility = Telerik.WinControls.ElementVisibility:Collapsed.
    CAST(THIS-OBJECT:richTextEditorRibbonBar1:GetChildAt(0):GetChildAt(3), Telerik.WinControls.UI.RadRibbonBarCaption):Enabled = FALSE.
    CAST(THIS-OBJECT:richTextEditorRibbonBar1:GetChildAt(0):GetChildAt(3), Telerik.WinControls.UI.RadRibbonBarCaption):Visibility = Telerik.WinControls.ElementVisibility:Collapsed.
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
    THIS-OBJECT:PerformLayout().
    CATCH e AS Progress.Lang.Error:
      UNDO, THROW e.
    END CATCH.
	END METHOD.

	DESTRUCTOR PUBLIC PrintDoc ( ):

	END DESTRUCTOR.

END CLASS.