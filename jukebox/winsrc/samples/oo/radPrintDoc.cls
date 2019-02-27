 
 /*------------------------------------------------------------------------
    File        : radPrintDoc
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Brynjar
    Created     : Fri Jan 06 00:14:07 CET 2017
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.
USING Progress.Windows.Form.


CLASS radPrintDoc INHERITS Form: 
	
	DEFINE PRIVATE VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.
  DEFINE PRIVATE VARIABLE radRichTextEditor1 AS Telerik.WinControls.UI.RadRichTextEditor NO-UNDO.
  DEFINE PRIVATE VARIABLE radPrintDocument1 AS Telerik.WinControls.UI.RadPrintDocument NO-UNDO.
  DEF VAR format1 AS Telerik.WinForms.Documents.FormatProviders.Txt.TxtFormatProvider NO-UNDO.
  DEF VAR document1 AS Telerik.WinControls.UI.RadPrintDocument NO-UNDO.
  DEF VAR stream1 AS System.IO.FileStream NO-UNDO.
  DEF VAR font1   AS System.Drawing.Font NO-UNDO.
  DEF VAR edit1 AS CLASS Telerik.WinForms.Documents.Model.RadDocumentEditor NO-UNDO.
  DEF VAR marg1 AS CLASS Telerik.WinForms.Documents.Layout.Padding NO-UNDO.

		
	CONSTRUCTOR PUBLIC radPrintDoc (  ):
		
		
    SUPER().
    InitializeComponent().
    THIS-OBJECT:ComponentsCollection:ADD(THIS-OBJECT:components).
    
    format1 = NEW Telerik.WinForms.Documents.FormatProviders.Txt.TxtFormatProvider().
    stream1 = NEW System.IO.FileStream("t:\prowrk\feste\data\FI-12358-kvittliste.txt",System.IO.FileMode:Open).
    
    font1 = NEW System.Drawing.Font("Courier New", 12).
    marg1 = NEW Telerik.WinForms.Documents.Layout.Padding(20,20,20,20).

    radPrintDocument1:PrinterSettings:DefaultPageSettings:Landscape = true.
    radPrintDocument1:Landscape = true.

    radRichTextEditor1:BeginInit().

    radRichTextEditor1:Document = format1:Import(stream1).

    edit1 = NEW Telerik.WinForms.Documents.Model.RadDocumentEditor(radRichTextEditor1:Document).
    radRichTextEditor1:Document:Selection:SelectAll().
    edit1:ChangeFontFamily(new Telerik.WinControls.RichTextEditor.UI.FontFamily("Courier New")).
    edit1:ChangeFontSize(12).
    edit1:ChangeSectionActualPageMargin(marg1).
    edit1:ChangeSectionPageOrientation(Telerik.WinForms.Documents.Model.PageOrientation:Landscape).
    radRichTextEditor1:Document:Selection:Clear().
    
    radPrintDocument1:AssociatedObject = radRichTextEditor1.
    THIS-OBJECT:radRichTextEditor1:PrintPreview().
    
    CATCH e AS Progress.Lang.Error:
      UNDO, THROW e.
    END CATCH.

	END CONSTRUCTOR.

	METHOD PRIVATE VOID InitializeComponent(  ):
		
    /* NOTE: The following method is automatically generated.
    
    We strongly suggest that the contents of this method only be modified using the
    Visual Designer to avoid any incompatible modifications.
    
    Modifying the contents of this method using a code editor will invalidate any support for this file. */
    @VisualDesigner.FormMember (NeedsInitialize="true").
    DEFINE VARIABLE radPrintWatermark1 AS Telerik.WinControls.UI.RadPrintWatermark NO-UNDO.
    radPrintWatermark1 = NEW Telerik.WinControls.UI.RadPrintWatermark().
    THIS-OBJECT:radRichTextEditor1 = NEW Telerik.WinControls.UI.RadRichTextEditor().
    THIS-OBJECT:radPrintDocument1 = NEW Telerik.WinControls.UI.RadPrintDocument().
    CAST(THIS-OBJECT:radRichTextEditor1, System.ComponentModel.ISupportInitialize):BeginInit().
    THIS-OBJECT:SuspendLayout().
    /*  */
    /* radRichTextEditor1 */
    /*  */
    THIS-OBJECT:radRichTextEditor1:BorderColor = System.Drawing.Color:FromArgb(System.Convert:ToInt32(System.Convert:ToByte(156)), System.Convert:ToInt32(System.Convert:ToByte(189)), System.Convert:ToInt32(System.Convert:ToByte(232))).
    THIS-OBJECT:radRichTextEditor1:CaretWidth = System.Single:NaN.
    THIS-OBJECT:radRichTextEditor1:Location = NEW System.Drawing.Point(12, 57).
    THIS-OBJECT:radRichTextEditor1:Name = "radRichTextEditor1".
    THIS-OBJECT:radRichTextEditor1:SelectionFill = System.Drawing.Color:FromArgb(System.Convert:ToInt32(System.Convert:ToByte(128)), System.Convert:ToInt32(System.Convert:ToByte(78)), System.Convert:ToInt32(System.Convert:ToByte(158)), System.Convert:ToInt32(System.Convert:ToByte(255))).
    THIS-OBJECT:radRichTextEditor1:Size = NEW System.Drawing.Size(268, 200).
    THIS-OBJECT:radRichTextEditor1:TabIndex = 0.
    /*  */
    /* radPrintDocument1 */
    /*  */
    THIS-OBJECT:radPrintDocument1:FooterFont = NEW System.Drawing.Font("Microsoft Sans Serif", Progress.Util.CastUtil:ToSingle(8.25), System.Drawing.FontStyle:Regular, System.Drawing.GraphicsUnit:Point, System.Convert:ToByte(0)).
    THIS-OBJECT:radPrintDocument1:HeaderFont = NEW System.Drawing.Font("Microsoft Sans Serif", Progress.Util.CastUtil:ToSingle(8.25), System.Drawing.FontStyle:Regular, System.Drawing.GraphicsUnit:Point, System.Convert:ToByte(0)).
    THIS-OBJECT:radPrintDocument1:Watermark = radPrintWatermark1.
    /*  */
    /* radPrintDoc */
    /*  */
    THIS-OBJECT:ClientSize = NEW System.Drawing.Size(292, 266).
    THIS-OBJECT:Controls:Add(THIS-OBJECT:radRichTextEditor1).
    THIS-OBJECT:Name = "radPrintDoc".
    THIS-OBJECT:Text = "radPrintDoc".
    CAST(THIS-OBJECT:radRichTextEditor1, System.ComponentModel.ISupportInitialize):EndInit().
    THIS-OBJECT:ResumeLayout(FALSE).
    CATCH e AS Progress.Lang.Error:
      UNDO, THROW e.
    END CATCH.
	END METHOD.

	DESTRUCTOR PUBLIC radPrintDoc ( ):

	END DESTRUCTOR.

END CLASS.