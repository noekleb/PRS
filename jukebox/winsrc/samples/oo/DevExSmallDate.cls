 
 /*------------------------------------------------------------------------
    File        : DevExSmallDate
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Brynjar
    Created     : Thu Mar 27 23:28:54 CET 2014
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.
USING Progress.Windows.Form.



CLASS winsrc.samples.oo.DevExSmallDate INHERITS Form: 
	
	DEFINE PRIVATE VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.
  DEFINE PRIVATE VARIABLE monthEdit1 AS DevExpress.XtraEditors.DateEdit NO-UNDO.

		
	CONSTRUCTOR PUBLIC DevExSmallDate (  ):
		
		
    SUPER().
    InitializeComponent().
    THIS-OBJECT:ComponentsCollection:ADD(THIS-OBJECT:components).
    CATCH e AS Progress.Lang.Error:
      UNDO, THROW e.
    END CATCH.

	END CONSTRUCTOR.

	METHOD PRIVATE VOID InitializeComponent(  ):
		
    /* NOTE: The following method is automatically generated.
    
    We strongly suggest that the contents of this method only be modified using the
    Visual Designer to avoid any incompatible modifications.
    
    Modifying the contents of this method using a code editor will invalidate any support for this file. */
    THIS-OBJECT:monthEdit1 = NEW DevExpress.XtraEditors.DateEdit().
    CAST(THIS-OBJECT:monthEdit1:Properties:VistaTimeProperties, System.ComponentModel.ISupportInitialize):BeginInit().
    CAST(THIS-OBJECT:monthEdit1:Properties, System.ComponentModel.ISupportInitialize):BeginInit().
    THIS-OBJECT:SuspendLayout().
    /*  */
    /* monthEdit1 */
    /*  */
    THIS-OBJECT:monthEdit1:EditValue = ?.
    THIS-OBJECT:monthEdit1:Location = NEW System.Drawing.Point(105, 82).
    THIS-OBJECT:monthEdit1:Margin = NEW System.Windows.Forms.Padding(4).
    THIS-OBJECT:monthEdit1:Name = "monthEdit1".
    @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
    DEFINE VARIABLE arrayvar0 AS DevExpress.XtraEditors.Controls.EditorButton EXTENT 1 NO-UNDO.
    arrayvar0[1] = NEW DevExpress.XtraEditors.Controls.EditorButton(DevExpress.XtraEditors.Controls.ButtonPredefines:Combo).
    THIS-OBJECT:monthEdit1:Properties:Buttons:AddRange(arrayvar0).
    THIS-OBJECT:monthEdit1:Properties:Mask:EditMask = "".
    THIS-OBJECT:monthEdit1:Properties:Mask:MaskType = DevExpress.XtraEditors.Mask.MaskType:None.
    THIS-OBJECT:monthEdit1:Properties:TextEditStyle = DevExpress.XtraEditors.Controls.TextEditStyles:DisableTextEditor.
    @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
    DEFINE VARIABLE arrayvar1 AS DevExpress.XtraEditors.Controls.EditorButton EXTENT 1 NO-UNDO.
    arrayvar1[1] = NEW DevExpress.XtraEditors.Controls.EditorButton().
    THIS-OBJECT:monthEdit1:Properties:VistaTimeProperties:Buttons:AddRange(arrayvar1).
    THIS-OBJECT:monthEdit1:RightToLeft = System.Windows.Forms.RightToLeft:Yes.
    THIS-OBJECT:monthEdit1:Size = NEW System.Drawing.Size(20, 20).
    THIS-OBJECT:monthEdit1:TabIndex = 0.
    /*  */
    /* DevExSmallDate */
    /*  */
    THIS-OBJECT:AutoSize = TRUE.
    THIS-OBJECT:ClientSize = NEW System.Drawing.Size(292, 266).
    THIS-OBJECT:Controls:Add(THIS-OBJECT:monthEdit1).
    THIS-OBJECT:Name = "DevExSmallDate".
    THIS-OBJECT:StartPosition = System.Windows.Forms.FormStartPosition:WindowsDefaultBounds.
    THIS-OBJECT:Text = "DevExSmallDate".
    CAST(THIS-OBJECT:monthEdit1:Properties:VistaTimeProperties, System.ComponentModel.ISupportInitialize):EndInit().
    CAST(THIS-OBJECT:monthEdit1:Properties, System.ComponentModel.ISupportInitialize):EndInit().
    THIS-OBJECT:ResumeLayout(FALSE).
    CATCH e AS Progress.Lang.Error:
      UNDO, THROW e.
    END CATCH.
	END METHOD.

	DESTRUCTOR PUBLIC DevExSmallDate ( ):

	END DESTRUCTOR.

END CLASS.