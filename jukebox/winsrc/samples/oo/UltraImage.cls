 
 /*------------------------------------------------------------------------
    File        : UltraImage
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Brynjar
    Created     : Mon Jun 16 15:26:18 CEST 2014
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.
USING Progress.Windows.Form.



CLASS winsrc.samples.oo.UltraImage INHERITS Form: 
	
	DEFINE PRIVATE VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.
  DEFINE PRIVATE VARIABLE ultraPictureBox1 AS Infragistics.Win.UltraWinEditors.UltraPictureBox NO-UNDO.

		
	CONSTRUCTOR PUBLIC UltraImage (  ):
		
		
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
    @VisualDesigner.FormMember (NeedsInitialize="true").
    DEFINE VARIABLE resources AS Progress.Util.ResourceManager NO-UNDO.
    resources = NEW Progress.Util.ResourceManager("winsrc.samples.oo.UltraImage").
    THIS-OBJECT:ultraPictureBox1 = NEW Infragistics.Win.UltraWinEditors.UltraPictureBox().
    THIS-OBJECT:SuspendLayout().
    /*  */
    /* ultraPictureBox1 */
    /*  */
    THIS-OBJECT:ultraPictureBox1:AutoSize = TRUE.
    THIS-OBJECT:ultraPictureBox1:BorderShadowColor = System.Drawing.Color:Empty.
    THIS-OBJECT:ultraPictureBox1:Image = CAST(resources:GetObject("ultraPictureBox1.Image"), System.Object).
    THIS-OBJECT:ultraPictureBox1:Location = NEW System.Drawing.Point(47, 57).
    THIS-OBJECT:ultraPictureBox1:Name = "ultraPictureBox1".
    THIS-OBJECT:ultraPictureBox1:Size = NEW System.Drawing.Size(375, 271).
    THIS-OBJECT:ultraPictureBox1:TabIndex = 0.
    /*  */
    /* UltraImage */
    /*  */
    THIS-OBJECT:ClientSize = NEW System.Drawing.Size(474, 375).
    THIS-OBJECT:Controls:Add(THIS-OBJECT:ultraPictureBox1).
    THIS-OBJECT:Name = "UltraImage".
    THIS-OBJECT:Text = "UltraImage".
    THIS-OBJECT:ResumeLayout(FALSE).
    THIS-OBJECT:PerformLayout().
    CATCH e AS Progress.Lang.Error:
      UNDO, THROW e.
    END CATCH.
	END METHOD.

	DESTRUCTOR PUBLIC UltraImage ( ):

	END DESTRUCTOR.

END CLASS.