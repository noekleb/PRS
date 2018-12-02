USING Progress.Lang.*.
USING Progress.Windows.UserControl.

ROUTINE-LEVEL ON ERROR UNDO, THROW.

CLASS JBoxImageViewer INHERITS UserControl: 
	
  DEFINE PRIVATE VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.
  DEFINE PRIVATE VARIABLE pictureBox1 AS System.Windows.Forms.PictureBox NO-UNDO.

		
  CONSTRUCTOR PUBLIC JBoxImageViewer (  ):
		
		
    SUPER().
    InitializeComponent().
/*     THIS-OBJECT:ComponentsCollection:ADD(THIS-OBJECT:components). */
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
    resources = NEW Progress.Util.ResourceManager("testImg").
    THIS-OBJECT:pictureBox1 = NEW System.Windows.Forms.PictureBox().
    CAST(THIS-OBJECT:pictureBox1, System.ComponentModel.ISupportInitialize):BeginInit().
    THIS-OBJECT:SuspendLayout().
    /*  */
    /* pictureBox1 */
    /*  */
    THIS-OBJECT:pictureBox1:Image = CAST(resources:GetObject("pictureBox1.Image"), System.Drawing.Image).
    THIS-OBJECT:pictureBox1:Location = NEW System.Drawing.Point(2, 1).
    THIS-OBJECT:pictureBox1:Name = "pictureBox1".
    THIS-OBJECT:pictureBox1:Size = NEW System.Drawing.Size(120, 95).
    THIS-OBJECT:pictureBox1:SizeMode = System.Windows.Forms.PictureBoxSizeMode:StretchImage.
    THIS-OBJECT:pictureBox1:TabIndex = 0.
    THIS-OBJECT:pictureBox1:TabStop = FALSE.
    /*  */
    /* testImg */
    /*  */
    THIS-OBJECT:ClientSize = NEW System.Drawing.Size(125, 98).
    THIS-OBJECT:Controls:Add(THIS-OBJECT:pictureBox1).
    THIS-OBJECT:Name = "testImg".
    THIS-OBJECT:Text = "testImg".
    CAST(THIS-OBJECT:pictureBox1, System.ComponentModel.ISupportInitialize):EndInit().
    THIS-OBJECT:ResumeLayout(FALSE).
    CATCH e AS Progress.Lang.Error:
      UNDO, THROW e.
    END CATCH.
  END METHOD.

  DESTRUCTOR PUBLIC JBoxImageViewer ( ):

		IF VALID-OBJECT(components) THEN DO:
			CAST(components, System.IDisposable):Dispose().
		END.

  END DESTRUCTOR.
END CLASS.
