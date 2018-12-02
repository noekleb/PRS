 
 /*------------------------------------------------------------------------
    File        : UltraTreeTest
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Brynjar
    Created     : Tue Aug 12 10:36:42 CEST 2014
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.
USING Progress.Windows.Form.



CLASS winsrc.samples.pug.UltraTreeTest INHERITS Form: 
	
	DEFINE PRIVATE VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.
  DEFINE PRIVATE VARIABLE ultraListView1 AS Infragistics.Win.UltraWinListView.UltraListView NO-UNDO.

		
	CONSTRUCTOR PUBLIC UltraTreeTest (  ):
		
		
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
    THIS-OBJECT:ultraListView1 = NEW Infragistics.Win.UltraWinListView.UltraListView().
    CAST(THIS-OBJECT:ultraListView1, System.ComponentModel.ISupportInitialize):BeginInit().
    THIS-OBJECT:SuspendLayout().
    /*  */
    /* ultraListView1 */
    /*  */
    THIS-OBJECT:ultraListView1:Location = NEW System.Drawing.Point(24, 60).
    THIS-OBJECT:ultraListView1:Name = "ultraListView1".
    THIS-OBJECT:ultraListView1:Size = NEW System.Drawing.Size(348, 184).
    THIS-OBJECT:ultraListView1:TabIndex = 0.
    THIS-OBJECT:ultraListView1:Text = "ultraListView1".
    /*  */
    /* UltraTreeTest */
    /*  */
    THIS-OBJECT:ClientSize = NEW System.Drawing.Size(384, 266).
    THIS-OBJECT:Controls:Add(THIS-OBJECT:ultraListView1).
    THIS-OBJECT:Name = "UltraTreeTest".
    THIS-OBJECT:Text = "UltraTreeTest".
    CAST(THIS-OBJECT:ultraListView1, System.ComponentModel.ISupportInitialize):EndInit().
    THIS-OBJECT:ResumeLayout(FALSE).
    CATCH e AS Progress.Lang.Error:
      UNDO, THROW e.
    END CATCH.
	END METHOD.


	DESTRUCTOR PUBLIC UltraTreeTest ( ):

	END DESTRUCTOR.

END CLASS.