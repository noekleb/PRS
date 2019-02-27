 
 /*------------------------------------------------------------------------
    File        : dataaccessform
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Brynjar
    Created     : Mon Nov 10 11:45:03 CET 2014
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.
USING Progress.Windows.Form.

BLOCK-LEVEL ON ERROR UNDO, THROW.

CLASS dataaccessform INHERITS Form: 
	
    DEFINE PRIVATE VARIABLE bindingSource1 AS Progress.Data.BindingSource NO-UNDO.
	DEFINE PRIVATE VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.
    DEFINE PRIVATE VARIABLE windowContainer1 AS Progress.Windows.WindowContainer NO-UNDO.

		
	CONSTRUCTOR PUBLIC dataaccessform (  ):
		
		
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
        THIS-OBJECT:components = NEW System.ComponentModel.Container().
        THIS-OBJECT:bindingSource1 = NEW Progress.Data.BindingSource(THIS-OBJECT:components).
        THIS-OBJECT:windowContainer1 = NEW Progress.Windows.WindowContainer().
        CAST(THIS-OBJECT:bindingSource1, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:SuspendLayout().
        /*  */
        /* bindingSource1 */
        /*  */
        THIS-OBJECT:bindingSource1:MaxDataGuess = 0.
        THIS-OBJECT:bindingSource1:NoLOBs = FALSE.
        THIS-OBJECT:bindingSource1:TableSchema = ?.
        /*  */
        /* windowContainer1 */
        /*  */
        THIS-OBJECT:windowContainer1:Location = NEW System.Drawing.Point(115, 53).
        THIS-OBJECT:windowContainer1:Name = "windowContainer1".
        THIS-OBJECT:windowContainer1:Size = NEW System.Drawing.Size(100, 100).
        THIS-OBJECT:windowContainer1:TabIndex = 0.
        /*  */
        /* dataaccessform */
        /*  */
        THIS-OBJECT:ClientSize = NEW System.Drawing.Size(292, 266).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:windowContainer1).
        THIS-OBJECT:Name = "dataaccessform".
        THIS-OBJECT:Text = "dataaccessform".
        CAST(THIS-OBJECT:bindingSource1, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:ResumeLayout(FALSE).
        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.
	END METHOD.

	DESTRUCTOR PUBLIC dataaccessform ( ):

	END DESTRUCTOR.

END CLASS.