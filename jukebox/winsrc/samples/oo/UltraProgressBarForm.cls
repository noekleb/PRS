 
 /*------------------------------------------------------------------------
    File        : UltraProgressBarForm
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Brynjar
    Created     : Wed Apr 27 09:59:13 CEST 2016
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.
USING Progress.Windows.Form.


CLASS UltraProgressBarForm INHERITS Form: 
	
	DEFINE PRIVATE VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.
  DEFINE PRIVATE VARIABLE ultraProgressBar1 AS Infragistics.Win.UltraWinProgressBar.UltraProgressBar NO-UNDO.

		
	CONSTRUCTOR PUBLIC UltraProgressBarForm (  ):
		DEF VAR ix AS INT NO-UNDO.
		
    SUPER().
    InitializeComponent().
    THIS-OBJECT:ComponentsCollection:ADD(THIS-OBJECT:components).
    ultraProgressBar1:VALUE = 0.
    ultraProgressBar1:show(). 
    ultraProgressBar1:VISIBLE = FALSE. 
    ultraProgressBar1:ENABLED = TRUE. 
    ultraProgressBar1:step = 1.
    
    DO ix = ultraProgressBar1:VALUE TO 10 WHILE ultraProgressBar1:VALUE LE 10:
        /*ultraProgressBar1:VALUE = iValue. */
        PAUSE 1.01 NO-MESSAGE. 
        ultraProgressBar1:performstep(). 
    END.
    
    
    CATCH e AS Progress.Lang.Error:
      UNDO, THROW e.
    END CATCH.

	END CONSTRUCTOR.

	METHOD PRIVATE VOID InitializeComponent(  ):
		
    /* NOTE: The following method is automatically generated.
    
    We strongly suggest that the contents of this method only be modified using the
    Visual Designer to avoid any incompatible modifications.
    
    Modifying the contents of this method using a code editor will invalidate any support for this file. */
    THIS-OBJECT:ultraProgressBar1 = NEW Infragistics.Win.UltraWinProgressBar.UltraProgressBar().
    THIS-OBJECT:SuspendLayout().
    /*  */
    /* ultraProgressBar1 */
    /*  */
    THIS-OBJECT:ultraProgressBar1:Location = NEW System.Drawing.Point(0, 0).
    THIS-OBJECT:ultraProgressBar1:Name = "ultraProgressBar1".
    THIS-OBJECT:ultraProgressBar1:TabIndex = 0.
    THIS-OBJECT:ultraProgressBar1:Text = "[Formatted]".
    /*  */
    /* UltraProgressBarForm */
    /*  */
    THIS-OBJECT:ClientSize = NEW System.Drawing.Size(292, 266).
    THIS-OBJECT:Name = "UltraProgressBarForm".
    THIS-OBJECT:Text = "UltraProgressBarForm".
    THIS-OBJECT:ResumeLayout(FALSE).
    CATCH e AS Progress.Lang.Error:
      UNDO, THROW e.
    END CATCH.
	END METHOD.

	DESTRUCTOR PUBLIC UltraProgressBarForm ( ):

	END DESTRUCTOR.

END CLASS.