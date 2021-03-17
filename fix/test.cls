 
 /*------------------------------------------------------------------------
    File        : test
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : tomn
    Created     : Thu Nov 21 13:53:14 CET 2019
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.
USING Progress.Windows.Form.


CLASS fix.test INHERITS Form: 
  
  DEFINE PRIVATE VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.

    
  CONSTRUCTOR PUBLIC test (  ):
    
    
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
    THIS-OBJECT:SuspendLayout().
    THIS-OBJECT:Name = "test".
    THIS-OBJECT:Text = "test".
    THIS-OBJECT:ResumeLayout(FALSE).
    CATCH e AS Progress.Lang.Error:
      UNDO, THROW e.
    END CATCH.

  END METHOD.

  DESTRUCTOR PUBLIC test ( ):

  END DESTRUCTOR.

END CLASS.