 
 /*------------------------------------------------------------------------
    File        : MSWord
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Brynjar
    Created     : Thu May 05 00:04:25 CEST 2016
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.
USING Progress.Windows.Form.
USING Microsoft.Office.Interop.Excel.*.


CLASS MSWord INHERITS Form: 
	
	DEFINE PRIVATE VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.
 
DEFINE VARIABLE objExcel    AS Microsoft.Office.Interop.Word.Application.
/*DEFINE VARIABLE objWorkbook AS Workbook.*/
/*DEFINE VARIABLE objRange    AS Range.         */
/*DEFINE VARIABLE objSheet    AS WorksheetClass.*/
 
/*objExcel = NEW Microsoft.Office.Interop.Excel.ApplicationClass().*/
/*objWorkbook = objExcel:Workbooks:Add("").                        */
/*objExcel:VISIBLE = TRUE.                                         */
/*objSheet = CAST(objWorkbook:ActiveSheet,WorkSheetClass).         */
/*objRange = objSheet:Range("A1", "A2").                           */

		
	CONSTRUCTOR PUBLIC MSWord (  ):
		
		
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
    THIS-OBJECT:Name = "MSWord".
    THIS-OBJECT:Text = "MSWord".
    THIS-OBJECT:ResumeLayout(FALSE).
    CATCH e AS Progress.Lang.Error:
      UNDO, THROW e.
    END CATCH.

	END METHOD.

	DESTRUCTOR PUBLIC MSWord ( ):

	END DESTRUCTOR.

END CLASS.