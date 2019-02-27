 
 /*------------------------------------------------------------------------
    File        : SpreadSheetViewer
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Joachim
    Created     : Fri Nov 29 11:24:00 CET 2013
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.
USING Progress.Windows.Form.
USING FarPoint.Win.Spread.ColumnHeader.*.
USING FarPoint.Win.Spread.PrintInfo.*.

ROUTINE-LEVEL ON ERROR UNDO, THROW.

CLASS SpreadSheetViewer INHERITS Form: 
	
	DEFINE PRIVATE VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.
  DEFINE PRIVATE VARIABLE saveToolStripMenuItem AS System.Windows.Forms.ToolStripMenuItem NO-UNDO.
  DEFINE PRIVATE VARIABLE menuStrip1 AS System.Windows.Forms.MenuStrip NO-UNDO.
  DEFINE PRIVATE VARIABLE fpSpread1_Sheet1 AS FarPoint.Win.Spread.SheetView NO-UNDO.
  DEFINE PRIVATE VARIABLE fpSpread1 AS FarPoint.Win.Spread.FpSpread NO-UNDO.
  DEFINE PRIVATE VARIABLE fileToolStripMenuItem AS System.Windows.Forms.ToolStripMenuItem NO-UNDO.
  DEFINE PRIVATE VARIABLE exitToolStripMenuItem AS System.Windows.Forms.ToolStripMenuItem NO-UNDO.
  DEFINE PRIVATE VARIABLE cDefaultFont     AS CHARACTER NO-UNDO.
  DEFINE PRIVATE VARIABLE iDefaultFontSize AS INTEGER   NO-UNDO.
  DEFINE PRIVATE VARIABLE toolStripMenuItem1 AS System.Windows.Forms.ToolStripMenuItem NO-UNDO.

		
	CONSTRUCTOR PUBLIC SpreadSheetViewer (  ):				
    SUPER().
    InitializeComponent().
    cDefaultFont = THIS-OBJECT:Font:Name.
    iDefaultFontSize = THIS-OBJECT:Font:Size.
    CATCH e AS Progress.Lang.Error:
      UNDO, THROW e.
    END CATCH.
	END CONSTRUCTOR.


	/*------------------------------------------------------------------------------
			Purpose:  																	  
			Notes:  																	  
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID exitToolStripMenuItem_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
	  Close().	
		RETURN.

	END METHOD.

	METHOD PRIVATE VOID InitializeComponent(  ):
		
    /* NOTE: The following method is automatically generated.
    
    We strongly suggest that the contents of this method only be modified using the
    Visual Designer to avoid any incompatible modifications.
    
    Modifying the contents of this method using a code editor will invalidate any support for this file. */
    THIS-OBJECT:menuStrip1 = NEW System.Windows.Forms.MenuStrip().
    THIS-OBJECT:fileToolStripMenuItem = NEW System.Windows.Forms.ToolStripMenuItem().
    THIS-OBJECT:saveToolStripMenuItem = NEW System.Windows.Forms.ToolStripMenuItem().
    THIS-OBJECT:toolStripMenuItem1 = NEW System.Windows.Forms.ToolStripMenuItem().
    THIS-OBJECT:exitToolStripMenuItem = NEW System.Windows.Forms.ToolStripMenuItem().
    THIS-OBJECT:fpSpread1 = NEW FarPoint.Win.Spread.FpSpread().
    THIS-OBJECT:fpSpread1_Sheet1 = NEW FarPoint.Win.Spread.SheetView().
    THIS-OBJECT:menuStrip1:SuspendLayout().
    CAST(THIS-OBJECT:fpSpread1, System.ComponentModel.ISupportInitialize):BeginInit().
    CAST(THIS-OBJECT:fpSpread1_Sheet1, System.ComponentModel.ISupportInitialize):BeginInit().
    THIS-OBJECT:SuspendLayout().
    /*  */
    /* menuStrip1 */
    /*  */
    @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
    DEFINE VARIABLE arrayvar0 AS System.Windows.Forms.ToolStripItem EXTENT 1 NO-UNDO.
    arrayvar0[1] = THIS-OBJECT:fileToolStripMenuItem.
    THIS-OBJECT:menuStrip1:Items:AddRange(arrayvar0).
    THIS-OBJECT:menuStrip1:Location = NEW System.Drawing.Point(0, 0).
    THIS-OBJECT:menuStrip1:Name = "menuStrip1".
    THIS-OBJECT:menuStrip1:Size = NEW System.Drawing.Size(784, 24).
    THIS-OBJECT:menuStrip1:TabIndex = 0.
    THIS-OBJECT:menuStrip1:Text = "menuStrip1".
    /*  */
    /* fileToolStripMenuItem */
    /*  */
    @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
    DEFINE VARIABLE arrayvar1 AS System.Windows.Forms.ToolStripItem EXTENT 3 NO-UNDO.
    arrayvar1[1] = THIS-OBJECT:saveToolStripMenuItem.
    arrayvar1[2] = THIS-OBJECT:toolStripMenuItem1.
    arrayvar1[3] = THIS-OBJECT:exitToolStripMenuItem.
    THIS-OBJECT:fileToolStripMenuItem:DropDownItems:AddRange(arrayvar1).
    THIS-OBJECT:fileToolStripMenuItem:Name = "fileToolStripMenuItem".
    THIS-OBJECT:fileToolStripMenuItem:Size = NEW System.Drawing.Size(37, 20).
    THIS-OBJECT:fileToolStripMenuItem:Text = "&File".
    /*  */
    /* saveToolStripMenuItem */
    /*  */
    THIS-OBJECT:saveToolStripMenuItem:Name = "saveToolStripMenuItem".
    DEFINE VARIABLE nestedvar0 AS System.Windows.Forms.Keys NO-UNDO.
    nestedvar0 = System.Windows.Forms.Keys:Control.
    THIS-OBJECT:saveToolStripMenuItem:ShortcutKeys = CAST(Progress.Util.EnumHelper:Or(nestedvar0, System.Windows.Forms.Keys:S), System.Windows.Forms.Keys).
    THIS-OBJECT:saveToolStripMenuItem:Size = NEW System.Drawing.Size(202, 22).
    THIS-OBJECT:saveToolStripMenuItem:Text = "&Save as Excel File".
    THIS-OBJECT:saveToolStripMenuItem:Click:Subscribe(THIS-OBJECT:saveToolStripMenuItem_Click).
    /*  */
    /* toolStripMenuItem1 */
    /*  */
    THIS-OBJECT:toolStripMenuItem1:Name = "toolStripMenuItem1".
    DEFINE VARIABLE nestedvar1 AS System.Windows.Forms.Keys NO-UNDO.
    nestedvar1 = System.Windows.Forms.Keys:Control.
    THIS-OBJECT:toolStripMenuItem1:ShortcutKeys = CAST(Progress.Util.EnumHelper:Or(nestedvar1, System.Windows.Forms.Keys:P), System.Windows.Forms.Keys).
    THIS-OBJECT:toolStripMenuItem1:Size = NEW System.Drawing.Size(202, 22).
    THIS-OBJECT:toolStripMenuItem1:Text = "&Print".
    THIS-OBJECT:toolStripMenuItem1:Click:Subscribe(THIS-OBJECT:toolStripMenuItem1_Click).
    /*  */
    /* exitToolStripMenuItem */
    /*  */
    THIS-OBJECT:exitToolStripMenuItem:Name = "exitToolStripMenuItem".
    THIS-OBJECT:exitToolStripMenuItem:Size = NEW System.Drawing.Size(202, 22).
    THIS-OBJECT:exitToolStripMenuItem:Text = "E&xit".
    THIS-OBJECT:exitToolStripMenuItem:Click:Subscribe(THIS-OBJECT:exitToolStripMenuItem_Click).
    /*  */
    /* fpSpread1 */
    /*  */
    THIS-OBJECT:fpSpread1:AccessibleDescription = "".
    THIS-OBJECT:fpSpread1:Dock = System.Windows.Forms.DockStyle:Fill.
    THIS-OBJECT:fpSpread1:Location = NEW System.Drawing.Point(0, 24).
    THIS-OBJECT:fpSpread1:Name = "fpSpread1".
    @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
    DEFINE VARIABLE arrayvar2 AS FarPoint.Win.Spread.SheetView EXTENT 1 NO-UNDO.
    arrayvar2[1] = THIS-OBJECT:fpSpread1_Sheet1.
    THIS-OBJECT:fpSpread1:Sheets:AddRange(arrayvar2).
    THIS-OBJECT:fpSpread1:Size = NEW System.Drawing.Size(784, 438).
    THIS-OBJECT:fpSpread1:TabIndex = 1.
    /*  */
    /* fpSpread1_Sheet1 */
    /*  */
    THIS-OBJECT:fpSpread1_Sheet1:Reset().
    THIS-OBJECT:fpSpread1_Sheet1:SheetName = "Sheet1".
    /*  */
    /* SpreadSheetViewer */
    /*  */
    THIS-OBJECT:ClientSize = NEW System.Drawing.Size(784, 462).
    THIS-OBJECT:Controls:Add(THIS-OBJECT:fpSpread1).
    THIS-OBJECT:Controls:Add(THIS-OBJECT:menuStrip1).
    THIS-OBJECT:MainMenuStrip = THIS-OBJECT:menuStrip1.
    THIS-OBJECT:Name = "SpreadSheetViewer".
    THIS-OBJECT:Text = "SpreadSheetViewer".
    THIS-OBJECT:menuStrip1:ResumeLayout(FALSE).
    THIS-OBJECT:menuStrip1:PerformLayout().
    CAST(THIS-OBJECT:fpSpread1, System.ComponentModel.ISupportInitialize):EndInit().
    CAST(THIS-OBJECT:fpSpread1_Sheet1, System.ComponentModel.ISupportInitialize):EndInit().
    THIS-OBJECT:ResumeLayout(FALSE).
    THIS-OBJECT:PerformLayout().
    CATCH e AS Progress.Lang.Error:
      UNDO, THROW e.
    END CATCH.
	END METHOD.

  METHOD PUBLIC VOID InitializeSpreadSheetFromTempTable(INPUT phttTempTable AS HANDLE):
    DEFINE VARIABLE hQuery      AS HANDLE    NO-UNDO.
    DEFINE VARIABLE hBuff       AS HANDLE    NO-UNDO.
    DEFINE VARIABLE hField      AS HANDLE    NO-UNDO.
    DEFINE VARIABLE cFieldValue AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iFieldCnt   AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iRow        AS INTEGER   NO-UNDO INITIAL 0.
    
    IF NOT VALID-HANDLE(phttTempTable) THEN 
      RETURN.
    
    hBuff = phttTempTable:DEFAULT-BUFFER-HANDLE.  
    CREATE QUERY hQuery.
    hQuery:SET-BUFFERS(hBuff).
    hQuery:QUERY-PREPARE("FOR EACH " + hBuff:NAME).
    hQuery:QUERY-OPEN().
    hQuery:GET-FIRST().
    
    DO WHILE hBuff:AVAILABLE:
      DO iFieldCnt = 1 TO hBuff:NUM-FIELDS:
        ASSIGN 
          hField      = hBuff:BUFFER-FIELD(iFieldCnt).
          cFieldValue = IF hField:BUFFER-VALUE = ? THEN
                             ""
                        ELSE STRING(hField:BUFFER-VALUE).                                         
        fpSpread1_Sheet1:SetValue(iRow, iFieldCnt - 1, cFieldValue).                                                        
      END.
      iRow = iRow + 1.
      hQuery:GET-NEXT().    
    END.
        
  END METHOD.  
  
  METHOD PUBLIC VOID InitializeSpreadSheetFromFile(INPUT pcFileName AS CHARACTER, 
                                                   INPUT pcDelimiter AS CHARACTER):
    DEFINE VARIABLE styleInfo    AS FarPoint.Win.Spread.StyleInfo NO-UNDO.
    DEFINE VARIABLE headerCell   AS FarPoint.Win.Spread.Cell NO-UNDO.
    DEFINE VARIABLE cHeaderLine  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iCnt         AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iColumnWidth AS INTEGER   NO-UNDO.
    DEFINE VARIABLE cNewLIne     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cDelimiter   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cDotNetTab   AS CHARACTER NO-UNDO INITIAL "\t".
    DEFINE VARIABLE c4GLTab      AS CHARACTER NO-UNDO INITIAL "~t".
                   
    IF pcFileName = "" OR pcFileName = ? THEN 
      RETURN.    
      
    INPUT FROM VALUE(pcFileName) NO-ECHO.
    IMPORT UNFORMATTED cHeaderLine.
    INPUT CLOSE.  
          
    ASSIGN       
      cNewLine   = System.Environment:NewLine
      cDelimiter = IF pcDelimiter = c4GLTab THEN 
                        cDotNetTab 
                   ELSE pcDelimiter. 
    
    fpSpread1_Sheet1:LoadTextFile(pcFileName, FarPoint.Win.Spread.TextFileFlags:Unformatted, FarPoint.Win.Spread.Model.IncludeHeaders:ColumnHeadersCustomOnly, cNewLine, "", cDelimiter). 
    
    DO iCnt = 1 TO NUM-ENTRIES(cHeaderLine, pcDelimiter):
      headerCell = fpSpread1_Sheet1:ColumnHeader:Cells:Get(0, iCnt - 1).
      headerCell:HorizontalAlignment = FarPoint.Win.Spread.CellHorizontalAlignment:Left.     
      headerCell:Font = new System.Drawing.Font(cDefaultFont, iDefaultFontSize, System.Drawing.FontStyle:Bold).
      iColumnWidth = fpSpread1_Sheet1:GetPreferredColumnWidth(iCnt - 1, false, false, false).
      fpSpread1_Sheet1:Columns[iCnt - 1]:Width = iColumnWidth.
    END.
  END.

	/*------------------------------------------------------------------------------
			Purpose: Save to Excel file 																	  
			Notes:  																	  
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID saveToolStripMenuItem_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
	  DEFINE VARIABLE cExcelFileName AS CHARACTER NO-UNDO.
	  DEFINE VARIABLE lOkToSave      AS LOGICAL   NO-UNDO.
	  SYSTEM-DIALOG GET-FILE cExcelFileName 
      FILTERS "Excel files (*.xls)" "*.xls"
      USE-FILENAME ASK-OVERWRITE SAVE-AS CREATE-TEST-FILE 
      UPDATE lOkToSave.
    IF NOT lOkToSave THEN 
      RETURN.	  	  
	  fpSpread1:SaveExcel(cExcelFileName, FarPoint.Win.Spread.Model.IncludeHeaders:ColumnHeadersCustomOnly). 	  	
		RETURN.
	END METHOD.

	/*------------------------------------------------------------------------------
			Purpose: Print  																	  
			Notes:  																	  
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID toolStripMenuItem1_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
	  DEFINE VARIABLE printSetting AS FarPoint.Win.Spread.PrintInfo NO-UNDO.
	  printSetting = NEW FarPoint.Win.Spread.PrintInfo().
	  printSetting:ShowPrintDialog = true.	  
    /* printSetting:Preview = true. */
	  fpSpread1_Sheet1:PrintInfo = printSetting.	        
		fpSpread1:PrintSheet(-1).
		RETURN.
	END METHOD.

	DESTRUCTOR PUBLIC SpreadSheetViewer ( ):
		IF VALID-OBJECT(components) THEN DO:
			CAST(components, System.IDisposable):Dispose().
		END.
	END DESTRUCTOR.

END CLASS.