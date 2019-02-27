 
 /*------------------------------------------------------------------------
    File        : BRWTest1
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : co
    Created     : Tue Nov 13 13:40:59 CET 2012
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.
USING System.Enum.*.
USING Progress.Windows.Form.

ROUTINE-LEVEL ON ERROR UNDO, THROW.

CLASS uc.JboxIGGrid INHERITS uc.JBoxUCbase: 
	
    DEFINE PRIVATE VARIABLE bindingSource1 AS Progress.Data.BindingSource NO-UNDO.
	DEFINE PRIVATE VARIABLE components     AS System.ComponentModel.IContainer NO-UNDO.

    DEFINE PRIVATE VARIABLE tableDesc1       AS Progress.Data.TableDesc NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraGridBand    AS Infragistics.Win.UltraWinGrid.UltraGridBand EXTENT NO-UNDO.
    DEFINE PUBLIC  VARIABLE ultraGridColumn  AS Infragistics.Win.UltraWinGrid.UltraGridColumn  EXTENT NO-UNDO.
    DEFINE PUBLIC  VARIABLE ultraGrid1       AS Infragistics.Win.UltraWinGrid.UltraGrid NO-UNDO.
    DEFINE PUBLIC  VARIABLE CellAppearance   AS Infragistics.Win.Appearance EXTENT NO-UNDO.
    DEFINE PUBLIC  VARIABLE HeaderAppearance AS Infragistics.Win.Appearance EXTENT NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraGridExcelExporter1 AS Infragistics.Win.UltraWinGrid.ExcelExport.UltraGridExcelExporter NO-UNDO.
	
    
    DEFINE VARIABLE arrayvar0 AS "Progress.Data.TableDesc[]" NO-UNDO.
    DEFINE VARIABLE arrayvar1 AS Progress.Data.ColumnPropDesc EXTENT  NO-UNDO.
    DEFINE VARIABLE hBuffer1 AS HANDLE NO-UNDO. 
    DEFINE VARIABLE ghPropertyTable AS HANDLE NO-UNDO. 


    DEFINE TEMP-TABLE ttField
        FIELD FieldSeq  AS INT 
        FIELD FieldName AS CHAR 
        FIELD FieldFormat AS CHAR 
        FIELD FieldLabel AS CHAR
        FIELD FieldDataType AS CHAR . 

    DEF VAR ghQuery AS HANDLE NO-UNDO. 

 CONSTRUCTOR PUBLIC JboxIGGrid (  INPUT ihWinSuper    AS HANDLE, /* Window super proc - unknown if object created from window */
                                    INPUT ihWinProc     AS HANDLE,
                                    INPUT ihPlaceHolder AS HANDLE, /* Design widget (rectangle, field, etc) */
                                    INPUT iiDeltaWidth  AS INTEGER
                                    
                                    ):
     SUPER(ihWinSuper,ihWinProc,ihPlaceHolder,iiDeltaWidth).
  END CONSTRUCTOR.
  
  
  CONSTRUCTOR PUBLIC JboxIGGrid ( INPUT ihWinProc     AS HANDLE,
                                  INPUT ihPlaceHolder AS HANDLE,
                                  hQuery AS HANDLE,
                                  hPropertyTable AS HANDLE ): /* Design widget (rectangle, field, etc) */
 
         SUPER(?,ihWinProc,ihPlaceHolder,0,hQuery,hPropertyTable).
 
         CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.
     
     
  END CONSTRUCTOR.


METHOD PUBLIC VOID PreInitializeComponent  (INPUT hQuery AS HANDLE, INPUT hPropertyTable AS HANDLE):
    
        ghPropertyTable = hPropertyTable. 
      
        /* If the input is buffer or temp-table then create query and open query */
        IF hQuery:TYPE = "BUFFER"  OR hQuery:TYPE = "TEMP-TABLE" THEN
        DO: 
            EXTENT(ultraGridBand) = 1.

            IF hQuery:TYPE = "TEMP-TABLE" THEN hBuffer1 = hQuery:DEFAULT-BUFFER-HANDLE. 
            ELSE hBuffer1 = hQuery.

            CREATE QUERY hQuery. 
            hQuery:SET-BUFFERS (hBuffer1).
            hQuery:QUERY-PREPARE ("for each " + hBuffer1:NAME).
        END.

        ghQuery = hQuery.

        setColumnData(hQuery).
        
        InitializeComponent().
        bindingSource1:HANDLE = hQuery.

END. 


/*
	CONSTRUCTOR PUBLIC JboxIGGrid (iphBindingSource AS HANDLE  ):
				
        SUPER().

        /* If the input is buffer or temp-table then create query and open query */
        IF iphBindingSource:TYPE = "BUFFER"  OR iphBindingSource:TYPE = "TEMP-TABLE" THEN
        DO: 
            IF iphBindingSource:TYPE = "TEMP-TABLE" THEN hBuffer1 = iphBindingSource:DEFAULT-BUFFER-HANDLE. 
            ELSE hBuffer1 = iphBindingSource.
            
            CREATE QUERY iphBindingSource. 
            iphBindingSource:SET-BUFFERS (hBuffer1).
            iphBindingSource:QUERY-PREPARE ("for each " + hBuffer1:NAME).
        END.
        ELSE IF iphBindingSource:TYPE = "DATASET" THEN
        DO: MESSAGE "dataaset" VIEW-AS ALERT-BOX.  

        END.

        ghQuery = iphBindingSource.

        setColumnData(iphBindingSource). /* Query handle */ 
        InitializeComponent().
        bindingSource1:HANDLE = iphBindingSource.
        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.

	END CONSTRUCTOR.

    CONSTRUCTOR PUBLIC JboxIGGrid (hQuery AS HANDLE,hPropertyTable AS HANDLE ):

        SUPER().
        ghPropertyTable = hPropertyTable. 
      
        /* If the input is buffer or temp-table then create query and open query */
        IF hQuery:TYPE = "BUFFER"  OR hQuery:TYPE = "TEMP-TABLE" THEN
        DO: 
            EXTENT(ultraGridBand) = 1.

            IF hQuery:TYPE = "TEMP-TABLE" THEN hBuffer1 = hQuery:DEFAULT-BUFFER-HANDLE. 
            ELSE hBuffer1 = hQuery.

            CREATE QUERY hQuery. 
            hQuery:SET-BUFFERS (hBuffer1).
            hQuery:QUERY-PREPARE ("for each " + hBuffer1:NAME).
        END.

        ghQuery = hQuery.

        setColumnData(hQuery).
        
        InitializeComponent().
        bindingSource1:HANDLE = hQuery.
        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.
    
    END CONSTRUCTOR.
*/

	METHOD PUBLIC VOID RefreshQuery ():
      /*  ghQuery:QUERY-CLOSE().
        ghQuery:QUERY-PREPARE ("for each " + hBuffer1:NAME).
        */
        ghQuery:QUERY-OPEN ().

        /*bindingSource1:HANDLE = ghQuery.*/
    END.


    /* Property methods executed dynamic from ghPropertyTable (if exists - else use default values ) */ 
	METHOD PUBLIC VOID PropertyHidden (INPUT ipiColumn AS INT, INPUT ipcValue AS CHAR):
        ultraGridColumn[ipiColumn]:Hidden = LOGICAL(ipcValue).
    END.
    
    METHOD PUBLIC VOID PropertyAllowEdit (INPUT ipiColumn AS INT, INPUT ipcValue AS CHAR):
        ultraGridColumn[ipiColumn]:CellActivation = Infragistics.Win.UltraWinGrid.Activation:AllowEdit.
    END.
    
    /* Property methods executed dynamic from ghPropertyTable (if exists - else use default values ) */ 
	METHOD PRIVATE VOID PropertyWidth (INPUT ipiColumn AS INT, INPUT ipcValue AS CHAR):
        ultraGridColumn[ipiColumn]:WIDTH = INTEGER(ipcValue).
    END.

	METHOD PRIVATE VOID PropertyLabel (INPUT ipiColumn AS INT, INPUT ipcValue AS CHAR):
       ultraGridColumn[ipiColumn]:Header:Caption = ipcValue.
    END.

	METHOD PRIVATE VOID PropertyLabelAlign (INPUT ipiColumn AS INT, INPUT ipcValue AS CHAR):
          HeaderAppearance[ipiColumn]:TextHAlignAsString = ipcValue.
    END.
    
    METHOD PRIVATE VOID PropertyAlign (INPUT ipiColumn AS INT, INPUT ipcValue AS CHAR):
          CellAppearance[ipiColumn]:TextHAlignAsString = ipcValue.
    END.

    METHOD PRIVATE VOID PropertyFormat (INPUT ipiColumn AS INT, INPUT ipcValue AS CHAR):
          ultraGridColumn[ipiColumn]:Format = getGridFormatFromABL(ipcValue).  
    END.
    
    METHOD PRIVATE VOID PropertyButton (INPUT ipiColumn AS INT, INPUT ipcValue AS CHAR):
          ultraGridColumn[ipiColumn]:Format = getGridFormatFromABL(ipcValue).  
    END.

    METHOD PRIVATE VOID PropertyDropDown (INPUT ipiColumn AS INT, INPUT ipcValue AS CHAR):
     /*
       ultraGridColumn[ipiColumn]:Style  =  Infragistics.Win.UltraWinGrid.ColumnStyle:Button.   */
       ultraGridColumn[ipiColumn]:Style  = Infragistics.Win.UltraWinGrid.ColumnStyle:DropDownList.

    END.


    METHOD PRIVATE VOID GridPropertyAllowUpdate (INPUT ipcValue AS CHAR):
        MESSAGE ipcValue VIEW-AS ALERT-BOX. 
        IF LOGICAL(ipcValue) THEN
            THIS-OBJECT:ultraGrid1:DisplayLayout:Bands[0]:Override:AllowUpdate = Infragistics.Win.DefaultableBoolean:TRUE.
        
        THIS-OBJECT:ultraGridBand[1]:Override:AllowUpdate = Infragistics.Win.DefaultableBoolean:TRUE.
    END. 


    METHOD PRIVATE VOID GridPropertyPerformAutoResizeColumns (INPUT ipcValue AS CHAR):
        THIS-OBJECT:ultraGrid1:DisplayLayout:Bands[0]:PerformAutoResizeColumns(LOGICAL(ipcValue),Infragistics.Win.UltraWinGrid.PerformAutoSizeType:AllRowsInBand).
    END.
    
    METHOD PRIVATE VOID GridPropertyResizeAllColumns (INPUT ipcValue AS CHAR):
        THIS-OBJECT:ultraGrid1:DisplayLayout:AutoFitStyle = Infragistics.Win.UltraWinGrid.AutoFitStyle:ResizeAllColumns.
    END. 


    METHOD PRIVATE VOID GridPropertySummaryColumn (INPUT ipcValue AS CHAR):
         DEFINE VARIABLE summarySettings1 AS Infragistics.Win.UltraWinGrid.SummarySettings NO-UNDO.
         DEFINE VARIABLE arrayvar6 AS Infragistics.Win.UltraWinGrid.SummarySettings EXTENT 1 NO-UNDO.
         
         DEFINE VARIABLE appearance4 AS Infragistics.Win.Appearance NO-UNDO.
         appearance4 = NEW Infragistics.Win.Appearance().

         summarySettings1 = NEW Infragistics.Win.UltraWinGrid.SummarySettings("", Infragistics.Win.UltraWinGrid.SummaryType:Sum, ?, 
                                                                             ipcValue, 6, TRUE,
                                                                              "_File", 0,
                                                                              Infragistics.Win.UltraWinGrid.SummaryPosition:Left, ?, -1, FALSE).

        summarySettings1:GroupBySummaryValueAppearance = appearance4.
        summarySettings1:Lines = 2.
        arrayvar6[1] = summarySettings1.
        ultraGridBand[1]:Summaries:AddRange(arrayvar6).
        ultraGridBand[1]:SummaryFooterCaption = "SUM TOTAL".

    END. 


    METHOD PUBLIC VOID ExportGridToExcel (INPUT ipcFilename AS CHAR):
       THIS-OBJECT:ultraGridExcelExporter1 = NEW Infragistics.Win.UltraWinGrid.ExcelExport.UltraGridExcelExporter(THIS-OBJECT:components).
       /* THIS-OBJECT:ultraGridExcelExporter1:DefaultWorkbookPaletteMode = Infragistics.Excel.WorkbookPaletteMode.StandardPalette.*/
       THIS-OBJECT:ultraGridExcelExporter1:Export(THIS-OBJECT:ultraGrid1, ipcFilename).
    END. 

    METHOD PRIVATE CHAR getGridFormatFromABL (INPUT cABLFieldFormat AS CHAR):
        
        DEFINE VARIABLE cFormat AS CHAR NO-UNDO.
        ASSIGN 
            cFormat = REPLACE(cABLFieldFormat,">","#")
            cFormat = REPLACE(cFormat,"9","0")
            cFormat = REPLACE(cFormat,","," ")
            cFormat = REPLACE(cFormat,"-","").
        RETURN cFormat. 
    END.


	METHOD PRIVATE VOID setColumnData(iphQuery AS HANDLE ):

        DEFINE VARIABLE hBuffer1 AS HANDLE NO-UNDO. 
        DEFINE VARIABLE iCnt AS INT NO-UNDO. 
        DEFINE VARIABLE iFieldNum AS INT NO-UNDO. 
        hBuffer1 = iphQuery:GET-BUFFER-HANDLE(1). 
    
        DO iCnt = 1 TO hBuffer1:NUM-FIELDS:
            IF hBuffer1:BUFFER-FIELD(icnt):DATA-TYPE = "RAW" THEN NEXT. 
            
            iFieldNum = iFieldNum + 1. 
            CREATE ttField . 
            ASSIGN 
                 ttField.FieldSeq      =  iFieldNum
                 ttField.FieldName     =  hBuffer1:BUFFER-FIELD(icnt):NAME
                 ttField.FieldFormat   =  hBuffer1:BUFFER-FIELD(icnt):FORMAT
                 ttField.FieldLabel    =  hBuffer1:BUFFER-FIELD(icnt):LABEL
                 ttField.FieldDataType =  hBuffer1:BUFFER-FIELD(icnt):DATA-TYPE.
        END. 

        ASSIGN 

            EXTENT(ultraGridColumn)  = iFieldNum
            EXTENT(arrayvar1)        = iFieldNum
            EXTENT(CellAppearance)   = iFieldNum
            EXTENT(HeaderAppearance) = iFieldNum.
    END. 

	/* ------------------------------ Setting properties for fields/columns and the grid in general  --- */   
    /* Method for looping through all field level properties */
    METHOD VOID setColumnProperty (INPUT ipiColumn AS INT,
                                   INPUT ipcFieldName AS CHAR) :
    
        IF NOT VALID-HANDLE(ghPropertyTable)  THEN RETURN. 
        DEFINE VARIABLE hQuery AS HANDLE NO-UNDO. 
        CREATE QUERY hQuery .
        hQuery:SET-BUFFERS( ghPropertyTable ) NO-ERROR.
        hQuery:QUERY-PREPARE(SUBSTITUTE("FOR EACH &1", ghPropertyTable:NAME) + 
                             ' WHERE PropertyParentName = ' + QUOTER(ipcFieldName) ) NO-ERROR.
        hQuery:QUERY-OPEN() .
        DO WHILE hQuery:GET-NEXT(NO-LOCK,NO-WAIT) AND NOT hQuery:QUERY-OFF-END:                  
            DYNAMIC-INVOKE(THIS-OBJECT,'Property' + ghPropertyTable::PropertyName,ipiColumn,ghPropertyTable::PropertyValue).
        END. 
        DELETE OBJECT hQuery NO-ERROR. 
    END METHOD.


    METHOD VOID setGridProperty ():
    
        IF NOT VALID-HANDLE(ghPropertyTable)  THEN RETURN. 
        DEFINE VARIABLE hQuery AS HANDLE NO-UNDO. 
        CREATE QUERY hQuery .
        hQuery:SET-BUFFERS( ghPropertyTable ) NO-ERROR.
        hQuery:QUERY-PREPARE(SUBSTITUTE("FOR EACH &1", ghPropertyTable:NAME) + ' WHERE PropertyParentType = ' + QUOTER("TABLE") ) .
        hQuery:QUERY-OPEN() .
        DO WHILE hQuery:GET-NEXT(NO-LOCK,NO-WAIT) AND NOT hQuery:QUERY-OFF-END:                  
            DYNAMIC-INVOKE(THIS-OBJECT,'GridProperty' + ghPropertyTable::PropertyName, ghPropertyTable::PropertyValue).
        END. 
        DELETE OBJECT hQuery NO-ERROR. 
    END METHOD.

    /* -------------------------------------------------------------------------------------------------------------------------- */

    METHOD OVERRIDE VOID InitializeComponent(  ):
		
        DEFINE VARIABLE hQuery AS HANDLE NO-UNDO. 
        DEFINE VARIABLE iNumFields AS INT NO-UNDO. 
        THIS-OBJECT:components = NEW System.ComponentModel.Container().
              
        
        ASSIGN 
            tableDesc1         = NEW Progress.Data.TableDesc("Table")
            ultraGridBand[1]   = NEW Infragistics.Win.UltraWinGrid.UltraGridBand("Table", -1).
        

        DO iNumFields = 1 TO EXTENT(ultraGridColumn): 
            FIND ttField WHERE ttField.FieldSeq = iNumFields NO-LOCK NO-ERROR. 
            ultraGridColumn[iNumFields] = NEW Infragistics.Win.UltraWinGrid.UltraGridColumn(ttField.FieldName).
            
            IF ttField.FieldDataType = "CHARACTER" THEN
                arrayvar1[iNumFields] = NEW Progress.Data.ColumnPropDesc(ttField.FieldName,ttField.FieldName, Progress.Data.DataType:CHARACTER).
            ELSE IF ttField.FieldDataType = "DECIMAL" THEN
                arrayvar1[iNumFields] = NEW Progress.Data.ColumnPropDesc(ttField.FieldName,ttField.FieldName, Progress.Data.DataType:DECIMAL).
            ELSE IF ttField.FieldDataType = "INTEGER" THEN
                arrayvar1[iNumFields] = NEW Progress.Data.ColumnPropDesc(ttField.FieldName,ttField.FieldName, Progress.Data.DataType:INTEGER).
            ELSE IF ttField.FieldDataType = "LOGICAL" THEN
                arrayvar1[iNumFields] = NEW Progress.Data.ColumnPropDesc(ttField.FieldName,ttField.FieldName, Progress.Data.DataType:LOGICAL).
            ELSE IF ttField.FieldDataType = "DATE" THEN
                arrayvar1[iNumFields] = NEW Progress.Data.ColumnPropDesc(ttField.FieldName,ttField.FieldName, Progress.Data.DataType:DATE).
            ELSE IF ttField.FieldDataType = "RECID" THEN
                arrayvar1[iNumFields] = NEW Progress.Data.ColumnPropDesc(ttField.FieldName,ttField.FieldName, Progress.Data.DataType:RECID).
            ELSE IF ttField.FieldDataType = "DATETIME-TZ" THEN
                arrayvar1[iNumFields] = NEW Progress.Data.ColumnPropDesc(ttField.FieldName,ttField.FieldName, Progress.Data.DataType:DATETIME_TZ).
            ELSE IF ttField.FieldDataType = "DATETIME" THEN
                arrayvar1[iNumFields] = NEW Progress.Data.ColumnPropDesc(ttField.FieldName,ttField.FieldName, Progress.Data.DataType:DATETIME_TZ).
            ELSE IF ttField.FieldDataType = "HANDLE" THEN
                arrayvar1[iNumFields] = NEW Progress.Data.ColumnPropDesc(ttField.FieldName,ttField.FieldName, Progress.Data.DataType:HANDLE).

        END.
                
        THIS-OBJECT:bindingSource1 = NEW Progress.Data.BindingSource(THIS-OBJECT:components).
        THIS-OBJECT:ultraGrid1     = NEW Infragistics.Win.UltraWinGrid.UltraGrid().

        CAST(THIS-OBJECT:bindingSource1, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:ultraGrid1, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:SuspendLayout().
        
        /* bindingSource1 */
        THIS-OBJECT:bindingSource1:AutoUpdate = TRUE.
        THIS-OBJECT:bindingSource1:MaxDataGuess = 0.
        THIS-OBJECT:bindingSource1:NoLOBs = FALSE.
        THIS-OBJECT:bindingSource1:Position = 0.
        
        arrayvar0 = NEW "Progress.Data.TableDesc[]"(0).
        tableDesc1:ChildTables = arrayvar0.
     
        tableDesc1:Columns = arrayvar1.
        THIS-OBJECT:bindingSource1:TableSchema = tableDesc1.
        THIS-OBJECT:bindingSource1:UseFullFieldNames = FALSE.
        
        /* ultraGrid1 */
        THIS-OBJECT:ultraGrid1:DataSource = THIS-OBJECT:bindingSource1.
        
        DEFINE VARIABLE cFormat AS CHAR NO-UNDO.


        DO iNumFields = 1 TO EXTENT(ultraGridColumn): 
            FIND ttField WHERE ttField.FieldSeq = iNumFields NO-LOCK NO-ERROR. 
            DO:
                CellAppearance[iNumFields] = NEW Infragistics.Win.Appearance().
                HeaderAppearance[iNumFields] = NEW Infragistics.Win.Appearance().
 
                ASSIGN 
                    ultraGridColumn[iNumFields]:Header:VisiblePosition = iNumFields
                    ultraGridColumn[iNumFields]:Header:Caption = ttField.FieldLabel.
                   
            
                /* Set default format and adjust Right for decimal/integer columns */ 
                IF ttField.FieldDataType = "DECIMAL" THEN
                DO:
                    HeaderAppearance[iNumFields]:TextHAlignAsString = "Right".
                    CellAppearance[iNumFields]:TextHAlignAsString   = "Right".
                    IF ttField.FieldFormat NE ?  THEN
                        cFormat = getGridFormatFromABL(ttField.FieldFormat).
                    ELSE 
                        cFormat = "#### ### ### ##0.00".
                    ultraGridColumn[iNumFields]:Format = cFormat.  
                END.
                ELSE IF ttField.FieldDataType = "INTEGER" THEN
                DO:
                    HeaderAppearance[iNumFields]:TextHAlignAsString = "Right".
                    CellAppearance[iNumFields]:TextHAlignAsString = "Right".
                    cFormat = "#########0".
                    ultraGridColumn[iNumFields]:Format = cFormat.
                END.
                ultraGridColumn[iNumFields]:CellAppearance = CellAppearance[iNumFields].
                ultraGridColumn[iNumFields]:Header:Appearance = HeaderAppearance[iNumFields].
                
                /* Default value = noEdit for the columns */ 
                ultraGridColumn[iNumFields]:CellActivation = Infragistics.Win.UltraWinGrid.Activation:noEdit.
                
                /* execute dynamic - field property methods for field (perform after default values are set) */ 
                setColumnProperty(iNumFields,ttField.FieldName).
                
                /* ultraGridColumn[iNumFields]:WIDTH = 20. */
            END. 
        END. 


        ultraGridBand[1]:Columns:AddRange(ultraGridColumn).
        setDisplayLayout(). 
     
        THIS-OBJECT:ultraGrid1:Dock = System.Windows.Forms.DockStyle:Fill.
        THIS-OBJECT:ultraGrid1:Font = NEW System.Drawing.Font("Microsoft Sans Serif", 8.25, System.Drawing.FontStyle:Regular, System.Drawing.GraphicsUnit:Point, System.Convert:ToByte(0)).
      /*  THIS-OBJECT:ultraGrid1:Location = NEW System.Drawing.Point(0, 0). */
        THIS-OBJECT:ultraGrid1:Name = "ultraGrid1".
     /*   THIS-OBJECT:ultraGrid1:Size = NEW System.Drawing.Size(611, 292).     */
        THIS-OBJECT:ultraGrid1:TabIndex = 0.
        THIS-OBJECT:ultraGrid1:Text = "ultraGrid1".
        THIS-OBJECT:ultraGrid1:InitializeLayout:Subscribe(THIS-OBJECT:ultraGrid1_InitializeLayout).
        
        /* BRWTest1 */
  /*      THIS-OBJECT:ClientSize = NEW System.Drawing.Size(611, 292).    */
        THIS-OBJECT:Controls:Add(THIS-OBJECT:ultraGrid1).

        THIS-OBJECT:Name = "BRWTest1".
        THIS-OBJECT:Text = "BRWTest1".
       
        
        CAST(THIS-OBJECT:bindingSource1, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:ultraGrid1, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:ResumeLayout(FALSE).
       
       
        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.
	END METHOD.


	/*------------------------------------------------------------------------------
			Purpose:  																	  
			Notes:  																	  
	------------------------------------------------------------------------------*/
   
	METHOD PRIVATE VOID ultraGrid1_InitializeLayout( INPUT sender AS System.Object, INPUT e AS Infragistics.Win.UltraWinGrid.InitializeLayoutEventArgs ):
	    THIS-OBJECT:setGridProperty().
          
		RETURN.
	END METHOD.


	METHOD PRIVATE VOID setDisplayLayout():
    /* ---- Display layout ---- */ 
    DEFINE VARIABLE  appearance AS Infragistics.Win.Appearance EXTENT 30 NO-UNDO. 
    DEFINE VARIABLE iNumBands AS INT NO-UNDO. 

        appearance[21] = NEW Infragistics.Win.Appearance().
        appearance[22] = NEW Infragistics.Win.Appearance().
        appearance[23] = NEW Infragistics.Win.Appearance().
        appearance[24] = NEW Infragistics.Win.Appearance().
        appearance[13] = NEW Infragistics.Win.Appearance().
        appearance[14] = NEW Infragistics.Win.Appearance().
        appearance[15] = NEW Infragistics.Win.Appearance().
        appearance[16] = NEW Infragistics.Win.Appearance().
        appearance[17] = NEW Infragistics.Win.Appearance().
        appearance[18] = NEW Infragistics.Win.Appearance().
        appearance[19] = NEW Infragistics.Win.Appearance().
        appearance[20] = NEW Infragistics.Win.Appearance().
    
        appearance[21]:BackColor = System.Drawing.SystemColors:Window.
        appearance[21]:BorderColor = System.Drawing.SystemColors:InactiveCaption.
        appearance[21]:BackColor = System.Drawing.SystemColors:Window.
        appearance[21]:BorderColor = System.Drawing.SystemColors:InactiveCaption.
        appearance[17]:BackColor = System.Drawing.SystemColors:Control.
        appearance[17]:BackColor2 = System.Drawing.SystemColors:ControlDark.
        appearance[17]:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
        appearance[17]:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
        appearance[17]:BorderColor = System.Drawing.SystemColors:Window.
        appearance[16]:BorderColor = System.Drawing.Color:Silver.
        appearance[16]:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
        appearance[21]:BackColor = System.Drawing.SystemColors:Window.
        appearance[21]:BorderColor = System.Drawing.SystemColors:InactiveCaption.
        appearance[20]:BackColor = System.Drawing.SystemColors:ControlLight.
        appearance[19]:BackColor = System.Drawing.SystemColors:Window.
        appearance[19]:BorderColor = System.Drawing.Color:Silver.
        appearance[18]:TextHAlignAsString = "Left".
        appearance[15]:BackColor = System.Drawing.SystemColors:Window.
        appearance[14]:BackColor = System.Drawing.SystemColors:Highlight.
        appearance[14]:ForeColor = System.Drawing.SystemColors:HighlightText.
        appearance[13]:BackColor = System.Drawing.SystemColors:Window.
        appearance[13]:ForeColor = System.Drawing.SystemColors:ControlText.
        appearance[24]:BackColor = System.Drawing.SystemColors:ControlLightLight.
        appearance[24]:BackColor2 = System.Drawing.SystemColors:Control.
        appearance[24]:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
        appearance[24]:ForeColor = System.Drawing.SystemColors:GrayText.
        appearance[23]:ForeColor = System.Drawing.SystemColors:GrayText.
        appearance[22]:BackColor = System.Drawing.SystemColors:ActiveBorder.
        appearance[22]:BackColor2 = System.Drawing.SystemColors:ControlDark.
        appearance[22]:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
        appearance[22]:BorderColor = System.Drawing.SystemColors:Window.

        /* add all Bands */ 
        DO iNumBands = 1 TO EXTENT(ultraGridBand): 
           THIS-OBJECT:ultraGrid1:DisplayLayout:BandsSerializer:Add(ultraGridBand[iNumBands]).
        END. 

        THIS-OBJECT:ultraGrid1:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
        THIS-OBJECT:ultraGrid1:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
        THIS-OBJECT:ultraGrid1:DisplayLayout:GroupByBox:Appearance = appearance[22].
        THIS-OBJECT:ultraGrid1:DisplayLayout:GroupByBox:BandLabelAppearance = appearance[23].
        THIS-OBJECT:ultraGrid1:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
        THIS-OBJECT:ultraGrid1:DisplayLayout:GroupByBox:PromptAppearance = appearance[24].
        THIS-OBJECT:ultraGrid1:DisplayLayout:MaxColScrollRegions = 1.
        THIS-OBJECT:ultraGrid1:DisplayLayout:MaxRowScrollRegions = 1.
        THIS-OBJECT:ultraGrid1:DisplayLayout:Override:ActiveCellAppearance = appearance[13].
        THIS-OBJECT:ultraGrid1:DisplayLayout:Override:ActiveRowAppearance = appearance[14].
        THIS-OBJECT:ultraGrid1:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
        THIS-OBJECT:ultraGrid1:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
        THIS-OBJECT:ultraGrid1:DisplayLayout:Override:CardAreaAppearance = appearance[15].
        THIS-OBJECT:ultraGrid1:DisplayLayout:Override:CellAppearance = appearance[16].
        
        /* --- CellClickActivation --- RowSelect/EditAndSelect/ */        
        THIS-OBJECT:ultraGrid1:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:RowSelect.
        THIS-OBJECT:ultraGrid1:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText. 


        THIS-OBJECT:ultraGrid1:DisplayLayout:Override:CellPadding = 0.
        THIS-OBJECT:ultraGrid1:DisplayLayout:Override:GroupByRowAppearance = appearance[17].
        THIS-OBJECT:ultraGrid1:DisplayLayout:Override:HeaderAppearance = appearance[18].
        THIS-OBJECT:ultraGrid1:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
        THIS-OBJECT:ultraGrid1:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
        THIS-OBJECT:ultraGrid1:DisplayLayout:Override:RowAppearance = appearance[19].
        THIS-OBJECT:ultraGrid1:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
        THIS-OBJECT:ultraGrid1:DisplayLayout:Override:TemplateAddRowAppearance = appearance[20].
        THIS-OBJECT:ultraGrid1:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
        THIS-OBJECT:ultraGrid1:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
        THIS-OBJECT:ultraGrid1:DisplayLayout:ViewStyle = Infragistics.Win.UltraWinGrid.ViewStyle:SingleBand.
      /*  THIS-OBJECT:ultraGrid1:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.*/
        THIS-OBJECT:ultraGrid1:DisplayLayout:Appearance = appearance[21].
        
        /* autofit all columns to widith */
        /* Infragistics.Win.UltraWinGrid.AutoFitStyle:ExtendLastColumn; 
        THIS-OBJECT:ultraGrid1:DisplayLayout:AutoFitStyle = Infragistics.Win.UltraWinGrid.AutoFitStyle:ResizeAllColumns.*/
        /* ---- Display Layout ---- */ 
		
		RETURN.
	END METHOD.


    METHOD PUBLIC VOID PerformAutoResizeColumns():
            THIS-OBJECT:ultraGrid1:DisplayLayout:Bands[0]:PerformAutoResizeColumns(true,Infragistics.Win.UltraWinGrid.PerformAutoSizeType:AllRowsInBand).
            /* THIS-OBJECT:ultraGrid1:DisplayLayout:Bands[0]:Override:AllowUpdate = FALSE.*/
            /* disable Cell Update */
            THIS-OBJECT:ultraGridBand[1]:Override:AllowUpdate = Infragistics.Win.DefaultableBoolean:False.
            THIS-OBJECT:ultraGridBand[1]:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:RowSelect.
            RETURN.
    END METHOD.

    METHOD PUBLIC VOID ResizeAllColumns():
             THIS-OBJECT:ultraGrid1:DisplayLayout:AutoFitStyle = Infragistics.Win.UltraWinGrid.AutoFitStyle:ResizeAllColumns.
         RETURN.
    END METHOD.
    
    /* Mandatory methods: */
  METHOD PUBLIC Infragistics.Win.UltraWinGrid.UltraGrid getDotNetWidget():
    RETURN ultraGrid1.
  END METHOD.

 
  METHOD PUBLIC CHARACTER getDotNetWidgetType():
    RETURN STRING(ultraGrid1:GetType()).
  END METHOD.

  METHOD PUBLIC CHARACTER DEFAULT():
    RETURN DYNAMIC-INVOKE(THIS-OBJECT,ENTRY(1,PROGRAM-NAME(2)," ")).
  END METHOD.
    

	DESTRUCTOR PUBLIC JboxIGGrid ( ):

		IF VALID-OBJECT(components) THEN DO:
			CAST(components, System.IDisposable):Dispose().
		END.

	END DESTRUCTOR.

END CLASS.
