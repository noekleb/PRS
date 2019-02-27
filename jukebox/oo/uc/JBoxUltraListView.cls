USING Progress.Lang.*.
USING Progress.Util.*.
USING Progress.Windows.UserControl.
USING Progress.Windows.Form.
USING System.TimeSpan.*.
USING Infragistics.Win.*. 

USING uc.* FROM PROPATH.

ROUTINE-LEVEL ON ERROR UNDO, THROW.



CLASS uc.JBoxUltraListView INHERITS uc.JBoxUCbase: 
	  			
  DEFINE PRIVATE VARIABLE components      AS System.ComponentModel.IContainer NO-UNDO.
  DEFINE PRIVATE VARIABLE ultraListView1 AS Infragistics.Win.UltraWinListView.UltraListView NO-UNDO.
  DEFINE PUBLIC VARIABLE PARENT-BROWSE AS HANDLE NO-UNDO. 


  DEFINE VARIABLE ghPropertyTable AS HANDLE NO-UNDO. 
  DEFINE VARIABLE hBuffer1 AS HANDLE NO-UNDO.
  DEFINE VARIABLE iFieldNum AS INT    NO-UNDO.

  /* Column Definitions - Default from Temp-table definitions */
  DEFINE TEMP-TABLE ttField
        FIELD FieldSeq  AS INT 
        FIELD FieldName AS CHAR 
        FIELD FieldFormat AS CHAR 
        FIELD FieldLabel AS CHAR
        FIELD FieldDataType AS CHAR 
        FIELD ColumnListViewSubItem AS Progress.Lang.OBJECT.

  /* Data Row management */ 
  DEFINE TEMP-TABLE ttRow
        FIELD RowRowId           AS ROWID 
        FIELD RowSeq             AS INT 
        FIELD RowAppearance      AS CLASS Progress.Lang.OBJECT
        FIELD RowListViewSubItem AS CLASS Progress.Lang.OBJECT 
        FIELD RowListViewItem    AS Progress.Lang.OBJECT. 


  DEFINE VARIABLE ghQuery AS HANDLE NO-UNDO. 

  CONSTRUCTOR PUBLIC JBoxUltraListView (INPUT ihWinSuper    AS HANDLE, /* Window super proc - unknown if object created from window */
                                     INPUT ihWinProc     AS HANDLE,
                                     INPUT ihPlaceHolder AS HANDLE, /* Design widget (rectangle, field, etc) */
                                     INPUT iiDeltaWidth  AS INTEGER):
    SUPER(ihWinSuper,ihWinProc,ihPlaceHolder,iiDeltaWidth).
    THIS-OBJECT:ComponentsCollection:ADD(THIS-OBJECT:components).
  END CONSTRUCTOR.
  
 CONSTRUCTOR PUBLIC JBoxUltraListView ( INPUT ihWinProc     AS HANDLE,
                                  INPUT ihPlaceHolder AS HANDLE,
                                  hQuery AS HANDLE
                                  ): /* Design widget (rectangle, field, etc) */
 
         SUPER(?,ihWinProc,ihPlaceHolder,0,hQuery,?).
 
         CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.
  END CONSTRUCTOR.
  
  CONSTRUCTOR PUBLIC JBoxUltraListView ( INPUT ihWinProc     AS HANDLE,
                                  INPUT ihPlaceHolder AS HANDLE,
                                  hQuery AS HANDLE,
                                  hPropertyTable AS HANDLE ): /* Design widget (rectangle, field, etc) */
 
         SUPER(?,ihWinProc,ihPlaceHolder,0,hQuery,hPropertyTable).
 
         CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.
  END CONSTRUCTOR.
  
  
  CONSTRUCTOR PUBLIC JBoxUltraListView ( INPUT ihWinProc     AS HANDLE,
                                      INPUT ihPlaceHolder AS HANDLE ): /* Design widget (rectangle, field, etc) */
                                   
    SUPER(?,ihWinProc,ihPlaceHolder,0).
  END CONSTRUCTOR.



  METHOD PUBLIC OVERRIDE VOID InitializeComponent(  ):
	    
        DEFINE VARIABLE hQuery    AS HANDLE NO-UNDO. 
        DEFINE VARIABLE hTable    AS HANDLE NO-UNDO. 
        DEFINE VARIABLE hBuffer   AS HANDLE NO-UNDO. 
        DEFINE VARIABLE iCnt      AS INT    NO-UNDO. 	    
	    

	    
        hBuffer1 = THIS-OBJECT:DATA-DEFAULT-BUFFER-HANDLE.
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
     



        /* color setting and more for each row (should be moved to addrow method 
        RowAppearance:BackColor = System.Drawing.Color:FromArgb(System.Convert:ToInt32(System.Convert:ToByte(255)), System.Convert:ToInt32(System.Convert:ToByte(128)), System.Convert:ToInt32(System.Convert:ToByte(128))).
        RowAppearance:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical. 
        */
    
        THIS-OBJECT:ultraListView1 = NEW Infragistics.Win.UltraWinListView.UltraListView().
        CAST(THIS-OBJECT:ultraListView1, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:SuspendLayout().
        
        /*  */
        /* ultraListView1 */
        /*  */
        THIS-OBJECT:ultraListView1:Dock = System.Windows.Forms.DockStyle:Fill.
        THIS-OBJECT:ultraListView1:ItemSettings:HotTracking = TRUE.
        THIS-OBJECT:ultraListView1:ItemSettings:SubItemsVisibleInToolTipByDefault = TRUE.
        THIS-OBJECT:ultraListView1:Location = NEW System.Drawing.Point(0, 0).
        THIS-OBJECT:ultraListView1:MainColumn:Key = "Name Main".
        THIS-OBJECT:ultraListView1:Name = "ultraListView1".
        THIS-OBJECT:ultraListView1:Size = NEW System.Drawing.Size(709, 388).

        /* Add column definitions and fill with data */ 
        FOR EACH ttField : 
            addColumn(BUFFER ttField:HANDLE, ttField.FieldLabel,"",ttField.FieldSeq,TRUE).
        END.
        OpenDataQuery().
        
        
        THIS-OBJECT:ultraListView1:TabIndex = 5.
        THIS-OBJECT:ultraListView1:Text = "ultraListView1".
        
        /* addUIEvents(). */

        THIS-OBJECT:ultraListView1:ItemActivated:SUBSCRIBE   (THIS-OBJECT:Event_ItemActivated).
        THIS-OBJECT:ultraListView1:ItemDoubleClick:SUBSCRIBE (THIS-OBJECT:Event_ItemDoubleClick).
        /*
        THIS-OBJECT:ultraListView1:ItemSelectionChanging:Subscribe(THIS-OBJECT:Event_ItemSelectionChanging).
        THIS-OBJECT:ultraListView1:ItemSelectionChanged:Subscribe(THIS-OBJECT:Event_ItemSelectionChanged).
        THIS-OBJECT:ultraListView1:ItemEnteredEditMode:Subscribe(THIS-OBJECT:Event_ItemEnteredEditMode).
        THIS-OBJECT:ultraListView1:ItemDoubleClick:Subscribe(THIS-OBJECT:Event_ItemDoubleClick).
        
        THIS-OBJECT:ultraListView1:MouseClick:Subscribe(THIS-OBJECT:Event_MouseClick).
        THIS-OBJECT:ultraListView1:DoubleClick:Subscribe(THIS-OBJECT:Event_DoubleClick).
        THIS-OBJECT:ultraListView1:DragDrop:Subscribe(THIS-OBJECT:FileDrop).
        THIS-OBJECT:ultraListView1:PreviewKeyDown:Subscribe(THIS-OBJECT:Event_PreviewKeyDown).
        */

        /*  */
        /* DocumentListBar */
        /*  */
        THIS-OBJECT:ClientSize = NEW System.Drawing.Size(709, 388).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:ultraListView1).
        THIS-OBJECT:Name = "DocumentListBar".
        THIS-OBJECT:Text = "DocumentListBar".
       
   /* EXTENT(RowAppearance) = 200 . /*hQuery:NUM-RESULTS.*/ */
    
    SUPER:InitializeComponent().

    CAST(THIS-OBJECT:ultraListView1, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:ResumeLayout(FALSE).
        
            CATCH e AS Progress.Lang.Error:
      UNDO, THROW e.
    END CATCH.
    

  END METHOD.
 
  /* Mandatory methods: */

  METHOD PUBLIC Infragistics.Win.UltraWinListView.UltraListView getDotNetWidget():
    RETURN ultraListView1.
  END METHOD.

  METHOD PUBLIC CHARACTER getDotNetWidgetType():
    RETURN STRING(ultraListView1:GetType()).
  END METHOD.


  /* Methods invoked from jukebox (via window super): */
  /* (Semi-)class specific methods - event handlers: */
  /* Class specific methods: */




    METHOD PRIVATE VOID Event_ItemActivated( INPUT sender AS System.Object, INPUT e AS Infragistics.Win.UltraWinListView.ItemActivatedEventArgs ):
       
        /*
        IF VALID-HANDLE(parent-browse) THEN
            APPLY "mouse-click" TO PARENT-BROWSE.
         
        RUN InvokeMethod IN THIS-OBJECT:hWinProc (PARENT-BROWSE,"ShiftHomeBrowse") .
        RUN DoProcessEvent (ihProcedure,"default-action"). */
        RUN InvokeMethod IN THIS-OBJECT:hWinProc (PARENT-BROWSE,"MouseMenuDownBrowse").
        RETURN.
    END METHOD.
    
    METHOD PRIVATE VOID Event_ItemDoubleClick( INPUT sender AS System.Object, INPUT e AS Infragistics.Win.UltraWinListView.ItemDoubleClickEventArgs ):
        RUN InvokeMethod IN THIS-OBJECT:hWinProc (PARENT-BROWSE,"MouseMenuDownBrowse").
        RETURN.
    END METHOD.
    


    DEFINE VARIABLE iRowNumber AS INT NO-UNDO. 

    METHOD PUBLIC VOID OpenDataQuery ():
        DEFINE VARIABLE hQuery  AS HANDLE NO-UNDO. 
        DEFINE VARIABLE hBuffer AS HANDLE NO-UNDO. 
        
        DeleteAllRows().
        
        iRowNumber = 0. 
        hBuffer = THIS-OBJECT:DATA-DEFAULT-BUFFER-HANDLE.
         
        IF NOT VALID-HANDLE(hBuffer)  THEN RETURN. 
           
        CREATE QUERY hQuery .
        hQuery:SET-BUFFERS( hBuffer ) NO-ERROR.
        hQuery:QUERY-PREPARE(SUBSTITUTE("FOR EACH &1", hBuffer:NAME)) .
        hQuery:QUERY-OPEN() .
        
      /*  EXTENT(RowAppearance) = 200 . hQuery:NUM-RESULTS. moved to initialize */

        DO WHILE hQuery:GET-NEXT(NO-LOCK,NO-WAIT) AND NOT hQuery:QUERY-OFF-END :     
             iRowNumber = iRowNumber + 1. 
             addRow(hBuffer).
        END. 
        DELETE OBJECT hQuery NO-ERROR. 

    END METHOD.
 

    METHOD PUBLIC CHARACTER getIconFileName(INPUT ipcType AS CHAR):

       IF iRowNumber = 3  THEN ipcType = "FORM".
       IF iRowNumber = 5  THEN ipcType = "xls".
       IF iRowNumber = 1  THEN ipcType = "JPeg".
       IF iRowNumber = 2  THEN ipcType = "WORD".

       IF ipcType = "FORM"  THEN RETURN "img\listviewforms.ico".
       IF ipcType = "PDF"   THEN RETURN "img\listviewpdf.ico".
       IF ipcType = "XLS"   THEN RETURN "img\listviewexcel.ico".
       IF ipcType = "IMAGE" THEN RETURN "img\listviewimage.ico".
       IF ipcType = "PNG"   THEN RETURN "img\listviewimage.ico".
       IF ipcType = "GIF"   THEN RETURN "img\listviewimage.ico".
       IF ipcType = "JPEG"  THEN RETURN "img\listviewjpeg.ico".
       IF ipcType = "PNG"   THEN RETURN "img\listviewimage.ico".
       IF ipcType = "BMP"   THEN RETURN "img\listviewimage.ico".
       IF ipcType = "ZIP"   THEN RETURN "img\listviewzip.ico".
       IF ipcType = "TEXT"  THEN RETURN "img\listviewtxt.ico".
       IF ipcType = "TXT"   THEN RETURN "img\listviewtxt.ico".
       IF ipcType = "WORD"  THEN RETURN "img\listviewword.ico".
       IF ipcType = ""      THEN RETURN "img\listviewpdf.ico".

       RETURN "img\listviewpdf.ico". 
    END. 

    METHOD PUBLIC VOID deleteAllRows(): 
    
     FOR EACH ttRow : 
           IF VALID-OBJECT(ttRow.RowListViewItem) THEN
               THIS-OBJECT:ultraListView1:Items:remove(CAST(ttRow.RowListViewItem,Infragistics.Win.UltraWinListView.UltraListViewItem)).
           DELETE OBJECT(ttRow.RowListViewItem) NO-ERROR. 
       END.


    END. 

    METHOD PUBLIC VOID addRow(INPUT iphBuffer AS HANDLE): /* temp-table buffer copy to Item/subItem */ 

        DEFINE VARIABLE RowAppearance      AS Infragistics.Win.Appearance  NO-UNDO.
        DEFINE VARIABLE RowArray           AS Infragistics.Win.UltraWinListView.UltraListViewSubItem EXTENT NO-UNDO.
        DEFINE VARIABLE ultraListViewItem1 AS Infragistics.Win.UltraWinListView.UltraListViewItem NO-UNDO.
        DEFINE VARIABLE arrayvar1          AS Infragistics.Win.UltraWinListView.UltraListViewItem EXTENT 1 NO-UNDO.
        DEFINE VARIABLE iField             AS INT NO-UNDO.

        DEFINE VARIABLE FileType     AS CHAR NO-UNDO.

        EXTENT(RowArray) = iFieldNum - 1.  

        DO iField = 2 TO iphBuffer:NUM-FIELDS:
            IF hBuffer1:BUFFER-FIELD(iField):DATA-TYPE = "RAW" THEN NEXT. 
            RowArray[iField - 1] = NEW Infragistics.Win.UltraWinListView.UltraListViewSubItem(iphBuffer:BUFFER-FIELD(iField):BUFFER-VALUE, ?).
        END. 
        
        CREATE ttRow.                        

        /* First Field */ 
        ultraListViewItem1 = NEW Infragistics.Win.UltraWinListView.UltraListViewItem(iphBuffer:BUFFER-FIELD(1):BUFFER-VALUE, RowArray, ?).
        ttRow.rowlistviewItem = ultraListViewItem1.
        /* ttRow.rowListViewSubItem = RowArray. */

        /* Predefined icons for different type's - ListViewXXXX.ico hard coded .... */
        FileType = hBuffer1:BUFFER-FIELD("type"):BUFFER-VALUE NO-ERROR. 
        
        RowAppearance       = NEW Infragistics.Win.Appearance().
        RowAppearance:Image = System.Drawing.Image:FromFile(SEARCH(getIconFileName(filetype))) NO-ERROR.
        ttRow.RowAppearance = RowAppearance. 


        /*
        RowAppearance[iRowNumber]       = NEW Infragistics.Win.Appearance().
        RowAppearance[iRowNumber]:Image = System.Drawing.Image:FromFile(SEARCH(getIconFileName(filetype))) NO-ERROR.
        RowAppearance:BackColor = System.Drawing.Color:FromArgb(System.Convert:ToInt32(System.Convert:ToByte(255)), System.Convert:ToInt32(System.Convert:ToByte(128)), System.Convert:ToInt32(System.Convert:ToByte(128))).
        RowAppearance:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical. 
        */

        ultraListViewItem1:Appearance = RowAppearance.  /* should probably be in this Method */
        ultraListViewItem1:Key        = STRING(iphBuffer:ROWID).     /* Use record-rowid as key */
        
        arrayvar1[1] = ultraListViewItem1.  /* adding one by one row */
        THIS-OBJECT:ultraListView1:Items:AddRange(arrayvar1).
    END. 



METHOD PUBLIC VOID addColumn(INPUT ihBuffer AS HANDLE, 
                             INPUT icFieldName AS CHARACTER,
                             INPUT icDataType AS CHARACTER,
                             INPUT iiSequence AS INTEGER,
                             INPUT ilReadOnly AS LOGICAL):
 
    DEFINE VARIABLE arrayvar2                   AS Infragistics.Win.UltraWinListView.UltraListViewSubItemColumn EXTENT 1 NO-UNDO. 
    DEFINE VARIABLE ultraListViewSubItemColumn0 AS Infragistics.Win.UltraWinListView.UltraListViewSubItemColumn NO-UNDO.

    ultraListViewSubItemColumn0     = NEW Infragistics.Win.UltraWinListView.UltraListViewSubItemColumn(icFieldName).
    ultraListViewSubItemColumn0:Key = icFieldName + string(iiSequence).   
    
    /* assigning object to table */ 
    ihBuffer::ColumnListViewSubItem = ultraListViewSubItemColumn0.
    
    arrayvar2[1] = ultraListViewSubItemColumn0.
    THIS-OBJECT:ultraListView1:SubItemColumns:AddRange(arrayvar2).
   
END. 


/* used for getting column object from temp-table  for setting properties */ 
METHOD PUBLIC Infragistics.Win.UltraWinListView.UltraListViewSubItemColumn SubItemColumn(INPUT iColumnNumber AS INTEGER):
   FIND FIRST ttField WHERE ttField.FieldSeq = iColumnNumber NO-LOCK NO-ERROR. 
   IF AVAIL ttField  THEN
       RETURN  CAST(ttField.ColumnListViewSubItem,Infragistics.Win.UltraWinListView.UltraListViewSubItemColumn).
END.



METHOD PUBLIC LOGICAL hideColumn(INPUT iColumnNumber AS INTEGER):
    SubItemColumn(iColumnNumber):VisibleinDetailsView = Infragistics.Win.DefaultableBoolean:FALSE.
END. 


METHOD PUBLIC LOGICAL ColumnLabel(INPUT iColumnNumber AS INTEGER, INPUT icLabel AS CHAR):
     IF iColumnNumber = 0  THEN THIS-OBJECT:ultraListView1:MainColumn:KEY = icLabel.
     ELSE 
        SubItemColumn(iColumnNumber):KEY = icLabel.
END. 


    
METHOD PUBLIC LOGICAL Layout(INPUT cLayout AS CHAR):
        
    IF cLayout = "Details" THEN 
      THIS-OBJECT:ultraListView1:View = Infragistics.Win.UltraWinListView.UltraListViewStyle:Details.
    IF cLayout = "Icons" THEN
      THIS-OBJECT:ultraListView1:View = Infragistics.Win.UltraWinListView.UltraListViewStyle:icons.
    IF cLayout = "List" OR cLayout = "" OR cLayout = ? THEN
      THIS-OBJECT:ultraListView1:View = Infragistics.Win.UltraWinListView.UltraListViewStyle:List.
    IF cLayout = "Tiles" THEN
      THIS-OBJECT:ultraListView1:View = Infragistics.Win.UltraWinListView.UltraListViewStyle:Tiles.
    IF cLayout = "Thumbnails" THEN
      THIS-OBJECT:ultraListView1:View = Infragistics.Win.UltraWinListView.UltraListViewStyle:Thumbnails.
    
  END. 
      
	
END CLASS.
