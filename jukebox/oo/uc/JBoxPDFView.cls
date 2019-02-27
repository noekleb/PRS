USING Progress.Lang.*.
USING Progress.Windows.UserControl.
USING Progress.Windows.Form.
USING uc.* FROM PROPATH.


ROUTINE-LEVEL ON ERROR UNDO, THROW.

CLASS uc.JBoxPDFView INHERITS uc.JBoxUCbase: 

  DEFINE PUBLIC VARIABLE FormField AS O2S.Components.PDFView4NET.Forms.PDFField EXTENT NO-UNDO.
  DEFINE PUBLIC VARIABLE FormFieldNumber AS INT NO-UNDO. 
  	
  DEFINE PRIVATE VARIABLE components      AS System.ComponentModel.IContainer       NO-UNDO.
  DEFINE PRIVATE VARIABLE pdfPageView1    AS O2S.Components.PDFView4NET.PDFPageView NO-UNDO.
  DEFINE PRIVATE VARIABLE pdfDocument1    AS O2S.Components.PDFView4NET.PDFDocument NO-UNDO.
  DEFINE PUBLIC VARIABLE PDFFieldTable AS HANDLE NO-UNDO.
  DEFINE PUBLIC VARIABLE SaveToFileName AS CHAR INIT ? NO-UNDO. 
  
DEFINE TEMP-TABLE PDFField NO-UNDO
   FIELD Object    AS Progress.Lang.Object
   FIELD fieldValue     AS CHAR
   FIELD FieldName AS CHAR 
   FIELD Number    AS INT  
   FIELD ReadOnly  AS LOGICAL INIT FALSE 
   FIELD Required  AS LOGICAL INIT FALSE
   FIELD Type      AS CHAR 
   FIELD NoExport  AS LOGICAL INIT FALSE. 
        
      

  CONSTRUCTOR PUBLIC JBoxPDFView (  INPUT ihWinSuper    AS HANDLE, /* Window super proc - unknown if object created from window */
                                    INPUT ihWinProc     AS HANDLE,
                                    INPUT ihPlaceHolder AS HANDLE, /* Design widget (rectangle, field, etc) */
                                    INPUT iiDeltaWidth  AS INTEGER):
     SUPER(ihWinSuper,ihWinProc,ihPlaceHolder,iiDeltaWidth).
     PDFFieldTable = BUFFER PDFField:handle.   
   
  END CONSTRUCTOR.
  
  
  CONSTRUCTOR PUBLIC JBoxPDFView ( INPUT ihWinProc     AS HANDLE,
                                   INPUT ihPlaceHolder AS HANDLE ): /* Design widget (rectangle, field, etc) */
                                   
     SUPER(?,ihWinProc,ihPlaceHolder,0).
     PDFFieldTable = BUFFER PDFField:handle.   
   
  END CONSTRUCTOR.


	
	METHOD PUBLIC OVERRIDE VOID InitializeComponent(  ):

        THIS-OBJECT:components = NEW System.ComponentModel.Container().
        
        DEFINE VARIABLE pdfDocumentInformation1 AS O2S.Components.PDFView4NET.PDFDocumentInformation NO-UNDO.
        DEFINE VARIABLE pdfxmpMetadata1         AS O2S.Components.PDFView4NET.PDFXMPMetadata NO-UNDO.
   
        pdfDocumentInformation1 = NEW O2S.Components.PDFView4NET.PDFDocumentInformation().
        pdfxmpMetadata1 = NEW O2S.Components.PDFView4NET.PDFXMPMetadata().
   
        THIS-OBJECT:pdfDocument1 = NEW O2S.Components.PDFView4NET.PDFDocument(THIS-OBJECT:components).
        THIS-OBJECT:pdfPageView1 = NEW O2S.Components.PDFView4NET.PDFPageView().
        
        THIS-OBJECT:SuspendLayout().
        /*  */
        /* pdfDocument1 */
        /*  
        pdfDocumentInformation1:Author = "KAI Research, Inc.".
        pdfDocumentInformation1:CreationDate = Progress.Util.DateTimeHelper:Create(2014, 5, 9, 10, 52, 25, 0).
        pdfDocumentInformation1:Creator = "Microsoft® Word 2010".
        pdfDocumentInformation1:Keywords = "".
        pdfDocumentInformation1:ModDate = Progress.Util.DateTimeHelper:Create(2014, 5, 9, 10, 52, 25, 0).
        pdfDocumentInformation1:Producer = "Microsoft® Word 2010".
        pdfDocumentInformation1:Subject = "".
        pdfDocumentInformation1:Title = "SAE report form".
        */ 
         
        /* THIS-OBJECT:pdfDocument1:DocumentInformation = pdfDocumentInformation1. */
       /* THIS-OBJECT:pdfDocument1:FilePath = "c:\tmp\xxx.pdf". */
        pdfxmpMetadata1:XMPMetadata = "".
        THIS-OBJECT:pdfDocument1:Metadata = pdfxmpMetadata1.
        THIS-OBJECT:pdfDocument1:PageLayout = O2S.Components.PDFView4NET.PDFPageLayout:SinglePage.
        THIS-OBJECT:pdfDocument1:PageMode = O2S.Components.PDFView4NET.PDFPageMode:UseNone.
        THIS-OBJECT:pdfDocument1:SerialNumber = "PDFVW4WIN-3EJMD-C28Y8-SZMUF-RN3T4-UKN4H".
      
        /*  */
        /* pdfPageView1 */
        /*  */
        THIS-OBJECT:pdfPageView1:AutoScroll = TRUE.
        THIS-OBJECT:pdfPageView1:AutoScrollMinSize = NEW System.Drawing.Size(0, 1056).
        THIS-OBJECT:pdfPageView1:DefaultEllipseAnnotationBorderWidth = 1.
        THIS-OBJECT:pdfPageView1:DefaultInkAnnotationWidth = 1.
        THIS-OBJECT:pdfPageView1:DefaultRectangleAnnotationBorderWidth = 1.
        THIS-OBJECT:pdfPageView1:Dock = System.Windows.Forms.DockStyle:Fill.
        THIS-OBJECT:pdfPageView1:Document = THIS-OBJECT:pdfDocument1.
        THIS-OBJECT:pdfPageView1:DownscaleLargeImages = FALSE.
        THIS-OBJECT:pdfPageView1:EnableRepeatedKeys = FALSE.
        THIS-OBJECT:pdfPageView1:Location = NEW System.Drawing.Point(0, 0).
        THIS-OBJECT:pdfPageView1:Name = "pdfPageView1".
        THIS-OBJECT:pdfPageView1:PageNumber = 0.
        THIS-OBJECT:pdfPageView1:RenderingProgressColor = System.Drawing.Color:Empty.
        THIS-OBJECT:pdfPageView1:RequiredFormFieldHighlightColor = System.Drawing.Color:Empty.
        THIS-OBJECT:pdfPageView1:ScrollPosition = NEW System.Drawing.Point(0, 0).
        THIS-OBJECT:pdfPageView1:Size = NEW System.Drawing.Size(939, 483).
        THIS-OBJECT:pdfPageView1:SubstituteFonts = ?.
        THIS-OBJECT:pdfPageView1:TabIndex = 0.
        THIS-OBJECT:pdfPageView1:WorkMode = O2S.Components.PDFView4NET.UserInteractiveWorkMode:None.
        /*  */
        /* PDFView */
        /*  */
        THIS-OBJECT:ClientSize = NEW System.Drawing.Size(939, 483).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:pdfPageView1).
        THIS-OBJECT:Name = "PDFView".
        THIS-OBJECT:Text = "PDFView".
 
    SUPER:InitializeComponent().

  END METHOD.


METHOD PUBLIC LOGICAL setFormFields ():
    DEFINE VARIABLE oCollectionList AS CLASS System.Collections.IList NO-UNDO.
    DEFINE VARIABLE oCollectionFormFields AS CLASS  O2S.Components.PDFView4NET.Forms.PDFFieldCollection  .
    DEFINE VARIABLE iFieldNum AS INTEGER NO-UNDO.
    
    oCollectionFormFields = pdfDocument1:form:Fields.
    oCollectionList = CAST(oCollectionFormFields,System.Collections.IList).
    
    EXTENT(FormField) = oCollectionList:Count. 
    DO iFieldNum = 1 TO EXTENT(FormField):
         FormField[iFieldNum] = CAST(oCollectionList:item[iFieldNum - 1],O2S.Components.PDFView4NET.Forms.PDFField).
    END.
    FORMFIELDNumber =  oCollectionList:Count.
     
    EMPTY TEMP-TABLE pdfField. 
    DO iFieldNum = 1 TO EXTENT(FormField):
      CREATE pdfField. 
         pdffield.object     = FormField[iFieldNum].
         pdffield.Number     = iFieldNum.
         pdffield.FieldValue = FormField[iFieldNum]:value.
         pdffield.FieldName  =  FormField[iFieldNum]:name.
         pdffield.readonly   = FormField[iFieldNum]:readonly .
         pdffield.noExport   = FormField[iFieldNum]:noexport .
         pdffield.Type       = STRING(FormField[iFieldNum]:Type) .
    END.    
     
END METHOD.
   

  METHOD PUBLIC LOGICAL SaveDocumentToFile(INPUT cFileName AS CHAR ):
    THIS-OBJECT:pdfDocument1:save(cFileName).
    RETURN TRUE.
  END METHOD.
  
  
  METHOD PUBLIC LOGICAL SaveDocumentToDatabase(INPUT icContext AS CHAR,
                                               INPUT icEntityId AS CHAR,
                                               INPUT ipcDescription AS CHAR ):
    
    IF SaveToFileName = ? THEN 
    DO:
        SaveToFileName = "Form-" + GUID(GENERATE-UUID) + ".pdf".
    END.
    
    THIS-OBJECT:pdfDocument1:SAVE(SaveToFileName).
    
    DYNAMIC-FUNCTION("LoadDocs",SEARCH(SaveToFileName),icContext,icEntityId,ipcDescription).    
    OS-DELETE VALUE (SaveToFileName) NO-ERROR.                                      
       
    RETURN TRUE.
  END METHOD.

  METHOD PUBLIC LOGICAL OpenDocument(INPUT cFileName AS CHAR ):
    THIS-OBJECT:pdfDocument1:FilePath = cFileName.
    setFormFields ().
    RETURN TRUE.
  END METHOD.


 
/* Mandatory methods: */
  METHOD PUBLIC O2S.Components.PDFView4NET.PDFPageView getDotNetWidget():
    RETURN pdfPageView1.
  END METHOD.

  METHOD PUBLIC CHARACTER getDotNetWidgetType():
    RETURN STRING(pdfPageView1:GetType()).
  END METHOD.

  METHOD PUBLIC CHARACTER DEFAULT():
    RETURN DYNAMIC-INVOKE(THIS-OBJECT,ENTRY(1,PROGRAM-NAME(2)," ")).
  END METHOD.
  
DESTRUCTOR PUBLIC JBoxPDFView ( ):
   
   THIS-OBJECT:pdfDocument1:close().
   delete object pdfDocument1.
   delete object pdfPageView1.
   
        IF VALID-OBJECT(components) THEN DO:
            CAST(components, System.IDisposable):Dispose().
        END.

    END DESTRUCTOR.

END CLASS.
