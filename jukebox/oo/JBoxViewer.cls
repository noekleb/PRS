 
 /*------------------------------------------------------------------------
    File        : JBoxViewer
    Purpose     : 
    Syntax      : 
    Description : Start and manage an embedded frame (suppressed window)
    Author(s)   : brynjar
    Created     : Wed Aug 26 09:07:12 CEST 2015
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.

CLASS JBoxViewer: 
	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
  DEF VAR hSourceProc     AS HANDLE     NO-UNDO.
  DEF VAR bOk             AS LOG        NO-UNDO.
  DEF VAR ix              AS INT        NO-UNDO.
  DEF VAR cOptions        AS CHAR       NO-UNDO.
  DEF VAR oChildBrowse    AS JBoxBrowse NO-UNDO.
  DEF VAR oChildQuery     AS JBoxQuery  NO-UNDO.
  DEF VAR cLinkFields     AS CHAR       NO-UNDO.
  DEF VAR cDefaultLinkFields AS CHAR    NO-UNDO.
  DEF VAR hHelperProc     AS HANDLE     NO-UNDO.
  DEF VAR bParentIsBrowse AS LOG        NO-UNDO.
  DEF VAR hChildQryOrBrw  AS HANDLE     NO-UNDO.
      
  DEF PUBLIC PROPERTY PARENT-BROWSE-OBJECT AS JBoxBrowse GET. SET. 
  DEF PUBLIC PROPERTY PARENT-QUERY-OBJECT AS JBoxQuery GET. SET. 

  DEF PUBLIC PROPERTY CHILD-BROWSE-HANDLE AS HANDLE GET. PRIVATE SET.
  DEF PUBLIC PROPERTY CHILD-QUERY-HANDLE AS HANDLE GET. PRIVATE SET.
  DEF PUBLIC PROPERTY CHILD-QUERY-OBJECT AS JBoxQuery GET. PRIVATE SET. 
  DEF PUBLIC PROPERTY CHILD-BROWSE-OBJECT AS JBoxBrowse GET. PRIVATE SET. 
  DEF PUBLIC PROPERTY CHILD-TOOLBAR-OBJECT AS JBoxToolbar GET. PRIVATE SET. 

  DEF PUBLIC PROPERTY VIEWER-HANDLE AS HANDLE GET. PRIVATE SET. 
  DEF PUBLIC PROPERTY VIEWER-FRAME AS HANDLE GET. PRIVATE SET. 
  DEF PUBLIC PROPERTY PLACEHOLDER AS HANDLE GET. PRIVATE SET. 
		
  CONSTRUCTOR PUBLIC JBoxViewer (ihPlaceholder  AS HANDLE
                                ,ioBrowseObject AS JBoxBrowse
                                ,icLinkType     AS CHAR
                                ,icLinkFields   AS CHAR
                                ,icViewerProc   AS CHAR):
      SUPER ().

    IF PROVERSION < "11" THEN hSourceProc = DYNAMIC-FUNCTION ("getProcedureHandle",PROGRAM-NAME(2)).
    ELSE hSourceProc = SOURCE-PROCEDURE.
    
    THIS-OBJECT:PARENT-BROWSE-OBJECT = ioBrowseObject.                       
    bParentIsBrowse = VALID-OBJECT(ioBrowseObject).                       

    InitializeComponent(ihPlaceholder
                       ,IF VALID-OBJECT(ioBrowseObject) THEN ioBrowseObject:BROWSE-HANDLE ELSE ?
                       ,icLinkType
                       ,icLinkFields
                       ,icViewerProc).
                                              
  END CONSTRUCTOR.
  
  CONSTRUCTOR PUBLIC JBoxViewer (ihPlaceholder  AS HANDLE
                                ,ioQueryObject  AS JBoxQuery
                                ,icLinkType     AS CHAR
                                ,icLinkFields   AS CHAR
                                ,icViewerProc   AS CHAR):
      SUPER ().

    IF PROVERSION < "11" THEN hSourceProc = DYNAMIC-FUNCTION ("getProcedureHandle",PROGRAM-NAME(2)).
    ELSE hSourceProc = SOURCE-PROCEDURE.

    THIS-OBJECT:PARENT-QUERY-OBJECT = ioQueryObject.
                           
    InitializeComponent(ihPlaceholder
                       ,IF VALID-OBJECT(ioQueryObject) THEN ioQueryObject:QUERY-HANDLE ELSE ?
                       ,icLinkType
                       ,icLinkFields
                       ,icViewerProc).
                                              
  END CONSTRUCTOR.

  CONSTRUCTOR PUBLIC JBoxViewer (ihPlaceholder  AS HANDLE
                                ,ioBrowseObject AS JBoxBrowse
                                ,icViewerProc   AS CHAR):
      SUPER ().

    IF PROVERSION < "11" THEN hSourceProc = DYNAMIC-FUNCTION ("getProcedureHandle",PROGRAM-NAME(2)).
    ELSE hSourceProc = SOURCE-PROCEDURE.
    
    THIS-OBJECT:PARENT-BROWSE-OBJECT = ioBrowseObject.
    bParentIsBrowse = VALID-OBJECT(ioBrowseObject).                       

    InitializeComponent(ihPlaceholder
                       ,IF VALID-OBJECT(ioBrowseObject) THEN ioBrowseObject:BROWSE-HANDLE ELSE ?
                       ,""
                       ,""
                       ,icViewerProc).
                                              
  END CONSTRUCTOR.
  
  CONSTRUCTOR PUBLIC JBoxViewer (ihPlaceholder  AS HANDLE
                                ,ioQueryObject  AS JBoxQuery
                                ,icViewerProc   AS CHAR):
      SUPER ().

    IF PROVERSION < "11" THEN hSourceProc = DYNAMIC-FUNCTION ("getProcedureHandle",PROGRAM-NAME(2)).
    ELSE hSourceProc = SOURCE-PROCEDURE.

    THIS-OBJECT:PARENT-QUERY-OBJECT = ioQueryObject.                       

    InitializeComponent(ihPlaceholder
                       ,IF VALID-OBJECT(ioQueryObject) THEN ioQueryObject:QUERY-HANDLE ELSE ?
                       ,""
                       ,""
                       ,icViewerProc).
                                              
  END CONSTRUCTOR.

  CONSTRUCTOR PUBLIC JBoxViewer (ihPlaceholder  AS HANDLE
                                ,icViewerProc   AS CHAR):
      SUPER ().

    IF PROVERSION < "11" THEN hSourceProc = DYNAMIC-FUNCTION ("getProcedureHandle",PROGRAM-NAME(2)).
    ELSE hSourceProc = SOURCE-PROCEDURE.

    InitializeComponent(ihPlaceholder
                       ,?
                       ,""
                       ,""
                       ,icViewerProc).
                                              
  END CONSTRUCTOR.
  
  METHOD PRIVATE VOID initializeComponent(ihPlaceholder AS HANDLE
                                         ,ihQuery       AS HANDLE
                                         ,icLinkType    AS CHAR
                                         ,icLinkFields  AS CHAR
                                         ,icViewerProc  AS CHAR):
                        
    RUN JBoxViewerHelper.p PERSIST SET hHelperProc (THIS-OBJECT).          
    
    VIEWER-HANDLE = DYNAMIC-FUNCTION("NewViewer",
                   ihPlaceHolder,
                   ihQuery,                 /* Navigation query */
                   icViewerProc).

    IF VALID-OBJECT(CHILD-BROWSE-OBJECT) THEN          
      ASSIGN CHILD-BROWSE-HANDLE = CHILD-BROWSE-OBJECT:BROWSE-HANDLE
             hChildQryOrBrw      = CHILD-BROWSE-HANDLE.
    ELSE IF VALID-OBJECT(CHILD-QUERY-OBJECT) THEN
      ASSIGN CHILD-QUERY-HANDLE = CHILD-QUERY-OBJECT:QUERY-HANDLE
             hChildQryOrBrw     = CHILD-QUERY-HANDLE.
    ELSE DO:            
      CHILD-BROWSE-HANDLE = DYNAMIC-FUNCTION("GetLinkedObject",VIEWER-HANDLE,"browse","from").
      IF NOT VALID-HANDLE(CHILD-BROWSE-HANDLE) THEN
        ASSIGN CHILD-QUERY-HANDLE = DYNAMIC-FUNCTION("GetLinkedObject",VIEWER-HANDLE,"query","from")
               hChildQryOrBrw     = CHILD-QUERY-HANDLE.
      ELSE hChildQryOrBrw = CHILD-BROWSE-HANDLE.         
    END.
              
    IF VALID-HANDLE(ihQuery) AND icLinkFields NE "" THEN DO:
  
      IF icLinkType = "oneToOne" THEN DO:
        IF bParentIsBrowse THEN DO:
          IF VALID-OBJECT(CHILD-QUERY-OBJECT) THEN
            CHILD-QUERY-OBJECT:setOneToOneBrowseObject(PARENT-BROWSE-OBJECT,icLinkFields).
          ELSE  
            DYNAMIC-FUNCTION("createOneToOneLink",hChildQryOrBrw,PARENT-BROWSE-OBJECT:BROWSE-HANDLE,icLinkFields).
        END.
      END.
      ELSE DO:
        IF bParentIsBrowse THEN DO:
          IF VALID-OBJECT(CHILD-BROWSE-OBJECT) THEN
            CHILD-BROWSE-OBJECT:setParentBrowseObject(PARENT-BROWSE-OBJECT,icLinkFields).
          ELSE IF VALID-OBJECT(CHILD-QUERY-OBJECT) THEN
            CHILD-QUERY-OBJECT:setParentBrowseObject(PARENT-BROWSE-OBJECT,icLinkFields).
          ELSE  
            DYNAMIC-FUNCTION("createParentLink",hChildQryOrBrw,PARENT-BROWSE-OBJECT:BROWSE-HANDLE,icLinkFields).
        END.
        ELSE DO:
          IF VALID-OBJECT(CHILD-BROWSE-OBJECT) THEN
            CHILD-BROWSE-OBJECT:setParentQueryObject(PARENT-QUERY-OBJECT,icLinkFields).
          ELSE IF VALID-OBJECT(CHILD-QUERY-OBJECT) THEN
            CHILD-QUERY-OBJECT:setParentQueryObject(PARENT-QUERY-OBJECT,icLinkFields).
          ELSE  
            DYNAMIC-FUNCTION("createParentLink",hChildQryOrBrw,PARENT-QUERY-OBJECT:QUERY-HANDLE,icLinkFields).
        END.  
      END.  
    END.
    
    ASSIGN PLACEHOLDER   = ihPlaceholder
           VIEWER-FRAME = DYNAMIC-FUNCTION("getFrameHandle" IN VIEWER-HANDLE)
           .
           
  END METHOD.                                       

  METHOD PUBLIC VOID setChildBrowseObject (ioChildBrowse AS JBoxBrowse):
    CHILD-BROWSE-OBJECT = ioChildBrowse.                                        
  END METHOD.
                                              
  METHOD PUBLIC VOID setChildBrowseObjectIfVoid (ioChildBrowse AS JBoxBrowse):
    IF NOT VALID-OBJECT(CHILD-BROWSE-OBJECT) THEN                                              
      CHILD-BROWSE-OBJECT = ioChildBrowse.                                        
  END METHOD.                                            

  METHOD PUBLIC VOID setChildQueryObjectIfVoid (ioChildQuery AS JBoxQuery):
    IF NOT VALID-OBJECT(CHILD-QUERY-OBJECT) THEN                                              
      CHILD-QUERY-OBJECT = ioChildQuery.                                        
  END METHOD.                                            

  METHOD PUBLIC VOID setChildToolbarObjectIfVoid (ioChildToolbar AS JBoxToolbar):
    IF NOT VALID-OBJECT(CHILD-TOOLBAR-OBJECT) THEN                                              
      CHILD-TOOLBAR-OBJECT = ioChildToolbar.                                        
  END METHOD.                                            
    
  METHOD PUBLIC VOID moveViewerToTop():
    RUN MoveToTop IN VIEWER-HANDLE NO-ERROR.
  END.  

  METHOD PUBLIC VOID setNoResizeX(INPUT icWidgetNameList AS CHAR):
    PUBLISH "DoResizeSetting" FROM VIEWER-HANDLE (ENTRY(1,PROGRAM-NAME(1)," "),VIEWER-FRAME,icWidgetNameList).
  END METHOD.
  METHOD PUBLIC VOID setNoResizeY(INPUT icWidgetNameList AS CHAR):
    PUBLISH "DoResizeSetting" FROM VIEWER-HANDLE (ENTRY(1,PROGRAM-NAME(1)," "),VIEWER-FRAME,icWidgetNameList).
  END METHOD.
  METHOD PUBLIC VOID setAddResizeX(INPUT icWidgetNameList AS CHAR):
    PUBLISH "DoResizeSetting" FROM VIEWER-HANDLE (ENTRY(1,PROGRAM-NAME(1)," "),VIEWER-FRAME,icWidgetNameList).
  END METHOD.
  METHOD PUBLIC VOID setAddResizeY(INPUT icWidgetNameList AS CHAR):
    PUBLISH "DoResizeSetting" FROM VIEWER-HANDLE (ENTRY(1,PROGRAM-NAME(1)," "),VIEWER-FRAME,icWidgetNameList).
  END METHOD.

  METHOD PUBLIC VOID setNoMoveX(INPUT icWidgetNameList AS CHAR):
    PUBLISH "DoResizeSetting" FROM VIEWER-HANDLE (ENTRY(1,PROGRAM-NAME(1)," "),VIEWER-FRAME,icWidgetNameList).
  END METHOD.
  METHOD PUBLIC VOID setNoMoveY(INPUT icWidgetNameList AS CHAR):
    PUBLISH "DoResizeSetting" FROM VIEWER-HANDLE (ENTRY(1,PROGRAM-NAME(1)," "),VIEWER-FRAME,icWidgetNameList).
  END METHOD.
  METHOD PUBLIC VOID setAddMoveX(INPUT icWidgetNameList AS CHAR):
    PUBLISH "DoResizeSetting" FROM VIEWER-HANDLE (ENTRY(1,PROGRAM-NAME(1)," "),VIEWER-FRAME,icWidgetNameList).
  END METHOD.
  METHOD PUBLIC VOID setAddMoveY(INPUT icWidgetNameList AS CHAR):
    PUBLISH "DoResizeSetting" FROM VIEWER-HANDLE (ENTRY(1,PROGRAM-NAME(1)," "),VIEWER-FRAME,icWidgetNameList).
  END METHOD.


	DESTRUCTOR PUBLIC JBoxViewer ( ):
    DELETE PROCEDURE hHelperProc NO-ERROR.
	END DESTRUCTOR.

END CLASS.