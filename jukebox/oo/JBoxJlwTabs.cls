 
 /*------------------------------------------------------------------------
    File        : JBoxJlwTabs
    Purpose     : Create tab-folder using Controls.dll
    Syntax      : 
    Description : 
    Author(s)   : Brynjar
    Created     : Mon Feb 09 2015
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.

CLASS JBoxJlwTabs:
  DEF VAR hSourceProc        AS HANDLE     NO-UNDO.
  DEF VAR hTabFolder         AS HANDLE     NO-UNDO.
  DEF VAR bFirstPage         AS LOG        NO-UNDO INIT YES.
  DEF VAR oChildBrowse       AS JBoxBrowse NO-UNDO EXTENT 10.
  DEF VAR oChildQuery        AS JBoxQuery  NO-UNDO EXTENT 10.
  DEF VAR oChildToolbar      AS JBoxToolbar NO-UNDO EXTENT 10.
  DEF VAR cLinkFields        AS CHAR       NO-UNDO EXTENT 10.
  DEF VAR cTabLabels         AS CHAR       NO-UNDO EXTENT 10.
  DEF VAR cDefaultLinkFields AS CHAR       NO-UNDO.
  DEF VAR bInitialRun        AS LOG        NO-UNDO INIT YES.
  DEF VAR iNumPages          AS INT        NO-UNDO.
    
  DEF PUBLIC PROPERTY PARENT-BROWSE-OBJECT AS JBoxBrowse GET. 
      SET(ioBrowseObject AS JBoxBrowse):
        DYNAMIC-FUNCTION("setParentQuery" IN hTabfolder,ioBrowseObject:BROWSE-HANDLE).
        PARENT-BROWSE-OBJECT = ioBrowseObject.
      END SET.

  DEF PUBLIC PROPERTY PARENT-QUERY-OBJECT AS JBoxQuery GET. 
      SET(ioQueryObject AS JBoxQuery):
        DYNAMIC-FUNCTION("setParentQuery" IN hTabfolder,ioQueryObject:QUERY-HANDLE).
        PARENT-QUERY-OBJECT = ioQueryObject.
      END SET.

  DEF PUBLIC PROPERTY PARENT-TAB-FOLDER-OBJECT AS JBoxJlwTabs GET. SET. 

  DEF PUBLIC PROPERTY TAB-FOLDER-HANDLE AS HANDLE GET. PROTECTED SET. 
  DEF PUBLIC PROPERTY TAB-FOLDER-FRAME  AS HANDLE GET(): DYNAMIC-FUNCTION("getTabFrame" IN hTabFolder). END GET. PROTECTED SET. 
  DEF PUBLIC PROPERTY PrevTabNum        AS INT  GET. PROTECTED SET.
  DEF PUBLIC PROPERTY CurrTabNum        AS INT  GET. SET(iiTab AS INT): PrevTabNum = CurrTabNum. CurrTabNum = iiTab. END SET.
  DEF PUBLIC PROPERTY CurrTabLabel      AS CHAR GET(): RETURN IF CurrTabNum > 0 THEN cTabLabels[CurrTabNum] ELSE "". END GET. PROTECTED SET.

  DEF PUBLIC PROPERTY PLACEHOLDER AS HANDLE GET. PROTECTED SET. 
      
  DEF PUBLIC PROPERTY pageOneType AS CHAR GET. 
      SET(icRole AS CHAR):
        IF NOT CAN-DO("oneToOne,navigation,child",icRole) THEN 
          MESSAGE "Error assigning role for page 1. Valid options: oneToOne, Navigation or Child" SKIP
                  "(Assuming oneToOne)"
                  VIEW-AS ALERT-BOX.
        ELSE pageOneType = icRole.          
      END SET.

  DEF PUBLIC PROPERTY pageTwoType AS CHAR INIT "oneToOne" GET. 
      SET(icRole AS CHAR):
        IF NOT CAN-DO("oneToOne,child",icRole) THEN 
          MESSAGE "Error assigning role for page 2. Valid options: oneToOne or Child" SKIP
                  "(Assuming oneToOne)" SKIP
                  "This setting is relevant when the navigation query is on page 1"
                  VIEW-AS ALERT-BOX.
        ELSE pageTwoType = icRole.          
      END SET.
  
  CONSTRUCTOR PUBLIC JBoxJlwTabs (ihPlaceholder AS HANDLE):
      SUPER ().

    IF PROVERSION < "11" THEN hSourceProc = DYNAMIC-FUNCTION ("getProcedureHandle",PROGRAM-NAME(2)).
    ELSE hSourceProc = SOURCE-PROCEDURE.

    InitializeComponent(ihPlaceholder,?,"").                 
      
  END CONSTRUCTOR.

  CONSTRUCTOR PUBLIC JBoxJlwTabs (ihPlaceholder AS HANDLE
                                 ,icImageList   AS CHAR):
      SUPER ().

    IF PROVERSION < "11" THEN hSourceProc = DYNAMIC-FUNCTION ("getProcedureHandle",PROGRAM-NAME(2)).
    ELSE hSourceProc = SOURCE-PROCEDURE.

    InitializeComponent(ihPlaceholder,?,icImageList).                 
      
  END CONSTRUCTOR.

  CONSTRUCTOR PUBLIC JBoxJlwTabs (ihPlaceholder AS HANDLE
                                 ,ioBrowseObject AS JBoxBrowse):
      SUPER ().

    IF PROVERSION < "11" THEN hSourceProc = DYNAMIC-FUNCTION ("getProcedureHandle",PROGRAM-NAME(2)).
    ELSE hSourceProc = SOURCE-PROCEDURE.

    InitializeComponent(ihPlaceholder
                       ,IF VALID-OBJECT(ioBrowseObject) THEN ioBrowseObject:BROWSE-HANDLE ELSE ?
                       ,"").
                                        
    THIS-OBJECT:PARENT-BROWSE-OBJECT = ioBrowseObject.                       
    ioBrowseObject:TAB-FOLDER-OBJECT = THIS-OBJECT.                       
      
  END CONSTRUCTOR.
  
  CONSTRUCTOR PUBLIC JBoxJlwTabs (ihPlaceholder AS HANDLE
                                 ,ioQueryObject AS JBoxQuery):
      SUPER ().

    IF PROVERSION < "11" THEN hSourceProc = DYNAMIC-FUNCTION ("getProcedureHandle",PROGRAM-NAME(2)).
    ELSE hSourceProc = SOURCE-PROCEDURE.

    InitializeComponent(ihPlaceholder
                       ,IF VALID-OBJECT(ioQueryObject) THEN ioQueryObject:QUERY-HANDLE ELSE ?
                       ,"").
                                        
    THIS-OBJECT:PARENT-QUERY-OBJECT = ioQueryObject.
    ioQueryObject:TAB-FOLDER-OBJECT = THIS-OBJECT.                       
      
  END CONSTRUCTOR.

  CONSTRUCTOR PUBLIC JBoxJlwTabs (ihPlaceholder  AS HANDLE
                                 ,ioBrowseObject AS JBoxBrowse
                                 ,icImageList    AS CHAR):
      SUPER ().

    IF PROVERSION < "11" THEN hSourceProc = DYNAMIC-FUNCTION ("getProcedureHandle",PROGRAM-NAME(2)).
    ELSE hSourceProc = SOURCE-PROCEDURE.

    InitializeComponent(ihPlaceholder
                       ,IF VALID-OBJECT(ioBrowseObject) THEN ioBrowseObject:BROWSE-HANDLE ELSE ?
                       ,icImageList).                 
                       
    THIS-OBJECT:PARENT-BROWSE-OBJECT = ioBrowseObject.                       
    ioBrowseObject:TAB-FOLDER-OBJECT = THIS-OBJECT.    
            
  END CONSTRUCTOR.

  CONSTRUCTOR PUBLIC JBoxJlwTabs (ihPlaceholder AS HANDLE
                                 ,ioQueryObject AS JBoxQuery
                                 ,icImageList   AS CHAR):
      SUPER ().

    IF PROVERSION < "11" THEN hSourceProc = DYNAMIC-FUNCTION ("getProcedureHandle",PROGRAM-NAME(2)).
    ELSE hSourceProc = SOURCE-PROCEDURE.

    InitializeComponent(ihPlaceholder
                       ,IF VALID-OBJECT(ioQueryObject) THEN ioQueryObject:QUERY-HANDLE ELSE ?
                       ,icImageList).                 
                       
    THIS-OBJECT:PARENT-QUERY-OBJECT = ioQueryObject.                       
    ioQueryObject:TAB-FOLDER-OBJECT = THIS-OBJECT.                       
      
  END CONSTRUCTOR.

  METHOD PRIVATE VOID initializeComponent(ihPlaceholder AS HANDLE
                                         ,ihQuery       AS HANDLE
                                         ,icImageList   AS CHAR
                                          ):
           
    DYNAMIC-FUNCTION("setAttribute",ihPlaceHolder,"TabFolderProg","JBoxJlwTabFolder.w").

    DYNAMIC-FUNCTION("setObjectSourceProc",hSourceProc).  

    hTabFolder = DYNAMIC-FUNCTION("NewTabFolder",ihPlaceholder).

    DYNAMIC-FUNCTION("setImageList" IN hTabFolder,REPLACE(icImageList,",",";")).
    
    /* X and Y limits for move of widget are not yet set for the window. 
       Since we want the tabs to resize according to the window size these values must be set here and
       they must be exact the same as in setOrwWinSize (see InitResize function) 
       Here the values are set to not give any automatic move of widgets */
       
    DYNAMIC-FUNCTION("setMinXYmove",2000,1200). 
  
    /* Alt 1: If the container has a navigation browse or query add it to the tabfolder call so it can be set in
       the suppressed windows (see the setParentQueryObject function in the template for suppresse window: */
    
    DYNAMIC-FUNCTION("InitPages" IN hTabFolder,
                     "",ihQuery).  
  /*   DYNAMIC-FUNCTION("InitPages" IN hTabFolder,"Page 1|template/JBoxSuppressedWindow.w|Page 2|template/JBoxSuppressedWindow.w",hBrowse). */
    
    DYNAMIC-FUNCTION("setTabObject" IN hTabFolder,THIS-OBJECT).
    
    ASSIGN TAB-FOLDER-HANDLE = hTabFolder
           PLACEHOLDER = ihPlaceholder.
  END METHOD.                                       

  METHOD PUBLIC VOID AddPage (icLabel   AS CHAR
                             ,icLaunch  AS CHAR
                             ,icImage   AS CHAR):
    DEF VAR iCurrTab AS INT NO-UNDO.

    IF icImage NE "" THEN
      ASSIGN icImage = REPLACE(icImage,"/","\")
             icImage = ENTRY(NUM-ENTRIES(icImage,"\"),icImage,"\").

    iCurrTab = DYNAMIC-FUNCTION("addPage" IN hTabFolder,icLabel,icLaunch,icImage).
    
    cTabLabels[iCurrTab] = icLabel.

    IF iCurrTab = 1 THEN 
      DYNAMIC-FUNCTION("TabFolderChanged" IN hTabFolder,1).
    ELSE RUN ViewHideTabUserControl IN hSourceProc (hTabFolder,1) NO-ERROR.  

    iNumPages = iNumPages + 1.

  END METHOD.
  
  METHOD PUBLIC VOID AddPage (icLabel   AS CHAR
                             ,icLaunch  AS CHAR):

    THIS-OBJECT:AddPage(icLabel,icLaunch,"").

  END METHOD.

  METHOD PUBLIC HANDLE getPageQuery (iiTab AS INT):

    RETURN DYNAMIC-FUNCTION("getPageQuery" IN hTabFolder,iiTab).

  END METHOD.

  METHOD PUBLIC HANDLE getPageHandle (iiTab AS INT):
    /* Return the procedure handle for page */
    RETURN DYNAMIC-FUNCTION("getPageHandle" IN hTabFolder,iiTab).
  END METHOD.

  METHOD PUBLIC VOID selectPage (iiTab AS INT):
    /* Invokes also the TabChanged function (input iiTab as int) if it exists */
    DYNAMIC-FUNCTION("TabFolderChanged" IN hTabFolder,iiTab).

  END METHOD.
  
  METHOD PUBLIC VOID setChildBrowseObject (ioChildBrowse AS JBoxBrowse
                                          ,iiTab         AS INT):
    IF ioChildBrowse = ? THEN 
      DYNAMIC-FUNCTION("DeleteObjectLink",oChildBrowse[iiTab]:BROWSE-HANDLE,THIS-OBJECT:getPageHandle(iiTab)).                               
    oChildBrowse[iiTab] = ioChildBrowse.         
  END METHOD.
                                              
  METHOD PUBLIC VOID setChildBrowseObjectIfVoid (ioChildBrowse AS JBoxBrowse
                                                ,iiTab         AS INT):
    IF NOT VALID-OBJECT(oChildBrowse[iiTab]) THEN                                              
      oChildBrowse[iiTab] = ioChildBrowse.                                        
  END METHOD.                                            

  METHOD PUBLIC VOID setChildQueryObjectIfVoid (ioChildQuery AS JBoxQuery
                                         ,iiTab       AS INT):
    IF NOT VALID-OBJECT(oChildQuery[iiTab]) THEN                                              
      oChildQuery[iiTab] = ioChildQuery.                                        
  END METHOD.                                            

  METHOD PUBLIC VOID setChildToolbarObjectIfVoid (ioChildToolbar AS JBoxToolbar
                                                 ,iiTab          AS INT):
    IF NOT VALID-OBJECT(oChildToolbar[iiTab]) THEN                                              
      oChildToolbar[iiTab] = ioChildToolbar.                                        
  END METHOD.                                            

  METHOD PUBLIC JBoxBrowse getPageBrowseObject (iiTab AS INT):
    RETURN oChildBrowse[iiTab].
  END METHOD.

  METHOD PUBLIC JBoxQuery getPageQueryObject (iiTab AS INT):
    RETURN oChildQuery[iiTab].
  END METHOD.

  METHOD PUBLIC JBoxToolbar getPageToolbarObject (iiTab AS INT):
    RETURN oChildToolbar[iiTab].
  END METHOD.

  METHOD PUBLIC HANDLE getPageFrame (iiTab AS INT):
    RETURN DYNAMIC-FUNCTION("getPageFrame" IN hTabFolder,iiTab).
  END METHOD.
  
  METHOD PUBLIC VOID setLinkFields(icLinkFields AS CHAR):
    cDefaultLinkFields = icLinkFields.
  END.
    
  METHOD PUBLIC VOID setLinkFields(icLinkFields AS CHAR,iiTab AS INT):
    cLinkFields[iiTab] = icLinkFields.
  END.  
  
  METHOD PUBLIC VOID movePageToTop():
    DEF VAR hTabProc AS HANDLE NO-UNDO.
    hTabProc = THIS-OBJECT:getPageHandle(CurrTabNum).
    THIS-OBJECT:ViewTabLabel().
    RUN MoveToTop IN hTabProc NO-ERROR.
  END.  

  METHOD PUBLIC VOID movePageToTop(iiTab AS INT):
    DEF VAR hTabProc AS HANDLE NO-UNDO.
    hTabProc = THIS-OBJECT:getPageHandle(iiTab).
    THIS-OBJECT:ViewTabLabel().
    RUN MoveToTop IN hTabProc NO-ERROR.
  END.  
  
  METHOD PUBLIC VOID LinkChildObject (iiTab AS INT):
    DEF VAR bParentIsBrowse AS LOG NO-UNDO.
    DEF VAR cLinkFlds       AS CHAR NO-UNDO.
    
    bParentIsBrowse = VALID-OBJECT(PARENT-BROWSE-OBJECT).

    IF cLinkFields[iiTab] NE "" THEN cLinkFlds = cLinkFields[iiTab].
    ELSE IF cDefaultLinkFields NE "" THEN cLinkFlds = cDefaultLinkFields.
    ELSE DO:
      MESSAGE "Object link info not available for " + hSourceProc:FILE-NAME
              VIEW-AS ALERT-BOX ERROR.
      RETURN.        
    END.

    IF NOT bInitialRun THEN DO: 
      DYNAMIC-FUNCTION("DoLockWindow",hSourceProc:CURRENT-WINDOW).
      DYNAMIC-FUNCTION("setAttribute",SESSION,"userkeepswindowlocked","yes").
    END.       
        
    IF CurrTabNum NE 0 THEN DO:
      IF bParentIsBrowse THEN
        DYNAMIC-FUNCTION("DeleteObjectLink",PARENT-BROWSE-OBJECT:BROWSE-HANDLE,THIS-OBJECT:getPageQuery(CurrTabNum)).
      ELSE
        DYNAMIC-FUNCTION("DeleteObjectLink",PARENT-QUERY-OBJECT:QUERY-HANDLE,THIS-OBJECT:getPageQuery(CurrTabNum)).
    END.  
    ELSE DO:
      IF bParentIsBrowse THEN DO:
        IF VALID-OBJECT(oChildBrowse[iiTab]) AND VALID-OBJECT(oChildBrowse[iiTab]:TAB-FOLDER-OBJECT) THEN
          oChildBrowse[iiTab]:TAB-FOLDER-OBJECT:PARENT-TAB-FOLDER-OBJECT = THIS-OBJECT.
      END.  
      ELSE DO:
        IF VALID-OBJECT(oChildQuery[iiTab]) AND VALID-OBJECT(oChildQuery[iiTab]:TAB-FOLDER-OBJECT) THEN
          oChildQuery[iiTab]:TAB-FOLDER-OBJECT:PARENT-TAB-FOLDER-OBJECT = THIS-OBJECT.
/*        PARENT-TAB-FOLDER-OBJECT = PARENT-QUERY-OBJECT:TAB-FOLDER-OBJECT.*/
      END.  
    END.
    
    IF iiTab = 1 THEN 
      CASE pageOneType:
        WHEN "oneToOne" THEN DO:
          IF bParentIsBrowse THEN DO:
            IF VALID-OBJECT(oChildQuery[iiTab]) THEN
              oChildQuery[iiTab]:setOneToOneBrowseObject(PARENT-BROWSE-OBJECT,cLinkFlds).
            ELSE  
              DYNAMIC-FUNCTION("createOneToOneLink",THIS-OBJECT:getPageQuery(iiTab),PARENT-BROWSE-OBJECT:BROWSE-HANDLE,cLinkFlds).
          END.  
          ELSE DO: 
            IF VALID-OBJECT(oChildQuery[iiTab]) THEN
              oChildQuery[iiTab]:setOneToOneQueryObject(PARENT-QUERY-OBJECT,cLinkFlds).
            ELSE  
              DYNAMIC-FUNCTION("createOneToOneLink",THIS-OBJECT:getPageQuery(iiTab),PARENT-QUERY-OBJECT:QUERY-HANDLE,cLinkFlds).
          END.  
        END.                                             
        WHEN "Child" THEN DO:
          IF bParentIsBrowse THEN DO:
            IF VALID-OBJECT(oChildBrowse[iiTab]) THEN
              oChildBrowse[iiTab]:setParentBrowseObject(PARENT-BROWSE-OBJECT,cLinkFlds).
            ELSE IF VALID-OBJECT(oChildQuery[iiTab]) THEN 
              oChildQuery[iiTab]:setParentBrowseObject(PARENT-BROWSE-OBJECT,cLinkFlds).
            ELSE 
              DYNAMIC-FUNCTION("createParentLink",THIS-OBJECT:getPageQuery(iiTab),PARENT-BROWSE-OBJECT:BROWSE-HANDLE,cLinkFlds).
          END.  
          ELSE DO:  
            IF VALID-OBJECT(oChildBrowse[iiTab]) THEN
              oChildBrowse[iiTab]:setParentQueryObject(PARENT-QUERY-OBJECT,cLinkFlds).
            ELSE IF VALID-OBJECT(oChildQuery[iiTab]) THEN 
              oChildQuery[iiTab]:setParentQueryObject(PARENT-QUERY-OBJECT,cLinkFlds).
            ELSE  
              DYNAMIC-FUNCTION("createParentLink",THIS-OBJECT:getPageQuery(iiTab),PARENT-QUERY-OBJECT:QUERY-HANDLE,cLinkFlds).
          END.    
        END.     
        OTHERWISE DO:
          MESSAGE "Warning link-type for page 1 not defined (yet). You probably need to do a tab-changed to view correct data" SKIP
                  "(This is the programmers mistake .. as most times)" 
          VIEW-AS ALERT-BOX.
        END.                                        
      END CASE.
        
    ELSE IF iiTab = 2 AND pageOneType = "Navigation" THEN 
      CASE pageTwoType:
        WHEN "oneToOne" THEN DO:
          IF bParentIsBrowse THEN DO:
            IF VALID-OBJECT(oChildQuery[iiTab]) THEN
              oChildQuery[iiTab]:setOneToOneBrowseObject(PARENT-BROWSE-OBJECT,cLinkFlds).
            ELSE  
              DYNAMIC-FUNCTION("createOneToOneLink",THIS-OBJECT:getPageQuery(iiTab),PARENT-BROWSE-OBJECT:BROWSE-HANDLE,cLinkFlds).
          END.  
          ELSE DO:
            IF VALID-OBJECT(oChildQuery[iiTab]) THEN
              oChildQuery[iiTab]:setOneToOneQueryObject(PARENT-QUERY-OBJECT,cLinkFlds).
            ELSE  
              DYNAMIC-FUNCTION("createOneToOneLink",THIS-OBJECT:getPageQuery(iiTab),PARENT-QUERY-OBJECT:QUERY-HANDLE,cLinkFlds).
          END.  
        END.
        WHEN "Child" THEN DO:
          IF bParentIsBrowse THEN DO:
            IF VALID-OBJECT(oChildBrowse[iiTab]) THEN
              oChildBrowse[iiTab]:setParentBrowseObject(PARENT-BROWSE-OBJECT,cLinkFlds).
            ELSE IF VALID-OBJECT(oChildQuery[iiTab]) THEN
              oChildQuery[iiTab]:setParentBrowseObject(PARENT-BROWSE-OBJECT,cLinkFlds).
            ELSE  
              DYNAMIC-FUNCTION("createParentLink",THIS-OBJECT:getPageQuery(iiTab),PARENT-BROWSE-OBJECT:BROWSE-HANDLE,cLinkFlds).
          END.  
          ELSE DO:  
            IF VALID-OBJECT(oChildBrowse[iiTab]) THEN
              oChildBrowse[iiTab]:setParentQueryObject(PARENT-QUERY-OBJECT,cLinkFlds).
            ELSE  
              oChildQuery[iiTab]:setParentQueryObject(PARENT-QUERY-OBJECT,cLinkFlds).
          END.    
        END.                                             
      END CASE.
      
    ELSE DO:
      IF bParentIsBrowse THEN DO:
        IF VALID-OBJECT(oChildBrowse[iiTab]) THEN
          oChildBrowse[iiTab]:setParentBrowseObject(PARENT-BROWSE-OBJECT,cLinkFlds).
        ELSE IF VALID-OBJECT(oChildQuery[iiTab]) THEN 
          oChildQuery[iiTab]:setParentBrowseObject(PARENT-BROWSE-OBJECT,cLinkFlds).
        ELSE  
          DYNAMIC-FUNCTION("createParentLink",THIS-OBJECT:getPageQuery(iiTab),PARENT-BROWSE-OBJECT:BROWSE-HANDLE,cLinkFlds).
      END.  
      ELSE DO:  
        IF VALID-OBJECT(oChildBrowse[iiTab]) THEN
          oChildBrowse[iiTab]:setParentQueryObject(PARENT-QUERY-OBJECT,cLinkFlds).
        ELSE IF VALID-OBJECT(oChildQuery[iiTab]) THEN 
          oChildQuery[iiTab]:setParentQueryObject(PARENT-QUERY-OBJECT,cLinkFlds).
        ELSE  
          DYNAMIC-FUNCTION("createParentLink",THIS-OBJECT:getPageQuery(iiTab),PARENT-QUERY-OBJECT:QUERY-HANDLE,cLinkFlds).
      END.    
    END.
    
    CurrTabNum = iiTab.
    
    IF CAN-DO(hSourceProc:INTERNAL-ENTRIES,"preTabDisplayRecord") THEN
      DYNAMIC-FUNCTION("preTabDisplayRecord" IN hSourceProc,THIS-OBJECT).

    IF NOT bInitialRun THEN DO:      
      IF bParentIsBrowse THEN
        PARENT-BROWSE-OBJECT:displayRecord().
      ELSE
        PARENT-QUERY-OBJECT:displayRecord().
        
      DYNAMIC-FUNCTION("setAttribute",SESSION,"userkeepswindowlocked","").
      DYNAMIC-FUNCTION("DoLockWindow",?). 
    END.   
    bInitialRun = NO.
     
  END METHOD.
  
  METHOD PUBLIC VOID ViewTabLabel ():
    DEF VAR hTabFrame AS HANDLE NO-UNDO.
    
    hTabFrame = DYNAMIC-FUNCTION("getTabFrame" IN hTabFolder).
    hTabFrame:MOVE-TO-TOP ().
    
  END METHOD.    

  METHOD PUBLIC VOID setNoResizeX():
    DEF VAR ix AS INT NO-UNDO.
    DEF VAR hFrame AS HANDLE NO-UNDO.
    DYNAMIC-FUNCTION("setNoResizeTabX" IN hTabFolder).
    DO ix = 1 TO iNumPages:
      hFrame = THIS-OBJECT:getPageFrame(ix).
      IF VALID-HANDLE(hFrame) THEN
        PUBLISH "DoResizeSetting" FROM hSourceProc (ENTRY(1,PROGRAM-NAME(1)," "),hFrame,hFrame:NAME).
    END.  
    PUBLISH "DoResizeSetting" FROM hSourceProc (ENTRY(1,PROGRAM-NAME(1)," "),PLACEHOLDER:FRAME,PLACEHOLDER:NAME).
  END METHOD.
  
  METHOD PUBLIC VOID setNoResizeY():
    DEF VAR ix AS INT NO-UNDO.
    DEF VAR hFrame AS HANDLE NO-UNDO.
    DYNAMIC-FUNCTION("setNoResizeTabY" IN hTabFolder).
    DO ix = 1 TO iNumPages:
      hFrame = THIS-OBJECT:getPageFrame(ix).
      IF VALID-HANDLE(hFrame) THEN
        PUBLISH "DoResizeSetting" FROM hSourceProc (ENTRY(1,PROGRAM-NAME(1)," "),hFrame,hFrame:NAME).
    END.  
    PUBLISH "DoResizeSetting" FROM hSourceProc (ENTRY(1,PROGRAM-NAME(1)," "),PLACEHOLDER:FRAME,PLACEHOLDER:NAME).
  END METHOD.  
              
  METHOD PUBLIC VOID setNoMoveX():
    PUBLISH "DoResizeSetting" FROM hSourceProc (ENTRY(1,PROGRAM-NAME(1)," "),TAB-FOLDER-FRAME:FRAME,TAB-FOLDER-FRAME:NAME).
    PUBLISH "DoResizeSetting" FROM hSourceProc (ENTRY(1,PROGRAM-NAME(1)," "),PLACEHOLDER:FRAME,PLACEHOLDER:NAME).
  END METHOD.
  METHOD PUBLIC VOID setNoMoveY():
    PUBLISH "DoResizeSetting" FROM hSourceProc (ENTRY(1,PROGRAM-NAME(1)," "),TAB-FOLDER-FRAME:FRAME,TAB-FOLDER-FRAME:NAME).
    PUBLISH "DoResizeSetting" FROM hSourceProc (ENTRY(1,PROGRAM-NAME(1)," "),PLACEHOLDER:FRAME,PLACEHOLDER:NAME).
  END METHOD.
  METHOD PUBLIC VOID setAddMoveX():
    PUBLISH "DoResizeSetting" FROM hSourceProc (ENTRY(1,PROGRAM-NAME(1)," "),TAB-FOLDER-FRAME:FRAME,TAB-FOLDER-FRAME:NAME).
    PUBLISH "DoResizeSetting" FROM hSourceProc (ENTRY(1,PROGRAM-NAME(1)," "),PLACEHOLDER:FRAME,PLACEHOLDER:NAME).
  END METHOD.
  METHOD PUBLIC VOID setAddMoveY():
    PUBLISH "DoResizeSetting" FROM hSourceProc (ENTRY(1,PROGRAM-NAME(1)," "),TAB-FOLDER-FRAME:FRAME,TAB-FOLDER-FRAME:NAME).
    PUBLISH "DoResizeSetting" FROM hSourceProc (ENTRY(1,PROGRAM-NAME(1)," "),PLACEHOLDER:FRAME,PLACEHOLDER:NAME).
  END METHOD.

END CLASS.
