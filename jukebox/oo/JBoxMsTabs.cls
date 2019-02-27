 
 /*------------------------------------------------------------------------
    File        : JBoxMsTabs
    Purpose     : Create tab-folder using MS .net TabFolder component
    Syntax      : 
    Description : 
    Author(s)   : Brynjar
    Created     : Thu Oct 29 2015
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.
USING Progress.Windows.UserControl.
USING Progress.Windows.Form.

CLASS JBoxMsTabs INHERITS UserControl:
  DEF VAR oForm              AS Progress.Windows.Form NO-UNDO.
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
  DEF VAR hWindow            AS HANDLE     NO-UNDO.
  DEF VAR iTab#              AS INT        NO-UNDO.
  DEF VAR hHelper            AS HANDLE     NO-UNDO.
    
  DEFINE PRIVATE VARIABLE components  AS System.ComponentModel.IContainer NO-UNDO.
  DEFINE PRIVATE VARIABLE imageList1  AS System.Windows.Forms.ImageList NO-UNDO.
  DEFINE PRIVATE VARIABLE tabControl1 AS System.Windows.Forms.TabControl NO-UNDO.
/*  DEFINE PRIVATE VARIABLE windowContainer AS Progress.Windows.WindowContainer NO-UNDO.*/

  DEF TEMP-TABLE ttProgram NO-UNDO 
      FIELD hProgram     AS HANDLE
      FIELD hPlaceholder AS HANDLE  
      FIELD iTab#        AS INT 
      FIELD cProgramName AS CHAR 
      FIELD cLabel       AS CHAR 
      FIELD cImage       AS CHAR 
      FIELD hWindow      AS HANDLE
      FIELD oWinCont     AS Progress.Lang.Object
      FIELD hPageQuery   AS HANDLE /* handle for browse or query on tab-page - 1st come 1st serve */
      INDEX idxTab# IS PRIMARY UNIQUE iTab#
      .
      
  DEF PUBLIC PROPERTY PARENT-BROWSE-OBJECT AS JBoxBrowse GET. SET.
  DEF PUBLIC PROPERTY PARENT-QUERY-OBJECT AS JBoxQuery GET. SET. 
  DEF PUBLIC PROPERTY PrevTabNum        AS INT  GET. PROTECTED SET.
  DEF PUBLIC PROPERTY CurrTabNum        AS INT  GET. SET(iiTab AS INT): PrevTabNum = CurrTabNum. CurrTabNum = iiTab. END SET.
  DEF PUBLIC PROPERTY CurrTabLabel      AS CHAR GET(): RETURN IF CurrTabNum > 0 THEN cTabLabels[CurrTabNum] ELSE "". END GET. PROTECTED SET.

  DEF PUBLIC PROPERTY PLACEHOLDER     AS HANDLE GET. PROTECTED SET. 
  DEF PUBLIC PROPERTY PublishForm     AS LOGICAL GET. SET. 
  DEF PUBLIC PROPERTY FormIsChildForm AS LOGICAL GET. SET.
      
  DEF PUBLIC PROPERTY pageOneType AS CHAR INIT "oneToOne" GET. 
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
  
  CONSTRUCTOR PUBLIC JBoxMsTabs (ihPlaceholder AS HANDLE):
      SUPER ().

    IF PROVERSION < "11" THEN hSourceProc = DYNAMIC-FUNCTION ("getProcedureHandle",PROGRAM-NAME(2)).
    ELSE hSourceProc = SOURCE-PROCEDURE.

    InitializeComponent(ihPlaceholder,?,"").                 
      
  END CONSTRUCTOR.

  CONSTRUCTOR PUBLIC JBoxMsTabs (ihPlaceholder AS HANDLE
                                 ,icImageList   AS CHAR):
      SUPER ().

    IF PROVERSION < "11" THEN hSourceProc = DYNAMIC-FUNCTION ("getProcedureHandle",PROGRAM-NAME(2)).
    ELSE hSourceProc = SOURCE-PROCEDURE.

    InitializeComponent(ihPlaceholder,?,icImageList).                 
      
  END CONSTRUCTOR.

  CONSTRUCTOR PUBLIC JBoxMsTabs (ihPlaceholder AS HANDLE
                                 ,ioBrowseObject AS JBoxBrowse):
      SUPER ().

    IF PROVERSION < "11" THEN hSourceProc = DYNAMIC-FUNCTION ("getProcedureHandle",PROGRAM-NAME(2)).
    ELSE hSourceProc = SOURCE-PROCEDURE.

    InitializeComponent(ihPlaceholder
                       ,IF VALID-OBJECT(ioBrowseObject) THEN ioBrowseObject:BROWSE-HANDLE ELSE ?
                       ,"").
                                        
    THIS-OBJECT:PARENT-BROWSE-OBJECT = ioBrowseObject.                       
      
  END CONSTRUCTOR.
  
  CONSTRUCTOR PUBLIC JBoxMsTabs (ihPlaceholder AS HANDLE
                                 ,ioQueryObject AS JBoxQuery):
      SUPER ().

    IF PROVERSION < "11" THEN hSourceProc = DYNAMIC-FUNCTION ("getProcedureHandle",PROGRAM-NAME(2)).
    ELSE hSourceProc = SOURCE-PROCEDURE.

    InitializeComponent(ihPlaceholder
                       ,IF VALID-OBJECT(ioQueryObject) THEN ioQueryObject:QUERY-HANDLE ELSE ?
                       ,"").
                                        
    THIS-OBJECT:PARENT-QUERY-OBJECT = ioQueryObject.                       
      
  END CONSTRUCTOR.

  CONSTRUCTOR PUBLIC JBoxMsTabs (ihPlaceholder  AS HANDLE
                                ,ioBrowseObject AS JBoxBrowse
                                ,icImageList    AS CHAR):
      SUPER ().

    IF PROVERSION < "11" THEN hSourceProc = DYNAMIC-FUNCTION ("getProcedureHandle",PROGRAM-NAME(2)).
    ELSE hSourceProc = SOURCE-PROCEDURE.

    InitializeComponent(ihPlaceholder
                       ,IF VALID-OBJECT(ioBrowseObject) THEN ioBrowseObject:BROWSE-HANDLE ELSE ?
                       ,icImageList).                 
                       
    THIS-OBJECT:PARENT-BROWSE-OBJECT = ioBrowseObject.                       
      
  END CONSTRUCTOR.

  CONSTRUCTOR PUBLIC JBoxMsTabs (ihPlaceholder  AS HANDLE
                                ,ioQueryObject AS JBoxQuery
                                ,icImageList    AS CHAR):
      SUPER ().

    IF PROVERSION < "11" THEN hSourceProc = DYNAMIC-FUNCTION ("getProcedureHandle",PROGRAM-NAME(2)).
    ELSE hSourceProc = SOURCE-PROCEDURE.

    InitializeComponent(ihPlaceholder
                       ,IF VALID-OBJECT(ioQueryObject) THEN ioQueryObject:QUERY-HANDLE ELSE ?
                       ,icImageList).                 
                       
    THIS-OBJECT:PARENT-QUERY-OBJECT = ioQueryObject.                       
      
  END CONSTRUCTOR.

  METHOD PRIVATE VOID initializeComponent(ihPlaceholder AS HANDLE
                                         ,ihQuery       AS HANDLE
                                         ,icImageList   AS CHAR
                                          ):
      
    DEF VAR hbUserControls AS HANDLE NO-UNDO.
    DEF VAR ix             AS INT NO-UNDO.
    DEF VAR cImgFile       AS CHAR NO-UNDO.
           
    PLACEHOLDER = ihPlaceholder.
    
    RUN JBoxMsTabsHelper.p PERSIST SET hHelper (THIS-OBJECT).
    
    THIS-OBJECT:components = NEW System.ComponentModel.Container().
    THIS-OBJECT:tabControl1 = NEW System.Windows.Forms.TabControl().
    
    IF icImageList NE "" THEN DO:
      THIS-OBJECT:imageList1 = NEW System.Windows.Forms.ImageList(THIS-OBJECT:components).
      icImageList = REPLACE(icImageList,",",";").
      DO ix = 1 TO NUM-ENTRIES(icImageList,";"):
        cImgFile = REPLACE(ENTRY(ix,icImageList,";"),"\","/").
        IF SEARCH(cImgFile) NE ? THEN DO:
          FILE-INFO:FILE-NAME = cImgFile.
          imageList1:Images:Add(System.Drawing.Image:FromFile(FILE-INFO:FULL-PATHNAME)).
          imageList1:Images:SetKeyName(ix - 1, ENTRY(NUM-ENTRIES(cImgFile,"/"),cImgFile,"/")).
        END.    
      END.  
    END.
    
    THIS-OBJECT:tabControl1:SuspendLayout().
    
    /*  */
    /* tabControl1 */
    /*  */
    THIS-OBJECT:tabControl1:ImageList = THIS-OBJECT:imageList1.
    THIS-OBJECT:tabControl1:Location = NEW System.Drawing.Point(0, 0).
    THIS-OBJECT:tabControl1:Name = "tabControl1".
    THIS-OBJECT:tabControl1:SelectedIndex = 0.
    THIS-OBJECT:tabControl1:Size = NEW System.Drawing.Size(ihPlaceholder:WIDTH-PIXELS,ihPlaceholder:HEIGHT-PIXELS). /* ihPlaceholder:HEIGHT-PIXELS */
    THIS-OBJECT:tabControl1:TabIndex = 0.
    THIS-OBJECT:tabControl1:SelectedIndexChanged:Subscribe(THIS-OBJECT:testClick).
    THIS-OBJECT:tabControl1:TabIndexChanged:Subscribe(THIS-OBJECT:testClick).
    
    
    /* X and Y limits for move of widget are not yet set for the window. 
       Since we want the tabs to resize according to the window size these values must be set here and
       they must be exact the same as in setOrwWinSize (see InitResize function) 
       Here the values are set to not give any automatic move of widgets */
       
    DYNAMIC-FUNCTION("setMinXYmove",2000,1200). 
  
    /* Alt 1: If the container has a navigation browse or query add it to the tabfolder call so it can be set in
       the suppressed windows (see the setParentQueryObject function in the template for suppresse window: */
    
/*    DYNAMIC-FUNCTION("InitPages" IN hTabFolder,*/
/*                     "",ihQuery).              */
    
/*    DYNAMIC-FUNCTION("setTabObject" IN hTabFolder,THIS-OBJECT).*/
    
/*    ASSIGN TAB-FOLDER-HANDLE = hTabFolder*/
/*           PLACEHOLDER = ihPlaceholder.  */
    /*  */
    /* MsTabForm */
    /*  */
/*    THIS-OBJECT:ClientSize = NEW System.Drawing.Size(ihPlaceholder:WIDTH-PIXELS,ihPlaceholder:HEIGHT-PIXELS).*/

    oForm = CAST(DYNAMIC-FUNCTION("getWindowForm" IN hSourceProc),"Progress.Windows.Form").
    
    FormIsChildForm = DYNAMIC-FUNCTION("getIsMDIchild" IN hSourceProc).

    oForm:Controls:Add(THIS-OBJECT).

/*    THIS-OBJECT:windowContainer = NEW Progress.Windows.WindowContainer().                                                        */
/*    THIS-OBJECT:windowContainer:Size = NEW System.Drawing.Size(ihPlaceholder:WIDTH-PIXELS - 10,ihPlaceholder:HEIGHT-PIXELS - 27).*/
/*    THIS-OBJECT:windowContainer:Location = NEW System.Drawing.Point(5, 25).                                                      */
/*    THIS-OBJECT:Controls:Add(THIS-OBJECT:windowContainer).                                                                       */
/*    windowContainer:EmbeddedWindow = hWindow.*/

/*    IF ibView THEN*/
    THIS-OBJECT:BringToFront().
/*    ELSE */ 
/*     THIS-OBJECT:SendToBack().*/

    DYNAMIC-FUNCTION("NewObject",hSourceProc:CURRENT-WINDOW,ihPlaceHolder,STRING(THIS-OBJECT:GetType())).

    hbUserControls = DYNAMIC-FUNCTION("getUserControlsBuffer" IN hSourceProc).

    hbUserControls:BUFFER-CREATE().
    ASSIGN hbUserControls::oUserControl     = CAST(THIS-OBJECT,"Progress.Lang.Object")
           hbUserControls::cUserControlType = STRING(THIS-OBJECT:GetType())
           hbUserControls::oWidget          = CAST(THIS-OBJECT:tabControl1,"Progress.Lang.Object")
           hbUserControls::cWidgetType      = THIS-OBJECT:tabControl1:ToString()
           hbUserControls::hPlaceHolder     = ihPlaceHolder
           hbUserControls::hTabFolder       = DYNAMIC-FUNCTION("getCurrTabFolder" IN hSourceProc)
           hbUserControls::iTabPage         = DYNAMIC-FUNCTION("getCurrTabPage" IN hSourceProc)
           hbUserControls::hSourceProc      = hSourceProc
           hbUserControls::cMethodSuffix    = "TabControl"
/*           hbUserControls::iDeltaWidth      = iDeltaWidth*/
           .

    THIS-OBJECT:Controls:Add(THIS-OBJECT:tabControl1).
    THIS-OBJECT:Name = "MsTabForm".
    THIS-OBJECT:Text = "MsTabForm".
    THIS-OBJECT:tabControl1:ResumeLayout(FALSE).
    THIS-OBJECT:ResumeLayout(FALSE).
    CATCH e AS Progress.Lang.Error:
      UNDO, THROW e.
    END CATCH.

  END METHOD.                                       

  METHOD PRIVATE VOID testClick( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):

    THIS-OBJECT:TabFolderChanged(tabControl1:SelectedIndex + 1).

  END METHOD.

  METHOD PUBLIC VOID AddPage (icLabel          AS CHAR
                             ,icLaunch         AS CHAR
                             ,icImage          AS CHAR
                             ,ibUsePlaceholder AS LOG): /* For support of "suppressed windows" */
                               
    DEF VAR iCurrTab        AS INT    NO-UNDO.
    DEF VAR hProc           AS HANDLE NO-UNDO.
    DEF VAR hFrame          AS HANDLE NO-UNDO.
    DEF VAR cPPathCheck     AS CHAR   NO-UNDO.
    DEF VAR ABLwinCont      AS Progress.Windows.WindowContainer NO-UNDO.
    DEF VAR tabPage1        AS System.Windows.Forms.TabPage NO-UNDO.
    DEF VAR iDeltaX         AS INT NO-UNDO.
    DEF VAR iDeltaY         AS INT NO-UNDO. 
    DEF VAR rttProgram      AS ROWID NO-UNDO.

    IF icImage NE "" THEN
      ASSIGN icImage = REPLACE(icImage,"\","/")
             icImage = ENTRY(NUM-ENTRIES(icImage,"/"),icImage,"/").

    tabPage1 = NEW System.Windows.Forms.TabPage().
    tabPage1:Size = NEW System.Drawing.Size(PLACEHOLDER:WIDTH-PIXELS,PLACEHOLDER:HEIGHT-PIXELS - 20).
    THIS-OBJECT:tabControl1:Controls:Add(tabPage1).
    tabPage1:Name = icLabel.
    tabPage1:Padding = NEW System.Windows.Forms.Padding(3).
    tabPage1:Text = icLabel.
    tabPage1:UseVisualStyleBackColor = TRUE.
    IF icImage NE "" THEN
      tabPage1:ImageKey = icImage.
      
    ABLwinCont = NEW Progress.Windows.WindowContainer().
    ABLwinCont:Size = NEW System.Drawing.Size(tabPage1:Width,tabPage1:Height).
/*    ABLwinCont:Location = NEW System.Drawing.Point(5, 25).*/
    tabPage1:Controls:Add(ABLwinCont).

    CREATE ttProgram.
    ASSIGN iTab# = iTab# + 1
           ttProgram.iTab# = iTab#
           ttProgram.oWinCont = CAST(ABLwinCont,"Progress.Lang.Object")
           rttProgram = ROWID(ttProgram).
        
    IF SEARCH(icLaunch) = ? AND SEARCH(SUBSTR(icLaunch,1,LENGTH(icLaunch) - 1) + "r") = ? THEN DO:
      cPPathCheck = REPLACE(icLaunch,"/","\").
      cPPathCheck = ENTRY(NUM-ENTRIES(cPPathCheck,"\"),cPPathCheck,"\").
      IF SEARCH(cPPathCheck) NE ? OR SEARCH(SUBSTR(cPPathCheck,1,LENGTH(cPPathCheck) - 1) + "r") NE ? THEN
        icLaunch = cPPathCheck.
    END.
    
    ASSIGN ttProgram.cLabel       = icLabel
           ttProgram.cProgramName = icLaunch
           ttProgram.cImage       = icImage
           .
    
    IF SEARCH(ENTRY(1,ttProgram.cProgramName,".") + ".r") NE ? OR SEARCH(ENTRY(1,ttProgram.cProgramName,".") + ".w") NE ? THEN DO:
      IF ibUsePlaceholder THEN
        RUN JBoxTabPlaceholder.w PERSIST SET ttProgram.hPlaceholder (ttProgram.cProgramName,PLACEHOLDER,OUTPUT ttProgram.hProgram) NO-ERROR. 
      ELSE
        RUN VALUE(ttProgram.cProgramName) PERSIST SET ttProgram.hProgram NO-ERROR.
      IF ERROR-STATUS:ERROR THEN DO:
        MESSAGE "Failed to start: " ttProgram.cProgramName SKIP 
                 ERROR-STATUS:GET-MESSAGE(1)
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        RETURN.
      END.
    
      hProc = IF VALID-HANDLE(ttProgram.hPlaceholder) THEN ttProgram.hPlaceholder ELSE ttProgram.hProgram.

/*      IF VALID-HANDLE(ttProgram.hPlaceholder) THEN ttProgram.hProgram = ttProgram.hPlaceholder. /* vekk */*/

      SUBSCRIBE PROCEDURE hSourceProc TO "InvalidateHandle" IN ttProgram.hProgram NO-ERROR.

      DYNAMIC-FUNCTION("CreateParentLink",ttProgram.hProgram,hSourceProc,"page" + STRING(ttProgram.iTab#)) NO-ERROR.
    
      PUBLISH "CurrentTabFolderPage" FROM hSourceProc (hHelper,ttProgram.iTab#). /* target: JBoxWrapWindowInForm.p to assign user-control to tab-page  */
    
      IF PublishForm THEN DO:
        IF FormIsChildForm THEN
          PUBLISH "SetChildWrapperForm" FROM hSourceProc(hProc,oForm).
        ELSE
          PUBLISH "SetWrapperForm" FROM hSourceProc(hProc,oForm).
      END.          
    
      IF CAN-DO(ttProgram.hProgram:INTERNAL-ENTRIES,"setParent") THEN
        DYNAMIC-FUNCTION("setParent" IN ttProgram.hProgram,hSourceProc) NO-ERROR.

      IF CAN-DO(ttProgram.hProgram:INTERNAL-ENTRIES,"InitializeResize") THEN
        DYNAMIC-FUNCTION("InitializeResize" IN ttProgram.hProgram) NO-ERROR.
        
      IF CAN-DO(ttProgram.hProgram:INTERNAL-ENTRIES,"getFrameHandle") THEN DO:
        ttProgram.hWindow = DYNAMIC-FUNCTION("getFrameHandle" IN ttProgram.hProgram) NO-ERROR.
        DYNAMIC-FUNCTION("setAttribute",hSourceProc,"pageframe" + STRING(ttProgram.iTab#),STRING(ttProgram.hWindow)) NO-ERROR.
      END.
      
      ASSIGN ttProgram.hWindow = ttProgram.hProgram:CURRENT-WINDOW
             iDeltaX = PLACEHOLDER:WIDTH-PIXELS - ttProgram.hWindow:WIDTH-PIXELS - 10
             iDeltaY = PLACEHOLDER:HEIGHT-PIXELS - ttProgram.hWindow:HEIGHT-PIXELS - 25
             ABLwinCont:EmbeddedWindow = ttProgram.hProgram:CURRENT-WINDOW
             .
      DYNAMIC-FUNCTION("copyAttributes",hSourceProc:CURRENT-WINDOW,ttProgram.hProgram:CURRENT-WINDOW,"*").
          
/*      IF VALID-HANDLE(hParentQuery) THEN DO:                                                       */
/*        DYNAMIC-FUNCTION("setQuery" IN ttProgram.hProgram,hParentQuery) NO-ERROR.                  */
/*        IF ERROR-STATUS:ERROR THEN                                                                 */
/*          MESSAGE "Function 'setQuery' missing in tab-folder procedure for tab # " + STRING(iiTab#)*/
/*                  VIEW-AS ALERT-BOX ERROR.                                                         */
/*      END.                                                                                         */
    
      RUN InitializeObject IN ttProgram.hProgram NO-ERROR.
      IF ERROR-STATUS:ERROR THEN 
        MESSAGE "Procedure 'InitializeObject' missing in tab-folder procedure for tab # " + STRING(ttProgram.iTab#)
                VIEW-AS ALERT-BOX ERROR.

      IF NOT AVAIL ttProgram THEN FIND ttProgram WHERE ROWID(ttProgram) = rttProgram.

      DYNAMIC-FUNCTION("setDeltaX",iDeltaX).
      DYNAMIC-FUNCTION("setDeltaY",iDeltaY).
      DYNAMIC-FUNCTION("DoWidgetResize",ttProgram.hWindow:FIRST-CHILD).
        
      DYNAMIC-FUNCTION("NewObject",hSourceProc:CURRENT-WINDOW,ttProgram.hProgram,"procedure").
      DYNAMIC-FUNCTION("CreateObjectLink",ttProgram.hProgram,hSourceProc).

      PUBLISH "CurrentTabFolderPage" FROM hSourceProc (?,0). /* target: JBoxWrapWindowInForm.p to reset tab-page ref (controls on page should be created by now)  */

      IF iTab# = 1 THEN
        THIS-OBJECT:TabFolderChanged(1).
      ELSE  
        RUN ViewHideTabUserControl IN (IF VALID-HANDLE(ttProgram.hPlaceholder) THEN ttProgram.hPlaceholder ELSE ttProgram.hProgram)
            (hHelper,-1) NO-ERROR.      
        
    END.
    ELSE 
      MESSAGE PROGRAM-NAME(1) SKIP
             "Couldn't find tab program: " icLaunch 
             VIEW-AS ALERT-BOX.

  END METHOD.

  METHOD PUBLIC VOID AddPage (icLabel     AS CHAR
                             ,icLaunch    AS CHAR
                             ,icImage     AS CHAR):
                               
    THIS-OBJECT:AddPage(icLabel,icLaunch,icImage,NO).
    
  END METHOD.
  
  METHOD PUBLIC VOID AddPage (icLabel   AS CHAR
                             ,icLaunch  AS CHAR):

    THIS-OBJECT:AddPage(icLabel,icLaunch,"",NO).

  END METHOD.

  METHOD PUBLIC LOGICAL setPageQueryIfVoid (ihPageQuery AS HANDLE):
    IF AVAIL ttProgram AND ttProgram.hPageQuery = ? THEN DO:
      ttProgram.hPageQuery = ihPageQuery.
      RETURN YES.
    END.
    ELSE RETURN NO.
  END.   

  METHOD PUBLIC HANDLE getPageQuery (iiTab AS INT):
    DEF VAR hPageQuery AS HANDLE NO-UNDO.
    
    IF iiTab = 0 THEN RETURN ?.
    
    IF VALID-OBJECT(oChildBrowse[iiTab]) THEN
      hPageQuery = oChildBrowse[iiTab]:BROWSE-HANDLE.
    ELSE IF VALID-OBJECT(oChildQuery[iiTab]) THEN
      hPageQuery = oChildQuery[iiTab]:QUERY-HANDLE.
    ELSE DO:
      FIND FIRST ttProgram 
           WHERE ttProgram.iTab# = iiTab 
           NO-ERROR.
      IF AVAIL ttProgram THEN            
        hPageQuery = ttProgram.hPageQuery.
    END.     
    
    RETURN hPageQuery. 
  END METHOD.

  METHOD PUBLIC HANDLE getPageHandle (iiTab AS INT):
    /* Return the procedure handle for page */
    FIND FIRST ttProgram 
         WHERE ttProgram.iTab# = iiTab
         NO-ERROR.
    IF AVAIL ttProgram THEN
      RETURN ttProgram.hProgram.
    ELSE       
      RETURN ?.
  END METHOD.

  METHOD PUBLIC INTEGER getPageTabNum (ihPageProc AS HANDLE):
    /* Return the tab num for a page procedure handle */
    FIND FIRST ttProgram 
         WHERE ttProgram.hProgram = ihPageProc
         NO-ERROR.
    IF AVAIL ttProgram THEN
      RETURN ttProgram.iTab#.
    ELSE       
      RETURN 0.
      
  END METHOD.

  METHOD PUBLIC VOID selectPage (iiTab AS INT):
    /* Invokes also the TabChanged function (input iiTab as int) if it exists */
    THIS-OBJECT:TabFolderChanged(iiTab).

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
      /*
      IF bParentIsBrowse THEN DO:
        IF VALID-OBJECT(oChildBrowse[iiTab]) AND VALID-OBJECT(oChildBrowse[iiTab]:TAB-FOLDER-OBJECT) THEN
          oChildBrowse[iiTab]:TAB-FOLDER-OBJECT:PARENT-TAB-FOLDER-OBJECT = THIS-OBJECT.
      END.  
      ELSE DO:
        IF VALID-OBJECT(oChildQuery[iiTab]) AND VALID-OBJECT(oChildQuery[iiTab]:TAB-FOLDER-OBJECT) THEN
          oChildQuery[iiTab]:TAB-FOLDER-OBJECT:PARENT-TAB-FOLDER-OBJECT = THIS-OBJECT.
      END.
      */  
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
            ELSE IF THIS-OBJECT:getPageQuery(iiTab) NE ? THEN
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
        ELSE IF THIS-OBJECT:getPageQuery(iiTab) NE ? THEN 
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

/*    hTabFrame = DYNAMIC-FUNCTION("getTabFrame" IN hTabFolder).*/
/*    hTabFrame:MOVE-TO-TOP ().                                 */

  END METHOD.
  
  METHOD PUBLIC VOID resizeTabWindows (iiX      AS INT
                                      ,iiY      AS INT
                                      ,iiDeltaX AS INT
                                      ,iiDeltaY AS INT):
                                        
    DEF VAR ABLwinCont      AS Progress.Windows.WindowContainer NO-UNDO.
            
    FOR EACH ttProgram:
      RUN SetWinContainerXY IN ttProgram.hProgram (iiX + 5,iiY + 23) NO-ERROR.
      
      ASSIGN ABLwinCont = CAST(ttProgram.oWinCont,"Progress.Windows.WindowContainer")
             ABLwinCont:Size = NEW System.Drawing.Size(ABLwinCont:Width + iiDeltaX,ABLwinCont:Height + iiDeltaY)
             ttProgram.hWindow:WIDTH-PIXELS = ttProgram.hWindow:WIDTH-PIXELS + iiDeltaX             
             ttProgram.hWindow:HEIGHT-PIXELS = ttProgram.hWindow:HEIGHT-PIXELS + iiDeltaY
             .
      RUN ResizeFormComponents IN ttProgram.hProgram (ttProgram.hWindow) NO-ERROR.       
    END.
 
  END METHOD.                  
  
  METHOD PUBLIC LOGICAL TabFolderChanged ( INPUT iiTab AS INT ) :
    /*------------------------------------------------------------------------------
      Purpose:  
        Notes:  
    ------------------------------------------------------------------------------*/
    DEF VAR hFieldMap  AS HANDLE NO-UNDO.
    DEF VAR hToolbar   AS HANDLE NO-UNDO.
    DEF VAR cCurrState AS CHAR   NO-UNDO.
    DEF VAR iReturn    AS INT    NO-UNDO.
    
    
/*    DYNAMIC-FUNCTION("SaveModified",getPageQuery(CurrTabNum)) NO-ERROR.*/
    
    IF NOT DYNAMIC-FUNCTION("SaveModified",getPageQuery(CurrTabNum)) 
       AND CAN-DO(hSourceProc:INTERNAL-ENTRIES,"UndoTabChanges") THEN
      DYNAMIC-FUNCTION("UndoTabChanges" IN hSourceProc,CurrTabNum).
    
    IF iiTab = 0 THEN iiTab = CurrTabNum.
    
    CurrTabNum = iiTab.
            
    FIND FIRST ttProgram
         WHERE ttProgram.iTab# = iiTab NO-ERROR.
    IF AVAIL ttProgram THEN DO:
      IF VALID-HANDLE(ttProgram.hWindow) THEN
        ttProgram.hWindow:MOVE-TO-TOP().    
    END.
    ELSE RETURN FALSE.
        
    PUBLISH "ViewHideTabUserControl" FROM hSourceProc (hHelper,CurrTabNum). /* target: JBoxWrapWindowInForm.p to view/hide user-controls on tab-pages  */
    
    IF CAN-DO(hSourceProc:INTERNAL-ENTRIES,"TabChanged") THEN 
      DYNAMIC-FUNCTION("TabChanged" IN hSourceProc,CurrTabNum).
    ELSE  
      THIS-OBJECT:LinkChildObject(CurrTabNum).  
    
    IF VALID-HANDLE(ttProgram.hPlaceholder) THEN
      RUN MoveToTop IN ttProgram.hPlaceholder NO-ERROR.
    ELSE  
      RUN MoveToTop IN ttProgram.hProgram NO-ERROR.
    
    RETURN TRUE.
  
  END METHOD.
                            

  DESTRUCTOR PUBLIC JBoxMsTabs ( ):
    DELETE PROCEDURE hHelper NO-ERROR.
    FOR EACH ttProgram:
      IF VALID-HANDLE(ttProgram.hPlaceholder) THEN
        APPLY "close" TO ttProgram.hPlaceholder.
      ELSE IF VALID-HANDLE(ttProgram.hProgram) THEN
        APPLY "close" TO ttProgram.hProgram.
    END.
  END DESTRUCTOR.

END CLASS.
