 
/*------------------------------------------------------------------------
   File        : JBoxToolbar
   Purpose     : 
   Syntax      : 
   Description : JukeBox Toolbar object
   Author(s)   : brynjar
   Created     : Wed Oct 02 20:57:07 CEST 2013
   Notes       : 
 ----------------------------------------------------------------------*/

USING Progress.Lang.*.


CLASS JBoxToolbar: 
  /*------------------------------------------------------------------------------
   Purpose:
   Notes:
  ------------------------------------------------------------------------------*/
  DEFINE VARIABLE hSourceProc      AS HANDLE       NO-UNDO.
  DEFINE VARIABLE cOptions         AS CHARACTER    NO-UNDO INIT "maxborder".
  DEFINE VARIABLE cParentMenuLabel AS CHARACTER    NO-UNDO.
  DEFINE VARIABLE bOk              AS LOG          NO-UNDO.
  DEFINE VARIABLE ix               AS INTEGER      NO-UNDO.
  DEFINE VARIABLE oBrowse          AS JBoxBrowse   NO-UNDO.
  DEFINE VARIABLE oQuery           AS JBoxQuery    NO-UNDO.
  DEFINE VARIABLE oFieldMap        AS JBoxFieldMap NO-UNDO.
  
  DEFINE TEMP-TABLE ttToolDef 
    FIELD cTool    AS CHARACTER
    FIELD cToolDef AS CHARACTER
    FIELD cLabel   AS CHARACTER
    FIELD iSeq     AS INTEGER   
    FIELD hWidget  AS HANDLE 
    . 
		
  DEFINE PUBLIC PROPERTY designWidget     AS HANDLE GET. SET.
  DEFINE PUBLIC PROPERTY TOOLBAR-HANDLE   AS HANDLE GET. 
    PROTECTED SET.
  DEFINE PUBLIC PROPERTY BROWSE-HANDLE    AS HANDLE 
    GET(): 
      RETURN DYNAMIC-FUNCTION ("getLinkedObject",TOOLBAR-HANDLE,"browse","from"). 
    END GET. 
    PROTECTED SET.
  DEFINE PUBLIC PROPERTY QUERY-HANDLE    AS HANDLE 
    GET(): 
      RETURN DYNAMIC-FUNCTION ("getLinkedObject",TOOLBAR-HANDLE,"query","from"). 
    END GET. 
    PROTECTED SET.
  DEFINE PUBLIC PROPERTY BUFFER-HANDLE    AS HANDLE 
    GET(): 
      RETURN DYNAMIC-FUNCTION ("getLinkedObject",TOOLBAR-HANDLE,"fieldMap","from"). 
    END GET. 
    PROTECTED SET.
  
  DEFINE PUBLIC PROPERTY BROWSE-OBJECT AS JBoxBrowse GET. 
    SET(ioBrowse AS JBoxBrowse):
      IF NOT VALID-OBJECT(ioBrowse) THEN 
      DO:
        IF VALID-OBJECT(oBrowse) THEN
          DYNAMIC-FUNCTION ("deleteObjectLink",TOOLBAR-HANDLE,BROWSE-HANDLE).
      END.
      ELSE DYNAMIC-FUNCTION("createObjectLink",TOOLBAR-HANDLE,ioBrowse:BROWSE-HANDLE).
      ASSIGN 
        oBrowse       = ioBrowse
        BROWSE-HANDLE = ioBrowse:BROWSE-HANDLE.
    END SET.
  
  DEFINE PUBLIC PROPERTY QUERY-OBJECT AS JBoxQuery GET. 
    SET(ioQuery AS JBoxQuery):
      IF NOT VALID-OBJECT(ioQuery) THEN 
      DO:
        IF VALID-OBJECT(oQuery) THEN
          DYNAMIC-FUNCTION ("deleteObjectLink",TOOLBAR-HANDLE,QUERY-HANDLE).
      END.
      ELSE DYNAMIC-FUNCTION("createObjectLink",TOOLBAR-HANDLE,ioQuery:QUERY-HANDLE).
      ASSIGN 
        oQuery       = ioQuery
        QUERY-HANDLE = ioQuery:QUERY-HANDLE.
    END SET.

  DEFINE PUBLIC PROPERTY FIELDMAP-OBJECT AS JBoxFieldMap GET. 
    SET(ioFieldMap AS JBoxFieldMap):
      IF NOT VALID-OBJECT(ioFieldMap) THEN 
      DO:
        IF VALID-OBJECT(oFieldMap) THEN
          DYNAMIC-FUNCTION ("deleteObjectLink",TOOLBAR-HANDLE,BUFFER-HANDLE).
      END.
      ELSE DYNAMIC-FUNCTION("createObjectLink",TOOLBAR-HANDLE,ioFieldMap:BUFFER-HANDLE).
      ASSIGN 
        oFieldMap     = ioFieldMap
        BUFFER-HANDLE = ioFieldMap:BUFFER-HANDLE
        .
    END SET.
  			
  DEFINE PUBLIC PROPERTY objectState AS CHARACTER 
    GET(): 
      RETURN DYNAMIC-FUNCTION("getObjectState",TOOLBAR-HANDLE). 
    END GET. 
    SET(icState AS CHARACTER): 
      DYNAMIC-FUNCTION("setToolbar",TOOLBAR-HANDLE,icState).  
    END SET. 
      
  DEFINE PUBLIC PROPERTY buttonAndRuleWidgetList AS CHARACTER
    GET():
      RETURN DYNAMIC-FUNCTION("getToolBarHandles",TOOLBAR-HANDLE,"button,rule").
    END GET.
    PROTECTED SET.
        
  DEFINE PUBLIC PROPERTY buttonNames AS CHARACTER
    GET():
      RETURN DYNAMIC-FUNCTION("getToolBarNames",TOOLBAR-HANDLE,"").
    END GET.
    PROTECTED SET.

  DEFINE PUBLIC PROPERTY disabledTools AS CHARACTER
    GET():
      RETURN DYNAMIC-FUNCTION("getAttribute",TOOLBAR-HANDLE,"disabledEvents").
    END GET.
    SET(icSetting AS CHARACTER): 
      DYNAMIC-FUNCTION("setAttribute",TOOLBAR-HANDLE,"disabledEvents",icSetting).
    END SET.
        
  DEFINE PUBLIC PROPERTY enabledTools AS CHARACTER
    GET():
      RETURN DYNAMIC-FUNCTION("getAttribute",TOOLBAR-HANDLE,"enabledEvents").
    END GET.
    SET(icSetting AS CHARACTER): 
      DYNAMIC-FUNCTION("setAttribute",TOOLBAR-HANDLE,"enabledEvents",icSetting).
    END SET.  
  				
  DEFINE PUBLIC PROPERTY IsCurrent  AS LOG 
    GET(): 
      RETURN THIS-OBJECT:TOOLBAR-HANDLE = DYNAMIC-FUNCTION ("getCurrentObject"). 
    END GET. 
    PROTECTED SET.  				

  DEFINE PUBLIC PROPERTY activeNoteButton AS CHARACTER
    GET():    
      RETURN DYNAMIC-FUNCTION("getAttribute",TOOLBAR-HANDLE,"activeNoteButton").
    END.
    SET(icSetting AS CHARACTER):
      DYNAMIC-FUNCTION("setAttribute",TOOLBAR-HANDLE,"activeNoteButton",icSetting).
    END.   

  DEFINE PUBLIC PROPERTY passiveNoteButton AS CHARACTER
    GET():    
      RETURN DYNAMIC-FUNCTION("getAttribute",TOOLBAR-HANDLE,"passiveNoteButton").
    END.
    SET(icSetting AS CHARACTER):
      DYNAMIC-FUNCTION("setAttribute",TOOLBAR-HANDLE,"passiveNoteButton",icSetting).
    END.   
      
  DEFINE PUBLIC PROPERTY noteCurrentValue AS CHARACTER
    GET():    
      RETURN DYNAMIC-FUNCTION("getAttribute",TOOLBAR-HANDLE,"noteCurrentValue").
    END.
    SET(icSetting AS CHARACTER):
      DYNAMIC-FUNCTION("setAttribute",TOOLBAR-HANDLE,"noteCurrentValue",icSetting).
    END.   
       
  DEFINE PUBLIC PROPERTY noteCurrentValueChanged AS LOG
    GET():    
      RETURN DYNAMIC-FUNCTION("getAttribute",TOOLBAR-HANDLE,"noteCurrentValueChanged") = "yes".
    END.
    PROTECTED SET.
      
  DEFINE PUBLIC PROPERTY noteDialogBox AS LOG
    GET():    
      RETURN DYNAMIC-FUNCTION("getAttribute",TOOLBAR-HANDLE,"noteDialogBox") = "yes".
    END.
    SET(ibSetting AS LOG):
      DYNAMIC-FUNCTION("setAttribute",TOOLBAR-HANDLE,"noteDialogBox",IF ibSetting = YES THEN "yes" ELSE "").
    END.   

  DEFINE PUBLIC PROPERTY noteReadOnly AS LOG
    GET():    
      RETURN DYNAMIC-FUNCTION("getAttribute",TOOLBAR-HANDLE,"noteReadOnly") = "yes".
    END.
    SET(ibSetting AS LOG):
      DYNAMIC-FUNCTION("setAttribute",TOOLBAR-HANDLE,"noteReadOnly",IF ibSetting = YES THEN "yes" ELSE "").
    END.   

  DEFINE PUBLIC PROPERTY noteAddTimeStamp AS LOG
    GET():    
      RETURN DYNAMIC-FUNCTION("getAttribute",TOOLBAR-HANDLE,"NoteAddTimeStamp") = "yes".
    END.
    SET(ibSetting AS LOG):
      DYNAMIC-FUNCTION("setAttribute",TOOLBAR-HANDLE,"NoteAddTimeStamp",IF ibSetting = YES THEN "yes" ELSE "").
    END.   

  DEFINE PUBLIC PROPERTY noteSplitEditor AS LOG
    GET():    
      RETURN DYNAMIC-FUNCTION("getAttribute",TOOLBAR-HANDLE,"NoteSplitEditor") = "yes".
    END.
    SET(ibSetting AS LOG):
      DYNAMIC-FUNCTION("setAttribute",TOOLBAR-HANDLE,"NoteSplitEditor",IF ibSetting = YES THEN "yes" ELSE "").
    END.   
  /*       cEditorDirection = getAttribute(hCurrObject,"NoteEditDirection") /* up / down (default) */*/
                    
  DEFINE PUBLIC PROPERTY docAvailable AS LOG
    GET():    
      RETURN DYNAMIC-FUNCTION("getAttribute",TOOLBAR-HANDLE,"docCurrentValue") = YES.
    END.
    SET(ibSetting AS LOG):
      DYNAMIC-FUNCTION("setAttribute",TOOLBAR-HANDLE,"docCurrentValue",STRING(ibSetting)).
    END.   

  DEFINE PUBLIC PROPERTY lastToolSelected AS CHAR
    GET():    
      RETURN DYNAMIC-FUNCTION("getAttribute",TOOLBAR-HANDLE,"lasttoolbaraction").
    END.
    PROTECTED SET.   
  				
  CONSTRUCTOR PUBLIC JBoxToolbar (ihDesignWidget AS HANDLE):
    SUPER ().

    IF PROVERSION < "11" THEN hSourceProc = DYNAMIC-FUNCTION ("getProcedureHandle",PROGRAM-NAME(2)).
    ELSE hSourceProc = SOURCE-PROCEDURE.

    InitializeComponent(ihDesignWidget,"").            
  END CONSTRUCTOR.


  CONSTRUCTOR PUBLIC JBoxToolbar (ihDesignWidget AS HANDLE
    ,icParentMenuLabel AS CHARACTER):
    SUPER ().
    
    IF PROVERSION < "11" THEN hSourceProc = DYNAMIC-FUNCTION ("getProcedureHandle",PROGRAM-NAME(2)).
    ELSE hSourceProc = SOURCE-PROCEDURE.
    
    InitializeComponent(ihDesignWidget,icParentMenuLabel).            
  END CONSTRUCTOR.

  CONSTRUCTOR PUBLIC JBoxToolbar (ihDesignWidget AS HANDLE
                                 ,ihSourceProc   AS HANDLE):
    SUPER ().
    
    hSourceProc = ihSourceProc.
    
    InitializeComponent(ihDesignWidget,"").            
  END CONSTRUCTOR.

  METHOD PRIVATE VOID InitializeComponent(ihDesignWidget AS HANDLE
    ,icParentMenuLabel AS CHARACTER):
    DEFINE VARIABLE cStaticBtns  AS CHARACTER NO-UNDO.    
    DEFINE VARIABLE hButton      AS HANDLE    NO-UNDO.
    DEFINE VARIABLE bPrivateData AS LOG       NO-UNDO. 
    DEFINE VARIABLE cToolList    AS CHARACTER NO-UNDO. 
                                          
    IF VALID-HANDLE(ihDesignWidget) THEN
      designWidget = ihDesignWidget.


    IF designWidget:PRIVATE-DATA NE "" AND designWidget:PRIVATE-DATA NE ? THEN 
    DO:
      bPrivateData = YES.
      IF icParentMenuLabel = "" THEN icParentMenuLabel = ENTRY(2,designWidget:PRIVATE-DATA,CHR(1)).
      cToolList = ENTRY(1,designWidget:PRIVATE-DATA,CHR(1)).
      IF NUM-ENTRIES(designWidget:PRIVATE-DATA,CHR(1)) > 2 THEN cOptions = ENTRY(3,designWidget:PRIVATE-DATA,CHR(1)).
      DO ix = 1 TO NUM-ENTRIES(cToolList):
        CREATE ttToolDef.
        ASSIGN 
          ttToolDef.cToolDef = ENTRY(ix,cToolList)
          ttToolDef.cTool    = ENTRY(1,ENTRY(1,ttToolDef.cToolDef,";"),"|")
          ttToolDef.iSeq     = ix
          .
      END.  
    END.  

    hButton = designWidget:NEXT-SIBLING.
    ix = 0.
    REPEAT WHILE VALID-HANDLE(hButton):
      IF hButton:TYPE = "button" AND hButton:NAME MATCHES "*" + designWidget:NAME THEN 
      DO:
        FIND FIRST ttToolDef
          WHERE ttToolDef.cTool = REPLACE(hButton:NAME,"_" + designWidget:NAME,"")
          NO-ERROR.
        IF AVAILABLE ttToolDef THEN ttToolDef.hWidget = hButton.
        ELSE 
        DO: /* To fix toolbars that misses private data */
          CREATE ttToolDef.
          ASSIGN 
            ttToolDef.cTool   = REPLACE(hButton:NAME,"_" + designWidget:NAME,"")
            ttToolDef.cLabel  = hButton:LABEL
            ttToolDef.hWidget = hButton
            ix                = ix + 1
            ttToolDef.iSeq    = ix
            .
        END.         
      END.              
      hButton = hButton:NEXT-SIBLING.
    END.
    
    FOR EACH ttToolDef BY iSeq:
      cStaticBtns = cStaticBtns + (IF cStaticBtns NE "" THEN "," ELSE "") 
        + (IF bPrivateData THEN
        ttToolDef.cToolDef
        + (IF VALID-HANDLE(ttToolDef.hWidget) AND NUM-ENTRIES(ttToolDef.cTool,"¤") = 1 THEN "¤" ELSE "")  
        ELSE ttToolDef.cTool + ";" + ttToolDef.cLabel + "¤")
        + (IF VALID-HANDLE(ttToolDef.hWidget) THEN
        "¤" + STRING(ttToolDef.hWidget) ELSE "")
        .
    END.
    
    IF cStaticBtns NE "" THEN                               
      DYNAMIC-FUNCTION("setAttribute",designWidget,"rectangleSet","yes").                           
                            
    DYNAMIC-FUNCTION("setObjectSourceProc",hSourceProc).  
    TOOLBAR-HANDLE = DYNAMIC-FUNCTION("NewToolBar"
      ,designWidget
      ,icParentMenuLabel
      ,""
      ,"").
    cParentMenuLabel = icParentMenuLabel.
    
    IF cStaticBtns NE "" THEN 
    DO:
      DYNAMIC-FUNCTION("setObjectSourceProc",hSourceProc).  
      DYNAMIC-FUNCTION("AppendToolbar",TOOLBAR-HANDLE
        ,designWidget
        ,IF icParentMenuLabel NE "" THEN icParentMenuLabel ELSE cParentMenuLabel
        ,cStaticBtns
        ,cOptions
        ).                             
    END.
    
    PUBLISH "setChildToolbarObject" FROM hSourceProc (hSourceProc,THIS-OBJECT).   /* targets: tabfolder and viewer */                           
     
  END METHOD.
  
  METHOD PUBLIC VOID addTool (icTool            AS CHARACTER
    ,icLabel           AS CHARACTER
    ,icImage           AS CHARACTER
    ,icParentMenuLabel AS CHARACTER
    ,ibAlwaysEnabled   AS LOG
    ,icAddAs           AS CHARACTER /* "button": button only. "menu": menu-item only. "": both */
    ,icShortCut        AS CHARACTER
    ,icTooltip         AS CHARACTER
    ,icMethod          AS CHARACTER
    ).
    
    DEFINE VARIABLE cToolDef AS CHARACTER NO-UNDO.
    
    IF icTool = "" THEN 
    DO:
      MESSAGE "Invalid definition: No Tool (action) is defined"
        VIEW-AS ALERT-BOX.
      RETURN.        
    END.
    
    IF icParentMenuLabel NE "" THEN 
    DO:
      IF cParentMenuLabel = "" THEN 
      DO:
        MESSAGE "Invalid reference to sub-menu. There is no root menu. Tool: " icTool
          VIEW-AS ALERT-BOX.
        RETURN.
      END.  
      IF DYNAMIC-FUNCTION("getAttribute",TOOLBAR-HANDLE,"sub-menu" + icParentMenuLabel) = "" THEN 
      DO:
        MESSAGE "Invalid reference to parent menu: " icParentMenuLabel " for tool: " icTool
          VIEW-AS ALERT-BOX.
        RETURN.
      END.     
    END.
                                        
    IF icAddAs = "button" THEN icTool = icTool + "|".
    cToolDef = icTool + ";" + icLabel + icShortCut + ";" + icTooltip + ";" + icMethod + ";" + icImage.
    IF ibAlwaysEnabled   THEN cToolDef = cToolDef + "¤enable".
    IF icAddAs BEGINS "menu" OR designWidget:TYPE = "window" THEN cToolDef = cToolDef + (IF NUM-ENTRIES(cToolDef,"¤") = 1 THEN "¤" ELSE " ") + "menu".
    DYNAMIC-FUNCTION("setObjectSourceProc",hSourceProc).  
    DYNAMIC-FUNCTION("AppendToolbar",TOOLBAR-HANDLE
      ,designWidget
      ,IF icParentMenuLabel NE "" THEN icParentMenuLabel ELSE cParentMenuLabel
      ,cToolDef
      ,cOptions
      ).                             
                                
  END METHOD.
               
                                      
  METHOD PUBLIC VOID addTool (icTool AS CHARACTER):
    addTool(icTool,"","","",NO,"","","","").
  END METHOD.
  METHOD PUBLIC VOID addTool (icTool        AS CHARACTER
    ,icLabel       AS CHARACTER):
    addTool(icTool,icLabel,"","",NO,"","","","").
  END METHOD.
  METHOD PUBLIC VOID addTool (icTool        AS CHARACTER
    ,icLabel       AS CHARACTER
    ,icImage       AS CHARACTER):
    addTool(icTool,icLabel,icImage,"",NO,"","","","").
  END METHOD.
  METHOD PUBLIC VOID addTool (icTool        AS CHARACTER
    ,icLabel       AS CHARACTER
    ,icImage       AS CHARACTER
    ,icParentMenu  AS CHARACTER):
    addTool(icTool,icLabel,icImage,icParentMenu,NO,"","","","").
  END METHOD.
  
  METHOD PUBLIC VOID addTool (icTool          AS CHARACTER
    ,icLabel         AS CHARACTER
    ,icImage         AS CHARACTER
    ,icParentMenu    AS CHARACTER
    ,ibAlwaysEnabled AS LOG):
    addTool(icTool,icLabel,icImage,icParentMenu,ibAlwaysEnabled,"","","","").
  END METHOD.
  
  METHOD PUBLIC VOID addTool (icTool          AS CHARACTER
    ,icLabel         AS CHARACTER
    ,icImage         AS CHARACTER
    ,icParentMenu    AS CHARACTER
    ,ibAlwaysEnabled AS LOG
    ,icAddAs         AS CHARACTER):  
    addTool(icTool,icLabel,icImage,icParentMenu,ibAlwaysEnabled,icAddAs,"","","").
  END METHOD.
  
  METHOD PUBLIC VOID addTool (icTool          AS CHARACTER
    ,icLabel         AS CHARACTER
    ,icImage         AS CHARACTER
    ,icParentMenu    AS CHARACTER
    ,ibAlwaysEnabled AS LOG
    ,icAddAs         AS CHARACTER
    ,icShortcut      AS CHARACTER):  
    addTool(icTool,icLabel,icImage,icParentMenu,ibAlwaysEnabled,icAddAs,icShortcut,"","").
  END METHOD.
  
  METHOD PUBLIC VOID addTool (icTool          AS CHARACTER
    ,icLabel         AS CHARACTER
    ,icImage         AS CHARACTER
    ,icParentMenu    AS CHARACTER
    ,ibAlwaysEnabled AS LOG
    ,icAddAs         AS CHARACTER
    ,icShortcut      AS CHARACTER
    ,icTooltip       AS CHARACTER):  
    addTool(icTool,icLabel,icImage,icParentMenu,ibAlwaysEnabled,icAddAs,icShortcut,icTooltip,"").
  END METHOD.
    
  /* Same again + set button to enabled */
  METHOD PUBLIC VOID addTool (icTool          AS CHARACTER
    ,ibAlwaysEnabled AS LOG):
    addTool(icTool,"","","",ibAlwaysEnabled,"","","","").
  END METHOD.
  METHOD PUBLIC VOID addTool (icTool          AS CHARACTER
    ,icLabel         AS CHARACTER
    ,ibAlwaysEnabled AS LOG):
    addTool(icTool,icLabel,"","",ibAlwaysEnabled,"","","","").
  END METHOD.
  
  METHOD PUBLIC VOID addTool (icTool          AS CHARACTER
    ,icLabel         AS CHARACTER
    ,icImage         AS CHARACTER
    ,ibAlwaysEnabled AS LOG):
    addTool(icTool,icLabel,icImage,"",ibAlwaysEnabled,"","","","").
  END METHOD.
  /* Same again + set button to enabled and decide how it should be added (button/menu-item/both) */
  METHOD PUBLIC VOID addTool (icTool          AS CHARACTER
    ,ibAlwaysEnabled AS LOG
    ,icAddAs         AS CHARACTER): /* "button": only buttons. "menu": only menu items. "": both */  
    addTool(icTool,"","","",ibAlwaysEnabled,icAddAs,"","","").
  END METHOD.
  METHOD PUBLIC VOID addTool (icTool          AS CHARACTER
    ,icLabel         AS CHARACTER
    ,ibAlwaysEnabled AS LOG
    ,icAddAs         AS CHARACTER):  
    addTool(icTool,icLabel,"","",ibAlwaysEnabled,icAddAs,"","","").
  END METHOD.
  METHOD PUBLIC VOID addTool (icTool          AS CHARACTER
    ,icLabel         AS CHARACTER
    ,icImage         AS CHARACTER
    ,ibAlwaysEnabled AS LOG
    ,icAddAs         AS CHARACTER):  
    addTool(icTool,icLabel,icImage,"",ibAlwaysEnabled,icAddAs,"","","").
  END METHOD.

 
  METHOD PUBLIC VOID addRule ():
    addTool("-","","","",NO,"","","","").
  END METHOD. 
  
  /* Toolgroup implementations */
  METHOD PUBLIC VOID addToolGroup (icGroup         AS CHARACTER
    ,icMenu          AS CHARACTER
    ,ibAlwaysEnabled AS LOG
    ,icAddAs         AS CHARACTER /* "button": only buttons. "menu": only menu items. "": both */  
    ,ibRule          AS LOG):
    DEFINE VARIABLE cButtons   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cTool      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cToolsList AS CHARACTER NO-UNDO.

    IF icMenu NE "" THEN 
    DO:
      IF cParentMenuLabel = "" THEN 
      DO:
        MESSAGE "Invalid reference to sub-menu. There is no root menu"
          VIEW-AS ALERT-BOX.
        RETURN.
      END.  
      IF DYNAMIC-FUNCTION("getAttribute",TOOLBAR-HANDLE,"sub-menu" + icMenu) = "" THEN 
      DO:
        MESSAGE "Invalid reference to parent menu: " icMenu
          VIEW-AS ALERT-BOX.
        RETURN.
      END.     
    END.
                         
    cButtons = DYNAMIC-FUNCTION("getAttribute",SESSION,icGroup).
    IF cButtons = "" THEN cButtons = icGroup.
    DO ix = 1 TO NUM-ENTRIES(cButtons):
      cTool = ENTRY(ix,cButtons).
      IF icAddAs = "button" THEN ENTRY(1,cTool,";") = ENTRY(1,cTool,";") + "|".
      IF ibAlwaysEnabled AND (NUM-ENTRIES(cTool,"¤") = 1 OR (NUM-ENTRIES(cTool,"¤") > 1 AND NOT entry(2,cTool,"¤") MATCHES "*enable*")) THEN 
        cTool = cTool + "¤enable".
      IF icAddAs = "menu" AND (NUM-ENTRIES(cTool,"¤") = 1 OR (NUM-ENTRIES(cTool,"¤") > 1 AND NOT entry(2,cTool,"¤") MATCHES "*menu*")) THEN 
        cTool = cTool + (IF NUM-ENTRIES(cTool,"¤") = 1 THEN "¤" ELSE " ") + "menu".
      
      cToolsList = cToolsList + (IF cToolsList NE "" THEN "," ELSE "") + cTool.
    END.
    IF ibRule AND NOT can-do("-,rule",ENTRY(1,cToolsList)) THEN cToolsList = "-," + cToolsList.
     
    DYNAMIC-FUNCTION("setObjectSourceProc",hSourceProc).  
    DYNAMIC-FUNCTION("AppendToolbar",TOOLBAR-HANDLE
      ,designWidget
      ,IF icMenu NE "" THEN icMenu ELSE cParentMenuLabel
      ,cToolsList
      ,cOptions
      ).                             
      
  END METHOD.
  METHOD PUBLIC VOID addToolGroup (icGroup AS CHARACTER):
    addToolGroup(icGroup,"",NO,"",NO).
  END METHOD.
  METHOD PUBLIC VOID addToolGroup (icGroup AS CHARACTER
    ,icMenu  AS CHARACTER):
    addToolGroup(icGroup,icMenu,NO,"",NO).
  END METHOD.
  METHOD PUBLIC VOID addToolGroup (icGroup         AS CHARACTER
    ,icMenu          AS CHARACTER
    ,ibAlwaysEnabled AS LOG):
    addToolGroup(icGroup,icMenu,ibAlwaysEnabled,"",NO).
  END METHOD.
  METHOD PUBLIC VOID addToolGroup (icGroup         AS CHARACTER
    ,icMenu          AS CHARACTER
    ,ibAlwaysEnabled AS LOG
    ,icAddAs         AS CHARACTER):
    addToolGroup(icGroup,icMenu,ibAlwaysEnabled,icAddAs,NO).
  END METHOD.
  /*
  METHOD PUBLIC VOID addToolGroup (icGroup         AS CHAR
                                  ,ibAlwaysEnabled AS LOG):
    addToolGroup(icGroup,"",ibAlwaysEnabled,"",NO).
  END METHOD.
  METHOD PUBLIC VOID addToolGroup (icGroup         AS CHAR
                                  ,ibAlwaysEnabled AS LOG
                                  ,icAddAs         AS CHAR):
    addToolGroup(icGroup,"",ibAlwaysEnabled,icAddAs,NO).
  END METHOD.
  */
   
  /* Sub-menu implementations */ 
  METHOD PUBLIC VOID addSubMenu (icSubMenu    AS CHARACTER
    ,icParentMenu AS CHARACTER):
    DEFINE VARIABLE cTmpMenuLabel AS CHARACTER NO-UNDO.                 
  
    IF cParentMenuLabel = "" THEN 
    DO:
      MESSAGE "Sub-menus cannot be created when there is no root menu"
        VIEW-AS ALERT-BOX.
      RETURN.
    END.
                 
    cTmpMenuLabel = cParentMenuLabel.                              
    IF icParentMenu = cParentMenuLabel THEN cParentMenuLabel = "".
    ELSE IF icParentMenu NE "" THEN 
      DO:
        IF DYNAMIC-FUNCTION("getAttribute",TOOLBAR-HANDLE,"sub-menu" + icParentMenu) = "" THEN 
        DO:
          MESSAGE "Invalid reference to parent menu: " icParentMenu
            VIEW-AS ALERT-BOX.
          RETURN.
        END.
      END.              
                            
    addTool(IF icParentMenu NE "" THEN 
      "sub-menu|" + icParentMenu + ";" + icSubMenu
      ELSE "|" + icSubMenu  
      ,"","","",NO,"menu","","","").
    cParentMenuLabel = cTmpMenuLabel.             
  END METHOD.
  
  METHOD PUBLIC VOID addSubMenu (icSubMenu AS CHARACTER):
    addTool("|" + icSubMenu,"","","",NO,"menu","","","").
  END METHOD.

  METHOD PUBLIC VOID applyTool (icTool AS CHARACTER):
    DEFINE VARIABLE hWidget AS HANDLE NO-UNDO.
    
    hWidget = DYNAMIC-FUNCTION("getEventWidget",TOOLBAR-HANDLE,icTool,"").
    IF VALID-HANDLE(hWidget) THEN 
      RUN InvokeMethod(TOOLBAR-HANDLE,DYNAMIC-FUNCTION("getEventMethod",TOOLBAR-HANDLE,hWidget)).
  END METHOD.

  METHOD PUBLIC VOID deleteMenuItem (icTool AS CHARACTER):
    DEFINE VARIABLE hWidget AS HANDLE NO-UNDO.
    
    hWidget = WIDGET-HANDLE(DYNAMIC-FUNCTION("getAttribute",TOOLBAR-HANDLE,"menu-item" + icTool)) NO-ERROR.
    IF VALID-HANDLE(hWidget) THEN DELETE OBJECT hWidget.
  END METHOD.
  
  METHOD PUBLIC HANDLE getButtonHandle (icTool  AS CHARACTER):
    DEFINE VARIABLE hWidget AS HANDLE NO-UNDO.
    hWidget = WIDGET-HANDLE(DYNAMIC-FUNCTION("getAttribute",TOOLBAR-HANDLE,"button" + icTool)) NO-ERROR.
    RETURN hWidget.
  END METHOD.
  
  METHOD PUBLIC HANDLE getMenuItemHandle (icTool  AS CHARACTER):
    DEFINE VARIABLE hWidget AS HANDLE NO-UNDO.
    hWidget = WIDGET-HANDLE(DYNAMIC-FUNCTION("getAttribute",TOOLBAR-HANDLE,"menu-item" + icTool)) NO-ERROR.
    RETURN hWidget.
  END METHOD.

  METHOD PUBLIC HANDLE getSubMenuHandle (icSubMenu  AS CHARACTER):
    DEFINE VARIABLE hWidget AS HANDLE NO-UNDO.
    hWidget = WIDGET-HANDLE(DYNAMIC-FUNCTION("getAttribute",TOOLBAR-HANDLE,"sub-menu" + icSubMenu)) NO-ERROR.
    RETURN hWidget.
  END METHOD.

  METHOD PUBLIC VOID setDisabledTools (icToolList AS CHARACTER):
    DYNAMIC-FUNCTION("setAttribute",TOOLBAR-HANDLE,"disabledEvents",icToolList).
  END METHOD.

  METHOD PUBLIC VOID setToolDisabled (icTool AS CHARACTER):
    DEFINE VARIABLE cEnaEvents AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cDisEvents AS CHARACTER NO-UNDO.
    DEFINE VARIABLE ix         AS INTEGER   NO-UNDO.
    DEFINE VARIABLE cTools     AS CHARACTER NO-UNDO.
 
    IF icTool = "" THEN 
    DO:
      THIS-OBJECT:setDisabledTools("").
      RETURN.
    END.  
    
    cDisEvents = DYNAMIC-FUNCTION("getAttribute",TOOLBAR-HANDLE,"disabledEvents").
    cEnaEvents = DYNAMIC-FUNCTION("getAttribute",TOOLBAR-HANDLE,"enabledEvents").

    cTools = icTool.  
    DO ix = 1 TO NUM-ENTRIES(cTools):
      icTool = ENTRY(ix,cTools).

      IF NOT CAN-DO(cDisEvents,icTool) THEN 
      DO:
        cDisEvents = cDisEvents + (IF cDisEvents NE "" THEN "," ELSE "") + icTool.
        DYNAMIC-FUNCTION("setAttribute",TOOLBAR-HANDLE,"disabledEvents",cDisEvents).
      END.  
      IF CAN-DO(cEnaEvents,icTool) THEN 
      DO:
        cEnaEvents = DYNAMIC-FUNCTION("DeleteListItem",cEnaEvents,icTool,0,"").
        DYNAMIC-FUNCTION("setAttribute",TOOLBAR-HANDLE,"enabledEvents",cEnaEvents).
      END.
    END.    
  END METHOD.

  METHOD PUBLIC VOID setEnabledTools (icToolList AS CHARACTER):
    DYNAMIC-FUNCTION("setAttribute",TOOLBAR-HANDLE,"enabledEvents",icToolList).
  END METHOD.
  
  METHOD PUBLIC VOID setToolEnabled (icTool AS CHARACTER):
    DEFINE VARIABLE cEnaEvents AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cDisEvents AS CHARACTER NO-UNDO.
    DEFINE VARIABLE ix         AS INTEGER   NO-UNDO.
    DEFINE VARIABLE cTools     AS CHARACTER NO-UNDO.
    
    IF icTool = "" THEN 
    DO:
      THIS-OBJECT:setEnabledTools("").
      RETURN.
    END.
    cDisEvents = DYNAMIC-FUNCTION("getAttribute",TOOLBAR-HANDLE,"disabledEvents").
    cEnaEvents = DYNAMIC-FUNCTION("getAttribute",TOOLBAR-HANDLE,"enabledEvents").
    
    cTools = icTool.  
    DO ix = 1 TO NUM-ENTRIES(cTools):
      icTool = ENTRY(ix,cTools).
      IF NOT CAN-DO(cEnaEvents,icTool) THEN 
      DO:
        cEnaEvents = cEnaEvents + (IF cEnaEvents NE "" THEN "," ELSE "") + icTool.
        DYNAMIC-FUNCTION("setAttribute",TOOLBAR-HANDLE,"enabledEvents",cEnaEvents).
      END.
      IF CAN-DO(cDisEvents,icTool) THEN 
      DO:
        cDisEvents = DYNAMIC-FUNCTION("DeleteListItem",cDisEvents,icTool,0,"").
        DYNAMIC-FUNCTION("setAttribute",TOOLBAR-HANDLE,"disabledEvents",cDisEvents).
      END.
    END.    
  END METHOD.
  
  METHOD PUBLIC VOID setSubMenuSensitive (icSubMenu AS CHARACTER,ibSensitive AS LOG):
    DEFINE VARIABLE hWidget AS HANDLE NO-UNDO.
    hWidget = getSubMenuHandle(icSubMenu).
    IF VALID-HANDLE(hWidget) THEN hWidget:SENSITIVE = ibSensitive.
  END METHOD.

  METHOD PUBLIC VOID setToolLabel (icTool AS CHARACTER
    ,icLabel  AS CHARACTER):
    DEFINE VARIABLE hWidget AS HANDLE NO-UNDO.
    hWidget = WIDGET-HANDLE(DYNAMIC-FUNCTION("getAttribute",TOOLBAR-HANDLE,"button" + icTool)) NO-ERROR.
    IF VALID-HANDLE(hWidget) THEN 
    DO:
      hWidget:LABEL = icLabel.
      IF hWidget:IMAGE NE "" THEN hWidget:TOOLTIP = icLabel.
    END.  
    hWidget = WIDGET-HANDLE(DYNAMIC-FUNCTION("getAttribute",TOOLBAR-HANDLE,"menu-item" + icTool)) NO-ERROR.
    IF VALID-HANDLE(hWidget) THEN
      hWidget:LABEL = icLabel.                                  
  END METHOD.

  METHOD PUBLIC LOGICAL setToolMethod (icTool    AS CHARACTER
    ,icMethod  AS CHARACTER):
    DEFINE VARIABLE hWidget AS HANDLE NO-UNDO.
    DEFINE VARIABLE bReturn AS LOG    NO-UNDO.
    hWidget = WIDGET-HANDLE(DYNAMIC-FUNCTION("getAttribute",TOOLBAR-HANDLE,"button" + icTool)) NO-ERROR.
    IF VALID-HANDLE(hWidget) THEN 
    DO:
      DYNAMIC-FUNCTION("setEventMethod",TOOLBAR-HANDLE,hWidget,icMethod).
      bReturn = YES.
    END.  
    hWidget = WIDGET-HANDLE(DYNAMIC-FUNCTION("getAttribute",TOOLBAR-HANDLE,"menu-item" + icTool)) NO-ERROR.
    IF VALID-HANDLE(hWidget) THEN 
    DO:
      DYNAMIC-FUNCTION("setEventMethod",TOOLBAR-HANDLE,hWidget,icMethod).
      bReturn = YES.
    END.  
  END METHOD.
  
  METHOD PUBLIC VOID setToolShortcut (icTool AS CHARACTER
    ,icShortcut  AS CHARACTER):                                       
    DEFINE VARIABLE hWidget AS HANDLE    NO-UNDO.
    DEFINE VARIABLE cLabel  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cScChar AS CHARACTER NO-UNDO.
    
    icShortcut = CAPS(icShortcut).
    IF INDEX(icShortCut,"-") > 0 THEN
      cScChar = substr(icShortCut,INDEX(icShortCut,"-") + 1,1).
    
    hWidget = WIDGET-HANDLE(DYNAMIC-FUNCTION("getAttribute",TOOLBAR-HANDLE,"button" + icTool)) NO-ERROR.
    IF VALID-HANDLE(hWidget) THEN 
    DO:
      hWidget:LABEL = REPLACE(hWidget:LABEL,"&","").
      IF INDEX(hWidget:TOOLTIP,"ctrl-") > 0 THEN 
        hWidget:TOOLTIP = substr(hWidget:TOOLTIP,1,INDEX(hWidget:TOOLTIP,"ctrl-") - 2) 
          + (IF icShortCut NE "" THEN "(" + icShortcut + ")" ELSE "").
      ELSE 
      DO:
        IF INDEX(hWidget:TOOLTIP,"alt-") > 0 THEN
          hWidget:TOOLTIP = substr(hWidget:TOOLTIP,1,INDEX(hWidget:TOOLTIP,"alt-") - 1) + icShortcut + ")".
        ELSE  
          hWidget:TOOLTIP = hWidget:TOOLTIP + " (" + icShortcut + ")".
        IF INDEX(hWidget:LABEL,cScChar) > 0 THEN
          ASSIGN cLabel        = substr(hWidget:LABEL,INDEX(hWidget:LABEL,cScChar))
            hWidget:LABEL = SUBSTR(hWidget:LABEL,1,INDEX(hWidget:LABEL,cScChar) - 1) + "&" + cLabel.     
      END.  
      cLabel = hWidget:LABEL.
    END.  
    hWidget = WIDGET-HANDLE(DYNAMIC-FUNCTION("getAttribute",TOOLBAR-HANDLE,"menu-item" + icTool)) NO-ERROR.
    IF VALID-HANDLE(hWidget) THEN
      ASSIGN hWidget:ACCELERATOR = icShortCut
        hWidget:LABEL       = cLabel.                                 
  END METHOD.
  
  METHOD PUBLIC VOID setToolTooltip (icTool  AS CHARACTER
    ,icTooltip AS CHARACTER):
    DEFINE VARIABLE hWidget AS HANDLE NO-UNDO.
    hWidget = WIDGET-HANDLE(DYNAMIC-FUNCTION("getAttribute",TOOLBAR-HANDLE,"button" + icTool)) NO-ERROR.
    IF VALID-HANDLE(hWidget) THEN
      hWidget:TOOLTIP = icTooltip.
  END METHOD.

  METHOD PUBLIC VOID LinkToBrowse (ioBrowse  AS JBoxBrowse).
    DYNAMIC-FUNCTION("CreateObjectLink",TOOLBAR-HANDLE,ioBrowse:BROWSE-HANDLE).  
  END METHOD.                                  

  METHOD PUBLIC VOID setTools(icState  AS CHARACTER):
    DYNAMIC-FUNCTION("setToolbar",TOOLBAR-HANDLE,icState).
  END METHOD.

  METHOD PUBLIC VOID setTools ():
    THIS-OBJECT:setTools("avail"). /* same as property objectState */
  END METHOD.
  
  METHOD PUBLIC VOID setJBoxAttribute(icAttribute AS CHAR
                                     ,icValue AS CHAR):
    DYNAMIC-FUNCTION("setAttribute",TOOLBAR-HANDLE,icAttribute,icValue).
  END METHOD.
  METHOD PUBLIC CHARACTER getJBoxAttribute(icAttribute AS CHAR):
    RETURN  DYNAMIC-FUNCTION("getAttribute",TOOLBAR-HANDLE,icAttribute).
  END METHOD.

  METHOD PUBLIC LOGICAL getActionPermission(INPUT icAction AS CHAR):
    IF PROVERSION < "11" THEN hSourceProc = DYNAMIC-FUNCTION ("getProcedureHandle",PROGRAM-NAME(2)).
    ELSE hSourceProc = SOURCE-PROCEDURE.

    RUN JBoxLogFuncToMeth.p ("getActionPermission"
                            ,"CHARACTER,CHARACTER,CHARACTER"
                            ,hSourceProc:FILE-NAME + CHR(1) + designWidget:NAME + CHR(1) + icAction
                            ,?
                            ,OUTPUT bOk).
    RETURN bOk.                        
  END.

  DESTRUCTOR PUBLIC JBoxToolbar ( ):
  /*    DYNAMIC-FUNCTION("DeleteObject",TOOLBAR-HANDLE).*/
  END DESTRUCTOR.

END CLASS.
