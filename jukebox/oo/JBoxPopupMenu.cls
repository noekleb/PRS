 
 /*------------------------------------------------------------------------
    File        : JBoxPopupMenu
    Purpose     : 
    Syntax      : 
    Description : ABL popup menu for use with browse, button, frame, etc
    Author(s)   : brynjar
    Created     : Fri Oct 04 18:07:12 CEST 2013
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.

CLASS JBoxPopupMenu: 
	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
  DEF VAR hSourceProc   AS HANDLE NO-UNDO.
  DEF VAR bOk           AS LOG    NO-UNDO.
  DEF VAR ix            AS INT    NO-UNDO.
  DEF VAR cOptions      AS CHAR   NO-UNDO.
  DEF VAR hParentMenu   AS HANDLE NO-UNDO.
      
  DEF PUBLIC PROPERTY POPUP-MENU-HANDLE    AS HANDLE GET. PROTECTED SET.
		
	CONSTRUCTOR PUBLIC JBoxPopupMenu (  ):
		SUPER ().
		
    IF PROVERSION < "11" THEN hSourceProc = DYNAMIC-FUNCTION ("getProcedureHandle",PROGRAM-NAME(2)).
    ELSE hSourceProc = SOURCE-PROCEDURE.
    
    CREATE MENU POPUP-MENU-HANDLE.
    POPUP-MENU-HANDLE:POPUP-ONLY = yes.
	END CONSTRUCTOR.

  CONSTRUCTOR PUBLIC JBoxPopupMenu (ihPopup      AS HANDLE
                                   ,ihSourceProc AS HANDLE):
    SUPER ().
    
    hSourceProc = ihSourceProc.
    
    POPUP-MENU-HANDLE = ihPopup.
  END CONSTRUCTOR.
		
  METHOD PUBLIC VOID addTool (icTool            AS CHAR
                             ,icLabel           AS CHAR
                             ,icParentMenuLabel AS CHAR
                             ,ibAlwaysEnabled   AS LOG
                             ,ibToggle          AS LOG
                             ,ibToggleChecked   AS LOG
                             ,icMethod          AS CHAR
                              ).
    
    if icTool = "" then do:
      MESSAGE "Invalid definition: No Tool is defined for addTool"
              VIEW-AS ALERT-BOX.
      return.        
    end.                                    
    IF icParentMenuLabel NE "" THEN DO:
      hParentMenu = WIDGET-HANDLE(DYNAMIC-FUNCTION("getAttribute",POPUP-MENU-HANDLE,"sub-menu" + icParentMenuLabel)) NO-ERROR.
      IF NOT VALID-HANDLE(hParentMenu) THEN DO:
        MESSAGE "Invalid parent menu " + icParentMenuLabel
                 VIEW-AS ALERT-BOX.
        RETURN.
      END.         
    END.                          

    if ibAlwaysEnabled   then icTool = icTool + "¤enable".
    DYNAMIC-FUNCTION("setObjectSourceProc",hSourceProc).  
    DYNAMIC-FUNCTION("NewMenuBand",IF VALID-HANDLE(hParentMenu) THEN hParentMenu ELSE POPUP-MENU-HANDLE
                                  ,icTool + ";" + icLabel + ";" + icMethod                                  
                                  ,cOptions
                                  ).
                                                               
    hParentMenu = ?.                            
  END METHOD.
  METHOD PUBLIC VOID addTool (icTool AS CHAR):
    addTool(icTool,"","",NO,NO,NO,"").
  END METHOD.
  METHOD PUBLIC VOID addTool (icTool  AS CHAR
                             ,icLabel AS CHAR):
    addTool(icTool,icLabel,"",NO,NO,NO,"").
  END METHOD.
  METHOD PUBLIC VOID addTool (icTool       AS CHAR
                             ,icLabel      AS CHAR
                             ,icParentMenuLabel AS CHAR):
    addTool(icTool,icLabel,icParentMenuLabel,NO,NO,NO,"").
  END METHOD.

  METHOD PUBLIC VOID addRule ():
    addTool("-","","",NO,NO,NO,"").
  END METHOD.
  
  /* Toolgroup implementations */
  METHOD PUBLIC VOID addToolGroup (icGroup           AS CHAR
                                  ,icParentMenuLabel AS CHAR
                                  ,ibAlwaysEnabled   AS LOG
                                  ,ibRule            AS LOG):
    def var cMenuItems   as char no-undo.
    def var cTool      as char no-undo.
    def var cToolsList as char no-undo.
        
    cMenuItems = DYNAMIC-FUNCTION("getAttribute",SESSION,icGroup).
    if cMenuItems = "" then cMenuItems = icGroup.
    do ix = 1 to num-entries(cMenuItems):
      cTool = entry(ix,cMenuItems).
      if ibAlwaysEnabled and (num-entries(cTool,"¤") = 1 or (num-entries(cTool,"¤") > 1 and not entry(2,cTool,"¤") matches "*enable*")) then 
        cTool = cTool + "¤enable".
      cToolsList = cToolsList + (if cToolsList ne "" then "," else "") + cTool.
    end.
    if ibRule and not can-do("-,rule",entry(1,cToolsList)) then cToolsList = "-," + cToolsList.
     
    IF icParentMenuLabel NE "" THEN DO:
      hParentMenu = WIDGET-HANDLE(DYNAMIC-FUNCTION("getAttribute",POPUP-MENU-HANDLE,"sub-menu" + icParentMenuLabel)) NO-ERROR.
      IF NOT VALID-HANDLE(hParentMenu) THEN DO:
        MESSAGE "Invalid parent menu " + icParentMenuLabel
                 VIEW-AS ALERT-BOX.
        RETURN.
      END.         
    END.                          

    DYNAMIC-FUNCTION("setObjectSourceProc",hSourceProc).  
    DYNAMIC-FUNCTION("NewMenuBand",IF VALID-HANDLE(hParentMenu) THEN hParentMenu ELSE POPUP-MENU-HANDLE
                                  ,cToolsList                                  
                                  ,cOptions
                                  ).
                                                               
    hParentMenu = ?.                            
      
  END METHOD.
  METHOD PUBLIC VOID addToolGroup (icGroup AS CHAR):
    addToolGroup(icGroup,"",NO,NO).
  END METHOD.
  METHOD PUBLIC VOID addToolGroup (icGroup AS CHAR
                                  ,icMenu  AS CHAR):
    addToolGroup(icGroup,icMenu,NO,NO).
  END METHOD.
  METHOD PUBLIC VOID addToolGroup (icGroup         AS CHAR
                                  ,icMenu          AS CHAR
                                  ,ibAlwaysEnabled AS LOG):
    addToolGroup(icGroup,icMenu,ibAlwaysEnabled,NO).
  END METHOD.
  /*
  METHOD PUBLIC VOID addToolGroup (icGroup         AS CHAR
                                  ,ibAlwaysEnabled AS LOG):
    addToolGroup(icGroup,"",ibAlwaysEnabled,NO).
  END METHOD.
  METHOD PUBLIC VOID addToolGroup (icGroup         AS CHAR
                                  ,ibAlwaysEnabled AS LOG
                                  ,ibRule          AS LOG):
    addToolGroup(icGroup,"",ibAlwaysEnabled,ibRule).
  END METHOD. 
  */

  /* Sub-menu implementations: */
  METHOD PUBLIC VOID addSubMenu (icSubMenu         AS CHAR
                                ,icParentMenuLabel AS CHAR):
                                  
    if icSubMenu = "" then do:
      MESSAGE "Invalid definition: No Sub-menu is defined for addSubMenu"
              VIEW-AS ALERT-BOX.
      return.        
    end.                                                         
    addTool("|" + icSubMenu,"",icParentMenuLabel,NO,NO,NO,"").
  END METHOD.
  METHOD PUBLIC VOID addSubMenu (icSubMenu AS CHAR):
    addSubMenu(icSubMenu,"").
  END METHOD.

  METHOD PUBLIC HANDLE getMenuItemHandle (icTool  as char):
    def var hWidget as handle no-undo.
    hWidget = WIDGET-HANDLE(DYNAMIC-FUNCTION("getAttribute",POPUP-MENU-HANDLE,"menu-item" + icTool)) NO-ERROR.
    return hWidget.
  end method.

  METHOD PUBLIC HANDLE getSubMenuHandle (icTool  as char):
    def var hWidget as handle no-undo.
    hWidget = WIDGET-HANDLE(DYNAMIC-FUNCTION("getAttribute",POPUP-MENU-HANDLE,"sub-menu" + icTool)) NO-ERROR.
    return hWidget.
  end method.

	DESTRUCTOR PUBLIC JBoxPopupMenu ( ):

	END DESTRUCTOR.

END CLASS.