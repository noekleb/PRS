 
 /*------------------------------------------------------------------------
    File        : JBoxMainMenu	
    Purpose     : Singleton for main menu mgmt
    Syntax      : 
    Description : 
    Author(s)   : Brynjar
    Created     : Tue Mar 15 10:03:36 CEST 2015
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.

ROUTINE-LEVEL ON ERROR UNDO, THROW.

CLASS JBoxMainMenu: 

  DEF VAR hSourceProc  AS HANDLE NO-UNDO.
  DEF VAR iClientXsize AS INT NO-UNDO INIT 1200.
  DEF VAR iClientYsize AS INT NO-UNDO INIT 800.
  DEF VAR hMenu        AS HANDLE NO-UNDO.

  DEFINE PUBLIC STATIC PROPERTY Instance AS CLASS JBoxMainMenu NO-UNDO
         GET():
           IF Instance = ? THEN Instance = NEW JBoxMainMenu().
           RETURN Instance.
         END GET.
         PRIVATE SET.
         
  DEFINE PUBLIC PROPERTY MainMenuHandle AS HANDLE NO-UNDO GET. SET.       

  DEFINE PUBLIC PROPERTY oMainForm AS JBoxMainForm NO-UNDO GET. PROTECTED SET.       

  DEFINE PUBLIC PROPERTY ProgramMultiInstanceList AS CHAR NO-UNDO
         GET(): RETURN DYNAMIC-FUNCTION("getProgramMultiInstanceList" IN hMenu). END GET.
         SET(INPUT icValue AS CHAR): DYNAMIC-FUNCTION("setProgramMultiInstanceList" IN hMenu,icValue). END SET.

  DEFINE PUBLIC PROPERTY StatusText AS CHAR NO-UNDO
         GET(): RETURN oMainForm:getStatusText(). END GET.
         SET(INPUT icValue AS CHAR): oMainForm:setStatusText(icValue). END SET.
	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
		
	CONSTRUCTOR PRIVATE JBoxMainMenu ():
	  
	  hMenu = WIDGET-HANDLE(DYNAMIC-FUNCTION("getAttribute",SESSION,"mainmenuhandle")) NO-ERROR.
	  
	  IF NOT VALID-HANDLE(hMenu) THEN
      RUN JBoxRibbonMenu.p PERSIST SET hMenu.    
	  
	END CONSTRUCTOR.

	METHOD PUBLIC LOGICAL InitializeObject(INPUT iiMenuId AS INT):
	        
    SESSION:SET-WAIT-STATE("General").
    
    IF hSourceProc = ? THEN hSourceProc = SOURCE-PROCEDURE.
    
    MainMenuHandle = hMenu.

    IF SESSION:HEIGHT-PIXELS LT 800 THEN
      DYNAMIC-FUNCTION("setRibbonMinimized" IN MainMenuHandle,YES).
      
    DYNAMIC-FUNCTION("setClientSize" IN MainMenuHandle,iClientXsize,iClientYsize).
    DYNAMIC-FUNCTION("setAttribute",SESSION,"mainMenuType","Ribbon").

    DYNAMIC-FUNCTION("setAttribute",SESSION,"mainmenuhandle",STRING(MainMenuHandle)).     
    
    SUBSCRIBE PROCEDURE hSourceProc TO "InvalidateHandle" IN MainMenuHandle.   
    
    RUN InitializeObject IN MainMenuHandle(iiMenuId).

    DYNAMIC-FUNCTION("setParentHandle" IN MainMenuHandle,hSourceProc).
    DYNAMIC-FUNCTION("setWindowTitle" IN MainMenuHandle,DYNAMIC-FUNCTION("getAppTitle") + " [" + DYNAMIC-FUNCTION("getCompanyName") + "]").
    DYNAMIC-FUNCTION("setStatusText" IN MainMenuHandle,DYNAMIC-FUNCTION("getASuserName")).
    DYNAMIC-FUNCTION("setWinMenuGroupLabel" IN MainMenuHandle,IF DYNAMIC-FUNCTION("Scandinavian") THEN "Behandle" ELSE "Actions").
    IF NOT OS-GETENV("SESSIONNAME") BEGINS "RDP" THEN
      DYNAMIC-FUNCTION("setAppStyleSheet" IN MainMenuHandle,"vs2008_test.isl").
  
    RUN getMainFormObject IN MainMenuHandle (OUTPUT oMainForm).
  
    SESSION:SET-WAIT-STATE("").
  
	END METHOD.

  METHOD PUBLIC LOGICAL InitializeObject():
    hSourceProc = SOURCE-PROCEDURE.
    THIS-OBJECT:InitializeObject(-1).
  END METHOD.  

  METHOD PUBLIC LOGICAL setWindowCaption(icAppTitle     AS CHAR
                                        ,icContextTitle AS CHAR):
         
    IF VALID-OBJECT(oMainForm) THEN DO:                               
      oMainForm:setWindowTitle((IF icAppTitle NE "" THEN icAppTitle ELSE DYNAMIC-FUNCTION("getAppTitle")) + (IF icContextTitle NE "" THEN " [" + icContextTitle + "]" ELSE "")).
      RETURN YES.
    END.  
    ELSE RETURN NO. 
    
  END METHOD.

  METHOD PUBLIC LOGICAL setWindowCaption(icContextTitle AS CHAR):
                                   
    RETURN THIS-OBJECT:setWindowCaption("",icContextTitle).
    
  END METHOD.


  METHOD PUBLIC LOGICAL setWindowSize(iiWidthPixels  AS INT
                                     ,iiHeightPixels AS INT):  
    IF VALID-HANDLE(hMenu) THEN DO:
      DYNAMIC-FUNCTION("setClientSize" IN hMenu,iiWidthPixels,iiHeightPixels).
      RETURN YES.
    END.
    ELSE RETURN NO.
                                        
  END METHOD.
  
  METHOD PUBLIC LOGICAL setInitAutoAdjustWindowSize():
    DEF VAR iClientYsize AS INT NO-UNDO.
      
    IF SESSION:HEIGHT-PIXELS > 800 THEN DO:
      iClientYsize = SESSION:HEIGHT-PIXELS - 50.
      RETURN THIS-OBJECT:setWindowSize(INT(iClientYsize * 1.5), iClientYsize).
    END.
    RETURN NO.
                                        
  END METHOD.

  METHOD PUBLIC VOID LoadMenuFromXMLfile (icMenuXML  AS CHAR):
    DEF VAR lcXML AS LONGCHAR NO-UNDO.
    
    DEF VAR oMenuXml AS JBoxXmlDoc NO-UNDO.
    
    IF MainMenuHandle NE ? THEN 
      MESSAGE "Cannot load menu definition because the menu is already initialized"
              VIEW-AS ALERT-BOX ERROR.
    ELSE DO:
      IF SEARCH(icMenuXML) NE ? THEN 
        COPY-LOB FROM FILE icMenuXML TO OBJECT lcXML.
      ELSE 
        lcXML = icMenuXML.        
      
      oMenuXml = NEW JBoxXmlDoc().
      oMenuXml:LoadXml(lcXML).
              
      RUN LoadFromXmlNodeTable IN hMenu (oMenuXml:hbttNodes).
    END.  
          
  END METHOD.

  METHOD PUBLIC VOID LoadMenuFromXMLNodeTable (ihNodeTable AS HANDLE):
    
    IF MainMenuHandle NE ? THEN 
      MESSAGE "Cannot load menu definition because the menu is already initialized"
              VIEW-AS ALERT-BOX ERROR.
    ELSE               
      RUN LoadFromXmlNodeTable IN hMenu (ihNodeTable).
          
  END METHOD.

  METHOD PUBLIC VOID StartMenu(iiMenuId  AS INT):
    
    RUN StartMenuId IN hMenu (iiMenuId).
          
  END METHOD.

  METHOD PUBLIC HANDLE StartChildWindow(icProcName   AS CHAR
                                       ,icWinTitle   AS CHAR
                                       ,ihParent     AS HANDLE
                                       ,ibNew        AS LOG):
          
    DEF VAR hChild AS HANDLE NO-UNDO.
   
    RUN StartChildWindow IN hMenu(icProcName,icWinTitle,ihParent,ibNew,OUTPUT hChild). 
      
    RETURN hChild.
  END METHOD.    

  METHOD PUBLIC HANDLE StartChildWindow(icProcName   AS CHAR
                                       ,icWinTitle   AS CHAR
                                       ,icParameter  AS CHAR
                                       ,ihParent     AS HANDLE
                                       ,ibNew        AS LOG):
          
    DEF VAR hChild AS HANDLE NO-UNDO.
    
    RUN setWinStartParam IN hMenu(icParameter).
    RUN StartChildWindow IN hMenu(icProcName,icWinTitle,ihParent,ibNew,OUTPUT hChild). 
      
    RETURN hChild.
  END METHOD.    

  METHOD PUBLIC HANDLE StartChildWindow(icProcName   AS CHAR
                                       ,icWinTitle   AS CHAR
                                       ,ibNew        AS LOG):
          
    DEF VAR hParent AS HANDLE NO-UNDO.

    IF PROVERSION < "11" THEN hParent = DYNAMIC-FUNCTION ("getProcedureHandle",PROGRAM-NAME(2)).
    ELSE hParent = SOURCE-PROCEDURE.
  
    RETURN THIS-OBJECT:StartChildWindow(icProcName,icWinTitle,hParent,ibNew). 
  END METHOD.    

  METHOD PUBLIC HANDLE StartChildWindow(icProcName   AS CHAR
                                       ,icWinTitle   AS CHAR):
          
    DEF VAR hParent AS HANDLE NO-UNDO.

    IF PROVERSION < "11" THEN hParent = DYNAMIC-FUNCTION ("getProcedureHandle",PROGRAM-NAME(2)).
    ELSE hParent = SOURCE-PROCEDURE.
  
    RETURN THIS-OBJECT:StartChildWindow(icProcName,icWinTitle,hParent,NO). 
  END METHOD.    

  METHOD PUBLIC HANDLE StartChildWindow(icProcName   AS CHAR):
          
    DEF VAR hParent AS HANDLE NO-UNDO.

    IF PROVERSION < "11" THEN hParent = DYNAMIC-FUNCTION ("getProcedureHandle",PROGRAM-NAME(2)).
    ELSE hParent = SOURCE-PROCEDURE.
  
    RETURN THIS-OBJECT:StartChildWindow(icProcName,"",hParent,NO). 
  END METHOD.    

  METHOD PUBLIC HANDLE StartTabWindow(icProcName   AS CHAR
                                      ,icWinTitle  AS CHAR
                                      ,ihParent    AS HANDLE
                                      ,ibNew       AS LOG
                                      ,ibKeepOpen  AS LOG):
          
    DEF VAR hChild AS HANDLE NO-UNDO.
   
    RUN StartTabWindow IN hMenu(icProcName,icWinTitle,ihParent,ibNew,ibKeepOpen,OUTPUT hChild). 
      
    RETURN hChild.
  END METHOD.    

  METHOD PUBLIC HANDLE StartTabWindow(icProcName   AS CHAR
                                      ,icWinTitle  AS CHAR
                                      ,ibNew       AS LOG
                                      ,ibKeepOpen  AS LOG):
          
    DEF VAR hParent AS HANDLE NO-UNDO.

    IF PROVERSION < "11" THEN hParent = DYNAMIC-FUNCTION ("getProcedureHandle",PROGRAM-NAME(2)).
    ELSE hParent = SOURCE-PROCEDURE.
   
    RETURN THIS-OBJECT:StartTabWindow(icProcName,icWinTitle,hParent,ibNew,ibKeepOpen). 
  END METHOD.    

  METHOD PUBLIC HANDLE StartTabWindow(icProcName   AS CHAR
                                      ,icWinTitle  AS CHAR
                                      ,icParameter AS CHAR
                                      ,ibNew       AS LOG
                                      ,ibKeepOpen  AS LOG):
          
    DEF VAR hParent AS HANDLE NO-UNDO.

    IF PROVERSION < "11" THEN hParent = DYNAMIC-FUNCTION ("getProcedureHandle",PROGRAM-NAME(2)).
    ELSE hParent = SOURCE-PROCEDURE.
   
    RUN setWinStartParam IN hMenu(icParameter).
    RETURN THIS-OBJECT:StartTabWindow(icProcName,icWinTitle,hParent,ibNew,ibKeepOpen). 
  END METHOD.    

  METHOD PUBLIC HANDLE StartTabWindow(icProcName   AS CHAR
                                      ,icWinTitle  AS CHAR):
          
    DEF VAR hParent AS HANDLE NO-UNDO.

    IF PROVERSION < "11" THEN hParent = DYNAMIC-FUNCTION ("getProcedureHandle",PROGRAM-NAME(2)).
    ELSE hParent = SOURCE-PROCEDURE.
   
    RETURN THIS-OBJECT:StartTabWindow(icProcName,icWinTitle,hParent,NO,NO). 
  END METHOD.    

  METHOD PUBLIC HANDLE StartTabWindow(icProcName   AS CHAR):
          
    DEF VAR hParent AS HANDLE NO-UNDO.

    IF PROVERSION < "11" THEN hParent = DYNAMIC-FUNCTION ("getProcedureHandle",PROGRAM-NAME(2)).
    ELSE hParent = SOURCE-PROCEDURE.
   
    RETURN THIS-OBJECT:StartTabWindow(icProcName,"",hParent,NO,NO). 
  END METHOD.    

  METHOD PUBLIC HANDLE StartChildTab  (icProcName   AS CHAR
                                      ,icWinTitle  AS CHAR
                                      ,ihParent    AS HANDLE
                                      ,ibNew       AS LOG
                                      ,ibKeepOpen  AS LOG):
          
    DEF VAR hChild AS HANDLE NO-UNDO.
   
    RUN StartChildTab IN hMenu(icProcName,icWinTitle,ihParent,ibNew,ibKeepOpen,"",OUTPUT hChild). 
      
    RETURN hChild.
  END METHOD.    

  METHOD PUBLIC HANDLE StartChildTab  (icProcName  AS CHAR
                                      ,icWinTitle  AS CHAR
                                      ,icParameter AS CHAR
                                      ,ibNew       AS LOG
                                      ,ibKeepOpen  AS LOG):
          
    DEF VAR hChild AS HANDLE NO-UNDO.
    DEF VAR hParent AS HANDLE NO-UNDO.

    IF PROVERSION < "11" THEN hParent = DYNAMIC-FUNCTION ("getProcedureHandle",PROGRAM-NAME(2)).
    ELSE hParent = SOURCE-PROCEDURE.
   
    RUN setWinStartParam IN hMenu(icParameter).
    RUN StartChildTab IN hMenu(icProcName,icWinTitle,hParent,ibNew,ibKeepOpen,"",OUTPUT hChild). 
      
    RETURN hChild.
  END METHOD.    

  METHOD PUBLIC HANDLE StartChildTab(icProcName   AS CHAR
                                      ,icWinTitle  AS CHAR
                                      ,ibNew       AS LOG
                                      ,ibKeepOpen  AS LOG):
          
    DEF VAR hParent AS HANDLE NO-UNDO.

    IF PROVERSION < "11" THEN hParent = DYNAMIC-FUNCTION ("getProcedureHandle",PROGRAM-NAME(2)).
    ELSE hParent = SOURCE-PROCEDURE.
   
    RETURN THIS-OBJECT:StartChildTab(icProcName,icWinTitle,hParent,ibNew,ibKeepOpen). 
  END METHOD.    

  METHOD PUBLIC HANDLE StartChildTab(icProcName   AS CHAR
                                      ,icWinTitle  AS CHAR
                                      ,ibNew       AS LOG):
          
    DEF VAR hParent AS HANDLE NO-UNDO.

    IF PROVERSION < "11" THEN hParent = DYNAMIC-FUNCTION ("getProcedureHandle",PROGRAM-NAME(2)).
    ELSE hParent = SOURCE-PROCEDURE.
   
    RETURN THIS-OBJECT:StartChildTab(icProcName,icWinTitle,hParent,ibNew,NO). 
  END METHOD.    

  METHOD PUBLIC HANDLE StartChildTab(icProcName   AS CHAR
                                      ,icWinTitle  AS CHAR):
          
    DEF VAR hParent AS HANDLE NO-UNDO.

    IF PROVERSION < "11" THEN hParent = DYNAMIC-FUNCTION ("getProcedureHandle",PROGRAM-NAME(2)).
    ELSE hParent = SOURCE-PROCEDURE.
   
    RETURN THIS-OBJECT:StartChildTab(icProcName,icWinTitle,hParent,NO,NO). 
  END METHOD.    

  METHOD PUBLIC HANDLE StartChildTab(icProcName   AS CHAR):
          
    DEF VAR hParent AS HANDLE NO-UNDO.

    IF PROVERSION < "11" THEN hParent = DYNAMIC-FUNCTION ("getProcedureHandle",PROGRAM-NAME(2)).
    ELSE hParent = SOURCE-PROCEDURE.
   
    RETURN THIS-OBJECT:StartChildTab(icProcName,"",hParent,NO,NO). 
  END METHOD.    
	
	METHOD PUBLIC LOGICAL DeleteMenu ():
	  DELETE PROCEDURE hMenu NO-ERROR.
	  IF VALID-HANDLE(MainMenuHandle) THEN
	    DELETE PROCEDURE MainMenuHandle NO-ERROR.
	    
	  MainMenuHandle = ?.  
	  
    RUN JBoxRibbonMenu.p PERSIST SET hMenu.    
	END.  

    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    METHOD PUBLIC LOGICAL ViewStartPanel(INPUT icPanelURL AS CHAR ):
        
      DYNAMIC-FUNCTION("setStartPanelURL" IN hMenu,icPanelURL).
      
      RUN ViewStartPanel IN hMenu.
        
      RETURN YES.

    END METHOD.
	
	DESTRUCTOR PUBLIC JBoxMainMenu ( ):

	END DESTRUCTOR.
	
	
END CLASS.

