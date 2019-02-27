/*USING System.Windows.Forms.SendKeys.*/
USING Progress.Lang.*.
USING Progress.Windows.UserControl.
USING Progress.Windows.Form.
USING uc.* FROM PROPATH.

ROUTINE-LEVEL ON ERROR UNDO, THROW.

CLASS uc.JBoxUltraTabFolder INHERITS uc.JBoxUCbase: 
	
  DEFINE PRIVATE VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.
  DEFINE PUBLIC VARIABLE giInitNumFolders    AS INTEGER INIT 7 NO-UNDO. 
  DEFINE PUBLIC VARIABLE SelectedTabIndexHist AS INT EXTENT 6 NO-UNDO. 

  DEFINE PUBLIC VARIABLE  SelectedTabIndex AS INT NO-UNDO. 
  DEFINE PRIVATE VARIABLE ultraTabPageControl AS Infragistics.Win.UltraWinTabControl.UltraTabPageControl EXTENT  NO-UNDO.
  DEFINE PRIVATE VARIABLE ultraTab AS Infragistics.Win.UltraWinTabControl.UltraTab EXTENT NO-UNDO.
  DEFINE PRIVATE VARIABLE ultraTabControl1 AS Infragistics.Win.UltraWinTabControl.UltraTabControl NO-UNDO.
  DEFINE PRIVATE VARIABLE ultraTabSharedControlsPage1 AS Infragistics.Win.UltraWinTabControl.UltraTabSharedControlsPage NO-UNDO.
  
  DEFINE PUBLIC VARIABLE hCall AS HANDLE NO-UNDO.   
  DEFINE VARIABLE  bDisableTextChanged    AS LOG NO-UNDO INIT YES.

  CONSTRUCTOR PUBLIC JBoxUltraTabFolder (INPUT ihWinSuper    AS HANDLE, /* Window super proc - unknown if object created from window */
                                        INPUT ihWinProc     AS HANDLE,
                                        INPUT ihPlaceHolder AS HANDLE, /* Design widget (rectangle, field, etc) */
                                        INPUT iiDeltaWidth  AS INTEGER):
    SUPER(ihWinSuper,ihWinProc,ihPlaceHolder,iiDeltaWidth).
/*    THIS-OBJECT:ComponentsCollection:ADD(THIS-OBJECT:components).*/
  END CONSTRUCTOR.
  CONSTRUCTOR PUBLIC JBoxUltraTabFolder ( INPUT ihWinProc     AS HANDLE,
                                          INPUT ihPlaceHolder AS HANDLE ): /* Design widget (rectangle, field, etc) */
                                   
    SUPER(?,ihWinProc,ihPlaceHolder,0).
  END CONSTRUCTOR.

	METHOD PUBLIC OVERRIDE VOID InitializeComponent(  ):

    DEFINE VARIABLE iFolderCnt AS INT NO-UNDO. 

    IF CAN-DO(hWinProc:INTERNAL-ENTRIES,"getNumFolders") THEN 
      giInitNumFolders = DYNAMIC-FUNCTION("getNumFolders" IN hWinProc).

    ASSIGN 
        EXTENT(ultraTabPageControl) = giInitNumFolders
        EXTENT(ultraTab)            = giInitNumFolders.

    DEFINE VARIABLE appearance2 AS Infragistics.Win.Appearance NO-UNDO.
    appearance2 = NEW Infragistics.Win.Appearance().
    
    DO iFolderCnt = 1 TO EXTENT(ultraTabPageControl):
        ASSIGN 
        ultraTab[iFolderCnt]                        = NEW Infragistics.Win.UltraWinTabControl.UltraTab()
        THIS-OBJECT:ultraTabPageControl[iFolderCnt] = NEW Infragistics.Win.UltraWinTabControl.UltraTabPageControl().
    END.
        
    ASSIGN 
        THIS-OBJECT:ultraTabControl1            = NEW Infragistics.Win.UltraWinTabControl.UltraTabControl()
        THIS-OBJECT:ultraTabSharedControlsPage1 = NEW Infragistics.Win.UltraWinTabControl.UltraTabSharedControlsPage().
    
    
    /* Folder properties */ 
    appearance2:FontData:BoldAsString = "True".
    appearance2:FontData:SizeInPoints = 20.
    THIS-OBJECT:ultraTabControl1:Appearance = appearance2.
    THIS-OBJECT:ultraTabControl1:Dock = System.Windows.Forms.DockStyle:FILL.

    CAST(THIS-OBJECT:ultraTabControl1, System.ComponentModel.ISupportInitialize):BeginInit().
    THIS-OBJECT:ultraTabControl1:SuspendLayout().
    THIS-OBJECT:SuspendLayout().
    THIS-OBJECT:Controls:Add(THIS-OBJECT:ultraTabControl1).

    THIS-OBJECT:ultraTabControl1:Controls:Add(THIS-OBJECT:ultraTabSharedControlsPage1).
    /* - curt  
    THIS-OBJECT:ultraTabControl1:UseOsThemes = Infragistics.Win.DefaultableBoolean:False. 
    */
    DO iFolderCnt = 1 TO EXTENT(ultraTabPageControl):
        ASSIGN 
        THIS-OBJECT:ultraTabPageControl[iFolderCnt]:Location = NEW System.Drawing.Point(-10000, -10000)
        THIS-OBJECT:ultraTabPageControl[iFolderCnt]:Name = "ultraTabPageControl2"
        THIS-OBJECT:ultraTabPageControl[iFolderCnt]:Size = NEW System.Drawing.Size(355, 167).
        THIS-OBJECT:ultraTabControl1:Controls:Add(THIS-OBJECT:ultraTabPageControl[iFolderCnt]).
        ASSIGN 
        ultraTab[iFolderCnt]:TabPage = THIS-OBJECT:ultraTabPageControl[iFolderCnt]
        ultraTab[iFolderCnt]:Text = STRING(iFolderCnt)
        ultraTab[iFolderCnt]:VISIBLE = FALSE.
    END.

    ASSIGN 
                                                               /*
                                                               
    appearance1:AlphaLevel = System.Convert:ToInt16(141).
    appearance1:BackColor = System.Drawing.Color:Transparent.*/
/*    
          THIS-OBJECT:ultraTabControl1:TabHeaderAreaAppearance:AlphaLevel = System.Convert:ToInt16(141)
          THIS-OBJECT:ultraTabControl1:TabHeaderAreaAppearance:BackColor = System.Drawing.Color:TRANSPARENT

*/
    THIS-OBJECT:ultraTabControl1:Location = NEW System.Drawing.Point(0, 0)
    THIS-OBJECT:ultraTabControl1:Name = "ultraTabControl1"
    THIS-OBJECT:ultraTabControl1:SharedControlsPage = THIS-OBJECT:ultraTabSharedControlsPage1
    THIS-OBJECT:ultraTabControl1:Size = NEW System.Drawing.Size(620, 334)
    THIS-OBJECT:ultraTabControl1:TabIndex = 2

/*        THIS-OBJECT:ultraTabControl1:TabOrientation = Infragistics.Win.UltraWinTabs.TabOrientation:LeftTop. */
    THIS-OBJECT:ultraTabControl1:TabOrientation = Infragistics.Win.UltraWinTabs.TabOrientation:TopLeft.
    THIS-OBJECT:ultraTabControl1:Tabs:AddRange(ultraTab).
    THIS-OBJECT:ultraTabControl1:SelectedTabChanged:Subscribe(THIS-OBJECT:SelectedTabChanged).
    THIS-OBJECT:ultraTabControl1:MouseClick:Subscribe(THIS-OBJECT:mouse_click_event). 
    THIS-OBJECT:ultraTabControl1:MouseDoubleClick:Subscribe(THIS-OBJECT:mouse_click_event).
    THIS-OBJECT:ultraTabControl1:ActiveTabChanging:Subscribe(THIS-OBJECT:Before_SelectedTabChanged).

    /* ultraTabSharedControlsPage1 */
    ASSIGN 
    THIS-OBJECT:ultraTabSharedControlsPage1:Location = NEW System.Drawing.Point(-10000, -10000)
    THIS-OBJECT:ultraTabSharedControlsPage1:Name = "ultraTabSharedControlsPage1"
    THIS-OBJECT:ultraTabSharedControlsPage1:Size = NEW System.Drawing.Size(294, 330).
    
    ASSIGN 
/*        THIS-OBJECT:ClientSize =  NEW System.Drawing.Size(320, 334)*/
    THIS-OBJECT:Name = "Menu"
    THIS-OBJECT:Text = "Menu"
/*        THIS-OBJECT:TopLevel = FALSE*/
    /*
    THIS-OBJECT:TopMost  = TRUE. 
    */
/*        THIS-OBJECT:TopMost  = FALSE.*/

    /*
    THIS-OBJECT:ultraTabControl1:SelectedTab = THIS-OBJECT:ultraTabControl1:Tabs[1].
    THIS-OBJECT:ultraTabControl1:Tabs[2]:Selected = true.
    */


    /*            ultraTabControl1:SelectedTab = THIS-OBJECT:ultraTabControl1:Tabs[3].

          ultraTabControl1:Tabs[2]:Selected = true.
*/
            /*
            ultraTabControl1.Tabs(2).Selected = true
              */
    .


    THIS-OBJECT:ultraTabPageControl[1]:ResumeLayout(FALSE).
         CAST(THIS-OBJECT:ultraTabControl1, System.ComponentModel.ISupportInitialize):EndInit().
    THIS-OBJECT:ultraTabControl1:ResumeLayout(TRUE).
    THIS-OBJECT:ResumeLayout(TRUE).

    CATCH e AS Progress.Lang.Error:
        UNDO, THROW e.
    END CATCH.
  END METHOD.
 
  /* Mandatory methods: */

  METHOD PUBLIC Infragistics.Win.UltraWinTabControl.UltraTabControl getDotNetWidget():
    RETURN ultraTabControl1.
  END METHOD.

  METHOD PUBLIC CHARACTER getDotNetWidgetType():
    RETURN STRING(ultraTabControl1:GetType()).
  END METHOD.


  /* Methods invoked from jukebox (via window super): */

  /* (Semi-)class specific methods - event handlers: */

  /* Class specific methods: */

    METHOD PRIVATE VOID before_SelectedTabChanged ( INPUT sender AS System.Object, INPUT e AS Infragistics.Win.UltraWinTabControl.ActiveTabChangingEventArgs ):
        SelectedTabIndex = THIS-OBJECT:ultraTabControl1:SelectedTab:INDEX  + 1.  
        ASSIGN 
            SelectedTabIndexHist[6] = SelectedTabIndexHist[5].
            SelectedTabIndexHist[5] = SelectedTabIndexHist[4].
            SelectedTabIndexHist[4] = SelectedTabIndexHist[3].
            SelectedTabIndexHist[3] = SelectedTabIndexHist[2].
            SelectedTabIndexHist[2] = SelectedTabIndexHist[1].
            SelectedTabIndexHist[1] = SelectedTabIndex.
        RETURN.
    END METHOD.


    METHOD PUBLIC INTEGER SelectedTabMenuIndex ():
        RETURN THIS-OBJECT:ultraTabControl1:SelectedTab:INDEX  + 1.
    END METHOD.


    METHOD PRIVATE VOID SelectedTabChanged( INPUT sender AS System.Object, INPUT e AS Infragistics.Win.UltraWinTabControl.SelectedTabChangedEventArgs ):
        DEFINE VARIABLE cChar AS CHAR INIT "~{ENTER~}" NO-UNDO. 
        
        IF VALID-HANDLE(hCall) THEN RUN ApplyFrameFocus IN hCall NO-ERROR.
        /*
        DYNAMIC-FUNCTION ( '__clearFocusField') NO-ERROR.
       
        IF NOT PROGRAM-NAME(2) BEGINS "setActiveTab" AND           
            THIS-OBJECT:ultraTabControl1:SelectedTab:INDEX NE 2 AND  
            SelectedTabIndex NE 3 
              THEN sendKeys:Send(cChar).
        
        /* Navigation from OptMenu to other menu */ 
        IF  NOT PROGRAM-NAME(2) BEGINS "setActiveTab"  AND
            SelectedTabIndexHist[1] = 3 AND 
            SelectedTabIndexHist[2] NE 
            (THIS-OBJECT:ultraTabControl1:SelectedTab:INDEX + 1) 
               THEN sendKeys:Send(cChar).
        */       
  END METHOD.


    METHOD PRIVATE VOID mouse_click_event( INPUT sender AS System.Object,INPUT e AS System.Windows.Forms.MouseEventArgs):
        IF VALID-HANDLE(hCall) THEN RUN ApplyFrameFocus IN hCall NO-ERROR.
  END METHOD.


    METHOD PUBLIC LOGICAL SwapHideView () :
          THIS-OBJECT:VISIBLE = NOT THIS-OBJECT:VISIBLE. 
          IF THIS-OBJECT:VISIBLE THEN
             THIS-OBJECT:bringToFront().
          RETURN THIS-OBJECT:VISIBLE.
    END METHOD.


    METHOD PUBLIC LOGICAL ACTIVE ( INPUT lActive AS LOG) :
          THIS-OBJECT:VISIBLE = lActive. 
          /*
          IF THIS-OBJECT:VISIBLE THEN
             THIS-OBJECT:bringToFront().topmost
          */
          RETURN THIS-OBJECT:VISIBLE.
    END METHOD.


    METHOD PUBLIC LOGICAL AddObjectToPage (INPUT ipOTabObjectForm AS OBJECT,
                                           INPUT ipiPage AS INTEGER,
                                           INPUT ipcTabLabel AS CHAR ):

        DEFINE VARIABLE TabObjectForm AS FORM NO-UNDO.
        TabObjectForm = CAST(ipOTabObjectForm,form).
        TabObjectForm:IsMdiContainer = YES. /* Brynjar 26.09.14: To be able to add 4gl windows as tabs */

        THIS-OBJECT:ultraTabPageControl[ipiPage]:Controls:Add(TabObjectForm).
        ASSIGN 
            TabObjectForm:Dock = System.Windows.Forms.DockStyle:FILL
            TabObjectForm:Location = NEW System.Drawing.Point(0, 0)
            TabObjectForm:TabIndex = ipiPage
            ultraTab[ipiPage]:VISIBLE = TRUE
            ultraTab[ipiPage]:TEXT = ipcTabLabel.
        TabObjectForm:bringtofront().
    END. 
    
    METHOD PUBLIC HANDLE AddProcToPage (INPUT icFileName AS CHAR,
                                         INPUT iiPage AS INT,
                                         INPUT icLabel AS CHAR):

       DEF VAR oUC AS JBoxWrapperUC NO-UNDO.
/*       DEF VAR oWindowContainer AS Progress.Windows.WindowContainer NO-UNDO.*/
       DEF VAR hProc      AS HANDLE NO-UNDO.
       DEF VAR hWinSuper  AS HANDLE NO-UNDO.
        
        hWinSuper = DYNAMIC-FUNCTION ("getWindowSuper" IN SOURCE-PROCEDURE).
                       
       ASSIGN icFileName = "template\JBoxSuppressedWindow.w"
              iiPage = 1
              icLabel = "Customer"
              .
              
       RUN VALUE(icFileName) PERSIST SET hProc.       
              
      oUC = NEW JBoxWrapperUC(hProc,hWinSuper).
      
      THIS-OBJECT:AddObjectToPage(CAST(oForm,OBJECT),iiPage,icLabel).
/*    
      ASSIGN oWindowContainer = oForm:WindowContainer
             oWindowContainer:EmbeddedWindow = hProc:CURRENT-WINDOW
             oForm:Size = NEW System.Drawing.Size(hProc:CURRENT-WINDOW:WIDTH-PIXELS + 17,hProc:CURRENT-WINDOW:HEIGHT-PIXELS + 36)
             oWindowContainer:Size = NEW System.Drawing.Size(hProc:CURRENT-WINDOW:WIDTH-PIXELS,hProc:CURRENT-WINDOW:HEIGHT-PIXELS)    
             .
  */            
                                                      
    END METHOD.                                             

    METHOD PUBLIC LOGICAL SetActiveTab (INPUT ipiIndex AS INTEGER ) :
           THIS-OBJECT:ultraTabControl1:Tabs[ipiIndex - 1]:Selected = TRUE .
    END METHOD.

    METHOD PUBLIC LOGICAL DisableTab (INPUT ipiIndex AS INTEGER ) :
           THIS-OBJECT:ultraTabControl1:Tabs[ipiIndex - 1]:ENABLED = FALSE.
    END METHOD.

    METHOD PUBLIC LOGICAL HideTab (INPUT ipiIndex AS INTEGER ) :
       THIS-OBJECT:ultraTabControl1:Tabs[ipiIndex - 1]:VISIBLE = FALSE.
    END METHOD.

    METHOD PUBLIC LOGICAL ViewTab (INPUT ipiIndex AS INTEGER ) :
       THIS-OBJECT:ultraTabControl1:Tabs[ipiIndex - 1]:VISIBLE = TRUE.
    END METHOD.

    METHOD PUBLIC LOGICAL EnableTab (INPUT ipiIndex AS INTEGER ) :
       THIS-OBJECT:ultraTabControl1:Tabs[ipiIndex - 1]:ENABLED = TRUE.
    END METHOD.


    METHOD PUBLIC LOGICAL MoveToPosition () :
           THIS-OBJECT:Location = NEW System.Drawing.Point(100, 100).
    END METHOD.


    METHOD PUBLIC LOGICAL MoveToPosition (INPUT iXpos AS INT,INPUT iYpos AS INT) :
           THIS-OBJECT:Location = NEW System.Drawing.Point(iXpos,iYpos). 
    END METHOD.

    METHOD PUBLIC LOGICAL SetSize (INPUT iWidth AS INT,INPUT iHeight AS INT) :
       
       THIS-OBJECT:ultraTabControl1:Size = NEW System.Drawing.Size(iWidth, iHeight).
       THIS-OBJECT:ClientSize =  NEW System.Drawing.Size(iWidth , iHeight ).

    END METHOD.

	DESTRUCTOR PUBLIC JBoxUltraTabFolder ( ):

		IF VALID-OBJECT(components) THEN DO:
			CAST(components, System.IDisposable):Dispose().
		END.

	END DESTRUCTOR.
	
END CLASS.
