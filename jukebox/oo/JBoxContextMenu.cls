USING System.Windows.Controls.*.
USING Progress.Lang.*.
USING Progress.Windows.UserControl.

ROUTINE-LEVEL ON ERROR UNDO, THROW.

CLASS JBoxContextMenu INHERITS UserControl: 
	
	DEFINE PRIVATE VARIABLE components         AS System.ComponentModel.IContainer NO-UNDO.
  DEFINE PUBLIC  PROPERTY contextMenu1       AS System.Windows.Forms.ContextMenu GET. SET.
  DEFINE PUBLIC  VARIABLE hWinSuper  AS HANDLE NO-UNDO.
  DEFINE PUBLIC  VARIABLE hWinProc           AS HANDLE NO-UNDO.
  DEFINE PUBLIC  VARIABLE hWindow            AS HANDLE NO-UNDO.
  DEFINE PUBLIC  VARIABLE hPlaceHolder       AS HANDLE NO-UNDO.
  DEFINE PUBLIC  VARIABLE cMethodSuffix      AS CHAR   NO-UNDO.
  DEFINE PUBLIC  VARIABLE cEnabledList       AS CHAR   NO-UNDO.
  DEFINE PUBLIC  VARIABLE iMenuIdx           AS INT    NO-UNDO.
		
  DEF TEMP-TABLE ttMenu
      FIELD iMenuIdx      AS INT
      FIELD cAction       AS CHAR
      FIELD cLabel        AS CHAR
      FIELD cMethod       AS CHAR
      FIELD cSubType      AS CHAR  /* menu-item, sub-menu, rule, toggle */
      FIELD cParent       AS CHAR
      .

	CONSTRUCTOR PUBLIC  JBoxContextMenu ( INPUT ihWinSuper     AS HANDLE,    /* If source proc is the object super proc */
                                        INPUT ihWinProc      AS HANDLE,
                                        INPUT ihPlaceHolder  AS HANDLE,    /* Placeholder for parent UC */
                                        INPUT icMethodSuffix AS CHAR ): 
	 
    SUPER().
    ASSIGN hWinSuper  = ihWinSuper
           hWinProc           = ihWinProc
           hWindow            = hWinProc:CURRENT-WINDOW           
           hPlaceHolder       = ihPlaceHolder
           cMethodSuffix      = icMethodSuffix
           . 
    InitializeComponent().
        
    CATCH e AS Progress.Lang.Error:
       UNDO, THROW e.
    END CATCH.

  END CONSTRUCTOR.

	METHOD PUBLIC VOID InitializeComponent():
    THIS-OBJECT:contextMenu1 = NEW System.Windows.Forms.ContextMenu().
    THIS-OBJECT:SuspendLayout().

    THIS-OBJECT:Name = "JBoxContextMenu".
    THIS-OBJECT:ResumeLayout(FALSE).
    THIS-OBJECT:PerformLayout().
    CATCH e AS Progress.Lang.Error:
        UNDO, THROW e.
    END CATCH.
  END METHOD.
 
  METHOD PUBLIC VOID NewDotNetMenuBand (ioParent AS Progress.Lang.Object,icActionList AS CHAR):
    DEF VAR ix                  AS INT  NO-UNDO.
    DEF VAR cActionDef          AS CHAR NO-UNDO.
    DEF VAR cAction             AS CHAR NO-UNDO.
    DEF VAR cMethod             AS CHAR NO-UNDO.
    DEF VAR cLabel              AS CHAR NO-UNDO.
    DEF VAR bToggle             AS LOG  NO-UNDO.
    DEF VAR bTglChecked         AS LOG  NO-UNDO.
    DEF VAR oMenuItem           AS System.Windows.Forms.MenuItem NO-UNDO.
    DEF VAR bFirstInGroup       AS LOG NO-UNDO.
    DEF VAR parentMenuItem      AS System.Windows.Forms.MenuItem  NO-UNDO. 
    DEF VAR bEvent              AS LOG  NO-UNDO.

    IF ioParent NE ? THEN
      parentMenuItem = CAST(ioParent,"System.Windows.Forms.MenuItem").


    IF icActionList NE "" THEN DO ix = 1 TO NUM-ENTRIES(icActionList):
      ASSIGN cActionDef  = ENTRY(1,ENTRY(ix,icActionList),"¤")
             bToggle     = NO
             bTglChecked = NO
             bEvent      = NO.
             .


      IF NUM-ENTRIES(cActionDef,";") = 1 THEN 
        ASSIGN cAction      = ENTRY(1,cActionDef,"|")
               cMethod      = cAction + cMethodSuffix
               cLabel       = CAPS(SUBSTR(cAction,1,1)) + SUBSTR(cAction,2).
      ELSE IF NUM-ENTRIES(cActionDef,";") = 2 THEN 
        ASSIGN cAction  = ENTRY(1,ENTRY(1,cActionDef,";"),"|")
               cLabel   = ENTRY(2,cActionDef,";")
               cMethod  = cAction + cMethodSuffix.
      ELSE IF NUM-ENTRIES(cActionDef,";") = 3 THEN 
        ASSIGN cAction  = ENTRY(1,ENTRY(1,cActionDef,";"),"|")
               cLabel   = ENTRY(2,cActionDef,";")
               cMethod  = IF ENTRY(3,cActionDef,";") NE "" AND ENTRY(3,cActionDef,";") NE "toggle" THEN ENTRY(3,cActionDef,";") ELSE cAction + cMethodSuffix
               bToggle  = ENTRY(3,cActionDef,";") = "toggle"
               .
      ELSE IF NUM-ENTRIES(cActionDef,";") = 4 THEN 
        ASSIGN cAction  = ENTRY(1,ENTRY(1,cActionDef,";"),"|")
               cLabel   = ENTRY(2,cActionDef,";")
               cMethod  = IF ENTRY(3,cActionDef,";") NE "" THEN ENTRY(3,cActionDef,";") ELSE cAction + cMethodSuffix
               bToggle  = ENTRY(4,cActionDef,";") = "toggle"
               .
      ELSE IF NUM-ENTRIES(cActionDef,";") = 5 THEN 
        ASSIGN cAction     = ENTRY(1,ENTRY(1,cActionDef,";"),"|")
               cLabel      = ENTRY(2,cActionDef,";")
               cMethod     = IF ENTRY(3,cActionDef,";") NE "" THEN ENTRY(3,cActionDef,";") ELSE cAction + cMethodSuffix
               bToggle     = ENTRY(4,cActionDef,";") = "toggle"
               bTglChecked = LOGICAL(ENTRY(5,cActionDef,";"))
               .
      ELSE DO:
        MESSAGE "Error in menu-band definition for element " SKIP
                cActionDef SKIP
                "Menu-bands are defined as a comma-separated list like this: " SKIP(1)
                "[<Action>] | [|<Label for placeholder] [;<Label>;<Method>],[<Action>] | [|<Label for placeholder] [;<Label>;<Method>].." SKIP(1)
                "If label for placeholder equals - (dash) it's considered a rule"
                VIEW-AS ALERT-BOX ERROR.
      END.

      IF NUM-ENTRIES(ENTRY(ix,icActionList),"¤") > 1 THEN DO: 
        IF ENTRY(2,ENTRY(ix,icActionList),"¤") MATCHES "*enable*" THEN
          cEnabledList = cEnabledList + "," + cAction.
      END.

      IF CAN-DO("rule,-",cAction) THEN 
        ASSIGN cAction = "-"
               cLabel  = "-".
      ELSE IF NUM-ENTRIES(ENTRY(1,cActionDef,";"),"|") > 1 THEN
          ASSIGN cAction      = ENTRY(2,ENTRY(1,cActionDef,";"),"|")
                 cLabel       = cAction.
      ELSE bEvent = YES.

      CREATE ttMenu.
      ASSIGN ttMenu.cAction  = cAction
             ttMenu.cLabel   = cLabel
             ttMenu.cMethod  = cMethod
             ttMenu.iMenuIdx = iMenuIdx
             .

      oMenuItem = NEW System.Windows.Forms.MenuItem(cLabel).

      IF bEvent THEN DO:
        oMenuItem:Click:SUBSCRIBE(THIS-OBJECT:MenuClick).
        ttMenu.cSubType = "menu-item".
      END.

      IF bToggle THEN DO:
        ttMenu.cSubType = "toggle".
        oMenuItem:RadioCheck = YES.
        IF bTglChecked THEN
          oMenuItem:Checked = YES.
      END. 
      ELSE IF cAction = "-" THEN
        cSubType = "rule".
      ELSE IF NUM-ENTRIES(ENTRY(1,cActionDef,";"),"|") > 1 THEN
        cSubType = "sub-menu".

      IF ioParent NE ? THEN DO:
        parentMenuItem:MenuItems:Add(oMenuItem).
        ttMenu.cParent = parentMenuItem:Tag.
      END.
      ELSE 
        contextMenu1:MenuItems:Add(oMenuItem).

      oMenuItem:Tag = cMethod.


      iMenuIdx = iMenuIdx + 1.
    END.

  END METHOD.


  METHOD PRIVATE VOID MenuClick( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
    DEFINE VARIABLE oMenuItem AS System.Windows.Forms.MenuItem NO-UNDO.
    
    DEFINE VARIABLE cMethod AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iNumMenu AS INTEGER   NO-UNDO.
    DEFINE VARIABLE cForm    AS CHARACTER NO-UNDO.

    ASSIGN oMenuItem = CAST(sender, "System.Windows.Forms.MenuItem")
           cMethod   = oMenuItem:Tag
           .

    FIND FIRST ttMenu 
         WHERE ttMenu.cMethod = cMethod
           AND ttMenu.cLabel  = oMenuItem:Text
         NO-ERROR.

    IF AVAIL ttMenu THEN
      RUN InvokeMethod IN hWinSuper (hPlaceHolder,cMethod).

    RETURN.
  END METHOD.

  METHOD PUBLIC System.Windows.Forms.MenuItem getMenuItem(INPUT icAction AS CHAR):
    FIND FIRST ttMenu WHERE ttMenu.cAction = icAction NO-ERROR.
    IF AVAIL ttMenu THEN
      RETURN contextMenu1:MenuItems[ttMenu.iMenuIdx].

  END METHOD.

  METHOD PUBLIC System.Windows.Forms.MenuItem getMenuItem(INPUT icParent AS CHAR, INPUT icAction AS CHAR):
    FIND FIRST ttMenu 
         WHERE ttMenu.cAction = icAction 
           AND ttMenu.cParent = icParent
         NO-ERROR.
    IF AVAIL ttMenu THEN
      RETURN contextMenu1:MenuItems[ttMenu.iMenuIdx].
  END METHOD.

  METHOD PUBLIC System.Windows.Forms.MenuItem getSelectedMenuItem():
    IF AVAIL ttMenu THEN
      RETURN contextMenu1:MenuItems[ttMenu.iMenuIdx].
  END METHOD.

  METHOD PUBLIC CHARACTER getSelectedMenuLabel():
    IF AVAIL ttMenu THEN
      RETURN ttMenu.cLabel.
  END METHOD.

	DESTRUCTOR PUBLIC JBoxContextMenu ( ):

		IF VALID-OBJECT(components) THEN DO:
			CAST(components, System.IDisposable):Dispose().
		END.

	END DESTRUCTOR.
END CLASS.
