USING Progress.Lang.*.
USING Progress.Util.*.
USING Progress.Windows.Form.

ROUTINE-LEVEL ON ERROR UNDO, THROW.

CLASS JBoxWrapperForm INHERITS Form: 
	
	DEFINE PRIVATE VARIABLE components      AS System.ComponentModel.IContainer NO-UNDO.
  DEFINE PUBLIC VARIABLE windowContainer  AS Progress.Windows.WindowContainer NO-UNDO.
  DEFINE PUBLIC VARIABLE menuStrip1       AS System.Windows.Forms.MenuStrip NO-UNDO.
  DEFINE PUBLIC PROPERTY bSkipResize      AS LOGICAL NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY iDeltaYStatusBar AS INTEGER NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY iDeltaYWinMenu   AS INTEGER NO-UNDO GET. SET.

  DEFINE PUBLIC VARIABLE hProc         AS HANDLE NO-UNDO.
  DEFINE PUBLIC VARIABLE hSuperProc    AS HANDLE NO-UNDO.
		

	CONSTRUCTOR PUBLIC JBoxWrapperForm ( INPUT ihProc      AS HANDLE,
                                       INPUT ihSuperProc AS HANDLE ):
	 
    SUPER().

    ASSIGN hProc      = ihProc
           hSuperProc = ihSuperProc
           .

    InitializeComponent().
        
    CATCH e AS Progress.Lang.Error:
       UNDO, THROW e.
    END CATCH.

  END CONSTRUCTOR.


	METHOD PRIVATE VOID InitializeComponent(  ):
/*     THIS-OBJECT:KeyPreview = YES. */

    THIS-OBJECT:IsMdiContainer = YES.

    THIS-OBJECT:windowContainer = NEW Progress.Windows.WindowContainer().
    THIS-OBJECT:Controls:Add(THIS-OBJECT:windowContainer).
    THIS-OBJECT:FormClosing:Subscribe(THIS-OBJECT:OnFormClosing).
    THIS-OBJECT:Resize:Subscribe(THIS-OBJECT:handler_Resize).

/*     THIS-OBJECT:PreviewKeyDown:Subscribe(THIS-OBJECT:handler_PreviewKeyDown). */
  END METHOD.


	DESTRUCTOR PUBLIC JBoxWrapperForm ( ):

		IF VALID-OBJECT(components) THEN DO:
			CAST(components, System.IDisposable):Dispose().
		END.

	END DESTRUCTOR.
 
/*   METHOD PUBLIC VOID OnFormClosed( INPUT sender AS System.Object, INPUT e AS System.Windows.Forms.FormClosedEventArgs ): */
  METHOD PUBLIC VOID OnFormClosing( INPUT sender AS System.Object, INPUT e AS System.Windows.Forms.FormClosingEventArgs ):  
     APPLY "close" to hProc.
    IF RETURN-VALUE = "cancel" THEN e:Cancel = YES.
  END METHOD.


  METHOD PRIVATE VOID handler_Resize (sender AS CLASS System.Object, e AS CLASS System.EventArgs):
    IF NOT bSkipResize THEN
      ASSIGN windowContainer:Width  = THIS-OBJECT:Width - 17
             windowContainer:Height = THIS-OBJECT:Height - 36 - iDeltaYWinMenu 
             .     
     bSkipResize = NO.
  END METHOD. 

  /*
  METHOD PRIVATE VOID handler_PreviewKeyDown (sender AS CLASS System.Object, e AS CLASS System.Windows.Forms.PreviewKeyDownEventArgs):
    DEF VAR hNextTabItem AS HANDLE NO-UNDO.

    MESSAGE e:keycode
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
/*     cPreviewKey = STRING(e:KEYCODE).                      */
/*                                                           */
/*     IF e:Shift THEN DO:                                   */
/*       hNextTabItem = hPlaceHolder:PREV-TAB-ITEM NO-ERROR. */
/*       IF VALID-HANDLE(hNextTabItem) THEN                  */
/*         RUN widgetFocus IN hWinSuper (hNextTabItem).      */
/*     END.                                                  */

  END METHOD.
  */

  METHOD PUBLIC Progress.Lang.Object AddSubMenu
        (ioParent       AS Progress.Lang.Object,
         iiId           AS INT, 
         icText         AS CHAR,
         icImage        AS CHAR,
         icAccel        AS CHAR,
         ibFirstInGroup AS LOG):

    DEFINE VARIABLE newMenuItem         AS System.Windows.Forms.ToolStripMenuItem  NO-UNDO. 
    DEFINE VARIABLE parentMenuItem      AS System.Windows.Forms.ToolStripMenuItem  NO-UNDO. 
    DEFINE VARIABLE toolStripSeparator1 AS System.Windows.Forms.ToolStripSeparator NO-UNDO.
    
    IF ioParent NE ? THEN
      parentMenuItem = CAST(ioParent,"System.Windows.Forms.ToolStripMenuItem").

    IF ibFirstInGroup THEN DO:
      toolStripSeparator1 = NEW System.Windows.Forms.ToolStripSeparator().
      parentMenuItem:DropDownItems:Add(toolStripSeparator1).
    END.

    newMenuItem = NEW System.Windows.Forms.ToolStripMenuItem(icText).
    newMenuItem:tag = STRING(iiId).
    newMenuItem:Click:SUBSCRIBE(THIS-OBJECT:MenuClick).
    IF ioParent NE ? THEN
      parentMenuItem:DropDownItems:Add(newMenuItem).
    ELSE
      menuStrip1:Items:Add(newMenuItem).

    RETURN CAST(newMenuItem,"Progress.Lang.Object").

  END METHOD.

  METHOD PUBLIC Progress.Lang.Object AddMenuItem
        (ioParent       AS Progress.Lang.Object,
         iiId           AS INT, 
         icText         AS CHAR,
         icImage        AS CHAR,
         icAccel        AS CHAR,
         ibFirstInGroup AS LOG):

    DEFINE VARIABLE parentMenuItem      AS System.Windows.Forms.ToolStripMenuItem  NO-UNDO. 
    DEFINE VARIABLE newMenuItem         AS System.Windows.Forms.ToolStripMenuItem  NO-UNDO. 
    DEFINE VARIABLE toolStripSeparator1 AS System.Windows.Forms.ToolStripSeparator NO-UNDO.

    parentMenuItem = CAST(ioParent,"System.Windows.Forms.ToolStripMenuItem").

    IF ibFirstInGroup THEN DO:
      toolStripSeparator1 = NEW System.Windows.Forms.ToolStripSeparator().
      parentMenuItem:DropDownItems:Add(toolStripSeparator1).
    END.

    newMenuItem = NEW System.Windows.Forms.ToolStripMenuItem(icText).
    newMenuItem:Tag = STRING(iiId).
    IF icAccel NE "" THEN THIS-OBJECT:setShortcut(newMenuItem,icAccel).
    IF icImage NE "" AND SEARCH(icImage) NE ? THEN
       newMenuItem:Image = System.Drawing.Image:FromFile(SEARCH(icImage)).
    newMenuItem:Click:SUBSCRIBE(THIS-OBJECT:MenuClick).
    parentMenuItem:DropDownItems:Add(newMenuItem).

    RETURN CAST(newMenuItem,"Progress.Lang.Object").

  END METHOD.

  METHOD PRIVATE VOID MenuClick( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
    DEFINE VARIABLE oMenuItem AS System.Windows.Forms.ToolStripMenuItem NO-UNDO.
    
    DEFINE VARIABLE cTagItem AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iNumMenu AS INTEGER   NO-UNDO.
    DEFINE VARIABLE cForm    AS CHARACTER NO-UNDO.

    ASSIGN oMenuItem = CAST(sender,System.Windows.Forms.ToolStripMenuItem)
           cTagItem = oMenuItem:Tag
           .

    RUN ApplyWinMenu IN hProc (cTagItem).

    RETURN.
  END METHOD.


  METHOD PUBLIC LOGICAL setShortcut (ioMenuItem AS System.Windows.Forms.ToolStripMenuItem,icAccel AS CHAR):        
    DEFINE VARIABLE oEnum               AS System.Enum               NO-UNDO.
    DEFINE VARIABLE oKey1               AS System.Windows.Forms.Keys NO-UNDO.
    DEFINE VARIABLE oKey2               AS System.Windows.Forms.Keys NO-UNDO.

    IF NUM-ENTRIES(icAccel,"-") NE 2 THEN RETURN NO.

    IF icAccel BEGINS "alt" THEN
      oKey1 = System.Windows.Forms.Keys:Alt.
    ELSE
      oKey1 = System.Windows.Forms.Keys:Control.
    
    CASE SUBSTR(icAccel,LENGTH(icAccel)):
      WHEN "A" THEN oKey2 = System.Windows.Forms.Keys:A.
      WHEN "B" THEN oKey2 = System.Windows.Forms.Keys:B.
      WHEN "C" THEN oKey2 = System.Windows.Forms.Keys:C.
      WHEN "D" THEN oKey2 = System.Windows.Forms.Keys:D.
      WHEN "E" THEN oKey2 = System.Windows.Forms.Keys:E.
      WHEN "F" THEN oKey2 = System.Windows.Forms.Keys:F.
      WHEN "G" THEN oKey2 = System.Windows.Forms.Keys:G.
      WHEN "H" THEN oKey2 = System.Windows.Forms.Keys:H.
      WHEN "I" THEN oKey2 = System.Windows.Forms.Keys:I.
      WHEN "J" THEN oKey2 = System.Windows.Forms.Keys:J.
      WHEN "K" THEN oKey2 = System.Windows.Forms.Keys:K.
      WHEN "L" THEN oKey2 = System.Windows.Forms.Keys:L.
      WHEN "M" THEN oKey2 = System.Windows.Forms.Keys:M.
      WHEN "N" THEN oKey2 = System.Windows.Forms.Keys:N.
      WHEN "O" THEN oKey2 = System.Windows.Forms.Keys:O.
      WHEN "P" THEN oKey2 = System.Windows.Forms.Keys:P.
      WHEN "Q" THEN oKey2 = System.Windows.Forms.Keys:Q.
      WHEN "R" THEN oKey2 = System.Windows.Forms.Keys:R.
      WHEN "S" THEN oKey2 = System.Windows.Forms.Keys:S.
      WHEN "T" THEN oKey2 = System.Windows.Forms.Keys:T.
      WHEN "U" THEN oKey2 = System.Windows.Forms.Keys:U.
      WHEN "V" THEN oKey2 = System.Windows.Forms.Keys:V.
      WHEN "W" THEN oKey2 = System.Windows.Forms.Keys:W.
      WHEN "X" THEN oKey2 = System.Windows.Forms.Keys:X.
      WHEN "Y" THEN oKey2 = System.Windows.Forms.Keys:Y.
      WHEN "Z" THEN oKey2 = System.Windows.Forms.Keys:Z.
      OTHERWISE RETURN NO.
    END CASE.

    oEnum = EnumHelper:Add(oKey1,oKey2).
    ioMenuItem:ShortcutKeys = CAST(oEnum,System.Windows.Forms.Keys).
    
    RETURN YES.

  END METHOD.

END CLASS.
