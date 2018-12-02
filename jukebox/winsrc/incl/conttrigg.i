/* conttrigg.i 
   Trigger include for container.
   Invoked to capture hot-keys and apply then in current tab
   Must be customized according to default hot-key setup in UIlib
   Parameter 1 is either the handle to a browse or a field-map that is
   linked to the toolbar in that holds the event
---------------------------------------------------------------------------------*/
DEF VAR hLinkedToolbar AS HANDLE NO-UNDO.

ON 'ctrl-d' OF FRAME {&FRAME-NAME} ANYWHERE DO:
  IF DYNAMIC-FUNCTION("getLinkedObject",{1},"toolbar","from") NE ? THEN DO:
    DYNAMIC-FUNCTION("setCurrentObject",DYNAMIC-FUNCTION("getLinkedObject",{1},"toolbar","from")).
    RUN DeleteRecord.
  END.
END.
ON 'delete-character' OF FRAME {&FRAME-NAME} ANYWHERE DO:
  IF {1}:TYPE NE "browse" THEN RETURN NO-APPLY.
  hLinkedToolbar = DYNAMIC-FUNCTION("getLinkedObject",{1},"toolbar","from").
  IF VALID-HANDLE(hLinkedToolbar) THEN
    DYNAMIC-FUNCTION("setCurrentObject",hLinkedToolbar).
  ELSE 
    DYNAMIC-FUNCTION("setCurrentObject",{1}).
  RUN DeleteRecord.
END.

ON 'ctrl-n' OF FRAME {&FRAME-NAME} ANYWHERE DO:
  IF DYNAMIC-FUNCTION("getLinkedObject",{1},"toolbar","from") NE ? THEN 
    DYNAMIC-FUNCTION("ApplyEvent",DYNAMIC-FUNCTION("getLinkedObject",{1},"toolbar","from"),"new").
END.

ON 'ctrl-e' OF FRAME {&FRAME-NAME} ANYWHERE DO:
  IF DYNAMIC-FUNCTION("getLinkedObject",{1},"toolbar","from") NE ? THEN 
    DYNAMIC-FUNCTION("ApplyEvent",DYNAMIC-FUNCTION("getLinkedObject",{1},"toolbar","from"),"edit").
END.

ON 'insert-mode' OF FRAME {&FRAME-NAME} ANYWHERE DO:
  hLinkedToolbar = DYNAMIC-FUNCTION("getLinkedObject",{1},"toolbar","from").
  IF VALID-HANDLE(hLinkedToolbar) THEN 
    DYNAMIC-FUNCTION("setCurrentObject",hLinkedToolbar).
  ELSE
    DYNAMIC-FUNCTION("setCurrentObject",{1}).
  RUN NewRecord.
END.

ON 'ctrl-s':U OF FRAME {&FRAME-NAME} ANYWHERE DO:
  DYNAMIC-FUNCTION("setCurrentObject",DYNAMIC-FUNCTION("getLinkedObject",{1},"toolbar","from")).
  RUN SaveRecord.
  &IF DEFINED(use_fld_color) &THEN
    IF SEARCH("reset_fld_color.r") NE ? OR SEARCH("reset_fld_color.p") NE ? THEN 
      RUN reset_fld_color.p (FRAME {&FRAME-NAME}:FIRST-CHILD,?,?,?,?).
  &ENDIF
END.

ON 'alt-c':U OF FRAME {&FRAME-NAME} ANYWHERE DO:
  IF DYNAMIC-FUNCTION("getLinkedObject",{1},"toolbar","from") NE ? THEN DO:
    DYNAMIC-FUNCTION("setCurrentObject",DYNAMIC-FUNCTION("getLinkedObject",{1},"toolbar","from")).
    RUN CopyRecord.
  END.
END.

ON 'ctrl-z':U OF FRAME {&FRAME-NAME} ANYWHERE DO:
  DYNAMIC-FUNCTION("setCurrentObject",DYNAMIC-FUNCTION("getLinkedObject",{1},"toolbar","from")).
  RUN UndoRecord.
  &IF DEFINED(use_fld_color) &THEN
    IF SEARCH("reset_fld_color.r") NE ? OR SEARCH("reset_fld_color.p") NE ? THEN 
      RUN reset_fld_color.p (FRAME {&FRAME-NAME}:FIRST-CHILD,?,?,?,?).
  &ENDIF
END.
ON 'end-error':U OF FRAME {&FRAME-NAME} ANYWHERE DO:
  IF DYNAMIC-FUNCTION("getLinkedObject",{1},"toolbar","from") NE ? THEN DO:
    DYNAMIC-FUNCTION("setCurrentObject",DYNAMIC-FUNCTION("getLinkedObject",{1},"toolbar","from")).
    RUN UndoRecord.
    &IF DEFINED(use_fld_color) &THEN
      IF SEARCH("reset_fld_color.r") NE ? OR SEARCH("reset_fld_color.p") NE ? THEN 
        RUN reset_fld_color.p (FRAME {&FRAME-NAME}:FIRST-CHILD,?,?,?,?).
    &ENDIF
    RETURN NO-APPLY.
  END.
END.
ON 'F1':U OF FRAME {&FRAME-NAME} ANYWHERE DO:
  IF DYNAMIC-FUNCTION("getLinkedObject",{1},"toolbar","from") NE ? THEN DO:
    DYNAMIC-FUNCTION("setCurrentObject",DYNAMIC-FUNCTION("getLinkedObject",{1},"toolbar","from")).
    RUN Help.
    RETURN NO-APPLY.
  END.
  ELSE DO:
    RUN Help NO-ERROR.
    RETURN NO-APPLY.
  END.
END.
ON 'alt-cursor-left':U OF FRAME {&FRAME-NAME} ANYWHERE DO:
  IF DYNAMIC-FUNCTION("getLinkedObject",{1},"toolbar","from") NE ? THEN DO:
    DYNAMIC-FUNCTION("setCurrentObject",DYNAMIC-FUNCTION("getLinkedObject",{1},"toolbar","from")).
    RUN FirstRecord.
  END.
END.
ON 'alt-cursor-right':U OF FRAME {&FRAME-NAME} ANYWHERE DO:
  IF DYNAMIC-FUNCTION("getLinkedObject",{1},"toolbar","from") NE ? THEN DO:
    DYNAMIC-FUNCTION("setCurrentObject",DYNAMIC-FUNCTION("getLinkedObject",{1},"toolbar","from")).
    RUN LastRecord.
  END.
END.
ON 'alt-cursor-up':U OF FRAME {&FRAME-NAME} ANYWHERE DO:
  IF DYNAMIC-FUNCTION("getLinkedObject",{1},"toolbar","from") NE ? THEN DO:
    DYNAMIC-FUNCTION("setCurrentObject",DYNAMIC-FUNCTION("getLinkedObject",{1},"toolbar","from")).
    RUN PrevRecord.
  END.
END.
ON 'alt-cursor-down':U OF FRAME {&FRAME-NAME} ANYWHERE DO:
  IF DYNAMIC-FUNCTION("getLinkedObject",{1},"toolbar","from") NE ? THEN DO:
    DYNAMIC-FUNCTION("setCurrentObject",DYNAMIC-FUNCTION("getLinkedObject",{1},"toolbar","from")).
    RUN NextRecord.
  END.
END.
ON 'F8':U OF {&WINDOW-NAME} ANYWHERE DO:
  DEF VAR hParent      AS HANDLE NO-UNDO.
  DEF VAR hSearchField AS HANDLE NO-UNDO.
  hParent = DYNAMIC-FUNCTION("getLinkedObject",{1},"oneToOne","from").                      
  IF hParent = ? THEN
    hParent = DYNAMIC-FUNCTION("getLinkedObject",{1},"parent","from").
  IF hParent NE ? THEN DO:
    hSearchField = DYNAMIC-FUNCTION("getLinkedObject",hParent,"browse-search-field","from").
    IF VALID-HANDLE(hSearchField) THEN
      APPLY "entry" TO hSearchField.
  END.
END.
ON 'ctrl-tab':U OF {&WINDOW-NAME} ANYWHERE DO:
  DEF VAR hTabFolder AS HANDLE NO-UNDO.
  hTabFolder = DYNAMIC-FUNCTION("GetLinkedObject",THIS-PROCEDURE,"TabFolder","from").
  IF VALID-HANDLE(hTabFolder) THEN
    RUN NextTab IN hTabFolder (THIS-PROCEDURE).
  RETURN.
END.
ON 'shift-ctrl-tab':U OF {&WINDOW-NAME} ANYWHERE DO:
  PUBLISH "PrevTab" (THIS-PROCEDURE).
  RETURN.
END.
