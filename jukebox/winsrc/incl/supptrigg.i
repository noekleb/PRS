/* supptrigg.i 
   Trigger include for suppressed windows.
   Must be customized according to default hot-key setup in UIlib
   Parameter 1 is either the handle to a browse or a field-map that is
   linked to the toolbar in that holds the event
---------------------------------------------------------------------------------*/
ON 'ctrl-d':U OF FRAME {&FRAME-NAME} ANYWHERE DO:
  IF DYNAMIC-FUNCTION("getLinkedObject",{1},"toolbar","from") NE ? THEN DO:
    DYNAMIC-FUNCTION("setCurrentObject",DYNAMIC-FUNCTION("getLinkedObject",{1},"toolbar","from")).
    RUN DeleteRecord.
  END.
END.

ON 'ctrl-e':U OF FRAME {&FRAME-NAME} ANYWHERE DO:
  IF DYNAMIC-FUNCTION("getLinkedObject",{1},"toolbar","from") NE ? THEN DO:
    DYNAMIC-FUNCTION("setCurrentObject",DYNAMIC-FUNCTION("getLinkedObject",{1},"toolbar","from")).
    RUN EditRecord.
  END.
END.

ON 'ctrl-n':U OF FRAME {&FRAME-NAME} ANYWHERE DO:
  IF DYNAMIC-FUNCTION("getLinkedObject",{1},"toolbar","from") NE ? THEN DO:
    DYNAMIC-FUNCTION("ApplyEvent",DYNAMIC-FUNCTION("getLinkedObject",{1},"toolbar","from"),"new").
/*     DYNAMIC-FUNCTION("setCurrentObject",DYNAMIC-FUNCTION("getLinkedObject",{1},"toolbar","from")). */
/*     RUN NewRecord.                                                                                 */
  END.
END.
/* ON 'insert-mode' OF FRAME {&FRAME-NAME} ANYWHERE DO:                                              */
/*   DYNAMIC-FUNCTION("setCurrentObject",DYNAMIC-FUNCTION("getLinkedObject",{1},"toolbar","from")).  */
/*   RUN NewRecord.                                                                                  */
/* END.                                                                                              */

ON 'ctrl-s':U OF FRAME {&FRAME-NAME} ANYWHERE DO:
  DYNAMIC-FUNCTION("setCurrentObject",DYNAMIC-FUNCTION("getLinkedObject",{1},"toolbar","from")).
  RUN SaveRecord.
  &IF DEFINED(use_fld_color) &THEN
    IF SEARCH("reset_fld_color.r") NE ? OR SEARCH("reset_fld_color.p") NE ? THEN 
      RUN reset_fld_color.p (FRAME {&FRAME-NAME}:FIRST-CHILD,?,?,?,?).
  &ENDIF
END.

ON 'ctrl-f':U OF FRAME {&FRAME-NAME} ANYWHERE DO:
  IF DYNAMIC-FUNCTION("getLinkedObject",{1},"toolbar","from") NE ? THEN DO:
    DYNAMIC-FUNCTION("setCurrentObject",DYNAMIC-FUNCTION("getLinkedObject",{1},"toolbar","from")).
    RUN FilterRecord.
  END.
END.

ON 'alt-c' OF FRAME {&FRAME-NAME} ANYWHERE DO:
  IF DYNAMIC-FUNCTION("getLinkedObject",{1},"toolbar","from") NE ? THEN DO:
    DYNAMIC-FUNCTION("setCurrentObject",DYNAMIC-FUNCTION("getLinkedObject",{1},"toolbar","from")).
    RUN CopyRecord.
  END.
END.
ON 'alt-k' OF FRAME {&FRAME-NAME} ANYWHERE DO:
  IF DYNAMIC-FUNCTION("getLinkedObject",{1},"toolbar","from") NE ? THEN DO:
    DYNAMIC-FUNCTION("setCurrentObject",DYNAMIC-FUNCTION("getLinkedObject",{1},"toolbar","from")).
    RUN CopyRecord.
  END.
END.

ON 'ctrl-z':U OF FRAME {&FRAME-NAME} ANYWHERE DO:
  RUN InvokeMethod(DYNAMIC-FUNCTION("getLinkedObject",{1},"toolbar","from"),"UndoRecord").
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
  RUN Help NO-ERROR.
  RETURN NO-APPLY.
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

