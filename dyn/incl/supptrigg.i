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

ON 'alt-n':U OF FRAME {&FRAME-NAME} ANYWHERE DO:
  IF DYNAMIC-FUNCTION("getLinkedObject",{1},"toolbar","from") NE ? THEN DO:
    DYNAMIC-FUNCTION("setCurrentObject",DYNAMIC-FUNCTION("getLinkedObject",{1},"toolbar","from")).
    RUN NewRecord.
  END.
END.
/* ON 'insert-mode' OF FRAME {&FRAME-NAME} ANYWHERE DO:                                              */
/*   DYNAMIC-FUNCTION("setCurrentObject",DYNAMIC-FUNCTION("getLinkedObject",{1},"toolbar","from")).  */
/*   RUN NewRecord.                                                                                  */
/* END.                                                                                              */

ON 'alt-l':U OF FRAME {&FRAME-NAME} ANYWHERE DO:
  DYNAMIC-FUNCTION("setCurrentObject",DYNAMIC-FUNCTION("getLinkedObject",{1},"toolbar","from")).
  RUN SaveRecord.
END.

ON 'ctrl-f':U OF FRAME {&FRAME-NAME} ANYWHERE DO:
  DYNAMIC-FUNCTION("setCurrentObject",DYNAMIC-FUNCTION("getLinkedObject",{1},"toolbar","from")).
  RUN FilterRecord.
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
END.
ON 'end-error':U OF FRAME {&FRAME-NAME} ANYWHERE DO:
  IF DYNAMIC-FUNCTION("getLinkedObject",{1},"toolbar","from") NE ? THEN DO:
    DYNAMIC-FUNCTION("setCurrentObject",DYNAMIC-FUNCTION("getLinkedObject",{1},"toolbar","from")).
    RUN UndoRecord.
    RETURN NO-APPLY.
  END.
END.
