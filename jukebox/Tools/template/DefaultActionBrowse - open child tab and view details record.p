DEF VAR hChildTab AS HANDLE NO-UNDO.

IF <queryObject>:IsCurrent THEN DO:
  hChildTab = JBoxSession:Instance:StartChildTab("details window.w",
                                     "Tab title",
                                     NO, /* Don't start new instance if one is active */
                                     NO). /* Close the child tab on close of this program */
     
  /* Go get the rowid for the detils record and pass it on - here as publish */   
  IF JBoxServerAPI:Instance:Find("Objekt","WHERE Objekt.Prg-nr = " + STRING(Priseksempel.Prg-nr) 
                                        + " AND Objekt.Lopenr = " + STRING(Priseksempel.lopenr)) THEN     
    RUN FlatViewDblClick IN hChildTab("Objekt",JBoxServerAPI:Instance:RowIdent("Objekt")).
  ELSE DO:
    JBoxServerAPI:Instance:viewCallMessage().
    APPLY "close" TO hChildTab.
  END.    
                                     
END.
