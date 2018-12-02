/* In order to speed up processing of calculated fields it can be crucial
   to place the procedures in a single persistent procedure that is loaded
   once before processing the records.
   To invoke:
   
   DYNAMIC-FUNCTION("setAttribute",hBrowse,"calcfieldproc","orderline_browsecalc.p").
   
   Usage in browse (the name must not end on .p) :
   
   + ";+LineTotal|DECIMAL|->><>>><>>9.99|orderline_total|Total"
   
   (vs, when calling a .p each time - : 
   + ";+LineTotal|DECIMAL|->><>>><>>9.99|orderline_total.p|Total"
   )
------------------------------------------------------------------------*/   
DEF VAR iCurrTopLevelMenu AS INT NO-UNDO.

FUNCTION ToToplevel RETURNS CHARACTER (INPUT iiJBoxMenuId AS INT):
  DEF VAR cParentMenyLabelString AS CHAR NO-UNDO.

  DEF BUFFER JBoxMenu FOR JBoxMenu.

  IF CAN-FIND(FIRST JBoxMenuToMenu WHERE JBoxMenuToMenu.iFromMenuId = iiJBoxMenuId) THEN DO:
    FOR EACH JBoxMenuToMenu NO-LOCK
        WHERE JBoxMenuToMenu.iFromMenuId = iiJBoxMenuId
       ,FIRST JBoxMenu NO-LOCK
              WHERE JBoxMenu.iJBoxMenuId = JBoxMenuToMenu.iToMenuId
        :
      cParentMenyLabelString = ToTopLevel(JBoxMenuToMenu.iToMenuId).
      IF cParentMenyLabelString NE "" THEN DO:         
        IF JBoxMenu.cMenuLabel NE cParentMenyLabelString THEN 
          cParentMenyLabelString = cParentMenyLabelString + " - " + JBoxMenu.cMenuLabel.
        RETURN cParentMenyLabelString.
      END.
    END.
    RETURN "".
  END.
  ELSE DO:
    IF iiJBoxMenuId NE iCurrTopLevelMenu THEN RETURN "".
    ELSE DO:
      FIND JBoxMenu NO-LOCK
           WHERE JBoxMenu.iJBoxMenuId = iiJboxMenuId
           NO-ERROR.
      IF AVAIL JBoxMenu THEN RETURN JBoxMenu.cMenuLabel.
      ELSE RETURN "".
    END.
  END.
END FUNCTION.

PROCEDURE menubranch:
  DEF INPUT PARAM  irJBoxMenu   AS ROWID NO-UNDO.
  DEF INPUT PARAM  icParam      AS CHAR  NO-UNDO.
  DEF INPUT PARAM  icSessionId  AS CHAR  NO-UNDO.
  DEF OUTPUT PARAM ocReturn     AS CHAR  NO-UNDO.

  iCurrTopLevelMenu = INTEGER(icParam).

  FIND JBoxMenu NO-LOCK
       WHERE ROWID(JBoxMenu) = irJBoxMenu
       NO-ERROR.
  IF AVAIL JBoxMenu THEN
    ocReturn = ToTopLevel (JBoxMenu.iJBoxMenuId).

  IF ocReturn NE "" THEN
    ocReturn = ocReturn + " - " + JBoxMenu.cMenuLabel.
  ELSE
    ocReturn = "skiprow".
END PROCEDURE.
