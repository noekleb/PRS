/*------------------------------------------------------------------------
  File:               lng.p
  Description:        Initier nytt språk ..
  Input Parameters:   GetLng(INPUT CHAR wLngStr, INPUT CHAR wPrgNavn, INPUT WIDGET wiH)
  Output Parameters:  none
  Author:             Sturla Johnsen
  Created:            18.12.98
------------------------------------------------------------------------*/

DEF SHARED VAR wCurrLng AS CHAR NO-UNDO.

DEF VAR wLen     AS INTE NO-UNDO.
DEF VAR wTekster AS CHAR NO-UNDO.

FUNCTION GetId RETURNS CHARACTER (INPUT wh AS HANDLE):
   DEF VAR whParent    AS WIDGET NO-UNDO.
   DEF VAR wMainType   AS LOGI   NO-UNDO.
   DEF VAR wPar        AS CHAR   NO-UNDO.
   DEF VAR wId         AS CHAR   NO-UNDO.
   
   ASSIGN whParent = wh:PARENT.
  
   IF VALID-HANDLE(whparent) THEN DO:
      IF whParent:NAME <> ? THEN ASSIGN wPar = whParent:NAME.
      ELSE DO:
         ASSIGN whParent = IF CAN-QUERY(wh,"FRAME") THEN wh:FRAME ELSE ?.
         IF VALID-HANDLE(whParent) THEN ASSIGN wPar = whParent:NAME.
      END.
   END.      
    
   ASSIGN wId = (IF VALID-HANDLE(whParent) AND whParent:TYPE = "BROWSE" THEN "CELL" 
                 ELSE wh:TYPE)
              +  (IF NOT CAN-QUERY(wh,"TABLE") OR CAN-QUERY(wh,"TABLE") AND wh:TABLE = ? THEN "" ELSE (wh:TABLE + "."))
              + (IF wh:NAME <> ? THEN wh:NAME ELSE "")
              +  wPar.
              
   RETURN wId.              
END FUNCTION.

PROCEDURE GetLng:
   DEF INPUT PARAMETER wLngStr  AS CHAR   NO-UNDO.
   DEF INPUT PARAMETER wPrgNavn AS CHAR   NO-UNDO.
   DEF INPUT PARAMETER wiH      AS WIDGET NO-UNDO.

   IF wLngStr = "" THEN DO:
      FIND Lng WHERE Lng.Lng     = wCurrLng AND
                     Lng.PrgNavn = ENTRY(1,wPrgNavn,".") NO-LOCK NO-ERROR.
      IF NOT AVAIL Lng THEN RETURN.               
      ASSIGN wTekster = Lng.Tekster.
   END.   
   
   ELSE ASSIGN wTekster = wLngStr.

   CASE wiH:type:
      WHEN "DIALOG-BOX" OR
      WHEN "FRAME"      THEN RUN Frame  (wiH).
      WHEN "WINDOW"     THEN RUN Window (wiH).  
   END CASE.   

END PROCEDURE.

PROCEDURE Window:
   DEF INPUT PARAMETER wh AS WIDGET NO-UNDO.

   DEF VAR wId AS CHAR NO-UNDO.
   DEF VAR wStart AS INTE NO-UNDO.
  
   ASSIGN wId = "£" + GetId(wh) + "$".
          wStart = INDEX(wTekster,wId) + 1.
  
   IF wStart > 1 THEN
      ASSIGN wh:TITLE = ENTRY(2,SUBSTR(wTekster,wStart),"$") NO-ERROR.

   ASSIGN wh = wh:FIRST-CHILD.
   DO WHILE VALID-HANDLE(wh):
      IF wh:TYPE = "FRAME"  THEN RUN Frame (wh). ELSE
      IF wh:TYPE = "BROWSE" THEN RUN Browse(wh).
      ASSIGN wh = wh:NEXT-SIBLING.
   END.
END PROCEDURE.

PROCEDURE Frame:
   DEF INPUT PARAMETER wh AS WIDGET NO-UNDO.

   DEF VAR wId AS CHAR NO-UNDO.
   DEF VAR wStart AS INTE NO-UNDO.
  
   ASSIGN wId = "£" + GetId(wh) + "$".
   IF wId = "£$" THEN RETURN.

   ASSIGN wStart = INDEX(wTekster,wId) + 1.
   IF wStart > 1 THEN DO:
      IF wh:TITLE <> ? THEN 
         ASSIGN wh:TITLE = ENTRY(2,SUBSTR(wTekster,wStart),"$").
   END.
   ASSIGN wh     = wh:CURRENT-ITERATION
          wh     = wh:FIRST-CHILD.
     
   DO WHILE VALID-HANDLE(wh):
      IF wh:TYPE = "FRAME"  THEN RUN Frame (wh). ELSE
      IF wh:TYPE = "BROWSE" THEN RUN Browse(wh). ELSE 
      IF wh:TYPE <> "LITERAL" THEN
      DO:
         ASSIGN wId = "£" + GetId(wh) + "$".
         IF wId <> "£$" THEN DO:
            ASSIGN wStart = INDEX(wTekster,wId) + 1.
            IF wStart > 1 THEN DO:
               IF wh:TYPE = "RADIO-SET" THEN
                    ASSIGN wh:RADIO-BUTTONS = ENTRY(3,SUBSTR(wTekster,wStart),"$") NO-ERROR.
               ELSE 
               /*
               IF CAN-DO("COMBO-BOX,SELECTION-LIST",wh:TYPE) AND ENTRY(3,SUBSTR(wTekster,wStart),"$") <> "" THEN
                    ASSIGN wh:LIST-ITEMS = ENTRY(3,SUBSTR(wTekster,wStart),"$") NO-ERROR.
               */
               IF CAN-SET(wh,"LABEL") THEN DO:
                  ASSIGN wLen = LENGTH(ENTRY(2,SUBSTR(wTekster,wStart),"$")) + 1.
                  REPEAT:
                     IF wLen = 1 THEN LEAVE.
                     ASSIGN wLen = wLen - 1
                            wh:LABEL = SUBSTR(ENTRY(2,SUBSTR(wTekster,wStart),"$"),1,wLen) NO-ERROR.
                     IF NOT ERROR-STATUS:NUM-MESSAGES > 0 THEN LEAVE.
                  END.   
               END.   
              
               IF (NOT CAN-QUERY(wh,"TABLE") OR CAN-QUERY(wh,"TABLE") AND wh:Table = ? OR wh:TABLE = "") AND
                  CAN-SET(wh,"SCREEN-VALUE") AND ENTRY(4,SUBSTR(wTekster,wStart),"$") <> "" THEN
                  ASSIGN wh:SCREEN-VALUE = ENTRY(4,SUBSTR(wTekster,wStart),"$") NO-ERROR.
              
               ASSIGN wh:HELP    = ENTRY(5,SUBSTR(wTekster,wStart),"$")
                      wh:TOOLTIP = ENTRY(6,SUBSTR(wTekster,wStart),"$") NO-ERROR.
            END.   
         END.
      END.
      ASSIGN wh = wh:NEXT-SIBLING.
   END.
END PROCEDURE.

PROCEDURE Browse:
   DEF INPUT PARAMETER wh AS WIDGET NO-UNDO.
  
   DEF VAR wId AS CHAR NO-UNDO.
   DEF VAR wStart AS INTE NO-UNDO.
   
   ASSIGN wId = "£" + GetId(wh) + "$".
   IF wId = "£$" THEN RETURN.

   ASSIGN wStart = INDEX(wTekster,wId) + 1.
   IF wStart > 1 THEN DO:
      IF wh:TITLE <> ? THEN 
      ASSIGN wh:TITLE   = ENTRY(2,SUBSTR(wTekster,wStart),"$")
             wh:HELP    = ENTRY(5,SUBSTR(wTekster,wStart),"$") 
             wh:TOOLTIP = ENTRY(6,SUBSTR(wTekster,wStart),"$") NO-ERROR.
   END.      
    
   ASSIGN wh = wh:FIRST-COLUMN.
    
   DO WHILE VALID-HANDLE(wh):
      ASSIGN wId = "£" + GetId(wh) + "$".
      IF wId <> "£$" THEN DO:
         ASSIGN wStart = INDEX(wTekster,wId) + 1.
         IF wStart > 1 THEN
            ASSIGN wh:LABEL = ENTRY(2,SUBSTR(wTekster,wStart),"$") 
                   wh:HELP  = ENTRY(5,SUBSTR(wTekster,wStart),"$") NO-ERROR.
      END.
      ASSIGN wh = wh:NEXT-COLUMN.
   END.
END PROCEDURE.
 
