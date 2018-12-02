/*
	Last change:  SJ    5 Apr 2000   12:18 pm
*/

DEF VAR wLocked AS INTE   NO-UNDO.

DEF TEMP-TABLE tSort NO-UNDO
   FIELD BrowseH     AS HANDLE
   FIELD ColName     AS CHAR
   FIELD QueryH      AS HANDLE
   FIELD Seq#        AS INTE
   FIELD Teller#     AS INTE
   FIELD SokAktiv    AS LOGI
   INDEX Hoved IS PRIMARY UNIQUE
                BrowseH ASCENDING
                ColName ASCENDING
                QueryH  ASCENDING
   INDEX Seq    BrowseH ASCENDING
                ColName ASCENDING
                Seq#    ASCENDING
   INDEX Teller BrowseH ASCENDING
                Teller# ASCENDING.

DEF TEMP-TABLE tSearch NO-UNDO
   FIELD BrowseH     AS HANDLE
   FIELD FillSearchH AS HANDLE
   FIELD BtnSearchH  AS HANDLE
   INDEX Hoved IS PRIMARY UNIQUE BrowseH ASCENDING.

DEF TEMP-TABLE tCurrSort NO-UNDO
    FIELD BrowseH  AS HANDLE
    FIELD QueryH   AS HANDLE
    FIELD ColH     AS HANDLE
    FIELD Teller#  AS INTE
    INDEX Hoved IS PRIMARY UNIQUE BrowseH ASCENDING.

SUBSCRIBE TO "GetBrSortHandle" ANYWHERE.

PROCEDURE GetBrSortHandle:
  DEF OUTPUT PARAMETER wBrowseSortH AS HANDLE NO-UNDO.
  ASSIGN wBrowseSortH = THIS-PROCEDURE.
END PROCEDURE.

FUNCTION ColHandle RETURNS HANDLE (INPUT wiBrowseH AS HANDLE, INPUT wiColName AS CHAR):
   DEF VAR wh AS HANDLE NO-UNDO.
   ASSIGN wh = wiBrowseH:FIRST-COLUMN.
   DO WHILE VALID-HANDLE(wh):
      IF wh:NAME = wiColName THEN RETURN wh.
      ASSIGN wh = wh:NEXT-COLUMN.
   END.
   RETURN ?.
END.

PROCEDURE Initier:
   DEF INPUT PARAMETER wiBrowseH     AS HANDLE NO-UNDO.
   DEF INPUT PARAMETER wiColName     AS CHAR   NO-UNDO.
   DEF INPUT PARAMETER wiQueryH      AS HANDLE NO-UNDO.
   DEF INPUT PARAMETER wiSokAktiv    AS LOGI   NO-UNDO.

   DEF VAR wSeq#    AS INTE NO-UNDO.
   DEF VAR wTeller# AS INTE NO-UNDO.

   FIND tSort WHERE tSort.BrowseH = wiBrowseH AND
                    tSort.ColName = wiColName AND
                    tSort.QueryH  = wiQueryH  NO-ERROR.
   IF AVAIL tSort THEN DELETE tSort.
   
   FIND LAST tSort USE-INDEX Seq WHERE
             tSort.BrowseH = wiBrowseH AND
             tSort.ColName = wiColName NO-ERROR.

   ASSIGN wSeq# = IF AVAIL tSort THEN tSort.Seq# + 1 ELSE 1.

   FIND LAST tSort USE-INDEX Teller WHERE
             tSort.BrowseH = wiBrowseH NO-ERROR.

   ASSIGN wTeller# = IF AVAIL tSort THEN tSort.Teller# + 1 ELSE 1.

   FIND tSort WHERE tSort.BrowseH = wiBrowseH AND
                    tSort.ColName = wiColName AND
                    tSort.QueryH  = wiQueryH  NO-ERROR.
   CREATE tSort.
   ASSIGN tSort.BrowseH     = wiBrowseH
          tSort.ColName     = wiColName
          tSort.QueryH      = wiQueryH
          tSort.Seq#        = wSeq#
          tSort.Teller#     = wTeller#
          tSort.SokAktiv    = wiSokAktiv.
END PROCEDURE.

PROCEDURE ByttSort:
   DEF INPUT PARAMETER wiBrowseH   AS HANDLE NO-UNDO.
   DEF INPUT PARAMETER wiRowid     AS ROWID  NO-UNDO.
   DEF INPUT PARAMETER wiColName   AS CHAR   NO-UNDO.
   DEF INPUT PARAMETER wiSeq#      AS INTE   NO-UNDO.
   DEF INPUT PARAMETER wiQueryName AS CHAR   NO-UNDO.
   DEF INPUT PARAMETER wiCaller    AS HANDLE NO-UNDO.

   DEF VAR wColH         AS HANDLE NO-UNDO.
   DEF VAR wWorkH        AS HANDLE NO-UNDO.
   DEF VAR wByttet       AS LOGI   NO-UNDO.
   DEF VAR i             AS INTE   NO-UNDO.

   Bytt: DO:

      DO i = 1 TO 2.
         FIND tCurrSort WHERE
              tCurrSort.BrowseH = wiBrowseH NO-ERROR.

         IF NOT AVAIL tCurrSort THEN DO:
            IF i = 2 THEN LEAVE Bytt.
            FIND FIRST tSort WHERE tSort.BrowseH = wiBrowseH NO-ERROR.
            IF AVAIL tSort THEN
               RUN SettCurrSort (wiBrowseH, tSort.QueryH, tSort.ColName).
         END.
         ELSE LEAVE.
      END.

      IF wiColName <> "" THEN
         ASSIGN wColH = ColHandle(wiBrowseH,wiColName).
      ELSE
      ASSIGN wColH     = wiBrowseH:CURRENT-COLUMN
             wiColName = wColH:NAME.

      IF wiSeq# > 0 THEN
         FIND FIRST tSort WHERE
                    tSort.BrowseH = wiBrowseH AND
                    tSort.ColName = wiColName AND
                    tSort.Seq#    = wiSeq#    NO-ERROR.
      ELSE
      IF wiQueryName <> "" THEN DO:
         FOR EACH tSort WHERE
                  tSort.BrowseH = wiBrowseH AND
                  tSort.ColName = wiColName NO-LOCK:
            IF tSort.QueryH:NAME = wiQueryName THEN LEAVE.
         END.
      END.
      ELSE DO:
         FIND tSort WHERE
              tSort.BrowseH = wiBrowseH AND
              tSort.ColName = wiColName AND
              tSort.QueryH  = tCurrSort.QueryH NO-ERROR.
         IF AVAIL tSort THEN
         FIND NEXT tSort USE-INDEX Seq WHERE
                   tSort.BrowseH = wiBrowseH AND
                   tSort.ColName = wiColName NO-ERROR.
         IF NOT AVAIL tSort THEN
         FIND FIRST tSort USE-INDEX Seq WHERE
                    tSort.BrowseH = wiBrowseH AND
                    tSort.ColName = wiColName NO-ERROR.
      END.

      IF NOT AVAIL tSort THEN LEAVE Bytt.

      IF tSort.QueryH = tCurrSort.QueryH AND tSort.Teller# = tCurrSort.Teller# THEN LEAVE Bytt.

      IF tCurrSort.ColH:LABEL MATCHES("*>") OR tCurrSort.ColH:LABEL MATCHES("*<") THEN
           ASSIGN tCurrSort.ColH:LABEL         = REPLACE(tCurrSort.ColH:LABEL,IF tCurrSort.ColH:LABEL MATCHES("*>") THEN ">" ELSE "<" ,"*").

      ASSIGN tCurrSort.ColH:LABEL-BGCOLOR = ?.

      {sww.i}

      FIND tSearch OF tSort NO-ERROR.
      IF AVAIL tSearch AND VALID-HANDLE(tSearch.FillSearchH) THEN
         ASSIGN tSearch.FillSearchH:SCREEN-VALUE = ""
                tSearch.FillSearchH:PRIVATE-DATA = STRING(wColH:DATA-TYPE BEGINS "CHAR","A/N")
                tSearch.FillSearchH:SENSITIVE    = tSort.SokAktiv
                tSearch.BtnSearcH:SENSITIVE      = tSort.SokAktiv.

      ASSIGN wWorkH  = wiBrowseH:FRAME
             wByttet = YES.

      RUN LockWindowUpdate (wWorkH:HWND, OUTPUT wLocked).

      wiBrowseH:SET-REPOSITIONED-ROW(wiBrowseH:FOCUSED-ROW,"CONDITIONAL").

      ASSIGN wiBrowseH:QUERY = tSort.QueryH NO-ERROR.
      IF ERROR-STATUS:NUM-MESSAGES = 0 THEN DO:
         IF VALID-HANDLE(tCurrSort.QueryH) THEN
             tCurrSort.QueryH:QUERY-CLOSE().
         RUN VALUE("OpenQuery" + wiBrowseH:NAME + tSort.ColName + tSort.QueryH:NAME) IN wiCaller.
         ASSIGN tCurrSort.Teller#  = tSort.Teller#
                tCurrSort.ColH     = wColH.

         tSort.QueryH:REPOSITION-TO-ROWID(wiRowid) NO-ERROR.

         RUN SettCurrSort(wiBrowseH,tSort.QueryH,wColH:NAME).
      END.

   END.

   APPLY "ENTRY" TO wiBrowseH.
   APPLY "LEAVE" TO wiBrowseH.
   APPLY "ENTRY" TO wiBrowseH.
   IF wLocked <> 0 THEN
      RUN LockWindowUpdate ( 0, OUTPUT wLocked).
   IF wByttet THEN {swn.i}.
   IF VALID-HANDLE(wColH) THEN ASSIGN wiBrowseH:CURRENT-COLUMN = wColH.

END PROCEDURE.

PROCEDURE SettCurrSort:
   DEF INPUT PARAMETER wiBrowseH AS HANDLE NO-UNDO.
   DEF INPUT PARAMETER wiQueryH  AS HANDLE NO-UNDO.
   DEF INPUT PARAMETER wiColName AS CHAR   NO-UNDO.

   FIND tCurrSort WHERE
        tCurrSort.BrowseH = wiBrowseH NO-ERROR.
   IF NOT AVAIL tCurrSort THEN DO:
      CREATE tCurrSort.
      ASSIGN tCurrSort.BrowseH = wiBrowseH
             tCurrSort.Teller# = 1.
   END.

   ASSIGN tCurrSort.QueryH     = wiQueryH
          tCurrSort.ColH       = ColHandle(wiBrowseH,wiColName) NO-ERROR.
   IF tCurrSort.QueryH:NAME MATCHES "*-asc"  OR tCurrSort.QueryH:NAME MATCHES "*-desc" THEN DO:
      IF tCurrSort.ColH:LABEL  MATCHES "*~*" THEN
         ASSIGN tCurrSort.ColH:LABEL = REPLACE(tCurrSort.ColH:LABEL,"*",IF tCurrSort.QueryH:NAME MATCHES "*-asc" THEN ">" ELSE "<").
   END.
   ASSIGN tCurrSort.ColH:LABEL-BGCOLOR = 15.
END PROCEDURE.

PROCEDURE SettNyCurrSort:
   DEF INPUT PARAMETER wiBrowseH   AS HANDLE NO-UNDO.
   DEF INPUT PARAMETER wiQueryName AS CHAR   NO-UNDO.
   DEF INPUT PARAMETER wiColName   AS CHAR   NO-UNDO.

   FOR EACH tSort WHERE tSort.BrowseH = wiBrowseH AND
                        tSort.ColName = wiColName:
      IF tSort.QueryH:NAME = wiQueryName THEN LEAVE.
   END.
   IF AVAIL tSort THEN DO:
      RUN SettCurrSort (wiBrowseH,tSort.QueryH,wiColName).
      IF wiBrowseH:QUERY <> tSort.QueryH THEN
         ASSIGN wiBrowseH:QUERY = tSort.QueryH NO-ERROR.
      RETURN "<Ok>".
   END.
   ELSE RETURN "<Feil>".

END PROCEDURE.

PROCEDURE CursorSort:
   DEF INPUT PARAMETER wiCaller   AS HANDLE NO-UNDO.
   DEF INPUT PARAMETER wiColNameA AS CHAR   NO-UNDO.
   DEF INPUT PARAMETER wiColNameB AS CHAR   NO-UNDO.
   DEF INPUT PARAMETER wiColNameC AS CHAR   NO-UNDO.
   DEF INPUT PARAMETER wiColNameD AS CHAR   NO-UNDO.
   DEF INPUT PARAMETER wiColNameE AS CHAR   NO-UNDO.
   DEF INPUT PARAMETER wiColNameF AS CHAR   NO-UNDO.

   &SCOP KolonneFunnet IF CAN-DO(wiColNameA + "," + wiColNameB + "," + wiColNameC + "," + wiColNameD + "," + wiColNameE + "," + wiColNameF,wh:NAME) THEN LEAVE.
   DEF VAR wh AS HANDLE NO-UNDO.

   ASSIGN wh = SELF:CURRENT-COLUMN.

   IF NOT VALID-HANDLE(wh) THEN ASSIGN wh = ColHandle(SELF,wiColNameA).

   IF wiColNameA + wiColNameB + wiColNameC + wiColNameD + wiColNameE + wiColNameF = wh:NAME
      THEN RETURN NO-APPLY.

   IF LAST-EVENT:FUNCTION = "CURSOR-RIGHT" THEN DO:
      ASSIGN wh = wh:NEXT-COLUMN.
      DO WHILE VALID-HANDLE(wh):
         {&KolonneFunnet}
         ASSIGN wh = wh:NEXT-COLUMN.
      END.
      IF NOT VALID-HANDLE(wh) THEN DO:
         ASSIGN wh = SELF:FIRST-COLUMN.
         DO WHILE VALID-HANDLE(wh):
            {&KolonneFunnet}
            ASSIGN wh = wh:NEXT-COLUMN.
         END.
      END.
   END.
   ELSE DO:
      ASSIGN wh = wh:PREV-COLUMN.
      DO WHILE VALID-HANDLE(wh):
         {&KolonneFunnet}
         ASSIGN wh = wh:PREV-COLUMN.
      END.
      IF NOT VALID-HANDLE(wh) THEN DO:
         ASSIGN wh = SELF:CURRENT-COLUMN.
         IF NOT VALID-HANDLE(wh) THEN ASSIGN wh = ColHandle(SELF,wiColNameA).
         DO WHILE VALID-HANDLE(wh):
            IF NOT VALID-HANDLE(wh:NEXT-COLUMN) THEN LEAVE.
            ASSIGN wh = wh:NEXT-COLUMN.
         END.
         DO WHILE VALID-HANDLE(wh):
            {&KolonneFunnet}
            ASSIGN wh = wh:PREV-COLUMN.
         END.
      END.
   END.

   RUN VALUE("ByttSort" + SELF:NAME) IN wiCaller (wh:NAME,0,"").
   RETURN NO-APPLY.
END PROCEDURE.

PROCEDURE MakeMenu:
   DEF INPUT PARAMETER wiBrowseH AS HANDLE NO-UNDO.
   DEF INPUT PARAMETER wiMenu    AS CHAR   NO-UNDO.
   DEF INPUT PARAMETER wiCaller  AS HANDLE NO-UNDO.

   DEF VAR wBrowse-popup     AS HANDLE NO-UNDO.
   DEF VAR wBrowse-sub       AS HANDLE NO-UNDO.
   DEF VAR wBrowse-popupItem AS HANDLE NO-UNDO.
   DEF VAR i                 AS INTE   NO-UNDO.

   IF wiMenu = "" THEN RETURN NO-APPLY.

   ASSIGN wBrowse-popup = wiBrowseH:POPUP-MENU.
  
   IF VALID-HANDLE(wBrowse-popup) THEN DO:
      ASSIGN wiBrowseH:POPUP-MENU = ?.
      CREATE MENU-ITEM wBrowse-popupItem
         ASSIGN PARENT  = wBrowse-popup
                SUBTYPE = "RULE".
   END.
   ELSE
   CREATE MENU wBrowse-popup
      ASSIGN POPUP-ONLY = YES.

   CREATE SUB-MENU wBrowse-sub
      ASSIGN PARENT    = wBrowse-popup
             LABEL     = "Sorter &etter"
             SENSITIVE = YES
      TRIGGERS:
         ON MENU-DROP PERSISTENT RUN MenuDrop IN THIS-PROCEDURE (wiBrowseH).
      END TRIGGERS.

   DO i = 1 TO NUM-ENTRIES(wiMenu):
      CREATE MENU-ITEM wBrowse-popupItem
         ASSIGN PARENT       = wBrowse-sub
                LABEL        = ENTRY(i,wiMenu)
                TOGGLE-BOX   = YES
                SENSITIVE    = YES
                PRIVATE-DATA = STRING(i)
      TRIGGERS:
         ON VALUE-CHANGED PERSISTENT RUN MenuBytt IN THIS-PROCEDURE (wiCaller,wiBrowseH).
      END TRIGGERS.
   END.

   ASSIGN wiBrowseH:POPUP-MENU = wBrowse-popup.
END PROCEDURE.

PROCEDURE MenuDrop:
   DEF INPUT PARAMETER wiBrowseH AS HANDLE NO-UNDO.
   DEF VAR wh AS HANDLE NO-UNDO.
   ASSIGN wh = SELF:FIRST-CHILD.
   FIND tCurrSort WHERE tCurrSort.BrowseH = wiBrowseH NO-ERROR.
   IF AVAIL tCurrSort THEN
   DO WHILE VALID-HANDLE(wh):
      ASSIGN wh:CHECKED = STRING(tCurrSort.Teller#) = wh:PRIVATE-DATA
             wh         = wh:NEXT-SIBLING.
   END.
END PROCEDURE.

PROCEDURE MenuBytt:
   DEF INPUT PARAMETER wiCaller  AS HANDLE NO-UNDO.
   DEF INPUT PARAMETER wiBrowseH AS HANDLE NO-UNDO.

   RUN VALUE("InitBrowse" + wiBrowseH:NAME) IN wiCaller.
   FIND FIRST tSort WHERE
              tSort.BrowseH = wiBrowseH AND
              tSort.Teller# = INT(SELF:PRIVATE-DATA) NO-ERROR.
   IF AVAIL tSort THEN
      RUN VALUE("ByttSort" + wiBrowseH:NAME) IN wiCaller (tSort.ColName,tSort.Seq#,"").
END PROCEDURE.

PROCEDURE MakeMbMenu:
   DEF INPUT PARAMETER wiBrowseH AS HANDLE NO-UNDO.
   DEF INPUT PARAMETER wiMenu    AS CHAR   NO-UNDO.

   DEF VAR wBrowse-popup     AS HANDLE NO-UNDO.
   DEF VAR wBrowse-popupItem AS HANDLE NO-UNDO.
   DEF VAR i                 AS INTE   NO-UNDO.
   DEF VAR wh                AS HANDLE NO-UNDO.

   ASSIGN wBrowse-popup = wiBrowseH:POPUP-MENU.
  
   IF VALID-HANDLE(wBrowse-popup) THEN DO:
      ASSIGN wiBrowseH:POPUP-MENU = ?.
      CREATE MENU-ITEM wBrowse-popupItem
         ASSIGN PARENT  = wBrowse-popup
                SUBTYPE = "RULE".
   END.
   ELSE
   CREATE MENU wBrowse-popup
      ASSIGN POPUP-ONLY = YES.     
  
   DO i = 1 TO NUM-ENTRIES(wiMenu):
      IF ENTRY(i,wiMenu) = "" THEN NEXT.
      ASSIGN wh = WIDGET-HANDLE(ENTRY(i,wiMenu)).
      CREATE MENU-ITEM wBrowse-popupItem
         ASSIGN PARENT       = wBrowse-popup
                LABEL        = wh:LABEL
                SENSITIVE    = YES
                PRIVATE-DATA = STRING(wh)
      TRIGGERS:
         ON CHOOSE PERSISTENT RUN ChooseMbMenu IN THIS-PROCEDURE (wBrowse-popupItem:PRIVATE-DATA).
      END TRIGGERS.
   END.
   ASSIGN wiBrowseH:POPUP-MENU = wBrowse-popup.
END PROCEDURE.

PROCEDURE ChooseMbMenu:
   DEF INPUT PARAMETER wiChHandle AS CHAR NO-UNDO.

   DEF VAR wh AS HANDLE NO-UNDO.
   ASSIGN wh = WIDGET-HANDLE(wiChHandle) NO-ERROR.
   IF VALID-HANDLE(wh) THEN DO:
      IF wh:SENSITIVE THEN
           APPLY "CHOOSE" TO wh.
      ELSE MESSAGE "Beklager, men den valgte funksjonen er ikke tilgjengelig for øyeblikket."
                VIEW-AS ALERT-BOX WARNING TITLE "Funksjon ikke tilgjengelig".
   END.
END PROCEDURE.



PROCEDURE MakeSearch:
   DEF INPUT PARAMETER wiBrowseH  AS HANDLE NO-UNDO.
   DEF INPUT PARAMETER wiCaller   AS HANDLE NO-UNDO.
   DEF INPUT PARAMETER wiSokAktiv AS LOGI   NO-UNDO.

   DEF VAR wBrowseFrameH AS HANDLE NO-UNDO.
   DEF VAR wFillH AS HANDLE NO-UNDO.

   IF MIN(21,wiBrowseH:Y - 4) <= 0 THEN RETURN NO-APPLY.

   ASSIGN wBrowseFrameH = wiBrowseH:FRAME.

   CREATE FILL-IN wFillH
      ASSIGN FRAME         = wBrowseFrameH
             HEIGHT-PIXELS = MIN(21,wiBrowseH:Y - 4)
             WIDTH-PIXELS  = MIN(95,wiBrowseH:WIDTH-PIXELS)
             X             = wiBrowseH:X
             Y             = wiBrowseH:Y - (wFillH:HEIGHT-PIXELS + 4)
             FORMAT        = "X(35)"
             SENSITIVE     = wiSokAktiv
             VISIBLE       = wBrowseFrameH:VISIBLE
      TRIGGERS:
         ON RETURN        PERSISTENT RUN Searching  IN THIS-PROCEDURE(wiBrowseH, wFillH, wiCaller).
         ON ENTRY         PERSISTENT RUN SettFormat IN THIS-PROCEDURE(wiBrowseH, wFillH).
         ON ANY-PRINTABLE PERSISTENT RUN SjekkInput IN THIS-PROCEDURE(wFillH).
      END TRIGGERS.

   FIND tSearch WHERE tSearch.BrowseH = wiBrowseH NO-ERROR.
   IF NOT AVAIL tSearch THEN DO:
      CREATE tSearch.
      ASSIGN tSearch.BrowseH = wiBrowseH.
   END.
   ASSIGN tSearch.FillSearchH = wFillH.
          wBrowseFrameH       = wiBrowseH:FRAME.


   CREATE BUTTON tSearch.BtnSearchH
      ASSIGN FRAME         = wBrowseFrameH
             HEIGHT-PIXELS = wFillH:HEIGHT-PIXELS
             WIDTH-PIXELS  = 30
             LABEL         = "Søk"
             X             = wFillH:X + wFillH:WIDTH-PIXELS + 5
             Y             = wFillH:Y
             SENSITIVE     = wiSokAktiv
             VISIBLE       = (wFillH:X + wFillH:WIDTH-PIXELS + 5 + 30 < wBrowseFrameH:WIDTH-PIXELS) AND wBrowseFrameH:VISIBLE
      TRIGGERS:
         ON CHOOSE PERSISTENT RUN Searching IN THIS-PROCEDURE(wiBrowseH, wFillH, wiCaller).
      END TRIGGERS.
END PROCEDURE.

PROCEDURE ViewSearch:
   DEF INPUT PARAMETER wiBrowseName AS CHAR NO-UNDO.
   FOR EACH tSearch:
      IF tSearch.BrowseH:NAME = wiBrowseName THEN DO:
         IF VALID-HANDLE(tSearch.FillSearcH) THEN
            ASSIGN tSearch.FillSearcH:HIDDEN = NO.
         IF VALID-HANDLE(tSearch.BtnSearcH) THEN
            ASSIGN tSearch.BtnSearcH:HIDDEN = NO.
         RETURN NO-APPLY.
      END.
   END.
END.

PROCEDURE InitSearch:
   DEF INPUT PARAMETER wiBrowseH AS HANDLE NO-UNDO.

   DEF VAR wh AS HANDLE NO-UNDO.

   FIND tSearch WHERE tSearch.BrowseH = wiBrowseH NO-ERROR.
   IF NOT AVAIL tSearch THEN RETURN NO-APPLY.
   IF tSearch.FillSearchH:PRIVATE-DATA = "N" AND INDEX("0123456789",KEYFUNCT(LASTKEY)) = 0 THEN DO:
      BELL.
      RETURN NO-APPLY.
   END.
   APPLY "ENTRY" TO tSearch.FillSearchH.
   APPLY LASTKEY.
   ASSIGN tSearch.FillSearcH:CURSOR-OFFSET = 2.
END PROCEDURE.

PROCEDURE LockWindowUpdate EXTERNAL "user32.dll" :
  DEF INPUT  PARAMETER hWndLock AS LONG.
  DEF RETURN PARAMETER IsLocked AS LONG.
END PROCEDURE.

PROCEDURE Searching:
   DEF INPUT PARAMETER wiBrowseH AS HANDLE NO-UNDO.
   DEF INPUT PARAMETER wiFillH   AS HANDLE NO-UNDO.
   DEF INPUT PARAMETER wiCaller  AS HANDLE NO-UNDO.
   DEF VAR wTestDato AS DATE  NO-UNDO.
   DEF VAR wRowid    AS ROWID NO-UNDO.
   IF wiFillH:FORMAT = "99.99.99" THEN DO:
      ASSIGN wTestDato = DATE(wiFillH:SCREEN-VALUE) NO-ERROR.
      IF ERROR-STATUS:ERROR THEN DO:
         APPLY "ENTRY" TO wiFillH.
         RETURN NO-APPLY.
      END.
   END.
   FIND tCurrSort WHERE tCurrSort.BrowseH = wiBrowseH.
   FIND tSort     WHERE tSort.BrowseH = wiBrowseH AND tSort.Teller# = tCurrSort.Teller#.
   RUN VALUE("FindSearch" + wiBrowseH:NAME) IN wiCaller (wiFillH:SCREEN-VALUE, tCurrSort.ColH:NAME, tSort.Seq#, OUTPUT wRowid).
   IF wRowid <> ? THEN DO:
      tCurrSort.QueryH:REPOSITION-TO-ROWID(wRowid) NO-ERROR.
      IF ERROR-STATUS:NUM-MESSAGES = 0 THEN DO:
         APPLY "ENTRY"         TO wiBrowseH.
         APPLY "VALUE-CHANGED" TO wiBrowseH.
      END.
   END.
   RETURN NO-APPLY.
END PROCEDURE.

PROCEDURE SettFormat:
   DEF INPUT PARAMETER wiBrowseH AS HANDLE NO-UNDO.
   DEF INPUT PARAMETER wiFillH   AS HANDLE NO-UNDO.

   DEF VAR wFormat    AS CHAR NO-UNDO INIT "X(35)".
   DEF VAR wPrivData  AS CHAR NO-UNDO INIT "A".

   IF wiFillH:SCREEN-VALUE <> "" THEN RETURN.

   FIND tCurrSort WHERE tCurrSort.BrowseH = wiBrowseH NO-ERROR.
   IF AVAIL tCurrSort AND VALID-HANDLE(tCurrSort.ColH) THEN DO:
      IF tCurrSort.ColH:DATA-TYPE = "DATE" THEN
           ASSIGN wFormat = "99.99.99".
      ELSE
      IF NOT tCurrSort.ColH:DATA-TYPE BEGINS "CHAR" THEN
         ASSIGN wFormat = REPLACE(REPLACE(REPLACE(REPLACE(REPLACE(tCurrSort.ColH:FORMAT,"-",""),",",""),"9","X"),"z","X"),">","X").

      ASSIGN wPrivData = STRING(tCurrSort.ColH:DATA-TYPE BEGINS "CHAR","A/N").
   END.
   ASSIGN wiFillH:FORMAT       = wFormat
          wiFillH:PRIVATE-DATA = wPrivData.
END PROCEDURE.

PROCEDURE SjekkInput:
   DEF INPUT PARAMETER wiFillH AS HANDLE NO-UNDO.
   IF wiFillH:PRIVATE-DATA = "N" AND INDEX("0123456789",KEYFUNCT(LASTKEY)) = 0 THEN DO:
      BELL.
      RETURN NO-APPLY.
   END.
END PROCEDURE.

PROCEDURE Cleanup:
   FOR EACH tSort:
      IF NOT VALID-HANDLE(tSort.BrowseH) THEN DELETE tSort.
   END.
   FOR EACH tSearch:
      IF NOT VALID-HANDLE(tSearch.BrowseH) THEN DELETE tSearch.
   END.
   FOR EACH tCurrSort:
      IF NOT VALID-HANDLE(tCurrSort.BrowseH) THEN DELETE tCurrSort.
   END.
END PROCEDURE. /* Cleanp */

