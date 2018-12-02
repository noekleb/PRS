CURRENT-WINDOW:WIDTH = 250.

DEF VAR piLoop AS INT NO-UNDO.
DEF VAR cNrLst AS CHAR NO-UNDO.
ASSIGN
    cNrLst = "301578,302669,302734,302812,302909,302970"
    .

DO piLoop = 1 TO NUM-ENTRIES(cNrLst):
  FIND FIRST PKSDLHode EXCLUSIVE-LOCK WHERE
    PkSdlHode.EkstID = entry(piLoop,cNrLst).
    
  
  PAUSE.
  DISPLAY 
      PKSDLHode.PkSdlId
      PKSDLHode.EkstId
      PKSDLHode.PkSdlNr
      PKSDLHode.PkSdlStatus
  
      .
  PKSDLHode.PkSdlStatus = 5.
      
END.

DO piLoop = 1 TO NUM-ENTRIES(cNrLst):
  FIND FIRST PKSDLHode EXCLUSIVE-LOCK WHERE
    PkSdlHode.EkstID = entry(piLoop,cNrLst).
    
  PKSDLHode.PkSdlStatus = 6.
END.


