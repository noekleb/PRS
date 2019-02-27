/* getWinOsVer.p
Get windows version:
5: XP/2003
6: 7/2008
*/

DEF OUTPUT PARAM ocVer AS CHAR NO-UNDO INIT "99999".

DEF VAR cVersion AS CHAR NO-UNDO.
DEF VAR ix       AS INT  NO-UNDO.
DEF VAR bAccess  AS LOG  NO-UNDO.

OS-COMMAND SILENT VALUE("ver > " + SESSION:TEMP-DIR + "osver.txt").

INPUT FROM VALUE(SESSION:TEMP-DIR + "osver.txt").
SET ^.
IMPORT UNFORMATTED cVersion.
INPUT CLOSE.

DO ix = 2 TO NUM-ENTRIES(cVersion," "):
  IF ENTRY(ix - 1,cVersion," ") BEGINS "[Ver" THEN DO:
    ocVer = ENTRY(ix,cVersion," ").
    LEAVE.
  END.
END.
IF ocVer BEGINS "1" THEN ocVer = "9". 
 
/* Check also access to .net classes: */
IF ocVer GE "6" AND (SEARCH("checkDotNetAccess.p") NE ? OR SEARCH("checkDotNetAccess.r") NE ?) THEN DO:
  RUN checkDotNetAccess.p (OUTPUT bAccess) NO-ERROR.
  IF NOT bAccess OR bAccess = ? THEN ocVer = "0 (no dotnet access)".
END.
