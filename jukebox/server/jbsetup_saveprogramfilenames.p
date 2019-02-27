DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.

DEF VAR ix               AS INT NO-UNDO.
DEF VAR bOK              AS LOG NO-UNDO.
DEF VAR iModuleId        AS INT NO-UNDO.
DEF VAR cPrograms        AS CHAR NO-UNDO.
DEF VAR cFileName        AS CHAR NO-UNDO.
DEF VAR cFilePath        AS CHAR NO-UNDO.

ASSIGN iModuleId = INT(ENTRY(1,icParam,"|"))
       cPrograms = ENTRY(2,icParam,"|")
       .
DO ix = 1 TO NUM-ENTRIES(cPrograms):
  ASSIGN cFileName = SUBSTR(ENTRY(ix,cPrograms),R-INDEX(ENTRY(ix,cPrograms),"\") + 1)
         cFilePath = SUBSTR(ENTRY(ix,cPrograms),1,R-INDEX(ENTRY(ix,cPrograms),"\"))
         .
  FIND FIRST JBoxProgram
       WHERE JBoxProgram.cProgramFile = cFileName
       NO-LOCK NO-ERROR.
  IF AVAIL JBoxProgram AND JBoxProgram.iJBoxModuleId NE iModuleId THEN DO:
    FIND FIRST JBoxModule 
         WHERE JBoxModule.iJBoxModuleId = JBoxProgram.iJBoxModuleId
         NO-LOCK NO-ERROR.
    ocReturn = ocReturn + cFileName + (IF AVAIL JBoxModule THEN "  (" + JBoxModule.cModuleDescription + ")" ELSE "") + CHR(10).
  END.
  ELSE IF AVAIL JBoxProgram THEN NEXT.

  CREATE JBoxProgram.
  ASSIGN JBoxProgram.cProgramFile  = cFileName
         JBoxProgram.cPath         = cFilePath
         JBoxProgram.iJBoxModuleId = iModuleId
         .
END.

IF ocReturn NE "" THEN
  ocReturn = "Note that these programs were already registrered in other modules:" + CHR(10) + ocReturn.
ELSE obOK = TRUE.
