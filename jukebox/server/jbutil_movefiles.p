/* Endret 25/01/05 av BHa:
   Flytter filer via innlesing i MEMPTR variabel for å takle null-tegn i importfiler fra SAP 4.6 
----------------------------------------------------------------------------*/
DEF INPUT  PARAM cFileList  AS CHAR NO-UNDO.
DEF INPUT  PARAM cTargetCat AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn   AS CHAR NO-UNDO.       

DEF VAR ix          AS INT NO-UNDO.
DEF VAR cFileToMove AS CHAR NO-UNDO.
DEF VAR memFile     AS MEMPTR NO-UNDO.
DEF VAR iBytes      AS INT NO-UNDO.
DEF VAR cTargetFile AS CHAR NO-UNDO.

DO ix = 1 TO NUM-ENTRIES(cFileList,";"):
  cFileToMove = ENTRY(ix,cFileList,";").
  IF SEARCH(cFileToMove) NE ? THEN DO:

    SET-SIZE(memFile) = 0.
    FILE-INFO:FILE-NAME = cFileToMove.
    SET-SIZE(memFile) = 4 + FILE-INFO:FILE-SIZE.
    PUT-LONG(memFile, 1) = FILE-INFO:FILE-SIZE.

    INPUT FROM VALUE(cFileToMove) BINARY NO-CONVERT.
    IMPORT memFile.
    INPUT CLOSE.
    
    IF OPSYS = "UNIX" THEN
      cTargetFile = RIGHT-TRIM(cTargetCat,"/") + "/" + SUBSTR(cFileToMove,R-INDEX(cFileToMove,"/") + 1).
    ELSE
      cTargetFile = RIGHT-TRIM(cTargetCat,"\") + "\" + SUBSTR(cFileToMove,R-INDEX(cFileToMove,"\") + 1).
    OUTPUT TO VALUE(cTargetFile).
    iBytes = 0.
    REPEAT:
      iBytes = iBytes + 1.
      PUT UNFORMATTED GET-STRING(memFile, iBytes,1).
      IF iBytes >= GET-SIZE(memFile) THEN LEAVE.
    END.
    OUTPUT CLOSE.

    OS-DELETE VALUE (cFileToMove).
    IF OS-ERROR NE 0 THEN DO:
      ocReturn =
          "Error in OS-DELETE " + cFileToMove + CHR(10) +
          "         Error code: " + STRING(OS-ERROR) + " (See Progress help on OS-ERROR codes)" + CHR(10) +
          "         Procedure:  " + PROGRAM-NAME(1). 
    END.
  END.
END.

