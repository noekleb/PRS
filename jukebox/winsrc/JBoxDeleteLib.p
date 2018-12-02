/* Delete one or many libraries */               

DEF INPUT PARAM icLibList AS CHAR NO-UNDO.
DEF INPUT PARAM ibRestart AS LOG  NO-UNDO.

DEF VAR ix           AS INT    NO-UNDO.
                   
FUNCTION DeleteLib RETURNS LOGICAL
  (INPUT ihProc     AS HANDLE,
   INPUT icLibName  AS CHAR):

  IF NOT VALID-HANDLE(ihProc) THEN
    RETURN NO.
  ELSE REPEAT WHILE VALID-HANDLE(ihProc):
    IF ihProc:FILE-NAME = icLibName THEN DELETE PROCEDURE ihProc.
    ELSE ihProc = ihProc:NEXT-SIBLING.
  END.
  RETURN NO.
END FUNCTION.


DO ix = 1 TO NUM-ENTRIES(icLibList):
  DeleteLib(SESSION:FIRST-PROCEDURE,ENTRY(ix,icLibList)).
END.
IF ibRestart THEN
  RUN JBoxLoadLib.p (icLibList).

