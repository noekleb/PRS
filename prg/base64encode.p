
/* Quick - base64 encode procedure for version 9 */ 

DEFINE INPUT PARAMETER cInputFile  AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER cOutputFile AS CHAR NO-UNDO.

DEFINE VARIABLE rrr AS RAW  NO-UNDO.
DEFINE VARIABLE ccc AS CHAR NO-UNDO.
DEFINE VARIABLE iii AS INT  NO-UNDO.

INPUT  FROM VALUE(cInputFile) BINARY NO-ECHO NO-MAP NO-CONVERT.
OUTPUT TO   VALUE(cOutputFile).

LENGTH(rrr) = 342.
REPEAT:
  IMPORT UNFORMATTED rrr.
  ASSIGN
      ccc = STRING(rrr)
      iii = LENGTH(ccc).

  PUT CONTROL
      (if iii gt   7 then substring(ccc,   7, 76, 'RAW':U) + '~r~n':U else '':U) +
      (if iii gt  83 then substring(ccc,  83, 76, 'RAW':U) + '~r~n':U else '':U) +
      (if iii gt 159 then substring(ccc, 159, 76, 'RAW':U) + '~r~n':U else '':U) +
      (if iii gt 235 then substring(ccc, 235, 76, 'RAW':U) + '~r~n':U else '':U) +
      (if iii gt 311 then substring(ccc, 311, 76, 'RAW':U) + '~r~n':U else '':U) +
      (if iii gt 387 then substring(ccc, 387, 76, 'RAW':U) + '~r~n':U else '':U).
END.
LENGTH(rrr) = 0.

INPUT  close.
OUTPUT close.
