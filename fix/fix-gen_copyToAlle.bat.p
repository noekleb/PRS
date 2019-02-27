DEF VAR cInnfil AS CHAR NO-UNDO.
DEF VAR cUtfil  AS CHAR NO-UNDO.

DEF VAR cBut    AS CHAR NO-UNDO. 

ASSIGN
    cInnFil = "Sport1_butikkliste.txt"
    cUtFil  = "copyToAlleBut.bat".

DEF STREAM Inn.
DEF STREAM Ut.

INPUT STREAM inn FROM VALUE(cInnFil) NO-ECHO.
OUTPUT STREAM Ut TO VALUE(cUtFil).

REPEAT:
  IMPORT STREAM Inn cBut.

  PUT STREAM Ut UNFORMATTED 
      "COPY %1 C:\HOME\LINDBAK\SENDES\%1." + cBut 
      SKIP.

END.


OUTPUT STREAM Ut CLOSE.
INPUT STREAM Inn CLOSE.
