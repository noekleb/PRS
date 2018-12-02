CURRENT-WINDOW:WIDTH = 250.

DEF VAR cFilNavn AS CHAR NO-UNDO.
DEF VAR cLinje AS CHAR FORMAT "x(100)" NO-UNDO.
DEF VAR cUtFilNavn AS CHAR NO-UNDO.
ASSIGN
    cFilNavn   = "C:\Polygon\PRS\Preem_Art_Vask_Org.txt"
    cUtFilNavn = "C:\Polygon\PRS\Preem_Art_Vasket.txt".

DEF STREAM Inn.
DEF STREAM Ut.

INPUT STREAM Inn FROM VALUE(cFilNavn) NO-ECHO.
OUTPUT STREAM Ut TO VALUE(cUtFilNavn) NO-ECHO.
REPEAT:
  IMPORT STREAM Inn UNFORMATTED cLinje.

  IF num-entries(cLinje,';') > 7 THEN
  DO:
      PUT STREAM Ut UNFORMATTED cLinje SKIP.
      /*
      DISPLAY
          num-entries(cLinje,';') cLinje
          WITH WIDTH 250.
      */
  END.
END.
OUTPUT CLOSE.
INPUT STREAM Inn CLOSE.
