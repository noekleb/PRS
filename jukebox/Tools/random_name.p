DEF OUTPUT PARAM cName AS CHAR NO-UNDO.

DEF VAR ix AS INT NO-UNDO.

DO ix = 1 TO RANDOM(5,12):
  cName = cName + CHR(RANDOM(97,122)).
END.
cName = cName + " ".
DO ix = 1 TO RANDOM(8,16):
  cName = cName + CHR(RANDOM(97,122)).
END.
ASSIGN SUBSTR(cName,1,1) = CAPS(SUBSTR(cName,1,1))
       SUBSTR(cName,INDEX(cName," ") + 1,1) = CAPS(SUBSTR(cName,INDEX(cName," ") + 1,1))
       .
