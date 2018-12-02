DEFINE VARIABLE iCount AS INTEGER    NO-UNDO.
DEFINE VARIABLE cStr   AS CHARACTER  NO-UNDO.
OUTPUT TO "c:\temp\butkontr.txt".
PUT UNFORMATTED "Butiknr" CHR(9) "Kjedebutik" CHR(9) "Butik" CHR(9) "BongHode" SKIP.
DO iCount = 1 TO 90000:
   ASSIGN cStr = STRING(iCount) + "," + 
                 string(CAN-FIND(FIRST KjedensButikker WHERE KJedensButikker.butikknr = iCount)) + "," +
                 string(CAN-FIND(Butiker WHERE Butiker.butik = iCount)) + "," +
                 string(CAN-FIND(FIRST bonghode WHERE bonghode.butikknr = iCount)).
   IF CAN-DO(cStr,"yes") THEN
       PUT UNFORMATTED REPLACE(cStr,",",CHR(9)) SKIP.
END.
OUTPUT CLOSE.
