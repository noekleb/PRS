{ xprint.i }

DEF VAR M AS MEMPTR NO-UNDO.
DEF VAR A AS CHAR   NO-UNDO.

SET-SIZE(M) = 256.                       
RUN printerDialog( M ).

A = GET-STRING(M, 1).

SET-SIZE(M) = 0.

MESSAGE "Printer name :" ENTRY(1, A)
        SKIP
        "Format Name :" ENTRY(2, A)
        SKIP(1)
        "Dim :" ENTRY(3, A) "x" ENTRY(4, A) "(tenth mm)"
        SKIP
        "Orientation :" ENTRY(5, A)
        VIEW-AS ALERT-BOX INFO.
        
