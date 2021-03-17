def VAR cFilNavn AS CHAR NO-UNDO.

ASSIGN
    cFilNavn = 'C:\Appdir\SkoTex\posntummert.d'
    .
DEF STREAM Inn.

DEF TEMP-TABLE bPost 
    FIELD PostNr AS CHAR
    FIELD Navn AS CHAR
 INDEX IdxPost PostNr.

CREATE bPost.

INPUT STREAM inn FROM VALUE(cFilNavn) NO-ECHO.

REPEAT:
    IMPORT STREAM inn bPost.
    CREATE Post.
    ASSIGN
        Post.PostNr      = bPost.PostNr
        Post.Beskrivelse = bPost.Navn
        Post.KommNr      = "1"
        FylkesNr         = "1"
        .
END.

INPUT STREAM Inn CLOSE.
