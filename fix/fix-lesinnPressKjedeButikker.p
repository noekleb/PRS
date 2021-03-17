DEF VAR cLinje AS CHAR NO-UNDO.
DEF VAR cFilNavn AS CHAR NO-UNDO.
DEF VAR iLoop    AS INT  NO-UNDO.

ASSIGN
    cFilNavn = "c:\appdir\skotex\data\sport1\butikker.sdv"
    .
DEF STREAM Inn.

DEF BUFFER bKjedensButikker FOR KjedensButikker.

INPUT STREAM Inn FROM VALUE(cFilNavn) NO-ECHO.

MAIN:
REPEAT:
    IMPORT STREAM Inn UNFORMATTED cLinje.

    CREATE KjedensButikker.
    ASSIGN
        iLoop = iLoop + 1
        KjedensButikker.ButikkNr      = int(ENTRY(1,cLinje,";"))
        KjedensButikker.FirmaNavn     = (ENTRY(2,cLinje,";"))
        KjedensButikker.ButikkNavn    = (ENTRY(2,cLinje,";"))
        KjedensButikker.Adresse1      = (ENTRY(3,cLinje,";"))
        KjedensButikker.Adresse2      = (ENTRY(4,cLinje,";"))
        KjedensButikker.PostNr        = (ENTRY(5,cLinje,";"))
        KjedensButikker.DagligLeder   = (ENTRY(7,cLinje,";"))
        KjedensButikker.Kontakt       = (ENTRY(8,cLinje,";"))
        KjedensButikker.Telefon       = (ENTRY(9,cLinje,";"))
        KjedensButikker.Mobil         = (ENTRY(10,cLinje,";"))
        KjedensButikker.Telefaks      = (ENTRY(11,cLinje,";"))
        NO-ERROR.

    FIND Post WHERE
        Post.PostNr = KjedensButikker.PostNr NO-ERROR.
    IF NOT AVAILABLE Post THEN
    DO:
        CREATE Post.
        ASSIGN
            Post.PostNr      = ENTRY(5,clinje,";")
            Post.Beskrivelse = ENTRY(6,clinje,";")
            Post.KommNr      = "1"
            Post.FylkesNr    = "1"
            .
    END.

    IF AVAILABLE KjedensButikker THEN
        RELEASE KjedensButikker.
END.   /* MAIN */

INPUT STREAM Inn CLOSE.





