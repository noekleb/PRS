/*
Leser inn strekkoder fra Laguneparken.
MxLaguneparjen 27/10-07
*/


DEF VAR cinnFilNavn AS CHAR NO-UNDO.
DEF VAR cutFilNavn  AS CHAR NO-UNDO.

DEF VAR cLinje      AS CHAR FORMAT "x(170)" NO-UNDO.
DEF VAR piLoop      AS INT                  NO-UNDO.
DEF VAR cEan        AS CHAR FORMAT "x(20)"  NO-UNDO.
DEF VAR lEan        AS DEC  FORMAT ">>>>>9999999999999" NO-UNDO.
DEF VAR cArtikkelNr AS CHAR FORMAT "x(30)"  no-undo. 

CURRENT-WINDOW:WIDTH = 240.


ASSIGN
    cinnFilNavn = "C:\ArkivDokument\LRS\Kunder & Prospects\Norge\Kunder\MxSport\Vaeregister_ny_oppstart\VPI010-pricat_fra_Mx_SE_26102007.csv"
    .

DEF STREAM Inn.

FORM 
    WITH FRAME Gurre.

INPUT  STREAM Inn from VALUE(cInnFilNavn) NO-ECHO.

LESBLOKK:
REPEAT:
    IMPORT STREAM Inn UNFORMATTED cLinje .

    ASSIGN
        piLoop      = piLoop + 1
        cEan        = ENTRY(4,cLinje,";")
        cArtikkelNr = ENTRY(36,cLinje,";")
        .

    SJEKKMOTVPI:
    DO:
        /* Finnes strekkoden i VPI regsiteret, skal ikke denne linjen ut i Delta filen */
        IF NOT CAN-FIND(VPIStrekkode WHERE
                        VPISTrekkode.EkstVPILevNr = 110 AND
                        VPISTrekkode.VareNr       = cArtikkelNr AND
                        VPIStrekkode.Kode         = cEAN) THEN
        DO:
            CREATE VPIStrekkode.
            ASSIGN
                VPISTrekkode.EkstVPILevNr = 110
                VPISTrekkode.VareNr       = cArtikkelNr
                VPIStrekkode.Kode         = cEAN
                .
        END.

    END. /* SJEKKMOTVPI */

    PAUSE 0.
    DISPLAY 
        piLoop
        cEan
        cArtikkelNr
        WITH FRAME Gurre WIDTH 240.

    /* TEST 
    IF piLoop > 100 THEN
        LEAVE LESBLOKK.
    */
END. /* LESBLOKK */

INPUT  STREAM Inn CLOSE.

