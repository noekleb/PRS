/*

Kontroll av vareregister fra MxSport mot VPI strekkode register.
Artikler som ikke finnes i VPI strekkode, legges ut en fy fil.
MxLaguneparjen 27/10-07
*/


DEF VAR cinnFilNavn AS CHAR NO-UNDO.
DEF VAR cutFilNavn  AS CHAR NO-UNDO.

DEF VAR cLinje      AS CHAR FORMAT "x(170)" NO-UNDO.
DEF VAR piLoop      AS INT                  NO-UNDO.
DEF VAR piTot       AS INT                  NO-UNDO.
DEF VAR piEksport   AS INT                  NO-UNDO.
DEF VAR cEan        AS CHAR FORMAT "x(20)"  NO-UNDO.
DEF VAR lEan        AS DEC  FORMAT ">>>>>9999999999999" NO-UNDO.

CURRENT-WINDOW:WIDTH = 240.


ASSIGN
    cinnFilNavn = "C:\ArkivDokument\LRS\Kunder & Prospects\Norge\Kunder\MxSport\Vaeregister_ny_oppstart\Varereg_Laguneparken_261007.csv"
    cutFilNavn  = "C:\ArkivDokument\LRS\Kunder & Prospects\Norge\Kunder\MxSport\Vaeregister_ny_oppstart\Varereg_Laguneparken_delta_271007.csv"
    .

DEF STREAM Inn.
DEF STREAM Ut.

/* FORM                  */
/*     WITH FRAME Gurre. */

INPUT  STREAM Inn from VALUE(cInnFilNavn) NO-ECHO.
OUTPUT STREAM Ut  TO   VALUE(cUtFilNavn) NO-ECHO.

LESBLOKK:
REPEAT:
    IMPORT STREAM Inn UNFORMATTED cLinje .

    ASSIGN
        piLoop = piLoop + 1
        piTot  = piTot + 1
        cEan   = ENTRY(4,cLinje,";")
        NO-ERROR.

    /* Finnes strekkoden i VPI regsiteret, skal ikke denne linjen ut i Delta filen */
    IF NOT CAN-FIND(FIRST VPIStrekkode WHERE
                    VPISTrekkode.EkstVPILevNr = 110 AND
                    VPIStrekkode.Kode         = cEAN) THEN
    DO:
        PUT STREAM Ut UNFORMATTED cLinje SKIP.
        piEksport = piEksport + 1.
    END.

    
    PAUSE 0.
    DISPLAY 
        piTot
        piEksport
        cEan
        CAN-FIND(FIRST VPIStrekkode WHERE
                    VPISTrekkode.EkstVPILevNr = 110 AND
                    VPIStrekkode.Kode         = cEAN)
        cLinje
        WITH FRAME G WIDTH 240.

    /* TEST */
    /*
    IF piLoop > 100 THEN
        LEAVE LESBLOKK.
    */
END. /* LESBLOKK */

OUTPUT STREAM Ut  CLOSE.
INPUT  STREAM Inn CLOSE.

DISPLAY pitot piEksport.

