CURRENT-WINDOW:WIDTH = 200.
DEF VAR i AS INT NO-UNDO.
DEF VAR piLinjer AS INT NO-UNDO.
DEF VAR piLogg AS INT NO-UNDO.
DEF VAR piDatasett AS INT NO-UNDO.
DEF VAR piBonger AS INT NO-UNDO.
DEF VAR piBongLinje AS INT NO-UNDO.
DEF VAR cButikker   AS CHAR NO-UNDO.
DEF VAR piLoop      AS INT  NO-UNDO.
DEF VAR dFraDato AS DATE NO-UNDO.
DEF VAR dTilDato AS DATE NO-UNDO.
DEF VAR dDataSEttId AS DEC NO-UNDO.


ASSIGN
    cButikker = "5646,5602,5506,5316,5311,8383,8322,8304,8403," + 
                "8834,8818,8667,8655,8644,8608,8535,8512,8508," + 
                "8500,8483,8478,8457,8857"
    .

BUTIKKLOOP:
DO piLoop = 1 TO NUM-ENTRIES(cButikker):
    BUTIKK:
    FOR EACH Butiker WHERE 
        Butiker.Butik = int(ENTRY(piLoop,cButikker)):
        ASSIGN
            i = i + 1
            .
        PAUSE 0.
        DISPLAY Butiker.Butik
            Butiker.ButNamn
            piBonger
            piBongLinje
            WITH WIDTH 198
            .

        FOR EACH BongHode EXCLUSIVE-LOCK WHERE
            BongHode.ButikkNr = Butiker.Butik AND
            BongHode.GruppeNr = 1 AND
            BongHode.KasseNr >= 1 AND
            BongHode.Dato >= dFraDato AND
            BongHode.Dato <= dTilDato:

            FOR EACH BongLinje WHERE
                BongLinje.B_Id = BongHode.B_Id:
                DELETE BongLinje.
            END.
            ASSIGN
                dDataSettID = BongHode.DataSettId
                .
            DELETE BongHode.
        END.
        FIND DataSett WHERE
            Datasett.DataSEttId = dDataSettId NO-ERROR.
        IF AVAILABLE DataSett THEN
        DO:
            IF NOT CAN-FIND(FIRST BongHode OF DataSett) THEN
                DELETE DataSett.
        END.

    END. /* BUTIKK */
END. /* BUTIKKLOOP */

DISPLAY i.
