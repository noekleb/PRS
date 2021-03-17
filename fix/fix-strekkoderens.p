CURRENT-WINDOW:WIDTH = 250.

DEF VAR iAntall AS INT NO-UNDO.
DEF STREAM Ut.
OUTPUT STREAM ut TO VALUE("EAN-Rens.txt") APPEND.
FOR EACH ArtBas NO-LOCK WHERE ArtBas.RegistrertDato < 01/01/2004:
    FOR EACH Strekkode OF ArtBas WHERE
    /*     Strekkode.ArtikkelNr = 9001024 AND */
    /*    Strekkode.Kode BEGINS "02" */ :
        FIND StrKonv NO-LOCK WHERE
            StrKonv.StrKode = Strekkode.StrKode NO-ERROR.

        IF AVAILABLE StrKonv THEN
            FIND FIRST TransLogg NO-LOCK WHERE
                   Translogg.ArtikkelNr = STrekkode.ArtikkelNr AND
                   Translogg.Butik      > 0 AND
                   trim(Translogg.Storl) = trim(StrKonv.Storl) NO-ERROR.

    /*     DISPLAY                                      */
    /*         Strekkode.ArtikkelNr                     */
    /*         (IF AVAILABLE ArtBAs                     */
    /*            THEN ArtBas.Beskr                     */
    /*            ELSE "** Feil")                       */
    /*         Strekkode.Kode                           */
    /*         Strekkode.StrKode                        */
    /*         (IF AVAILABLE StrKonv                    */
    /*            THEN StrKonv.Storl                    */
    /*          ELSE IF Strekkode.StrKode = 0           */
    /*              THEN " "                            */
    /*          ELSE "** Feil") FORMAT "x(20)"          */
    /*         "*" WHEN NOT AVAILABLE Translogg         */
    /*         TransLogg.Butik WHEN AVAILABLE Translogg */
    /*         iAntall                                  */
    /*         WITH WIDTH 248                           */
    /*         .                                        */

        IF Strekkode.StrKode > 0 AND NOT AVAILABLE Translogg THEN
        DO:
            iAntall = iAntall + 1.
            EXPORT STREAM Ut Strekkode.
            DELETE strekkode.
        END.
    END.
END.
DISPLAY iAntall.
OUTPUT STREAM ut CLOSE.
