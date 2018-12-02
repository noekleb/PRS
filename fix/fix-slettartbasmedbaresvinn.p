CURRENT-WINDOW:WIDTH = 250.

DEF VAR piAnt AS INT NO-UNDO.
DEF VAR piArt AS INT NO-UNDO.

FOR EACH ArtBas NO-LOCK WHERE
    ArtBAs.OPris = FALSE AND
    ArtBAs.RegistrertDato < 01/01/2004 AND
    NOT CAN-FIND(FIRST TransLogg WHERE
             Translogg.ArtikkelNr = ArtBas.ArtikkelNr AND
             TransLogg.Butik > 0 AND
             Translogg.TTId <> 9) AND
    CAN-FIND(FIRST TransLogg WHERE
             Translogg.ArtikkelNr = ArtBas.ArtikkelNr AND
             TransLogg.Butik > 0 AND
             Translogg.TTId = 9) AND
    NOT CAN-FIND(FIRST Lager OF ArtBAs WHERE
                 Lager.LAgant > 0):
    FIND FIRST LAger OF ArtBAs NO-LOCK WHERE
        Lager.LagAnt > 0 NO-ERROR.
/*     DISPLAY                               */
/*         ArtBas.RegistrertDato             */
/*         ArtBas.ArtikkelNr                 */
/*         ArtBAs.Beskr                      */
/*         Lager.Lagant WHEN AVAILABLE Lager */
/*         .                                 */

    piArt = piArt + 1.
    FOR EACH Strekkode OF ArtBAs:
        piAnt = piAnt + 1.
        DELETE Strekkode.
    END.

END.
DISPLAY
    "Artikkler:" piArt SKIP
    "Strekkoder: " piAnt
    .
