CURRENT-WINDOW:WIDTH = 300.

DEF VAR cEDBSystem  AS CHAR NO-UNDO.
DEF VAR cArtnumList AS CHAR NO-UNDO.
DEF VAR iAnt        AS INT  FORMAT ">>>>>>>>9" NO-UNDO.
DEF VAR iLoop       AS INT  FORMAT ">>>>>>>>9" NO-UNDO.
DEF VAR iSkip       AS INT  FORMAT ">>>>>>>>9" NO-UNDO.
DEF VAR bOk         AS LOG  NO-UNDO.

ARTLOOP:
FOR EACH ArtBas NO-LOCK WHERE
    ArtBas.RegistrertDato >= 01/01/2009 AND 
    CAN-FIND(FIRST Strekkode OF ArtBas):
    iLoop = iLoop + 1.

    /* Overfører ArtBas til VPI register. */
    RUN artbas_til_vpi.p (1,ArtBas.ArtikkelNr).
END. /* ARTLOOP */

ARTLOOP2:
FOR EACH ArtBas NO-LOCK WHERE
    ArtBas.EDato >= 01/01/2009 AND 
    CAN-FIND(FIRST Strekkode OF ArtBas) AND 
    NOT CAN-FIND(FIRST VPIArtBas WHERE
                 VPIArtBas.EkstVPILevNr = 1 AND
                 VPIArtBas.ArtikkelNr = ArtBas.ArtikkelNr):
    iSkip = iSkip + 1.

    /* Overfører ArtBas til VPI register. */
    RUN artbas_til_vpi.p (1,ArtBas.ArtikkelNr).
END. /* ARTLOOP2 */




