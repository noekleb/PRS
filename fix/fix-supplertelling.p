{incl/DevMode.i}
{incl/CustDevMode.i}

DEF VAR iTelleNr  AS INT NO-UNDO.
DEF VAR i2TelleNr AS INT NO-UNDO.

DEFINE TEMP-TABLE TT_Suppler NO-UNDO
    FIELD Artikkelnr LIKE ArtBas.Artikkelnr
    FIELD Vg         LIKE ArtBas.Vg.

ASSIGN
    iTelleNr  = 0
    i2TelleNr = 62
    .
IF iTelleNr = 0 THEN
DO TRANSACTION:
    FIND LAST Tellehode NO-LOCK  NO-ERROR.
    IF AVAILABLE TelleHode THEN
        iTelleNr = TelleHode.TelleNr + 1.
    ELSE
        iTelleNr = 1.
    CREATE TelleHode.
    ASSIGN
        TelleHode.TelleNr     = iTelleNr
        TelleHode.Beskrivelse = "FIX - Supplert artikkler"
        TelleHode.TTId        = 9
        TelleHode.TBId        = 1
        TelleHode.StartDato   = TODAY
        TelleHode.ButikkListe = "174"
        .
    FIND CURRENT tellehode NO-LOCK.
END.
ELSE
    FIND TelleHode NO-LOCK WHERE
        TelleHode.TelleNr = iTelleNr NO-ERROR.
IF NOT AVAILABLE TelleHode THEN
DO:
    MESSAGE 
    program-name(1) 
    SKIP "Ukjent telleing " iTelleNr 
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
    RETURN.
END.

MESSAGE 
program-name(1) 
SKIP TelleHode.TelleNr TelleHode.Beskrivelse 
VIEW-AS ALERT-BOX INFO BUTTONS OK.

RUN Suppler.

MESSAGE 
program-name(1) 
"ETter" 
VIEW-AS ALERT-BOX INFO BUTTONS OK.

PROCEDURE Suppler :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE BUFFER bTellelinje FOR TelleLinje.

    DEFINE VARIABLE hh AS HANDLE     NO-UNDO.
    DEFINE VARIABLE bOK AS LOGICAL    NO-UNDO.

    EMPTY TEMP-TABLE TT_Suppler.

    ARTIKKEL:
    FOR EACH ArtBas NO-LOCK WHERE
        ArtBas.ArtikkelNr > 9999 AND
        ArtBas.OPris = FALSE AND
        ArtBas.Lager = TRUE:

        /* Finnes artikklene i denne listen, skal de ikke legges opp i ny liste */
        IF i2TelleNr <> 0 THEN
        DO:
            IF CAN-FIND(FIRST TelleLinje WHERE
                        TelleLinje.TelleNr = i2TelleNr AND
                        TelleLinje.ArtikkelNr = ArtBas.ArtikkelNr) THEN
                NEXT ARTIKKEL.
        END.

        CREATE TT_Suppler.
        ASSIGN TT_Suppler.Artikkelnr = ArtBas.artikkelnr
               TT_Suppler.Vg         = ArtBas.Vg.

        RELEASE TT_Suppler.
    END. /* ARTIKKEL */

    hh = BUFFER TT_Suppler:HANDLE:TABLE-HANDLE.
    bOK = DYNAMIC-FUNCTION("RunProc","art_to_telling.p",STRING(TelleHode.TelleNr),hh).

END PROCEDURE.
