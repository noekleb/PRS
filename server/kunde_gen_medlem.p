/* Kopier artikkel 
   Parametere: Aksjon (new|edit|copy),artikkelnr,prisprofil
               temp-tabell med overstyrte verdier for ny artikkel
   
   Opprettet: 17.11.04 av BHa                  
-----------------------------------------------------------------------------------*/
DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.


DEF VAR plLoop      AS DEC  NO-UNDO.
DEF VAR plKundeNr   AS DEC  NO-UNDO.
DEF VAR plMedlemsNr AS DEC NO-UNDO.
DEF VAR bOk         AS LOG  NO-UNDO.
DEF VAR cMsgs       AS CHAR NO-UNDO.
DEF VAR piAntKort   AS INT  NO-UNDO.
DEF VAR plKortNr    AS DEC  NO-UNDO.

ASSIGN
    plKundeNr = dec(ENTRY(1,icParam,"|"))
    .

DO WITH FRAME Default-frame TRANSACTION:
    /* NB NB NB Bytt ut med trelagsprogramering */
    FIND Kunde EXCLUSIVE-LOCK WHERE
        Kunde.KundeNr = plKundeNr NO-ERROR.
    /* Sletter kundekortet, da dette blir opprettet etterpå igjen. */
    /* Benytter bare nummeret.                                     */
    DO:
        FIND FIRST KundeKort EXCLUSIVE-LOCK where
            KundeKort.KundeNr = plKundeNr NO-ERROR.
        IF AVAILABLE Kundekort THEN
        DO:
            plKortNr = dec(KundeKort.KortNr).
            DELETE KundeKort.
        END.
        ELSE
            plKortNr = 0.
    END.
    DO plLoop = plKortNr TO plKortNr:
        /* Bare medlem skal opprettes. */
        RUN genkundeMedlem.p (Kunde.ButikkNr,
                              Kunde.GruppeId,
                              INPUT-OUTPUT plKundeNr,
                              OUTPUT plMedlemsNr,
                              OUTPUT bOk,
                              OUTPUT cMsgs).
        /* Kundekortene legges opp på samme kunde, men medlemskortene */
        /* legges på separate medlemmer.                              */
        RUN genkundekort_og_medlem.p (Kunde.ButikkNr,
                                      plKundeNr,
                                      plMedlemsNr,
                                      plLoop,
                                      plLoop,
                                      720,
                                      OUTPUT bOk,
                                      OUTPUT cMsgs).
        IF bOk THEN piAntKort = piAntKort + 1.
    END.

    /* Flytter på kundeinfo til kundekort. */
    ASSIGN
        Kunde.ETid = TIME + 1
        .
    FIND Medlem WHERE
        Medlem.MedlemsNr = plMedlemsNr EXCLUSIVE-LOCK NO-ERROR.
    IF AVAILABLE Medlem THEN
        assign
        Medlem.ForNavn   = Kunde.Navn
        Medlem.EtterNavn = "".
END. /* TRANSACTION */

