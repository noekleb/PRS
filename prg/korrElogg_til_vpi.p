/* korrelogg_til_vpi.p  TN 11/1-08
   Artikler som er opprettet i butikk, skal via VPIArtBas sendes til HK.
   Denne rutinen tar loggede lokale artikler og legger inn i VPI registeret.
   Deretter logges posten for sending til HK. Selve utlegget til HK blir så gjort 
   via ELoggServer.
*/

DEF VAR ocReturn    AS CHAR NO-UNDO.
DEF VAR obOk        AS LOG  NO-UNDO.
DEF VAR iCL         AS INT  NO-UNDO.
DEF VAR iAntSlett   AS INT  NO-UNDO.
DEF VAR iAntNyEndre AS INT  NO-UNDO.
DEF VAR cEDBSystem  AS CHAR NO-UNDO.
DEFINE VARIABLE piLoop AS INTEGER NO-UNDO.

DEF BUFFER bufELogg FOR Elogg.
{syspara.i 5 1 1 iCL INT}

ASSIGN
    cEDBSystem = "KORR" + STRING(TIME)
    .

LOGGLOOP:
FOR EACH bufElogg NO-LOCK WHERE
    bufELogg.TabellNavn     = "ArtBas" AND
    bufELogg.EksterntSystem = "TILKORRPOS":

    FIND ELogg NO-LOCK WHERE
        RECID(ELogg) = RECID(bufELogg) NO-ERROR.
    IF AVAILABLE ELogg THEN
    DO:
        /* Artikkelen skal finnes og det må være lagt inn strekkoder på den før den sendes. */
        FIND ArtBas NO-LOCK WHERE
            ArtBas.ArtikkelNr = DEC(ELogg.Verdier) NO-ERROR.
        FIND FIRST Strekkode OF ArtBas NO-LOCK NO-ERROR.

        IF AVAILABLE ArtBas AND AVAILABLE Strekkode THEN
        DO:
            piLoop = piLoop + 1.
            
            /* Artikkelen skal inn i butikkens VPI register. */
            RUN artbas_til_vpi.p (1000000 + iCL,ArtBas.Artikkelnr).

            /* Logger i ELogg for sending */
            RUN create_elogg.p ('VPIArtBas',cEDBSystem,STRING(1000000 + iCL) + CHR(1) + string(ArtBas.ArtikkelNr)).

            obOk = TRUE.

            /* Legger ut 100 og 100 hvis det er mange :) */
            IF piLoop > 99 THEN DO:
              ASSIGN
                obOk = FALSE
                piLoop = 0.
              /* Så skal den legges ut på fil og sendes til HK */
              RUN vpieksport.w (cEDBSystem,
                      STRING(iCL), /* Ekstent skal være butikk som har sendt filen. */
                      0, /* Bildedata skal ikke sendes. */
                      OUTPUT iAntSlett,
                      OUTPUT iAntNyEndre).
            END.
        END.
        DO TRANSACTION:
            FIND CURRENT Elogg EXCLUSIVE-LOCK NO-ERROR.
            IF AVAILABLE Elogg AND NOT LOCKED Elogg THEN
                DELETE ELogg.
        END.
    END.
END. /* LOGGLOOP */

/* Så legges resten ut på fil og sendes til HK */
IF obOk THEN 
    RUN vpieksport.w (cEDBSystem,
                      STRING(iCL), /* Ekstent skal være butikk som har sendt filen. */
                      0, /* Bildedata skal ikke sendes. */
                      OUTPUT iAntSlett,
                      OUTPUT iAntNyEndre).
