/*
    File        : start.w
    Purpose     : Oppstart av SkoTex

    Syntax      : start.w

    Description : Rutinen utf›rer f›lgende:
                    1. Oppstart valg av database og innloggingskontroll.
                    2. Oppstart av prosedurebiblotek.
                    3. Oppstart av ModulMeny - Hovedmeny som benyttes av bruker.
                    4. Nedkj›ring av systemet. Rydder opp og sletter alle
                       aktive (Peristente programmer) f›r sesjonen avsluttes.
                       

    Author(s)   : Tom N›kleby
    Created     : 11/6-98
    Notes       :

        TN  8/3-99      Splittet rutinen i to for å ungå at databasene må være oppkoblet.

	Last change:  TN   22 Sep 99   10:09 am
*/
IF SEARCH(".\DRIFTSTOPP.txt") <> ? THEN DO:
    RUN w-driftstopp.w (".\DRIFTSTOPP.txt") NO-ERROR.
    QUIT.
END.

DEFINE VARIABLE hOversett2SE AS HANDLE      NO-UNDO.
DEFINE VARIABLE cPgm         AS CHARACTER   NO-UNDO.
DEFINE VARIABLE ii AS INTEGER     NO-UNDO.
{adecomm/appserv.i}

/* Definerer variabler som benyttes av gamle SkoTex programmer. */
/*{syscom.i " " new global}.*/
    RUN keyfunk.p. /* Setter opp tangentbordet. */

    RUN w-login.w. /* Logger inn i applikasjonen */

    IF RETURN-VALUE <> "OK" THEN
      QUIT.        /* Avslutter hvis bruker ikke klarte å logge seg inn. */
    /* Initiering av JukeBox */
    RUN initjukebox.p.
    DYNAMIC-FUNCTION('setEnableColor',NO).
    DYNAMIC-FUNCTION("setAttribute",SESSION,"btnPanelHeight","small").
    DYNAMIC-FUNCTION("setAttribute",SESSION,"SE_PRINTER",SESSION:PRINTER-NAME).

    RUN OversettGrid2SE.p PERSISTENT SET hOversett2SE.
    SESSION:ADD-SUPER-PROCEDURE(hOversett2SE).

    DO ii = 1 TO NUM-ENTRIES(SESSION:PARAMETER):
        IF ENTRY(1,ENTRY(ii,SESSION:PARAMETER),"=") = "PROGRAM" THEN
            cPgm = ENTRY(2,ENTRY(ii,SESSION:PARAMETER),"=").
    END.
    RUN START2.p (cPgm). /* Starter oppkjøring av systemet. */



