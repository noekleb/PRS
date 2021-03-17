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

	Last change:  TN   22 Aug 101   12:14 pm
*/

DEF INPUT PARAMETER cProgram AS CHAR NO-UNDO.
DEF VAR hProgram AS HANDLE NO-UNDO.
DEF VAR hSuperBibl   AS HANDLE NO-UNDO.

/* Definerer variabler som benyttes av gamle SkoTex programmer. */
/* {syscom.i " "}. */
{dproclibstart.i} /* SDO Procedurebibliotek            */
{runlib.i}        /* WIN Oppstart av prosedurebiblotek */

&SCOP SilentProc JA
/*
{lng.i &New = "NEW GLOBAL" &NewCode = "ASSIGN wCurrLng = 'DES'."}
*/
    DEF NEW SHARED VAR wCurrLng   AS CHAR   INITIAL "DES" NO-UNDO.
    DEF NEW SHARED VAR wLngHandle AS HANDLE NO-UNDO.
    RUN GetLng IN h_dproclib (OUTPUT wCurrLng).
    RUN GetLngHandle IN h_dproclib (OUTPUT wLngHandle).
/* ------------------*/

/* Starter funksjonsbibliotek. */
RUN superBibl.p PERSIST SET hSuperBibl.
SESSION:ADD-SUPER-PROC(hSuperBibl).

IF VALID-HANDLE(h_dproclib) THEN
    RUN SetLng IN h_dproclib (INPUT wCurrLng).

IF NOT VALID-HANDLE(wLngHandle) THEN 
    RUN lng.p PERSISTENT SET wLngHandle.
IF VALID-HANDLE(h_dproclib) THEN
    RUN SetLngHandle IN h_dproclib (INPUT wLngHandle).

/* Initierer clipboard for å ungå feil ved paste av bilde. */
assign
  CLIPBOARD:VALUE = "SkoTex"
  cProgram        = IF cProgram = ""
                      THEN 'w-modul.w'
                      ELSE cProgram
  .

/* Initiering av systemtabeller */
RUN sysinit.p.

/* Setter RETURN til å virke som TAB */
if  VALID-HANDLE(wLibHandle) then
  RUN DataEntryReturn in wLibHandle ("True").

{swn.i}

IF cProgram = 'w-modul.w' THEN
DO:
    RUN value(cProgram).
    RUN AvsluttSystem.   /* Nedkjøring av systemet. Sletter alle persistente programmer. */
    quit.        /* Avslutter. */
END.
ELSE  DO:
    /* For programmer som kjører AutoLogin */
    RUN value(cProgram) PERSISTENT SET hProgram.
    SUBSCRIBE "InvalidateHandle" IN hProgram.
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
    QUIT.
END.

/* ----------------- Proceduredefinisjoner ----------------------- */
PROCEDURE InvalidateHandle:
    DEF INPUT PARAMETER hProgram AS HANDLE NO-UNDO.
  QUIT.
END.
PROCEDURE AvsluttSystem: /* Avsluttningsprosedure */

  if  VALID-HANDLE(wLibHandle) then
    DO:
      RUN StoppProgramListe in wLibHandle. /* Stopper aktive programmer */
      {stopplib.i &Stopp = "Stopp"}                  /* Stopp av prosedurebiblotek. */
    END.

  DYNAMIC-FUNCTION("ReleaseExcelHandle").
END PROCEDURE.

/* ------------------- Procedurer ferdig ------------------------- */

