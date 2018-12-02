/************************************************************
    Program:  inifil.i
    Created:  TN   12 Nov 98
Description:  Håndtering av inifil.

Last change:  TN   21 Sep 100    6:59 pm
************************************************************/

/* Sjekker om systemet startes opp p† en LapTop. */
PROCEDURE SjekkLapTop:
  DEF OUTPUT PARAMETER wSvar as log NO-UNDO.

  DEF VAR wTekst as CHAR NO-UNDO.


  GET-KEY-VALUE SECTION "SYSPARA" KEY "LapTopNr" VALUE wTekst.
  if wTekst = ? then
    wSvar = False.
  else
    wSvar = True.

  /* Sjekk mot LapTop tabellen.                             */
  /* Nummeret skal peke p† en gyldig post i denne tabellen. */
  if wSvar = TRUE then
    DO:
      if CAN-FIND(LapTop where
                  LapTop.LapTopNr = INT(wTekst)) then
        wSvar = TRUE.
      ELSE
        wSvar = ?.
    END.
END PROCEDURE.

/* Henter LapTopNummer fra Ini filen.                    */
/* F›r det returneres valideres det mot LapTop tabellen. */
PROCEDURE HentLapTopNr:
  DEF OUTPUT PARAMETER wLapTopNr as int NO-UNDO.

  DEF VAR wTekst as CHAR NO-UNDO.


  GET-KEY-VALUE SECTION "SYSPARA" KEY "LapTopNr" VALUE wTekst.
  if wTekst = ? then
    wLapTopNr = ?.
  else
    wLapTopNr = INT(wTekst).
  if not CAN-FIND(LapTop where
                  LapTop.LapTopNr = wLapTopNr) then
    wLapTopNr = ?.

END PROCEDURE.

/* Sjekker om det skal benyttes direkte oppdatering av pris. */
PROCEDURE DirektePrisOppdat:
  DEF OUTPUT PARAMETER wSvar as log NO-UNDO.

  DEF VAR wTekst as CHAR NO-UNDO.


/* Denne parameter skal ikke lenger benyttes..........................
  GET-KEY-VALUE SECTION "SYSPARA" KEY "DirektePrisOppdat" VALUE wTekst.
  if wTekst = ? then
    DO:
      {syspara.i 2 1 10 wtekst}
    END.
  if CAN-DO("True,Ja,Yes,1",wTekst) then
    wSvar = TRUE.
  else
    wSvar = FALSE.
*/
  wSvar = FALSE.

END PROCEDURE.

/* Henter omregningskurs for euro. */
PROCEDURE Euro:
  DEF OUTPUT PARAMETER wKurs as dec NO-UNDO.

  DEF VAR wTekst as CHAR NO-UNDO.


  GET-KEY-VALUE SECTION "SYSPARA" KEY "EuroKurs" VALUE wTekst.
  if wTekst = ? then
    DO:
      {syspara.i 2 1 1 wtekst}
    END.
  ASSIGN wKurs = DEC(wTekst).
  if wKurs = ? or wKurs = 0 then
    wKurs = 0.5.

END PROCEDURE.

/* Setter navnet på den mappe hvor applikasjonen. */
PROCEDURE Appdir:
  DEF OUTPUT PARAMETER wAppdir as CHAR NO-UNDO.

  GET-KEY-VALUE SECTION "SYSPARA" KEY "Appdir" VALUE wAppdir.
  if wAppDir = ? then
    DO:
      {syspara.i 1 1 7 wAppdir}
    END.

END PROCEDURE.

/* Setter navnet på den mappe hvor den lokale INI filen ligger. */
PROCEDURE Mappe:
  DEF OUTPUT PARAMETER wMappe as CHAR NO-UNDO.

  ASSIGN
      wMappe = RIGHT-TRIM(SESSION:TEMP-DIR,"\")
      .
/*   IF wMappe = ? then                                            */
/*     DO:                                                         */
/*       GET-KEY-VALUE SECTION "SYSPARA" KEY "Mappe" VALUE wMappe. */
/*       {syspara.i 1 1 6 wMappe}                                  */
/*     END.                                                        */
END PROCEDURE.

/* Setter navnet på den lokale INI filen. */
PROCEDURE IniFil:
  DEF OUTPUT PARAMETER wIniFil as CHAR NO-UNDO.

  wIniFil = "lokal.ini".

/*   GET-KEY-VALUE SECTION "SYSPARA" KEY "IniFil" VALUE wIniFil. */
/*   IF wIniFil = ? then                                         */
/*     DO:                                                       */
/*       {syspara.i 1 1 5 wIniFil}                               */
/*     END.                                                      */

END PROCEDURE.

/* Setter navnet på iconfilen som skal benyttes på vinduene i SkoTex. */
PROCEDURE IconFil:
  DEF OUTPUT PARAMETER wIconFil as CHAR NO-UNDO.

  GET-KEY-VALUE SECTION "SYSPARA" KEY "IconFil" VALUE wIconFil.
  IF wIconFil = ? then
    DO:
      {syspara.i 1 1 3 wIconFil}
    END.

  if SEARCH(wIconFil) <> ? then
    assign
      wIconFil = SEARCH(wIconFil).
  ELSE
    ASSIGN wIconFil = "icon\sk-ico.ico".

END PROCEDURE.

/* Laster iconfilen i vinduet. */
procedure LoadIconFil:
  DEF INPUT PARAMETER wVindu   as handle NO-UNDO.
  DEF       VAR       wIconFil as CHAR   NO-UNDO.

  /* Henter navn p† iconfil. */
  RUN IconFil in THIS-PROCEDURE (OUTPUT wIconFil).

  /* Setter inn iconfil. */
  IF wIconFil <> "" THEN
    DO:
      IF wVindu:LOAD-ICON(wIconFil) THEN.
    END.

  /*
  ASSIGN /* THIS-PROCEDURE:PRIVATE-DATA = PROPATH */
         wVindu:NAME = "{&WindowName}".
  */
END procedure.

/* Setter navnet systemet. */
PROCEDURE SystemNavn:
  DEF OUTPUT PARAMETER wSystemNavn as CHAR NO-UNDO.

  ASSIGN
    wSystemNavn = "PRS".
END PROCEDURE.

/* Henter oppkoblingsparametre for SkoTex databasen for ReportBuilder. */
PROCEDURE RpbSkoDB:
  DEF OUTPUT PARAMETER wDbListe as char NO-UNDO.

  DEF VAR wTekst as CHAR NO-UNDO.

  /* SkoTex databasen */
  GET-KEY-VALUE SECTION "SYSPARA" KEY "SkoDb" VALUE wTekst.
  if wTekst = ? then
    DO:
      {syspara.i 1 1 10 wtekst}
    END.
  ASSIGN wDbListe = wTekst.

END PROCEDURE.

/* Henter oppkoblingsparametre for Rapport databasen for ReportBuilder. */
PROCEDURE RpbWrDB:
  DEF OUTPUT PARAMETER wDbListe as char NO-UNDO.

  DEF VAR wTekst as CHAR NO-UNDO.

  /* RapportDatabasen */
  GET-KEY-VALUE SECTION "SYSPARA" KEY "WrDb" VALUE wTekst.
  if wTekst = ? then
    DO:
      {syspara.i 1 1 11 wtekst}
    END.
  ASSIGN wDbListe = wTekst.

END PROCEDURE.

PROCEDURE LagreParametre:
/*------------------------------------------------------------------------------
  Purpose:     Lagrer en parameter i lokal inifil.
  Parameters:  wSection, wKey, wValue
  Notes:       Lagrer verdien i parameter wValue under seksjon wSection,
               med label wKey.
------------------------------------------------------------------------------*/
  DEF input parameter wSection as CHAR NO-UNDO.
  DEF INPUT PARAMETER wKey     as CHAR NO-UNDO.
  DEF INPUT PARAMETER wValue   as CHAR NO-UNDO.

  if wSection = ? or wKey = ? or wValue = ? then
    RETURN "AVBRYT".

  SaveSettings:
  DO:
    IF wMappeLokalInifil <> ? THEN DO:
       LOAD wLokalIniFil DIR wMappeLokalInifil BASE-KEY "INI" NO-ERROR.
       IF ERROR-STATUS:ERROR THEN DO:
          LOAD wLokalIniFil DIR wMappeLokalInifil NEW BASE-KEY "INI" NO-ERROR.
          IF ERROR-STATUS:ERROR THEN LEAVE SaveSettings.
       END.
           
       USE wLokalIniFil NO-ERROR.
       IF NOT ERROR-STATUS:ERROR THEN DO:
          PUT-KEY-VALUE SECTION wSection KEY wKey VALUE wValue  NO-ERROR.
          UNLOAD wLokalIniFil NO-ERROR.
       END.
    END.
  END.
END PROCEDURE.

PROCEDURE HentParametre:
/*------------------------------------------------------------------------------
  Purpose:     Lagrer en parameter i lokal inifil.
  Parameters:  wSection, wKey, wValue
  Notes:       Lagrer verdien i parameter wValue under seksjon wSection,
               med label wKey.
------------------------------------------------------------------------------*/
  DEF input  parameter wSection as CHAR NO-UNDO.
  DEF INPUT  PARAMETER wKey     as CHAR NO-UNDO.
  DEF output PARAMETER wValue   as CHAR NO-UNDO.

  if wSection = ? or wKey = ? then
    RETURN "AVBRYT".

  SaveSettings:
  DO:
    IF wMappeLokalInifil <> ? THEN DO:
       LOAD wLokalIniFil DIR wMappeLokalInifil BASE-KEY "INI" NO-ERROR.
       IF ERROR-STATUS:ERROR THEN DO:
          LOAD wLokalIniFil DIR wMappeLokalInifil NEW BASE-KEY "INI" NO-ERROR.
          IF ERROR-STATUS:ERROR THEN LEAVE SaveSettings.
       END.
           
       USE wLokalIniFil NO-ERROR.
       IF NOT ERROR-STATUS:ERROR THEN DO:
          GET-KEY-VALUE SECTION wSection KEY wKey VALUE wValue.
          UNLOAD wLokalIniFil NO-ERROR.
       END.
    END.
  END.
END PROCEDURE.

