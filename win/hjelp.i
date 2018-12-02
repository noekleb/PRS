/************************************************************
    Program:  hjelp.i
    Created:  TN   12 Nov 98
Description:  H†ndterer kobling og interface mot hjelpefil.

Last change:  TN   12 Nov 98   10:33 am
************************************************************/

/* Registrering av koblinger mot hjelpefil. */
PROCEDURE HjelpMap:

/*
DEF VAR wh-test AS HANDLE NO-UNDO.
IF VALID-HANDLE(FOCUS) THEN DO:
   ASSIGN wh-test = FOCUS:PARENT.
   DO WHILE VALID-HANDLE(wh-test):
      IF wh-test:TYPE = "DIALOG-BOX" THEN DO:
         APPLY "HELP" TO wh-test.
         RETURN NO-APPLY.
      END.
      ASSIGN wh-test = wh-test:PARENT.
   END.
END.
*/
  RUN d-vhlpmap.w(LDBNAME("dictdb"),CURRENT-WINDOW:NAME).
  RETURN NO-APPLY.
END PROCEDURE.

/* Håndterer hjelp */
PROCEDURE Hjelp:
  DEF INPUT PARAMETER wPara1 as CHAR NO-UNDO.
  DEF INPUT PARAMETER wPara2 as CHAR NO-UNDO.

  DEF VAR wHjelpeFil as CHAR NO-UNDO INITIAL ".\hlp\basis.hlp".

  GET-KEY-VALUE SECTION "SYSPARA" KEY "HjelpeFil" VALUE wHjelpeFil.
  IF wHjelpeFil = ? then
    DO:
      {syspara.i 1 1 2 wHjelpeFil}
    END.

  IF wHjelpefil <> "" THEN DO:
    IF SEARCH(wHjelpefil) = ? THEN DO:
      MESSAGE "Finner ikke hjelpefilen:" SKIP
              wHjelpefil        SKIP(1)
              "Kontakt systemansvarlig...."
        VIEW-AS ALERT-BOX ERROR TITLE "Feil".
      RETURN.
    END.
    SYSTEM-HELP wHjelpefil CONTENTS.
  END.
  ELSE MESSAGE "Navn på hjelpefil er ikke angitt i INI-Filen." SKIP(1)
              "Kontrakt systemansvarlig."
      VIEW-AS ALERT-BOX WARNING TITLE "Hjelp".
END PROCEDURE.

