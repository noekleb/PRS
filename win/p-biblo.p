/************************************************************
    Program:  p-biblo.p
    Created:  TN   13 Jun 98
Description:  Generelt prosedurebiblotek.

Last change:  TN   16 Aug 101   12:34 pm
************************************************************/

DEF VAR wSystemNavn       AS CHAR NO-UNDO INIT "[SkoTex]".
DEF VAR wLokalIniFil      AS CHAR NO-UNDO INIT "sk-lok.ini".
DEF VAR wMappeLokalIniFil AS CHAR NO-UNDO INIT "C:\WINDOWS".
DEF VAR wHuskPos          AS LOG  NO-UNDO INIT TRUE.
DEF VAR wSistKompilert    AS CHAR NO-UNDO.
DEF VAR wSkoDB            AS CHAR NO-UNDO.
DEF VAR wWrDB             AS CHAR NO-UNDO.
DEF VAR wProgram          AS HANDLE EXTENT 20 NO-UNDO.
DEF VAR wEndringsNr       AS INT  NO-UNDO.

DEF STREAM Stream1.

/* For kalkyle av artikkel. */
DEF VAR FI-EuroKurs  AS DEC NO-UNDO.
DEF VAR FI-Pris      AS DEC NO-UNDO.
DEF VAR FI-Mva%      AS DEC NO-UNDO.
DEF VAR wWork        AS DEC NO-UNDO.

RUN IniFil     IN THIS-PROCEDURE (OUTPUT wLokalIniFil).
RUN Mappe      IN THIS-PROCEDURE (OUTPUT wMappeLokalIniFil).
RUN SystemNavn IN THIS-PROCEDURE (OUTPUT wSystemNavn).
RUN Euro       IN THIS-PROCEDURE (OUTPUT FI-EuroKurs).

/* Hotkeys for utvikklere m.m. */
ON "Ctrl-F1", "Ctrl-F2", "Ctrl-F3", "Ctrl-F4", "Ctrl-F5",
   "Ctrl-F6", "Ctrl-F7", "Ctrl-F8", "Ctrl-F9", "Ctrl-F10",
   "Ctrl-F11", "Ctrl-F12", "F2" ANYWHERE
DO:
  RUN CtrlHjelp.
  RETURN NO-APPLY.
END.

FUNCTION Mva2 RETURNS DEC ().
    wWork = FI-Pris - (FI-Pris / (1 + (FI-Mva% / 100))).
    IF wWork = ? THEN wWork = 0.

    RETURN wWork.
END FUNCTION.

/* Kalkulerer mva et beløp. */
PROCEDURE MvaKalk:
  DEF INPUT  PARAMETER wMva%  AS DEC NO-UNDO.
  DEF INPUT  PARAMETER wBelop AS DEC NO-UNDO.
  DEF OUTPUT PARAMETER wMvaKr AS DEC NO-UNDO.

  ASSIGN
    FI-Mva% = wMva%
    FI-Pris = wBelop
    wMvaKr  = Mva2().

END PROCEDURE.

{divfunk.i}          /* Diverse funksjoner.                                      */
{programliste.i}     /* Håndtering av programliste.                              */
{gmlinterface.i}     /* Funksjoner får h†ndtering av gamle SkoTex programmer.    */
{vindushandtering.i} /* Vindush†ndtering                                         */
{inifil.i}           /* Håndtering av inifil og systemparametre.                 */
{hjelp.i}            /* H†ndterer interface mot hjelp.                           */
{bilder.i}           /* Rutiner for bildeh†ndtering.                             */
/*{kalkyle.i}          /* Program og funksjoner for h†ndtering av kalkyle.         */*/
{brwsset.i}          /* Hanterar personliga inst. av browser.                    */
{windows.i}          /* Funksjonsbilbiotek for windowsrutiner fra Jurjen Dikstra */
{filer.i}            /* Funksjoner for å håndtere filer.                         */
{beregninger.i}      /* Rutiner for utføring av ulike beregninger. Mva f.eks.    */

/* Håndtering av Ctrl taster. */
PROCEDURE CtrlHjelp:
  CASE LAST-EVENT:LABEL:
    /*
    WHEN "F2"       THEN RUN w-oppslag.w.  /* Oppslagsverk                       */
    */
    WHEN "Ctrl-F1"  THEN RUN HjelpMap IN THIS-PROCEDURE.
                                           /* Starter kobling av hjelp           */
    WHEN "Ctrl-F2"  THEN RUN d-kompiler.w. /* Kompilering av program.            */
    WHEN "Ctrl-F3"  THEN RUN _edit.p.      /* Progress editor.                   */
    WHEN "Ctrl-F4"  THEN RUN DictView.w.      /* Dictionary.                        */
    /*
    WHEN "Ctrl-F5"  THEN RUN dicthtml.p.   /* DB - Rapport i HTML.               */
    */
    WHEN "Ctrl-F5"  THEN RUN xdict.w.      /* DB - Rapport i HTML.               */
    WHEN "Ctrl-F6"  THEN RUN _comp.p .     /* Kompilator.                        */
    WHEN "Ctrl-F7"  THEN RUN w-meny.w.     /* Redigering av meny i aktivt vindu. */
    WHEN "Ctrl-F8"  THEN RUN d-prginf.w.   /* Programinformasjon                 */
    WHEN "Ctrl-F9"  THEN RUN d-vistast.w.  /* Viser informasjon om siste tastetrykk */
    WHEN "Ctrl-F10" THEN RUN VisPrgLst IN THIS-PROCEDURE.
    WHEN "Ctrl-F11" THEN RUN probuddy.w PERSISTENT SET wProgram[2]. /* Probyddy. */
    WHEN "Ctrl-F12" THEN RUN protools/_walker.w PERSISTENT SET wProgram[3]. /* Walker */
/*    WHEN "Ctrl-P"    THEN run w-WindowPrint.w (CURRENT-WINDOW:HWND).*/
/*    WHEN "Ctrl-P"    THEN run printscreen.p (CURRENT-WINDOW:HWND,TRUE). */
    OTHERWISE DO:
        RUN hjelp (STRING(KEYLABEL(LASTKEY) = "CTRL-F1","M/H"),ENTRY(1,THIS-PROCEDURE:FILE-NAME,".")).
    END.
  END CASE.
END PROCEDURE.


