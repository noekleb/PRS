/************************************************************
    Program:  p-biblo.p
    Created:  TN   13 Jun 98
Description:  Generelt prosedurebiblotek.

Last change:  TN   16 Aug 101   12:34 pm
************************************************************/

DEF VAR wSystemNavn       AS CHAR NO-UNDO INIT "[SkoTex]".
DEF VAR wLokalIniFil      AS CHAR NO-UNDO INIT "sk-lok.ini".
DEF VAR wMappeLokalIniFil AS CHAR NO-UNDO INIT "C:\WINDOWS".
DEF VAR wHuskPos          as LOG  NO-UNDO INIT TRUE.
DEF VAR wSistKompilert    as CHAR NO-UNDO.
DEF VAR wSkoDB            as CHAR NO-UNDO.
DEF VAR wWrDB             as CHAR NO-UNDO.
DEF VAR wProgram          as HANDLE EXTENT 20 NO-UNDO.
DEF VAR wEndringsNr       as INT  NO-UNDO.

DEF STREAM Stream1.

/* For kalkyle av artikkel. */
DEF VAR FI-EuroKurs  as DEC NO-UNDO.
def var FI-Pris      as DEC NO-UNDO.
def var FI-Mva%      as DEC NO-UNDO.
DEF VAR wWork        as DEC NO-UNDO.

RUN IniFil     in THIS-PROCEDURE (output wLokalIniFil).
RUN Mappe      in THIS-PROCEDURE (OUTPUT wMappeLokalIniFil).
RUN SystemNavn in THIS-PROCEDURE (OUTPUT wSystemNavn).
RUN Euro       in THIS-PROCEDURE (OUTPUT FI-EuroKurs).

/* Hotkeys for utvikklere m.m. */
on "Ctrl-F1", "Ctrl-F2", "Ctrl-F3", "Ctrl-F4", "Ctrl-F5",
   "Ctrl-F6", "Ctrl-F7", "Ctrl-F8", "Ctrl-F9", "Ctrl-F10",
   "Ctrl-F11", "Ctrl-F12", "F2", "Ctrl-P" ANYWHERE
DO:
  RUN CtrlHjelp.
  RETURN NO-APPLY.
END.

FUNCTION Mva2 RETURNS dec ().
    wWork = FI-Pris - (FI-Pris / (1 + (FI-Mva% / 100))).
    if wWork = ? THEN wWork = 0.

    RETURN wWork.
END FUNCTION.

/* Kalkulerer mva et beløp. */
PROCEDURE MvaKalk:
  DEF INPUT  PARAMETER wMva%  as DEC NO-UNDO.
  def INPUT  PARAMETER wBelop as DEC NO-UNDO.
  def OUTPUT PARAMETER wMvaKr as DEC NO-UNDO.

  assign
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
  CASE LAST-EVENT:label:
    /*
    WHEN "F2"       THEN RUN w-oppslag.w.  /* Oppslagsverk                       */
    */
    WHEN "Ctrl-F1"  then run HjelpMap in THIS-PROCEDURE.
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
    WHEN "Ctrl-F8"  then RUN d-prginf.w.   /* Programinformasjon                 */
    WHEN "Ctrl-F9"  then RUN d-vistast.w.  /* Viser informasjon om siste tastetrykk */
    WHEN "Ctrl-F10" THEN RUN VisPrgLst in THIS-PROCEDURE.
    WHEN "Ctrl-F11" THEN run probuddy.w PERSISTENT set wProgram[2]. /* Probyddy. */
    WHEN "Ctrl-F12" THEN run protools/_walker.w PERSISTENT set wProgram[3]. /* Walker */
    WHEN "Ctrl-P"    THEN run w-WindowPrint.w (CURRENT-WINDOW:HWND).
/*    WHEN "Ctrl-P"    THEN run printscreen.p (CURRENT-WINDOW:HWND,TRUE). */
    OTHERWISE DO:
        run hjelp (STRING(KEYLABEL(LASTKEY) = "CTRL-F1","M/H"),ENTRY(1,THIS-PROCEDURE:FILE-NAME,".")).
    END.
  END CASE.
END PROCEDURE.


