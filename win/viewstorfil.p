/* Programnavn: viewstorfil.p
   Laget av   : TN 25.08.98
   System     : SkoTex
   Beskrivelse: Starter program for visning av en utskriftsfil fil.
                Benyttes dersom Progress' EDITOR widget ikke kan lese filen.
   Parametre  : INPUT CHAR wFilnavn
   Endringer  :

*/

DEF INPUT PARAMETER wFilnavn AS CHAR NO-UNDO.
DEF VAR wSvar    AS LOGI NO-UNDO INIT YES.
DEF VAR wProgram AS CHAR NO-UNDO INIT "Notepad".

MESSAGE "Utskriftsfilen, " + wFilNavn + ", er for stor til at den kan forhåndsvises." SKIP(1)
        "Vil du prøve å åpne den i" wProgram + "?"
      VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "Stor utskriftsfil" UPDATE wSvar.


IF wSvar = YES THEN RUN WinExec (wProgram + " " + wFilnavn,1). /* 1=normal, 2=minimert */

PROCEDURE WinExec EXTERNAL "kernel32.dll":
   DEF INPUT PARAMETER wProg AS CHAR.
   DEF INPUT PARAMETER wPres AS SHORT.
END PROCEDURE.

