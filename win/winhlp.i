/* Programnavn: winhelp.i
   Laget av   : SJ 18.06.98
   System     : Felles VS90
   Beskrivelse: Felles include for hjelpeknapp i vinduer
   Parametre  : 1 tekst som skal "mappes" mo CONTEXT
   Endringer  :
	Last change:  TN    5 Oct 98    9:09 pm
*/

DO:
  /*
   RUN Hjelp IN (SESSION:FIRST-PROCEDURE) ("H",{&WINDOW-NAME}:NAME).
  */
  /*
  if VALID-HANDLE(wLibHandle) then
    RUN Hjelp IN wLibHandle
        (STRING(KEYLABEL(LASTKEY) = "CTRL-F1","H"),ENTRY(1,THIS-PROCEDURE:FILE-NAME,".")).
  */
  if VALID-HANDLE(wLibHandle) then
    RUN CtrlHjelp IN wLibHandle.
END.
