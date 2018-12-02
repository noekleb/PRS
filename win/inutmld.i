/************************************************************
    Program:  inutmld.i
    Created:  TN   17 Jun 98
Description:  Inn - utmelding av programliste som vedlikeholdes
              i prosedurebibloteket.
              Det er kun peristente rutiner som logges inn/ut.

Last change:  TN    5 Jul 98    0:24 am
************************************************************/


IF THIS-PROCEDURE:PERSISTENT then
  DO:
    if valid-handle(wLibHandle) then
      run {&Modus}ProgramListe in wLibHandle
          (this-procedure,
           {&WINDOW-NAME}:HANDLE,
           program-name(1)).
  END.

