/* Programnavn: gotofield.p
   Laget av   : SJ 16.06.98
   System     : Felles VS90
   Beskrivelse: Går til et spesiet felt i en frame når feltnavnet er kjent
   Parametre  : INPUT CHAR wFieldName
                Returnerer <OK> eller <Feil>.
   Endringer  :
	Last change:  SJ    3 Aug 98   11:42 am
*/
DEF INPUT PARAMETER wFieldName AS CHAR NO-UNDO.

DEF VAR wh AS WIDGET NO-UNDO.

ASSIGN wh = SELF:FRAME
       wh = wh:CURRENT-ITERATION
       wh = wh:FIRST-CHILD.

DO WHILE VALID-HANDLE(wh):
  IF wh:NAME = wFieldName THEN DO:
     APPLY "ENTRY" TO wh.
     RETURN "<OK>".
  END.
  ASSIGN wh = wh:NEXT-SIBLING.
END.
RETURN "<Feil>".
