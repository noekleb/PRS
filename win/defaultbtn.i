/* Programnavn  : defaultbtn.i
   Laget av     : TN 4/8-98
   System       : SkoTex
   Funksjoner   : Setter evt. default-button
   Endringer    :
*/

IF NOT SESSION:DATA-ENTRY-RETURN THEN
   ASSIGN
     {1}:DEFAULT IN FRAME {&FRAME-NAME}= YES
     FRAME {&FRAME-NAME}:DEFAULT-BUTTON = {1}:HANDLE IN FRAME {&FRAME-NAME}.

