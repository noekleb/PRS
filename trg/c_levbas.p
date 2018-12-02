TRIGGER PROCEDURE FOR CREATE OF SkoTex.LevBas.

DEFINE VARIABLE bHk    AS LOG NO-UNDO.
DEFINE VARIABLE cTekst AS CHARACTER NO-UNDO.

{syspara.i 1 1 18 cTekst}
IF CAN-DO('1,j,ja,y,yes,true',cTekst) THEN 
  bHk = TRUE.

ASSIGN
  SkoTex.LevBas.RegistrertDato = TODAY
  SkoTex.LevBas.RegistrertTid  = TIME
  SkoTex.LevBas.RegistrertAV   = userid("skotex")
  SkoTex.LevBas.KjedeAvtale    = bHK.


