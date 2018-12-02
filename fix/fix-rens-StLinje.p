
DEF VAR X AS INT NO-UNDO.

DEF STREAM Rens.

OUTPUT STREAM Rens TO VALUE("rensStLinje.d") APPEND.

LOOPEN:                    
FOR EACH StLinje EXCLUSIVE-LOCK WHERE
   StLinje.StTypeId = "ARTIKKEL":

    IF CAN-FIND(ArtBas WHERE
                ArtBAs.ArtikkelNr = DEC(StLinje.DataObjekt)) THEN
        NEXT LOOPEN.
   /*
   StLinje.DataObjekt = string(ArtBas.ArtikkelNr,"9999999999999"):
   */

   X = X + 1.

   IF X MODULO 50 = 0 THEN
   DO:
       PAUSE 0.
       DISPLAY X WITH FRAME g.
   END.

   EXPORT STREAM Rens StLinje.
   DELETE StLinje.

END. /* LOOPEN */

OUTPUT STREAM Rens CLOSE.
