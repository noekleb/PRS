TRIGGER PROCEDURE FOR CREATE OF MixMatchHode.

DEF VAR trgMixNr AS INT NO-UNDO.

{trg\c_w_trg.i &Fil=SkoTex.MixMatchHode &TYPE=C}

EVIGHETEN:
DO WHILE TRUE:
    RUN genpakemixnr.p (OUTPUT trgMixNr).

  /* Er nummeret brukt til pakke eller Mix, tar vi neste. */
  IF CAN-FIND(FIRST ArtBas WHERE
              ArtBas.PakkeNr = trgMixNr) OR 
     CAN-FIND(MixMatchHode WHERE
              MixMatchHode.MixNr = trgMixNr) THEN
      NEXT EVIGHETEN.
  ELSE DO:
      ASSIGN
          MixMatchHode.MixNr = trgMixNr
          .
      LEAVE EVIGHETEN.
  END.
END. /* EVIGHETEN */



