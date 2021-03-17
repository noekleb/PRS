TRIGGER PROCEDURE FOR CREATE OF StrType.

{trg\c_w_trg.i &Fil=SkoTex.strtype &Type="C"}

DEFINE VARIABLE trgStrTypeId AS INTEGER NO-UNDO.

  BLOKKEN: 
  DO:
      RUN trg/genstrtypeid.p (OUTPUT trgStrTypeId).
      
      IF trgStrTypeId > 999999 THEN 
          trgStrTypeId = 0.
      ELSE ASSIGN
        StrType.StrTypeId  = trgStrTypeId
        NO-ERROR.
  END.


