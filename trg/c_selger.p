TRIGGER PROCEDURE FOR CREATE OF Selger.



  DEF VAR trgSelgerNr AS DEC NO-UNDO.

  {trg\c_w_trg.i &Fil=SkoTex.Selger &Type="C"}

  LOOPEN:
  DO WHILE TRUE:
      RUN trg/genselgernr.p (OUTPUT trgSelgerNr).
      ASSIGN
        Selger.SelgerNr = trgSelgerNr
        NO-ERROR.
      IF NOT ERROR-STATUS:ERROR THEN
          LEAVE LOOPEN.
  END.


