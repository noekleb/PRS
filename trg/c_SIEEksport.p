TRIGGER PROCEDURE FOR CREATE OF SIEEksport.

DEF VAR trgSIEEksportNr AS DECIMAL NO-UNDO.

{trg\c_w_trg.i &Fil=SIEEksport &TYPE=C}

  LOOPEN:
  DO WHILE TRUE:
      RUN trg/genSIEexportnr.p (OUTPUT trgSIEEksportNr).
      ASSIGN
        SIEEksport.SIEEksportNr = trgSIEEksportNr
        NO-ERROR.
      IF NOT ERROR-STATUS:ERROR THEN
          LEAVE LOOPEN.
  END.


