TRIGGER PROCEDURE FOR CREATE OF TGExport.

DEF VAR trgEksportNr AS DECIMAL NO-UNDO.

{trg\c_w_trg.i &Fil=TGExport &Type="C"}

LOOPEN:
DO WHILE TRUE:
    RUN trg/gentgexportid.p (OUTPUT trgEksportNr).
    ASSIGN
      TGExport.TGExportId= trgEksportNr
      NO-ERROR.
    IF NOT ERROR-STATUS:ERROR THEN
        LEAVE LOOPEN.
END.



