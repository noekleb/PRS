TRIGGER PROCEDURE FOR CREATE OF VPIMottak.

{trg\c_w_trg.i &Fil=SkoTex.VPIMottak &TYPE=C}

DEF VAR trgNr AS DECIMAL NO-UNDO.

DEFINE BUFFER bufVPIMottak FOR VPIMottak.

FIND LAST bufVPIMottak NO-LOCK USE-INDEX VPIMottakId NO-ERROR.
IF AVAILABLE bufVPIMottak THEN 
  trgNr = bufVPIMottak.VPIMottakId + 1.
ELSE 
  trgNr = 1.

ASSIGN
  VPIMottak.VPIMottakId = trgNr
  NO-ERROR.

/* Denne nummerserien gikk full i mange butikker TN 20/01-2012
LOOPEN:
DO WHILE TRUE:
    RUN trg/genvpimottak.p (OUTPUT trgNr).
    assign
      VPIMottak.VPIMottakId = trgNr
      NO-ERROR.
    IF NOT ERROR-STATUS:ERROR THEN
        LEAVE LOOPEN.
END.
*/

