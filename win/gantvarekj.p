DEFINE VARIABLE ii AS INTEGER    NO-UNDO.
DEFINE VARIABLE dNyttPar   LIKE tellelinje.AntallPar NO-UNDO.
DEFINE VARIABLE dNyttTalt  LIKE tellelinje.AntallTalt NO-UNDO.
DEFINE VARIABLE dNyttDiff  LIKE tellelinje.AntallDiff NO-UNDO.

CURRENT-WINDOW:WIDTH = 120.
FOR EACH tellehode:
FOR EACH tellelinje OF tellehode. 
    ASSIGN dNyttPar  = 0
           dNyttTalt = IF tellelinje.AntallPar > 0 THEN tellelinje.AntallPar
                       ELSE tellelinje.AntallDiff + tellelinje.AntallTalt
           dNyttDiff = -1 * dNyttTalt.

    ASSIGN tellelinje.AntallPar = 0
           tellelinje.AntallTalt = dNyttTalt
           tellelinje.AntallDiff = dNyttDiff
           tellelinje.OpprVerdi = 0
           tellelinje.OpptVerdi = tellelinje.AntallTalt * tellelinje.VVareKost
           tellelinje.VerdiDiff = tellelinje.AntallDiff * tellelinje.VVarekost.
/*     DISP tellenr antallpar antalltalt antalldiff dNyttPar FORMAT "->>9" dNyttTalt FORMAT "->>9" dNyttDiff FORMAT "->>9" */
/*         WITH WIDTH 120.                                                                                                 */
END.
END.

