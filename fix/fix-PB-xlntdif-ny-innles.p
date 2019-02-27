
DEF VAR ii AS DEC NO-UNDO.
DEFINE VARIABLE iTotBut AS INTEGER    NO-UNDO.
DEFINE VARIABLE i2 AS INTEGER    NO-UNDO.
DEF STREAM bongh.
DEF STREAM bongl.
DEF STREAM movescript.

FORM "Behandlar:" ii FORMAT "zz9" SKIP 
     "Kvar     :" i2 FORMAT "zz9"
     WITH FRAME AA.

OUTPUT STREAM bongh TO "i:\2004\bonghode-diff-1.d".
OUTPUT STREAM bongl TO "i:\2004\bonglinje-diff-1.d".
OUTPUT STREAM movescript TO "F:\Home\Pressbyran\SAKNAS.TXT".
FOR EACH xlntbutik WHERE xlntbutik.ankommetdir <> "" NO-LOCK:
    iTotBut = iTotBut + 1.
END.
FOR EACH xlntbutik WHERE xlntbutik.ankommetdir <> "" NO-LOCK BREAK BY xlntbutik.butikknr:
    ii = ii + 1.
    i2 = iTotBut - ii.
    DISP ii i2 WITH FRAME AA.
    PAUSE 0.
    FOR EACH xlntdag WHERE xlntdag.butikknr = xlntbutik.butikknr AND xlntDag.SumAntall <> xlntDag.ISumAntall NO-LOCK:
        FOR EACH bonghode WHERE bonghode.butikknr = xlntdag.butikknr AND 
                                bonghode.gruppenr = 1 AND
                                bonghode.kassenr  = 11 AND
                                bonghode.dato     = xlntDag.dato.
            FOR EACH bonglinje WHERE bonglinje.butikknr = bonghode.butikknr AND
                                     bonglinje.gruppenr = bonghode.gruppenr AND
                                     bonglinje.kassenr  = bonghode.kassenr  AND
                                     bonglinje.dato     = bonghode.dato     AND
                                     bonglinje.bongnr   = bonghode.bongnr:

                EXPORT STREAM bongl bonglinje.
                DELETE bonglinje.
            END.
            EXPORT STREAM bongh bonghode.
            DELETE bonghode.
        END.
        FOR EACH bonghode WHERE bonghode.butikknr = altbutikknr AND 
                                bonghode.gruppenr = 1 AND
                                bonghode.kassenr  = 11 AND
                                bonghode.dato     = xlntDag.dato.
            FOR EACH bonglinje WHERE bonglinje.butikknr = bonghode.butikknr AND
                                     bonglinje.gruppenr = bonghode.gruppenr AND
                                     bonglinje.kassenr  = bonghode.kassenr  AND
                                     bonglinje.dato     = bonghode.dato     AND
                                     bonglinje.bongnr   = bonghode.bongnr:

                EXPORT STREAM bongl bonglinje.
                DELETE bonglinje.
            END.
            EXPORT STREAM bongh bonghode.
            DELETE bonghode.
        END.
        IF FIRST-OF(xlntbutik.butikknr) THEN
            PUT STREAM movescript UNFORMATTED "MD" " %2\" + STRING(xlntbutik.butikknr,"99999") SKIP.
        PUT STREAM movescript UNFORMATTED "MOVE %1\" + STRING(xlntbutik.butikknr,"99999") + "\MD" + STRING(YEAR(xlntDag.dato),"9999")
                                                         + STRING(MONTH(xlntDag.dato),"99")
                                                         + STRING(DAY(xlntDag.dato),"99")   + ".r1"
                                                         + " %2\" + STRING(xlntbutik.butikknr,"99999") SKIP.
    END.
END.
OUTPUT STREAM bongh      CLOSE.
OUTPUT STREAM bongl      CLOSE.
OUTPUT STREAM movescript CLOSE.
