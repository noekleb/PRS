DEFINE VARIABLE ii AS INTEGER    NO-UNDO.

DEF VAR lVaresalg  AS DEC NO-UNDO.
DEF VAR lBetaling  AS DEC NO-UNDO.
DEF VAR lLinjeRab  AS DEC NO-UNDO.
DEF VAR lSubTotRab AS DEC NO-UNDO.

CURRENT-WINDOW:WIDTH = 300.

BONGHODE:
FOR EACH bonghode NO-LOCK WHERE 
    BongHode.butikkNr >= 0 AND 
    BongHode.butikkNr <= 13 AND 
    BongHode.GruppeNr >= 0 AND
    BongHode.KasseNr  >= 0 AND
    BongHode.dato >= 04/01/10  AND 
    BongHode.dato <= 09/30/10 /* AND
    BongHode.BongNr = 8719      */
    BREAK BY BongHode.ButikkNr
          BY BongHode.GruppeNr
          BY BongHode.KasseNr
          BY BongHode.DAto
          BY BongHode.BongNr:

    ASSIGN
        ii         = ii + 1
        lVaresalg  = 0
        lBetaling  = 0
        lLinjeRab  = 0
        lSubTotRab = 0
        .

    LINJE:
    FOR EACH BongLinje NO-LOCK WHERE
        BongLinje.B_Id = BongHode.B_Id AND
        BongLinje.Makulert = FALSE:
        
        /* Bonger som ikke skal tas med */
        IF CAN-DO('004,006',STRING(BongLinje.TTId,'999')) THEN
            NEXT LINJE.

        
        /* Varesalg */
        IF CAN-DO('001,002,005,006,007,008,009,011',string(BongLinje.TTId,'999')) THEN
            ASSIGN
              lVareSalg  = lVaresalg  + BongLinje.LinjeSum
              lLinjeRab  = lLinjeRab  + BongLinje.LinjeRab
              lSubTotRab = lSubTotRab + BongLinje.SubtotalRab
              .
        /* Retur, reklamasjon m.m. */
        ELSE IF CAN-DO('030,004,010',string(BongLinje.TTId,'999')) THEN
            ASSIGN
              lVareSalg  = lVaresalg  - BongLinje.LinjeSum
              lLinjeRab  = lLinjeRab  - BongLinje.LinjeRab
              lSubTotRab = lSubTotRab - BongLinje.SubtotalRab
              .
        /* Betaling */
        ELSE IF CAN-DO('050,051,052,053,054,055,056,057,058,060,063,065,066,070,071',STRING(BongLinje.TTId,'999')) THEN
            lBetaling = lBetaling + BongLinje.LinjeSum.
    END. /* LINJE */


    IF lVareSalg = 0 AND lLinjeRab = 0 AND lSubTotRab = 0 THEN NEXT.
    
    IF         (
        lVaresalg  -  
        lLinjeRab  -
        lSubTotRab -
        lBetaling  
        ) <> 0 THEN
        
    DISPLAY
        ii COLUMN-LABEL 'Ant.bonger'
        BongHode.ButikkNr
        BongHode.KasseNr
        BongHode.Dato
        BongHode.BongNr
        lVaresalg  
        lLinjeRab  
        lSubTotRab 
        lBetaling  
        (
        lVaresalg  -  
        lLinjeRab  -
        lSubTotRab -
        lBetaling  
        ) (TOTAL) COLUMN-LABEL 'Ber. diff'
        BongHode.Belop COLUMN-LABEL 'BongHode.Belop'
        WITH WIDTH 300.

END.
DISPL ii.
