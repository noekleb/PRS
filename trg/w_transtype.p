TRIGGER PROCEDURE FOR WRITE OF TransType.

{trg\c_w_trg.i &Fil=TransType &TYPE=W}


IF NOT CAN-FIND(FIRST TransBeskr OF TransType) THEN
DO :
    CREATE TransBeskr.
    ASSIGN
        TransBeskr.TTID = TransType.TTId
        TransBeskr.Beskrivelse = TransType.Beskrivelse.
END.
