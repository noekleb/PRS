DEFINE VARIABLE dDato    AS DATE       NO-UNDO.
DEFINE VARIABLE iKassenr AS INTEGER  INIT 11  NO-UNDO.
DEFINE TEMP-TABLE TT_Plu NO-UNDO
    FIELD cPlu   AS CHAR
    FIELD cBeskr AS CHAR
    INDEX cPlu cPlu.
FOR EACH Butiker WHERE CAN-FIND(kasse WHERE kasse.butik = butiker.butik AND kasse.kassenr = iKassenr) NO-LOCK:
    DO dDato = DATE(01,01,2004) TO DATE(12,31,2004):
        FOR EACH Bonglinje NO-LOCK WHERE 
            Bonglinje.butikknr = Butiker.Butik AND
            Bonglinje.gruppenr = 1  AND
            bonglinje.kassenr  = iKassenr AND
            Bonglinje.Dato     = dDato AND 
            Bonglinje.TTId     < 12 AND
            LENGTH(TRIM(Bonglinje.strekkode)) < 7 USE-INDEX BongLinje:
            IF NOT CAN-FIND(TT_Plu WHERE TT_Plu.cPlu = TRIM(Bonglinje.strekkode)) THEN DO:
                CREATE TT_Plu.
                ASSIGN TT_Plu.cPlu   = TRIM(Bonglinje.strekkode)
                       TT_Plu.cBeskr = TRIM(Bonglinje.BongTekst).
                RELEASE TT_Plu.
            END.
        END.
    END.
END.
OUTPUT TO VALUE(SESSION:TEMP-DIR + "MD_PLU.txt").
FOR EACH TT_Plu:
    EXPORT DELIMITER ";" TT_Plu.
END.
OUTPUT CLOSE.
