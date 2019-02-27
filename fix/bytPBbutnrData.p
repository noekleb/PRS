DEFINE VARIABLE icount AS INTEGER    NO-UNDO.
DEFINE VARIABLE ccGammal AS CHARACTER  NO-UNDO.
DEFINE VARIABLE ccByt AS CHARACTER  NO-UNDO.
DEFINE BUFFER bBongHode FOR BongHode.
    DEFINE VARIABLE ii AS INTEGER    NO-UNDO.
    DEFINE VARIABLE i2 AS INTEGER    NO-UNDO.
/* ASSIGN ccGammal = "8135,8250,8265,8271,8295,8310,8385,8473,8488,28133,28145,28274,28372,28396,68207,68210,78691"  */
/*        ccByt    = "5135,5250,5265,5271,5395,5310,5385,5473,5488,25133,25145,25274,25372,25396,65207,65210,75691". */
ii = TIME.
ASSIGN ccGammal = "8271"
       ccByt    = "5271".
DO icount = 1 TO NUM-ENTRIES(ccGammal):
    FOR EACH DataSett WHERE DataSett.ButikkNr = INT(ENTRY(iCount,ccGammal)):
        FOR EACH BongHode OF DataSett NO-LOCK:
            FIND bBongHode WHERE ROWID(bBongHode) = ROWID(BongHode).
            ASSIGN bBongHode.ButikkNr = INT(ENTRY(iCount,ccByt)) NO-ERROR.
            IF ERROR-STATUS:ERROR THEN
                NEXT.
            FOR EACH Bonglinje WHERE BongLinje.b_id = BongHode.b_id:
                ASSIGN BongLinje.butikknr = INT(ENTRY(iCount,ccByt)).
            END.
        END.
        ASSIGN DataSett.ButikkNr = INT(ENTRY(iCount,ccByt)).
    END.
END.
i2 = TIME.
MESSAGE STRING(i2 - ii,"HH:MM:SS")
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
