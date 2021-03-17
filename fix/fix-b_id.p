def var trgB_Id as dec no-undo.
def buffer trgBongHode for Data.BongHode.

DEF VAR pdInt AS INT NO-UNDO.

FOR EACH BongHode EXCLUSIVE-LOCK:
    pdInt = pdInt + 1.

    IF BongHode.b_id = 0 THEN
    DO:
        ASSIGN            
            trgB_Id = dec(SUBstring(STRING(year(BongHode.Dato),"9999"),3,2) +
                      STRING(MONTH(BongHode.Dato),"99") + 
                          string(pdInt,"99999999"))
            Data.BongHode.B_Id  = trgB_Id
            Data.BongHode.ODato = BongHode.Dato
            Data.BongHode.OTid  = BongHode.Tid
            Data.BongHode.OAv   = "JF"
            .
        FOR EACH BongLinje OF BongHode EXCLUSIVE-LOCK:
            ASSIGN
                BongLinje.B_Id = BongHode.b_id
                .
        END.
    END.

END.
