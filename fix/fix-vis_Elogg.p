CURRENT-WINDOW:WIDTH = 300.

DEF VAR lNow AS DEC FORMAT "->>>>>>>>>>>>>>>9" NO-UNDO.
DEF VAR lDiff AS DEC FORMAT "->>>>>>>>>>>>>>>9" NO-UNDO.

ASSIGN 
    lDiff = 60 * 5
    lNow  = dec(
                string(Year(today),"9999") +
                string(month(today),"99") + 
                string(day(today),"99") +
                string(time,"99999")
               ).

FOR EACH ELogg WHERE Tabellnavn = 'KOrdrehode':
    DISPLAY 
        ELogg.TabellNavn
        ELogg.eksterntSystem
        ELogg.Endringstype
        ELogg.Behandlet
        ELogg.Verdier
        ELogg.Opprettet
        lNow
        lDiff
        lNow - ELogg.Opprettet
        /*
        ELogg.EDato
        ELogg.ETid
        ELogg.BrukerId
        ELogg.RegistrertDato
        ELogg.RegistrertTid
        ELogg.RegistrertAv
        */
    WITH WIDTH 300.
END.
