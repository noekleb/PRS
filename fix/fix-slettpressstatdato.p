DEF VAR pi AS DEC FORMAT ">>>,>>>,>>>,>>>,>>>,>>9.99" NO-UNDO.
DEF VAR cButLst AS CHAR NO-UNDO.
DEF VAR piLoop  AS INT  NO-UNDO.
DEF VAR dDato   AS DATE NO-UNDO.

ASSIGN
    cButLSt = "5271"
    .


BUTLST:
DO piLoop= 1 TO NUM-ENTRIES(cButLst):
    BUTIKK:
    FOR EACH Butiker NO-LOCK WHERE
        Butiker.Butik = int(ENTRY(piLoop,cButLst)):

        DATO:
        DO dDato = 01/01/2004 TO 04/30/2004:
            pause 0. Display "pfTenderRevenue Flipp 1".
            pi = 0.
            FOR EACH pfTenderRevenue WHERE
                pfTenderRevenue.Store_No = Butiker.Butik AND
                pfTenderRevenue.DATE     = dDato:

                pi = pi + 1.
                PAUSE 0.
                IF pi MODULO 500 = 0 THEN DISPLAY pi dDato.
                DELETE pfTenderRevenue. 
            END.

            pause 0. Display "pfStatisticsRevenue Flipp 2".
            pi = 0.
            FOR EACH pfStatisticsRevenue WHERE
                pfStatisticsRevenue.Store_No = Butiker.Butik AND
                pfStatisticsRevenue.DATE     = dDato:

                pi = pi + 1.
                PAUSE 0.
                IF pi MODULO 500 = 0 THEN DISPLAY pi dDato.
                DELETE pfStatisticsRevenue. 
            END.

            pause 0. Display "pfDaySales_HourExt Flipp 8".
            pi = 0.
            AVDELING:
            FOR EACH Avdeling:
                FOR EACH pfDaySales_HourExt WHERE
                    pfDaySales_HourExt.DepartmentId = Avdeling.AvdelingNr AND
                    pfDaySales_HourExt.Store_No     = Butiker.Butik AND 
                    pfDaySales_HourExt.DATE         = dDato: 

                    pi = pi + 1.
                    PAUSE 0.
                    IF pi MODULO 500 = 0 THEN DISPLAY pi dDato.
                    DELETE pfDaySales_HourExt. 
                END.
            END. /* AVDELING */

            pause 0. Display "pfDaySales Flipp 9".
            pi = 0.
            FOR EACH pfDaySales WHERE
                pfDaySales.Store_No = Butiker.Butik AND 
                pfDaySales.DATE     = dDato:

                pi = pi + 1.
                PAUSE 0.
                IF pi MODULO 500 = 0 THEN DISPLAY pi dDato.
                DELETE pfDaySales. 
            END.
        END. /* DATO */

    END. /* BUTIKK */    
END. /* BUTLST */


