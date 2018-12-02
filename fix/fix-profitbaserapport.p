DEF VAR iButikkNr AS INT  NO-UNDO.
DEF VAR dDato     AS DATE NO-UNDO.
DEF VAR d2Dato    AS DATE NO-UNDO.

DEF BUFFER bpfTenderRevenue FOR pfTenderRevenue.

CURRENT-WINDOW:WIDTH = 244.

DEF STREAM Ut.

OUTPUT STREAM Ut TO TERMINAL.

DATOLOOP:
REPEAT d2Dato = 11/08/2004 TO 11/11/2004:
    EKSPORTER:
    FOR EACH bPfTenderRevenue NO-LOCK WHERE 
        bpfTenderRevenue.Store_No = 78691 AND 
        bpfTenderRevenue.DATE = d2Dato
        BREAK BY bpfTenderRevenue.DATE:
        IF FIRST-OF(bpfTenderRevenue.DATE) THEN
        DO:
            PAUSE 0.
            DISPLAY
                STREAM Ut
                bpfTenderRevenue.Store_No
                bpfTenderRevenue.DATE
                WITH FRAME G DOWN.
            DOWN STREAM Ut 1 WITH FRAME G.

            OUTPUT TO VALUE("ProfitBase" + 
                            STRING(bpfTenderRevenue.Store_No,"999999") + "-" +
                            STRING(bPfTenderRevenue.DATE,"99.99.9999") + ".txt").

            ASSIGN
                iButikkNr = bpfTenderRevenue.Store_No
                dDato     = bpfTenderRevenue.DATE
                .

            PUT SKIP(1).
            PUT UNFORMATTED "** Tender revenue **" SKIP.
            FOR EACH pfTenderRevenue NO-LOCK  WHERE
                pfTenderRevenue.Store_No = iButikkNr AND
                pfTenderRevenue.DATE     = dDato:

              FIND TransType NO-LOCK WHERE
                TransType.TTId = pfTenderRevenue.Media_no NO-ERROR.
              DISPLAY 
                pfTenderRevenue.Store_No
                pfTenderRevenue.DATE
                pfTenderRevenue.Media_no
                (IF AVAILABLE TransType
                 THEN TransType.Beskrivelse
                 ELSE "***")
                pfTenderRevenue.Sale_Amount (TOTAL)
                WITH WIDTH 242 STREAM-IO.
              DOWN 1.
            END.

            PUT SKIP(1).
            PUT UNFORMATTED "** DaySales **" SKIP.
            FOR EACH pfDaySales NO-LOCK WHERE
                pfDaySales.Store_No = iButikkNr AND
                pfDaySales.DATE     = dDato:
              FIND Strekkode NO-LOCK WHERE
                  Strekkode.Kode = pfDaySales.Plu_Code NO-ERROR.
              IF AVAILABLE Strekkode THEN
                  FIND ArtBas OF Strekkode NO-LOCK NO-ERROR.
              DISPLAY 
                pfDaySales.Store_No COLUMN-LABEL "But"
                pfDaySales.Plu_Code COLUMN-LABEL "EAN/PLU"
                (IF AVAILABLE ArtBas THEN
                    ArtBAs.Beskr
                ELSE "** Ukjent") FORMAT "x(30)" COLUMN-LABEL "Bongtekst"
                pfDaySales.DATE COLUMN-LABEL "Dato"
                pfDaySales.VendorId COLUMN-LABEL "Lev"
                pfDaySales.Sales (TOTAL) COLUMN-LABEL "Pris"
                pfDaySales.Disc (TOTAL) COLUMN-LABEL "Rab"
                pfDaySales.qty (TOTAL) COLUMN-LABEL "Ant"
                pfDaySales.Vat (TOTAL) COLUMN-LABEL "Mva"
                pfDaySales.SalesCost (TOTAL) COLUMN-LABEL "Kost"

                ((pfDaySales.Sales - pfDaySales.SalesCost) * 100) / pfDaySales.Sales COLUMN-LABEL "Db%"
                WITH WIDTH 242 STREAM-IO.
            END.

            PUT SKIP(1).
            PUT UNFORMATTED "** DaySales_HourExt **" SKIP.
            FOR EACH pfDaySales_HourExt NO-LOCK WHERE
                pfDaySales_HourExt.Store_No = iButikkNr AND
                pfDaySales_HourExt.DATE     = dDato:
              DISPLAY 
                pfDaySales_HourExt.Store_No
                pfDaySales_HourExt.DATE
                pfDaySales_HourExt.Tid
                pfDaySales_HourExt.DepartmentId
                pfDaySales_HourExt.RetailDate
                pfDaySales_HourExt.Omsetning (TOTAL)
                pfDaySales_HourExt.Disc (TOTAL)
                pfDaySales_HourExt.qty (TOTAL)
                pfDaySales_HourExt.Vat (TOTAL)

                WITH WIDTH 242 STREAM-IO.
            END.

            PUT SKIP(1).
            PUT UNFORMATTED "** DaySales_HourExt **" SKIP.
            FOR EACH pfStatisticsRevenue NO-LOCK WHERE
                pfStatisticsRevenue.Store_No = iButikkNr AND
                pfStatisticsRevenue.DATE     = dDato:
              DISPLAY 
                pfStatisticsRevenue.Store_No
                pfStatisticsRevenue.DATE
                pfStatisticsRevenue.Customers_Count (total)
                pfStatisticsRevenue.ITEM_Count
                pfStatisticsRevenue.Neg_Ticket_Amount (TOTAL)
                pfStatisticsRevenue.Neg_Ticket_Count (TOTAL)
                pfStatisticsRevenue.Nosale_Count (TOTAL)
                pfStatisticsRevenue.Refund_Amount (TOTAL)
                pfStatisticsRevenue.Refund_Count (TOTAL)

                WITH WIDTH 242 STREAM-IO.
            END.

            OUTPUT CLOSE.
        END.
    END. /* EKSPORTER */
END. /* DATOLOOP */

OUTPUT STREAM Ut CLOSE.



