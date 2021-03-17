/* RUN connectDb.p ("db\data.pf").   */
/* RUN connectDb.p ("db\skotex.pf"). */
RUN connectDb.p ("q:\db\data.pf").
RUN connectDb.p ("q:\db\skotex.pf").
IF CONNECTED("skotex") AND CONNECTED("data") THEN
    RUN xGantSalesthrough.p.
QUIT.
