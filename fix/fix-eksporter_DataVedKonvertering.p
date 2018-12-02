DEF VAR ocReturn AS CHAR NO-UNDO.

DEF VAR dFraDato AS DATE NO-UNDO.
DEF VAR dTilDato AS DATE NO-UNDO.

ASSIGN /* Bruk datoformat MM/DD/YYYY */
    dFraDato = 08/06/2014
    dTilDato = TODAY
    .

RUN eksportfinans.p (dFraDato,dTilDato,OUTPUT ocReturn). 
RUN eksportsalg.p (dFraDato,dTilDato,YES,OUTPUT ocReturn).
RUN eksportkreditsalg.p (OUTPUT ocReturn).
RUN eksportbokforingsbilag.p (dFraDato,dTilDato,OUTPUT ocReturn).


