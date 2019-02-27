/* fix-nonsale_spes.p */

DEF VAR bButikkNr AS INT NO-UNDO.
DEF VAR bKasseNr AS INT NO-UNDO.
DEF VAR dDato AS DATE NO-UNDO.

CURRENT-WINDOW:WIDTH = 250.

ASSIGN
    bButikkNR = 323
    bKasseNr  = 1 
    dDato     = 10/21/2011
    .

FOR EACH Kasse NO-LOCK WHERE 
    Kasse.ButikkNr = bButikkNr,
  EACH NON_Sale_Spes NO-LOCK WHERE
    Non_Sale_Spes.Butikk   = bButikkNR AND
    Non_Sale_Spes.Kasse    = Kasse.KasseNr AND
    Non_Sale_spes.Dato     = dDato 
  BREAK BY Kasse.ButikkNr
        /*BY Kasse.KasseNr*/
        BY Non_Sale_Spes.Dato
        BY Non_Sale_Spes.Non_Sale_Type:

    DISPLAY
        Non_Sale_Spes.butikk FORMAT '>>>>>9'
        Non_Sale_Spes.Kasse
        Non_Sale_Spes.Dato
        Non_Sale_Spes.Non_Sale_Type
        Non_Sale_Spes.KassererNr
        Non_Sale_Spes.Kode
        Non_Sale_Spes.Non_SaleAntall (TOTAL BY Non_Sale_Spes.Non_Sale_Type)
        Non_Sale_Spes.Non_SaleVerdi  (TOTAL BY Non_Sale_Spes.Non_Sale_Type)
        WITH WIDTH 250.

END.
