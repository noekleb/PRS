CURRENT-WINDOW:WIDTH = 300.

FOR EACH Non_Sale_Spes WHERE
    Non_Sale_Spes.Dato >= 05/21/2010 AND
    Non_Sale_Spes.Dato <= 05/23/2010 AND
    Non_Sale_Spes.kode = '99999':

    /* Henter kassarapporten. */
    FIND FIRST kas_rap WHERE 
        kas_rap.dato        = Non_sale_spes.Dato     AND
        kas_rap.butikk      = Non_sale_spes.Butik AND
        kas_rap.kasse       = Non_sale_spes.Kasse  AND
        kas_rap.KassererNr >= 0 /*int(BongHode.KassererNr)*/
       EXCLUSIVE-LOCK NO-ERROR.

    Non_Sale_Spes.Non_SaleVerdi = Non_Sale_Spes.Non_SaleVerdi * -1.
    Kas_Rap.Non_SaleNeg  = Kas_Rap.Non_SaleNeg  + (Non_Sale_Spes.Non_SaleVerdi * 2)

    DISPLAY 
        Non_Sale_Spes.Butikk FORMAT '>>>>>9'
        Non_Sale_Spes.Kasse
        Non_sale_spes.Dato
        Non_Sale_Spes.Kasserer
        Non_Sale_Spes.Kode
        Non_Sale_Spes.Non_Sale_Type
        Non_Sale_Spes.Non_SaleAntall
        Non_Sale_Spes.Non_SaleVerdi
        Kas_Rap.Non_SaleNeg
        WITH WIDTH 300.


END.
