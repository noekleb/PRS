CURRENT-WINDOW:WIDTH = 300.
DEF VAR dDatoFra AS DATE NO-UNDO.
DEF VAR dDatoTil AS DATE NO-UNDO.

dDatoFra = 10/22/2012.
dDatoFra = 10/25/2012.
      
FOR EACH KAsse NO-LOCK:

  FOR EACH Non_Sale_Spes WHERE 
      Non_Sale_Spes.Butikk = 1 AND
      Non_Sale_Spes.Kasse = Kasse.KasseNr AND 
      Non_Sale_Spes.Dato  >= dDatoFra AND
      Non_Sale_spes.Dato <= dDatoTil:

      ASSIGN
          Non_Sale_Spes.Non_SaleAntall = Non_Sale_Spes.Non_SaleAntall / 2
          Non_Sale_Spes.Non_SaleVerdi  = Non_Sale_Spes.Non_SaleVerdi / 2
          .

      DISPLAY
          Non_Sale_Spes.Butikk
          Non_Sale_Spes.Kasse
          Non_Sale_Spes.Dato
          Non_Sale_Spes.Non_SaleVerdi
          Non_Sale_Spes.Non_SaleAntall
          Non_Sale_Spes.Non_Sale_Type
          Non_Sale_Spes.KassererNr
          Non_Sale_Spes.EDato
          Non_Sale_Spes.ETid
          WITH WIDTH 300.


  END.
END.
