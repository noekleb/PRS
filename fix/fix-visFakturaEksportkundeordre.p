DEF VAR pdDato AS DATE NO-UNDO.
DEF VAR cButLstTillat AS CHAR NO-UNDO.

CURRENT-WINDOW:WIDTH = 350.

ASSIGN 
    cButLstTillat = '1,20'
    pdDato = TODAY - 30
    .
    
FOR EACH FakturaHode EXCLUSIVE-LOCK WHERE 
  FakturaHode.FakturertDato >= pdDato AND 
  /*FakturaHode.EksportertDato > ? AND */
  FakturaHode.Opphav         = 20 /* Fra overføringer mellom butikker. */
  :
    
  FIND kunde NO-LOCK WHERE 
    Kunde.KundeNr = FakturaHode.KundeNr NO-ERROR.
    
  /* TN 11/5-20 Er denne variabelen satt, skal faktura bare eksporteres til butikkene i listen. */
  /* Inneholder normalt '1,20'.                                                                 */  
  IF cButLstTillat <> '' THEN 
  DO:
    IF NOT CAN-DO(cButLstTillat,STRING(Kunde.butikkNr)) THEN 
      NEXT.
  END.  
  
  FakturaHode.EksportertDato = ?.
  
  /*IF FakturaHode.eksportertDato <> ? THEN*/
  DISPLAY
      FakturaHode.FakturaNr
      FakturaHode.FakturertDato
      FakturaHode.EksportertDato
      Fakturahode.KundeNr
      Kunde.butikkNr WHEN AVAILABLE Kunde
  WITH WIDTH 350.            
END.
