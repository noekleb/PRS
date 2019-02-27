current-window:width = 300.

def var dDato as date no-undo.
DEF VAR lDec AS DEC NO-UNDO.
DEF VAR cTransactionId AS CHAR FORMAT "x(20)" NO-UNDO.

dDato = 05/01/2014.

form 
with frame Q Down.

for each Butiker no-lock:
  for each BongHode no-lock where
    BongHode.ButikkNr = Butiker.Butik and
    BongHode.Dato     >= dDato:

    FOR EACH BongLinje NO-LOCK WHERE
      BongLinje.B_Id     = BongHode.B_Id AND
      BongLinje.Makulert = FALSE AND
      CAN-DO("095",STRING(BongLinje.TTId,"999")):

      cTransactionID = ''.
      RUN LoggNets.
      
      display
        BongHode.ButikkNr
        BongHode.Dato
        BongLinje.BongTekst
        cTransactionId
      with frame Q down width 300.
      down 1 with frame Q.
      
    end.    
  end.
end.

PROCEDURE LoggNets:
    IF Bonglinje.BongTekst BEGINS 'Ref.:' OR 
       BongLinje.BongTekst BEGINS 'REF:' THEN 
    LOGG_NETS:
    DO:
      /* Gammel format */
      IF Bonglinje.BongTekst BEGINS 'Ref.:' THEN 
      DO:
        ASSIGN lDec = DECIMAL(TRIM(ENTRY(2,Bonglinje.BongTekst,' '))) NO-ERROR.
        IF ERROR-STATUS:ERROR THEN 
          LEAVE LOGG_NETS.

        ASSIGN lDec = DECIMAL(TRIM(ENTRY(3,Bonglinje.BongTekst,' '))) NO-ERROR.
        IF ERROR-STATUS:ERROR THEN 
          LEAVE LOGG_NETS.
        ASSIGN cTransactionId = TRIM(ENTRY(2,Bonglinje.BongTekst,' ')) + ' ' + TRIM(ENTRY(3,Bonglinje.BongTekst,' ')).
      END.
      
      /* Nytt format */
      ELSE IF LENGTH(TRIM(ENTRY(2,Bonglinje.BongTekst,' '))) = 12 THEN
      DO:
        ASSIGN lDec = DECIMAL(TRIM(ENTRY(2,Bonglinje.BongTekst,' '))) NO-ERROR.
        IF ERROR-STATUS:ERROR THEN 
          LEAVE LOGG_NETS.
        ASSIGN cTransactionId = SUBSTRING(TRIM(ENTRY(2,Bonglinje.BongTekst,' ')),1,6) + ' ' + SUBSTRING(TRIM(ENTRY(2,Bonglinje.BongTekst,' ')),7,6).
      END.

      ELSE LEAVE LOGG_NETS.

      IF cTransactionID > '' THEN 
      DO TRANSACTION:                                   
        FIND FIRST Nets NO-LOCK WHERE
          Nets.B_Id          = BongHode.B_Id AND
          Nets.TransactionId = cTransactionId NO-ERROR.
        IF NOT AVAILABLE Nets THEN
        DO:
          CREATE Nets.
          ASSIGN
            Nets.B_Id           = BongHode.B_Id
            Nets.TransactionId  = cTransactionId
            Nets.iJBoxCompanyId = 175
            Nets.ButikkNr       = BongLinje.ButikkNr
            Nets.Dato           = BongHode.Dato
            .
          RELEASE Nets.
        END.
      END. /* TRANSACTION */
    END. /* LOGG_NETS */

END PROCEDURE.

