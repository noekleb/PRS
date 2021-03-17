DEFINE VARIABLE iAntiGive          AS INTEGER FORMAT "->>,>>9" NO-UNDO.
DEFINE VARIABLE lVerdiiGive        AS DECIMAL FORMAT "->>>,>>>,>>9.99" NO-UNDO.
DEFINE VARIABLE cigiveArtNr        AS CHARACTER                NO-UNDO.
DEF VAR iButNr AS INT NO-UNDO.
DEF VAR dDato AS DATE NO-UNDO.

  ASSIGN 
    iAntigive   = 0
    lVerdiiGive = 0
    ciGiveArtNr = '9002'
      ibutNr = 2
      dDato = 05/14/2020
    .
  FOR EACH Kasse NO-LOCK WHERE 
      Kasse.ButikkNr = 2 AND 
      Kasse.GruppeNr = 1 AND 
      Kasse.KasseNr <= 90,
      EACH bongHode NO-LOCK WHERE 
          BongHode.ButikkNr = Kasse.butikkNr AND 
          BongHode.GruppeNr = Kasse.GruppeNr AND          
          BongHode.KasseNr = Kasse.KasseNr AND
          BongHode.Dato = dDato AND
          CAN-FIND(FIRST BongLinje WHERE 
                   BongLinje.B_Id = BongHode.B_Id AND 
                   BongLinje.TTId = 1),
      EACH BongLinje NO-LOCK WHERE 
          BongLinje.ButikkNr = Kasse.ButikkNr AND 
          BongLinje.GruppeNr = Kasse.GruppeNr AND 
          BongLinje.KasseNr = Kasse.KasseNr AND
          BongLinje.Dato = BongHode.Dato AND 
          BongLinje.TTId = 1 AND 
          BongLinje.ArtikkelNr = cigiveArtNr:
    ASSIGN
      iAntigive   = iAntiGive   + 1
      lVerdiiGive = lVerdiiGive + BongLinje.LinjeSum
      .
  END.

MESSAGE 'GURRE:'
iantigive lVerdiiGive ciGiveArtNr
VIEW-AS ALERT-BOX.
