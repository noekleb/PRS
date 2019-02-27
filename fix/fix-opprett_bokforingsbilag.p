DEF VAR piBokforingsNr AS INT NO-UNDO.

DEF BUFFER bBokforingsbilag FOR Bokforingsbilag.      

FOR EACH Kas_Rap NO-LOCK WHERE
    Kas_Rap.Dato >= 01/01/2008:
    IF NOT CAN-FIND(FIRST Bokforingsbilag WHERE
                    Bokforingsbilag.ButikkNr = Kas_Rap.Butik AND
                    Bokforingsbilag.OmsetningsDato = Kas_Rap.Dato) THEN
    DO:
      FIND LAST bBokforingsbilag NO-LOCK WHERE
          bBokforingsbilag.ButikkNr = Kas_Rap.Butik AND
          bBokforingsbilag.Aar      = YEAR(Kas_Rap.Dato) 
          USE-INDEX BokforingsBilag NO-ERROR.
      IF AVAILABLE bBokforingsbilag THEN
          piBokforingsNr = bBokforingsbilag.BokforingsNr + 1.
      ELSE
          piBokforingsNr = 1.

      CREATE Bokforingsbilag.
      ASSIGN
          Bokforingsbilag.ButikkNr           = Kas_Rap.Butik
          Bokforingsbilag.Aar                = YEAR(Kas_Rap.Dato)
          Bokforingsbilag.OmsetningsDato     = Kas_Rap.Dato
          BokforingsBilag.BokforingsNr       = piBokforingsNr
          .
    END.
END.
