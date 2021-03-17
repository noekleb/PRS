  FIELD Aar LIKE Bokforingsbilag.Aar VALIDATE ~
  FIELD fuEndretKl AS CHARACTER FORMAT "x(8)" LABEL "Kl"~
  FIELD fuSendtKl AS CHARACTER FORMAT "x(8)" LABEL "Kl"~
  FIELD fuRegistrertKl AS CHARACTER FORMAT "x(8)" LABEL "Kl"~
  FIELD fuGodkjentKl AS CHARACTER FORMAT "x(8)" LABEL "Kl"~
  FIELD BokforingsNr LIKE Bokforingsbilag.BokforingsNr VALIDATE ~
  FIELD BrukerID LIKE Bokforingsbilag.BrukerID VALIDATE ~
  FIELD ButikkNr LIKE Bokforingsbilag.ButikkNr VALIDATE ~
  FIELD EDato LIKE Bokforingsbilag.EDato VALIDATE ~
  FIELD ETid LIKE Bokforingsbilag.ETid VALIDATE ~
  FIELD OmsetningsDato LIKE Bokforingsbilag.OmsetningsDato VALIDATE ~
  FIELD RegistrertAv LIKE Bokforingsbilag.RegistrertAv VALIDATE ~
  FIELD RegistrertDato LIKE Bokforingsbilag.RegistrertDato VALIDATE ~
  FIELD RegistrertTid LIKE Bokforingsbilag.RegistrertTid VALIDATE ~
  FIELD SendAv LIKE Bokforingsbilag.SendAv VALIDATE ~
  FIELD SendtDato LIKE Bokforingsbilag.SendtDato VALIDATE ~
  FIELD SendtRegnskap LIKE Bokforingsbilag.SendtRegnskap VALIDATE  FORMAT "*/"~
  FIELD SendtTid LIKE Bokforingsbilag.SendtTid VALIDATE ~
  FIELD GodkjentAv LIKE Bokforingsbilag.GodkjentAv VALIDATE ~
  FIELD GodkjentDato LIKE Bokforingsbilag.GodkjentDato VALIDATE ~
  FIELD GodkjentFlagg LIKE Bokforingsbilag.GodkjentFlagg VALIDATE ~
  FIELD GodkjentTid LIKE Bokforingsbilag.GodkjentTid VALIDATE ~
  FIELD EODDato LIKE Bokforingsbilag.EODDato VALIDATE ~
  FIELD EODMottatt LIKE Bokforingsbilag.EODMottatt VALIDATE 
