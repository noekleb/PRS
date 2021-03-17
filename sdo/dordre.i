  FIELD BrukerID LIKE Ordre.BrukerID VALIDATE ~
  FIELD fuLevPostSted AS CHARACTER FORMAT "x(30)" LABEL "Poststed"~
  FIELD EDato LIKE Ordre.EDato VALIDATE ~
  FIELD EkstId LIKE Ordre.EkstId VALIDATE  LABEL "Ekstern ref"~
  FIELD ETid LIKE Ordre.ETid VALIDATE ~
  FIELD LapTop LIKE Ordre.LapTop VALIDATE  LABEL "LT"~
  FIELD LevAdresse1 LIKE Ordre.LevAdresse1 VALIDATE ~
  FIELD fStatusTxt AS CHARACTER FORMAT "x(8)" LABEL "Statustekst"~
  FIELD LevAdresse2 LIKE Ordre.LevAdresse2 VALIDATE ~
  FIELD LevKontakt LIKE Ordre.LevKontakt VALIDATE ~
  FIELD LevMerknad LIKE Ordre.LevMerknad VALIDATE ~
  FIELD LevNr LIKE Ordre.LevNr VALIDATE ~
  FIELD LevPostBoks LIKE Ordre.LevPostBoks VALIDATE ~
  FIELD LevPostNr LIKE Ordre.LevPostNr VALIDATE ~
  FIELD LevTelefon LIKE Ordre.LevTelefon VALIDATE ~
  FIELD Merknad LIKE Ordre.Merknad VALIDATE ~
  FIELD Notat LIKE Ordre.Notat VALIDATE ~
  FIELD OrdreNr LIKE Ordre.OrdreNr VALIDATE  FORMAT "zzzzzzz9"~
  FIELD OrdreStatus LIKE Ordre.OrdreStatus VALIDATE ~
  FIELD RegistrertAv LIKE Ordre.RegistrertAv VALIDATE ~
  FIELD RegistrertDato LIKE Ordre.RegistrertDato VALIDATE ~
  FIELD fLevNamn AS CHARACTER FORMAT "x(40)" LABEL "Levnavn"~
  FIELD RegistrertTid LIKE Ordre.RegistrertTid VALIDATE ~
  FIELD SendtDato LIKE Ordre.SendtDato VALIDATE ~
  FIELD BekreftetAv LIKE Ordre.BekreftetAv VALIDATE ~
  FIELD BekreftetDato LIKE Ordre.BekreftetDato VALIDATE ~
  FIELD BekreftetOrdre LIKE Ordre.BekreftetOrdre VALIDATE ~
  FIELD fraERP LIKE Ordre.fraERP VALIDATE ~
  FIELD HkOrdre LIKE Ordre.HkOrdre VALIDATE ~
  FIELD LeveringsDato LIKE Ordre.LeveringsDato VALIDATE ~
  FIELD VareBehNr LIKE Ordre.VareBehNr VALIDATE ~
  FIELD CL LIKE Ordre.CL VALIDATE ~
  FIELD Hasteordre LIKE Ordre.Hasteordre VALIDATE  FORMAT "*/"~
  FIELD sendtButikkDato LIKE Ordre.sendtButikkDato VALIDATE ~
  FIELD sendtButikkFlagg LIKE Ordre.sendtButikkFlagg VALIDATE ~
  FIELD sendtButikkTid LIKE Ordre.sendtButikkTid VALIDATE ~
  FIELD ULevNr LIKE Ordre.ULevNr VALIDATE ~
  FIELD OrdreMottaker LIKE Ordre.OrdreMottaker VALIDATE ~
  FIELD Opphav LIKE Ordre.Opphav VALIDATE 
