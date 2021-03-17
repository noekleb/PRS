  FIELD Backup LIKE Filer.Backup VALIDATE  FORMAT "*/"~
  FIELD fuFilTypeTekst AS CHARACTER FORMAT "x(15)" LABEL "FilType"~
  FIELD Dato LIKE Filer.Dato VALIDATE ~
  FIELD Dobbel LIKE Filer.Dobbel VALIDATE  FORMAT "*/"~
  FIELD fuInnlestInfo AS CHARACTER FORMAT "x(40)" LABEL "Innlest"~
  FIELD Feil LIKE Filer.Feil VALIDATE  FORMAT "*/"~
  FIELD FilId LIKE Filer.FilId VALIDATE ~
  FIELD FilNavn LIKE Filer.FilNavn VALIDATE  FORMAT "X(40)"~
  FIELD Innlest LIKE Filer.Innlest VALIDATE  FORMAT "*/"~
  FIELD InnlestAv LIKE Filer.InnlestAv VALIDATE ~
  FIELD InnlestDato LIKE Filer.InnlestDato VALIDATE ~
  FIELD fuOppdatertInfo AS CHARACTER FORMAT "x(40)" LABEL "Oppdatert"~
  FIELD fuOverfortInfo AS CHARACTER FORMAT "x(40)" LABEL "Overfort"~
  FIELD InnlestKl LIKE Filer.InnlestKl VALIDATE ~
  FIELD Katalog LIKE Filer.Katalog VALIDATE ~
  FIELD Kl LIKE Filer.Kl VALIDATE ~
  FIELD Oppdatert LIKE Filer.Oppdatert VALIDATE  FORMAT "*/"~
  FIELD OppdatertAv LIKE Filer.OppdatertAv VALIDATE ~
  FIELD OppdatertDato LIKE Filer.OppdatertDato VALIDATE ~
  FIELD fuOppdatertKl AS CHARACTER FORMAT "x(8)" LABEL "Oppdatert kl"~
  FIELD OppdatertKl LIKE Filer.OppdatertKl VALIDATE ~
  FIELD Storrelse LIKE Filer.Storrelse VALIDATE ~
  FIELD AntLinjer LIKE Filer.AntLinjer VALIDATE ~
  FIELD SlettetAv LIKE Filer.SlettetAv VALIDATE ~
  FIELD SlettetDato LIKE Filer.SlettetDato VALIDATE ~
  FIELD Slettet LIKE Filer.Slettet VALIDATE  FORMAT "*/"~
  FIELD fuInnlestKl AS CHARACTER FORMAT "x(8)" LABEL "Innlest kl"~
  FIELD SlettetTid LIKE Filer.SlettetTid VALIDATE ~
  FIELD FilType LIKE Filer.FilType VALIDATE ~
  FIELD Overfort LIKE Filer.Overfort VALIDATE  FORMAT "*/"~
  FIELD fuSlettetInfo AS CHARACTER FORMAT "x(40)" LABEL "Slettet"~
  FIELD OverfortAv LIKE Filer.OverfortAv VALIDATE ~
  FIELD OverfortDato LIKE Filer.OverfortDato VALIDATE ~
  FIELD OverfortTid LIKE Filer.OverfortTid VALIDATE 
