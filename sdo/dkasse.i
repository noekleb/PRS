  FIELD BrukerId LIKE Kasse.BrukerId VALIDATE ~
  FIELD fuModellNavn AS CHARACTER FORMAT "x(20)" LABEL "Modell"~
  FIELD ButikkNr LIKE Kasse.ButikkNr VALIDATE ~
  FIELD fuKortNavn AS CHARACTER FORMAT "x(8)" LABEL "KortNavn"~
  FIELD EDato LIKE Kasse.EDato VALIDATE ~
  FIELD ETid LIKE Kasse.ETid VALIDATE ~
  FIELD GruppeNr LIKE Kasse.GruppeNr VALIDATE ~
  FIELD KasseNr LIKE Kasse.KasseNr VALIDATE ~
  FIELD LayoutNr LIKE Kasse.LayoutNr VALIDATE ~
  FIELD Navn LIKE Kasse.Navn VALIDATE ~
  FIELD RegistrertAv LIKE Kasse.RegistrertAv VALIDATE ~
  FIELD RegistrertDato LIKE Kasse.RegistrertDato VALIDATE ~
  FIELD RegistrertTid LIKE Kasse.RegistrertTid VALIDATE ~
  FIELD fuGruppeNavn AS CHARACTER FORMAT "x(30)" LABEL "Navn"~
  FIELD Aktiv LIKE Kasse.Aktiv VALIDATE  FORMAT "*/"~
  FIELD ElJournal1 LIKE Kasse.ElJournal[1] VALIDATE ~
  FIELD ElJournal2 LIKE Kasse.ElJournal[2] VALIDATE ~
  FIELD Kvittering1 LIKE Kasse.Kvittering[1] VALIDATE ~
  FIELD Kvittering2 LIKE Kasse.Kvittering[2] VALIDATE ~
  FIELD Utskriftskopi1 LIKE Kasse.Utskriftskopi[1] VALIDATE ~
  FIELD Utskriftskopi2 LIKE Kasse.Utskriftskopi[2] VALIDATE ~
  FIELD DagsOpgj1 LIKE Kasse.DagsOpgj[1] VALIDATE ~
  FIELD DagsOpgj2 LIKE Kasse.DagsOpgj[2] VALIDATE ~
  FIELD KassererOpgj1 LIKE Kasse.KassererOpgj[1] VALIDATE ~
  FIELD KassererOpgj2 LIKE Kasse.KassererOpgj[2] VALIDATE  LABEL "Ekstent"~
  FIELD DagsOppgj LIKE Kasse.DagsOppgj VALIDATE  FORMAT "X(50)"~
  FIELD DagsOppgjAktiv LIKE Kasse.DagsOppgjAktiv VALIDATE  FORMAT "*/" LABEL "DagsOppgj"~
  FIELD DagsOppgjKatalog LIKE Kasse.DagsOppgjKatalog VALIDATE  FORMAT "X(50)"~
  FIELD DagsOppgjKonv LIKE Kasse.DagsOppgjKonv VALIDATE ~
  FIELD ElJournalAktiv LIKE Kasse.ElJournalAktiv VALIDATE  FORMAT "*/" LABEL "ElJournal"~
  FIELD ElJournalId LIKE Kasse.ElJournalId VALIDATE  FORMAT "X(50)"~
  FIELD ElJournalKatalog LIKE Kasse.ElJournalKatalog VALIDATE  FORMAT "X(50)"~
  FIELD ElJournalKonv LIKE Kasse.ElJournalKonv VALIDATE ~
  FIELD KassererOppgjAktiv LIKE Kasse.KassererOppgjAktiv VALIDATE  FORMAT "*/" LABEL "KassererOppgj"~
  FIELD KassererOppgjId LIKE Kasse.KassererOppgjId VALIDATE  FORMAT "X(50)"~
  FIELD KassererOppgjKatalog LIKE Kasse.KassererOppgjKatalog VALIDATE  FORMAT "X(50)"~
  FIELD KassererOppgjKonv LIKE Kasse.KassererOppgjKonv VALIDATE ~
  FIELD KvitteringAktiv LIKE Kasse.KvitteringAktiv VALIDATE  FORMAT "*/" LABEL "Kvittering"~
  FIELD KvitteringId LIKE Kasse.KvitteringId VALIDATE  FORMAT "X(50)"~
  FIELD KvitteringKatalog LIKE Kasse.KvitteringKatalog VALIDATE  FORMAT "X(50)"~
  FIELD KvitteringKonv LIKE Kasse.KvitteringKonv VALIDATE ~
  FIELD UtskriftskopiAktiv LIKE Kasse.UtskriftskopiAktiv VALIDATE  FORMAT "*/" LABEL "Utskriftskopi"~
  FIELD UtskriftsKopiId LIKE Kasse.UtskriftsKopiId VALIDATE  FORMAT "X(50)"~
  FIELD UtskriftskopiKatalog LIKE Kasse.UtskriftskopiKatalog VALIDATE  FORMAT "X(50)"~
  FIELD UTskriftskopiKonv LIKE Kasse.UTskriftskopiKonv VALIDATE ~
  FIELD DagsOppgjId LIKE Kasse.DagsOppgjId VALIDATE  FORMAT "X(50)"~
  FIELD DagsOppgjOperand LIKE Kasse.DagsOppgjOperand VALIDATE ~
  FIELD ElJournalOperand LIKE Kasse.ElJournalOperand VALIDATE ~
  FIELD KassererOppgjOperand LIKE Kasse.KassererOppgjOperand VALIDATE ~
  FIELD KvitteringOperand LIKE Kasse.KvitteringOperand VALIDATE ~
  FIELD UtskriftsKopiOperand LIKE Kasse.UtskriftsKopiOperand VALIDATE ~
  FIELD DagsOppgjBehandle LIKE Kasse.DagsOppgjBehandle VALIDATE ~
  FIELD DagsOppgjInnles LIKE Kasse.DagsOppgjInnles VALIDATE ~
  FIELD ElJournalBehandle LIKE Kasse.ElJournalBehandle VALIDATE ~
  FIELD ElJournalInnles LIKE Kasse.ElJournalInnles VALIDATE ~
  FIELD KassererOppgjBehandle LIKE Kasse.KassererOppgjBehandle VALIDATE ~
  FIELD KassererOppgjInnles LIKE Kasse.KassererOppgjInnles VALIDATE ~
  FIELD KvitteringBehandle LIKE Kasse.KvitteringBehandle VALIDATE ~
  FIELD KvitteringInnles LIKE Kasse.KvitteringInnles VALIDATE ~
  FIELD UtskriftskopiBehandle LIKE Kasse.UtskriftskopiBehandle VALIDATE ~
  FIELD UtskriftskopiInnles LIKE Kasse.UtskriftskopiInnles VALIDATE ~
  FIELD ModellNr LIKE Kasse.ModellNr VALIDATE ~
  FIELD FakturaKopi LIKE Kasse.FakturaKopi VALIDATE ~
  FIELD FakturaLayout LIKE Kasse.FakturaLayout VALIDATE ~
  FIELD Fakturaskriver LIKE Kasse.Fakturaskriver VALIDATE 
