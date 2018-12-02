  FIELD ArtikkelNr LIKE ArtBas.ArtikkelNr VALIDATE ~
  FIELD fuBildeFilNavn AS CHARACTER FORMAT "x(20)" LABEL "Bilde"~
  FIELD Vg LIKE ArtBas.Vg VALIDATE ~
  FIELD LopNr LIKE ArtBas.LopNr VALIDATE  FORMAT "zzzzz9"~
  FIELD Beskr LIKE ArtBas.Beskr VALIDATE  FORMAT "x(30)"~
  FIELD BongTekst LIKE ArtBas.BongTekst VALIDATE ~
  FIELD Aktivert LIKE ArtBas.Aktivert VALIDATE  FORMAT "*/" LABEL "A"~
  FIELD fSesong AS CHARACTER FORMAT "x(10)" LABEL "Sesong"~
  FIELD IKasse LIKE ArtBas.IKasse VALIDATE  FORMAT "*/" LABEL "IK"~
  FIELD LevNr LIKE ArtBas.LevNr VALIDATE ~
  FIELD LevKod LIKE ArtBas.LevKod VALIDATE ~
  FIELD LevFargKod LIKE ArtBas.LevFargKod VALIDATE ~
  FIELD Farg LIKE ArtBas.Farg VALIDATE  FORMAT "zzzz9"~
  FIELD SaSong LIKE ArtBas.SaSong VALIDATE ~
  FIELD Pakke LIKE ArtBas.Pakke VALIDATE ~
  FIELD Lokasjon LIKE ArtBas.Lokasjon VALIDATE ~
  FIELD fuPris AS DECIMAL FORMAT "->>>,>>9.99" LABEL "Pris"~
  FIELD OPris LIKE ArtBas.OPris VALIDATE ~
  FIELD BildeIKasse LIKE ArtBas.BildeIKasse VALIDATE ~
  FIELD fuVarekost AS DECIMAL FORMAT "->>>,>>9.99" LABEL "Varekost"~
  FIELD HkStyrt LIKE ArtBas.HkStyrt VALIDATE ~
  FIELD LokPris LIKE ArtBas.LokPris VALIDATE ~
  FIELD BildNr LIKE ArtBas.BildNr VALIDATE ~
  FIELD fFargBeskr AS CHARACTER FORMAT "x(20)" LABEL "Farge"~
  FIELD RegistrertDato LIKE ArtBas.RegistrertDato VALIDATE ~
  FIELD EDato LIKE ArtBas.EDato VALIDATE ~
  FIELD Hg LIKE ArtBas.Hg VALIDATE ~
  FIELD fLevNamn AS CHARACTER FORMAT "x(20)" LABEL "Leverandør"
