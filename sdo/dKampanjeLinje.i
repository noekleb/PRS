  FIELD KampanjeId LIKE KampanjeLinje.KampanjeId VALIDATE ~
  FIELD Vg LIKE KampanjeLinje.Vg VALIDATE ~
  FIELD LopNr LIKE KampanjeLinje.LopNr VALIDATE ~
  FIELD LevKod AS CHARACTER FORMAT "x(20)" LABEL "Lev.art.nr"~
  FIELD ArtikkelNr LIKE KampanjeLinje.ArtikkelNr VALIDATE ~
  FIELD Beskr AS CHARACTER FORMAT "x(20)"~
  FIELD RegistrertDato LIKE KampanjeLinje.RegistrertDato VALIDATE ~
  FIELD RegistrertAv LIKE KampanjeLinje.RegistrertAv VALIDATE ~
  FIELD EDato LIKE KampanjeLinje.EDato VALIDATE ~
  FIELD BrukerID LIKE KampanjeLinje.BrukerID VALIDATE ~
  FIELD Pris1 LIKE KampanjeLinje.Pris[1] VALIDATE ~
  FIELD Pris2 LIKE KampanjeLinje.Pris[2] VALIDATE  FORMAT ">>>,>>9.99"~
  FIELD ProfilNr LIKE KampanjeLinje.ProfilNr VALIDATE ~
  FIELD RegistrertTid LIKE KampanjeLinje.RegistrertTid VALIDATE ~
  FIELD ETid LIKE KampanjeLinje.ETid VALIDATE ~
  FIELD Behandlet LIKE KampanjeLinje.Behandlet VALIDATE ~
  FIELD BildNr AS INTEGER FORMAT "zzzzz9"~
  FIELD Feilkode LIKE KampanjeLinje.Feilkode VALIDATE ~
  FIELD VareKost LIKE KampanjeLinje.VareKost VALIDATE ~
  FIELD Endring% AS DECIMAL FORMAT "->>9.9" LABEL "Endring%"~
  FIELD Konflikt AS CHARACTER FORMAT "x(150)" LABEL "Konflikt"~
  FIELD Farge AS CHARACTER FORMAT "x(15)" LABEL "Farge"~
  FIELD NormalPris AS DECIMAL FORMAT ">>>,>>9.99" LABEL "Normalpris"~
  FIELD LevFargKod AS CHARACTER FORMAT "x(20)" LABEL "Lev.fargekode"
