  FIELD Antall LIKE OvBuffer.Antall VALIDATE ~
  FIELD fVgBeskr AS CHARACTER FORMAT "x(30)"~
  FIELD ArtikkelNr LIKE OvBuffer.ArtikkelNr VALIDATE ~
  FIELD BrukerID LIKE OvBuffer.BrukerID VALIDATE ~
  FIELD fBeskr AS CHARACTER FORMAT "x(30)" LABEL "Beskrivelse"~
  FIELD ButikkNrFra LIKE OvBuffer.ButikkNrFra VALIDATE ~
  FIELD ButikkNrTil LIKE OvBuffer.ButikkNrTil VALIDATE ~
  FIELD fFraButikk AS CHARACTER FORMAT "x(30)" LABEL "Fra butikk"~
  FIELD EDato LIKE OvBuffer.EDato VALIDATE ~
  FIELD ETid LIKE OvBuffer.ETid VALIDATE ~
  FIELD fTilbutikk AS CHARACTER FORMAT "x(30)" LABEL "Til butikk"~
  FIELD LinjeNr LIKE OvBuffer.LinjeNr VALIDATE ~
  FIELD LopNr LIKE OvBuffer.LopNr VALIDATE ~
  FIELD fKlOpprettet AS CHARACTER FORMAT "x(8)" LABEL "Kl"~
  FIELD Merknad LIKE OvBuffer.Merknad VALIDATE ~
  FIELD RegistrertAv LIKE OvBuffer.RegistrertAv VALIDATE ~
  FIELD fKlEndret AS CHARACTER FORMAT "x(8)" LABEL "Kl"~
  FIELD RegistrertDato LIKE OvBuffer.RegistrertDato VALIDATE ~
  FIELD RegistrertTid LIKE OvBuffer.RegistrertTid VALIDATE ~
  FIELD Storl LIKE OvBuffer.Storl VALIDATE ~
  FIELD TilStorl LIKE OvBuffer.TilStorl VALIDATE ~
  FIELD Vg LIKE OvBuffer.Vg VALIDATE ~
  FIELD BuntNr LIKE OvBuffer.BuntNr VALIDATE ~
  FIELD Mva% LIKE OvBuffer.Mva% VALIDATE ~
  FIELD MvaKr LIKE OvBuffer.MvaKr VALIDATE ~
  FIELD VareKost LIKE OvBuffer.VareKost VALIDATE ~
  FIELD fDatoOppdatert AS DATE FORMAT "99/99/99"~
  FIELD BildNr AS INTEGER FORMAT "zzzzz9"
