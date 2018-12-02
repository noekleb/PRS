  FIELD Aktivert LIKE KampanjeHode.Aktivert VALIDATE ~
  FIELD Beskrivelse LIKE KampanjeHode.Beskrivelse VALIDATE ~
  FIELD KampanjeId LIKE KampanjeHode.KampanjeId VALIDATE ~
  FIELD Notat LIKE KampanjeHode.Notat VALIDATE ~
  FIELD SluttDato LIKE KampanjeHode.SluttDato VALIDATE ~
  FIELD StartDato LIKE KampanjeHode.StartDato VALIDATE ~
  FIELD RegistrertDato LIKE KampanjeHode.RegistrertDato VALIDATE ~
  FIELD RegistrertTid LIKE KampanjeHode.RegistrertTid VALIDATE ~
  FIELD EDato LIKE KampanjeHode.EDato VALIDATE ~
  FIELD ETid LIKE KampanjeHode.ETid VALIDATE ~
  FIELD BrukerID LIKE KampanjeHode.BrukerID VALIDATE ~
  FIELD RegistrertAv LIKE KampanjeHode.RegistrertAv VALIDATE ~
  FIELD KannAktiveres AS CHARACTER FORMAT "x(8)"~
  FIELD ProfilNr LIKE KampanjeHode.ProfilNr VALIDATE ~
  FIELD AktiveresTid LIKE KampanjeHode.AktiveresTid VALIDATE ~
  FIELD GyldigTilTid LIKE KampanjeHode.GyldigTilTid VALIDATE ~
  FIELD Komplett LIKE KampanjeHode.Komplett VALIDATE ~
  FIELD NormalPris LIKE KampanjeHode.NormalPris VALIDATE ~
  FIELD Kamp% LIKE KampanjeHode.Kamp% VALIDATE ~
  FIELD KampanjePris LIKE KampanjeHode.KampanjePris VALIDATE ~
  FIELD AvslagType LIKE KampanjeHode.AvslagType VALIDATE ~
  FIELD setAnnonse LIKE KampanjeHode.setAnnonse VALIDATE ~
  FIELD fAktiveresTid AS CHARACTER FORMAT "x(5)" LABEL "Fra tid"~
  FIELD fGyldigTidTil AS CHARACTER FORMAT "x(5)" LABEL "Til tid"~
  FIELD LeverandorKampanje LIKE KampanjeHode.LeverandorKampanje VALIDATE ~
  FIELD KampId LIKE KampanjeHode.KampId VALIDATE 
