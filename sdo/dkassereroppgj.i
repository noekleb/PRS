  FIELD ButikkNr LIKE KassererOppgj.ButikkNr VALIDATE ~
  FIELD Dato LIKE KassererOppgj.Dato VALIDATE ~
  FIELD KassererNr LIKE KassererOppgj.KassererNr VALIDATE ~
  FIELD z_nummer LIKE KassererOppgj.z_nummer VALIDATE ~
  FIELD OpptaltInnVeksel LIKE KassererOppgj.OpptaltInnVeksel VALIDATE ~
  FIELD OpptaltKontanter LIKE KassererOppgj.OpptaltKontanter VALIDATE ~
  FIELD OpptaltSjekk LIKE KassererOppgj.OpptaltSjekk VALIDATE ~
  FIELD OpptaltValuta LIKE KassererOppgj.OpptaltValuta VALIDATE ~
  FIELD OpptaltReserve LIKE KassererOppgj.OpptaltReserve VALIDATE ~
  FIELD OpptaltGavekort LIKE KassererOppgj.OpptaltGavekort VALIDATE ~
  FIELD OpptaltGavekortAndre LIKE KassererOppgj.OpptaltGavekortAndre VALIDATE ~
  FIELD OpptaltGavekortUtlevert LIKE KassererOppgj.OpptaltGavekortUtlevert VALIDATE ~
  FIELD OpptaltTilgode LIKE KassererOppgj.OpptaltTilgode VALIDATE ~
  FIELD OpptaltTilgodeAndre LIKE KassererOppgj.OpptaltTilgodeAndre VALIDATE ~
  FIELD OpptaltTilgodeUtlevert LIKE KassererOppgj.OpptaltTilgodeUtlevert VALIDATE ~
  FIELD OpptaltBilag LIKE KassererOppgj.OpptaltBilag VALIDATE ~
  FIELD OpptaltVeksel LIKE KassererOppgj.OpptaltVeksel VALIDATE ~
  FIELD OpptaltLevertBank LIKE KassererOppgj.OpptaltLevertBank VALIDATE ~
  FIELD PoseNr LIKE KassererOppgj.PoseNr VALIDATE ~
  FIELD fuEndretInfo AS CHARACTER FORMAT "x(40)" LABEL "Opprettet/endret"~
  FIELD RegistrertAv LIKE KassererOppgj.RegistrertAv VALIDATE ~
  FIELD RegistrertDato LIKE KassererOppgj.RegistrertDato VALIDATE ~
  FIELD RegistrertTid LIKE KassererOppgj.RegistrertTid VALIDATE ~
  FIELD BrukerID LIKE KassererOppgj.BrukerID VALIDATE ~
  FIELD EDato LIKE KassererOppgj.EDato VALIDATE ~
  FIELD ETid LIKE KassererOppgj.ETid VALIDATE ~
  FIELD fuValorer AS LOGICAL FORMAT "yes/no" LABEL "Valører"~
  FIELD fuBilag AS LOGICAL FORMAT "yes/no" LABEL "Bilag"~
  FIELD fuValuta AS LOGICAL FORMAT "yes/no" LABEL "Valuta"~
  FIELD OpptaltKupong LIKE KassererOppgj.OpptaltKupong VALIDATE 
