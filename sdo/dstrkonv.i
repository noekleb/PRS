  FIELD StrKode LIKE StrKonv.StrKode VALIDATE  FORMAT "z999"~
  FIELD Storl LIKE StrKonv.Storl VALIDATE  FORMAT "X(10)"~
  FIELD SeqNr LIKE StrKonv.SeqNr VALIDATE ~
  FIELD Merknad LIKE StrKonv.Merknad VALIDATE ~
  FIELD fBrukt AS LOGICAL FORMAT "J/N" LABEL "Brukt"
