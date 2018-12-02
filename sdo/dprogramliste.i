  FIELD ProgNavn LIKE ProgramListe.ProgNavn VALIDATE  FORMAT "X(30)"~
  FIELD Programbeskrivelse LIKE ProgramListe.Programbeskrivelse VALIDATE ~
  FIELD Gradering LIKE ProgramListe.grad VALIDATE ~
  FIELD fuGradering AS CHARACTER FORMAT "x(20)" LABEL "Tilgangsnivå"
