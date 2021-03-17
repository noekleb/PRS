  FIELD FilId LIKE VPIFilLogg.FilId VALIDATE ~
  FIELD LinjeNr LIKE VPIFilLogg.LinjeNr VALIDATE ~
  FIELD Gradering LIKE VPIFilLogg.Gradering VALIDATE ~
  FIELD fuGraderingFillogg AS CHARACTER FORMAT "x(15)" LABEL "Gradering"~
  FIELD Tekst LIKE VPIFilLogg.Tekst VALIDATE  FORMAT "X(180)"
