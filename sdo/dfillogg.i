  FIELD FilId LIKE FilLogg.FilId VALIDATE ~
  FIELD LinjeNr LIKE FilLogg.LinjeNr VALIDATE ~
  FIELD Tekst LIKE FilLogg.Tekst VALIDATE  FORMAT "X(245)"~
  FIELD Gradering LIKE FilLogg.Gradering VALIDATE ~
  FIELD fuGraderingFilLogg AS CHARACTER FORMAT "x(15)" LABEL "Gradering"
