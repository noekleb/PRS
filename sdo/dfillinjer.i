  FIELD Datasett LIKE FilLinjer.Datasett VALIDATE  FORMAT "*/"~
  FIELD FilId LIKE FilLinjer.FilId VALIDATE ~
  FIELD LinjeNr LIKE FilLinjer.LinjeNr VALIDATE ~
  FIELD Tekst LIKE FilLinjer.Tekst VALIDATE  FORMAT "X(254)"~
  FIELD Behandlet LIKE FilLinjer.Behandlet VALIDATE  FORMAT "*/"~
  FIELD DataSettId LIKE FilLinjer.DataSettId VALIDATE ~
  FIELD StorTekst LIKE FilLinjer.StorTekst VALIDATE  FORMAT "X(248)"
