DEFINE {&NEW} SHARED TEMP-TABLE ttUtskrKopi
  FIELD ButikkNr  AS INTE
  FIELD GruppeNr  AS INTE INITIAL 1
  FIELD KasseNr   AS INTE
  FIELD Dato      AS DATE
  FIELD BongNr    AS INTE
  FIELD Filnavn   AS CHAR
  FIELD Tekst     AS CHAR
  FIELD MedlemsNr AS CHAR
  FIELD TekstIdx  AS CHAR
  INDEX UtskrKopi IS PRIMARY 
         Filnavn  ASCENDING
         ButikkNr ASCENDING
         GruppeNr ASCENDING
         KasseNr  ASCENDING
         Dato     ASCENDING
         BongNr   ASCENDING.
