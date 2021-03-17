current-window:width = 300.

def var piLoop as int no-undo.
def var cFilNavn as char no-undo.

assign
  cFilNavn = 'c:\appdir\EANListe' + 
              REPLACE(STRING(TODAY),'/','') + 
              '_' +
              REPLACE(STRING(TIME,"HH:MM:SS"),':','') +
              '.csv'
  .

def temp-table EANListe 
  field Strekkode as char format "x(20)"
  field Bongtekst as char format "x(25)"
  field ArtikkelNr as char format "x(20)"
  field Storrelse as char format "x(10)"
  field Pris as dec format "->>>,>>>,>>9.99"
  field Dato as date format "99/99/9999"
  FIELD VarGr AS INT FORMAT ">>>>>9"
  FIELD LevNr AS INT FORMAT ">>>>>9"
  field VareKost as dec format "->>>,>>>,>>9.99"
  index EAN Strekkode.
def stream Ut.
  
BLOKKEN:  
for each BongHode no-lock where
  BongHode.ButikkNr = 212 and 
  BongHode.Dato >= 01/01/2010,
  EACH BongLinje no-lock where BongLinje.B_Id = BongHode.B_Id and 
  can-do('1,2,3,4,5,6,7,8,9,10,11',STRING(BongLinje.TTID)) and 
  BongLinje.Makulert = false:
  
  if (not can-find(first Strekkode where
                  Strekkode.Kode = BongLinje.Strekkode) and 
      not can-find(first EANListe where
                  EANListe.Strekkode = BongLinje.Strekkode)) then 
  do:
    create EANListe.
    assign
      EANListe.Strekkode  = BongLinje.Strekkode
      EANListe.BongTekst  = TRIM(BongLinje.BongTekst)
      EANListe.ArtikkelNr = TRIM(BongLinje.ArtikkelNr)
      EANListe.Storrelse  = BongLinje.STorrelse
      .
  end.
  else find first EANListe where
    EANListe.Strekkode = BongLinje.Strekkode no-error.
    
  if available EANListe then 
  assign
    EANListe.Pris     = BongLinje.LinjeSum / ABS(BongLinje.Antall)
    EANListe.Dato     = BongLinje.Dato
    EANListe.VarGr    = BongLinje.VareGr
    EANListe.LevNr    = BongLinje.LevNr
    EANListe.Varekost = BongLinje.VVareKost.

  
  piLoop = piLoop + 1.
  /*  
  display
  BongLinje.Dato
  BongHode.BongNr
  BongLinje.TTId
  BongLinje.StreKKode
  BongLinje.ArtikkelNr
  BongLinje.BongTekst
  BongLinje.Storrelse
  BongLinje.Antall
  BongLinje.LinjeSum / ABS(BongLinje.Antall)
  with width 300.
  if piLoop > 100 then   
  LEAVE BLOKKEN.  
  */
end. /* BLOKKEN */

/* Legger ut alle EAN koder */
output stream Ut to value(cFilNavn) no-echo.
for each EANListe:
  put stream Ut unformatted
  /* 1 */ EANListe.Strekkode ';'
  /* 2 */ EANListe.BongTekst ';'
  /* 3 */ EANListe.ArtikkelNr ';'
  /* 4 */ EANListe.Storrelse ';'
  /* 5 */ EANListe.Dato ';'
  /* 6 */ EANListe.Pris ';'
  /* 7 */ EANListe.VarGr ';'
  /* 8 */ EANListe.LevNr ';'
  /* 9 */ EANListe.Varekost
  skip.
end.
output stream Ut close.
