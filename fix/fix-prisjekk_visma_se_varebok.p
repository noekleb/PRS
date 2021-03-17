current-window:width = 300.

def var cFilNavn      as char no-undo.
def var cFilUt        as char no-undo.
def var cLinje        as char format "x(80)" no-undo.
def var lArtikkelNr   as dec format ">>>>>>>>>>>>9" no-undo.
def var iStrKode      as int no-undo.
def var cVAreBokNrLSt as char no-undo.
def var piLoop        as int no-undo.
def var lVareBokNr    as dec no-undo.
def var cArtikkelNr   as char no-undo.
def var cHeading      as char no-undo.

def var bDiff11 as log no-undo.
def var bDiff12 as log no-undo.
def var bDiff13 as log no-undo.
def var bDiff14 as log no-undo.

def var bDiff21 as log no-undo.
def var bDiff22 as log no-undo.
def var bDiff23 as log no-undo.
def var bDiff24 as log no-undo.

def var bDiff31 as log no-undo.
def var bDiff32 as log no-undo.
def var bDiff33 as log no-undo.
def var bDiff34 as log no-undo.

def buffer buf1VareBokLinje for VareBokLinje.
def buffer buf2VarebokLinje for VarebokLinje.
def buffer buf3VarebokLinje for VarebokLinje.

assign
  cVAreBokNrLSt = "90000081,90000086,90000090"
  cFilNAvn      = "Q:\Appdir\Sport1HK\PrisSjekkVisma05032007\Visma register 270308.csv"
  cFilUt        = "Q:\Appdir\Sport1HK\PrisSjekkVisma05032007\Visma register sjekk 270308.csv.xls"
  cHeading = 
             /*  1*/ "Artikkelnr./ SE nr" + chr(9) +
             /*  2*/ "Alt. Artikkel / Modellnummer" + chr(9) +
             /*  3*/ "Art. nr. hovedlev" + chr(9) +
             /*  4*/ "Hovedlev." + chr(9) +
             /*  5*/ "Artikkel.navn" + chr(9) +
             /*  6*/ "Levfarge" + chr(9) +
             /*  7*/ "Størrelse" + chr(9) +
             /*  8*/ "Netto forh." + chr(9) +
             /*  9*/ "Anb. utpris" + chr(9) + 
             /* 10*/ "Kjedens Innpris" + chr(9) +
             /* 11*/ "Art.finnes" + chr(9) + 

             /* 12*/ "Varebok" + chr(9) + 
             /* 34*/ "Avvik" + chr(9) + 
             /* 35*/ "Netto forh. til butikk" + chr(9) +
             /* 13*/ "Avvik" + chr(9) + 
             /* 14*/ "Utpris til butikk" + chr(9) +
             /* 15*/ "Avvik" + chr(9) + 
             /* 16*/ "Anb. utpris" + chr(9) + 
             /* 17*/ "Avvik" + chr(9) + 
             /* 18*/ "Kjedens Innpris" + chr(9) +

             /* 19*/ "Varebok" + chr(9) + 
             /* 36*/ "Avvik" + chr(9) + 
             /* 37*/ "Netto forh. til butikk" + chr(9) +
             /* 21*/ "Avvik" + chr(9) + 
             /* 22*/ "Utpris til butikk" + chr(9) +
             /* 23*/ "Avvik" + chr(9) + 
             /* 24*/ "Anb. utpris" + chr(9) + 
             /* 25*/ "Avvik" + chr(9) + 
             /* 26*/ "Kjedens Innpris" + chr(9) +

             /* 27*/ "Varebok" + chr(9) + 
             /* 38*/ "Avvik" + chr(9) + 
             /* 39*/ "Netto forh. til butikk" + chr(9) +
             /* 28*/ "Avvik" + chr(9) + 
             /* 29*/ "Utpris til butikk" + chr(9) +
             /* 30*/ "Avvik" + chr(9) + 
             /* 31*/ "Anb. utpris" + chr(9) + 
             /* 32*/ "Avvik" + chr(9) + 
             /* 33*/ "Kjedens Innpris"
  .
  
def stream Inn.
def stream Ut.

input stream Inn from value(cFilNAvn) no-echo.
output stream Ut to value(cFilUt ) no-echo.

put stream Ut unformatted cHeading skip.

repeat:
  import stream Inn unformatted cLinje.
  assign    
    cLinje      = cLinje + fill(";",50)
    bDiff11 = false
    bDiff12 = false
    bDiff13 = false
    bDiff14 = false
    bDiff21 = false
    bDiff22 = false
    bDiff23 = false
    bDiff24 = false
    bDiff31 = false
    bDiff32 = false
    bDiff33 = false
    bDiff34 = false
    .

  assign
    lArtikkelNr = dec(entry(1,cLinje,";")) 
    cArtikkelNr = entry(1,cLinje,";")
    no-error.
  if error-status:error = false and length(entry(1,cLinje,";")) > 3 then
  do:
    assign
      lArtikkelNr = dec(substring(cArtikkelNr,1,length(cArtikkelNr) - 3)).
    if lArtikkelNr > 0 then find ArtBAs where ArtBAs.ArtikkelNr = lArtikkelNr no-error. 
    do:
      ENTRY ( 11 , cLinje , ";") = if available ArtBas then "*" else "".
      lVareBokNr = dec(entry(1,cVAreBokNrLSt)).
      find buf1VareBokLinje no-lock where
        buf1VareBokLinje.VarebokNr  = lVAreBokNr and
        buf1VareBokLinje.ArtikkelNr = lArtikkelNr no-error.
      lVareBokNr = dec(entry(2,cVAreBokNrLSt)).
      find buf2VareBokLinje no-lock where
        buf2VareBokLinje.VarebokNr  = lVAreBokNr and
        buf2VareBokLinje.ArtikkelNr = lArtikkelNr no-error.
      lVareBokNr = dec(entry(3,cVAreBokNrLSt)).
      find buf3VareBokLinje no-lock where
        buf3VareBokLinje.VarebokNr  = lVAreBokNr and
        buf3VareBokLinje.ArtikkelNr = lArtikkelNr no-error.
    end.
  end.

  if available buf1VareBokLinje then
  do:
    ENTRY ( 12 , cLinje , ";") = if available buf1VarebokLinje then string(buf1VarebokLinje.VareBokNr) else "".
    assign    
      bDiff11 = if dec(entry(8,cLinje,";")) <> buf1VarebokLinje.supVarekost then true else FALSE /* Netto forh - bytt med netto suppl. */
      bDiff12 = if dec(entry(9,cLinje,";")) <> buf1VarebokLinje.AnbefaltPris then true else false
      bDiff13 = if dec(entry(10,cLinje,";")) <> buf1VarebokLinje.KjedeInnkPris then true else false
      bDiff14 = if dec(entry(34,cLinje,";")) <> buf1VarebokLinje.Varekost then true else false
      .
      ENTRY ( 34 , cLinje , ";") = if bDiff14 then "*" else "".
      ENTRY ( 35 , cLinje , ";") = string(buf1VarebokLinje.Varekost).
      ENTRY ( 13 , cLinje , ";") = if bDiff11 then "*" else "".
      ENTRY ( 14 , cLinje , ";") = string(buf1VarebokLinje.supVarekost).
      ENTRY ( 15 , cLinje , ";") = if bDiff12 then "*" else "".
      ENTRY ( 16 , cLinje , ";") = string(buf1VarebokLinje.AnbefaltPris).
      ENTRY ( 17 , cLinje , ";") = if bDiff13 then "*" else "".
      ENTRY ( 18 , cLinje , ";") = string(buf1VarebokLinje.KjedeInnkPris).
  end.

  if available buf2VareBokLinje then
  do:
    ENTRY ( 19 , cLinje , ";") = if available buf2VarebokLinje then string(buf2VarebokLinje.VAreBokNr) else "".
    assign    
      bDiff21 = if dec(entry(8,cLinje,";")) <> buf2VarebokLinje.supVarekost then true else false
      bDiff22 = if dec(entry(9,cLinje,";")) <> buf2VarebokLinje.AnbefaltPris then true else false
      bDiff23 = if dec(entry(10,cLinje,";")) <> buf2VarebokLinje.KjedeInnkPris then true else false
      bDiff24 = if dec(entry(36,cLinje,";")) <> buf2VarebokLinje.Varekost then true else false
      .
      ENTRY ( 36 , cLinje , ";") = if bDiff24 then "*" else "".
      ENTRY ( 37 , cLinje , ";") = string(buf2VarebokLinje.Varekost).
      ENTRY ( 20 , cLinje , ";") = if bDiff21 then "*" else "".
      ENTRY ( 21 , cLinje , ";") = string(buf2VarebokLinje.supVarekost).
      ENTRY ( 22 , cLinje , ";") = if bDiff22 then "*" else "".
      ENTRY ( 23 , cLinje , ";") = string(buf2VarebokLinje.AnbefaltPris).
      ENTRY ( 24 , cLinje , ";") = if bDiff23 then "*" else "".
      ENTRY ( 25 , cLinje , ";") = string(buf2VarebokLinje.KjedeInnkPris).
  end.

  if available buf3VareBokLinje then
  do:
    ENTRY ( 26 , cLinje , ";") = if available buf3VarebokLinje then string(buf3VarebokLinje.VAreBokNr) else "".
    assign    
      bDiff31 = if dec(entry(8,cLinje,";")) <> buf3VarebokLinje.supVarekost then true else false
      bDiff32 = if dec(entry(9,cLinje,";")) <> buf3VarebokLinje.AnbefaltPris then true else false
      bDiff33 = if dec(entry(10,cLinje,";")) <> buf3VarebokLinje.KjedeInnkPris then true else false
      bDiff34 = if dec(entry(38,cLinje,";")) <> buf3VarebokLinje.Varekost then true else false
      .
      ENTRY ( 38 , cLinje , ";") = if bDiff34 then "*" else "".
      ENTRY ( 39 , cLinje , ";") = string(buf3VarebokLinje.Varekost).
      ENTRY ( 27 , cLinje , ";") = if bDiff31 then "*" else "".
      ENTRY ( 28 , cLinje , ";") = string(buf3VarebokLinje.supVarekost).
      ENTRY ( 29 , cLinje , ";") = if bDiff32 then "*" else "".
      ENTRY ( 30 , cLinje , ";") = string(buf3VarebokLinje.AnbefaltPris).
      ENTRY ( 31 , cLinje , ";") = if bDiff33 then "*" else "".
      ENTRY ( 32 , cLinje , ";") = string(buf3VarebokLinje.KjedeInnkPris).
  end.
  
  /*cLinje = replace(cLinje,";",chr(9)).*/
  put stream Ut unformatted 
      entry( 1,cLinje,";") + chr(9) +    /*  1 */
      entry( 2,cLinje,";") + chr(9) +    /*  2 */
      entry( 3,cLinje,";") + chr(9) +    /*  3 */
      entry( 4,cLinje,";") + chr(9) +    /*  4 */
      entry( 5,cLinje,";") + chr(9) +    /*  5 */
      entry( 6,cLinje,";") + chr(9) +    /*  6 */
      entry( 7,cLinje,";") + chr(9) +    /*  7 */
      entry( 8,cLinje,";") + chr(9) +    /*  8 */
      entry( 9,cLinje,";") + chr(9) +    /*  9 */
      entry(10,cLinje,";") + chr(9) +    /*  0 */
      entry(11,cLinje,";") + chr(9) +    /* 11 */
      /* Varebok 1 */
      entry(12,cLinje,";") + chr(9) +    /* 12 */
      entry(34,cLinje,";") + chr(9) +    /* 13 */
      entry(35,cLinje,";") + chr(9) +    /* 14 */
      entry(13,cLinje,";") + chr(9) +    /* 15 */
      entry(14,cLinje,";") + chr(9) +    /* 16 */
      entry(15,cLinje,";") + chr(9) +    /* 17 */
      entry(16,cLinje,";") + chr(9) +    /* 18 */
      entry(17,cLinje,";") + chr(9) +    /* 19 */
      entry(18,cLinje,";") + chr(9) +    /* 20 */
      /* Varebok 2 */
      entry(19,cLinje,";") + chr(9) +    /* 21 */
      entry(36,cLinje,";") + chr(9) +    /* 22 */
      entry(37,cLinje,";") + chr(9) +    /* 23 */
      entry(20,cLinje,";") + chr(9) +    /* 24 */
      entry(21,cLinje,";") + chr(9) +    /* 25 */
      entry(22,cLinje,";") + chr(9) +    /* 26 */
      entry(23,cLinje,";") + chr(9) +    /* 27 */
      entry(24,cLinje,";") + chr(9) +    /* 28 */
      entry(25,cLinje,";") + chr(9) +    /* 29 */
      /* Varebok 3 */
      entry(26,cLinje,";") + chr(9) +    /* 30 */
      entry(38,cLinje,";") + chr(9) +    /* 31 */
      entry(39,cLinje,";") + chr(9) +    /* 32 */
      entry(27,cLinje,";") + chr(9) +    /* 33 */
      entry(28,cLinje,";") + chr(9) +    /* 34 */
      entry(29,cLinje,";") + chr(9) +    /* 35 */
      entry(30,cLinje,";") + chr(9) +    /* 36 */    
      entry(31,cLinje,";") + chr(9) +    /* 37 */
      entry(32,cLinje,";") + chr(9) +    /* 38 */
      entry(33,cLinje,";") + chr(9)      /* 39 */
      skip.
  
  /*
  display
  entry(1,cLinje,";") format "x(15)" column-label "ArtNr" 
  ArtBAs.ArtikkelNr when available ArtBas
  ArtBas.LEvKod when available ArtBAs
  ArtBAs.Beskr when available ArtBas
  ArtBAs.LEvFargKod when available ArtBas
  dec(entry(8,cLinje,";")) column-label "Utpr.but"
  dec(entry(9,cLinje,";")) column-label "AnbUtpris"
  dec(entry(10,cLinje,";")) column-label "Farge"
  
  "*" when available ArtBAs
  buf1VarebokLinje.VareBokNr when available buf1VareBokLinje  
  buf2VarebokLinje.VareBokNr when available buf2VareBokLinje
  
  buf3VarebokLinje.VareBokNr when available buf3VareBokLinje
  "*" when bDiff31 = true
  buf3VarebokLinje.supVarekost when available buf3VareBokLinje
  "*" when bDiff32 = true
  buf3VarebokLinje.AnbefaltPris when available buf3VareBokLinje
  "*" when bDiff33 = true
  buf3VarebokLinje.KjedeInnkPris when available buf3VareBokLinje
  with width 300.
  */
end.

input stream Inn close.
