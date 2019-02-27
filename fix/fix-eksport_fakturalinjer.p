def var cKndNrLst as char no-undo.
def var iLoop as int no-undo.
def var lKndNr as dec format ">>>>>>>>>>>>>9" no-undo.

def var cFilNavn as char no-undo.
def stream Ut.

assign
  cFilNavn = 'c:\appdir\FakturaEksport' + REPLACE(STRING(TODAY),'/','') + '.csv'.

assign
  cKndNrLst = 
  '17302011,' + 
  '17303450,' + 
  '17303451,' + 
  '17303452,' + 
  '17303453,' + 
  '17400758' 
  .

current-window:width = 300.


output stream Ut to value(cFilNavn) no-echo.

put stream Ut unformatted 
      'FakturaHode.KundeNr;'
      'Kunde.Navn;'
      'FakturaHode.FakturaNr;'
      'FakturaHode.FakturertDato;'
      'FakturaLinje.ArtikkelNr;'
      'ArtBas.Beskr;'
      'ArtBas.LevKod;'
      'FakturaLinje.LevFargKod;'
      'FakturaLinje.Antall;'
      'FakturaLinje.Pris;'
      'FakturaLinje.MvaKr;'
      'FakturaLinje.LinjeSum - FakturaLinje.MvaKr;'
      'FakturaLinje.LinjeSum;'
      'ArtBas.Vg;'
      'VarGr.VgBeskr;'
      'VarGr.Hg;'
      'HuvGr.HgBeskr'
      skip.

do iLoop = 1 to num-entries(cKndNrLst):
  
  lKndNr = DEC(entry(iLoop,cKndNrLst)).
  
  find Kunde no-lock where
    kunde.KundeNr = lKndNr no-error.
  
  for each FakturaHode no-lock where
    FakturaHode.KundeNr = lKndNr and
    FakturaHode.FakturertDato >= 01/01/2014,
    each FakturaLinje of FakturaHode no-lock,
    first ArtBas of FakturaLinje no-lock:
    find VarGr of ArtBas no-lock no-error.
    find HuvGr of VarGr no-lock no-error.
    
    put stream Ut unformatted 
      FakturaHode.KundeNr ';'
      Kunde.Navn ';' 
      FakturaHode.FakturaNr ';'
      FakturaHode.FakturertDato ';'
      FakturaLinje.ArtikkelNr ';'
      ArtBas.Beskr ';'
      ArtBas.LevKod ';'
      FakturaLinje.LevFargKod ';'
      FakturaLinje.Antall ';'
      FakturaLinje.Pris ';'
      FakturaLinje.MvaKr ';'
      FakturaLinje.LinjeSum - FakturaLinje.MvaKr ';'
      FakturaLinje.LinjeSum ';'
      ArtBas.Vg ';'
      VarGr.VgBeskr ';'
      VarGr.Hg ';'
      HuvGr.HgBeskr
      skip.
    
  end.
  
end.

output stream Ut close.
