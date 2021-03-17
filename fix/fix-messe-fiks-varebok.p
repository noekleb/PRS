current-window:width = 300.

def var cFil as char no-undo.
def var cMerknad as char format "x(20)" no-undo.
def var cRecord  as char format "x(200)" no-undo.

def var cLevNamn as char format "x(20)" no-undo.
def var cBeskr   as char format "x(20)" no-undo.
def var cLevKod  as char format "x(20)" no-undo.
def var cLevFarg as char format "x(20)" no-undo.

def var cDiff1 as char format "x(20)"no-undo.
def var cDiff2 as char format "x(20)"no-undo.

def var dInnkjopsPris like VareBokLinje.Innkjopspris no-undo.
def var dVareKost     like VarebokLinje.Varekost     no-undo.
def var dForhKalkyle  like VarebokLinje.forhKalkyle  no-undo.
def var dDB%          like VarebokLinje.DB%          no-undo.
def var dsupVareKost  like VarebokLinje.supVarekost  no-undo.
def var dsupKalkyle   like VarebokLinje.supKalkyle   no-undo.
def var dPris         like VarebokLinje.Pris         no-undo.
def var dKampanjePris like VarebokLinje.KampanjePris no-undo.
def var iAnt as int.
def var iKorr as int.

DEF VAR hvbl AS HANDLE.


def stream inn.

assign
  cFil = "q:\appdir\anbefaltaug06-1.csv"
  .
hvbl = BUFFER VarebokLinje:HANDLE.

input stream Inn from value(cFil) no-echo. 

LESLINJE:
repeat:
  import stream inn unformatted cRecord.
  
  assign
    cLevNamn = entry(1,cRecord,";")
    cBeskr   = entry(2,cRecord,";")
    cLevKod  = entry(3,cRecord,";")
    cLevFarg = entry(4,cRecord,";")
    cMerknad = entry(17,cRecord,";")
    cDiff1   = trim(entry(17,cRecord,";"))
    .
    
  /* TEST */
  /*
  if cLevNamn = "Stians Sport AS" then.
  else next LESLINJE.
  */
    
  find first VarebokLinje exclusive-lock where
    VareBokLinje.LevNamn = cLevNamn and
    VareBokLinje.Beskr   = cBeskr and
    VareBokLinje.LevKod  = cLevKod and
    VareBokLinje.LevFargKod = cLevFarg no-error.
  if not available VareBokLinje then
    find first VarebokLinje exclusive-lock where
      VareBokLinje.LevNamn = cLevNamn and
      VareBokLinje.Beskr   = cBeskr and
      VareBokLinje.LevKod  = "0" + cLevKod and
      VareBokLinje.LevFargKod = cLevFarg no-error.
  if not available VareBokLinje then
    find first VarebokLinje exclusive-lock where
      VareBokLinje.LevNamn = cLevNamn and
      VareBokLinje.LevKod  = cLevKod and
      VareBokLinje.LevFargKod = cLevFarg no-error.
  if not available VareBokLinje then
  find first VarebokLinje exclusive-lock where
    VareBokLinje.Beskr   = cBeskr and
    VareBokLinje.LevKod  = cLevKod and
    VareBokLinje.LevFargKod = cLevFarg no-error.
  if available VareBokLinje then
  assign
    cDiff2   = trim(VareBokLinje.LinjeMerknad)
  .
  
  /*if not available VareBokLinje then*/
  /*
  do:
  iAnt = iAnt + 1.
  display
    iAnt
    cLevNamn 
    cBeskr   
    cLevKod  
    cLevFarg 
    cMerknad 
    VareBokLinje.LevNamn when available VareBokLinje
    VareBokLinje.Beskr when available VareBokLinje
    VareBokLinje.LevKod when available VareBokLinje
    VareBokLinje.LevFargKod when available VareBokLinje
    cDiff1 column-label "Merke"
    cDiff2 column-label "MerkBoka"
  with width 300.
  end.
  */
  
  if available VareBokLinje then
  KORR: 
  do:
    assign
      iKorr                     = iKorr + 1
      VarebokLinje.InnkjopsPris = dec(entry( 8,cRecord,";"))
      VarebokLinje.Varekost     = dec(entry( 9,cRecord,";"))
      VarebokLinje.forhKalkyle  = dec(entry(10,cRecord,";"))
      VarebokLinje.DB%          = dec(entry(11,cRecord,";"))
      VarebokLinje.supVarekost  = dec(entry(12,cRecord,";"))
      VarebokLinje.supKalkyle   = dec(entry(13,cRecord,";"))
      VarebokLinje.Pris         = dec(entry(14,cRecord,";"))
      VarebokLinje.KampanjePris = dec(entry(15,cRecord,";"))
      VarebokLinje.LinjeMerknad =    (entry(17,cRecord,";"))
      .
      
    run vareboklinje_kalkuler.p (hvbl,"Varekost").
    run vareboklinje_kalkuler.p (hvbl,"supVarekost").
  end.
  
end.

/* ---------------------------------------------------------------------------
for each VareBokLinje no-lock where
  VareBokLinje.VareBokNr = 90000065 and
  VarebokLinje.LevNr = 1
  by Vareboklinje.Beskr:
  
  display
    VarebokLinje.VareBokNr
    VareBokLinje.levnr 
    VareBokLinje.LevKod 
    VareBokLinje.Beskr 
    VareBokLinje.LevFargKod 
    /*
    VarebokLinje.InnkjopsPris
    VarebokLinje.Varekost
    VarebokLinje.forhKalkyle format "->>9.99"
    VarebokLinje.DB%
    VarebokLinje.supVarekost
    VarebokLinje.supKalkyle format "->>9.99"
    VarebokLinje.Pris
    VarebokLinje.KampanjePris
    */
    VarebokLinje.LinjeMerknad
    
    /*
    PUT UNFORMATTED
      (IF VarebokLinje.Beskr NE ? THEN VarebokLinje.Beskr ELSE "") + "~t" +
      (IF VarebokLinje.LevKod NE ? THEN VarebokLinje.LevKod ELSE "") + "~t" +
      (IF VarebokLinje.LevFargKod NE ? THEN VarebokLinje.LevFargKod ELSE "") + "~t" +
      (IF Varemerke.Beskrivelse NE ? THEN Varemerke.Beskrivelse ELSE "") + "~t" +
      cStrFraTil + "~t" +
  /*     STRING(VarebokLinje.Vg) + "~t" + */
      SUBSTR(VarebokLinje.VgBeskr,1,15) + "~t" +
      (IF VarebokLinje.InnkjopsPris NE ? THEN STRING(VarebokLinje.InnkjopsPris) ELSE "") + "~t" +
      (IF VarebokLinje.Varekost NE ? THEN STRING(VarebokLinje.Varekost) ELSE "") + "~t" +
      (IF VarebokLinje.forhKalkyle NE ? THEN STRING(VarebokLinje.forhKalkyle) ELSE "") + "~t" +
      (IF VarebokLinje.DB% NE ? THEN STRING(VarebokLinje.DB%) ELSE "") + "~t" +
      (IF VarebokLinje.supVarekost NE ? THEN STRING(VarebokLinje.supVarekost) ELSE "") + "~t" +
      (IF VarebokLinje.supKalkyle NE ? THEN STRING(VarebokLinje.supKalkyle) ELSE "") + "~t" +
  /*     (IF VarebokLinje.supDB% NE ? THEN STRING(VarebokLinje.supDB%) ELSE "") + "~t" +  */
      (IF VarebokLinje.Pris NE ? THEN STRING(VarebokLinje.Pris) ELSE "") + "~t" +
      (IF VarebokLinje.KampanjePris NE ? AND VarebokLinje.KampanjePris > 0 THEN STRING(VarebokLinje.KampanjePris) ELSE "") + "~t" +
      (IF ArtBas.LevDato1 NE ? THEN getWeekNum(ArtBas.LevDato1) ELSE "") + "~t" +
      (IF VarebokLinje.LinjeMerknad NE ? THEN VarebokLinje.LinjeMerknad ELSE "")
  /*     (IF VarebokLinje.LinjeMerknad NE ? THEN REPLACE(REPLACE(REPLACE(VarebokLinje.LinjeMerknad,CHR(10)," "),CHR(9)," "),CHR(13)," ") ELSE "") */
    */ 
    
    /* 
    VareBokLinje.AnbefaltPris 
    VareBokLinje.Antall 
    VareBokLinje.ArtikkelNr 
    VareBokLinje.AvdelingNavn 
    VareBokLinje.AvdelingNr 
    VareBokLinje.BrukerID 
    VareBokLinje.DB% 
    VareBokLinje.DBKr 
    VareBokLinje.EDato 
    VareBokLinje.ETid 
    VareBokLinje.forhKalkyle 
    VareBokLinje.forhRab% 
    VareBokLinje.Hg 
    VareBokLinje.HgBeskr 
    VareBokLinje.InnkjopsPris 
    VareBokLinje.KampanjePris 
    VareBokLinje.KatalogPris 
    VareBokLinje.KjedeInnkPris 
    VareBokLinje.KjedeRab% 
    VareBokLinje.LevDato1 
    VareBokLinje.LevDato2 
    VareBokLinje.LevDato3 
    VareBokLinje.LevDato4 
    VareBokLinje.ModellFarge 
    VareBokLinje.Mva% 
    VareBokLinje.Pris 
    VareBokLinje.ProdNr 
    VareBokLinje.ProdusentBeskrivelse 
    VareBokLinje.Rab1Kr 
    VareBokLinje.RegistrertAv 
    VareBokLinje.RegistrertDato 
    VareBokLinje.RegistrertTid 
    VareBokLinje.Sekv 
    
    VareBokLinje.supAntall 
    VareBokLinje.supDB% 
    VareBokLinje.supDBKr 
    VareBokLinje.supInnkjopsPris 
    VareBokLinje.supKalkyle 
    VareBokLinje.supPris 
    VareBokLinje.supRab% 
    VareBokLinje.supRab1Kr 
    VareBokLinje.supVareKost 
    VareBokLinje.VareBokNr 
    
    
    VareBokLinje.Vg 
    VareBokLinje.VgBeskr 
    VareBokLinje.VPIDato
    */
    with width 300.
    
end.
*/
