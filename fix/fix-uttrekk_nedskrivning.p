/* fix-uttrekk_neskrivning.p */

current-window:width = 300.

def var cFilNAvn as char no-undo.

def stream Ut.

def buffer bufTransLogg for TransLogg.

assign
    cFilNavn = 'uttrekk_nedskrivning_But346.csv'.

output stream Ut to value(cFilNavn) no-echo.

put stream Ut unformatted
    'ArtikkelNr;'
    'Varetekst;'
    'Lev. art.nr;'
    'Varegruppe;'
    'Antall;'
    'Varekost kalkyle;' 
    'Utpris;'
    'Nedskrevet dato;'
    'Nedskrevet varekost;'
    'Dato gml. varekost;'
    'Gml. varekost;'
    'Nedskr.pr. enhet;'
    'Nedskr. sum;'
    skip.


for each TransLogg no-lock where
    TransLogg.butik = 346 and
    TransLogg.TTID = 8 and 
    TransLogg.Dato >= 02/01/2015:
    
    find ArtBas no-lock where
      ArtBas.ArtikkelNr = TransLogg.ArtikkelNr no-error.
    if not available TransLogg or TransLogg.ArtikkelNr = 0 then 
      next.
    find last ArtPris of ArtBas no-lock no-error.
    find VarGr of ArtBas no-lock no-error.
      
    find last bufTransLogg no-lock where
      bufTransLogg.ArtikkelNr = TransLogg.ArtikkelNr and
      bufTransLogg.Butik      = TransLogg.buti and
      /*bufTransLogg.Storl      = TransLogg.Storl and*/
      bufTransLogg.TTID       = 5 /* Varekjøp */ and
      bufTransLogg.Dato       < TransLogg.Dato no-error.
    if not available bufTransLogg then 
      find last bufTransLogg no-lock where
        bufTransLogg.ArtikkelNr = TransLogg.ArtikkelNr and
        bufTransLogg.Butik      = TransLogg.buti and
        bufTransLogg.TTID       = 1 /* Varesalg */ and
        bufTransLogg.Dato       < TransLogg.Dato no-error.
    if not available bufTransLogg then 
      find last bufTransLogg no-lock where
        bufTransLogg.ArtikkelNr = TransLogg.ArtikkelNr and
        bufTransLogg.Butik      = TransLogg.buti and
        bufTransLogg.TTID       = 9 /* Svinn */ and
        bufTransLogg.Dato       < TransLogg.Dato no-error.
       
      
    
    put stream Ut unformatted 
    TransLogg.ArtikkelNr ';'
    ArtBas.Beskr ';'
    ArtBas.LevKod ';'
    VarGr.VgBeskr ';'
    TransLogg.antall ';'
    ArtPris.VareKost[1] ';'
    ArtPris.Pris[1] ';'
    TransLogg.Dato ';'
    TransLogg.VVareKost ';'
    bufTransLogg.Dato ';'
    bufTransLogg.VVareKost ';'
    if available bufTransLogg then
      (bufTransLogg.VVareKost - TransLogg.VVareKost)
      else ArtPris.Varekost[1] ';'
    Translogg.antall * (bufTransLogg.VVareKost - TransLogg.VVareKost)
    skip.

    display
    TransLogg.ArtikkelNr
    ArtBas.Beskr format "x(40)"
    ArtBas.LevKod
    VarGr.VgBeskr column-label 'Varegruppe'
    /*TransLogg.TTId*/
    TransLogg.antall
    ArtPris.VareKost[1] column-label 'Varekost kalkyle' 
    ArtPris.Pris[1] column-label 'Utpris'
    TransLogg.Dato column-label 'Nedskrevet dato'
    TransLogg.VVareKost column-label 'Nedskrevet varekost'
    bufTransLogg.Dato when available bufTransLogg column-label 'Dato gml. varekost'
    bufTransLogg.VVareKost when available bufTransLogg column-label 'Gml. varekost'
    (bufTransLogg.VVareKost - TransLogg.VVareKost) when available bufTransLogg column-label 'Nedskr.pr. enhet'
    Translogg.antall * (bufTransLogg.VVareKost - TransLogg.VVareKost) when available bufTransLogg column-label 'Nedskr. sum'
    with width 300.
end.    

output stream Ut close.
