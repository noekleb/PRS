def var iAnt as dec format '->>,>>>,>>>,>>9.999' no-undo.
def var iVerdi as dec format '->>,>>>,>>>,>>9.999' no-undo.

def stream ut.

output stream ut to value('Telling03062009.csv').

put stream Ut
'Artikkelnr;LevKod;Varetekst;LevFargKod;Str;Vg;LopNr;Vg;LopNr;Antall;Verdi;D;Manger;Lager;Opris;Pakke' skip.


current-window:width = 300.
for each ArtLag no-lock where 
  ArtLag.LagAnt <> 0:
  
  find ArtBas no-lock where
    ArtBas.ArtikkelNr = ArtLag.ArtikkelNr no-error.
  find Lager no-lock where
    Lager.ArtikkelNr = ArtLag.ArtikkelNr and 
    Lager.Butik = ArtLag.Butik no-error.

  find TelleLinje no-lock where
    TelleLinje.TelleNr = 113 and
    TelleLinje.ArtikkelNr = ArtLag.ArtikkelNr and
    TelleLinje.Storl = ArtLAg.Storl and
    TelleLinje.butik = ArtLag.Butik no-error.
    
  if not available TelleLinje then 
  OPPRETT:
  do:  
    assign
      IAnt = iAnt + ArtLag.LagAnt
      iVerdi = iVerdi + (Lager.VVAreKost * ArtLag.LagAnt)
      .
  
    put stream Ut unformatted
    ArtLag.ArtikkelNr ';'
    ArtBas.LevKod ';'
    ArtBas.Beskr ';'
    ArtBas.LevFargKod ';'
    ArtLag.Storl ';'
    ArtBas.Vg ';'
    ArtBas.LopNr ';'
    ArtLag.Vg ';'
    ArtLag.LopNr ';'
    ArtLag.LagAnt ';'
    (Lager.VVAreKost * ArtLag.LagAnt) ';'
    (if (string(ArtLag.Vg) + string(ArtLag.LopNr) <> string(ArtBas.Vg) + string(ArtBas.LopNr)) then '*' else '') ';'
    (if not available TelleLinje then 'Mangler' else '') ';'
    ArtBas.Lager ';'
    ArtBas.OPris ';'
    ArtBas.Pakke skip.
    
    /*----*/
    
        IF NOT AVAILABLE TelleLinje THEN 
        DO:
            CREATE TelleLinje.

            /* Setter index. */
            ASSIGN
              TelleLinje.TelleNr    = 113
              TelleLinje.ArtikkelNr = ArtBas.ArtikkelNr
              Tellelinje.Beskr      = ArtBas.Beskr
              TelleLinje.Vg         = ArtBas.Vg
              TelleLinje.LopNr      = ArtBas.LopNr
              TelleLinje.Butik      = ArtLag.butik
              TelleLinje.Storl      = ArtLag.Storl
              TelleLinje.AntallPar  = ArtLag.LagAnt
              TelleLinje.OpprAntalTalt = (if TelleLinje.OpprAntalTalt = 0 then ArtLag.LagAnt else TelleLinje.OpprAntalTalt)
              TelleLinje.OpprVerdi  = ArtLag.LagAnt * Lager.VVareKost
              TelleLinje.LevFargKod = ArtBas.LevFargKod
              TelleLinje.Vg         = ArtBas.Vg 
              TelleLinje.LopNr      = ArtBas.LopNr
              TelleLinje.LevKod     = ArtBas.LevKod
              TelleLinje.VVareKost  = Lager.VVAreKost
              TelleLinje.AntallDiff = TelleLinje.AntallPar - TelleLinje.AntallTalt
              TelleLinje.VerdiDiff  = TelleLinje.AntallDiff * TelleLinje.VVareKost
              TelleLinje.LevNr      = ArtBas.LevNr
              TelleLinje.Sasong     = ArtBas.SaSong
              TelleLinje.Farg       = ArtBas.Farg
              TelleLinje.MatKod     = ArtBas.MatKod
              TelleLinje.VgLopNr    = TRIM(STRING(ArtBas.Vg,">>>>>9")) + "/" + TRIM(STRING(ArtBas.LopNr,">>>>>9"))
              TelleLinje.Kode       = ''
              .
        END.
        RELEASE TelleLinje. /* Denne MÅ få stå */
    /*----*/
    
  end. /* OPPRETT */
    

end.

output stream Ut close.

message iAnt round(iVerdi,2) view-as alert-box.
