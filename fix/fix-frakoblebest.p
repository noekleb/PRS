/* PRogram for frakobling av bestillinger. */

def var wBestDato1 as date no-undo.
def var wBestDato2 as date no-undo.
def var wSesListe  as char no-undo.

/* Lister opp den/de sesonger som er aktuelle */
assign
  wSesListe = "100,200".

ORDRELOOP:
for each BestHode exclusive-lock where
  BestHode.BestillingsDato >= wBestDato1 and
  BestHode.BestillingsDato <= wBestDato2 and
  BestHode.OrdreNr > 0:
  
  /* Det kan forekomme at det står ? i feltet også */
  if BestHode.OrdreNr = ? then
    do:
      BestHode.OrdreNr = 0.
      next ORDRELOOP.
    end.
  
  /* Henter artikkel for ytterligere kriterier. */
  find ArtBas of BestHode no-lock no-error.
  if not available ArtBas then
    next ORDRELOOP. /* Huuup! */
  
  /* Avgrenser på sesong */
  if not can-do(wSesListe,string(ArtBas.Sasong)) then
    next ORDRELOOP.
  
  /* Nullstiller kobling til ordre. */
  assign
    BestHode.OrdreNr = 0.
  
end. /* ORDRELOOP */
