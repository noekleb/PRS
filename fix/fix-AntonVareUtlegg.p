def var iAntVarer as int no-undo.
def var cUtFil as char no-undo.

def stream Ut.

assign
  cUtFil = 'Anton_Artikkler.csv'
  .

output stream ut to Value(cUtFil).

put stream Ut unformatted
     'Anton.ArtikkelNr;'
     'Anton.WebButikkArtikkel;'
     'Anton.Kode;'
     'Anton.LevKod;'
     'Anton.Beskr;'
     'Anton.LevFargKod;'
     'Anton.StrKode;'
     'Anton.Storl;'
     ';Skille'
     skip.

current-window:width = 300.
for each Strekkode no-lock where
  Strekkode.StrKode > 0:
   find ArtBas of Strekkode no-lock no-error.
   
   /* Ikke åpen pris artikler. */
   if ArtBas.OPris then
     next.
     
   find StrKonv no-lock where
       StrKonv.StrKode = Strekkode.StrKode no-error.
   
   /* Teller opp antall varer med lagerbevegelse. */
   iAntVarer = iAntVarer + 1.
   
   if iAntVarer modulo 1000 = 0 then
     do:
       pause 0.
       display iAntVarer.
     end.
   
   put stream Ut Unformatted
     Strekkode.ArtikkelNr ';'
     ArtBas.WebButikkArtikkel ';'
     Strekkode.Kode ';'
     ArtBas.LevKod ';'
     ArtBas.Beskr ';'
     ArtBas.LevFargKod ';'
     Strekkode.StrKode ';'
     (if available StrKonv 
       then StrKonv.Storl
       else '*Ukjent*') ';****'
     skip.
end.   

output stream Ut close.

message iAntVarer
  view-as alert-box.
