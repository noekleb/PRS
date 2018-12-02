def stream Ut.

def var x as int no-undo.

output stream ut to value("C:\Appdir\Filer_til_David_17122008\AntonStrekkoder18122008.txt").

    put stream ut unformatted
      "Strekkode.ArtikkelNr;"
      "Strekkode.Kode;"
      "Strekkode.Bestillingsnummer;"
      "ArtBas.LevKod;"
      "ArtBas.Beskr;"
      "ArtBas.LEvFargKod;"
      "Strekkode.StrKode;"
      "StrKonv.Storl;"
      "ArtBas.RegistrertDato"
      skip.


MAINLOOP:
for each Strekkode no-lock:
  find ArtBas of Strekkode no-lock no-error.
  if available ArtBas then
  do:
    find StrKonv no-lock where
      StrKonv.StrKode = Strekkode.StrKode no-error.

    x = x + 1.
        
    put stream ut unformatted
      Strekkode.ArtikkelNr ";"
      Strekkode.Kode ";"
      Strekkode.Bestillingsnummer ";"
      ArtBas.LevKod ";"
      ArtBas.Beskr ";"
      ArtBas.LEvFargKod ";"
      Strekkode.StrKode ";"
      (if available StrKonv then StrKonv.Storl else '****') ";"
      ArtBas.RegistrertDato
      skip.
    
    /*if x > 1000 then leave MAINLOOP.*/
  end.
  
end. /* MAINLOOP */

output stream Ut close.

display x.
