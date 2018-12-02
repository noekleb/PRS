CURRENT-WINDOW:WIDTH = 200.
CREATE Strekkode.
ASSIGN
    Strekkode.ArtikkelNr = 15522
    Strekkode.StrKode    = 54
    Strekkode.KodeType = 1
    Strekkode.VareId   = Strekkode.ArtikkelNr
    Strekkode.IKasse   = true
    .
UPDATE 
    Strekkode.Kode
    .

