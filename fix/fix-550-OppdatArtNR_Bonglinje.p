current-window:width = 300.

def var piLoop as int no-undo.
def var cFilNavn as char no-undo.
DEF VAR cKode    AS CHAR NO-UNDO.

BLOKKEN:  
for each BongHode no-lock where
  BongHode.ButikkNr = 550,
  EACH BongLinje EXCLUSIVE-LOCK where BongLinje.B_Id = BongHode.B_Id and 
  can-do('1,2,3,4,5,6,7,8,9,10,11',STRING(BongLinje.TTID)) and 
  BongLinje.Makulert = false:
  
  cKode = BongLinje.Strekkode.
  RUN bibl_chkean.p (INPUT-OUTPUT cKode).
  
  FIND Strekkode NO-LOCK WHERE
      Strekkode.Kode = cKode NO-ERROR.
  IF AVAILABLE STrekkode THEN
  DO:
      BongLinje.ArtikkelNR = STRING(Strekkode.ArtikkelNr).
  END.
  
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
