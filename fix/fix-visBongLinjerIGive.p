
CURRENT-WINDOW:WIDTH = 350.
CURRENT-WINDOW:HEIGHT = 40.

FOR EACH BongLinje NO-LOCK WHERE
  /*BongLinje.B_Id     = BongHode.B_Id AND*/
  BongLinje.Makulert = FALSE AND
  /* Solgt 
  BongLinje.TTId     = 134 AND 
  BongLinje.VareGr   = 9002 AND 
  */
  /* Brukt */
  BongLinje.TTId     = 52 AND 
  BongLinje.TBId     = 38 

  /*BongLinje.LopeNr   = 1*/
    :


  DISPLAY
      BongLinje.BongNr
      BongLinje.LinjeNr
      BongLinje.Makulert
      BongLinje.TTId    
      BongLinje.TBId
      BongLinje.VareGr  
      BongLinje.LopeNr  
      BongLinje.bongTekst
      BongLinje.Strekkode FORMAT "x(40)"
  WITH WIDTH 350.

END.

