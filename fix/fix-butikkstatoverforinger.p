CURRENT-WINDOW:WIDTH = 200.

DEF VAR wWork  AS DEC NO-UNDO.
DEF VAR wWork2 AS DEC NO-UNDO.
DEF VAR wWork3 AS DEC NO-UNDO.

DEF BUFFER bStLinje FOR StLinje.
DEF BUFFER bStLager FOR StLager. 

FOR EACH StLinje WHERE
    StLinje.StTypeId = "BUTSTAT" AND
    StLinje.DataObjekt > "" AND
    int(StLinje.DataObjekt) <> StLinje.butik:

    FIND bStLinje EXCLUSIVE-LOCK WHERE
            bStLinje.StTypeId   = StLinje.StTypeId AND
            bStLinje.PerId      = StLinje.PerId AND 
            bStLinje.DataObjekt = string(StLinje.Butik,"999999") AND 
            bStLinje.Diverse    = "" AND
            bStLinje.Butik      = StLinje.Butik AND
            bStLinje.Aar        = StLinje.Aar AND
            bStLinje.PerLinNr   = stLinje.PerLinNr
        NO-ERROR. 
    IF NOT AVAILABLE bStLinje THEN
    DO:
        CREATE bStLinje.
        ASSIGN
            bStLinje.StTypeId   = StLinje.StTypeId 
            bStLinje.PerId      = StLinje.PerId  
            bStLinje.DataObjekt = string(StLinje.Butik,"999999") 
            bStLinje.Diverse    = ""
            bStLinje.Butik      = StLinje.Butik
            bStLinje.Aar        = StLinje.Aar
            bStLinje.PerLinNr   = stLinje.PerLinNr
            .
    END.

    /* FLytter overføringene */
    ASSIGN
        bStLinje.OvAnt   = bStLinje.Ovant   + StLinje.OvAnt
        bStLinje.OvVerdi = bStLinje.OvVerdi + StLinje.OvVerdi
        .

/*
    DISPLAY
        bStLinje.StTypeId
        bStLinje.PerId
        bStLinje.DataObjekt
        bStLinje.Diverse
        bStLinje.Butik
        bStLinje.Aar
        bStLinje.PerLinNr
        bStLinje.ovAnt
        bStLinje.ovVerdi
        WITH WIDTH 198
        .
*/
    DELETE StLinje.
END.

FOR EACH StLager WHERE
    StLager.StTypeId = "BUTSTAT" AND
    StLager.DataObjekt > "" AND
    int(StLager.DataObjekt) <> StLager.butik:

    FIND bStLager EXCLUSIVE-LOCK WHERE
            bStLager.StTypeId   = StLager.StTypeId AND
            bStLager.DataObjekt = string(StLager.Butik,"999999") AND 
            bStLager.Butik      = StLager.Butik
        NO-ERROR. 
    IF NOT AVAILABLE bStLager THEN
    DO:
        CREATE bStLager.
        ASSIGN
            bStLager.StTypeId   = StLager.StTypeId 
            bStLager.DataObjekt = string(StLager.Butik,"999999") 
            bStLager.Butik      = StLager.Butik
            .
    END.

  /* Innleveranse medfører ny vekting av varekost i mottagende butikk */
  assign
    wWork  = ABSOLUTE(bStLager.Lagant * bStLager.VVareKost)   /* Gammel lagerverdi */
    wWork2 = ABSOLUTE(StLager.OvVerdi) /* Verdi av overføring  */
    wWork3 = (wWork + wWork2) / (ABSOLUTE(bStLager.LagAnt) + ABSOLUTE(StLager.OvAnt))
    wWork3 = if wWork3 = ? then bStLager.VVareKost else wWork3
    .

  /* FLytter overføringene */
  ASSIGN
      bStLager.Lagant    = bStLager.Lagant  + StLager.OvAnt
      bStLager.VVareKost = wWork3 /* Setter ny vektet snittpris */
      bStLager.OvAnt     = bStLager.Ovant   + StLager.OvAnt
      bStLager.OvVerdi   = bStLager.OvVerdi + StLager.OvVerdi
      .

  DELETE StLager.
  /*
  DISPLAY
      bStLager.StTypeId   
      bStLager.DataObjekt 
      bStLager.Butik      
      bStLager.Lagant    
      bStLager.VVareKost 
      bStLager.OvAnt     
      bStLager.OvVerdi   
      .
  */
END.
