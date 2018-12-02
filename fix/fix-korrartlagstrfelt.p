CURRENT-WINDOW:WIDTH = 200.
DEF BUFFER bufArtLAg FOR Artlag.
DEF VAR cStorl AS CHAR NO-UNDO.

FORM WITH FRAME G DOWN.

FOR EACH ArtBAs:
  FOR EACH ArtLag EXCLUSIVE-LOCK where
      ArtLAg.Vg    = ArtBAs.Vg and
      ArtLAg.LopNR = ArtBAs.LopNR  AND
      trim(ArtLAg.Storl) = "":

    cStorl = "".

    FIND StrType OF ArtBAs NO-LOCK NO-ERROR.
    IF AVAILABLE StrType THEN
        FIND FIRST StrTStr OF StrType NO-ERROR.
    IF AVAILABLE StrTStr THEN
        cStorl = StrTStr.SoStorl.
    ELSE
        cStorl = "".

    FIND bufArtLAg WHERE
         bufArtLag.Vg    = ArtLag.Vg AND
         bufArtlag.LopNr = ArtLag.LopNr AND
         bufArtLAg.Storl = cStorl AND
         bufArtLag.Butik = ArtLag.Butik NO-ERROR.
    IF NOT AVAILABLE bufArtLAg THEN
    DO:
        PAUSE 0.
        DISPLAY 
            "LLOPEN:"
            ArtLAg.Butik
            Artlag.ArtikkelNR
            ArtLAg.Vg
            Artlag.LopNr
            ArtLAg.Storl
            Artlag.StrKode
            cStorl
            WITH WIDTH 198 DOWN FRAME G.
        PAUSE 0.
        DOWN 1 WITH FRAME G.
        ASSIGN
            ArtLAg.Storl = cStorl
            .
    END.
    ELSE DO:
        PAUSE 0.
        DISPLAY 
            "LAGER:"
            ArtLAg.Butik
            Artlag.ArtikkelNR
            ArtLAg.Vg
            Artlag.LopNr
            ArtLAg.Storl
            Artlag.StrKode
            cStorl
            WITH WIDTH 198 DOWN FRAME G.
        PAUSE 0.
        DOWN 1 WITH FRAME G.
        
        ASSIGN
            bufArtLag.lagant      = bufArtLag.lagant        + ArtLag.lagant     
            bufArtLag.retant      = bufArtLag.retant        + ArtLag.retant     
            bufArtLag.AntSolgt    = bufArtLag.AntSolgt      + ArtLag.AntSolgt   
            bufArtLag.BrekkAnt    = bufArtLag.BrekkAnt      + ArtLag.BrekkAnt   
            bufArtLag.IntAnt      = bufArtLag.IntAnt        + ArtLag.IntAnt     
            bufArtLag.ReklAnt     = bufArtLag.ReklAnt       + ArtLag.ReklAnt    
            bufArtLag.ReklLAnt    = bufArtLag.ReklLAnt      + ArtLag.ReklLAnt   
            bufArtLag.GjenkjopAnt = bufArtLag.GjenkjopAnt   + ArtLag.GjenkjopAnt
            bufArtLag.RetLAnt     = bufArtLag.RetLAnt       + ArtLag.RetLAnt    
            bufArtLag.KjopAnt     = bufArtLag.KjopAnt       + ArtLag.KjopAnt    
            bufArtLag.OvAnt       = bufArtLag.OvAnt         + ArtLag.OvAnt      
            bufArtLag.JustAnt     = bufArtLag.JustAnt       + ArtLag.JustAnt    
            bufArtLag.JustVerdi   = bufArtLag.JustVerdi     + ArtLag.JustVerdi  
            bufArtLag.SvinnAnt    = bufArtLag.SvinnAnt      + ArtLag.SvinnAnt  
            bufArtLag.SvinnVerdi  = bufArtLag.SvinnVerdi    + ArtLag.SvinnVerdi 
            bufArtLag.NedAnt      = bufArtLag.NedAnt        + ArtLag.NedAnt     
            bufArtLag.NedVerdi    = bufArtLag.NedVerdi      + ArtLag.NedVerdi   
            bufArtLag.AntRab      = bufArtLag.AntRab        + ArtLag.AntRab     
            .
        DELETE ArtLAg.
    END.
  END.
END.

