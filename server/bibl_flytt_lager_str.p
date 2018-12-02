/* bibl_flytt_lager_str.p 

        RUN bibl_flytt_lager_str.p (Strekkode.Kode, VPISTrekkode.StrKode).
        
        Flytter lager fra en størrelse til en annen størrelse på samme artikkel. 

*/

DEFINE INPUT PARAMETER cKode       LIKE StrekKode.Kode    NO-UNDO.
DEFINE INPUT PARAMETER iStrKodeTil LIKE StrekKode.StrKode NO-UNDO.

FLYTT_LAGER:
DO:
    FIND StrekKode NO-LOCK WHERE
       StrekKode.Kode = cKode NO-ERROR.
    IF NOT AVAILABLE StrekKode THEN 
      LEAVE FLYTT_LAGER.
    IF Strekkode.StrKode = iStrKodeTil THEN 
      LEAVE FLYTT_LAGER.  
      
    /* Flytter lager */
    IF AVAILABLE ArtLag THEN RELEASE ArtLag.        
    /* Positivt lager ? */
    FIND FIRST ArtLag NO-LOCK WHERE
      ArtLag.ArtikkelNr = StrekKode.ArtikkelNr AND
      ArtLag.butik      > 0 AND 
      ArtLag.StrKode    = StrekKode.StrKode AND 
      ArtLag.lagant     > 0 NO-ERROR.
    
    /* Neg lager ? */
    IF NOT AVAILABLE ArtLag THEN 
    FIND FIRST ArtLag NO-LOCK WHERE
      ArtLag.ArtikkelNr = StrekKode.ArtikkelNr AND
      ArtLag.butik      > 0 AND 
      ArtLag.StrKode    = StrekKode.StrKode AND 
      ArtLag.lagant     < 0 NO-ERROR.
    
    IF AVAILABLE ArtLag THEN
    DO:
      FOR EACH ArtLag NO-LOCK WHERE
        ArtLag.ArtikkelNr = StrekKode.ArtikkelNr AND
        ArtLag.butik      > 0 AND 
        ArtLag.StrKode    = Strekkode.StrKode:
        FIND Lager NO-LOCK WHERE
          Lager.ArtikkelNr = ArtLag.ArtikkelNr AND
          Lager.Butik      = ArtLag.butik NO-ERROR.
        IF NOT AVAILABLE Lager THEN 
          NEXT.
        FIND StrKonv NO-LOCK WHERE
          StrKonv.StrKode = iStrKodeTil NO-ERROR.
        IF NOT AVAILABLE StrKonv THEN NEXT.
        /* Skaper transer som nuller gammelt lager */
        RUN oppdatlagerjust.p (ArtLag.Butik, ArtLag.ArtikkelNr, 7, ArtLag.Storl, STRING(ArtLag.LagAnt), STRING(Lager.VVareKost)).            
        /* Skaper transer som oppdaterer nytt lager. */
        RUN oppdatlagerjust.p (ArtLag.Butik, ArtLag.ArtikkelNr, 7, StrKonv.Storl, STRING(ArtLag.lagant * -1), STRING(Lager.VVareKost)).
      END. 
    END. 
    
    /* Bytter størrelsen på strekkoden. */
    DO TRANSACTION:
      FIND CURRENT Strekkode EXCLUSIVE-LOCK.
      ASSIGN Strekkode.StrKode = iStrKodeTil.
      FIND CURRENT Strekkode NO-LOCK.
    END.
END. /* FLYTT_LAGER*/