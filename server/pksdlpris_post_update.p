DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icAction    AS CHAR   NO-UNDO.  /* Create or Update */
DEF INPUT  PARAM icSessionId AS CHAR   NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR   NO-UNDO.
DEFINE VARIABLE lMva% AS DECIMAL NO-UNDO.

DEF VAR fPrisExMva      AS DEC NO-UNDO.

ASSIGN 
    lMva% = 25.

FIND FIRST PkSdlLinje NO-LOCK
     WHERE PkSdlLinje.PkSdlId    = DEC(ihBuffer:BUFFER-FIELD("PkSdlId"):BUFFER-VALUE)
       AND PkSdlLinje.ArtikkelNr = DEC(ihBuffer:BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE)
     NO-ERROR.

IF AVAIL PkSdlLinje AND NOT CAN-FIND(FIRST PkSdlMottak
                                     WHERE PkSdlMottak.PkSdlId   = PkSdlLinje.PkSdlId
                                       AND PkSdlMottak.MottaksId = PkSdlLinje.MottaksId
                                       AND PkSdlMottak.MottaksId > 0) THEN 
DO:
  FIND Butiker NO-LOCK WHERE 
      Butiker.Butik = PkSdlLinje.ButikkNr NO-ERROR.
      
  FIND FIRST ArtPris NO-LOCK WHERE 
      ArtPris.ArtikkelNr = PkSdlLinje.ArtikkelNr AND 
      ArtPris.ProfilNr   = Butiker.ProfilNr NO-ERROR.
  IF NOT AVAILABLE ArtPris THEN 
      FIND FIRST ArtPris NO-LOCK WHERE 
      ArtPris.ArtikkelNr = PkSdlLinje.ArtikkelNr NO-ERROR.
  IF AVAILABLE ArtPris THEN 
      lMva% = ArtPris.Mva%[1].
  ELSE 
      lMva% = 25.
  DO: 
    ASSIGN ihBuffer:BUFFER-FIELD("NyVarekost"):BUFFER-VALUE = 
                ihBuffer:BUFFER-FIELD("NyInnkjopsPris"):BUFFER-VALUE 
              - ihBuffer:BUFFER-FIELD("NyInnkjopsPris"):BUFFER-VALUE * ihBuffer:BUFFER-FIELD("NyRab1%"):BUFFER-VALUE / 100
              + ihBuffer:BUFFER-FIELD("NyFrakt"):BUFFER-VALUE
           fPrisExMva = ihBuffer:BUFFER-FIELD("NyPris"):BUFFER-VALUE / (1 + lMva% / 100)
           ihBuffer:BUFFER-FIELD("NyDb%"):BUFFER-VALUE = 
                (fPrisExMva - ihBuffer:BUFFER-FIELD("NyVarekost"):BUFFER-VALUE) / fPrisExMva * 100
           .
    ihBuffer:BUFFER-FIELD("OverstyrPris"):BUFFER-VALUE =
       ihBuffer:BUFFER-FIELD("Pris"):BUFFER-VALUE NE ihBuffer:BUFFER-FIELD("NyPris"):BUFFER-VALUE OR
       ihBuffer:BUFFER-FIELD("Varekost"):BUFFER-VALUE NE ihBuffer:BUFFER-FIELD("NyVarekost"):BUFFER-VALUE.
  END.
END.

