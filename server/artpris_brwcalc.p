DEF VAR iCl         AS INT NO-UNDO.
DEF VAR fGjPris     AS DEC NO-UNDO.
DEF VAR bPrisAvvik  AS LOG NO-UNDO.
DEF VAR bVkAvvik    AS LOG NO-UNDO.

{syspara.i 5 1 1 iCl INT}.
FIND Butiker NO-LOCK WHERE
    Butiker.Butik = iCl NO-ERROR.

PROCEDURE artpris_Beskr:
  DEF INPUT  PARAM ifArtikkelNr AS DEC  NO-UNDO.
  DEF INPUT  PARAM icSessionId  AS CHAR NO-UNDO.
  DEF OUTPUT PARAM ocValue      AS CHAR NO-UNDO.
  
  FIND ArtBas NO-LOCK WHERE 
    ArtPris.ArtikkelNr = ifArtikkelNr NO-ERROR.
  IF AVAILABLE ArtBas THEN 
    ocValue = ArtBas.Beskr.
  ELSE 
    ocValue = "".  
END.

PROCEDURE artpris_LevKod:
  DEF INPUT  PARAM ifArtikkelNr AS DEC  NO-UNDO.
  DEF INPUT  PARAM icSessionId  AS CHAR NO-UNDO.
  DEF OUTPUT PARAM ocValue      AS CHAR NO-UNDO.
  
  FIND ArtBas NO-LOCK WHERE 
    ArtPris.ArtikkelNr = ifArtikkelNr NO-ERROR.
  IF AVAILABLE ArtBas THEN 
    ocValue = ArtBas.LevKod.
  ELSE 
    ocValue = "".  
END.

PROCEDURE artpris_LevFargKod:
  DEF INPUT  PARAM ifArtikkelNr AS DEC  NO-UNDO.
  DEF INPUT  PARAM icSessionId  AS CHAR NO-UNDO.
  DEF OUTPUT PARAM ocValue      AS CHAR NO-UNDO.
  
  FIND ArtBas NO-LOCK WHERE 
    ArtPris.ArtikkelNr = ifArtikkelNr NO-ERROR.
  IF AVAILABLE ArtBas THEN 
    ocValue = ArtBas.LevFargKod.
  ELSE 
    ocValue = "".  
END.

PROCEDURE artpris_innkjopspris:
  DEF INPUT  PARAM ifArtikkelNr AS DEC  NO-UNDO.
  DEF INPUT  PARAM icSessionId  AS CHAR NO-UNDO.
  DEF OUTPUT PARAM ocValue      AS CHAR NO-UNDO.
  
  
  IF AVAILABLE Butiker THEN DO:
    FIND FIRST ArtPris NO-LOCK
         WHERE ArtPris.ArtikkelNr = ifArtikkelNr  
           AND ArtPris.ProfilNr   = Butiker.ProfilNr 
         NO-ERROR.
    IF AVAILABLE ArtPris THEN
      ocValue = STRING(ArtPris.InnkjopsPris[IF Artpris.Tilbud THEN 2 ELSE 1]).
    ELSE
      ocValue = "".
  END.
  ELSE 
    ocValue = "".
END.


PROCEDURE artpris_rab1%:
  DEF INPUT  PARAM ifArtikkelNr AS DEC  NO-UNDO.
  DEF INPUT  PARAM icSessionId  AS CHAR NO-UNDO.
  DEF OUTPUT PARAM ocValue      AS CHAR NO-UNDO.
  
  
  IF AVAILABLE Butiker THEN DO:
    FIND FIRST ArtPris NO-LOCK
         WHERE ArtPris.ArtikkelNr = ifArtikkelNr  
           AND ArtPris.ProfilNr   = Butiker.ProfilNr 
         NO-ERROR.
    IF AVAILABLE ArtPris THEN
      ocValue = STRING(ArtPris.Rab1%[IF Artpris.Tilbud THEN 2 ELSE 1]).
    ELSE
      ocValue = "".
  END.
  ELSE 
    ocValue = "".
END.

PROCEDURE artpris_frakt:
  DEF INPUT  PARAM ifArtikkelNr AS DEC  NO-UNDO.
  DEF INPUT  PARAM icSessionId  AS CHAR NO-UNDO.
  DEF OUTPUT PARAM ocValue      AS CHAR NO-UNDO.
  
  
  IF AVAILABLE Butiker THEN DO:
    FIND FIRST ArtPris NO-LOCK
         WHERE ArtPris.ArtikkelNr = ifArtikkelNr  
           AND ArtPris.ProfilNr   = Butiker.ProfilNr 
         NO-ERROR.
    IF AVAILABLE ArtPris THEN
      ocValue = STRING(ArtPris.Frakt[IF Artpris.Tilbud THEN 2 ELSE 1]).
    ELSE
      ocValue = "".
  END.
  ELSE 
    ocValue = "".
END.

PROCEDURE artpris_varekost:
  DEF INPUT  PARAM ifArtikkelNr AS DEC  NO-UNDO.
  DEF INPUT  PARAM icSessionId  AS CHAR NO-UNDO.
  DEF OUTPUT PARAM ocValue      AS CHAR NO-UNDO.
  
  
  IF AVAILABLE Butiker THEN DO:
    FIND FIRST ArtPris NO-LOCK
         WHERE ArtPris.ArtikkelNr = ifArtikkelNr  
           AND ArtPris.ProfilNr   = Butiker.ProfilNr 
         NO-ERROR.
    IF AVAILABLE ArtPris THEN DO:
      IF ArtPris.Tilbud AND ArtPris.VareKost[2] NE 0 THEN
        ocValue = STRING(ArtPris.Varekost[2]).
      ELSE
        ocValue = STRING(ArtPris.Varekost[1]).
    END.
  END.
END.

PROCEDURE artpris_pris:
  DEF INPUT  PARAM ifArtikkelNr AS DEC  NO-UNDO.
  DEF INPUT  PARAM icSessionId  AS CHAR NO-UNDO.
  DEF OUTPUT PARAM ocValue      AS CHAR NO-UNDO.
  
  fGjPris = 0.
  
  IF AVAILABLE Butiker THEN DO:
    FIND FIRST ArtPris NO-LOCK
         WHERE ArtPris.ArtikkelNr = ifArtikkelNr  
           AND ArtPris.ProfilNr   = Butiker.ProfilNr 
         NO-ERROR.
    IF AVAILABLE ArtPris THEN
      ASSIGN ocValue = STRING(ArtPris.Pris[IF Artpris.Tilbud THEN 2 ELSE 1])
             fGjPris = ArtPris.Pris[IF Artpris.Tilbud THEN 2 ELSE 1]
             .
    ELSE
      ocValue = "".
  END.
  ELSE 
    ocValue = "".
END.

PROCEDURE artpris_db%:
  DEF INPUT  PARAM ifArtikkelNr AS DEC  NO-UNDO.
  DEF INPUT  PARAM icSessionId  AS CHAR NO-UNDO.
  DEF OUTPUT PARAM ocValue      AS CHAR NO-UNDO.
  
  
  IF AVAILABLE Butiker THEN DO:
    FIND FIRST ArtPris NO-LOCK
         WHERE ArtPris.ArtikkelNr = ifArtikkelNr  
           AND ArtPris.ProfilNr   = Butiker.ProfilNr 
         NO-ERROR.
    IF AVAILABLE ArtPris THEN
      ocValue = STRING(ArtPris.DB%[IF Artpris.Tilbud THEN 2 ELSE 1]).
    ELSE
      ocValue = "".
  END.
  ELSE 
    ocValue = "".
END.

PROCEDURE pksdlpris_prisavvik:
  DEF INPUT  PARAM irPkSdlPris  AS ROWID NO-UNDO.
  DEF INPUT  PARAM icParam      AS CHAR  NO-UNDO.
  DEF INPUT  PARAM icSessionId  AS CHAR  NO-UNDO.
  DEF OUTPUT PARAM ocValue      AS CHAR  NO-UNDO.
  


  FIND PkSdlPris NO-LOCK
       WHERE ROWID(PkSdlPris) = irPkSdlPris
       NO-ERROR.

  IF AVAILABLE PkSdlPris THEN DO:   
    IF icParam NE "" AND LOGICAL(icParam) AND PkSdlPris.NyPris = fGjPris THEN
      ocValue = "skiprow".
    ELSE
      ocValue = STRING(PkSdlPris.NyPris NE fGjPris).
  END.
  ELSE ocValue = "false".

  IF ocValue NE "skiprow" THEN
    bPrisAvvik = LOGICAL(ocValue).
  ELSE bPrisAvvik = YES.
END.

PROCEDURE pksdlpris_innprisavvik:
  DEF INPUT  PARAM irPkSdlPris  AS ROWID NO-UNDO.
  DEF INPUT  PARAM icParam      AS CHAR  NO-UNDO.
  DEF INPUT  PARAM icSessionId  AS CHAR  NO-UNDO.
  DEF OUTPUT PARAM ocValue      AS CHAR  NO-UNDO.
  
  FIND PkSdlPris NO-LOCK
       WHERE ROWID(PkSdlPris) = irPkSdlPris
       NO-ERROR.

  IF AVAILABLE PkSdlPris THEN DO:   
    IF icParam NE "" AND LOGICAL(icParam) AND PkSdlPris.NyVarekost = PkSdlPris.Varekost THEN
      ocValue = "skiprow".
    ELSE
      ocValue = STRING(PkSdlPris.NyVarekost NE PkSdlPris.Varekost).
  END.
  ELSE ocValue = "false".

  IF ocValue NE "skiprow" THEN
    bVkAvvik = LOGICAL(ocValue).
  ELSE bVkAvvik = YES.
END.

PROCEDURE pksdlpris_avvik_pris_vk:
  DEF INPUT  PARAM irPkSdlPris  AS ROWID NO-UNDO.
  DEF INPUT  PARAM icParam      AS CHAR  NO-UNDO.
  DEF INPUT  PARAM icSessionId  AS CHAR  NO-UNDO.
  DEF OUTPUT PARAM ocValue      AS CHAR  NO-UNDO.

  /* Forhindrer feilmelding - men er det riktig???? */
  IF icParam = "" THEN
      icParam = 'yes'.

  IF LOGICAL(icParam) AND (bPrisAvvik OR bVkAvvik) THEN
    ocValue = STRING(bPrisAvvik OR bVkAvvik).
  ELSE IF LOGICAL(icParam) THEN 
    ocValue = "skiprow".
  ELSE ocValue = "false".

END PROCEDURE.
