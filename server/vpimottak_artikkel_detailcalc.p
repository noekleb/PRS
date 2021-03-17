
PROCEDURE DB%:
  /*KundeRabatt;AnbefaltPris;KatalogPris;suppRab%;forhRab%*/
  DEF INPUT  PARAM irRowid      AS ROWID NO-UNDO.
  DEF INPUT  PARAM icSessionId  AS CHAR  NO-UNDO.
  DEF OUTPUT PARAM ocValue      AS CHAR  NO-UNDO.

  DEF VAR fMvaKr AS DEC NO-UNDO.
  DEF VAR fDbKr  AS DEC NO-UNDO.
  DEF VAR fDB%   AS DEC NO-UNDO.
  
  FIND VPImottak WHERE ROWID(VPImottak) = irRowid NO-LOCK NO-ERROR.
  IF AVAIL VPImottak THEN
    ASSIGN 
      fMvaKr   = VPImottak.pris - (VPImottak.pris / (1 + vpimottak.mva% / 100)) 
      fDbKr    = (VPImottak.pris - fMvaKr - VPImottak.varekost)
      fDB%     = ROUND((fDbKr * 100) / (VPImottak.varekost + fDbKr),2)
      ocValue  = IF fDb% = ? THEN '' ELSE STRING(fDB%)
    NO-ERROR.
END PROCEDURE.

PROCEDURE Varekost-2:
  /*KundeRabatt;AnbefaltPris;KatalogPris;suppRab%;forhRab%*/
  DEF INPUT  PARAM irRowid      AS ROWID NO-UNDO.
  DEF INPUT  PARAM icSessionId  AS CHAR  NO-UNDO.
  DEF OUTPUT PARAM ocValue      AS CHAR  NO-UNDO.
  
  FIND ArtBas WHERE ROWID(ArtBas) = irRowid NO-LOCK NO-ERROR.
  IF AVAIL ArtBas THEN
  DO:
    ocValue = STRING(ArtBas.Katalogpris - (ArtBas.Katalogpris * (ArtBas.ForhRab% / 100))).
  END.
END PROCEDURE.

PROCEDURE SuppVarekost-2:
  /*KundeRabatt;AnbefaltPris;KatalogPris;suppRab%;forhRab%*/
  DEF INPUT  PARAM irRowid      AS ROWID NO-UNDO.
  DEF INPUT  PARAM icSessionId  AS CHAR  NO-UNDO.
  DEF OUTPUT PARAM ocValue      AS CHAR  NO-UNDO.
  
  FIND ArtBas WHERE ROWID(ArtBas) = irRowid NO-LOCK NO-ERROR.
  IF AVAIL ArtBas THEN
  DO:
    ocValue = STRING(ArtBas.Katalogpris - (ArtBas.Katalogpris * (ArtBas.SupRab% / 100))).
  END.
END PROCEDURE.

PROCEDURE Innkjopspris#1-2:
  /*KundeRabatt;AnbefaltPris;KatalogPris;suppRab%;forhRab%*/
  DEF INPUT  PARAM irRowid      AS ROWID NO-UNDO.
  DEF INPUT  PARAM icSessionId  AS CHAR  NO-UNDO.
  DEF OUTPUT PARAM ocValue      AS CHAR  NO-UNDO.
  
  FIND ArtPris WHERE ROWID(ArtPris) = irRowid NO-LOCK NO-ERROR.
  IF AVAIL ArtPris THEN 
    ocValue = STRING(ArtPris.Innkjopspris[1]).

END PROCEDURE.

PROCEDURE Rab1%#1-2:
  /*KundeRabatt;AnbefaltPris;KatalogPris;suppRab%;forhRab%*/
  DEF INPUT  PARAM irRowid      AS ROWID NO-UNDO.
  DEF INPUT  PARAM icSessionId  AS CHAR  NO-UNDO.
  DEF OUTPUT PARAM ocValue      AS CHAR  NO-UNDO.
  
  FIND ArtPris WHERE ROWID(ArtPris) = irRowid NO-LOCK NO-ERROR.
  IF AVAIL ArtPris THEN 
    ocValue = STRING(ArtPris.Rab1%[1]).

END PROCEDURE.

PROCEDURE Varekost#1-2:
  /*KundeRabatt;AnbefaltPris;KatalogPris;suppRab%;forhRab%*/
  DEF INPUT  PARAM irRowid      AS ROWID NO-UNDO.
  DEF INPUT  PARAM icSessionId  AS CHAR  NO-UNDO.
  DEF OUTPUT PARAM ocValue      AS CHAR  NO-UNDO.
  
  FIND ArtPris WHERE ROWID(ArtPris) = irRowid NO-LOCK NO-ERROR.
  IF AVAIL ArtPris THEN 
    ocValue = STRING(ArtPris.varekost[1]).
END PROCEDURE.

PROCEDURE DB%#1-2:
  /*KundeRabatt;AnbefaltPris;KatalogPris;suppRab%;forhRab%*/
  DEF INPUT  PARAM irRowid      AS ROWID NO-UNDO.
  DEF INPUT  PARAM icSessionId  AS CHAR  NO-UNDO.
  DEF OUTPUT PARAM ocValue      AS CHAR  NO-UNDO.
  
  FIND ArtPris WHERE ROWID(ArtPris) = irRowid NO-LOCK NO-ERROR.
  IF AVAIL ArtPris THEN 
    ocValue = STRING(ArtPris.DB%[1]).

END PROCEDURE.

PROCEDURE Mva%#1-2:
  /*KundeRabatt;AnbefaltPris;KatalogPris;suppRab%;forhRab%*/
  DEF INPUT  PARAM irRowid      AS ROWID NO-UNDO.
  DEF INPUT  PARAM icSessionId  AS CHAR  NO-UNDO.
  DEF OUTPUT PARAM ocValue      AS CHAR  NO-UNDO.
  
  FIND ArtPris WHERE ROWID(ArtPris) = irRowid NO-LOCK NO-ERROR.
  IF AVAIL ArtPris THEN 
    ocValue = STRING(ArtPris.Mva%[1]).

END PROCEDURE.

PROCEDURE Pris#1-2:
  /*KundeRabatt;AnbefaltPris;KatalogPris;suppRab%;forhRab%*/
  DEF INPUT  PARAM irRowid      AS ROWID NO-UNDO.
  DEF INPUT  PARAM icSessionId  AS CHAR  NO-UNDO.
  DEF OUTPUT PARAM ocValue      AS CHAR  NO-UNDO.
  
  FIND ArtPris WHERE ROWID(ArtPris) = irRowid NO-LOCK NO-ERROR.
  IF AVAIL ArtPris THEN 
    ocValue = STRING(ArtPris.Pris[1]).

END PROCEDURE.



