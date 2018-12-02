DEF VAR icl       AS INT NO-UNDO.
DEF VAR iProfilnr AS INT NO-UNDO.
DEF BUFFER clButiker FOR butiker.

{syspara.i 5 1 1 icl INTEGER}
FIND clButiker WHERE clButiker.butik = icl NO-LOCK NO-ERROR.
iProfilnr = IF AVAIL clButiker THEN clButiker.profilnr ELSE 1.

PROCEDURE chkPris:
  DEF INPUT  PARAM irRowid      AS ROWID NO-UNDO.
  DEF INPUT  PARAM icParam      AS CHAR  NO-UNDO. 
  DEF INPUT  PARAM icSessionId  AS CHAR  NO-UNDO.
  DEF OUTPUT PARAM ocValue      AS CHAR  NO-UNDO.

  DEF VAR cSjekkFieldsVPI AS CHAR   NO-UNDO.
  DEF VAR cSjekkFieldsA   AS CHAR   NO-UNDO.
  DEF VAR bhVPI           AS HANDLE NO-UNDO.
  DEF VAR bhArtBas        AS HANDLE NO-UNDO.
  DEF VAR iCounter        AS INT    NO-UNDO.
  DEF VAR fVBL            AS DEC    NO-UNDO.
  DEF VAR fVPI            AS DEC    NO-UNDO.
  DEF VAR bPrisDiff       AS LOG    NO-UNDO.
  DEF VAR bChkPris        AS LOG    NO-UNDO.
  DEF VAR ii              AS INT    NO-UNDO.

  ASSIGN 
    bPrisDiff       = FALSE
    bChkPris        = LOGICAL(ENTRY(1,icParam,'¤'))
    cSjekkFieldsVPI = 'innkjopspris,rab1%,Db%,Varekost,Mva%,Pris'
    cSjekkFieldsA   = 'innkjopspris,rab1%,Db%,Varekost,Mva%,Pris'
  .
  FIND VPImottak WHERE ROWID(VPImottak) = irRowid NO-LOCK NO-ERROR.
  IF AVAIL VPImottak THEN 
  DO:
    FIND ArtBas WHERE ArtBas.ArtikkelNr = VPImottak.Artikkelnr NO-LOCK NO-ERROR.
    IF AVAIL ArtBas THEN
    DO:
      FIND ArtPris WHERE ArtPris.Artikkelnr = ArtBas.ArtikkelNr 
                     AND ArtPris.ProfilNr   = VPImottak.profilnr 
                   NO-LOCK NO-ERROR.
      IF NOT AVAIL ArtPris THEN
        FIND ArtPris WHERE ArtPris.Artikkelnr = ArtBas.ArtikkelNr 
                       AND ArtPris.ProfilNr   = iProfilNr 
                     NO-LOCK NO-ERROR.

      bhVPI    = BUFFER VPImottak:HANDLE.
      bhArtBas = BUFFER ArtPris:HANDLE.
      
      DO iCounter = 1 TO NUM-ENTRIES(cSjekkFieldsVPI):
        ASSIGN 
          fVPI = IF bhVPI:BUFFER-FIELD(ENTRY(iCounter,cSjekkFieldsVPI)):EXTENT GT 0 THEN
                    bhVPI:BUFFER-FIELD(ENTRY(iCounter,cSjekkFieldsVPI)):BUFFER-VALUE
                 ELSE
                   bhVPI:BUFFER-FIELD(ENTRY(iCounter,cSjekkFieldsVPI)):BUFFER-VALUE

          fVBL = IF bhArtBas:BUFFER-FIELD(ENTRY(iCounter,cSjekkFieldsA)):EXTENT GT 0 THEN 
                    bhArtBas:BUFFER-FIELD(ENTRY(iCounter,cSjekkFieldsA)):BUFFER-VALUE(1)
                 ELSE
                   bhArtBas:BUFFER-FIELD(ENTRY(iCounter,cSjekkFieldsA)):BUFFER-VALUE
        NO-ERROR.
        IF ERROR-STATUS:ERROR THEN 
           MESSAGE program-name(1) ERROR-STATUS:GET-MESSAGE(1)
             bhVPI:BUFFER-FIELD(ENTRY(iCounter,cSjekkFieldsVPI)):EXTENT SKIP
             bhArtBas:BUFFER-FIELD(ENTRY(iCounter,cSjekkFieldsA)):EXTENT
           VIEW-AS ALERT-BOX INFO BUTTONS OK.
        IF NOT bPrisDiff THEN
         bPrisDiff = fVBL NE fVPI.
      END. 
    END.
    ASSIGN 
      ocValue = STRING(bPrisDiff)
      ocValue = IF bChkPris AND NOT bPrisDiff THEN 'SKIPROW' ELSE ocValue
    .
  END.

END PROCEDURE.

PROCEDURE chkArtInfo:
  DEF INPUT  PARAM irRowid      AS ROWID NO-UNDO.
  DEF INPUT  PARAM icParam      AS CHAR  NO-UNDO. 
  DEF INPUT  PARAM icSessionId  AS CHAR  NO-UNDO.
  DEF OUTPUT PARAM ocValue      AS CHAR  NO-UNDO.

  DEF VAR cSjekkFieldsVPI AS CHAR   NO-UNDO.
  DEF VAR cSjekkFieldsA   AS CHAR   NO-UNDO.
  DEF VAR bhVPI           AS HANDLE NO-UNDO.
  DEF VAR bhArtBas        AS HANDLE NO-UNDO.
  DEF VAR iCounter        AS INT    NO-UNDO.
  DEF VAR cArtBas         AS CHAR   NO-UNDO.
  DEF VAR cVPI            AS CHAR   NO-UNDO.
  DEF VAR bArtInfoDiff    AS LOG    NO-UNDO.
  DEF VAR bArtInfo        AS LOG    NO-UNDO.

  ASSIGN 
    bArtInfoDiff    = FALSE
    bArtInfo        = LOGICAL(ENTRY(1,icParam,'¤'))
    cSjekkFieldsVPI = 'LevKod,LevFargKod,Beskr'
    cSjekkFieldsA   = 'LevKod,LevFargKod,Beskr'
  .

  FIND VPImottak WHERE ROWID(VPImottak) = irRowid NO-LOCK NO-ERROR.
  IF AVAIL VPImottak THEN 
  DO:
    FIND FIRST ArtBas WHERE ArtBas.ArtikkelNr = VPImottak.ArtikkelNr NO-LOCK NO-ERROR.
    IF AVAIL ArtBas THEN
    DO:
      bhVPI    = BUFFER VPImottak:HANDLE.
      bhArtBas = BUFFER ArtBas:HANDLE.
      DO iCounter = 1 TO NUM-ENTRIES(cSjekkFieldsVPI):
        ASSIGN 
          cVPI    = bhVPI:BUFFER-FIELD(ENTRY(iCounter,cSjekkFieldsVPI)):BUFFER-VALUE
          cArtBas = bhArtBas:BUFFER-FIELD(ENTRY(iCounter,cSjekkFieldsA)):BUFFER-VALUE
        .
        IF NOT bArtInfoDiff THEN 
          bArtInfoDiff = cArtBas NE cVPI. 
      END.
    END.
  END.
  ocValue = STRING(bArtInfoDiff).
  IF bArtInfo AND NOT bArtInfoDiff THEN ocValue = 'SKIPROW'.

END PROCEDURE.

PROCEDURE chkStrek:
  DEF INPUT  PARAM irRowid      AS ROWID NO-UNDO.
  DEF INPUT  PARAM icParam      AS CHAR  NO-UNDO.
  DEF INPUT  PARAM icSessionId  AS CHAR  NO-UNDO.
  DEF OUTPUT PARAM ocValue      AS CHAR  NO-UNDO.

  DEF VAR bNotFound       AS LOG    NO-UNDO.
  DEF VAR bStrek          AS LOG    NO-UNDO.

  ASSIGN
    bNotFound = FALSE
    bStrek    = LOGICAL(ENTRY(1,icParam,'¤'))
  .

  FIND VPImottak WHERE ROWID(VPImottak) = irRowid NO-LOCK NO-ERROR.
  IF AVAIL VPImottak THEN
  DO:
    FOR EACH Strekkode
      WHERE Strekkode.kode = STRING(VPImottak.ArtikkelNr)
      NO-LOCK:
      IF NOT bNotFound THEN
        bNotFound = NOT CAN-FIND(FIRST Strekkode
                             WHERE Strekkode.artikkelnr = DEC(VPImottak.ArtikkelNr)
                               AND Strekkode.strkode    = VPIStrekkode.strkode
                               AND Strekkode.kode       = VPIStrekkode.kode).
      IF bNotFound THEN LEAVE.
    END.
    ASSIGN
      ocValue = STRING(bNotFound).
      ocValue = IF bStrek AND NOT bNotFound THEN 'SKIPROW' ELSE STRING(bNotFound).
  END.

END PROCEDURE.

PROCEDURE Innkjopspris_artbas:
  /*KundeRabatt;AnbefaltPris;KatalogPris;suppRab%;forhRab%*/
  DEF INPUT  PARAM irRowid      AS ROWID NO-UNDO.
  DEF INPUT  PARAM icSessionId  AS CHAR  NO-UNDO.
  DEF OUTPUT PARAM ocValue      AS CHAR  NO-UNDO.
  
  FIND ArtBas WHERE ROWID(ArtBas) = irRowid NO-LOCK NO-ERROR.
  IF AVAIL ArtBas THEN
  DO:
    FIND FIRST ArtPris WHERE ArtPris.Artikkelnr = ArtBas.artikkelnr NO-LOCK NO-ERROR.
    ocValue = IF AVAIL artpris AND artpris.innkjopspris[1] NE ? THEN STRING(artpris.Innkjopspris[1]) ELSE ''.
  END.

END PROCEDURE.

PROCEDURE Varekost_artbas:
  /*KundeRabatt;AnbefaltPris;KatalogPris;suppRab%;forhRab%*/
  DEF INPUT  PARAM irRowid      AS ROWID NO-UNDO.
  DEF INPUT  PARAM icSessionId  AS CHAR  NO-UNDO.
  DEF OUTPUT PARAM ocValue      AS CHAR  NO-UNDO.
  
  FIND ArtBas WHERE ROWID(ArtBas) = irRowid NO-LOCK NO-ERROR.
  IF AVAIL ArtBas THEN
    ocValue = STRING(ArtBas.forhRab%).

END PROCEDURE.

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

PROCEDURE Innkjopspris_artpris:
  /*KundeRabatt;AnbefaltPris;KatalogPris;suppRab%;forhRab%*/
  DEF INPUT  PARAM irRowid      AS ROWID NO-UNDO.
  DEF INPUT  PARAM icSessionId  AS CHAR  NO-UNDO.
  DEF OUTPUT PARAM ocValue      AS CHAR  NO-UNDO.
  
  FIND ArtPris WHERE ROWID(ArtPris) = irRowid NO-LOCK NO-ERROR.
  ocValue = IF AVAIL artpris AND artpris.innkjopspris[1] NE ? THEN STRING(artpris.Innkjopspris[1]) ELSE ''.

END PROCEDURE.

PROCEDURE Rab1%_artpris:
  /*KundeRabatt;AnbefaltPris;KatalogPris;suppRab%;forhRab%*/
  DEF INPUT  PARAM irRowid      AS ROWID NO-UNDO.
  DEF INPUT  PARAM icSessionId  AS CHAR  NO-UNDO.
  DEF OUTPUT PARAM ocValue      AS CHAR  NO-UNDO.
  
  FIND ArtPris WHERE ROWID(ArtPris) = irRowid NO-LOCK NO-ERROR.
  ocValue = IF AVAIL artpris AND artpris.Rab1%[1] NE ? THEN STRING(artpris.Rab1%[1]) ELSE ''.

END PROCEDURE.
PROCEDURE DB%_artpris:
  /*KundeRabatt;AnbefaltPris;KatalogPris;suppRab%;forhRab%*/
  DEF INPUT  PARAM irRowid      AS ROWID NO-UNDO.
  DEF INPUT  PARAM icSessionId  AS CHAR  NO-UNDO.
  DEF OUTPUT PARAM ocValue      AS CHAR  NO-UNDO.
  
  FIND ArtPris WHERE ROWID(ArtPris) = irRowid NO-LOCK NO-ERROR.
  ocValue = IF AVAIL artpris AND artpris.DB%[1] NE ? THEN STRING(artpris.DB%[1]) ELSE ''.

END PROCEDURE.
PROCEDURE Varekost_artpris:
  /*KundeRabatt;AnbefaltPris;KatalogPris;suppRab%;forhRab%*/
  DEF INPUT  PARAM irRowid      AS ROWID NO-UNDO.
  DEF INPUT  PARAM icSessionId  AS CHAR  NO-UNDO.
  DEF OUTPUT PARAM ocValue      AS CHAR  NO-UNDO.
  
  FIND ArtPris WHERE ROWID(ArtPris) = irRowid NO-LOCK NO-ERROR.
  ocValue = IF AVAIL artpris AND artpris.Varekost[1] NE ? THEN STRING(artpris.Varekost[1]) ELSE ''.

END PROCEDURE.
PROCEDURE Mva%_artpris:
  /*KundeRabatt;AnbefaltPris;KatalogPris;suppRab%;forhRab%*/
  DEF INPUT  PARAM irRowid      AS ROWID NO-UNDO.
  DEF INPUT  PARAM icSessionId  AS CHAR  NO-UNDO.
  DEF OUTPUT PARAM ocValue      AS CHAR  NO-UNDO.
  
  FIND ArtPris WHERE ROWID(ArtPris) = irRowid NO-LOCK NO-ERROR.
  ocValue = IF AVAIL artpris AND artpris.Mva%[1] NE ? THEN STRING(artpris.Mva%[1]) ELSE ''.

END PROCEDURE.
PROCEDURE Pris_artpris:
  /*KundeRabatt;AnbefaltPris;KatalogPris;suppRab%;forhRab%*/
  DEF INPUT  PARAM irRowid      AS ROWID NO-UNDO.
  DEF INPUT  PARAM icSessionId  AS CHAR  NO-UNDO.
  DEF OUTPUT PARAM ocValue      AS CHAR  NO-UNDO.
  
  FIND ArtPris WHERE ROWID(ArtPris) = irRowid NO-LOCK NO-ERROR.
  ocValue = IF AVAIL artpris AND artpris.Pris[1] NE ? THEN STRING(artpris.Pris[1]) ELSE ''.

END PROCEDURE.
