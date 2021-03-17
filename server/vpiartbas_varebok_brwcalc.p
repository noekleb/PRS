PROCEDURE chkArtReg:
  DEF INPUT  PARAM irRowid      AS ROWID NO-UNDO.
  DEF INPUT  PARAM icParam      AS CHAR  NO-UNDO. 
  DEF INPUT  PARAM icSessionId  AS CHAR  NO-UNDO.
  DEF OUTPUT PARAM ocValue      AS CHAR  NO-UNDO.
  
  DEF VAR bArtReg    AS LOG NO-UNDO.
  DEF VAR bFound     AS LOG NO-UNDO.

  ASSIGN 
    bFound    = FALSE
    bArtReg   = LOGICAL(ENTRY(1,icParam,'¤'))
  .

  FIND VPIArtBas WHERE ROWID(VPIArtBas) = irRowid NO-LOCK NO-ERROR.
  IF AVAIL VPIArtBas THEN 
  DO:    
      bFound    = CAN-FIND(FIRST artbas WHERE ArtBas.artikkelnr = vpiartbas.artikkelnr).
  END.
  ASSIGN 
    ocValue   = STRING(NOT bFound)
    ocValue   = IF bArtReg AND bFound THEN 'SKIPROW' ELSE ocValue
  . 
  
END PROCEDURE.

PROCEDURE chkVarebok:
  DEF INPUT  PARAM irRowid      AS ROWID NO-UNDO.
  DEF INPUT  PARAM icParam      AS CHAR  NO-UNDO. 
  DEF INPUT  PARAM icSessionId  AS CHAR  NO-UNDO.
  DEF OUTPUT PARAM ocValue      AS CHAR  NO-UNDO.
  
  DEF VAR iVarebokNr   AS DEC    NO-UNDO.
  DEF VAR bVarebok     AS LOG NO-UNDO.
  DEF VAR bFound       AS LOG NO-UNDO.

  ASSIGN 
    bFound      = FALSE
    iVarebokNr  = DEC(ENTRY(1,icParam,'¤'))
    bVarebok    = LOGICAL(ENTRY(2,icParam,'¤'))
  .

  FIND FIRST VPIArtBas WHERE ROWID(VPIArtBas) = irRowid NO-LOCK NO-ERROR.
  IF AVAIL VPIArtBas THEN
  DO:
    FIND FIRST VareBokHode WHERE VareBokHode.vareboknr = iVarebokNr NO-LOCK NO-ERROR.
    IF AVAIL VareBokHode THEN
    DO:
      bFound = CAN-FIND(varebokLinje WHERE Vareboklinje.vareboknr = varebokhode.vareboknr 
                                       AND VarebokLinje.ArtikkelNr = VPIArtBas.ArtikkelNr).       
    END.
    ASSIGN 
      ocValue = STRING(NOT bFound)  /*Må være omvendt da det er ikke-funn som er avvik*/
      ocValue = IF bVarebok AND bFound THEN 'SKIPROW' ELSE ocValue
    .
  END.
END PROCEDURE.

PROCEDURE chkPris:
  DEF INPUT  PARAM irRowid      AS ROWID NO-UNDO.
  DEF INPUT  PARAM icParam      AS CHAR  NO-UNDO. 
  DEF INPUT  PARAM icSessionId  AS CHAR  NO-UNDO.
  DEF OUTPUT PARAM ocValue      AS CHAR  NO-UNDO.

  DEF VAR iVarebokNr      AS dec    NO-UNDO.
  DEF VAR cSjekkFieldsVPI AS CHAR   NO-UNDO.
  DEF VAR cSjekkFieldsVBL AS CHAR   NO-UNDO.
  DEF VAR bhVPI           AS HANDLE NO-UNDO.
  DEF VAR bhVarebokLinje  AS HANDLE NO-UNDO.
  DEF VAR iCounter        AS INT    NO-UNDO.
  DEF VAR fVBL            AS DEC    NO-UNDO.
  DEF VAR fVPI            AS DEC    NO-UNDO.
  DEF VAR bPrisDiff       AS LOG    NO-UNDO.
  DEF VAR bChkPris        AS LOG    NO-UNDO.
  DEF VAR ii              AS INT    NO-UNDO.

  ASSIGN 
    bPrisDiff       = FALSE
    iVarebokNr      = DEC(ENTRY(1,icParam,'¤'))
    bChkPris        = LOGICAL(ENTRY(2,icParam,'¤'))
    cSjekkFieldsVPI = 'anbefaltpris,forhRab%,Katalogpris,suppRab%'
    cSjekkFieldsVBL = 'anbefaltpris,forhRab%,Innkjopspris,supRab%'
  .
  IF iVareboknr LE 0 THEN
    LEAVE.
  FIND VPIArtBas WHERE ROWID(VPIArtBas) = irRowid NO-LOCK NO-ERROR.
  IF AVAIL VPIArtBas THEN 
  DO:
    bhVPI = BUFFER VPIArtBAS:HANDLE.
    FIND VareBokHode WHERE VareBokHode.vareboknr = iVarebokNr NO-LOCK NO-ERROR.
    IF AVAIL VareBokHode THEN
    DO:
      FOR FIRST varebokLinje 
        WHERE Vareboklinje.vareboknr = varebokhode.vareboknr 
          AND VarebokLinje.ArtikkelNr = VPIArtBas.ArtikkelNr NO-LOCK: LEAVE. END.
      IF AVAIL VarebokLinje THEN
      DO:
        bhVarebokLinje = BUFFER VarebokLinje:HANDLE.
        DO iCounter = 1 TO NUM-ENTRIES(cSjekkFieldsVPI):
          ASSIGN 
            fVPI = IF bhVPI:BUFFER-FIELD(ENTRY(iCounter,cSjekkFieldsVPI)):EXTENT GT 0 THEN
                     bhVPI:BUFFER-FIELD(ENTRY(iCounter,cSjekkFieldsVPI)):BUFFER-VALUE(1)
                   ELSE
                     bhVPI:BUFFER-FIELD(ENTRY(iCounter,cSjekkFieldsVPI)):BUFFER-VALUE

            fVBL = IF bhVarebokLinje:BUFFER-FIELD(ENTRY(iCounter,cSjekkFieldsVBL)):EXTENT GT 0 THEN 
                     bhVarebokLinje:BUFFER-FIELD(ENTRY(iCounter,cSjekkFieldsVBL)):BUFFER-VALUE(1)
                   ELSE
                     bhVarebokLinje:BUFFER-FIELD(ENTRY(iCounter,cSjekkFieldsVBL)):BUFFER-VALUE
          NO-ERROR.
          IF ERROR-STATUS:ERROR THEN 
            MESSAGE program-name(1) ERROR-STATUS:GET-MESSAGE(1)
              bhVPI:BUFFER-FIELD(ENTRY(iCounter,cSjekkFieldsVPI)):EXTENT SKIP 
              bhVarebokLinje:BUFFER-FIELD(ENTRY(iCounter,cSjekkFieldsVBL)):EXTENT  
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
          IF NOT bPrisDiff THEN
            bPrisDiff = fVBL NE fVPI.
/*           IF bPrisDiff THEN MESSAGE vareboklinje.vareboknr SKIP vareboklinje.artikkelnr SKIP ENTRY(iCounter,cSjekkFieldsVPI) SKIP fVPI SKIP fVBL */
/*             VIEW-AS ALERT-BOX.                                                                                                                   */
        END.
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

  DEF VAR iVarebokNr      AS DEC    NO-UNDO.
  DEF VAR cSjekkFieldsVPI AS CHAR   NO-UNDO.
  DEF VAR cSjekkFieldsVBL AS CHAR   NO-UNDO.
  DEF VAR bhVPI           AS HANDLE NO-UNDO.
  DEF VAR bhVarebokLinje  AS HANDLE NO-UNDO.
  DEF VAR iCounter        AS INT    NO-UNDO.
  DEF VAR cVBL            AS CHAR   NO-UNDO.
  DEF VAR cVPI            AS CHAR   NO-UNDO.
  DEF VAR bArtInfoDiff    AS LOG    NO-UNDO.
  DEF VAR bArtInfo        AS LOG    NO-UNDO.

  ASSIGN 
    bArtInfoDiff    = FALSE
    iVarebokNr      = DEC(ENTRY(1,icParam,'¤'))
    bArtInfo        = LOGICAL(ENTRY(2,icParam,'¤'))
    cSjekkFieldsVPI = 'LevKod,LevFargKod,Beskr'
    cSjekkFieldsVBL = 'LevKod,LevFargKod,Beskr'
  .

  FIND VPIArtBas WHERE ROWID(VPIArtBas) = irRowid NO-LOCK NO-ERROR.
  IF AVAIL VPIArtBas THEN 
  DO:
    bhVPI = BUFFER VPIArtBAS:HANDLE.
    FIND VareBokHode WHERE VareBokHode.vareboknr = iVarebokNr NO-LOCK NO-ERROR.
    IF AVAIL VareBokHode THEN
    DO:
      FOR FIRST varebokLinje 
        WHERE Vareboklinje.vareboknr = varebokhode.vareboknr 
          AND VarebokLinje.ArtikkelNr = VPIArtBas.ArtikkelNr NO-LOCK: LEAVE. END.
      IF AVAIL VarebokLinje THEN
      DO:
        bhVarebokLinje = BUFFER VarebokLinje:HANDLE.
        DO iCounter = 1 TO NUM-ENTRIES(cSjekkFieldsVPI):
          ASSIGN 
            cVPI = bhVPI:BUFFER-FIELD(ENTRY(iCounter,cSjekkFieldsVPI)):BUFFER-VALUE
            cVBL = bhVarebokLinje:BUFFER-FIELD(ENTRY(iCounter,cSjekkFieldsVBL)):BUFFER-VALUE
          .
          IF NOT bArtInfoDiff THEN bArtInfoDiff = cVBL NE cVPI. 
        END.
      END.
    END.
    ocValue = STRING(bArtInfoDiff).
    IF bArtInfo AND NOT bArtInfoDiff THEN ocValue = 'SKIPROW'.

  END.
END PROCEDURE.

PROCEDURE chkStrek:
  DEF INPUT  PARAM irRowid      AS ROWID NO-UNDO.
  DEF INPUT  PARAM icParam      AS CHAR  NO-UNDO. 
  DEF INPUT  PARAM icSessionId  AS CHAR  NO-UNDO.
  DEF OUTPUT PARAM ocValue      AS CHAR  NO-UNDO.

  DEF VAR bFound       AS LOG    NO-UNDO.
  DEF VAR bStrek       AS LOG    NO-UNDO.
  
  ASSIGN 
    bFound = TRUE
    bStrek = LOGICAL(ENTRY(1,icParam,'¤'))
  .

  FIND VPIArtBas WHERE ROWID(VPIArtBas) = irRowid NO-LOCK NO-ERROR.
  IF AVAIL VPIArtBas THEN 
  DO:
    FOR EACH VPIStrekkode 
      WHERE VPIStrekkode.EkstVPILevNr = VPIArtBas.EkstVPILevNr 
        AND VPIStrekkode.varenr       = VPIArtBas.Varenr
      NO-LOCK:
      IF bFound THEN 
        bFound = CAN-FIND(FIRST Strekkode 
                             WHERE Strekkode.artikkelnr = DEC(VPIStrekkode.varenr) 
                               AND Strekkode.strkode    = VPIStrekkode.strkode
                               AND Strekkode.kode       = VPIStrekkode.kode).
      IF NOT bFound THEN LEAVE.
    END.
    ASSIGN 
      ocValue = STRING(NOT bFound).
      ocValue = IF bStrek AND bFound THEN 'SKIPROW' ELSE ocValue.
  END.

END PROCEDURE.

PROCEDURE chkAnonseArtikkel:
  DEF INPUT  PARAM irRowid      AS ROWID NO-UNDO.
  DEF INPUT  PARAM icParam      AS CHAR  NO-UNDO. 
  DEF INPUT  PARAM icSessionId  AS CHAR  NO-UNDO.
  DEF OUTPUT PARAM ocValue      AS CHAR  NO-UNDO.

  DEF VAR bAnnonsePlass   AS LOG    NO-UNDO.
  
  ASSIGN 
    bAnnonsePlass = LOGICAL(ENTRY(1,icParam,'¤'))
  .

  FIND VPIArtBas WHERE ROWID(VPIArtBas) = irRowid NO-LOCK NO-ERROR.
  IF AVAIL VPIArtBas THEN
    ASSIGN
      ocValue = STRING(VPIArtBas.AnonseArtikkel)
      ocValue = IF bAnnonsePlass AND NOT VPIArtBas.AnonseArtikkel THEN 'SKIPROW' ELSE ocValue
    .

END PROCEDURE.

PROCEDURE chkKontroll:
  DEF INPUT  PARAM irRowid      AS ROWID NO-UNDO.
  DEF INPUT  PARAM icParam      AS CHAR  NO-UNDO. 
  DEF INPUT  PARAM icSessionId  AS CHAR  NO-UNDO.
  DEF OUTPUT PARAM ocValue      AS CHAR  NO-UNDO.
  
  DEF BUFFER bVPIArtBas FOR VPIArtBas.

  DEF VAR bKontroll   AS LOG    NO-UNDO.
  DEF VAR cKontroll   AS CHAR   NO-UNDO.
  DEF VAR bHit        AS LOG    NO-UNDO.
  DEF VAR dFraVPIDato AS DATE   NO-UNDO.
  DEF VAR dTilVPIDato AS DATE   NO-UNDO.

  ASSIGN
    cKontroll      = ENTRY(1,icParam,'¤')
    bKontroll      = NOT cKontroll = ''
    dFraVPIDato    = ?
    dTilVPIDato    = ?
  .
  IF NUM-ENTRIES(icParam,'¤') = 3 THEN
      ASSIGN
        dFraVPIDato = IF ENTRY(2,icParam,'¤') <> '' THEN DATE(ENTRY(2,icParam,'¤')) ELSE ?
        dTilVPIDato = IF ENTRY(3,icParam,'¤') <> '' THEN DATE(ENTRY(3,icParam,'¤')) ELSE ?
        .

  /*
  MESSAGE icParam SKIP
      dFraVPIDato dTilVPIDato
      VIEW-AS ALERT-BOX INFO BUTTONS OK.
  */
  CASE cKontroll:
      WHEN 'Rule6_EAN' THEN
      DO:
        FIND VPIArtBas WHERE ROWID(VPIArtBas) = irRowid NO-LOCK NO-ERROR.
        IF AVAIL VPIArtBas THEN
        DO:
          FOR EACH VPIStrekkode OF VPIArtBas NO-LOCK:
              FIND Strekkode NO-LOCK WHERE
                  Strekkode.Kode = VPIStrekkode.Kode NO-ERROR.
              IF AVAILABLE Strekkode AND 
                  Strekkode.ArtikkelNr = DEC(VPIArtBas.VareNr) and
                  Strekkode.StrKode <> VPIStrekkode.StrKode THEN
                  bHit = TRUE.
          END.
          ocValue = STRING(bHit) .
          IF bKontroll AND NOT bHit THEN ocValue = 'SKIPROW'.
        END.
      END.
      WHEN 'Rule5_EAN' THEN
      DO:
        FIND VPIArtBas WHERE ROWID(VPIArtBas) = irRowid NO-LOCK NO-ERROR.
        IF AVAIL VPIArtBas THEN
        DO:
          FOR EACH VPIStrekkode OF VPIArtBas NO-LOCK:
              FIND Strekkode NO-LOCK WHERE
                  Strekkode.Kode = VPIStrekkode.Kode NO-ERROR.
              IF AVAILABLE Strekkode AND
                  Strekkode.ArtikkelNr <> DEC(VPIArtBas.VareNr) THEN
                  bHit = TRUE.
          END.
          ocValue = STRING(bHit) .
          IF bKontroll AND NOT bHit THEN ocValue = 'SKIPROW'.
        END.
      END.
    WHEN 'Rule1_EAN' THEN
    DO:
      FIND VPIArtBas WHERE ROWID(VPIArtBas) = irRowid NO-LOCK NO-ERROR.
      IF AVAIL VPIArtBas THEN
      DO:
        bHit = NOT CAN-FIND(FIRST VPIStrekkode WHERE VPIStrekkode.EkstVPILevNr = VPIArtBas.EkstVPILevNr
                                                       AND VPIStrekkode.VareNr       = VPIArtBas.VareNr).
        ocValue = STRING(bHit) .
        IF bKontroll AND NOT bHit THEN ocValue = 'SKIPROW'.
      END.
    END.
    WHEN 'Rule2_LevKod' THEN
    DO:
      FIND VPIArtBas WHERE ROWID(VPIArtBas) = irRowid NO-LOCK NO-ERROR.
      IF AVAIL VPIArtBas THEN
      VPIKONTROLL1:
      DO:
          IF (dFraVPIDato <> ? AND VPIArtBas.VPIDato < dFraVPIDato) OR
             (dTilVPIDato <> ? AND VPIArtBas.VPIDato > dTilVPIDato)
              THEN DO :
                ocValue = 'SKIPROW'.
                LEAVE VPIKONTROLL1.
              END.

        bHit =  CAN-FIND(FIRST bVPIArtBas WHERE 
                      bVPIArtBas.EkstVPILevNr = VPIArtBas.EkstVPILEvNr 
                  AND bVPIArtBas.Beskr        = VPIArtBas.Beskr 
                  and bVPIArtBas.LevKod      NE VPIArtBas.LevKod
                  AND (IF dFraVPIDato <> ? THEN bVPIArtBas.VPIDato >= dFraVPIDato ELSE TRUE)
                  AND (IF dTilVPIDato <> ? THEN bVPIArtBas.VPIDAto <= dTilVPIDato ELSE TRUE)
                  AND ROWID(bVPIArtBas) NE ROWID(VPIArtBas)).
        ocValue = STRING(bHit) .
        IF bKontroll AND NOT bHit THEN ocValue = 'SKIPROW'.        
      END. /* VPIKONTROLL1 */
    END.
    WHEN 'Rule3_Beskr' THEN
    DO:
      FIND VPIArtBas WHERE ROWID(VPIArtBas) = irRowid NO-LOCK NO-ERROR.
      IF AVAIL VPIArtBas THEN
      VPIKONTROLL2:
      DO:
        IF (dFraVPIDato <> ? AND VPIArtBas.VPIDato < dFraVPIDato) OR
           (dTilVPIDato <> ? AND VPIArtBas.VPIDato > dTilVPIDato)
            THEN DO :
              ocValue = 'SKIPROW'.
              LEAVE VPIKONTROLL2.
            END.

        bHit =  CAN-FIND(FIRST bVPIArtBas WHERE 
                       bVPIArtBas.EkstVPILevNr = VPIArtBas.EkstVPILEvNr 
                   AND bVPIArtBas.LevKod       = VPIArtBas.LevKod 
                   and bVPIArtBas.Beskr       NE VPIArtBas.Beskr
                   AND (IF dFraVPIDato <> ? THEN bVPIArtBas.VPIDato >= dFraVPIDato ELSE TRUE)
                   AND (IF dTilVPIDato <> ? THEN bVPIArtBas.VPIDAto <= dTilVPIDato ELSE TRUE)
                   AND ROWID(bVPIArtBas) NE ROWID(VPIArtBas)).
         
      END. /* VPIKONTROLL2 */
      ocValue = STRING(bHit) .
      IF bKontroll AND NOT bHit THEN ocValue = 'SKIPROW'.
    END.
    WHEN 'Rule4_bilderef' THEN
    DO:
        bHit = VPIArtBas.VPIBildeKode = ''.
        ocValue = STRING(bHit) .
        IF bKontroll AND NOT bHit THEN ocValue = 'SKIPROW'.
    END.
    OTHERWISE
    DO:
      ocValue = 'FALSE'.
    END.
  END CASE.
END PROCEDURE.

PROCEDURE KatalogPris#1:
  /*KundeRabatt;AnbefaltPris;KatalogPris;suppRab%;forhRab%*/
  DEF INPUT  PARAM irRowid      AS ROWID NO-UNDO.
  DEF INPUT  PARAM icSessionId  AS CHAR  NO-UNDO.
  DEF OUTPUT PARAM ocValue      AS CHAR  NO-UNDO.
  
  FIND VPIArtBas WHERE ROWID(VPIArtBas) = irRowid NO-LOCK NO-ERROR.
  IF AVAIL VPIArtBas THEN 
    ocValue = STRING(VPIArtBas.KatalogPris[1]).

END PROCEDURE.

PROCEDURE suppRab%#1:
  /*KundeRabatt;AnbefaltPris;KatalogPris;suppRab%;forhRab%*/
  DEF INPUT  PARAM irRowid      AS ROWID NO-UNDO.
  DEF INPUT  PARAM icSessionId  AS CHAR  NO-UNDO.
  DEF OUTPUT PARAM ocValue      AS CHAR  NO-UNDO.
  
  FIND VPIArtBas WHERE ROWID(VPIArtBas) = irRowid NO-LOCK NO-ERROR.
  IF AVAIL VPIArtBas THEN 
    ocValue = STRING(VPIArtBas.suppRab%[1]).

END PROCEDURE.

PROCEDURE forhRab%#1:
  /*KundeRabatt;AnbefaltPris;KatalogPris;suppRab%;forhRab%*/
  DEF INPUT  PARAM irRowid      AS ROWID NO-UNDO.
  DEF INPUT  PARAM icSessionId  AS CHAR  NO-UNDO.
  DEF OUTPUT PARAM ocValue      AS CHAR  NO-UNDO.
  
  FIND VPIArtBas WHERE ROWID(VPIArtBas) = irRowid NO-LOCK NO-ERROR.
  IF AVAIL VPIArtBas THEN 
    ocValue = STRING(VPIArtBas.forhRab%[1]).

END PROCEDURE.

PROCEDURE InnkjopsPris#1:
  /*KundeRabatt;AnbefaltPris;KatalogPris;suppRab%;forhRab%*/
  DEF INPUT  PARAM irRowid      AS ROWID NO-UNDO.
  DEF INPUT  PARAM icSessionId  AS CHAR  NO-UNDO.
  DEF OUTPUT PARAM ocValue      AS CHAR  NO-UNDO.
  
/*   FIND VPIArtBas WHERE ROWID(VPIArtBas) = irRowid NO-LOCK NO-ERROR. */
/*   IF AVAIL VPIArtBas THEN                                           */
/*     ocValue = STRING(VPIArtBas.forhRab%[1]).                        */

END PROCEDURE.

PROCEDURE VarekostForh:
  DEF INPUT  PARAM irRowid      AS ROWID NO-UNDO.
  DEF INPUT  PARAM icSessionId  AS CHAR  NO-UNDO.
  DEF OUTPUT PARAM ocValue      AS CHAR  NO-UNDO.
  
  FIND VPIArtBas WHERE ROWID(VPIArtBas) = irRowid NO-LOCK NO-ERROR.
  IF AVAIL VPIArtBas THEN
  DO:
    ocValue = STRING(VPIArtBas.Katalogpris[1] - (VPIArtBas.Katalogpris[1] * (VPIArtBas.ForhRab%[1] / 100))).
  END.
END PROCEDURE.

PROCEDURE VarekostSup:
  DEF INPUT  PARAM irRowid      AS ROWID NO-UNDO.
  DEF INPUT  PARAM icSessionId  AS CHAR  NO-UNDO.
  DEF OUTPUT PARAM ocValue      AS CHAR  NO-UNDO.
  
  FIND VPIArtBas WHERE ROWID(VPIArtBas) = irRowid NO-LOCK NO-ERROR.
  IF AVAIL VPIArtBas THEN
  DO:
    ocValue = STRING(VPIArtBas.Katalogpris[1] - (VPIArtBas.Katalogpris[1] * (VPIArtBas.SuppRab%[1] / 100))).
  END.
END PROCEDURE.

