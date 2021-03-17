DEF VAR iCL AS INT NO-UNDO.
DEF VAR iProfilNr AS INT NO-UNDO.

DEF BUFFER clButiker FOR Butiker.

{syspara.i 5 1 1 iCL INT}
FIND clButiker NO-LOCK WHERE
    clButiker.Butik = iCL NO-ERROR.
IF AVAILABLE clButiker THEN
    iProfilNr = clButiker.ProfilNr.
ELSE
    iProfilNr = 1.



/* **********************  Internal Procedures  *********************** */


PROCEDURE AktivInnkjopsPris:
/*------------------------------------------------------------------------------
		Purpose:  																	  
		Notes:  																	  
------------------------------------------------------------------------------*/
  DEF INPUT  PARAM irRowid      AS ROWID NO-UNDO.
  DEF INPUT  PARAM icParam      AS CHAR  NO-UNDO. 
  DEF INPUT  PARAM icSessionId  AS CHAR  NO-UNDO.
  DEF OUTPUT PARAM ocValue      AS CHAR  NO-UNDO.

  FIND VPIArtBas WHERE ROWID(VPIArtBas) = irRowid NO-LOCK NO-ERROR.

  IF AVAIL VPIArtBas THEN
  DO:
    FIND FIRST VPIArtPris OF VPIArtBas NO-LOCK NO-ERROR.
    IF AVAILABLE VPIArtPris THEN 
        FIND FIRST ArtPris NO-LOCK WHERE
            ArtPris.ArtikkelNr = DEC(VPIArtBas.VareNr) AND
            ArtPris.ProfilNr   = VPIArtPris.ProfilNr NO-ERROR.
    IF NOT AVAILABLE ArtPris THEN 
      FIND FIRST ArtPris NO-LOCK WHERE
          ArtPris.ArtikkelNr = DEC(VPIArtBas.VareNr) NO-ERROR.
    ocValue = IF AVAIL ArtPris AND ArtPris.InnkjopsPris[1] NE ? 
        THEN STRING(ArtPris.InnkjopsPris[1]) 
        ELSE ''.
  END.

END PROCEDURE.

PROCEDURE AktivRabatt%:
/*------------------------------------------------------------------------------
    Purpose:                                      
    Notes:                                      
------------------------------------------------------------------------------*/
  DEF INPUT  PARAM irRowid      AS ROWID NO-UNDO.
  DEF INPUT  PARAM icParam      AS CHAR  NO-UNDO. 
  DEF INPUT  PARAM icSessionId  AS CHAR  NO-UNDO.
  DEF OUTPUT PARAM ocValue      AS CHAR  NO-UNDO.

  FIND VPIArtBas WHERE ROWID(VPIArtBas) = irRowid NO-LOCK NO-ERROR.

  IF AVAIL VPIArtBas THEN
  DO:
    FIND FIRST VPIArtPris OF VPIArtBas NO-LOCK NO-ERROR.
    IF AVAILABLE VPIArtPris THEN 
        FIND FIRST ArtPris NO-LOCK WHERE
            ArtPris.ArtikkelNr = DEC(VPIArtBas.VareNr) AND
            ArtPris.ProfilNr   = VPIArtPris.ProfilNr NO-ERROR.
    IF NOT AVAILABLE ArtPris THEN 
      FIND FIRST ArtPris NO-LOCK WHERE
          ArtPris.ArtikkelNr = DEC(VPIArtBas.VareNr) NO-ERROR.
    ocValue = IF AVAIL ArtPris AND ArtPris.Rab1%[1] NE ? 
        THEN STRING(ArtPris.Rab1%[1]) 
        ELSE ''.
  END.

END PROCEDURE.

PROCEDURE AktivDb%:
/*------------------------------------------------------------------------------
    Purpose:                                      
    Notes:                                      
------------------------------------------------------------------------------*/
  DEF INPUT  PARAM irRowid      AS ROWID NO-UNDO.
  DEF INPUT  PARAM icParam      AS CHAR  NO-UNDO. 
  DEF INPUT  PARAM icSessionId  AS CHAR  NO-UNDO.
  DEF OUTPUT PARAM ocValue      AS CHAR  NO-UNDO.

  FIND VPIArtBas WHERE ROWID(VPIArtBas) = irRowid NO-LOCK NO-ERROR.

  IF AVAIL VPIArtBas THEN
  DO:
    FIND FIRST VPIArtPris OF VPIArtBas NO-LOCK NO-ERROR.
    IF AVAILABLE VPIArtPris THEN 
        FIND FIRST ArtPris NO-LOCK WHERE
            ArtPris.ArtikkelNr = DEC(VPIArtBas.VareNr) AND
            ArtPris.ProfilNr   = VPIArtPris.ProfilNr NO-ERROR.
    IF NOT AVAILABLE ArtPris THEN 
      FIND FIRST ArtPris NO-LOCK WHERE
          ArtPris.ArtikkelNr = DEC(VPIArtBas.VareNr) NO-ERROR.
    ocValue = IF AVAIL ArtPris AND ArtPris.Db%[1] NE ? 
        THEN STRING(ArtPris.Db%[1]) 
        ELSE ''.
  END.

END PROCEDURE.

PROCEDURE AktivPris:
/*------------------------------------------------------------------------------
    Purpose:                                      
    Notes:                                      
------------------------------------------------------------------------------*/
  DEF INPUT  PARAM irRowid      AS ROWID NO-UNDO.
  DEF INPUT  PARAM icParam      AS CHAR  NO-UNDO. 
  DEF INPUT  PARAM icSessionId  AS CHAR  NO-UNDO.
  DEF OUTPUT PARAM ocValue      AS CHAR  NO-UNDO.

  FIND VPIArtBas WHERE ROWID(VPIArtBas) = irRowid NO-LOCK NO-ERROR.

  IF AVAIL VPIArtBas THEN
  DO:
    FIND FIRST VPIArtPris OF VPIArtBas NO-LOCK NO-ERROR.
    IF AVAILABLE VPIArtPris THEN 
        FIND FIRST ArtPris NO-LOCK WHERE
            ArtPris.ArtikkelNr = DEC(VPIArtBas.VareNr) AND
            ArtPris.ProfilNr   = VPIArtPris.ProfilNr NO-ERROR.
    IF NOT AVAILABLE ArtPris THEN 
      FIND FIRST ArtPris NO-LOCK WHERE
          ArtPris.ArtikkelNr = DEC(VPIArtBas.VareNr) NO-ERROR.
    ocValue = IF AVAIL ArtPris AND ArtPris.Pris[1] NE ? 
        THEN STRING(ArtPris.Pris[1]) 
        ELSE ''.
  END.

END PROCEDURE.

PROCEDURE chkArtReg:
  DEF INPUT  PARAM irRowid      AS ROWID NO-UNDO.
  DEF INPUT  PARAM icParam      AS CHAR  NO-UNDO. 
  DEF INPUT  PARAM icSessionId  AS CHAR  NO-UNDO.
  DEF OUTPUT PARAM ocValue      AS CHAR  NO-UNDO.
  
  DEF VAR bArtReg    AS LOG NO-UNDO.
  DEF VAR bNotFound  AS LOG NO-UNDO.

  ASSIGN 
    bNotFound = FALSE
    bArtReg   = LOGICAL(ENTRY(1,icParam,'¤'))
  .

  FIND VPIArtBas WHERE ROWID(VPIArtBas) = irRowid NO-LOCK NO-ERROR.
  IF AVAIL VPIArtBas THEN 
  DO:    
    ASSIGN 
      bNotFound = NOT CAN-FIND(FIRST artbas WHERE ArtBas.artikkelnr = vpiartbas.artikkelnr)
      ocValue   = STRING(bNotFound)
      ocValue   = IF bArtReg AND NOT bNotFound THEN 'SKIPROW' ELSE ocValue 
    .
  END.
  
END PROCEDURE.

PROCEDURE chkVPIinfo:
  DEF INPUT  PARAM irRowid      AS ROWID NO-UNDO.
  DEF INPUT  PARAM icParam      AS CHAR  NO-UNDO. 
  DEF INPUT  PARAM icSessionId  AS CHAR  NO-UNDO.
  DEF OUTPUT PARAM ocValue      AS CHAR  NO-UNDO.

  DEF VAR cSjekkFieldsVPI AS CHAR   NO-UNDO.
  DEF VAR cSjekkFieldsA AS CHAR   NO-UNDO.
  DEF VAR bhVPI           AS HANDLE NO-UNDO.
  DEF VAR bhArtBas  AS HANDLE NO-UNDO.
  DEF VAR iCounter        AS INT    NO-UNDO.
  DEF VAR fVBL            AS DEC    NO-UNDO.
  DEF VAR fVPI            AS DEC    NO-UNDO.
  DEF VAR bPrisDiff       AS LOG    NO-UNDO.
  DEF VAR bChkPris        AS LOG    NO-UNDO.
  DEF VAR ii              AS INT    NO-UNDO.

  ASSIGN 
    bPrisDiff       = FALSE
    bChkPris        = LOGICAL(ENTRY(1,icParam,'¤'))
    cSjekkFieldsVPI = 'anbefaltpris,forhRab%,KatalogPris,suppRab%,ForhRab%,LevDato1,LevDato2,LevDato3,LevDato4'
    cSjekkFieldsA   = 'anbefaltpris,forhRab%,KatalogPris,supRab%,ForhRab%,LevDato1,LevDato2,LevDato3,LevDato4'
  .
  FIND VPIArtBas WHERE ROWID(VPIArtBas) = irRowid NO-LOCK NO-ERROR.
  IF AVAIL VPIArtBas THEN 
  DO:
    FIND ArtBas WHERE ArtBas.ArtikkelNr = VPIArtBas.Artikkelnr NO-LOCK NO-ERROR.
    IF AVAIL ArtBas THEN 
    DO:
      bhVPI    = BUFFER VPIArtBAS:HANDLE.
      bhArtBas = BUFFER ArtBas:HANDLE.
      DO iCounter = 1 TO NUM-ENTRIES(cSjekkFieldsVPI):
        ASSIGN 
          fVPI = IF bhVPI:BUFFER-FIELD(ENTRY(iCounter,cSjekkFieldsVPI)):EXTENT GT 0 THEN
                    bhVPI:BUFFER-FIELD(ENTRY(iCounter,cSjekkFieldsVPI)):BUFFER-VALUE(1)
                 ELSE
                   bhVPI:BUFFER-FIELD(ENTRY(iCounter,cSjekkFieldsVPI)):BUFFER-VALUE

          fVBL = IF bhArtBas:BUFFER-FIELD(ENTRY(iCounter,cSjekkFieldsA)):EXTENT GT 0 THEN 
                    bhArtBas:BUFFER-FIELD(ENTRY(iCounter,cSjekkFieldsA)):BUFFER-VALUE(1)
                 ELSE
                   bhArtBas:BUFFER-FIELD(ENTRY(iCounter,cSjekkFieldsA)):BUFFER-VALUE
        NO-ERROR.
        IF ERROR-STATUS:ERROR THEN 
          MESSAGE PROGRAM-NAME(1) ERROR-STATUS:GET-MESSAGE(1)
            bhVPI:BUFFER-FIELD(ENTRY(iCounter,cSjekkFieldsVPI)):EXTENT SKIP 
            bhArtBas:BUFFER-FIELD(ENTRY(iCounter,cSjekkFieldsA)):EXTENT  
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
        IF NOT bPrisDiff THEN bPrisDiff = fVBL NE fVPI. 
      END.
    END.
    ASSIGN 
      ocValue = STRING(bPrisDiff)
      ocValue = IF bChkPris AND NOT bPrisDiff THEN 'SKIPROW' ELSE ocValue
    .
  END.
  
END PROCEDURE.

PROCEDURE ArtBasRegistrertDato:
  DEF INPUT  PARAM irRowid      AS ROWID NO-UNDO.
  DEF INPUT  PARAM icParam      AS CHAR  NO-UNDO. 
  DEF INPUT  PARAM icSessionId  AS CHAR  NO-UNDO.
  DEF OUTPUT PARAM ocValue      AS CHAR  NO-UNDO.
  
  DEF VAR dDato AS DATE NO-UNDO.
  DEF VAR dFra  AS DATE NO-UNDO.
  DEF VAR dTil  AS DATE NO-UNDO.

  IF icParam NE "¤" THEN
    ASSIGN dFra = DATE(ENTRY(1,icParam,"¤"))
           dTil = DATE(ENTRY(2,icParam,"¤"))
           .
  FIND VPIArtBas WHERE ROWID(VPIArtBas) = irRowid NO-LOCK NO-ERROR.
  IF AVAIL VPIArtBas THEN 
  DO:
    FIND ArtBas WHERE ArtBas.ArtikkelNr = VPIArtBas.Artikkelnr NO-LOCK NO-ERROR.
    IF AVAIL ArtBas THEN 
    DO:
      ASSIGN 
        dDato   = ArtBas.RegistrertDato.
        ocValue = (IF dDato = ? THEN '' ELSE STRING(dDato))
        .
      IF dFra <> ? AND dTil <> ? THEN DO:
          IF NOT (ArtBas.RegistrertDato >= dFra AND
                  ArtBas.RegistrertDato <= dTil) THEN
              ocValue = 'SKIPROW'.
      END.
      ELSE IF dFra <> ? AND dTil = ? THEN DO:
          IF NOT (ArtBas.RegistrertDato >= dFra) THEN
              ocValue = 'SKIPROW'.
      END.
      ELSE IF dTil <> ? AND dFra = ? THEN DO:
          IF NOT (ArtBas.RegistrertDato <= dTil) THEN
              ocValue = 'SKIPROW'.
      END.
    END.
  END.
END PROCEDURE.

PROCEDURE StrekkodeListe:
  DEF INPUT  PARAM irRowid      AS ROWID NO-UNDO.
  DEF INPUT  PARAM icParam      AS CHAR  NO-UNDO. 
  DEF INPUT  PARAM icSessionId  AS CHAR  NO-UNDO.
  DEF OUTPUT PARAM ocValue      AS CHAR  NO-UNDO.
  
  DEFINE VARIABLE cStrekkoder AS CHARACTER NO-UNDO.

  FIND VPIArtBas WHERE ROWID(VPIArtBas) = irRowid NO-LOCK NO-ERROR.
  IF AVAIL VPIArtBas THEN 
  DO:
    FIND LAST VPIStrekkode OF VPIArtBas NO-LOCK NO-ERROR.
    IF AVAILABLE VPIStrekkode THEN 
        ocValue = VPIStrekkode.Kode.
    /*
    FOR EACH VPIStrekkode OF VPIArtBas NO-LOCK:
        IF VPIStrekkode.Kode <> '' THEN 
            ocValue = ocValue + (IF ocValue <> '' THEN ',' ELSE '') + VPIStrekkode.Kode.
    END.
    */    
  END.
END PROCEDURE.

PROCEDURE AktivStrekkodeListe:
  DEF INPUT  PARAM irRowid      AS ROWID NO-UNDO.
  DEF INPUT  PARAM icParam      AS CHAR  NO-UNDO. 
  DEF INPUT  PARAM icSessionId  AS CHAR  NO-UNDO.
  DEF OUTPUT PARAM ocValue      AS CHAR  NO-UNDO.
  
  DEFINE VARIABLE cStrekkoder AS CHARACTER NO-UNDO.

  FIND VPIArtBas WHERE ROWID(VPIArtBas) = irRowid NO-LOCK NO-ERROR.
  IF AVAIL VPIArtBas THEN 
  DO:
    FIND LAST VPIStrekkode OF VPIArtBas NO-LOCK NO-ERROR.
    IF AVAILABLE VPIStrekkode THEN 
    DO:
        FIND Strekkode NO-LOCK WHERE
            Strekkode.Kode = VPIStrekkode.Kode NO-ERROR.
        IF AVAILABLE Strekkode THEN 
            ocValue = Strekkode.Kode.
    END.
    ELSE DO:
        FIND LAST Strekkode NO-LOCK WHERE
            Strekkode.ArtikkelNr = DEC(VPIArtBas.VareNr) NO-ERROR.
        IF AVAILABLE Strekkode THEN 
            ocValue = Strekkode.Kode.
    END.
  END.
END PROCEDURE.

PROCEDURE Bestillingsnr:
  DEF INPUT  PARAM irRowid      AS ROWID NO-UNDO.
  DEF INPUT  PARAM icParam      AS CHAR  NO-UNDO. 
  DEF INPUT  PARAM icSessionId  AS CHAR  NO-UNDO.
  DEF OUTPUT PARAM ocValue      AS CHAR  NO-UNDO.
  
  DEFINE VARIABLE cStrekkoder AS CHARACTER NO-UNDO.

  FIND VPIArtBas WHERE ROWID(VPIArtBas) = irRowid NO-LOCK NO-ERROR.
  IF AVAIL VPIArtBas THEN 
  DO:
    FIND LAST VPIStrekkode OF VPIArtBas NO-LOCK NO-ERROR.
    IF AVAILABLE VPIStrekkode THEN 
        ocValue = VPIStrekkode.Bestillingsnummer.
  END.
END PROCEDURE.

PROCEDURE AktivBestillingsnr:
  DEF INPUT  PARAM irRowid      AS ROWID NO-UNDO.
  DEF INPUT  PARAM icParam      AS CHAR  NO-UNDO. 
  DEF INPUT  PARAM icSessionId  AS CHAR  NO-UNDO.
  DEF OUTPUT PARAM ocValue      AS CHAR  NO-UNDO.
  
  DEFINE VARIABLE cStrekkoder AS CHARACTER NO-UNDO.

  FIND VPIArtBas WHERE ROWID(VPIArtBas) = irRowid NO-LOCK NO-ERROR.
  IF AVAIL VPIArtBas THEN 
  DO:
    FIND LAST VPIStrekkode OF VPIArtBas NO-LOCK NO-ERROR.
    IF AVAILABLE VPIStrekkode THEN 
    DO:
        FIND Strekkode NO-LOCK WHERE
            Strekkode.Kode = VPIStrekkode.Kode NO-ERROR.
        IF AVAILABLE Strekkode THEN 
            ocValue = Strekkode.Bestillingsnummer.
    END.
    ELSE DO:
        FIND LAST Strekkode NO-LOCK WHERE
            Strekkode.ArtikkelNr = DEC(VPIArtBas.VareNr) NO-ERROR.
        IF AVAILABLE Strekkode THEN 
            ocValue = Strekkode.Bestillingsnummer.
    END.
  END.
END PROCEDURE.

PROCEDURE AktivLevKod:
  DEF INPUT  PARAM irRowid      AS ROWID NO-UNDO.
  DEF INPUT  PARAM icParam      AS CHAR  NO-UNDO. 
  DEF INPUT  PARAM icSessionId  AS CHAR  NO-UNDO.
  DEF OUTPUT PARAM ocValue      AS CHAR  NO-UNDO.
  
  DEFINE VARIABLE cStrekkoder AS CHARACTER NO-UNDO.

  FIND VPIArtBas WHERE ROWID(VPIArtBas) = irRowid NO-LOCK NO-ERROR.
  IF AVAIL VPIArtBas THEN 
  DO:
    FIND ArtBas NO-LOCK WHERE ArtBas.ArtikkelNr = DEC(VPIArtBas.VareNr) NO-ERROR.
    IF AVAILABLE ArtBas THEN 
        ocValue = ArtBas.LevKod.
  END.
END PROCEDURE.

PROCEDURE AktivBeskr:
  DEF INPUT  PARAM irRowid      AS ROWID NO-UNDO.
  DEF INPUT  PARAM icParam      AS CHAR  NO-UNDO. 
  DEF INPUT  PARAM icSessionId  AS CHAR  NO-UNDO.
  DEF OUTPUT PARAM ocValue      AS CHAR  NO-UNDO.
  
  DEFINE VARIABLE cStrekkoder AS CHARACTER NO-UNDO.

  FIND VPIArtBas WHERE ROWID(VPIArtBas) = irRowid NO-LOCK NO-ERROR.
  IF AVAIL VPIArtBas THEN 
  DO:
    FIND ArtBas NO-LOCK WHERE ArtBas.ArtikkelNr = DEC(VPIArtBas.VareNr) NO-ERROR.
    IF AVAILABLE ArtBas THEN 
        ocValue = ArtBas.Beskr.
  END.
END PROCEDURE.

PROCEDURE AktivLevFargKod:
  DEF INPUT  PARAM irRowid      AS ROWID NO-UNDO.
  DEF INPUT  PARAM icParam      AS CHAR  NO-UNDO. 
  DEF INPUT  PARAM icSessionId  AS CHAR  NO-UNDO.
  DEF OUTPUT PARAM ocValue      AS CHAR  NO-UNDO.
  
  DEFINE VARIABLE cStrekkoder AS CHARACTER NO-UNDO.

  FIND VPIArtBas WHERE ROWID(VPIArtBas) = irRowid NO-LOCK NO-ERROR.
  IF AVAIL VPIArtBas THEN 
  DO:
    FIND ArtBas NO-LOCK WHERE ArtBas.ArtikkelNr = DEC(VPIArtBas.VareNr) NO-ERROR.
    IF AVAILABLE ArtBas THEN 
        ocValue = ArtBas.LevFargKod.
  END.
END PROCEDURE.

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
/*     cSjekkFieldsVPI = 'anbefaltpris,forhRab%,KatalogPris,suppRab%' */
/*     cSjekkFieldsA   = 'anbefaltpris,forhRab%,KatalogPris,supRab%'  */
    cSjekkFieldsVPI = 'InnkjopsPris,Rab1%,VareKost,DB%,Mva%,Pris'
    cSjekkFieldsA   = 'InnkjopsPris,Rab1%,VareKost,DB%,Mva%,Pris'
  .
  FIND VPIArtBas WHERE ROWID(VPIArtBas) = irRowid NO-LOCK NO-ERROR.
  FIND VPIArtPris OF VPIArtbas NO-LOCK NO-ERROR.
  IF AVAIL VPIArtBas AND AVAILABLE VPIArtPris THEN 
  DO:
    FIND ArtBas WHERE ArtBas.ArtikkelNr = VPIArtBas.Artikkelnr NO-LOCK NO-ERROR.
    FIND ArtPris OF ArtBas NO-LOCK WHERE
        ArtPris.ProfilNr = VPIArtPris.ProfilNr NO-ERROR.
    IF NOT AVAILABLE ArtPris THEN
        FIND FIRST ArtPris OF ArtBas NO-LOCK NO-ERROR.
    IF AVAIL ArtBas AND AVAILABLE ArtPris THEN
    DO:
      bhVPI    = BUFFER VPIArtPris:HANDLE.
      bhArtBas = BUFFER ArtPris:HANDLE.
      DO iCounter = 1 TO NUM-ENTRIES(cSjekkFieldsVPI):
        ASSIGN 
          fVPI = bhVPI:BUFFER-FIELD(ENTRY(iCounter,cSjekkFieldsVPI)):BUFFER-VALUE(1)
          fVBL = bhArtBas:BUFFER-FIELD(ENTRY(iCounter,cSjekkFieldsA)):BUFFER-VALUE(1)
        NO-ERROR.
        IF ERROR-STATUS:ERROR THEN 
           MESSAGE PROGRAM-NAME(1) ERROR-STATUS:GET-MESSAGE(1)
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

  FIND VPIArtBas WHERE ROWID(VPIArtBas) = irRowid NO-LOCK NO-ERROR.
  IF AVAIL VPIArtBas THEN 
  DO:
    FIND FIRST ArtBas WHERE ArtBas.ArtikkelNr = VPIArtBas.ArtikkelNr NO-LOCK NO-ERROR.
    IF AVAIL ArtBas THEN
    DO:
      bhVPI    = BUFFER VPIArtBAS:HANDLE.
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
  FIND VPIArtBas WHERE ROWID(VPIArtBas) = irRowid NO-LOCK NO-ERROR.
  IF AVAIL VPIArtBas THEN 
  DO:                    
    FOR EACH VPIStrekkode 
      WHERE VPIStrekkode.EkstVPILevNr = VPIArtBas.EkstVPILevNr 
        AND VPIStrekkode.VareNr       = VPIArtBas.Varenr
      NO-LOCK:
      IF NOT bNotFound THEN 
        bNotFound = NOT CAN-FIND(FIRST Strekkode 
                             WHERE Strekkode.artikkelnr = DEC(VPIStrekkode.varenr) 
                               AND Strekkode.strkode    = VPIStrekkode.strkode
                               AND Strekkode.kode       = VPIStrekkode.kode).
      IF bNotFound THEN LEAVE.
    END.
    ASSIGN 
      ocValue = STRING(bNotFound).
      ocValue = IF bStrek AND NOT bNotFound THEN 'SKIPROW' ELSE STRING(bNotFound).
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

PROCEDURE chkArtStrek:
  DEF INPUT  PARAM irRowid      AS ROWID NO-UNDO.
  DEF INPUT  PARAM icParam      AS CHAR  NO-UNDO. 
  DEF INPUT  PARAM icSessionId  AS CHAR  NO-UNDO.
  DEF OUTPUT PARAM ocValue      AS CHAR  NO-UNDO.

  DEF VAR bFound          AS LOG    NO-UNDO.
  DEF VAR bStrek          AS LOG    NO-UNDO.
  
  ASSIGN 
    bFound = FALSE
  .

  FIND VPIStrekkode WHERE ROWID(VPIStrekkode) = irRowid NO-LOCK NO-ERROR.
  IF AVAIL VPIStrekkode THEN 
  DO:
    ASSIGN 
      bFound = CAN-FIND(FIRST Strekkode 
                          WHERE Strekkode.artikkelnr = DEC(VPIStrekkode.varenr) 
                            AND Strekkode.strkode    = VPIStrekkode.strkode
                            AND Strekkode.kode       = VPIStrekkode.kode).
      ocValue = STRING(bFound).
      ocValue = IF bFound THEN 'SKIPROW' ELSE STRING(bFound).
  END.

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
                  Strekkode.ArtikkelNr = DEC(VPIArtBas.VareNr) AND
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
                  AND bVPIArtBas.LevKod      NE VPIArtBas.LevKod
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
                   AND bVPIArtBas.Beskr       NE VPIArtBas.Beskr
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

PROCEDURE Innkjopspris#1:
  /*KundeRabatt;AnbefaltPris;KatalogPris;suppRab%;forhRab%*/
  DEF INPUT  PARAM irRowid      AS ROWID NO-UNDO.
  DEF INPUT  PARAM icSessionId  AS CHAR  NO-UNDO.
  DEF OUTPUT PARAM ocValue      AS CHAR  NO-UNDO.
  
  FIND VPIArtBas WHERE ROWID(VPIArtBas) = irRowid NO-LOCK NO-ERROR.

  IF AVAIL VPIArtBas THEN
  DO:
    /*FIND FIRST ArtPris WHERE ArtPris.Artikkelnr = VPIartbas.artikkelnr NO-LOCK NO-ERROR.*/
    FIND FIRST VPIArtPris NO-LOCK WHERE
        VPIArtPris.EkstVPILevNr = VPIArtBas.EkstVPILevNr AND
        VPIArtPris.VareNr       = VPIArtBas.VareNr AND
        VPIArtPRis.ProfilNr     > 0 NO-ERROR.
    ocValue = IF AVAIL vpiartpris AND vpiartpris.innkjopspris[1] NE ? THEN STRING(vpiartpris.Innkjopspris[1]) ELSE ''.
  END.

END PROCEDURE.

PROCEDURE Rab1%#1:
  /*KundeRabatt;AnbefaltPris;KatalogPris;suppRab%;forhRab%*/
  DEF INPUT  PARAM irRowid      AS ROWID NO-UNDO.
  DEF INPUT  PARAM icSessionId  AS CHAR  NO-UNDO.
  DEF OUTPUT PARAM ocValue      AS CHAR  NO-UNDO.
  
  FIND VPIArtBas WHERE ROWID(VPIArtBas) = irRowid NO-LOCK NO-ERROR.
  IF AVAIL VPIArtBas THEN
  DO:
/*     FIND FIRST ArtPris WHERE ArtPris.Artikkelnr = VPIartbas.artikkelnr NO-LOCK NO-ERROR. */
      FIND FIRST VPIArtPris NO-LOCK WHERE
          VPIArtPris.EkstVPILevNr = VPIArtBas.EkstVPILevNr AND
          VPIArtPris.VareNr       = VPIArtBas.VareNr AND
          VPIArtPRis.ProfilNr     > 0 NO-ERROR.
    ocValue = IF AVAIL VPIArtpris AND VPIArtpris.rab1%[1] NE ? THEN STRING(VPIArtpris.Rab1%[1]) ELSE ''.
  END.

END PROCEDURE.

PROCEDURE DB%#1:
  /*KundeRabatt;AnbefaltPris;KatalogPris;suppRab%;forhRab%*/
  DEF INPUT  PARAM irRowid      AS ROWID NO-UNDO.
  DEF INPUT  PARAM icSessionId  AS CHAR  NO-UNDO.
  DEF OUTPUT PARAM ocValue      AS CHAR  NO-UNDO.
  
  FIND VPIArtBas WHERE ROWID(VPIArtBas) = irRowid NO-LOCK NO-ERROR.
  IF AVAIL VPIArtBas THEN
  DO:
/*     FIND FIRST ArtPris WHERE ArtPris.Artikkelnr = VPIartbas.artikkelnr NO-LOCK NO-ERROR. */
    FIND FIRST VPIArtPris NO-LOCK WHERE
        VPIArtPris.EkstVPILevNr = VPIArtBas.EkstVPILevNr AND
        VPIArtPris.VareNr       = VPIArtBas.VareNr AND 
        VPIArtPRis.ProfilNr     > 0 NO-ERROR.
    ocValue = IF AVAIL VPIArtpris AND VPIArtpris.DB%[1] NE ?  THEN STRING(VPIartpris.DB%[1]) ELSE ''.
  END.

END PROCEDURE.

PROCEDURE Pris#1:
  /*KundeRabatt;AnbefaltPris;KatalogPris;suppRab%;forhRab%*/
  DEF INPUT  PARAM irRowid      AS ROWID NO-UNDO.
  DEF INPUT  PARAM icSessionId  AS CHAR  NO-UNDO.
  DEF OUTPUT PARAM ocValue      AS CHAR  NO-UNDO.
  
  FIND VPIArtBas WHERE ROWID(VPIArtBas) = irRowid NO-LOCK NO-ERROR.
  IF AVAIL VPIArtBas THEN
  DO:
/*     FIND FIRST ArtPris WHERE ArtPris.Artikkelnr = VPIartbas.artikkelnr NO-LOCK NO-ERROR. */
    FIND FIRST VPIArtPris NO-LOCK WHERE
        VPIArtPris.EkstVPILevNr = VPIArtBas.EkstVPILevNr AND
        VPIArtPris.VareNR       = VPIArtBas.VareNr AND
        VPIArtPRis.ProfilNr     > 0 NO-ERROR.
    ocValue = IF AVAIL VPIArtpris THEN STRING(VPIartpris.Pris[1]) ELSE ''.
  END.

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

PROCEDURE Varekost#1:
  /*KundeRabatt;AnbefaltPris;KatalogPris;suppRab%;forhRab%*/
  DEF INPUT  PARAM irRowid      AS ROWID NO-UNDO.
  DEF INPUT  PARAM icSessionId  AS CHAR  NO-UNDO.
  DEF OUTPUT PARAM ocValue      AS CHAR  NO-UNDO.
  
/*   FIND VPIArtBas WHERE ROWID(VPIArtBas) = irRowid NO-LOCK NO-ERROR. */
/*   IF AVAIL VPIArtBas THEN                                           */
/*     ocValue = STRING(VPIArtBas.forhRab%[1]).                        */

END PROCEDURE.
