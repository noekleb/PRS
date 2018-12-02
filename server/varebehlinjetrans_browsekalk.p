/* Alle kalkulerte felter i rapporteringsbrowser for varebehlinjetrans */

FUNCTION Inndeling  RETURNS INTEGER () FORWARD.
FUNCTION bInndeling RETURNS INTEGER () FORWARD.  /* Brukes når bufferet er bVarebehLinjeTrans */

DEF BUFFER bVarebehLinjeTrans FOR VarebehLinjeTrans.

PROCEDURE varebehlinjetrans_ordretot:
  /* Dersom artikkelnr er angitt som parameter skal bestillingen summeres pr artikkelnr 
     Dersom artikkelnr_butikk skal det summeres pr artikkel og butikk */
  DEF INPUT  PARAM irVarebehLinjeTrans AS ROWID NO-UNDO.
  DEF INPUT  PARAM icParam             AS CHAR  NO-UNDO.
  DEF INPUT  PARAM icSessionId         AS CHAR  NO-UNDO.
  DEF OUTPUT PARAM ocValue             AS CHAR  NO-UNDO.
    
  DEF VAR iTotAnt AS INT NO-UNDO.

  FIND VarebehLinjeTrans WHERE ROWID(VarebehLinjeTrans) = irVarebehLinjeTrans NO-LOCK NO-ERROR.
  IF AVAIL VarebehLinjeTrans THEN DO:
    FIND VarebehLinje 
         WHERE VarebehLinje.ArtikkelNr = VarebehLinjeTrans.Artikkelnr
           AND VarebehLinje.VarebehNr  = VarebehLinjeTrans.VarebehNr
         NO-LOCK NO-ERROR.
    IF AVAIL VarebehLinje THEN DO:
      IF icParam = "" THEN
        ocValue = STRING((VarebehLinjeTrans.Bestilt1 +
                          VarebehLinjeTrans.Bestilt2 + 
                          VarebehLinjeTrans.Bestilt3 +
                          VarebehLinjeTrans.Bestilt4) * Inndeling() * VarebehLinje.Varekost).
      ELSE DO:
        IF icParam = "artikkelnr" THEN DO:
          FOR EACH bVarebehLinjeTrans NO-LOCK
              WHERE bVarebehLinjeTrans.VarebehNr  = VarebehLinje.VarebehNr
                AND bVarebehLinjeTrans.ArtikkelNr = VarebehLinje.ArtikkelNr
                AND bVarebehLinjeTrans.RegistrertBestilling
                :
            iTotAnt = iTotAnt + (bVarebehLinjeTrans.Bestilt1 +
                                 bVarebehLinjeTrans.Bestilt2 + 
                                 bVarebehLinjeTrans.Bestilt3 +
                                 bVarebehLinjeTrans.Bestilt4) * bInndeling().
          END.
          ocValue = STRING(iTotAnt * BInndeling() * VarebehLinje.Varekost).
        END.
        ELSE IF icParam = "artikkelnr_butikknr" THEN DO:
          FOR EACH bVarebehLinjeTrans NO-LOCK
              WHERE bVarebehLinjeTrans.VarebehNr  = VarebehLinje.VarebehNr
                AND bVarebehLinjeTrans.ArtikkelNr = VarebehLinje.ArtikkelNr
                AND bVarebehLinjeTrans.ButikkNr   = VarebehLinjeTrans.ButikkNr
                AND bVarebehLinjeTrans.RegistrertBestilling
                :
            iTotAnt = iTotAnt + (bVarebehLinjeTrans.Bestilt1 +
                                 bVarebehLinjeTrans.Bestilt2 + 
                                 bVarebehLinjeTrans.Bestilt3 +
                                 bVarebehLinjeTrans.Bestilt4) * bInndeling().
          END.
          ocValue = STRING(iTotAnt * VarebehLinje.Varekost).
        END.
      END.
    END.
  END.
END.

PROCEDURE varebehlinjetrans_distinct:
  /* Dersom artikkelnr er angitt som parameter skal bestillingen summeres pr artikkelnr 
     Dersom artikkelnr_butikk skal det summeres pr artikkel og butikk */
  DEF INPUT  PARAM irVarebehLinjeTrans AS ROWID NO-UNDO.
  DEF INPUT  PARAM icParam             AS CHAR  NO-UNDO.
  DEF INPUT  PARAM icSessionId         AS CHAR  NO-UNDO.
  DEF OUTPUT PARAM ocValue             AS CHAR  NO-UNDO.
    
  IF icParam = "" THEN RETURN.

  FIND VarebehLinjeTrans WHERE ROWID(VarebehLinjeTrans) = irVarebehLinjeTrans NO-LOCK NO-ERROR.

  IF NOT AVAIL VarebehLinjeTrans THEN RETURN.

  IF icParam = "artikkelnr" THEN
    ocValue = STRING(VarebehLinjeTrans.ArtikkelNr).
  ELSE IF icParam = "artikkelnr_butikknr" THEN
    ocValue = STRING(VarebehLinjeTrans.ArtikkelNr) + "_" + STRING(VarebehLinjeTrans.ButikkNr).
END.

PROCEDURE varebehlinjetrans_bestilt1:
  /* Dersom artikkelnr er angitt som parameter skal bestillingen summeres pr artikkelnr 
     Dersom artikkelnr_butikk skal det summeres pr artikkel og butikk */
  DEF INPUT  PARAM irVarebehLinjeTrans AS ROWID NO-UNDO.
  DEF INPUT  PARAM icParam             AS CHAR  NO-UNDO.
  DEF INPUT  PARAM icSessionId         AS CHAR  NO-UNDO.
  DEF OUTPUT PARAM ocValue             AS CHAR  NO-UNDO.
    
  DEF VAR iTotAnt AS INT NO-UNDO.

  FIND VarebehLinjeTrans WHERE ROWID(VarebehLinjeTrans) = irVarebehLinjeTrans NO-LOCK NO-ERROR.
  IF AVAIL VarebehLinjeTrans THEN DO:
    IF icParam = "" THEN
      ocValue = STRING(VarebehLinjeTrans.Bestilt1 * Inndeling()).
    ELSE DO:
      IF icParam = "artikkelnr" THEN DO:
        FOR EACH bVarebehLinjeTrans NO-LOCK
            WHERE bVarebehLinjeTrans.VarebehNr  = VarebehLinjeTrans.VarebehNr
              AND bVarebehLinjeTrans.ArtikkelNr = VarebehLinjeTrans.ArtikkelNr
              AND bVarebehLinjeTrans.RegistrertBestilling
              :
          iTotAnt = iTotAnt + bVarebehLinjeTrans.Bestilt1 * bInndeling().
        END.
        ocValue = STRING(iTotAnt).
      END.
      ELSE IF icParam = "artikkelnr_butikknr" THEN DO:
        FOR EACH bVarebehLinjeTrans NO-LOCK
            WHERE bVarebehLinjeTrans.VarebehNr  = VarebehLinjeTrans.VarebehNr
              AND bVarebehLinjeTrans.ArtikkelNr = VarebehLinjeTrans.ArtikkelNr
              AND bVarebehLinjeTrans.ButikkNr   = VarebehLinjeTrans.ButikkNr
              AND bVarebehLinjeTrans.RegistrertBestilling
              :
          iTotAnt = iTotAnt + bVarebehLinjeTrans.Bestilt1 * bInndeling().
        END.
        ocValue = STRING(iTotAnt).
      END.
    END.
  END.
END.

PROCEDURE varebehlinjetrans_bestilt2:
  /* Dersom artikkelnr er angitt som parameter skal bestillingen summeres pr artikkelnr 
     Dersom artikkelnr_butikk skal det summeres pr artikkel og butikk */
  DEF INPUT  PARAM irVarebehLinjeTrans AS ROWID NO-UNDO.
  DEF INPUT  PARAM icParam             AS CHAR  NO-UNDO.
  DEF INPUT  PARAM icSessionId         AS CHAR  NO-UNDO.
  DEF OUTPUT PARAM ocValue             AS CHAR  NO-UNDO.
    
  DEF VAR iTotAnt AS INT NO-UNDO.

  FIND VarebehLinjeTrans WHERE ROWID(VarebehLinjeTrans) = irVarebehLinjeTrans NO-LOCK NO-ERROR.
  IF AVAIL VarebehLinjeTrans THEN DO:
    IF icParam = "" THEN
      ocValue = STRING(VarebehLinjeTrans.Bestilt2 * Inndeling()).
    ELSE DO:
      IF icParam = "artikkelnr" THEN DO:
        FOR EACH bVarebehLinjeTrans NO-LOCK
            WHERE bVarebehLinjeTrans.VarebehNr  = VarebehLinjeTrans.VarebehNr
              AND bVarebehLinjeTrans.ArtikkelNr = VarebehLinjeTrans.ArtikkelNr
              AND bVarebehLinjeTrans.RegistrertBestilling
              :
          iTotAnt = iTotAnt + bVarebehLinjeTrans.Bestilt2 * bInndeling().
        END.
        ocValue = STRING(iTotAnt).
      END.
      ELSE IF icParam = "artikkelnr_butikknr" THEN DO:
        FOR EACH bVarebehLinjeTrans NO-LOCK
            WHERE bVarebehLinjeTrans.VarebehNr  = VarebehLinjeTrans.VarebehNr
              AND bVarebehLinjeTrans.ArtikkelNr = VarebehLinjeTrans.ArtikkelNr
              AND bVarebehLinjeTrans.ButikkNr   = VarebehLinjeTrans.ButikkNr
              AND bVarebehLinjeTrans.RegistrertBestilling
              :
          iTotAnt = iTotAnt + bVarebehLinjeTrans.Bestilt2 * bInndeling().
        END.
        ocValue = STRING(iTotAnt).
      END.
    END.
  END.
END.

PROCEDURE varebehlinjetrans_bestilt3:
  /* Dersom artikkelnr er angitt som parameter skal bestillingen summeres pr artikkelnr 
     Dersom artikkelnr_butikk skal det summeres pr artikkel og butikk */
  DEF INPUT  PARAM irVarebehLinjeTrans AS ROWID NO-UNDO.
  DEF INPUT  PARAM icParam             AS CHAR  NO-UNDO.
  DEF INPUT  PARAM icSessionId         AS CHAR  NO-UNDO.
  DEF OUTPUT PARAM ocValue             AS CHAR  NO-UNDO.
    
  DEF VAR iTotAnt AS INT NO-UNDO.

  FIND VarebehLinjeTrans WHERE ROWID(VarebehLinjeTrans) = irVarebehLinjeTrans NO-LOCK NO-ERROR.
  IF AVAIL VarebehLinjeTrans THEN DO:
    IF icParam = "" THEN
      ocValue = STRING(VarebehLinjeTrans.Bestilt3 * Inndeling()).
    ELSE DO:
      IF icParam = "artikkelnr" THEN DO:
        FOR EACH bVarebehLinjeTrans NO-LOCK
            WHERE bVarebehLinjeTrans.VarebehNr  = VarebehLinjeTrans.VarebehNr
              AND bVarebehLinjeTrans.ArtikkelNr = VarebehLinjeTrans.ArtikkelNr
              AND bVarebehLinjeTrans.RegistrertBestilling
              :
          iTotAnt = iTotAnt + bVarebehLinjeTrans.Bestilt3 * bInndeling().
        END.
        ocValue = STRING(iTotAnt).
      END.
      ELSE IF icParam = "artikkelnr_butikknr" THEN DO:
        FOR EACH bVarebehLinjeTrans NO-LOCK
            WHERE bVarebehLinjeTrans.VarebehNr  = VarebehLinjeTrans.VarebehNr
              AND bVarebehLinjeTrans.ArtikkelNr = VarebehLinjeTrans.ArtikkelNr
              AND bVarebehLinjeTrans.ButikkNr   = VarebehLinjeTrans.ButikkNr
              AND bVarebehLinjeTrans.RegistrertBestilling
              :
          iTotAnt = iTotAnt + bVarebehLinjeTrans.Bestilt3 * bInndeling().
        END.
        ocValue = STRING(iTotAnt).
      END.
    END.
  END.
END.

PROCEDURE varebehlinjetrans_bestilt4:
  /* Dersom artikkelnr er angitt som parameter skal bestillingen summeres pr artikkelnr 
     Dersom artikkelnr_butikk skal det summeres pr artikkel og butikk */
  DEF INPUT  PARAM irVarebehLinjeTrans AS ROWID NO-UNDO.
  DEF INPUT  PARAM icParam             AS CHAR  NO-UNDO.
  DEF INPUT  PARAM icSessionId         AS CHAR  NO-UNDO.
  DEF OUTPUT PARAM ocValue             AS CHAR  NO-UNDO.
    
  DEF VAR iTotAnt AS INT NO-UNDO.

  FIND VarebehLinjeTrans WHERE ROWID(VarebehLinjeTrans) = irVarebehLinjeTrans NO-LOCK NO-ERROR.
  IF AVAIL VarebehLinjeTrans THEN DO:
    IF icParam = "" THEN
      ocValue = STRING(VarebehLinjeTrans.Bestilt4 * Inndeling()).
    ELSE DO:
      IF icParam = "artikkelnr" THEN DO:
        FOR EACH bVarebehLinjeTrans NO-LOCK
            WHERE bVarebehLinjeTrans.VarebehNr  = VarebehLinjeTrans.VarebehNr
              AND bVarebehLinjeTrans.ArtikkelNr = VarebehLinjeTrans.ArtikkelNr
              AND bVarebehLinjeTrans.RegistrertBestilling
              :
          iTotAnt = iTotAnt + bVarebehLinjeTrans.Bestilt4 * bInndeling().
        END.
        ocValue = STRING(iTotAnt).
      END.
      ELSE IF icParam = "artikkelnr_butikknr" THEN DO:
        FOR EACH bVarebehLinjeTrans NO-LOCK
            WHERE bVarebehLinjeTrans.VarebehNr  = VarebehLinjeTrans.VarebehNr
              AND bVarebehLinjeTrans.ArtikkelNr = VarebehLinjeTrans.ArtikkelNr
              AND bVarebehLinjeTrans.ButikkNr   = VarebehLinjeTrans.ButikkNr
              AND bVarebehLinjeTrans.RegistrertBestilling
              :
          iTotAnt = iTotAnt + bVarebehLinjeTrans.Bestilt4 * bInndeling().
        END.
        ocValue = STRING(iTotAnt).
      END.
    END.
  END.
END.

PROCEDURE varebehlinjetrans_totbestilt:
  /* Dersom artikkelnr er angitt som parameter skal bestillingen summeres pr artikkelnr 
     Dersom artikkelnr_butikk skal det summeres pr artikkel og butikk */
  DEF INPUT  PARAM irVarebehLinjeTrans AS ROWID NO-UNDO.
  DEF INPUT  PARAM icParam             AS CHAR  NO-UNDO.
  DEF INPUT  PARAM icSessionId         AS CHAR  NO-UNDO.
  DEF OUTPUT PARAM ocValue             AS CHAR  NO-UNDO.
    
  DEF VAR iTotAnt AS INT NO-UNDO.

  FIND VarebehLinjeTrans WHERE ROWID(VarebehLinjeTrans) = irVarebehLinjeTrans NO-LOCK NO-ERROR.
  IF AVAIL VarebehLinjeTrans THEN DO:
    IF icParam = "" THEN
      ocValue = STRING((VarebehLinjeTrans.Bestilt1
                      + VarebehLinjeTrans.Bestilt2
                      + VarebehLinjeTrans.Bestilt3
                      + VarebehLinjeTrans.Bestilt4) * Inndeling()).
    ELSE DO:
      IF icParam = "artikkelnr" THEN DO:
        FOR EACH bVarebehLinjeTrans NO-LOCK
            WHERE bVarebehLinjeTrans.VarebehNr  = VarebehLinjeTrans.VarebehNr
              AND bVarebehLinjeTrans.ArtikkelNr = VarebehLinjeTrans.ArtikkelNr
              AND bVarebehLinjeTrans.RegistrertBestilling
              :
          iTotAnt = iTotAnt + (bVarebehLinjeTrans.Bestilt1
                             + bVarebehLinjeTrans.Bestilt2
                             + bVarebehLinjeTrans.Bestilt3
                             + bVarebehLinjeTrans.Bestilt4) * bInndeling().
        END.
        ocValue = STRING(iTotAnt).
      END.
      ELSE IF icParam = "artikkelnr_butikknr" THEN DO:
        FOR EACH bVarebehLinjeTrans NO-LOCK
            WHERE bVarebehLinjeTrans.VarebehNr  = VarebehLinjeTrans.VarebehNr
              AND bVarebehLinjeTrans.ArtikkelNr = VarebehLinjeTrans.ArtikkelNr
              AND bVarebehLinjeTrans.ButikkNr   = VarebehLinjeTrans.ButikkNr
              AND bVarebehLinjeTrans.RegistrertBestilling
              :
          iTotAnt = iTotAnt + (bVarebehLinjeTrans.Bestilt1
                             + bVarebehLinjeTrans.Bestilt2
                             + bVarebehLinjeTrans.Bestilt3
                             + bVarebehLinjeTrans.Bestilt4) * bInndeling().
        END.
        ocValue = STRING(iTotAnt).
      END.
    END.
  END.
END.

PROCEDURE varebehlinjetrans_bestforslag:
  DEF INPUT PARAM  irVarebehLinjeTrans AS ROWID NO-UNDO.
  DEF INPUT PARAM  icParam             AS CHAR  NO-UNDO.
  DEF INPUT PARAM  icSessionId         AS CHAR  NO-UNDO.
  DEF OUTPUT PARAM ocReturn            AS CHAR  NO-UNDO INIT "no".

  DEF VAR bGodkjent AS LOG NO-UNDO.

  IF icParam NE "" THEN 
    bGodkjent = LOGICAL(icParam).
  ELSE bGodkjent = ?.

  FIND VarebehLinjeTrans WHERE ROWID(VarebehLinjeTrans) = irVarebehLinjeTrans NO-LOCK NO-ERROR.
  IF AVAIL VarebehLinjeTrans THEN DO:
    IF bGodkjent NE ? AND VareBehLinjeTrans.GodkjentBestilling NE bGodkjent THEN 
      ocReturn = "skiprow".
    ELSE
      ocReturn = STRING(VareBehLinjeTrans.GodkjentBestilling).
  END.

END PROCEDURE.

FUNCTION Inndeling RETURNS INTEGER ():

  DEF VAR iAntFord AS INT NO-UNDO.
  
  IF CAN-FIND(FIRST ArtSort
              WHERE ArtSort.ArtikkelNr = VarebehLinjeTrans.ArtikkelNr
                AND ArtSort.SortId     = VarebehLinjeTrans.Kode) THEN DO:
    FOR EACH ArtSort NO-LOCK
        WHERE ArtSort.ArtikkelNr = VarebehLinjeTrans.ArtikkelNr
          AND ArtSort.SortId     = VarebehLinjeTrans.Kode
       ,FIRST LevSort OF ArtSort NO-LOCK
       ,EACH LevSAnt FIELDS(SoAnt) OF LevSort NO-LOCK:
         iAntFord = iAntFord + LevSAnt.SoAnt.
    END.
    RETURN iAntFord.
  END.
  ELSE RETURN 1.

END FUNCTION.

FUNCTION bInndeling RETURNS INTEGER ():

  DEF VAR iAntFord AS INT NO-UNDO.
  
  IF CAN-FIND(FIRST ArtSort
              WHERE ArtSort.ArtikkelNr = bVarebehLinjeTrans.ArtikkelNr
                AND ArtSort.SortId     = bVarebehLinjeTrans.Kode) THEN DO:
    FOR EACH ArtSort NO-LOCK
        WHERE ArtSort.ArtikkelNr = bVarebehLinjeTrans.ArtikkelNr
          AND ArtSort.SortId     = bVarebehLinjeTrans.Kode
       ,FIRST LevSort OF ArtSort NO-LOCK
       ,EACH LevSAnt FIELDS(SoAnt) OF LevSort NO-LOCK:
         iAntFord = iAntFord + LevSAnt.SoAnt.
    END.
    RETURN iAntFord.
  END.
  ELSE RETURN 1.

END FUNCTION.
