DEF VAR bPrisAvvik     AS LOG NO-UNDO.
DEF VAR bInnPrisAvvik  AS LOG NO-UNDO.
DEF VAR iCl            AS INT NO-UNDO.
DEF VAR fGjPris        AS DEC NO-UNDO.
DEF VAR fTotLevAvvik   AS DEC NO-UNDO.
DEF VAR fTotLevAnt     AS DEC NO-UNDO.
DEF VAR bSjekkPrisavv  AS LOG NO-UNDO.
DEFINE VARIABLE cTotalt AS CHARACTER NO-UNDO.

{syspara.i 5 1 1 iCl INT}.
FIND Butiker NO-LOCK WHERE
    Butiker.Butik = iCl NO-ERROR.

/* **********************  Internal Procedures  *********************** */

PROCEDURE pksdl_SkipSendtOutlet:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEF INPUT  PARAM irPksdlHode  AS ROWID NO-UNDO.
    DEF INPUT  PARAM icParam      AS CHAR NO-UNDO.
    DEF INPUT  PARAM icSessionId  AS CHAR NO-UNDO.
    DEF OUTPUT PARAM ocValue      AS CHAR NO-UNDO.

    DEFINE VARIABLE cTekst AS CHARACTER NO-UNDO.
    
    icParam = REPLACE(icParam,CHR(1),',').
    
    FIND PkSdlHode NO-LOCK
        WHERE ROWID(PkSdlHode) = irPksdlHode
        NO-ERROR.
    IF AVAIL PkSdlHode AND icParam <> '' THEN
    DO:
      IF CAN-DO(icParam,STRING(PkSdlHode.SendtOutlet)) THEN 
        ocValue = ''.   
      ELSE 
        ocValue = "SKIPROW".   
    END.
    ELSE 
      ocValue = ''.
END PROCEDURE.

PROCEDURE pksdl_SkipFraButikk: 
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEF INPUT  PARAM irPksdlHode  AS ROWID NO-UNDO.
    DEF INPUT  PARAM icParam      AS CHAR NO-UNDO.
    DEF INPUT  PARAM icSessionId  AS CHAR NO-UNDO.
    DEF OUTPUT PARAM ocValue      AS CHAR NO-UNDO.

    DEF VAR cTekst AS CHAR NO-UNDO.
    DEF VAR cRecord AS CHAR NO-UNDO.
    DEF VAR iLoop AS INT NO-UNDO.
    
    icParam = REPLACE(icParam,CHR(1),',').
    
    FIND PkSdlHode NO-LOCK
        WHERE ROWID(PkSdlHode) = irPksdlHode
        NO-ERROR.
    IF AVAIL PkSdlHode THEN
    DO:
      cTekst = PkSdlHode.Merknad.
      IF NUM-ENTRIES(PkSdlHode.Merknad,CHR(13)) > 1 THEN
      LOOPEN:
      DO iLoop = 1 TO NUM-ENTRIES(PkSdlHode.Merknad,CHR(13)):
         cRecord = ENTRY(iLoop,PkSdlHode.Merknad,CHR(13)).
         IF cRecord BEGINS 'Overført fra' THEN
          LEAVE Loopen.
      END. /* LOOPEN */
      
      IF cRecord <> '' THEN
      DO:
          cRecord = ENTRY(1,cRecord,'.').
          IF NUM-ENTRIES(cRecord,' ') > 3 THEN
              cRecord = ENTRY(4,cRecord,' ').
          ELSE 
              cRecord = ''.
      END.
    END.
    ELSE cRecord = ''.
    
    IF icParam <> '' THEN 
      DO:
        IF NOT CAN-DO(icParam,cRecord) THEN 
          ocValue = 'SKIPROW'.
        ELSE 
          ocValue = ''.
      END.
    ELSE 
      ocValue = ''.
    
END PROCEDURE.

PROCEDURE pksdl_FraButikk:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEF INPUT  PARAM irPksdlHode  AS ROWID NO-UNDO.
    DEF INPUT  PARAM icParam      AS CHAR NO-UNDO.
    DEF INPUT  PARAM icSessionId  AS CHAR NO-UNDO.
    DEF OUTPUT PARAM ocValue      AS CHAR NO-UNDO.

    DEF VAR cTekst AS CHAR NO-UNDO.
    DEF VAR cRecord AS CHAR NO-UNDO.
    DEF VAR iLoop AS INT NO-UNDO.
    

    FIND PkSdlHode NO-LOCK
        WHERE ROWID(PkSdlHode) = irPksdlHode
        NO-ERROR.
    IF AVAIL PkSdlHode THEN
    DO:
      cTekst = PkSdlHode.Merknad.
      IF NUM-ENTRIES(PkSdlHode.Merknad,CHR(13)) > 1 THEN
      LOOPEN:
      DO iLoop = 1 TO NUM-ENTRIES(PkSdlHode.Merknad,CHR(13)):
         cRecord = ENTRY(iLoop,PkSdlHode.Merknad,CHR(13)).
         IF cRecord BEGINS 'Overført fra' THEN
          LEAVE Loopen.
      END. /* LOOPEN */
      
      IF cRecord <> '' THEN
      DO:
          cRecord = ENTRY(1,cRecord,'.').
          IF NUM-ENTRIES(cRecord,' ') > 3 THEN
              cRecord = ENTRY(4,cRecord,' ').
          ELSE 
              cRecord = ''.
      END.
    END.
    ELSE cRecord = ''.
    
    ocValue = TRIM(cRecord).
    
END PROCEDURE.

PROCEDURE pksdl_LevKod:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEF INPUT  PARAM irPksdlHode  AS ROWID NO-UNDO.
    DEF INPUT  PARAM icButNr      AS CHAR NO-UNDO.
    DEF INPUT  PARAM icSessionId  AS CHAR NO-UNDO.
    DEF OUTPUT PARAM ocValue      AS CHAR NO-UNDO.

    DEFINE VARIABLE cTekst AS CHARACTER NO-UNDO.
    

    FIND PkSdlHode NO-LOCK
        WHERE ROWID(PkSdlHode) = irPksdlHode
        NO-ERROR.
    IF AVAIL PkSdlHode THEN
    DO:
      /* Leser færre linjer her enn når man leser varelinjene. */
      FOR EACH PkSdlPris OF PkSdlHode NO-LOCK:
        FIND ArtBas OF PkSdlPris NO-LOCK NO-ERROR.
        IF NOT AVAILABLE ArtBas THEN 
          NEXT.
        IF AVAILABLE ArtBas THEN 
          ocValue = ocValue + 
                    (IF ocValue <> '' THEN ',' ELSE '') + 
                    (IF NOT CAN-DO(ocValue,STRING(ArtBas.LevKod)) THEN 
                    ArtBas.LevKod ELSE '').      
      END.
    END.
END PROCEDURE.

PROCEDURE pksdl_Merknad:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEF INPUT  PARAM irPksdlHode  AS ROWID NO-UNDO.
    DEF INPUT  PARAM icButNr      AS CHAR NO-UNDO.
    DEF INPUT  PARAM icSessionId  AS CHAR NO-UNDO.
    DEF OUTPUT PARAM ocValue      AS CHAR NO-UNDO.

    DEFINE VARIABLE cTekst AS CHARACTER NO-UNDO.
    

    FIND PkSdlHode NO-LOCK
        WHERE ROWID(PkSdlHode) = irPksdlHode
        NO-ERROR.
    IF AVAIL PkSdlHode THEN
    DO:
      IF NUM-ENTRIES(PkSdlHode.Merknad,CHR(10)) > 1 THEN 
        ocValue = ENTRY(2,PkSdlHode.Merknad,CHR(10)). 
    END.
END PROCEDURE.

PROCEDURE pksdl_LevFargKod:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEF INPUT  PARAM irPksdlHode  AS ROWID NO-UNDO.
    DEF INPUT  PARAM icButNr      AS CHAR NO-UNDO.
    DEF INPUT  PARAM icSessionId  AS CHAR NO-UNDO.
    DEF OUTPUT PARAM ocValue      AS CHAR NO-UNDO.

    DEFINE VARIABLE cTekst AS CHARACTER NO-UNDO.
    

    FIND PkSdlHode NO-LOCK
        WHERE ROWID(PkSdlHode) = irPksdlHode
        NO-ERROR.
    IF AVAIL PkSdlHode THEN
    DO:
      /* Leser færre linjer her enn når man leser varelinjene. */
      FOR EACH PkSdlPris OF PkSdlHode NO-LOCK:
        FIND ArtBas OF PkSdlPris NO-LOCK NO-ERROR.
        IF NOT AVAILABLE ArtBas THEN 
          NEXT.
        IF AVAILABLE ArtBas THEN 
          ocValue = ocValue + 
                    (IF ocValue <> '' THEN ',' ELSE '') + 
                    (IF NOT CAN-DO(ocValue,STRING(ArtBas.LevFargKod)) THEN 
                    ArtBas.LevFargKod ELSE '').      
      END.
    END.
END PROCEDURE.

PROCEDURE pksdl_Storl:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEF INPUT  PARAM irPksdlHode  AS ROWID NO-UNDO.
    DEF INPUT  PARAM icButNr      AS CHAR NO-UNDO.
    DEF INPUT  PARAM icSessionId  AS CHAR NO-UNDO.
    DEF OUTPUT PARAM ocValue      AS CHAR NO-UNDO.

    DEFINE VARIABLE cTekst AS CHARACTER NO-UNDO.
    

    FIND PkSdlHode NO-LOCK
        WHERE ROWID(PkSdlHode) = irPksdlHode
        NO-ERROR.
    IF AVAIL PkSdlHode THEN
    DO:
      /* Leser færre linjer her enn når man leser varelinjene. */
      FOR EACH PkSdlLinje OF PkSdlHode NO-LOCK:
        FIND StrKonv OF PkSdlLinje NO-LOCK NO-ERROR.
        IF NOT AVAILABLE StrKonv OR TRIM(StrKonv.Storl) = '' THEN 
          NEXT.
        ocValue = ocValue + 
                  (IF ocValue <> '' THEN ',' ELSE '') + 
                  (IF NOT CAN-DO(ocValue,TRIM(StrKonv.Storl)) THEN 
                  TRIM(StrKonv.Storl) ELSE '').      
      END.
    END.
END PROCEDURE.

PROCEDURE pksdl_MainGroup:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEF INPUT  PARAM irPksdlHode  AS ROWID NO-UNDO.
    DEF INPUT  PARAM icButNr      AS CHAR NO-UNDO.
    DEF INPUT  PARAM icSessionId  AS CHAR NO-UNDO.
    DEF OUTPUT PARAM ocValue      AS CHAR NO-UNDO.

    DEFINE VARIABLE cTekst AS CHARACTER NO-UNDO.
    

    FIND PkSdlHode NO-LOCK
        WHERE ROWID(PkSdlHode) = irPksdlHode
        NO-ERROR.
    IF AVAIL PkSdlHode THEN
    DO:
      /* Leser færre linjer her enn når man leser varelinjene. */
      FOR EACH PkSdlPris OF PkSdlHode NO-LOCK:
        FIND ArtBas OF PkSdlPris NO-LOCK NO-ERROR.
        IF NOT AVAILABLE ArtBas THEN 
          NEXT.
        FIND Anv-Kod OF ArtBas NO-LOCK NO-ERROR.
        IF AVAILABLE Anv-Kod THEN 
          ocValue = ocValue + 
                    (IF ocValue <> '' THEN ',' ELSE '') + 
                    (IF NOT CAN-DO(ocValue,STRING(Anv-Kod.Anv-Id)) THEN 
                    STRING(Anv-Kod.Anv-Id) + ',' + Anv-Kod.AnvBeskr ELSE '').      
      END.
    END.
END PROCEDURE.

PROCEDURE pksdl_ArtGroup:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEF INPUT  PARAM irPksdlHode  AS ROWID NO-UNDO.
    DEF INPUT  PARAM icButNr      AS CHAR NO-UNDO.
    DEF INPUT  PARAM icSessionId  AS CHAR NO-UNDO.
    DEF OUTPUT PARAM ocValue      AS CHAR NO-UNDO.

    DEFINE VARIABLE cTekst AS CHARACTER NO-UNDO.
    

    FIND PkSdlHode NO-LOCK
        WHERE ROWID(PkSdlHode) = irPksdlHode
        NO-ERROR.
    IF AVAIL PkSdlPris THEN
    DO:
      /* Leser færre linjer her enn når man leser varelinjene. */
      FOR EACH PkSdlPris OF PkSdlHode NO-LOCK:
        FIND ArtBas OF PkSdlPris NO-LOCK NO-ERROR.
        IF NOT AVAILABLE ArtBas THEN 
          NEXT.
        FIND HovedKategori OF ArtBas NO-LOCK NO-ERROR.
        IF AVAILABLE HovedKategori THEN 
          ocValue = ocValue + 
                    (IF ocValue <> '' THEN ',' ELSE '') + 
                    (IF NOT CAN-DO(ocValue,STRING(HovedKategori.HovedKatNr)) THEN  
                    STRING(HovedKategori.HovedKatNr) + ',' + HovedKategori.HovedKatTekst ELSE '').      
      END.
    END.
END PROCEDURE.

PROCEDURE pksdl_ButikkNr:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEF INPUT  PARAM irPksdlHode  AS ROWID NO-UNDO.
    DEF INPUT  PARAM icParam      AS CHAR NO-UNDO.
    DEF INPUT  PARAM icSessionId  AS CHAR NO-UNDO.
    DEF OUTPUT PARAM ocValue      AS CHAR NO-UNDO.

    DEFINE VARIABLE cTekst AS CHARACTER NO-UNDO.
    
    icParam = REPLACE(icParam,CHR(1),',').

    FIND PkSdlHode NO-LOCK
        WHERE ROWID(PkSdlHode) = irPksdlHode
        NO-ERROR.
    IF AVAIL PkSdlHode THEN
    DO:
      FIND FIRST PkSdlLinje OF PkSdlHode NO-LOCK NO-ERROR.
      IF AVAILABLE PkSdlLinje THEN
      DO: 
        IF icParam <> '' THEN 
        DO:
          IF NOT CAN-DO(icParam,STRING(PkSdlLinje.ButikkNr)) THEN
            ocValue = 'SKIPROW'.
          ELSE
            ocValue = STRING(PkSdlLinje.ButikkNr).
        END.
          ELSE
            ocValue = STRING(PkSdlLinje.ButikkNr).
      END.
      ELSE 
/*        ocValue = '0'.*/
        ocValue = 'SKIPROW'.
    END.
END PROCEDURE.

PROCEDURE pksdl_fakturaBelop:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEF INPUT  PARAM irPksdlHode  AS ROWID NO-UNDO.
    DEF INPUT  PARAM icButNr      AS CHAR NO-UNDO.
    DEF INPUT  PARAM icSessionId  AS CHAR NO-UNDO.
    DEF OUTPUT PARAM ocValue      AS CHAR NO-UNDO.

    DEFINE VARIABLE cTekst AS CHARACTER NO-UNDO.

    FIND PkSdlHode NO-LOCK
        WHERE ROWID(PkSdlHode) = irPksdlHode
        NO-ERROR.
    IF AVAIL PkSdlHode THEN
        FIND FakturaHode NO-LOCK WHERE 
            FakturaHode.BilagsType = 1 AND 
            FakturaHode.FakturaNr = PkSdlHode.FakturaNr NO-ERROR.

    ocValue = (IF AVAILABLE FakturaHode THEN STRING(FakturaHode.Totalt) ELSE '').
END PROCEDURE.

PROCEDURE pksdl_levverdi:
  DEF INPUT  PARAM irPksdlHode  AS ROWID NO-UNDO.
  DEF INPUT  PARAM icSessionId  AS CHAR  NO-UNDO.
  DEF OUTPUT PARAM ocValue      AS CHAR  NO-UNDO.
  
  DEF VAR fLevVerdi  AS DEC NO-UNDO.

  ASSIGN bPrisAvvik    = NO
         bInnPrisAvvik = NO
         fTotLevAvvik  = 0
         fTotLevAnt    = 0
         .

  FIND PkSdlHode NO-LOCK
       WHERE ROWID(PkSdlHode) = irPksdlHode
       NO-ERROR.
  IF AVAIL PkSdlHode THEN 
    FOR EACH PkSdlPris OF PkSdlHode NO-LOCK:

      IF AVAILABLE Butiker THEN DO:
        FIND FIRST ArtPris NO-LOCK
             WHERE ArtPris.ArtikkelNr = PkSdlPris.ArtikkelNr  
               AND ArtPris.ProfilNr   = Butiker.ProfilNr 
             NO-ERROR.
        IF AVAILABLE ArtPris THEN DO:
          IF ArtPris.Pris[IF Artpris.Tilbud THEN 2 ELSE 1] NE PkSdlPris.NyPris THEN
            bPrisAvvik = YES.
          IF ArtPris.Varekost[1] NE PkSdlPris.NyVarekost THEN
            bInnPrisAvvik = YES.
        END.
      END.
      FOR EACH PkSdlLinje OF PkSdlPris NO-LOCK:
        ASSIGN fLevVerdi    = fLevVerdi + PkSdlLinje.AntLevert * PkSdlPris.NyInnkjopsPris
               fTotLevAvvik = fTotLevAvvik + PkSdlLinje.AntRest
               fTotLevAnt   = fTotLevAnt + PkSdlLinje.AntLevert.
      END.
    END.
  
  ocValue = STRING(fLevVerdi).
END PROCEDURE.

PROCEDURE pksdl_WholeSaleVerdiURab:
  DEF INPUT  PARAM irPksdlHode  AS ROWID NO-UNDO.
  DEF INPUT  PARAM icSessionId  AS CHAR  NO-UNDO.
  DEF OUTPUT PARAM ocValue      AS CHAR  NO-UNDO.
  
  DEF VAR fLevVerdi  AS DEC NO-UNDO.

  FIND PkSdlHode NO-LOCK
       WHERE ROWID(PkSdlHode) = irPksdlHode
       NO-ERROR.
  IF AVAIL PkSdlHode THEN 
    FOR EACH PkSdlPris OF PkSdlHode NO-LOCK:
      FOR EACH PkSdlLinje OF PkSdlPris NO-LOCK:
        ASSIGN fLevVerdi    = fLevVerdi + PkSdlLinje.AntLevert * PkSdlPris.NyInnkjopsPris.
      END.
    END.
  
  ocValue = STRING(fLevVerdi).
END PROCEDURE.

PROCEDURE pksdl_WholeSaleVerdiMRab:
  DEF INPUT  PARAM irPksdlHode  AS ROWID NO-UNDO.
  DEF INPUT  PARAM icSessionId  AS CHAR  NO-UNDO.
  DEF OUTPUT PARAM ocValue      AS CHAR  NO-UNDO.
  
  DEF VAR fLevVerdi  AS DEC NO-UNDO.

  FIND PkSdlHode NO-LOCK
       WHERE ROWID(PkSdlHode) = irPksdlHode
       NO-ERROR.
  IF AVAIL PkSdlHode THEN 
    FOR EACH PkSdlPris OF PkSdlHode NO-LOCK:
      FOR EACH PkSdlLinje OF PkSdlPris NO-LOCK:
        ASSIGN fLevVerdi    = fLevVerdi + PkSdlLinje.AntLevert * PkSdlPris.NyVarekost.
      END.
    END.
  
  ocValue = STRING(fLevVerdi).
END PROCEDURE.

PROCEDURE pksdl_Rab1:
  DEF INPUT  PARAM irPksdlHode  AS ROWID NO-UNDO.
  DEF INPUT  PARAM icSessionId  AS CHAR  NO-UNDO.
  DEF OUTPUT PARAM ocValue      AS CHAR  NO-UNDO.
  
  FIND PkSdlHode NO-LOCK
       WHERE ROWID(PkSdlHode) = irPksdlHode
       NO-ERROR.
  IF AVAIL PkSdlHode THEN 
    FIND FIRST PkSdlPris OF PkSdlHode NO-LOCK NO-ERROR.
  
  IF AVAILABLE PkSdlPris THEN 
    ocValue = STRING(PkSdlPris.NyRab1%).
  ELSE 
    ocValue = '0'.
END PROCEDURE.

PROCEDURE pksdl_SOTxt:
  DEF INPUT  PARAM irPksdlHode  AS ROWID NO-UNDO.
  DEF INPUT  PARAM icSessionId  AS CHAR  NO-UNDO.
  DEF OUTPUT PARAM ocReturn      AS CHAR  NO-UNDO.
  
  FIND PkSdlHode NO-LOCK
       WHERE ROWID(PkSdlHode) = irPksdlHode
       NO-ERROR.
  IF AVAIL PkSdlHode THEN
  DO:
      CASE PkSdlHode.SendtOutlet:
        WHEN 1 THEN ocReturn = 'Tilgjengelig'.
        WHEN 2 THEN ocReturn = 'Ikke lov å sende'.
        WHEN 3 THEN ocReturn = 'Sendt vestby'.
        WHEN 4 THEN ocReturn = 'Sendt Ålgård'.
        WHEN 5 THEN ocReturn = 'eCom'.
        WHEN 6 THEN ocReturn = '???'.
        OTHERWISE ''.
      END CASE. 
  END.
END PROCEDURE.

PROCEDURE pksdl_prisavvik:
  DEF INPUT  PARAM irPksdlHode  AS ROWID NO-UNDO.
  DEF INPUT  PARAM icParam      AS CHAR  NO-UNDO.
  DEF INPUT  PARAM icSessionId  AS CHAR  NO-UNDO.
  DEF OUTPUT PARAM ocValue      AS CHAR  NO-UNDO.
  
  /* Brukes for Sport1 - varekost, ikke innkj.pris */

  bSjekkPrisavv = LOGICAL(icParam) NO-ERROR.
  IF NOT bSjekkPrisavv THEN RETURN.

  ASSIGN bPrisAvvik    = NO
         bInnPrisAvvik = NO
         .

  FIND PkSdlHode NO-LOCK
       WHERE ROWID(PkSdlHode) = irPksdlHode
       NO-ERROR.
  IF AVAIL PkSdlHode THEN 
    FOR EACH PkSdlPris OF PkSdlHode NO-LOCK:
      IF PkSdlPris.NyVarekost NE PkSdlPris.Varekost THEN
        bInnPrisavvik = YES.

      IF AVAILABLE Butiker THEN DO:
        FIND FIRST ArtPris NO-LOCK
             WHERE ArtPris.ArtikkelNr = PkSdlPris.ArtikkelNr  
               AND ArtPris.ProfilNr   = Butiker.ProfilNr 
             NO-ERROR.
        IF AVAILABLE ArtPris THEN DO:
          IF ArtPris.Pris[IF Artpris.Tilbud THEN 2 ELSE 1] NE PkSdlPris.NyPris THEN
            bPrisAvvik = YES.
/*           IF ArtPris.Varekost[1] NE PkSdlPris.NyVarekost THEN  */
/*             bInnPrisAvvik = YES.                               */
        END.
      END.
    END.
END PROCEDURE.


PROCEDURE pksdl_avvikstekst:
  DEF INPUT  PARAM irPksdlHode  AS ROWID NO-UNDO.
  DEF INPUT  PARAM icSessionId  AS CHAR NO-UNDO.
  DEF OUTPUT PARAM ocValue      AS CHAR NO-UNDO.
  
  IF NOT bSjekkPrisavv THEN RETURN.

  IF bPrisAvvik OR bInnPrisAvvik THEN DO:      
    ocValue = "Prisavvik: ".
    IF bPrisAvvik THEN DO:        
      ocValue = ocValue + "Utpris".
      IF bInnPrisAvvik THEN
        ocValue = ocValue + "/Varekost".
    END.
    ELSE 
      ocValue = ocValue + "Varekost".
  END.
/*  ELSE ocValue = "skiprow". /* "Ingen prisavvik". */*/

END PROCEDURE.

PROCEDURE pksdl_status:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
  DEF INPUT  PARAM irPksdlHode  AS ROWID NO-UNDO.
  DEF INPUT  PARAM icSessionId  AS CHAR NO-UNDO.
  DEF OUTPUT PARAM ocValue      AS CHAR NO-UNDO.

  FIND PkSdlHode NO-LOCK
       WHERE ROWID(PkSdlHode) = irPksdlHode
       NO-ERROR.
  IF AVAIL PkSdlHode THEN
  DO: 
      FIND FIRST SysPara NO-LOCK WHERE 
        SysPara.SysHId = 5 AND 
        SysPara.SysGr = 25 AND 
        SysPara.ParaNr = PkSdlHode.PkSdlStatus NO-ERROR. 
      ocValue = IF AVAILABLE SysPara THEN SysPara.Parameter1 ELSE ''.
  END. 

END PROCEDURE.

PROCEDURE pksdl_totlev_avvik:
  DEF INPUT  PARAM irPksdlHode  AS ROWID NO-UNDO.
  DEF INPUT  PARAM icSessionId  AS CHAR NO-UNDO.
  DEF OUTPUT PARAM ocValue      AS CHAR NO-UNDO.

  ocValue = STRING(fTotLevAvvik). 

END PROCEDURE.


PROCEDURE pksdl_totrest:
  DEF INPUT  PARAM irPksdlHode  AS ROWID NO-UNDO.
  DEF INPUT  PARAM icSessionId  AS CHAR NO-UNDO.
  DEF OUTPUT PARAM ocValue      AS CHAR NO-UNDO.

  DEF VAR fRest  AS DEC NO-UNDO.  

  FIND PkSdlHode NO-LOCK
       WHERE ROWID(PkSdlHode) = irPksdlHode
       NO-ERROR.
  IF AVAIL PkSdlHode THEN 
    FOR EACH PkSdlPris OF PkSdlHode NO-LOCK
       ,EACH PkSdlLinje OF PkSdlPris NO-LOCK:
        fRest = fRest + PkSdlLinje.AntRest.
    END.

  ocValue = STRING(fRest).
END PROCEDURE.

PROCEDURE pksdl_totbest:
  DEF INPUT  PARAM irPksdlHode  AS ROWID NO-UNDO.
  DEF INPUT  PARAM icSessionId  AS CHAR NO-UNDO.
  DEF OUTPUT PARAM ocValue      AS CHAR NO-UNDO.

  DEF VAR fRest  AS DEC NO-UNDO.  

  FIND PkSdlHode NO-LOCK
       WHERE ROWID(PkSdlHode) = irPksdlHode
       NO-ERROR.
  IF AVAIL PkSdlHode THEN 
    FOR EACH PkSdlLinje OF PkSdlHode NO-LOCK:
        fRest = fRest + PkSdlLinje.Antall.
    END.

  ocValue = STRING(fRest).
END PROCEDURE.

PROCEDURE pksdl_OrdreType:
    DEF INPUT  PARAM irPksdlHode  AS ROWID NO-UNDO.
    DEF INPUT  PARAM icButNr      AS CHAR NO-UNDO.
    DEF INPUT  PARAM icSessionId  AS CHAR NO-UNDO.
    DEF OUTPUT PARAM ocValue      AS CHAR NO-UNDO.

    DEFINE VARIABLE cTekst AS CHARACTER NO-UNDO.

    FIND PkSdlHode NO-LOCK
        WHERE ROWID(PkSdlHode) = irPksdlHode
        NO-ERROR.
    IF AVAIL PkSdlHode THEN
    DO:
        IF NUM-ENTRIES(PkSdlHode.MeldingFraLev,CHR(10)) >= 3 THEN 
        DO:
            cTekst = ENTRY(1,PkSdlHode.MeldingFraLev,CHR(10)).
            cTekst = ENTRY(2,cTekst,' ').
        END.
    END. 
    ocValue = cTekst.
    
END PROCEDURE.

PROCEDURE pksdl_OrdreTypeSkip:
  DEF INPUT  PARAM irPkSdlHode AS ROWID NO-UNDO.
  DEF INPUT  PARAM icParam AS CHAR NO-UNDO.
  DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
  DEF OUTPUT PARAM ocReturn AS CHAR NO-UNDO.
  DEFINE VARIABLE cModus AS CHARACTER NO-UNDO.

  DEFINE VARIABLE cTekst AS CHARACTER NO-UNDO.

  ASSIGN 
    cModus = icParam.

  FIND PkSdlHode NO-LOCK
      WHERE ROWID(PkSdlHode) = irPksdlHode
      NO-ERROR.
  IF AVAIL PkSdlHode THEN
  DO:
      IF NUM-ENTRIES(PkSdlHode.MeldingFraLev,CHR(10)) >= 3 THEN 
      DO:
          cTekst = ENTRY(1,PkSdlHode.MeldingFraLev,CHR(10)).
          cTekst = ENTRY(2,cTekst,' ').            
      END.
  END.
  /* Bytte butnr på pakkseddel skal ikke vise ordretype 1 og 12. */
  IF cModus = '10' THEN
  DO:
    IF CAN-DO('1,12',TRIM(cTekst)) THEN
      ocReturn = 'SKIPROW'.
    ELSE
      ocReturn = cTekst.
  END.
END PROCEDURE.

PROCEDURE pksdl_Sesong:
    DEF INPUT  PARAM irPksdlHode  AS ROWID NO-UNDO.
    DEF INPUT  PARAM icButNr      AS CHAR NO-UNDO.
    DEF INPUT  PARAM icSessionId  AS CHAR NO-UNDO.
    DEF OUTPUT PARAM ocValue      AS CHAR NO-UNDO.

    DEFINE VARIABLE cTekst AS CHARACTER NO-UNDO.

    FIND PkSdlHode NO-LOCK
        WHERE ROWID(PkSdlHode) = irPksdlHode
        NO-ERROR.
    IF AVAIL PkSdlHode THEN
    DO:
        IF NUM-ENTRIES(PkSdlHode.MeldingFraLev,CHR(10)) >= 3 THEN 
        DO:
            cTekst = ENTRY(2,PkSdlHode.MeldingFraLev,CHR(10)).
            cTekst = ENTRY(2,cTekst,' ').
        END.
    END. 

    ocValue = cTekst.
    
END PROCEDURE.

PROCEDURE pksdl_LandedCost:
    DEF INPUT  PARAM irPksdlHode  AS ROWID NO-UNDO.
    DEF INPUT  PARAM icButNr      AS CHAR NO-UNDO.
    DEF INPUT  PARAM icSessionId  AS CHAR NO-UNDO.
    DEF OUTPUT PARAM ocValue      AS CHAR NO-UNDO.

    DEFINE VARIABLE lsumLc AS DECIMAL NO-UNDO.

    FIND PkSdlHode NO-LOCK
        WHERE ROWID(PkSdlHode) = irPksdlHode
        NO-ERROR.
    IF AVAIL PkSdlHode THEN
    DO:
      lSumLc = 0.
      FOR EACH PkSdlLinje OF PkSdlHode NO-LOCK:
        FIND ArtBas OF PkSdlLinje NO-LOCK NO-ERROR.
        IF AVAILABLE ArtBas THEN 
          lSumLc = lSumLc + (ArtBas.KjedeInnkPris * PkSdlLinje.AntLevert).
      END.
    END. 

    ocValue = STRING(lSumLc).
    
END PROCEDURE.

PROCEDURE pksdl_butlst:
    DEF INPUT  PARAM irPksdlHode  AS ROWID NO-UNDO.
    DEF INPUT  PARAM icButNr      AS CHAR NO-UNDO.
    DEF INPUT  PARAM icSessionId  AS CHAR NO-UNDO.
    DEF OUTPUT PARAM ocValue      AS CHAR NO-UNDO.

    DEF VAR cButLSt AS CHAR NO-UNDO.
    DEF VAR iButNr  AS INT  NO-UNDO.

    FIND PkSdlHode NO-LOCK
         WHERE ROWID(PkSdlHode) = irPksdlHode
         NO-ERROR.
    IF AVAIL PkSdlHode THEN 
      FOR EACH PkSdlLinje OF PkSdlHode NO-LOCK:
              
        IF NOT CAN-DO(cButLst,STRING(PkSdlLinje.ButikkNr)) THEN
            cButLst = cButLst + string(PkSdlLinje.ButikkNr) + ",".
      END.
    ocValue = TRIM(cButLst,",").
END PROCEDURE.
 
PROCEDURE pksdl_InnlevDato:
  DEF INPUT  PARAM irPkSdlHode AS ROWID NO-UNDO.
  DEF INPUT  PARAM icParam AS CHAR NO-UNDO.
  DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
  DEF OUTPUT PARAM ocReturn AS CHAR NO-UNDO.

    DEF VAR dInnlevDato AS DATE NO-UNDO.  

    FIND PkSdlHode NO-LOCK
        WHERE ROWID(PkSdlHode) = irPksdlHode
        NO-ERROR.
    IF AVAIL PkSdlHode THEN
    DO:
        FIND FIRST PkSdlMottak NO-LOCK WHERE 
            PkSdlMottak.PkSdlId = PkSdlHode.PkSdlId NO-ERROR.
        IF AVAILABLE PkSdlMottak THEN 
            dInnlevDato = PkSdlMottak.MottattDato.
    END. 
    ELSE dInnlevDato = ?.
    
    ocReturn = IF dInnlevDato <> ? THEN STRING(dInnlevDato) ELSE ''. 
END PROCEDURE.


































