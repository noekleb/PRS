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
        ASSIGN fLevVerdi    = fLevVerdi + PkSdlLinje.AntLevert * PkSdlPris.NyVarekost
               fTotLevAvvik = fTotLevAvvik + PkSdlLinje.AntRest
               fTotLevAnt   = fTotLevAnt + PkSdlLinje.AntLevert.
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
  ELSE ocValue = "skiprow". /* "Ingen prisavvik". */

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

    DEFINE VARIABLE cTekst AS CHARACTER NO-UNDO.

    FIND PkSdlHode NO-LOCK
        WHERE ROWID(PkSdlHode) = irPksdlHode
        NO-ERROR.
    IF AVAIL PkSdlHode THEN
    DO:
        IF NUM-ENTRIES(PkSdlHode.MeldingFraLev,CHR(10)) >= 3 THEN 
        DO:
            cTekst = ENTRY(3,PkSdlHode.MeldingFraLev,CHR(10)).
            cTekst = ENTRY(2,cTekst,' ').
        END.
    END. 

    ocValue = cTekst.
    
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
      FOR EACH PkSdlLinje OF PkSdlHode NO-LOCK
          WHERE (IF icButNr <> "" THEN PkSdlLinje.ButikkNr = iButNr ELSE TRUE)
          :
              
        IF NOT CAN-DO(cButLst,STRING(PkSdlLinje.ButikkNr)) THEN
            cButLst = cButLst + string(PkSdlLinje.ButikkNr) + ",".
      END.
    IF icButNr NE "" AND cButLst = "" THEN
      ocValue = "skiprow".
    ELSE
      ocValue = TRIM(cButLst,",").
END PROCEDURE.
 
PROCEDURE pksdl_InnlevDato:
    DEF INPUT  PARAM irPksdlHode  AS ROWID NO-UNDO.
    DEF INPUT  PARAM icSessionId  AS CHAR NO-UNDO.
    DEF OUTPUT PARAM ocValue      AS CHAR NO-UNDO.

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
    
    ocValue = IF dInnlevDato <> ? THEN STRING(dInnlevDato) ELSE ''. 
END PROCEDURE.











