/* Inndeling av ordrelinjer i pakke
   Parametere: <KOrdre_id>;<linjenr>;<fordelingstype>;<Pakkepris>;<evt.Pakkeidx>
              Fordelingstype: 1: DB, 2: Pris
   Opprettet: 30.06.06 av BHa                  
-----------------------------------------------------------------------------------*/
DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.

DEF VAR ix           AS INT    NO-UNDO.
DEF VAR iLnr         AS INT    NO-UNDO.

DEF VAR cLnrList     AS CHAR   NO-UNDO.
DEF VAR fKOrdre_id   AS DEC    NO-UNDO.
DEF VAR iFordType    AS INT    NO-UNDO.
DEF VAR fPakkePris   AS DEC    NO-UNDO.
DEF VAR iPakkeidx    AS INT    NO-UNDO.
DEF VAR fTotDb       AS DEC    NO-UNDO.
DEF VAR fTotPris     AS DEC    NO-UNDO.
DEF VAR fAndelRabKr  AS DEC    NO-UNDO.
DEF VAR hBufKOlinje  AS HANDLE NO-UNDO.

hBufKOlinje = BUFFER KOrdreLinje:HANDLE.

ASSIGN fKOrdre_id = DEC(ENTRY(1,icParam,";"))
       cLnrList   = REPLACE(ENTRY(2,icParam,";"),"|",",")
       iFordType  = INT(ENTRY(3,icParam,";"))
       fPakkePris = DEC(ENTRY(4,icParam,";"))
       iPakkeidx  = INT(ENTRY(5,icParam,";"))
       .

FIND KOrdreHode EXCLUSIVE-LOCK
     WHERE KOrdreHode.KOrdre_id = fKOrdre_id
     NO-ERROR.

IF AVAIL KOrdreHode THEN DO ON ERROR UNDO, LEAVE:
  /* Fjerner først varer som ikke lenger er med i pakken: */
  IF iPakkeidx NE 0 THEN
    FOR EACH KOrdreLinje OF KOrdreHode EXCLUSIVE-LOCK
        WHERE KOrdreLinje.Pakkeidx = iPakkeidx
          AND NOT CAN-DO(cLnrList,STRING(KOrdreLinje.KOrdreLinjeNr)):
      RUN kordrelinje_set_varenr.p (STRING(ROWID(KOrdreLinje)) + "|" + KOrdreLinje.Varenr,
                                    ?,
                                    icSessionId,
                                    OUTPUT ocReturn,
                                    OUTPUT obOk).
      IF NOT obOk THEN UNDO, LEAVE.      
    END.
  ELSE DO:  /* Finner høyest pakkeindex: */
    FOR EACH KOrdreLinje OF KOrdreHode NO-LOCK:
      iPakkeidx = MAX(iPakkeidx,KOrdreLinje.Pakkeidx).
    END.
    IF iPakkeidx = 0 THEN iPakkeidx = 1.
  END.

  /* Beregner enten sum db eller pris for artikler i pakken: */
  FOR EACH KOrdreLinje OF KOrdreHode EXCLUSIVE-LOCK
      WHERE CAN-DO(cLnrList,STRING(KOrdreLinje.KOrdreLinjeNr)):

    IF KOrdreLinje.VareNr NE "" AND iFordType = 1 THEN DO:
      FIND ArtBas NO-LOCK 
           WHERE ArtBas.ArtikkelNr = DEC(KOrdreLinje.VareNr)
           NO-ERROR.
      IF AVAIL ArtBas THEN DO:
        FIND FIRST PrisProfil NO-LOCK NO-ERROR.
        FIND FIRST ArtPris OF ArtBas NO-LOCK NO-ERROR.
        IF AVAIL ArtPris AND AVAIL PrisProfil THEN DO:
          IF NOT ArtBas.Opris THEN
            ASSIGN KOrdreLinje.DbKr = (KOrdreLinje.BruttoPris - ArtPris.VareKost[Prisprofil.ProfilNr]) * KOrdreLinje.Antall
                   fTotDb = fTotDb + KOrdreLinje.DbKr.
          ELSE
            fTotDb = fTotDb + KOrdreLinje.NettoPris * ArtPris.Db%[Prisprofil.ProfilNr].
        END.
      END.
    END.
    ELSE IF iFordType = 1 THEN 
      fTotDb = fTotDb + KOrdreLinje.NettoLinjesum * 0.35.
      
    fTotPris = fTotPris + KOrdreLinje.BruttoPris * KOrdreLinje.Antall.
  END.

  /* Beregner linjerabatt og setter pakkeindex: */
  FOR EACH KOrdreLinje OF KOrdreHode EXCLUSIVE-LOCK
      WHERE CAN-DO(cLnrList,STRING(KOrdreLinje.KOrdreLinjeNr)):
    KOrdreLinje.Pakkeidx = iPakkeidx.

    IF iFordType = 1 THEN 
     fAndelRabKr = (fTotPris - fPakkePris) * KOrdreLinje.DbKr / fTotDB.
    ELSE 
      fAndelRabKr = (fTotPris - fPakkePris) * KOrdreLinje.BruttoPris * KOrdreLinje.Antall / fTotPris.

    ASSIGN KOrdreLinje.NettoPris = (KOrdreLinje.BruttoPris * KOrdreLinje.Antall - fAndelRabKr) / KOrdreLinje.Antall
           KOrdreLinje.LinjeRab% = (1 - KOrdreLinje.NettoPris / KOrdreLinje.BruttoPris) * 100.
    
    RUN kordrelinje_post_update.p (hBufKOlinje,"","",OUTPUT ocReturn).
  END.

END.
ELSE ocReturn = "Kundeordre ikke tilgjengelig for oppdatering".

obOk = ocReturn = "".
