/* Utskrift av ordrebekreftelse fra varehåndteringsbok, messeregistrering
   Parametere: Input: Varebehandlingsnr,levnr,<godkjent/''> bare bekreftede / ikke bekreftede
               Output (ocReturn): Filnavn
   Opprettet: 07.02.08 av BHa             
-----------------------------------------------------------------------------------*/
DEF INPUT  PARAM icParam     AS CHAR   NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR   NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR   NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG    NO-UNDO.

DEF VAR cFileName       AS CHAR   NO-UNDO.
DEF VAR iCount          AS INT    NO-UNDO INIT 1.
DEF VAR hQuery          AS HANDLE NO-UNDO.
DEF VAR fGrandTot       AS DEC    NO-UNDO.
DEF VAR tTotMerk        AS DEC    NO-UNDO.
DEF VAR iPrevAvdNr      AS INT    NO-UNDO.
DEF VAR iPrevHg         AS INT    NO-UNDO.
DEF VAR iPrevLevnr      AS INT    NO-UNDO.
DEF VAR cPrevMerk       AS CHAR   NO-UNDO.
DEF VAR iSumAntStr      AS INT    NO-UNDO.
DEF VAR cSortString     AS CHAR   NO-UNDO.
DEF VAR bEAN            AS LOG    NO-UNDO.
DEF VAR cRowIdList      AS CHAR   NO-UNDO.

DEF VAR cStrList        AS CHAR   NO-UNDO.
DEF VAR cMinStr         AS CHAR   NO-UNDO.
DEF VAR cMaxStr         AS CHAR   NO-UNDO.
DEF VAR cSortFordList   AS CHAR   NO-UNDO.
DEF VAR cKodeList       AS CHAR   NO-UNDO.
DEF VAR iAntFord        AS INT    NO-UNDO.
DEF VAR bInndeling      AS LOG    NO-UNDO.
DEF VAR ix              AS INT    NO-UNDO.
DEF VAR cStrekkode      AS CHAR   NO-UNDO.
DEF VAR hStrLib         AS HANDLE NO-UNDO.
DEF VAR cKjedeLevGjFakt AS CHAR   NO-UNDO.
DEF VAR cPrevLevKod     AS CHAR   NO-UNDO.
DEF VAR cPrevArtBeskr   AS CHAR   NO-UNDO.
DEF VAR cPrevFargeTekst AS CHAR   NO-UNDO.
DEF VAR fTotLevKod      AS DEC    NO-UNDO.  


IF NUM-ENTRIES(icParam) > 3 THEN 
  CASE ENTRY(4,icParam):
    WHEN "1" THEN cKjedeLevGjFakt = " AND VareBehLinje.KjedeVare".
    WHEN "2" THEN cKjedeLevGjFakt = " AND VareBehLinje.Gjennomfaktureres".
    WHEN "3" THEN cKjedeLevGjFakt = " AND (VareBehLinje.Gjennomfaktureres OR VareBehLinje.KjedeVare)".
  END CASE.

CREATE QUERY hQuery.
hQuery:SET-BUFFERS(BUFFER VarebehLinje:HANDLE
                  ,BUFFER VareBehLinjeTrans:HANDLE
/*                   ,BUFFER Strekkode:HANDLE */
                  ,BUFFER StrKonv:HANDLE
                  ,BUFFER ArtBas:HANDLE
                  ,BUFFER Butiker:HANDLE
                  ).
hQuery:QUERY-PREPARE("FOR EACH VareBehLinje NO-LOCK " 
                     + " WHERE VareBehLinje.VarebehNr = " + ENTRY(1,icParam)
                     + "   AND VareBehLinje.levnr = " + ENTRY(2,icParam)
                     + cKjedeLevGjFakt
                     + ",EACH VarebehlinjeTrans OF VarebehLinje NO-LOCK"
                     + " WHERE VarebehLinjeTrans.ArtikkelNr > 0"
                     + "   AND RegistrertBestilling"
                     + (IF ENTRY(3,icParam) = "godkjent" THEN
                         " AND GodkjentBestilling"
                        ELSE
                         " AND NOT GodkjentBestilling")
                     + ",FIRST StrKonv OF VareBehLinjeTrans NO-LOCK OUTER-JOIN"
                     + ",FIRST ArtBas OF VarebehLinje NO-LOCK"
                     + ",FIRST Butiker WHERE Butiker.Butik = VareBehLinjeTrans.ButikkNr NO-LOCK"
                     + " BY VarebehLinje.LevKod"
                     ).

hQuery:QUERY-OPEN().
hQuery:GET-FIRST().

IF NOT AVAIL VarebehLinjeTrans THEN DO:
  ocReturn = "Ingen bestillinger funnet".
  DELETE OBJECT hQuery NO-ERROR.
  RETURN.
END.
FIND FIRST LevBas NO-LOCK
     WHERE LevBas.levnr = INT(ENTRY(2,icParam))
           NO-ERROR.
IF NOT AVAIL LevBas THEN DO:
  ocReturn = "Leverandør ikke funnet: " + ENTRY(2,icParam).
  DELETE OBJECT hQuery NO-ERROR.
  RETURN.
END.


cFileName       = SESSION:TEMP-DIR + STRING(VarebehLinjeTrans.VarebehNr) + "_" + ENTRY(2,icParam) + "_" + STRING(YEAR(TODAY)) + STRING(MONTH(TODAY),"99") + STRING(DAY(TODAY)) + STRING(TIME) + ".txt".

OUTPUT TO VALUE(cFileName).

PUT UNFORMATTED "Forecast ~tLevnr: " ENTRY(2,icParam) " " LevBas.levnamn SKIP(1).

PUT UNFORMATTED "Lev Artnr"              "~t"
                "Artikkelnavn"           "~t"
                "Fargetekst"             "~t"
                "Antall "                "~t" 
                SKIP.

REPEAT WHILE NOT hQuery:QUERY-OFF-END:

  IF VarebehLinje.LevKod NE cPrevLevKod AND cPrevArtBeskr NE "" THEN DO:
    PUT UNFORMATTED cPrevLevKod     + "~t"  
                    cPrevArtBeskr   + "~t" 
                    cPrevFargeTekst + "~t" 
                    fTotLevKod SKIP.
    ASSIGN fTotLevKod = 0
           iCount     = iCount + 1.
  END.

  RUN Inndeling.

  ASSIGN iSumAntStr = (VareBehLinjeTrans.Bestilt1 + VareBehLinjeTrans.Bestilt2 + VareBehLinjeTrans.Bestilt3 + VareBehLinjeTrans.Bestilt4) * iAntFord
         fTotLevKod = fTotLevKod + iSumAntStr
         fGrandTot  = fGrandTot + iSumAntStr
         .

  ASSIGN cPrevLevKod     = VareBehLinje.LevKod
         cPrevArtBeskr   = VareBehLinje.Beskr
         cPrevFargeTekst = VareBehLinje.LevFargKod
         .

  hQuery:GET-NEXT().
END.
PUT UNFORMATTED cPrevLevKod     + "~t"  
                cPrevArtBeskr   + "~t" 
                cPrevFargeTekst + "~t" 
                fTotLevKod SKIP.

PUT UNFORMATTED "TOTALT" + FILL("~t",3) 
                fGrandTot SKIP(2).


OUTPUT CLOSE.
DELETE OBJECT hQuery.

IF iCount = 1 THEN
  ocReturn = "Varehåndteringsboken inneholder ingen bestillinger".
ELSE 
  ASSIGN ocReturn = cFileName + "|" + STRING(iCount + 3)
         obOk     = TRUE.


PROCEDURE Inndeling:
  ASSIGN cStrList      = ""
         cMinStr       = ""
         cMaxStr       = ""
         cSortFordList = ""
         cKodeList     = ""
         bInndeling    = FALSE
         iAntFord      = 1
         .
  FOR EACH ArtSort NO-LOCK
      WHERE ArtSort.ArtikkelNr = VarebehLinje.ArtikkelNr
        AND ArtSort.SortId     = VarebehLinjeTrans.Kode
     ,FIRST LevSort OF ArtSort NO-LOCK:

    ASSIGN iAntFord   = 0
           bInndeling = TRUE.
    FOR EACH LevSAnt OF LevSort NO-LOCK BY SeqNr:
      ASSIGN cStrList      = cStrList + TRIM(LevSAnt.SoStorl) + ","
             cSortFordList = cSortFordList + STRING(LevSAnt.SoAnt) + ","
             iAntFord      = iAntFord + LevSAnt.SoAnt
             cMaxStr       = TRIM(LevSAnt.SoStorl)
             .
      FIND FIRST StrKonv NO-LOCK
           WHERE TRIM(StrKonv.Storl) = TRIM(LevSAnt.SoStorl)
           NO-ERROR.
      IF AVAIL StrKonv THEN DO:
        FIND FIRST Strekkode NO-LOCK
             WHERE Strekkode.Artikkelnr = VarebehLinje.ArtikkelNr
               AND Strekkode.StrKode    = StrKonv.StrKode
               AND Strekkode.kode       NE ""
               AND NOT Strekkode.kode   BEGINS "02"  
             NO-ERROR.
        IF AVAIL Strekkode THEN
          cKodeList = cKodeList + Strekkode.kode + ",".
        ELSE cKodeList = cKodeList + ",".
      END.
      ELSE cKodeList = cKodeList + ",".
      IF cMinStr = "" THEN cMinStr = TRIM(LevSAnt.SoStorl).
    END.
    ASSIGN cStrList      = TRIM(cStrList,",")
           cSortFordList = TRIM(cSortFordList,",")
           iCount        = iCount + 1
           .
  END.
END PROCEDURE.
