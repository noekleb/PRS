
/* Utskrift av ordrebekreftelse fra varehåndteringsbok, messeregistrering
   Parametere: Input: Varebehandlingsnr,levnr,<godkjent/''> bare bekreftede / ikke bekreftede
               Output (ocReturn): Filnavn
   Opprettet: 30.01.06 av BHa             
   Endret:    31.01.06 av BHa
              - Tar med strekkoder for å muiggjøre import             
              03.05.07 av BHa
              - Henter StrekKode fra StrekKode tabellen
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

DEF TEMP-TABLE ttTotUke
    FIELD iUkeNr    AS INT
    FIELD fTotUke   AS DEC.

DEF TEMP-TABLE ttTotMerk
    FIELD cMerknad  AS CHAR
    FIELD fTotMerk  AS DEC.

DEF TEMP-TABLE ttTotBut
    FIELD iButNr    AS INT
    FIELD cButNavn  AS CHAR
    FIELD fTotBut   AS DEC.

IF NUM-ENTRIES(icParam) > 3 THEN 
  CASE ENTRY(4,icParam):
    WHEN "1" THEN cKjedeLevGjFakt = " AND VareBehLinje.KjedeVare".
    WHEN "2" THEN cKjedeLevGjFakt = " AND VareBehLinje.Gjennomfaktureres".
    WHEN "3" THEN cKjedeLevGjFakt = " AND (VareBehLinje.Gjennomfaktureres OR VareBehLinje.KjedeVare)".
  END CASE.

RUN strekkode_str_lib.p PERSIST SET hStrLib.

FUNCTION AccumUke  RETURNS LOG () FORWARD.
FUNCTION AccumMerk RETURNS LOG () FORWARD.
FUNCTION AccumBut  RETURNS LOG () FORWARD.

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
/*                      + ",FIRST Strekkode OF VareBehLinjeTrans NO-LOCK OUTER-JOIN" */
/*                      + ",FIRST StrKonv OF Strekkode NO-LOCK OUTER-JOIN" */
                     + ",FIRST StrKonv OF VareBehLinjeTrans NO-LOCK OUTER-JOIN"
                     + ",FIRST ArtBas OF VarebehLinje NO-LOCK"
                     + ",FIRST Butiker WHERE Butiker.Butik = VareBehLinjeTrans.ButikkNr NO-LOCK"
                     + " BY VarebehLinjeTrans.ButikkNr BY VarebehLinje.LevKod"
                     ).

hQuery:QUERY-OPEN().
hQuery:GET-FIRST().

IF NOT AVAIL VarebehLinjeTrans THEN DO:
  ocReturn = "Varebehandlingsbok ikke funnet: " + CHR(10) + hQuery:PREPARE-STRING.
  RETURN.
END.

cFileName       = SESSION:TEMP-DIR + STRING(VarebehLinjeTrans.VarebehNr) + "_" + ENTRY(2,icParam) + "_" + STRING(YEAR(TODAY)) + STRING(MONTH(TODAY),"99") + STRING(DAY(TODAY)) + STRING(TIME) + ".txt".

OUTPUT TO VALUE(cFileName).

PUT UNFORMATTED "Butikknr"               "~t" 
                "Butikknavn"             "~t"
                "Lev Artnr"              "~t"
                "Artikkelnavn"           "~t"
                "Fargetekst"             "~t"         
                "Str"                    "~t"  
                "Nto forh.   Pris"       "~t"   
                "Levuke1  "              "~t"
                "Antall"                 "~t" 
                "Levuke2"                "~t"
                "Antall"                 "~t" 
                "Levuke3"                "~t"
                "Antall"                 "~t" 
                "Levuke4"                "~t"
                "Antall"                 "~t" 
                "Tot.ant"                "~t" 
                "Sum ordre"              "~t" 
                "Endret"                 "~t" 
                "Merknad"                "~t"
                "Strekkode"
                SKIP.

REPEAT WHILE NOT hQuery:QUERY-OFF-END:

  RUN Inndeling.

  ASSIGN iSumAntStr = (VareBehLinjeTrans.Bestilt1 + VareBehLinjeTrans.Bestilt2 + VareBehLinjeTrans.Bestilt3 + VareBehLinjeTrans.Bestilt4) * iAntFord
         fGrandTot  = fGrandTot + VareBehLinje.VareKost * iSumAntStr
         .

  AccumUke().
  AccumMerk().
  AccumBut().

  IF NOT bInndeling THEN DO:
    PUT UNFORMATTED
       Butiker.Butik         "~t" 
       Butiker.ButNamn       "~t"
      (IF VareBehLinje.LevKod NE ? THEN VareBehLinje.LevKod ELSE "") "~t"
      (IF VareBehLinje.Beskr NE ? THEN (IF VareBehLinje.Beskr BEGINS "+" THEN " " ELSE "") + VareBehLinje.Beskr ELSE "") "~t"
      (IF VareBehLinje.LevFargKod NE ? THEN VareBehLinje.LevFargKod ELSE "") "~t "
      (IF StrKonv.Storl NE ? THEN StrKonv.Storl ELSE "") "~t"
/*       (IF StrKonv.Storl NE ? AND TRIM(StrKonv.Storl) NE "1" THEN StrKonv.Storl ELSE "") "~t" */
      (IF VareBehLinje.VareKost NE ? THEN STRING(VareBehLinje.VareKost) ELSE "") "~t"
       STRING(VareBehLinjeTrans.Levuke1) "~t"
       STRING(VareBehLinjeTrans.Bestilt1 * iAntFord) "~t"
       STRING(VareBehLinjeTrans.Levuke2) "~t"
       STRING(VareBehLinjeTrans.Bestilt2 * iAntFord) "~t"
       STRING(VareBehLinjeTrans.Levuke3) "~t"
       STRING(VareBehLinjeTrans.Bestilt3 * iAntFord) "~t"
       STRING(VareBehLinjeTrans.Levuke4) "~t"
       STRING(VareBehLinjeTrans.Bestilt4 * iAntFord) "~t"
       STRING(iSumAntStr) "~t"
       STRING(iSumAntStr * VareBehLinje.VareKost) "~t"
      (IF VarebehLinjeTrans.EDato NE ? THEN STRING(VarebehLinjeTrans.EDato) ELSE "") "~t"
      VarebehLinje.LinjeMerknad "~t"
      DYNAMIC-FUNCTION("getGyldigStrekkode" IN hStrLib,VareBehLinjeTrans.ArtikkelNr,VareBehLinjeTrans.StrKode,"")
/*       (IF AVAIL Strekkode AND Strekkode.kode NE "" AND NOT Strekkode.kode BEGINS "02" THEN Strekkode.kode ELSE "") */
      SKIP.
    iCount = iCount + 1.
  END.
  ELSE DO ix = 1 TO NUM-ENTRIES(cStrList):
    PUT UNFORMATTED
       Butiker.Butik         "~t" 
       Butiker.ButNamn       "~t"
      (IF VareBehLinje.LevKod NE ? THEN VareBehLinje.LevKod ELSE "") "~t"
      (IF VareBehLinje.Beskr NE ? THEN (IF VareBehLinje.Beskr BEGINS "+" THEN " " ELSE "") + VareBehLinje.Beskr ELSE "") "~t"
      (IF VareBehLinje.LevFargKod NE ? THEN VareBehLinje.LevFargKod ELSE "") "~t "
      ENTRY(ix,cStrList) "~t"
      (IF VareBehLinje.VareKost NE ? THEN STRING(VareBehLinje.VareKost) ELSE "") "~t"
      STRING(VareBehLinjeTrans.Levuke1) "~t"
      STRING(VareBehLinjeTrans.Bestilt1 * INT(ENTRY(ix,cSortFordList))) "~t"
      STRING(VareBehLinjeTrans.Levuke2) "~t"
      STRING(VareBehLinjeTrans.Bestilt2 * INT(ENTRY(ix,cSortFordList))) "~t"
      STRING(VareBehLinjeTrans.Levuke3) "~t"
      STRING(VareBehLinjeTrans.Bestilt3 * INT(ENTRY(ix,cSortFordList))) "~t"
      STRING(VareBehLinjeTrans.Levuke4) "~t"
      STRING(VareBehLinjeTrans.Bestilt4 * INT(ENTRY(ix,cSortFordList))) "~t"
      STRING((VareBehLinjeTrans.Bestilt1 + VareBehLinjeTrans.Bestilt2 + VareBehLinjeTrans.Bestilt3 + VareBehLinjeTrans.Bestilt4) * INT(ENTRY(ix,cSortFordList))) "~t"
      STRING((VareBehLinjeTrans.Bestilt1 + VareBehLinjeTrans.Bestilt2 + VareBehLinjeTrans.Bestilt3 + VareBehLinjeTrans.Bestilt4) * INT(ENTRY(ix,cSortFordList)) * VareBehLinje.VareKost) "~t" 
      (IF VarebehLinjeTrans.EDato NE ? THEN STRING(VarebehLinjeTrans.EDato) ELSE "") "~t"
      VarebehLinje.LinjeMerknad "~t"
      ENTRY(ix,cKodeList)
      SKIP.
    iCount = iCount + 1.
  END.

  hQuery:GET-NEXT().
END.

PUT UNFORMATTED "TOTALT" + FILL("~t",16) 
                fGrandTot SKIP(2).


PUT UNFORMATTED "SUM pr uke:" SKIP.
FOR EACH ttTotUke BY ttTotUke.iUkeNr:
  PUT UNFORMATTED "~t" STRING(ttTotUke.iUkeNr) + FILL("~t",5) + STRING(ttTotUke.fTotUke) SKIP.
END.
PUT UNFORMATTED "TOTALT" + FILL("~t",6) 
                fGrandTot SKIP(2).

PUT UNFORMATTED "SUM pr butikk:" SKIP.
FOR EACH ttTotBut BY ttTotBut.cButNavn:
  PUT UNFORMATTED "~t" + STRING(ttTotBut.cButNavn) +  FILL("~t",5) + STRING(ttTotBut.fTotBut) SKIP.
END.
PUT UNFORMATTED "TOTALT" + FILL("~t",6) 
                fGrandTot SKIP(2).

PUT UNFORMATTED "SUM pr merknad:" SKIP.
FOR EACH ttTotMerk BY ttTotMerk.cMerknad:
  PUT UNFORMATTED "~t" + STRING(ttTotMerk.cMerknad) +  FILL("~t",5) + STRING(ttTotMerk.fTotMerk) SKIP.
END.
PUT UNFORMATTED "TOTALT" + FILL("~t",6) 
                fGrandTot SKIP(2).


OUTPUT CLOSE.
DELETE OBJECT hQuery.

DELETE PROCEDURE hStrLib NO-ERROR.

IF iCount = 1 THEN
  ocReturn = "Varehåndteringsboken inneholder ingen bestillinger".
ELSE 
  ASSIGN ocReturn = cFileName + "|" + STRING(iCount + 3)
         obOk     = TRUE.

FUNCTION AccumUke RETURNS LOG:

  IF VareBehLinjeTrans.Bestilt1 NE 0 THEN DO:
    FIND FIRST ttTotUke
         WHERE ttTotUke.iUkeNr = VareBehLinjeTrans.Levuke1
         NO-ERROR.
    IF NOT AVAIL ttTotUke THEN DO:
      CREATE ttTotUke.
      ttTotUke.iUkeNr = VareBehLinjeTrans.Levuke1.
    END.
    ttTotUke.fTotUke = ttTotUke.fTotUke + VareBehLinjeTrans.Bestilt1 * VareBehLinje.Varekost * iAntFord.
  END.
  IF VareBehLinjeTrans.Bestilt2 NE 0 THEN DO:
    FIND FIRST ttTotUke
         WHERE ttTotUke.iUkeNr = VareBehLinjeTrans.Levuke2
         NO-ERROR.
    IF NOT AVAIL ttTotUke THEN DO:
      CREATE ttTotUke.
      ttTotUke.iUkeNr = VareBehLinjeTrans.Levuke2.
    END.
    ttTotUke.fTotUke = ttTotUke.fTotUke + VareBehLinjeTrans.Bestilt2 * VareBehLinje.Varekost * iAntFord.
  END.
  IF VareBehLinjeTrans.Bestilt3 NE 0 THEN DO:
    FIND FIRST ttTotUke
         WHERE ttTotUke.iUkeNr = VareBehLinjeTrans.Levuke3
         NO-ERROR.
    IF NOT AVAIL ttTotUke THEN DO:
      CREATE ttTotUke.
      ttTotUke.iUkeNr = VareBehLinjeTrans.Levuke3.
    END.
    ttTotUke.fTotUke = ttTotUke.fTotUke + VareBehLinjeTrans.Bestilt3 * VareBehLinje.Varekost * iAntFord.
  END.
  IF VareBehLinjeTrans.Bestilt4 NE 0 THEN DO:
    FIND FIRST ttTotUke
         WHERE ttTotUke.iUkeNr = VareBehLinjeTrans.Levuke4
         NO-ERROR.
    IF NOT AVAIL ttTotUke THEN DO:
      CREATE ttTotUke.
      ttTotUke.iUkeNr = VareBehLinjeTrans.Levuke4.
    END.
    ttTotUke.fTotUke = ttTotUke.fTotUke + VareBehLinjeTrans.Bestilt4 * VareBehLinje.Varekost * iAntFord.
  END.

END FUNCTION.

FUNCTION AccumMerk RETURNS LOG:

  FIND FIRST ttTotMerk
       WHERE ttTotMerk.cMerknad = VarebehLinje.LinjeMerknad
       NO-ERROR.
  IF NOT AVAIL ttTotMerk THEN DO:
    CREATE ttTotMerk.
    ttTotMerk.cMerknad = VarebehLinje.LinjeMerknad.
  END.
  ttTotMerk.fTotMerk = ttTotMerk.fTotMerk + iSumAntStr * VareBehLinje.Varekost.

END FUNCTION.

FUNCTION AccumBut RETURNS LOG:

  FIND FIRST ttTotBut
       WHERE ttTotBut.iButNr = VarebehLinjeTrans.ButikkNr
       NO-ERROR.
  IF NOT AVAIL ttTotBut THEN DO:
    CREATE ttTotBut.
    ASSIGN ttTotBut.iButNr   = VarebehLinjeTrans.ButikkNr
           ttTotBut.cButNavn = Butiker.Butnamn.
  END.
  ttTotBut.fTotBut = ttTotBut.fTotBut + iSumAntStr * VareBehLinje.Varekost.

END FUNCTION.

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

