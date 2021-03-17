/* Sammendrag av registreringer for varehåndteringsbok, messeregistrering
   Parametere: Input: Varebehandlingsnr,butikknr,levnr (butikknr og levnr trenger ikke være med)
               Output (ocReturn): Filnavn
   Opprettet: 20.03.06 av BHa                          
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
DEF VAR fTotAvd         AS DEC    NO-UNDO.
DEF VAR fTotBut         AS DEC    NO-UNDO.
DEF VAR fTotHg          AS DEC    NO-UNDO.
DEF VAR fTotVg          AS DEC    NO-UNDO.
DEF VAR fTotLev         AS DEC    NO-UNDO.
DEF VAR iPrevAvdNr      AS INT    NO-UNDO.
DEF VAR iPrevBut        AS INT    NO-UNDO.
DEF VAR iPrevHg         AS INT    NO-UNDO.
DEF VAR iPrevVg         AS INT    NO-UNDO.
DEF VAR iPrevLevnr      AS INT    NO-UNDO.
DEF VAR cPrevAvdNavn    AS CHAR   NO-UNDO.
DEF VAR cPrevHgBeskr    AS CHAR   NO-UNDO.
DEF VAR cPrevLevNavn    AS CHAR   NO-UNDO.
DEF VAR iSumAntStr      AS INT    NO-UNDO.
DEF VAR cSortString     AS CHAR   NO-UNDO.
DEF VAR bEAN            AS LOG    NO-UNDO.
DEF VAR cRowIdList      AS CHAR   NO-UNDO.

DEF VAR cStrList        AS CHAR   NO-UNDO.
DEF VAR cMinStr         AS CHAR   NO-UNDO.
DEF VAR cMaxStr         AS CHAR   NO-UNDO.
DEF VAR cSortFordList   AS CHAR   NO-UNDO.
DEF VAR iAntFord        AS INT    NO-UNDO.
DEF VAR bInndeling      AS LOG    NO-UNDO.
DEF VAR bDumpReport     AS LOG    NO-UNDO.
DEF VAR ix              AS INT    NO-UNDO.
DEF VAR iDumpCount      AS INT    NO-UNDO INIT 1.
DEF VAR fGrandTotKj     AS DEC    NO-UNDO.
DEF VAR fKjedeDB%       AS DEC    NO-UNDO.
DEF VAR bKjedePris      AS LOG    NO-UNDO.
DEF VAR bOrdreForslag   AS LOG    NO-UNDO.
DEF VAR bTotalSam       AS LOG    NO-UNDO.
DEF VAR bFirstVg        AS LOG    NO-UNDO.
DEF VAR fTotKjedeLev    AS DEC    NO-UNDO.
DEF VAR fTotGjFakt      AS DEC    NO-UNDO.

DEF TEMP-TABLE ttTotUke
    FIELD iUkeNr    AS INT
    FIELD fTotUke   AS DEC
    FIELD fTotUkeKj AS DEC
    .

DEF TEMP-TABLE ttTotLev
    FIELD iLevNr    AS INT
    FIELD cLevNavn  AS CHAR
    FIELD fTotLev   AS DEC
    FIELD fTotLevKj AS DEC
    .

DEF TEMP-TABLE ttTotBut
    FIELD iButNr    AS INT
    FIELD cButNavn  AS CHAR
    FIELD fTotBut   AS DEC
    FIELD fTotButKj AS DEC
    .

DEF TEMP-TABLE ttTotAvd
    FIELD iAvdNr    AS INT
    FIELD cAvdNavn  AS CHAR
    FIELD fTotAvd   AS DEC
    FIELD fTotAvdKj AS DEC
    .

DEF TEMP-TABLE ttTotHg
    FIELD iHgNr    AS INT
    FIELD cHgNavn  AS CHAR
    FIELD fTotHg   AS DEC
    FIELD fTotHgKj AS DEC
    .

DEF TEMP-TABLE ttTotVg
    FIELD iVgNr    AS INT
    FIELD iHgNr    AS INT
    FIELD cVgNavn  AS CHAR
    FIELD fTotVg   AS DEC
    .

FUNCTION AccumUke RETURNS LOG () FORWARD.
FUNCTION AccumLev RETURNS LOG () FORWARD.
FUNCTION AccumBut RETURNS LOG () FORWARD.
FUNCTION AccumHg  RETURNS LOG () FORWARD.
FUNCTION AccumVg  RETURNS LOG () FORWARD.
FUNCTION AccumAvd RETURNS LOG () FORWARD.

bTotalSam = ENTRY(2,icParam,";") = "".

FIND FIRST VarebehHode WHERE VarebehHode.VarebehNr = DEC(ENTRY(1,icParam,";")) NO-LOCK.

CREATE QUERY hQuery.
hQuery:SET-BUFFERS(BUFFER VareBehLinjeTrans:HANDLE
                  ,BUFFER VarebehLinje:HANDLE
                  ).

hQuery:QUERY-PREPARE("FOR EACH VareBehLinjeTrans NO-LOCK " 
                     + " WHERE VareBehLinjeTrans.VarebehNr = " + ENTRY(1,icParam,";") 
                     + " AND VarebehLinjeTrans.ArtikkelNr > 0"
                     + (IF ENTRY(2,icParam,";") NE "" THEN
                         " AND VareBehLinjeTrans.ButikkNr = " + ENTRY(2,icParam,";")
                        ELSE "")
                     + " AND RegistrertBestilling"
                     + " AND GodkjentBestilling"
                     + ",FIRST VarebehLinje OF VarebehLinjeTrans NO-LOCK"
                     + (IF ENTRY(3,icParam,";") NE "" THEN
                         " WHERE CAN-DO('" + ENTRY(3,icParam,";") + "',STRING(VareBehLinje.levnr))"
                        ELSE "")
                     + cSortString
                     ).

hQuery:QUERY-OPEN().
hQuery:GET-FIRST().

IF NOT AVAIL VarebehLinjeTrans THEN DO:
  ocReturn = "Varebehandlingsbok ikke funnet".
  RETURN.
END.

cFileName       = SESSION:TEMP-DIR + STRING(VarebehLinjeTrans.VarebehNr) + "_" + STRING(VarebehLinjeTrans.ButikkNr) + "_" + STRING(YEAR(TODAY)) + STRING(MONTH(TODAY),"99") + STRING(DAY(TODAY)) + STRING(TIME) + ".txt".

OUTPUT TO VALUE(cFileName).

IF NOT bTotalSam THEN
  PUT UNFORMATTED "Butikknr: " + ENTRY(2,icParam,";")             + "~t" +
                  "Leverandør: " + ENTRY(3,icParam,";")
                  SKIP.
ELSE 
  PUT UNFORMATTED "Vareh.bok: " + ENTRY(1,icParam,";")             + "~t" +
                  VareBehHode.VareBehBeskrivelse
                  SKIP.


REPEAT WHILE NOT hQuery:QUERY-OFF-END:
  iCount  = iCount + 1.

  IF VarebehLinje.Hg NE iPrevHg AND iPrevHg NE 0 THEN
    fTotHg = 0.
  IF VarebehLinje.AvdelingNr NE iPrevAvdNr AND iPrevAvdNr NE 0 THEN 
    fTotAvd = 0.
  IF VarebehLinje.levnr NE iPrevLevnr AND iPrevLevnr NE 0 THEN 
    fTotLev = 0.

  RUN Inndeling.

  ASSIGN iSumAntStr  = (VareBehLinjeTrans.Bestilt1 + VareBehLinjeTrans.Bestilt2 + VareBehLinjeTrans.Bestilt3 + VareBehLinjeTrans.Bestilt4) * iAntFord
         fGrandTot   = fGrandTot + VareBehLinje.VareKost * iSumAntStr
         fGrandTotKj = fGrandTotKj + (IF VarebehLinje.KjedeInnkPris NE 0 THEN VarebehLinje.KjedeInnkPris ELSE VareBehLinje.VareKost) * iSumAntStr
         .

  IF VarebehLinje.KjedeVare THEN
    fTotKjedeLev = fTotKjedeLev + VareBehLinje.VareKost * iSumAntStr.
  ELSE IF VarebehLinje.Gjennomfaktureres THEN
    fTotGjFakt = fTotGjFakt + VareBehLinje.VareKost * iSumAntStr.

  AccumUke().
  AccumLev().
  AccumBut(). 
  AccumAvd().
  AccumHg().
  AccumVg().

  ASSIGN iPrevAvdNr   = VarebehLinje.AvdelingNr
         iPrevBut     = VarebehLinjeTrans.ButikkNr
         iPrevHg      = VarebehLinje.Hg
         iPrevVg      = VarebehLinje.Vg
         iPrevLevnr   = VarebehLinje.Levnr
         cPrevAvdNavn = VarebehLinje.AvdelingNavn
         cPrevHgbeskr = VarebehLinje.HgBeskr
         cPrevLevNavn = VarebehLinje.Levnamn
         .
  hQuery:GET-NEXT().
END.

PUT SKIP(2).
PUT UNFORMATTED "SUM pr avd:" SKIP.
FOR EACH ttTotAvd BY ttTotAvd.cAvdNavn:
  PUT UNFORMATTED "~t" + STRING(ttTotAvd.cAvdNavn) +  "~t" + STRING(ttTotAvd.fTotAvd) +
      (IF bTotalSam THEN "~t~t" + STRING(ttTotAvd.fTotAvdKj) ELSE "") SKIP.
END.
PUT UNFORMATTED "TOTALT" + FILL("~t",2) 
                fGrandTot (IF bTotalSam THEN "~t~t" + STRING(fGrandTotKj) ELSE "") SKIP(2).

PUT UNFORMATTED "SUM pr hg" + (IF NOT bTotalSam THEN "/vg:" ELSE ":") SKIP.
FOR EACH ttTotHg BY ttTotHg.cHgNavn:
  PUT UNFORMATTED "~t" + STRING(ttTotHg.cHgNavn) +  "~t" + STRING(ttTotHg.fTotHg).
  IF NOT bTotalSam THEN DO:
    bFirstVg = YES.
    FOR EACH ttTotVg 
        WHERE ttTotVg.iHgNr = ttTotHg.iHgNr
        BY ttTotVg.cVgNavn:
      PUT UNFORMATTED (IF bFirstVg THEN "~t" ELSE "~t~t~t") + STRING(ttTotVg.cVgNavn) +  "~t" + STRING(ttTotVg.fTotVg) SKIP.
      bFirstVg = NO.
    END.
  END.
  ELSE PUT "~t~t" + STRING(ttTotHg.fTotHgKj) SKIP.
END.
PUT UNFORMATTED "TOTALT" + FILL("~t",2) 
                fGrandTot (IF bTotalSam THEN "~t~t" + STRING(fGrandTotKj) ELSE "") SKIP(2).

PUT UNFORMATTED "SUM pr uke:" SKIP.
FOR EACH ttTotUke BY ttTotUke.iUkeNr:
  PUT UNFORMATTED "~t" + STRING(ttTotUke.iUkeNr) + "~t" + STRING(ttTotUke.fTotUke) +
      (IF bTotalSam THEN "~t~t" + STRING(ttTotUke.fTotUkeKj) ELSE "") SKIP.

END.
PUT UNFORMATTED "TOTALT" + FILL("~t",2) 
                fGrandTot (IF bTotalSam THEN "~t~t" + STRING(fGrandTotKj) ELSE "") SKIP(2).

PUT UNFORMATTED "SUM pr lev:" SKIP.
FOR EACH ttTotLev BY ttTotLev.cLevNavn:
  PUT UNFORMATTED "~t" + STRING(ttTotLev.cLevNavn) +  "~t" + STRING(ttTotLev.fTotLev) +
      (IF bTotalSam THEN "~t~t" + STRING(ttTotLev.fTotLevKj) ELSE "") SKIP.

END.
PUT UNFORMATTED "TOTALT" + FILL("~t",2) 
                fGrandTot (IF bTotalSam THEN "~t~t" + STRING(fGrandTotKj) ELSE "") SKIP(2).
  
IF bTotalSam THEN DO:
  PUT UNFORMATTED "SUM pr butikk:" SKIP.
  FOR EACH ttTotBut BY ttTotBut.cButNavn:
    PUT UNFORMATTED "~t" + STRING(ttTotBut.cButNavn) +  "~t" + STRING(ttTotBut.fTotBut) +
        "~t~t" + STRING(ttTotBut.fTotButKj) SKIP.

  END.
  PUT UNFORMATTED "TOTALT" + FILL("~t",2)
                  fGrandTot "~t~t" + STRING(fGrandTotKj) SKIP(2).

  PUT UNFORMATTED "SUM kjedemargin: ~t~t" fGrandTot - fGrandTotKj SKIP(2).
  PUT UNFORMATTED "SUM gjennomfaktureres: ~t~t" fTotGjFakt SKIP.
  PUT UNFORMATTED "SUM kjedevarer: ~t~t" fTotKjedeLev SKIP.

  IF fGrandTot - (fTotGjFakt + fTotKjedeLev) > 0 THEN
    PUT UNFORMATTED "SUM andre: ~t~t" fGrandTot - (fTotGjFakt + fTotKjedeLev) SKIP.
END.


OUTPUT CLOSE.
DELETE OBJECT hQuery.

IF iCount = 1 THEN
  ocReturn = "Varehåndteringsboken inneholder ingen bestillinger".
ELSE 
  ASSIGN ocReturn = cFileName
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
    ASSIGN ttTotUke.fTotUke   = ttTotUke.fTotUke   + VareBehLinjeTrans.Bestilt1 * VareBehLinje.Varekost * iAntFord
           ttTotUke.fTotUkeKj = ttTotUke.fTotUkeKj + VarebehLinjeTrans.Bestilt1 * (IF VarebehLinje.KjedeInnkPris NE 0 THEN VarebehLinje.KjedeInnkPris ELSE VareBehLinje.VareKost)
           .
  END.
  IF VareBehLinjeTrans.Bestilt2 NE 0 THEN DO:
    FIND FIRST ttTotUke
         WHERE ttTotUke.iUkeNr = VareBehLinjeTrans.Levuke2
         NO-ERROR.
    IF NOT AVAIL ttTotUke THEN DO:
      CREATE ttTotUke.
      ttTotUke.iUkeNr = VareBehLinjeTrans.Levuke2.
    END.
    ASSIGN ttTotUke.fTotUke   = ttTotUke.fTotUke   + VareBehLinjeTrans.Bestilt2 * VareBehLinje.Varekost * iAntFord
           ttTotUke.fTotUkeKj = ttTotUke.fTotUkeKj + VarebehLinjeTrans.Bestilt2 * (IF VarebehLinje.KjedeInnkPris NE 0 THEN VarebehLinje.KjedeInnkPris ELSE VareBehLinje.VareKost)
           .
  END.
  IF VareBehLinjeTrans.Bestilt3 NE 0 THEN DO:
    FIND FIRST ttTotUke
         WHERE ttTotUke.iUkeNr = VareBehLinjeTrans.Levuke3
         NO-ERROR.
    IF NOT AVAIL ttTotUke THEN DO:
      CREATE ttTotUke.
      ttTotUke.iUkeNr = VareBehLinjeTrans.Levuke3.
    END.
    ASSIGN ttTotUke.fTotUke   = ttTotUke.fTotUke   + VareBehLinjeTrans.Bestilt3 * VareBehLinje.Varekost * iAntFord
           ttTotUke.fTotUkeKj = ttTotUke.fTotUkeKj + VarebehLinjeTrans.Bestilt3 * (IF VarebehLinje.KjedeInnkPris NE 0 THEN VarebehLinje.KjedeInnkPris ELSE VareBehLinje.VareKost)
           .
  END.
  IF VareBehLinjeTrans.Bestilt4 NE 0 THEN DO:
    FIND FIRST ttTotUke
         WHERE ttTotUke.iUkeNr = VareBehLinjeTrans.Levuke4
         NO-ERROR.
    IF NOT AVAIL ttTotUke THEN DO:
      CREATE ttTotUke.
      ttTotUke.iUkeNr = VareBehLinjeTrans.Levuke4.
    END.
    ASSIGN ttTotUke.fTotUke   = ttTotUke.fTotUke   + VareBehLinjeTrans.Bestilt4 * VareBehLinje.Varekost * iAntFord
           ttTotUke.fTotUkeKj = ttTotUke.fTotUkeKj + VarebehLinjeTrans.Bestilt4 * (IF VarebehLinje.KjedeInnkPris NE 0 THEN VarebehLinje.KjedeInnkPris ELSE VareBehLinje.VareKost)
           .
  END.

END FUNCTION.

FUNCTION AccumLev RETURNS LOG:

  FIND FIRST ttTotLev
       WHERE ttTotLev.iLevNr = VarebehLinje.Levnr
       NO-ERROR.
  IF NOT AVAIL ttTotLev THEN DO:
    CREATE ttTotLev.
    ASSIGN ttTotLev.iLevNr   = VarebehLinje.Levnr
           ttTotLev.cLevNavn = VarebehLinje.Levnamn.
  END.
  ASSIGN ttTotLev.fTotLev   = ttTotLev.fTotLev   + iSumAntStr * VareBehLinje.Varekost
         ttTotLev.fTotLevKj = ttTotLev.fTotLevKj + iSumAntStr * (IF VarebehLinje.KjedeInnkPris NE 0 THEN VarebehLinje.KjedeInnkPris ELSE VareBehLinje.VareKost)
         .

END FUNCTION.

FUNCTION AccumBut RETURNS LOG:

  FIND FIRST ttTotBut
       WHERE ttTotBut.iButNr = VarebehLinjeTrans.ButikkNr
       NO-ERROR.
  IF NOT AVAIL ttTotBut THEN DO:
    FIND FIRST Butiker NO-LOCK
         WHERE Butiker.Butik = VarebehLinjeTrans.ButikkNr
         NO-ERROR.
    IF AVAIL Butiker THEN DO:
      CREATE ttTotBut.
      ASSIGN ttTotBut.iButNr   = VarebehLinjeTrans.ButikkNr
             ttTotBut.cButNavn = Butiker.Butnamn.
    END.
  END.
  ASSIGN ttTotBut.fTotBut   = ttTotBut.fTotBut   + iSumAntStr * VareBehLinje.Varekost
         ttTotBut.fTotButKj = ttTotBut.fTotButKj + iSumAntStr * (IF VarebehLinje.KjedeInnkPris NE 0 THEN VarebehLinje.KjedeInnkPris ELSE VareBehLinje.VareKost)
         .

END FUNCTION.

FUNCTION AccumAvd RETURNS LOG:

  FIND FIRST ttTotAvd
       WHERE ttTotAvd.iAvdNr = VarebehLinje.AvdelingNr
       NO-ERROR.
  IF NOT AVAIL ttTotAvd THEN DO:
    CREATE ttTotAvd.
    ASSIGN ttTotAvd.iAvdNr   = VarebehLinje.AvdelingNr
           ttTotAvd.cAvdNavn = VarebehLinje.AvdelingNavn.
  END.
  ASSIGN ttTotAvd.fTotAvd   = ttTotAvd.fTotAvd   + iSumAntStr * VareBehLinje.Varekost
         ttTotAvd.fTotAvdKj = ttTotAvd.fTotAvdKj + iSumAntStr * (IF VarebehLinje.KjedeInnkPris NE 0 THEN VarebehLinje.KjedeInnkPris ELSE VareBehLinje.VareKost)
         .

END FUNCTION.

FUNCTION AccumHg RETURNS LOG:

  FIND FIRST ttTotHg
       WHERE ttTotHg.iHgNr = VarebehLinje.Hg
       NO-ERROR.
  IF NOT AVAIL ttTotHg THEN DO:
    CREATE ttTotHg.
    ASSIGN ttTotHg.iHgNr   = VarebehLinje.Hg
           ttTotHg.cHgNavn = VarebehLinje.HgBeskr.
  END.
  ASSIGN ttTotHg.fTotHg   = ttTotHg.fTotHg   + iSumAntStr * VareBehLinje.Varekost
         ttTotHg.fTotHgKj = ttTotHg.fTotHgKj + iSumAntStr * (IF VarebehLinje.KjedeInnkPris NE 0 THEN VarebehLinje.KjedeInnkPris ELSE VareBehLinje.VareKost)
         .

END FUNCTION.

FUNCTION AccumVg RETURNS LOG:

  FIND FIRST ttTotVg
       WHERE ttTotVg.iVgNr = VarebehLinje.Vg
       NO-ERROR.
  IF NOT AVAIL ttTotVg THEN DO:
    CREATE ttTotVg.
    ASSIGN ttTotVg.iVgNr   = VarebehLinje.Vg
           ttTotVg.iHgNr   = VarebehLinje.Hg
           ttTotVg.cVgNavn = VarebehLinje.VgBeskr.
  END.
  ttTotVg.fTotVg = ttTotVg.fTotVg + iSumAntStr * VareBehLinje.Varekost.

END FUNCTION.

PROCEDURE Inndeling:
  ASSIGN cStrList      = ""
         cMinStr       = ""
         cMaxStr       = ""
         cSortFordList = ""
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
      IF cMinStr = "" THEN cMinStr = TRIM(LevSAnt.SoStorl).
    END.
    ASSIGN cStrList      = TRIM(cStrList,",")
           cSortFordList = TRIM(cSortFordList,",")
           iCount        = iCount + 1
           .
  END.
END PROCEDURE.
