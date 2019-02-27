/* Sammendrag av registreringer for varehåndteringsbok, supplering
   Parametere: Input: Query
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

DEF VAR iWeek           AS INT    NO-UNDO.
DEF VAR cButikkListe    AS CHAR   NO-UNDO.
DEF VAR iBestStat       AS INT    NO-UNDO.
DEF VAR bOk             AS LOG    NO-UNDO.
DEF VAR bGjFakt         AS LOG    NO-UNDO.
DEF VAR bKjedeVare      AS LOG    NO-UNDO.
DEF VAR iPrevBestStat   AS INT    NO-UNDO.
DEF VAR iPrevBestNr     AS INT    NO-UNDO.
DEF VAR cPrevStorl      AS CHAR   NO-UNDO.

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

ASSIGN cButikkListe = ENTRY(3,icParam,"¤")
       bTotalSam    = ENTRY(4,icParam,"¤") = "yes"
       iBestStat    = INT(ENTRY(5,icParam,"¤"))
       bKjedeVare   = IF ENTRY(6,icParam,"¤") NE "" THEN LOGICAL(ENTRY(6,icParam,"¤")) ELSE ?
       bGjFakt      = IF ENTRY(7,icParam,"¤") NE "" THEN LOGICAL(ENTRY(7,icParam,"¤")) ELSE ?
       .

CREATE QUERY hQuery.

hQuery:SET-BUFFERS(BUFFER Ordre:HANDLE
                  ,BUFFER BestHode:HANDLE
                  ,BUFFER VarebehLinje:HANDLE
                  ,BUFFER BestStr:HANDLE
                  ).

hQuery:QUERY-PREPARE("FOR EACH Ordre NO-LOCK " + ENTRY(1,icParam,"¤")
                   + ",EACH BestHode OF Ordre NO-LOCK"
                   + " WHERE " +
                    (IF iBestStat = 4 THEN
                      "BestHode.BestStat = 4 AND BestHode.BekreftetDato = ?"
                     ELSE IF iBestStat = 44 THEN
                       "BestHode.BestStat = 4 AND BestHode.BekreftetDato NE ?"
                     ELSE IF iBestStat > 0 THEN
                       "BestHode.BestStat = " + STRING(iBestStat)
                     ELSE "TRUE")
                   + ",FIRST VarebehLinje NO-LOCK "
                   + "   WHERE VarebehLinje.VarebehNr = " + ENTRY(2,icParam,"¤")
                   + "     AND VarebehLinje.ArtikkelNr = BestHode.ArtikkelNr"
                   + (IF bKjedeVare NE ? THEN 
                        " AND VareBehLinje.KjedeVare = " + STRING(bKjedeVare)
                      ELSE "")
                   + (IF bGjFakt NE ? THEN 
                        " AND VareBehLinje.Gjennomfaktureres = " + STRING(bGjFakt)
                      ELSE "")
                   + ",EACH BestStr NO-LOCK OF BestHode"
                   + "   WHERE CAN-DO('" + cButikkListe + "',STRING(BestStr.Butik))"
                   + " BY Ordre.OrdreNr BY BestHode.BestNr BY BestStr.Butik BY BestStr.Storl BY BestStr.BestStat DESC").

hQuery:QUERY-OPEN().
hQuery:GET-FIRST().

IF NOT AVAIL Besthode THEN DO:
  ocReturn = "Ingen bestillinger funnet: " + CHR(10) + hQuery:PREPARE-STRING.
  RETURN.
END.

cFileName       = SESSION:TEMP-DIR + STRING(BestHode.VarebehNr) + "_" 
                + (IF cButikkListe NE "*" THEN ENTRY(1,cButikkListe) + "_" ELSE "") 
                + STRING(YEAR(TODAY)) + STRING(MONTH(TODAY),"99") + STRING(DAY(TODAY)) + STRING(TIME) + ".txt".

OUTPUT TO VALUE(cFileName).

PUT UNFORMATTED "Butikk(er): " + "~t" + cButikkListe             
                SKIP.

REPEAT WHILE NOT hQuery:QUERY-OFF-END:

  IF BestStr.BestNr = iPrevBestNr AND BestStr.Butik = iPrevBut AND BestStr.Storl = cPrevStorl AND iPrevBestNr NE 0 THEN DO:
    hQuery:GET-NEXT().
    NEXT.
  END.

  RUN weeknum.p (BestHode.Levdato,OUTPUT iWeek).

  iCount  = iCount + 1.

  IF VarebehLinje.Hg NE iPrevHg AND iPrevHg NE 0 THEN
    fTotHg = 0.
  IF VarebehLinje.AvdelingNr NE iPrevAvdNr AND iPrevAvdNr NE 0 THEN 
    fTotAvd = 0.
  IF VarebehLinje.levnr NE iPrevLevnr AND iPrevLevnr NE 0 THEN 
    fTotLev = 0.

  ASSIGN fGrandTot   = fGrandTot + VarebehLinje.VareKost * BestStr.Bestilt
         fGrandTotKj = fGrandTotKj + (IF VarebehLinje.KjedeInnkPris NE 0 THEN VarebehLinje.KjedeInnkPris ELSE VarebehLinje.VareKost) * BestStr.Bestilt
         .

  AccumUke().
  AccumLev().
  AccumBut(). 
  AccumAvd().
  AccumHg().
  AccumVg().

  ASSIGN iPrevAvdNr    = VarebehLinje.AvdelingNr
         iPrevBut      = BestStr.Butik
         iPrevHg       = VarebehLinje.Hg
         iPrevVg       = VarebehLinje.Vg
         iPrevLevnr    = VarebehLinje.Levnr
         cPrevAvdNavn  = VarebehLinje.AvdelingNavn
         cPrevHgbeskr  = VarebehLinje.HgBeskr
         cPrevLevNavn  = VarebehLinje.Levnamn
         cPrevStorl    = BestStr.Storl
         iPrevBestNr   = BestStr.BestNr
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

  PUT UNFORMATTED "SUM kjedemargin: ~t~t" fGrandTot - fGrandTotKj.
END.


OUTPUT CLOSE.
DELETE OBJECT hQuery.

IF iCount = 1 THEN
  ocReturn = "Varehåndteringsboken inneholder ingen bestillinger".
ELSE 
  ASSIGN ocReturn = cFileName
         obOk     = TRUE.

FUNCTION AccumUke RETURNS LOG:
  IF BestStr.Bestilt NE 0 THEN DO:
    FIND FIRST ttTotUke
         WHERE ttTotUke.iUkeNr = iWeek
         NO-ERROR.
    IF NOT AVAIL ttTotUke THEN DO:
      CREATE ttTotUke.
      ttTotUke.iUkeNr = iWeek.
    END.
    ASSIGN ttTotUke.fTotUke   = ttTotUke.fTotUke   + BestStr.Bestilt * VarebehLinje.Varekost
           ttTotUke.fTotUkeKj = ttTotUke.fTotUkeKj + BestStr.Bestilt * (IF VarebehLinje.KjedeInnkPris NE 0 THEN VarebehLinje.KjedeInnkPris ELSE VarebehLinje.VareKost)
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
  ASSIGN ttTotLev.fTotLev   = ttTotLev.fTotLev   + BestStr.Bestilt * VarebehLinje.Varekost
         ttTotLev.fTotLevKj = ttTotLev.fTotLevKj + BestStr.Bestilt * (IF VarebehLinje.KjedeInnkPris NE 0 THEN VarebehLinje.KjedeInnkPris ELSE VarebehLinje.VareKost)
         .

END FUNCTION.

FUNCTION AccumBut RETURNS LOG:

  FIND FIRST ttTotBut
       WHERE ttTotBut.iButNr = BestStr.Butik
       NO-ERROR.
  IF NOT AVAIL ttTotBut THEN DO:
    FIND FIRST Butiker NO-LOCK
         WHERE Butiker.Butik = BestStr.Butik
         NO-ERROR.
    IF AVAIL Butiker THEN DO:
      CREATE ttTotBut.
      ASSIGN ttTotBut.iButNr   = BestStr.Butik
             ttTotBut.cButNavn = Butiker.Butnamn.
    END.
  END.
  ASSIGN ttTotBut.fTotBut   = ttTotBut.fTotBut   + BestStr.Bestilt * VarebehLinje.Varekost
         ttTotBut.fTotButKj = ttTotBut.fTotButKj + BestStr.Bestilt * (IF VarebehLinje.KjedeInnkPris NE 0 THEN VarebehLinje.KjedeInnkPris ELSE VarebehLinje.VareKost)
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
  ASSIGN ttTotAvd.fTotAvd   = ttTotAvd.fTotAvd   + BestStr.Bestilt * VarebehLinje.Varekost
         ttTotAvd.fTotAvdKj = ttTotAvd.fTotAvdKj + BestStr.Bestilt * (IF VarebehLinje.KjedeInnkPris NE 0 THEN VarebehLinje.KjedeInnkPris ELSE VarebehLinje.VareKost)
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
  ASSIGN ttTotHg.fTotHg   = ttTotHg.fTotHg   + BestStr.Bestilt * VarebehLinje.Varekost
         ttTotHg.fTotHgKj = ttTotHg.fTotHgKj + BestStr.Bestilt * (IF VarebehLinje.KjedeInnkPris NE 0 THEN VarebehLinje.KjedeInnkPris ELSE VarebehLinje.VareKost)
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
  ttTotVg.fTotVg = ttTotVg.fTotVg + BestStr.Bestilt * VarebehLinje.Varekost.

END FUNCTION.

