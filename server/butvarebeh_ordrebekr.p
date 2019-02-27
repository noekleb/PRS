/* Rapport fra varehåndteringsbok
   Parametere: Input: Varebehandlingsnr,butikknr,levnr (butikknr og levnr trenger ikke være med)
               Output (ocReturn): Filnavn
               Merk at antall parametere styrer layout, summeringer, etc
               ENTRY(1,icParam): VarebehNr
               ENTRY(2,icParam): (Liste over) butikknr 
               ENTRY(3,icParam): (Liste over) leverandørnr 
               ENTRY(4,icParam): Evt annen sortering enn default for butikkbekr 
               ENTRY(9,icParam) = "kjedepris": Benytt kjedepris i stedet for varekost
   Opprettet: 13.10.10 av BHa                          
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
DEF VAR fTotHg          AS DEC    NO-UNDO.
DEF VAR fTotLev         AS DEC    NO-UNDO.
DEF VAR iPrevAvdNr      AS INT    NO-UNDO.
DEF VAR iPrevHg         AS INT    NO-UNDO.
DEF VAR iPrevLevnr      AS INT    NO-UNDO.
DEF VAR cPrevAvdNavn    AS CHAR   NO-UNDO.
DEF VAR cPrevHgBeskr    AS CHAR   NO-UNDO.
DEF VAR cPrevLevNavn    AS CHAR   NO-UNDO.
DEF VAR iSumAntStr      AS INT    NO-UNDO.
DEF VAR cSortString     AS CHAR   NO-UNDO.
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
DEF VAR fGrandTotKjede  AS DEC    NO-UNDO.
DEF VAR fKjedeDB%       AS DEC    NO-UNDO.
DEF VAR bKjedePris      AS LOG    NO-UNDO.
DEF VAR bOrdreForslag   AS LOG    NO-UNDO.
DEF VAR cLevNr          AS CHAR   NO-UNDO.    /* Hvis entry(3,icParam) = "vis_butnr" blankes denne men butnr legges på filen (for rapportering flere but */
DEF VAR cButNr          AS CHAR   NO-UNDO.
DEF VAR cBestStatus     AS CHAR   NO-UNDO.

{incl/ean13bc.i}

DEF TEMP-TABLE ttTotUke
    FIELD iUkeNr    AS INT
    FIELD fTotUke   AS DEC.

DEF TEMP-TABLE ttTotLev
    FIELD iLevNr    AS INT
    FIELD cLevNavn  AS CHAR
    FIELD fTotLev   AS DEC.

DEF TEMP-TABLE ttTotBut
    FIELD iButNr    AS INT
    FIELD cButNavn  AS CHAR
    FIELD fTotBut   AS DEC.

DEF TEMP-TABLE ttTotAvd
    FIELD iAvdNr    AS INT
    FIELD cAvdNavn  AS CHAR
    FIELD fTotAvd   AS DEC.

DEF TEMP-TABLE ttTotHg
    FIELD iHgNr    AS INT
    FIELD cHgNavn  AS CHAR
    FIELD fTotHg   AS DEC.

FUNCTION AccumUke RETURNS LOG () FORWARD.
FUNCTION AccumLev RETURNS LOG () FORWARD.
FUNCTION AccumBut RETURNS LOG () FORWARD.
FUNCTION AccumHg  RETURNS LOG () FORWARD.
FUNCTION AccumAvd RETURNS LOG () FORWARD.

ASSIGN cButNr      = ENTRY(2,icParam)
       cLevNr      = ENTRY(3,icParam)
       cBestStatus = ENTRY(4,icParam)
       .

IF cLevNr NE "" THEN
  cSortString = " BY VarebehLinje.LevNr".
ELSE
  cSortString = " BY VarebehLinje.AvdelingNr BY VarebehLinje.Hg".

cSortString = cSortString + " BY VarebehLinje.Artikkelnr BY StrKonv.SeqNr".

FIND FIRST VarebehHode WHERE VarebehHode.VarebehNr = DEC(ENTRY(1,icParam)) NO-LOCK.

CREATE QUERY hQuery.
hQuery:SET-BUFFERS(BUFFER VareBehBestLinje:HANDLE
                  ,BUFFER VareBehBestHode:HANDLE
                  ,BUFFER StrKonv:HANDLE
                  ,BUFFER VarebehLinje:HANDLE
                  ,BUFFER ArtBas:HANDLE
                  ,BUFFER Varemerke:HANDLE
                  ,BUFFER Butiker:HANDLE
                  ,BUFFER BestHode:HANDLE
                  ).

hQuery:QUERY-PREPARE("FOR EACH VareBehBestLinje NO-LOCK " 
                     + " WHERE VareBehBestLinje.VarebehNr = " + ENTRY(1,icParam) 
                     + (IF NUM-ENTRIES(cButNr,"|") > 1 THEN 
                         " AND LOOKUP(STRING(VareBehBestLinje.BestiltButikkNr),'" + cButNr + "','|') > 0"
                        ELSE IF cButNr NE "" THEN
                         " AND VareBehBestLinje.BestiltButikkNr = " + cButNr
                        ELSE "")
                     + (IF cRowIdList NE "" THEN
                         " AND CAN-DO('" + cRowIdList + "',STRING(ROWID(VareBehBestLinje)))"
                        ELSE "")
                     + ",FIRST VarebehBestHode OF VareBehBestLinje NO-LOCK"
                     + ",FIRST StrKonv OF VareBehBestLinje NO-LOCK OUTER-JOIN"
                     + ",FIRST VarebehLinje OF VareBehBestHode NO-LOCK"
                     + (IF NUM-ENTRIES(cLevNr,"|") > 1 THEN 
                         " WHERE LOOKUP(STRING(VareBehLinje.LevNr),'" + cLevNr + "','|') > 0"
                        ELSE IF cLevNr NE "" THEN
                         " WHERE VareBehLinje.levnr = " + cLevNr
                        ELSE "")
                     + ",FIRST ArtBas OF VarebehLinje NO-LOCK"
                     + ",FIRST Varemerke OF ArtBas NO-LOCK OUTER-JOIN" 
                     + ",FIRST Butiker WHERE Butiker.Butik = VareBehBestLinje.BestiltButikkNr NO-LOCK" 
                     + ",FIRST BestHode NO-LOCK OF VareBehBestHode WHERE LOOKUP(STRING(BestHode.BestStat),'" + cBestStatus + "','|') > 0"
                     + cSortString
                     ).
                                                             
hQuery:QUERY-OPEN().
hQuery:GET-FIRST().

IF NOT AVAIL VareBehBestLinje THEN DO:
  ocReturn = "Ingen rader for søkekriterium".
  DELETE OBJECT hQuery NO-ERROR.
  RETURN.
END.

cFileName = SESSION:TEMP-DIR + STRING(VareBehBestLinje.VarebehNr) + "_" + STRING(VareBehBestLinje.BestiltButikkNr) + "_" + STRING(YEAR(TODAY)) + STRING(MONTH(TODAY),"99") + STRING(DAY(TODAY)) + STRING(TIME) + ".txt".

OUTPUT TO VALUE(cFileName).

PUT UNFORMATTED "Butikknr"               + "~t" +
                "Butikknavn"             + "~t" +
                "PRS art.nr"             + "~t"     
                "Lev Artnr"              + "~t"
                "Artikkelnavn"           + "~t"
                "Fargetekst"             + "~t"         
                "Leverandør"             + "~t"
                "Varemerke"              + "~t"        
                "Nto.innpris (varekost)" + "~t"        
                "DB%"                    + "~t"
                "Veil uts.   Pris"       + "~t"        
                "Str"                    + "~t"  
                "Lev.uke"                + "~t"
                "Antall"                 + "~t" 
                "Sum ordre"  
                SKIP.

REPEAT WHILE NOT hQuery:QUERY-OFF-END:

  IF cButNr NE "" THEN DO:
    IF VarebehLinje.Hg NE iPrevHg AND iPrevHg NE 0 THEN DO:
      PUT UNFORMATTED "SUM H.gr" + "~t~t" 
                      STRING(iPrevHg) + "~t~t"  
                      cPrevHgBeskr + FILL("~t",10) 
                      fTotHg SKIP.
      ASSIGN fTotHg = 0
             iCount  = iCount + 1.
    END.
    IF VarebehLinje.AvdelingNr NE iPrevAvdNr AND iPrevAvdNr NE 0 THEN DO:
      PUT UNFORMATTED "SUM Avd" + "~t~t" 
                      STRING(iPrevAvdNr) + "~t~t"  
                      cPrevAvdNavn + FILL("~t",10) 
                      fTotAvd SKIP.
      ASSIGN fTotAvd = 0
             iCount  = iCount + 1.
    END.

  END.
  ELSE IF cLevNr NE "" THEN DO: /* Leverandørsortert */
    IF VarebehLinje.levnr NE iPrevLevnr AND iPrevLevnr NE 0 THEN DO:
      PUT UNFORMATTED "SUM Levnr" + "~t" 
                      STRING(iPrevLevnr) + "~t~t~t~t~t"  
                      cPrevLevNavn + FILL("~t",8) 
                      fTotLev SKIP.
      ASSIGN fTotLev = 0
             iCount  = iCount + 1.
    END.
  END.

  iDumpCount = iDumpCount + 1.

  ASSIGN iCount     = iCount + 1
         iSumAntStr = VareBehBestLinje.Bestilt
         fTotAvd    = fTotAvd + VareBehLinje.VareKost * iSumAntStr
         fTotHg     = fTotHg  + VareBehLinje.VareKost * iSumAntStr
         fTotLev    = fTotLev + VareBehLinje.VareKost * iSumAntStr
         fGrandTot  = fGrandTot + VareBehLinje.VareKost * iSumAntStr
         .

  AccumUke().
  AccumLev().
  AccumBut().
  AccumAvd().
  AccumHg().

  PUT UNFORMATTED
       STRING(Butiker.Butik) + "~t" + 
       Butiker.ButNamn       + "~t" +
      STRING(VarebehLinje.Artikkelnr) + "~t" +
      (IF VareBehLinje.LevKod NE ? THEN VareBehLinje.LevKod ELSE "") + "~t" +
      (IF VareBehLinje.Beskr NE ? THEN (IF VareBehLinje.Beskr BEGINS "+" THEN " " ELSE "") + VareBehLinje.Beskr ELSE "") + "~t" +
      (IF VareBehLinje.LevFargKod NE ? THEN VareBehLinje.LevFargKod ELSE "") + "~t" +
      (IF VareBehLinje.levnamn NE ? THEN VareBehLinje.levnamn ELSE "") + "~t" +
      (IF Varemerke.Beskrivelse NE ? THEN Varemerke.Beskrivelse ELSE "") + "~t" +
      (IF VareBehLinje.Varekost NE ? THEN STRING(VareBehLinje.Varekost) ELSE "") + "~t" +
      (IF VareBehLinje.DB% NE ? THEN STRING(VareBehLinje.DB%) ELSE "") + "~t" +
      (IF VareBehLinje.Pris NE ? THEN STRING(VareBehLinje.Pris) ELSE "") + "~t" +
      (IF StrKonv.Storl NE ? AND TRIM(StrKonv.Storl) NE "1" AND NOT bInndeling THEN " " + StrKonv.Storl 
       ELSE "") + "~t" +
      STRING(VareBehBestHode.Levuke) + "~t" +
      STRING(VareBehBestLinje.Bestilt) + "~t" +
      STRING(VareBehBestLinje.Bestilt * VareBehLinje.VareKost) +
      (IF cButNr = "" AND cLevNr = "" THEN
        "~t" +
        (IF VareBehBestLinje.EDato NE ? THEN STRING(VareBehBestLinje.EDato) ELSE "") + "~t" +
        STRING(VarebehLinje.Vg) + " - " + VarebehLinje.VgBeskr + "~t" +
        STRING(VarebehLinje.Hg) + " - " + VarebehLinje.HgBeskr + "~t" +
        STRING(VarebehLinje.AvdelingNr) + " - " + VarebehLinje.AvdelingNavn + "~t" +
        STRING(Butiker.Butik) + "~t" + 
        Butiker.ButNamn + "~t" +
        VarebehLinje.LinjeMerknad + "~t" +
        (IF VarebehLinje.KjedeInnkPris NE ? THEN STRING(VarebehLinje.KjedeInnkPris) ELSE "") + "~t" +
        (IF fKjedeDB% NE ? THEN STRING(fKjedeDB%) ELSE "")
       ELSE "")
      SKIP.
  

  ASSIGN iPrevAvdNr   = VarebehLinje.AvdelingNr
         iPrevHg      = VarebehLinje.Hg
         iPrevLevnr   = VarebehLinje.Levnr
         cPrevAvdNavn = VarebehLinje.AvdelingNavn
         cPrevHgbeskr = VarebehLinje.HgBeskr
         cPrevLevNavn = VarebehLinje.Levnamn
         .
  hQuery:GET-NEXT().
END.

IF cButNr NE "" THEN DO:
  PUT UNFORMATTED "SUM H.gr" + "~t~t" 
                  STRING(iPrevHg) + "~t~t"  
                  cPrevHgBeskr + FILL("~t",10) 
                  fTotHg SKIP.

  
  PUT UNFORMATTED "SUM Avd" + "~t~t" 
                  STRING(iPrevAvdNr) + "~t~t"  
                  cPrevAvdNavn + FILL("~t",10) 
                  fTotAvd SKIP.

END.
ELSE IF cLevNr NE "" THEN DO:
  PUT UNFORMATTED "SUM Lev" + "~t" 
                  STRING(iPrevLevnr) + "~t~t~t~t~t"  
                  cPrevLevNavn + FILL("~t",8) 
                  fTotLev SKIP.
  PUT SKIP(1).
END.

PUT UNFORMATTED "TOTALT" + FILL("~t",14) 
                fGrandTot SKIP(2).

PUT UNFORMATTED "SUM pr uke:" SKIP.
FOR EACH ttTotUke BY ttTotUke.iUkeNr:
  PUT UNFORMATTED STRING(ttTotUke.iUkeNr) + FILL("~t",8) + STRING(ttTotUke.fTotUke) SKIP.
END.
PUT UNFORMATTED "TOTALT" + FILL("~t",8) 
                fGrandTot SKIP(2).

PUT UNFORMATTED "SUM pr lev:" SKIP.
FOR EACH ttTotLev BY ttTotLev.iLevNr:
  PUT UNFORMATTED STRING(ttTotLev.iLevNr) + "~t~t~t~t~t~t" + STRING(ttTotLev.cLevNavn) +  "~t~t" + STRING(ttTotLev.fTotLev) SKIP.
END.
PUT UNFORMATTED "TOTALT" + FILL("~t",8) 
                fGrandTot SKIP(2).

PUT UNFORMATTED "SUM pr butikk:" SKIP.
FOR EACH ttTotBut BY ttTotBut.iButNr:
  PUT UNFORMATTED STRING(ttTotBut.iButNr) + "~t" + STRING(ttTotBut.cButNavn) +  "~t~t~t~t~t~t~t" + STRING(ttTotBut.fTotBut) SKIP.
END.
PUT UNFORMATTED "TOTALT" + FILL("~t",8) 
                fGrandTot SKIP(2).

IF cLevNr NE "" THEN DO:
  PUT UNFORMATTED "SUM pr avd:" SKIP.
  FOR EACH ttTotAvd BY ttTotAvd.iAvdNr:
    PUT UNFORMATTED STRING(ttTotAvd.iAvdNr) + "~t~t~t~t" + STRING(ttTotAvd.cAvdNavn) +  "~t~t~t~t" + STRING(ttTotAvd.fTotAvd) SKIP.
  END.
  PUT UNFORMATTED "TOTALT" + FILL("~t",8) 
                  fGrandTot SKIP(2).

  PUT UNFORMATTED "SUM pr hg:" SKIP.
  FOR EACH ttTotHg BY ttTotHg.iHgNr:
    PUT UNFORMATTED STRING(ttTotHg.iHgNr) + "~t~t~t~t" + STRING(ttTotHg.cHgNavn) +  "~t~t~t~t" + STRING(ttTotHg.fTotHg) SKIP.
  END.
  PUT UNFORMATTED "TOTALT" + FILL("~t",8) 
                  fGrandTot SKIP(2).
END.

OUTPUT CLOSE.
DELETE OBJECT hQuery.

IF iCount = 1 THEN
  ocReturn = "Varehåndteringsboken inneholder ingen bestillinger".
ELSE IF iDumpCount > 65000 THEN
  ocReturn = "Dump av varehåndteringsboken inneholder mer enn 65000 linjer og vil ikke bli forsøkt åpnet i Excel." + CHR(10) +
             "Filnavn: " + cFileName.
         
IF ocReturn = "" THEN
  ASSIGN ocReturn = cFileName + "|" + STRING(iCount)
         obOK     = YES.

FUNCTION AccumUke RETURNS LOG:

  IF VareBehBestLinje.Bestilt NE 0 THEN DO:
    FIND FIRST ttTotUke
         WHERE ttTotUke.iUkeNr = VareBehBestHode.Levuke
         NO-ERROR.
    IF NOT AVAIL ttTotUke THEN DO:
      CREATE ttTotUke.
      ttTotUke.iUkeNr = VareBehBestHode.Levuke.
    END.
    ttTotUke.fTotUke = ttTotUke.fTotUke + VareBehBestLinje.Bestilt * VareBehLinje.Varekost.
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
  ttTotLev.fTotLev = ttTotLev.fTotLev + iSumAntStr * VareBehLinje.Varekost.

END FUNCTION.

FUNCTION AccumBut RETURNS LOG:

  FIND FIRST ttTotBut
       WHERE ttTotBut.iButNr = VareBehBestLinje.BestiltButikkNr
       NO-ERROR.
  IF NOT AVAIL ttTotBut THEN DO:
    CREATE ttTotBut.
    ASSIGN ttTotBut.iButNr   = VareBehBestLinje.BestiltButikkNr
           ttTotBut.cButNavn = Butiker.Butnamn.
  END.
  ttTotBut.fTotBut = ttTotBut.fTotBut + iSumAntStr * VareBehLinje.Varekost.

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
  ttTotAvd.fTotAvd = ttTotAvd.fTotAvd + iSumAntStr * VareBehLinje.Varekost.

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
  ttTotHg.fTotHg = ttTotHg.fTotHg + iSumAntStr * VareBehLinje.Varekost.

END FUNCTION.
