/* Utskrift av ordrebekreftelse fra varehåndteringsbok, messeregistrering
   Parametere: Input: Varebehandlingsnr,butikknr,levnr (butikknr og levnr trenger ikke være med)
               Output (ocReturn): Filnavn
               Merk at antall parametere styrer layout, summeringer, etc
               ENTRY(1,icParam): VarebehNr
               ENTRY(2,icParam): Butikknr 
               ENTRY(3,icParam): Leverandørnr (dersom ordrebekreftelse er for leverandør) 
               ENTRY(4,icParam): Evt annen sortering enn default for butikkbekr 
            -  ENTRY(5,icParam) = "yes": med EAN kode 
            -  ENTRY(5,icParam) = "forslag": Ordreforslag (ikke bestillinger) 
               ENTRY(6,icParam): Dato for siste endring 
               ENTRY(7,icParam): Liste over rowid'er 
               ENTRY(8,icParam) = "dump": Ikke lag delsummer. Feltene (+ noen til) skal bare eksporteres 
               ENTRY(9,icParam) = "kjedepris": Benytt kjedepris i stedet for varekost
   Opprettet: 04.01.05 av BHa                          
   Endret:    17.08.05 av BHa
              - Beregner størrelser ut fra innhold i inndelinger
              28.02.06 av BHa
              - Mulig å hente kun artikler med registrert kjede inn-pris
              03.05.07 av BHa
              - Tar bort oppslag mot strekkkode
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
DEF VAR fGrandTotKjede  AS DEC    NO-UNDO.
DEF VAR fKjedeDB%       AS DEC    NO-UNDO.
DEF VAR bKjedePris      AS LOG    NO-UNDO.
DEF VAR bOrdreForslag   AS LOG    NO-UNDO.
DEF VAR cLevNr          AS CHAR   NO-UNDO.    /* Hvis entry(3,icParam) = "vis_butnr" blankes denne men butnr legges på filen (for rapportering flere but */
DEF VAR cButNr          AS CHAR   NO-UNDO.
DEF VAR bVisButnr       AS LOG    NO-UNDO.

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

cButNr = ENTRY(2,icParam).

cLevNr = ENTRY(3,icParam).
IF cLevNr NE "" THEN
  bVisButnr = YES.
IF cLevNr = "vis_butnr" THEN
  cLevNr = "".

IF ENTRY(4,icParam) NE "" THEN
  cSortString = " BY " + ENTRY(4,icParam).
ELSE
  cSortString = " BY VarebehLinje.AvdelingNr BY VarebehLinje.Hg".

cSortString = cSortString + " BY VarebehLinje.Artikkelnr BY VarebehLinjeTrans.SeqNr".

IF ENTRY(5,icParam) = "forslag" THEN
  bOrdreForslag = TRUE.
ELSE IF ENTRY(5,icParam) NE "" THEN
  bEAN = TRUE.
IF NUM-ENTRIES(icParam) GE 8 THEN
  ASSIGN cRowIdList  = REPLACE(ENTRY(7,icParam),"|",",")
         bDumpReport = ENTRY(8,icParam) = "dump".
IF NUM-ENTRIES(icParam) GE 9 AND ENTRY(9,icParam) = "kjedepris" THEN
  bKjedePris = YES.

FIND FIRST VarebehHode WHERE VarebehHode.VarebehNr = DEC(ENTRY(1,icParam)) NO-LOCK.

CREATE QUERY hQuery.
hQuery:SET-BUFFERS(BUFFER VareBehLinjeTrans:HANDLE
/*                   ,BUFFER Strekkode:HANDLE */
                  ,BUFFER StrKonv:HANDLE
                  ,BUFFER VarebehLinje:HANDLE
                  ,BUFFER ArtBas:HANDLE
                  ,BUFFER Varemerke:HANDLE
                  ,BUFFER Butiker:HANDLE
                  ).
hQuery:QUERY-PREPARE("FOR EACH VareBehLinjeTrans NO-LOCK " 
                     + " WHERE VareBehLinjeTrans.VarebehNr = " + ENTRY(1,icParam) 
                     + " AND VarebehLinjeTrans.ArtikkelNr > 0"
                     + (IF cButNr NE "" THEN
                         " AND VareBehLinjeTrans.ButikkNr = " + cButNr
                        ELSE "")
                     + (IF cRowIdList NE "" THEN
                         " AND CAN-DO('" + cRowIdList + "',STRING(ROWID(VareBehLinjeTrans)))"
                        ELSE "")
                     + " AND RegistrertBestilling"
/*                      + " AND (Bestilt1 NE 0 OR Bestilt2 NE 0 OR Bestilt3 NE 0 OR Bestilt4 NE 0)" */
                     + (IF bOrdreForslag THEN 
                          " AND NOT GodkjentBestilling"
                        ELSE 
                         " AND GodkjentBestilling")
                     + (IF cLevNr NE "" THEN 
                          " USE-INDEX RegistrertBestilling"
                        ELSE "")
/*                     + " AND ButikkNr = 1" */
/*                      + ",FIRST Strekkode OF VareBehLinjeTrans NO-LOCK OUTER-JOIN" */
/*                      + ",FIRST StrKonv OF Strekkode NO-LOCK OUTER-JOIN"           */
                     + ",FIRST StrKonv OF VareBehLinjeTrans NO-LOCK OUTER-JOIN"
                     + ",FIRST VarebehLinje OF VarebehLinjeTrans NO-LOCK"
                     + (IF cLevNr NE "" THEN
                         " WHERE VareBehLinje.levnr = " + cLevNr
                        ELSE "")
                     + (IF bKjedePris THEN 
                          (IF cLevNr NE "" THEN " AND " ELSE " WHERE ") + "VarebehLinje.KjedeInnkPris > 0"
                        ELSE "")                          
                     + ",FIRST ArtBas OF VarebehLinje NO-LOCK"
                     + ",FIRST Varemerke OF ArtBas NO-LOCK OUTER-JOIN" 
                     + ",FIRST Butiker WHERE Butiker.Butik = VareBehLinjeTrans.ButikkNr NO-LOCK" 
                     + cSortString
                     ).

hQuery:QUERY-OPEN().
hQuery:GET-FIRST().

IF NOT AVAIL VarebehLinjeTrans THEN DO:
  ocReturn = "Varebehandlingsbok ikke funnet".
  RETURN.
END.
IF bVisButnr THEN DO:
  FIND FIRST Butiker NO-LOCK
       WHERE Butiker.Butik = INT(cButNr)
       NO-ERROR.
  IF NOT AVAIL Butiker THEN DO:
    ocReturn = "Finner ikke butikk: " + cButNr.
    RETURN.
  END.
END.

cFileName       = SESSION:TEMP-DIR + STRING(VarebehLinjeTrans.VarebehNr) + "_" + STRING(VarebehLinjeTrans.ButikkNr) + "_" + STRING(YEAR(TODAY)) + STRING(MONTH(TODAY),"99") + STRING(DAY(TODAY)) + STRING(TIME) + ".txt".

OUTPUT TO VALUE(cFileName).

IF bVisButnr THEN
  PUT UNFORMATTED "Butikk:~t" cButNr "~t" Butiker.ButNamn SKIP(1). 

PUT UNFORMATTED (IF cLevNr NE "" THEN
                  "Butikknr"               + "~t" +
                  "Butikknavn"             + "~t"
                 ELSE "") +
                "PRS art.nr"      + "~t"     
                "Lev Artnr"              + "~t"
                "Artikkelnavn"           + "~t"
                "Fargetekst"             + "~t"         
                "Leverandør"             + "~t"
                "Varemerke"              + "~t"        
                "Nto forh.   Pris"       + "~t"   
                "DB%"                    + "~t"
                "Nto supl.   Pris"       + "~t"        
                "Veil uts.   Pris"       + "~t"        
                "Kamp.     Pris"         + "~t"        
                (IF bEAN THEN
                  "EAN kode"               + "~t"       
                 ELSE "")
                "Str/        Innd."           + "~t"  
                "Lev uke 1"              + "~t"
                "Antall"                 + "~t" 
                "Lev uke 2"              + "~t"
                "Antall"                 + "~t" 
                "Lev uke 3"              + "~t"
                "Antall"                 + "~t" 
                "Lev uke 4"              + "~t"
                "Antall"                 + "~t" 
                "Tot.ant"                + "~t" 
                "Sum ordre"  +
                (IF cButNr = "" AND cLevNr = "" THEN
                  "~t" +
                  "Endret"               + "~t" +
                  "Varegruppe"           + "~t" +
                  "Hovedgruppe"          + "~t" +
                  "Avdeling"             + "~t" +
                  "Butikknr"             + "~t" +
                  "Butikknavn"           + "~t" +
                  "Merknad"              + "~t" +
                  "Kj.innpris"           + "~t" +
                  "DB kjede"
                 ELSE "")
                SKIP.

REPEAT WHILE NOT hQuery:QUERY-OFF-END:

  IF cButNr NE "" AND ENTRY(4,icParam) = "" THEN DO:
    IF VarebehLinje.Hg NE iPrevHg AND iPrevHg NE 0 THEN DO:
      PUT UNFORMATTED "SUM H.gr" + "~t" 
                      STRING(iPrevHg) + "~t"  
                      cPrevHgBeskr + FILL("~t",IF bEAN THEN 20 ELSE 19) 
                      fTotHg SKIP.
      ASSIGN fTotHg = 0
             iCount  = iCount + 1.
    END.
    IF VarebehLinje.AvdelingNr NE iPrevAvdNr AND iPrevAvdNr NE 0 THEN DO:
      PUT UNFORMATTED "SUM Avd" + "~t" 
                      STRING(iPrevAvdNr) + "~t"  
                      cPrevAvdNavn + FILL("~t",IF bEAN THEN 20 ELSE 19) 
                      fTotAvd SKIP.
      ASSIGN fTotAvd = 0
             iCount  = iCount + 1.
    END.

  END.
  ELSE IF ENTRY(4,icParam) NE "" THEN DO: /* Leverandørsortert */
    IF VarebehLinje.levnr NE iPrevLevnr AND iPrevLevnr NE 0 THEN DO:
      PUT UNFORMATTED "SUM Levnr" + "~t" 
                      STRING(iPrevLevnr) + "~t"  
                      cPrevLevNavn + FILL("~t",IF bEAN THEN 20 ELSE 19) 
                      fTotLev SKIP.
      ASSIGN fTotLev = 0
             iCount  = iCount + 1.
    END.
  END.

  iDumpCount = iDumpCount + 1.
  RUN Inndeling.

  ASSIGN iCount = iCount + 1
         iSumAntStr = (VareBehLinjeTrans.Bestilt1 + VareBehLinjeTrans.Bestilt2 + VareBehLinjeTrans.Bestilt3 + VareBehLinjeTrans.Bestilt4) * iAntFord
         fTotAvd    = fTotAvd + VareBehLinje.VareKost * iSumAntStr
         fTotHg     = fTotHg  + VareBehLinje.VareKost * iSumAntStr
         fTotLev    = fTotLev + VareBehLinje.VareKost * iSumAntStr
         fGrandTot  = fGrandTot + VareBehLinje.VareKost * iSumAntStr
         fGrandTotKjede = fGrandTotKjede + (IF VarebehLinje.KjedeInnkPris NE 0 THEN VarebehLinje.KjedeInnkPris ELSE VareBehLinje.VareKost) * iSumAntStr
         .

  AccumUke().
  AccumLev().
  AccumBut().
  AccumAvd().
  AccumHg().

  IF cButNr = "" AND cLevNr = "" THEN DO:
  /*
    FIND FIRST VarebokLinje NO-LOCK
         WHERE VarebokLinje.VarebokNr = VarebehHode.kilde
           AND VarebokLinje.ArtikkelNr = VarebokLinje.ArtikkelNr
         NO-ERROR.
    IF AVAIL VarebokLinje THEN
      fKjedeDB% = (VareBehLinje.VareKost - VareBokLinje.KjedeInnkPris) / VareBokLinje.KjedeInnkPris * 100.
      */
    IF VarebehLinje.KjedeInnkPris NE 0 THEN
      fKjedeDB% = (VareBehLinje.VareKost - VareBehLinje.KjedeInnkPris) / VarebehLinje.KjedeInnkPris * 100.
    ELSE
      fKjedeDB% = 0.
  END.

  IF NOT (bInndeling AND bDumpReport) THEN
    PUT UNFORMATTED
      (IF cLevNr NE "" THEN
         STRING(Butiker.Butik) + "~t" + 
         Butiker.ButNamn       + "~t"
       ELSE "") +
      STRING(VarebehLinjeTrans.Artikkelnr) + "~t" +
      (IF VareBehLinje.LevKod NE ? THEN VareBehLinje.LevKod ELSE "") + "~t" +
      (IF VareBehLinje.Beskr NE ? THEN (IF VareBehLinje.Beskr BEGINS "+" THEN " " ELSE "") + VareBehLinje.Beskr ELSE "") + "~t" +
      (IF VareBehLinje.LevFargKod NE ? THEN VareBehLinje.LevFargKod ELSE "") + "~t" +
      (IF VareBehLinje.levnamn NE ? THEN VareBehLinje.levnamn ELSE "") + "~t" +
      (IF Varemerke.Beskrivelse NE ? THEN Varemerke.Beskrivelse ELSE "") + "~t" +
      (IF VareBehLinje.VareKost NE ? THEN STRING(VareBehLinje.VareKost) ELSE "") + "~t" +
      (IF VareBehLinje.DB% NE ? THEN STRING(VareBehLinje.DB%) ELSE "") + "~t" +
      (IF VareBehLinje.supVarekost NE ? THEN STRING(VareBehLinje.supVarekost) ELSE "") + "~t" +
      (IF VareBehLinje.Pris NE ? THEN STRING(VareBehLinje.Pris) ELSE "") + "~t" +
      (IF VareBehLinje.KampanjePris NE ? THEN STRING(VareBehLinje.KampanjePris) ELSE "") + "~t" +
      (IF bEAN THEN
        (IF VareBehLinjeTrans.Kode NE ? THEN 
          (IF cButNr = "" OR bInndeling THEN VareBehLinjeTrans.Kode
           ELSE EAN13BC(VareBehLinjeTrans.Kode))
         ELSE "") + "~t"
       ELSE "") +
      (IF StrKonv.Storl NE ? AND TRIM(StrKonv.Storl) NE "1" AND NOT bInndeling THEN " " + StrKonv.Storl 
       ELSE IF bInndeling AND VareBehLinjeTrans.Kode NE ? THEN VareBehLinjeTrans.Kode
       ELSE "") + "~t" +
      STRING(VareBehLinjeTrans.Levuke1) + "~t" +
      STRING(VareBehLinjeTrans.Bestilt1 * iAntFord) + "~t" +
      STRING(VareBehLinjeTrans.Levuke2) + "~t" +
      STRING(VareBehLinjeTrans.Bestilt2 * iAntFord) + "~t" +
      STRING(VareBehLinjeTrans.Levuke3) + "~t" +
      STRING(VareBehLinjeTrans.Bestilt3 * iAntFord) + "~t" +
      STRING(VareBehLinjeTrans.Levuke4) + "~t" +
      STRING(VareBehLinjeTrans.Bestilt4 * iAntFord) + "~t" +
      STRING(iSumAntStr) + "~t" +
      STRING(iSumAntStr * VareBehLinje.VareKost) +
      (IF cButNr = "" AND cLevNr = "" THEN
        "~t" +
        (IF VarebehLinjeTrans.EDato NE ? THEN STRING(VarebehLinjeTrans.EDato) ELSE "") + "~t" +
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
  
  IF bInndeling THEN DO:
    IF NOT bDumpReport THEN
      PUT UNFORMATTED (IF cLevNr NE "" THEN "~t~t" ELSE "") + "~t" +
                      "Størrelser:"            + "~t" +   
                      (IF LENGTH(cStrList) > 27 THEN
                        cMinStr + " - " + cMaxStr
                       ELSE cStrList)          + "~t" +
                      "Fordeling:"             + "~t"
                      cSortFordList            + "~t~t~t~t~t~t~t~t~t" +     
                      STRING(VareBehLinjeTrans.Bestilt1) + "~t~t" +
                      STRING(VareBehLinjeTrans.Bestilt2) + "~t~t" +
                      STRING(VareBehLinjeTrans.Bestilt3) + "~t~t" +
                      STRING(VareBehLinjeTrans.Bestilt4)
                      SKIP.
    ELSE DO ix = 1 TO NUM-ENTRIES(cStrList):
      PUT UNFORMATTED
        (IF cLevNr NE "" THEN
           STRING(Butiker.Butik) + "~t" + 
           Butiker.ButNamn       + "~t"
         ELSE "") +
        STRING(VarebehLinjeTrans.Artikkelnr) + "~t" +
        (IF VareBehLinje.LevKod NE ? THEN VareBehLinje.LevKod ELSE "") + "~t" +
        (IF VareBehLinje.Beskr NE ? THEN (IF VareBehLinje.Beskr BEGINS "+" THEN " " ELSE "") + VareBehLinje.Beskr ELSE "") + "~t" +
        (IF VareBehLinje.LevFargKod NE ? THEN VareBehLinje.LevFargKod ELSE "") + "~t" +
        (IF VareBehLinje.levnamn NE ? THEN VareBehLinje.levnamn ELSE "") + "~t" +
        (IF Varemerke.Beskrivelse NE ? THEN Varemerke.Beskrivelse ELSE "") + "~t" +
        (IF VareBehLinje.VareKost NE ? THEN STRING(VareBehLinje.VareKost) ELSE "") + "~t" +
        (IF VareBehLinje.DB% NE ? THEN STRING(VareBehLinje.DB%) ELSE "") + "~t" +
        (IF VareBehLinje.supVarekost NE ? THEN STRING(VareBehLinje.supVarekost) ELSE "") + "~t" +
        (IF VareBehLinje.Pris NE ? THEN STRING(VareBehLinje.Pris) ELSE "") + "~t" +
        (IF VareBehLinje.KampanjePris NE ? THEN STRING(VareBehLinje.KampanjePris) ELSE "") + "~t" +
        (IF bEAN THEN
          (IF VareBehLinjeTrans.Kode NE ? THEN 
            (IF cButNr = "" OR bInndeling THEN VareBehLinjeTrans.Kode
             ELSE EAN13BC(VareBehLinjeTrans.Kode))
           ELSE "") + "~t"
         ELSE "") +
        ENTRY(ix,cStrList) + "~t" +
        STRING(VareBehLinjeTrans.Levuke1) + "~t" +
        STRING(VareBehLinjeTrans.Bestilt1 * INT(ENTRY(ix,cSortFordList))) + "~t" +
        STRING(VareBehLinjeTrans.Levuke2) + "~t" +
        STRING(VareBehLinjeTrans.Bestilt2 * INT(ENTRY(ix,cSortFordList))) + "~t" +
        STRING(VareBehLinjeTrans.Levuke3) + "~t" +
        STRING(VareBehLinjeTrans.Bestilt3 * INT(ENTRY(ix,cSortFordList))) + "~t" +
        STRING(VareBehLinjeTrans.Levuke4) + "~t" +
        STRING(VareBehLinjeTrans.Bestilt4 * INT(ENTRY(ix,cSortFordList))) + "~t" +
        STRING((VareBehLinjeTrans.Bestilt1 + VareBehLinjeTrans.Bestilt2 + VareBehLinjeTrans.Bestilt3 + VareBehLinjeTrans.Bestilt4) * INT(ENTRY(ix,cSortFordList))) + "~t" +
        STRING((VareBehLinjeTrans.Bestilt1 + VareBehLinjeTrans.Bestilt2 + VareBehLinjeTrans.Bestilt3 + VareBehLinjeTrans.Bestilt4) * INT(ENTRY(ix,cSortFordList)) * VareBehLinje.VareKost) +
        (IF cButNr = "" AND cLevNr = "" THEN
          "~t" +
          (IF VarebehLinjeTrans.EDato NE ? THEN STRING(VarebehLinjeTrans.EDato) ELSE "") + "~t" +
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
        iDumpCount = iDumpCount + 1.
    END.
  END.

  ASSIGN iPrevAvdNr   = VarebehLinje.AvdelingNr
         iPrevHg      = VarebehLinje.Hg
         iPrevLevnr   = VarebehLinje.Levnr
         cPrevAvdNavn = VarebehLinje.AvdelingNavn
         cPrevHgbeskr = VarebehLinje.HgBeskr
         cPrevLevNavn = VarebehLinje.Levnamn
         .
  hQuery:GET-NEXT().
END.

IF cButNr NE "" AND ENTRY(4,icParam) = "" THEN DO:
  PUT UNFORMATTED "SUM H.gr" + "~t" 
                  STRING(iPrevHg) + "~t"  
                  cPrevHgBeskr + FILL("~t",IF bEAN THEN 20 ELSE 19) 
                  fTotHg SKIP.
  
  PUT UNFORMATTED "SUM Avd" + "~t" 
                  STRING(iPrevAvdNr) + "~t"  
                  cPrevAvdNavn + FILL("~t",IF bEAN THEN 20 ELSE 19) 
                  fTotAvd SKIP.

END.
ELSE IF ENTRY(4,icParam) NE "" THEN DO:
  PUT UNFORMATTED "SUM Lev" + "~t" 
                  STRING(iPrevLevnr) + "~t"  
                  cPrevLevNavn + FILL("~t",IF bEAN THEN 20 ELSE 19) 
                  fTotLev SKIP.
  PUT SKIP(1).
END.
ELSE IF cLevNr NE "" THEN PUT SKIP(2).

IF cButNr NE "" OR cLevNr NE "" THEN
  PUT UNFORMATTED "TOTALT" + FILL("~t",IF cButNr NE "" THEN (IF bEAN THEN 22 ELSE 21) ELSE (IF bEAN THEN 24 ELSE 23)) 
                  fGrandTot SKIP(2).
ELSE PUT SKIP(4).

PUT UNFORMATTED "SUM pr uke:" SKIP.
FOR EACH ttTotUke BY ttTotUke.iUkeNr:
  PUT UNFORMATTED STRING(ttTotUke.iUkeNr) + FILL("~t",IF cButNr NE "" THEN 6 ELSE 8) + STRING(ttTotUke.fTotUke) SKIP.
END.
PUT UNFORMATTED "TOTALT" + FILL("~t",IF cButNr NE "" THEN 6 ELSE 8) 
                fGrandTot SKIP(2).

IF cLevNr = "" OR ENTRY(4,icParam) NE "" THEN DO:
  PUT UNFORMATTED "SUM pr lev:" SKIP.
  FOR EACH ttTotLev BY ttTotLev.cLevNavn:
    PUT UNFORMATTED "~t~t~t~t" + STRING(ttTotLev.cLevNavn) +  "~t~t" + STRING(ttTotLev.fTotLev) SKIP.
  END.
  PUT UNFORMATTED "TOTALT" + FILL("~t",6) 
                  fGrandTot SKIP(2).
END.
IF cLevNr NE "" OR cButNr = "" THEN DO:
  PUT UNFORMATTED "SUM pr butikk:" SKIP.
  FOR EACH ttTotBut BY ttTotBut.cButNavn:
    PUT UNFORMATTED "~t" + STRING(ttTotBut.cButNavn) +  "~t~t~t~t~t~t~t" + STRING(ttTotBut.fTotBut) SKIP.
  END.
  PUT UNFORMATTED "TOTALT" + FILL("~t",8) 
                  fGrandTot SKIP(2).
END.
IF ENTRY(4,icParam) NE "" THEN DO:
  PUT UNFORMATTED "SUM pr avd:" SKIP.
  FOR EACH ttTotAvd BY ttTotAvd.cAvdNavn:
    PUT UNFORMATTED "~t~t" + STRING(ttTotAvd.cAvdNavn) +  "~t~t~t~t" + STRING(ttTotAvd.fTotAvd) SKIP.
  END.
  PUT UNFORMATTED "TOTALT" + FILL("~t",6) 
                  fGrandTot SKIP(2).

  PUT UNFORMATTED "SUM pr hg:" SKIP.
  FOR EACH ttTotHg BY ttTotHg.cHgNavn:
    PUT UNFORMATTED "~t~t" + STRING(ttTotHg.cHgNavn) +  "~t~t~t~t" + STRING(ttTotHg.fTotHg) SKIP.
  END.
  PUT UNFORMATTED "TOTALT" + FILL("~t",6) 
                  fGrandTot SKIP(2).
END.
IF bKjedePris THEN 
  PUT UNFORMATTED "TOT.Kjede innpr" + FILL("~t",6) fGrandTotKjede SKIP(1)
                  "TOT.Kjede margin" + FILL("~t",6) fGrandTot - fGrandTotKjede
      .

IF cButNr NE "" OR cLevNr NE "" THEN DO:
  IF cButNr NE "" THEN
    FIND LAST PrintLogg 
         WHERE PrintLogg.LoggType       = "but_ordrebekr_messe" 
           AND PrintLogg.NumLoggNokkel  = DEC(cButNr) 
           AND PrintLogg.CharLoggNokkel = ENTRY(1,icParam) 
         NO-LOCK NO-ERROR.
  ELSE
    FIND LAST PrintLogg 
         WHERE PrintLogg.LoggType       = "lev_ordrebekr_messe" 
           AND PrintLogg.NumLoggNokkel  = DEC(cLevNr) 
           AND PrintLogg.CharLoggNokkel = ENTRY(1,icParam) 
         NO-LOCK NO-ERROR.
  IF AVAIL PrintLogg THEN DO:
    hQuery:QUERY-CLOSE().
    hQuery:QUERY-PREPARE("FOR EACH VareBehLinjeTrans NO-LOCK " 
                         + " WHERE VareBehLinjeTrans.VarebehNr = " + ENTRY(1,icParam) 
                         + (IF cButNr NE "" THEN
                             " AND VareBehLinjeTrans.ButikkNr = " + cButNr
                            ELSE "")
                         + "   AND VareBehLinjeTrans.EDato NE ?"
                         + "   AND (VareBehLinjeTrans.EDato > DATE('" + STRING(PrintLogg.RegistrertDato) + "') "
                         + "        OR (VareBehLinjeTrans.EDato = DATE('" + STRING(PrintLogg.RegistrertDato) + "')"
                         + "            AND VareBehLinjeTrans.ETid > " + STRING(PrintLogg.RegistrertTid) + "))"
/*                          + ",FIRST Strekkode OF VareBehLinjeTrans NO-LOCK OUTER-JOIN" */
/*                          + ",FIRST StrKonv OF Strekkode NO-LOCK OUTER-JOIN" */
                         + ",FIRST StrKonv OF VareBehLinjeTrans NO-LOCK OUTER-JOIN"
                         + ",FIRST VarebehLinje OF VarebehLinjeTrans NO-LOCK"
                         + (IF cLevNr NE "" THEN
                             " WHERE VareBehLinje.levnr = " + cLevNr
                            ELSE "")
                         + ",FIRST ArtBas OF VarebehLinje NO-LOCK"
                         + ",FIRST Varemerke OF ArtBas NO-LOCK OUTER-JOIN" 
                         + ",FIRST Butiker WHERE Butiker.Butik = VareBehLinjeTrans.ButikkNr NO-LOCK" 
                         + " BY VarebehLinje.LevKod"
                         ).
    hQuery:QUERY-OPEN().
    hQuery:GET-FIRST().
    IF AVAIL VareBehLinjeTrans THEN DO:
      PUT UNFORMATTED SKIP(2)
          "Endring etter" + "~t" + 
          SUBSTR(STRING(PrintLogg.RegistrertDato),1,2) + "_" + SUBSTR(STRING(PrintLogg.RegistrertDato),3,1) + "_" + SUBSTR(STRING(PrintLogg.RegistrertDato),4,2)
          SKIP.
      fGrandTot = 0.
      REPEAT WHILE NOT hQuery:QUERY-OFF-END:
        RUN Inndeling.

        ASSIGN iSumAntStr = (VareBehLinjeTrans.Bestilt1 + VareBehLinjeTrans.Bestilt2 + VareBehLinjeTrans.Bestilt3 + VareBehLinjeTrans.Bestilt4) * iAntFord
               fGrandTot  = fGrandTot + VareBehLinje.VareKost * iSumAntStr
               .

        PUT UNFORMATTED
            (IF cLevNr NE "" THEN
              STRING(Butiker.Butik) + "~t" + 
              Butiker.ButNamn       + "~t"
             ELSE "") +
            STRING(VarebehLinjeTrans.Artikkelnr) + "~t" +
            (IF VareBehLinje.LevKod NE ? THEN VareBehLinje.LevKod ELSE "") + "~t" +
            (IF VareBehLinje.Beskr NE ? THEN VareBehLinje.Beskr ELSE "") + "~t" +
            (IF VareBehLinje.LevFargKod NE ? THEN VareBehLinje.LevFargKod ELSE "") + "~t" +
            (IF VareBehLinje.levnamn NE ? THEN VareBehLinje.levnamn ELSE "") + "~t" +
            (IF Varemerke.Beskrivelse NE ? THEN (IF VareBehLinje.Beskr BEGINS "+" THEN " " ELSE "") + Varemerke.Beskrivelse ELSE "") + "~t" +
            (IF VareBehLinje.VareKost NE ? THEN STRING(VareBehLinje.VareKost) ELSE "") + "~t" +
            (IF VareBehLinje.DB% NE ? THEN STRING(VareBehLinje.DB%) ELSE "") + "~t" +
            (IF VareBehLinje.supVarekost NE ? THEN STRING(VareBehLinje.supVarekost) ELSE "") + "~t" +
            (IF VareBehLinje.Pris NE ? THEN STRING(VareBehLinje.Pris) ELSE "") + "~t" +
            (IF VareBehLinje.KampanjePris NE ? THEN STRING(VareBehLinje.KampanjePris) ELSE "") + "~t" +
            (IF bEAN THEN
              (IF VareBehLinjeTrans.Kode NE ? THEN VareBehLinjeTrans.Kode ELSE "") + "~t"
             ELSE "") +
            (IF StrKonv.Storl NE ? AND TRIM(StrKonv.Storl) NE "1" AND NOT bInndeling THEN StrKonv.Storl 
             ELSE IF bInndeling AND VareBehLinjeTrans.Kode NE ? THEN VareBehLinjeTrans.Kode
             ELSE "") + "~t" +
            STRING(VareBehLinjeTrans.Levuke1) + "~t" +
            STRING(VareBehLinjeTrans.Bestilt1 * iAntFord) + "~t" +
            STRING(VareBehLinjeTrans.Levuke2) + "~t" +
            STRING(VareBehLinjeTrans.Bestilt2 * iAntFord) + "~t" +
            STRING(VareBehLinjeTrans.Levuke3) + "~t" +
            STRING(VareBehLinjeTrans.Bestilt3 * iAntFord) + "~t" +
            STRING(VareBehLinjeTrans.Levuke4) + "~t" +
            STRING(VareBehLinjeTrans.Bestilt4 * iAntFord) + "~t" +
            STRING(iSumAntStr) + "~t" +
            STRING(iSumAntStr * VareBehLinje.VareKost)
            SKIP.

        IF bInndeling THEN 
          PUT UNFORMATTED (IF cLevNr NE "" THEN "~t~t" ELSE "") + "~t" +
                          "Størrelser:"            + "~t" +   
                          (IF LENGTH(cStrList) > 27 THEN
                            cMinStr + " - " + cMaxStr
                           ELSE cStrList)          + "~t" +
                          "Fordeling:"             + "~t"
                          cSortFordList            + "~t~t~t~t~t~t~t~t~t" +     
                          STRING(VareBehLinjeTrans.Bestilt1) + "~t~t" +
                          STRING(VareBehLinjeTrans.Bestilt2) + "~t~t" +
                          STRING(VareBehLinjeTrans.Bestilt3) + "~t~t" +
                          STRING(VareBehLinjeTrans.Bestilt4)
                          SKIP.

        hQuery:GET-NEXT().
      END.
/*       PUT UNFORMATTED "TOT.ENDRING" + FILL("~t",IF cButNr NE "" THEN 22 ELSE 24)  */
/*                       fGrandTot SKIP(2).                                                    */
    END.
  END.
END.

OUTPUT CLOSE.
DELETE OBJECT hQuery.

IF iCount = 1 THEN
  ocReturn = "Varehåndteringsboken inneholder ingen bestillinger".
ELSE IF iDumpCount > 65000 THEN
  ocReturn = "Dump av varehåndteringsboken inneholder mer enn 65000 linjer og vil ikke bli forsøkt åpnet i Excel." + CHR(10) +
             "Filnavn: " + cFileName.
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
       WHERE ttTotBut.iButNr = VarebehLinjeTrans.ButikkNr
       NO-ERROR.
  IF NOT AVAIL ttTotBut THEN DO:
    CREATE ttTotBut.
    ASSIGN ttTotBut.iButNr   = VarebehLinjeTrans.ButikkNr
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
