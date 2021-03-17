/* Utskrift av varebok pr lev
   Parametere: Input: Vareboknr
               Output (ocReturn): Filnavn
   
   Opprettet: 08.12.04 av BHa               
   Endret:    17.08.05 av BHa
            - Lagt til leverandørens fargetekst og fjernet veil.pris (anbefalt pris)  
              08.08.06 av BHa
            - Kan benyttes for å kontrollere at det finnes strekkode for samtlige str. i strtype
-----------------------------------------------------------------------------------*/
DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.

DEF VAR cFileName       AS CHAR NO-UNDO.
DEF VAR ix              AS INT NO-UNDO.
DEF VAR iy              AS INT NO-UNDO.
DEF VAR iCount          AS INT NO-UNDO.
DEF VAR cCurrNumFormat  AS CHAR NO-UNDO.
DEF VAR hQuery          AS HANDLE NO-UNDO.
DEF VAR cStrFraTil      AS CHAR NO-UNDO.
DEF VAR iPrevLevNr      AS INT NO-UNDO.
DEF VAR bStdHeaderPrint AS LOG NO-UNDO.
DEF VAR iMaxStr         AS INT NO-UNDO.
DEF VAR iMinStr         AS INT NO-UNDO.
DEF VAR cFraStr         AS CHAR NO-UNDO.
DEF VAR cTilStr         AS CHAR NO-UNDO.
DEF VAR iDenneStr       AS INT NO-UNDO.
DEF VAR bUse02kode      AS LOG NO-UNDO.
DEF VAR bStrkontroll    AS LOG NO-UNDO.
DEF VAR iAntStrKoder    AS INT NO-UNDO.
DEF VAR iAntTypeKoder   AS INT NO-UNDO.
DEF VAR bIkkeIStrType   AS LOG NO-UNDO.
DEF VAR bIkkeKjedeInnpris AS LOG NO-UNDO.

cCurrNumFormat = SESSION:NUMERIC-FORMAT.

bStrkontroll = NUM-ENTRIES(icParam,";") > 2.

DEF BUFFER bStrkonv FOR strkonv.

FUNCTION getWeekNum RETURNS CHARACTER
        (INPUT idDate AS DATE) FORWARD.

icParam = REPLACE(icParam,"and ArtikkelNr < 0","").

CREATE QUERY hQuery.
hQuery:SET-BUFFERS(BUFFER VarebokLinje:HANDLE,BUFFER ArtBas:HANDLE,BUFFER Varemerke:HANDLE).
hQuery:QUERY-PREPARE("FOR EACH VarebokLinje NO-LOCK " 
                     + ENTRY(1,icParam,";") 
                     + ",FIRST ArtBas OF VarebokLinje NO-LOCK,FIRST Varemerke OF ArtBas NO-LOCK"
                     + IF ENTRY(2,icParam,";") NE "" THEN 
/*                      + IF NUM-ENTRIES(icParam,";") > 1 THEN */
                         " BY LevNamn BY " + ENTRY(2,icParam,";")
                       ELSE " BY VarebokLinje.LevNamn BY VarebokLinje.Vg BY VarebokLinje.Pris"
                      ).
hQuery:QUERY-OPEN().
hQuery:GET-FIRST().

IF NOT AVAIL VarebokLinje THEN DO:
  ocReturn = "Varebok ikke funnet".
  RETURN.
END.

cFileName       = SESSION:TEMP-DIR + STRING(VarebokLinje.VarebokNr) + "_" + STRING(YEAR(TODAY)) + STRING(MONTH(TODAY),"99") + STRING(DAY(TODAY)) + STRING(TIME) + ".txt".

FIND VarebokHode WHERE VarebokHode.VarebokNr = VarebokLinje.VarebokNr NO-LOCK NO-ERROR.

OUTPUT TO VALUE(cFileName).

PUT UNFORMATTED VarebokHode.VareBokBeskrivelse + "~t~t~t"
              + (IF bStrkontroll THEN "Artikler med strekkoder som mangler i str.type" ELSE "")
                SKIP.
PUT UNFORMATTED 
                (IF bStrkontroll THEN "SE art.nr~t" ELSE "")
                "Varetekst"              + "~t" 
                "Lev.artnr"              + "~t"
                "Lev.fargetekst"         + "~t"
                "Varemerke"              + "~t"
                "Str fra-til"            + "~t"
/*                 "Varegr"                 + "~t" */
                "Varegruppetekst"        + "~t"
                "Engros"                 + "~t"
                "Nto.forh"               + "~t" 
                "Kal.f"                  + "~t" 
                "DB forh"                + "~t"     
                "Nto supl"               + "~t"    
                "Kal.s"                  + "~t" 
/*                 "DB supl"                + "~t"  */
                "Markedspris"            + "~t"  
                "Kamp.pris"              + "~t"
                "KjedeInnkPris"          + "~t"
                "Lagerkoder"             + "~t"
                "Lev.uke1"               + "~t"      
                "Merknad"                        
                SKIP.

REPEAT WHILE NOT hQuery:QUERY-OFF-END:
  iCount = iCount + 1.

  IF iPrevLevNr NE VarebokLinje.Levnr THEN DO:
    FIND LevBas WHERE LevBas.levNr = varebokLinje.levnr NO-LOCK NO-ERROR.
    IF NOT AVAIL LevBas THEN DO:
      ocReturn = "Finner ikke leverandør: " + STRING(VarebokLinje.LevNr).
      LEAVE.
    END.
/*     IF NOT bStrkontroll THEN */
    /* Legger ut header record pr. leverandør. */
    PUT UNFORMATTED 
      "~t" SKIP
      LevBas.levnamn + "~t" SKIP.

    ASSIGN iCount = iCount + 2
           bStdHeaderPrint = TRUE.
    IF NOT bStrkontroll THEN DO ix = 1 TO 4:
      IF LevBas.kommentar[ix] NE ? AND LevBas.kommentar[ix] NE "" THEN DO:
        IF bStdHeaderPrint THEN DO:
          PUT UNFORMATTED
            "Standardbetingelser: " + "~t" SKIP.
          ASSIGN iCount = iCount + 1
                 bStdHeaderPrint = FALSE.
        END.
        PUT UNFORMATTED LevBas.kommentar[ix] SKIP.
        iCount = iCount + 1.
      END.
    END.
  END.

  ASSIGN cStrFraTil    = ""
         iAntStrKoder  = 0
         iAntTypeKoder = 0
         bIkkeIStrType = NO
         bIkkeKjedeInnpris = FALSE
         .

  IF ArtBas.StrKode1 NE 0 AND ArtBas.StrKode2 NE 0 AND ArtBas.StrKode1 NE ArtBas.StrKode2 THEN DO:
    FIND FIRST strkonv  WHERE strkonv.strkode  = ArtBas.StrKode1 NO-LOCK NO-ERROR.
    FIND FIRST bStrkonv WHERE bStrkonv.strkode = ArtBas.StrKode2 NO-LOCK NO-ERROR.
    IF AVAIL strkonv AND AVAIL bStrkonv THEN
      cStrFraTil = strkonv.storl + " - " + bStrkonv.storl.
  END.
  ELSE DO:

    FIND FIRST StrType OF ArtBas NO-LOCK NO-ERROR.
    IF AVAIL StrType AND StrType.StrTypeID > 1 /* 2 */ THEN DO:
      ASSIGN iMaxStr    = 0
             iMinStr    = 1000
             cFraStr    = ""
             cTilStr    = ""
/*              bUse02kode = NOT CAN-FIND(FIRST StrekKode OF ArtBas WHERE NOT StrekKode.Kode BEGINS "02") */
             iAntTypeKoder = NUM-ENTRIES(StrType.fordeling)
             .

      FOR EACH StrekKode OF ArtBas NO-LOCK,
          FIRST StrKonv OF StrekKode NO-LOCK
          BREAK BY StrekKode.StrKode:

/*         IF NOT bUse02kode AND StrekKode.kode BEGINS "02" THEN NEXT. */
        IF FIRST-OF(StrekKode.StrKode) THEN DO:
          iDenneStr = LOOKUP(STRING(StrekKode.StrKode),StrType.fordeling).
          IF iDenneStr > 0 THEN DO:
/*             iAntStrKoder = iAntStrKoder + 1. */
            IF iDenneStr < iMinStr THEN 
              ASSIGN iMinStr = iDenneStr
                     cFraStr = TRIM(StrKonv.Storl)
                     .
            IF iDenneStr > iMaxStr THEN 
              ASSIGN iMaxStr = iDenneStr
                     cTilStr = TRIM(StrKonv.Storl)
                     .
          END.
          ELSE bIkkeIStrType = YES.
        END.
      END.
      IF cFraStr NE "" AND cTilStr NE "" THEN
        cStrFraTil = cFraStr + " -- " + cTilStr.
    END.
    ELSE IF AVAIL StrType THEN
      cStrFraTil = StrType.Beskrivelse.
  END.

  /* Det skal feilmeldes hvis det ikke ligger strekkoder på artikkelen også. */
  IF bIkkeIStrType = FALSE THEN
  DO:
      IF NOT CAN-FIND(FIRST Strekkode WHERE
                      Strekkode.ArtikkelNr = VareBokLinje.ArtikkelNr) THEN
          bIkkeIStrType = TRUE.
  END.

  /* Det skal feilmeldes hvis lagerkode er angitt og kjedens innkjøpspris ikke er angitt. */
  IF bIkkeIStrType = FALSE AND VarebokLinje.Lagerkoder <> ? THEN
  DO:
      IF VarebokLinje.Lagerkoder <> "" AND VarebokLinje.Lagerkoder <> ? THEN DO:
          IF VarebokLinje.KjedeInnkPris = ? THEN bIkkeKjedeInnpris = TRUE.
          IF VarebokLinje.KjedeInnkPris = 0 THEN bIkkeKjedeInnpris = TRUE.
      END.
  END.

  IF NOT bStrkontroll OR bIkkeIStrType OR bIkkeKjedeInnpris /* iAntStrKoder < iAntTypeKoder */ THEN DO:
    PUT UNFORMATTED
      (IF bStrkontroll THEN STRING(VarebokLinje.ArtikkelNr) + "~t" ELSE "")
      (IF VarebokLinje.Beskr NE ? THEN VarebokLinje.Beskr ELSE "") + "~t" +
      (IF VarebokLinje.LevKod NE ? THEN VarebokLinje.LevKod ELSE "") + "~t" +
      (IF VarebokLinje.LevFargKod NE ? THEN VarebokLinje.LevFargKod ELSE "") + "~t" +
      (IF Varemerke.Beskrivelse NE ? THEN Varemerke.Beskrivelse ELSE "") + "~t" +
      cStrFraTil + "~t" +
  /*     STRING(VarebokLinje.Vg) + "~t" + */
      SUBSTR(VarebokLinje.VgBeskr,1,15) + "~t" +
      (IF VarebokLinje.InnkjopsPris NE ? THEN STRING(VarebokLinje.InnkjopsPris) ELSE "") + "~t" +
      (IF VarebokLinje.Varekost NE ? THEN STRING(VarebokLinje.Varekost) ELSE "") + "~t" +
      (IF VarebokLinje.forhKalkyle NE ? THEN STRING(VarebokLinje.forhKalkyle) ELSE "") + "~t" +
      (IF VarebokLinje.DB% NE ? THEN STRING(VarebokLinje.DB%) ELSE "") + "~t" +
      (IF VarebokLinje.supVarekost NE ? THEN STRING(VarebokLinje.supVarekost) ELSE "") + "~t" +
      (IF VarebokLinje.supKalkyle NE ? THEN STRING(VarebokLinje.supKalkyle) ELSE "") + "~t" +
  /*     (IF VarebokLinje.supDB% NE ? THEN STRING(VarebokLinje.supDB%) ELSE "") + "~t" +  */
      (IF VarebokLinje.Pris NE ? THEN STRING(VarebokLinje.Pris) ELSE "") + "~t" +
      (IF VarebokLinje.KampanjePris NE ? AND VarebokLinje.KampanjePris > 0 THEN STRING(VarebokLinje.KampanjePris) ELSE "") + "~t" +
      (IF VarebokLinje.KjedeInnkPris NE ? THEN STRING(VarebokLinje.KjedeInnkPris) ELSE "") + "~t" +
      (IF VarebokLinje.Lagerkoder NE ? THEN STRING(VarebokLinje.Lagerkoder) ELSE "") + "~t" +        
      (IF ArtBas.LevDato1 NE ? THEN getWeekNum(ArtBas.LevDato1) ELSE "") + "~t" +
      (IF VarebokLinje.LinjeMerknad NE ? THEN VarebokLinje.LinjeMerknad ELSE "")
  /*     (IF VarebokLinje.LinjeMerknad NE ? THEN REPLACE(REPLACE(REPLACE(VarebokLinje.LinjeMerknad,CHR(10)," "),CHR(9)," "),CHR(13)," ") ELSE "") */
      .
  /*   DO ix = 2 TO NUM-ENTRIES(VarebokLinje.LinjeMerknad):           */
  /*     PUT UNFORMATTED CHR(13) ENTRY(ix,VarebokLinje.LinjeMerknad). */
  /*   END.                                                           */
    PUT SKIP.
  END.

  iPrevLevNr = VarebokLinje.levnr.

  hQuery:GET-NEXT().
END.

OUTPUT CLOSE.
DELETE OBJECT hQuery.

SESSION:NUMERIC-FORMAT = cCurrNumFormat.

IF iCount = 0 THEN
  ocReturn = "Vareboken inneholder ingen artikler".
ELSE 
  ASSIGN ocReturn = cFileName + "|" + STRING(iCount)
         obOk     = TRUE.



FUNCTION getWeekNum RETURNS CHARACTER
        (INPUT idDate AS DATE):

  DEF VAR iWeekNum AS INT NO-UNDO.

  RUN weeknum.p (idDate, OUTPUT iWeekNum).
  IF iWeekNum NE ? THEN
    RETURN STRING(iWeekNum).
  ELSE RETURN "".
END FUNCTION.
