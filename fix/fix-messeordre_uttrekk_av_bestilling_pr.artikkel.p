CURRENT-WINDOW:WIDTH = 320.

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
DEF VAR cButLst         AS CHAR   NO-UNDO.

DEF VAR cFilNavn   AS CHAR NO-UNDO.
DEF VAR cSep       AS CHAR NO-UNDO.
DEF VAR lVareBehNr AS DEC  NO-UNDO.
DEF VAR iSumant    AS INT  FORMAT "->>,>>>,>>9" NO-UNDO.
DEF VAR lSumVerdi  AS DEC  FORMAT "->>>,>>>,>>9.99" NO-UNDO.
DEF VAR iSumAntStr AS INT  FORMAT "->>,>>>,>>9" NO-UNDO.
DEF VAR lTrue      AS LOG NO-UNDO.

DEF STREAM Ut.

ASSIGN
    lVareBehNr = 9000009
    lTrue      = TRUE
    cSep       = ";" /*CHR(9)*/
    .

FIND VareBehHode NO-LOCK WHERE 
    VareBehhode.VareBehNr = lVareBehNr NO-ERROR.
FIND VareBokHode NO-LOCK WHERE
    VareBokHode.VareBokNr = VareBehHode.Kilde NO-ERROR.

ASSIGN
    cFilNavn   = "Messeordre_" + trim(VareBehHode.VareBehBeskrivelse) + string(lVareBehNr) + "_" + STRING(lTrue) + ".csv"
    cFilNavn   = REPLACE(cFilNavn,"(","_")
    cFilNavn   = REPLACE(cFilNavn,")","_")
    cFilNavn   = REPLACE(cFilNavn,"/","_")
    .

OUTPUT STREAM Ut TO VALUE(cFilNavn) NO-ECHO.

DISPLAY 
    VareBehhode.VarebehNr
    VareBehHode.Kilde
    VareBokHode.VareBokNr
    WITH WIDTH 320.

PUT STREAM ut UNFORMATTED
    "LevNr" cSep
    "LevNamn" cSep
    "ArtikkelNr" cSep
    "LevKod" cSep
    "Beskr" cSep
    "LevFargKod" cSep
    "Kjedevare" cSep
    "Gjennomfaktureres" cSep
    "Sortimentkoder" cSep
    "Kampanjeuker" cSep
    "Kampanjestotte" cSep
    "Lagerkoder" cSep
    "Sumant" 
    "SumVerdi"
    SKIP.

FOR EACH VareBehLinje OF VareBehHode NO-LOCK WHERE
    VareBehLinje.ArtikkelNr > 0:
    FIND VareBokLinje NO-LOCK WHERE
        VareBokLinje.VareBokNr = VareBokHode.VareBokNr AND
        VareBokLinje.ArtikkelNr = VareBehLinje.ArtikkelNr NO-ERROR.
    FIND LevBas OF VareBokLinje NO-LOCK NO-ERROR.

    ASSIGN
      iSumant = 0
      lSumVerdi = 0
      .
    /* Summerer */
    SUMMERER:
    FOR EACH VareBehLinjetrans NO-LOCK WHERE
        VareBehLinjeTrans.VareBehNr = VareBehHode.VareBehNr AND
        VareBehLinjetrans.ArtikkelNr = VareBehLinje.ArtikkelNr AND
        VareBehLinjeTrans.GodkjentBestilling = lTrue
        ,FIRST StrKonv OF VareBehLinjeTrans NO-LOCK
        ,FIRST ArtBas OF VarebehLinje NO-LOCK
        ,FIRST Varemerke OF ArtBas NO-LOCK 
        ,FIRST Butiker WHERE Butiker.Butik = VareBehLinjeTrans.ButikkNr NO-LOCK: 
        
        RUN Inndeling.
        ASSIGN
            iSumAntStr = (VareBehLinjeTrans.Bestilt1 +
                          VareBehLinjeTrans.Bestilt2 +
                          VareBehLinjeTrans.Bestilt3 +
                          VareBehLinjeTrans.Bestilt4) * iantFord
            lSumVerdi  = lSumVerdi + (iSumAntStr * VareBehLinje.VareKost)
            iSumAnt    = iSumant + iSumAntStr
            .

        IF (VareBehLinjeTrans.Bestilt1 +
                          VareBehLinjeTrans.Bestilt2 +
                          VareBehLinjeTrans.Bestilt3 +
                          VareBehLinjeTrans.Bestilt4) > 0 AND NOT CAN-DO(cbutLst,STRING(VareBehLinjeTrans.ButikkNr)) THEN
            cButLst = cButLst + (IF cButLst <> "" THEN "," ELSE "") + STRING(VareBehLinjeTrans.ButikkNr).
    END. /* SUMMERER */
    /*
    DISPLAY
        VarebokLinje.LevNr
        LevBas.LevNamn FORMAT "x(20)"
        VareBehLinje.ArtikkelNr
        VarebokLinje.LevKod
        VareBoklinje.Beskr
        VareBoklinje.LevFargKod
        VareBoklinje.Kjedevare
        VareBoklinje.Gjennomfaktureres
        VareBokLinje.Sortimentkoder FORMAT "x(10)"
        VarebokLinje.Kampanjeuker FORMAT "x(10)"
        VarebokLinje.Kampanjestotte FORMAT "x(10)"
        VareBokLinje.Lagerkoder FORMAT "x(10)"
        iSumant
        lSumVerdi
        WITH WIDTH 320.
    */
    PUT STREAM ut UNFORMATTED
        VarebokLinje.LevNr cSep
        LevBas.LevNamn cSep
        VareBehLinje.ArtikkelNr cSep
        VarebokLinje.LevKod cSep
        VareBoklinje.Beskr cSep
        VareBoklinje.LevFargKod cSep
        VareBoklinje.Kjedevare cSep
        VareBoklinje.Gjennomfaktureres cSep
        VareBokLinje.Sortimentkoder cSep
        VarebokLinje.Kampanjeuker cSep
        VarebokLinje.Kampanjestotte cSep
        VareBokLinje.Lagerkoder cSep
        iSumant cSep
        lSumVerdi
        SKIP.

END.



OUTPUT STREAM Ut CLOSE.

MESSAGE cButLst
    VIEW-AS ALERT-BOX INFO BUTTONS OK.

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
