/* Rapport - priser pr prisprofil
   Parameter:  
   Opprettet: 18.06.10 av BHa              
-----------------------------------------------------------------------------------*/
DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.

DEF VAR hQuery       AS HANDLE NO-UNDO.
DEF VAR cFileName    AS CHAR   NO-UNDO.
DEF VAR cOutput      AS CHAR   NO-UNDO.
DEF VAR hField       AS HANDLE NO-UNDO.
DEF VAR fDec         AS DEC    NO-UNDO.
DEF VAR ix           AS INT    NO-UNDO.
DEF VAR iColumn      AS INT    NO-UNDO.
DEF VAR cLabelList   AS CHAR   NO-UNDO.
DEF VAR cNameList    AS CHAR   NO-UNDO.
DEF VAR cRpFieldList AS CHAR   NO-UNDO.

DEFINE BUFFER bArtPris FOR ArtPris.

cFileName = SESSION:TEMP-DIRECTORY + "artpris_" + STRING(YEAR(TODAY)) + STRING(MONTH(TODAY)) + STRING(DAY(TODAY))
          + "_" + STRING(TIME) + ".xls".  

ASSIGN cLabelList = ENTRY(1,icParam,"|")
       cNameList  = ENTRY(2,icParam,"|").

OUTPUT TO VALUE(cFileName).

PUT UNFORMATTED 
   (IF CAN-DO(cNameList,"Artikkelnr") THEN ENTRY(LOOKUP("ArtikkelNr",cNameList),cLabelList,";") ELSE "Artikkelnr")  "~t"
   (IF CAN-DO(cNameList,"Profilnr") THEN ENTRY(LOOKUP("ProfilNr",cNameList),cLabelList,";") ELSE "Profilnr")  "~t"
   (IF CAN-DO(cNameList,"LevKod") THEN ENTRY(LOOKUP("LevKod",cNameList),cLabelList,";") ELSE "Lev.art.nr")  "~t"
   (IF CAN-DO(cNameList,"Beskr") THEN ENTRY(LOOKUP("Beskr",cNameList),cLabelList,";") ELSE "Tekst")  "~t"
   (IF CAN-DO(cNameList,"Bestillingsnummer") THEN ENTRY(LOOKUP("Bestillingsnummer",cNameList),cLabelList,";") ELSE "Best.nr")  "~t"
   "Pris~tVarekost~tDB%~tJamførenhet~tLev.nr~tNavn~tVaremerke~tProdusent~tNavn"
/*    (IF CAN-DO(cNameList,"Pris[1]") THEN ENTRY(LOOKUP("Pris[1]",cNameList),cLabelList,";") ELSE "Pris")  "~t"             */
/*    (IF CAN-DO(cNameList,"Varekost[1]") THEN ENTRY(LOOKUP("Varekost[1]",cNameList),cLabelList,";") ELSE "Varekost")  "~t" */
/*    (IF CAN-DO(cNameList,"DB&[1]") THEN ENTRY(LOOKUP("DB%[1]",cNameList),cLabelList,";") ELSE "DB%")  "~t"                */
    SKIP.


CREATE QUERY hQuery.
hQuery:SET-BUFFERS(ihBuffer).
hQuery:QUERY-PREPARE("FOR EACH " + ihBuffer:NAME + " BY ArtikkelNr BY Profilnr").
hQuery:QUERY-OPEN().
hQuery:GET-FIRST().

REPEAT WHILE NOT hQuery:QUERY-OFF-END:
  FIND FIRST ArtPris NO-LOCK 
       WHERE ArtPris.ArtikkelNr = DEC(ihBuffer:BUFFER-FIELD("Artikkelnr"):BUFFER-VALUE)
         AND ArtPris.ProfilNr   = 1
       NO-ERROR.
  IF AVAIL ArtPris  
    THEN
    DO:
    FIND ArtBas OF ArtPris NO-LOCK NO-ERROR.
    FIND LevBas OF ArtBas NO-LOCK NO-ERROR.
    FIND VareMerke OF ArtBas NO-LOCK NO-ERROR.
    FIND Produsent OF ArtBas NO-LOCK NO-ERROR.
    FIND FIRST Strekkode NO-LOCK WHERE
      Strekkode.ArtikkelNr = ArtPris.ArtikkelNr NO-ERROR.
    PUT ArtPris.ArtikkelNr "~t" 
        ArtPris.ProfilNr "~t" 
        ihBuffer:BUFFER-FIELD("LevKod"):BUFFER-VALUE FORMAT "X(30)" "~t" 
        ihBuffer:BUFFER-FIELD("Beskr"):BUFFER-VALUE FORMAT "X(50)" "~t" 
        (IF AVAILABLE Strekkode THEN Strekkode.BestillingsNummer ELSE '') "~t"
        ArtPris.Pris[1] "~t"
        ArtPris.VareKost[1] "~t"
        ArtPris.DB%[1] "~t"
        ArtBas.Mengde "~t"
        ArtBas.LevNr "~t"
        (IF AVAILABLE LevBas THEN LevBas.LevNamn ELSE '') "~t"
        (IF AVAILABLE VareMerke THEN VareMerke.Beskrivelse ELSE '') "~t"
        ArtBas.ProdNr "~t"
        (IF AVAILABLE Produsent THEN Produsent.Beskrivelse ELSE '') "~t"
        SKIP.
    /* Legger ut lokale priser for artikkelen. */    
    FOR EACH bArtPris NO-LOCK
        WHERE bArtPris.ArtikkelNr = ArtPris.ArtikkelNr
          AND bArtPris.ProfilNr   > ArtPris.ProfilNr
       ,FIRST Prisprofil NO-LOCK 
              OF bArtPris
        BY bArtPris.ProfilNr:
      /* Kun de med avvikende priser skal legges ut. */
      IF (bArtPris.pris[1] NE ArtPris.Pris[1] OR bArtPris.VareKost[1] NE ArtPris.VareKost[1]) THEN 
      PUT "~t" 
          bArtPris.ProfilNr "~t" 
          " ~t"
          Prisprofil.Beskrivelse "~t" 
         (IF AVAILABLE Strekkode THEN Strekkode.BestillingsNummer ELSE '') "~t"
          bArtPris.Pris[1] "~t"
          bArtPris.VareKost[1] "~t"
          bArtPris.DB%[1] "~t"
          SKIP.
    END.

  END.

/*   DO iColumn = 1 TO ihBuffer:NUM-FIELDS:                                                                                                                                 */
/*     hField = ihBuffer:BUFFER-FIELD(iColumn) NO-ERROR.                                                                                                                    */
/*     DO ix = 0 TO hField:EXTENT:                                                                                                                                          */
/*       IF hField:EXTENT > 0 AND ix = 0 THEN NEXT.                                                                                                                         */
/*                                                                                                                                                                          */
/*       IF hField:DATA-TYPE = "integer" OR hField:DATA-TYPE = "decimal" THEN DO:                                                                                           */
/*         fDec = hField:BUFFER-VALUE[ix].                                                                                                                                  */
/*         cOutput = cOutput + (IF fDec NE ? THEN STRING(fDec) ELSE "0") + "~t".                                                                                            */
/*       END.                                                                                                                                                               */
/*       ELSE IF hField:DATA-TYPE = "DATE" OR hField:DATA-TYPE = "LOGICAL" THEN                                                                                             */
/*         cOutput = cOutput + (IF hField:STRING-VALUE[ix] NE ? THEN hField:STRING-VALUE[ix] ELSE "") + "~t".                                                               */
/*       ELSE                                                                                                                                                               */
/*         cOutput = cOutput + (IF hField:BUFFER-VALUE[ix] NE ? THEN REPLACE(REPLACE(REPLACE(hField:BUFFER-VALUE[ix],CHR(10)," "),CHR(9)," "),CHR(13)," ") ELSE "") + "~t". */
/*     END.                                                                                                                                                                 */
/*   END.                                                                                                                                                                   */
/*   PUT UNFORMATTED cOutput SKIP.                                                                                                                                          */

  hQuery:GET-NEXT().
END.

OUTPUT CLOSE.

DELETE OBJECT hQuery NO-ERROR.

obOK = ocReturn = "".
IF obOk THEN ocReturn = cFileName.

