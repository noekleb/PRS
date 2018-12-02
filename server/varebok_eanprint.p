/* Utskrift av varebok pr lev
   Parametere: Input: Vareboknr
               Output (ocReturn): Filnavn
   
   Opprettet: 08.12.04 av BHa       
   Endret:    17.08.05 av BHa
            - Tagger lev.navn for å kunne gjøre sidebryt.           
-----------------------------------------------------------------------------------*/
DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.

DEF VAR cFileName       AS CHAR   NO-UNDO.
DEF VAR ix              AS INT    NO-UNDO.
DEF VAR iy              AS INT    NO-UNDO.
DEF VAR iCount          AS INT    NO-UNDO.
DEF VAR cStrList        AS CHAR   NO-UNDO.
DEF VAR hQuery          AS HANDLE NO-UNDO.
DEF VAR cSortList       AS CHAR   NO-UNDO.
DEF VAR cSortStrList    AS CHAR   NO-UNDO.
DEF VAR cSortFordList   AS CHAR   NO-UNDO.
DEF VAR cSortAntList    AS CHAR   NO-UNDO.
DEF VAR iAntFord        AS INT    NO-UNDO.

DEF VAR bUse02kode      AS LOG    NO-UNDO.
DEF VAR cRowIdSelection AS CHAR   NO-UNDO.
DEF VAR cQueryString    AS CHAR   NO-UNDO.


{incl/ean13bc.i}
FUNCTION getWeekNum RETURNS CHARACTER
        (INPUT idDate AS DATE) FORWARD.

icParam = REPLACE(icParam,"and ArtikkelNr < 0","").
IF NUM-ENTRIES(icParam,";") GE 3 THEN DO:
  IF ENTRY(1,icParam,";") MATCHES "*use-index*" THEN
    cQueryString = SUBSTR(ENTRY(1,icParam,";"),1,INDEX(ENTRY(1,icParam,";"),"use-index") - 1) 
                 + " AND CAN-DO('" + ENTRY(3,icParam,";") + "',STRING(ROWID(VarebokLinje)))"
                 + " " + SUBSTR(ENTRY(1,icParam,";"),INDEX(ENTRY(1,icParam,";"),"use-index") - 1)
                   .
  ELSE cQueryString = ENTRY(1,icParam,";") + " AND CAN-DO('" + ENTRY(3,icParam,";") + "',STRING(ROWID(VarebokLinje)))".
END.
ELSE cQueryString = ENTRY(1,icParam,";").

CREATE QUERY hQuery.
hQuery:SET-BUFFERS(BUFFER VarebokLinje:HANDLE,BUFFER ArtBas:HANDLE, BUFFER Varemerke:HANDLE).
hQuery:QUERY-PREPARE("FOR EACH VarebokLinje NO-LOCK " + cQueryString
                     + ",FIRST ArtBas OF VarebokLinje NO-LOCK,FIRST Varemerke OF ArtBas NO-LOCK OUTER-JOIN" 
                     + IF NUM-ENTRIES(icParam,";") > 1 AND ENTRY(2,icParam,";") NE "levnamn" AND ENTRY(2,icParam,";") NE "" THEN 
                         " BY " + ENTRY(2,icParam,";")
                       ELSE " BY VarebokLinje.Vg BY VarebokLinje.LevNamn BY VarebokLinje.Pris"
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

PUT UNFORMATTED "~t" 
              + "~t"     
              + "~t"
              + "~t"         
                VarebokHode.VareBokBeskrivelse 
                SKIP.
PUT UNFORMATTED "Leverandørnavn"         + "~t" 
                "Artikkelnr PRS"  + "~t"     
                "Varemerke"              + "~t"
                "Varegr"                 + "~t"         
                "Varegruppetekst"        + "~t"
                "Engros"                 + "~t"
                "Enh"                    + "~t"            
                "Ant.enh"                + "~t"        
                                         + "~t"        
                "1.mul.lev.uke"          + "~t"      
                "1.lev.uke"              + "~t"      
                "2.lev.uke"              + "~t"      
                "3.lev.uke"              + "~t"      
                SKIP.
PUT UNFORMATTED "Lev.artikkelnr"         + "~t"          
                "Artikkelnavn"           + "~t"   
                "Farge"                  + "~t"
                "Markedspris"            + "~t"  
                "Merknad"                + "~t"  
                "Nto.forh"               + "~t" 
                "DB forh"                + "~t"     
                "Nto supl"               + "~t"    
                "Str"                    + "~t"       
                "Ant"                    + "~t"            
                "Ant"                    + "~t"            
                "Ant"                    + "~t"            
                "Ant"                    + "~t"            
                SKIP.

REPEAT WHILE NOT hQuery:QUERY-OFF-END:
  
  ASSIGN iCount        = iCount + 2
         cStrList      = ""
         cSortList     = ""
         cSortStrList  = ""
         cSortFordList = ""
         cSortAntList  = ""
         .

  FIND FIRST StrType OF ArtBas NO-LOCK NO-ERROR.
  IF AVAIL StrType THEN DO:
    DO ix = 1 TO NUM-ENTRIES(StrType.fordeling):
      FOR EACH StrekKode OF ArtBas NO-LOCK
          WHERE Strekkode.kode > ""
            AND StrekKode.StrKode = INT(ENTRY(ix,StrType.fordeling))
        , FIRST StrKonv OF StrekKode NO-LOCK
          BREAK BY StrekKode.StrKode:
    
        IF LAST-OF(StrekKode.StrKode) THEN
          cStrList = cStrList + " " + TRIM(STRING(StrKonv.Storl)) + "|" + strekkode.kode + "|".    
      END.
    END.
    FOR EACH ArtSort NO-LOCK
        WHERE ArtSort.ArtikkelNr = VarebokLinje.ArtikkelNr
       ,FIRST LevSort OF ArtSort NO-LOCK:
      
      iAntFord  = 0.
      FOR EACH LevSAnt OF LevSort NO-LOCK BY SeqNr:
        ASSIGN cSortStrList  = cSortStrList + TRIM(LevSAnt.SoStorl) + ","
               cSortFordList = cSortFordList + STRING(LevSAnt.SoAnt) + ","
               iAntFord      = iAntFord + LevSAnt.SoAnt.
      END.
      ASSIGN cSortList     = cSortList + LevSort.SortId + "|"
             cSortStrList  = TRIM(cSortStrList,",") + "|"
             cSortFordList = TRIM(cSortFordList,",") + "|"
             cSortAntList  = cSortAntList + STRING(iAntFord) + "|"
             .
    END.
    ASSIGN cSortList     = TRIM(cSortList,"|")
           cSortStrList  = TRIM(cSortStrList,"|")
           cSortFordList = TRIM(cSortFordList,"|")
           cSortAntList  = TRIM(cSortAntList,"|")
           .
  END.

  ELSE FOR EACH strtstr  NO-LOCK
      WHERE strtstr.strtypeid = ArtBas.StrTypeId,
      FIRST strkonv WHERE strkonv.storl = strtstr.sostorl NO-LOCK,
      FIRST strekkode WHERE strekkode.artikkelnr = VarebokLinje.ArtikkelNr
                        AND strekkode.kode > ''
                        AND strekkode.strkode = strkonv.strkode:

    MESSAGE PROGRAM-NAME(1) SKIP
            artbas.beskr SKIP
            "strekkode.kode: " strekkode.kode SKIP
            cStrList SKIP
            VIEW-AS ALERT-BOX.

    IF NOT bUse02kode AND StrekKode.kode BEGINS "02" THEN NEXT.

    cStrList = cStrList + STRING(sostorl) + "|" + strekkode.kode + "|".
  END.
  cStrList = TRIM(cStrList,"|").

  DO ix = 1 TO 2:           
    IF ix = 1 THEN 
      PUT UNFORMATTED
          "<lev>" + (IF VarebokLinje.levnamn NE ? THEN VarebokLinje.levnamn ELSE "") + "~t" + 
          STRING(VarebokLinje.Artikkelnr) + "~t" +
          (IF Varemerke.Beskrivelse NE ? THEN Varemerke.Beskrivelse ELSE "") + "~t" +
          STRING(VarebokLinje.Vg) + "~t" +
          VarebokLinje.VgBeskr + "~t" +
          (IF VarebokLinje.InnkjopsPris NE ? THEN STRING(VarebokLinje.InnkjopsPris) ELSE "") + "~t" +
          (IF ArtBas.SalgsEnhet NE ? THEN ArtBas.SalgsEnhet ELSE "") + "~t" +
          (IF ArtBas.AntIPakn NE ? THEN STRING(ArtBas.AntIPakn) ELSE "") + "~t" +
          "~t" +
          (IF ArtBas.LevDato1 NE ? THEN getWeekNum(ArtBas.LevDato1) ELSE "") + "~t" +
          "~t" +
          "~t" +
          "~t"
          SKIP.
    ELSE DO:
      PUT UNFORMATTED
          (IF VarebokLinje.LevKod NE ? THEN VarebokLinje.LevKod ELSE "") + "~t" +
          (IF VarebokLinje.Beskr NE ? THEN VarebokLinje.Beskr ELSE "") + "~t" +
          (IF VarebokLinje.LevFargKod NE ? THEN VarebokLinje.LevFargKod ELSE "") + "~t" +
          (IF VarebokLinje.Pris NE ? THEN STRING(VarebokLinje.Pris) ELSE "") + "~t" +
          (IF VarebokLinje.LinjeMerknad NE ? THEN REPLACE(REPLACE(REPLACE(REPLACE(VarebokLinje.LinjeMerknad,CHR(10)," "),CHR(9)," "),CHR(13)," "),"|"," ") ELSE "") + "~t" +
          (IF VarebokLinje.Varekost NE ? THEN STRING(VarebokLinje.Varekost) ELSE "") + "~t" +
          (IF VarebokLinje.DB% NE ? THEN STRING(VarebokLinje.DB%) ELSE "") + "~t" +
          (IF VarebokLinje.supVarekost NE ? THEN STRING(VarebokLinje.supVarekost) ELSE "") + "~t" +
          (IF cStrList NE "" AND cSortList = "" THEN ENTRY(1,cStrList,"|") ELSE "") + "~t" +
          "~t" +
          "~t" +
          "~t" +
          "~t" 
          SKIP.

      IF cSortList NE "" THEN DO: 
        iCount = iCount + 1.
        PUT UNFORMATTED
            "~t~t~t~tInndeling~tStørrelser~tFordeling~t~tAnt" SKIP.
        DO iy = 1 TO NUM-ENTRIES(cSortList,"|"):
          iCount = iCount + 1.
          PUT UNFORMATTED
              "~t~t~t~t" +
              ENTRY(iy,cSortList,"|") + "~t" + 
              ENTRY(iy,cSortStrList,"|") + "~t" + 
              ENTRY(iy,cSortFordList,"|") + "~t~t" + 
              ENTRY(iy,cSortAntList,"|") + "~t" 
              SKIP.
        END.

        /* Dersom det er angitt minst en inndelinging må "fritt tillegg" være slått på for at størrelser skal skrives ut: */
        IF NOT ArtBas.FrittTillegg THEN cStrList = "".
      END.

      IF cSortList = "" AND NUM-ENTRIES(cStrList,"|") > 2 THEN DO iy = 3 TO NUM-ENTRIES(cStrList,"|") BY 2:
        iCount = iCount + 1.
        PUT UNFORMATTED
            "~t" +
            (IF VarebokLinje.Beskr NE ? THEN VarebokLinje.Beskr ELSE "") + "~t" +
            "~t" +
            "~t" +
            "~t" +
            "~t" +
            "~t" +       
            "~t" +
            ENTRY(iy,cStrList,"|") + "~t" +
            "~t" +
            "~t" +
            "~t" +
            "~t" 
            SKIP.
      END.
      ELSE IF cSortList NE "" AND NUM-ENTRIES(cStrList,"|") > 1 THEN DO iy = 1 TO NUM-ENTRIES(cStrList,"|") BY 2:
        iCount = iCount + 1.
        PUT UNFORMATTED
            "~t" +
            (IF VarebokLinje.Beskr NE ? THEN VarebokLinje.Beskr ELSE "") + "~t" +
            "~t" +
            "~t" +
            "~t" +
            "~t" +
            "~t" +       
            "~t" +
            ENTRY(iy,cStrList,"|") + "~t" +
            "~t" +
            "~t" +
            "~t" +
            "~t" 
            SKIP.
      END.
    END.
  END.

  hQuery:GET-NEXT().
END.

OUTPUT CLOSE.
DELETE OBJECT hQuery.

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
