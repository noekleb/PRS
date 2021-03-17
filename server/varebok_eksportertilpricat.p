/* Eksporter varebok til fil 
   Parametere:  filnavn 
                buffersandfields
                query 
   
   Opprettet: 24.08.05 av BHa 
-----------------------------------------------------------------------------------*/
DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.

DEF VAR hTempTable        AS HANDLE NO-UNDO.
DEF VAR hQuery            AS HANDLE NO-UNDO.
DEF VAR hBuffer           AS HANDLE NO-UNDO.
DEF VAR ix                AS INT    NO-UNDO.
DEF VAR iy                AS INT    NO-UNDO.
DEF VAR cDummy            AS CHAR   NO-UNDO.
DEF VAR cStrKodeLst       AS CHAR   NO-UNDO.
DEF VAR cBestNrLSt        AS CHAR   NO-UNDO.

DEF VAR dVarebokNr  LIKE VareBokHode.VareBokNr NO-UNDO.
DEFINE VARIABLE cFileName AS CHARACTER NO-UNDO.

DEF VAR cStrList        AS CHAR   NO-UNDO.
DEF VAR cKodeList       AS CHAR   NO-UNDO.


ASSIGN 
  dVarebokNr = DECI(ENTRY(2,icParam,"¤"))
  cFileName  = RIGHT-TRIM(ENTRY(1,icParam,"¤"),'.csv')
  .

IF CAN-FIND(FIRST Vareboklinje WHERE VareBokLinje.Vareboknr = dVareBokNr) THEN 
  DO:
    VAREBOKLINJEN:
    FOR EACH VareBokLinje NO-LOCK WHERE
      VareBokLinje.VareBokNr = dVareBokNr
      BREAK BY VareBokLinje.LevNr:
      /* Åpner stream og legger ut header. */
      IF FIRST-OF(VareBokLinje.LevNr) THEN 
      DO:
        OUTPUT TO VALUE(cFileName + '_' + STRING(VareBokLinje.LevNr) + '.csv').
        PUT UNFORMATTED
            "R1" + 
            ";" + "LevNr" + 
            ";" + "LevModellNr" +
            ";" + "EANnr" + 
            ";" + "VareTekst" +
            ";" + "FargeKode" + 
            ";" + "FargeTekst" + 
            ";" + "Str" +       
            ";" + "StrTab" + 
            ";" + "Varemerke" +
            ";" + "Enh" +
            ";" + "AntIEnh" +
            ";" + "LevPrisEngros" +
            ";" + "ValKod" +
            ";" + "forhRab%" +
            ";" + "suppRab%" +
            ";" + "VeilPris" +
            ";" + "PAKstru" +
            ";" + "LevUke1" +
            ";" + "LevUke2" +
            ";" + "LevUke3" +
            ";" + "LevUke4" +
            ";" + "VareGruppe" +
            ";" + "LevNavn" +
            ";" + "Bestillingsnr" +
            ";" + "nettoForh" +
            ";" + "kalkForh" +
            ";" + "BFforh" +
            ";" + "nettoSupp" +
            ";" + "kalkSupp" +
            ";" + "BFsupp" + 
            ";" + "MarkedsPris" +
            ";" + "Sortiment" +
            ";" + "Sesong" +
            ";" + "VPIBildeKode" +
            ";" + "Merknad" 
            SKIP.
      END.
      
      /* Legger ut vareboklinje. */
      LINJEUTLEGG:
      DO:
        FIND FIRST ArtBas WHERE ArtBas.ArtikkelNr = VarebokLinje.ArtikkelNr NO-LOCK NO-ERROR.
        IF NOT AVAIL ArtBas THEN 
          LEAVE LINJEUTLEGG.
        FIND varemerke OF artbas NO-LOCK NO-ERROR.
        FIND LevBas OF artbas NO-LOCK NO-ERROR.
        FIND farg OF artbas NO-LOCK NO-ERROR.
      
        FIND FIRST StrType OF ArtBas NO-LOCK NO-ERROR.
        IF AVAIL StrType THEN DO:
          ASSIGN cStrList    = ""
                 cKodeList   = ""
                 cStrKodeLSt = ""
                 cBestNrLst  = "".
          FIND FIRST StrekKode OF ArtBas WHERE StrekKode.KodeType = 1 NO-LOCK  NO-ERROR.
          IF AVAIL StrekKode THEN
              RUN HentEanStorl (OUTPUT cKodeList,OUTPUT cStrList, OUTPUT cStrKodeLst, OUTPUT cBestNrLSt).
          ELSE
              ASSIGN cStrList    = TRIM(cStrList," ")
                     cKodeList   = TRIM(cKodeList," ")
                     cStrKodeLSt = TRIM(cStrKodeLst," ")
                     cBestNrLst  = TRIM(cBestNrLst," ").
        END.
        ELSE
            ASSIGN cStrList    = " "
                   cKodeList   = " "
                   cStrKodeLSt = " "
                   cBestNrLst  = " ".             
      
          DO ix = 1 TO NUM-ENTRIES(cKodeList):
              IF TRIM(ENTRY(ix,cKodeList)) <> '' THEN 
              PUT UNFORMATTED
                  "R1"
                  ";" TRIM(STRING(ArtBas.levnr,">>>>>9"))
                  ";" VareBokLinje.LevKod
                  ";" TRIM(ENTRY(ix,cKodeList))
                  ";" VarebokLinje.Beskr
                  ";" TRIM(STRING(ArtBas.Farg,">>>>9"))
                  ";" ArtBas.LevFargKod
                  ";" TRIM(ENTRY(ix,cStrList))
                  ";" TRIM(STRING(ArtBas.StrTypeId,">>>>>>9"))
                  ";" IF AVAIL Varemerke THEN REPLACE(Varemerke.Beskrivelse,";","") ELSE ""
                  ";" ArtBas.SalgsEnhet
                  ";" TRIM(STRING(ArtBas.AntIPakn,">>>>>>9"))
                  ";" STRING(VarebokLinje.InnkjopsPris)
                  ";" LevBas.ValKod
                  ";" STRING(VarebokLinje.forhRab%)
                  ";" STRING(VarebokLinje.supRab%)
                  ";" STRING(VarebokLinje.AnbefaltPris)                  
                  ";" ""
                  ";" IF VarebokLinje.LevDato1 <> ? THEN STRING(VarebokLinje.LevDato1) ELSE ""
                  ";" IF VarebokLinje.LevDato2 <> ? THEN STRING(VarebokLinje.LevDato2) ELSE ""
                  ";" IF VarebokLinje.LevDato3 <> ? THEN STRING(VarebokLinje.LevDato3) ELSE ""
                  ";" IF VarebokLinje.LevDato4 <> ? THEN STRING(VarebokLinje.LevDato4) ELSE ""
                  ";" TRIM(STRING(VarebokLinje.vg,">>>>>9"))                 
                  ";" IF AVAIL levbas THEN LevBas.levnamn ELSE ""
                  ";" TRIM(ENTRY(ix,cBestNrLSt))
                  ";" STRING(VarebokLinje.varekost)
                  ";" STRING(VarebokLinje.forhKalkyle)
                  ";" STRING(VarebokLinje.DBKr)
                  ";" STRING(VarebokLinje.supVarekost)
                  ";" STRING(VarebokLinje.supkalkyle)
                  ";" STRING(VarebokLinje.supDBKr)
                  ";" STRING(VarebokLinje.Pris)
                  ";" IF ArtBas.AnonseArtikkel = TRUE THEN "1" ELSE ""                                    
                  ";" TRIM(STRING(ArtBas.Sasong,">>>>9"))                      
                  ";" STRING(ArtBAs.VPIBildeKode)                
                  ";" STRING(ArtBas.ArtikkelNr) + TRIM(ENTRY(ix,cStrKodeLst)) + '|' + VarebokLinje.Linjemerknad
                  SKIP
                  .
          END.
      END. /* LINJNEUTLEGG */
      
      /* Lukker stream */
      IF LAST-OF(Vareboklinje.LevNr) THEN 
      DO:
        OUTPUT CLOSE.
      END.  
    END. /* VAREBOKLINJEN */  
  END.
  
/* Gammel kode --------------------------    
CREATE QUERY hQuery.
CREATE BUFFER hBuffer FOR TABLE "VareBokLinje".
hQuery:SET-BUFFERS(hBuffer).
hQuery:QUERY-PREPARE("For each Vareboklinje no-lock where vareboklinje.vareboknr = " + ENTRY(2,icParam,"¤")).
hQuery:QUERY-OPEN().

hQuery:GET-FIRST().
IF hBuffer:AVAIL THEN DO:
  OUTPUT TO VALUE(ENTRY(1,icParam,"¤")).
/*   DO ix = 1 TO hBuffer:NUM-FIELDS:                                     */
/*     IF CAN-DO(ENTRY(3,icParam,"¤"),hBuffer:BUFFER-FIELD(ix):NAME) THEN */
/*       PUT UNFORMATTED hBuffer:BUFFER-FIELD(ix):LABEL ";".              */
/*   END.                                                                 */
  PUT UNFORMATTED
      "R1" + 
      ";" + "LevNr" + 
      ";" + "LevModellNr" +
      ";" + "EANnr" + 
      ";" + "VareTekst" +
      ";" + "FargeKode" + 
      ";" + "FargeTekst" + 
      ";" + "Str" +       
      ";" + "StrTab" + 
      ";" + "Varemerke" +
      ";" + "Enh" +
      ";" + "AntIEnh" +
      ";" + "LevPrisEngros" +
      ";" + "ValKod" +
      ";" + "forhRab%" +
      ";" + "suppRab%" +
      ";" + "VeilPris" +
      ";" + "PAKstru" +
      ";" + "LevUke1" +
      ";" + "LevUke2" +
      ";" + "LevUke3" +
      ";" + "LevUke4" +
      ";" + "VareGruppe" +
      ";" + "LevNavn" +
      ";" + "Bestillingsnr" +
      ";" + "nettoForh" +
      ";" + "kalkForh" +
      ";" + "BFforh" +
      ";" + "nettoSupp" +
      ";" + "kalkSupp" +
      ";" + "BFsupp" + 
      ";" + "MarkedsPris" +
      ";" + "Sortiment" +
      ";" + "Sesong" +
      ";" + "VPIBildeKode" +
      ";" + "Merknad" 
      /* Tilleggsfelter */
      /*
      ";" + "ErrFlag
      ";" + "LinjeNr
      ";" + "ArtikkelNr
      ";" + "ModellNr
      ";" + "ModellFarge
      ";" + "HovedModellFarge
      ";" + "EkstVPILevNr
      ";" + "Vg
      ";" + "Hg
      ";" + "Farg
      ";" + "StrTypeId
      ";" + "iLevNr
      ";" + "MomsKod
      ";" + "VmId
      ";" + "Sasong
      */
      SKIP
      .
END.
ELSE RETURN.
REPEAT WHILE NOT hQuery:QUERY-OFF-END TRANSACTION:

  FIND FIRST ArtBas WHERE ArtBas.ArtikkelNr = DEC(hBuffer:BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE) NO-LOCK NO-ERROR.
  IF NOT AVAIL ArtBas THEN DO:
    hQuery:GET-NEXT().
    NEXT.
  END.
  FIND varemerke OF artbas NO-LOCK NO-ERROR.
  FIND LevBas OF artbas NO-LOCK NO-ERROR.
  FIND farg OF artbas NO-LOCK NO-ERROR.

  FIND FIRST StrType OF ArtBas NO-LOCK NO-ERROR.
  IF AVAIL StrType THEN DO:
    ASSIGN cStrList    = ""
           cKodeList   = ""
           cStrKodeLSt = ""
           cBestNrLst  = "".
    FIND FIRST StrekKode OF ArtBas WHERE StrekKode.KodeType = 1 NO-LOCK  NO-ERROR.
    IF AVAIL StrekKode THEN
        RUN HentEanStorl (OUTPUT cKodeList,OUTPUT cStrList, OUTPUT cStrKodeLst, OUTPUT cBestNrLSt).
    ELSE
        ASSIGN cStrList    = TRIM(cStrList," ")
               cKodeList   = TRIM(cKodeList," ")
               cStrKodeLSt = TRIM(cStrKodeLst," ")
               cBestNrLst  = TRIM(cBestNrLst," ").
  END.
  ELSE
      ASSIGN cStrList    = " "
             cKodeList   = " "
             cStrKodeLSt = " "
             cBestNrLst  = " ".             

    DO ix = 1 TO NUM-ENTRIES(cKodeList):
        PUT UNFORMATTED
            "R1"
            ";" STRING(ArtBas.levnr)
            ";" REPLACE(hBuffer:BUFFER-FIELD("LevKod"):BUFFER-VALUE,";","")
            ";" TRIM(ENTRY(ix,cKodeList))
            ";" REPLACE(hBuffer:BUFFER-FIELD("Beskr"):BUFFER-VALUE,";","")
            ";" STRING(ArtBas.Farg)
            ";" REPLACE(hBuffer:BUFFER-FIELD("LevFargKod"):BUFFER-VALUE,";","")
            ";" TRIM(ENTRY(ix,cStrList))
            ";" STRING(ArtBas.StrTypeId)
            ";" IF AVAIL Varemerke THEN REPLACE(Varemerke.Beskrivelse,";","") ELSE ""
            ";" ArtBas.SalgsEnhet
            ";" STRING(ArtBas.AntIPakn)
            ";" STRING(hBuffer:BUFFER-FIELD("InnkjopsPris"):BUFFER-VALUE)
            ";" ArtBas.ValKod
            ";" STRING(hBuffer:BUFFER-FIELD("forhRab%"):BUFFER-VALUE)
            ";" STRING(hBuffer:BUFFER-FIELD("supRab%"):BUFFER-VALUE)
            ";" STRING(hBuffer:BUFFER-FIELD("AnbefaltPris"):BUFFER-VALUE)
            ";" ""
            ";" IF hBuffer:BUFFER-FIELD("LevDato1"):BUFFER-VALUE <> ? THEN STRING(hBuffer:BUFFER-FIELD("LevDato1"):BUFFER-VALUE) ELSE ""
            ";" IF hBuffer:BUFFER-FIELD("LevDato2"):BUFFER-VALUE <> ? THEN STRING(hBuffer:BUFFER-FIELD("LevDato2"):BUFFER-VALUE) ELSE ""
            ";" IF hBuffer:BUFFER-FIELD("LevDato3"):BUFFER-VALUE <> ? THEN STRING(hBuffer:BUFFER-FIELD("LevDato3"):BUFFER-VALUE) ELSE ""
            ";" IF hBuffer:BUFFER-FIELD("LevDato4"):BUFFER-VALUE <> ? THEN STRING(hBuffer:BUFFER-FIELD("LevDato4"):BUFFER-VALUE) ELSE ""
            ";" STRING(hBuffer:BUFFER-FIELD("vg"):BUFFER-VALUE)                 
            ";" IF AVAIL levbas THEN LevBas.levnamn ELSE ""
            ";" TRIM(ENTRY(ix,cBestNrLSt))
            ";" STRING(hBuffer:BUFFER-FIELD("varekost"):BUFFER-VALUE)
            ";" STRING(hBuffer:BUFFER-FIELD("forhKalkyle"):BUFFER-VALUE)
            ";" STRING(hBuffer:BUFFER-FIELD("DBKr"):BUFFER-VALUE)
            ";" STRING(hBuffer:BUFFER-FIELD("supVarekost"):BUFFER-VALUE)
            ";" STRING(hBuffer:BUFFER-FIELD("supkalkyle"):BUFFER-VALUE)
            ";" STRING(hBuffer:BUFFER-FIELD("supDBKr"):BUFFER-VALUE)
            ";" STRING(hBuffer:BUFFER-FIELD("Pris"):BUFFER-VALUE)
            ";" IF ArtBas.AnonseArtikkel = TRUE THEN "1" ELSE ""                                    
            ";" STRING(ArtBas.Sasong)                      
            ";" STRING(ArtBAs.VPIBildeKode)                
            ";" STRING(ArtBas.ArtikkelNr) + TRIM(ENTRY(ix,cStrKodeLst)) + '|' + hBuffer:BUFFER-FIELD("Linjemerknad"):BUFFER-VALUE                  
            /* Tilleggsfelter */
            /*
            ";" ttPrikat.ErrFlag
            ";" ttPrikat.LinjeNr
            ";" ttPrikat.ArtikkelNr
            ";" ttPrikat.ModellNr
            ";" ttPrikat.ModellFarge
            ";" ttPrikat.HovedModellFarge
            ";" ttPrikat.EkstVPILevNr
            ";" ttPrikat.Vg
            ";" ttPrikat.Hg
            ";" ttPrikat.Farg
            ";" ttPrikat.StrTypeId
            ";" ttPrikat.iLevNr
            ";" ttPrikat.MomsKod
            ";" ttPrikat.VmId
            ";" ttPrikat.Sasong
            */
            SKIP
            .
    END.
  hQuery:GET-NEXT().
END.

OUTPUT CLOSE.
Gammel kode --------------------------*/    

DELETE OBJECT hBuffer NO-ERROR.
DELETE OBJECT hQuery  NO-ERROR.

IF ocReturn = "" THEN obOk = TRUE.

PROCEDURE HentEanStorl:
    DEFINE OUTPUT PARAMETER cEan     AS CHARACTER  NO-UNDO.
    DEFINE OUTPUT PARAMETER cStorl   AS CHARACTER  NO-UNDO.
    DEFINE OUTPUT PARAMETER cStrKode AS CHARACTER  NO-UNDO.
    DEFINE OUTPUT PARAMETER cBestNr  AS CHARACTER  NO-UNDO.

    DEFINE VARIABLE ii AS INTEGER    NO-UNDO.
    DO ii = 1 TO NUM-ENTRIES(StrType.Fordeling):
        FIND FIRST StrekKode WHERE Strekkode.ArtikkelNr = ArtBas.ArtikkelNr AND
                                   StrekKode.StrKode = INT(ENTRY(ii,StrType.Fordeling)) AND 
                               NOT StrekKode.Kode BEGINS "02" NO-LOCK NO-ERROR.
        IF NOT AVAIL Strekkode THEN
            FIND FIRST StrekKode WHERE Strekkode.ArtikkelNr = ArtBas.ArtikkelNr AND
                                       StrekKode.StrKode = INT(ENTRY(ii,StrType.Fordeling)) AND 
                                       StrekKode.Kode BEGINS "02" NO-LOCK NO-ERROR.
        IF AVAIL StrekKode AND StrekKode.Kode BEGINS "02" THEN
            ASSIGN cStorl    = cStorl + (IF cStorl <> "" THEN "," ELSE "") + TRIM(ENTRY(ii,StrType.AlfaFordeling))
                   cEan      = cEan + (IF cEan <> "" THEN "," ELSE "") + " "
                   cStrKode  = cStrKode + (IF cStrKode <> "Q" THEN "," ELSE "") + string(StrekKode.StrKode,">>999")
                   cBestNr   = cBestNr + (IF cBestNr <> "Q" THEN "," ELSE "") + StrekKode.Bestillingsnummer
                   .
        ELSE IF AVAIL StrekKode THEN
            ASSIGN cStorl   = cStorl + (IF cStorl <> "" THEN "," ELSE "") + TRIM(ENTRY(ii,StrType.AlfaFordeling))
                   cEan     = cEan + (IF cEan <> "" THEN "," ELSE "") + StrekKode.Kode
                   cStrKode = cStrKode + (IF cStrKode <> "Q" THEN "," ELSE "") + string(StrekKode.StrKode,">>999")
                   cBestNr  = cBestNr + (IF cBestNr <> "Q" THEN "," ELSE "") + StrekKode.Bestillingsnummer
                   .
    END.

END PROCEDURE.
