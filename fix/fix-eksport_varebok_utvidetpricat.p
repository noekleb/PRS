CURRENT-WINDOW:WIDTH = 250.

DEF VAR lVareBokNr AS DEC  NO-UNDO.
DEF VAR cHeader    AS CHAR NO-UNDO.
DEF VAR cEan       AS CHAR NO-UNDO.
DEF VAR cStorl     AS CHAR NO-UNDO.
DEF VAR cBestNR    AS CHAR NO-UNDO.
DEF VAR iLevUke1   AS INT FORMAT "zzzzzzz9" NO-UNDO.
DEF VAR iLevUke2   AS INT FORMAT "zzzzzzz9" NO-UNDO.
DEF VAR iLevUke3   AS INT FORMAT "zzzzzzz9" NO-UNDO.
DEF VAR iLevUke4   AS INT FORMAT "zzzzzzz9" NO-UNDO.
DEF VAR cValgtSort AS CHAR NO-UNDO.
DEF VAR cVarefakta AS CHAR NO-UNDO.


DEF VAR cTekst        AS CHAR NO-UNDO.
DEF VAR cTekst2       AS CHAR NO-UNDO.
DEF VAR iTotAnt       AS INT  NO-UNDO.
DEF VAR cInndelingLst AS CHAR NO-UNDO.
DEF VAR cStrList      AS CHAR NO-UNDO.
DEF VAR cKodeList     AS CHAR NO-UNDO.
DEF VAR bInndeling    AS LOG  NO-UNDO.
DEF VAR ix            AS INT  NO-UNDO.
DEF VAR iy            AS INT  NO-UNDO.
DEF VAR bUse02kode    AS LOG  NO-UNDO.
DEF VAR piLoop        AS INT  NO-UNDO.
DEF VAR cBestNrList   AS CHAR NO-UNDO.
DEF VAR cLevBestNr    AS CHAR NO-UNDO.

DEF STREAM Ut.

ASSIGN
    lVareBokNr = 10000001
    .

/* Header */
{headerstring.i}


FIND VareBokHode NO-LOCK WHERE
    VareBokHode.VareBokNr = lVareBokNr.

OUTPUT STREAM Ut TO VALUE("Varebok_jan_08.csv") NO-ECHO.

/* Heading */
PUT STREAM Ut UNFORMATTED cHeader SKIP.

FOR EACH VareBokLinje OF VareBokHode NO-LOCK:
  ASSIGN
      bInndeling = FALSE.

  FIND ArtBas OF VareBokLinje NO-LOCK.
  FIND Varemerke OF ArtBas NO-LOCK NO-ERROR.
  FIND Sasong OF ArtBas NO-LOCK NO-ERROR.

  bUse02kode = NOT CAN-FIND(FIRST StrekKode OF ArtBas WHERE NOT StrekKode.Kode BEGINS "02").
  RUN weeknum.p (VareBokLinje.LevDato1,OUTPUT iLevUke1).
  RUN weeknum.p (VareBokLinje.LevDato2,OUTPUT iLevUke2).
  RUN weeknum.p (VareBokLinje.LevDato3,OUTPUT iLevUke3).
  RUN weeknum.p (VareBokLinje.LevDato4,OUTPUT iLevUke4).

  IF iLevUke1 = ? THEN iLevUke1 = 0.
  IF iLevUke2 = ? THEN iLevUke2 = 0.
  IF iLevUke3 = ? THEN iLevUke3 = 0.
  IF iLevUke4 = ? THEN iLevUke4 = 0.

  IF can-find(FIRST ArtSort WHERE ArtSort.ArtikkelNr = VarebokLinje.ArtikkelNr) 
      THEN cValgtSort = "*".
      ELSE cValgtSort = "".
  IF ArtBas.VareFakta <> "" 
      THEN cVarefakta = "*".
      ELSE cVarefakta = "".

  IF CAN-FIND(FIRST ArtSort WHERE ArtSort.ArtikkelNr = VareBokLinje.ArtikkelNr) THEN
  DO:
      FOR EACH ArtSort NO-LOCK WHERE
          ArtSort.ArtikkelNr = VareBokLinje.ArtikkelNr:
          FIND FIRST LevSort OF Artsort NO-LOCK NO-ERROR.

          ASSIGN
              cTekst  = ""
              cTekst2 = ""
              iTotAnt = 0
              .
          FOR EACH LevSAnt OF LevSort NO-LOCK BREAK BY LevSAnt.SeqNr:
              ASSIGN
                  cTekst  = cTekst + string(LevSAnt.SoAnt) + ","
                  iTotAnt = iTotAnt + LevSAnt.SoAnt 
                  .
              IF FIRST(LevSAnt.SeqNr) THEN cTekst2 = LevSAnt.SoStorl + "-".
              IF LAST(LevSAnt.SeqNr) THEN cTekst2 = cTekst2 + LevSAnt.SoStorl.
          END.
          cTekst = TRIM(cTekst,",").

          cInndelingLst = cInndelingLst + (ArtSort.SortId + "|" + 
                                           cTekst + "|" + 
                                           cTekst2) + "|" + 
                                           string(iTotAnt) + ";".
      END.
      cInndelingLst = TRIM(cInndelingLst,";").
  END.
  ELSE cInndelingLst = "".

  /* VarebokLinjer. */
  /*RUN Utlegg.*/
  FIND FIRST StrType OF ArtBas NO-LOCK NO-ERROR.
  IF AVAIL StrType THEN DO:
    ASSIGN cStrList    = ""
           cKodeList   = ""
           cBestNrList = ""
           bInndeling  = FALSE.
    IF NOT ArtBas.FrittTillegg THEN 
      FOR EACH ArtSort NO-LOCK
          WHERE ArtSort.ArtikkelNr = ArtBas.ArtikkelNr
         ,FIRST LevSort OF ArtSort NO-LOCK:
  
        bInndeling = TRUE.
        FOR EACH LevSAnt OF LevSort NO-LOCK BY SeqNr:
          IF NOT CAN-DO(cStrList,TRIM(LevSAnt.SoStorl)) THEN
            cStrList = cStrList + TRIM(LevSAnt.SoStorl) + ",".
        END.
      END.
    cStrList   = TRIM(cStrList,",").

    DO ix = 1 TO NUM-ENTRIES(StrType.fordeling):
      FOR EACH StrekKode OF ArtBas NO-LOCK
          WHERE Strekkode.kode > ""
            AND StrekKode.StrKode = INT(ENTRY(ix,StrType.fordeling))
        , FIRST StrKonv OF StrekKode NO-LOCK
          BREAK BY StrekKode.StrKode:

        IF NOT bUse02kode AND StrekKode.kode BEGINS "02" THEN NEXT.

        IF LAST-OF(StrekKode.StrKode) THEN DO:
          IF bInndeling AND CAN-DO(cStrList,TRIM(StrKonv.Storl)) THEN
              ASSIGN
              cKodeList   = cKodeList   + Strekkode.kode + ","
              cBestNrList = cBestNrList + Strekkode.Bestillingsnummer + ",".
          ELSE IF NOT bInndeling AND NOT CAN-DO(cKodeList,Strekkode.kode) THEN 
            ASSIGN cKodeList   = cKodeList   + Strekkode.kode + ","
                   cBestNrList = cBestNrList + Strekkode.Bestillingsnummer + "," 
                   cStrList    = cStrList    + TRIM(StrKonv.Storl) + ",".
        END.
      END.
    END.

    ASSIGN cStrList    = TRIM(cStrList,",")
           cKodeList   = TRIM(cKodeList,",").
           cBestNrList = substring(cBestNrList,1,LENGTH(cBestNrList) - 1).

    IF bInndeling AND
       NUM-ENTRIES(cStrList) NE NUM-ENTRIES(cKodeList) AND
       NOT bUse02Kode THEN DO:
      cKodeList = "".
      cBestNrList = "".
      DO ix = 1 TO NUM-ENTRIES(StrType.fordeling):
        FOR EACH StrekKode OF ArtBas NO-LOCK
            WHERE Strekkode.kode > ""
              AND StrekKode.StrKode = INT(ENTRY(ix,StrType.fordeling))
          , FIRST StrKonv OF StrekKode NO-LOCK
            BREAK BY StrekKode.StrKode:
    
          IF LAST-OF(StrekKode.StrKode) AND CAN-DO(cStrList,TRIM(StrKonv.Storl)) THEN
              ASSIGN
              cKodeList = cKodeList + Strekkode.kode + ","
              cBestNrList = cBestNrList + Strekkode.Bestillingsnummer + "," 
              .
        END.
      END.
    END.

    ASSIGN cStrList    = TRIM(cStrList,",")
           cKodeList   = TRIM(cKodeList,",")
           /*cBestNrList = substring(cBestNrList,1,LENGTH(cBestNrList) - 1)*/.
    
    /*
    MESSAGE 
        "cStrList   "  cStrList    NUM-ENTRIES(cStrList)  SKIP
        "cKodeList  "  cKodeList   NUM-ENTRIES(cKodeList)  SKIP
        "cBestNrList"  cBestNrList NUM-ENTRIES(cBestNrList)
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
    */

    DO ix = 1 TO MIN(NUM-ENTRIES(cStrList),NUM-ENTRIES(cKodeList)):
/*       DO iy = 1 TO hBuffer:NUM-FIELDS:                                     */
/*         IF CAN-DO(ENTRY(3,icParam,"¤"),hBuffer:BUFFER-FIELD(iy):NAME) THEN */
/*           PUT UNFORMATTED hBuffer:BUFFER-FIELD(iy):BUFFER-VALUE ";".       */
/*       END.                                                                 */
      ASSIGN
          cEan       = ENTRY(ix,cKodeList)
          cStorl     = replace(ENTRY(ix,cStrList),".",",")
          /*cBestNr    = ENTRY(ix,cBestNrList)*/
          .
      IF NUM-ENTRIES(cStrList) = 1 THEN
          cStrList = REPLACE(cStrList,".",",").

      RUN Utlegg.

      FIND Strekkode NO-LOCK WHERE
          Strekkode.Kode = ENTRY(ix,cKodeList) NO-ERROR.
      IF AVAILABLE Strekkode THEN
          PUT STREAM ut UNFORMATTED string(ArtBas.ArtikkelNr) + string(Strekkode.StrKode,"999") ";".
      ELSE PUT STREAM ut " " ";".

      PUT STREAM Ut UNFORMATTED 
          iLevUke1 ";"
          iLevUke2 ";"
          iLevUke3 ";"
          iLevUke4 ";"                      
          replace(ENTRY(ix,cStrList),".",",") ";"
          ENTRY(ix,cKodeList) ";"
          .
            

      IF cInndelingLst <> "" THEN
      DO piLoop = 1 TO NUM-ENTRIES(cInndelingLst,";"):
          IF ENTRY(piLoop,(cInndelingLst),";") <> "" THEN
              PUT STREAM ut UNFORMATTED entry(piLoop,cInndelingLst,";") ";".
      END.
      PUT STREAM Ut UNFORMATTED SKIP.
    END.

  END.

END.

OUTPUT CLOSE.

/* Utlegg av vareboklinje. */
PROCEDURE Utlegg:
/* MESSAGE                                */
/*     VareBokLinje.levnr                 */
/*     VareBokLinje.LevKod                */
/*     VareBokLinje.Beskr                 */
/*     VIEW-AS ALERT-BOX INFO BUTTONS OK. */

    PUT STREAM Ut UNFORMATTED
                "R1;"
                VareBokLinje.levnr ";"
                VareBokLinje.LevKod ";"
                cEan ";" /*";" TRIM(ENTRY(ix,cKodeList))  EAN koden for størrelsen - En linje for hver strekkode som er registrert */
                VareBokLinje.Beskr ";"
                string(ArtBas.Farg) ";"
                VareBokLinje.LevFargKod ";"
                cStorl ";" /*";" TRIM(ENTRY(ix,cStrList))  /* Størrelse - En linje for hver strekkode som er registrert */*/
                ArtBas.STrTypeId ";" 
                (IF AVAIL Varemerke THEN Varemerke.Beskrivelse ELSE '') ";" 
                ArtBas.SalgsEnhet ";"
                ArtBas.AntIPakn ";"
                VareBokLinje.InnkjopsPris ";"
                ArtBas.ValKod ";"
                VareBokLinje.forhRab% ";"
                VareBokLinje.supRab% ";"
                VareBokLinje.AnbefaltPris ";"
                " ;"  /* Her skal det legges ut en blank kolonne med label PAKstru */
                VareBokLinje.LevDato1 ";"
                VareBokLinje.LevDato2 ";"
                VareBokLinje.LevDato3 ";"
                VareBokLinje.LevDato4 ";"
                VareBokLinje.Vg ";"
                VareBokLinje.levnamn ";"
                cBestNr ";" /*TRIM(ENTRY(ix,cBestNrLSt)) /* Lev.art.nr på størrelsen (Fra strekkode.Bestillingsnummer) */*/
                VareBokLinje.VareKost ";"
                VareBokLinje.forhKalkyle ";"
                VareBokLinje.DBKr ";"
                VareBokLinje.supVareKost ";"
                VareBokLinje.supKalkyle ";"
                VareBokLinje.supDBKr ";"
                VareBokLinje.Pris ";"
                (IF ArtBas.AnonseArtikkel = TRUE  THEN "1" ELSE "") ";" 
                VareBokLinje.Sasong ";"
                "mini" + STRING(ArtBas.ArtikkelNr) + ".jpg" ";" /*STRING(ArtBAs.VPIBildeKode)   Ligger ikke på vareboklinje */
                replace(replace(VareBokLinje.LinjeMerknad,CHR(13)," "),CHR(10)," ") ";"
                " " ";"
                " " ";"
                " " ";"
                " " ";"
                /* Pricat slutt */

                /* Øvrige felt legges bak */
                VareBokLinje.Sekv ";"
                VareBokLinje.KampanjePris ";"
                " " /*VareBokLinje.LevUke1*/ ";"
                VareBokLinje.Sortimentkoder ";"
                VareBokLinje.Kampanjeuker ";"
                VareBokLinje.Kampanjestotte ";"
                VareBokLinje.Lagerkoder ";"
                (IF AVAILABLE Sasong 
                   THEN Sasong.SasBeskr
                   ELSE "") ";"
                VareBokLinje.KjedeVare ";"
                VareBokLinje.Gjennomfaktureres ";"
                cVarefakta ";"
                ArtBas.FrittTillegg ";" 
                cValgtSort ";"
                VareBokLinje.KjedeRab% ";"
                VareBokLinje.KjedeInnkPris ";"
                VareBokLinje.Antall ";"
                VareBokLinje.supAntall ";"
                VareBokLinje.DB% ";"
                VareBokLinje.supDB% ";"
                VareBokLinje.Mva% ";"
                VareBokLinje.ArtikkelNr ";"
                VareBokLinje.ModellFarge ";"
                VareBokLinje.VgBeskr ";"
                VareBokLinje.Hg ";"
                VareBokLinje.HgBeskr ";"
                VareBokLinje.AvdelingNr ";"
                VareBokLinje.AvdelingNavn ";"
                VareBokLinje.ProdNr ";"
                VareBokLinje.ProdusentBeskrivelse ";"
                VareBokLinje.EDato ";"
                /*VareBokLinje.cEndretTid*/ ";"
                VareBokLinje.BrukerID ";"
                VareBokLinje.VPIDato ";"
                " ;" /*VareBokLinje.AndreVareboker*/
                " ;" /*VareBokLinje.Beskrivelse*/
                " ;" /*VareBokLinje.AktNr*/
                " ;" /* VareBokLinje.Beskrivelse6 */ 
                ArtBas.VPIBildeKode ";"
                ArtBas.Pakke ";"
        .

END PROCEDURE.
