/* Sender plukklsite til fil 
   Parametere:  buffersandfields
                query 
   
   Opprettet: 24.08.05 av BHa 
-----------------------------------------------------------------------------------*/
DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.

DEF VAR hQuery            AS HANDLE NO-UNDO.
DEF VAR hBuffer           AS HANDLE NO-UNDO.
DEF VAR hBuffArtBas       AS HANDLE NO-UNDO.
DEF VAR hBuffStrKonv      AS HANDLE NO-UNDO.
DEF VAR hBuffStrTStr      AS HANDLE NO-UNDO.
DEF VAR cEanNrLst         AS CHAR   NO-UNDO.
DEF VAR cFilNavn          AS CHAR   NO-UNDO.
DEF VAR iAntLinjer        AS INT    NO-UNDO.
DEF VAR iAntFeil          AS INT    NO-UNDO.
DEF VAR cLevKod           AS CHAR   NO-UNDO.
DEF VAR cBeskr            AS CHAR   NO-UNDO.
DEF VAR cLevFargKod       AS CHAR   NO-UNDO.
DEF VAR cSendesKatalog    AS CHAR   NO-UNDO.
DEF VAR cVaretekst        AS CHAR   NO-UNDO.
DEF VAR iVaretekst        AS INT    NO-UNDO.
DEF VAR iEAN              AS INT    NO-UNDO.
DEF VAR iSortering        AS INT    NO-UNDO.
DEF VAR cSortering        AS CHAR   NO-UNDO.
DEF VAR cLopNr            AS CHAR   NO-UNDO.
DEF VAR cLager            AS CHAR   NO-UNDO.
DEF VAR cTekst            AS CHAR   NO-UNDO.

DEF VAR dPlListeId  LIKE PlListeHode.PlListeId NO-UNDO.

/* Innhold i varetekst */
{syspara.i 11 5 1 iVaretekst INT}
/* EAN kode eller bestillingsnummer */
{syspara.i 11 5 5 iEAN INT}
/* Sortering på utlegg */
{syspara.i 11 5 6 iSortering INT}

/* Henter eksportkatalog. */
{syspara.i 1 1 51 cSendesKatalog} /* Mulig dette skal skilles ut som egen parameter */
IF cSendesKatalog = "" THEN
DO:
    {syspara.i 1 1 51 cSendesKatalog} /* Henter standard katalog for eksport */
END.

ASSIGN 
    dPlListeId = DECI(ENTRY(1,icParam,"¤"))
    .
    
FIND PlListeHode NO-LOCK WHERE
    PlListeHode.PlListeId = dPlListeId NO-ERROR.

/*  TN 11/4-18 JF har sortering 2 satt */
CASE iSortering:
    WHEN 0 THEN cSortering = " By Beskr By PlListeLinje.ArtikkelNr By StrTStr.SeqNr".
    WHEN 1 THEN cSortering = " By LevNr By LevKod By PlListeLinje.ArtikkelNr By StrTStr.SeqNr".
    WHEN 2 THEN cSortering = " By VarGr By LopNr By StrTStr.SeqNr".
END CASE.

CREATE QUERY hQuery.
CREATE BUFFER hBuffer FOR TABLE "PlListeLinje".
CREATE BUFFER hBuffArtBas FOR TABLE "ArtBas".
CREATE BUFFER hBuffStrKonv FOR TABLE "StrKonv".
CREATE BUFFER hBuffStrTStr FOR TABLE "StrTStr".
hQuery:SET-BUFFERS(hBuffer,hBuffArtBas,hBuffStrKonv,hBuffStrTStr).

hQuery:QUERY-PREPARE("For each PlListeLinje no-lock where PlListeLinje.PlListeId = '" + STRING(dPlListeId) + "'" 
                     + ",first Artbas no-lock where ArtBas.ArtikkelNr = PlListeLinje.ArtikkelNr OUTER-JOIN"
                     + ",first StrKonv no-lock where StrKonv.StrKode = PlListeLinje.StrKode  OUTER-JOIN"
                     + ",first StrTStr no-lock where StrTStr.StrTypeId = ArtBas.StrTypeId and StrTStr.SoStorl = StrKonv.Storl OUTER-JOIN"
                     + cSortering
                      ).
hQuery:QUERY-OPEN().

hQuery:GET-FIRST().
IF hBuffer:AVAIL THEN DO:

  ASSIGN
      cSendesKatalog = RIGHT-TRIM(cSendesKatalog,"\")
      cFilNavn       = cSendesKatalog + "\PLUKK" + 
                       SUBstring(STRING(YEAR(TODAY),"9999"),3) + 
                       STRING(MONTH(TODAY),"99") + 
                       STRING(DAY(TODAY),"99") + "-" +
                       string(TIME) + "." + 
                       STRING(PlListeHode.TilButikkNr)
      .

  OUTPUT TO VALUE(cFilNavn).
/* Utlegg av header informasjon. */
/*   PUT UNFORMATTED             */
/*       ";" +                   */
/*       ";" + "LevNr" +         */
/*       SKIP                    */
/*       .                       */
END.
ELSE RETURN.
REPEAT WHILE NOT hQuery:QUERY-OFF-END TRANSACTION:
  /*
  FIND FIRST ArtBas WHERE ArtBas.ArtikkelNr = DEC(hBuffer:BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE) NO-LOCK NO-ERROR.
  IF NOT AVAIL ArtBas THEN DO:
    hQuery:GET-NEXT().
    iAntFeil = iAntFeil + 1.
    NEXT.
  END.
  */

  FIND VarGr NO-LOCK WHERE
      VarGr.Vg = int(hBuffArtBas:BUFFER-FIELD("Vg"):BUFFER-VALUE) NO-ERROR.
  IF NOT AVAIL VarGr THEN DO:
    hQuery:GET-NEXT().
    iAntFeil = iAntFeil + 1.
    NEXT.
  END.
/*
  FIND StrKonv NO-LOCK WHERE
      StrKonv.StrKode = INT(hBuffStrKonv:BUFFER-FIELD("StrKode"):BUFFER-VALUE) NO-ERROR.
  IF NOT AVAIL StrKonv THEN DO:
    hQuery:GET-NEXT().
    iAntFeil = iAntFeil + 1.
    NEXT.
  END.
  */
  /* Henter lager. */
/*   IF AVAILABLE StrKonv THEN */
  /* Frånbutiks lager */
  FIND ArtLag NO-LOCK WHERE
          ArtLag.ArtikkelNr = dec(hBuffArtBas:BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE) AND
          ArtLag.Butik      = 20 AND
          ArtLag.StrKode    = int(hBuffStrKonv:BUFFER-FIELD("StrKode"):BUFFER-VALUE) NO-ERROR.
  IF AVAILABLE ArtLag THEN
      cLager = " (" + STRING(ArtLag.LagAnt) + "-".
  ELSE
      cLager = "".
  /* Tillbutiks lager */
  FIND ArtLag NO-LOCK WHERE
        ArtLag.ArtikkelNr = dec(hBuffArtBas:BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE) AND
        ArtLag.Butik      = PlListeHode.TilButikk AND
        ArtLag.StrKode    = int(hBuffStrKonv:BUFFER-FIELD("StrKode"):BUFFER-VALUE) NO-ERROR.
  IF AVAILABLE ArtLag THEN
      cLager = cLager + STRING(ArtLag.LagAnt) + ")".
  ELSE
      cLager = "".

  IF hBuffArtBas:BUFFER-FIELD("LopNr"):BUFFER-VALUE = ? THEN cLopNr = "?".
  ELSE IF hBuffArtBas:BUFFER-FIELD("LopNr"):BUFFER-VALUE <= 9999 THEN cLopNr = STRING(hBuffArtBas:BUFFER-FIELD("LopNr"):BUFFER-VALUE).
  ELSE cLopNr = STRING(hBuffArtBas:BUFFER-FIELD("LopNr"):BUFFER-VALUE).

  /* Innhold i varetekst */
  CASE iVaretekst:
      WHEN 0 THEN cBeskr = STRING(hBuffArtBas:BUFFER-FIELD("Vg"):BUFFER-VALUE) + "/" + cLopNr + " " + hBuffStrKonv:BUFFER-FIELD("Storl"):BUFFER-VALUE + cLager.
      WHEN 1 THEN cBeskr = SUBSTRING(hBuffArtBas:BUFFER-FIELD("Beskr"):BUFFER-VALUE,1,40) + cLager.
      WHEN 2 THEN cBeskr = SUBSTRING(STRING(hBuffArtBas:BUFFER-FIELD("LevNr"):BUFFER-VALUE) + "/" + hBuffArtBas:BUFFER-FIELD("LevKod"):BUFFER-VALUE,1,40) + cLager.
  END CASE.

  /* Stripper tekstfelt for karrakterer som ikke skal kunne legges ut i feltene. */
  ASSIGN
      cLevKod     = REPLACE(hBuffArtBas:BUFFER-FIELD("LevKod"):BUFFER-VALUE,";",",")
      cBeskr      = REPLACE(cBeskr,";",",")
      cLevFargKod = REPLACE(hBuffArtBas:BUFFER-FIELD("LevFargKod"):BUFFER-VALUE,";",",")
      cEanNrLst   = ""
      .

  FOR EACH Strekkode NO-LOCK WHERE
      Strekkode.ArtikkelNr = dec(hBuffArtBas:BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE) AND
      Strekkode.StrKode = int(hBuffStrKonv:BUFFER-FIELD("StrKode"):BUFFER-VALUE):
      IF LENGTH(cEanNrLst) + length(Strekkode.Kode) < 1900 THEN
      cEanNrLst = cEanNrLst +
                  (IF cEanNrLst = "" THEN "" ELSE ",") + 
                  (IF iEAN = 0 THEN Strekkode.Kode ELSE Strekkode.Bestillingsnummer).
  END.
  
  /* Antall linjer lagt ut i filen */
  iAntLinjer = iAntLinjer + 1.

  /*  Fileksempel ....
  12151;Damsko hög;Brun;Vinterskor;12;2;XL;23;5;12;12345;7310870008758,001106400100,111,1111;
  12151;Snoojogger;Brun;Vinterskor;12;2;XL;23;6;12;12345;001106400095,222,2222;
  12151;Foppatofflor;Brun;Vinterskor;12;2;XL;23;4;12;12345;333,3333;
  */

  PUT UNFORMATTED
      SUBSTRING(cLevKod,1,30)
      ";" SUBSTRING(cBeskr,1,30)
      ";" SUBSTRING(cLevFargKod,1,30)
      ";" SUBSTRING(VarGr.VgBeskr,1,40)
      ";" STRING(hBuffArtBas:BUFFER-FIELD("Vg"):BUFFER-VALUE)
      ";" (IF hBuffArtBas:BUFFER-FIELD("LopNr"):BUFFER-VALUE = ? THEN '0' ELSE STRING(hBuffArtBas:BUFFER-FIELD("LopNr"):BUFFER-VALUE))
      ";" SUBSTRING(hBuffStrKonv:BUFFER-FIELD("Storl"):BUFFER-VALUE,1,10)
      ";" STRING(hBuffer:BUFFER-FIELD("StrKode"):BUFFER-VALUE)
      ";" STRING(hBuffer:BUFFER-FIELD("Antall"):BUFFER-VALUE)
      ";" STRING(PlListeHode.FraButikkNr)
      ";" STRING(PlListeHode.TilButikkNr)
      ";" cEanNrLst
      ";" TRIM(STRING(PlListeHode.PlListeId,">>>>>>>>>>>>9"))
      ";"
      SKIP
      .
  hQuery:GET-NEXT().
END.

OUTPUT CLOSE.

DELETE OBJECT hBuffer NO-ERROR.
DELETE OBJECT hQuery  NO-ERROR.

IF iAntLinjer > 0 THEN
DO TRANSACTION:
    FIND CURRENT PlListeHode EXCLUSIVE-LOCK.
    ASSIGN
        PlListeHode.SendtPda = TODAY
        PlListeHode.plListeStatus = 10 /* Sendt Pda */
        .
    RELEASE PlListeHode.
END.

IF ocReturn = "" THEN obOk = TRUE.

