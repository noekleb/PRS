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
DEF VAR iAntLinjer        AS INT    NO-UNDO.

DEF VAR dPlListeId  LIKE PlListeHode.PlListeId NO-UNDO.

ASSIGN 
    dPlListeId = DECI(ENTRY(2,icParam,"¤"))
    iAntLinjer = 2.
    
FIND PlListeHode NO-LOCK WHERE
    PlListeHode.PlListeId = dPlListeId NO-ERROR.
IF NOT AVAILABLE PlListeHode THEN
    RETURN.

CREATE QUERY hQuery.
CREATE BUFFER hBuffer FOR TABLE "PlListeLinje".
hQuery:SET-BUFFERS(hBuffer).
hQuery:QUERY-PREPARE("For each PlListeLinje no-lock where PlListeLinje.PlListeId = " + ENTRY(2,icParam,"¤")).
hQuery:QUERY-OPEN().

hQuery:GET-FIRST().
IF hBuffer:AVAIL THEN DO:
  OUTPUT TO VALUE(ENTRY(1,icParam,"¤")).
  PUT UNFORMATTED
      "PLUKKLISTE " string(PlListeHode.PlListeId) SKIP
        "Pl.listenr"
      + ";PlLinjeNr"
      + ";Prioritet"
      + ";Fra butikk"
      + ";Til butikk"
      + ";Listenavn"
      + ";BuntNr"
      + ";ArtikkelNr"
      + ";LevKod"
      + ";Beskr"
      + ";LevFargKod"
      + ";Antall"
      + ";AntallPlukket"
      + ";Diff"
      + ";Størrelse"
      + ";StrKode"
      + ";VarGr"
      + ";Plukket"
      + ";Overført"
      + ";Endret"
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

  FIND StrKonv NO-LOCK WHERE
      StrKonv.StrKode = INT(hBuffer:BUFFER-FIELD("StrKode"):BUFFER-VALUE) NO-ERROR.
  IF NOT AVAILABLE StrKonv THEN DO:
      hQuery:GET-NEXT().
      NEXT.
    END.

  PUT UNFORMATTED
    STRING(PlListeHode.PlListeId)
    ";" STRING(hBuffer:BUFFER-FIELD("PlLinjeNr"):BUFFER-VALUE)
    ";" STRING(PlListeHode.PrioPlukket)
    ";" STRING(PlListeHode.FraButikk)
    ";" STRING(PlListeHode.TilButikk)
    ";" STRING(PlListeHode.PlNavn)
    ";" STRING(PlListeHode.BuntNr)
    ";" STRING(hBuffer:BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE)
    ";" ArtBas.LevKod
    ";" ArtBas.Beskr
    ";" ArtBas.LevFargKod
    ";" STRING(hBuffer:BUFFER-FIELD("Antall"):BUFFER-VALUE)
    ";" STRING(hBuffer:BUFFER-FIELD("AntallPlukket"):BUFFER-VALUE)
    ";" STRING(hBuffer:BUFFER-FIELD("Antall"):BUFFER-VALUE - hBuffer:BUFFER-FIELD("AntallPlukket"):BUFFER-VALUE)
    ";" StrKonv.Storl
    ";" STRING(StrKonv.StrKode)
    ";" STRING(ArtBas.Vg)
    ";" STRING(PlListeHode.DatoPlukket)
    ";" STRING(PlListeHode.OverfortDato)
    ";" STRING(PlListeHode.EDato)
    SKIP
    .
  iAntLinjer = iAntLinjer + 1.
  hQuery:GET-NEXT().
END.

OUTPUT CLOSE.

DELETE OBJECT hBuffer NO-ERROR.
DELETE OBJECT hQuery  NO-ERROR.

ASSIGN
    obOk     = TRUE
    ocReturn = STRING(iAntLinjer).
RETURN ocReturn.
