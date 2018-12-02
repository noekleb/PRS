/* Overføring av priser til butikk 
   Parametere:  temp-tabell med feltet Artikkelnr 
              eller
                 Liste over rowid's med artikler i parameterstreng:
                   <"ROWID">,<Rowid1,Rowid2..>
              eller
                   Liste over artikkelnr i parameterstreng:
                   <"ARTNR">,<Artnr1,Artnr2..>

   Opprettet: 9/2-11 av TN                  
-----------------------------------------------------------------------------------*/

DEF INPUT  PARAM icParam     AS CHAR   NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR   NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR   NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG    NO-UNDO.

DEF VAR hQuery        AS HANDLE NO-UNDO.
DEF VAR ix            AS INT    NO-UNDO.
DEF VAR httTable      AS HANDLE NO-UNDO.
DEF VAR cFeilArtPris  AS CHAR   NO-UNDO. 
DEF VAR cTid          AS CHAR   NO-UNDO.
DEF VAR ii            AS INT    NO-UNDO.
DEF VAR cEDBSystem    AS CHAR   NO-UNDO.
DEF VAR lArtikkelNr   AS DEC    NO-UNDO.
DEFINE VARIABLE iCl   AS INTEGER NO-UNDO.
DEFINE VARIABLE cButLst   AS CHARACTER NO-UNDO.
DEFINE VARIABLE cArtNrLst AS CHARACTER NO-UNDO.
DEFINE VARIABLE iLoop     AS INTEGER   NO-UNDO.
DEFINE VARIABLE iAnt      AS INTEGER   NO-UNDO.

DEFINE BUFFER clButiker FOR Butiker. 

{syspara.i 5 1 1 iCL INT}
FIND clButiker NO-LOCK WHERE
  clButiker.butik = iCL NO-ERROR.
IF NOT AVAILABLE clButiker THEN
DO: 
  ASSIGN 
    ocReturn = '** Ukjent butikk satt opp som sentrallager.'
    obOk     = FALSE.
  RETURN.
END.

ASSIGN 
    cEDBSystem    = IF NUM-ENTRIES(icParam,"|") > 1 THEN 
                      ENTRY(2,icParam,"|") ELSE "" /* Er bare satt når det valgt at det skal sendes til butikk */
    cArtNrLst     = IF NUM-ENTRIES(icParam,"|") > 2 THEN 
                      ENTRY(3,icParam,"|") ELSE "" /* Er bare satt når det sendes med en liste med artikkelnr. */
    cButLst       = IF NUM-ENTRIES(icParam,"|") > 4 THEN 
                      ENTRY(5,icParam,"|") ELSE "" /* Er bare satt når det sendes med en liste med butikknr. */
    cTid          = STRING(TODAY) + " " + STRING(TIME,"HH:MM:SS").

/* Sending til ALLE butikker hvis butikklisten er blank. */
IF TRIM(cButLst) = '' THEN 
DO:
  FOR EACH Butiker NO-LOCK WHERE
    Butiker.ApningsDato <> ? AND 
    Butiker.NedlagtDato = ? AND  
    Butiker.harButikksystem = TRUE:
    ASSIGN
      cButLst = cButLst + 
                (IF cButLst = '' THEN '' ELSE ',') +
                STRING(Butiker.Butik).
  END. 
END.

IF NOT VALID-HANDLE(ihBuffer) AND NUM-ENTRIES(icParam,'|') > 1 THEN DO:
  CREATE TEMP-TABLE httTable.
  httTable:ADD-LIKE-FIELD("ArtikkelNr","ArtBas.ArtikkelNr").
  httTable:TEMP-TABLE-PREPARE("ttArtBas").
  ihBuffer = httTable:DEFAULT-BUFFER-HANDLE.

  IF ENTRY(1,icParam,"|") = "ROWID" THEN
    DO ix = 2 TO NUM-ENTRIES(icParam):
      FIND ArtBas WHERE ROWID(ArtBas) = TO-ROWID(ENTRY(ix,icParam)) NO-LOCK NO-ERROR.
      iAnt = iAnt + 1.

      IF AVAIL ArtBas THEN DO:
          FIND FIRST ArtPris OF ArtBas NO-LOCK NO-ERROR.
          IF AVAIL ArtPris THEN DO:
              IF (ArtPris.ValPris[1]  = ? OR
                  ArtPris.ValPris[1]  = ? OR
                  ArtPris.VareKost[1] = 0 OR
                  ArtPris.VareKost[1] = ? OR
                  ArtPris.DB%[1]      = ? OR
                  ArtPris.DBKr[1]     = ? OR
                  ArtPris.DivKost%[1] = ? OR
                  ArtPris.DivKostKr[1] = ? OR
                  /* ArtPris.EuroPris[1]  = ? OR */
                  ArtPris.Frakt%[1] = ? OR
                  ArtPris.Frakt[1]  = ? OR
                  ArtPris.InnkjopsPris[1] = 0 OR 
                  ArtPris.InnkjopsPris[1] = ? OR 
                  ArtPris.MomsKod[1] = ? OR
                  ArtPris.Mva%[1] = ? OR
                  ArtPris.MvaKr[1] = ? OR
                  ArtPris.Pris[1]  = 0 OR
                  ArtPris.Pris[1]  = ? OR
                  ArtPris.Rab1%[1] = ? OR
                  ArtPris.Rab1Kr[1] = ? OR
                  ArtPris.Rab2%[1] = ? OR
                  ArtPris.Rab2Kr[1] = ? OR
                  ArtPris.Rab3%[1] = ? OR
                  ArtPris.Rab3Kr[1] = ?) THEN
                  ASSIGN cFeilArtPris = cFeilArtPris + (IF cFeilArtPris <> "" THEN "," ELSE "") + STRING(ArtBas.Artikkelnr).
              ELSE DO:
                  ihBuffer:BUFFER-CREATE().
                  ihBuffer:BUFFER-COPY(BUFFER ArtBas:HANDLE).
              END.
          END.
          ELSE
              ASSIGN cFeilArtPris = cFeilArtPris + (IF cFeilArtPris <> "" THEN "," ELSE "") + STRING(ArtBas.Artikkelnr).
      END.
    END.
  ELSE
    DO ix = 1 TO NUM-ENTRIES(cArtNrLst):
      iAnt = iAnt + 1.
      FIND ArtBas WHERE ArtBas.ArtikkelNr = DEC(ENTRY(ix,cArtNrLst)) NO-LOCK NO-ERROR.
      IF AVAIL ArtBas THEN DO:
          FIND FIRST ArtPris OF ArtBas NO-LOCK NO-ERROR.
          IF AVAIL ArtPris THEN DO:
              IF (ArtPris.ValPris[1]  = ? OR
                  ArtPris.ValPris[1]  = ? OR
                  ArtPris.VareKost[1] = 0 OR
                  ArtPris.VareKost[1] = ? OR
                  ArtPris.DB%[1]      = ? OR
                  ArtPris.DBKr[1]     = ? OR
                  ArtPris.DivKost%[1] = ? OR
                  ArtPris.DivKostKr[1] = ? OR
                  /* ArtPris.EuroPris[1]  = ? OR */
                  ArtPris.Frakt%[1] = ? OR
                  ArtPris.Frakt[1]  = ? OR
                  ArtPris.InnkjopsPris[1] = 0 OR 
                  ArtPris.InnkjopsPris[1] = ? OR 
                  ArtPris.MomsKod[1] = ? OR
                  ArtPris.Mva%[1] = ? OR
                  ArtPris.MvaKr[1] = ? OR
                  ArtPris.Pris[1]  = 0 OR
                  ArtPris.Pris[1]  = ? OR
                  ArtPris.Rab1%[1] = ? OR
                  ArtPris.Rab1Kr[1] = ? OR
                  ArtPris.Rab2%[1] = ? OR
                  ArtPris.Rab2Kr[1] = ? OR
                  ArtPris.Rab3%[1] = ? OR
                  ArtPris.Rab3Kr[1] = ?) THEN
                  ASSIGN cFeilArtPris = cFeilArtPris + (IF cFeilArtPris <> "" THEN "," ELSE "") + STRING(ArtBas.Artikkelnr).
              ELSE DO:
                  ihBuffer:BUFFER-CREATE().
                  ihBuffer:BUFFER-COPY(BUFFER ArtBas:HANDLE).
              END.
          END.
          ELSE
              ASSIGN cFeilArtPris = cFeilArtPris + (IF cFeilArtPris <> "" THEN "," ELSE "") + STRING(ArtBas.Artikkelnr).
      END.
    END.
END.

CREATE QUERY hQuery.
hQuery:SET-BUFFERS(ihBuffer).
hQuery:QUERY-PREPARE("FOR EACH " + ihBuffer:NAME + " NO-LOCK").
hQuery:QUERY-OPEN().

hQuery:GET-FIRST().
REPEAT WHILE NOT hQuery:QUERY-OFF-END:
  IF DEC(STRING(ihBuffer:BUFFER-FIELD("Artikkelnr"):BUFFER-VALUE)) GE lArtikkelNr THEN 
  ARTBLOKK:
  DO:
    BUTIKKLOOP:
    FOR EACH Butiker NO-LOCK WHERE 
      CAN-DO(cButLSt,STRING(Butiker.Butik)):
       
      FIND ArtPris WHERE 
        ArtPris.ArtikkelNr = DEC(STRING(ihBuffer:BUFFER-FIELD("Artikkelnr"):BUFFER-VALUE)) AND 
        ArtPris.ProfilNr   = Butiker.ProfilNr NO-LOCK NO-ERROR.
      IF AVAIL ArtPris THEN DO:
          IF (ArtPris.ValPris[1]  = ? OR
              ArtPris.ValPris[1]  = ? OR
              ArtPris.VareKost[1] = 0 OR
              ArtPris.VareKost[1] = ? OR
              ArtPris.DB%[1]      = ? OR
              ArtPris.DBKr[1]     = ? OR
              ArtPris.DivKost%[1] = ? OR
              ArtPris.DivKostKr[1] = ? OR
              /* ArtPris.EuroPris[1]  = ? OR */
              ArtPris.Frakt%[1] = ? OR
              ArtPris.Frakt[1]  = ? OR
              ArtPris.InnkjopsPris[1] = 0 OR 
              ArtPris.InnkjopsPris[1] = ? OR 
              ArtPris.MomsKod[1] = ? OR
              ArtPris.Mva%[1] = ? OR
              ArtPris.MvaKr[1] = ? OR
              ArtPris.Pris[1]  = 0 OR
              ArtPris.Pris[1]  = ? OR
              ArtPris.Rab1%[1] = ? OR
              ArtPris.Rab1Kr[1] = ? OR
              ArtPris.Rab2%[1] = ? OR
              ArtPris.Rab2Kr[1] = ? OR
              ArtPris.Rab3%[1] = ? OR
              ArtPris.Rab3Kr[1] = ?) THEN DO:
              ASSIGN cFeilArtPris = cFeilArtPris + (IF cFeilArtPris <> "" THEN "," ELSE "") + STRING(ihBuffer:BUFFER-FIELD("Artikkelnr"):BUFFER-VALUE).
          END.
          ELSE DO:
              IF cEDBSystem <> "" THEN
                  RUN create_elogg.p ('ArtPris',
                                      cEDBSystem,
                                      STRING(ihBuffer:BUFFER-FIELD("Artikkelnr"):BUFFER-VALUE) + CHR(1) + STRING(ArtPris.ProfilNr)).

              IF RETURN-VALUE NE "" THEN DO:
                  ocReturn = RETURN-VALUE.
                  LEAVE.
              END.
          END.
      END.
      ELSE DO:
              IF cEDBSystem <> "" THEN
                  RUN create_elogg.p ('ArtPris',
                                      cEDBSystem,
                                      STRING(ihBuffer:BUFFER-FIELD("Artikkelnr"):BUFFER-VALUE) + CHR(1) + STRING(Butiker.ProfilNr)).

              IF RETURN-VALUE NE "" THEN DO:
                  ocReturn = RETURN-VALUE.
                  LEAVE.
              END.
      END.
    END. /* BUTIKKLOOP */
  END. /* ARTBLOKK */
  hQuery:GET-NEXT().
END.
DELETE OBJECT hQuery.

IF ocReturn = "" THEN
DO: 
  ocReturn = "Det er overført "  + STRING(iAnt) + " artikler til " + STRING(NUM-ENTRIES(cButLst)) + " butikker." .
  obOk = TRUE.
END.

