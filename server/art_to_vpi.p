/* Overføring av artikler til VPI-register 
   Parametere:  temp-tabell med feltet Artikkelnr 
              eller
                 Liste over rowid's med artikler i parameterstreng:
                   <"ROWID">,<Rowid1,Rowid2..>
              eller
                   Liste over artikkelnr i parameterstreng:
                   <"ARTNR">,<Artnr1,Artnr2..>
   Kommentar: Benytter eksisterende rutiner, varetilvpielogg.p og (evt) eloggtilvpivare.p
   
   Variabel cEDBSystem skal bare være satt når det skal logges oversending til butikk (iElogg).
   
   Opprettet: 29.07.04 av BHa                  
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
DEF VAR lEkstVPILevNr AS DEC    NO-UNDO.
DEF VAR lArtikkelNr   AS DEC    NO-UNDO.
DEFINE VARIABLE cArtNrLst AS CHARACTER NO-UNDO.

{windows.i}

ASSIGN 
    cEDBSystem    = IF NUM-ENTRIES(icParam,"|") > 1 THEN 
                      ENTRY(2,icParam,"|") ELSE "" /* Er bare satt når det valgt at det skal sendes til butikk */
    cArtNrLst     = IF NUM-ENTRIES(icParam,"|") > 2 THEN 
                      ENTRY(3,icParam,"|") ELSE "" /* Er bare satt når det sendes med en liste med artikkelnr. */
    lEkstVPILevNr = 1 /* Fra ArtBas, sendes det alltid via HK's VPI register. */
    cTid          = STRING(TODAY) + " " + STRING(TIME,"HH:MM:SS").

IF NOT VALID-HANDLE(ihBuffer) AND NUM-ENTRIES(icParam,'|') > 1 THEN DO:
  CREATE TEMP-TABLE httTable.
  httTable:ADD-LIKE-FIELD("ArtikkelNr","ArtBas.ArtikkelNr").
  httTable:TEMP-TABLE-PREPARE("ttArtBas").
  ihBuffer = httTable:DEFAULT-BUFFER-HANDLE.

  IF ENTRY(1,icParam,"|") = "ROWID" THEN
    DO ix = 2 TO NUM-ENTRIES(icParam):
      FIND ArtBas WHERE ROWID(ArtBas) = TO-ROWID(ENTRY(ix,icParam)) NO-LOCK NO-ERROR.

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
  IF DEC(STRING(ihBuffer:BUFFER-FIELD("Artikkelnr"):BUFFER-VALUE)) GE lArtikkelNr THEN DO:
      FIND FIRST ArtPris WHERE ArtPris.ArtikkelNr = DEC(STRING(ihBuffer:BUFFER-FIELD("Artikkelnr"):BUFFER-VALUE)) NO-LOCK NO-ERROR.
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
              /* Overfører ArtBas til VPI register. */
              RUN artbas_til_vpi.p (1,ihBuffer:BUFFER-FIELD("Artikkelnr"):BUFFER-VALUE).

              /* Logger for overføring til butikk når EDB system er angitt.   */
              /* Og overføring skjer fra artbas alltid via HK's VPI register. */
              IF cEDBSystem <> "" THEN
                  RUN create_elogg.p ('VPIArtBas',cEDBSystem,
                                                  '1' + CHR(1) + 
                                                  string(ihBuffer:BUFFER-FIELD("Artikkelnr"):BUFFER-VALUE))).

              IF RETURN-VALUE NE "" THEN DO:
                  ocReturn = RETURN-VALUE.
                  LEAVE.
              END.
          END.
      END.
      ELSE 
          ASSIGN cFeilArtPris = cFeilArtPris + (IF cFeilArtPris <> "" THEN "," ELSE "") + STRING(ihBuffer:BUFFER-FIELD("Artikkelnr"):BUFFER-VALUE).
    /* Kopierer inn i VPI registeret. */
   /*     RUN eloggtilvpivare.p. */
  END.
  hQuery:GET-NEXT().
END.
DELETE OBJECT hQuery.

IF ocReturn = "" THEN obOk = TRUE.
/* ELSE */

/* IF CAN-FIND(FIRST ELogg WHERE ELogg.TabellNavn = "ArtBas"                                               */
/*                                AND ELogg.EksterntSystem = (IF cEDBSystem <> "" THEN "KORRVPI" ELSE "VPI") */
/*                                AND ELogg.Behandlet = FALSE) THEN                                        */
/*   RUN eloggtilvpivare.p.                                                                                */

IF cFeilArtPris <> "" THEN DO:
    obOK = FALSE.
    ocReturn = "Feil i kalkyle. Errorfil: " + SESSION:TEMP-DIRECTORY + "ERR_Art2Vpi.txt".
    OUTPUT TO VALUE(SESSION:TEMP-DIRECTORY + "ERR_Art2Vpi.txt").
    DO ii = 1 TO NUM-ENTRIES(cFeilArtPris):
        PUT UNFORMATTED cTid + " " + ENTRY(ii,cFeilArtPris) SKIP.
    END.
    OUTPUT CLOSE.
END.


IF SEARCH(SESSION:TEMP-DIRECTORY + "ERR_Art2Vpi.txt") <> ? THEN
DO:
    DEF VAR hInstance AS INT.
    /*
    RUN ShellExecute{&A} IN hpApi(0,
                                  "open",
                                  "notepad.exe",
                                  SEARCH(SESSION:TEMP-DIRECTORY + "ERR_Art2Vpi.txt"),
                                  "",
                                  1,
                                  OUTPUT hInstance).
    */
    OS-DELETE VALUE(SESSION:TEMP-DIRECTORY + "ERR_Art2Vpi.txt").
END.

