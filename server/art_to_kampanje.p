/* Overføring av artikler til Kampanje 
   Parametere:   Kampanjeid i parametersteng og temp-tabell med feltene Artikkelnr og Vg eller 
              eller
                 Liste over rowid's med artikler i parameterstreng:
                   <KampanjeId>,<"ROWID">,<Rowid1,Rowid2..>
              eller
                   Liste over artikkelnr i parameterstreng:
                   <KampanjeId>,<"ARTNR">,<Artnr1,Artnr2..>
-----------------------------------------------------------------------------------*/

DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.

DEF VAR hQuery      AS HANDLE NO-UNDO.
DEF VAR ix          AS INT NO-UNDO.
DEF VAR iNumErrors  AS INT NO-UNDO.
DEF VAR dDeci       AS DEC NO-UNDO.
DEF VAR iKampanjeId AS INT NO-UNDO.
DEF VAR httTable    AS HANDLE NO-UNDO.


IF NOT VALID-HANDLE(ihBuffer) AND NUM-ENTRIES(icParam) > 1 THEN DO:
  CREATE TEMP-TABLE httTable.
  httTable:ADD-LIKE-FIELD("ArtikkelNr","ArtBas.ArtikkelNr").
  httTable:ADD-LIKE-FIELD("Vg","ArtBas.Vg").
  httTable:TEMP-TABLE-PREPARE("ttArtBas").
  ihBuffer = httTable:DEFAULT-BUFFER-HANDLE.
  IF ENTRY(2,icParam) = "ROWID" THEN
    DO ix = 3 TO NUM-ENTRIES(icParam):
      FIND ArtBas WHERE ROWID(ArtBas) = TO-ROWID(ENTRY(ix,icParam)) NO-LOCK NO-ERROR.
      IF AVAIL ArtBas THEN DO:
        ihBuffer:BUFFER-CREATE().
        ihBuffer:BUFFER-COPY(BUFFER ArtBas:HANDLE).
      END.
    END.
  ELSE
    DO ix = 3 TO NUM-ENTRIES(icParam):
      FIND ArtBas WHERE ArtBas.ArtikkelNr = DEC(ENTRY(ix,icParam)) NO-LOCK NO-ERROR.
      IF AVAIL ArtBas THEN DO:
        ihBuffer:BUFFER-CREATE().
        ihBuffer:BUFFER-COPY(BUFFER ArtBas:HANDLE).
      END.
    END.
END.

iKampanjeId = INT(ENTRY(1,icParam)).

CREATE QUERY hQuery.
hQuery:SET-BUFFERS(ihBuffer).
hQuery:QUERY-PREPARE("FOR EACH " + ihBuffer:NAME + " NO-LOCK").
hQuery:QUERY-OPEN().

FIND KampanjeHode 
     WHERE KampanjeHode.KampanjeId = INT(iKampanjeId)
     NO-LOCK NO-ERROR.
IF NOT AVAIL KampanjeHode THEN DO:
  ocReturn = "Ugyldig KampanjeId: " + STRING(iKampanjeId).
  RETURN.
END.
ELSE IF KampanjeHode.Aktivert THEN DO:
  ocReturn = "Kampanje: " + STRING(iKampanjeId) + " er aktivert. Artikler kan ikke legges til".
  RETURN.
END.

DO /*TRANSACTION */:
  hQuery:GET-FIRST().
  REPEAT WHILE NOT hQuery:QUERY-OFF-END:
    FIND FIRST ArtBas
         WHERE ArtBas.ArtikkelNr = DEC(ihBuffer:BUFFER-FIELD("Artikkelnr"):BUFFER-VALUE)
           AND ArtBas.Vg         = INT(ihBuffer:BUFFER-FIELD("Vg"):BUFFER-VALUE)
         NO-LOCK NO-ERROR.
    IF AVAIL ArtBas AND ArtBas.LopNr NE ? THEN DO:
      FIND FIRST ArtPris OF ArtBas 
                 WHERE ArtPris.ProfilNr = KampanjeHode.ProfilNr
           NO-LOCK NO-ERROR.
      IF NOT AVAILABLE ArtPris THEN 
          FIND FIRST ArtPris OF ArtBas NO-LOCK NO-ERROR.
      IF NOT AVAIL ArtPris THEN 
        ASSIGN ocReturn   = ocReturn + "Feil prisprofil: " + STRING(ArtBas.ArtikkelNr) + CHR(10)
               iNumErrors = iNumErrors + 1.
      ELSE DO:
        FIND FIRST KampanjeLinje OF KampanjeHode
             WHERE KampanjeLinje.ArtikkelNr = ArtBas.ArtikkelNr
               AND KampanjeLinje.Vg         = ArtBas.Vg
               AND KampanjeLinje.LopNr      = ArtBas.LopNr
             NO-LOCK NO-ERROR.
        IF NOT AVAIL KampanjeLinje THEN DO:
          IF KampanjeHode.AvslagType = 1 THEN DO: /* Prosent */
            ASSIGN dDeci = ROUND(ArtPris.Pris[1] * (1 - ((KampanjeHode.Kamp% * -1) / 100)),1).
            IF dDeci > 50 THEN
                ASSIGN dDeci = TRUNC(dDeci,0).
            ELSE
               ASSIGN dDeci = IF dDeci - TRUNC(dDeci,0) > 0.5 THEN TRUNC(dDeci,0) + 0.5 ELSE TRUNC(dDeci,0).
          END.
          ELSE dDeci = KampanjeHode.Kampanjepris.

          CREATE KampanjeLinje.
          ASSIGN KampanjeLinje.KampanjeId     = KampanjeHode.KampanjeId
                 KampanjeLinje.ArtikkelNr     = ArtBas.ArtikkelNr
                 KampanjeLinje.Vg             = ArtBas.Vg
                 KampanjeLinje.LopNr          = ArtBas.LopNr
                 KampanjeLinje.ProfilNr       = KampanjeHode.ProfilNr /*ArtPris.ProfilNr*/
                 KampanjeLinje.Pris[2]        = dDeci
                 .
        END.
      END.
    END.
    ELSE IF AVAIL ArtBas AND ArtBas.LopNr = ? THEN
      ASSIGN ocReturn   = ocReturn + "Mangler løpenr: " + STRING(ArtBas.ArtikkelNr) + CHR(10)
             iNumErrors = iNumErrors + 1.
    ELSE IF NOT AVAIL ArtBas AND ArtBas.LopNr = ? THEN
      ASSIGN ocReturn   = ocReturn + "Artikkel mangler: " + STRING(ihBuffer:BUFFER-FIELD("Artikkelnr"):BUFFER-VALUE) + CHR(10)
             iNumErrors = iNumErrors + 1.
  
    hQuery:GET-NEXT().
    IF iNumErrors > 20 THEN DO:
      ocReturn = "Ingen oppdatering ble utført pga for mange feil: " + CHR(10) + CHR(10) + ocReturn.
      UNDO, LEAVE.
    END.
  END.
END.

IF ocReturn = "" THEN obOk = TRUE.

DELETE OBJECT hQuery.
