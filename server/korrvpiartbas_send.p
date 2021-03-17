/* Registrer innleveranse fra pakkseddel
   Parameter:  Artikkelnr|VarebehNr
   Opprettet: 25.11.2007             
-----------------------------------------------------------------------------------*/
DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.

DEF VAR iEkstVPILevNr   AS INT    NO-UNDO.
DEF VAR i               AS INT    NO-UNDO.
DEF VAR iNewArt         AS INT    NO-UNDO.
DEF VAR iUpdate         AS INT    NO-UNDO.
DEF VAR iNewVL          AS INT    NO-UNDO.
DEF VAR hQuery          AS HANDLE NO-UNDO.
DEF VAR cSendParam      AS CHAR   NO-UNDO.
DEF VAR cEDBSystem      AS CHAR   NO-UNDO.
DEF VAR iAntSlett   AS INT  NO-UNDO.
DEF VAR iAntNyEndre AS INT  NO-UNDO.
DEF VAR iKorrVPI    AS INT  NO-UNDO.
DEF VAR cButikkListe    AS CHAR NO-UNDO.
DEFINE VARIABLE cArtNrListe AS CHARACTER NO-UNDO.
DEFINE VARIABLE cPrefix     AS CHARACTER NO-UNDO.
DEF VAR httTable    AS HANDLE NO-UNDO.
DEF VAR ix          AS INT NO-UNDO.

{syspara.i 22 10 3 iKorrVPI INT}

DEF BUFFER bufVPIArtBas FOR VPIArtBas.

ASSIGN
  iEkstVPILevNr = INT(ENTRY(1,icParam,'|'))
  cEDBSystem    = "HKKORR" + STRING(TIME)
.
IF NUM-ENTRIES(icParam,'|') > 1 THEN 
  cArtNrListe = ENTRY(2,icParam,'|').
IF NUM-ENTRIES(icParam,'|') > 2 THEN 
  cPrefix = ENTRY(3,icParam,'|').


IF NOT VALID-HANDLE(ihBuffer) AND NUM-ENTRIES(icParam,'|') > 1 THEN DO:
  CREATE TEMP-TABLE httTable.
  httTable:ADD-LIKE-FIELD("VareNr","VPIArtBas.VareNr").
  httTable:TEMP-TABLE-PREPARE("ttVPIArtBas").
  ihBuffer = httTable:DEFAULT-BUFFER-HANDLE.
  IF cPrefix = "ROWID" THEN
    DO ix = 3 TO NUM-ENTRIES(icParam):
      FIND VPIArtBas WHERE ROWID(VPIArtBas) = TO-ROWID(ENTRY(ix,cArtNrListe)) NO-LOCK NO-ERROR.
      IF AVAIL VPIArtBas THEN DO:
        ihBuffer:BUFFER-CREATE().
        ihBuffer:BUFFER-COPY(BUFFER VPIArtBas:HANDLE).
      END.
    END.
  ELSE
    DO ix = 1 TO NUM-ENTRIES(cArtNrListe):
      FIND VPIArtBas WHERE VPIArtBas.EkstVPILevNr = iEkstVPILevNr AND 
        VPIArtBas.ArtikkelNr = DEC(ENTRY(ix,cArtNrListe)) NO-LOCK NO-ERROR.
      IF AVAIL VPIArtBas THEN DO:
        ihBuffer:BUFFER-CREATE().
        ihBuffer:BUFFER-COPY(BUFFER VPIArtBas:HANDLE).
      END.
    END.
END.

CREATE QUERY hQuery.
hQuery:SET-BUFFERS(ihBuffer).
hQuery:QUERY-PREPARE("FOR EACH " + ihBuffer:NAME + " NO-LOCK").
hQuery:QUERY-OPEN().

hQuery:GET-FIRST().
REPEAT WHILE NOT hQuery:QUERY-OFF-END:
  FIND VPIArtBas WHERE VPIArtBas.EkstVPILevNr = iEkstVPILevNr
                   AND VPIArtBas.VareNr       = STRING(ihBuffer:BUFFER-FIELD('varenr'):BUFFER-VALUE)
                 NO-LOCK NO-ERROR.
  IF AVAIL VPIArtBas THEN
  SEND_KORREKSJON:
  DO TRANSACTION:
    FIND CURRENT VPIArtBas EXCLUSIVE-LOCK.
    
    IF AVAILABLE ArtBas THEN 
      RELEASE ArtBas.

    /* Artikkelen som den er koblet til må eksistere. */
    IF VPIArtBas.KorrArtikkelNr > 0 THEN
        FIND ArtBas NO-LOCK WHERE
             ArtBas.ArtikkelNr = VPIArtBas.KorrArtikkelNr NO-ERROR.
    IF NOT AVAILABLE ArtBas THEN
        hQuery:GET-NEXT().

    /* Hopper over poster som ikke er koblet og varegruppe/plu artikler. */
    IF VPIArtBas.KorrArtikkelNr < 1000000 OR
        VPIArtBas.KorrArtikkelNr = ? THEN
        hQuery:GET-NEXT().
    ELSE DO:
        /* Følgende skal gjøres for å sende korreksjonsposten til butikken.                                                    */
        /* 1 - Artbas.artikkel skal inn i VPI ArtBas på EkstVPILevnr = 1. (Artikkelnummer hentes fra VPIArtBas.KorrArtikkelNr) */         
        RUN artbas_til_vpi.p (1,ArtBas.Artikkelnr).

        /* VPIArtBas artikkelen skal merkes med riktig KorrArtikkelNr.                 */
        /* Opprinnelig artikkelnummer fra butikk legges nå i VPIArtBas.KorrArtikkelNr. */
        FIND bufVPIArtBas EXCLUSIVE-LOCK WHERE
            bufVPIArtBAs.EkstVPILevNr = 1 AND
            bufVPIArtBas.VareNr       = STRING(ArtBAs.ArtikkelNr).
        IF AVAILABLE bufVPIArtBas THEN
            bufVPIArtBas.KorrArtikkelNr = VPIArtBas.ArtikkelNr.

        /* Logger i ELogg for sending.                                 */
        RUN create_elogg.p ('VPIArtBas',cEDBSystem,'1' + CHR(1) + string(ArtBas.ArtikkelNr)).

        /* Korreksjon fra ukjent vare pakksedelimport på hk. */
        /* Denne skal sendes til ALLE butikker.              */
        IF VPIArtBas.EkstVPILevNr = iKorrVPI THEN
        DO:
            RUN initButikkListe (OUTPUT cButikkListe).
            RUN vpieksport.w (cEDBSystem,
                              cButikkListe, /* Til alle butikker */ 
                              1, /* Bildedata skal sendes med */
                              OUTPUT iAntSlett,
                              OUTPUT iAntNyEndre).
        END.
        /* Korrigert butikkmelding til hk sendes tilbake til butikk. */
        /* Denne skal bare sende til butikken den kom fra.           */
        ELSE DO: 
            RUN vpieksport.w (cEDBSystem,
                              STRING(VPIArtBas.EkstVPILevNr - 1000000), /* Ekstent skal være butikk som skal ha filen. */
                              1, /* Bildedata skal sendes med */
                              OUTPUT iAntSlett,
                              OUTPUT iAntNyEndre).
        END.

        /* Rydder opp. VPIArtBas posten skal bli liggende, men VPIArtBas.KorrArtikkelNr skal nullstilles. */
        FIND bufVPIArtBas EXCLUSIVE-LOCK WHERE
            bufVPIArtBAs.EkstVPILevNr = 1 AND
            bufVPIArtBas.VareNr       = STRING(ArtBAs.ArtikkelNr).
        IF AVAILABLE bufVPIArtBas THEN
            bufVPIArtBas.KorrArtikkelNr = 0.

        obOk = TRUE.
    END.

    IF obOk THEN
    DO:
      iUpdate = iUpdate + 1.
      IF AVAIL vpiartbas THEN
      DO:
        vpiartbas.KorrStatus = 90. /*Sendt*/
        ocReturn = 'Det ble sendt ' + STRING(iUpdate) + ' poster'. 
      END.
    END.
    hQuery:GET-NEXT().
  END. /* SEND_KORREKSJON TRANSACTION */
  ELSE
  DO:
    hQuery:GET-NEXT().
  END.
END.

/* Setter opp en liste over alle aktive butikker. */
PROCEDURE initButikkListe:
    DEF OUTPUT PARAMETER pcButikkListe AS CHAR NO-UNDO.

    FOR EACH Butiker NO-LOCK WHERE
        Butiker.Sentrallager = TRUE AND
        Butiker.ApningsDato <> ? AND
        Butiker.NedlagtDato = ? 
        BY Butiker.butik:
        ASSIGN
            pcButikkListe = pcButikkListe + 
                            (IF pcButikkListe = "" THEN "" ELSE ",") + 
                            STRING(Butiker.Butik).

    END.

END PROCEDURE.


