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
DEFINE VARIABLE iLinjeNr AS INTEGER NO-UNDO.
DEFINE VARIABLE lIngenavrund AS LOGICAL     NO-UNDO.
DEFINE VARIABLE cTmp AS CHARACTER   NO-UNDO.
DEFINE VARIABLE iAnt AS INTEGER NO-UNDO.
DEFINE VARIABLE bAppend AS LOG NO-UNDO.

{syspara.i 17 1 10 cTmp}

lIngenavrund = cTmp = "1".

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
IF NUM-ENTRIES(icParam) >= 4 THEN 
  IF ENTRY(4,icParam) = 'TRUE' THEN 
    bAppend = TRUE. 

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
ELSE IF KampanjeHode.Aktivert AND bAppend = FALSE THEN DO:
  ocReturn = "Kampanje: " + STRING(iKampanjeId) + " er aktivert. Artikler kan ikke legges til".
  RETURN.
END.

FIND LAST KampanjeLinje OF KampanjeHode USE-INDEX idxKampanjeLinje NO-LOCK NO-ERROR.
IF AVAILABLE KampanjeLinje THEN 
  iLinjeNr = KampanjeLinje.KampanjeLinje + 1.
ELSE 
  iLinjeNr = 1.

iAnt = 0.
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

            IF /* ENTRY(2,dPrisStr,";") = "J" AND */ lIngenavrund THEN DO:
                ASSIGN dDeci = ROUND(ArtPris.Pris[1] * (1 - ((KampanjeHode.Kamp% * -1) / 100)),2).
            END.
            ELSE DO:
                ASSIGN dDeci = ROUND(ArtPris.Pris[1] * (1 - ((KampanjeHode.Kamp% * -1) / 100)),1).
                IF dDeci > 50 THEN
                    ASSIGN dDeci = TRUNC(dDeci,0).
                ELSE
                   ASSIGN dDeci = IF dDeci - TRUNC(dDeci,0) > 0.5 THEN TRUNC(dDeci,0) + 0.5 ELSE TRUNC(dDeci,0).
            END.
          END.
          ELSE dDeci = KampanjeHode.Kampanjepris.

          CREATE KampanjeLinje.
          ASSIGN 
            KampanjeLinje.KampanjeId     = KampanjeHode.KampanjeId
            KampanjeLinje.KampanjeLinje = iLinjeNr
            KampanjeLinje.ArtikkelNr     = ArtBas.ArtikkelNr
            KampanjeLinje.Vg             = ArtBas.Vg
            KampanjeLinje.LopNr          = ArtBas.LopNr
            KampanjeLinje.ProfilNr       = KampanjeHode.ProfilNr /*ArtPris.ProfilNr*/
            KampanjeLinje.EDato      = TODAY 
            KampanjeLinje.ETid       = TIME
            KampanjeLinje.BrukerId   = USERID('skotex')
            KampanjeLinje.RegistrertDato = TODAY 
            KampanjeLinje.RegistrertTid  = TIME
            KampanjeLinje.RegistrertAv   = USERID('skotex')

            KampanjeLinje.Pris[2]        = dDeci
            iLinjeNr = iLinjeNr + 1
            iAnt = iAnt + 1. 
            .
                 
          FIND ArtPris NO-LOCK WHERE
            ArtPris.ArtikkelNr = ArtBas.ArtikkelNr AND
            ArtPris.ProfilNr   = KampanjeHode.ProfilNr NO-ERROR.
          IF NOT AVAILABLE ArtPris THEN 
            FIND FIRST ArtPris NO-LOCK WHERE
              ArtPris.ArtikkelNr = ArtBas.ArtikkelNr NO-ERROR.
          IF AVAILABLE ArtPris THEN
            ASSIGN
              KampanjeLinje.VareKost = ArtPris.VareKost[1]
              KampanjeLinje.Pris[1]  = ArtPris.Pris[1]
              KampanjeLinje.Pris[2]  = ROUND(ArtPris.Pris[1] - ((ArtPris.Pris[1] * (KampanjeHode.Kamp% * -1)) / 100),0)
              KampanjeLinje.VareKost = IF KampanjeLinje.VareKost = ? THEN 0 ELSE KampanjeLinje.VareKost
              KampanjeLinje.Pris[2]  = IF KampanjeLinje.Pris[2] = ? THEN 0 ELSE KampanjeLinje.Pris[2]
              KampanjeLinje.Pris[1]  = IF KampanjeLinje.Pris[1] = ? THEN 0 ELSE KampanjeLinje.Pris[1]
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
IF iAnt > 0 THEN 
  ocReturn = STRING(iAnt).
ELSE 
  ocReturn = 'Ingen artikkler funnet som skal legges til kampanjen.'.
  
DELETE OBJECT hQuery.
