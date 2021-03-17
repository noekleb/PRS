/* Overføring av artikler til varebok 
   Parametere:   VarebokNr i parametersteng og temp-tabell med feltene Artikkelnr og Vg eller 
              eller
                 Liste over rowid's med artikler i parameterstreng:
                   <VareBokNr>,<"ROWID">,<Rowid1,Rowid2..>
              eller
                   Liste over artikkelnr i parameterstreng:
                   <VareBokNr>,<"ARTNR">,<Artnr1,Artnr2..>
   
   Opprettet: 29.07.04 av BHa. Kode er sakset fra prosedyre ByggUtvalg i d-byggtelleliste.w                
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
DEF VAR iVarebokNr  AS DEC NO-UNDO.
DEF VAR httTable    AS HANDLE NO-UNDO.

DEF VAR iCl         AS INT   NO-UNDO.
DEF VAR wVVAreKost  AS DEC   NO-UNDO.
DEF VAR wTabell     AS CHAR  NO-UNDO INIT "ArtBas".
DEF VAR wTekst      AS CHAR  NO-UNDO.
DEF VAR wEDB-System AS CHAR  NO-UNDO.
DEF VAR rRowIdLinje AS ROWID NO-UNDO.

{runlib.i}


DEF BUFFER     clButiker FOR Butiker.

IF /*NOT VALID-HANDLE(ihBuffer) AND */ NUM-ENTRIES(icParam) > 1 THEN 
DO:
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

iVarebokNr = DEC(ENTRY(1,icParam)).

CREATE QUERY hQuery.
hQuery:SET-BUFFERS(ihBuffer).
hQuery:QUERY-PREPARE("FOR EACH " + ihBuffer:NAME + " NO-LOCK").
hQuery:QUERY-OPEN().

FIND VarebokHode 
     WHERE VarebokHode.VareBokNr = iVarebokNr
     NO-LOCK NO-ERROR.
IF NOT AVAIL VarebokHode THEN DO:
  ocReturn = "Ugyldig VareBokNr: " + STRING(iVarebokNr).
  RETURN.
END.

{syspara.i 5 1 1 iCL INT}
FIND clButiker WHERE clButiker.Butik = iCL NO-LOCK NO-ERROR.
IF NOT AVAIL clButiker THEN DO:
  ocReturn = "Finner ikke sentral-lager: " + STRING(iCL).
  RETURN.
END.
{syspara.i 1 2 4 wEDB-System}

/* Leser artikkellisten som vi fikk inn og henter artikkelinformasjonen. */
hQuery:GET-FIRST().
REPEAT WHILE NOT hQuery:QUERY-OFF-END:
  FIND ArtBas NO-LOCK WHERE 
      ArtBas.ArtikkelNr = DEC(ihBuffer:BUFFER-FIELD("Artikkelnr"):BUFFER-VALUE) NO-ERROR.
  IF AVAIL ArtBas 
     AND ArtBas.LopNr NE ? 
     /*AND NOT ArtBas.pakke */
     AND NOT ArtBas.OPris
     AND NOT ArtBas.Pant
     THEN 
      run newvareboklinje.p (ArtBas.ArtikkelNr,iVarebokNr,OUTPUT rRowIdLinje) NO-ERROR.

  hQuery:GET-NEXT().
  IF iNumErrors > 20 THEN DO:
    ocReturn = "Ingen oppdatering ble utført pga for mange feil (Manglende Hg eller Pris): " + CHR(10) + CHR(10) + ocReturn.
    UNDO, LEAVE.
  END.
END.

IF ocReturn = "" THEN obOk = TRUE.

DELETE OBJECT hQuery.
