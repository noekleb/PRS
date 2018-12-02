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
DEF VAR httTable    AS HANDLE NO-UNDO.

DEF VAR iCl         AS INT   NO-UNDO.
DEF VAR wVVAreKost  AS DEC   NO-UNDO.
DEF VAR wTabell     AS CHAR  NO-UNDO INIT "ArtBas".
DEF VAR wTekst      AS CHAR  NO-UNDO.
DEF VAR wEDB-System AS CHAR  NO-UNDO.

{runlib.i}

DEF VAR cTable          AS CHAR   NO-UNDO.
DEF VAR cType           AS CHAR   NO-UNDO.
DEF VAR bhBuffer        AS HANDLE NO-UNDO.
DEF VAR iEkstVPILevnr   AS CHAR   NO-UNDO.
DEF VAR fVareboknr      AS DEC    NO-UNDO.
DEF VAR fArtikkelnr     AS DEC    NO-UNDO.

ASSIGN 
  iEkstVPILevNr = ENTRY(1,icParam)
  fVarebokNr    = DEC(ENTRY(2,icParam))
  cType         = ENTRY(3,icParam) 
  .

DEF BUFFER     clButiker FOR Butiker.
cTable = IF CAN-DO('?,0',STRING(iEkstVPILevNr)) THEN 'ArtBas' ELSE 'VPIArtBas'.

CREATE BUFFER bhBuffer FOR TABLE cTable.

IF /*NOT VALID-HANDLE(ihBuffer) AND */ NUM-ENTRIES(icParam) > 1 THEN 
DO:
  CREATE TEMP-TABLE httTable.
  httTable:ADD-LIKE-FIELD("ArtikkelNr",bhBuffer:NAME + '.' + bhBuffer:BUFFER-FIELD('artikkelnr'):NAME).
  httTable:ADD-LIKE-FIELD("Vg",bhBuffer:NAME + '.' + bhBuffer:BUFFER-FIELD('Vg'):NAME).
  httTable:TEMP-TABLE-PREPARE('tt' + bhBuffer:NAME).
  ihBuffer = httTable:DEFAULT-BUFFER-HANDLE.
  IF cType = "ROWID" THEN
    DO ix = 3 TO NUM-ENTRIES(icParam):
      bhBuffer:FIND-BY-ROWID(TO-ROWID(ENTRY(ix,icParam))).
      IF bhBuffer:AVAIL THEN
      DO:
        ihBuffer:BUFFER-CREATE().
        ihBuffer:BUFFER-COPY(bhBuffer).
      END.
    END.
  ELSE /*ArtNr*/
    DO ix = 4 TO NUM-ENTRIES(icParam):
      fArtikkelnr = DEC(ENTRY(ix,icParam)).
      CASE cTable:
        WHEN 'VPIArtBas' THEN
          bhBuffer:FIND-UNIQUE('WHERE EkstVPILevNr = ' + STRING(iEkstVPILevNr) + ' AND VareNr = ' + QUOTER(fArtikkelNr) ).
        OTHERWISE /*ArtBas*/
          bhBuffer:FIND-UNIQUE('WHERE artikkelnr = DEC(' +  QUOTER(fArtikkelNr) + ')').
      END CASE.
      IF bhBuffer:AVAIL THEN 
      DO:
        ihBuffer:BUFFER-CREATE().
        ihBuffer:BUFFER-COPY(bhBuffer).
      END.
    END.
END.


CREATE QUERY hQuery.
hQuery:SET-BUFFERS(ihBuffer).
hQuery:QUERY-PREPARE("FOR EACH " + ihBuffer:NAME + " NO-LOCK").
hQuery:QUERY-OPEN().

FIND VarebokHode 
     WHERE VarebokHode.VareBokNr = fVarebokNr
     NO-LOCK NO-ERROR.
IF NOT AVAIL VarebokHode THEN DO:
  ocReturn = "Ugyldig VareBokNr: " + STRING(fVarebokNr).
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
  CASE cTable:
    WHEN 'VPIArtBas' THEN
      bhBuffer:FIND-UNIQUE('WHERE EkstVPILevNr = ' + STRING(iEkstVPILevNr) + ' AND Varenr = ' + QUOTER(STRING(ihBuffer:BUFFER-FIELD("Artikkelnr"):BUFFER-VALUE))).
    OTHERWISE /*ArtBas*/
      bhBuffer:FIND-UNIQUE('WHERE artikkelnr = DEC(' +  QUOTER(ihBuffer:BUFFER-FIELD("Artikkelnr"):BUFFER-VALUE) + ')').
  END CASE.
  IF bhBuffer:AVAIL 
    AND bhBuffer:BUFFER-FIELD('LopNr'):BUFFER-VALUE NE ? 
    AND bhBuffer:BUFFER-FIELD('pakke'):BUFFER-VALUE 
    AND bhBuffer:BUFFER-FIELD('OPris'):BUFFER-VALUE
    AND bhBuffer:BUFFER-FIELD('Pant'):BUFFER-VALUE
    THEN
      run vpiartbas_newvareboklinje.p (STRING(iEkstVPILevNr) + ',' + STRING(bhBuffer:BUFFER-FIELD('Artikkelnr'):BUFFER-VALUE) 
                             + ',' + STRING(fVarebokNr) 
                             ,?,'', OUTPUT ocReturn, OUTPUT obOk) NO-ERROR.

  hQuery:GET-NEXT().
  IF iNumErrors > 20 THEN DO:
    ocReturn = "Ingen oppdatering ble utført pga for mange feil (Manglende Hg eller Pris): " + CHR(10) + CHR(10) + ocReturn.
    UNDO, LEAVE.
  END.
END.

IF ocReturn = "" THEN obOk = TRUE.

DELETE OBJECT hQuery.
