/* Sett verdi for mange forekomster i Varebehlinje 
   Parametere:  <Feltnavn>,<Verdi> i parameterstreng
           
                temp-tabell med feltene Varebehnr og Artikkelnr 
                
              eller
                 Tilleggsliste over rowid's med Varebehlinjer i parameterstreng:
                   Fom entry(3): <"ROWID">,<Rowid1,Rowid2..>
              eller
                   Tilleggsiste over artikkelnr i parameterstreng:
                   Fom entry(3): <"ARTNR">,<Varebehnr>,<Artnr1,Artnr2..>
   Kommentar: Benytter eksisterende rutiner, varetilvpielogg.p og (evt) eloggtilvpivare.p
   
   Opprettet: 06.09.04 av BHa                  
-----------------------------------------------------------------------------------*/
DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.

DEF VAR hQuery      AS HANDLE NO-UNDO.
DEF VAR ix          AS INT    NO-UNDO.
DEF VAR httTable    AS HANDLE NO-UNDO.
DEF VAR hVBLbuffer  AS HANDLE NO-UNDO.
DEF VAR fVarebehNr  AS DEC    NO-UNDO.

hVBLbuffer = BUFFER VarebehLinje:HANDLE.

IF NOT VALID-HANDLE(ihBuffer) AND NUM-ENTRIES(icParam,"|") > 2 THEN DO:
  CREATE TEMP-TABLE httTable.
  httTable:ADD-LIKE-FIELD("VarebehNr","VarebehLinje.VarebehNr").
  httTable:ADD-LIKE-FIELD("ArtikkelNr","VarebehLinje.ArtikkelNr").
  httTable:TEMP-TABLE-PREPARE("ttVarebehLinje").
  ihBuffer = httTable:DEFAULT-BUFFER-HANDLE.
  IF ENTRY(3,icParam,"|") = "ROWID" THEN
    DO ix = 4 TO NUM-ENTRIES(icParam,"|"):
      FIND VarebehLinje WHERE ROWID(VarebehLinje) = TO-ROWID(ENTRY(ix,icParam,"|")) NO-LOCK NO-ERROR.
      IF AVAIL VarebehLinje THEN DO:
        ihBuffer:BUFFER-CREATE().
        ihBuffer:BUFFER-COPY(BUFFER VarebehLinje:HANDLE).
      END.
    END.
  ELSE DO:
    fVarebehNr = DEC(ENTRY(4,icParam,"|")).
    DO ix = 5 TO NUM-ENTRIES(icParam,"|"):
      FIND VarebehLinje 
           WHERE VarebehLinje.VarebehNr = fVarebehnr
             AND VarebehLinje.ArtikkelNr = DEC(ENTRY(ix,icParam,"|")) NO-LOCK NO-ERROR.
      IF AVAIL VarebehLinje THEN DO:
        ihBuffer:BUFFER-CREATE().
        ihBuffer:BUFFER-COPY(BUFFER VarebehLinje:HANDLE).
      END.
    END.
  END.
END.

CREATE QUERY hQuery.
hQuery:SET-BUFFERS(ihBuffer).
hQuery:QUERY-PREPARE("FOR EACH " + ihBuffer:NAME + " NO-LOCK").
hQuery:QUERY-OPEN().


DO TRANSACTION:
  hQuery:GET-FIRST().
  REPEAT WHILE NOT hQuery:QUERY-OFF-END:

    obOK = hVBLbuffer:FIND-FIRST("WHERE Varebehnr = " + STRING(ihBuffer:BUFFER-FIELD("VarebehNr"):BUFFER-VALUE) 
                                 + " AND Artikkelnr = " + STRING(ihBuffer:BUFFER-FIELD("Artikkelnr"):BUFFER-VALUE),
                                 EXCLUSIVE-LOCK,NO-WAIT) NO-ERROR.
    IF obOk THEN DO:
      IF hVBLbuffer:BUFFER-FIELD(ENTRY(1,icParam,"|")):DATA-TYPE = "DECIMAL" THEN 
        hVBLbuffer:BUFFER-FIELD(ENTRY(1,icParam,"|")):BUFFER-VALUE = DEC(ENTRY(2,icParam,"|")).
      ELSE IF hVBLbuffer:BUFFER-FIELD(ENTRY(1,icParam,"|")):DATA-TYPE = "DATE" THEN
        hVBLbuffer:BUFFER-FIELD(ENTRY(1,icParam,"|")):BUFFER-VALUE = DATE(ENTRY(2,icParam,"|")).
      ELSE IF hVBLbuffer:BUFFER-FIELD(ENTRY(1,icParam,"|")):DATA-TYPE = "INTEGER" THEN
        hVBLbuffer:BUFFER-FIELD(ENTRY(1,icParam,"|")):BUFFER-VALUE = INT(ENTRY(2,icParam,"|")).
      ELSE 
        hVBLbuffer:BUFFER-FIELD(ENTRY(1,icParam,"|")):BUFFER-VALUE = ENTRY(2,icParam,"|").

      RUN Varebehlinje_kalkuler.p (hVBLbuffer,ENTRY(1,icParam,"|")).
    END.
    ELSE DO:
      ocReturn = "Varebeh / Art "
                 + STRING(ihBuffer:BUFFER-FIELD("VarebehNr"):BUFFER-VALUE)
                 + " / " + STRING(ihBuffer:BUFFER-FIELD("Artikkelnr"):BUFFER-VALUE) +
                 " ikke tilgj. for oppdatering".
      LEAVE.
    END.
    hQuery:GET-NEXT().
  END.
END.

DELETE OBJECT hQuery.

IF ocReturn = "" THEN obOk = TRUE.

