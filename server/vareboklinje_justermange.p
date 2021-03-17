/* Sett verdi for mange forekomster i vareboklinje 
   Parametere:  <Feltnavn>,<Verdi> i parameterstreng
           
                temp-tabell med feltene Vareboknr og Artikkelnr 
                
              eller
                 Tilleggsliste over rowid's med vareboklinjer i parameterstreng:
                   Fom entry(3): <"ROWID">,<Rowid1,Rowid2..>
              eller
                   Tilleggsiste over artikkelnr i parameterstreng:
                   Fom entry(3): <"ARTNR">,<Vareboknr>,<Artnr1,Artnr2..>
   Kommentar: Benytter eksisterende rutiner, varetilvpielogg.p og (evt) eloggtilvpivare.p
   
   Opprettet: 02.09.04 av BHa                  
   Endret:    08.08.06 av BHa: Tillegg for vedlikehold av Kjedevare, ArtBas
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
DEF VAR fVarebokNr  AS DEC    NO-UNDO.
DEF VAR cNyMerknad  AS CHAR   NO-UNDO.

hVBLbuffer = BUFFER VarebokLinje:HANDLE.

IF NOT VALID-HANDLE(ihBuffer) AND NUM-ENTRIES(icParam,"|") > 2 THEN DO:
  CREATE TEMP-TABLE httTable.
  httTable:ADD-LIKE-FIELD("VarebokNr","VarebokLinje.VarebokNr").
  httTable:ADD-LIKE-FIELD("ArtikkelNr","VarebokLinje.ArtikkelNr").
  httTable:TEMP-TABLE-PREPARE("ttVarebokLinje").
  ihBuffer = httTable:DEFAULT-BUFFER-HANDLE.
  IF ENTRY(3,icParam,"|") = "ROWID" THEN
    DO ix = 4 TO NUM-ENTRIES(icParam,"|"):
      FIND VarebokLinje WHERE ROWID(VarebokLinje) = TO-ROWID(ENTRY(ix,icParam,"|")) NO-LOCK NO-ERROR.
      IF AVAIL VarebokLinje THEN DO:
        ihBuffer:BUFFER-CREATE().
        ihBuffer:BUFFER-COPY(BUFFER VarebokLinje:HANDLE).
      END.
    END.
  ELSE DO:
    fVarebokNr = DEC(ENTRY(4,icParam,"|")).
    DO ix = 5 TO NUM-ENTRIES(icParam,"|"):
      FIND VarebokLinje 
           WHERE VarebokLinje.VarebokNr = fVareboknr
             AND VarebokLinje.ArtikkelNr = DEC(ENTRY(ix,icParam,"|")) NO-LOCK NO-ERROR.
      IF AVAIL VarebokLinje THEN DO:
        ihBuffer:BUFFER-CREATE().
        ihBuffer:BUFFER-COPY(BUFFER VarebokLinje:HANDLE).
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

    IF ENTRY(1,icParam,"|") = "Kjedevare" THEN DO:
      FIND ArtBas EXCLUSIVE-LOCK
           WHERE ArtBas.ArtikkelNr = DEC(ihBuffer:BUFFER-FIELD("Artikkelnr"):BUFFER-VALUE)
           NO-ERROR.
      IF AVAIL ArtBas THEN
        ArtBas.Kjedevare = LOGICAL(ENTRY(2,icParam,"|")).
    END.
    ELSE IF ENTRY(1,icParam,"|") = "Gjennomfaktureres" THEN DO:
      FIND ArtBas EXCLUSIVE-LOCK
           WHERE ArtBas.ArtikkelNr = DEC(ihBuffer:BUFFER-FIELD("Artikkelnr"):BUFFER-VALUE)
           NO-ERROR.
      IF AVAIL ArtBas THEN
        ArtBas.Gjennomfaktureres = LOGICAL(ENTRY(2,icParam,"|")).
    END.
    ELSE IF ENTRY(1,icParam,"|") = "LevDato1" THEN DO:
      FIND ArtBas EXCLUSIVE-LOCK
           WHERE ArtBas.ArtikkelNr = DEC(ihBuffer:BUFFER-FIELD("Artikkelnr"):BUFFER-VALUE)
           NO-ERROR.
      IF AVAIL ArtBas THEN
        ArtBas.LevDato1 = DATE(ENTRY(2,icParam,"|")).
    END.
    ELSE IF ENTRY(1,icParam,"|") = "LevDato2" THEN DO:
      FIND ArtBas EXCLUSIVE-LOCK
           WHERE ArtBas.ArtikkelNr = DEC(ihBuffer:BUFFER-FIELD("Artikkelnr"):BUFFER-VALUE)
           NO-ERROR.
      IF AVAIL ArtBas THEN
        ArtBas.LevDato2 = DATE(ENTRY(2,icParam,"|")).
    END.
    ELSE IF ENTRY(1,icParam,"|") = "LevDato3" THEN DO:
      FIND ArtBas EXCLUSIVE-LOCK
           WHERE ArtBas.ArtikkelNr = DEC(ihBuffer:BUFFER-FIELD("Artikkelnr"):BUFFER-VALUE)
           NO-ERROR.
      IF AVAIL ArtBas THEN
        ArtBas.LevDato3 = DATE(ENTRY(2,icParam,"|")).
    END.
    ELSE IF ENTRY(1,icParam,"|") = "LevDato4" THEN DO:
      FIND ArtBas EXCLUSIVE-LOCK
           WHERE ArtBas.ArtikkelNr = DEC(ihBuffer:BUFFER-FIELD("Artikkelnr"):BUFFER-VALUE)
           NO-ERROR.
      IF AVAIL ArtBas THEN
        ArtBas.LevDato4 = DATE(ENTRY(2,icParam,"|")).
    END.
    ELSE IF ENTRY(1,icParam,"|") = "RAvdNr" THEN DO:
      FIND ArtBas EXCLUSIVE-LOCK
           WHERE ArtBas.ArtikkelNr = DEC(ihBuffer:BUFFER-FIELD("Artikkelnr"):BUFFER-VALUE)
           NO-ERROR.
      IF AVAIL ArtBas THEN
        ArtBas.RAvdNr = INT(ENTRY(2,icParam,"|")).
      hQuery:GET-NEXT().
      NEXT.
    END.

    obOK = hVBLbuffer:FIND-FIRST("WHERE Vareboknr = " + STRING(ihBuffer:BUFFER-FIELD("VarebokNr"):BUFFER-VALUE) 
                                 + " AND Artikkelnr = " + STRING(ihBuffer:BUFFER-FIELD("Artikkelnr"):BUFFER-VALUE),
                                 EXCLUSIVE-LOCK,NO-WAIT) NO-ERROR.
    IF obOk THEN DO:
      IF ENTRY(1,icParam,"|") = "Vg" THEN DO:
        RUN art_sett_varegruppe.p (ENTRY(2,icParam,"|") + ",artnr," + STRING(hVBLbuffer:BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE),
                                   ?,
                                   icSessionId,
                                   OUTPUT ocReturn,
                                   OUTPUT obOk).
        IF obOk THEN DO:
          FIND FIRST VarGr NO-LOCK
               WHERE VarGr.Vg = INT(ENTRY(2,icParam,"|")) 
               NO-ERROR.
          IF AVAIL VarGr THEN DO:
            FIND FIRST HuvGr OF VarGr NO-LOCK NO-ERROR.
             IF NOT AVAIL HuvGr THEN DO:
              ocReturn = "Ugyldig varegruppe (hovedgruppe mangler): " + ENTRY(2,icParam,"|").
              LEAVE.                
            END.  

            IF AVAIL HuvGr THEN DO:
              FIND Avdeling NO-LOCK WHERE
                  Avdeling.AvdelingNr = HuvGr.AvdelingNr NO-ERROR.
              IF NOT AVAIL Avdeling THEN DO:
                 ocReturn = "Ugyldig avdeling (Avdeling mangler): " + ENTRY(2,icParam,"|").
                   LEAVE. 
              END.
              ASSIGN 
                hVBLbuffer:BUFFER-FIELD("Vg"):BUFFER-VALUE           = VarGr.Vg
                hVBLbuffer:BUFFER-FIELD("VgBeskr"):BUFFER-VALUE      = VarGr.VgBeskr
                hVBLbuffer:BUFFER-FIELD("Hg"):BUFFER-VALUE           = VarGr.Hg
                hVBLbuffer:BUFFER-FIELD("HgBeskr"):BUFFER-VALUE      = HuvGr.HgBeskr
                hVBLbuffer:BUFFER-FIELD("AvdelingNr"):BUFFER-VALUE   = Avdeling.AvdelingNr
                hVBLbuffer:BUFFER-FIELD("AvdelingNavn"):BUFFER-VALUE = Avdeling.AvdelingNavn
                .
              FOR EACH VareBehHode NO-LOCK WHERE
                  VareBehHode.Kilde = VareBokLinje.VareBokNr:
                  FOR EACH VareBeHLinje OF VareBehHode WHERE
                      VareBehLinje.ArtikkelNr = dec(hVBLbuffer:BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE):
                      ASSIGN
                        VareBehLinje.Vg           = VarGr.Vg
                        VareBehLinje.VgBeskr      = VarGr.VgBeskr
                        VareBehLinje.Hg           = VarGr.Hg
                        VareBehLinje.HgBeskr      = HuvGr.HgBeskr
                        VareBehLinje.AvdelingNr   = Avdeling.AvdelingNr
                        VareBehLinje.AvdelingNavn = Avdeling.AvdelingNavn
                        .
                  END.
              END.

            END.
            ELSE DO:
                ocReturn = "Ugyldig hovedgruppe: " + ENTRY(2,icParam,"|").
                LEAVE.
            END.
          END.
          ELSE DO:
            ocReturn = "Ugyldig varegruppe: " + ENTRY(2,icParam,"|").
            LEAVE.
          END. 
        END.
      END.
      ELSE DO:          
        IF hVBLbuffer:BUFFER-FIELD(ENTRY(1,icParam,"|")):DATA-TYPE = "DECIMAL" THEN 
          hVBLbuffer:BUFFER-FIELD(ENTRY(1,icParam,"|")):BUFFER-VALUE = DEC(ENTRY(2,icParam,"|")).
        ELSE IF hVBLbuffer:BUFFER-FIELD(ENTRY(1,icParam,"|")):DATA-TYPE = "DATE" THEN
          hVBLbuffer:BUFFER-FIELD(ENTRY(1,icParam,"|")):BUFFER-VALUE = DATE(ENTRY(2,icParam,"|")).
        ELSE IF hVBLbuffer:BUFFER-FIELD(ENTRY(1,icParam,"|")):DATA-TYPE = "INTEGER" THEN
          hVBLbuffer:BUFFER-FIELD(ENTRY(1,icParam,"|")):BUFFER-VALUE = INT(ENTRY(2,icParam,"|")).
        ELSE IF hVBLbuffer:BUFFER-FIELD(ENTRY(1,icParam,"|")):DATA-TYPE = "LOGICAL" THEN
          hVBLbuffer:BUFFER-FIELD(ENTRY(1,icParam,"|")):BUFFER-VALUE = LOGICAL(ENTRY(2,icParam,"|")).
        ELSE 
          hVBLbuffer:BUFFER-FIELD(ENTRY(1,icParam,"|")):BUFFER-VALUE = ENTRY(2,icParam,"|").
    
        IF CAN-DO("Sortimentkoder,Kampanjeuker,Kampanjestotte,Lagerkoder",ENTRY(1,icParam,"|")) THEN DO:
          cNyMerknad = (IF hVBLbuffer:BUFFER-FIELD("Sortimentkoder"):BUFFER-VALUE NE "" THEN 
                          hVBLbuffer:BUFFER-FIELD("Sortimentkoder"):BUFFER-VALUE + ","
                        ELSE "") + 
                       (IF hVBLbuffer:BUFFER-FIELD("Kampanjeuker"):BUFFER-VALUE NE "" THEN 
                          hVBLbuffer:BUFFER-FIELD("Kampanjeuker"):BUFFER-VALUE + ","
                        ELSE "") +
                       (IF hVBLbuffer:BUFFER-FIELD("Kampanjestotte"):BUFFER-VALUE NE "" THEN 
                          hVBLbuffer:BUFFER-FIELD("Kampanjestotte"):BUFFER-VALUE + ","
                        ELSE "") +
                       (IF hVBLbuffer:BUFFER-FIELD("Lagerkoder"):BUFFER-VALUE NE "" THEN 
                          hVBLbuffer:BUFFER-FIELD("Lagerkoder"):BUFFER-VALUE + ","
                        ELSE "").
           IF cNyMerknad NE "" THEN DO:       
             cNyMerknad = SUBSTR(cNyMerknad,1,LENGTH(cNyMerknad) - 1).
             hVBLbuffer:BUFFER-FIELD("LinjeMerknad"):BUFFER-VALUE = cNyMerknad.
             FIND FIRST VarebokHode NO-LOCK 
                  WHERE VarebokHode.VareBokNr = DEC(hVBLbuffer:BUFFER-FIELD("Vareboknr"):BUFFER-VALUE)
                  NO-ERROR.
             IF AVAIL VarebokHode THEN DO: 
               FIND FIRST Messe EXCLUSIVE-LOCK 
                    WHERE Messe.MesseNr = VarebokHode.MesseNr
                    NO-ERROR.
               IF AVAIL Messe AND LOOKUP(cNyMerknad,Messe.Oppmerking,"¤") = 0 THEN
                 Messe.Oppmerking = Messe.Oppmerking + (IF Messe.Oppmerking NE "" THEN "¤" ELSE "") + cNyMerknad.
             END.
           END.
           ELSE hVBLbuffer:BUFFER-FIELD("LinjeMerknad"):BUFFER-VALUE = cNyMerknad.
        END.

        RUN vareboklinje_kalkuler.p (hVBLbuffer,ENTRY(1,icParam,"|")).  
      END.
    END.
    ELSE DO:
      ocReturn = "Varebok / Art "
                 + STRING(ihBuffer:BUFFER-FIELD("VarebokNr"):BUFFER-VALUE)
                 + " / " + STRING(ihBuffer:BUFFER-FIELD("Artikkelnr"):BUFFER-VALUE) +
                 " ikke tilgj. for oppdatering".
      LEAVE.
    END.

    hQuery:GET-NEXT().
  END.
END.

DELETE OBJECT hQuery.

IF ocReturn = "" THEN obOk = TRUE.

