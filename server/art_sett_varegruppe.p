/* Endre varegruppe for artikler 
   Parametere:  temp-tabell med feltet Artikkelnr 
              eller
                 Liste over rowid's med artikler i parameterstreng:
                   <"ROWID">,<Rowid1,Rowid2..>
              eller
                   Liste over artikkelnr i parameterstreng:
                   <"ARTNR">,<Artnr1,Artnr2..>
   Kommentar: Benytter eksisterende rutiner, varetilvpielogg.p og (evt) eloggtilvpivare.p
   
   Opprettet: 29.07.04 av BHa                  
-----------------------------------------------------------------------------------*/

DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.

DEF VAR hQuery      AS HANDLE NO-UNDO.
DEF VAR ix          AS INT NO-UNDO.
DEF VAR httTable    AS HANDLE NO-UNDO.
DEF VAR cGenEan     AS CHAR NO-UNDO.
DEF VAR piOldVgKat  AS INT NO-UNDO.
DEF VAR piOldVg     AS INT NO-UNDO.
DEF VAR iLopNr      AS INT NO-UNDO.

DEF VAR cGenInterleave AS CHAR NO-UNDO.
DEF VAR cStrl AS CHARACTER  NO-UNDO.
DEF VAR cKode AS CHARACTER  NO-UNDO.

DEF BUFFER bArtBas FOR ArtBas.

{syspara.i 2 4 8 cGenEan} 
{syspara.i 2 4 17 cGenInterleave}

/* Stopper ugyldige verdier. */
IF entry(1,icParam) = "?" THEN
    RETURN.

IF NOT VALID-HANDLE(ihBuffer) AND NUM-ENTRIES(icParam) > 1 THEN DO:
  CREATE TEMP-TABLE httTable.
  httTable:ADD-LIKE-FIELD("ArtikkelNr","ArtBas.ArtikkelNr").
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

CREATE QUERY hQuery.
hQuery:SET-BUFFERS(ihBuffer).
hQuery:QUERY-PREPARE("FOR EACH " + ihBuffer:NAME + " NO-LOCK").
hQuery:QUERY-OPEN().


DO:
  hQuery:GET-FIRST().
  REPEAT WHILE NOT hQuery:QUERY-OFF-END TRANSACTION:

    FIND ArtBas 
         WHERE ArtBas.ArtikkelNr = DECI(STRING(ihBuffer:BUFFER-FIELD("Artikkelnr"):BUFFER-VALUE))
         EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
    IF AVAIL ArtBas THEN DO:

        

        ASSIGN
            piOldVg      = ArtBas.Vg
            piOldVgKat   = ArtBas.VgKat
            iLopNr       = ArtBas.LopNr
            .
        IF CAN-FIND(bArtBas WHERE
                    bArtBas.Vg    = int(entry(1,icParam)) AND
                    bArtBas.LopNr = ArtBas.LopNr) THEN
        DO:
            /* Henter nytt løpenummer */
            iLopNr = 0.
            RUN SettLopNr.p (int(entry(1,icParam)),'F',OUTPUT iLopNr).
        END.
        ASSIGN
            ArtBas.Vg    = int(entry(1,icParam))
            ArtBas.LopNr = iLopNr
            .

        FIND VarGr NO-LOCK WHERE
            VarGr.Vg = ArtBas.Vg NO-ERROR.
        IF AVAILABLE VarGr THEN
            ASSIGN
            ArtBas.Hg = VarGr.Hg
            .
        IF cGenInterleave = "1" AND ArtBas.Vg < 1000 AND ArtBas.Lopnr < 10000 THEN DO:
            FOR EACH StrekKode OF ArtBas WHERE Strekkode.kodetype = 1:
                FIND StrKonv OF Strekkode NO-LOCK NO-ERROR.
                IF AVAIL StrKonv THEN DO:
                    ASSIGN cStrl = IF NUM-ENTRIES(StrKonv.Storl,".") = 2 THEN
                        TRIM(REPLACE(StrKonv.Storl,".","")) ELSE TRIM(StrKonv.Storl) + "0"
                           cStrl = FILL("0",4 - LENGTH(cStrl)) + cStrl
                           cKode = STRING(ArtBas.Vg,"999")     +
                                   STRING(ArtBas.LopNr,"9999") +
                                   "0" +
                                   cStrl.
                    ASSIGN Strekkode.BestillingsNummer = cKode.
                END.
                ELSE
                    ASSIGN Strekkode.BestillingsNummer = "".
            END.
        END.
        /* Korreksjon av vgkat fil */
        IF NOT CAN-FIND(VgKat WHERE VgKat.Vg = ArtBas.Vg AND
                        VgKat.VgKat = ArtBas.VgKat) THEN
        DO:
            FIND FIRST VgKat NO-LOCK WHERE
                VgKat.Vg = ArtBas.Vg NO-ERROR.
            IF AVAILABLE VgKat THEN
                ArtBas.VgKat = VgKat.VgKat.
            ELSE 
                ArtBas.VgKat = 0.
        END.

        /* Korreksjon av lagerfil */
        FOR EACH ArtLag WHERE ArtLag.ArtikkelNr = ArtBas.Artikkelnr:
            ASSIGN ArtLag.Vg    = ArtBas.Vg
                   ArtLag.LopNr = ArtBas.Lopnr.
        END.

        /* Korr av translogg poster for artikkelen. */
        FOR EACH TransLogg WHERE
            TransLogg.ArtikkelNr = ArtBas.ArtikkelNr:
            ASSIGN
                TransLogg.Vg    = ArtBas.Vg
                TransLogg.LopNr = ArtBas.LopNr
                .
        END.
    END.
    ELSE ocReturn = ocReturn + "Art " + STRING(ihBuffer:BUFFER-FIELD("Artikkelnr"):BUFFER-VALUE) +
                               " ikke tilgj. for oppdatering" + CHR(10).
    hQuery:GET-NEXT().
  END.
END.

DELETE OBJECT hQuery.

IF ocReturn = "" THEN obOk = TRUE.

