/* Program:     reklamasjonslinje_skriv_og_fakturer.p 
   Parametere:  temp-tabell med feltet Artikkelnr 
              eller
                 Liste over rowid's med artikler i parameterstreng:
                   <"ROWID">,<Rowid1,Rowid2..>
              eller
                   Liste over artikkelnr i parameterstreng:
                   <"ARTNR">,<Artnr1,Artnr2..>
   
   
   Opprettet: 16/2-09 TN                  
-----------------------------------------------------------------------------------*/

DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.

DEF VAR iBatchNr    AS INT NO-UNDO.
DEF VAR piTransNr   AS INT NO-UNDO.
DEF VAR hQuery      AS HANDLE NO-UNDO.
DEF VAR ix          AS INT NO-UNDO.
DEF VAR httTable    AS HANDLE NO-UNDO.
DEF VAR cTekst      AS CHAR NO-UNDO.
DEF VAR iCL         AS INT NO-UNDO.
DEF VAR dVVAreKost  AS DEC NO-UNDO.
DEFINE VARIABLE cSprak AS CHARACTER   NO-UNDO.

DEF BUFFER bButiker FOR Butiker.
DEF BUFFER clButiker FOR Butiker.
FIND bruker WHERE Bruker.BrukerID = USERID("skotex") NO-LOCK NO-ERROR.
IF AVAIL bruker THEN
    cSprak = Bruker.Lng.

{syspara.i 5 1 1 iCL INT}
FIND clButiker NO-LOCK WHERE
    clbutiker.Butik = iCL NO-ERROR.

CREATE QUERY hQuery.
hQuery:SET-BUFFERS(ihBuffer).
hQuery:QUERY-PREPARE("FOR EACH " + ihBuffer:NAME + " NO-LOCK").
hQuery:QUERY-OPEN().

DO:
  hQuery:GET-FIRST().
  REPEAT WHILE NOT hQuery:QUERY-OFF-END:

    FIND ReklamasjonsLinje 
         WHERE ReklamasjonsLinje.ReklamasjonsNr = DECI(STRING(ihBuffer:BUFFER-FIELD("ReklamasjonsNr"):BUFFER-VALUE)) AND
               ReklamasjonsLinje.LinjeNr        = INT(STRING(ihBuffer:BUFFER-FIELD("LinjeNr"):BUFFER-VALUE))
         NO-LOCK NO-ERROR.
    IF AVAIL Reklamasjonslinje THEN DO:
      FIND Reklamasjonslogg OF Reklamasjonslinje NO-LOCK NO-ERROR.

      IF AVAILABLE Reklamasjonslogg AND Reklamasjonslogg.Fakturert = FALSE THEN
        DO:
          FIND CURRENT ReklamasjonsLogg EXCLUSIVE-LOCK.
          ASSIGN
              ReklamasjonsLogg.Fakturert     = TRUE
              ReklamasjonsLogg.FakturertDato = TODAY
              Reklamasjonslogg.FakturertAv   = USERID('SkoTex')
              .
          FIND CURRENT ReklamasjonsLogg NO-LOCK.
        END. /* OPPRETT_TRANS */
    END.
    ELSE DO:
        IF cSprak = "SE" THEN
            ocReturn = ocReturn + "Reklamationslinje " + STRING(ihBuffer:BUFFER-FIELD("ReklamasjonsNr"):BUFFER-VALUE) + "/" 
                                 + STRING(ihBuffer:BUFFER-FIELD("LinjeNr"):BUFFER-VALUE) +
                                   " inte tillgänglig för utskrift og markering av fakturering" + CHR(10).
        ELSE
            ocReturn = ocReturn + "Reklamasjonslinje " + STRING(ihBuffer:BUFFER-FIELD("ReklamasjonsNr"):BUFFER-VALUE) + "/" 
                                 + STRING(ihBuffer:BUFFER-FIELD("LinjeNr"):BUFFER-VALUE) +
                                   " ikke tilgj. for utskrift og markering av fakturering" + CHR(10).
    END.
    hQuery:GET-NEXT().
  END.
END.

DELETE OBJECT hQuery.

IF ocReturn = "" THEN obOk = TRUE.


