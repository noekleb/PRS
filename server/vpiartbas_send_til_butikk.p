/* Registrer innleveranse fra pakkseddel
   Parameter:  Artikkelnr|VarebehNr
   Opprettet: 25.11.2007             
-----------------------------------------------------------------------------------*/
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


DEF VAR iEkstVPILevNr   AS INT    NO-UNDO.
DEF VAR cFieldList      AS CHAR   NO-UNDO.
DEF VAR i               AS INT    NO-UNDO.
DEF VAR iSendtArt       AS INT    NO-UNDO.
DEF VAR iUpdate         AS INT    NO-UNDO.
DEF VAR iNewVL          AS INT    NO-UNDO.
DEF VAR hQuery          AS HANDLE NO-UNDO.
DEF VAR cEDBSystem      AS CHAR   NO-UNDO.
DEF VAR iAntSlett       AS INT    NO-UNDO.
DEF VAR iAntNyEndre     AS INT    NO-UNDO.
DEF VAR cButikkListe        AS CHAR   NO-UNDO.
DEF VAR fAlleButikker       AS LOG    NO-UNDO.
DEF VAR bBilder             AS LOG    NO-UNDO.
DEF VAR bHKVpi              AS LOG    NO-UNDO.

ASSIGN 
  iEkstVPILevNr  = int(ENTRY(1,icParam,';'))
  cButikkListe   = ENTRY(2,icParam,';')
  fAlleButikker  = LOGICAL(ENTRY(2,cButikkListe,'|'))
  bBilder        = LOGICAL(ENTRY(3,cButikkListe,'|'))
  bHKVpi         = LOGICAL(ENTRY(4,cButikkListe,'|'))
  cButikkListe   = ENTRY(1,cButikkListe,'|')
  cEDBSystem     = "KORR" + STRING(TIME)
.

CREATE QUERY hQuery.
hQuery:SET-BUFFERS(ihBuffer).
hQuery:QUERY-PREPARE("FOR EACH " + ihBuffer:NAME + " NO-LOCK").
hQuery:QUERY-OPEN().

hQuery:GET-FIRST().
MAINLOOP:
REPEAT WHILE NOT hQuery:QUERY-OFF-END:

  /*Ved feil, skal alle feil samles, og sendes etter alle er lest. Gi da også stat på 
  hvor mange som ble oppettet i artbas og vareboklinje*/
  FIND VPIArtBas WHERE VPIArtBas.EkstVPILevNr = iEkstVPILevNr
                   AND VPIArtBas.VareNr       = STRING(ihBuffer:BUFFER-FIELD('varenr'):BUFFER-VALUE)
                 NO-LOCK NO-ERROR.
  IF AVAIL VPIArtBas THEN
  DO:
    /* Kopierer til VPI register 1 før det sendes. */
    IF iEkstVPILevNr > 1 AND bHKVpi THEN
        RUN vpiartbas_kopier_vpi_til_vpi.p (iEkstVPILevNr,1,VPIArtBas.ArtikkelNr).

    /* Logger i ELogg for sending. */
    IF bHKVpi THEN
        RUN create_elogg.p ('VPIArtBas',cEDBSystem,'1' + CHR(1) + string(VPIArtBas.ArtikkelNr)).
    ELSE
        RUN create_elogg.p ('VPIArtBas',cEDBSystem,STRING(iEkstVPILevNr) + CHR(1) + string(VPIArtBas.ArtikkelNr)).

    iSendtArt = iSendtArt + 1.
    hQuery:GET-NEXT().
  END.
END. /* MAINLOOP */

/* Så skal den legges ut på fil og sendes til HK */
IF iSendtArt > 0 THEN
    RUN vpieksport.w (cEDBSystem,
                      cButikkListe, /* Ekstent skal være butikk som skal ha filen. */
                      (IF bBilder THEN 1 ELSE 0), /* Bildedata skal sendes med */
                      OUTPUT iAntSlett,
                      OUTPUT iAntNyEndre).
ASSIGN
    ocReturn = IF iSendtArt > 0
                 THEN 'Det ble endret ' + STRING(iSendtArt) + ' poster i artikkelregisteret'
                 ELSE 'Ingen data ble eksportert'
    obOk     = TRUE
    .

