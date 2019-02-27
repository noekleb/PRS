/* Hent merkelapper for telleliste
   Parameter:  <TelleNr>
   Opprettet: 07.09.07 av BHa              
-----------------------------------------------------------------------------------*/
DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.

DEF VAR fPkSdlId        AS DEC  NO-UNDO.
DEF VAR cArtNrList      AS CHAR NO-UNDO.
DEF VAR cEANlist        AS CHAR NO-UNDO.
DEF VAR cAntallList     AS CHAR NO-UNDO.
DEF VAR cIndividList    AS CHAR NO-UNDO.
DEF VAR cPrisList       AS CHAR NO-UNDO.
DEF VAR cStrListe       AS CHAR NO-UNDO.
DEFINE VARIABLE lArtikkelNr AS DECIMAL NO-UNDO.
DEFINE VARIABLE iStrKode    AS INTEGER NO-UNDO.

DEF VAR hQuery          AS HANDLE NO-UNDO.

ocReturn = "etikett".

CREATE QUERY hQuery.
hQuery:SET-BUFFERS(ihBuffer).
hQuery:QUERY-PREPARE("FOR EACH " + ihBuffer:NAME + " WHERE AntallTalt > 0").
hQuery:QUERY-OPEN().
hQuery:GET-FIRST().
BLOKKEN:
REPEAT WHILE NOT hQuery:QUERY-OFF-END:
  ASSIGN
    lArtikkelNr = DECIMAL(ihBuffer:BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE)
    /*iStrKode    = INTEGER(ihBuffer:BUFFER-FIELD("StrKode"):BUFFER-VALUE)*/
    .
  FIND ArtBas NO-LOCK WHERE ArtBas.ArtikkelNr = lArtikkelNr NO-ERROR.
  IF NOT AVAILABLE ArtBas THEN 
    NEXT. 
  FIND Butiker NO-LOCK WHERE Butiker.Butik = INT(ihBuffer:BUFFER-FIELD("Butik"):BUFFER-VALUE) NO-ERROR.
  IF NOT AVAILABLE Butiker THEN 
    NEXT.
  FIND ArtPris NO-LOCK WHERE 
    ArtPris.ArtikkelNr = ArtBas.ArtikkelNr AND
    ArtPris.ProfilNr = Butiker.ProfilNr NO-ERROR.
  IF NOT AVAILABLE ArtPris THEN 
    FIND FIRST ArtPris WHERE 
      ArtPris.ArtikkelNr = lArtikkelNr NO-LOCK NO-ERROR.
  IF NOT AVAILABLE ArtPris THEN 
    NEXT.
  FIND StrKonv NO-LOCK WHERE 
    StrKonv.Storl = STRING(ihBuffer:BUFFER-FIELD("Storl"):BUFFER-VALUE) NO-ERROR.
  IF NOT AVAILABLE StrKonv THEN 
    NEXT.
        
  FIND LAST Strekkode NO-LOCK WHERE
       Strekkode.ArtikkelNr = lArtikkelNr AND 
       Strekkode.StrKode    = StrKonv.StrKode AND 
       LENGTH(Strekkode.Kode) > 7 NO-ERROR.
  IF NOT AVAIL StrekKode THEN DO:
      RUN genstrekkode.p (ihBuffer:BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE,1,StrKonv.Storl).
      FIND FIRST StrekKode NO-LOCK
           WHERE StrekKode.ArtikkelNr = lArtikkelNr 
             AND StrekKode.StrKode    = StrKonv.StrKode
           NO-ERROR.
  END.
  IF AVAIL StrekKode THEN
    ASSIGN cArtNrList   = cArtNrList   + STRING(Strekkode.ArtikkelNr) + CHR(1)
           cEANlist     = cEANlist     + Strekkode.Kode + CHR(1)
           cAntallList  = cAntallList  + STRING(ihBuffer:BUFFER-FIELD("AntallTalt"):BUFFER-VALUE) + CHR(1)
           cIndividList = cIndividList + "0" + CHR(1) 
           cPrisList    = cPrisList    + STRING(ArtPris.Pris[1]) + CHR(1) 
           .
  hQuery:GET-NEXT().
END. /* BLOKKEN */

DELETE OBJECT hQuery NO-ERROR.

IF ocReturn BEGINS "etikett" THEN
  ASSIGN ocReturn = ocReturn
                  + TRIM(cArtNrList,CHR(1)) + "|"
                  + TRIM(cEANlist,CHR(1)) + "|"
                  + TRIM(cAntallList,CHR(1)) + "|"
                  + TRIM(cIndividList,CHR(1)) + "|"
                  + TRIM(cPrisList,CHR(1))
         obOk     = YES.
