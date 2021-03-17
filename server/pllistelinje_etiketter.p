/* Hent merkelapper for pakkseddel
   Parameter:  <PlListeId>
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

/* fPkSdlId   = DEC(ENTRY(1,icParam,";")).                                         */
/*                                                                                 */
/* FIND PkSdlHode NO-LOCK                                                          */
/*      WHERE PkSdlHode.PkSdlId = fPkSdlId                                         */
/*      NO-ERROR.                                                                  */
/* IF NOT AVAIL PkSdlHode THEN DO:                                                 */
/*   ocReturn = "Finner ikke pakklisteid " + STRING(fPkSdlId) + " - programfeil".  */
/*   RETURN.                                                                       */
/* END.                                                                            */

ocReturn = "etikett".

CREATE QUERY hQuery.
hQuery:SET-BUFFERS(ihBuffer).
hQuery:QUERY-PREPARE("FOR EACH " + ihBuffer:NAME + " WHERE AntallPlukket > 0").
hQuery:QUERY-OPEN().
hQuery:GET-FIRST().
BLOKKEN:
REPEAT WHILE NOT hQuery:QUERY-OFF-END:
  ASSIGN
    lArtikkelNr = DECIMAL(ihBuffer:BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE)
    iStrKode    = INTEGER(ihBuffer:BUFFER-FIELD("StrKode"):BUFFER-VALUE)
    .
  FIND PlListeHode NO-LOCK WHERE
    PlListeHode.PlListeId = DECIMAL(ihBuffer:BUFFER-FIELD("PlListeId"):BUFFER-VALUE) NO-ERROR.
  FIND Butiker NO-LOCK WHERE
    Butiker.Butik = INTEGER(ihBuffer:BUFFER-FIELD("FraButikkNr"):BUFFER-VALUE) NO-ERROR.
  /* Henter pris fra pakksedel som skal stå på etiketten. */
  FIND FIRST ArtPris NO-LOCK WHERE
      ArtPris.ArtikkelNr = lArtikkelNr AND 
      ArtPris.ProfilNr   = Butiker.ProfilNr NO-ERROR.
  IF NOT AVAILABLE ArtPris THEN 
    FIND FIRST ArtPris NO-LOCK WHERE
        ArtPris.ArtikkelNr = lArtikkelNr NO-ERROR. 
  FIND LAST StrekKode NO-LOCK
       WHERE StrekKode.ArtikkelNr = lArtikkelNr
         AND StrekKode.StrKode    = iStrKode 
         AND NOT Strekkode.Kode BEGINS '02'
       NO-ERROR.
  IF NOT AVAILABLE Strekkode THEN 
    FIND LAST StrekKode NO-LOCK
      WHERE StrekKode.ArtikkelNr = lArtikkelNr
         AND StrekKode.StrKode    = iStrKode
      NO-ERROR.
      
  IF NOT AVAIL StrekKode THEN DO:
    /*RUN finnStr (ihBuffer:BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE,OUTPUT cStrListe).*/
    FIND StrKonv NO-LOCK WHERE
      StrKonv.StrKode = iStrKode NO-ERROR.
    IF AVAILABLE StrKonv THEN cStrListe = TRIM(StrKonv.Storl).
    ELSE DO:
      ocReturn = "Det er ikke registrert strekkoder for artikkel og ukjent størrelse " + STRING(ihBuffer:BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE).
      LEAVE BLOKKEN.
    END.
    DO:        
      RUN genstrekkode.p (ihBuffer:BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE,1,cStrListe).
      FIND FIRST StrekKode NO-LOCK
           WHERE StrekKode.ArtikkelNr = DECIMAL(ihBuffer:BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE)
             AND StrekKode.StrKode    = INTEGER(ihBuffer:BUFFER-FIELD("StrKode"):BUFFER-VALUE)
           NO-ERROR.
    END.
  END.
  IF AVAIL StrekKode THEN
    ASSIGN cArtNrList   = cArtNrList   + STRING(Strekkode.ArtikkelNr) + CHR(1)
           cEANlist     = cEANlist     + StrekKode.Kode + CHR(1)
           cAntallList  = cAntallList  + STRING(ihBuffer:BUFFER-FIELD("AntallPlukket"):BUFFER-VALUE) + CHR(1)
           cIndividList = cIndividList + "0" + CHR(1) 
           cPrisList    = cPrisList    + STRING(ArtPris.Pris[1]) + CHR(1) 
           .
  ELSE DO:
    ocReturn = "Det er ikke registrert strekkoder for artikkel " + STRING(ihBuffer:BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE).
    LEAVE.
  END.
  hQuery:GET-NEXT().
END. /* BLOKKEN */

DELETE OBJECT hQuery NO-ERROR.

/* FOR EACH PkSdlLinje OF PkSdlHode NO-LOCK                                       */
/*     WHERE PkSdlLinje.AntallPlukket > 0                                             */
/*     ,FIRST StrekKode NO-LOCK                                                   */
/*            WHERE StrekKode.ArtikkelNr = PkSdlLinje.ArtikkelNr                  */
/*              AND StrekKode.StrKode    = PkSdlLinje.StrKode                     */
/*     BY PkSdlLinje.Linjenr                                                      */
/*     :                                                                          */
/*   ASSIGN cArtNrList   = cArtNrList   + STRING(PkSdlLinje.ArtikkelNr) + CHR(1)  */
/*          cEANlist     = cEANlist     + StrekKode.Kode + CHR(1)                 */
/*          cAntallList  = cAntallList  + STRING(PkSdlLinje.AntallPlukket) + CHR(1)   */
/*          cIndividList = cIndividList + "0" + CHR(1)                            */
/*          .                                                                     */
/* END.                                                                           */
  
IF ocReturn BEGINS "etikett" THEN
  ASSIGN ocReturn = ocReturn
                  + TRIM(cArtNrList,CHR(1)) + "|"
                  + TRIM(cEANlist,CHR(1)) + "|"
                  + TRIM(cAntallList,CHR(1)) + "|"
                  + TRIM(cIndividList,CHR(1)) + "|"
                  + TRIM(cPrisList,CHR(1))
         obOk     = YES.
  
PROCEDURE finnStr:
  DEF INPUT  PARAM ifArtikkelNr AS DEC  NO-UNDO.
  DEF OUTPUT PARAM ocStrList    AS CHAR NO-UNDO.

  DEF VAR ix AS INT NO-UNDO.

  DEF BUFFER bArtBas FOR ArtBas.

  FIND FIRST StrType OF bArtBas 
       NO-LOCK NO-ERROR.
  IF NOT AVAIL StrType THEN RETURN ERROR.

  DO ix = 1 TO NUM-ENTRIES(StrType.AlfaFordeling):
    ocStrList = ocStrList + (IF ocStrList NE "" THEN "," ELSE "") + ENTRY(ix,StrType.AlfaFordeling).
  END.

END PROCEDURE.
