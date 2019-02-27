/* Sjekk om det finnes en post i KampanjeTilbArtikkel
   Opprettet: 29.05.2007 Geir Otto Olsen
 -----------------------------------------------------------------------------------*/
  DEF INPUT  PARAM icParam     AS CHAR   NO-UNDO.
  DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
  DEF INPUT  PARAM icSessionId AS CHAR   NO-UNDO.
  DEF OUTPUT PARAM ocReturn    AS CHAR   NO-UNDO.
  DEF OUTPUT PARAM obOK        AS LOG    NO-UNDO.
  
  
  DEF VAR fNextSeq AS DEC NO-UNDO.

  DEF BUFFER bKTA FOR kampanjeTilbArtikkel.
  
  /*
  FOR LAST bKTA WHERE bKTA.KampTilbArtSeq GT 0
/*                   bKTA.kampid         = DEC(ENTRY(1,icParam,';')) AND */
/*                   bKTA.kamptilbid     = INT(ENTRY(2,icParam,';')) */
                 NO-LOCK: LEAVE. END.
  */

  RUN genkamptilbartseq.p (0,"1",OUTPUT fNextSeq).

  /*
  fNextSeq = IF AVAIL bKTA THEN bKTA.KampTilbArtSeq + 1 ELSE 1.
  IF AVAIL bKTA THEN RELEASE bKTA.
  */

  FIND KampanjeTilbud NO-LOCK WHERE
    KampanjeTilbud.KampId     = DEC(ENTRY(1,icParam,';')) AND
    KampanjeTilbud.KamptilbId = INT(ENTRY(2,icParam,';')) NO-ERROR.

  CREATE KampanjeTilbArtikkel.
  
  ASSIGN 
    KampanjeTilbArtikkel.KampId               = DEC(ENTRY(1,icParam,';'))
    KampanjeTilbArtikkel.KampTilbId           = INT(ENTRY(2,icParam,';'))
    KampanjeTilbArtikkel.KampTilbArtSeq       = fNextSeq
    KampanjeTilbArtikkel.KampTilbArtId        = DEC(ENTRY(3,icParam,';'))
    KampanjeTilbArtikkel.ProdFamId            = DEC(ENTRY(4,icParam,';'))
    KampanjeTilbArtikkel.KupongId             = DEC(ENTRY(5,icParam,';'))
    KampanjeTilbArtikkel.KampRabattTypeId     = (IF KampanjeTilbud.KampTilbTypeId = 10 
                                                   THEN 7 
                                                 ELSE IF KampanjeTilbud.KampTilbTypeId = 11
                                                   THEN 7
                                                 ELSE KampanjeTilbArtikkel.KampRabattTypeId)
    KampanjeTilbArtikkel.KampTilbArtMinAntall = (IF KampanjeTilbud.KampTilbTypeId = 10 
                                                   THEN 1
                                                 ELSE IF KampanjeTilbud.KampTilbTypeId = 11 
                                                   THEN 1
                                                 ELSE KampanjeTilbArtikkel.KampTilbArtMinAntall)
  NO-ERROR.
  
  obOk = NOT ERROR-STATUS:ERROR.
  IF NOT obOk THEN ocReturn = ERROR-STATUS:GET-MESSAGE(1).
  
  IF KampanjeTilbArtikkel.KampTilbArtId NE 0 THEN
  DO:
    CREATE ProduktFamilie.
    ASSIGN 
      ProduktFamilie.ProdFamAutoReg = TRUE
      KampanjeTilbArtikkel.ProdFamId = ProduktFamilie.ProdFamId
    NO-ERROR.
    obOk = NOT ERROR-STATUS:ERROR.
    IF NOT obOk THEN ocReturn = ERROR-STATUS:GET-MESSAGE(1).
  END.


  /*Muligens ikke nødvendig*/
/*   /*Legg til pris for artikkel*/                                       */
/*   IF KampanjeTilbArtikkel.KampTilbArtId GT 0 THEN                      */
/*   DO:                                                                  */
/*     FIND FIRST ArtPris                                                 */
/*          WHERE ArtPris.ArtikkelNr = KampanjeTilbArtikkel.KampTilbArtId */
/*            AND ArtPris.ProfilNr  = 1                                   */
/*     NO-LOCK NO-ERROR.                                                  */
/*     IF AVAIL artpris THEN                                              */
/*       ASSIGN                                                           */
/*         KampanjeTilbArtikkel.KampTilbArtBelop = ArtPris.Pris[1]        */
/*       .                                                                */
/*   END.                                                                 */


