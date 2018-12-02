/* Parameters:  <Value> 
   Created: 21.10.04 by Brynjar Hasle                  
   
   Rabatt type indikerer hva kampartBelop innehar
   rabatttypeid: 2 - gratis, 
                 3 - undertrykk rabatt
                 6 - prosent
                 
                 
-----------------------------------------------------------------------------------*/
DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.

DEF VAR iKampId     AS INT NO-UNDO.
DEF VAR iKampTilbId AS INT NO-UNDO.
DEF VAR fLpris      AS DEC INIT 9999999 NO-UNDO.
DEF VAR fHpris      AS DEC INIT 0       NO-UNDO.
DEF VAR i           AS INT NO-UNDO.

DEF TEMP-TABLE ttPris NO-UNDO
  /*Extent 1: original pris Lav, Extent 2: Original Pris Høy*/
  FIELD seqnr  AS INT
  FIELD Id     AS DEC
  FIELD fOrigPris AS DEC EXTENT 2
  FIELD iOrigAnt  AS INT EXTENT 2
  FIELD fPris     AS DEC EXTENT 2
  FIELD fRabatt   AS DEC EXTENT 2
  FIELD iAntall   AS INT EXTENT 2
  INDEX seqnr IS PRIMARY UNIQUE seqnr
  INDEX Id id
  .

FUNCTION getAntall RETURNS INT () FORWARD.
FUNCTION getPris   RETURNS DEC (INPUT ipfPris AS DEC) FORWARD.

ASSIGN 
  iKampId     = INT(ENTRY(1,icParam,'|'))
  iKampTilbId = INT(ENTRY(2,icParam,'|'))
.

FOR EACH KampanjeTilbArtikkel WHERE KampanjeTilbArtikkel.KampId     = iKampId
                                AND KampanjeTilbArtikkel.KampTilbId = iKampTilbId
                              NO-LOCK:
  IF KampanjeTilbArtikkel.KampTilbArtId GT 0 THEN
  DO: /*Artikkel*/
    FOR FIRST artpris WHERE artpris.ProfilNr = 1
                        AND artpris.artikkelNr = KampanjeTilbArtikkel.KampTilbArtId
                      NO-LOCK: LEAVE. END.
/*     ASSIGN                                                                                       */
/*       fLpris = (IF AVAIL artpris AND artpris.pris[1] LT fLpris THEN artpris.pris[1] ELSE fLpris) */
/*       fHpris = (IF AVAIL artpris AND artpris.pris[1] GT fHpris THEN artpris.pris[1] ELSE fHpris) */
/*     .                                                                                            */
    IF AVAIL artpris THEN
    DO:
      CREATE ttPris.
      ASSIGN 
        i = i + 1
        ttPris.seqnr = i 
      .
      ASSIGN
        ttPris.fOrigPris[1] = artpris.pris[1]
        ttPris.fOrigPris[2] = artpris.pris[1]
        ttPris.iOrigAnt[1]  = KampanjeTilbArtikkel.KampTilbArtMinAntall
        ttPris.iOrigAnt[2]  = KampanjeTilbArtikkel.KampTilbArtMinAntall
        ttPris.iAntall[1]   = getAntall()
        ttPris.iAntall[2]   = ttPris.iAntall[1]
        ttPris.fPris[1]     = getPris(artpris.pris[1])
        ttPris.fPris[2]     = ttPris.fPris[1]   
      NO-ERROR.
      IF ERROR-STATUS:ERROR THEN MESSAGE ERROR-STATUS:GET-MESSAGE(1) VIEW-AS ALERT-BOX.      
    END.
    RELEASE artpris.
  END.
  ELSE
  DO: /*Produkt familie*/
    FOR EACH ProduktFamMedlem WHERE ProduktFamMedlem.ProdFamId = KampanjeTilbArtikkel.ProdFamId NO-LOCK:
      FOR FIRST artpris WHERE artpris.ProfilNr = 1
                          AND artpris.artikkelNr = ProduktFamMedlem.ProdFamArtikkelNr
                        NO-LOCK: LEAVE. END.
      ASSIGN 
        fLpris = (IF AVAIL artpris AND artpris.pris[1] LT fLpris THEN artpris.pris[1] ELSE fLpris)
        fHpris = (IF AVAIL artpris AND artpris.pris[1] GT fHpris THEN artpris.pris[1] ELSE fHpris)
      .
      IF AVAIL artpris THEN
      DO:
        FOR FIRST ttPris WHERE ttPris.Id = KampanjeTilbArtikkel.ProdFamId: LEAVE. END.
        IF NOT AVAIL ttPris THEN
        DO:
          CREATE ttPris.
          ASSIGN 
            i            = i + 1
            ttPris.seqnr = i 
            ttPris.Id    = KampanjeTilbArt.ProdFamId
          .
        END.
        ASSIGN
          ttPris.fOrigPris[1] = fLpris
          ttPris.fOrigPris[2] = fHpris
          ttPris.iOrigAnt[1]  = KampanjeTilbArtikkel.KampTilbArtMinAntall
          ttPris.iOrigAnt[2]  = KampanjeTilbArtikkel.KampTilbArtMinAntall
          ttPris.iAntall[1]   = getAntall()
        .
        ASSIGN
          ttPris.fPris[1]     = getPris(fLpris)
          ttPris.iAntall[2]   = ttPris.iAntall[1]
          ttPris.fPris[2]     = getPris(fHpris)
        .

      END.
      RELEASE artpris.                                      

    END.
  END.
/*         MESSAGE                                                   */
/* 'id ' kampanjeTilbArtikkel.KampTilbArtId SKIP KampTilbArtSeq SKIP */
/* 'origpris 'ttPris.fOrigPris[1]  ' ' ttPris.fOrigPris[2] SKIP      */
/* 'origant ' ttPris.iOrigAnt[1] ' ' ttPris.iOrigAnt[2]  SKIP        */
/* 'pris ' ttPris.fPris[1] ttPris.fPris[2] SKIP                      */
/* 'ant ' ttPris.iAntall[1] ttPris.iAntall[2] SKIP                   */
/*                                                                   */
/* VIEW-AS ALERT-BOX INFO BUTTONS OK.                                */


END.
DEF VAR fOrigPayL AS DEC.
DEF VAR fCustPayL AS DEC.
DEF VAR fCustSaveL AS DEC.
DEF VAR fOrigPayH AS DEC.
DEF VAR fCustPayH AS DEC.
DEF VAR fCustSaveH AS DEC.

FOR EACH ttPris:
  ASSIGN 
    fOrigPayL  = fOrigPayL + (ttPris.fOrigPris[1] * ttPris.iOrigAnt[1])
    fOrigPayH  = fOrigPayH + (ttPris.fOrigPris[2] * ttPris.iOrigAnt[2])
    fCustPayL  = fCustPayL + (ttPris.fPris[1] * ttPris.iAntall[1])
    fCustPayH  = fCustPayH + (ttPris.fPris[2] * ttPris.iAntall[2])
    fCustSaveL = fOrigPayL - fCustPayL
    fCustSaveH = fOrigPayH - fCustPayH
  .
/*   MESSAGE                                                 */
/*     seq                                                   */
/*     fCustSaveL SKIP fCustSaveH                            */
/*     fOrigPayL - fCustPayL SKIP                            */
/*     fOrigPayH - fCustPayH                                 */
/*     SKIP                                                  */
/*      'OrigL: ' ttPris.fOrigPris[1] ' ' ttPris.iOrigAnt[1] */
/*      'OrigH: ' ttPris.fOrigPris[2] ' ' ttPris.iOrigAnt[2] */
/*      'KundprisL: ' ttPris.fPris[1] ' ' ttPris.iAntall[1]  */
/*      'KundprisH: ' ttPris.fPris[2] ' ' ttPris.iAntall[2]  */
/*   VIEW-AS ALERT-BOX.                                      */

END.
/* MESSAGE                              */
/*   fOrigPayL ' ' fOrigPayH SKIP       */
/*   fCustPayL ' ' fCustPayH SKIP       */
/*   fCustSaveL ' ' fCustSaveH          */
/*   VIEW-AS ALERT-BOX INFO BUTTONS OK. */
ASSIGN 
  ocReturn =          STRING(fOrigPayL,'->>>>>>>9.9') 
              + '|' + string(fOrigPayH,'->>>>>>>9.9')
              + '|' + string(fCustPayL,'->>>>>>>9.9')
              + '|' + string(fCustPayH,'->>>>>>>9.9')
              + '|' + string(fCustSaveL,'->>>>>>>9.9')
              + '|' + string(fCustSaveH,'->>>>>>>9.9')
  obOk = TRUE
.

FUNCTION getAntall RETURNS INT ():
  IF AVAIL KampanjeTilbArtikkel THEN
  DO:
    FIND KampanjeTilbud OF KampanjeTilbArtikkel NO-LOCK NO-ERROR.
    IF AVAIL KampanjeTilbud THEN
    DO:
      IF KampanjeTilbud.KampTilbTypeId = 4 THEN /*PayFor*/
        RETURN INT(KampanjeTilbArtikkel.KampTilbArtBelop).
      ELSE
        RETURN INT(KampanjeTilbArtikkel.KampTilbArtMinAntall).
    END.
  END.
END FUNCTION.

FUNCTION getPris RETURNS DEC (INPUT ipfPris AS DEC):
  IF AVAIL KampanjeTilbArtikkel THEN
  DO:
    FIND KampanjeTilbud OF KampanjeTilbArtikkel NO-LOCK NO-ERROR.
    IF AVAIL KampanjeTilbud THEN
    DO:
      IF KampanjeTilbArt.KampRabattTypeId = 3 THEN /*Undertrykk rabatt*/
        RETURN ipfPris.
      IF KampanjeTilbArt.KampRabattTypeId = 2 THEN /*Gratis*/
        RETURN 0.00.
      IF KampanjeTilbArt.KampRabattTypeId = 8 THEN /*Enhetssum å betale*/
        RETURN KampanjeTilbArt.KampTilbArtBelop.
      IF KampanjeTilbArt.KampRabattTypeId = 7 THEN /*Totalsum å betale*/
        RETURN KampanjeTilbArt.KampTilbArtBelop / ttPris.iAntall[2].

      CASE KampanjeTilbud.KampTilbTypeId:
        WHEN 1 THEN RETURN (IF KampanjeTilbArt.KampTilbArtBelop GT 0 THEN KampanjeTilbArt.KampTilbArtBelop / ttPris.iAntall[2] ELSE 0.00).
        WHEN 2 THEN RETURN ((ipfPris * ttPris.iAntall[2] - KampanjeTilbArt.KampTilbArtBelop) / ttPris.iAntall[2]).
        WHEN 3 THEN RETURN ((ipfPris / (1 + (KampanjeTilbArt.KampTilbArtBelop / 100)))).
        WHEN 4 THEN RETURN (ipfPris).
      END CASE.
    END.
  END.
END FUNCTION.
