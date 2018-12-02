PROCEDURE PromoType:
  DEF INPUT  PARAM irRowid      AS ROWID NO-UNDO.
  DEF INPUT  PARAM icSessionId  AS CHAR  NO-UNDO.
  DEF OUTPUT PARAM ocValue      AS CHAR  NO-UNDO.
    
  FIND KampanjeTilbArtikkel WHERE ROWID(KampanjeTilbArtikkel) = irRowid NO-LOCK NO-ERROR.
  IF AVAIL KampanjeTilbArtikkel THEN 
    ocValue = IF kampanjetilbartikkel.KampTilbArtId GT 0 THEN '' 
              ELSE IF KampanjeTilbArtikkel.ProdFamId GT 0 THEN 'F'
              ELSE 'K'.
END.

PROCEDURE VareTekst:
  DEF INPUT  PARAM irRowid      AS ROWID NO-UNDO.
  DEF INPUT  PARAM icSessionId  AS CHAR  NO-UNDO.
  DEF OUTPUT PARAM ocValue      AS CHAR  NO-UNDO.
  
  FIND KampanjeTilbArtikkel WHERE ROWID(KampanjeTilbArtikkel) = irRowid NO-LOCK NO-ERROR.
  IF AVAIL KampanjeTilbArtikkel THEN 
  DO:
    IF KampanjeTilbArtikkel.KampTilbArtId NE 0 THEN
    DO:
      FIND FIRST artbas WHERE ArtBas.ArtikkelNr = KampanjeTilbArtikkel.KampTilbArtId NO-LOCK NO-ERROR.
      IF AVAIL artbas THEN
        ocValue = artbas.beskr.
    END.
    ELSE IF KampanjeTilbArtikkel.kupongid NE 0 THEN
    DO:
      FIND FIRST Kupong WHERE kupong.kupongid = KampanjeTilbArtikkel.KupongId NO-LOCK NO-ERROR.
      IF AVAIL kupong THEN
        ocValue = kupong.kupBeskrivelse.
    END.
    ELSE IF KampanjeTilbArtikkel.ProdFamId NE 0 THEN 
    DO: /*Familie*/
         /* 17.09.07 GOO - Endret etter ønske fra brukerne, skal vise ProdFamNavn istedet for alle artiklene */
/*       FOR EACH ProduktFamMedlem WHERE ProduktFamMedlem.ProdFamId = KampanjeTilbArtikkel.ProdFamId NO-LOCK: */
/*         FIND FIRST artbas WHERE ArtBas.ArtikkelNr = ProduktFamMedlem.ProdFamArtikkelNr NO-LOCK NO-ERROR.   */
/*         IF AVAIL artbas THEN                                                                               */
/*           ocValue = ocValue + ' / ' + ArtBas.Beskr.                                                        */
/*       END.                                                                                                 */
/*       ocValue = TRIM(ocValue,' / ').                                                                       */
      FIND ProduktFamilie OF KampanjeTilbArtikkel NO-LOCK NO-ERROR.
      ocValue = IF AVAIL ProduktFamilie THEN ProduktFamilie.ProdFamNavn ELSE ''.
    END.
  END.
END.

PROCEDURE VareNr:
  DEF INPUT  PARAM irRowid      AS ROWID NO-UNDO.
  DEF INPUT  PARAM icSessionId  AS CHAR  NO-UNDO.
  DEF OUTPUT PARAM ocValue      AS CHAR  NO-UNDO.
  
  FIND KampanjeTilbArtikkel WHERE ROWID(KampanjeTilbArtikkel) = irRowid NO-LOCK NO-ERROR.
  IF AVAIL KampanjeTilbArtikkel THEN 
  DO:
    IF KampanjeTilbArtikkel.KampTilbArtId GT 0 THEN 
      ocValue = STRING(KampanjeTilbArtikkel.KampTilbArtId).
    ELSE IF KampanjeTilbArtikkel.ProdFamId GT 0 THEN 
      ocValue = STRING(KampanjeTilbArtikkel.ProdFamId).
    ELSE IF KampanjeTilbArtikkel.KupongId GT 0 THEN
      ocValue = STRING(KampanjeTilbArtikkel.KupongId).
    ELSE ocValue = '0.0'.
  END.
  
END PROCEDURE.

PROCEDURE VarePris:
  DEF INPUT  PARAM irRowid      AS ROWID NO-UNDO.
  DEF INPUT  PARAM icSessionId  AS CHAR  NO-UNDO.
  DEF OUTPUT PARAM ocValue      AS CHAR  NO-UNDO.

  DEF VAR fHighPrice AS DEC INIT '-999999999' NO-UNDO.
  DEF VAR fLowPrice  AS DEC INIT  '999999999'  NO-UNDO.
    
  FIND KampanjeTilbArtikkel WHERE ROWID(KampanjeTilbArtikkel) = irRowid NO-LOCK NO-ERROR.
  IF AVAIL KampanjeTilbArtikkel THEN 
  DO:
    IF KampanjeTIlbArtikkel.KampTilbArtId GT 0 THEN
    DO:
      FOR FIRST artpris WHERE ArtPris.ProfilNr  = 1 
                          AND ArtPris.ArtikkelNr = KampanjeTilbArtikkel.KampTilbArtId NO-LOCK: LEAVE. END.
      IF AVAIL artpris THEN
        ocValue = STRING(artpris.pris[1]).
    END.
    ELSE IF KampanjeTilbArtikkel.KupongId GT 0 THEN
    DO:
      FIND FIRST Kupong WHERE kupong.kupongid = KampanjeTilbArtikkel.KupongId NO-LOCK NO-ERROR.
      IF AVAIL kupong THEN
        ocValue = STRING(kupong.belop).
    END.
    ELSE IF KampanjeTilbArtikkel.ProdFamId GT 0 THEN 
    DO: /*Familie*/
      FOR EACH ProduktFamMedlem WHERE ProduktFamMedlem.ProdFamId = KampanjeTilbArtikkel.ProdFamId NO-LOCK:
        FOR FIRST artpris WHERE ArtPris.ProfilNr  = 1 
                            AND ArtPris.ArtikkelNr = ProduktFamMedlem.ProdFamArtikkelNr NO-LOCK: LEAVE. END.
        IF AVAIL artpris THEN
          ASSIGN 
            fHighPrice = IF ArtPris.Pris[1] GT fHighPrice THEN ArtPris.Pris[1] ELSE fHighPrice
            fLowPrice  = IF ArtPris.Pris[1] LT fLowPrice  THEN ArtPris.Pris[1] ELSE fLowPrice
          .
          RELEASE artpris.
      END.
      ocValue = IF fLowPrice = 999999999 THEN '0' ELSE STRING(fLowPrice).
    END.    
  END.
END.

PROCEDURE VarePrisH:
  DEF INPUT  PARAM irRowid      AS ROWID NO-UNDO.
  DEF INPUT  PARAM icSessionId  AS CHAR  NO-UNDO.
  DEF OUTPUT PARAM ocValue      AS CHAR  NO-UNDO.

  DEF VAR fHighPrice AS DEC INIT '-999999999' NO-UNDO.
  DEF VAR fLowPrice  AS DEC INIT  '999999999' NO-UNDO.
    
  FIND KampanjeTilbArtikkel WHERE ROWID(KampanjeTilbArtikkel) = irRowid NO-LOCK NO-ERROR.
  IF AVAIL KampanjeTilbArtikkel THEN 
  DO:
    IF KampanjeTilbArtikkel.KampTilbArtId GT 0 THEN ocValue = ''.
    ELSE IF KampanjeTilbArtikkel.ProdFamId GT 0 THEN 
    DO: /*Familie*/
      FOR EACH ProduktFamMedlem WHERE ProduktFamMedlem.ProdFamId = KampanjeTilbArtikkel.ProdFamId NO-LOCK:
        FIND FIRST artpris 
          WHERE ArtPris.ProfilNr   = 1 
            AND ArtPris.ArtikkelNr = ProduktFamMedlem.ProdFamArtikkelNr 
           
          NO-LOCK NO-ERROR.
        IF AVAIL artpris THEN
          ASSIGN 
            fHighPrice = IF ArtPris.Pris[1] GT fHighPrice THEN ArtPris.Pris[1] ELSE fHighPrice
            fLowPrice  = IF ArtPris.Pris[1] LT fLowPrice  THEN ArtPris.Pris[1] ELSE fLowPrice
          .
      END.
      ocValue = IF fHighPrice = -999999999 THEN '0' ELSE STRING(fHighPrice).
    END.
    ELSE
    DO:
      ocValue = ''.
    END.
  END.
END.

PROCEDURE MaksAntall:
  DEF INPUT  PARAM irRowid      AS ROWID NO-UNDO.
  DEF INPUT  PARAM icSessionId  AS CHAR  NO-UNDO.
  DEF OUTPUT PARAM ocValue      AS CHAR  NO-UNDO.
  
  FIND KampanjeTilbArtikkel WHERE ROWID(KampanjeTilbArtikkel) = irRowid NO-LOCK NO-ERROR.
  IF AVAIL KampanjeTilbArtikkel THEN 
  DO:
    FIND KampanjeTilbud OF KampanjeTilbArtikkel NO-LOCK NO-ERROR.
    IF AVAIL KampanjeTilbud THEN
      ocValue = IF CAN-DO('0,?',STRING(KampanjeTilbud.KampTilbGrenseAntall)) THEN 
                  STRING('')
                ELSE
                  STRING(KampanjeTilbud.KampTilbGrenseAntall * KampanjeTilbArtikkel.KampTilbArtMinAntall).
  END.

END PROCEDURE.

