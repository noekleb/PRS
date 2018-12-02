PROCEDURE KatalogPris#1:
  /*KundeRabatt;AnbefaltPris;KatalogPris;suppRab%;forhRab%*/
  DEF INPUT  PARAM irRowid      AS ROWID NO-UNDO.
  DEF INPUT  PARAM icSessionId  AS CHAR  NO-UNDO.
  DEF OUTPUT PARAM ocValue      AS CHAR  NO-UNDO.
  
  FIND VPIArtBas WHERE ROWID(VPIArtBas) = irRowid NO-LOCK NO-ERROR.
  IF AVAIL VPIArtBas THEN 
    ocValue = STRING(VPIArtBas.KatalogPris[1]).

END PROCEDURE.

PROCEDURE suppRab%#1:
  /*KundeRabatt;AnbefaltPris;KatalogPris;suppRab%;forhRab%*/
  DEF INPUT  PARAM irRowid      AS ROWID NO-UNDO.
  DEF INPUT  PARAM icSessionId  AS CHAR  NO-UNDO.
  DEF OUTPUT PARAM ocValue      AS CHAR  NO-UNDO.
  
  FIND VPIArtBas WHERE ROWID(VPIArtBas) = irRowid NO-LOCK NO-ERROR.
  IF AVAIL VPIArtBas THEN 
    ocValue = STRING(VPIArtBas.suppRab%[1]).

END PROCEDURE.

PROCEDURE forhRab%#1:
  /*KundeRabatt;AnbefaltPris;KatalogPris;suppRab%;forhRab%*/
  DEF INPUT  PARAM irRowid      AS ROWID NO-UNDO.
  DEF INPUT  PARAM icSessionId  AS CHAR  NO-UNDO.
  DEF OUTPUT PARAM ocValue      AS CHAR  NO-UNDO.
  
  FIND VPIArtBas WHERE ROWID(VPIArtBas) = irRowid NO-LOCK NO-ERROR.
  IF AVAIL VPIArtBas THEN 
    ocValue = STRING(VPIArtBas.forhRab%[1]).

END PROCEDURE.

PROCEDURE Varekost:
  /*KundeRabatt;AnbefaltPris;KatalogPris;suppRab%;forhRab%*/
  DEF INPUT  PARAM irRowid      AS ROWID NO-UNDO.
  DEF INPUT  PARAM icSessionId  AS CHAR  NO-UNDO.
  DEF OUTPUT PARAM ocValue      AS CHAR  NO-UNDO.
  
  FIND VPIArtBas WHERE ROWID(VPIArtBas) = irRowid NO-LOCK NO-ERROR.
  IF AVAIL VPIArtBas THEN
  DO:
    ocValue = STRING(VPIArtBas.Katalogpris[1] - (VPIArtBas.Katalogpris[1] * (VPIArtBas.ForhRab%[1] / 100))).
  END.
END PROCEDURE.

PROCEDURE Varekost-2:
  /*KundeRabatt;AnbefaltPris;KatalogPris;suppRab%;forhRab%*/
  DEF INPUT  PARAM irRowid      AS ROWID NO-UNDO.
  DEF INPUT  PARAM icSessionId  AS CHAR  NO-UNDO.
  DEF OUTPUT PARAM ocValue      AS CHAR  NO-UNDO.
  
  FIND Vareboklinje WHERE ROWID(Vareboklinje) = irRowid NO-LOCK NO-ERROR.
  IF AVAIL Vareboklinje THEN
  DO:
    ocValue = STRING(Vareboklinje.Innkjopspris - (Vareboklinje.Innkjopspris * (Vareboklinje.ForhRab% / 100))).
  END.
END PROCEDURE.

PROCEDURE SuppVarekost:
  /*KundeRabatt;AnbefaltPris;KatalogPris;suppRab%;forhRab%*/
  DEF INPUT  PARAM irRowid      AS ROWID NO-UNDO.
  DEF INPUT  PARAM icSessionId  AS CHAR  NO-UNDO.
  DEF OUTPUT PARAM ocValue      AS CHAR  NO-UNDO.
  
  FIND VPIArtBas WHERE ROWID(VPIArtBas) = irRowid NO-LOCK NO-ERROR.
  IF AVAIL VPIArtBas THEN
  DO:
    ocValue = STRING(VPIArtBas.Katalogpris[1] - (VPIArtBas.Katalogpris[1] * (VPIArtBas.SuppRab%[1] / 100))).
  END.
END PROCEDURE.

PROCEDURE SuppVarekost-2:
  /*KundeRabatt;AnbefaltPris;KatalogPris;suppRab%;forhRab%*/
  DEF INPUT  PARAM irRowid      AS ROWID NO-UNDO.
  DEF INPUT  PARAM icSessionId  AS CHAR  NO-UNDO.
  DEF OUTPUT PARAM ocValue      AS CHAR  NO-UNDO.
  
  FIND Vareboklinje WHERE ROWID(Vareboklinje) = irRowid NO-LOCK NO-ERROR.
  IF AVAIL Vareboklinje THEN
  DO:
    ocValue = STRING(Vareboklinje.Innkjopspris - (Vareboklinje.Innkjopspris * (Vareboklinje.SupRab% / 100))).
  END.
END PROCEDURE.

PROCEDURE chkStrekNy:
  DEF INPUT  PARAM irRowid      AS ROWID NO-UNDO.
  DEF INPUT  PARAM icParam      AS CHAR  NO-UNDO. 
  DEF INPUT  PARAM icSessionId  AS CHAR  NO-UNDO.
  DEF OUTPUT PARAM ocValue      AS CHAR  NO-UNDO.

  DEF VAR bFound          AS LOG    NO-UNDO.
  DEF VAR bStrek          AS LOG    NO-UNDO.
  
  ASSIGN 
    bFound = FALSE
  .

  FIND VPIStrekkode WHERE ROWID(VPIStrekkode) = irRowid NO-LOCK NO-ERROR.
  IF AVAIL VPIStrekkode THEN 
  DO:
    ASSIGN 
      bFound = CAN-FIND(FIRST Strekkode 
                          WHERE Strekkode.artikkelnr = DEC(VPIStrekkode.varenr) 
                            AND Strekkode.strkode    = VPIStrekkode.strkode
                            AND Strekkode.kode       = VPIStrekkode.kode).
      ocValue = STRING(NOT bFound).
  END.

END PROCEDURE.

PROCEDURE chkStrekFeil:
  DEF INPUT  PARAM irRowid      AS ROWID NO-UNDO.
  DEF INPUT  PARAM icParam      AS CHAR  NO-UNDO. 
  DEF INPUT  PARAM icSessionId  AS CHAR  NO-UNDO.
  DEF OUTPUT PARAM ocValue      AS CHAR  NO-UNDO.

  DEF VAR bFound          AS LOG    NO-UNDO.
  
  ASSIGN 
    bFound = FALSE
  .

  FIND VPIStrekkode WHERE ROWID(VPIStrekkode) = irRowid NO-LOCK NO-ERROR.
  IF AVAIL VPIStrekkode THEN 
  DO:
    ASSIGN 
      bFound = CAN-FIND(FIRST Strekkode 
                          WHERE Strekkode.kode       = VPIStrekkode.kode)
               AND NOT CAN-FIND(FIRST Strekkode 
                          WHERE Strekkode.artikkelnr = DEC(VPIStrekkode.varenr) 
                            AND Strekkode.strkode    = VPIStrekkode.strkode
                            AND Strekkode.kode       = VPIStrekkode.kode)
      ocValue = STRING(bFound).
  END.

END PROCEDURE.

PROCEDURE gmlBestNr:
  DEF INPUT  PARAM irRowid      AS ROWID NO-UNDO.
  DEF INPUT  PARAM icParam      AS CHAR  NO-UNDO. 
  DEF INPUT  PARAM icSessionId  AS CHAR  NO-UNDO.
  DEF OUTPUT PARAM ocValue      AS CHAR  NO-UNDO.

  FIND VPIStrekkode WHERE ROWID(VPIStrekkode) = irRowid NO-LOCK NO-ERROR.
  IF AVAIL VPIStrekkode THEN 
  DO:
      FIND Strekkode NO-LOCK WHERE
          Strekkode.Kode = VPIStrekkode.Kode NO-ERROR.
      IF AVAIL Strekkode THEN
          ocValue = Strekkode.Bestillingsnummer.
  END.

END PROCEDURE.
