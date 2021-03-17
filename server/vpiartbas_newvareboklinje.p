DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.

DEF VAR iEkstVPILevNr   AS INT    NO-UNDO.
DEF VAR fArtikkelnr     AS DEC    NO-UNDO.
DEF VAR fVarebokNr      AS DEC    NO-UNDO.

DEF VAR iCl               AS INT NO-UNDO.
DEF VAR hBuffVarebokLinje AS HANDLE NO-UNDO.
DEF VAR bhBuffer          AS HANDLE NO-UNDO.
DEF VAR bhArtPris         AS HANDLE NO-UNDO.
DEF VAR bhVareboklinje    AS HANDLE NO-UNDO.

DEF BUFFER clButiker FOR Butiker.

  ASSIGN
    iEkstVPILevNr  = INT(ENTRY(1,icParam))
    fArtikkelNr    = DEC(ENTRY(2,icParam))
    fVareboknr     = DEC(ENTRY(3,icParam))
  .
  RUN nyLinje.

PROCEDURE nyLinje:
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
  /* Sjekker om artikkelen ligger på vareboken fra før. */
  IF CAN-FIND(VareBokLinje WHERE
              VareBokLinje.VareBokNr = fVareBokNr AND
              VareBokLinje.ArtikkelNr = fArtikkelNr) THEN
  DO:
      ASSIGN 
/*       rRowIdLinje = ?. */
        obOk        = FALSE
        ocReturn    = "Artikkel er allerede lagt inn i vareboken."
      .
  END.
  IF CAN-DO('?,0',STRING(iEkstVPILevNr)) THEN
  DO:
    CREATE BUFFER bhBuffer  FOR TABLE 'ArtBas'.
    CREATE BUFFER bhArtPris FOR TABLE 'ArtPris'.
  END.
  ELSE
  DO:
    CREATE BUFFER bhBuffer  FOR TABLE 'VPIArtBas'.
    CREATE BUFFER bhArtPris FOR TABLE 'VPIArtPris'.
  END.
  
  CASE bhBuffer:NAME:
    WHEN 'VPIArtBas' THEN
      bhBuffer:FIND-UNIQUE('WHERE EkstVPILevNr = ' + STRING(iEkstVPILevNr) + ' AND Varenr = ' + QUOTER(fArtikkelNr),NO-LOCK).
    OTHERWISE /*ArtBas*/
      bhBuffer:FIND-UNIQUE('WHERE artikkelnr = DEC(' +  QUOTER(STRING(fArtikkelNr)) + ')',NO-LOCK).
  END CASE.

  IF NOT bhBuffer:AVAIL THEN
  DO:
/*       rRowIdLinje = ?. */
      ASSIGN 
        obOk = FALSE
        ocReturn = "Ukjent artikkel."
      .
      RETURN.
  END.
  
  IF bhBuffer:NAME = 'ArtBas' THEN
  DO:
    IF bhBuffer:BUFFER-FIELD('Lopnr'):BUFFER-VALUE   = ? 
      /*OR bhBuffer:BUFFER-FIELD('pakke'):BUFFER-VALUE = TRUE*/
      OR bhBuffer:BUFFER-FIELD('OPris'):BUFFER-VALUE = TRUE
      OR bhBuffer:BUFFER-FIELD('Pant'):BUFFER-VALUE  = TRUE THEN
    DO:
      
  /*       rRowIdLinje = ?. */
      ASSIGN 
        obOk     = FALSE
        ocReturn = "PLU, Pant og artikler uten løpenummer, kan ikke legges inn i varebok."
      .
      RETURN.
    END.
  END.
  ELSE /*VPIArtBas*/
  DO:
    IF bhBuffer:BUFFER-FIELD('OPris'):BUFFER-VALUE = TRUE
      OR bhBuffer:BUFFER-FIELD('Pant'):BUFFER-VALUE  = TRUE THEN
    DO:
      
  /*       rRowIdLinje = ?. */
      ASSIGN 
        obOk     = FALSE
        ocReturn = "PLU, Pant og artikler uten løpenummer, kan ikke legges inn i varebok."
      .
      RETURN.
    END.
  END.
    
  FIND VareBokHode NO-LOCK WHERE
      VareBokHode.VareBokNr = fVareBokNr NO-ERROR.

  /* Henter pris. ALLTID normalpris. */
  IF bhBuffer:NAME = 'VPIArtBas' THEN
    bhArtPris:FIND-FIRST('WHERE EkstVPILevNr = ' + STRING(iEkstVPILevnr) 
                        + ' AND Artikkelnr = DEC(' + QUOTER(STRING(bhBuffer:BUFFER-FIELD('artikkelnr'):BUFFER-VALUE)) + ')'
                        + ' AND ProfileNr  = ' + STRING(varebokhode.profilnr)
                         ,NO-LOCK
                         ) NO-ERROR.
  ELSE
    bhArtPris:FIND-FIRST('WHERE Artikkelnr = DEC(' + QUOTER(STRING(bhBuffer:BUFFER-FIELD('artikkelnr'):BUFFER-VALUE)) + ')'
                        + ' AND ProfileNr  = ' + STRING(varebokhode.profilnr)
                         ,NO-LOCK
                         ) NO-ERROR.

  /* Henter pris fra sentrallageret hvis den ikke finnes for butikken. */
  IF NOT bhArtPris:AVAIL THEN
  DO:
    IF bhBuffer:NAME = 'VPIArtBas' THEN
      bhArtPris:FIND-FIRST('WHERE EkstVPILevNr = ' + STRING(iEkstVPILevnr) 
                          + ' AND Artikkelnr = DEC(' + QUOTER(STRING(bhBuffer:BUFFER-FIELD('artikkelnr'):BUFFER-VALUE)) + ')'
                          + ' AND ProfileNr  = ' + STRING(clButiker.profilnr)
                           ,NO-LOCK
                           ) NO-ERROR.
    ELSE
      bhArtPris:FIND-FIRST('WHERE Artikkelnr = DEC(' + QUOTER(STRING(bhBuffer:BUFFER-FIELD('artikkelnr'):BUFFER-VALUE)) + ')'
                          + ' AND ProfileNr  = ' + STRING(clButiker.profilnr)
                           ,NO-LOCK
                           ) NO-ERROR.

  END.
  IF NOT bhArtPris:AVAIL THEN
  DO:
    ASSIGN 
      obOk     = FALSE
      ocReturn = "Ingen pris på artikkelen."
    .
    RETURN.
  END.

  FIND VarGr WHERE VarGr.Vg = INT(bhBuffer:BUFFER-FIELD('Vg'):BUFFER-VALUE) NO-LOCK NO-ERROR.
  IF NOT AVAILABLE VarGr THEN
  DO:
    ASSIGN
      obOk     = FALSE
      ocReturn = "Ukjent varegruppe på artikkelen."
    .
    RETURN.
  END.
  FIND HuvGr NO-LOCK WHERE HuvGr.Hg = VarGr.Hg NO-ERROR.
  IF NOT AVAILABLE HuvGr THEN
  DO:
    ASSIGN
      obOk     = FALSE
      ocReturn = "Ukjent hovedgruppe på artikkelen."
    .
    RETURN.
  END.
  FIND Produsent WHERE produsent.prodnr = INT(bhBuffer:BUFFER-FIELD('ProdNr'):BUFFER-VALUE) NO-LOCK NO-ERROR.
  IF NOT AVAILABLE Produsent THEN
  DO:
    ASSIGN
      obOk     = FALSE
      ocReturn = "Unkjent produsent på artikkelen."
    .
    RETURN.
  END.
  FIND LevBas WHERE LevBas.LevNr = INT(bhBuffer:BUFFER-FIELD('LevNr'):BUFFER-VALUE) NO-LOCK NO-ERROR.
  IF NOT AVAILABLE LevBas THEN
  DO:
    ASSIGN
      obOk     = FALSE
      ocReturn = "Ukjent leverandør på artikkelen."
    .
    RETURN.
  END.
  FIND Avdeling OF HuvGr NO-LOCK NO-ERROR.
  IF NOT AVAILABLE Avdeling THEN
  DO:
    ASSIGN
      obOk     = FALSE
      ocReturn = "Ukjent avdeling på artikkelens varegruppe."
    .
    RETURN.
  END.
  DO TRANSACTION:
    CREATE VareBokLinje.
    bhVareboklinje = BUFFER vareboklinje:HANDLE.
    bhVareboklinje:BUFFER-COPY(bhBuffer).
    ASSIGN
      VareBokLinje.VareBokNr            = fVareBokNr
      VareBokLinje.ProdusentBeskrivelse = Produsent.Beskrivelse
      VareBokLinje.VgBeskr              = VarGr.VgBeskr
      VareBokLinje.HgBeskr              = HuvGr.HgBeskr
      VareBokLinje.LevNamn              = LevBas.LevNamn
      VareBokLinje.AvdelingNr           = Avdeling.AvdelingNr
      VareBokLinje.AvdelingNavn         = Avdeling.AvdelingNavn
      VareBokLinje.Gjennomfaktureres    = TRUE
    .
    IF bhArtPris:AVAIL THEN
        ASSIGN
          VareBokLinje.Mva%         = IF bhBuffer:NAME = 'VPIArtBas' THEN bhArtPris:BUFFER-FIELD('Mva%'):BUFFER-VALUE(1)
                                      ELSE
                                        bhArtPris:BUFFER-FIELD('Mva%'):BUFFER-VALUE
        .
    ASSIGN
        VareBokLinje.supInnkjopsPris = VareBokLinje.InnkjopsPris
        VareBokLinje.supPris         = VareBokLinje.Pris        
        .
    RUN vareboklinje_kalkuler.p (bhVareboklinje,"forhRab%").
    RUN vareboklinje_kalkuler.p (bhVarebokLinje,"supRab%").
    
    IF AVAILABLE VareBokLinje THEN RELEASE Vareboklinje.
  END. /* TRANSACTION */
END PROCEDURE.


