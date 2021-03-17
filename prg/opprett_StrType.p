/* Oppretter størrelsestype på ArtikkelNrSerie. */
DEFINE INPUT PARAMETER iEkstVPILevNr AS INTEGER   NO-UNDO.
DEFINE INPUT PARAMETER cVareNr       AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER iStrTypeId   AS INTEGER   NO-UNDO.

DEFINE VARIABLE iSeqNr AS INTEGER NO-UNDO.
DEFINE VARIABLE iAnt   AS INTEGER NO-UNDO.
DEFINE VARIABLE bFlagg AS LOG     NO-UNDO.
DEFINE VARIABLE bTest AS LOG NO-UNDO.

DEFINE TEMP-TABLE ttStr 
  FIELD Storl   AS CHARACTER 
  FIELD StrKode AS INTEGER 
  FIELD SeqNr   AS INTEGER
  .

FOR EACH ttSTr:
  DELETE ttSTr.
END.

bTest = TRUE.

IF CAN-FIND(FIRST StrType WHERE 
                  StrType.StrTypeId >= 999999) THEN 
DO:
  iStrTypeId = 2.
  RETURN.
END.                  

FIND VPIArtBas NO-LOCK WHERE
  VPIArtBas.EkstVPILevNr = iEkstVPILevNr AND
  VPIArtBas.VareNr       = cVareNr NO-ERROR.

IF bTest THEN RUN bibl_logg.p ('VPIImport', 'opprettStrType.p - 1').
 
/* Ukjent vare */  
IF NOT AVAILABLE VPIArtBas THEN 
DO:
  iStrTypeId = 2.
  RETURN.
END.

IF bTest THEN RUN bibl_logg.p ('VPIImport', 'opprettStrType.p - 2').

/* Definert fra før. Sjekker at alle størrelsene på artikkelen ligger i størrelsestypen. */
FIND ArtBas NO-LOCK WHERE
  ArtBas.ArtikkelNr = DECIMAL(VPIArtBas.VareNr) NO-ERROR.
IF AVAILABLE ArtBas THEN 
DO: 
  IF bTest THEN RUN bibl_logg.p ('VPIImport', 'opprettStrType.p - 3 Artikkel funnet ' + STRING(ArtBas.ArtikkelNr)).
  /* Sjekker i VPI registeret */
  bFlagg = TRUE.
  FOR EACH VPIStrekkode OF VPIArtBas NO-LOCK:
    FIND StrKonv NO-LOCK WHERE 
        StrKonv.StrKode = VPIStrekkode.StrKode NO-ERROR.
    IF AVAILABLE StrKonv THEN 
    DO:
        IF NOT CAN-FIND(StrTStr WHERE STrTStr.SoStorl = StrKonv.Storl) THEN 
          bFlagg = FALSE.
    END.
    ELSE bFlagg = FALSE.
  END.
  IF bTest THEN RUN bibl_logg.p ('VPIImport', 'opprettStrType.p - 4 Artikkel funnet ' + STRING(ArtBas.ArtikkelNr) + ' ' + STRING(bFlagg)).
  
  /* Sjekker i artikkelregisteret. */
  IF bFlagg = TRUE THEN 
  FOR EACH Strekkode NO-LOCK WHERE Strekkode.ArtikkelNr = VPIArtBas.ArtikkelNr:
    FIND StrKonv NO-LOCK WHERE 
        StrKonv.StrKode = Strekkode.StrKode NO-ERROR.
    IF AVAILABLE StrKonv THEN 
    DO:
        IF NOT CAN-FIND(StrTStr WHERE STrTStr.SoStorl = StrKonv.Storl) THEN 
          bFlagg = FALSE.
    END.
    ELSE bFlagg = FALSE.
  END.
  IF bTest THEN RUN bibl_logg.p ('VPIImport', 'opprettStrType.p - 5 Artikkel funnet ' + STRING(ArtBas.ArtikkelNr) + ' ' + STRING(bFlagg)).
  
  /* Allt ok, vi beholder størrelsestypen */
  IF bFlagg THEN 
  DO:
      IF bTest THEN RUN bibl_logg.p ('VPIImport', 'opprettStrType.p - 6 Artikkel funnet ' + STRING(ArtBas.ArtikkelNr) + ' ' + STRING(ArtBas.StrTypeId)).
    
      iStrTypeId = ArtBas.StrTypeId.
      RETURN.
  END.
END. 

IF bTest THEN RUN bibl_logg.p ('VPIImport', 'opprettStrType.p - 7').

/* Bygger temp-tabell fra VPI register */
BYGG_TMPTAB:
FOR EACH VPIStrekkode OF VPIArtBas NO-LOCK:
  FIND StrKonv NO-LOCK WHERE
    StrKonv.StrKode = VPIStrekkode.StrKode NO-ERROR.
  IF AVAILABLE StrKonv THEN 
  DO:
    FIND FIRST ttStr WHERE
      ttStr.StrKode = StrKonv.StrKode NO-ERROR.
    IF NOT AVAILABLE ttStr THEN 
      DO:
        CREATE ttStr.
        ASSIGN
           ttStr.Storl   = StrKonv.Storl
           ttStr.StrKode = StrKonv.StrKode
           ttStr.SeqNr   = StrKonv.SeqNr           
          .
      END.
  END.
END. /* BYGG_TMPTAB */

IF bTest THEN RUN bibl_logg.p ('VPIImport', 'opprettStrType.p - 8').

/* Bygger temp-tabell fra Artikkelregister */
BYGG_ART_TMPTAB:
FOR EACH Strekkode NO-LOCK WHERE Strekkode.ArtikkelNr = VPIArtBas.ArtikkelNr:
  FIND StrKonv NO-LOCK WHERE
    StrKonv.StrKode = Strekkode.StrKode NO-ERROR.
  IF AVAILABLE StrKonv THEN 
  DO:
    FIND FIRST ttStr WHERE
      ttStr.StrKode = StrKonv.StrKode NO-ERROR.
    IF NOT AVAILABLE ttStr THEN 
      DO:
        CREATE ttStr.
        ASSIGN
           ttStr.Storl   = StrKonv.Storl
           ttStr.StrKode = StrKonv.StrKode
           ttStr.SeqNr   = StrKonv.SeqNr           
          .
      END.
  END.
END. /* BYGG_ART_TMPTAB */

IF bTest THEN RUN bibl_logg.p ('VPIImport', 'opprettStrType.p - 9').

iSeqNr = 0.
/* Oppretter strtype */
OPPRETT:
FOR EACH ttStr BREAK 
  BY ttSTr.SeqNr:

  iSeqNr = iSeqNr + 1.

IF bTest THEN RUN bibl_logg.p ('VPIImport', 'opprettStrType.p - 9 - 1' + STRING(iSeqNr)).

  IF FIRST(ttStr.SeqNr) THEN 
    DO:
      /* Id settes i DB trigger */
      CREATE StrType.
      ASSIGN
        StrType.Beskrivelse = 'Generert ' + STRING(StrType.StrTypeId) + ' Art: ' + string(VPIArtBas.VareNr)
        iStrTypeId          = StrType.StrTypeId
        .
      DO:
        FIND HuvGr NO-LOCK WHERE
          HuvGr.Hg = VPIArtBas.Hg NO-ERROR.
        ASSIGN
          StrType.Beskrivelse = IF VPIArtBas.EkstStrTypeNavn <> '' 
                                  THEN VPIArtBas.EkstStrTypeNavn 
                                  ELSE StrType.Beskrivelse
          StrType.Hg          = VPIArtBas.Hg
          StrType.AvdelingNr  = (IF AVAILABLE HuvGr THEN HuvGr.AvdelingNr ELSE 0)
          .
      END.
    END.

IF bTest THEN RUN bibl_logg.p ('VPIImport', 'opprettStrType.p - 9 - 2' + STRING(iSeqNr)).

  IF NOT CAN-FIND(FIRST StrTStr WHERE
                  StrTStr.StrTypeId = iStrTypeId AND
                  StrTStr.SeqNr     = iSeqNr) THEN 
  DO:
    CREATE StrTStr.
    ASSIGN
      StrTStr.SeqNr     = iSeqNr
      StrTStr.STrTypeId = iStrTypeId
      StrTStr.SoStorl   = ttStr.Storl
      .
  END.

IF bTest THEN RUN bibl_logg.p ('VPIImport', 'opprettStrType.p - 9 - 3' + STRING(iSeqNr)).
  
END. /* OPPRETT */

IF bTest THEN RUN bibl_logg.p ('VPIImport', 'opprettStrType.p - 10').

IF iStrTypeId > 0 AND CAN-FIND(StrType WHERE StrTypeId = iStrTypeId) THEN 
  RUN settStrTypeFelt.p (iStrTypeID).
  
IF bTest THEN RUN bibl_logg.p ('VPIImport', 'opprettStrType.p - 11').
  
RETURN 'OK'.