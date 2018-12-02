/* Setter ny størrelsestype på artikkel og alle andre artikler i samme modell. */
/* bibl_opprettStrtypeForModell.p */
/* Tar også hånd om VPI registeret. */
DEFINE INPUT  PARAMETER lArtikkelNr   AS DECIMAL NO-UNDO.
DEFINE INPUT  PARAMETER iEkstVPILEvNr AS INTEGER NO-UNDO.
DEFINE OUTPUT PARAMETER iStrTypeId  AS INTEGER NO-UNDO. 

DEF VAR lModellFarge AS DECIMAL   FORMAT ">>>>>>>>>>>>9" NO-UNDO. 
DEF VAR cTekst       AS CHARACTER FORMAT "x(20)" NO-UNDO.

DEF BUFFER bArtBas   FOR ArtBas.
DEF BUFFER bufArtBas FOR ArtBas.
DEFINE BUFFER bufVPIArtBas FOR VPIArtBas.
DEFINE BUFFER buf2VPIArtBas FOR VPIArtBAs.

/* Tar størrelsestypen fra artikkelen som er opprettet fra før. */
FIND  bArtBas NO-LOCK WHERE
    bArtBas.ArtikkelNr = lArtikkelNr NO-ERROR.
IF AVAILABLE bArtBas THEN 
  ASSIGN 
    iStrTypeId   = bArtBas.StrTypeId.

/* Hvis artikkelen bare ligger i VPIArtBas, må dette gjøres */
IF iStrTypeId = 0 THEN 
DO FOR bufVPIArtBas: 
  FOR EACH bufVPIArtBas NO-LOCK WHERE
      bufVPIArtBas.ekstVPILevNr = iEkstVPILEvNr AND 
      bufVPIArtBas.ArtikkelNr = lArtikkelNr:

      IF lModellFarge = 0 THEN 
      DO:
        IF bufVPIArtBas.ModellFarge > 0 
          THEN lModellFarge = bufVPIArtBas.ModellFarge.
        ELSE lModellFarge = bufVPIArtBas.ArtikkelNr.
      END. 
      cTekst = 'Modell ' + STRING(lModellFarge).
  
      /* Legger opp størrelsestype og legger inn alle størrelser. */
      IF iStrTypeId = 0 THEN 
      FOR EACH VPIStrekkode NO-LOCK WHERE 
          VPIStrekkode.EkstVPILevNr = bufVPIArtBas.EkstVPILevNr AND 
          VPIStrekkode.VareNr       = bufVPIArtBas.VareNr:
          FIND StrKonv NO-LOCK WHERE
              StrKonv.StrKode = VPIStrekkode.StrKode NO-ERROR.
          IF AVAILABLE StrKonv THEN 
            RUN opprett_strtype2.p (cTekst, StrKonv.Storl, bufVPIArtBas.Vg, OUTPUT iStrTypeId). 
      END.  
      
  END.
  IF AVAILABLE bufVPIArtBas THEN 
    RELEASE bufVPIArtBAs.
END. /* bufVPIArtBas */
