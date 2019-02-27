/* Setter ny størrelsestype på artikkel og alle andre artikler i samme modell. */
/* bibl_opprettStrtypeForModell.p */
/* Tar også hånd om VPI registeret. */
DEFINE INPUT  PARAMETER lArtikkelNr AS DECIMAL NO-UNDO.
DEFINE OUTPUT PARAMETER iStrTypeId  AS INTEGER NO-UNDO. 

DEFINE VARIABLE lModellFarge AS DECIMAL   FORMAT ">>>>>>>>>>>>9" NO-UNDO. 
DEFINE VARIABLE cTekst       AS CHARACTER FORMAT "x(20)" NO-UNDO.
DEFINE VARIABLE bFlagg AS LOG NO-UNDO.

DEFINE BUFFER bArtBas       FOR ArtBas.
DEFINE BUFFER bufArtBas     FOR ArtBas.
DEFINE BUFFER bufVPIArtBas  FOR VPIArtBas.
DEFINE BUFFER buf2VPIArtBas FOR VPIArtBas.

ASSIGN
  lModellFarge = 0
  iStrTypeId   = 0.
ARTBASEN:
DO FOR bArtBas TRANSACTION:
    FIND bArtBas NO-LOCK WHERE
      bArtBas.ArtikkelNr = lArtikkelNr NO-ERROR.
    IF NOT AVAILABLE bArtBas THEN 
      LEAVE ARTBASEN.

    IF bArtBas.ModellFarge > 0 
        THEN lModellFarge = bArtBas.ModellFarge.
    ELSE lModellFarge = bArtBas.ArtikkelNr.

    ASSIGN
        bFlagg = TRUE 
        cTekst = 'Modell ' + STRING(lModellFarge).
    
    /* Legger opp størrelsestype og legger inn alle størrelser. */
    FOR EACH Strekkode OF bArtBas NO-LOCK:
        FIND StrKonv NO-LOCK WHERE
            StrKonv.StrKode = Strekkode.StrKode NO-ERROR.
        IF AVAILABLE StrKonv THEN 
          RUN opprett_strtype2.p (cTekst, StrKonv.Storl, bArtBas.Vg, OUTPUT iStrTypeId). 
    END.
END. /* ARTBASEN TRANSACTION */
      


