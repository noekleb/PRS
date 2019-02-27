DEFINE INPUT PARAMETER lArtikkelNr AS DECIMAL NO-UNDO.

DEFINE VARIABLE iCL AS INTEGER NO-UNDO.
DEFINE VARIABLE bOk AS LOG NO-UNDO.

{syspara.i 5 1 1 iCL INT}
FIND Butiker NO-LOCK WHERE
  Butiker.Butik = iCL NO-ERROR.
IF NOT AVAILABLE Butiker THEN 
RETURN.

FIND ArtBas NO-LOCK WHERE
  ArtBas.ArtikkelNr = lArtikkelNr NO-ERROR.
IF NOT AVAILABLE ArtBas THEN
  RETURN.
  
bOk = FALSE.  
MESSAGE 'Skal alle lokale priser slettes på denne artikkelen?' SKIP 
        'Dvs. priser opprettet lokalt i butikkslettes. Pris opprettet på HK blir liggende på artikkelen og vil gjelde for alle butikker.'
VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO UPDATE bOk.

IF bOk THEN 
DO:
  FOR EACH ArtPris OF ArtBas EXCLUSIVE-LOCK:
    IF ArtPris.ProfilNr <> Butiker.ProfilNr THEN 
      DELETE ArtPris.
  END.
  DO TRANSACTION:
    FIND ELogg WHERE 
         ELogg.TabellNavn     = "ArtBas" AND
         ELogg.EksterntSystem = "POS"    AND
         ELogg.Verdier        = STRING(ArtBas.ArtikkelNr) NO-ERROR.
    IF NOT AVAIL Elogg THEN DO:
    CREATE Elogg.
    ASSIGN ELogg.TabellNavn     = "ArtBas"
           ELogg.EksterntSystem = "POS"   
           ELogg.Verdier        = STRING(ArtBas.ArtikkelNr).
    END.
    ASSIGN ELogg.EndringsType = 1
          ELogg.Behandlet    = FALSE.
    IF AVAILABLE ELogg THEN RELEASE ELogg.
  END.
END.  

  