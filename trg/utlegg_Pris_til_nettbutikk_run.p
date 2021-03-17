/* Legger ut pris til nettbutikken. */

DEFINE VARIABLE iProfilNr   AS INTEGER NO-UNDO.
DEFINE VARIABLE iCL         AS INTEGER NO-UNDO.
DEFINE VARIABLE iNettButikk AS INTEGER NO-UNDO.
DEFINE VARIABLE cErrLoggFil AS CHARACTER NO-UNDO.

DEFINE BUFFER clButiker FOR Butiker.
DEFINE BUFFER wbButiker FOR Butiker.

ASSIGN
  cErrLoggFil = 'Utlegg_pris_nettbutikk' + REPLACE(STRING(TODAY),'/','').

/* Henter nettbutikk. Er ikke nettbutikk lagt opp, velges sentrallageret. */
/* Uansett er det buffer wbButiker som benyttes for nettbutikk.           */
{syspara.i 150 1 2 iNettButikk INT}
FIND wbButiker NO-LOCK WHERE 
  wbButiker.Butik = iNettButikk NO-ERROR.
IF NOT AVAILABLE wbButiker THEN 
  DO:
    FIND wbButiker NO-LOCK WHERE 
      wbButiker.Butik = icL NO-ERROR.
  END.

{syspara.i 5 1 1 iCL INT}
FIND clButiker NO-LOCK WHERE
  clButiker.Butik = iCl NO-ERROR.
  
IF NOT AVAILABLE clButiker OR NOT AVAILABLE wbButiker THEN 
DO:
  RUN bibl_loggDbFri.p (cErrLoggFil, 'Gant_utlegg_Pris_til_nettbutikk_run.p: ** Ukjent ' 
                                      + (IF NOT AVAILABLE clButiker THEN 'sentrallager' ELSE 'nettbutikk')   
                                      + ' (' + STRING(iCL) + ').').
  LEAVE.    
END.
ELSE DO:
    FOR EACH ArtBas NO-LOCK WHERE 
        ArtBas.WebButikkArtikkel = TRUE:
        /*    
        FIND ArtPris OF ArtBas NO-LOCK WHERE
          ArtPris.ProfilNr = wbButiker.ProfilNr NO-ERROR.
        IF NOT AVAILABLE ArtPris THEN 
        FIND ArtPris OF ArtBas NO-LOCK WHERE
          ArtPris.ProfilNr = clButiker.ProfilNr NO-ERROR.
        */
        
        FIND ELogg WHERE 
             ELogg.TabellNavn     = "ArtBas" AND
             ELogg.EksterntSystem = "WEBBUT"    AND
             ELogg.Verdier        = STRING(ArtBas.ArtikkelNr) NO-ERROR NO-WAIT.
        IF NOT LOCKED ELogg THEN 
        DO:
          IF NOT AVAIL ELogg THEN DO:
              CREATE ELogg.
              ASSIGN ELogg.TabellNavn     = "ArtBas"
                     ELogg.EksterntSystem = "WEBBUT"   
                     ELogg.Verdier        = STRING(ArtBas.ArtikkelNr).
          END.
          ASSIGN ELogg.EndringsType = 1 
                 ELogg.Behandlet    = FALSE.
          RELEASE ELogg.
        END.
          
    END.
END.

QUIT.
