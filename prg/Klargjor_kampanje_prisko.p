DEFINE INPUT PARAMETER rRowId AS ROWID NO-UNDO.

DEFINE VARIABLE iTid AS INTEGER NO-UNDO.
DEFINE VARIABLE h_PrisKo AS HANDLE NO-UNDO.

IF NOT VALID-HANDLE(h_PrisKo) THEN
  RUN prisko.p PERSISTENT SET h_PrisKo.

FIND Butiker NO-LOCK WHERE
    Butiker.Butik = 16 NO-ERROR.
FIND ArtBas WHERE ROWID(ArtBas) = rRowId NO-ERROR.
IF NOT AVAILABLE ArtBas THEN 
  RETURN.
  
FIND ArtPris NO-LOCK WHERE
    ArtPris.ArtikkelNr = ArtBas.ArtikkelNR AND
    ArtPris.ProfilNr   = Butiker.ProfilNr NO-ERROR.

/* Sletter ikke startede kampanjer på artikkelen. */
FOR EACH PrisKo EXCLUSIVE-LOCK WHERE
    Prisko.ArtikkelNr = ArtBAs.ArtikkelNr AND
    PrisKo.ProfilNr   = ArtPris.ProfilNr AND
    PrisKo.TYPE       = 2: /* P åkampanje */
    
    DELETE Prisko.
END.

/* Slår av pågående kampanjer på artikkelen. */
DO:
    /* Initierer iTid. */
    iTid = TIME - 10.   
    FOR EACH PrisKo EXCLUSIVE-LOCK WHERE
        Prisko.ArtikkelNr = ArtBAs.ArtikkelNr AND
        PrisKo.ProfilNr   = ArtPris.ProfilNr AND
        PrisKo.TYPE       = 3: /* Av kampanje */
        
        /* Er det mer enn en kampanjepost, må itid manipuleres :) */
        ASSIGN 
            Prisko.AktiveresDato = TODAY 
            Prisko.AktiveresTid  = iTid
            iTid                 = iTid + 1
            .
    END.    
    RUN KlargjorPriskoEn IN h_PrisKo (ROWID(ArtBas)).
END.

IF VALID-HANDLE(h_PrisKo) THEN
  DELETE PROCEDURE h_PrisKo.

