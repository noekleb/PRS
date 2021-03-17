
/*------------------------------------------------------------------------
    File        : PkSdlUtPrisKontroll.p
    Purpose     : Hindre at priser som er endret etter at pakkseddel er kommet 
                  inn, blir overskrevet ved varemottak. Det er bare forhåndsordre 
                  pakksedler som skal kunne overstyre prisen. For Gant er dette 
                  ordretype 1 og 12.

    Syntax      :

    Description : Utfører priskontroll på Utpris.

    Author(s)   : Tom nøkleby
    Created     : Thu Dec 21 16:46:18 CET 2017
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE INPUT PARAMETER lPkSdlId AS DECIMAL NO-UNDO.

DEFINE VARIABLE cOutletLst AS CHARACTER NO-UNDO.
DEFINE VARIABLE cTekst AS CHARACTER NO-UNDO.
DEFINE VARIABLE iButNr AS INTEGER NO-UNDO.
DEFINE VARIABLE iProfilNr AS INTEGER NO-UNDO.

DEF VAR fDbKr   AS DEC NO-UNDO.
DEF VAR fMvaKr  AS DEC NO-UNDO.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
{syspara.i 22 5 2 cOutletLst}

/* Avbryter hvis ikke pakkseddel finnes. */
FIND PkSdlHode NO-LOCK WHERE 
    PkSdlHode.PkSdlId = lPksdlId NO-ERROR.
IF NOT AVAILABLE PkSdlHode THEN 
    RETURN.
    
/* Bare nye ordre skal sjekkes. */    
IF PkSdlHode.PkSdlStatus <> 10 THEN 
    RETURN.    
    
/* Det må ligge pakkseddel linjer på pakkseddelen. */    
FIND FIRST PkSdlLinje OF PkSdlHode NO-LOCK NO-ERROR.
IF NOT AVAILABLE PkSdlLinje THEN 
    RETURN.
ELSE 
    iButNr = PkSdlLinje.ButikkNr.
IF iButNr = 0 THEN 
    RETURN.
    
/* Skal ikke gjøres for Outlet. */
IF CAN-DO(cOutletLst,STRING(PkSdlLinje.ButikkNr)) THEN 
    RETURN.    
    
/* Forhåndsordre (Ordretype 1 og 12) skal ikke kontrolleres. */
IF CAN-DO(
          '1,12', 
          TRIM(PkSdlHode.OrdreType)        
          ) THEN
    RETURN.                
        
/* Henter Prisprofil for butikken. */
FIND Butiker NO-LOCK WHERE 
    Butiker.butik = iButNr NO-ERROR.
IF NOT AVAILABLE Butiker THEN 
    RETURN.
iProfilNr = Butiker.ProfilNr.
IF iProfilNr = 0 THEN 
    RETURN.        
        
/* Sjekker priser. Først lokal pris, deretter HK prisen hvis ikke lokal pris finnes. */
PRISBLOKK:
FOR EACH PkSdlPris OF PkSdlHode EXCLUSIVE-LOCK:
    FIND ArtPris NO-LOCK WHERE 
        ArtPris.ArtikkelNr = PkSdlPris.ArtikkelNr AND 
        ArtPris.ProfilNr   = iProfilNr NO-ERROR.
    IF NOT AVAILABLE ArtPris THEN     
        FIND ArtPris NO-LOCK WHERE 
            ArtPris.ArtikkelNr = PkSdlPris.ArtikkelNr AND 
            ArtPris.ProfilNr   = 1 NO-ERROR.
    IF NOT AVAILABLE ArtPris THEN 
        NEXT.
    
    /* Tar prisen fra kalkylen og legger den inn på pakkseddelen. */
    IF PkSdlPris.NyPris <> ArtPris.Pris[1] THEN 
    DO:
        ASSIGN 
            PkSdlPris.NyPris  = ArtPris.Pris[1]
            fMvaKr            = PkSdlPris.NyPris - PkSdlPris.NyPris / (1 + 25 / 100)
            fDbKr             = PkSdlPris.NyPris - fMvaKr - PkSdlPris.NyVarekost
            PkSdlPris.NyFrakt = 0 
            PkSdlPris.NyDB%   =  ROUND((fDbKr * 100) / (PkSdlPris.NyPris - fMvaKr),2) 
            .
    END.    
    
END. /* PRISBLOKK*/