
/*------------------------------------------------------------------------
    File        : fix-PakkseddelLCRapportPrArtikkel.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : tny
    Created     : Tue Jan 08 09:24:12 CET 2019
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE VARIABLE cUtFil AS CHARACTER NO-UNDO.

DEFINE STREAM Ut.

CURRENT-WINDOW:WIDTH = 350.


/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
ASSIGN 
    cUtFil = 'konv\PkSdlLCRapportLand' + REPLACE(STRING(TODAY),'/','') + '_' + REPLACE(STRING(TIME,"HH:MM:SS"),':','') + '.csv'
    .

OUTPUT STREAM Ut TO VALUE(cUtFil).

PUT STREAM Ut UNFORMATTED 
    'PkSdlId;'
    'PkSdlNr;'
    'ekstId;'
    'PkSdlStatus;'
    'ButikkNr;'
    'ButNamn;'
    'ArtikkelNr;'
    'Strekkode;'
    'Beskr;'
    'LevKod;'
    'LevFargKod;'
    'Sesong;'
    'Varemerke;'
    'Produsent;'
    'LandKode;'
    'Land;'
    'KjedeInnkPris;'
    'Antall;'
    'Sum;'
    'Registrert;'
    'Sendt;'
    'Levert'
SKIP.        
 
FOR EACH PkSdlHode NO-LOCK WHERE 
    PkSdlHode.PkSdlStatus = 10,
    EACH PkSdlLinje OF PkSdlHode NO-LOCK,
    FIRST PkSdlPris OF PkSdlHode NO-LOCK WHERE 
        PkSdlPris.ArtikkelNr = PkSdlLinje.ArtikkelNr,
    FIRST ArtBas NO-LOCK WHERE 
        ArtBas.ArtikkelNr = PkSdlPris.ArtikkelNr,
    FIRST Butiker NO-LOCK WHERE 
        Butiker.Butik = PkSdlLinje.ButikkNr,
    FIRST Varemerke NO-LOCK WHERE 
        Varemerke.VMId = ArtBas.VmId,
    FIRST Produsent NO-LOCK WHERE 
        Produsent.ProdNr = ArtBas.ProdNr:
            
    IF AVAILABLE AlfaLandKode THEN RELEASE AlfaLandKode.
    IF AVAILABLE NumLandKode THEN RELEASE NumLandKode.
    
    FIND FIRST AlfaLandKode NO-LOCK WHERE 
        AlfaLandKode.AlfaKode2 = ArtBas.AlfaKode2 NO-ERROR.
    IF AVAILABLE AlfaLandKode THEN 
        FIND FIRST NumLandKode NO-LOCK WHERE
            NumLandKode.NumLandKode = AlfaLandKode.NumLandKode NO-ERROR.     
                
    /*            
    DISPLAY 
        PkSdlHode.PkSdlId
        PkSdlHode.PkSdlNr
        PkSdlHode.ekstId
        PkSdlHode.PkSdlStatus
        PkSdlLinje.ButikkNr
        Butiker.ButNamn
        PkSdlLinje.ArtikkelNr
        ArtBas.Beskr
        ArtBas.LevKod
        ArtBas.LevFargKod
        ArtBas.AlfaKode2
        NumLandKode.Land
        ArtBas.KjedeInnkPris
        PkSdlLinje.Antall
        ArtBas.KjedeInnkPris * PkSdlLinje.Antall COLUMN-LABEL 'Sum'
    WITH WIDTH 350.
    */
    
    PUT STREAM Ut UNFORMATTED 
        PkSdlHode.PkSdlId ';'
        PkSdlHode.PkSdlNr ';'
        PkSdlHode.ekstId ';'
        PkSdlHode.PkSdlStatus ';'
        PkSdlLinje.ButikkNr ';'
        Butiker.ButNamn ';'
        PkSdlLinje.ArtikkelNr ';'
        PkSdlLinje.Kode ';'
        ArtBas.Beskr ';'
        ArtBas.LevKod ';'
        ArtBas.LevFargKod ';'
        ArtBas.SaSong ';'
        Varemerke.Beskrivelse ';'
        Produsent.Beskrivelse ';'
        ArtBas.AlfaKode2 ';'
        (IF AVAILABLE NumLandKode THEN NumLandKode.Land ELSE '') ';'
        ArtBas.KjedeInnkPris ';'
        PkSdlLinje.Antall ';'
        ArtBas.KjedeInnkPris * PkSdlLinje.Antall ';'
        PkSdlHode.RegistrertDato ';'
        PkSdlHode.SendtDato ';'
        PkSdlHode.LeveringsDato
    SKIP.        
END.             
OUTPUT STREAM Ut CLOSE.
 