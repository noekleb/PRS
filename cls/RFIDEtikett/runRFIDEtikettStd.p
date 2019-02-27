
/*------------------------------------------------------------------------
    File        : testRFIDEtikett.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : 
    Created     : Wed May 16 09:06:30 CEST 2018
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE INPUT PARAMETER iSkriverNr AS INTEGER NO-UNDO.

DEFINE VARIABLE iLoop AS INTEGER NO-UNDO.
DEFINE VARIABLE lArtikkelNr AS DECIMAL FORMAT ">>>>>>>>>>>>>9" NO-UNDO.
DEFINE VARIABLE iSortering AS INTEGER NO-UNDO.
DEFINE VARIABLE cLogg AS CHARACTER NO-UNDO.

DEFINE VARIABLE rStandardFunksjoner AS CLASS cls.StdFunk.StandardFunksjoner NO-UNDO.
DEFINE VARIABLE rRFIDEtikettTilFil AS CLASS cls.RFIDEtikett.RFIDEtikettTilFil NO-UNDO.
DEFINE VARIABLE rftpSendFile        AS CLASS cls.RFIDEtikett.ftpSendFile    NO-UNDO.

{cls\RFIDEtikett\etiko.i}
{etikettlogg.i}

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */

ASSIGN 
    cLogg   = 'RFIDEtikettStd' + REPLACE(STRING(TODAY),'/','') 
    .

rStandardFunksjoner = NEW cls.StdFunk.StandardFunksjoner( ).
rRFIDEtikettTilFil = NEW cls.RFIDEtikett.RFIDEtikettTilFil( INPUT cLogg ).

RUN genTmpFile.

IF CAN-FIND(FIRST ttEtiko) THEN  
DO: 
    rRFIDEtikettTilFil:Skrivetikett( INPUT DATASET dsEtiko ).
END.

RETURN.

/* **********************  Internal Procedures  *********************** */

PROCEDURE genTmpFile:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
           field Vg        like ArtBas.Vg
  field LopNr     like ArtBas.LopNr
  field Ant       as INTEGER FORMAT "->>>>>>>9"
  field Storl     as char
  field bongtekst as char
  field pris      as dec format "-zzz,zz9.99"
  field pris2     as dec format "-zzz,zz9.99"
  FIELD individ   AS DEC DECIMALS 0
  field butik     as int
  field SeqNr     as int.
         
    ------------------------------------------------------------------------------*/

    EMPTY TEMP-TABLE ttEtiko.
    
    /* TEST */
/*    TEMP-TABLE EtikettLogg:WRITE-JSON('file', 'konv\EtikettLogg' +                 */
/*                                          REPLACE(STRING(TODAY),'/','') +          */
/*                                          '_' +                                    */
/*                                          REPLACE(STRING(TIME,"HH:MM:SS"),':','') +*/
/*                                          '.json', TRUE).                          */
    
    FOR EACH EtikettLogg:
        FIND ArtBas NO-LOCK WHERE 
            ArtBas.Vg = etikettLogg.Vg AND
            ArtBas.LopNr = EtikettLogg.LopNr NO-ERROR. 
        FIND StrKonv NO-LOCK WHERE 
            StrKonv.Storl = EtikettLogg.Storl NO-ERROR.
        IF NOT AVAILABLE ArtBas AND AVAILABLE StrKonv THEN 
            NEXT.
        FIND LAST Strekkode NO-LOCK WHERE 
            Strekkode.ArtikkelNr = ArtBas.ArtikkelNr AND
            Strekkode.StrKode    = StrKonv.StrKode NO-ERROR.
        IF NOT AVAILABLE Strekkode THEN 
            NEXT.
        FIND VarGr OF ArtBas NO-LOCK NO-ERROR.
        FIND Varemerke OF ArtBas NO-LOCK NO-ERROR.
        FIND LevBas OF ArtBas NO-LOCK NO-ERROR.
  
        iSortering = iSortering + 1.
    
        CREATE ttEtiko.
        ASSIGN 
            ttEtiko.BrukerId     = USERID('SkoTex')
            ttEtiko.StyleCode    = 1
            ttEtiko.ButNr        = EtikettLogg.butik
            ttEtiko.Sortering    = STRING(Etikettlogg.SeqNr)
            ttEtiko.SekNr        = Etikettlogg.SeqNr
            ttEtiko.Ean          = Strekkode.Kode
            ttEtiko.Skrivernavn  = '1'
            ttEtiko.storrtekst   = StrKonv.Storl
            ttEtiko.fargetekst   = ArtBas.LevFargKod
            ttEtiko.quantity     = EtikettLogg.Ant
            ttEtiko.Etitekst1    = EtikettLogg.bongtekst
            ttEtiko.enhtekst     = ArtBas.SalgsEnhet
            ttEtiko.utpris       = EtikettLogg.Pris
            ttEtiko.antpkn       = ArtBas.AntIPakn
            ttEtiko.emb          = ''
            ttEtiko.hgr          = ArtBas.Vg
            ttEtiko.sortkode     = ''
            ttEtiko.levnr        = ArtBas.LevNr
            ttEtiko.bestnr       = Strekkode.Bestillingsnummer
            ttEtiko.enhpris      = EtikettLogg.Pris
            ttEtiko.pristekst    = ''
            ttEtiko.prisntekst   = ''
            ttEtiko.levvnr       = ArtBas.LevKod
            ttEtiko.veilpris     = EtikettLogg.pris2
            ttEtiko.hgrtekst     = VarGr.VgBeskr
            ttEtiko.fabrikatnavn = Varemerke.Beskrivelse  
            ttEtiko.levnavn      = LevBas.levnamn
            ttEtiko.modellnr2    = ArtBas.LevKod
            ttEtiko.utskrdato    = TODAY
            ttEtiko.OpprettetDatoTid = NOW 
            .            
    END.
    
    /* TEST */
/*    TEMP-TABLE ttEtiko:WRITE-JSON('file', 'konv\ttEtiko' +                         */
/*                                          REPLACE(STRING(TODAY),'/','') +          */
/*                                          '_' +                                    */
/*                                          REPLACE(STRING(TIME,"HH:MM:SS"),':','') +*/
/*                                          '.json', TRUE).                          */
    
END PROCEDURE.

