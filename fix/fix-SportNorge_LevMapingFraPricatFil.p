
/*------------------------------------------------------------------------
    File        : fix-SportNorge_LevMapingFraPricatFil.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : tny
    Created     : Thu Oct 25 14:10:21 CEST 2018
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE VARIABLE cInnFil AS CHARACTER NO-UNDO.
DEFINE VARIABLE cUtFil AS CHARACTER NO-UNDO.
DEFINE VARIABLE cRecord AS CHARACTER NO-UNDO.
DEFINE VARIABLE iInt AS INTEGER NO-UNDO. 
DEFINE VARIABLE cEksternLev AS CHARACTER NO-UNDO.
DEFINE VARIABLE cInternLev AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEan AS CHARACTER NO-UNDO.
DEFINE VARIABLE iLoop AS INTEGER NO-UNDO.
DEFINE VARIABLE cEDB-System AS CHARACTER NO-UNDO.
DEFINE VARIABLE lDec AS DECIMAL NO-UNDO.
DEFINE VARIABLE cTabell AS CHARACTER NO-UNDO.
DEFINE VARIABLE cVaremerke AS CHARACTER NO-UNDO.
DEFINE VARIABLE iVmId AS INTEGER NO-UNDO.

DEFINE STREAM Inn.
DEFINE STREAM Ut.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
CURRENT-WINDOW:WIDTH = 350.

ASSIGN 
    cInnFil = 'konv\pricat08102018.csv'
    cUtfil  = 'konv\PRSMappingEksport.p'
    cEDB-System = 'Sport Norge Eksport'
    cTabell = 'LevBas'
    .
 
RUN lesPricat.
RUN lesLevReg.



/* **********************  Internal Procedures  *********************** */

PROCEDURE lesPricat:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/

INPUT STREAM Inn FROM VALUE(cInnFil).
REPEAT:
    IMPORT STREAM Inn UNFORMATTED 
        cRecord.
    IF TRIM(cRecord) = '' THEN 
        NEXT.
        
    ASSIGN 
        iInt = INT(ENTRY(2,cRecord,';'))
        NO-ERROR.
    IF ERROR-STATUS:ERROR OR iInt <= 0 THEN 
        NEXT.    
        
    ASSIGN 
        cEksternLev = ENTRY( 2,cRecord,';')
        cVareMerke  = ENTRY(10,cRecord,';')
        cEan        = ENTRY( 4,cRecord,';')
        .
        
    IF NUM-ENTRIES(cRecord,';') >= 24 THEN 
    MAPPING:
    DO: 
        ASSIGN 
            cEan = ENTRY(4,cRecord,';')
            .
        IF TRIM(cEan) = '' THEN 
            NEXT.
        ASSIGN 
            lDec = Dec(cEan)
            NO-ERROR.
        IF ERROR-STATUS:ERROR OR lDec <= 0 THEN 
            NEXT.
        FIND StrekKode NO-LOCK WHERE 
            StrekKode.Kode = cEan NO-ERROR.
        IF NOT AVAILABLE StrekKode THEN 
            NEXT.
        FIND ArtBas OF StrekKode NO-ERROR.     
        IF NOT AVAILABLE ArtBas THEN 
            NEXT.
        cInternLev = STRING(ArtBas.LevNr).
                
        FIND FIRST ImpKonv NO-LOCK WHERE 
            ImpKonv.EDB-System = cEDB-System AND 
            ImpKonv.Tabell = cTabell AND 
            ImpKonv.EksterntID = cEksternLev AND 
            ImpKonv.InterntID = cInternLev NO-ERROR.
        IF NOT AVAILABLE ImpKonv THEN 
        MORGENLYSET:
        DO:     
            CREATE ImpKonv.
            ASSIGN 
                ImpKonv.EDB-System = cEDB-System
                ImpKonv.Tabell     = cTabell
                ImpKonv.InterntID  = cInternLev
                ImpKonv.EksterntId = cEksternLev
                ImpKonv.Merknad    = ENTRY(24,cRecord,';')
                .
            
        END. /* MORGENLYSET */
    END. /* MAPPING */
    
    IF cVareMerke <> '' THEN 
    VAREMERKE:
    DO:
        FIND FIRST Varemerke NO-LOCK WHERE 
            Varemerke.Beskrivelse = cVareMerke NO-ERROR.
        IF AVAILABLE Varemerke THEN 
        DO:
            FIND StrekKode NO-LOCK WHERE 
                StrekKode.Kode = cEan NO-ERROR.
            IF AVAILABLE StrekKode THEN 
                FIND ArtBas OF StrekKode EXCLUSIVE-LOCK NO-ERROR.
            IF AVAILABLE ArtBas THEN 
                ArtBas.VMId = Varemerke.VmId.
        END.
        IF NOT AVAILABLE Varemerke THEN
        OPPRETT_VAREMERKE: 
        DO:
            FIND LAST Varemerke NO-LOCK NO-ERROR.
            IF AVAILABLE Varemerke THEN 
                iVmId = Varemerke.VMId + 1.
            ELSE 
                iVmId = 1.
            CREATE Varemerke.
            ASSIGN 
                Varemerke.VMId = iVmId
                Varemerke.Beskrivelse = cVareMerke
                Varemerke.Kortnavn = cVareMerke
                Varemerke.Merknad = 'Pricat import - ' + cEan /* Fra pricat */
                .
            FIND StrekKode NO-LOCK WHERE 
                StrekKode.Kode = cEan NO-ERROR.
            IF AVAILABLE StrekKode THEN 
                FIND ArtBas OF StrekKode EXCLUSIVE-LOCK NO-ERROR.
            IF AVAILABLE ArtBas THEN 
                ArtBas.VMId = Varemerke.VMId.
        END. /* OPPRETT_VAREMERKE */ 
    END. /* VAREMERKE */
        
END.    
INPUT STREAM Inn CLOSE.    

END PROCEDURE.


PROCEDURE lesLevReg:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cDefaultLevNr AS CHARACTER NO-UNDO.
    
    ASSIGN 
        cDefaultLevNr = '999999'
        .
        
    FOR EACH LevBas NO-LOCK:
        FIND FIRST ImpKonv NO-LOCK WHERE 
            ImpKonv.EDB-System = cEDB-System AND 
            ImpKonv.Tabell = cTabell AND 
            ImpKonv.InterntID = STRING(LevBas.LevNr) NO-ERROR.
        IF NOT AVAILABLE ImpKonv THEN 
        MORGENLYSET:
        DO:                 
            CREATE ImpKonv.
            ASSIGN 
                ImpKonv.EDB-System = cEDB-System
                ImpKonv.Tabell     = cTabell
                ImpKonv.InterntID  = STRING(LevBas.LevNr)
                ImpKonv.EksterntId = cDefaultLevNr
                ImpKonv.Merknad    = (IF CAN-FIND(FIRST ArtBas WHERE ArtBas.LevNr = LevBas.levnr) THEN '(Har artikler) - ' ELSE '(Har IKKE artikler) - ') + 
                                     LevBas.LevNamn 
                .
            
        END. /* MORGENLYSET */
        
    END.

END PROCEDURE.



