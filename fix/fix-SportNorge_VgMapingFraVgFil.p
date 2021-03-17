
/*------------------------------------------------------------------------
    File        : fix-SportNorge_VgMapingFraVgFil.p
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
DEFINE VARIABLE cEksternVg AS CHARACTER NO-UNDO.
DEFINE VARIABLE cInternVg AS CHARACTER NO-UNDO.
DEFINE VARIABLE iLoop AS INTEGER NO-UNDO.
DEFINE VARIABLE cEDB-System AS CHARACTER NO-UNDO.
DEFINE VARIABLE cVaremerke AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEan AS CHARACTER NO-UNDO.

DEFINE STREAM Inn.
DEFINE STREAM Ut.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
CURRENT-WINDOW:WIDTH = 350.

ASSIGN 
    cInnFil = 'konv\Varegruppeplan_aktiv.csv'
    cUtfil  = 'konv\PRSMappingEksport.p'
    cEDB-System = 'Sport Norge Eksport'
    
    .
    
RUN lesFil.
RUN lesVgReg.


/* **********************  Internal Procedures  *********************** */

PROCEDURE lesFil:
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
        iInt = INT(ENTRY(1,cRecord,';'))
        NO-ERROR.
    IF ERROR-STATUS:ERROR OR iInt <= 0 THEN 
        NEXT.    
        
    ASSIGN 
        cEksternVg = ENTRY(5,cRecord,';')
        .
    IF NUM-ENTRIES(cRecord,';') >= 7 THEN 
    DO iLoop = 8 TO NUM-ENTRIES(cRecord,';'): 
        ASSIGN 
            cInternVg = ENTRY(iLoop,cRecord,';')
            .
        IF TRIM(cInternVg) = '' THEN 
            NEXT.
        ASSIGN 
            iInt = INT(cInternVg)
            NO-ERROR.
        IF ERROR-STATUS:ERROR THEN 
            NEXT.
        FIND FIRST ImpKonv NO-LOCK WHERE 
            ImpKonv.EDB-System = cEDB-System AND 
            ImpKonv.Tabell = 'VarGr' AND 
            ImpKonv.EksterntID = cEksternVg AND 
            ImpKonv.InterntID = cInternVg NO-ERROR.
        IF NOT AVAILABLE ImpKonv THEN 
        MORGENLYSET:
        DO:     
            CREATE ImpKonv.
            ASSIGN 
                ImpKonv.EDB-System = cEDB-System
                ImpKonv.Tabell     = 'VarGr'
                ImpKonv.InterntID  = cInternVg
                ImpKonv.EksterntId = cEksternVg
                ImpKonv.Merknad    = ENTRY(6,cRecord,';')
                .
            
        END. /* MORGENLYSET */
            
/*        DISPLAY                      */
/*            cEksternVg FORMAT "x(10)"*/
/*            cInternVg FORMAT "x(10)" */
/*        WITH WIDTH 350.              */
    END.
        
END.    
INPUT STREAM Inn CLOSE.
END PROCEDURE.

PROCEDURE lesVgReg:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cDefaultVg AS CHARACTER NO-UNDO.
    
    ASSIGN 
        cDefaultVg = '999' /* Diverse diverse */
        .
    
    FOR EACH VarGr NO-LOCK:
        
        FIND FIRST ImpKonv NO-LOCK WHERE 
            ImpKonv.EDB-System = cEDB-System AND 
            ImpKonv.Tabell = 'VarGr' AND 
            ImpKonv.InterntID = STRING(VarGr.Vg) NO-ERROR.
        IF NOT AVAILABLE ImpKonv THEN 
        MORGENLYSET:
        DO:                 
            CREATE ImpKonv.
            ASSIGN 
                ImpKonv.EDB-System = cEDB-System
                ImpKonv.Tabell     = 'VarGr'
                ImpKonv.InterntID  = STRING(VarGr.Vg)
                ImpKonv.EksterntId = cDefaultVg
                ImpKonv.Merknad    = (IF CAN-FIND(FIRST ArtBas WHERE ArtBas.Vg = VarGr.Vg) 
                                        THEN 'Har artikler - ' 
                                        ELSE ''  + VarGr.VgBeskr
                                     ) 
                .
            
        END. /* MORGENLYSET */
        
    END.

END PROCEDURE.

    