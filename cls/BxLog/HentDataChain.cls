 
 /*------------------------------------------------------------------------
    File        : HentDataPRS
    Purpose     : 
    Syntax      : 
    Description : Henter registerdata fra Chain databasen.
    Author(s)   : tny
    Created     : Mon Nov 06 16:56:58 CET 2017
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.

ROUTINE-LEVEL ON ERROR UNDO, THROW.

CLASS cls.BxLog.HentDataChain: 
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE rStandardFunksjoner AS cls.StdFunk.StandardFunksjoner NO-UNDO.
        
    DEFINE VARIABLE cLogg    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE bTest    AS LOG NO-UNDO.
    DEFINE VARIABLE cDatoTid AS CHARACTER NO-UNDO.  
    DEFINE VARIABLE cTekst  AS CHARACTER NO-UNDO.  
        
    {cls\BxLog\tempTabeller.i}    
    {cls\BxLog\tempDatasett.i}       
        
    CONSTRUCTOR PUBLIC HentDataChain ( INPUT pcLogg AS CHARACTER ):
        SUPER ().

        rStandardFunksjoner  = NEW cls.StdFunk.StandardFunksjoner() NO-ERROR.
        
        ASSIGN 
            cLogg    = pcLogg
            bTest    = FALSE 
            cDatoTid = REPLACE(STRING(TODAY),'/','') + REPLACE(STRING(TIME,"HH:MM:SS"),':','')
            .
                
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    METHOD PUBLIC LOGICAL initBxProduct( OUTPUT DATASET dsBxProduct ):
        
        DEFINE VARIABLE bResult AS LOGICAL NO-UNDO.

        RETURN bResult.

    END METHOD.

    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    METHOD PUBLIC VOID initBxSupplier( OUTPUT DATASET dsBxSupplier ):
        
        EMPTY TEMP-TABLE tmpBxSupplier.
        
        FOR EACH Lev NO-LOCK:
            CREATE tmpBxSupplier.
            ASSIGN 
                tmpBxSupplier.Supplierno   = (Lev.LevNr)
                tmpBxSupplier.Suppliername = STRING(Lev.Navn)
                tmpBxSupplier.Address      = Lev.Adresse 
                tmpBxSupplier.PostCode     = STRING(Lev.PostNr)
                tmpBxSupplier.Country      = ''
                tmpBxSupplier.City         = ''
                NO-ERROR.
            IF ERROR-STATUS:ERROR THEN 
            DO:
                IF AVAILABLE tmpBxSupplier AND 
                tmpBxSupplier.Supplierno = 0 THEN 
                    DELETE tmpBxSupplier.
                
            END.
        END.

        IF bTest THEN DO:
            cTekst = 'konv\BxSupplierChain' + cDatoTid + '.JSon'.
            DATASET dsBxSupplier:WRITE-JSON('file', cTekst, TRUE).
            rStandardFunksjoner:SkrivTilLogg(cLogg,
                '  Dataset: ' + cTekst 
                ).    
        END. 
        
        RETURN.

    END METHOD.

    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/


    METHOD VOID initCompany( OUTPUT DATASET dsCompany ):
        
        EMPTY TEMP-TABLE tmpCompany.

        FOR EACH Butikk NO-LOCK:
            CREATE tmpCompany.
            ASSIGN 
                tmpCompany.Id          = INT(Butikk.ButNr)
                tmpCompany.CompanyName = Butikk.Navn
                NO-ERROR.
            IF ERROR-STATUS:ERROR THEN 
            DO:
                IF AVAILABLE tmpCompany AND 
                    tmpCompany.Id = 0 THEN 
                    DELETE tmpCompany.
                
            END.
        END.

        IF bTest THEN
        DO: 
            cTekst = 'konv\CompanyChain' + cDatoTid + '.JSon'.
            DATASET dsCompany:WRITE-JSON('file', cTekst, TRUE). 
            rStandardFunksjoner:SkrivTilLogg(cLogg,
                '  Dataset: ' + cTekst 
                ).    
        END.
        
        RETURN.

    END METHOD.

    DESTRUCTOR PUBLIC HentDataChain ( ):

    END DESTRUCTOR.

END CLASS.