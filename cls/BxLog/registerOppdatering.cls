 
 /*------------------------------------------------------------------------
    File        : registerOppdatering
    Purpose     : Sikrer at data i registertabellene alltid inneholder de samme data som ligger i tabellene de styres fra.
    Syntax      : 
    Description : Oppdater register tabellene i bxEngine databasen fra tilkoblet butikkdatasystem.
    Author(s)   : tny
    Created     : Mon Nov 06 14:42:38 CET 2017
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.

ROUTINE-LEVEL ON ERROR UNDO, THROW.

CLASS cls.BxLog.registerOppdatering: 
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    DEFINE VARIABLE rStandardFunksjoner AS cls.StdFunk.StandardFunksjoner NO-UNDO.
/*    DEFINE VARIABLE rHentDataPRS AS cls.BxLog.HentDataPRS NO-UNDO.*/
/*    DEFINE VARIABLE rHentDataChain AS cls.BxLog.HentDataChain NO-UNDO.*/
    DEFINE VARIABLE rLesSkrivBxEngine AS cls.BxLog.LesSkrivBxEngine NO-UNDO.
        
    DEFINE VARIABLE cLogg     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE bOk       AS LOG       NO-UNDO.
    DEFINE VARIABLE cReturMsg AS CHARACTER NO-UNDO. 
    DEFINE VARIABLE cSystem   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE bTest     AS LOG       NO-UNDO.
    DEFINE VARIABLE cTekst    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cDatoTid  AS CHARACTER NO-UNDO. 
    DEFINE VARIABLE hDataSet  AS HANDLE    NO-UNDO.
    DEFINE VARIABLE iX        AS INTEGER   NO-UNDO.
    DEFINE VARIABLE cLanguage AS CHARACTER NO-UNDO.
        
    {cls\BxLog\tempTabeller.i}    
    {cls\BxLog\tempDatasett.i}
        
    CONSTRUCTOR PUBLIC registerOppdatering ( INPUT pcLogg AS CHARACTER,
                                             OUTPUT pbOk AS LOG, 
                                             OUTPUT cReturMsg AS CHARACTER ):
        SUPER ().

        rLesSkrivBxEngine  = NEW cls.BxLog.LesSkrivBxEngine( cLogg ) NO-ERROR.
        rStandardFunksjoner  = NEW cls.StdFunk.StandardFunksjoner( cLogg ) NO-ERROR.

        ASSIGN 
            bTest     = TRUE
            cDatoTid  = REPLACE(STRING(TODAY),'/','') /*+ REPLACE(STRING(TIME,"HH:MM:SS"),':','')*/
            cLogg     = pcLogg
            bOk       = pbOk
            cReturMsg = ''
            cLanguage = 'NO'
            .

        rStandardFunksjoner:hentSystem( INPUT-OUTPUT cSystem ).
        rStandardFunksjoner:SkrivTilLogg(cLogg,
            '  Hentet System: ' + cSystem 
            ).    

        /* Sikrer at temp tabell registrene er tomme. */
        EMPTY TEMP-TABLE tmpCompany.
        EMPTY TEMP-TABLE tmpBxSupplier.
            
        /* Henter data og skriver disse til bxengine databasen. */
        rStandardFunksjoner:hentSystem(INPUT-OUTPUT cSystem).

        
        CASE cSystem:
            WHEN 'PRS' THEN DO ON ERROR UNDO, THROW:
                    RUN cls\bxLog\hentRegisterDataPRSCl.p ( cLogg, OUTPUT DATASET dsBxSupplier, OUTPUT DATASET dsCompany ).
            /*                    rHentDataPRS = NEW cls.BxLog.HentDataPRS( cLogg ) NO-ERROR.*/
            /*                    rHentDataPRS:initCompany( OUTPUT DATASET dsCompany ).      */
            /*                    rHentDataPRS:initBxSupplier( OUTPUT DATASET dsBxSupplier ).*/


                cTekst = 'konv\TEST_BxSupplier' + cDatoTid + '.JSon'.
                DATASET dsBxSupplier:WRITE-JSON('file', cTekst, TRUE).
                rStandardFunksjoner:SkrivTilLogg(cLogg,
                    '  Dataset: ' + cTekst 
                    ).    
                cTekst = 'konv\TEST_Company' + cDatoTid + '.JSon'.
                DATASET dsCompany:WRITE-JSON('file', cTekst, TRUE). 
                rStandardFunksjoner:SkrivTilLogg(cLogg,
                    '  Dataset: ' + cTekst 
                    ).    
                END.
/*            WHEN 'Chain' THEN                                                    */
/*                DO ON ERROR UNDO, THROW:                                         */
/*                    rHentDataChain = NEW cls.HentDataChain( cLogg ) NO-ERROR.    */
/*                    rHentDataChain:initCompany( OUTPUT DATASET dsCompany ).      */
/*                    rHentDataChain:initBxSupplier( OUTPUT DATASET dsBxSupplier ).*/
/*                END.                                                             */
        END CASE.

        /* Kobler opp Sql server. */
        bOk = rLesSkrivBxEngine:oppkoblingSqlServer( ).
        IF NOT bOk THEN 
        DO:
            rStandardFunksjoner:SkrivTilLogg(cLogg,
                '  ** Feil ved oppkobling av Sql Server. Oppdatering avbrutt.' 
                ).    
            RETURN.
        END.

        /* Posterer Company */
        IF CAN-FIND(FIRST tmpCompany) THEN 
        DO ON ERROR UNDO, THROW:
            ASSIGN 
                hDataset = DATASET dsCompany:HANDLE.
            rLesSkrivBxEngine:posterCompany( INPUT-OUTPUT hDataSet ).
        END.

        /* Oppretter profiler i tmp tabell */
        bOk = opprettClientProfile(  ).
        /* Posterer clientProfile. */
        IF CAN-FIND(FIRST tmpclientProfile) THEN 
        DO:
            ASSIGN 
                hDataset = DATASET dsClientProfile:HANDLE.
            rLesSkrivBxEngine:posterClientProfile( INPUT-OUTPUT hDataSet ).
        END.
        
        /* Oppretter bruker for hver butikk */
        bOk = opprettUser(  ).
        IF CAN-FIND(FIRST tmpUser) THEN 
        DO:
            ASSIGN 
                hDataset = DATASET dsUser:HANDLE.
            rLesSkrivBxEngine:posterUser( INPUT-OUTPUT hDataSet ).
        END.
        
        /* Oppretter kunder */
        bOk = opprettBxCustomer(  ).
        IF CAN-FIND(FIRST tmpBxCustomer) THEN 
        DO:
            ASSIGN 
                hDataset = DATASET dsBxCustomer:HANDLE.
            rLesSkrivBxEngine:posterBxCustomer( INPUT-OUTPUT hDataSet ).
        END.
        
        /* Posterer leverandør */
        IF CAN-FIND(FIRST tmpBxSupplier) THEN
        DO ON ERROR UNDO, THROW:
            ASSIGN hDataset = DATASET dsBxSupplier:HANDLE.
            rLesSkrivBxEngine:posterBxSupplier( INPUT-OUTPUT hDataSet ).
        END.
        
        /* Kobler ned SQL server. */
        bOk = rLesSkrivBxEngine:nedkoblingSqlServer(  ).
        IF NOT bOk THEN 
        DO:
            rStandardFunksjoner:SkrivTilLogg(cLogg,
                '  ** Feil ved nedkobling av Sql Server.' 
                ).    
            RETURN.
        END.

        /* Legger ut datasettene som er hentett og oppdatert mot SQL server. */
        /* Id feltene skal nå være utfyllt.                                  */
        IF bTest THEN 
        DO:
            cTekst = 'konv\BxSupplier' + cDatoTid + '.JSon'.
            DATASET dsBxSupplier:WRITE-JSON('file', cTekst, TRUE).
            rStandardFunksjoner:SkrivTilLogg(cLogg,
                '  Dataset: ' + cTekst 
                ).    
            cTekst = 'konv\Company' + cDatoTid + '.JSon'.
            DATASET dsCompany:WRITE-JSON('file', cTekst, TRUE). 
            rStandardFunksjoner:SkrivTilLogg(cLogg,
                '  Dataset: ' + cTekst 
                ).    
            cTekst = 'konv\ClientProfile' + cDatoTid + '.JSon'.
            DATASET dsClientProfile:WRITE-JSON('file', cTekst, TRUE). 
            rStandardFunksjoner:SkrivTilLogg(cLogg,
                '  Dataset: ' + cTekst 
                ).    
            cTekst = 'konv\User' + cDatoTid + '.JSon'.
            DATASET dsUser:WRITE-JSON('file', cTekst, TRUE). 
            rStandardFunksjoner:SkrivTilLogg(cLogg,
                '  Dataset: ' + cTekst 
                ).    
            cTekst = 'konv\BxCustomer' + cDatoTid + '.JSon'.
            DATASET dsBxCustomer:WRITE-JSON('file', cTekst, TRUE). 
            rStandardFunksjoner:SkrivTilLogg(cLogg,
                '  Dataset: ' + cTekst 
                ).    
        END. 
            
        CATCH e1 AS Progress.Lang.AppError:
            DO ix = 1 TO e1:NumMessages:
                rStandardFunksjoner:SkrivTilLogg(cLogg,
                    'registerOppdatering.cls e1: ' + e1:GetMessage(ix) 
                    ).    
            END.
        
            IF e1:ReturnValue > "" THEN
                rStandardFunksjoner:SkrivTilLogg(cLogg,
                    'registerOppdatering.cls Returverdi e1: ' + e1:ReturnValue 
                    ).    
        END CATCH.
        CATCH e2 AS Progress.Lang.Error:
            DO ix = 1 TO e2:NumMessages:
                rStandardFunksjoner:SkrivTilLogg(cLogg,
                    'registerOppdatering.cls e2: ' + e2:GetMessage(ix) 
                    ).    
            END.
        END CATCH.
            
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    METHOD PUBLIC VOID CATCH(  ):
        
        RETURN.

    END METHOD.

    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    METHOD PUBLIC LOGICAL opprettBxCustomer(  ):
        
        DEFINE VARIABLE bResult AS LOGICAL NO-UNDO.

        EMPTY TEMP-TABLE tmpBxCustomer.

        FOR EACH tmpCompany:
            CREATE tmpBxCustomer.
            ASSIGN
                tmpBxCustomer.Customerno   = INT(tmpCompany.Warehouseno)
                tmpBxCustomer.Customername = tmpcompany.CompanyName
                .
        END. /* tmpCompany */

    END METHOD.

    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    METHOD PUBLIC LOGICAL opprettClientProfile(  ):
        
        DEFINE VARIABLE bResult AS LOGICAL NO-UNDO.

        EMPTY TEMP-TABLE tmpClientProfile.

        FOR EACH tmpCompany:
            CREATE tmpClientProfile.
            ASSIGN
                tmpclientProfile.CompanyId   = tmpCompany.Id
                tmpClientProfile.Profilename = tmpcompany.CompanyName
                tmpclientProfile.Language    = cLanguage
                tmpClientProfile.Comment     = STRING(tmpCompany.Warehouseno)
            .
        END. /* tmpCompany */
        
        RETURN bResult.

    END METHOD.

    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    METHOD PUBLIC LOGICAL opprettUser(  ):
        
        DEFINE VARIABLE bResult AS LOGICAL NO-UNDO.

        EMPTY TEMP-TABLE tmpUser.

        FOR EACH tmpClientProfile:
            CREATE tmpUser.
            ASSIGN
                tmpUser.ProfileId    = tmpClientProfile.Id
                tmpUser.Username     = STRING(tmpclientProfile.CompanyId)
                tmpUser.Password     = STRING(tmpclientProfile.CompanyId)
                tmpUser.Fullname     = tmpClientProfile.Profilename
                tmpUser.Enabled      = TRUE 
                tmpUser.UserType     = 'user'
                tmpUser.Warehouse    = STRING(tmpclientProfile.CompanyId)
                tmpUser.Employee     = tmpclientProfile.CompanyId
                tmpUser.Employeename = STRING(tmpclientProfile.CompanyId) + '/' + tmpClientProfile.Profilename 
                .
                
        END. /* tmpCompany */
        
        RETURN bResult.

    END METHOD.

    DESTRUCTOR PUBLIC registerOppdatering ( ):
        

    END DESTRUCTOR.

END CLASS.