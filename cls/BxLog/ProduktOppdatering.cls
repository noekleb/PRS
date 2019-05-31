 
 /*------------------------------------------------------------------------
    File        : ProduktOppdatering
    Purpose     : Sikrer at data i artikkelregisteret alltid inneholder oppdaterte artikkeldata.
    Syntax      : 
    Description : Oppdater artikkel tabellen (BxProduct og BxUnit) i bxEngine databasen fra tilkoblet butikkdatasystem.
    Author(s)   : tny
    Created     : Fri Nov 10 15:15:38 CET 2017
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.

ROUTINE-LEVEL ON ERROR UNDO, THROW.

CLASS cls.BxLog.ProduktOppdatering: 
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    DEFINE VARIABLE rStandardFunksjoner AS cls.StdFunk.StandardFunksjoner NO-UNDO.
/*    DEFINE VARIABLE rHentDataPRS AS cls.BxLog.HentDataPRS NO-UNDO.    */
/*    DEFINE VARIABLE rHentDataChain AS cls.BxLog.HentDataChain NO-UNDO.*/
    DEFINE VARIABLE rLesSkrivBxEngine AS cls.BxLog.LesSkrivBxEngine NO-UNDO.
        
    DEFINE VARIABLE cLogg     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE bOk       AS LOG       NO-UNDO.
    DEFINE VARIABLE cReturMsg AS CHARACTER NO-UNDO. 
    DEFINE VARIABLE bTest     AS LOG       NO-UNDO.
    DEFINE VARIABLE cTekst    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cDatoTid  AS CHARACTER NO-UNDO. 
    DEFINE VARIABLE hDataSet  AS HANDLE    NO-UNDO.
    DEFINE VARIABLE iX        AS INTEGER   NO-UNDO.
        
    {cls\BxLog\tempTabeller.i}    
    {cls\BxLog\tempDatasett.i}
        
    /* Variabel som holder på informasjon om hvilket system det er integrert mot */    
    DEFINE PUBLIC PROPERTY cSystem AS CHARACTER NO-UNDO 
    GET.
    SET. 

    CONSTRUCTOR PUBLIC ProduktOppdatering ( INPUT pcLogg AS CHARACTER,
                                             OUTPUT pbOk AS LOG, 
                                             OUTPUT cReturMsg AS CHARACTER ):
                                               
        SUPER ().

        rStandardFunksjoner  = NEW cls.StdFunk.StandardFunksjoner( cLogg ) NO-ERROR.
        rLesSkrivBxEngine  = NEW cls.BxLog.LesSkrivBxEngine( cLogg ) NO-ERROR.

        rStandardFunksjoner:hentSystem( INPUT-OUTPUT cSystem ).
        
        ASSIGN 
            bTest     = TRUE
            cDatoTid  = REPLACE(STRING(TODAY),'/','') + REPLACE(STRING(TIME,"HH:MM:SS"),':','')
            cLogg     = pcLogg
            bOk       = pbOk
            cReturMsg = ''
            .
            
        /* Sikrer at produkttabellen er tom. */
        EMPTY TEMP-TABLE tmpBxProduct.
            
        /* Henter data og skriver disse til bxengine databasen. */
        rStandardFunksjoner:hentSystem(INPUT-OUTPUT cSystem).
        
        CASE cSystem:
            WHEN 'PRS' THEN DO ON ERROR UNDO, THROW:
                    RUN cls\bxLog\hentProduktDataPRSCl.p ( cLogg, OUTPUT DATASET dsBxProduct, OUTPUT DATASET dsBxUnit ).
                
/*                    rHentDataPRS = NEW cls.BxLog.HentDataPRS( cLogg ) NO-ERROR.*/
/*                    rHentDataPRS:initBxProduct( OUTPUT DATASET dsBxProduct,    */
/*                                                OUTPUT DATASET dsBxUnit ).     */
                END.
/*            WHEN 'Chain' THEN                                                      */
/*                DO ON ERROR UNDO, THROW:                                           */
/*                    rHentDataChain = NEW cls.BxLog.HentDataChain( cLogg ) NO-ERROR.*/
/*                    rHentDataChain:initBxProduct( OUTPUT DATASET dsBxProduct ).    */
/*                END.                                                               */
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

        /* Posterer enheter */
        IF CAN-FIND(FIRST tmpBxUnit) THEN 
        DO ON ERROR UNDO, THROW:
            ASSIGN 
                hDataset = DATASET dsBxUnit:HANDLE.
            rLesSkrivBxEngine:posterBxUnit( hDataSet ).
        END.
        
        /* Posterer produkter */
        IF CAN-FIND(FIRST tmpBxProduct) THEN 
        DO ON ERROR UNDO, THROW:
            rStandardFunksjoner:SkrivTilLogg(cLogg,
                '  Start rLesSkrivData:posterBxProduct' 
                ).    

            ASSIGN hDataset = DATASET dsBxProduct:HANDLE.
            rLesSkrivBxEngine:posterBxProduct( hDataSet ) NO-ERROR.

            rStandardFunksjoner:SkrivTilLogg(cLogg,
                '  Ferdig rLesSkrivData:posterBxProduct' 
                ).    
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
            
        IF bTest THEN 
        DO:
            cTekst = 'konv\BxProduct' + cDatoTid + '.JSon'.
            DATASET dsBxProduct:WRITE-JSON('file', cTekst, TRUE).
            rStandardFunksjoner:SkrivTilLogg(cLogg,
                '  Dataset: ' + cTekst 
                ).    
        END. 

        CATCH e1 AS Progress.Lang.AppError:
            DO ix = 1 TO e1:NumMessages:
                rStandardFunksjoner:SkrivTilLogg(cLogg,
                    '  ' + e1:GetMessage(ix) 
                    ).    
            END.
        
            IF e1:ReturnValue > "" THEN
                rStandardFunksjoner:SkrivTilLogg(cLogg,
                    '  Returverdi: ' + e1:ReturnValue 
                    ).    
        END CATCH.
        CATCH e2 AS Progress.Lang.Error:
            DO ix = 1 TO e2:NumMessages:
                rStandardFunksjoner:SkrivTilLogg(cLogg,
                    '  ' + e2:GetMessage(ix) 
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

    DESTRUCTOR PUBLIC ProduktOppdatering ( ):
        

    END DESTRUCTOR.

END CLASS.