
/*------------------------------------------------------------------------
    File        : asHentDataPRS.p
    Purpose     : 

    Syntax      :

    Description : Server rutine som trigges fra appserver.

    Author(s)   : 
    Created     : Sat Feb 24 12:06:01 CET 2018
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
{cls\BxLog\tempTabeller.i}    
{cls\BxLog\tempDatasett.i}

DEFINE INPUT  PARAMETER cLogg AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER DATASET FOR dsBxSupplier.
DEFINE OUTPUT PARAMETER DATASET FOR dsCompany.

DEFINE VARIABLE cSystem             AS CHARACTER                      NO-UNDO.
DEFINE VARIABLE ix AS INTEGER NO-UNDO.

DEFINE VARIABLE rStandardFunksjoner AS cls.StdFunk.StandardFunksjoner NO-UNDO.
DEFINE VARIABLE rHentDataPRS        AS cls.BxLog.HentDataPRS          NO-UNDO.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
rStandardFunksjoner  = NEW cls.StdFunk.StandardFunksjoner( cLogg ).

rStandardFunksjoner:hentSystem( INPUT-OUTPUT cSystem ).

rStandardFunksjoner:SkrivTilLogg(cLogg,
    'asHentRegisterDataPRS.p Start.' 
    ).    

rHentDataPRS = NEW cls.BxLog.HentDataPRS( cLogg ).


rStandardFunksjoner:SkrivTilLogg(cLogg,
    'asHentRegisterDataPRS.p Henter dsCompany.' 
    ).    
rHentDataPRS:initCompany( OUTPUT DATASET dsCompany ).

rStandardFunksjoner:SkrivTilLogg(cLogg,
    'asHentRegisterDataPRS.p Henter dsBxSupplier.' 
    ).    
rHentDataPRS:initBxSupplier( OUTPUT DATASET dsBxSupplier ).

rStandardFunksjoner:SkrivTilLogg(cLogg,
    'asHentRegisterDataPRS.p Ferdig.' 
    ).    

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
    

