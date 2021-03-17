DEFINE VARIABLE cLogg      AS CHARACTER NO-UNDO.
DEFINE VARIABLE bOk        AS LOG       NO-UNDO.
DEFINE VARIABLE cReturn    AS CHARACTER NO-UNDO.
DEFINE VARIABLE bTest      AS LOG NO-UNDO.
DEFINE VARIABLE iX         AS INTEGER   NO-UNDO.
DEFINE VARIABLE cLogKat  AS CHARACTER NO-UNDO.

DEFINE VARIABLE rStandardFunksjoner AS cls.StdFunk.StandardFunksjoner NO-UNDO.
DEFINE VARIABLE rFakturaEksportNavision AS cls.Navision.FakturaEksportNavision NO-UNDO.

HOVEDBLOKK:
DO  ON ERROR  UNDO, LEAVE
    ON ENDKEY UNDO, LEAVE
    ON STOP   UNDO, LEAVE
    ON QUIT   UNDO, LEAVE:

    ASSIGN 
        cLogKat  = 'log\' 
        cLogg     = 'FakturaEksportNavision' + REPLACE(STRING(TODAY),'/','')
        bTest     = TRUE 
        .
    rStandardFunksjoner  = NEW cls.Stdfunk.StandardFunksjoner( INPUT cLogg ).    
    /* Starter med tom linje i loggen. */
    rStandardFunksjoner:SkrivTilLogg(cLogg,
        '' 
        ).    
    rStandardFunksjoner:SkrivTilLogg(cLogg,
        'Start.' 
        ).    

    /* Starter klassene. */
    rFakturaEksportNavision = NEW cls.Navision.FakturaEksportNavision( INPUT cLogg ).

    /* Eksporterer alle ikke eksporterte faktura. */
    rFakturaEksportNavision:eksporterFakturaData(  ).

	CATCH e1 AS Progress.Lang.AppError:
    	DO ix = 1 TO e1:NumMessages:
            rStandardFunksjoner:SkrivTilLogg(cLogg,
                '  AppError: ' + e1:GetMessage(ix) 
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
                '  Lang.Error: ' + e2:GetMessage(ix) 
                ).    
		END.
    END CATCH.
END. /* HOVEDBLOKK */

rStandardFunksjoner:SkrivTilLogg(cLogg,
    'Ferdig.' 
    ).    

/* Avslutter */
QUIT.

FINALLY.
/* Må kjøres fra infopos02. */
/*    RETURN cReturn.*/
END FINALLY.


/* **********************  Internal Procedures  *********************** */
