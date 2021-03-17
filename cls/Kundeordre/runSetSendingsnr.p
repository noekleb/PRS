DEFINE VARIABLE ix         AS INTEGER NO-UNDO.
DEFINE VARIABLE cLogg      AS CHARACTER NO-UNDO.
DEFINE VARIABLE bOk        AS LOG       NO-UNDO.
DEFINE VARIABLE cReturnMsg AS CHARACTER NO-UNDO.
DEFINE VARIABLE iTime      AS INTEGER   NO-UNDO.
DEFINE VARIABLE dDato      AS DATE      NO-UNDO.

DEFINE VARIABLE rStandardFunksjoner AS cls.Stdfunk.StandardFunksjoner NO-UNDO.
DEFINE VARIABLE rsetSendingsnr AS cls.Kundeordre.setSendingsnr NO-UNDO.

HOVEDBLOKK:
DO ON ERROR  UNDO, LEAVE
	ON ENDKEY UNDO, LEAVE
	ON STOP   UNDO, LEAVE
	ON QUIT   UNDO, LEAVE:

    ASSIGN 
        iTime = TIME 
        dDato = TODAY 
        cLogg = 'setSendingsnr' + REPLACE(STRING(TODAY),'/','')
        .

    rStandardFunksjoner  = NEW cls.stdfunk.StandardFunksjoner(cLogg) NO-ERROR.        

    rStandardFunksjoner:SkrivTilLogg(cLogg,
        'Start av setSendingsnr.' 
        ).    
    
    /* Oppstart av og kjøring av register oppdateringer. */
    rsetSendingsnr  = NEW cls.Kundeordre.setSendingsnr( INPUT cLogg,
                                                        OUTPUT bOk ) NO-ERROR.
       
    
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
END. /* HOVEDBLOKK */

FINALLY.
    /*    IF VALID-OBJECT(rTemp) THEN DELETE OBJECT rTemp NO-ERROR.*/
    rStandardFunksjoner:SkrivTilLogg(cLogg,
        '  Tidsbruk ' + STRING(TIME - iTime,'HH:MM:SS') 
        ).    
    rStandardFunksjoner:SkrivTilLogg(cLogg,
        'Avsluttning av setSendingsnr - resultat ' + STRING(bOk) + '.' + CHR(10) 
        ).    
END FINALLY.
