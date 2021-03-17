DEFINE VARIABLE ix         AS INTEGER NO-UNDO.
DEFINE VARIABLE cLogg      AS CHARACTER NO-UNDO.
DEFINE VARIABLE bOk        AS LOG       NO-UNDO.
DEFINE VARIABLE cReturnMsg AS CHARACTER NO-UNDO.
DEFINE VARIABLE dDato      AS DATE      NO-UNDO.
DEFINE VARIABLE iTime      AS INTEGER NO-UNDO.
DEFINE VARIABLE cVgLst AS CHARACTER NO-UNDO.

DEFINE VARIABLE rStandardFunksjoner AS cls.StdFunk.StandardFunksjoner NO-UNDO.
DEFINE VARIABLE rHentUNI2Data AS cls.UNI2.HentUNI2Data NO-UNDO.

/* Endringer her skal ikke utløse ny ELogg post og resending av ordre. */    
ON CREATE OF ArtBas OVERRIDE DO: END.
ON WRITE  OF ArtBas OVERRIDE DO: END.
ON CREATE OF ArtBas OVERRIDE DO: END.

{cls\UNI2\tmpTblvArticle_NO.i}
{cls\UNI2\tmpDsvArticle_NO.i}
{cls\UNI2\tmpTblvArticles.i}
{cls\UNI2\tmpDsvArticles.i}
{cls\UNI2\tmpTblSeasons.i}
{cls\UNI2\tmpDsSeasons.i}
{cls\UNI2\tmpTblvSupplier.i}
{cls\UNI2\tmpDsvSupplier.i}
{cls\UNI2\tmpTblregArticles.i}
{cls\UNI2\tmpDsregArticles.i}
{cls\UNI2\tmpTblregArtSKU.i}
{cls\UNI2\tmpDsregArtSKU.i}
{cls\UNI2\tmpTblregEanSKU.i}
{cls\UNI2\tmpDsregEanSKU.i}
{cls\UNI2\tmpTblArtEan.i}
{cls\UNI2\tmpDsArtEan.i}

HOVEDBLOKK:
DO  ON ERROR  UNDO, LEAVE
    ON ENDKEY UNDO, LEAVE
    ON STOP   UNDO, LEAVE
    ON QUIT   UNDO, LEAVE:

    ASSIGN
        cVgLSt   = '10' /* Settes blank for flere varegrupper */
        dDato    = TODAY 
        cLogg    = 'runKorrVaregrupper' + REPLACE(STRING(TODAY),'/','')
        .

    IF cVgLst = ''  THEN 
    DO:
        FOR EACH VarGr NO-LOCK WHERE 
            VarGr.Vg >= 0 AND 
            VarGr.Vg <= 99:
            cVgLst = cVgLst + 
                     (IF cVgLst = '' THEN '' ELSE ',') + 
                     STRING(Vargr.Vg).
        END.
        
        FOR EACH VarGr NO-LOCK WHERE 
            VarGr.Vg > 999:
            cVgLst = cVgLst + 
                     (IF cVgLst = '' THEN '' ELSE ',') + 
                     STRING(Vargr.Vg).
        END.
    END.

    rStandardFunksjoner  = NEW cls.Stdfunk.StandardFunksjoner( cLogg ).
    /* Starter med tom linje i loggen. */
    rStandardFunksjoner:SkrivTilLogg(cLogg,
        '' 
        ).    
    rStandardFunksjoner:SkrivTilLogg(cLogg,
        'Start.' 
        ).    
    rStandardFunksjoner:SkrivTilLogg(cLogg,
        '  VgLst: ' + cVgLst 
        ).    
    /* Starter og kjører klassen. */
    rHentUNI2Data  = NEW cls.UNI2.HentUNI2Data( INPUT cLogg ).

    /* Kobler opp SQL server databasen */
    rHentUNI2Data:oppkoblingSQL( OUTPUT bOk).
    IF bOk THEN 
    DO:    
        rHentUNI2Data:hentvArticles_NOVarGrData( INPUT cVgLst, OUTPUT DATASET dsvArticle_NO ).
    END.
    
    /* Kobler ned SQL server databasen */
    rHentUNI2Data:nedkoblingSQL( OUTPUT bOk).    

    /* Oppdaterer LC i ArtBas. */
    IF CAN-FIND(FIRST tmpvArticle_NO) THEN
        rHentUNI2Data:oppdaterLC( INPUT DATASET dsvArticle_NO ).

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
        'Ferdig.' 
        ).    
END FINALLY.
