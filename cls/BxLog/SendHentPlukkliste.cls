 
 /*------------------------------------------------------------------------
    File        : SendHentPlukkliste
    Purpose     : 
    Syntax      : 
    Description : Sender og henter plukklister til/fra BxLogistics.
    Author(s)   : tny
    Created     : ons Jan 17 13:32:10 CET 2018
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.


CLASS cls.BxLog.SendHentPlukkliste: 

    DEFINE VARIABLE cSystemName AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cLogg   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iX      AS INTEGER NO-UNDO.
    DEFINE VARIABLE bOk     AS LOG       NO-UNDO.
    DEFINE VARIABLE bTest   AS LOG       NO-UNDO.
    DEFINE VARIABLE ctekst  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iLoop   AS INTEGER   NO-UNDO.
    DEFINE VARIABLE cDatoTid AS CHARACTER NO-UNDO.
    DEFINE VARIABLE hDataset AS HANDLE NO-UNDO.
    DEFINE VARIABLE iMaksAntLister AS INTEGER NO-UNDO.
    DEFINE VARIABLE cKatalog AS CHARACTER NO-UNDO.
    DEFINE VARIABLE bOppkoblet AS LOG NO-UNDO.

    DEFINE VARIABLE rStandardFunksjoner AS cls.StdFunk.StandardFunksjoner NO-UNDO.
    DEFINE VARIABLE rLesSkrivBxEngine AS cls.BxLog.LesSkrivBxEngine NO-UNDO.
/*    DEFINE VARIABLE rPlukkliste AS cls.Plukking.Plukkliste NO-UNDO.*/
    
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    {cls\bxLog\tmpTblBxPickinglist.i}
    {cls\BxLog\tmpDsBxPickinglist.i}        
        
    CONSTRUCTOR PUBLIC SendHentPlukkliste ( INPUT pcLogg AS CHARACTER ):
        SUPER ().

        ASSIGN 
            cKatalog = 'konv\Plukklister'
            cLogg = pcLogg
            bTest = TRUE
            iMaksAntLister = 10 /* Maks antall plukklister som hentes pr. oppslag */
            cDatoTid = REPLACE(STRING(TODAY),'/','') + REPLACE(STRING(TIME,"HH:MM:SS"),':','')
            .

        rStandardFunksjoner  = NEW cls.StdFunk.StandardFunksjoner( cLogg ).
        rLesSkrivBxEngine = NEW cls.BxLog.LesSkrivBxEngine( INPUT cLogg ).
/*        rPlukkliste  = NEW cls.Plukking.Plukkliste( INPUT cLogg ).*/

        cSystemName = rStandardFunksjoner:SystemName.

        /* Oppretter loggkatalog hvis den ikke finnes */
        rStandardFunksjoner:opprettKatalog(cKatalog).
        

        CATCH zeroError AS Progress.Lang.AppError:
            DO ix = 1 TO zeroError:NumMessages.
                rStandardFunksjoner:SkrivTilLogg(cLogg,
                    '** ENDFeil: ' + zeroError:GetMessage(ix) 
                    ).
            END.
        END CATCH.
        CATCH oneError AS Progress.Lang.SysError:
            DO ix = 1 TO oneError:NumMessages.
                rStandardFunksjoner:SkrivTilLogg(cLogg,
                    '** ENDFeil: ' + oneError:GetMessage(ix) 
                    ).
            END.
        END CATCH.                
        CATCH twoError AS Progress.Lang.ProError:
            DO ix = 1 TO twoError:NumMessages.
                rStandardFunksjoner:SkrivTilLogg(cLogg,
                    '** ENDFeil: ' + twoError:GetMessage(ix) 
                    ).
            END.
        END CATCH.    
        
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    METHOD PUBLIC VOID kjorSendHent(  ):
        
        /* Kobler opp Sql server. */
        bOppkoblet = rLesSkrivBxEngine:oppkoblingSqlServer( ).

        rStandardFunksjoner:SkrivTilLogg(cLogg,
            '  SendHentPlukkliste Oppkoblet SqlServer: ' + STRING(bOppkoblet) + '.'
            ).

        /* Henter og posterer alle tilgjengelige plukklister som skal sendes til PDA for plukking. */
        IF bOppkoblet THEN 
        EVIGHETEN:
        DO WHILE TRUE:
            EMPTY TEMP-TABLE tmpBxPickinglist.
            EMPTY TEMP-TABLE tmpBxPickinglistline.

            /* Teller antall lister som blir hentet. */
            ASSIGN 
                iLoop = iLoop + 1
                bOk   = FALSE 
                .

            /* Laster opp plukklister i PRS som er flagget klar for overføring til tellepenn */
            /*            bOk = rPlukkliste:initPlukkliste( INPUT-OUTPUT DATASET dsBxPickinglist ).*/
            RUN cls\Plukking\initPlukklistePRSCl.p ( cLogg, INPUT-OUTPUT DATASET dsBxPickinglist ).
            IF NOT CAN-FIND(FIRST tmpBxPickinglist) THEN
            DO:
                rStandardFunksjoner:SkrivTilLogg(cLogg,
                    '  Ingen plukklister returnert fra rPlukkliste:initPlukkliste.'
                    ).
                LEAVE EVIGHETEN.
            END.
            
            IF bTest THEN 
            DO:
                cTekst = cKatalog + '\BxPickinglistHentedeListehoderPDA' + cDatoTid + '_' + STRING(iLoop) + '.JSon'.
                DATASET dsBxPickinglist:WRITE-JSON('file', cTekst, TRUE).
                rStandardFunksjoner:SkrivTilLogg(cLogg,
                    '  Dataset hentede listehoder skrevet til fil: ' + cTekst 
                    ).    
            END. 
            

            /* Posterer listene i BxEngine sql databasen til tellepennen. */
            bOk = rLesSkrivBxEngine:posterBxPickinglist( INPUT DATASET dsBxPickinglist ).
            IF NOT bOk THEN
            DO:
                rStandardFunksjoner:SkrivTilLogg(cLogg,
                    '  Feil returnert fra rLesSkrivBxEngine:posterBxPickinglist.'
                    ).
                LEAVE EVIGHETEN.
            END.
            ELSE 
            DO:
                /* Oppdater status på lister som er sendt i PRS. */
                /*                rPlukkliste:flaggSendPlukkliste( INPUT DATASET dsBxPickinglist ).*/
                RUN cls\Plukking\flaggSendPlukklistePRSCl.p ( cLogg, INPUT-OUTPUT DATASET dsBxPickinglist ).
                /*LEAVE EVIGHETEN.*/
            END.

            /* Sikrer at det ikke går i evig loop. */
            IF iLoop > 10 THEN 
                LEAVE EVIGHETEN.
        END. /* EVIGHETEN2 */

        IF bTest THEN 
        DO:
            cTekst = cKatalog + '\BxPickinglistInit' + cDatoTid + '_' + STRING(iLoop) + '.JSon'.
            DATASET dsBxPickinglist:WRITE-JSON('file', cTekst, TRUE).
            rStandardFunksjoner:SkrivTilLogg(cLogg,
                '  Dataset skrevet til fil: ' + cTekst 
                ).    
        END. 

        /* klargjør for telling av antall lister som hentes fra PDA. */
        ASSIGN 
            iLoop = 0
            .
        /* Henter alle tilgjengelige Plukklister fra PDA (Henter maks 10 for hvert kall). */
        IF bOppkoblet THEN 
        EVIGHETEN2:
        DO WHILE TRUE:
            /* Tømmer temptabeller. Klargjør for henting av plukkede lister fra PDA. */
            EMPTY TEMP-TABLE tmpBxPickinglist.
            EMPTY TEMP-TABLE tmpBxPickinglistline.

            /* Teller antall lister som blir hentet. */
            ASSIGN 
                iLoop = iLoop + 1
                bOk   = FALSE 
                .

            /* Leser plukkede, ikke overførte telleliste fra PDA inn i temp-tabell. */
            rLesSkrivBxEngine:lesBxPickinglist( INPUT iMaksAntLister, 
                INPUT-OUTPUT DATASET dsBxPickinglist ).        

            IF bTest THEN 
            DO:
                cTekst = cKatalog + '\BxPickinglistListerAHente' + cDatoTid + '_' + STRING(iLoop) + '.JSon'.
                DATASET dsBxPickinglist:WRITE-JSON('file', cTekst, TRUE).
                rStandardFunksjoner:SkrivTilLogg(cLogg,
                    '  Dataset lister å hente skrevet til fil: ' + cTekst 
                    ).    
            END. 
    
            /* Er returen tom (Ingen lister å hente fra PDA), avsluttes kjøringen. */
            IF NOT CAN-FIND(FIRST tmpBxPickinglist) THEN 
                LEAVE EVIGHETEN2.
            /* Ellers går vi videre og henter linjene i tellelistene, og flagger listene som hentet. */
            ELSE 
            HENT_LISTELINJER:
            DO:
                rStandardFunksjoner:SkrivTilLogg(cLogg,
                    '  HENT_LISTELINJER TEST-1 SendHentPlukkliste.cls' 
                    ).    

                /* Henter listelinjene ALLE mottatte ferdig plukkede lister. */
                rLesSkrivBxEngine:lesBxPickinglistline( INPUT-OUTPUT DATASET dsBxPickinglist ).

                rStandardFunksjoner:SkrivTilLogg(cLogg,
                    '  HENT_LISTELINJER TEST-2 SendHentPlukkliste.cls' 
                    ).    
                    
                /* Posterer mottatte tellelister og flagger at tellelisten er mottatt. */
                /*                rPlukkliste:posterPlukkliste( INPUT-OUTPUT DATASET dsBxPickinglist ).*/
                RUN cls\Plukking\posterPlukklistePRSCl.p ( cLogg, INPUT-OUTPUT DATASET dsBxPickinglist ).
                
                rStandardFunksjoner:SkrivTilLogg(cLogg,
                    '  HENT_LISTELINJER TEST-3 SendHentPlukkliste.cls' 
                    ).    
                    
                /* Flagger listene som er mottatt og postert i PRS som sendt. */
                rLesSkrivBxEngine:oppdBxPickingList( INPUT-OUTPUT DATASET dsBxPickinglist ).

                rStandardFunksjoner:SkrivTilLogg(cLogg,
                    '  HENT_LISTELINJER TEST-4 SendHentPlukkliste.cls' 
                    ).    
            END. /* HENT_LISTELINJER */

            IF bTest THEN 
            DO:
                cTekst = cKatalog + '\BxPickinglistLesPlukkede' + cDatoTid + '_' + STRING(iLoop) + '.JSon'.
                DATASET dsBxPickinglist:WRITE-JSON('file', cTekst, TRUE).
                rStandardFunksjoner:SkrivTilLogg(cLogg,
                    '  Dataset for ferdig hentede plukklister skrevet til fil: ' + cTekst 
                    ).    
            END. 
            
            /* Sikrer at det ikke går i evig loop. */
            IF iLoop > 10 THEN 
                LEAVE EVIGHETEN2.
        END. /* EVIGHETEN2 */

        /* Tømmer temptabeller. Klargjør for henting av plukkede lister fra PDA. */
        EMPTY TEMP-TABLE tmpBxPickinglist.
        EMPTY TEMP-TABLE tmpBxPickinglistline.

        /* Kobler ned SQL server. */
        bOk = rLesSkrivBxEngine:nedkoblingSqlServer(  ).
        IF NOT bOk THEN 
        DO:
            rStandardFunksjoner:SkrivTilLogg(cLogg,
                '  ** Feil ved nedkobling av Sql Server.' 
                ).    
            RETURN.
        END.
        ELSE RETURN.

        CATCH zeroError AS Progress.Lang.AppError:
            DO ix = 1 TO zeroError:NumMessages.
                rStandardFunksjoner:SkrivTilLogg(cLogg,
                    '** ENDFeil: ' + zeroError:GetMessage(ix) 
                    ).
            END.
        END CATCH.
        CATCH oneError AS Progress.Lang.SysError:
            DO ix = 1 TO oneError:NumMessages.
                rStandardFunksjoner:SkrivTilLogg(cLogg,
                    '** ENDFeil: ' + oneError:GetMessage(ix) 
                    ).
            END.
        END CATCH.                
        CATCH twoError AS Progress.Lang.ProError:
            DO ix = 1 TO twoError:NumMessages.
                rStandardFunksjoner:SkrivTilLogg(cLogg,
                    '** ENDFeil: ' + twoError:GetMessage(ix) 
                    ).
            END.
        END CATCH.    
    END METHOD.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	DESTRUCTOR PUBLIC SendHentPlukkliste ( ):

	END DESTRUCTOR.

END CLASS.