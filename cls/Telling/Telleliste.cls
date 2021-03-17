 
 /*------------------------------------------------------------------------
    File        : posterTelleliste
    Purpose     : Denne klassen skal innholde alle funksjoner som skal kunne gjøres mot en telleliste som kommer fra hånterminalen.
    Syntax      : 
    Description : Mottar en temp-tabell som inneholder en telleliste, og posterer denne i SkoTex databasen hvis den ikke finnes der fra før.
    Author(s)   : tny
    Created     : Sun Dec 31 12:25:58 CET 2017
    
    Notes       : Forutsetninger 
                  * Forutsetter at EAN er korrect. Den kommer fra db i utgangspunktet og er ikke endret.
                  * Forutsetter at størrelsen (Stor og StrKode) er korrekt. Den kommer fra db i utgangspunktet og er ikke endret.
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.


CLASS cls.Telling.Telleliste: 
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE cLogg AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cSystem AS CHARACTER NO-UNDO.
    DEFINE VARIABLE ocReturn AS CHARACTER NO-UNDO.
    DEFINE VARIABLE obOk AS LOG NO-UNDO.
    DEFINE VARIABLE bOppdaterDirekte AS LOG NO-UNDO.
    DEFINE VARIABLE cTekst AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE rStandardFunksjoner AS cls.StdFunk.StandardFunksjoner NO-UNDO.
        
    {cls\bxLog\tmpTblStockCount.i}
    {cls\BxLog\tmpDsStockCount.i}        
        
    CONSTRUCTOR PUBLIC Telleliste ( INPUT pcLogg AS CHARACTER ):
        SUPER ().

        ASSIGN 
            cLogg = pcLogg
            .

        rStandardFunksjoner  = NEW cls.StdFunk.StandardFunksjoner( cLogg ) NO-ERROR.
        rStandardFunksjoner:hentSystem(INPUT-OUTPUT cSystem).

        {syspara.i 4 1 4 cTekst}
        IF CAN-DO('1,Ja,J,Yes,True',cTekst) THEN 
            bOppdaterDirekte = TRUE.

        rStandardFunksjoner:SkrivTilLogg(cLogg,
            '    Oppsett direkte oppdatering: ' + cTekst + '.' 
            ).    

        
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    METHOD PUBLIC LOGICAL posterTelleliste( INPUT-OUTPUT DATASET dsStockCount ):
        
        DEFINE VARIABLE bResult   AS LOGICAL NO-UNDO.
        DEFINE VARIABLE piTelleNr AS INTEGER NO-UNDO.
        DEFINE VARIABLE pcTekst   AS CHARACTER NO-UNDO.
        DEFINE VARIABLE plVVarekost AS DECIMAL FORMAT "->>>,>>>,>>>,>>9.99"NO-UNDO.
        
        IF NOT CAN-FIND(FIRST tmpStockCount) THEN 
        DO:
            rStandardFunksjoner:SkrivTilLogg(cLogg,
                '    posterTelleliste: Ingen telleliste mottatt. Funksjon avsluttet.' 
                ).    
            bResult = FALSE.
        END.
        ELSE DO:
            FOR EACH tmpStockCount:
                rStandardFunksjoner:SkrivTilLogg(cLogg,
                    '    posterTelleliste: ' + tmpStockCount.Name + '.' 
                    ).    
                
                POSTER_TELLEHODE:
                DO TRANSACTION ON ERROR UNDO, LEAVE:
                    FIND LAST TelleHode NO-LOCK USE-INDEX TelleHode NO-ERROR.
                    IF AVAILABLE TelleHode THEN 
                        piTelleNr = TelleHode.TelleNr + 1.
                    ELSE
                        piTelleNr = 1.
                        
                    pcTekst = ENTRY(2,STRING(tmpStockCount.Received),' ').
                    pcTekst = ENTRY(1,pcTekst,',').
                        
                    CREATE TelleHode.
                    ASSIGN 
                        TelleHode.TelleNr     = piTelleNr
                        TelleHode.ButikkListe = tmpStockCount.cCompany
                        TelleHode.TTId        = 9 
                        TelleHode.TBId        = 1
                        TelleHode.Notat       = 'Fra håndterminal ' + STRING(TODAY) + ' ' + STRING(TIME,"HH:MM:SS") + '.'
                        TelleHode.StartDato   = TODAY /*tmpPDA.Dato*/
                        TelleHode.Beskrivelse = tmpStockCount.Name
                        TelleHode.TelleType   = IF bOppdaterDirekte THEN 1 /* Telling */ ELSE 2 /* Lokasjonsliste */
                        TelleHode.LokasjonsId = ''
                        TelleHode.BrukerIdPDA = tmpStockCount.cUser
                        TelleHode.FilDatoPDA  = DATE(tmpStockCount.Received)
                        .
                    ASSIGN 
                        TelleHode.FilTidPDA   = INT(ENTRY(1,pcTekst,':')) * 3600 + 
                                                INT(ENTRY(2,pcTekst,':')) * 60 +
                                                INT(ENTRY(3,pcTekst,':'))
                        NO-ERROR.
                    FIND CURRENT Tellehode NO-LOCK.
                END. /* POSTER_TELLEHODE */
                
                IF NOT ERROR-STATUS:ERROR THEN  
                POSTER_TELLELINJE:
                DO ON ERROR UNDO, LEAVE:
                    LINJE:
                    FOR EACH tmpStockCountLine WHERE 
                        tmpStockCountLine.cStockCount = tmpStockCount.cId:
                    
                        FIND Strekkode NO-LOCK WHERE 
                            Strekkode.Kode = tmpStockCountline.cProductno NO-ERROR.  
                        FIND ArtBas NO-LOCK WHERE 
                            ArtBas.ArtikkelNr = Strekkode.ArtikkelNr NO-ERROR.
                        IF NOT AVAILABLE ArtBas THEN
                        DO: 
                            rStandardFunksjoner:SkrivTilLogg(cLogg,
                                '    ** Ukjent EAN i tellefil: ' + tmpStockCount.Name + ' GTIN: ' + tmpStockCountLine.cGTIN + '.' 
                                ).    
                            NEXT LINJE.
                        END.

                        FIND Farg OF ArtBas NO-LOCK NO-ERROR.
              
                        /* Setter VVareKost. */
                        FIND Lager NO-LOCK WHERE Lager.ArtikkelNr = ArtBas.ArtikkelNr AND
                            Lager.Butik      = INT(tmpStockCount.cCompany) NO-ERROR.
                        /* Setter varekost */
                        IF AVAILABLE Lager THEN
                            plVVareKost = Lager.VVareKost.
                        ELSE   
                            plVVareKost = 0.

                        IF (plVVareKost = 0 OR plVVareKost = ? OR ArtBas.Lager = FALSE) THEN 
                        DO:
                            FIND Butiker NO-LOCK WHERE 
                                Butiker.Butik = INT(tmpStockCount.cCompany) NO-ERROR.
                            FIND ArtPris NO-LOCK WHERE
                                ArtPris.ArtikkelNr = ArtBas.ArtikkelNr AND
                                ArtPris.ProfilNr   = Butiker.ProfilNr NO-ERROR.
                            IF AVAILABLE ArtPris THEN
                                plVVareKost = ArtPris.VareKost[1]. /* Tar alltid normalpris */
                        END.

                        /* Ukjent varekost */
                        IF plVVareKost = ? THEN
                            plVVareKost = 0.              
                  
                        FIND ArtLag NO-LOCK WHERE
                            ArtLag.ArtikkelNr = ArtBas.ArtikkelNr AND 
                            ArtLag.Storl      = tmpStockCountLine.cUnitName AND 
                            ArtLag.Butik      = INT(tmpStockCount.cCompany) NO-ERROR.           
                  
                        FIND FIRST TelleLinje EXCLUSIVE-LOCK WHERE 
                            TelleLinje.TelleNr    = TelleHode.TelleNr AND        
                            TelleLinje.ArtikkelNr = ArtBas.ArtikkelNr AND
                            TelleLinje.Butik      = INT(tmpStockCount.cCompany) AND
                            TelleLinje.Storl      = tmpStockCountLine.cUnitName NO-ERROR.

                        IF NOT AVAILABLE TelleLinje THEN 
                        OPPSTANDELSEN:
                        DO:
                            CREATE TelleLinje.

                            /* Setter index. */
                            ASSIGN
                                TelleLinje.TelleNr    = TelleHode.TelleNr 
                                TelleLinje.ArtikkelNr = ArtBas.ArtikkelNr
                                TelleLinje.Butik      = INT(tmpStockCount.cCompany)
                                TelleLinje.Storl      = tmpStockCountLine.cUnitName
                                Tellelinje.Beskr      = ArtBas.Beskr
                                TelleLinje.LevKod     = ArtBas.LevKod
                                TelleLinje.Vg         = ArtBas.Vg
                                TelleLinje.LopNr      = ArtBas.LopNr
                                TelleLinje.LevFargKod = IF ArtBas.LevFargKod <> "" THEN ArtBas.LevFargKod ELSE IF AVAIL Farg THEN Farg.Farbeskr ELSE ""
                                TelleLinje.AntallPar  = IF AVAILABLE ArtLag THEN ArtLag.Lagant ELSE 0
                                TelleLinje.OpprVerdi  = TelleLinje.AntallPar * plVVareKost
                                TelleLinje.VVareKost  = plVVAreKost
                                TelleLinje.LevNr      = ArtBas.LevNr
                                TelleLinje.Sasong     = ArtBas.SaSong
                                TelleLinje.Farg       = ArtBas.Farg
                                TelleLinje.MatKod     = ArtBas.MatKod
                                TelleLinje.VgLopNr    = TRIM(STRING(ArtBas.Vg,">>>>>>9")) + "/" + TRIM(STRING(ArtBas.LopNr,">>>>>>9"))
                                TelleLinje.Kode       = tmpStockCountLine.cGTIN /*Strekkode.Kode*/
                                TelleLinje.NedSkrevet = plVVAreKost
                                .
                        END. /* OPPSTANDELSEN */

                        ASSIGN
                            TelleLinje.OpprAntalTalt = TelleLinje.OpprAntalTalt + DEC(tmpStockCountLine.cQuantity)
                            TelleLinje.AntallTalt    = TelleLinje.AntallTalt    + DEC(tmpStockCountLine.cQuantity) 
                            TelleLinje.OpptVerdi     = TelleLinje.AntallTalt * plVVarekost
                            TelleLinje.AntallDiff    = TelleLinje.AntallPar - TelleLinje.AntallTalt
                            TelleLinje.VerdiDiff     = TelleLinje.AntallDiff * TelleLinje.VVareKost
                            .                          
                        RELEASE TelleLinje. /* Denne MÅ få stå */            
                            
                    END. /* LINJE */
                    
                END. /* POSTER_TELLELINJE */

                ASSIGN 
                    tmpStockCount.cStatus = 'Sendt'
                    bResult               = TRUE 
                    .
                summerTelling( INPUT TelleHode.TelleNr ).  
                
                IF bOppdaterDirekte THEN 
                    RUN oppdaterTelling.p (INPUT TelleHode.TelleNr, ?).
            END.
        END.    

        RETURN bResult.

    END METHOD.

    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    METHOD PUBLIC LOGICAL summerTelling( INPUT piTelleNr AS INTEGER  ):
        
        DEFINE VARIABLE bResult AS LOGICAL NO-UNDO.

        DEF VAR piAntallPar     AS DEC  NO-UNDO.
        DEF VAR piAntallTalt    AS DEC  NO-UNDO.
        DEF VAR piOpptVerdi     AS DEC  NO-UNDO.
        DEF VAR piVerdiDiff     AS DEC  NO-UNDO.
        DEF VAR piAntallDiff    AS DEC  NO-UNDO.
        DEF VAR piOpprVerdi     AS DEC  NO-UNDO.
        DEF VAR piAntLinjer     AS DEC  NO-UNDO.
        DEF VAR plOppdatert     AS DATE NO-UNDO.
        DEF VAR piAntallNegDiff AS DEC  NO-UNDO.
        DEF VAR piAntallPosDiff AS DEC  NO-UNDO.
        DEF VAR piVerdiNegDiff  AS DEC  NO-UNDO.
        DEF VAR piVerdiPosDiff  AS DEC  NO-UNDO.

        FIND TelleHode NO-LOCK WHERE TelleHode.TelleNr = piTelleNr NO-ERROR.

        IF AVAIL TelleHode THEN
        DO:
            ASSIGN
                piAntallPar     = 0
                piAntallTalt    = 0
                piOpptVerdi     = 0
                piVerdiDiff     = 0
                piAntallDiff    = 0
                piOpprVerdi     = 0
                piAntLinjer     = 0
                piAntallNegDiff = 0
                piAntallPosDiff = 0
                piVerdiNegDiff  = 0
                piVerdiPosDiff  = 0
                .

            /* Sumerer opp linjene. */
            FOR EACH TelleLinje OF TelleHode NO-LOCK:
                ASSIGN
                    piAntallPar  = piAntallPar  + TelleLinje.AntallPar
                    piAntallTalt = piAntallTalt + TelleLinje.AntallTalt
                    piOpprVerdi  = piOpprVerdi  + TelleLinje.OpprVerdi      
                    piOpptVerdi  = piOpptVerdi  + TelleLinje.OpptVerdi
                    piAntLinjer  = piAntLinjer  + 1      
                    .
                /* Sumerer positive differanser. */
                IF (TelleLinje.AntallPar - TelleLinje.AntallTalt) < 0 THEN
                    ASSIGN
                        piAntallNegDiff = piAntallNegDiff + (TelleLinje.AntallPar - TelleLinje.AntallTalt)
                        piVerdiNegDiff  = piVerdiNegDiff  + (TelleLinje.OpprVerdi - TelleLinje.OpptVerdi).
                /* Sumerer negative differanser. */
                IF (TelleLinje.AntallPar - TelleLinje.AntallTalt) > 0 THEN
                    ASSIGN
                        piAntallPosDiff = piAntallPosDiff + (TelleLinje.AntallPar - TelleLinje.AntallTalt)
                        piVerdiPosDiff  = piVerdiPosDiff  + (TelleLinje.OpprVerdi - TelleLinje.OpptVerdi).
            END.
            /* Differanser */
            ASSIGN
                piVerdiDiff  = piOpprVerdi  - piOpptVerdi
                piAntallDiff = piAntallPar  - piAntallTalt
                .      

            DO TRANSACTION:
                FIND CURRENT TelleHode EXCLUSIVE-LOCK NO-ERROR.
                IF AVAIL TelleHode THEN
                DO:
                    ASSIGN
                        TelleHode.AntallPar  = piAntallPar  
                        TelleHode.AntallTalt = piAntallTalt 
                        TelleHode.OpptVerdi  = piOpptVerdi  
                        TelleHode.VerdiDiff  = piVerdiDiff  
                        TelleHode.AntallDiff = piAntallDiff 
                        TelleHode.OpprVerdi  = piOpprVerdi  
                        TelleHode.AntLinjer  = piAntLinjer.
                    /* Skal ikke sumeres for lokasjonslister. */
                    IF TelleHode.TelleType = 1 THEN
                        ASSIGN
                            TelleHode.AntallNegDiff = piAntallNegDiff
                            TelleHode.AntallPosDiff = piAntallPosDiff
                            TelleHode.VerdiNegDiff  = piVerdiNegDiff
                            TelleHode.VerdiPosDiff  = piVerdiPosdiff
                            .
                END.
            END. /*Transaction*/
            RELEASE TelleHode.
        END.

        RETURN bResult.

    END METHOD.

    DESTRUCTOR PUBLIC Telleliste ( ):

    END DESTRUCTOR.

END CLASS.