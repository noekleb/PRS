
/*------------------------------------------------------------------------
    File        : LCPrisImport.p
    Purpose     : Få satt inn LC så hurtig og korrekt som mulig i PRS.

    Syntax      :

    Description : Import av Landed cost (LC) fra Gant Global    

    Author(s)   : Tom Nøkleby
    Created     : Tue Jul 17 10:08:20 CEST 2018
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE VARIABLE cImpFil AS CHARACTER NO-UNDO.
DEFINE VARIABLE cBkufil AS CHARACTER NO-UNDO.
DEFINE VARIABLE cImpKatalog AS CHARACTER NO-UNDO.
DEFINE VARIABLE cBkuKatalog AS CHARACTER NO-UNDO.
DEFINE VARIABLE cTekst AS CHARACTER NO-UNDO.
DEFINE VARIABLE cLogg AS CHARACTER NO-UNDO.
DEFINE VARIABLE cTimeStamp AS CHARACTER NO-UNDO.
DEFINE VARIABLE bOk AS LOG INITIAL FALSE NO-UNDO.
DEFINE VARIABLE iAntLinjer AS INTEGER NO-UNDO.
DEFINE VARIABLE iAntLC AS INTEGER NO-UNDO.
DEFINE VARIABLE iAntPris AS INTEGER NO-UNDO.
DEFINE VARIABLE iProfilNr AS INTEGER NO-UNDO.
DEFINE VARIABLE iEndringsNr AS INTEGER NO-UNDO.

DEFINE TEMP-TABLE ttLcPriser
    FIELD cArtKey AS CHARACTER FORMAT "x(20)"
    FIELD cArtNo AS CHARACTER FORMAT "x(20)"
    FIELD cColCode AS CHARACTER FORMAT "x(20)"
    FIELD iSeason AS INTEGER FORMAT ">>>>>>>>9"
    FIELD cBRAND AS CHAR FORMAT "x(10)"
    FIELD lRetailPrice AS DECIMAL FORMAT "->>>,>>>,>99.99"
    FIELD lWholeSaleNet AS DECIMAL FORMAT "->>>,>>>,>99.99"    
    FIELD lLC AS DECIMAL FORMAT "->>>,>>>,>99.99"    
    INDEX idxArtKey AS PRIMARY cArtKey.

DEFINE STREAM Inn.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
IF SEARCH('tnc.txt') <> ? THEN  
    ASSIGN 
        cImpKatalog = 'kom\in\Gant\'
        cBkuKatalog = 'kom\in\Gant\bku\'
        .
ELSE         
    ASSIGN
        cImpKatalog = '\\norge0047\PackListExport\'
        cBkuKatalog = '\\norge0047\PackListExport\bku\'
        .
ASSIGN
    cTimeStamp = REPLACE(STRING(TODAY),'/','') + '_' + REPLACE(STRING(TIME,"HH:MM:SS"),':','')
    cLogg      = 'LCPriser' + REPLACE(STRING(TODAY),'/','') 
    cImpfil    = 'LCPriser.csv'
    cBkufil    = REPLACE(cImpfil,'.csv',cTimeStamp + '.csv')
    iProfilNr  = 1 
    .

RUN bibl_loggDbFri.p (cLogg, 'Start.'). 

IF SEARCH(cImpKatalog + cImpfil) <> ? THEN 
    RUN importerfil. 
ELSE DO:
    RUN bibl_loggDbFri.p (cLogg, '  Ingen fil å importere(' + cImpKatalog + cImpfil + ')'). 
END.

IF bOk THEN 
    RUN bkuAvfil.

RUN bibl_loggDbFri.p (cLogg, 'Ferdig.'). 


/* **********************  Internal Procedures  *********************** */

PROCEDURE importerfil:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE BUFFER bufArtBas  FOR ArtBas.
    DEFINE BUFFER bufArtPris FOR ArtPris.

    IF SEARCH(cImpKatalog + cImpfil) = ? THEN
        RETURN.

    RUN bibl_loggDbFri.p (cLogg, '  Importerer fil ' + cImpKatalog + cImpfil + '.'). 
    
    INPUT STREAM Inn FROM VALUE(cImpKatalog + cImpfil).
    IMPLOOP:
    REPEAT:
        iAntLinjer = iAntLinjer + 1.
        CREATE ttLCPriser.
        IMPORT STREAM Inn DELIMITER ',' 
            ttLCPriser
            NO-ERROR.
        /* Overskriftsrad */
        IF ERROR-STATUS:ERROR THEN 
        DO:
            IF AVAILABLE ttLCPriser THEN 
                DELETE ttLCPriser.
            NEXT IMPLOOP.
        END.
        /* Tomme record. */
        IF ttLcPriser.cArtKey = '' THEN 
        DO:
            IF AVAILABLE ttLCPriser THEN 
                DELETE ttLCPriser.
            NEXT IMPLOOP.
        END.
    
        /* Korrigerer sesong */
        IF LENGTH(STRING(ttLcPriser.iSeason)) = 6 THEN 
            ttLcPriser.iSeason = INT(SUBSTRING(STRING(ttLcPriser.iSeason),4)).
        
        /* Avrunder priser til hele kroner. */
        ASSIGN 
            ttLcPriser.lRetailPrice = ROUND(ttLcPriser.lRetailPrice,0)
            ttLcPriser.lLC          = ROUND(ttLcPriser.lLC,0)
            .
        
/*        /* TEST TEST */ IF iAntPris < 10 THEN*/
/*        /* TEST      */                      */
        ARTLOOP:
        FOR EACH ArtBas NO-LOCK WHERE 
            ArtBas.LevKod     = ttLcPriser.cArtNo AND 
            /*ArtBas.LevFargKod = ttLcPriser.cColCode AND*/ 
            ArtBas.Sasong     = ttLcPriser.iSeason /*AND 
            ArtBas.RegistrertDato > TODAY - (365 * 10)*/:
                
            /* Oppdaterer LC */ 
            IF ttLcPriser.lLC > 0 AND ttLcPriser.lLC <> ArtBas.KjedeInnkPris THEN 
            ENDRE_LC:
            DO:
                iAntLC = iAntLC + 1.
                DO FOR bufArtBas TRANSACTION:
                    FIND bufArtBas EXCLUSIVE-LOCK WHERE
                        RECID(bufArtBas) = RECID(ArtBas) NO-ERROR NO-WAIT.
                    IF AVAILABLE bufArtBas THEN 
                    DO:
                        RUN bibl_loggDbFri.p (cLogg, '  ENDRET LC på artikkel: ' + 
                                              ttLcPriser.cArtNo + ' / ' +  
                                              ttLcPriser.cColCode + ' / ' +
                                              STRING(ttLcPriser.iSeason) + ' Fra: ' +
                                              STRING(bufartBas.KjedeInnkPris) + ' til: ' + 
                                              STRING(ttLcPriser.lLC) + '.'). 
                        
                        ASSIGN 
                            bufartBas.KjedeInnkPris = ttLcPriser.lLC
                            . 
                        RELEASE bufArtBas.
                    END. 
                END.
            END. /* ENDRE_LC */    

            /* Oppdaterer FØR Pris (AnbefaltPris) */
            IF ttLcPriser.lRetailPrice > 0 AND ttLcPriser.lRetailPrice > ArtBas.AnbefaltPris THEN 
            ENDRE_LC:
            DO:
                iAntLC = iAntLC + 1.
                DO FOR bufArtBas TRANSACTION:
                    FIND bufArtBas EXCLUSIVE-LOCK WHERE
                        RECID(bufArtBas) = RECID(ArtBas) NO-ERROR NO-WAIT.
                    IF AVAILABLE bufArtBas THEN 
                    DO:
                        RUN bibl_loggDbFri.p (cLogg, '  ENDRET AnbefaltPris på artikkel: ' + 
                            ttLcPriser.cArtNo + ' / ' +  
                            ttLcPriser.cColCode + ' / ' +
                            STRING(ttLcPriser.iSeason) + ' Fra: ' +
                            STRING(bufartBas.AnbefaltPris) + ' til: ' + 
                            STRING(ttLcPriser.lRetailPrice) + '.'). 
                        
                        ASSIGN 
                            bufartBas.AnbefaltPris = ttLcPriser.lRetailPrice
                            . 
                        RELEASE bufArtBas.
                    END. 
                END.
            END. /* ENDRE_LC */    

            FIND FIRST ArtPris OF ArtBas NO-LOCK WHERE
                ArtPris.ProfilNr = iProfilNr NO-ERROR. 
               
/*            /* Oppdaterer Pris */                                                                                                */
/*            IF AVAILABLE ArtPris AND ttLcPriser.lRetailPrice > 0 AND ttLcPriser.lRetailPrice > ArtPris.Pris[1] THEN              */
/*            ENDRE_PRIS:                                                                                                          */
/*            DO:                                                                                                                  */
/*                iAntPris = iAntPris + 1.                                                                                         */
/*                                                                                                                                 */
/*                DO FOR bufArtPris TRANSACTION:                                                                                   */
/*                    FIND bufArtPris EXCLUSIVE-LOCK WHERE                                                                         */
/*                        RECID(bufArtPris) = RECID(ArtPris) NO-ERROR NO-WAIT.                                                     */
/*                    IF AVAILABLE bufArtPris THEN                                                                                 */
/*                    DO:                                                                                                          */
/*                        RUN bibl_loggDbFri.p (cLogg, '  ENDRET Pris på artikkel: ' +                                             */
/*                            ttLcPriser.cArtNo + ' / ' +                                                                          */
/*                            ttLcPriser.cColCode + ' / ' +                                                                        */
/*                            STRING(ttLcPriser.iSeason) + ' Fra: ' +                                                              */
/*                            STRING(bufArtPris.Pris[1]) + ' til: ' +                                                              */
/*                            STRING(ttLcPriser.lRetailPrice) + '.').                                                              */
/*                                                                                                                                 */
/*                        FIND FIRST HPrisKo NO-LOCK WHERE                                                                         */
/*                            HPrisKo.ArtikkelNr = ArtBas.ArtikkelNr AND                                                           */
/*                            HPrisKo.ProfilNr   = bufArtPris.ProfilNr NO-ERROR.                                                   */
/*                        IF AVAILABLE HPrisKo THEN                                                                                */
/*                            iEndringsNr = HPrisKo.EndringsNr + 1.                                                                */
/*                        ELSE                                                                                                     */
/*                            iEndringsNr = 1.                                                                                     */
/*                                                                                                                                 */
/*                        /* Ligger det ikke poster i hprisko, legges også den gamle prisen opp. */                                */
/*                        IF iendringsNr = 1 THEN                                                                                  */
/*                        DO:                                                                                                      */
/*                            /* Logger endret pris i priskø. */                                                                   */
/*                            CREATE HPrisKo.                                                                                      */
/*                            ASSIGN                                                                                               */
/*                                HPrisKo.ArtikkelNr = ArtBas.ArtikkelNr                                                           */
/*                                HPrisKo.ProfilNr   = bufArtPris.ProfilNr                                                         */
/*                                HPrisKo.EndringsNr = iEndringsNr.                                                                */
/*                            ASSIGN                                                                                               */
/*                                HPrisKo.LevNr        = ArtBas.LevNr                                                              */
/*                                HPrisKo.ValPris      = bufArtPris.ValPris[1]                                                     */
/*                                HPrisKo.InnkjopsPris = bufArtPris.InnKjopsPris[1]                                                */
/*                                HPrisKo.Rab1Kr       = bufArtPris.Rab1Kr[1]                                                      */
/*                                HPrisKo.Rab1%        = bufArtPris.Rab1%[1]                                                       */
/*                                HPrisKo.Rab2Kr       = bufArtPris.Rab2Kr[1]                                                      */
/*                                HPrisKo.Rab2%        = bufArtPris.Rab2%[1]                                                       */
/*                                HPrisKo.Frakt        = bufArtPris.Frakt[1]                                                       */
/*                                HPrisKo.Frakt%       = bufArtPris.Frakt%[1]                                                      */
/*                                HPrisKo.DivKostKr    = bufArtPris.DivKostKr[1]                                                   */
/*                                HPrisKo.DivKost%     = bufArtPris.DivKost%[1]                                                    */
/*                                HPrisKo.Rab3Kr       = bufArtPris.Rab3Kr[1]                                                      */
/*                                HPrisKo.Rab3%        = bufArtPris.Rab3%[1]                                                       */
/*                                HPrisKo.DBKr         = bufArtPris.DBKr[1]                                                        */
/*                                HPrisKo.DB%          = bufArtPris.DB%[1]                                                         */
/*                                HPrisKo.Pris         = bufArtPris.Pris[1]                                                        */
/*                                HPrisKo.EuroPris     = bufArtPris.EuroPris[1]                                                    */
/*                                HPrisKo.EuroManuel   = bufArtPris.EuroManuel.                                                    */
/*                                                                                                                                 */
/*                            ASSIGN                                                                                               */
/*                                HPrisKo.Tilbud         = FALSE                                                                   */
/*                                HPrisKo.AktiveresDato  = bufArtPris.AktivFraDato                                                 */
/*                                HPrisKo.GyldigTilDato  = ?                                                                       */
/*                                HPrisKo.AktiveresTid   = bufArtPris.AktivFraTid                                                  */
/*                                HPrisKo.GyldigTilTid   = 0                                                                       */
/*                                HPrisKo.Timestyrt      = bufArtPris.TilbudTimeStyrt                                              */
/*                                                                                                                                 */
/*                                HPrisKo.Aktivert       = TRUE                                                                    */
/*                                HPrisKo.Type           = 1                                                                       */
/*                                HPrisKo.VareKost       = bufArtPris.VareKost[1]                                                  */
/*                                HPrisKo.MvaKr          = bufArtPris.MvaKr[1]                                                     */
/*                                HPrisKo.Mva%           = bufArtPris.Mva%[1]                                                      */
/*                                                                                                                                 */
/*                                HPrisKo.EDato          = TODAY                                                                   */
/*                                HPrisKo.ETid           = TIME                                                                    */
/*                                HPrisKo.BrukerID       = USERID("dictdb")                                                        */
/*                                                                                                                                 */
/*                                HPrisKo.RegistrertDato = TODAY                                                                   */
/*                                HPrisKo.RegistrertTid  = TIME                                                                    */
/*                                HPrisKo.RegistrertAv   = USERID("dictdb").                                                       */
/*                                                                                                                                 */
/*                            iendringsNr = 2.                                                                                     */
/*                        END.                                                                                                     */
/*                                                                                                                                 */
/*                        ASSIGN                                                                                                   */
/*                            bufArtPris.Pris[1]  = ttLcPriser.lRetailPrice                                                        */
/*                            bufArtPris.MvaKr[1] = bufArtPris.Pris[1] * (bufArtPris.Mva%[1] / (100 + bufArtPris.Mva%[1]))         */
/*                            bufArtPris.DbKr[1]  = bufArtPris.Pris[1] - bufArtPris.MvaKr[1] - bufArtPris.Varekost[1]              */
/*                            bufArtPris.Db%[1]   = ROUND((bufArtPris.DbKr[1] * 100)/ (bufArtPris.Pris[1] - bufArtPris.MvaKr[1]),2)*/
/*                            bufArtPris.Db%[1]   = IF bufArtPris.Db%[1] = ? THEN 0 ELSE bufArtPris.Db%[1]                         */
/*                            .                                                                                                    */
/*                                                                                                                                 */
/*                        /* Logger endret pris i priskø. */                                                                       */
/*                        CREATE HPrisKo.                                                                                          */
/*                        ASSIGN                                                                                                   */
/*                            HPrisKo.ArtikkelNr = ArtBas.ArtikkelNr                                                               */
/*                            HPrisKo.ProfilNr   = bufArtPris.ProfilNr                                                             */
/*                            HPrisKo.EndringsNr = iEndringsNr.                                                                    */
/*                        ASSIGN                                                                                                   */
/*                            HPrisKo.LevNr        = ArtBas.LevNr                                                                  */
/*                            HPrisKo.ValPris      = bufArtPris.ValPris[1]                                                         */
/*                            HPrisKo.InnkjopsPris = bufArtPris.InnKjopsPris[1]                                                    */
/*                            HPrisKo.Rab1Kr       = bufArtPris.Rab1Kr[1]                                                          */
/*                            HPrisKo.Rab1%        = bufArtPris.Rab1%[1]                                                           */
/*                            HPrisKo.Rab2Kr       = bufArtPris.Rab2Kr[1]                                                          */
/*                            HPrisKo.Rab2%        = bufArtPris.Rab2%[1]                                                           */
/*                            HPrisKo.Frakt        = bufArtPris.Frakt[1]                                                           */
/*                            HPrisKo.Frakt%       = bufArtPris.Frakt%[1]                                                          */
/*                            HPrisKo.DivKostKr    = bufArtPris.DivKostKr[1]                                                       */
/*                            HPrisKo.DivKost%     = bufArtPris.DivKost%[1]                                                        */
/*                            HPrisKo.Rab3Kr       = bufArtPris.Rab3Kr[1]                                                          */
/*                            HPrisKo.Rab3%        = bufArtPris.Rab3%[1]                                                           */
/*                            HPrisKo.DBKr         = bufArtPris.DBKr[1]                                                            */
/*                            HPrisKo.DB%          = bufArtPris.DB%[1]                                                             */
/*                            HPrisKo.Pris         = bufArtPris.Pris[1]                                                            */
/*                            HPrisKo.EuroPris     = bufArtPris.EuroPris[1]                                                        */
/*                            HPrisKo.EuroManuel   = bufArtPris.EuroManuel.                                                        */
/*                                                                                                                                 */
/*                        ASSIGN                                                                                                   */
/*                            HPrisKo.Tilbud         = FALSE                                                                       */
/*                            HPrisKo.AktiveresDato  = TODAY                                                                       */
/*                            HPrisKo.GyldigTilDato  =  ?                                                                          */
/*                            HPrisKo.AktiveresTid   = TIME                                                                        */
/*                            HPrisKo.GyldigTilTid   = 0                                                                           */
/*                            HPrisKo.Timestyrt      = bufArtPris.TilbudTimeStyrt                                                  */
/*                                                                                                                                 */
/*                            HPrisKo.Aktivert       = TRUE                                                                        */
/*                            HPrisKo.Type           = 1                                                                           */
/*                            HPrisKo.VareKost       = bufArtPris.VareKost[1]                                                      */
/*                            HPrisKo.MvaKr          = bufArtPris.MvaKr[1]                                                         */
/*                            HPrisKo.Mva%           = bufArtPris.Mva%[1]                                                          */
/*                                                                                                                                 */
/*                            HPrisKo.EDato          = TODAY                                                                       */
/*                            HPrisKo.ETid           = TIME                                                                        */
/*                            HPrisKo.BrukerID       = USERID("dictdb")                                                            */
/*                                                                                                                                 */
/*                            HPrisKo.RegistrertDato = TODAY                                                                       */
/*                            HPrisKo.RegistrertTid  = TIME                                                                        */
/*                            HPrisKo.RegistrertAv   = USERID("dictdb").                                                           */
/*                                                                                                                                 */
/*                        IF AVAILABLE HPrisKo THEN                                                                                */
/*                            RELEASE HPrisKo.                                                                                     */
/*                        RELEASE bufArtPris.                                                                                      */
/*                    END.                                                                                                         */
/*                END.                                                                                                             */
/*            END. /* ENDRE_PRIS */                                                                                                */
                
        END. /* ARTLOOP*/
    END. /* IMPLOOP*/
    INPUT STREAM Inn CLOSE.
    
    bOk = TRUE.

    RUN bibl_loggDbFri.p (cLogg, '  Antall linjer i fil  : ' + STRING(iantLinjer) + '.'). 
    RUN bibl_loggDbFri.p (cLogg, '  Antall korrigert LC  : ' + STRING(iAntLC) + '.'). 
    RUN bibl_loggDbFri.p (cLogg, '  Antall korrigert pris: ' + STRING(iAntPris) + '.'). 
    
END PROCEDURE.

PROCEDURE bkuAvfil:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    OS-CREATE-DIR VALUE(RIGHT-TRIM(cBkuKatalog,'\')) NO-ERROR.
    OS-COPY VALUE(cImpKatalog + cImpfil) VALUE(cBkuKatalog + cBkufil).
    IF SEARCH(cBkuKatalog + cBkufil) <> ? THEN
    DO: 
        OS-DELETE VALUE(cImpKatalog + cImpfil). 
        RUN bibl_loggDbFri.p (cLogg, '  Bku fil ' + cBkuKatalog + cBkufil + '.'). 
    END.
END PROCEDURE.

