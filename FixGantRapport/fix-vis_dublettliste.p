DEFINE VARIABLE iantArt AS INTEGER NO-UNDO.
DEFINE VARIABLE cSasongLst AS CHARACTER NO-UNDO.
DEFINE VARIABLE cArtNrLst AS CHARACTER FORMAT "x(100)" NO-UNDO.
DEFINE VARIABLE cUtFil AS CHARACTER NO-UNDO.
DEFINE VARIABLE bHarEan AS LOG NO-UNDO.
DEFINE VARIABLE bHarTrans AS LOG NO-UNDO.
DEFINE VARIABLE iAntTrans AS INTEGER NO-UNDO.
DEFINE VARIABLE iAntEan AS INTEGER NO-UNDO.
DEFINE VARIABLE cAntLst AS CHARACTER NO-UNDO.
DEFINE VARIABLE cBeskrLst AS CHARACTER NO-UNDO.
DEFINE VARIABLE lSanertTil LIKE ArtBas.ArtikkelNr NO-UNDO.
DEFINE VARIABLE iMaksAnt AS INTEGER NO-UNDO.
DEFINE VARIABLE cVmLst AS CHARACTER NO-UNDO.
DEFINE VARIABLE iantLinjer AS INTEGER NO-UNDO.
DEFINE VARIABLE iLoop AS INTEGER NO-UNDO.
DEFINE VARIABLE bLikVaretekst AS LOG NO-UNDO.
DEFINE VARIABLE bLikSasong AS LOG NO-UNDO.
DEFINE VARIABLE cSasongSjekkLst AS CHARACTER NO-UNDO.

DEFINE STREAM Ut.

DEFINE TEMP-TABLE ttArtikkel
    FIELD ArtikkelNr LIKE ArtBas.ArtikkelNr
    FIELD AntEan AS INTEGER
    .

ASSIGN
    iMaksAnt   = 0
    cSasongLst = '201801,201802,201803,201804'
    cVmLst     = '92,94,103,106,109'
    cUtFil     = 'konv\dublettlisteUtvidet' + REPLACE(STRING(TODAY),'/','') + '.csv'
    .

OUTPUT STREAM Ut TO VALUE(cUtFil).
PUT STREAM Ut UNFORMATTED
    'iantArt;'
    'ArtikkelNr;'
    'Varetekst;'
    'LevKod;' 
    'Sasong;' 
    'LevFargKod;'
    'Varemerke;'
    'VaremerkeNavn;'
    'HarEan;'
    'HarTrans;'
    'RegistrertDato;'
    'SanertDato;'
    'EDato;'
    'lSanertTil;'
    'cArtNrLst;'
    'bLikSasong;'
    'cSasongSjekkLst;'
    'bLikVaretekst;'
    'cBeskrLst;' 
    'cAntLst'
    SKIP.


/*    cSasongLst = '201801,201802,201803,201804'*/
/*    cVmLst     = '103'                        */
/*BLOKKEN:                                        */
/*FOR EACH ArtBas NO-LOCK WHERE                   */
/*    ArtBas.LevKod > '' AND                      */
/*    ArtBas.Sasong >= 201800 AND                 */
/*    ArtBas.LevFargKod > '' AND                  */
/*    CAN-DO(cSasongLst,STRING(ArtBas.Sasong)) AND*/
/*    NOT CAN-DO(cVmLst,STRING(ArtBas.VmId)) AND  */
/*    ArtBas.SanertDato = ?                       */

BLOKKEN:
FOR EACH ArtBas NO-LOCK WHERE 
    ArtBas.LevKod > '' AND 
    /*ArtBas.LevKod = '14638606' AND /* TEST */*/
    NOT CAN-DO(cVmLst,STRING(ArtBas.VmId)) 
    BREAK BY ArtBas.LevKod
          BY ArtBas.LevFargKod
          BY ArtBas.SaSong:
    ASSIGN 
        iAntEan   = 0
        iantTrans = 0
        iAntArt   = iAntArt + 1
        .
    cArtNrLst = cArtNrLst + 
                (IF cArtNrLst = '' THEN '' ELSE '|') + 
                STRING(ArtBas.ArtikkelNr).

    IF bHarEan = FALSE AND 
        CAN-FIND(FIRST Strekkode OF ArtBas) THEN
        bHarEAN = TRUE.

    IF bHarTrans = FALSE AND 
        CAN-FIND(FIRST TransLogg WHERE 
                 TransLogg.ArtikkelNr = ArtBas.ArtikkelNr) THEN
        bHarTrans = TRUE.

    FOR EACH Strekkode OF ArtBas:
        iAntEan = iAntEan + 1.
    END.
    
    CREATE ttArtikkel.
    ASSIGN 
        ttArtikkel.ArtikkelNr = ArtBas.ArtikkelNr
        ttArtikkel.AntEan     = iAntEan
        .

    cAntLst = cAntLst + 
                (IF cAntLst = '' THEN '' ELSE '|') + 
                STRING(iAntEan).
    cBeskrLst = cBeskrLst + 
                (IF cBeskrLst = '' THEN '' ELSE '|') + 
                STRING(TRIM(ArtBas.Beskr)).                                
    cSasongSjekkLst = cSasongSjekkLst + 
                (IF cSasongSjekkLst = '' THEN '' ELSE '|') + 
                STRING(STRING(ArtBas.Sasong)).                                
    
    IF LAST-OF(ArtBas.Sasong) THEN
    DO:
        IF iantArt > 1 THEN
        SANERBLOKK:
        DO:
            FIND Varemerke OF ArtBas NO-LOCK NO-ERROR.
            iAntLinjer = iAntLinjer + 1.
            FOR EACH ttArtikkel
                BREAK BY ttArtikkel.AntEan:
                lSanertTil = ttArtikkel.ArtikkelNr.
            END.
            
            bLikVaretekst = TRUE.
            BESKRSjekk:
            DO iLoop = 1 TO NUM-ENTRIES(cBeskrLst,'|'):
                IF TRIM(ArtBas.Beskr) <> ENTRY(iLoop,cBeskrLst,'|') THEN 
                DO:                    
                    bLikVaretekst = FALSE.
                    LEAVE BESKRSjekk.
                END.                     
            END. /* BESKRSjekk */
            
            bLikSasong = TRUE.
            SASONGSjekk:
            DO iLoop = 1 TO NUM-ENTRIES(cSasongSjekkLst,'|'):
                IF STRING(ArtBas.Sasong) <> ENTRY(iLoop,cSasongSjekkLst,'|') THEN 
                DO:                    
                    bLikSasong = FALSE.
                    LEAVE SASONGSjekk.
                END.                     
            END. /* SASONGSjekk */
                        
            PUT STREAM Ut UNFORMATTED
                iantArt ';'
                ArtBas.ArtikkelNr ';'
                ArtBas.Beskr ';'
                ArtBas.LevKod ';' 
                ArtBas.Sasong ';' 
                ArtBas.LevFargKod ';'
                ArtBas.VMId ';'
                (IF AVAILABLE Varemerke THEN Varemerke.Beskrivelse ELSE '') ';'
                bHarEAN ';'
                bHarTrans ';'
                ArtBas.RegistrertDato ';' 
                ArtBas.SanertDato ';'
                ArtBas.EDato ';'
                lSanertTil ';'
                cArtNrLst ';'
                bLikSasong ';'
                cSasongSjekkLst ';'
                bLikVaretekst ';'
                cBeskrLst ';'
                cAntLst
                SKIP.
                
            IF (bHarEAN = FALSE AND bHarTrans = FALSE) THEN 
                RUN SlettArtbas.
            ELSE 
                RUN SanerArtikkel.
            IF iMaksAnt > 0 AND iAntLinjer >= iMaksAnt THEN
                LEAVE BLOKKEN.
        END. /* SANERBLOKK */
        
        ASSIGN 
            cArtNrLst = ''
            cBeskrLst = ''
            cSasongSjekkLst = ''
            cAntLst   = ''
            iAntArt   = 0
            bHarEan   = FALSE
            bHarTrans = FALSE
            .
        EMPTY TEMP-TABLE ttArtikkel.
    END.
END. /* BLOKKEN */ 
OUTPUT STREAM Ut CLOSE.

PROCEDURE SanerArtikkel:
/*    DEF BUFFER bufArtBas FOR ArtBas.                                               */
/*    FOR EACH ttArtikkel:                                                           */
/*        IF ttArtikkel.ArtikkelNr = lSanertTil THEN                                 */
/*            NEXT.                                                                  */
/*        ELSE DO TRANSACTION:                                                       */
/*            FIND bufArtBas EXCLUSIVE-LOCK WHERE                                    */
/*                bufArtBas.ArtikkelNr = ttArtikkel.ArtikkelNr.                      */
/*            DO:                                                                    */
/*                CREATE Elogg.                                                      */
/*                ASSIGN ELogg.EksterntSystem  = "KORRHK"                            */
/*                       ELogg.TabellNavn      = "VPIArtBas"                         */
/*                       ELogg.Verdier         = "0|" + STRING(lSanertTil) + "|"     */
/*                                                    + STRING(ttArtikkel.artikkelnr)*/
/*                       ELogg.EndringsType    = 1                                   */
/*                       ELogg.Behandlet       = FALSE.                              */
/*                IF AVAILABLE Elogg THEN RELEASE Elogg.                             */
/*            END. /* TRANSACTION */                                                 */
/*            ASSIGN bufArtbas.IKasse   = FALSE                                      */
/*                   bufArtBas.Sanertdato = TODAY                                    */
/*                   bufArtBas.Beskr = 'KORR: ' + bufArtBas.Beskr.                   */
/*            RUN vpikorreksjon.w.                                                   */
/*        END. /*  TRANSACTION */                                                    */
/*    END.                                                                           */
END PROCEDURE.

PROCEDURE SlettArtbas:
/*    DEFINE BUFFER bufArtBas FOR ArtBas.                      */
/*    FOR EACH ttArtikkel:                                     */
/*        RUN slettartbasbatch.w (INPUT ttArtikkel.ArtikkelNr).*/
/*    END.                                                     */
END PROCEDURE.
    


