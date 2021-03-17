 
/*------------------------------------------------------------------------
   File        : RFIDEtikettTilFil.cls
   Purpose     : 
   Syntax      : 
   Description : 
   Author(s)   : tn
   Created     : Thu feb 04 09:14:40 CEST 2018
   Notes       : 
 ----------------------------------------------------------------------*/

USING Progress.Lang.*.
USING cls.*.

CLASS cls.RFIDEtikett.RFIDEtikettTilFil: 
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE cPrefix1            AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cPrefix2            AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cEkstent            AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cKatalog            AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cFil                AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cButKatalog         AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lSeqNr              AS DECIMAL   FORMAT ">>>>>>>9" NO-UNDO.
    DEFINE VARIABLE cButNr              AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cRfid1              AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cRfid2              AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cRfid3              AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cRfid4              AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cKode               AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lRfidSeqNr          AS DECIMAL   FORMAT ">>>>>>>>>>>9". 
    DEFINE VARIABLE cBrukerId           AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cLogg               AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cHost               AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cReturn             AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cTxt                AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iIterasjon          AS INTEGER   NO-UNDO.
    DEFINE VARIABLE cTekst              AS CHARACTER NO-UNDO.

    DEFINE VARIABLE lftpSend AS LOG NO-UNDO.

    DEFINE VARIABLE rStandardFunksjoner AS CLASS     cls.StdFunk.StandardFunksjoner NO-UNDO.
    DEFINE VARIABLE rftpSendFile        AS CLASS     cls.RFIDEtikett.ftpSendFile        NO-UNDO.

    {cls\RFIDEtikett\etiko.i}
    
    DEFINE STREAM Ut.
    DEFINE STREAM cmdUt.    

    CONSTRUCTOR PUBLIC RFIDEtikettTilFil ( INPUT pcLogg AS CHARACTER ):
        SUPER ().

        /* Settes til FALSE. Sending av filer gjøres av eget program. */
        ASSIGN 
            lftpSend = FALSE      
            .
        /* TEST --------------------------------------------- TEST */
    
        ASSIGN
            cLogg      = pcLogg
            cKatalog   = 'filer\'
            cEkstent   = '.csv'
            cPrefix1   = '_ETI'
            cPrefix2   = REPLACE(cPrefix1,'_','')
            /*cHost      = '10.81.239.100'*/            
            cHost      = '10.0.0.100'            
            iIterasjon = 0
            .
        
        rStandardFunksjoner = NEW cls.StdFunk.StandardFunksjoner() NO-ERROR.
        rftpSendFile        = NEW cls.RFIDEtikett.ftpSendFile(INPUT cLogg ) NO-ERROR.
                
        rStandardFunksjoner:SkrivTilLogg(cLogg, 
            '   Skriver til loggfil: ' + cLogg
            ). 

        /* Sikrer at filkatalog finnes. */      
        OS-CREATE-DIR VALUE(RIGHT-TRIM(cKatalog,'\')) NO-ERROR. 

    END CONSTRUCTOR.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/

	METHOD PUBLIC VOID FoerstePost( ):
    
        PUT STREAM Ut UNFORMATTED 
            'Ean;' +
            'Etitekst1;' +
            'Enhetstekst;' +
            'Utpris;' +
            'Brukerid;' +
            'StyleCode;' +
            'ButNr;' +
            'Antall;' +
            'Antpkn;' +
            'Emb;' +
            'Vgr;' +
            'Sortiment;' +
            'Levnr;' +
            'Bestnr;' +
            'Enhetspris;' +
            'Pristekst;' +
            'Prisntekst;' +
            'Levvnr;' +
            'Veilpris;' +
            'Varegrtekst;' +
            'Fabrikatnavn;' +
            'Fargetekst;' +
            'Størrelsestekst;' +  
            'Levnavn;' +
            'Modellnr2;' + 
            'Rfid1;' + 
            'Rfid2;' + 
            'Rfid3;' + 
            'Rfid4;' +
            'Etitype' 
            SKIP.
    		
		RETURN.

	END METHOD.

    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    METHOD PUBLIC LOGICAL Skrivetikett( INPUT DATASET dsEtiko ):
        
        DEFINE VARIABLE bOk AS LOGICAL NO-UNDO.
        DEFINE VARIABLE iEtitype AS INTEGER  NO-UNDO.
        DEFINE VARIABLE piLoop AS INTEGER NO-UNDO.
        DEFINE VARIABLE cStr AS CHARACTER NO-UNDO.
        DEFINE VARIABLE cFarge AS CHARACTER NO-UNDO.
        DEFINE VARIABLE iAntLedig AS INTEGER NO-UNDO.
        DEFINE VARIABLE piButNr AS INTEGER NO-UNDO.
        
        bOk = FALSE.

        /* TEST */
        DATASET dsEtiko:WRITE-JSON('file', 'konv\dsEtiko.JSon', TRUE). 

        FIND FIRST ttEtiko WHERE 
            ttEtiko.butNr > 0 NO-ERROR.
        IF NOT AVAILABLE ttEtiko THEN 
            RETURN bOk.

        lRfidSeqNr = 0.
        IF lSeqNr > 99999999 THEN 
            lSeqNr = 0.
        ASSIGN 
            lSeqNr      = lSeqNr + 1
            cButNr      = STRING(ttEtiko.butNr)
            cButKatalog = cButNr + '\'
            .
        
        OS-CREATE-DIR VALUE(cKatalog + RIGHT-TRIM(cButKatalog,'\')) NO-ERROR. 
        
        ASSIGN 
            cTekst = STRING(NOW).
            cTekst = ENTRY(2,cTekst,' ').
            cTekst = REPLACE(cTekst,':','').
            cTekst = REPLACE(cTekst,',','').
            cTekst = REPLACE(cTekst,'+','').
            cFil   = cKatalog + cButKatalog + cPrefix1 + '_' + cbutNr + '_'  + STRING(lSeqNr) + '_' + ttEtiko.BrukerId  + '_' + cTekst + cEkstent
            .
        rStandardFunksjoner:SkrivTilLogg(cLogg, 
            'Skriver til fil: ' + cFil
            ). 
                    
        OUTPUT STREAM Ut TO VALUE(cFil) APPEND.
        FoerstePost( ).
        LESLOOP:
        FOR EACH ttEtiko NO-LOCK /*WHERE
            
            ttEtiko.BrukerId          >= '' AND     
            ttEtiko.StyleCode         >= 0  AND 
            ttEtiko.ButNr             >  0  AND 
            ttEtiko.levvnr            >= '' AND 
            ttEtiko.fargetekst        >= '' AND 
            ttEtiko.storrtekst        >= '' AND 
            ttEtiko.Sortering         >= '' AND 
            ttEtiko.SekNr             >= 0  AND 
            ttEtiko.RFIDSkrevetStatus  = 0
            */
            BREAK
            BY ttEtiko.Brukerid
            BY ttEtiko.StyleCode
            BY ttEtiko.ButNr
            BY ttEtiko.levvnr
            BY ttEtiko.fargetekst
            BY ttEtiko.storrtekst:
        
            iEtitype = INT(ttEtiko.Skrivernavn).
            IF ttEtiko.ButNr > 0 AND piButNr = 0 THEN 
                piButNr  = ttEtiko.ButNr.

            ASSIGN
                lRfidSeqNr = lRfidSeqNr + 1
                cKode      = FILL('0',13 - LENGTH(ttEtiko.Ean)) + ttEtiko.Ean
                cRfid1     = '1'
                cRfid2     = SUBSTRING(cKode,1,7)
                cRfid3     = '0' + SUBSTRING(cKode,8,5)
                cRfid4     = TRIM(STRING(lRfidSeqNr,">>>>>>>>>>>9"))
                .    
                
            ASSIGN 
                cStr   = TRIM(ttEtiko.storrtekst)
                cFarge = TRIM(ttEtiko.fargetekst) 
                .
            IF LENGTH(cStr + cFarge) > 17 THEN 
            DO:
                /* Ingen plass å manipulere med. Bege felt er for store. */
                IF LENGTH(cStr) >= 6 AND LENGTH(cFarge) >= 11 THEN 
                    ASSIGN
                    cStr   = SUBSTRING(cStr,1,6)
                    cFarge = SUBSTRING(cFarge,1,11) 
                    .
                /* Det er ledig plass i størrelsen */
                ELSE IF LENGTH(cStr) < 6 THEN 
                    DO:
                       iAntLedig = 6 - LENGTH(cStr).
                       cFarge = SUBSTRING(cFarge,1,11 + iAntLedig). 
                    END.
                /* Ledig plass i fargen */
                ELSE IF LENGTH(cFarge) < 11 THEN 
                    DO:
                        iAntLedig = 11 - LENGTH(cFarge).
                        cStr = SUBSTRING(cStr,1,6 + iAntLedig). 
                    END.
            END.
            
            /* Legger ut en rad pr. etikett hvis det skal skrives ut flere etiketter. */
            SKRIVLOOP:
            DO piLoop = 1 TO ttEtiko.quantity:
                PUT STREAM Ut UNFORMATTED
                    ttEtiko.Ean ';'
                    TRIM(SUBSTRING(ttEtiko.Etitekst1,1,17)) ';'
                    ttEtiko.enhtekst  ';'
                    ttEtiko.utpris  ';'
                    ttEtiko.Brukerid  ';' 
                    ttEtiko.StyleCode  ';' 
                    ttEtiko.ButNr  ';' 
                    /*ttEtiko.quantity*/ '1' ';'
                    ttEtiko.antpkn  ';'
                    ttEtiko.emb  ';'
                    ttEtiko.hgr  ';'
                    ttEtiko.sortkode  ';'
                    ttEtiko.levnr  ';'
                    ttEtiko.bestnr  ';'
                    ttEtiko.enhpris  ';'
                    ttEtiko.pristekst  ';'
                    ttEtiko.prisntekst  ';'
                    ttEtiko.levvnr  ';'
                    ttEtiko.veilpris  ';'
                    ttEtiko.hgrtekst  ';'
                    ttEtiko.fabrikatnavn  ';'
                    cFarge ';'
                    cStr ';'
                    ttEtiko.levnavn  ';'
                    TRIM(SUBSTRING(ttEtiko.modellnr2,1,10)) ';'
                    cRfid1 ';'
                    cRfid2 ';'
                    cRfid3 ';'
                    cRfid4 ';'
                    iEtitype
                    SKIP 
                    .
            END. /* SKRIVLOOP */

            /* Flagger posten som skrevet. */
            ASSIGN ttEtiko.RFIDSkrevetStatus = 1.
            
            ASSIGN 
                cTxt = STRING(ttEtiko.BrukerId) + ' / ' + 
                        STRING(ttEtiko.Stylecode) + ' / ' + 
                        STRING(ttEtiko.ButNr) + ' / ' + 
                        STRING(ttEtiko.Sortering) + ' / ' + 
                        STRING(ttEtiko.Ean) + ' / ' + 
                        STRING(ttEtiko.Etitekst1) + ' / ' + 
                        STRING(ttEtiko.FabrikatNavn) + ' / ' + 
                        STRING(ttEtiko.ModellNr2) + ' / ' + 
                        STRING(ttEtiko.Fargetekst) + ' / ' + 
                        STRING(ttEtiko.StorrTekst) + ' / ' + 
                        STRING(ttEtiko.Quantity) + ' / ' + 
                        STRING(ttEtiko.Utpris) + ' / ' + 
                        STRING(ttEtiko.UtprisN) + ' / ' + 
                        STRING(ttEtiko.EnhPris) + ' / ' + 
                        STRING(ttEtiko.UtskrDato) + ' / ' +
                        ttEtiko.SkriverNavn
                . 
                    
            rStandardFunksjoner:SkrivTilLogg(cLogg, 
                cTxt
                ). 
        END. /* LESLOOP */
        OUTPUT STREAM Ut CLOSE.
            
        /* RENAME skal inn her. */
        IF SEARCH(cFil) <> ? THEN 
            OS-RENAME VALUE(cFil) VALUE(REPLACE(cFil,cPrefix1,cPrefix2)).
        
        FIND SysPara WHERE 
            SysPara.SysHId = 5 AND 
            SysPara.SysGr  = 29 AND 
            SysPara.ParaNr = ttEtiko.ButNr NO-LOCK NO-ERROR.
        
        /* Sender fil */
        
        IF lftpSend = TRUE AND AVAILABLE SysPara THEN 
            rftpSendFile:SendFile (cFil,
                ENTRY(1,SysPAra.Parameter1),
                OUTPUT bOk,
                OUTPUT cReturn
                ).
            
        RETURN bOk.

        CATCH zeroError AS Progress.Lang.AppError:
            rStandardFunksjoner:SkrivTilLogg(cLogg,
                '** RFIDEtikettTilFil zeroError: ' + zeroError:GetMessage(1) 
                ).
        END CATCH.
        CATCH oneError AS Progress.Lang.SysError:
            rStandardFunksjoner:SkrivTilLogg(cLogg,
                '** RFIDEtikettTilFil oneError: ' + oneError:GetMessage(1) 
                ).
        END CATCH.                
        CATCH twoError AS Progress.Lang.ProError:
            rStandardFunksjoner:SkrivTilLogg(cLogg,
                '** RFIDEtikettTilFil twoError: ' + twoError:GetMessage(1) 
                ).
        END CATCH.    


        FINALLY:

        END FINALLY.

    END METHOD.

    DESTRUCTOR PUBLIC RFIDEtikettTilFil ( ):

    END DESTRUCTOR.

END CLASS.
