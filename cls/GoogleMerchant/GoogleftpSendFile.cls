 
 /*------------------------------------------------------------------------
    File        : ftpSendFile
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : tny
    Created     : Mon Sep 12 13:32:43 CEST 2016
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.
USING cls.*.

CLASS cls.GoogleMerchant.GoogleftpSendFile: 
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE cmdParaFil  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cmdFil      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cmdLogFil   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cKatalog    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cFil        AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cLinje      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cLogg       AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cBkuKatalog AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cBkuFil     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE piLoop      AS INTEGER   NO-UNDO.
    DEFINE VARIABLE cTekst      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE bTest       AS LOG NO-UNDO.
    DEFINE VARIABLE lETime      AS DECIMAL   FORMAT ">>>>>>>>>>>>9" NO-UNDO.
     
    /* Properties. */
    DEFINE PROPERTY iAktiv AS INTEGER NO-UNDO GET. SET.
    DEFINE PROPERTY cTjener AS CHARACTER NO-UNDO GET. SET.
    DEFINE PROPERTY cBrukernavn AS CHARACTER NO-UNDO GET. SET.
    DEFINE PROPERTY cPassord AS CHARACTER NO-UNDO GET. SET.
    DEFINE PROPERTY cFeederFil AS CHARACTER NO-UNDO GET. SET.

    DEFINE VARIABLE rStandardFunksjoner AS CLASS cls.StdFunk.StandardFunksjoner NO-UNDO.

    DEFINE STREAM Ut.
    DEF STREAM Inn.

	CONSTRUCTOR PUBLIC GoogleftpSendFile ( INPUT pcLogg AS CHARACTER ):
		SUPER ().

        rStandardFunksjoner = NEW cls.StdFunk.StandardFunksjoner().
		
		ASSIGN 
		    cLogg = pcLogg
		    bTest = TRUE  
		    .
		setParametre().
		
	END CONSTRUCTOR.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/

	METHOD PUBLIC VOID SendFile( INPUT  pcFil AS CHARACTER,
                                 OUTPUT bOk AS LOG, 
                                 OUTPUT cReturn AS CHARACTER ):
        DEFINE VARIABLE pcFeederFil AS CHARACTER NO-UNDO.

        rStandardFunksjoner:SkrivTilLogg(cLogg, 
            '   GoogleftpSendfile.SendFile: ' + cFil
            ). 
        
        ASSIGN 
            cFil        = pcFil         
            cmdParaFil  = 'cmd\Googleftpcmd#1.dat'
            cmdLogFil   = 'cmd\Googleftpcmd#1.log' 
            cmdFil      = 'cmd\Googleftpcmd#1.bat' 
            cKatalog    = 'cmd'
            lETime      = DEC(ETIME) 
            bOk         = FALSE
            .
        /* Legger over filen til FeederFil */
        ASSIGN 
          pcFeederFil = cFil
          ENTRY(NUM-ENTRIES(pcFeederFil,'\'),pcFeederFil,'\') = cFeederFil
          cBkuFil     = cFil 
          ENTRY(NUM-ENTRIES(cBkuFil,'\'),cBkuFil,'\') = 'bku\' + ENTRY(NUM-ENTRIES(cBkuFil,'\'),cBkuFil,'\')  
          . 
        IF SEARCH(pcFeederFil) <> ? THEN 
        DO:
          IF bTest THEN 
            rStandardFunksjoner:SkrivTilLogg(cLogg, 
                'FeederFil finnes fra før (' + pcFeederFil + ') - sending avbrutt.'  
                ).
            bOk = TRUE.
            RETURN.
        END.
        IF bTest THEN 
          rStandardFunksjoner:SkrivTilLogg(cLogg, 
              'FeederFil: ' + pcFeederFil  
              ).
        OS-COPY VALUE(cFil) VALUE(pcFeederFil).
            
        /* ---- Skaper parameterfil ---- */
        ASSIGN 
            cTekst = REPLACE(cmdParaFil,'#1',TRIM(STRING(lETime,">>>>>>>>>>>9"))).
        OUTPUT STREAM Ut TO VALUE(cTekst).
        PUT STREAM Ut UNFORMATTED 
            'user ' + cBrukerNavn SKIP
            cPassord SKIP 
            'bin' SKIP 
            'put ' + pcFeederFil SKIP 
            'quit' SKIP.
        OUTPUT STREAM Ut CLOSE.

        /* ---- Skaper kommandofil ---- */
        ASSIGN 
            cTekst = REPLACE(cmdFil,'#1',TRIM(STRING(lETime,">>>>>>>>>>>9"))).
        OUTPUT STREAM Ut TO VALUE(cTekst).
        PUT STREAM Ut UNFORMATTED 
            'ftp -n -s:' + REPLACE(cmdParaFil,'#1',TRIM(STRING(lETime,">>>>>>>>>>>9"))) + ' ' + cTjener SKIP 
            .
        rStandardFunksjoner:SkrivTilLogg(cLogg, 
            '   ftp -n -s:' + REPLACE(cmdParaFil,'#1',TRIM(STRING(lETime,">>>>>>>>>>>9"))) + ' ' + cTjener
            ). 
        OUTPUT STREAM Ut CLOSE.

        /* ---- Ufører kommando (Bat fil) og sender output til loggfil. ---- */
        ASSIGN 
            cTekst = REPLACE(cmdFil,'#1',TRIM(STRING(lETime,">>>>>>>>>>>9"))) + ' > ' + 
                REPLACE(cmdLogFil,'#1',TRIM(STRING(lETime,">>>>>>>>>>>9"))).
        OS-COMMAND SILENT VALUE(cTekst).
            
        /* Logger kommando */
        rStandardFunksjoner:SkrivTilLogg(cLogg, 
            cTekst
            ). 

        /* Sjekker resultat i loggfil. */
        cTekst = REPLACE(cmdLogFil,'#1',TRIM(STRING(lETime,">>>>>>>>>>>9"))).
        IF SEARCH(cTekst) <> ? THEN
        DO:
            INPUT STREAM Inn FROM VALUE(cTekst).
            REPEAT:
                IMPORT STREAM Inn UNFORMATTED 
                    cLinje.
                IF (cLinje = '200 PORT command successful' OR 
                    cLinje = '200 Command PORT okay.') THEN
                    bOk = TRUE.
            END.
            INPUT STREAM Inn CLOSE.
        END.


        /* ----- Presenterer svaret hvis ok ---- */
        IF bOk THEN
        VELLYKKET_SENDING:
        DO:
            /* --- Sletter FeederFil. ---*/
            OS-DELETE VALUE(pcFeederFil).
            
            cTekst = cFil.
            /* --- Kopierer til backup katalog. ---*/
            IF SEARCH(cFil) <> ? THEN
                OS-COPY VALUE(cFil) VALUE(cBkuFil).
            /* --- Sletter sendt fil. ---*/
            IF SEARCH(cBkuFil) <> ? THEN
                OS-DELETE VALUE(cFil).
                
            /* --- For test, skriv til logg --- */     
            cReturn = 'OK ---- Fil overført med ftp ' + cFil + ' (FeederFil: ' + cFeederFil + ').'.
            rStandardFunksjoner:SkrivTilLogg(cLogg, 
                cReturn
                ). 
            
            cTekst = REPLACE(cmdLogFil,'#1',TRIM(STRING(lETime,">>>>>>>>>>>9"))).
            IF SEARCH(cTekst) <> ? THEN
            DO:
                INPUT STREAM Inn FROM VALUE(cTekst).
                REPEAT:
                    IMPORT STREAM Inn UNFORMATTED 
                        cLinje.
                    rStandardFunksjoner:SkrivTilLogg(cLogg, 
                        cLinje
                        ). 
                END.
                INPUT STREAM Inn CLOSE.
            END.
            rStandardFunksjoner:SkrivTilLogg(cLogg, 
                '---- Logg ferdig '
                ). 
            rStandardFunksjoner:SkrivTilLogg(cLogg, 
                ' '
                ). 
            IF SEARCH(cFil) <> ? THEN
                OS-DELETE VALUE(cFil).
        END. /* VELLYKKET_SENDING */
        /* ----- Presenterer svaret hvis feil ---- */
        ELSE 
        FEIL_VED_SENDING:
        DO:
            cReturn = '**---- Feil ved ftp overføring av fil ' + cFil + '.'.
            rStandardFunksjoner:SkrivTilLogg(cLogg, cReturn). 
            IF SEARCH(cTekst) <> ? THEN
            DO:
                INPUT STREAM Inn FROM VALUE(cTekst).
                REPEAT:
                    IMPORT STREAM Inn UNFORMATTED 
                        cLinje.
                    rStandardFunksjoner:SkrivTilLogg(cLogg, cLinje). 
                END.
                INPUT STREAM Inn CLOSE.
            END.

/*            IF SEARCH(cFil) <> ? THEN                                                 */
/*            DO:                                                                       */
/*                rStandardFunksjoner:SkrivTilLogg(cLogg, '**FeederFil: ' + cFil + '.').*/
/*                INPUT STREAM Inn FROM VALUE(cFil).                                    */
/*                REPEAT:                                                               */
/*                    IMPORT STREAM Inn UNFORMATTED                                     */
/*                        cLinje.                                                       */
/*                    rStandardFunksjoner:SkrivTilLogg(cLogg, cLinje).                  */
/*                END.                                                                  */
/*                INPUT STREAM Inn CLOSE.                                               */
/*            END.                                                                      */
            rStandardFunksjoner:SkrivTilLogg(cLogg, '**---- Feillogg ferdig '). 
            rStandardFunksjoner:SkrivTilLogg(cLogg, ' '). 
        END. /* FEIL_VED_SENDING */

        /* Rydder bort filer */
        IF bTest = FALSE THEN
        DO:
            cTekst = REPLACE(cmdFil,'#1',TRIM(STRING(lETime,">>>>>>>>>>>9"))).
            IF SEARCH(cTekst) <> ? THEN
                OS-DELETE VALUE(cTekst).
            cTekst = REPLACE(cmdParaFil,'#1',TRIM(STRING(lETime,">>>>>>>>>>>9"))).
            IF SEARCH(cTekst) <> ? THEN
                OS-DELETE VALUE(cTekst).
            cTekst = REPLACE(cmdLogFil,'#1',TRIM(STRING(lETime,">>>>>>>>>>>9"))).
            IF SEARCH(cTekst) <> ? THEN
                OS-DELETE VALUE(cTekst).
            cTekst = cFil.
            IF SEARCH(cTekst) <> ? THEN
                OS-DELETE VALUE(cTekst).
		END.
		RETURN.

        CATCH zeroError AS Progress.Lang.AppError:
            rStandardFunksjoner:SkrivTilLogg(cLogg,
                '** GoogleftpSendFile zeroError: ' + zeroError:GetMessage(1) 
                ).
        END CATCH.
        CATCH oneError AS Progress.Lang.SysError:
            rStandardFunksjoner:SkrivTilLogg(cLogg,
                '** GoogleftpSendFile oneError: ' + oneError:GetMessage(1) 
                ).
        END CATCH.                
        CATCH twoError AS Progress.Lang.ProError:
            rStandardFunksjoner:SkrivTilLogg(cLogg,
                '** GoogleftpSendFile twoError: ' + twoError:GetMessage(1) 
                ).
        END CATCH.    

	END METHOD.

  /*------------------------------------------------------------------------------
   Purpose:
   Notes:
  ------------------------------------------------------------------------------*/

  METHOD PUBLIC LOGICAL setParametre(  ):   
    DEFINE VARIABLE bResult AS LOGICAL NO-UNDO.

    {syspara.i 50 67 1 iAktiv INT}
    {syspara.i 50 67 2 cTjener}
    {syspara.i 50 67 3 cBrukernavn}
    {syspara.i 50 67 4 cPassord}
    {syspara.i 50 65 5 cFeederFil}
    
    IF iAktiv <> 1 OR  
      cTjener = '' OR 
        cBrukernavn = '' OR 
          cPassord = '' THEN 
      bResult = FALSE.
    ELSE 
      bResult = TRUE.
      
    RETURN bResult.
  END METHOD.

	DESTRUCTOR PUBLIC GoogleftpSendFile ( ):

	END DESTRUCTOR.

END CLASS.