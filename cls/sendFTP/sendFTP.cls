 
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

CLASS cls.sendFTP.sendFTP: 
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
    DEFINE PROPERTY cKatalogSending AS CHARACTER NO-UNDO GET. SET.
    DEFINE PROPERTY cKatalogMottak AS CHARACTER NO-UNDO GET. SET.
    DEFINE PROPERTY cKatalogEksport AS CHARACTER NO-UNDO GET. SET.
    DEFINE PROPERTY cKatalogImport AS CHARACTER NO-UNDO GET. SET.
    DEFINE PROPERTY iKundeNr AS INTEGER NO-UNDO GET. SET.

    DEFINE VARIABLE rStandardFunksjoner AS CLASS cls.StdFunk.StandardFunksjoner NO-UNDO.

    DEFINE STREAM Ut.
    DEF STREAM Inn.

	CONSTRUCTOR PUBLIC sendFTP ( INPUT pcLogg AS CHARACTER ):
		SUPER ().

        rStandardFunksjoner = NEW cls.StdFunk.StandardFunksjoner().
		
		ASSIGN 
		    cLogg = pcLogg
		    cLogg = IF cLogg = '' THEN 'sendFTP' ELSE cLogg
		    bTest = IF SEARCH('test.txt') <> ? THEN TRUE ELSE FALSE.  
		    .
    /* Initierer kommunikasjonsparametrene. */
		setParametre().
		
	END CONSTRUCTOR.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:   Filnavn som sendes inn her, skal være full sti + filnavn.
	------------------------------------------------------------------------------*/

	METHOD PUBLIC VOID SendFile( INPUT  pcFil AS CHARACTER,
                               OUTPUT bOk AS LOG, 
                               OUTPUT cReturn AS CHARACTER ):
        DEFINE VARIABLE pcBkuFil AS CHARACTER NO-UNDO.

        rStandardFunksjoner:SkrivTilLogg(cLogg, 
            '   sendFTP.SendFile: ' + pcFil
            ). 
        
        ASSIGN 
            cmdParaFil  = 'log\sendFTPcmd#1.dat'
            cmdLogFil   = 'log\sendFTPcmd#1.log' 
            cmdFil      = 'log\sendFTPcmd#1.bat' 
            cKatalog    = 'log'
            lETime      = DEC(ETIME) 
            bOk         = FALSE
            .
        /* Legger over filen til FeederFil */
        ASSIGN 
          pcBkuFil = pcFil 
          ENTRY(NUM-ENTRIES(pcBkuFil,'\'),pcBkuFil,'\') = 'bku\' + ENTRY(NUM-ENTRIES(pcBkuFil,'\'),pcBkuFil,'\')  
          . 
            
        /* ---- Skaper parameterfil ---- */
        ASSIGN 
            cTekst = REPLACE(cmdParaFil,'#1',TRIM(STRING(lETime,">>>>>>>>>>>9"))).
        OUTPUT STREAM Ut TO VALUE(cTekst).
        PUT STREAM Ut UNFORMATTED 
            'open ftp://' + cBrukerNavn + ':' + cPassord + '@' + cTjener + '/' SKIP 
            'cd ' + cKatalogSending SKIP
            'bin' SKIP 
            'put ' + pcFil SKIP
            'close' SKIP  
            'exit' SKIP.
        OUTPUT STREAM Ut CLOSE.

        /* ---- Skaper kommandofil ---- */
        /* cmd\winscp.com /ini=nul /script=cmd\winscp.txt */
        ASSIGN 
            cTekst = REPLACE(cmdFil,'#1',TRIM(STRING(lETime,">>>>>>>>>>>9"))).
        OUTPUT STREAM Ut TO VALUE(cTekst).
        PUT STREAM Ut UNFORMATTED 
            'cmd\winscp.com /ini=nul /script=' + REPLACE(cmdParaFil,'#1',TRIM(STRING(lETime,">>>>>>>>>>>9"))) SKIP 
            .
        rStandardFunksjoner:SkrivTilLogg(cLogg, 
            '  cmd\winscp.com /ini=nul /script=' + REPLACE(cmdParaFil,'#1',TRIM(STRING(lETime,">>>>>>>>>>>9")))  
            ). 
        OUTPUT STREAM Ut CLOSE.

        /* ---- Ufører kommando (Bat fil) og sender output til loggfil. ---- */
        ASSIGN 
            cTekst = REPLACE(cmdFil,'#1',TRIM(STRING(lETime,">>>>>>>>>>>9"))) + ' > ' + 
                REPLACE(cmdLogFil,'#1',TRIM(STRING(lETime,">>>>>>>>>>>9"))).
        IF bTest THEN 
          rStandardFunksjoner:SkrivTilLogg(cLogg, 
              'Ufører kommando: ' + cTekst  
              ).           
        OS-COMMAND SILENT VALUE(cTekst).
        IF bTest THEN 
          rStandardFunksjoner:SkrivTilLogg(cLogg, 
              'Kommando utført.'  
              ). 

        /* Sjekker resultat i loggfil. */
        cTekst = REPLACE(cmdLogFil,'#1',TRIM(STRING(lETime,">>>>>>>>>>>9"))).
        IF SEARCH(cTekst) <> ? THEN
        DO:
            INPUT STREAM Inn FROM VALUE(cTekst).
            REPEAT:
                IMPORT STREAM Inn UNFORMATTED 
                    cLinje.
                IF cLinje MATCHES  '*| binary | 100%*' THEN
                    bOk = TRUE.
            END.
            INPUT STREAM Inn CLOSE.
        END.
        IF bTest THEN 
          rStandardFunksjoner:SkrivTilLogg(cLogg, 
              'Resultat sjekk av loggfil:' + STRING(bOk)  
              ). 

        /* ----- Presenterer svaret hvis ok ---- */
        IF bOk THEN
        VELLYKKET_SENDING:
        DO:
              rStandardFunksjoner:SkrivTilLogg(cLogg, 
                  'Filnavn: ' + pcFil + ' ' +  
                  'Bku filnavn: ' + pcBkuFil 
                  ). 
            cTekst = pcFil.
            /* --- Kopierer til backup katalog. ---*/
            IF SEARCH(pcFil) <> ? THEN
                OS-COPY VALUE(pcFil) VALUE(pcBkuFil).
            /* --- Sletter sendt fil. ---*/
            IF SEARCH(pcBkuFil) <> ? THEN
                OS-DELETE VALUE(pcFil).
                
            /* --- For test, skriv til logg --- */     
            cReturn = 'OK ---- Fil overført med ftp ' + pcFil + '.'.
            IF btest THEN 
              rStandardFunksjoner:SkrivTilLogg(cLogg, 
                  cReturn
                  ). 
            
            cTekst = REPLACE(cmdLogFil,'#1',TRIM(STRING(lETime,">>>>>>>>>>>9"))).
            IF SEARCH(cTekst) <> ? AND bTest THEN
            DO:
                rStandardFunksjoner:SkrivTilLogg(cLogg, 
                    'FTP LOGGFIL:'
                    ). 
                INPUT STREAM Inn FROM VALUE(cTekst).
                REPEAT:
                    IMPORT STREAM Inn UNFORMATTED 
                        cLinje.
                    rStandardFunksjoner:SkrivTilLogg(cLogg, 
                        '    ' + cLinje
                        ). 
                END.
                INPUT STREAM Inn CLOSE.
                rStandardFunksjoner:SkrivTilLogg(cLogg, 
                    '---- FTP Logg ferdig '
                    ). 
                rStandardFunksjoner:SkrivTilLogg(cLogg, 
                    ' '
                    ).
            END.
            IF SEARCH(cFil) <> ? THEN
                OS-DELETE VALUE(cFil).
        END. /* VELLYKKET_SENDING */
        
        /* ----- Presenterer svaret hvis feil ---- */
        ELSE 
        FEIL_VED_SENDING:
        DO:
            cReturn = '**---- Feil ved ftp overføring av fil ' + pcFil + '.'.
            rStandardFunksjoner:SkrivTilLogg(cLogg, cReturn). 
            IF SEARCH(cTekst) <> ? AND bTest THEN
            DO:
                rStandardFunksjoner:SkrivTilLogg(cLogg, 
                    'FTP LOGGFIL:'
                    ). 
                INPUT STREAM Inn FROM VALUE(cTekst).
                REPEAT:
                    IMPORT STREAM Inn UNFORMATTED 
                        cLinje.
                    rStandardFunksjoner:SkrivTilLogg(cLogg, 
                      '    ' + cLinje). 
                END.
                INPUT STREAM Inn CLOSE.
                rStandardFunksjoner:SkrivTilLogg(cLogg, 
                    '---- FTP Logg ferdig '
                    ). 
                rStandardFunksjoner:SkrivTilLogg(cLogg, 
                    ' '
                    ).
            END.
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
                '** sendFTPSendFile zeroError: ' + zeroError:GetMessage(1) 
                ).
        END CATCH.
        CATCH oneError AS Progress.Lang.SysError:
            rStandardFunksjoner:SkrivTilLogg(cLogg,
                '** sendFTPSendFile oneError: ' + oneError:GetMessage(1) 
                ).
        END CATCH.                
        CATCH twoError AS Progress.Lang.ProError:
            rStandardFunksjoner:SkrivTilLogg(cLogg,
                '** sendFTPSendFile twoError: ' + twoError:GetMessage(1) 
                ).
        END CATCH.    

	END METHOD.

  /*------------------------------------------------------------------------------
   Purpose:
   Notes:   Man kan velge en parametergruppe, eller overstyre alle.
            Eller overstyre en eller flere enkeltparametre.
  ------------------------------------------------------------------------------*/

  METHOD PUBLIC LOGICAL setParametre(  ):   
    DEFINE VARIABLE bResult AS LOGICAL NO-UNDO.

    IF iKundeNr = 0 THEN 
      iKundeNr = 10.
    IF iAktiv          =  0 THEN {syspara.i 55 iKundeNr 1 iAktiv INT}
    IF cTjener         = '' THEN {syspara.i 55 iKundeNr 2 cTjener}
    IF cBrukernavn     = '' THEN {syspara.i 55 iKundeNr 3 cBrukernavn}
    IF cPassord        = '' THEN {syspara.i 55 iKundeNr 4 cPassord}
    IF cKatalogSending = '' THEN {syspara.i 55 iKundeNr 5 cKatalogSending}
    IF cKatalogMottak  = '' THEN {syspara.i 55 iKundeNr 6 cKatalogMottak}
    IF cKatalogEksport = '' THEN {syspara.i 55 iKundeNr 7 cKatalogEksport}
    IF cKatalogImport  = '' THEN {syspara.i 55 iKundeNr 8 cKatalogImport}

    IF iAktiv <> 1 OR  
      cTjener = '' OR 
        cBrukernavn = '' OR 
          cPassord = '' THEN 
      bResult = FALSE.
    ELSE 
      bResult = TRUE.
      
    RETURN bResult.
  END METHOD.

	DESTRUCTOR PUBLIC sendFTP ( ):

	END DESTRUCTOR.

END CLASS.