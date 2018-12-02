 
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

CLASS cls.RFIDEtikett.ftpSendFile: 
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
    DEFINE VARIABLE piLoop      AS INTEGER   NO-UNDO.
    DEFINE VARIABLE cTekst      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lETime      AS DECIMAL   FORMAT ">>>>>>>>>>>>9" NO-UNDO.
    DEFINE VARIABLE cHost       AS CHARACTER NO-UNDO.
    DEFINE VARIABLE bTest       AS LOG NO-UNDO.

    DEFINE VARIABLE rStandardFunksjoner AS CLASS cls.StdFunk.StandardFunksjoner NO-UNDO.

    DEFINE STREAM Ut.
    DEF STREAM Inn.

	CONSTRUCTOR PUBLIC ftpSendFile ( INPUT pcLogg AS CHARACTER ):
		SUPER ().

        rStandardFunksjoner = NEW cls.StdFunk.StandardFunksjoner().
		
		ASSIGN 
		    cLogg = pcLogg
		    bTest = TRUE  
		    .
		
	END CONSTRUCTOR.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/

	METHOD PUBLIC VOID SendFile( INPUT  pcFil AS CHARACTER,
                                 INPUT  pcHost AS CHARACTER,
                                 OUTPUT bOk AS LOG, 
                                 OUTPUT cReturn AS CHARACTER ):

        rStandardFunksjoner:SkrivTilLogg(cLogg, 
            '   ftpSendfile.SendFile: ' + cFil
            ). 
        
        ASSIGN 
            cFil        = pcFil         
            cHost       = pcHost /* Test port '10.0.0.100' */            
            cmdParaFil  = 'cmd\ftpcmd#1.dat'
            cmdLogFil   = 'cmd\ftpcmd#1.log' 
            cmdFil      = 'cmd\ftpcmd#1.bat' 
            cKatalog    = 'cmd'
            lETime      = DEC(ETIME) 
            bOk         = FALSE
            .
            
        /* Oppretter nødvendige kataloger */
        cBkuKatalog = ''.
        DO piLoop = 1 TO NUM-ENTRIES(cFil,'\') - 1:
            cBkuKatalog = cBkuKatalog + 
                (IF cBkuKatalog = '' THEN '' ELSE '\') + 
                ENTRY(piLoop,cFil,'\').
            /* Sikrer at cmd katalogen finnes. */
            OS-CREATE-DIR VALUE(cBkuKatalog) NO-ERROR. 
        END.
        cBkuKatalog = cBkuKatalog + '\bku'.
        /* Sikrer at cmd katalogen finnes. */
        OS-CREATE-DIR VALUE(cBkuKatalog) NO-ERROR. 

        /* Sikrer at cmd katalogen finnes. */
        OS-CREATE-DIR VALUE(cKatalog) NO-ERROR. 
            
        /* ---- Skaper parameterfil ---- */
        ASSIGN 
            cTekst = REPLACE(cmdParaFil,'#1',TRIM(STRING(lETime,">>>>>>>>>>>9"))).
        OUTPUT STREAM Ut TO VALUE(cTekst).
        PUT STREAM Ut UNFORMATTED 
            'user -A' SKIP
            'tomn@sport1.no' SKIP 
            'bin' SKIP 
            'cd lp' SKIP 
            'pwd' SKIP 
            'put ' + cFil SKIP 
            'quit' SKIP.
        OUTPUT STREAM Ut CLOSE.

        /* ---- Skaper kommandofil ---- */
        ASSIGN 
            cTekst = REPLACE(cmdFil,'#1',TRIM(STRING(lETime,">>>>>>>>>>>9"))).
        OUTPUT STREAM Ut TO VALUE(cTekst).
        PUT STREAM Ut UNFORMATTED 
            'ftp -n -s:' + REPLACE(cmdParaFil,'#1',TRIM(STRING(lETime,">>>>>>>>>>>9"))) + ' ' + cHost SKIP 
            .
        rStandardFunksjoner:SkrivTilLogg(cLogg, 
            '   ftp -n -s:' + REPLACE(cmdParaFil,'#1',TRIM(STRING(lETime,">>>>>>>>>>>9"))) + ' ' + cHost
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
                IF cLinje = '200 PORT command successful' THEN
                    bOk = TRUE.
            END.
            INPUT STREAM Inn CLOSE.
        END.

        /* ----- Presenterer svaret hvis ok ---- */
        IF bOk THEN
        VELLYKKET_SENDING:
        DO:
            cTekst = cFil.
            /* --- Kopierer til backup katalog. ---*/
            IF SEARCH(cFil) <> ? THEN
                OS-COPY VALUE(cFil) VALUE(cBkuKatalog + '\' + ENTRY(NUM-ENTRIES(cFil,'\'),cFil,'\')).
            /* --- Sletter sendt fil. ---*/
            IF SEARCH(cBkuKatalog + '\' + ENTRY(NUM-ENTRIES(cFil,'\'),cFil,'\')) <> ? THEN
                OS-DELETE VALUE(cFil).
                
            /* --- For test, skriv til logg --- */     
            cReturn = 'OK ---- Fil overført med ftp ' + cFil + '.'.
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

            IF SEARCH(cFil) <> ? THEN
            DO:
                rStandardFunksjoner:SkrivTilLogg(cLogg, '**Etikettfil: ' + cFil + '.'). 
                INPUT STREAM Inn FROM VALUE(cFil).
                REPEAT:
                    IMPORT STREAM Inn UNFORMATTED 
                        cLinje.
                    rStandardFunksjoner:SkrivTilLogg(cLogg, cLinje). 
                END.
                INPUT STREAM Inn CLOSE.
            END.
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
                '** ftpSendFile zeroError: ' + zeroError:GetMessage(1) 
                ).
        END CATCH.
        CATCH oneError AS Progress.Lang.SysError:
            rStandardFunksjoner:SkrivTilLogg(cLogg,
                '** ftpSendFile oneError: ' + oneError:GetMessage(1) 
                ).
        END CATCH.                
        CATCH twoError AS Progress.Lang.ProError:
            rStandardFunksjoner:SkrivTilLogg(cLogg,
                '** ftpSendFile twoError: ' + twoError:GetMessage(1) 
                ).
        END CATCH.    

	END METHOD.

	DESTRUCTOR PUBLIC ftpSendFile ( ):

	END DESTRUCTOR.

END CLASS.