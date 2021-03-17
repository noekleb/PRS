 
 /*------------------------------------------------------------------------
    File        : standardfunksjoner
    Purpose     : 
    Syntax      : 
    Description : Generelt procedurebibliotek
    Author(s)   : tny
    Created     : Thu Apr 28 14:06:07 CEST 2016
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.

/* BLOCK-LEVEL ON ERROR UNDO, THROW. */

CLASS cls.StdFunk.StandardFunksjoner: 
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE cTekst AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cLogg  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iX     AS INTEGER   NO-UNDO.       
    DEFINE VARIABLE cTxt   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iErr-Status AS INT NO-UNDO.

    DEFINE VARIABLE cConnectionString AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lConnected        AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE obOk              AS LOG       NO-UNDO.
    DEFINE VARIABLE cIpAdr            AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cStdStrTypeLst    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iLoop AS INTEGER NO-UNDO.

    { cls\StdFunk\filliste.i }
    { cls\StdFunk\dsttImpFil.i } 
    
    DEFINE STREAM Inn.
    DEFINE STREAM Ut.
    
		
    DEFINE PUBLIC PROPERTY SystemName AS CHARACTER NO-UNDO 
    GET.
    SET. 

	CONSTRUCTOR PUBLIC StandardFunksjoner (  ):
		SUPER ().

        initStandardFunksjoner().
 
        ASSIGN 
            cLogg = 'StandardFunksjoner' + REPLACE(STRING(TODAY),'/','') 
            .
        {syspara.i 50 15 51 cStdStrTypeLst}
        IF cStdStrTypeLst = '' THEN 
            cStdStrTypeLst = "200,299".
		
	END CONSTRUCTOR.

    CONSTRUCTOR PUBLIC StandardFunksjoner ( INPUT pcLogg AS CHARACTER ):
        SUPER ().

        initStandardFunksjoner().
 
        ASSIGN 
            cLogg = pcLogg 
            .
        IF cLogg = '' THEN 
          ASSIGN 
              cLogg = 'StandardFunksjoner' + REPLACE(STRING(TODAY),'/','') 
              .
        {syspara.i 50 15 51 cStdStrTypeLst}
        IF cStdStrTypeLst = '' THEN 
            cStdStrTypeLst = "200,299".
    END CONSTRUCTOR.

  /*------------------------------------------------------------------------------
   Purpose:
   Notes:
   ABL OS-ERROR codes
      0 No error 
      1 Not owner 
      2 No such file or directory 
      3 Interrupted system call 
      4 I/O error 
      5 Bad file number 
      6 No more processes 
      7 Not enough core memory 
      8 Permission denied 
      9 Bad address 
      10 File exists 
      11 No such device 
      12 Not a directory 
      13 Is a directory 
      14 File table overflow 
      15 Too many open files 
      16 File too large 
      17 No space left on device 
      18 Directory not empty 
      999 Unmapped error (ABL default) 
     
  ------------------------------------------------------------------------------*/

  METHOD PUBLIC CHARACTER Error-Status( INPUT piError-Status AS INTEGER ):    
    DEFINE VARIABLE cResult AS CHARACTER NO-UNDO.
    CASE piError-Status:
      WHEN 0 THEN cResult = 'No error'. 
      WHEN 1 THEN cResult = 'Not owner'. 
      WHEN 2 THEN cResult = 'No such file or directory'. 
      WHEN 3 THEN cResult = 'Interrupted system call'. 
      WHEN 4 THEN cResult = 'I/O error'. 
      WHEN 5 THEN cResult = 'Bad file number'. 
      WHEN 6 THEN cResult = 'No more processes'. 
      WHEN 7 THEN cResult = 'Not enough core memory'. 
      WHEN 8 THEN cResult = 'Permission denied'. 
      WHEN 9 THEN cResult = 'Bad address'. 
      WHEN 10 THEN cResult = 'File exists'. 
      WHEN 11 THEN cResult = 'No such device'. 
      WHEN 12 THEN cResult = 'Not a directory'. 
      WHEN 13 THEN cResult = 'Is a directory'. 
      WHEN 14 THEN cResult = 'File table overflow'. 
      WHEN 15 THEN cResult = 'Too many open files'. 
      WHEN 16 THEN cResult = 'File too large'. 
      WHEN 17 THEN cResult = 'No space left on device'. 
      WHEN 18 THEN cResult = 'Directory not empty'. 
      WHEN 999 THEN cResult = 'Unmapped error (ABL default)'.
      OTHERWISE cResult = 'Ukjent OS feil.'.  
    END CASE.

    RETURN cResult.

  END METHOD.

    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    METHOD PUBLIC CHARACTER FixChk( INPUT cKode AS CHARACTER ):

        DEF VAR iCount1 AS INTE NO-UNDO.
        DEF VAR iMulti  AS INTE INIT 1 NO-UNDO.
        DEF VAR iSum AS INTE NO-UNDO.
      
        DO iCount1 = LENGTH(cKode) TO 1 BY -1:  
            ASSIGN iMulti = IF iMulti = 1 THEN 3 ELSE 1
                   iSum = iSum + INT(SUBSTR(cKode,iCount1,1)) * iMulti.
        END.
        RETURN cKode + string((10 - iSum MODULO 10) MODULO 10).

    END METHOD.

    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    METHOD PUBLIC CHARACTER getTempFileName(  ):
                            
      DEFINE VARIABLE Mbasename AS CHARACTER   NO-UNDO.
      DEFINE VARIABLE Merror    AS CHARACTER   NO-UNDO.
      DEFINE VARIABLE Mfilename AS CHARACTER   NO-UNDO.
      DEFINE VARIABLE Mpathname AS CHARACTER   NO-UNDO.
      DEFINE VARIABLE Pdir      AS CHARACTER   NO-UNDO.
      DEFINE VARIABLE Pext      AS CHARACTER   NO-UNDO.
 
      ASSIGN 
          Pext = 'tmp'
          Pdir = SESSION:TEMP-DIRECTORY.
          .
    
      DO WHILE TRUE:
        ASSIGN
          /* Start with 8-digit basename */
          Mbasename = STRING(RANDOM(0,9999),"9999")
                      + STRING((TIME * 100 + ETIME) MODULO 10000,"9999")
     
          /* Append extention, if specified */
          Mfilename = Mbasename + (IF Pext <> "" THEN "." + Pext ELSE "")
     
          /* Prefix with directory, if specified */
          Mpathname = (IF Pdir <> "" THEN
                              REPLACE(Pdir,"~\","~/")
                              + (IF SUBSTRING(Pdir,LENGTH(Pdir),1) = "/"
                                    OR SUBSTRING(Pdir,LENGTH(Pdir),1) = "~\" THEN
                                   ""
                                 ELSE
                                   "/")
                            ELSE
                              "")
                           + Mfilename.
     
        /* If the file doesn't exist, then leave the loop */
        IF SEARCH(Mpathname) = ? THEN LEAVE.
      END.
 
      RETURN Mpathname.
    END METHOD.

    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    METHOD PUBLIC VOID hentSystem( INPUT-OUTPUT pcSystem AS CHARACTER ):
        
        /* Her skal det legges inn kode som sjekker hvilket eksternt system som er aktivt. */
        /* foreløpig hardkodes det :)                                                      */
        IF pcSystem = '' THEN 
            ASSIGN
                pcSystem = 'PRS'
                .
        
        RETURN.

    END METHOD.

    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    METHOD PROTECTED VOID initStandardFunksjoner(  ):

        IF SEARCH('tnc.txt') <> ? THEN  
            ASSIGN
                cIpAdr            = 'localhost'
                cConnectionString = "-H " + cIpAdr + " -AppService asPRS -S 3090 -DirectConnect"
                .
        ELSE 
            ASSIGN
                cIpAdr            = 'localhost'
                cConnectionString = "-H " + cIpAdr + " -AppService asbroker1 -S 3090 -DirectConnect"
                .

        hentSystem(INPUT-OUTPUT SystemName).
        
        RETURN.

    END METHOD.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/

	METHOD PUBLIC INTEGER ISOWeekDay( ptDate AS DAT ):
		
        DEF VAR v_Weekday AS INTE EXTENT 7 NO-UNDO INITIAL [7,1,2,3,4,5,6].
        RETURN v_Weekday[WEEKDAY(ptdate)].

	END METHOD.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/

	METHOD PUBLIC INTEGER ISOWeekNumber( ptDate AS DATE):
		
        /* Returns the ISO week number
           Don't use the progress function WEEKDAY, as that starts on Sunday when the ISO weeks start on Monday! */
        DEFINE VARIABLE v_Days         AS INTE NO-UNDO.
 
        DEFINE VARIABLE v_WeekMonday   AS DATE NO-UNDO.
        DEFINE VARIABLE v_WeekThursday AS DATE NO-UNDO.
        DEFINE VARIABLE v_WeekSunday   AS DATE NO-UNDO.
        DEFINE VARIABLE v_WeekNumber AS INTEGER NO-UNDO.
 
        v_WeekMonday    = ptDate - ISOWeekDay(ptDate) + 1.    /* Start of week */
        v_WeekThursday  = v_WeekMonday + 3.
        v_WeekSunday    = v_WeekMonday + 6.
 
        v_Days = v_WeekMonday - DATE( 1, 1, YEAR( ptDate ) ).   /* get number of days for monday*/
        v_WeekNumber = INTE(ROUND(v_Days / 7,0 ) ) + 1.     /* calculate week number */
 
        IF v_WeekNumber = 53 THEN 
        DO                /* work where the overlapping week sits */
            :
            IF YEAR(v_WeekThursday) = YEAR(ptDate)
                THEN v_WeekNumber = 53.
            ELSE v_WeekNumber = 1.
        END.
        IF v_WeekNumber = 0 THEN v_WeekNumber = 53.
 
        RETURN v_WeekNumber.

	END METHOD.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/

	METHOD PUBLIC LOGICAL konvExcel2csv( INPUT pcInnFil AS CHARACTER,
	                                     INPUT pcformat AS CHARACTER,
	                                     OUTPUT pcUtFil AS CHARACTER, 
	                                     OUTPUT piLoop  AS INTEGER ):

    DEFINE VARIABLE pbOk  AS LOGICAL NO-UNDO.
    DEFINE VARIABLE piX AS INTEGER NO-UNDO.
    DEFINE VARIABLE pcNyUtFil AS CHARACTER NO-UNDO.
    DEFINE VARIABLE pcPrefix AS CHARACTER NO-UNDO.
    DEFINE VARIABLE pcUtfilLst AS CHARACTER NO-UNDO.

    DEFINE VARIABLE chExcelApplication AS COMPONENT-HANDLE NO-UNDO.  
    DEFINE VARIABLE chWorkbooks        AS COMPONENT-HANDLE NO-UNDO.
    DEFINE VARIABLE chWorksheets       AS COMPONENT-HANDLE NO-UNDO.

        SkrivTilLogg(cLogg,
            '  pcInnFil: ' + pcInnFil
            ).
        SkrivTilLogg(cLogg,
            '  Entries pcInnFil: ' + STRING(NUM-ENTRIES(pcInnFil,'|'))
            ).
        
        IF NUM-ENTRIES(pcInnFil,'|') = 2 THEN 
          ASSIGN 
            pcPrefix = ENTRY(2,pcInnFil,'|')
            pcInnFil = ENTRY(1,pcInnFil,'|')
            .
        ELSE 
          pcPrefix = ''.
          
        ASSIGN 
            pcFormat = (IF pcFormat = '' THEN '20' ELSE pcformat)
            FILE-INFO:FILE-NAME = SEARCH(pcInnFil).
            pcUtFil  = FILE-INFO:FILE-NAME
            .  
        ENTRY(
              NUM-ENTRIES(pcUtFil,'.'),
              pcUtFil,
              '.') = 'csv'
              .
        KONV_BLOKK:
        DO:
            CREATE "Excel.Application" chExcelApplication.  
            chExcelApplication:Visible = FALSE.                                     
            chExcelApplication:DisplayAlerts = FALSE.
            
            chExcelApplication:Workbooks:OpenText(pcInnFil,2,1,1,1,1,FALSE,TRUE,FALSE,FALSE,FALSE).
            
            /* Default sheet skal eksporteres. */
            IF pcPrefix = '' THEN             
              chExcelApplication:ActiveSheet:SaveAs(pcUtfil,42,,,,,,,TRUE).
            /* Alle sheet som har et navn som begynner med pcPrefix skal eksporteres. */
            ELSE DO:
              SkrivTilLogg(cLogg,
                  '  Excel.Application: ' + STRING(chExcelApplication:Sheets:Count())
                  ).
  
              pcUtfilLst = ''.
              REPEAT piX = 1 TO chExcelApplication:Sheets:Count():
                pcNyUtfil = REPLACE(pcUtFil,'NYOUTLET','NYOUTLET_' + STRING(piX) + '_').
                chExcelApplication:Worksheets(piX):Activate.
                IF STRING(chExcelApplication:Sheets:Item(piX):Name) BEGINS pcPrefix THEN
                DO:
                  chExcelApplication:ActiveSheet:SaveAs(pcNyUtfil,42,,,,,,,TRUE).
                  pcUtfilLst = pcUtfilLst + 
                               (IF pcUtfilLst <> '' THEN ',' ELSE '') +
                               pcNyUtfil. 
                END.
                SkrivTilLogg(cLogg,
                    '  Excel.Sheet: ' + STRING(chExcelApplication:Sheets:Item(piX):Name)
                    ).
              END.
            END.
            SkrivTilLogg(cLogg,
                '  Ferdig: ' + STRING(piX)
                ).

            /* ----- */
            
            chExcelApplication:Workbooks:CLOSE().
            chExcelApplication:QUIT().
        END. /* KONV_BLOKK */
        
        RELEASE OBJECT chWorkSheets NO-ERROR.
        RELEASE OBJECT chExcelApplication NO-ERROR.      /* release com-handles */
        
        ASSIGN 
            chWorksheets       = ?
            chWorkbooks        = ?
            chExcelApplication = ?.
 
        /* Flagger resultatet. */
        IF pcPrefix = '' THEN 
        DO:
          IF SEARCH(pcUtfil) <> ? THEN 
              pbOk = TRUE.
        END.
        /* Sender fillisten tilbake. */
        ELSE DO:
          pbOk = TRUE.
          pcUtFil = pcUtfilLst.
        END.
	END METHOD.

	/*------------------------------------------------------------------------------
	 Purpose: LagFillisteForKatalog
	           Lager en liste med filer. En record pr. fil, som ligger i angitt 
	           katalog. Et enkelt filter kan sendes inn. På fil prefix og suffix.
	 Notes:
	------------------------------------------------------------------------------*/

	METHOD PUBLIC VOID LagFillisteForKatalog( INPUT  cKatalog AS CHARACTER,
	                                          INPUT  cPrefixFilter AS CHARACTER, 
                                              INPUT  cExtentFilter AS CHARACTER, 
	                                          OUTPUT TABLE tmpFiler ):

        DEFINE VARIABLE cFilNavn    AS CHARACTER NO-UNDO.  
        DEFINE VARIABLE cFilePath   AS CHARACTER NO-UNDO.  
        DEFINE VARIABLE cFileAttrib AS CHARACTER NO-UNDO.
        DEFINE VARIABLE lFilId      AS DECIMAL FORMAT ">>>>>>>>9" NO-UNDO.
	
	    EMPTY TEMP-TABLE tmpFiler.
	    
	    ASSIGN cKatalog = RIGHT-TRIM(cKatalog,'\').
        INPUT STREAM Inn FROM OS-DIR (cKatalog) NO-ECHO .
        
        FILINPUT:
        REPEAT:
            IMPORT STREAM Inn
                cFilNavn  
                cFilePath  
                cFileAttrib
                .

            /* Bare filer skal bahandles. */
            IF LOOKUP("F",cFileAttrib) <> 0 THEN
            FILBLOKK:
            DO:
                /* Åpner for filinformasjonen */
                ASSIGN
                    FILE-INFO:FILE-NAME = cFilePath
                    cTekst              = IF NUM-ENTRIES(cFilNavn,'.') > 1 THEN ENTRY(2,cFilNavn,'.') ELSE ''.
                . 

                /* Ignorerer tomme filer. */
                IF FILE-INFO:FILE-SIZE = 0 THEN 
                    NEXT FILINPUT.

                /* Kun filer med filnavn som oppfyller masken på filnavn skal inn. */
                IF cPrefixFilter <> '' THEN 
                DO:
                    IF NOT cFilNavn BEGINS cPrefixFilter THEN 
                        NEXT FILINPUT.
                END.

                /* Kun filer med ekstent som oppfyller masken på extent skal inn. */
                IF cExtentFilter <> '' AND NUM-ENTRIES(cFilNavn,'.') >= 2 THEN 
                DO:
                    IF NOT ENTRY(NUM-ENTRIES(cFilNavn,'.'),cFilNavn,'.') = LEFT-TRIM(cExtentFilter,'.')  THEN 
                        NEXT FILINPUT.
                END.

                /* Sjekker at filen er mer enn 20 sec gammel. */
                /*
                IF FILE-INFO:FILE-MOD-DATE = TODAY THEN 
                DO:
                    IF TIME - FILE-INFO:FILE-MOD-TIME < 20 THEN 
                        NEXT FILINPUT.
                END.
                */ 

                /* Oppretter posten i filen. */
                IF NOT CAN-FIND(FIRST tmpFiler WHERE
                    tmpFiler.Full-Path-Name   = FILE-INFO:FILE-NAME AND
                    tmpFiler.File-Create-Date = FILE-INFO:FILE-CREATE-DATE AND
                    tmpFiler.File-Create-Time = FILE-INFO:FILE-CREATE-TIME AND
                    tmpFiler.File-Mod-Date    = FILE-INFO:FILE-MOD-DATE AND
                    tmpFiler.File-Mod-Time    = FILE-INFO:FILE-MOD-TIME AND
                    tmpFiler.File-Size        = FILE-INFO:FILE-SIZE AND
                    tmpFiler.PathName         = FILE-INFO:PATHNAME
                    ) THEN
                DO:
                    /* Finner FilId */
                    FIND LAST tmpFiler NO-LOCK NO-ERROR.
                    IF AVAILABLE tmpFiler THEN
                        lFilId = tmpFiler.FilId + 1.
                    ELSE
                        lFilId = 1.
                    CREATE tmpFiler.
                    ASSIGN
                        tmpFiler.FilId            = lFilId
                        tmpFiler.Full-Path-Name   = FILE-INFO:FILE-NAME 
                        tmpFiler.File-Create-Date = FILE-INFO:FILE-CREATE-DATE 
                        tmpFiler.File-Create-Time = FILE-INFO:FILE-CREATE-TIME 
                        tmpFiler.File-Size        = FILE-INFO:FILE-SIZE 
                        tmpFiler.File-Name        = cFilNavn
                        tmpFiler.PathName         = RIGHT-TRIM(REPLACE(FILE-INFO:PATHNAME,cFilNavn,''),'\')
                        tmpFiler.File-Mod-Date    = FILE-INFO:FILE-MOD-DATE 
                        tmpFiler.File-Mod-Time    = FILE-INFO:FILE-MOD-TIME 
                        tmpFiler.AntLinjer        = 0
                        tmpFiler.File-Type        = FILE-INFO:FILE-TYPE 
                        /*tmpFiler.cFile-Name        = cFilNavn*/
                        tmpFiler.DatoTidOpprettet = DATETIME(FILE-INFO:FILE-CREATE-DATE,FILE-INFO:FILE-CREATE-TIME * 1000)
                        tmpFiler.DatoTidEndret    = DATETIME(FILE-INFO:FILE-MOD-DATE,FILE-INFO:FILE-MOD-TIME * 1000)
                        .
                    IF NUM-ENTRIES(tmpFiler.File-Name,'.') > 0 THEN  
                        tmpfiler.Ekstent = ENTRY(NUM-ENTRIES(tmpFiler.File-Name,'.'),tmpFiler.File-Name,'.')
                        .
                END.

            END. /* FILBLOKK */
        END. /* FILINPUT */
        INPUT STREAM Inn CLOSE.
		
		
		RETURN. 

	END METHOD. /* LagFillisteForKatalog */

    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    METHOD PUBLIC LOGICAL nedkoblingAppServer( INPUT pcLogg AS CHARACTER,
                                               INPUT-OUTPUT phServer AS HANDLE ):
        
        DEFINE VARIABLE bResult AS LOGICAL NO-UNDO.
        IF VALID-HANDLE(phServer) THEN
        DO: 
            IF phServer:CONNECTED() THEN 
                phServer:DISCONNECT().
            DELETE OBJECT phServer.
            bResult = TRUE.
        END.

        RETURN bResult.

    END METHOD.

    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    METHOD PUBLIC LOGICAL oppkoblingAppServer( INPUT pcLogg AS CHARACTER,
                                               OUTPUT phServer AS HANDLE ):
        
        DEFINE VARIABLE bResult AS LOGICAL NO-UNDO.
        
        CREATE SERVER phServer. 
        bResult = phServer:CONNECT(cConnectionString) NO-ERROR.
        IF ERROR-STATUS:ERROR THEN
        DO:
            cTekst = ''. 
            DO ix = 1 TO ERROR-STATUS:NUM-MESSAGES:    
                cTekst = cTekst + 
                    (IF cTekst <> '' THEN CHR(10) ELSE '') + 
                    STRING(ERROR-STATUS:GET-NUMBER(ix)) + ' ' + ERROR-STATUS:GET-MESSAGE(ix).
            END.
            SkrivTilLogg(pcLogg,
                pcLogg + ' - Connect: ' + cTekst 
                ).    
        END.
        SkrivTilLogg(cLogg,
            pcLogg + ' - Return-value: ' + RETURN-VALUE + '.' 
            ).    
        RETURN bResult.

    END METHOD.

    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    METHOD PUBLIC LOGICAL opprettKatalog( INPUT pcKatalog AS CHARACTER ):
        
        DEFINE VARIABLE bResult AS LOGICAL NO-UNDO.
        DEFINE VARIABLE piLoop  AS INTEGER NO-UNDO.
        
        DO piLoop = 1 TO NUM-ENTRIES(pcKatalog,'\'):
            OS-COMMAND SILENT mkdir VALUE(ENTRY(piLoop,pcKatalog,'\')) NO-ERROR.
        END.
        IF NOT ERROR-STATUS:ERROR THEN 
            bResult = TRUE. 
        
        RETURN bResult.

    END METHOD.

  /*------------------------------------------------------------------------------
   Purpose:
   Notes:
  ------------------------------------------------------------------------------*/

  METHOD PUBLIC LOGICAL prepKatalog( INPUT pcKatalogLst AS CHARACTER,
                                     INPUT pcBku AS CHARACTER ):    
    DEFINE VARIABLE bResult AS LOGICAL NO-UNDO.
    DEFINE VARIABLE pcTekst AS CHARACTER NO-UNDO.

    ASSIGN 
      pcKatalogLst = TRIM(pcKatalogLst,'\')
      pcKatalogLst = TRIM(pcKatalogLst,'/')
      pcKatalogLst = pcKatalogLst + '\'
      pcBku        = TRIM(pcBku,'\')
      pcBku        = TRIM(pcBku,'/')
      .

    /* Oppretter kataloger hvis de mangler. */
    pcTekst = ''.
    DO iLoop = 1 TO NUM-ENTRIES(pcKatalogLst,'\'):
      pcTekst = pcTekst +
               (IF pcTekst <> '' THEN '\' ELSE '') +  
               ENTRY(iLoop,pcKatalogLst,'\').
      OS-CREATE-DIR VALUE(pcTekst).
      iErr-Status = OS-ERROR.
      IF iErr-Status <> 0 THEN
        bResult = FALSE.
      ELSE 
        bResult = TRUE.
    END.
    IF bResult = TRUE  THEN 
    DO:
      pcTekst = pcTekst + pcBku.
      OS-CREATE-DIR VALUE(pcTekst).
    END.

    RETURN bResult.
  END METHOD.

    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    METHOD PUBLIC VOID setStdStrType( INPUT plArtikkelNr AS DECIMAL ):
        DEFINE VARIABLE piStrTypeId1 AS INTEGER NO-UNDO.
        DEFINE VARIABLE piStrTypeId2 AS INTEGER NO-UNDO.
        DEFINE VARIABLE piValgtStrTypeId AS INTEGER NO-UNDO.
        DEFINE VARIABLE pbFlagg AS LOG NO-UNDO.

        IF NUM-ENTRIES(cStdStrTypeLst) = 2 THEN 
            ASSIGN 
                piStrTypeId1 = INT(ENTRY(1,cStdStrTypeLst))
                piStrTypeId2 = INT(ENTRY(2,cStdStrTypeLst))
            .
        FIND ArtBas NO-LOCK WHERE 
            ArtBas.ArtikkelNr = plArtikkelNr NO-ERROR.
        IF AVAILABLE ArtBas AND piStrTypeId1 > 0 THEN 
        DO:
            STRTYPER:
            FOR EACH StrType NO-LOCK WHERE 
                StrType.StrTypeID >= piStrTypeId1 AND 
                StrType.StrTypeID <= piStrTypeId2:

                pbFlagg = TRUE.    
                /* Tar ikke med PLU koder. */
                FOR EACH StrekKode OF ArtBas NO-LOCK WHERE 
                    LENGTH(StrekKode.Kode) > 7:
                    IF NOT CAN-DO(StrType.Fordeling,STRING(StrekKode.StrKode)) THEN 
                    DO:
                        pbFlagg = FALSE.
                        NEXT STRTYPER.
                    END.
                END.
                
                IF pbFlagg THEN 
                DO:
                    FIND CURRENT ArtBas EXCLUSIVE-LOCK.
                    ASSIGN 
                        ArtBas.StrTypeID = StrType.StrTypeID.
                    FIND CURRENT ArtBas NO-LOCK.
                    LEAVE STRTYPER.                    
                END.    
            END. /*STRTYPER*/
            
        END.
        
        RETURN.

    END METHOD.

	/*------------------------------------------------------------------------------
	 Purpose:  
	 Notes:
	------------------------------------------------------------------------------*/

	METHOD PUBLIC LOG SjekkOmDbErOppkoblet(INPUT pcDbLst AS CHARACTER,
	                                       INPUT pcLogg  AS CHARACTER ):
	    DEFINE VARIABLE piLoop AS INTEGER  NO-UNDO.
        DEFINE VARIABLE pbOk   AS LOG      NO-UNDO.

        pbOk = TRUE.

        /* Blank liste er godkjent */
        IF pcDbLst = '' THEN 
            pbOk = TRUE.
            
        IF pbOk = TRUE THEN 
        DO piLoop = 1 TO NUM-ENTRIES(pcDbLst):
            IF NOT CONNECTED(ENTRY(piLoop,pcDbLst)) THEN
            DO:
                pbOk = FALSE.
                IF pcLogg <> '' AND pcLogg <> ? THEN  
                    SkrivTilLogg(pcLogg, 
                        '  ** Ingen kontakt med DB ' + ENTRY(piLoop,pcDbLst) + '.'   
                        ). 
            END.
        END.    
		
		RETURN pbOk.

	END METHOD.

	/*------------------------------------------------------------------------------
	 Purpose: Skriver tekstlinje til loggfil.
	 Notes:
	------------------------------------------------------------------------------*/

	METHOD PUBLIC VOID SkrivTilLogg(INPUT cLoggfil AS CHARACTER,
	                                INPUT cTekst   AS CHARACTER ):

        DEFINE VARIABLE cDatoTid AS CHAR NO-UNDO.
        DEFINE VARIABLE cFilNavn AS CHAR NO-UNDO.
        DEFINE VARIABLE cKatalog AS CHAR NO-UNDO.

        cKatalog = '.\LOG'.
        IF cKatalog <> '' THEN 
        DO:
            /* Sikrer at katalog finnes. */
            OS-CREATE-DIR VALUE(RIGHT-TRIM(cKatalog,'\')).    
            cKatalog = RIGHT-TRIM(cKatalog,'\') + '\'.
        END.

        ASSIGN
            cFilNavn = cKatalog + cLoggfil + '.log'
            cDatoTid = STRING(TODAY) + ' ' + STRING(TIME,"HH:MM:SS") + ' '
            .

        OUTPUT STREAM Ut TO VALUE(cFilNavn) APPEND UNBUFFERED NO-ECHO.

        PUT STREAM Ut UNFORMATTED
            cDatoTid
            cTekst SKIP.

        OUTPUT STREAM Ut CLOSE.
		
		RETURN.

	END METHOD. /* SkrivTilLogg*/

    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    METHOD PUBLIC INTEGER WeekNum( INPUT pdDate AS DATE ):
        
        DEFINE VARIABLE piResult AS INTEGER NO-UNDO.
        DEFINE VARIABLE piYr   AS INT.  /* Year of indate, eg 1990      */
        DEFINE VARIABLE piD1   AS INT.  /* Weekday of 1/1 current year, eg 2  */
                          /* (01/01/90 is a Monday)      */
        DEFINE VARIABLE pdDat1 AS DATE. /* Starting date of week 1     */
        DEFINE VARIABLE piWn   AS INT.  /* Week number , eg 45         */
        
        ASSIGN
          piYr     = YEAR(pdDate)
          piD1     = WEEKDAY(DATE( 1 , 1 , piYr))
          pdDat1   = (IF piD1 LE 5 THEN DATE(1,  3, piYr) - piD1 ELSE
                        DATE(1, 10, piYr) - piD1 )
          piWn     = TRUNCATE((pdDate - pdDat1 + 7) / 7 , 0)
          piResult = piYr * 100 + piWn.
        
        IF piWn < 1 THEN       /* Week 52 or 53 previous year ? */
        ASSIGN
          piYr     = piYr - 1
          piD1     = WEEKDAY(DATE( 1 , 1 , piYr))
          pdDat1   = (IF piD1 LE 5 THEN DATE(1,  3, piYr) - piD1 ELSE
                        DATE(1, 10, piYr) - piD1 )
          piWn     = TRUNCATE((pdDate - pdDat1 + 7) / 7 , 0)
          piResult = piYr * 100 + piWn.
        
        ELSE IF piWn > 52 THEN  /* Week 53 this year or week 1 next year ? */
        ASSIGN
          piYr     = piYr + 1
          piD1     = WEEKDAY(DATE( 1 , 1 , piYr))
          piResult = IF piD1 EQ 6 OR piD1 EQ 7 OR piD1 EQ 1
                  THEN (piYr - 1) * 100 + 53 ELSE piYr * 100 + 1.

        RETURN piResult.

    END METHOD.

  /*------------------------------------------------------------------------------
   Purpose:
   Notes:
  ------------------------------------------------------------------------------*/

  METHOD PUBLIC LOGICAL importerImpFil( INPUT pcTempFil AS CHARACTER,
                                        INPUT piFilId AS INTEGER,
                                        INPUT-OUTPUT DATASET dsttImpFil ):
    
    DEFINE VARIABLE bResult AS LOGICAL NO-UNDO.
    DEFINE VARIABLE cLinje AS CHARACTER NO-UNDO.
    DEFINE VARIABLE piLinjeNr AS INTEGER NO-UNDO.

    EMPTY TEMP-TABLE ttImpFil.
    EMPTY TEMP-TABLE ttImpFilLinje.
    
    IF SEARCH(pcTempFil) = ? THEN 
      DO:
        bResult = FALSE.
        RETURN bResult.
      END.
    
    INPUT STREAM Inn FROM VALUE(pcTempFil)
      CONVERT TARGET "iso8859-1" SOURCE "utf-8".
    CREATE ttImpFil.
    ASSIGN 
      ttImpFil.FilId   = piFilId
      ttImpFil.FilNavn = pcTempFil
      . 
    REPEAT:
      IMPORT STREAM Inn UNFORMATTED
        cLinje.
      
      cLinje = REPLACE(cLinje,CHR(9),';'). /* Erstatter TAB med ; */
      
      IF cLinje BEGINS '1;2;3;4' THEN 
        NEXT.
      IF cLinje BEGINS ';;;;;;;;;;;;' THEN 
        NEXT.
        
      piLinjeNr = piLinjeNr + 1.
      IF cLinje <> '' THEN 
      DO:
        CREATE ttImpFilLinje.
        ASSIGN 
          ttImpFilLinje.FilId   = ttImpFil.FilId
          ttImpFilLinje.LinjeNr = piLinjeNr
          ttImpFilLinje.Record  = STRING(cLinje)
          .  
      END.
    END.
    INPUT STREAM Inn CLOSE.

    ASSIGN 
      bResult = TRUE 
      .
    RETURN bResult.

  END METHOD.

  /*------------------------------------------------------------------------------
   Purpose:
   Notes:
  ------------------------------------------------------------------------------*/

  METHOD PUBLIC LOGICAL getKommisjonsButLst( OUTPUT pcKommisjonsButLst AS CHARACTER ):

    DEFINE VARIABLE iKommisjonaktiv AS INTEGER NO-UNDO.
    DEFINE VARIABLE bResult AS LOGICAL NO-UNDO.
    {syspar2.i 50 55 14 iKommisjonaktiv INT}

    IF iKommisjonaktiv = 1 THEN 
    DO:
      FOR EACH Butiker NO-LOCK WHERE 
        Butiker.Butik >= 10100 AND 
        Butiker.butik <= 10999:
        pcKommisjonsButLst = pcKommisjonsButLst + 
                             (IF pcKommisjonsButLst <> '' THEN ',' ELSE '') + 
                             STRING(Butiker.Butik).  
      END.
    END.    

    RETURN bResult.

  END METHOD.

	DESTRUCTOR PUBLIC StandardFunksjoner ( ):

	END DESTRUCTOR.

END CLASS.