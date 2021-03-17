 
 /*------------------------------------------------------------------------
    File        : standardfunksjoner
    Purpose     : 
    Syntax      : 
    Description : Generelt procedurebibliotek for klient
    Author(s)   : tny
    Created     : Thu Apr 28 14:06:07 CEST 2016
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.

/* BLOCK-LEVEL ON ERROR UNDO, THROW. */

CLASS cls.StdFunk.clStandardFunksjoner: 
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE cTekst AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cLogg  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iX     AS INTEGER   NO-UNDO.       
    DEFINE VARIABLE cTxt   AS CHARACTER NO-UNDO.

    { cls\StdFunk\filliste.i }
    { cls\StdFunk\dsttImpFil.i } 
    
    DEFINE STREAM Inn.
    DEFINE STREAM Ut.
    
		
	CONSTRUCTOR PUBLIC clStandardFunksjoner (  ):
		SUPER ().

    ASSIGN 
        cLogg = 'clStandardFunksjoner' + REPLACE(STRING(TODAY),'/','') 
        .
		
	END CONSTRUCTOR.

  CONSTRUCTOR PUBLIC clStandardFunksjoner ( INPUT pcLogg AS CHARACTER ):
    SUPER ().

        ASSIGN 
            cLogg = pcLogg
            cLogg = IF cLogg = ''THEN 'clStandardFunksjoner' + REPLACE(STRING(TODAY),'/','') ELSE cLogg 
            .
    
  END CONSTRUCTOR.


  /*------------------------------------------------------------------------------
   Purpose:
   Notes:
  ------------------------------------------------------------------------------*/

  METHOD PUBLIC CHARACTER FixStorl( INPUT pcStorl AS CHARACTER ):
    
  
  ASSIGN
      pcStorl = TRIM(pcStorl)
      pcStorl = CAPS(pcStorl)
      pcStorl = IF (LENGTH(pcStorl) = 1 OR 
                   LENGTH(pcStorl) = 3
                   ) 
                  then " " + pcStorl
                  else pcStorl.          
  
    /* Bytter ut eventuelle comma med punkt. */
    IF INDEX(pcStorl,",") <> 0 THEN
      OVERLAY(pcStorl, INDEX(pcStorl,","), 1, "CHARACTER") = ".".

  RETURN pcStorl.   /* Function return value. */

  END METHOD.

  /*------------------------------------------------------------------------------
   Purpose:
   Notes:
  ------------------------------------------------------------------------------*/

  METHOD PUBLIC CHARACTER getTmpFileName(  ):
    
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

	METHOD PUBLIC INTEGER ISOWeekDay( ptDate AS DAT ):
		
        DEF VAR v_Weekday AS INTE EXTENT 7 NO-UNDO INITIAL [7,1,2,3,4,5,6].
        RETURN v_Weekday[WEEKDAY(ptdate)].

	END METHOD.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/

	METHOD PUBLIC INTEGER ISOWeekNumber( ptDate AS DATE ):
		
        /* Returns the ISO week number
           Don't use the progress function WEEKDAY, as that starts on Sunday when the ISO weeks start on Monday! */
        DEF VAR v_Days         AS INTE NO-UNDO.
        DEF VAR v_WeekNumber   AS INTE NO-UNDO.
 
        DEF VAR v_WeekMonday   AS DATE NO-UNDO.
        DEF VAR v_WeekThursday AS DATE NO-UNDO.
        DEF VAR v_WeekSunday   AS DATE NO-UNDO.
 
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
	                                     OUTPUT pcUtFil AS CHARACTER ):
		
		DEFINE VARIABLE pbOk               AS LOGICAL          NO-UNDO.

        DEFINE VARIABLE chExcelApplication AS COMPONENT-HANDLE NO-UNDO.  
        DEFINE VARIABLE chWorkbooks        AS COMPONENT-HANDLE NO-UNDO.
        DEFINE VARIABLE chWorksheets       AS COMPONENT-HANDLE NO-UNDO.
        
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

            chWorkbooks = chExcelApplication:Workbooks:OpenText(pcInnFil,2,1,1,1,1,FALSE,TRUE,FALSE,FALSE,FALSE).
            chWorkSheets = chExcelApplication:Sheets:Item(1).
            chWorkSheets:SaveAs(pcUtfil,pcFormat). /* 42 */
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
        IF SEARCH(pcUtfil) <> ? THEN 
            pbOk = TRUE.
        
/*---------------------------------------------------------------------------------------------------*/          
/*/*        pcUtFil  = getTmpFileName().*/                                                                                                        */
/*        ASSIGN                                                                                                                                  */
/*            pcFormat = (IF pcFormat = '' THEN '20' ELSE pcformat)                                                                               */
/*            FILE-INFO:FILE-NAME = SEARCH(pcInnFil).                                                                                             */
/*            pcUtFil  = FILE-INFO:FILE-NAME                                                                                                      */
/*            .                                                                                                                                   */
/*        ENTRY(                                                                                                                                  */
/*              NUM-ENTRIES(pcUtFil,'.'),                                                                                                         */
/*              pcUtFil,                                                                                                                          */
/*              '.') = 'csv'                                                                                                                      */
/*              .                                                                                                                                 */
/*        SkrivTilLogg(cLogg,                                                                                                                     */
/*            '    Fil for konvertering med FILE-INFO:FULL-PATHNAME  (' + FILE-INFO:FULL-PATHNAME + ')'                                           */
/*            ).                                                                                                                                  */
/*                                                                                                                                                */
/*        KONV_BLOKK:                                                                                                                             */
/*        DO:                                                                                                                                     */
/*            CREATE "Excel.Application" chExcelApplication.                                                                                      */
/*            chExcelApplication:Visible = FALSE.                                                                                                 */
/*            chExcelApplication:DisplayAlerts = FALSE.                                                                                           */
/*                                                                                                                                                */
/*            /* Leser inn filen i et excel Ark. Bruker search for å få full path. Excel krever det her. */                                       */
/*/*            chWorkbooks = chExcelApplication:Workbooks:OpenText(FILE-INFO:FULL-PATHNAME,2,1,1,1,1,FALSE,TRUE,FALSE,FALSE,FALSE).*/            */
/*            chWorkbooks = chExcelApplication:Workbooks:OpenText(pcInnFil,2,1,1,1,1,FALSE,TRUE,FALSE,FALSE,FALSE).                               */
/*            IF ERROR-STATUS:ERROR THEN                                                                                                          */
/*            DO ix = 1 TO ERROR-STATUS:NUM-MESSAGES:                                                                                             */
/*                cTxt = '    ** Feil ved innlesing av fil til Excel: '+ STRING(ERROR-STATUS:GET-NUMBER(ix)) + ' ' + ERROR-STATUS:GET-MESSAGE(ix).*/
/*                SkrivTilLogg(cLogg,                                                                                                             */
/*                    '    ' + cTxt                                                                                                               */
/*                    ).                                                                                                                          */
/*            END.                                                                                                                                */
/*            ELSE DO:                                                                                                                            */
/*                chWorkSheets = chExcelApplication:Sheets:Item(1).                                                                               */
/*                IF ERROR-STATUS:ERROR THEN                                                                                                      */
/*                DO ix = 1 TO ERROR-STATUS:NUM-MESSAGES:                                                                                         */
/*                    cTxt = '    ** Feil setting av Sheets: '+ STRING(ERROR-STATUS:GET-NUMBER(ix)) + ' ' + ERROR-STATUS:GET-MESSAGE(ix).         */
/*                    SkrivTilLogg(cLogg,                                                                                                         */
/*                        '    ' + cTxt                                                                                                           */
/*                        ).                                                                                                                      */
/*                END.                                                                                                                            */
/*                /* Skriver til temp fil csv format */                                                                                           */
/*                chWorkSheets:SaveAs(pcUtfil,pcFormat). /* 42 */                                                                                 */
/*                IF ERROR-STATUS:ERROR THEN                                                                                                      */
/*                DO ix = 1 TO ERROR-STATUS:NUM-MESSAGES:                                                                                         */
/*                    cTxt = '    ** Feil ved lagring av fil til csv: '+ STRING(ERROR-STATUS:GET-NUMBER(ix)) + ' ' + ERROR-STATUS:GET-MESSAGE(ix).*/
/*                    SkrivTilLogg(cLogg,                                                                                                         */
/*                        '    ' + cTxt                                                                                                           */
/*                        ).                                                                                                                      */
/*                END.                                                                                                                            */
/*            END.                                                                                                                                */
/*            chExcelApplication:Workbooks:CLOSE().                                                                                               */
/*            chExcelApplication:QUIT().                                                                                                          */
/*        END. /* KONV_BLOKK */                                                                                                                   */
/*                                                                                                                                                */
/*        RELEASE OBJECT chWorkSheets NO-ERROR.                                                                                                   */
/*        RELEASE OBJECT chExcelApplication NO-ERROR.      /* release com-handles */                                                              */
/*                                                                                                                                                */
/*        ASSIGN                                                                                                                                  */
/*            chWorksheets       = ?                                                                                                              */
/*            chWorkbooks        = ?                                                                                                              */
/*            chExcelApplication = ?.                                                                                                             */
/*                                                                                                                                                */
/*        /* Flagger resultatet. */                                                                                                               */
/*        IF SEARCH(pcUtfil) <> ? THEN                                                                                                            */
/*            pbOk = TRUE.                                                                                                                        */
            
		RETURN pbOk.

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
                        tmpFiler.PathName         = FILE-INFO:PATHNAME

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

  METHOD PUBLIC VOID ShellExecuteA( INPUT pcURL AS CHARACTER ):
    
    RUN startURL.p (pcURL).
    
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

        IF pcDbLst = '' THEN 
            pbOk = FALSE.
            
        IF pbOk = TRUE THEN
        DBLOOP: 
        DO piLoop = 1 TO NUM-ENTRIES(pcDbLst):
            IF NOT CONNECTED(ENTRY(piLoop,pcDbLst)) THEN
            DO:
                pbOk = FALSE.
                IF pcLogg <> '' AND pcLogg <> ? THEN  
                    SkrivTilLogg(pcLogg, 
                        '  ** Ingen kontakt med DB ' + ENTRY(piLoop,pcDbLst) + '.'   
                        ).
                LEAVE DBLOOP.  
            END.
        END. /* DBLOOP */   
		
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

	DESTRUCTOR PUBLIC clStandardFunksjoner ( ):

	END DESTRUCTOR.

END CLASS.