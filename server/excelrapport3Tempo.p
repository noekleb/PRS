DEFINE TEMP-TABLE TempoTemplate NO-UNDO
  FIELD Avdeling AS INT
  FIELD AvdelingNavn AS CHAR 
  FIELD RapportManed AS INT 
  FIELD RapportAr AS INT 
  FIELD AntallSolgt AS INT 
  FIELD BruttoSalg AS DEC 
  FIELD NettoSalg AS DEC
  FIELD DbKr AS DEC  
  FIELD Db% AS DEC .

DEFINE TEMP-TABLE PrevYearMonth NO-UNDO LIKE tempoTemplate.
DEFINE TEMP-TABLE SaleThisMonth NO-UNDO LIKE tempoTemplate. 

DEFINE TEMP-TABLE SaleByDay    NO-UNDO 
    FIELD butikkNavn AS CHAR 
    FIELD Dato AS DATE
    FIELD AntallSolgt AS INT 
    FIELD BruttoSalg AS DEC
    FIELD NettoSalg AS DEC 
    FIELD DbKr AS DEC  
    FIELD Db% AS DEC
    INDEX idx1 IS PRIMARY Dato .

FUNCTION getDirectoryName RETURNS CHAR 
    (INPUT ipcName AS CHAR): 

    DEFINE VARIABLE iCnt AS INT NO-UNDO.      
    DEFINE VARIABLE cPath AS CHAR NO-UNDO. 
    DEFINE VARIABLE cFileName AS CHAR NO-UNDO. 
    
    ipcName = REPLACE(ipcName,CHR(92),CHR(1)).
    ipcName = REPLACE(ipcName,CHR(47),CHR(1)).
    cFileName = ENTRY(NUM-ENTRIES(ipcName,chr(1)),ipcName,CHR(1)). 
    DO icnt = 1 TO ( NUM-ENTRIES(ipcName,chr(1)) - 1):
        cPath = cPath + (IF LENGTH(cPath) NE 0 THEN CHR(92) ELSE '' ) + ENTRY(iCnt,ipcName,CHR(1)).
    END. 
    FILE-INFO:FILE-NAME = cPath. 
    IF FILE-INFO:FILE-TYPE BEGINS "D" THEN RETURN FILE-INFO:FULL-PATHNAME.

    RETURN ''.
END. 

FUNCTION getFileName RETURNS CHAR 
    (INPUT ipcName AS CHAR): 

    DEFINE VARIABLE iCnt AS INT NO-UNDO.      
    DEFINE VARIABLE cPath AS CHAR NO-UNDO. 
    DEFINE VARIABLE cFileName AS CHAR NO-UNDO. 
    
    ipcName = REPLACE(ipcName,CHR(92),CHR(1)).
    ipcName = REPLACE(ipcName,CHR(47),CHR(1)).
    cFileName = ENTRY(NUM-ENTRIES(ipcName,chr(1)),ipcName,CHR(1)). 

    RETURN cFileName.
END. 



DEFINE INPUT PARAMETER  ipcTemplateFileName AS CHAR NO-UNDO. 
DEFINE INPUT PARAMETER  iplOpenExcel AS LOGICAL NO-UNDO. 
DEFINE INPUT PARAMETER  ipcNewFileName AS CHAR NO-UNDO. 
DEFINE INPUT PARAMETER  ipcButikkNavn AS CHAR NO-UNDO. 
DEFINE INPUT PARAMETER  ipdDate AS DATE NO-UNDO. 
DEFINE INPUT PARAMETER  ipiSalgsDager AS INT NO-UNDO. 
DEFINE INPUT PARAMETER  ipiTotSalgsDager AS INT NO-UNDO. 
DEFINE INPUT PARAMETER TABLE FOR SaleThisMonth. 
DEFINE INPUT PARAMETER TABLE FOR PrevYearMonth. 
DEFINE INPUT PARAMETER TABLE FOR SaleByDay. 


    DEFINE VARIABLE rGUID AS RAW       NO-UNDO.
    DEFINE VARIABLE hExcel AS COM-HANDLE NO-UNDO. 
    DEFINE VARIABLE cFileName AS CHAR NO-UNDO. 
    DEFINE VARIABLE cTemplateFileName AS CHAR NO-UNDO. 
    DEFINE VARIABLE cTmpFileName AS CHAR NO-UNDO. 

    DEFINE VARIABLE cButikkNavn AS CHAR INIT "SPORT-TEST1" NO-UNDO.
    DEFINE VARIABLE iSalgsDager AS INT INIT 23 NO-UNDO. 
    DEFINE VARIABLE iTotSalgsDager AS INT INIT 27 NO-UNDO. 
    
    DEFINE VARIABLE cHeadingSaleMonth    AS CHAR INIT "Salg i måneden per: &1" NO-UNDO. 
    DEFINE VARIABLE cHeadingPrognose     AS CHAR INIT "Tempo / Prognose måned &1" NO-UNDO. 
    DEFINE VARIABLE cHeadingSaleLastYear AS CHAR INIT "Salg i måned &1" NO-UNDO. 
    DEFINE VARIABLE cHeadingEndring      AS CHAR INIT "Endring Tempo &1 vs faktiske tall &2" NO-UNDO. 
    DEFINE VARIABLE cMonth AS CHAR EXTENT 12 INIT 
      ["Januar","Februar","Mars","April","Mai","Juni",
       "Juli","August","September","Oktober","Novmeber","Desember"] NO-UNDO.

    DEFINE VARIABLE cDay AS CHAR EXTENT 7 INIT 
         ["Søndag","Mandag","Tirsdag","Onsdag","Torsdag","Fredag","Lørdag"] NO-UNDO.

    DEFINE VARIABLE iStartRow AS INT NO-UNDO. 
    DEFINE VARIABLE iRow AS INT NO-UNDO. 
    DEFINE VARIABLE iRow_count AS INT NO-UNDO. 
    DEFINE VARIABLE cCell AS CHAR EXTENT 10 NO-UNDO. 
    DEFINE VARIABLE lOpenExcel AS LOGICAL INIT TRUE NO-UNDO. 
    DEFINE VARIABLE lSaveAS AS LOGICAL INIT FALSE NO-UNDO. 
    DEFINE VARIABLE cSaveAsFileName AS CHAR NO-UNDO. 
    DEFINE VARIABLE cDirectory AS CHAR NO-UNDO. 


    ASSIGN 

       cButikkNavn = ipcButikkNavn
       iSalgsDager = ipiSalgsDager 
       iTotSalgsDager = ipiTotSalgsDager 
       lOpenExcel = iplOpenExcel
       cFileName = ipcNewFileName 
       cButikkNavn = ipcButikkNavn
       cTemplateFileName = ipcTemplateFileName 
       cDirectory = getDirectoryName(cFileName) 
       cHeadingSaleMonth = SUBSTITUTE(cHeadingSaleMonth,cDay[WEEKDAY(ipdDate)] + ' ' + STRING(ipdDate))
       cHeadingPrognose = SUBSTITUTE(cHeadingPrognose,cMonth[MONTH(ipdDate)] + ' ' + STRING(YEAR(ipdDate)))
       cHeadingSaleLastYear = SUBSTITUTE(cHeadingSaleLastYear,cMonth[MONTH(ipdDate)] + ' ' + STRING(YEAR(ipdDate) - 1))
       cHeadingEndring = SUBSTITUTE(cHeadingEndring,STRING(YEAR(ipdDate) - 1),STRING(YEAR(ipdDate)))
       cFileName  = cDirectory + '\' + getFileName(cFileName).
       

    OS-COPY VALUE(cTemplateFileName) VALUE(cFileName).

    cTmpFileName = cFileName. 
    FILE-INFO:FILE-NAME = cTmpFileName. 
    cTmpFileName = FILE-INFO:FULL-PATHNAME. 

    CREATE "Excel.Application" hExcel.
    hExcel:visible = FALSE.
    hExcel:Workbooks:OPEN(cTmpFileName,1,FALSE,,,,,,2,,,,,,).
    
    hExcel:ActiveSheet:Range("B2"):VALUE = cButikkNavn.  
    hExcel:ActiveSheet:Range("I2"):VALUE = iSalgsDager.  
    hExcel:ActiveSheet:Range("I3"):VALUE = iTotSalgsDager.  

    /* Tabell 1 - 2 */ 
    /* Salg i MÃ¥ned */ 
    hExcel:ActiveSheet:Range("B5"):VALUE = cHeadingSaleMonth.  
    hExcel:ActiveSheet:Range("G5"):VALUE = cHeadingPrognose.  

    iStartRow = 8.
    FOR EACH SaleThisMonth NO-LOCK : 
        cCell[1] = 'B' + STRING(iStartRow + iRow).
        cCell[2] = 'C' + STRING(iStartRow + iRow).
        cCell[3] = 'D' + STRING(iStartRow + iRow).
        cCell[4] = 'G' + STRING(iStartRow + iRow).
        iRow = iRow + 1.
        hExcel:ActiveSheet:Range(cCell[1]):VALUE = SaleThisMonth.AvdelingNavn.
        hExcel:ActiveSheet:Range(cCell[4]):VALUE = SaleThisMonth.AvdelingNavn.
        hExcel:ActiveSheet:Range(cCell[2]):VALUE = SaleThisMonth.BruttoSalg.
        hExcel:ActiveSheet:Range(cCell[3]):VALUE = SaleThisMonth.DBkr.
        hExcel:ActiveSheet:Range(cCell[4]):VALUE = SaleThisMonth.AvdelingNavn.
    END. 
    
    DO iRow_count = iRow TO 8:
        cCell[1] = 'B' + STRING(iStartRow + iRow_count).
        cCell[2] = 'C' + STRING(iStartRow + iRow_count).
        cCell[3] = 'D' + STRING(iStartRow + iRow_count).
        cCell[4] = 'E' + STRING(iStartRow + iRow_count).
        hExcel:ActiveSheet:Range(cCell[1]):VALUE = ''.
        hExcel:ActiveSheet:Range(cCell[2]):VALUE = ''.
        hExcel:ActiveSheet:Range(cCell[3]):VALUE = ''.
        hExcel:ActiveSheet:Range(cCell[4]):VALUE = ''.
    END.
    
    /* Tabell 2 Tempo Prognose mÃ¥ned */     
    DO iRow_count = iRow TO 8:
        cCell[1] = 'G' + STRING(iStartRow + iRow_count).
        cCell[2] = 'H' + STRING(iStartRow + iRow_count).
        cCell[3] = 'I' + STRING(iStartRow + iRow_count).
        cCell[4] = 'J' + STRING(iStartRow + iRow_count).
        hExcel:ActiveSheet:Range(cCell[1]):VALUE = ''.
        hExcel:ActiveSheet:Range(cCell[2]):VALUE = ''.
        hExcel:ActiveSheet:Range(cCell[3]):VALUE = ''.
        hExcel:ActiveSheet:Range(cCell[4]):VALUE = ''.
    END.
    /* - Tabell 1 - 2 slutt -----------------------------*/ 


    /* --------------------------------------------------*/ 
    /* Tabell 3 - 4 */ 
    hExcel:ActiveSheet:Range("B21"):VALUE = cHeadingSaleLastYear.  
    hExcel:ActiveSheet:Range("G21"):VALUE = cHeadingEndring.  
    hExcel:ActiveSheet:Range("B37"):VALUE = cHeadingEndring + "%". 

    iRow = 0. 
    iStartRow = 24.
    FOR EACH PrevYearMonth NO-LOCK : 
        cCell[1] = 'B' + STRING(iStartRow + iRow).
        cCell[2] = 'C' + STRING(iStartRow + iRow).
        cCell[3] = 'D' + STRING(iStartRow + iRow).
        cCell[4] = 'G' + STRING(iStartRow + iRow).
        iRow = iRow + 1.
        hExcel:ActiveSheet:Range(cCell[1]):VALUE = PrevYearMonth.AvdelingNavn.
        hExcel:ActiveSheet:Range(cCell[4]):VALUE = PrevYearMonth.AvdelingNavn.
        hExcel:ActiveSheet:Range(cCell[2]):VALUE = PrevYearMonth.BruttoSalg.
        hExcel:ActiveSheet:Range(cCell[3]):VALUE = PrevYearMonth.DBkr.
        hExcel:ActiveSheet:Range(cCell[4]):VALUE = PrevYearMonth.AvdelingNavn.
    END. 
    
    DO iRow_count = iRow TO 8:
        cCell[1] = 'B' + STRING(iStartRow + iRow_count).
        cCell[2] = 'C' + STRING(iStartRow + iRow_count).
        cCell[3] = 'D' + STRING(iStartRow + iRow_count).
        cCell[4] = 'E' + STRING(iStartRow + iRow_count).
        hExcel:ActiveSheet:Range(cCell[1]):VALUE = ''.
        hExcel:ActiveSheet:Range(cCell[2]):VALUE = ''.
        hExcel:ActiveSheet:Range(cCell[3]):VALUE = ''.
        hExcel:ActiveSheet:Range(cCell[4]):VALUE = ''.
    END.
    
    /* Tabell 4 Tempo Endring vs faktiske tall */     
    DO iRow_count = iRow TO 8:
        cCell[1] = 'G' + STRING(iStartRow + iRow_count).
        cCell[2] = 'H' + STRING(iStartRow + iRow_count).
        cCell[3] = 'I' + STRING(iStartRow + iRow_count).
        cCell[4] = 'J' + STRING(iStartRow + iRow_count).
        hExcel:ActiveSheet:Range(cCell[1]):VALUE = ''.
        hExcel:ActiveSheet:Range(cCell[2]):VALUE = ''.
        hExcel:ActiveSheet:Range(cCell[3]):VALUE = ''.
        hExcel:ActiveSheet:Range(cCell[4]):VALUE = ''.
    END.


    iStartRow = 40.
    DO iRow_count = iRow TO 8:
        cCell[1] = 'B' + STRING(iStartRow + iRow_count).
        cCell[2] = 'C' + STRING(iStartRow + iRow_count).
        cCell[3] = 'D' + STRING(iStartRow + iRow_count).
        cCell[4] = 'E' + STRING(iStartRow + iRow_count).
        hExcel:ActiveSheet:Range(cCell[1]):VALUE = ''.
        hExcel:ActiveSheet:Range(cCell[2]):VALUE = ''.
        hExcel:ActiveSheet:Range(cCell[3]):VALUE = ''.
        hExcel:ActiveSheet:Range(cCell[4]):VALUE = ''.
    END.


    /* - Tabell 3 - 4 - 5 slutt -----------------------------*/ 

   /* Salg pr. dag ------------------------------------------*/ 
    iRow = 0. 
    iStartRow = 55.
    FOR EACH SaleByDay NO-LOCK : 
        cCell[1] = 'B' + STRING(iStartRow + iRow).
        cCell[2] = 'C' + STRING(iStartRow + iRow).
        cCell[3] = 'D' + STRING(iStartRow + iRow).
        cCell[4] = 'E' + STRING(iStartRow + iRow).
        cCell[5] = 'G' + STRING(iStartRow + iRow).
        cCell[6] = 'H' + STRING(iStartRow + iRow).

        iRow = iRow + 1.
        hExcel:ActiveSheet:Range(cCell[1]):VALUE = SaleByDay.butikknavn. 
        hExcel:ActiveSheet:Range(cCell[2]):VALUE = 
                  
                  STRING(DAY(SaleByDay.Dato)) + "." + 
                  STRING(MONTH(SaleByDay.Dato))   + "." +
                  STRING(YEAR(SaleByDay.Dato)).

        hExcel:ActiveSheet:Range(cCell[3]):VALUE = SaleByDay.antallSolgt.
        hExcel:ActiveSheet:Range(cCell[4]):VALUE = SaleByDay.BruttoSalg.
        hExcel:ActiveSheet:Range(cCell[5]):VALUE = SaleByDay.dbkr.
        hExcel:ActiveSheet:Range(cCell[6]):VALUE = SaleByDay.db% / 100.
    END. 
       
    /*
    hExcel:ActiveWorkbook:Save().
    IF lSaveAs THEN
    DO:
        cSaveAsFileName = "test.xls".
        hExcel:ActiveWorkbook:SaveAs(cSaveAsFileName,,,,,,,).
    END. */
    
    IF lOpenExcel THEN hExcel:visible = yes.  
    
    hExcel:ActiveWorkbook:SAVE.
    
    /* hExcel:ActiveWorkbook:SaveAs(cFileName,18,,,,,,,,,,).*/

    hExcel:QUIT.
    RELEASE OBJECT hExcel NO-ERROR.
    
 
