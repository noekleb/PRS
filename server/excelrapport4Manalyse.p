DEFINE TEMP-TABLE Md_Analyse NO-UNDO
    FIELD AvdelingNr AS INT
    FIELD AvdelingNavn AS CHAR 
    FIELD RapportManed AS INT 
    FIELD RapportAr  AS INT 
    FIELD NettoSalg AS DEC
    FIELD NettoSalgPrev AS DEC
    FIELD Db% AS DEC 
    FIELD Db%Prev AS DEC .

DEFINE TEMP-TABLE SaleByDay NO-UNDO 
    FIELD butikkNr AS INT 
    FIELD butikkNavn AS CHAR 
    FIELD Dato AS DATE
    FIELD AntallSolgt AS INT 
    FIELD BruttoSalg AS DEC
    FIELD BruttoSalg% AS DEC
    FIELD NettoSalg AS DEC 
    FIELD NettoSalg% AS DEC 
    FIELD MvaVerdi AS DEC 
    FIELD DbKr AS DEC  
    FIELD Db% AS DEC
    FIELD RabattAnt AS INT 
    FIELD RabattKr AS DEC
    FIELD Rabatt% AS DEC
    INDEX idx1 IS PRIMARY Dato .
                      
DEFINE TEMP-TABLE AkkumMd_Analyse NO-UNDO
    FIELD AvdelingNr AS INT 
    FIELD AvdelingNavn AS CHAR 
    FIELD RapportManed AS INT 
    FIELD RapportAr  AS INT 
    FIELD NettoSalg AS DEC
    FIELD NettoSalgPrev AS DEC
    FIELD dbkr AS DEC
    FIELD Db% AS DEC 
    FIELD dbkrPrev AS DEC 
    FIELD Db%Prev AS DEC.

DEFINE TEMP-TABLE Rullerende12md_Analyse NO-UNDO
    FIELD Avdelingnr AS INT
    FIELD AvdelingNavn AS CHAR 
    FIELD RapportManed AS INT 
    FIELD RapportAr  AS INT 
    FIELD NettoSalg AS DEC 
    FIELD BruttoSalg AS DEC
    FIELD DBKr AS DEC
    FIELD LagerVerdi AS DEC
    FIELD OmlopshastighetsMal AS DEC
    FIELD LagerVerdi2 AS DEC.


DEFINE TEMP-TABLE Rullerende12md_LeverandorAnalyse NO-UNDO
    FIELD LeverandorId AS INT
    FIELD LeverandorBeskrivelse AS CHAR
    FIELD AntallSalg AS INT 
    FIELD BruttoSalg12md AS DECIMAL
    FIELD Dbkr12md AS DEC
    FIELD LagerVerdi AS DEC.



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


DEFINE VARIABLE hExcel AS COM-HANDLE NO-UNDO. 
 
FUNCTION clearCell RETURNS LOGICAL 
  (INPUT ipCellList AS CHAR, 
   INPUT ipiSheet AS INT, 
   INPUT ipiRow AS INT):

  DEFINE VARIABLE cCell AS CHAR NO-UNDO. 
  DEFINE VARIABLE iCnt AS INT NO-UNDO. 

  DO iCnt = 1 TO NUM-ENTRIES(ipCellList):
      cCell = ENTRY(icnt,ipCellList) + STRING(ipiRow).
      hExcel:Sheets(ipiSheet):Range(cCell):VALUE = ''.
  END. 
END. 


DEFINE INPUT PARAMETER  ipcTemplateFileName AS CHAR NO-UNDO. 
DEFINE INPUT PARAMETER  iplOpenExcel AS LOGICAL NO-UNDO. 
DEFINE INPUT PARAMETER  ipcNewFileName AS CHAR NO-UNDO. 
DEFINE INPUT PARAMETER  ipcButikkNavn AS CHAR NO-UNDO. 
DEFINE INPUT PARAMETER  ipdDate AS DATE NO-UNDO. 
DEFINE INPUT PARAMETER TABLE FOR Md_Analyse. 
DEFINE INPUT PARAMETER TABLE FOR AkkumMd_Analyse. 
DEFINE INPUT PARAMETER TABLE FOR Rullerende12md_Analyse.
DEFINE INPUT PARAMETER TABLE FOR Rullerende12md_LeverandorAnalyse.
DEFINE INPUT PARAMETER TABLE FOR SaleByDay. 


    DEFINE VARIABLE rGUID AS RAW       NO-UNDO.
    DEFINE VARIABLE cFileName AS CHAR NO-UNDO. 
    DEFINE VARIABLE cTemplateFileName AS CHAR NO-UNDO. 
    DEFINE VARIABLE cTmpFileName AS CHAR NO-UNDO. 

    DEFINE VARIABLE cButikkNavn AS CHAR INIT "SPORT-TEST1" NO-UNDO.
    DEFINE VARIABLE iSalgsDager AS INT INIT 23 NO-UNDO. 
    DEFINE VARIABLE iTotSalgsDager AS INT INIT 27 NO-UNDO. 
    
    
    DEFINE VARIABLE cHeadingSheet1_1     AS CHAR INIT " &1 &2" NO-UNDO. 
    DEFINE VARIABLE cHeadingSheet1_2     AS CHAR INIT " &1 &2" NO-UNDO. 
    DEFINE VARIABLE cHeadingSheet2_1     AS CHAR INIT " &1-&2 &3" NO-UNDO. 
    DEFINE VARIABLE cHeadingSheet2_2     AS CHAR INIT " &1-&2 &3" NO-UNDO. 
    
    DEFINE VARIABLE cMonthShort AS CHAR EXTENT 12 INIT 
      ["Jan","Feb","Mar","Apr","Mai","Jun",
       "Jul","Aug","Sep","Okt","Nov","Des"] NO-UNDO.

    DEFINE VARIABLE cMonth AS CHAR EXTENT 12 INIT 
      ["Januar","Februar","Mars","April","Mai","Juni",
       "Juli","August","September","Oktober","Novmeber","Desember"] NO-UNDO.


    DEFINE VARIABLE cDay AS CHAR EXTENT 7 INIT 
      ["Mandag","Tirsdag","Onsdag","Torsdag","Fredag","Lørdag","Søndag"] NO-UNDO.

    /*
    DEFINE VARIABLE tAntallSalg AS DEC NO-UNDO.
    DEFINE VARIABLE tBruttoSalg12md AS DEC NO-UNDO.
    DEFINE VARIABLE tDbkr12md AS DEC NO-UNDO.
    DEFINE VARIABLE tLagerVerdi AS DEC NO-UNDO.
    */
    DEFINE VARIABLE iStartRow AS INT NO-UNDO. 
    DEFINE VARIABLE iRow AS INT NO-UNDO. 
    DEFINE VARIABLE iRow_count AS INT NO-UNDO. 
    DEFINE VARIABLE cCell AS CHAR EXTENT 20 NO-UNDO. 
    DEFINE VARIABLE lOpenExcel AS LOGICAL INIT TRUE NO-UNDO. 
    DEFINE VARIABLE lSaveAS AS LOGICAL INIT FALSE NO-UNDO. 
    DEFINE VARIABLE cSaveAsFileName AS CHAR NO-UNDO. 
    DEFINE VARIABLE cDirectory AS CHAR NO-UNDO. 


    ASSIGN 
       cButikkNavn = ipcButikkNavn
       lOpenExcel = iplOpenExcel
       cFileName = ipcNewFileName 
       cButikkNavn = ipcButikkNavn
       cTemplateFileName = ipcTemplateFileName 
       cDirectory = getDirectoryName(cFileName) 
       cFileName  = cDirectory + '\' + getFileName(cFileName)
       cTmpFileName  = cFileName.
  

    OS-COPY VALUE(cTemplateFileName) VALUE(cTmpFileName).
    FILE-INFO:FILE-NAME = cTmpFileName. 
    cTmpFileName = FILE-INFO:FULL-PATHNAME. 

    CREATE "Excel.Application" hExcel.
    hExcel:visible = FALSE.
    hExcel:Workbooks:OPEN(cTmpFileName,1,FALSE,,,,,,2,,,,,,).
    
    /* Sheet 1  Month - vs prev month analyze */ 

    iStartRow = 5.
    FOR EACH md_Analyse NO-LOCK : 
        cCell[1] = 'B' + STRING(iStartRow + iRow).
        cCell[2] = 'C' + STRING(iStartRow + iRow).
        cCell[3] = 'D' + STRING(iStartRow + iRow).
        cCell[4] = 'K' + STRING(iStartRow + iRow).
        cCell[5] = 'L' + STRING(iStartRow + iRow).
        iRow = iRow + 1.
        hExcel:Sheets(1):Range(cCell[1]):VALUE = md_analyse.AvdelingNavn.
        hExcel:Sheets(1):Range(cCell[2]):VALUE = md_analyse.nettoSalg.
        hExcel:Sheets(1):Range(cCell[3]):VALUE = md_analyse.nettosalgprev.
        hExcel:Sheets(1):Range(cCell[4]):VALUE = md_analyse.db% / 100.
        hExcel:Sheets(1):Range(cCell[5]):VALUE = md_analyse.db%prev / 100.
    END. 
    DO iRow_count = iRow TO 8: ClearCell('B,C,D,E,F,G,H,I,J,K,L,M',1,iStartRow + iRow_count). END.
    

    iRow = 0. 
    iStartRow = 28.
    FOR EACH SaleByDay NO-LOCK : 
        cCell[1] = 'B' + STRING(iStartRow + iRow).
        cCell[2] = 'C' + STRING(iStartRow + iRow).
        cCell[3] = 'D' + STRING(iStartRow + iRow).
        cCell[4] = 'E' + STRING(iStartRow + iRow).
        cCell[5] = 'F' + STRING(iStartRow + iRow).
        cCell[6] = 'G' + STRING(iStartRow + iRow).
        cCell[7] = 'H' + STRING(iStartRow + iRow).
        cCell[8] = 'I' + STRING(iStartRow + iRow).
        cCell[9] = 'J' + STRING(iStartRow + iRow).
        cCell[10] = 'K' + STRING(iStartRow + iRow).
        cCell[11] = 'L' + STRING(iStartRow + iRow).
        cCell[12] = 'M' + STRING(iStartRow + iRow).
        iRow = iRow + 1.
        hExcel:Sheets(1):Range(cCell[1]):VALUE = SaleByDay.butikknr. 
        hExcel:Sheets(1):Range(cCell[2]):VALUE = SaleByDay.butikknavn. 
        hExcel:Sheets(1):Range(cCell[3]):VALUE = STRING(DAY(SaleByDay.Dato)) + "." + 
                                                 STRING(MONTH(SaleByDay.Dato)) + "." +
                                                 STRING(YEAR(SaleByDay.Dato)).

        hExcel:Sheets(1):Range(cCell[4]):VALUE = SaleByDay.antallSolgt.
        hExcel:Sheets(1):Range(cCell[5]):VALUE = SaleByDay.bruttosalg.
        hExcel:Sheets(1):Range(cCell[6]):VALUE = SaleByDay.bruttosalg% / 100.
        hExcel:Sheets(1):Range(cCell[7]):VALUE = SaleByDay.mvaverdi.
        hExcel:Sheets(1):Range(cCell[8]):VALUE = SaleByDay.dbkr.
        hExcel:Sheets(1):Range(cCell[9]):VALUE = SaleByDay.db% / 100.
        hExcel:Sheets(1):Range(cCell[10]):VALUE = SaleByDay.Rabattant.
        hExcel:Sheets(1):Range(cCell[11]):VALUE = SaleByDay.Rabattkr.
        hExcel:Sheets(1):Range(cCell[12]):VALUE = SaleByDay.Rabatt% / 100 .

    END. 

    /* ----- heading sheet 1 ---- */
    cHeadingSheet1_1 = SUBSTITUTE(cHeadingSheet1_1,cMonthShort[MONTH(ipdDate)],STRING(YEAR(ipdDate))).
    cHeadingSheet1_2 = SUBSTITUTE(cHeadingSheet1_2,cMonthShort[MONTH(ipdDate)],STRING(YEAR(ipdDate) - 1)).
    hExcel:Sheets(1):Range('C3'):VALUE = cHeadingSheet1_1.
    hExcel:Sheets(1):Range('D3'):VALUE = cHeadingSheet1_2.
    hExcel:Sheets(1):Range('G3'):VALUE = cHeadingSheet1_1.
    hExcel:Sheets(1):Range('H3'):VALUE = cHeadingSheet1_2.
    hExcel:Sheets(1):Range('K3'):VALUE = cHeadingSheet1_1.
    hExcel:Sheets(1):Range('L3'):VALUE = cHeadingSheet1_2.


    /* --------------- Sheet 2 Salgsutviking pr. kategori  ------------- */ 
    iRow = 0. 
    iStartRow = 5.
    FOR EACH AkkumMd_Analyse NO-LOCK : 
        cCell[1] = 'B' + STRING(iStartRow + iRow).
        cCell[2] = 'C' + STRING(iStartRow + iRow).
        cCell[3] = 'D' + STRING(iStartRow + iRow).
        cCell[4] = 'K' + STRING(iStartRow + iRow).
        cCell[5] = 'L' + STRING(iStartRow + iRow).
        iRow = iRow + 1.
        hExcel:Sheets(2):Range(cCell[1]):VALUE = Akkummd_analyse.AvdelingNavn.
        hExcel:Sheets(2):Range(cCell[2]):VALUE = Akkummd_analyse.nettoSalg.
        hExcel:Sheets(2):Range(cCell[3]):VALUE = Akkummd_analyse.nettosalgprev.
        hExcel:Sheets(2):Range(cCell[4]):VALUE = Akkummd_analyse.db% / 100.
        hExcel:Sheets(2):Range(cCell[5]):VALUE = Akkummd_analyse.db%prev / 100.
    END. 

    cHeadingSheet2_1 = SUBSTITUTE(cHeadingSheet2_1,cMonthShort[1],cMonthShort[MONTH(ipdDate)],STRING(YEAR(ipdDate) - 1)).
    cHeadingSheet2_2 = SUBSTITUTE(cHeadingSheet2_2,cMonthShort[1],cMonthShort[MONTH(ipdDate)],STRING(YEAR(ipdDate))).

    hExcel:Sheets(2):Range('C3'):VALUE = cHeadingSheet2_2.
    hExcel:Sheets(2):Range('D3'):VALUE = cHeadingSheet2_1.

    hExcel:Sheets(2):Range('G3'):VALUE = cHeadingSheet2_2.
    hExcel:Sheets(2):Range('H3'):VALUE = cHeadingSheet2_1.

    hExcel:Sheets(2):Range('K3'):VALUE = cHeadingSheet2_1.
    hExcel:Sheets(2):Range('L3'):VALUE = cHeadingSheet2_2.

    DO iRow_count = iRow TO 8: ClearCell('B,C,D,E,F,G,H,I,J,K,L,M',2,iStartRow + iRow_count). END.


    /* --------------- Sheet 3 Lagerbeholdning pr. kategori  ----------- */ 
    iRow = 0. 
    iStartRow = 5.
    FOR EACH Rullerende12md_Analyse NO-LOCK : 
        cCell[1] = 'B' + STRING(iStartRow + iRow).
        cCell[2] = 'C' + STRING(iStartRow + iRow).
        cCell[3] = 'F' + STRING(iStartRow + iRow).
        cCell[4] = 'G' + STRING(iStartRow + iRow).
        cCell[5] = 'K' + STRING(iStartRow + iRow).
        cCell[6] = 'N' + STRING(iStartRow + iRow).
        
        iRow = iRow + 1.
        hExcel:Sheets(3):Range(cCell[1]):VALUE = Rullerende12md_Analyse.AvdelingNavn.
        hExcel:Sheets(3):Range(cCell[2]):VALUE = Rullerende12md_Analyse.BruttoSalg.
        hExcel:Sheets(3):Range(cCell[3]):VALUE = Rullerende12md_Analyse.dbkr.
        hExcel:Sheets(3):Range(cCell[4]):VALUE = Rullerende12md_Analyse.lagerverdi.
        hExcel:Sheets(3):Range(cCell[5]):VALUE = Rullerende12md_Analyse.omlopshastighetsmal.
        hExcel:Sheets(3):Range(cCell[6]):VALUE = Rullerende12md_Analyse.Lagerverdi2.
    END. 
    DO iRow_count = iRow TO 8: ClearCell('B,C,D,E,F,G,H,I,K,L,N,O',3,iStartRow + iRow_count). END.


    /* --------------- Sheet 4 Lagerbeholdning pr. kategori  ----------- */ 
    
    
    iRow = 0. 
    iStartRow = 4.
    FOR EACH Rullerende12md_LeverandorAnalyse NO-LOCK 
      BY Rullerende12md_LeverandorAnalyse.Dbkr12md DESCENDING : 
        cCell[1] = 'B' + STRING(iStartRow + iRow).
        cCell[2] = 'C' + STRING(iStartRow + iRow).
        cCell[3] = 'D' + STRING(iStartRow + iRow).
        cCell[4] = 'E' + STRING(iStartRow + iRow).
        cCell[5] = 'H' + STRING(iStartRow + iRow).
        cCell[6] = 'J' + STRING(iStartRow + iRow).
        iRow = iRow + 1.
        hExcel:Sheets(4):Range(cCell[1]):VALUE = Rullerende12md_LeverandorAnalyse.LeverandorId.
        hExcel:Sheets(4):Range(cCell[2]):VALUE = Rullerende12md_LeverandorAnalyse.LeverandorBeskrivelse.
        hExcel:Sheets(4):Range(cCell[3]):VALUE = Rullerende12md_LeverandorAnalyse.AntallSalg.
        hExcel:Sheets(4):Range(cCell[4]):VALUE = Rullerende12md_LeverandorAnalyse.BruttoSalg12md.
        hExcel:Sheets(4):Range(cCell[5]):VALUE = Rullerende12md_LeverandorAnalyse.Dbkr12md.
        hExcel:Sheets(4):Range(cCell[6]):VALUE = Rullerende12md_LeverandorAnalyse.LagerVerdi.

      /*  ASSIGN 
            tAntallSalg = tAntallSalg + Rullerende12md_LeverandorAnalyse.AntallSalg
            tBruttoSalg12md = tBruttoSalg12md + Rullerende12md_LeverandorAnalyse.BruttoSalg12md
            tDbkr12md = tDbkr12md + Rullerende12md_LeverandorAnalyse.Dbkr12md
            tLagerVerdi = tLagerVerdi + Rullerende12md_LeverandorAnalyse.LagerVerdi.
      */
    END. 
    
    DO iRow_count = iRow TO 531: ClearCell('B,C,D,E,F,G,H,I,J,K,L,M,O,Q,R,S',4,iStartRow + iRow_count). END.

    /* - Total Line for for sheet 4 ---- */ 
    iRow = iRow + 1.
    cCell[1] = 'B' + STRING(iStartRow + iRow).
    cCell[2] = 'C' + STRING(iStartRow + iRow).
    cCell[3] = 'D' + STRING(iStartRow + iRow).
    cCell[4] = 'E' + STRING(iStartRow + iRow).
    cCell[5] = 'F' + STRING(iStartRow + iRow).
    cCell[6] = 'G' + STRING(iStartRow + iRow).
    cCell[7] = 'H' + STRING(iStartRow + iRow).
    cCell[8] = 'I' + STRING(iStartRow + iRow).
    cCell[9] = 'J' + STRING(iStartRow + iRow).
    
    hExcel:Sheets(4):Range(cCell[1]):VALUE = ''.
    hExcel:Sheets(4):Range(cCell[2]):VALUE = ' SUM'.
    hExcel:Sheets(4):Range(cCell[3]):FormulaR1C1 = "=SUM(R[-" + STRING(iRow) + "]C:R[-1]C)".
    hExcel:Sheets(4):Range(cCell[4]):FormulaR1C1 = "=SUM(R[-" + STRING(iRow) + "]C:R[-1]C)".
    hExcel:Sheets(4):Range(cCell[5]):FormulaR1C1 = "=SUM(R[-" + STRING(iRow) + "]C:R[-1]C)".
    hExcel:Sheets(4):Range(cCell[6]):FormulaR1C1 = "=SUM(R[-" + STRING(iRow) + "]C:R[-1]C)".
    hExcel:Sheets(4):Range(cCell[7]):FormulaR1C1 = "=SUM(R[-" + STRING(iRow) + "]C:R[-1]C)".
    hExcel:Sheets(4):Range(cCell[8]):FormulaR1C1 = "=SUM(R[-" + STRING(iRow) + "]C:R[-1]C)".
    hExcel:Sheets(4):Range(cCell[9]):FormulaR1C1 = "=SUM(R[-" + STRING(iRow) + "]C:R[-1]C)".
   
    IF lOpenExcel THEN hExcel:visible = yes.  
    
    hExcel:ActiveWorkbook:SAVE.

    hExcel:QUIT.
    RELEASE OBJECT hExcel NO-ERROR.

    
 
