/* Eksport av budsjett til Excel 
   Parameter:  <PkSdlId>;<brukerid>
   Opprettet: 12.10.15 av TN              
-----------------------------------------------------------------------------------*/
DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.

DEF VAR hQuery   AS HANDLE NO-UNDO.
DEF VAR iSBudId  AS INT  NO-UNDO.
DEF VAR cUtfil   AS CHAR NO-UNDO.
DEF VAR cKatalog AS CHAR NO-UNDO.
DEF VAR cAar     AS CHAR NO-UNDO.
DEF VAR cMnd     AS CHAR NO-UNDO.
DEF VAR cDag     AS CHAR NO-UNDO.
DEF VAR cDato    AS CHAR NO-UNDO.
DEF VAR iLoop    AS INT  NO-UNDO.

/*--- local variable definitions ---*/
DEFINE VARIABLE cColList                 AS CHARACTER                 NO-UNDO.
DEFINE VARIABLE cRange                   AS CHARACTER                 NO-UNDO.
DEFINE VARIABLE cRow                     AS CHARACTER                 NO-UNDO.
DEFINE VARIABLE cPropEntry               AS CHARACTER                 NO-UNDO.
DEFINE VARIABLE cPropName                AS CHARACTER                 NO-UNDO.
DEFINE VARIABLE cPropValue               AS CHARACTER                 NO-UNDO.
DEFINE VARIABLE cFontName                AS CHARACTER                 NO-UNDO.
DEFINE VARIABLE chExcelApplication       AS COM-HANDLE                NO-UNDO.
DEFINE VARIABLE chWorkbook               AS COM-HANDLE                NO-UNDO.
DEFINE VARIABLE chWorkSheet              AS COM-HANDLE                NO-UNDO.
DEFINE VARIABLE hqTT                     AS HANDLE                    NO-UNDO.
DEFINE VARIABLE iRow                     AS INTEGER                   NO-UNDO.
DEFINE VARIABLE iCol                     AS INTEGER                   NO-UNDO.
DEFINE VARIABLE iPropNo                  AS INTEGER                   NO-UNDO.
DEFINE VARIABLE iPos                     AS INTEGER                   NO-UNDO.
DEFINE VARIABLE iFontSize                AS INTEGER                   NO-UNDO.

DEF VAR picExcelFileName AS CHAR NO-UNDO.

DEFINE VARIABLE chExcel    AS COM-HANDLE NO-UNDO.

DEF STREAM UT.

/*--- set the column attributes for the Worksheet ---*/
ASSIGN cColList = 'A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z'
                + ',AA,AB,AC,AD,AE,AF,AG,AH,AI,AJ,AK,AL,AM'
                + ',AN,AO,AP,AQ,AR,AS,AT,AU,AV,AW,AX,AY,AZ'
                + ',BA,BB,BC,BD,BE,BF,BG,BH,BI,BJ,BK,BL,BM'
                + ',BN,BO,BP,BQ,BR,BS,BT,BU,BV,BW,BX,BY,BZ'
                + ',CA,CB,CC,CD,CE,CF,CG,CH,CI,CJ,CK,CL,CM'
                + ',CN,CO,CP,CQ,CR,CS,CT,CU,CV,CW,CX,CY,CZ'
                + ',DA,DB,DC,DD,DE,DF,DG,DH,DI,DJ,DK,DL,DM'
                + ',DN,DO,DP,DQ,DR,DS,DT,DU,DV,DW,DX,DY,DZ'
                + ',EA,EB,EC,ED,EE,EF,EG,EH,EI,EJ,EK,EL,EM'
                + ',EN,EO,EP,EQ,ER,ES,ET,EU,EV,EW,EX,EY,EZ'
                + ',FA,FB,FC,FD,FE,FF,FG,FH,FI,FJ,FK,FL,FM'
                + ',FN,FO,FP,FQ,FR,FS,FT,FU,FV,FW,FX,FY,FZ'
                + ',GA,GB,GC,GD,GE,GF,GG,GH,GI,GJ,GK,GL,GM'
                + ',GN,GO,GP,GQ,GR,GS,GT,GU,GV,GW,GX,GY,GZ'.

ASSIGN
    obOK     = TRUE
    iSBudId  = INT(ENTRY(1,icParam,';'))
    cKatalog = 'log\'
    cUtFil   = 'SBud' + 
               REPLACE(STRING(TODAY,"99/99/99"),'/','') + 
               REPLACE(STRING(TIME,"HH:MM:SS"),':','') + 
               '.csv'
    . 

CREATE "Excel.Application" chExcel NO-ERROR.
 
IF ERROR-STATUS:ERROR = TRUE THEN
  /* do error handling here: log error, exit, etc. */
  DO:
    ocReturn = "Microsoft Excel er ikke tilgjengelig.".
    RETURN.
  END.
 
IF cFontName = '' THEN
  ASSIGN cFontName = "Arial Narrow".

ASSIGN chWorkbook  = chExcel:Workbooks:Add()
       chWorksheet = chExcel:Sheets:Item(1).
 
chWorksheet:Range("a2"):Select.

CREATE QUERY hQuery.
hQuery:SET-BUFFERS(ihBuffer).
hQuery:QUERY-PREPARE("FOR EACH " + ihBuffer:NAME + " WHERE true").
hQuery:QUERY-OPEN().

DO ON ERROR UNDO, LEAVE:

  hQuery:GET-FIRST().
  IF NOT ihBuffer:AVAIL THEN 
  DO:
    ocReturn = "Ingen budsjett valgt".
    UNDO, LEAVE.  
  END. 
  
  FIND LAST SBudHode NO-LOCK WHERE 
       SBudHode.SBudId = INT(ihBuffer:BUFFER-FIELD("SBudId"):BUFFER-VALUE)
       NO-ERROR.
  FIND Butiker NO-LOCK WHERE
      Butiker.Butik = SBudHode.ButikkNR NO-ERROR.

  RUN tmpAar(OUTPUT cAar).
  picExcelFileName = 'SBUD' + Butiker.ButNamn + '_' + cAar + '.xlsx'.  

  OUTPUT STREAM Ut TO VALUE(cKatalog + cUtFil) NO-ECHO.

  PUT STREAM Ut UNFORMATTED 
    "Butikk;"
    "Navn;"
    "SBudId;"
    "År;"
    "Måned;"
    "SalgBudsjett;"
    "SalgProsent;"
    "Dato;"
    "Dag;" 
    "SalgBudsjett;"
    "SalgProsent;"
    "DbBudsjett;"
    "DbProsent"
  SKIP.

  iLoop = 1. /* Tittelrad. */

  ASSIGN iRow = 1.
  ALLE_MANEDER:
  FOR EACH SBudManed NO-LOCK WHERE
      SBudManed.SBudId = SBudHode.SBudId:
      
      FOR EACH SBudDag OF SBudManed NO-LOCK:
          RUN tmpcUDag(OUTPUT cDag).
          RUN tmpdMDag(OUTPUT cDato).
          RUN tmpAar(OUTPUT cAar).
          RUN tmpMnd(OUTPUT cMnd).
          
          PUT STREAM Ut UNFORMATTED 
              SBudHode.ButikkNr ';'
              Butiker.ButNamn ';'
              SBudManed.SBudId ';'
              cAar ';'
              cMnd ';'
              SBudManed.SalgBudsjett ';'
              SBudManed.SalgProsent ';'
              cDato ';'
              cDag ';' 
              SBudDag.SalgBudsjett ';'
              SBudDag.SalgProsent ';'
              SBudDag.DbBudsjett ';'
              SBudDag.DbProsent
          SKIP.

          ASSIGN iRow = iRow + 1
                 cRow = STRING(iRow).
          DO iCol = 1 TO 13:
              CASE iCol:
                  WHEN  1 THEN chWorkSheet:Range(ENTRY(iCol,cColList) + cRow):Value = SBudHode.ButikkNr.
                  WHEN  2 THEN chWorkSheet:Range(ENTRY(iCol,cColList) + cRow):Value = Butiker.ButNamn.
                  WHEN  3 THEN chWorkSheet:Range(ENTRY(iCol,cColList) + cRow):Value = SBudManed.SBudId.
                  WHEN  4 THEN chWorkSheet:Range(ENTRY(iCol,cColList) + cRow):Value = cAar.
                  WHEN  5 THEN chWorkSheet:Range(ENTRY(iCol,cColList) + cRow):Value = cMnd.
                  WHEN  6 THEN chWorkSheet:Range(ENTRY(iCol,cColList) + cRow):Value = SBudManed.SalgBudsjett.
                  WHEN  7 THEN chWorkSheet:Range(ENTRY(iCol,cColList) + cRow):Value = SBudManed.SalgProsent.
                  WHEN  8 THEN chWorkSheet:Range(ENTRY(iCol,cColList) + cRow):Value = cDato.
                  WHEN  9 THEN chWorkSheet:Range(ENTRY(iCol,cColList) + cRow):Value = cDag.
                  WHEN 10 THEN chWorkSheet:Range(ENTRY(iCol,cColList) + cRow):Value = SBudDag.SalgBudsjett.
                  WHEN 11 THEN chWorkSheet:Range(ENTRY(iCol,cColList) + cRow):Value = SBudDag.SalgProsent.
                  WHEN 12 THEN chWorkSheet:Range(ENTRY(iCol,cColList) + cRow):Value = SBudDag.DbBudsjett.
                  WHEN 13 THEN chWorkSheet:Range(ENTRY(iCol,cColList) + cRow):Value = SBudDag.DbProsent.
              END CASE.
          END.
          chExcel:Selection:Offset(1,0):Select. /* Gir ny rad. */
      END.
  END. /* ALLE_MANEDER */

  OUTPUT STREAM Ut CLOSE.
END. 
DELETE OBJECT hQuery NO-ERROR.

chWorksheet:Range("a2"):Select.
ASSIGN
    chExcel:ActiveWindow:FreezePanes               = TRUE
    chWorksheet:Range("a1:m1"):Font:Bold           = TRUE
    chWorksheet:Range("a1:m1"):Interior:ColorIndex = 15.
    .
ASSIGN 
    chWorksheet:Range("a1"):Value = "Butikk"
    chWorksheet:Range("b1"):Value = "Navn"
    chWorksheet:Range("c1"):Value = "SBudId"
    chWorksheet:Range("d1"):Value = "År"
    chWorksheet:Range("e1"):Value = "Måned"
    chWorksheet:Range("f1"):Value = "SalgBudsjett"
    chWorksheet:Range("g1"):Value = "SalgProsent"
    chWorksheet:Range("h1"):Value = "Dato"
    chWorksheet:Range("i1"):Value = "Dag" 
    chWorksheet:Range("j1"):Value = "SalgBudsjett"
    chWorksheet:Range("k1"):Value = "SalgProsent"
    chWorksheet:Range("l1"):Value = "DbBudsjett"
    chWorksheet:Range("m1"):Value = "DbProsent"
    .
/* DO iCol = 1 TO 13:                                                                          */
/*   /*chWorkSheet:Range(ENTRY(iCol,cColList) + "1"):Value = pihTT:BUFFER-FIELD(iCol):LABEL.*/ */
/*   chWorkSheet:Columns(ENTRY(iCol,cColList)):Font:Name = cFontName.                          */
/*   IF iFontSize > 0 THEN                                                                     */
/*     chWorkSheet:Columns(ENTRY(iCol,cColList)):Font:Size = iFontSize.                        */
/*   IF CAN-DO('6,7,10,11,12,13',STRING(iCol)) THEN                                            */
/*       chWorkSheet:Columns(ENTRY(iCol,cColList)):Cells:NumberFormat = "#,###,##0.00".        */
/*   ELSE                                                                                      */
/*       chWorkSheet:Columns(ENTRY(iCol,cColList)):Cells:NumberFormat = "@".                   */
/* END.                                                                                        */

chWorksheet:Columns("a:m"):Select.
chExcel:Selection:Columns:AutoFit.
chWorksheet:Columns("a:a"):Select.
 
/*
/*--- save the result file in excel format ---*/
IF picExcelFileName > '' THEN DO:
  chWorkBook:SaveCopyAs(picExcelFileName).
  /*chWorkBook:Close(NO).*/
  /*chExcel:Quit().*/
END.
*/

chExcel:Visible = TRUE.

RELEASE OBJECT chWorksheet.
RELEASE OBJECT chWorkbook.
RELEASE OBJECT chExcel.

/* ------------ Procedure definisjoner ---------------- */
PROCEDURE tmpcUDag: 
  DEF OUTPUT PARAMETER pcDag AS CHAR NO-UNDO.

  DEFINE VARIABLE cDagLst AS CHARACTER NO-UNDO.
  DEFINE VARIABLE iWDay   AS INTEGER   NO-UNDO.
  DEFINE VARIABLE dDato   AS DATE NO-UNDO.
      
    
    {syspara.i 23 1 2 cDagLst}
    IF cDagLst = '' THEN cDagLst = 'SØN,MAN,TIR,ONS,TOR,FRE,LØR'.
    
  IF AVAIL SBudDag THEN
  DO:
    ASSIGN 
      dDato   = DATE (INT(SUBSTRING(STRING(SBudDag.AarMndDag,'99999999'),5,2)), 
                 INT(SUBSTRING(STRING(SBudDag.AarMndDag,'99999999'),7,2)), 
                 INT(SUBSTRING(STRING(SBudDag.AarMndDag,'99999999'),1,4)))
      iWDay   = WEEKDAY(dDato)
      pcDag = ' ' + ENTRY (iWDay,cDagLst)
      NO-ERROR.
  END.

END PROCEDURE.

PROCEDURE tmpdMDag:
  DEF OUTPUT PARAMETER pcDag AS CHAR NO-UNDO.
  
  DEFINE VARIABLE dDato   AS DATE NO-UNDO.
  
  IF AVAIL SBudDag THEN 
  DO:
    ASSIGN 
      pcDag = SUBSTRING(STRING(SBudDag.AarMndDag,'99999999'),7,2) + '.' + 
              SUBSTRING(STRING(SBudDag.AarMndDag,'99999999'),5,2) + '.' + 
              SUBSTRING(STRING(SBudDag.AarMndDag,'99999999'),1,4)
      NO-ERROR.
  END.

END PROCEDURE.

PROCEDURE tmpAar:
  DEF OUTPUT PARAMETER cAar AS CHAR NO-UNDO.

  IF AVAIL SBudManed THEN 
      cAar = SUBSTRING(STRING(SBudManed.AarMnd,'999999'),1,4).      
  ELSE 
      cAar = ''.

END PROCEDURE.

PROCEDURE tmpMnd:
  DEF OUTPUT PARAMETER cMnd AS CHAR NO-UNDO.

  DEFINE VARIABLE cMndLst AS CHARACTER NO-UNDO.
  DEFINE VARIABLE iMnd    AS INTEGER   NO-UNDO.
    
    {syspara.i 23 1 1 cMndLst}
    IF cMndLst = '' THEN cMndLst = 'JAN,FEB,MAR,APR,MAI,JUN,JUL,AUG,SEP,OKT,NOV,DES'.
    
  IF AVAIL SBudManed THEN 
      ASSIGN 
        iMnd = INT(SUBSTRING(STRING(SBudManed.AarMnd,'999999'),5,2))
        cMnd = ' ' + ENTRY (iMnd,cMndLst)
        NO-ERROR.

END PROCEDURE.

