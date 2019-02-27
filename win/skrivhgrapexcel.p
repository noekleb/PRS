&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
/* Connected Databases 
          skotex           PROGRESS
*/

/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE TT_dags_rap NO-UNDO LIKE dags_rap
       FIELD RadTotal AS DECI DECIMALS 2.


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : 
    Purpose     :

    Syntax      :

    Description :

    Author(s)   :
    Created     :
    Notes       :
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE INPUT  PARAMETER wKunde       AS CHARACTER  NO-UNDO.
DEFINE INPUT  PARAMETER wTittel      AS CHARACTER  NO-UNDO.
DEFINE INPUT  PARAMETER wKriterier   AS CHARACTER  NO-UNDO.
DEFINE INPUT  PARAMETER wButikkLbl   AS CHARACTER  NO-UNDO.
DEFINE INPUT  PARAMETER wKollonneLbl AS CHARACTER  NO-UNDO.
DEFINE INPUT  PARAMETER wSideLbl     AS CHARACTER  NO-UNDO.
DEFINE INPUT  PARAMETER iFraButik LIKE Butiker.Butik NO-UNDO.
DEFINE INPUT  PARAMETER iTilButik LIKE Butiker.Butik NO-UNDO.
DEFINE INPUT  PARAMETER dFraDato     AS DATE       NO-UNDO.
DEFINE INPUT  PARAMETER dTilDato     AS DATE       NO-UNDO.
DEFINE INPUT  PARAMETER cAkkumulering AS CHARACTER  NO-UNDO. /*1 = dag 2 = man */
DEF STREAM Eksport.


{runlib.i}
{windows.i}
{methodexcel.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Procedure
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: CODE-ONLY COMPILE
   Temp-Tables and Buffers:
      TABLE: TT_dags_rap T "?" NO-UNDO skotex dags_rap
      ADDITIONAL-FIELDS:
          FIELD RadTotal AS DECI DECIMALS 2
      END-FIELDS.
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Procedure ASSIGN
         HEIGHT             = 15
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */
RUN SkapaTT.
RUN ExportToExcel.
OUTPUT CLOSE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-ExportToExcel) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ExportToExcel Procedure 
PROCEDURE ExportToExcel :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR cExcEkstent  AS CHAR NO-UNDO.
  DEF VAR ctmpFileName AS CHAR NO-UNDO.
  DEF VAR cButikRader  AS CHAR NO-UNDO.
  DEF VAR iButikRad    AS INTE NO-UNDO.
  DEF VAR iCount1      AS INTE NO-UNDO.
  DEF VAR iCount2      AS INTE NO-UNDO.
  DEF VAR cPara        AS CHAR NO-UNDO.
  DEF VAR cPar1        AS CHAR NO-UNDO.
  DEF VAR cPar2        AS CHAR NO-UNDO.
  DEF VAR cPar3        AS CHAR NO-UNDO.
  {syspara.i 1 4 1 cExcEkstent}
  ASSIGN cExcEkstent = if cExcEkstent = "" then "sdv" else cExcEkstent
         wKollonneLbl = TRIM(ENTRY(2,wKollonneLbl,"=")).    
  if valid-handle(wLibHandle) then
    run GetTempFileName in wLibHandle (input "hgrapp", input cExcEkstent, output ctmpFileName).
  output stream Eksport to value(ctmpFileName).
  {sww.i}
  EXPORT STREAM Eksport  DELIMITER ";" wTittel.
  EXPORT STREAM Eksport  DELIMITER ";" wKunde.
  EXPORT STREAM Eksport  DELIMITER ";" wKriterier.
  ASSIGN iButikRad = 3.
  FOR EACH TT_dags_rap BREAK BY butikk:
          IF FIRST-OF(TT_dags_rap.butikk) THEN DO:
              FIND Butiker WHERE Butiker.Butik = TT_dags_rap.butikk NO-LOCK.
              EXPORT STREAM Eksport  DELIMITER ";" "".
              EXPORT STREAM Eksport  DELIMITER ";" "".
              ASSIGN iButikRad = iButikRad + 3
                     cButikRader = cButikRader + (IF cButikRader = "" THEN "" ELSE ",") + STRING(iButikRad).
              EXPORT STREAM Eksport  DELIMITER ";" wButikkLbl + " " + STRING(TT_dags_rap.butikk) + " " + Butiker.ButNamn.
              ASSIGN iButikRad = iButikRad + 1.
              EXPORT STREAM Eksport  DELIMITER ";" ENTRY(1,wKollonneLbl)
                                                   ENTRY(2,wKollonneLbl)
                                                   "%"
                                                   ENTRY(3,wKollonneLbl)
                                                   "%"
                                                   ENTRY(4,wKollonneLbl)
                                                   "%"
                                                   ENTRY(5,wKollonneLbl)
                                                   "%"
                                                   ENTRY(6,wKollonneLbl)
                                                   "%"
                                                   ENTRY(7,wKollonneLbl)
                                                   "%"
                                                   ENTRY(8,wKollonneLbl)
                                                   ENTRY(9,wKollonneLbl).
          END.
          ASSIGN iButikRad = iButikRad + 1.
          EXPORT STREAM Eksport DELIMITER ";" 
              IF cAkkumulering = "1" THEN 
                   STRING(TT_dags_rap.dato,"99/99/9999") ELSE
                   STRING(MONTH(TT_dags_rap.dato),"99") + "/" + STRING(YEAR(TT_dags_rap.dato))
                   ROUND(hg1_oms,0)
                   ROUND(hg1_oms * 100 / RadTotal,0)
                   ROUND(hg2_oms,0)
                   ROUND(hg2_oms * 100 / RadTotal,0)
                   ROUND(hg3_oms,0)
                   ROUND(hg3_oms * 100 / RadTotal,0)
                   ROUND(hg4_oms,0)
                   ROUND(hg4_oms * 100 / RadTotal,0)
                   ROUND(hg5_oms,0)
                   ROUND(hg5_oms * 100 / RadTotal,0)
                   ROUND(hg6_oms,0)
                   ROUND(hg6_oms * 100 / RadTotal,0)
                   ROUND(retur_korr,0)
                   ROUND(RadTotal,0).
  END.
  {swn.i}
  output stream Eksport close.
  STATUS DEFAULT "Importerer data i Excel...".
  CREATE "Excel.Application" chExcelApplication.  
  chExcelApplication:Visible = FALSE.                                     
  chWorkbooks = chExcelApplication:Workbooks:OpenText(ctmpFileName,2,1,1,1,1,FALSE,TRUE,FALSE,FALSE,FALSE).
  STATUS DEFAULT "Setter aktivt ark...".
  chWorkSheets = chExcelApplication:Sheets:Item(1).

  STATUS DEFAULT "Setter overskrift...".
  chWorkSheets:Range("A1:M3"):Font:Bold = TRUE.
  chWorkSheets:Range("A1:M3"):Font:Italic = TRUE.

/*
  STATUS DEFAULT "Blokkinndeling...".
  chWorkSheets:Range("C:C"):borders(10):LineStyle   = 9. /*** Dobbelt linje ****/
  chWorkSheets:Range("D:D"):borders(10):LineStyle   = 9. /*** Dobbelt linje ****/
  chWorkSheets:Range("J:J"):borders(10):LineStyle   = 9. /*** Dobbelt linje ****/
  chWorkSheets:Range("M:M"):borders(10):LineStyle   = 9. /*** Dobbelt linje ****/
  chWorkSheets:Range("A3:M3"):borders(10):LineStyle   = 9. /*** Dobbelt linje ****/
*/  
  STATUS DEFAULT "Setter nummerformat...".
  chWorkSheets:Range("B:O"):NumberFormat = "# ##0".
  /*
  chWorkSheets:Range("F:F"):NumberFormat = "# ##0,0".
  chWorkSheets:Range("G:G"):NumberFormat = "# ##0,00".
  chWorkSheets:Range("H:H"):NumberFormat = "# ##0,0".
  chWorkSheets:Range("I:I"):NumberFormat = "# ##0".
  chWorkSheets:Range("J:J"):NumberFormat = "# ##0,0".
  chWorkSheets:Range("K:K"):NumberFormat = "# ##0,0".
  chWorkSheets:Range("L:L"):NumberFormat = "# ##0,0".
  chWorkSheets:Range("M:M"):NumberFormat = "# ##0,00".
   */
  STATUS DEFAULT "Setter overskrift...".
  chWorkSheets:Range("A1:O1"):Merge().
  chWorkSheets:Range("A2:O2"):Merge().
  chWorkSheets:Range("A3:O3"):Merge().
  DO iCount1 = 1 TO NUM-ENTRIES(cButikRader):
      chWorkSheets:Range("A" + ENTRY(iCount1,cButikRader) + ":O" + ENTRY(iCount1,cButikRader)):Merge().
      chWorkSheets:Range("A" + ENTRY(iCount1,cButikRader) + ":D" + ENTRY(iCount1,cButikRader)):Font:Bold = TRUE.
      chWorkSheets:Range("A" + STRING(INT(ENTRY(iCount1,cButikRader)) + 1) + ":O" + STRING(INT(ENTRY(iCount1,cButikRader)) + 1)):Font:Bold = TRUE.
  END.
  /*
  chWorkSheets:Range("A1:C1"):HorizontalAlignment = 3.
        
  chWorkSheets:Range("C3:C3"):HorizontalAlignment = 4.
  chWorkSheets:Range("E2:E2"):HorizontalAlignment = 4.
  chWorkSheets:Range("E3:E3"):HorizontalAlignment = 4.   
  
  chWorkSheets:Range("F3:F3"):HorizontalAlignment = 4.   
  
  chWorkSheets:Range("G2:G2"):HorizontalAlignment = 4.
  chWorkSheets:Range("G3:G3"):HorizontalAlignment = 4.   
  
  chWorkSheets:Range("H3:H3"):HorizontalAlignment = 4.   

  chWorkSheets:Range("I2:I2"):HorizontalAlignment = 4.
  chWorkSheets:Range("I3:I3"):HorizontalAlignment = 4.   
  
  chWorkSheets:Range("J3:J3"):HorizontalAlignment = 4.   
  
  chWorkSheets:Range("K3:K3"):HorizontalAlignment = 4.   
  chWorkSheets:Range("L3:L3"):HorizontalAlignment = 4.   
  chWorkSheets:Range("M3:M3"):HorizontalAlignment = 4.   
*/  
  STATUS DEFAULT "Setter AutoFit...".
  chWorkSheets:Columns("A:O"):AutoFit().
/*
  STATUS DEFAULT "Setter Kriterier...".
  chWorkSheets:PageSetup:PrintTitleRows = "A1:X3".  
  chWorkSheets:PageSetup:LeftHeader     = "Kriterier - Periode: " + string(FI-FraDato) + " - " + 
                                                         STRING(FI-TilDato) + ", " +
                                                         "Butikker: " + FI-Butiker + ", " + 
                                                         "UkeDager: " + wUkeDager.
  chWorkSheets:PageSetup:RightHeader    = "Antall poster: " + String(wAntPoster).
  chWorkSheets:PageSetup:LeftFooter     = wKunde.
  chWorkSheets:PageSetup:RightFooter    = wSkoTex. */
  chWorksheets:PageSetup:PrintArea      = "A:O".
/*
  chWorkSheets:PageSetup:Orientation    = 2.
  chWorkSheets:PageSetup:FitToPagesWide = 1.
  
  STATUS DEFAULT "Setter FreezePanes...".
  chWorkSheets:Range("A4"):Select().
  chExcelApplication:ActiveWindow:FreezePanes = True.
  
  /* Legger inn sumlinjer. */                                        
  /* Excel macro som gjør jobben.
  Range("A4").Select
    Selection.Subtotal GroupBy:=1, Function:=xlSum, TotalList:=Array(5, 7, 9), _
        Replace:=True, PageBreaks:=True, SummaryBelowData:=True  
  */
  */
   DO iCount1 = 1 TO NUM-ENTRIES(cButikRader) - 1:
      ASSIGN cPar2 = STRING(INT(ENTRY(iCount1,cButikRader)) + 2)
             cPar3 = STRING(INT(ENTRY(iCount1 + 1,cButikRader)) - 2).
      DO iCount2 = 1 TO NUM-ENTRIES("B,D,F,H,J,L,N,O"):
          ASSIGN cPar1 = ENTRY(iCount2,"B,D,F,H,J,L,N,O")
                 cPara = SUBSTITUTE("&1&2:&1&3",cPar1,cPar2,cPar3).
          chWorkSheets:Range(cPara):Select.
          chWorkSheets:Range(cPar1 + cPar3):Activate.
          chExcelApplication:ActiveCell:FormulaR1C1 = SUBSTITUTE("=SUM(R[-&1]C:R[-1]C)",STRING(INT(cPar3) - INT(cPar2))).
      END.
   END.
   ASSIGN iCount1 = NUM-ENTRIES(cButikRader)
          cPar2   = STRING(INT(ENTRY(iCount1,cButikRader)) + 2)
          cPar3   = STRING(iButikRad + 1).
   DO iCount2 = 1 TO NUM-ENTRIES("B,D,F,H,J,L,N,O"):
          ASSIGN cPar1 = ENTRY(iCount2,"B,D,F,H,J,L,N,O")
                 cPara = SUBSTITUTE("&1&2:&1&3",cPar1,cPar2,cPar3).
          chWorkSheets:Range(cPara):Select.
          chWorkSheets:Range(cPar1 + cPar3):Activate.
          chExcelApplication:ActiveCell:FormulaR1C1 = SUBSTITUTE("=SUM(R[-&1]C:R[-1]C)",STRING(INT(cPar3) - INT(cPar2))).
   END.
   chWorkSheets:Range("A1:O1"):Select.
   chExcelApplication:Visible = TRUE.
  
  RELEASE OBJECT chWorksheets NO-ERROR.            /* release com-handles */
  RELEASE OBJECT chWorkbooks NO-ERROR.             /* release com-handles */
  RELEASE OBJECT chExcelApplication NO-ERROR.      /* release com-handles */
  ASSIGN chWorksheets       = ?
         chWorkbooks        = ?
         chExcelApplication = ?.
  STATUS DEFAULT "".
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SkapaTT) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SkapaTT Procedure 
PROCEDURE SkapaTT :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   FOR EACH Butiker WHERE Butiker.Butik >= iFraButik AND
                          Butiker.Butik <= iTilButik:
       FOR EACH dags_rap WHERE dags_rap.Butikk = Butiker.Butik AND
                                          dags_rap.dato >= dFraDato AND
                                          dags_rap.dato <= dTilDato:
           FIND TT_dags_rap WHERE TT_dags_rap.Butikk = dags_rap.Butikk AND
                                  TT_dags_rap.Dato   = IF cAkkumulering = "1" THEN
                  dags_rap.dato ELSE DATE(MONTH(dags_rap.dato),1,YEAR(dags_rap.dato)) NO-ERROR.
           IF NOT AVAIL TT_dags_rap THEN DO:
               CREATE TT_dags_rap.
               ASSIGN TT_dags_rap.Butikk = dags_rap.Butikk
                      TT_dags_rap.dato   = IF cAkkumulering = "1" THEN
                  dags_rap.dato ELSE DATE(MONTH(dags_rap.dato),1,YEAR(dags_rap.dato)).
           END.
           ASSIGN TT_dags_rap.hg1_oms = TT_dags_rap.hg1_oms + dags_rap.hg1_oms
                  TT_dags_rap.hg2_oms = TT_dags_rap.hg2_oms + dags_rap.hg2_oms
                  TT_dags_rap.hg3_oms = TT_dags_rap.hg3_oms + dags_rap.hg3_oms
                  TT_dags_rap.hg4_oms = TT_dags_rap.hg4_oms + dags_rap.hg4_oms
                  TT_dags_rap.hg5_oms = TT_dags_rap.hg5_oms + dags_rap.hg5_oms
                  TT_dags_rap.hg6_oms = TT_dags_rap.hg6_oms + dags_rap.hg6_oms
                  TT_dags_rap.retur_korr = TT_dags_rap.retur_korr + dags_rap.retur_korr
                  TT_dags_rap.RadTotal   = TT_dags_rap.RadTotal + dags_rap.hg1_oms         
                                                                + dags_rap.hg2_oms         
                                                                + dags_rap.hg3_oms         
                                                                + dags_rap.hg4_oms         
                                                                + dags_rap.hg5_oms         
                                                                + dags_rap.hg6_oms         
                                                                + dags_rap.retur_korr.
       END.
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

