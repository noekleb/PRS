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
DEFINE INPUT  PARAMETER wKunde        AS CHARACTER FORMAT "X(50)" NO-UNDO.
DEFINE INPUT  PARAMETER wTittel       AS CHARACTER FORMAT "X(20)" NO-UNDO.
DEFINE INPUT  PARAMETER wKriterier    AS CHARACTER FORMAT "X(50)" NO-UNDO.
DEFINE INPUT  PARAMETER wButikkLbl    AS CHARACTER FORMAT "X(50)" NO-UNDO.
DEFINE INPUT  PARAMETER wKollonneLbl  AS CHARACTER  NO-UNDO.
DEFINE INPUT  PARAMETER wSideLbl      AS CHARACTER  NO-UNDO.
DEFINE INPUT  PARAMETER iFraButik LIKE Butiker.Butik NO-UNDO.
DEFINE INPUT  PARAMETER iTilButik LIKE Butiker.Butik NO-UNDO.
DEFINE INPUT  PARAMETER dFraDato      AS DATE       NO-UNDO.
DEFINE INPUT  PARAMETER dTilDato      AS DATE       NO-UNDO.
DEFINE INPUT  PARAMETER cAkkumulering AS CHARACTER  NO-UNDO. /*1 = dag 2 = man */
DEFINE INPUT  PARAMETER cVi           AS CHARACTER  NO-UNDO.

DEFINE VARIABLE         cRub          AS CHARACTER EXTENT 11 FORMAT "X(15)" NO-UNDO.
DEFINE VARIABLE         cOurFirma     AS CHARACTER FORMAT "X(40)"           NO-UNDO.
DEFINE VARIABLE         iButikRad     AS INTEGER   INIT 8                   NO-UNDO.
/* DEF STREAM Eksport. */

  DEFINE FRAME PageHeader
     HEADER
        "<ALIGN=BASE><FArial><R4><P20><B><C1><CENTER=C110>" wTittel "<P12></B><C108><P10>" PAGE-NUMBER FORMAT ">>"
        "<R5><C6><P12><B>" wKunde "</B><P10>"
        "<R6><C6><B>" wKriterier "</B>"
        "<R7><C7><FROM><R7><C110><LINE>"
        WITH PAGE-TOP STREAM-IO WIDTH 255.
  DEFINE FRAME PageFooter
   HEADER
     "<R45><C6><FROM><R45><C110><LINE>" SKIP
     "<ALIGN=BASE><FArial><P10><R46><C6>Utskriven " USERID("dictdb") " " STRING(TODAY)  " " STRING(TIME,"HH:MM:SS")"<C55>" cOurFirma "" SKIP
     WITH PAGE-BOTTOM STREAM-IO WIDTH 255.


{xPrint.i}

/* {runlib.i} */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&IF DEFINED(EXCLUDE-setUndeline) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setUndeline Procedure 
FUNCTION setUndeline RETURNS CHARACTER
  ( INPUT ipcVerdier AS CHARACTER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-skrivrad) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD skrivrad Procedure 
FUNCTION skrivrad RETURNS CHARACTER
  ( INPUT ipcVerdier AS CHARACTER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF


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
IF CAN-FIND(FIRST TT_dags_rap) THEN DO:
    ASSIGN cOurFirma = IF NUM-ENTRIES(cVi,"=") = 2 THEN ENTRY(2,cVi,"=") ELSE "".
    RUN ExportToXprint.
END.
ELSE
    MESSAGE "Ingen data"
        VIEW-AS ALERT-BOX INFO BUTTONS OK.

OUTPUT CLOSE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-ExportToXprint) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ExportToXprint Procedure 
PROCEDURE ExportToXprint :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR cExcEkstent  AS CHAR NO-UNDO.
  DEF VAR ctmpFileName AS CHAR NO-UNDO.
  DEF VAR cButikRader  AS CHAR NO-UNDO.
  DEF VAR iCount1      AS INTE NO-UNDO.
  DEF VAR iCount2      AS INTE NO-UNDO.
  DEFINE VARIABLE iSum AS INTEGER EXTENT 10 NO-UNDO.
  DEFINE VARIABLE cBrowseRub AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cVerdier AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE pcRappFil AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE iLineCounter AS INTEGER    NO-UNDO.
  ASSIGN pcRappFil = SESSION:TEMP-DIRECTORY + "SEhgrap.xpr"
         wKollonneLbl = TRIM(ENTRY(2,wKollonneLbl,"=")).    
  ASSIGN cRub[1]  = ENTRY(1,wKollonneLbl)
         cRub[2]  = ENTRY(2,wKollonneLbl)
         cRub[3]  = ENTRY(3,wKollonneLbl)
         cRub[4]  = ENTRY(4,wKollonneLbl)
         cRub[5]  = ENTRY(5,wKollonneLbl)
         cRub[6]  = ENTRY(6,wKollonneLbl)
         cRub[7]  = ENTRY(7,wKollonneLbl)
         cRub[8]  = ENTRY(8,wKollonneLbl)
         cRub[9]  = ENTRY(9,wKollonneLbl)
         cRub[10] = ENTRY(10,wKollonneLbl)
         cRub[11] = ENTRY(11,wKollonneLbl)
         cBrowseRub = "<U>" + cRub[1] + "</U>,<U>" + cRub[2] + "</U>,<U>%</U>,<U>" + cRub[3] + "</U>,<U>%</U>,<U>" + cRub[4] + "</U>,<U>%</U>,<U>" + cRub[5] + "</U>,<U>%</U>,<U>" + 
                    cRub[6] + "</U>,<U>%</U>,<U>" + cRub[7] + "</U>,<U>%</U>,<U>" + cRub[8] + "</U>,<U>%</U>,<U>" + cRub[9] + "</U>,<U>%</U>,<U>" + cRub[10] + "</U>,<U>%</U>,<U>" + cRub[11] + "</U>".
/* tillfällig tömning av fil */
  OUTPUT TO VALUE(pcRappFil) PAGED PAGE-SIZE VALUE(48).
  PUT CONTROL '<PDF-OUTPUT=' + REPLACE(pcRappFil,"xpr","pdf") + '>'.
  PUT CONTROL '<PREVIEW=ZoomToWidth><OLANDSCAPE>'.
  {sww.i}
  FOR EACH TT_dags_rap BREAK BY butikk:
      IF FIRST-OF(TT_dags_rap.butikk) THEN DO:
          IF NOT FIRST(TT_dags_rap.butikk) THEN DO:
              PAGE.
              ASSIGN iSum      = 0.
          END.
          VIEW FRAME PageHeader.
          ASSIGN iLineCounter = iButikRad + 1.
          FIND Butiker WHERE Butiker.Butik = TT_dags_rap.butikk NO-LOCK.
          PUT UNFORMATTED "<R" STRING(iButikRad) "><C6><P12><B>" wButikkLbl " " STRING(TT_dags_rap.butikk) " " Butiker.ButNamn "</B><P9>" SKIP.
          PUT UNFORMATTED skrivrad(cBrowseRub).
          ASSIGN iLineCounter = iLineCounter + 1.
      END.
      ASSIGN iSum[1]  = iSum[1]  + ROUND(hg1_oms,0)
             iSum[2]  = iSum[2]  + ROUND(hg2_oms,0)
             iSum[3]  = iSum[3]  + ROUND(hg3_oms,0)
             iSum[4]  = iSum[4]  + ROUND(hg4_oms,0)
             iSum[5]  = iSum[5]  + ROUND(hg5_oms,0)
             iSum[6]  = iSum[6]  + ROUND(hg6_oms,0)
             iSum[7]  = iSum[7]  + ROUND(hg7_oms,0)
             iSum[8]  = iSum[8]  + ROUND(hg8_oms,0)
             iSum[9]  = iSum[9]  + ROUND(hg9_oms,0)
             iSum[10] = iSum[10] + ROUND(RadTotal,0)
           cVerdier = (IF cAkkumulering = "1" THEN STRING(TT_dags_rap.dato,"99/99/99") ELSE
           STRING(MONTH(TT_dags_rap.dato),"99") + "/" + STRING(YEAR(TT_dags_rap.dato))) + "," +
           STRING(ROUND(hg1_oms,0),"->,>>>,>>9") + "," +
           (IF RadTotal > 0 THEN STRING(ROUND(hg1_oms * 100 / RadTotal,0),"->>9") ELSE "0") + "," +
           STRING(ROUND(hg2_oms,0),"->,>>>,>>9") + "," +
           (IF RadTotal > 0 THEN STRING(ROUND(hg2_oms * 100 / RadTotal,0),"->>9") ELSE "0") + "," +
           STRING(ROUND(hg3_oms,0),"->,>>>,>>9") + "," +
           (IF RadTotal > 0 THEN STRING(ROUND(hg3_oms * 100 / RadTotal,0),"->>9") ELSE "0") + "," +
           STRING(ROUND(hg4_oms,0),"->,>>>,>>9") + "," +
           (IF RadTotal > 0 THEN STRING(ROUND(hg4_oms * 100 / RadTotal,0),"->>9") ELSE "0") + "," +
           STRING(ROUND(hg5_oms,0),"->,>>>,>>9") + "," +
           (IF RadTotal > 0 THEN STRING(ROUND(hg5_oms * 100 / RadTotal,0),"->>9") ELSE "0") + "," +
           STRING(ROUND(hg6_oms,0),"->,>>>,>>9") + "," +
           (IF RadTotal > 0 THEN STRING(ROUND(hg6_oms * 100 / RadTotal,0),"->>9") ELSE "0") + "," +
           STRING(ROUND(hg7_oms,0),"->,>>>,>>9") + "," +
           (IF RadTotal > 0 THEN STRING(ROUND(hg7_oms * 100 / RadTotal,0),"->>9") ELSE "0") + "," +
           STRING(ROUND(hg8_oms,0),"->,>>>,>>9") + "," +
           (IF RadTotal > 0 THEN STRING(ROUND(hg8_oms * 100 / RadTotal,0),"->>9") ELSE "0") + "," +
           STRING(ROUND(hg9_oms,0),"->,>>>,>>9") + "," +
           (IF RadTotal > 0 THEN STRING(ROUND(hg9_oms * 100 / RadTotal,0),"->>9") ELSE "0") + "," +
           STRING(ROUND(RadTotal,0),"->,>>>,>>9") NO-ERROR.
      IF NOT LAST-OF(TT_dags_rap.butikk) AND iLineCounter > 42 THEN DO:
          VIEW FRAME PageFooter.
          PAGE.
          VIEW FRAME PageHeader.
          PUT UNFORMATTED "<R" STRING(iButikRad) "><C6><P12><B>.... </B>" wButikkLbl " " STRING(TT_dags_rap.butikk) " " Butiker.ButNamn "<P9>" SKIP.
          PUT UNFORMATTED skrivrad(cBrowseRub).
          ASSIGN iLineCounter = iButikRad + 2.
      END.
      IF LAST-OF(TT_dags_rap.butikk) THEN
          ASSIGN cVerdier = setUndeline(cVerdier).
      PUT UNFORMATTED skrivrad(cVerdier).
      ASSIGN iLineCounter = iLineCounter + 1.
      IF LAST-OF(TT_dags_rap.butikk) THEN DO:
          ASSIGN cVerdier = "," + 
                 STRING(iSum[1],"->,>>>,>>9") + "," + STRING(ROUND(iSum[1] * 100 / iSum[10],0),"->>9") + "," +
                 STRING(iSum[2],"->,>>>,>>9") + "," + STRING(ROUND(iSum[2] * 100 / iSum[10],0),"->>9") + "," +
                 STRING(iSum[3],"->,>>>,>>9") + "," + STRING(ROUND(iSum[3] * 100 / iSum[10],0),"->>9") + "," +
                 STRING(iSum[4],"->,>>>,>>9") + "," + STRING(ROUND(iSum[4] * 100 / iSum[10],0),"->>9") + "," +
                 STRING(iSum[5],"->,>>>,>>9") + "," + STRING(ROUND(iSum[5] * 100 / iSum[10],0),"->>9") + "," +
                 STRING(iSum[6],"->,>>>,>>9") + "," + STRING(ROUND(iSum[6] * 100 / iSum[10],0),"->>9") + "," +
                 STRING(iSum[7],"->,>>>,>>9") + "," + STRING(ROUND(iSum[7] * 100 / iSum[10],0),"->>9") + "," +
                 STRING(iSum[8],"->,>>>,>>9") + "," + STRING(ROUND(iSum[8] * 100 / iSum[10],0),"->>9") + "," +
                 STRING(iSum[9],"->,>>>,>>9") + "," + STRING(ROUND(iSum[9] * 100 / iSum[10],0),"->>9") + "," +
                 STRING(iSum[10],"->,>>>,>>9").
          PUT UNFORMATTED skrivrad(cVerdier).
          VIEW FRAME PageFooter.
      END.
/*          STRING(98765432,"->>,>>>,>>9") + "," + */
/*          STRING(-100,"->>9") + "," +            */
/*          STRING(98765432,"->>,>>>,>>9") + "," + */
/*          STRING(-100,"->>9") + "," +            */
/*          STRING(98765432,"->>,>>>,>>9") + "," + */
/*          STRING(-100,"->>9") + "," +            */
/*          STRING(98765432,"->>,>>>,>>9") + "," + */
/*          STRING(-100,"->>9") + "," +            */
/*          STRING(98765432,"->>,>>>,>>9") + "," + */
/*          STRING(-100,"->>9") + "," +            */
/*          STRING(98765432,"->>,>>>,>>9") + "," + */
/*          STRING(-100,"->>9") + "," +            */
/*          STRING(98765432,"->>,>>>,>>9") + "," + */
/*          STRING(-100,"->>9") + "," +            */
/*          STRING(98765432,"->>,>>>,>>9") + "," + */
/*          STRING(-100,"->>9") + "," +            */
/*          STRING(98765432,"->>,>>>,>>9") + "," + */
/*          STRING(-100,"-999") + "," +            */
/*          STRING(98765432,"->>,>>>,>>9").        */
  END.
  {swn.i}
  OUTPUT CLOSE.
  OUTPUT TO TERMINAL.

/* Klargjør rapportfilnavnet */
ASSIGN FILE-INFO:File-NAME = pcRappFil.

/* Sender filen til visning og utskrift. */
 RUN PrintPDF(FILE-INFO:FULL-PATHNAME, 'POLYGON SOFTWARE AS', 'A1a9T4h4e2h_mqe2mbka' ). 
IF SEARCH(FILE-INFO:FULL-PATHNAME) <> ? THEN
    OS-DELETE VALUE(FILE-INFO:FILE-NAME).
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
                  TT_dags_rap.hg7_oms = TT_dags_rap.hg7_oms + dags_rap.hg7_oms
                  TT_dags_rap.hg8_oms = TT_dags_rap.hg8_oms + dags_rap.hg8_oms
                  TT_dags_rap.hg9_oms = TT_dags_rap.hg9_oms + dags_rap.hg9_oms
                  TT_dags_rap.RadTotal   = TT_dags_rap.RadTotal + dags_rap.hg1_oms         
                                                                + dags_rap.hg2_oms         
                                                                + dags_rap.hg3_oms         
                                                                + dags_rap.hg4_oms         
                                                                + dags_rap.hg5_oms         
                                                                + dags_rap.hg6_oms         
                                                                + dags_rap.hg7_oms         
                                                                + dags_rap.hg8_oms         
                                                                + dags_rap.hg9_oms.
       END.
       RELEASE TT_dags_rap.
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

/* ************************  Function Implementations ***************** */

&IF DEFINED(EXCLUDE-setUndeline) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setUndeline Procedure 
FUNCTION setUndeline RETURNS CHARACTER
  ( INPUT ipcVerdier AS CHARACTER ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE iCount AS INTEGER    NO-UNDO.
  DO iCount = 1 TO NUM-ENTRIES(ipcVerdier):
      IF CAN-DO("2,4,6,8,10,12,14,16,18,20",STRING(iCount)) THEN
          ASSIGN ENTRY(iCount,ipcVerdier) = "<U>" + FILL(" ",12 - LENGTH(ENTRY(iCount,ipcVerdier))) + ENTRY(iCount,ipcVerdier) + "</U>".
  END.
  RETURN ipcVerdier.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-skrivrad) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION skrivrad Procedure 
FUNCTION skrivrad RETURNS CHARACTER
  ( INPUT ipcVerdier AS CHARACTER ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cVerdier AS CHARACTER EXTENT 20 FORMAT "X(15)" NO-UNDO.
  DEFINE VARIABLE iCount AS INTEGER    NO-UNDO.
  DO iCount = 1 TO NUM-ENTRIES(ipcVerdier):
      ASSIGN cVerdier[iCount] = ENTRY(iCount,ipcVerdier).
  END.

  RETURN "<R+1>"  +
         "<C6><RIGHT=C+5>"   +  cVerdier[1] +
         "<C11><RIGHT=C+8>"  +  cVerdier[2] +
         "<C19><RIGHT=C+3>"  +  cVerdier[3] +
         "<C21><RIGHT=C+8>"  +  cVerdier[4] +
         "<C29><RIGHT=C+3>"  +  cVerdier[5] +
         "<C31><RIGHT=C+8>"  +  cVerdier[6] +
         "<C39><RIGHT=C+3>"  +  cVerdier[7] +
         "<C41><RIGHT=C+8>"  +  cVerdier[8] +
         "<C49><RIGHT=C+3>"  +  cVerdier[9] + 
         "<C51><RIGHT=C+8>"  + cVerdier[10] +
         "<C59><RIGHT=C+3>"  + cVerdier[11] +
         "<C61><RIGHT=C+8>"  + cVerdier[12] +
         "<C69><RIGHT=C+3>"  + cVerdier[13] +
         "<C71><RIGHT=C+8>"  + cVerdier[14] +
         "<C79><RIGHT=C+3>"  + cVerdier[15] +
         "<C81><RIGHT=C+8>"  + cVerdier[16] +
         "<C89><RIGHT=C+3>" + cVerdier[17] +
         "<C91><RIGHT=C+8>" + cVerdier[18] +
         "<C99><RIGHT=C+3>" + cVerdier[19] +
         "<C101><RIGHT=C+8>" + cVerdier[20].

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

