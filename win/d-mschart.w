&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame 
/*------------------------------------------------------------------------
  Forfatter:    T.Nøkleby
  Beskrivelse:  Grafisk visning av lager og salgsverdier.
  Parametere:   
  
           Eksempel på parametersett med 4 kollonner og 11 elementer.
           Ilustrerer hvordan verdiene skal bygges.
           
    wVindusTittel = "Vindustittel"
    wTitle        = "Artikkel 11/702 XL-13/94 Testgraf"
    wRowLabel     = "Kvartal 1,Kvartal 2,Kvartal 3,Kvartal 4"
    wColLabel     = "Kjøpt,Solgt,Gjenkjøp,K.Reklam,Lager.Rekl,Brekkasje,Overført,Justert,Svinn,Int.forbruk,Nedskrevet"
    wFColValues   = "3000,600,1100,1900" + ";" + 
                    "2000,100,200,2100" + ";" + 
                    "4000,800,350,1100" + ";" + 
                    "4500,800,350,1900" + ";" + 
                    "3200,800,350,1600" + ";" + 
                    "3000,600,450,1000" + ";" + 
                    "3000,600,450,1000" + ";" + 
                    "3000,600,450,1000" + ";" + 
                    "3000,600,450,1000" + ";" + 
                    "3000,600,450,1000" + ";" + 
                    "3000,600,450,1000"
    wColEnable    = "1,1,0,0,0,0,0,0,0,0,0".      
  
  Endringer:
  
                Antall rader og antall perioder kan varieres, men 
                antall elementer som skal kunne vises er maksimalt 11
                stk. Er det færre elementer, blir tilhørende toggel
                boks satt til ikke synlig.
------------------------------------------------------------------------*/

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameter Definisjoner ---                                           */
&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
    define var wVindusTittel as char init "Vindustittel" no-undo.
    DEFINE VAR wTitle        AS CHAR INIT "Artikkel 11/702 XL-13/94 Testgraf" NO-UNDO.
    DEFINE VAR wRowLabel     AS CHAR INIT "Kvartal 1,Kvartal 2,Kvartal 3,Kvartal 4" NO-UNDO.
    DEFINE VAR wColLabel     AS CHAR INIT "Kjøpt,Solgt,Gjenkjøp,K.Reklam,Lager.Rekl,Brekkasje,Overført,Justert,Svinn,Int.forbruk,Nedskrevet" NO-UNDO.
    DEFINE VAR wColValues AS CHAR INIT
          "3000,600,1100,1900;2000,100,200,2100;4000,800,350,1100;4500,800,350,1900;3200,800,350,1600;3000,600,450,1000;3000,600,450,1000;3000,600,450,1000;3000,600,450,1000;3000,600,450,1000;3000,600,450,1000".
    define var wColEnable    as char init "1,1,0,0,0,0,0,0,0,0,0" no-undo.      
&ELSE
    define input parameter wVindusTittel as char no-undo.
    DEFINE INPUT PARAMETER wTitle        AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER wRowLabel     AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER wColLabel     AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER wColValues   AS CHAR NO-UNDO.
    define input parameter wColEnable    as char NO-UNDO.      
&ENDIF

/* Preprossessor direktiver ---                                         */

/* Buffer og Temp-Table Definisjoner ---                                */

/* Lokale variabler ---                                                 */
def var retur-verdi as char initial "<avbryt>" no-undo.

  DEF VAR ch-S1      AS COM-HANDLE NO-UNDO.
  DEF VAR comSerColl AS COM-HANDLE NO-UNDO.
  DEF VAR comSeries  AS COM-HANDLE NO-UNDO.
  DEF VAR comPos     AS COM-HANDLE NO-UNDO.

DEF VAR i-type   AS I EXTENT 12 NO-UNDO 
  INITIAL [0,1,2,3,4,5,6,7,8,9,14,16] . /* map available chart-types nos. */

define var m-ja              as logical no-undo.
define var m-i               as integer no-undo.
define var m-x               as character no-undo.
define var m-handle          as handle no-undo.
define var m-wh              as widget-handle no-undo.

DEFINE FRAME PageHeader
       HEADER
          "<ALIGN=BASE><FArial><P12><B><C25><P24>" wVindusTittel FORMAT "x(30)"
          "<P12></B><C75><P10>" SKIP
          "<R3><C6><FROM><R3><C78><LINE>" SKIP
          WITH PAGE-TOP STREAM-IO WIDTH 255.

{xPrint.i}
{runlib.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME Dialog-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS CB-type TOGGLE-1 T-1 T-2 T-3 T-4 T-5 T-6 T-7 ~
T-8 T-9 T-10 T-11 T-12 T-13 T-14 T-15 Btn_Cancel BUTTON-2 Btn_Help RECT-49 ~
RECT-51 RECT-52 
&Scoped-Define DISPLAYED-OBJECTS CB-type TOGGLE-1 T-1 T-2 T-3 T-4 T-5 T-6 ~
T-7 T-8 T-9 T-10 T-11 T-12 T-13 T-14 T-15 FI-Tekst1 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of handles for OCX Containers                            */
DEFINE VARIABLE MSChart AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chMSChart AS COMPONENT-HANDLE NO-UNDO.
DEFINE VARIABLE Picbuf AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chPicbuf AS COMPONENT-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Cancel AUTO-END-KEY 
     LABEL "Avslutt" 
     SIZE 12 BY 1.1
     BGCOLOR 8 .

DEFINE BUTTON Btn_Help 
     LABEL "&Hjelp" 
     SIZE 12 BY 1.1
     BGCOLOR 8 .

DEFINE BUTTON BUTTON-2 
     LABEL "Utskrift" 
     SIZE 12 BY 1.1.

DEFINE BUTTON BUTTON-3 
     LABEL "Test hidden" 
     SIZE 15 BY 1.14.

DEFINE VARIABLE CB-type AS CHARACTER FORMAT "X(20)":U 
     LABEL "Diagram type" 
     VIEW-AS COMBO-BOX INNER-LINES 6
     LIST-ITEMS "3DBar","2DBar","3DLine","2DLine","3DArea","2DArea","3DStep","2DStep","3DCombination","2DCombination","2DPie","2DXY" 
     DROP-DOWN-LIST
     SIZE 25 BY 1 TOOLTIP "Change chart type" NO-UNDO.

DEFINE VARIABLE FI-Tekst1 AS CHARACTER FORMAT "X(256)":U INITIAL "Visning" 
      VIEW-AS TEXT 
     SIZE 14 BY .62
     FONT 6 NO-UNDO.

DEFINE RECTANGLE RECT-49
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 18 BY 25.

DEFINE RECTANGLE RECT-51
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 159.2 BY .1.

DEFINE RECTANGLE RECT-52
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 159.2 BY .1.

DEFINE VARIABLE T-1 AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 16 BY .81 NO-UNDO.

DEFINE VARIABLE T-10 AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 16 BY .81 NO-UNDO.

DEFINE VARIABLE T-11 AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 16 BY .81 NO-UNDO.

DEFINE VARIABLE T-12 AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 16 BY .81 NO-UNDO.

DEFINE VARIABLE T-13 AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 16 BY .81 NO-UNDO.

DEFINE VARIABLE T-14 AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 16 BY .81 NO-UNDO.

DEFINE VARIABLE T-15 AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 16 BY .81 NO-UNDO.

DEFINE VARIABLE T-2 AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 16 BY .81 NO-UNDO.

DEFINE VARIABLE T-3 AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 16 BY .81 NO-UNDO.

DEFINE VARIABLE T-4 AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 16 BY .81 NO-UNDO.

DEFINE VARIABLE T-5 AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 16 BY .81 NO-UNDO.

DEFINE VARIABLE T-6 AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 16 BY .81 NO-UNDO.

DEFINE VARIABLE T-7 AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 16 BY .81 NO-UNDO.

DEFINE VARIABLE T-8 AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 16 BY .81 NO-UNDO.

DEFINE VARIABLE T-9 AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 16 BY .81 NO-UNDO.

DEFINE VARIABLE TOGGLE-1 AS LOGICAL INITIAL yes 
     LABEL "Beskrivelse" 
     VIEW-AS TOGGLE-BOX
     SIZE 19 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     CB-type AT ROW 1.24 COL 25 COLON-ALIGNED
     TOGGLE-1 AT ROW 1.33 COL 55
     T-1 AT ROW 4.33 COL 142.6
     T-2 AT ROW 5.33 COL 142.6
     T-3 AT ROW 6.33 COL 142.6
     T-4 AT ROW 7.33 COL 142.6
     T-5 AT ROW 8.33 COL 142.6
     T-6 AT ROW 9.33 COL 142.6
     T-7 AT ROW 10.33 COL 142.6
     T-8 AT ROW 11.33 COL 142.6
     T-9 AT ROW 12.33 COL 142.6
     T-10 AT ROW 13.33 COL 142.6
     T-11 AT ROW 14.33 COL 142.6
     T-12 AT ROW 15.33 COL 142.6
     T-13 AT ROW 16.33 COL 142.6
     T-14 AT ROW 17.33 COL 142.6
     T-15 AT ROW 18.33 COL 142.6
     BUTTON-3 AT ROW 28.14 COL 55
     Btn_Cancel AT ROW 28.19 COL 2
     BUTTON-2 AT ROW 28.19 COL 19
     Btn_Help AT ROW 28.19 COL 147.4
     FI-Tekst1 AT ROW 3.38 COL 141 COLON-ALIGNED NO-LABEL
     RECT-49 AT ROW 2.91 COL 141.6
     RECT-51 AT ROW 1 COL 1
     RECT-52 AT ROW 2.43 COL 1
     SPACE(0.39) SKIP(26.94)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "<insert dialog title>"
         CANCEL-BUTTON Btn_Cancel.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: DIALOG-BOX
   Allow: Basic,Browse,DB-Fields,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX Dialog-Frame
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* SETTINGS FOR BUTTON BUTTON-3 IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
ASSIGN 
       BUTTON-3:HIDDEN IN FRAME Dialog-Frame           = TRUE.

/* SETTINGS FOR FILL-IN FI-Tekst1 IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
ASSIGN 
       T-1:HIDDEN IN FRAME Dialog-Frame           = TRUE.

ASSIGN 
       T-10:HIDDEN IN FRAME Dialog-Frame           = TRUE.

ASSIGN 
       T-11:HIDDEN IN FRAME Dialog-Frame           = TRUE.

ASSIGN 
       T-12:HIDDEN IN FRAME Dialog-Frame           = TRUE.

ASSIGN 
       T-13:HIDDEN IN FRAME Dialog-Frame           = TRUE.

ASSIGN 
       T-14:HIDDEN IN FRAME Dialog-Frame           = TRUE.

ASSIGN 
       T-15:HIDDEN IN FRAME Dialog-Frame           = TRUE.

ASSIGN 
       T-2:HIDDEN IN FRAME Dialog-Frame           = TRUE.

ASSIGN 
       T-3:HIDDEN IN FRAME Dialog-Frame           = TRUE.

ASSIGN 
       T-4:HIDDEN IN FRAME Dialog-Frame           = TRUE.

ASSIGN 
       T-5:HIDDEN IN FRAME Dialog-Frame           = TRUE.

ASSIGN 
       T-6:HIDDEN IN FRAME Dialog-Frame           = TRUE.

ASSIGN 
       T-7:HIDDEN IN FRAME Dialog-Frame           = TRUE.

ASSIGN 
       T-8:HIDDEN IN FRAME Dialog-Frame           = TRUE.

ASSIGN 
       T-9:HIDDEN IN FRAME Dialog-Frame           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX Dialog-Frame
/* Query rebuild information for DIALOG-BOX Dialog-Frame
     _Options          = "SHARE-LOCK"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX Dialog-Frame */
&ANALYZE-RESUME

 


/* **********************  Create OCX Containers  ********************** */

&ANALYZE-SUSPEND _CREATE-DYNAMIC

&IF "{&OPSYS}" = "WIN32":U AND "{&WINDOW-SYSTEM}" NE "TTY":U &THEN

CREATE CONTROL-FRAME MSChart ASSIGN
       FRAME           = FRAME Dialog-Frame:HANDLE
       ROW             = 2.91
       COLUMN          = 2
       HEIGHT          = 25
       WIDTH           = 138
       HIDDEN          = no
       SENSITIVE       = yes.

CREATE CONTROL-FRAME Picbuf ASSIGN
       FRAME           = FRAME Dialog-Frame:HANDLE
       ROW             = 21.71
       COLUMN          = 97
       HEIGHT          = 7.14
       WIDTH           = 44
       HIDDEN          = yes
       SENSITIVE       = no.
      MSChart:NAME = "MSChart":U .
/* MSChart OCXINFO:CREATE-CONTROL from: {3A2B370C-BA0A-11D1-B137-0000F8753F5D} type: MSChart */
      Picbuf:NAME = "Picbuf":U .
/* Picbuf OCXINFO:CREATE-CONTROL from: {9A93B740-C96B-11D0-8883-444553540000} type: Picbuf */
      MSChart:MOVE-AFTER(TOGGLE-1:HANDLE IN FRAME Dialog-Frame).
      Picbuf:MOVE-AFTER(T-15:HANDLE IN FRAME Dialog-Frame).

&ENDIF

&ANALYZE-RESUME /* End of _CREATE-DYNAMIC */


/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* <insert dialog title> */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Help
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Help Dialog-Frame
ON CHOOSE OF Btn_Help IN FRAME Dialog-Frame /* Hjelp */
OR HELP OF FRAME {&FRAME-NAME}
DO: /* Call Help Function (or a simple message). */
  {winhlp.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-2 Dialog-Frame
ON CHOOSE OF BUTTON-2 IN FRAME Dialog-Frame /* Utskrift */
DO:
  DEF VAR pcRappFil      AS CHAR NO-UNDO.
  DEF VAR pcBildFil      AS CHAR NO-UNDO.
  DEF VAR iRad           AS INTE NO-UNDO.
  DEF VAR iCount         AS INTE NO-UNDO.
  DEF VAR iCount2        AS INTE NO-UNDO.
  DEF VAR iMaxLen        AS INTE NO-UNDO.
  DEF VAR iMaxLenLbl     AS INTE NO-UNDO.
  DEF VAR cNumFormat     AS CHAR NO-UNDO.
  IF VALID-HANDLE(wLibHandle) THEN
     RUN GetTempFileName IN wLibHandle ("Chart", "xpr", OUTPUT pcRappFil). 
  ASSIGN pcBildFil = REPLACE(pcRappFil,"xpr","jpg").
  chMSChart:EditCopy().
  chPicbuf:PasteClipboard().
  chPicbuf:FILENAME = pcBildFil.
  chPicbuf:Store().
/*   DO iCount = 1 TO NUM-ENTRIES(wIColLabel):                                         */
/*       DO iCount2 = 1 TO NUM-ENTRIES(ENTRY(iCount,wColValues,";")):                  */
/*           ASSIGN iMaxLen = IF LENGTH(ENTRY(iCount2,ENTRY(iCount,wColValues,";"))) > */
/*               iMaxLen THEN LENGTH(ENTRY(iCount2,ENTRY(iCount,wColValues,";"))) ELSE */
/*                   iMaxLen.                                                          */
/*       END.                                                                          */
/*   END.                                                                              */
/*   DO iCount = 1 TO NUM-ENTRIES(wRowLabel):                                          */
/*       ASSIGN iMaxLen = IF LENGTH(ENTRY(iCount,wRowLabel)) > iMaxLen THEN            */
/*                 LENGTH(ENTRY(iCount,wRowLabel)) ELSE iMaxLen.                       */
/*   END.                                                                              */
/*   ASSIGN cNumFormat = FILL(">",iMaxLen - 1) + "9".                                  */
/*   DO iCount = 1 TO NUM-ENTRIES(wIColLabel):                                         */
/*       ASSIGN iMaxLenLbl = IF LENGTH(ENTRY(iCount,wIColLabel)) > iMaxLenLbl THEN     */
/*           LENGTH(ENTRY(iCount,wIColLabel)) ELSE iMaxLenLbl.                         */
/*   END.                                                                              */
  /* Åpner stream til skriverfil. */
  OUTPUT TO VALUE(pcRappFil) PAGED PAGE-SIZE 60.
  PUT CONTROL '<PREVIEW=ZoomToWidth>'.
  VIEW FRAME PageHeader.
  PUT UNFORMATTED
   "<TRANSPARENT=false><R" STRING(46)  ",2><C75><#3><R" STRING(3) ",25><C10,2><IMAGE#3=" 
   pcBildFil  ">".
/*   ASSIGN iRad = 32.                                                                                      */
/*   PUT UNFORMATTED "<FCourier NEW><P12><R" STRING(iRad) "><C" STRING(4 + iMaxLenLbl) ">"                  */
/*           FILL(" ",iMaxLen - LENGTH(ENTRY(1,wRowLabel))) ENTRY(1,wRowLabel).                             */
/*   DO iCount = 2 TO NUM-ENTRIES(wRowLabel):                                                               */
/*       PUT UNFORMATTED "<C+1>"                                                                            */
/*              FILL(" ",iMaxLen + (iCount * 2) - LENGTH(ENTRY(iCount,wRowLabel))) ENTRY(iCount,wRowLabel). */
/*   END.                                                                                                   */
/*   ASSIGN iRad = iRad + 1.                                                                                */
/*   DO iCount = 1 TO NUM-ENTRIES(wIColLabel):                                                              */
/*       PUT UNFORMATTED "<FCourier NEW><P12><R" STRING(iRad) "><C4>" ENTRY(iCount,WIColLabel).             */
/*       DO iCount2 = 1 TO NUM-ENTRIES(ENTRY(iCount,wColValues,";")):                                       */
/*        PUT UNFORMATTED "<C" (IF iCount2 = 1 THEN STRING(iMaxLenLbl + 1) ELSE "+1") ">"                   */
/*               STRING(DECI(ENTRY(iCount2,ENTRY(iCount,wColValues,";"))),cNumFormat).                      */
/*       END.                                                                                               */
/*       ASSIGN iRad = iRad + 1.                                                                            */
/*       PUT UNFORMATTED SKIP.                                                                              */
/*   END.                                                                                                   */
/*                                                                                                          */

  OUTPUT TO TERMINAL.

  /* Klargjør rapportfilnavnet */
  ASSIGN FILE-INFO:File-NAME = pcRappFil.
    
  /* Sender filen til visning og utskrift. */
   RUN PrintPDF(FILE-INFO:FULL-PATHNAME, 'POLYGON SOFTWARE AS', 'A1a9T4h4e2h_mqe2mbka' ). 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-3 Dialog-Frame
ON CHOOSE OF BUTTON-3 IN FRAME Dialog-Frame /* Test hidden */
DO:
/*   DEFINE VARIABLE chPlot    AS COM-HANDLE     NO-UNDO.      */
/*   DEFINE VARIABLE chSerColl AS COM-HANDLE     NO-UNDO.      */
/*   DEFINE VARIABLE chSeries  AS COM-HANDLE     NO-UNDO.      */
/*   DEFINE VARIABLE chDPoints AS COM-HANDLE     NO-UNDO.      */
/*   DEFINE VARIABLE chDataPoint AS COM-HANDLE     NO-UNDO.    */
/*   DEFINE VARIABLE chDataPointLbl AS COM-HANDLE     NO-UNDO. */
/*   DEFINE VARIABLE iAntSer   AS INTEGER    NO-UNDO.          */
/*   DEFINE VARIABLE iAntDPoints   AS INTEGER    NO-UNDO.      */
/*                                                             */
/*   chPlot = chMSChart:Plot.                                   */
/*   chSerColl = chPlot:SeriesCollection.                      */
/*   chSeries = chSerColl:ITEM(1).                             */
/*   chSeries:SELECT().                                        */
/*   chDPoints = chSeries:DataPoints.          */
/*   MESSAGE chDPoints:COUNT                   */
/*       VIEW-AS ALERT-BOX INFO BUTTONS OK.    */
/*   chDataPoint = chDPoints:Item(1 AS SHORT). */
/*   MESSAGE chSerColl:COUNT()                                 */
/*       VIEW-AS ALERT-BOX INFO BUTTONS OK.                    */
/*   DO iAntSer = 1 TO 11 /* chSerColl:COUNT() */ .            */
/*       chSeries = chSerColl:ITEM(iAntSer).                   */
/*       chDPoints = chSeries:DataPoints.                      */
/*       DO iAntDPoints = 1 TO 4 /* chDPoints:COUNT */ :       */
/*           chDataPoint = chDPoints:ITEM(iAntDPoints).        */
/*           chDataPoint:SELECT().                             */
/*           chDataPointLbl = chDataPoint:DataPointLabel.      */
/*           chDataPointLbl:TEXT = STRING(chMSChart:Data).      */
/*           LEAVE.                                            */
/*       END.                                                  */
/*       LEAVE.                                                */
/*   END.                                                      */


/*   chMSChart:Column = 1.                   */
/*   chMSChart:Data = 2900.                  */
/*   MESSAGE chMSChart:Data SKIP             */
/*           chMSChart:ROW SKIP              */
/*           chMSChart:RowLabel              */
/*       VIEW-AS ALERT-BOX INFO BUTTONS OK. */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CB-type
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CB-type Dialog-Frame
ON VALUE-CHANGED OF CB-type IN FRAME Dialog-Frame /* Diagram type */
DO:
  IF CB-type <> CB-type:SCREEN-VALUE THEN DO:
    ASSIGN CB-type
           chMSChart:ChartType = i-type[CB-type:LOOKUP(CB-type)].
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME T-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL T-1 Dialog-Frame
ON VALUE-CHANGED OF T-1 IN FRAME Dialog-Frame
DO:
    RUN EnaDis(INT(ENTRY(2,SELF:NAME,"-")),NOT SELF:CHECKED).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME T-10
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL T-10 Dialog-Frame
ON VALUE-CHANGED OF T-10 IN FRAME Dialog-Frame
DO:
    RUN EnaDis(INT(ENTRY(2,SELF:NAME,"-")),NOT SELF:CHECKED).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME T-11
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL T-11 Dialog-Frame
ON VALUE-CHANGED OF T-11 IN FRAME Dialog-Frame
DO:
    RUN EnaDis(INT(ENTRY(2,SELF:NAME,"-")),NOT SELF:CHECKED).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME T-12
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL T-12 Dialog-Frame
ON VALUE-CHANGED OF T-12 IN FRAME Dialog-Frame
DO:
    RUN EnaDis(INT(ENTRY(2,SELF:NAME,"-")),NOT SELF:CHECKED).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME T-13
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL T-13 Dialog-Frame
ON VALUE-CHANGED OF T-13 IN FRAME Dialog-Frame
DO:
    RUN EnaDis(INT(ENTRY(2,SELF:NAME,"-")),NOT SELF:CHECKED).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME T-14
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL T-14 Dialog-Frame
ON VALUE-CHANGED OF T-14 IN FRAME Dialog-Frame
DO:
    RUN EnaDis(INT(ENTRY(2,SELF:NAME,"-")),NOT SELF:CHECKED).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME T-15
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL T-15 Dialog-Frame
ON VALUE-CHANGED OF T-15 IN FRAME Dialog-Frame
DO:
    RUN EnaDis(INT(ENTRY(2,SELF:NAME,"-")),NOT SELF:CHECKED).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME T-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL T-2 Dialog-Frame
ON VALUE-CHANGED OF T-2 IN FRAME Dialog-Frame
DO:
    RUN EnaDis(INT(ENTRY(2,SELF:NAME,"-")),NOT SELF:CHECKED).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME T-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL T-3 Dialog-Frame
ON VALUE-CHANGED OF T-3 IN FRAME Dialog-Frame
DO:
    RUN EnaDis(INT(ENTRY(2,SELF:NAME,"-")),NOT SELF:CHECKED).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME T-4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL T-4 Dialog-Frame
ON VALUE-CHANGED OF T-4 IN FRAME Dialog-Frame
DO:
    RUN EnaDis(INT(ENTRY(2,SELF:NAME,"-")),NOT SELF:CHECKED).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME T-5
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL T-5 Dialog-Frame
ON VALUE-CHANGED OF T-5 IN FRAME Dialog-Frame
DO:
    RUN EnaDis(INT(ENTRY(2,SELF:NAME,"-")),NOT SELF:CHECKED).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME T-6
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL T-6 Dialog-Frame
ON VALUE-CHANGED OF T-6 IN FRAME Dialog-Frame
DO:
    RUN EnaDis(INT(ENTRY(2,SELF:NAME,"-")),NOT SELF:CHECKED).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME T-7
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL T-7 Dialog-Frame
ON VALUE-CHANGED OF T-7 IN FRAME Dialog-Frame
DO:
    RUN EnaDis(INT(ENTRY(2,SELF:NAME,"-")),NOT SELF:CHECKED).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME T-8
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL T-8 Dialog-Frame
ON VALUE-CHANGED OF T-8 IN FRAME Dialog-Frame
DO:
    RUN EnaDis(INT(ENTRY(2,SELF:NAME,"-")),NOT SELF:CHECKED).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME T-9
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL T-9 Dialog-Frame
ON VALUE-CHANGED OF T-9 IN FRAME Dialog-Frame
DO:
    RUN EnaDis(INT(ENTRY(2,SELF:NAME,"-")),NOT SELF:CHECKED).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TOGGLE-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TOGGLE-1 Dialog-Frame
ON VALUE-CHANGED OF TOGGLE-1 IN FRAME Dialog-Frame /* Beskrivelse */
DO:
    ASSIGN chMSChart:ShowLegend = SELF:CHECKED.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dialog-Frame 


/* ***************************  Main Block  *************************** */

/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT eq ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.

if num-entries(wColLabel)   <> num-entries(wColValues,";") or
   num-entries(wColLabel)   <> num-entries(wColEnable)  or
   num-entries(wColValues,";") <> num-entries(wColEnable) then
  do:
    message "Input parametre har ikke samsvarende antall enties." skip
            "Sjekk oppbygning av wColLabel, wColValues og wColEnable." skip
            "wColLabel:" num-entries(wColLabel) skip
            "wColValues:" num-entries(wColValues,";") skip
            "wColEnable:" num-entries(wColEnable) skip
            view-as alert-box.
    return no-apply.  
  end.

if num-entries(wRowLabel) <> num-entries(entry(1,wColValues,";")) then
  do:
     message "Antall kollonnelabler samsvarer ikke med antall verdier pr." skip
             "kollonne pr. element. Sjekk wRowLabel og wColValues." skip
             "wRowLabel: "  num-entries(wRowLabel) wRowLabel skip
             "wColValues[1]: " num-entries(entry(1,wColValues,";"))  entry(1,wColValues,";")
             view-as alert-box.
     return no-apply.  
  end.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  ASSIGN CB-Type:SCREEN-VALUE = "2DBar".
  {lng.i} 
  ASSIGN 
     frame Dialog-Frame:title = wVindusTittel.
  RUN enable_UI.
  run HideToggle.
  view frame Dialog-Frame.
  WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

RELEASE OBJECT chMSChart NO-ERROR.
RELEASE OBJECT comSerColl NO-ERROR.
RELEASE OBJECT comSeries NO-ERROR.
RELEASE OBJECT comPos NO-ERROR.
IF VALID-HANDLE(MSChart) THEN
    DELETE OBJECT MSChart NO-ERROR.

ASSIGN MSChart    = ?
       chMSChart  = ?
       comSerColl = ?
       comSeries  = ?
       comPos     = ?.

&IF DEFINED(UIB_IS_RUNNING) EQ 0 &THEN
 return retur-verdi.
&else
 message retur-verdi view-as alert-box.
&endif

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ByggColEnable Dialog-Frame 
PROCEDURE ByggColEnable :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
  do with frame Dialog-Frame:
    if num-entries(wColEnable) >  0 then ENTRY( 1, wColEnable) = if T-1:checked  = false then "0" else "1".
    if num-entries(wColEnable) >  1 then ENTRY( 2, wColEnable) = if T-2:checked  = false then "0" else "1".
    if num-entries(wColEnable) >  2 then ENTRY( 3, wColEnable) = if T-3:checked  = false then "0" else "1".
    if num-entries(wColEnable) >  3 then ENTRY( 4, wColEnable) = if T-4:checked  = false then "0" else "1".
    if num-entries(wColEnable) >  4 then ENTRY( 5, wColEnable) = if T-5:checked  = false then "0" else "1".
    if num-entries(wColEnable) >  5 then ENTRY( 6, wColEnable) = if T-6:checked  = false then "0" else "1".
    if num-entries(wColEnable) >  6 then ENTRY( 7, wColEnable) = if T-7:checked  = false then "0" else "1".
    if num-entries(wColEnable) >  7 then ENTRY( 8, wColEnable) = if T-8:checked  = false then "0" else "1".
    if num-entries(wColEnable) >  8 then ENTRY( 9, wColEnable) = if T-9:checked  = false then "0" else "1".
    if num-entries(wColEnable) >  9 then ENTRY(10, wColEnable) = if T-10:checked = false then "0" else "1".
    if num-entries(wColEnable) > 10 then ENTRY(11, wColEnable) = if T-11:checked = false then "0" else "1".
    if num-entries(wColEnable) > 11 then ENTRY(12, wColEnable) = if T-12:checked = false then "0" else "1".
    if num-entries(wColEnable) > 12 then ENTRY(13, wColEnable) = if T-13:checked = false then "0" else "1".
    if num-entries(wColEnable) > 13 then ENTRY(14, wColEnable) = if T-14:checked = false then "0" else "1".
    if num-entries(wColEnable) > 14 then ENTRY(15, wColEnable) = if T-15:checked = false then "0" else "1".
  end.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE control_load Dialog-Frame  _CONTROL-LOAD
PROCEDURE control_load :
/*------------------------------------------------------------------------------
  Purpose:     Load the OCXs    
  Parameters:  <none>
  Notes:       Here we load, initialize and make visible the 
               OCXs in the interface.                        
------------------------------------------------------------------------------*/

&IF "{&OPSYS}" = "WIN32":U AND "{&WINDOW-SYSTEM}" NE "TTY":U &THEN
DEFINE VARIABLE UIB_S    AS LOGICAL    NO-UNDO.
DEFINE VARIABLE OCXFile  AS CHARACTER  NO-UNDO.

OCXFile = SEARCH( "d-mschart.wrx":U ).
IF OCXFile = ? THEN
  OCXFile = SEARCH(SUBSTRING(THIS-PROCEDURE:FILE-NAME, 1,
                     R-INDEX(THIS-PROCEDURE:FILE-NAME, ".":U), "CHARACTER":U) + "wrx":U).

IF OCXFile <> ? THEN
DO:
  ASSIGN
    chMSChart = MSChart:COM-HANDLE
    UIB_S = chMSChart:LoadControls( OCXFile, "MSChart":U)
    chPicbuf = Picbuf:COM-HANDLE
    UIB_S = chPicbuf:LoadControls( OCXFile, "Picbuf":U)
  .
  RUN initialize-controls IN THIS-PROCEDURE NO-ERROR.
END.
ELSE MESSAGE "d-mschart.wrx":U SKIP(1)
             "The binary control file could not be found. The controls cannot be loaded."
             VIEW-AS ALERT-BOX TITLE "Controls Not Loaded".

&ENDIF

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI Dialog-Frame  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Hide all frames. */
  HIDE FRAME Dialog-Frame.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI Dialog-Frame  _DEFAULT-ENABLE
PROCEDURE enable_UI :
/*------------------------------------------------------------------------------
  Purpose:     ENABLE the User Interface
  Parameters:  <none>
  Notes:       Here we display/view/enable the widgets in the
               user-interface.  In addition, OPEN all queries
               associated with each FRAME and BROWSE.
               These statements here are based on the "Other 
               Settings" section of the widget Property Sheets.
------------------------------------------------------------------------------*/
  RUN control_load.
  DISPLAY CB-type TOGGLE-1 T-1 T-2 T-3 T-4 T-5 T-6 T-7 T-8 T-9 T-10 T-11 T-12 
          T-13 T-14 T-15 FI-Tekst1 
      WITH FRAME Dialog-Frame.
  ENABLE CB-type TOGGLE-1 T-1 T-2 T-3 T-4 T-5 T-6 T-7 T-8 T-9 T-10 T-11 T-12 
         T-13 T-14 T-15 Btn_Cancel BUTTON-2 Btn_Help RECT-49 RECT-51 RECT-52 
      WITH FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE EnaDis Dialog-Frame 
PROCEDURE EnaDis :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER iCol AS INTEGER    NO-UNDO.
DEFINE INPUT  PARAMETER iEnable AS LOGICAL    NO-UNDO.
         ASSIGN comSeries = comSerColl:ITEM(iCol)
                comPos = comSeries:POSITION
                comPos:HIDDEN = iEnable.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE HideToggle Dialog-Frame 
PROCEDURE HideToggle :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  /* Maks 15 toggler skal kunne være synlig. */
  
  def var wLoop as int no-undo.

  do with frame Dialog-Frame:
    assign 
      T-1:hidden  = if num-entries(wColEnable) >  0 then false else true 
      T-2:hidden  = if num-entries(wColEnable) >  1 then false else true 
      T-3:hidden  = if num-entries(wColEnable) >  2 then false else true
      T-4:hidden  = if num-entries(wColEnable) >  3 then false else true 
      T-5:hidden  = if num-entries(wColEnable) >  4 then false else true 
      T-6:hidden  = if num-entries(wColEnable) >  5 then false else true 
      T-7:hidden  = if num-entries(wColEnable) >  6 then false else true 
      T-9:hidden  = if num-entries(wColEnable) >  7 then false else true 
      T-8:hidden  = if num-entries(wColEnable) >  8 then false else true 
      T-10:hidden = if num-entries(wColEnable) >  9 then false else true 
      T-11:hidden = if num-entries(wColEnable) > 10 then false else true
      T-12:hidden = if num-entries(wColEnable) > 11 then false else true
      T-14:hidden = if num-entries(wColEnable) > 12 then false else true
      T-13:hidden = if num-entries(wColEnable) > 13 then false else true
      T-15:hidden = if num-entries(wColEnable) > 14 then false else true. 

    /* Setter på labler */
    if num-entries(wColLabel) >  0 then T-1:label  = entry( 1,wColLabel).
    if num-entries(wColLabel) >  1 then T-2:label  = entry( 2,wColLabel).
    if num-entries(wColLabel) >  2 then T-3:label  = entry( 3,wColLabel).
    if num-entries(wColLabel) >  3 then T-4:label  = entry( 4,wColLabel).
    if num-entries(wColLabel) >  4 then T-5:label  = entry( 5,wColLabel).
    if num-entries(wColLabel) >  5 then T-6:label  = entry( 6,wColLabel).
    if num-entries(wColLabel) >  6 then T-7:label  = entry( 7,wColLabel).
    if num-entries(wColLabel) >  7 then T-8:label  = entry( 8,wColLabel).
    if num-entries(wColLabel) >  8 then T-9:label  = entry( 9,wColLabel).
    if num-entries(wColLabel) >  9 then T-10:label = entry(10,wColLabel).
    if num-entries(wColLabel) > 10 then T-11:label = entry(11,wColLabel).
    if num-entries(wColLabel) > 11 then T-12:label = entry(12,wColLabel).
    if num-entries(wColLabel) > 12 then T-13:label = entry(13,wColLabel).
    if num-entries(wColLabel) > 13 then T-14:label = entry(14,wColLabel).
    if num-entries(wColLabel) > 14 then T-15:label = entry(15,wColLabel).

    /* Initierer defaultverdier */
    do wLoop = 1 to num-entries(wColLabel):
      case wLoop:
        when  1 then T-1:checked  = if entry(wLoop,wColEnable) = "1" then true else false.
        when  2 then T-2:checked  = if entry(wLoop,wColEnable) = "1" then true else false.
        when  3 then T-3:checked  = if entry(wLoop,wColEnable) = "1" then true else false.
        when  4 then T-4:checked  = if entry(wLoop,wColEnable) = "1" then true else false.
        when  5 then T-5:checked  = if entry(wLoop,wColEnable) = "1" then true else false.
        when  6 then T-6:checked  = if entry(wLoop,wColEnable) = "1" then true else false.
        when  7 then T-7:checked  = if entry(wLoop,wColEnable) = "1" then true else false.
        when  8 then T-8:checked  = if entry(wLoop,wColEnable) = "1" then true else false.
        when  9 then T-9:checked  = if entry(wLoop,wColEnable) = "1" then true else false.
        when 10 then T-10:checked = if entry(wLoop,wColEnable) = "1" then true else false.
        when 11 then T-11:checked = if entry(wLoop,wColEnable) = "1" then true else false.
        when 12 then T-12:checked = if entry(wLoop,wColEnable) = "1" then true else false.
        when 13 then T-13:checked = if entry(wLoop,wColEnable) = "1" then true else false.
        when 14 then T-14:checked = if entry(wLoop,wColEnable) = "1" then true else false.
        when 15 then T-15:checked = if entry(wLoop,wColEnable) = "1" then true else false.
      end case.
    end.
  end.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initialize-controls Dialog-Frame 
PROCEDURE initialize-controls :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR wRow    AS INTE NO-UNDO.
  DEF VAR wColumn AS INTE NO-UNDO.
  def var wAntCol as int  no-undo.
  
  ASSIGN chMSChart = chMSChart:MSChart
         chMSChart:ENABLED = FALSE
         chMSChart:Title       = wTitle
         chMSChart:RowCount    = NUM-ENTRIES(wRowLabel)
         chMSChart:ColumnCount = NUM-ENTRIES(wColLabel)
         chMSChart:ShowLegend  = TOGGLE-1
         chPicbuf = chPicbuf:Picbuf.
  DO wColumn = 1 TO chMSChart:ColumnCount:
          ASSIGN chMSChart:Column      = wColumn
                 chMSChart:ColumnLabel = ENTRY(wColumn,wColLabel).
  END.
  DO wRow = 1 TO chMSChart:RowCount:
      ASSIGN chMSChart:Row      = wRow
             chMSChart:RowLabel = ENTRY(wRow,wRowLabel).
      DO wColumn = 1 TO chMSChart:ColumnCount:
        ASSIGN chMSChart:Column = wColumn
               chMSChart:Data   = ENTRY(wRow,ENTRY(wColumn,wColValues,";")).
      END.
  END.

  ASSIGN comSerColl = chMSChart:Plot
         comSerColl = comSerColl:SeriesCollection.
  DO wAntCol = 1 TO NUM-ENTRIES(wColLabel): 
     if entry(wAntCol,wColEnable) = "0" then 
         ASSIGN comSeries = comSerColl:ITEM(wAntCol)
                comPos = comSeries:POSITION
                comPos:HIDDEN = TRUE.
  END.
  chMSChart:ENABLED = TRUE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

