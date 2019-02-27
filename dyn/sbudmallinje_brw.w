&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: 

  Description: 

update_varebok_from_artbas.p
vareboklinjeDvelgfelter.w           
vareboklinje_refresh_all.p           
           
           
           
  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

DEF VAR bOk               AS LOG  NO-UNDO.
DEF VAR ix                AS INT  NO-UNDO.
                          
DEF VAR hToolbar          AS HANDLE NO-UNDO.
DEF VAR hBrowse           AS HANDLE NO-UNDO.
DEF VAR hBrowse-2         AS HANDLE NO-UNDO.
DEF VAR hFilterWindow     AS HANDLE NO-UNDO.

DEF VAR hParent           AS HANDLE NO-UNDO.
DEF VAR hParentBrowse     AS HANDLE NO-UNDO.

DEF VAR hChild            AS HANDLE NO-UNDO.
DEF VAR hDetail           AS HANDLE NO-UNDO.
DEF VAR cRowidList        AS CHAR   NO-UNDO.
DEF VAR hSearchField      AS HANDLE NO-UNDO.
DEF VAR hFlatView              AS HANDLE NO-UNDO.

DEF VAR hfiProsent        AS HANDLE NO-UNDO.
DEF VAR hfiDbProsent      AS HANDLE NO-UNDO.
DEF VAR hfi2Prosent       AS HANDLE NO-UNDO.
DEF VAR hfi2DbProsent     AS HANDLE NO-UNDO.
DEF VAR hfiDbSnitt        AS HANDLE NO-UNDO.
DEF VAR hfcMerknad        AS HANDLE NO-UNDO.

DEF VAR hBuffer           AS HANDLE NO-UNDO.
DEF VAR hBuffer-2         AS HANDLE NO-UNDO.

/*Valg av poster*/
DEF VAR cDato        AS CHAR NO-UNDO.
DEF VAR iReturn      AS INT  NO-UNDO.
DEF VAR cLand        AS CHAR NO-UNDO.
DEF VAR cHelligDager AS CHAR NO-UNDO.
DEF VAR iAar         AS INT NO-UNDO.

/*Browse stuff*/
DEF VAR hbcProsent    AS HANDLE NO-UNDO.
DEF VAR hbfProsent    AS HANDLE NO-UNDO.
DEF VAR hbcDbProsent  AS HANDLE NO-UNDO.
DEF VAR hbfDbProsent  AS HANDLE NO-UNDO.
DEF VAR hbc2Prosent   AS HANDLE NO-UNDO.
DEF VAR hbf2Prosent   AS HANDLE NO-UNDO.
DEF VAR hbc2DbProsent AS HANDLE NO-UNDO.
DEF VAR hbf2DbProsent AS HANDLE NO-UNDO.
DEF VAR hbclinjeSum   AS HANDLE NO-UNDO.
DEF VAR hbflinjeSum   AS HANDLE NO-UNDO.
DEF VAR hbctmpdMDag   AS HANDLE NO-UNDO.
DEF VAR hbftmpdMDag   AS HANDLE NO-UNDO.
DEF VAR hbcDbSnitt    AS HANDLE NO-UNDO.
DEF VAR hbfDbSnitt    AS HANDLE NO-UNDO.

DEF VAR hbctmpcUDag   AS HANDLE NO-UNDO.
DEF VAR hbftmpcUDag   AS HANDLE NO-UNDO.
DEF VAR cParent       AS CHAR NO-UNDO.

DEF VAR bFlag  AS LOG NO-UNDO.
DEF VAR bFlag2 AS LOG NO-UNDO.

DEF VAR iGreen    AS INT INIT 10 NO-UNDO.
DEF VAR iRed      AS INT INIT 12 NO-UNDO.
DEF VAR iYellow   AS INT INIT 14 NO-UNDO.
DEF VAR iLtYellow AS INT INIT 16 NO-UNDO.
DEF VAR iLtRed    AS INT INIT 17 NO-UNDO.
DEF VAR iLtGreen  AS INT INIT 18 NO-UNDO.

DEF BUFFER tmpSBudMalManed FOR SBudMalManed.

/* DEF VAR iFontWingdings    AS INT    NO-UNDO.                                                    */
/* iFontWingdings = DYNAMIC-FUNCTION("setAppFont","Wingdings, size=11 Script=symbol","") NO-ERROR. */

{buildfunction.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS rectBrowse rectToolBar rectBrowse-2 
&Scoped-Define DISPLAYED-OBJECTS ProsentTotal FI-Mal DbSnittProsent ~
ProsentTotal-2 DbSnittProsent2 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD AdjustBrowseColumns C-Win 
FUNCTION AdjustBrowseColumns RETURNS LOGICAL
  ( INPUT ihBrowse     AS HANDLE,
    INPUT icBrowseName AS CHAR) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getFrameHandle C-Win 
FUNCTION getFrameHandle RETURNS HANDLE
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getParentBrowse C-Win 
FUNCTION getParentBrowse RETURNS HANDLE
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD InitializeResize C-Win 
FUNCTION InitializeResize RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setParent C-Win 
FUNCTION setParent RETURNS LOGICAL
  ( INPUT ihParent AS HANDLE )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setQuery C-Win 
FUNCTION setQuery RETURNS LOGICAL
  ( INPUT ihQuery AS HANDLE )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD ViewQueryStat C-Win 
FUNCTION ViewQueryStat RETURNS LOGICAL
  ( INPUT ihQueryObject AS HANDLE )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE VARIABLE DbSnittProsent AS DECIMAL FORMAT "->>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN NATIVE 
     SIZE 13.4 BY 1 TOOLTIP "Snitt db% for alle dager med db% > 0"
     BGCOLOR 15 FGCOLOR 0 FONT 6 NO-UNDO.

DEFINE VARIABLE DbSnittProsent2 AS DECIMAL FORMAT "->>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN NATIVE 
     SIZE 12.6 BY 1 TOOLTIP "Snitt db% for alle dager med db% > 0"
     BGCOLOR 15 FGCOLOR 0 FONT 6 NO-UNDO.

DEFINE VARIABLE FI-Mal AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN NATIVE 
     SIZE 149 BY 1
     FONT 6 NO-UNDO.

DEFINE VARIABLE ProsentTotal AS DECIMAL FORMAT "->>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN NATIVE 
     SIZE 17 BY 1 TOOLTIP "Sum omsetning%"
     BGCOLOR 15 FGCOLOR 0 FONT 6 NO-UNDO.

DEFINE VARIABLE ProsentTotal-2 AS DECIMAL FORMAT "->>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN NATIVE 
     SIZE 16.8 BY 1 TOOLTIP "Sum omsetning%"
     BGCOLOR 15 FGCOLOR 0 FONT 6 NO-UNDO.

DEFINE RECTANGLE rectBrowse
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 101.6 BY 12.38.

DEFINE RECTANGLE rectBrowse-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 96.6 BY 12.38.

DEFINE RECTANGLE rectToolBar
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 28 BY .95.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     ProsentTotal AT ROW 15 COL 16 COLON-ALIGNED NO-LABEL WIDGET-ID 4
     FI-Mal AT ROW 1.19 COL 50 COLON-ALIGNED NO-LABEL WIDGET-ID 12
     DbSnittProsent AT ROW 15 COL 65 RIGHT-ALIGNED NO-LABEL WIDGET-ID 10
     ProsentTotal-2 AT ROW 15 COL 131.2 COLON-ALIGNED NO-LABEL WIDGET-ID 6
     DbSnittProsent2 AT ROW 15 COL 148.6 COLON-ALIGNED NO-LABEL WIDGET-ID 8
     rectBrowse AT ROW 2.43 COL 1.4
     rectToolBar AT ROW 1.24 COL 2
     rectBrowse-2 AT ROW 2.43 COL 104.4 WIDGET-ID 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 201.2 BY 15.29.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* SUPPRESS Window definition (used by the UIB) 
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Tellelinje"
         HEIGHT             = 15.52
         WIDTH              = 201
         MAX-HEIGHT         = 57.14
         MAX-WIDTH          = 384
         VIRTUAL-HEIGHT     = 57.14
         VIRTUAL-WIDTH      = 384
         MAX-BUTTON         = no
         RESIZE             = no
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
                                                                        */
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME
ASSIGN C-Win = CURRENT-WINDOW.


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB C-Win 
/* ************************* Included-Libraries *********************** */

{incl/devmode.i}
{incl/custdevmode.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME L-To-R,COLUMNS                                            */
/* SETTINGS FOR FILL-IN DbSnittProsent IN FRAME DEFAULT-FRAME
   NO-ENABLE ALIGN-R                                                    */
/* SETTINGS FOR FILL-IN DbSnittProsent2 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-Mal IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ProsentTotal IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ProsentTotal-2 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME DEFAULT-FRAME
/* Query rebuild information for FRAME DEFAULT-FRAME
     _Options          = "SHARE-LOCK KEEP-EMPTY"
     _Query            is NOT OPENED
*/  /* FRAME DEFAULT-FRAME */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Tellelinje */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Tellelinje */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE DO:
  PUBLISH "InvalidateHandle".
  IF VALID-HANDLE(hChild) THEN APPLY "close" TO hChild.
END.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  
  DEF NEW SHARED VAR wCurrLng   AS CHAR   INITIAL "DES" NO-UNDO.
  DEF NEW SHARED VAR wLngHandle AS HANDLE NO-UNDO.
  RUN enable_UI.
  DYNAMIC-FUNCTION("setParent",SOURCE-PROCEDURE).

  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

{incl/supptrigg.i hBrowse}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ClearFilterRecord C-Win 
PROCEDURE ClearFilterRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DO WITH FRAME {&FRAME-NAME}:
      /*
      ASSIGN
          FraEDato:SCREEN-VALUE    = ?
          TilEDato:SCREEN-VALUE    = ?
          Sasong:SCREEN-VALUE      = ''
          LevKod:SCREEN-VALUE      = ''
          Beskr:SCREEN-VALUE       = ''
          LevFargKod:SCREEN-VALUE  = ''
          Storl:SCREEN-VALUE       = ''
          LevNr:SCREEN-VALUE       = ''
          ArtikkelNr:SCREEN-VALUE  = ''
          StrekKode:SCREEN-VALUE   = ''
          vg:SCREEN-VALUE          = ''
          fraEtid:SCREEN-VALUE     = ''
          tilEtid:SCREEN-VALUE     = ''
          .
      */    
      DYNAMIC-FUNCTION("setSortString",hBrowse,"").
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DefaultActionBrowse C-Win 
PROCEDURE DefaultActionBrowse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  /*
  IF DYNAMIC-FUNCTION("getCurrentObject") = hBrowse THEN    
    IF hBrowse:QUERY:GET-BUFFER-HANDLE(1):AVAIL THEN
      RUN ArtikkelKortRecord.
  */
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI C-Win  _DEFAULT-DISABLE
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
  HIDE FRAME DEFAULT-FRAME.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DisplayRecord C-Win 
PROCEDURE DisplayRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
  hToolbar = DYNAMIC-FUNCTION("NewToolBar",
                    rectToolBar:HANDLE,             /* Rectangle to define coordinates for toolbar */
                    "Fil",                         /* Corresponding menu label - no menu if blank */
                    "New;&Ny,delete;Slette,Print;S&kriv,rule,excel;Eksporter til E&xcel" 
                     + ",Linjeregistrering;Linjeregistrering,ArtikkelKort;Artikkelkort"
                     + ",HTexport;Varefil til håndterminal,HTsetup;Håndterminal oppsett"
                     + ",HentIkkeTalt;Hent alle størrelser på artiklene,Strekkode;Strekkoder"
                    ,"maxborder").                  /* Misc - enable, maxborder.. */                /*action;label;tooltip;Method;image*/
 "AntTaltLikNull;Sett antall talt = 0,AntTaltLikAntPar;Sett antall talt lik antal par,TaBortTommeLinjer;Ta bort linjer med 0 i lager og antall talt,TaBortPosLinjer;Ta bort linjer med pos. differanse,TaBortNegLinjer;Ta bort linjer med neg. differanse"

------------------------------------------------------------------------------*/
  DEF VAR cDisabledEvents    AS CHAR NO-UNDO.

  IF DYNAMIC-FUNCTION("getCurrentObject") = hBrowse THEN    
  DO WITH FRAME {&FRAME-NAME}:
    /*
    IF hBuffer:AVAILABLE AND hParentBrowse:QUERY:GET-BUFFER-HANDLE(1):AVAIL THEN
    DO:
      /*
      DYNAMIC-FUNCTION("CreateParentLink",hBrowse-2,hBrowse,"KobletTilTelleNr;tellenr").
      DYNAMIC-FUNCTION("setAttribute",hBrowse-2,'QueryJoin',",each buf1_tellelinje WHERE buf1_tellelinje.tellenr = buf1_tellehode.tellenr "  
                       + " AND buf1_tellelinje.artikkelnr = " + STRING(hBuffer:BUFFER-FIELD('artikkelnr'):BUFFER-VALUE) 
                       + " AND buf1_tellelinje.storl = " + QUOTER(hBuffer:BUFFER-FIELD('storl'):BUFFER-VALUE) 
                       + " AND buf1_tellelinje.AntallTalt gt 0").
      */

      /* --- Knappestyring ------ */
      /*
      IF (hParentBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("Oppdatert"):BUFFER-VALUE <> ?) THEN 
        cDisabledEvents = 'New,Delete,LinjeRegistrering,HentIkkeTalt,AntTaltLikNull,AntTaltLikAntPar,TaBortTommeLinjer,TaBortPosLinjer,TaBortNegLinjer,KobleTelleliste,LesInnFil'.
      ELSE
        cDisabledEvents = ''.
      DYNAMIC-FUNCTION("setAttribute",hToolbar,"DisabledEvents",cDisabledEvents).
    */
    END.
    /*
    IF hParentBrowse:QUERY:GET-BUFFER-HANDLE(1):AVAIL AND hParentBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('TelleType'):BUFFER-VALUE = 1 
      AND INDEX('HentLokasjon',cDisabledEvents) LE 0 THEN
        cDisabledEvents = cDisabledEvents + ',HentLokasjon'.
    DYNAMIC-FUNCTION("setAttribute",hToolbar,"DisabledEvents",cDisabledEvents).
    */
    */
  END.

  RUN SUPER.

  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI C-Win  _DEFAULT-ENABLE
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
  DISPLAY ProsentTotal FI-Mal DbSnittProsent ProsentTotal-2 DbSnittProsent2 
      WITH FRAME DEFAULT-FRAME.
  ENABLE rectBrowse rectToolBar rectBrowse-2 
      WITH FRAME DEFAULT-FRAME.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ExcelRecord C-Win 
PROCEDURE ExcelRecord :
/*------------------------------------------------------------------------------
                        Purpose:                                                                                                                                          
                        Notes:                                                                                                                                            
        ------------------------------------------------------------------------------*/
  DYNAMIC-FUNCTION("toExcelViaFile",hBrowse,0).         

  /*
  DEFINE VARIABLE iMalId AS INTEGER NO-UNDO.
  
  ASSIGN 
    iMalId = INT(hParentBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('MalId'):BUFFER-VALUE) NO-ERROR.
  IF ERROR-STATUS:ERROR 
    THEN RETURN.
  ELSE RUN SBudMal_Til_Excel.p (iMalId).
    
  /* RUN SUPER.*/
  */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ExcelSheetParams C-Win 
PROCEDURE ExcelSheetParams :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
PUBLISH "ExcelSheetParams" (ihObject,chExcelInstance,chWorkbook,chWorksheet,iCount - 1).
------------------------------------------------------------------------------*/
DEF INPUT PARAMETER ipObject        AS HANDLE     NO-UNDO.
DEF INPUT PARAMETER ipExcelInstance AS CHAR       NO-UNDO.
DEF INPUT PARAMETER ipWorkbook      AS COM-HANDLE NO-UNDO.
DEF INPUT PARAMETER ipWorksheet     AS COM-HANDLE NO-UNDO.
DEF INPUT PARAMETER ipI             AS INT        NO-UNDO.

  ipWorkSheet:Range("D:F"):NumberFormat = "@".
  ipWorkSheet:Range("P:P"):NumberFormat = "@".
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE fargeTotaler C-Win 
PROCEDURE fargeTotaler :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  /* Omsetnings% Browse 1 */
  IF DYNAMIC-FUNCTION("getCurrentObject") = hfiProsent THEN 
  DO:
    DYNAMIC-FUNCTION("getLocalQueryStat",hBrowse,"").
    ProsentTotal:SCREEN-VALUE IN FRAME {&FRAME-NAME} = DYNAMIC-FUNCTION("getAttribute",hBrowse,"LocalStatValueProsent").
  END.

  IF hBuffer:AVAIL AND DYNAMIC-FUNCTION("getCurrentObject") = hfiDbProsent THEN 
  DO:
      DYNAMIC-FUNCTION("getLocalQueryStat",hBrowse,"").
      DYNAMIC-FUNCTION("RefreshRowids",hBrowse,hBuffer:BUFFER-FIELD("RowIdent1"):BUFFER-VALUE). 
      
      DbSnittProsent:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(DEC(DYNAMIC-FUNCTION("getAttribute",hBrowse,"LocalStatValueDbProsent")) / hBuffer:BUFFER-FIELD("iAntMndDbGT0"):BUFFER-VALUE).
      IF hBuffer:BUFFER-FIELD("iAntMndDbGT0"):BUFFER-VALUE = 0 THEN
          DbSnittProsent:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ''.
  END.

  /* Omsetnings% Browse 2 */
  IF hBuffer-2:AVAIL AND hBuffer:AVAIL AND DYNAMIC-FUNCTION("getCurrentObject") = hfi2Prosent THEN 
  DO:
    DYNAMIC-FUNCTION("getLocalQueryStat",hBrowse-2,"").
    DYNAMIC-FUNCTION("RefreshRowids",hBrowse,hBuffer-2:BUFFER-FIELD("RowIdent1"):BUFFER-VALUE). 
    ProsentTotal-2:SCREEN-VALUE IN FRAME {&FRAME-NAME} = DYNAMIC-FUNCTION("getAttribute",hBrowse-2,"LocalStatValueProsent").

    DYNAMIC-FUNCTION("RefreshRowids",hBrowse,hBuffer:BUFFER-FIELD("RowIdent1"):BUFFER-VALUE). 

    DYNAMIC-FUNCTION("getLocalQueryStat",hBrowse,"").
    ProsentTotal:SCREEN-VALUE IN FRAME {&FRAME-NAME} = DYNAMIC-FUNCTION("getAttribute",hBrowse,"LocalStatValueProsent").
  END.

  /* Omsetnings% Browse 2 */
  IF hBuffer:AVAIL AND hBuffer-2:AVAIL AND DYNAMIC-FUNCTION("getCurrentObject") = hfi2DbProsent THEN 
  DO:
      DYNAMIC-FUNCTION("getLocalQueryStat",hBrowse-2,"").
      DYNAMIC-FUNCTION("RefreshRowids",hBrowse-2,hBuffer-2:BUFFER-FIELD("RowIdent1"):BUFFER-VALUE). 

      DbSnittProsent2:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(DEC(DYNAMIC-FUNCTION("getAttribute",hBrowse-2,"LocalStatValueDbProsent")) / hBuffer-2:BUFFER-FIELD("iAntDagDbGT0"):BUFFER-VALUE).
      IF hBuffer-2:BUFFER-FIELD("iAntDagDbGT0"):BUFFER-VALUE = 0 THEN
          DbSnittProsent2:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ''.

      DYNAMIC-FUNCTION("RefreshRowids",hBrowse,hBuffer:BUFFER-FIELD("RowIdent1"):BUFFER-VALUE). 
  END.

  ProsentTotal:BGCOLOR IN FRAME {&FRAME-NAME} =  IF 100.00 <> DEC(ProsentTotal:SCREEN-VALUE IN FRAME {&FRAME-NAME}) AND 
                                                  0 <> DEC(ProsentTotal:SCREEN-VALUE IN FRAME {&FRAME-NAME}) 
                                                 THEN iRed ELSE ?. 
  ProsentTotal-2:BGCOLOR IN FRAME {&FRAME-NAME} =  IF 100.00 <> DEC(ProsentTotal-2:SCREEN-VALUE IN FRAME {&FRAME-NAME}) AND 
                                                  0 <> DEC(ProsentTotal-2:SCREEN-VALUE IN FRAME {&FRAME-NAME})  
                                                 THEN iRed ELSE ?. 

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE FlatViewRecord C-Win 
PROCEDURE FlatViewRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
RUN SUPER.

/*
RUN InitQuery IN hFlatView (
                    ""   /* Filter fields (comma sep) */
                   ,""   /* Filter operators (comma sep) */
                   ,""   /* Filter values (pipe sep) */
                   ,""   /* Initial sort column(s) (comma sep) */
                   ,""   /* Distinct columns (comma sep) */
                   ,"Prosent,KostBekreftet"   /* Columns to accumulate (comma sep) */
                   ).
*/
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitializeObject C-Win 
PROCEDURE InitializeObject :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DO WITH FRAME {&FRAME-NAME}:
  /* TN 7/1-10 Dette slå ar spørsmål på sesjonsninvå. Skal ikke gjøres. Spørsmål skal heller ikke slås av. */
  /*DYNAMIC-FUNCTION("setAttribute",SESSION,'DropExcelWarning','YES').*/

  DYNAMIC-FUNCTION("NewObject",THIS-PROCEDURE:CURRENT-WINDOW,THIS-PROCEDURE,"procedure").
  hBrowse = DYNAMIC-FUNCTION("NewBrowse",
                              rectBrowse:HANDLE,
                              100,
                              "MULTIPLE",
                              "SBudMalManed"
                               + ";!MalId|Mal"
                               + ";!AarMnd"
                               + ";+tmpAar|Int|9999|tmpAar(ROWID)|År"
                               + ";+tmpMnd|Char|x(4)|tmpMnd(ROWID)|Mnd"
                               + ";Prosent|Omsetn% Mnd|->>>9.99"
                               + ";!+chkProc|logical|*/|chkProc(ROWID)|"
                               + ";+linjeSum|decimal|->>>9.99|linjeSum(ROWID)|Sum Oms% Dag"
                               + ";DbProsent|Db% Mnd|->>>9.99"
                               + ";+DbSnitt|decimal|->>>9.99|DbSnitt(ROWID)|Snitt Db% Dag%"
                               + ";!RegistrertDato"
                               + ";!RegistrertTid"
                               + ";!+rTid|character|x(6)|rTid(ROWID)|Kl"
                               + ";!RegistrertAv"
                               + ";EDato"
                               + ";!ETid"
                               + ";+endretTid|character|x(6)|endretTid(ROWID)|Kl"
                               + ";BrukerID|BrukerGG|x(10)"
                               + ";!+iAntMndDbGT0|integer|->>>9|iAntMndDbGT0(ROWID)|AntMndDbGT0"
                             ,"WHERE false"
                             ,"").
  /* Henter buffer handle for å kunne lese ut verdier fra feltene i bufferet. */
  hBuffer = hBrowse:QUERY:GET-BUFFER-HANDLE(1).

  DYNAMIC-FUNCTION("setAttribute",hBrowse,
                "queryStatFields","Prosent,DbProsent").

  DYNAMIC-FUNCTION("setSortString",hBrowse,"tmpAar").   

  /* hSearchField = DYNAMIC-FUNCTION("NewBrowseSearchField",SearchField:HANDLE,hBrowse,1).
  DYNAMIC-FUNCTION("CreateObjectLink",hSearchField,hBrowse). */
  
  DYNAMIC-FUNCTION("setAttribute",hBrowse,"customDeleteValProc","=delval_sbudmalmaned.p").
  DYNAMIC-FUNCTION('setAttribute',hBrowse,'calcfieldproc','sbudmalmaned_brwcalc.p').
  DYNAMIC-FUNCTION("createObjectLink",hBrowse,THIS-PROCEDURE).
  
  hToolbar = DYNAMIC-FUNCTION("NewToolBar",
                    rectToolBar:HANDLE,             /* Rectangle to define coordinates for toolbar */
                    "Fil",                         /* Corresponding menu label - no menu if blank */
                    "flatview,refresh" 
                     + ",rule,RowsToBatch;Ant. rader i resultatsett¤enable"
                     /*+ ",Linjeregistrering;Linjeregistrering"*/
                    ,"maxborder").                  /* Misc - enable, maxborder.. */                /*action;label;tooltip;Method;image*/

  /*
  DYNAMIC-FUNCTION("NewMenuBand",
                  hBrowse,  /* parent widget */
                  "AntTaltLikNull;Sett antall talt = 0,AntTaltLikAntPar;Sett antall talt lik antal par,TaBortTommeLinjer;Ta bort linjer med 0 i lager og antall talt,TaBortPosLinjer;Ta bort linjer med pos. differanse,TaBortNegLinjer;Ta bort linjer med neg. differanse,OppdVVarekostPaLinjer;Oppdater varekost på linje(r)"
                 ,"").   
  */
  DYNAMIC-FUNCTION("CreateObjectLink",hBrowse,hToolbar).  
  
/*   fillTime(fraEtid:HANDLE,'00:00','23:59',15).       */
/*   tilETid:LIST-ITEM-PAIRS = fraETid:LIST-ITEM-PAIRS. */

  /* Oppdaterbar felt i browse */
  hfiProsent = DYNAMIC-FUNCTION("NewBrowseFillIn",
                      hBrowse,          
                      "Prosent",     
                      "Prosent",     
                      "","","","").                
  DYNAMIC-FUNCTION("setAttribute",hfiProsent,"refreshrow","yes").          /* Refresh the row after update */
  DYNAMIC-FUNCTION("CreateOverlayLink",hBrowse,hfiProsent,"Prosent").
  
  /* Oppdaterbar felt i browse */
  hfiDbProsent = DYNAMIC-FUNCTION("NewBrowseFillIn",
                      hBrowse,          
                      "DbProsent",     
                      "DbProsent",     
                      "","","","").                
  DYNAMIC-FUNCTION("setAttribute",hfiDbProsent,"refreshrow","yes").          /* Refresh the row after update */
  DYNAMIC-FUNCTION("CreateOverlayLink",hBrowse,hfiDbProsent,"DbProsent").

  DYNAMIC-FUNCTION("setAttribute",hBrowse,"customUpdateValProc","=updval_SBudMalManed.p").
    
  /* Oppdaterbar felt i browse */
  /*
  hfcMerknad = DYNAMIC-FUNCTION("NewBrowseFillIn",
                    hBrowse,          
                    "Merknad",     
                    "Merknad",     
                    "","","","").                
  DYNAMIC-FUNCTION("setAttribute",hfcMerknad,"refreshrow","yes").          /* Refresh the row after update */
  DYNAMIC-FUNCTION("CreateOverlayLink",hBrowse,hfcMerknad,"Merknad").
  */
  hBrowse-2 = DYNAMIC-FUNCTION("NewBrowse"
              ,rectBrowse-2:HANDLE
              ,100
              ,""
              ,"SBudMalDag"
               + ";!MalId|Mal"
               + ";!AarMnd"
               + ";!AarMndDag"
               + ";+tmpMnd|Character|x(4)|tmpMnd(ROWID)|Mnd"
               + ";+tmpdMDag|date|99/99/99|tmpdMDag(ROWID)|Dato"
               + ";+tmpcUDag|Character|x(4)|tmpcUDag(ROWID)|UDag"
               + ";Prosent|Omsetn% Dag|->>>9.99" 
               + ";DbProsent|Db% Dag|->>>9.99" 
               + ";EDato"
               + ";!ETid"
               + ";+endretTid|character|x(6)|endretTid(ROWID)|Kl"
               + ";BrukerID|Bruker|x(10)"
               + ";!+iAntDagDbGT0|integer|->>>9|iAntDagDbGT0(ROWID)|AntDagDbGT0"
              ,"WHERE false"
              ,"").
  /* Henter buffer handle for å kunne lese ut verdier fra feltene i bufferet. */
  hBuffer-2 = hBrowse-2:QUERY:GET-BUFFER-HANDLE(1).

  DYNAMIC-FUNCTION('setAttribute',hBrowse-2,'calcfieldproc','sbudmaldag_brwcalc.p').
  DYNAMIC-FUNCTION("setAttribute",hBrowse-2,
                "queryStatFields","Prosent,DbProsent").

  DYNAMIC-FUNCTION("setSortString",hBrowse-2,"AarMndDag"). 

  /* Knytter Dag browser til måned browser. */
  DYNAMIC-FUNCTION("CreateParentLink",hBrowse-2,hBrowse,"MalId,AarMnd").
/*   DYNAMIC-FUNCTION("createObjectLink",hBrowse,hBrowse-2). */

  /* Oppdaterbar felt i browse */
  hfi2Prosent = DYNAMIC-FUNCTION("NewBrowseFillIn",
                      hBrowse-2,          
                      "Prosent",     
                      "Prosent",     
                      "","","","").                
  DYNAMIC-FUNCTION("setAttribute",hfi2Prosent,"refreshrow","yes").          /* Refresh the row after update */  
  DYNAMIC-FUNCTION("CreateOverlayLink",hBrowse-2,hfi2Prosent,"Prosent").

  /* Oppdaterbar felt i browse */
  hfi2DbProsent = DYNAMIC-FUNCTION("NewBrowseFillIn",
                      hBrowse-2,          
                      "DbProsent",     
                      "DbProsent",     
                      "","","","").                
  DYNAMIC-FUNCTION("setAttribute",hfi2DbProsent,"refreshrow","yes").          /* Refresh the row after update */  
  DYNAMIC-FUNCTION("CreateOverlayLink",hBrowse-2,hfi2DbProsent,"DbProsent").
  
  DYNAMIC-FUNCTION("setAttribute",hBrowse-2,"customUpdateValProc","=updval_SBudMalDag.p").


  DYNAMIC-FUNCTION("setNoResizeX", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE, "+," + hBrowse:NAME).

END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InvalidateHandle C-Win 
PROCEDURE InvalidateHandle :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DYNAMIC-FUNCTION('setCurrentObject',hBrowse).  
DYNAMIC-FUNCTION("CreateObjectLink",hBrowse,hToolbar).  
hChild = ?.
  APPLY 'value-changed' TO hBrowse.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LeaveBrowseFillIn C-Win 
PROCEDURE LeaveBrowseFillIn :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

RUN SUPER.

RUN fargeTotaler.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE MoveToTop C-Win 
PROCEDURE MoveToTop :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
FRAME {&FRAME-NAME}:MOVE-TO-TOP().
APPLY "entry" TO hBrowse.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OpenQuery C-Win 
PROCEDURE OpenQuery :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  RUN SUPER.
  
  RUN fargeTotaler.
  
  IF hBuffer:AVAIL THEN
  DO:
      DYNAMIC-FUNCTION("getLocalQueryStat",hBrowse,"").
      DYNAMIC-FUNCTION("RefreshRowids",hBrowse,hBuffer:BUFFER-FIELD("RowIdent1"):BUFFER-VALUE). 
      DbSnittProsent:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(DEC(DYNAMIC-FUNCTION("getAttribute",hBrowse,"LocalStatValueDbProsent")) / hBuffer:BUFFER-FIELD("iAntMndDbGT0"):BUFFER-VALUE).
      IF hBuffer:BUFFER-FIELD("iAntMndDbGT0"):BUFFER-VALUE = 0 THEN
          DbSnittProsent:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ''.
  END.

  IF hBuffer-2:AVAIL THEN
  DO:

      DYNAMIC-FUNCTION("getLocalQueryStat",hBrowse-2,"").
      DbSnittProsent2:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(DEC(DYNAMIC-FUNCTION("getAttribute",hBrowse-2,"LocalStatValueDbProsent")) / hBuffer-2:BUFFER-FIELD("iAntDagDbGT0"):BUFFER-VALUE).
      IF hBuffer-2:BUFFER-FIELD("iAntDagDbGT0"):BUFFER-VALUE = 0 THEN
          DbSnittProsent2:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ''.
  END.

/* To alternative måter å hente fra parent */
/*   cParent = DYNAMIC-FUNCTION("getFieldValues","budmalhode",                                                                                            */
/*                                              "WHERE mal-id = " + STRING(hParentBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("mal-id"):BUFFER-VALUE), */
/*                                             "Aar,MalBeskrivelse").                                                                                     */
/*   cParent = DYNAMIC-FUNCTION("getFieldValues","sbudmalhode",                                                             */
/*                                              "WHERE malid = " + STRING(hParentBrowse:QUERY:GET-BUFFER-HANDLE(1)::Malid), */
/*                                             "Aar,MalBeskrivelse").                                                       */
  
  IF hParentBrowse:QUERY:GET-BUFFER-HANDLE(1):AVAIL THEN
  DO:
      FI-Mal:SCREEN-VALUE = "Mal: " +
                            STRING(hParentBrowse:QUERY:GET-BUFFER-HANDLE(1)::MalId) + " " + 
                            "Butikk: " + 
                            STRING(hParentBrowse:QUERY:GET-BUFFER-HANDLE(1)::ButikkNr) + " " + 
                            "År: " + 
                            STRING(hParentBrowse:QUERY:GET-BUFFER-HANDLE(1)::Aar) + " " + 
                            hParentBrowse:QUERY:GET-BUFFER-HANDLE(1)::MalBeskrivelse.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OppdaterLagerantallRecord C-Win 
PROCEDURE OppdaterLagerantallRecord :
/*------------------------------------------------------------------------------
  Purpose: Oppdaterer lagerantall på alle valgte linjer eller alle linjer i utvalget.     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR iReturn     AS INT  NO-UNDO.
  DEF VAR ocValue     AS CHAR NO-UNDO.
  DEF VAR cStatusList AS CHAR NO-UNDO.
  DEF VAR cParam      AS CHAR NO-UNDO.

  bOK = FALSE.
  MESSAGE 
      "Oppdatering av lagerantall på linjene i tellelisten, er bare aktuelt å gjøre dersom man vet at tellelisten ble 'bygget' " + 
      "på feil tidspunkt, og at lagerantallet på linjene må oppdateres med aktuelt lagerantall." SKIP(1)
      "Er du sikker på at det er dette du vil gjøre?" SKIP(1)
      "På linjer som oppdateres, vil lagerantall bli satt til aktuelt lagerantall og eventuell differanse regnet ut på nytt. " +
      "Du vil få et nytt spørsmål om det er alle linjer i utvalget eller bare markerte linjer som skal oppdateres. " + 
      "Oppdatering av tellelisten vil ikke bli gjort før du har besvart neste spørsmål."
      VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE bOK.
  IF bOk = FALSE THEN
      RETURN.

  RUN JBoxBrowseMsgUpdateVal.w ("Oppdater lagerantall på valgte/alle artikler ?",
                                hBrowse:NUM-SELECTED-ROWS,
                                IF INT(DYNAMIC-FUNCTION("getAttribute",hBrowse,"Totalcount")) LT INT(DYNAMIC-FUNCTION("getAttribute",hBrowse,"rowstobatch")) THEN
                                  INT(DYNAMIC-FUNCTION("getAttribute",hBrowse,"Totalcount"))
                                ELSE 99999,
                                "",
                                OUTPUT ocValue, 
                                OUTPUT iReturn).

IF iReturn = 1 THEN
  bOk = DYNAMIC-FUNCTION("ProcessQuery",hBrowse,"oppdater_lager_til_telling.p",STRING(hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('TelleNr'):BUFFER-VALUE)).
ELSE IF iReturn = 2 THEN
DO:
  cRowIdList = ''.
  DO ix = 1 TO hBrowse:NUM-SELECTED-ROWS:
    IF hBrowse:FETCH-SELECTED-ROW(ix) THEN
      cRowIdList = cRowIdList + hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('RowIdent1'):BUFFER-VALUE + ','.
  END.
  IF cRowIdList <> '' THEN cRowIdList = 'ROWID,' + RIGHT-TRIM(cRowIdList,',').
  bOk = DYNAMIC-FUNCTION("ProcessSelectedRows",hBrowse,"oppdater_lager_til_telling.p",STRING(hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('TelleNr'):BUFFER-VALUE) + '|' + cRowIdList).
END.
ELSE
  LEAVE.
IF NOT bOK THEN
  DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"Feil i sending av informasjon ",""). 

RUN InvokeMethod (hBrowse,'OpenQuery').
  /*   IF iReturn = 1 THEN RUN InvokeMethod (hBrowse,'OpenQuery'). ELSE DYNAMIC-FUNCTION("RefreshRowids",hBrowse,TRIM(cRowIdList,",")). */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OppdVVarekostPaLinjerRecord C-Win 
PROCEDURE OppdVVarekostPaLinjerRecord :
/*------------------------------------------------------------------------------
                        Purpose: Oppdaterer linje(r) med vektet varekost og regner om summer.                                                                                                                                     
                        Notes:                                                                                                                                            
        ------------------------------------------------------------------------------*/
DEF VAR iReturn     AS INT  NO-UNDO.
DEF VAR ocValue     AS CHAR NO-UNDO.
DEF VAR cStatusList AS CHAR NO-UNDO.

  RUN JBoxBrowseMsgUpdateVal.w ("Oppdater varekost på linje(r)?",
                                hBrowse:NUM-SELECTED-ROWS,
                                IF INT(DYNAMIC-FUNCTION("getAttribute",hBrowse,"Totalcount")) LT INT(DYNAMIC-FUNCTION("getAttribute",hBrowse,"rowstobatch")) THEN
                                  INT(DYNAMIC-FUNCTION("getAttribute",hBrowse,"Totalcount"))
                                ELSE 99999,
                                "",
                                OUTPUT ocValue, 
                                OUTPUT iReturn).

IF iReturn = 1 THEN
  bOk = DYNAMIC-FUNCTION("ProcessQuery",hBrowse,"tellelinje_OppdVVarekost.p",'').
ELSE IF iReturn = 2 THEN
DO:
  DO ix = 1 TO hBrowse:NUM-SELECTED-ROWS:
    IF hBrowse:FETCH-SELECTED-ROW(ix) THEN
      cRowIdList = cRowIdList + hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('RowIdent1'):BUFFER-VALUE + ','.
  END.
  bOk = DYNAMIC-FUNCTION("ProcessSelectedRows",hBrowse,"tellelinje_OppdVVarekost.p",'').
END.
ELSE
  LEAVE.
IF NOT bOK THEN
  DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"Feil i sending av informasjon ",""). 

PUBLISH 'VaretellingOppdaterSumRecord'.
RUN InvokeMethod (hBrowse,'OpenQuery').
/*   IF iReturn = 1 THEN RUN InvokeMethod (hBrowse,'OpenQuery'). ELSE DYNAMIC-FUNCTION("RefreshRowids",hBrowse,TRIM(cRowIdList,",")). */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PrintRecord C-Win 
PROCEDURE PrintRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
  
IF iReturn = 1 THEN  
  bOk = DYNAMIC-FUNCTION("ProcessQuery",hBrowse,"vpiartbas_report.p",'').
ELSE IF iReturn = 2 THEN
DO:
  DO ix = 1 TO hBrowse:NUM-SELECTED-ROWS:
    IF hBrowse:FETCH-SELECTED-ROW(ix) THEN
      cRowIdList = cRowIdList + hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('RowIdent1'):BUFFER-VALUE + ','.
  END.
  bOk = DYNAMIC-FUNCTION("ProcessSelectedRows",hBrowse,"vpiartbas_report.p",'').
END.
ELSE
  LEAVE.
  
IF NOT bOK THEN
  DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"Feil i sending av informasjon ",""). 

IF iReturn = 1 THEN RUN InvokeMethod (hBrowse,'OpenQuery'). ELSE DYNAMIC-FUNCTION("RefreshRowids",hBrowse,TRIM(cRowIdList,",")).
  
------------------------------------------------------------------------------*/
DEF VAR iReturn AS INT NO-UNDO.
DEF VAR cList   AS CHAR NO-UNDO.

hBrowse:SELECT-FOCUSED-ROW() NO-ERROR.
IF ERROR-STATUS:ERROR THEN
    RETURN.
/*
RUN JBoxBrowseSelectMsg.w ("Sende valgte poster til rapport?",
                              hBrowse:NUM-SELECTED-ROWS,
                              IF INT(DYNAMIC-FUNCTION("getAttribute",hBrowse,"Totalcount")) LT INT(DYNAMIC-FUNCTION("getAttribute",hBrowse,"rowstobatch")) THEN
                                INT(DYNAMIC-FUNCTION("getAttribute",hBrowse,"Totalcount"))
                              ELSE 99999,
                              OUTPUT iReturn).  /*1=Alle,2=Valgte*/

/* MESSAGE 'Programmerer må ferdigstille listen...' */
/*   VIEW-AS ALERT-BOX INFO BUTTONS OK.             */
/* LEAVE.                                           */
/* MESSAGE hParentBrowse:QUERY:PREPARE-STRING */
/*     VIEW-AS ALERT-BOX INFO BUTTONS OK.     */
IF iReturn = 1 THEN
  RUN printTellelinjeX.p ('Tellehode|' + STRING(hParentBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('rowident1'):BUFFER-VALUE) + '|Tellelinje|*',"").
ELSE IF iReturn = 2 THEN
DO:
  DO ix = 1 TO hBrowse:NUM-SELECTED-ROWS:
    IF hBrowse:FETCH-SELECTED-ROW(ix) THEN 
      cList = cList + ',' + STRING(hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('Rowident1'):BUFFER-VALUE).
  END.
  cList = TRIM(cList,',').
  RUN printTellelinjeX.p ('Tellehode|' + STRING(hParentBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('rowident1'):BUFFER-VALUE) +  '|Tellelinje|' + cList,"").    
END.
ELSE 
  LEAVE.

*/
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE RefreshBrowser C-Win 
PROCEDURE RefreshBrowser :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:  DUMMY PROCEDURE for å tilfredsstille gammelt program      
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ipRowid AS ROWID NO-UNDO.
  DEF VAR iArtNr AS INT NO-UNDO.

  DEF VAR bOk AS LOG NO-UNDO.

  
  DO WITH FRAME Default-Frame:
      /*
      FIND TelleLinje NO-LOCK WHERE
          ROWID(TelleLinje) = to-rowid(STRING(ipRowid)) NO-ERROR.
      */
      /*
      iArtNr = int(DYNAMIC-FUNCTION("getFieldValues","Tellelinje","WHERE rowid(tellelinje) = to-rowid(" + quoter(STRING(ipRowid)) + ')',"ArtikkelNr")).
      DYNAMIC-FUNCTION("setAttribute",hBrowse,"QueryFilter",' AND ArtikkelNr = ' + STRING(iArtNr)).
      */
/*       DYNAMIC-FUNCTION("setAttribute",hBrowse,"QueryFilter",' AND ArtikkelNr = ' + STRING(TelleLinje.ArtikkelNr)). */

      bFlag = TRUE. /*Check openquery*/

      RUN InvokeMethod(hBrowse,"OpenQuery").

      IF hBuffer:AVAIL THEN
          DYNAMIC-FUNCTION("RefreshRowids",hBrowse,hBuffer:BUFFER-FIELD("RowIdent1"):BUFFER-VALUE).
  END.

  RETURN "Ok".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE RowDisplayBrowse C-Win 
PROCEDURE RowDisplayBrowse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  hbclinjeSum:BGCOLOR =  IF hbflinjeSum:BUFFER-VALUE = 100.00 
                           THEN ?
                         ELSE IF hbflinjeSum:BUFFER-VALUE = 0
                           THEN ?
                         ELSE iRed.
  hbcProsent:BGCOLOR    = iLtYellow.
  hbcDbProsent:BGCOLOR    = iLtYellow.
  hbc2Prosent:BGCOLOR   = iLtYellow.
  hbc2DbProsent:BGCOLOR = iLtYellow.

IF DYNAMIC-FUNCTION("getCurrentObject") = hBrowse-2 THEN 
DO: 

    IF iAar <> YEAR(hbftmpdMDag:BUFFER-VALUE) OR cHelligDager = '' THEN
    DO:
        iAar = YEAR(hbftmpdMDag:BUFFER-VALUE).
        RUN bibl_Helligdager.p (iAar, OUTPUT cHelligDager).
    END.

    hbctmpcUDag:BGCOLOR =  IF TRIM(hbftmpcUDag:BUFFER-VALUE) <> 'SØN' 
                             THEN ?
                           ELSE iLtGreen.

    IF CAN-DO(cHelligdager,TRIM(hbftmpdMDag:BUFFER-VALUE)) 
                             THEN hbctmpcUDag:BGCOLOR = iLtRed.
END.

IF hBuffer:AVAIL THEN
DO:
    IF hBuffer:BUFFER-FIELD("DbSnitt"):BUFFER-VALUE <> hBuffer:BUFFER-FIELD("DbProsent"):BUFFER-VALUE THEN
        ASSIGN 
            hbcDbSnitt:BGCOLOR = iRed
            .
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE RowsToBatchRecord C-Win 
PROCEDURE RowsToBatchRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR cRowsToBatch AS CHAR NO-UNDO.

RUN JBoxDSelectRowsToBatch.w (DYNAMIC-FUNCTION("getAttribute",hBrowse,"rowsToBatch"), OUTPUT cRowsToBatch).

IF cRowsToBatch NE "" THEN DO:
  DYNAMIC-FUNCTION("setAttribute",hBrowse,"rowsToBatch",cRowsToBatch).
  DYNAMIC-FUNCTION("setCustomWinSettings",THIS-PROCEDURE:CURRENT-WINDOW,"<StartRowsToBatch>|brwLinje|" + cRowsToBatch + "|<EndRowsToBatch>").
  RUN INVOKEMETHOD(hBrowse,'OpenQuery').
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION AdjustBrowseColumns C-Win 
FUNCTION AdjustBrowseColumns RETURNS LOGICAL
  ( INPUT ihBrowse     AS HANDLE,
    INPUT icBrowseName AS CHAR):
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  
  IF icBrowseName = 'RectBrowse' THEN
  DO ix = 1 TO ihBrowse:NUM-COLUMNS:
    CASE ihBrowse:GET-BROWSE-COLUMN(ix):NAME:
      WHEN 'Prosent' THEN 
        ASSIGN
          hbcProsent = ihBrowse:GET-BROWSE-COLUMN(ix)
          hbfProsent = ihBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('Prosent')                                                                            
        .
      
      WHEN 'DbProsent' THEN 
        ASSIGN
          hbcDbProsent = ihBrowse:GET-BROWSE-COLUMN(ix)
          hbfDbProsent = ihBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('DbProsent')                                                                            
        .

      WHEN 'linjeSum' THEN 
        ASSIGN
          hbclinjeSum = ihBrowse:GET-BROWSE-COLUMN(ix)
          hbflinjeSum = ihBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('linjeSum')
        .
      WHEN 'DbSnitt' THEN 
        ASSIGN
          hbcDbSnitt = ihBrowse:GET-BROWSE-COLUMN(ix)
          hbfDbSnitt = ihBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('DbSnitt')
        .
    END CASE.
  END.

  ELSE IF icBrowseName = 'RectBrowse-2' THEN
  DO ix = 1 TO ihBrowse:NUM-COLUMNS:
    CASE ihBrowse:GET-BROWSE-COLUMN(ix):NAME:
      WHEN 'Prosent' THEN 
        ASSIGN
          hbc2Prosent = ihBrowse:GET-BROWSE-COLUMN(ix)
          hbf2Prosent = ihBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('Prosent')                                                                            
        .

      WHEN 'DbProsent' THEN 
        ASSIGN
          hbc2DbProsent = ihBrowse:GET-BROWSE-COLUMN(ix)
          hbf2DbProsent = ihBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('DbProsent')                                                                            
        .

      WHEN 'tmpcUDag' THEN 
        ASSIGN
          hbctmpcUDag = ihBrowse:GET-BROWSE-COLUMN(ix)
          hbftmpcUDag = ihBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('tmpcUDag')
        .

      WHEN 'tmpdMDag' THEN 
        ASSIGN
          hbctmpdMDag = ihBrowse:GET-BROWSE-COLUMN(ix)
          hbftmpdMDag = ihBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('tmpdMDag')                                                                            
        .
    END CASE.
  END.
  
  RETURN TRUE.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getFrameHandle C-Win 
FUNCTION getFrameHandle RETURNS HANDLE
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

RETURN FRAME {&FRAME-NAME}:HANDLE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getParentBrowse C-Win 
FUNCTION getParentBrowse RETURNS HANDLE
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  RETURN hBrowse.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION InitializeResize C-Win 
FUNCTION InitializeResize RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  /*DYNAMIC-FUNCTION("setNoResizeX", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE, "searchField").*/
  DYNAMIC-FUNCTION("setAddMoveY", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE, "ProsentTotal,DbSnittProsent,ProsentTotal-2,DbSnittProsent2").
  DYNAMIC-FUNCTION("setNoResizeX", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE, "rectBrowse").


END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setParent C-Win 
FUNCTION setParent RETURNS LOGICAL
  ( INPUT ihParent AS HANDLE ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
hParent = ihParent.

RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setQuery C-Win 
FUNCTION setQuery RETURNS LOGICAL
  ( INPUT ihQuery AS HANDLE ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
hParentBrowse = ihQuery.

RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION ViewQueryStat C-Win 
FUNCTION ViewQueryStat RETURNS LOGICAL
  ( INPUT ihQueryObject AS HANDLE ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
IF ihQueryObject = hBrowse THEN DO WITH FRAME {&FRAME-NAME}:
  ProsentTotal:SCREEN-VALUE = 
     DYNAMIC-FUNCTION("getAttribute",
     ihQueryObject,"statValueProsent").

  DbSnittProsent:SCREEN-VALUE = 
     DYNAMIC-FUNCTION("getAttribute",
     ihQueryObject,"statValueDbProsent").
END.
ELSE DO:
    ProsentTotal-2:SCREEN-VALUE = 
       DYNAMIC-FUNCTION("getAttribute",
       ihQueryObject,"statValueProsent").
END.
ProsentTotal:BGCOLOR IN FRAME {&FRAME-NAME} =  IF 100 <> INT(ProsentTotal:SCREEN-VALUE IN FRAME {&FRAME-NAME})
                                                 THEN iRed ELSE ?. 
ProsentTotal-2:BGCOLOR IN FRAME {&FRAME-NAME} =  IF 100 <> INT(ProsentTotal-2:SCREEN-VALUE IN FRAME {&FRAME-NAME})
                                                 THEN iRed ELSE ?. 


RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

