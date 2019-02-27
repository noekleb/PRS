&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.       */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */
&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
  DEFINE VARIABLE cBestillingsnummer      AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cERPNr     AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cTekst     AS CHARACTER NO-UNDO. 
  ASSIGN
    cBestillingsnummer = '10'
    .
&ELSE
  DEFINE INPUT        PARAMETER cBestillingsnummer      AS CHAR NO-UNDO.
  DEFINE INPUT        PARAMETER cERPNr     AS CHARACTER NO-UNDO.
  DEFINE INPUT-OUTPUT PARAMETER cTekst      AS CHARACTER NO-UNDO. 
&ENDIF

/* Local Variable Definitions ---                                       */

DEF VAR bOK           AS LOG    NO-UNDO.
DEF VAR ix            AS INT    NO-UNDO.
DEF VAR hVisBilde     AS HANDLE NO-UNDO.
DEF VAR hParent       AS HANDLE NO-UNDO.
DEF VAR hLargePicture AS HANDLE NO-UNDO.
DEFINE VARIABLE hBuffer AS HANDLE NO-UNDO.
DEF VAR cBildeKatalog  AS CHAR   NO-UNDO.
DEF VAR iBildeNr       AS INT    NO-UNDO.

DEF VAR hArtBilde              AS HANDLE NO-UNDO.
DEF VAR hArtBildeFrame         AS HANDLE NO-UNDO.

DEFINE VARIABLE ocReturn AS CHARACTER NO-UNDO.
DEF VAR hToolbar        AS HANDLE NO-UNDO.
DEF VAR hBrowse         AS HANDLE NO-UNDO.
DEF VAR hFieldMap       AS HANDLE NO-UNDO.
DEF VAR hSearchField    AS HANDLE NO-UNDO.

DEFINE VARIABLE iCL       AS INTEGER NO-UNDO.
DEFINE VARIABLE iProfilNr AS INTEGER NO-UNDO.

DEFINE VARIABLE hbcKolonne AS HANDLE NO-UNDO.
DEFINE VARIABLE hbfKolonne AS HANDLE NO-UNDO.

DEFINE VARIABLE hbcVarekost   AS HANDLE NO-UNDO.
DEFINE VARIABLE hbfVarekost   AS HANDLE NO-UNDO.
DEFINE VARIABLE hbcPris       AS HANDLE NO-UNDO.
DEFINE VARIABLE hbfPris       AS HANDLE NO-UNDO.
DEFINE VARIABLE hbcSanert     AS HANDLE NO-UNDO.
DEFINE VARIABLE hbfSanert     AS HANDLE NO-UNDO.
DEFINE VARIABLE hbcArtikkelNr AS HANDLE NO-UNDO.
DEFINE VARIABLE hbfArtikkelNr AS HANDLE NO-UNDO.
DEFINE VARIABLE hbcBilde      AS HANDLE NO-UNDO.
DEFINE VARIABLE hbfBilde      AS HANDLE NO-UNDO.

{buildfunction.i}

DEFINE STREAM Stream1.

DEF TEMP-TABLE ttBildeData
    FIELD BildNr    AS INT
    FIELD Teller    AS INT
    FIELD RawData   AS RAW
    FIELD RowIdent  AS CHAR
    .
DEF VAR hBufBildeData AS HANDLE.
hBufBildeData = BUFFER ttBildeData:HANDLE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS rectBrowse ArtikkelBilde ~
fltBestillingsnummer fltERPNr btnBlank Btn_OK Btn_Cancel Btn_Help 
&Scoped-Define DISPLAYED-OBJECTS fltBestillingsnummer fltERPNr 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD AdjustBrowseColumns Dialog-Frame 
FUNCTION AdjustBrowseColumns RETURNS LOGICAL
        (INPUT ihBrowse     AS HANDLE,
    INPUT icBrowseName AS CHAR) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getControlFrameHandle Dialog-Frame 
FUNCTION getControlFrameHandle RETURNS HANDLE
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getFrameHandle Dialog-Frame 
FUNCTION getFrameHandle RETURNS HANDLE
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of handles for OCX Containers                            */
DEFINE VARIABLE IMAGE-sko AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chIMAGE-sko AS COMPONENT-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnBlank 
     LABEL "Blank filter" 
     SIZE 13 BY 1.14.

DEFINE BUTTON Btn_Cancel AUTO-END-KEY 
     LABEL "Avbryt" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON Btn_Help 
     LABEL "&Help" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON Btn_OK AUTO-GO 
     LABEL "OK" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE VARIABLE fltBestillingsnummer AS CHARACTER FORMAT "X(30)" 
     LABEL "Bestillingsnr" 
     VIEW-AS FILL-IN 
     SIZE 25 BY 1.

DEFINE VARIABLE fltERPNr AS CHARACTER FORMAT "X(22)" 
     LABEL "ERPNr" 
     VIEW-AS FILL-IN 
     SIZE 25 BY 1.

DEFINE RECTANGLE ArtikkelBilde
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 27 BY 5.33.

DEFINE RECTANGLE rectBrowse
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 200 BY 23.14.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     fltBestillingsnummer AT ROW 3.38 COL 58.8 COLON-ALIGNED
     fltERPNr AT ROW 4.38 COL 58.8 COLON-ALIGNED
     btnBlank AT ROW 5.33 COL 161.4
     Btn_OK AT ROW 29.95 COL 156.8
     Btn_Cancel AT ROW 29.95 COL 172
     Btn_Help AT ROW 29.95 COL 187
     rectBrowse AT ROW 6.71 COL 2
     ArtikkelBilde AT ROW 1.14 COL 174.8
     SPACE(0.39) SKIP(24.62)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Søk i strekkoderegister"
         DEFAULT-BUTTON Btn_OK CANCEL-BUTTON Btn_Cancel.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Dialog-Box
   Allow: Basic,Browse,DB-Fields,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB Dialog-Frame 
/* ************************* Included-Libraries *********************** */

{incl/devmode.i}
{incl/custdevmode.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX Dialog-Frame
   NOT-VISIBLE FRAME-NAME                                               */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 


/* **********************  Create OCX Containers  ********************** */

&ANALYZE-SUSPEND _CREATE-DYNAMIC

&IF "{&OPSYS}" = "WIN32":U AND "{&WINDOW-SYSTEM}" NE "TTY":U &THEN

CREATE CONTROL-FRAME IMAGE-sko ASSIGN
       FRAME           = FRAME Dialog-Frame:HANDLE
       ROW             = 1.38
       COLUMN          = 175.6
       HEIGHT          = 4.91
       WIDTH           = 25.4
       HIDDEN          = NO
       SENSITIVE       = YES.
/* IMAGE-sko OCXINFO:CREATE-CONTROL from: {9A93B740-C96B-11D0-8883-444553540000} type: Picbuf */

&ENDIF

&ANALYZE-RESUME /* End of _CREATE-DYNAMIC */


/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Søk i strekkoderegister */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnBlank
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnBlank Dialog-Frame
ON CHOOSE OF btnBlank IN FRAME Dialog-Frame /* Blank filter */
DO:
        DO WITH FRAME {&Frame-name}:
            ASSIGN
                fltBestillingsnummer:SCREEN-VALUE = ''
                fltERPNr:SCREEN-VALUE             = ''
                .
            DYNAMIC-FUNCTION("setCurrentObject",hBrowse).
            RUN OpenQuery.
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Help
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Help Dialog-Frame
ON CHOOSE OF Btn_Help IN FRAME Dialog-Frame /* Help */
OR HELP OF FRAME {&FRAME-NAME}
DO: /* Call Help Function (or a simple message). */
  MESSAGE "Help for File: {&FILE-NAME}" VIEW-AS ALERT-BOX INFORMATION.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK Dialog-Frame
ON CHOOSE OF Btn_OK IN FRAME Dialog-Frame /* OK */
DO:
  IF VALID-HANDLE(hbfArtikkelNr) THEN 
  DO:
    FIND ArtBas NO-LOCK WHERE
      ArtBas.ArtikkelNr = DECIMAL(hbfArtikkelNr:BUFFER-VALUE) NO-ERROR.
    ASSIGN
      cTekst   = ',' + STRING(ROWID(ArtBas)) + CHR(1) + STRING(hbfArtikkelNr:BUFFER-VALUE)
      ocRETURN = 'OK'.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fltBestillingsnummer
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fltBestillingsnummer Dialog-Frame
ON TAB OF fltBestillingsnummer IN FRAME Dialog-Frame /* Bestillingsnr */
OR RETURN OF fltBestillingsnummer IN FRAME DIALOG-FRAME /*  */
DO:
  DYNAMIC-FUNCTION("setCurrentObject",hBrowse).
  RUN OpenQuery.
  APPLY 'ENTRY' TO hBrowse.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fltERPNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fltERPNr Dialog-Frame
ON TAB OF fltERPNr IN FRAME Dialog-Frame /* ERPNr */
OR RETURN OF fltERPNr IN FRAME DIALOG-FRAME /* Hovedgrp */
DO:
  DYNAMIC-FUNCTION("setCurrentObject",hBrowse).
  RUN OpenQuery.
  APPLY 'ENTRY' TO hBrowse.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME IMAGE-sko
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL IMAGE-sko Dialog-Frame OCX.DblClick
PROCEDURE IMAGE-sko.Picbuf.DblClick .
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  None required for OCX.
  Notes:       
------------------------------------------------------------------------------*/
IF iBildeNr NE 0 THEN DO:
  IF NOT VALID-HANDLE(hVisBilde) THEN
    RUN VisBilde.w PERSIST SET hVisBilde.
  
  RUN VisBilde IN hVisBilde (iBildeNr).
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dialog-Frame 


/* ***************************  Main Block  *************************** */

/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT EQ ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.


/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN enable_UI.
  /*
  &IF DEFINED(UIB_is_Running) NE 0 &THEN
    RUN InitializeObject.
  &ENDIF
  */
  /* Når det ikke kjøres fra meny, må dette gjøres her. */
  RUN InitializeObject.
  RUN MoveToTop.
  RUN OpenQuery.

  WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
IF VALID-HANDLE(hArtBilde) THEN APPLY "close" TO hArtBilde.
RUN disable_UI.
RETURN ocReturn.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE BildeTilDisk Dialog-Frame 
PROCEDURE BildeTilDisk :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT-OUTPUT PARAMETER cBildeFil AS CHARACTER  NO-UNDO.

DEFINE VARIABLE rawData    AS RAW        NO-UNDO.
DEFINE VARIABLE cRowIdent1 AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cRowIdent2 AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cTellerStr AS CHARACTER  NO-UNDO.

IF iBildeNr = ? OR iBildeNr = 0 THEN
  ASSIGN cBildeFil = "".
ELSE DO:
  ASSIGN cTellerStr = " AND Teller " +
    IF ENTRY(NUM-ENTRIES(cBildeFil,"\"),cBildeFil,"\") BEGINS "mini" THEN
       ">= 200" ELSE "< 200".

  EMPTY TEMP-TABLE ttBildeData.

  DYNAMIC-FUNCTION("getTempTable","jbserv_gettemptable.p","BildeData|WHERE Bildnr = " + STRING(iBildeNr) + cTellerStr,hBufBildeData).

  LENGTH(rawData) = 30000.

  FIND FIRST ttBildeData NO-ERROR.
  IF NOT AVAIL ttBildeData THEN DO:
    cBildeFil = "".
    RETURN.
  END.

  ASSIGN rawData    = ttBildeData.RawData
         cRowIdent1 = ttBildeData.RowIdent.
  IF rawData = ? THEN DO:
    cBildeFil = "".
    RETURN.
  END.
  ELSE DO:
    OUTPUT STREAM Stream1 TO VALUE(cBildeFil) NO-MAP NO-CONVERT.
    PUT STREAM Stream1 CONTROL rawData.
    HENT:
    REPEAT:
      FIND NEXT ttBildeData.
      IF cRowIdent1 = ttBildeData.RowIdent THEN 
        LEAVE HENT.
      ASSIGN cRowIdent1 = ttBildeData.RowIdent
             rawData    = ttBildeData.RawData.
      IF rawData = ? THEN
        LEAVE HENT.
      PUT STREAM Stream1 CONTROL rawData.
    END.
    OUTPUT STREAM Stream1 CLOSE.
  END.
END.

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

OCXFile = SEARCH( "dstrekkode_sok.wrx":U ).
IF OCXFile = ? THEN
  OCXFile = SEARCH(SUBSTRING(THIS-PROCEDURE:FILE-NAME, 1,
                     R-INDEX(THIS-PROCEDURE:FILE-NAME, ".":U), "CHARACTER":U) + "wrx":U).

IF OCXFile <> ? THEN
DO:
  ASSIGN
    chIMAGE-sko = IMAGE-sko:COM-HANDLE
    UIB_S = chIMAGE-sko:LoadControls( OCXFile, "IMAGE-sko":U)
    IMAGE-sko:NAME = "IMAGE-sko":U
  .
  RUN initialize-controls IN THIS-PROCEDURE NO-ERROR.
END.
ELSE MESSAGE "dstrekkode_sok.wrx":U SKIP(1)
             "The binary control file could not be found. The controls cannot be loaded."
             VIEW-AS ALERT-BOX TITLE "Controls Not Loaded".

&ENDIF

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DefaultActionBrowse Dialog-Frame 
PROCEDURE DefaultActionBrowse :
/*------------------------------------------------------------------------------
                        Purpose:                                                                                                                                          
                        Notes:                                                                                                                                            
        ------------------------------------------------------------------------------*/
  APPLY 'CHOOSE' TO Btn_OK IN FRAME Dialog-Frame.      

  /*RUN SUPER. */

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DisplayRecord Dialog-Frame 
PROCEDURE DisplayRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  RUN SUPER.
  /*KundeNr:SENSITIVE IN FRAME {&FRAME-NAME} =  DYNAMIC-FUNCTION('getToolbarState',hToolbar) = 'New'.*/
  IF hBuffer:AVAILABLE THEN 
  DO:
    IF INTEGER(hbfBilde:BUFFER-VALUE) = 0 THEN chIMAGE-Sko:Picbuf:CLEAR(2). 
    RUN VisMiniBilde (INTEGER(hbfBilde:BUFFER-VALUE)). 
  END.
  
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
  DISPLAY fltBestillingsnummer fltERPNr 
      WITH FRAME Dialog-Frame.
  ENABLE rectBrowse ArtikkelBilde fltBestillingsnummer fltERPNr btnBlank Btn_OK 
         Btn_Cancel Btn_Help 
      WITH FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitializeObject Dialog-Frame 
PROCEDURE InitializeObject :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:      InitializeObject is called automatically when using JBoxDynMenu.w 
------------------------------------------------------------------------------*/
DO WITH FRAME {&FRAME-NAME}:
  cBildeKatalog   = DYNAMIC-FUNCTION("getFieldValues","SysPara",
                    "WHERE SysHId = 10 and SysGr = 1 and ParaNr = 2","Parameter1").
                    
  ASSIGN ocReturn = 'AVBRYT'
         .

  iCL = INTEGER((DYNAMIC-FUNCTION("getFieldValues","SysPara",
                            "WHERE SysHId = 5 and SysGr = 1 and ParaNr = 1","Parameter1"))).
  IF iCL > 0 THEN
    iProfilNr = INTEGER((DYNAMIC-FUNCTION("getFieldValues","Butiker",
                            "WHERE Butik = '" + STRING(iCL) + "'","ProfilNr"))).
  IF iProfilNr = ? OR iProfilNr = 0 THEN iProfilNr = 1.                                                          

  hBrowse = DYNAMIC-FUNCTION("NewBrowse",          /* Create a browse object */
                    rectBrowse:HANDLE,              /* Rectangle to define coordinates for browse */
                    200,                            /* Rows to batch */
                    "",                             /* Browse properties, ie MULTIPLE,NUM-LOCKED-COLUMNS, etc */
                    "Strekkode"
                    /*  1 */ + ";ArtikkelNr|Art.nr"                    
                    /*  1 */ + ";Kode|Strekkode@5"                    
                    /*  1 */ + ";Bestillingsnummer|Best.nr@6"                    
                    /*  1 */ + ";ERPNr|ERPNr@7"                    
                    /*  1 */ + ",ArtBas;!ArtikkelNr"                    
                    /*  2 */ +  ";+cVgLopNr|CHAR|x(12)|artbas_sok_vglopnr|Vg/løpenr" 
                    /*  3 */ +  ";LevKod|Lev.art.nr@1"
                    /*  4 */ +  ";Beskr|Varetekst|x(40)@2"
                    /*  5 */ +  ";LevFargKod|Lev.fargekode@3"
                    /*  6 */ +  ";+fVarekost|DECIMAL|->>><>>><>>9.99|artpris_varekost|Varekost@8"                    
                    /*  7 */ +  ";+fPris|DECIMAL|->>><>>><>>9.99|artpris_pris|Pris@9"                    
                    /*  8 */ +  ";Sasong|Ses|>>>9"
                    /*  9 */ +  ";Utgatt|U|*/ "
                    /* 10 */ +  ";iKasse|IK|*/ "
                             +  ";Lager|Lager|*/ "
                             +  ";OPris|Åpenpris|*/ "
                             +  ";Pakke|Pakke|*/ "
                             +  ";Pant|Pant|*/ "
                             +  ";AnonseArtikkel|Annonse|*/ "
                             +  ";KjedeVare|Kjede|*/ "
                             +  ";Gjennomfaktureres|Gj.fakt|*/ "
                             +  ";WebButikkArtikkel|Nettbutikk|*/ "
                             +  ";BildNr"
                    /* 11 */ +  ";+cHg|CHAR|x(15)|artbas_sok_avdeling|Avdeling" 
                    /* 12 */ +  ";+cTilb|CHAR|x(3)|artpris_tilbud|Tilb"
                    /* 13 */ +  ";+cSanert|CHAR|x(8)|artbas_sanertdato|Sanert" 
                    /* 14 */ +  " ;!Vg|Varegr"                    
                    /* 15 */ + ",Sasong;SasBeskr|Sesong"
                    /* 16 */ + ",HuvGr;HgBeskr|Hovedgrp"
                    /* 17 */ + ",VarGr;VgBeskr|Varegrp"
                    /* 18 */ + ",LevBas;LevNamn"
                    /* 18 */ + ",StrKonv;Storl|Str.@4"
                   ,"WHERE false"
                    + ",FIRST ArtBas NO-LOCK OF Strekkode OUTER-JOIN"
                    + ",FIRST Sasong NO-LOCK OF ArtBas OUTER-JOIN"
                    + ",FIRST HuvGr NO-LOCK OF ArtBas OUTER-JOIN"
                    + ",FIRST VarGr NO-LOCK OF ArtBas OUTER-JOIN"
                    + ",FIRST LevBas NO-LOCK OF ArtBas OUTER-JOIN"
                    + ",FIRST StrKonv NO-LOCK OF Strekkode OUTER-JOIN"
                   ,"").             
  hBuffer = hBrowse:QUERY:GET-BUFFER-HANDLE(1).
  DYNAMIC-FUNCTION("setAttribute",hBrowse,"calcfieldproc","artbas_sok_browsekalk.p"). 
  DYNAMIC-FUNCTION("setAttribute",hBrowse,"sortmap","ArtikkelNr;StrKode").  
  DYNAMIC-FUNCTION("setAttribute",hBrowse,"basequery","WHERE Kode > '' AND ArtikkelNr >= 0"). 
  
  ASSIGN
    fltBestillingsnummer:SCREEN-VALUE  = cBestillingsnummer
    fltERPNr:SCREEN-VALUE = cERPNr
    .

  APPLY "value-changed" TO hBrowse.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE MoveToTop Dialog-Frame 
PROCEDURE MoveToTop :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
{&WINDOW-NAME}:WINDOW-STATE = 3.
{&WINDOW-NAME}:MOVE-TO-TOP().
APPLY "entry" TO hBrowse.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OpenQuery Dialog-Frame 
PROCEDURE OpenQuery :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEF VAR cWhere   AS CHAR NO-UNDO.
  
    DO WITH FRAME {&FRAME-NAME}:

        DYNAMIC-FUNCTION("setAttribute",hBrowse,"QueryFilter",''). 
    
        cWhere = buildFilter(cWhere,fltBestillingsnummer:HANDLE,'Bestillingsnummer',IF fltBestillingsnummer:SCREEN-VALUE BEGINS '*' THEN 'MATCHES' ELSE 'BEGINS').
        cWhere = cWhere + buildFilter(cWhere,fltERPNr:HANDLE,'ERPNr',IF fltERPNr:SCREEN-VALUE BEGINS '*' THEN 'MATCHES' ELSE 'BEGINS').

        DYNAMIC-FUNCTION("setAttribute",hBrowse,"QueryFilter",cWhere). 

        ASSIGN
            fltBestillingsnummer:MODIFIED = FALSE  
            fltERPNr:MODIFIED             = FALSE 
            .

      RUN SUPER.
    
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE RowDisplayBrowse Dialog-Frame 
PROCEDURE RowDisplayBrowse :
/*------------------------------------------------------------------------------
                        Purpose:                                                                                                                                          
                        Notes:                                                                                                                                            
        ------------------------------------------------------------------------------*/

  DEF VAR iGreen  AS INT INIT 10 NO-UNDO.
  DEF VAR iRed    AS INT INIT 12 NO-UNDO.
  DEF VAR iYellow AS INT INIT 14 NO-UNDO.
  DEF VAR iBlue   AS INT INIT 11 NO-UNDO.

  IF VALID-HANDLE(hbcKolonne) THEN DO:
    ASSIGN
      hbcKolonne:BGCOLOR    = IF hbfKolonne:BUFFER-VALUE = '*' THEN iRed ELSE ?
      hbcVarekost:BGCOLOR   = IF hbfKolonne:BUFFER-VALUE = '*' THEN iRed ELSE ?
      hbcPris:BGCOLOR       = IF hbfKolonne:BUFFER-VALUE = '*' THEN iRed ELSE ?
      hbcArtikkelNr:BGCOLOR = IF hbfSanert:BUFFER-VALUE <> ''  THEN iRed ELSE ?
      hbcSanert:BGCOLOR     = IF hbfSanert:BUFFER-VALUE <> ''  THEN iRed ELSE ?
      .
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE VisMiniBilde Dialog-Frame 
PROCEDURE VisMiniBilde :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER iiBildeNr AS INT NO-UNDO.

DEF VAR cBildeFil AS CHAR NO-UNDO.

iBildeNr = iiBildeNr.

/*chIMAGE-Sko:Picbuf:CLEAR(2).*/

IF iBildeNr = 0 THEN RETURN.

cBildeFil = DYNAMIC-FUNCTION("getFieldValues","Bilderegister","where BildNr = " + STRING(iBildeNr),"Filnavn").

IF cBildeFil = "" OR cBildeFil = ? THEN
  RETURN.
ELSE 
  cBildeFil = TRIM(cBildeKatalog,"\") + "\mini" + cBildeFil.

IF SEARCH(cBildeFil) = ? THEN
  RUN BildeTilDisk (INPUT-OUTPUT cBildeFil).

IF SEARCH(cBildeFil) <> ? AND cBildeFil NE "" THEN DO:
  ASSIGN chIMAGE-Sko:Picbuf:FILENAME = cBildeFil
         chIMAGE-Sko:Picbuf:AutoScale = TRUE.
         chIMAGE-Sko:Picbuf:LOAD.
   IF ERROR-STATUS:GET-MESSAGE(1) BEGINS "Error" THEN
     DYNAMIC-FUNCTION('setWebDoc','open',cBildeFil).
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION AdjustBrowseColumns Dialog-Frame 
FUNCTION AdjustBrowseColumns RETURNS LOGICAL
        (INPUT ihBrowse     AS HANDLE,
    INPUT icBrowseName AS CHAR):

        /*------------------------------------------------------------------------------
                        Purpose:                                                                                                                                          
                        Notes:                                                                                                                                            
        ------------------------------------------------------------------------------*/
  DO ix = 1 TO ihBrowse:NUM-COLUMNS:
    CASE ihBrowse:GET-BROWSE-COLUMN(ix):NAME:
      WHEN 'cTilb' THEN /*Navn på kolonnen*/
        ASSIGN
          hbcKolonne = ihBrowse:GET-BROWSE-COLUMN(ix)
          hbfKolonne = ihBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('cTilb')
        .
      WHEN 'fVarekost' THEN /*Navn på kolonnen*/
        ASSIGN
          hbcVarekost = ihBrowse:GET-BROWSE-COLUMN(ix)
          hbfVarekost = ihBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('fVarekost')
        .
      WHEN 'fPris' THEN /*Navn på kolonnen*/
        ASSIGN
          hbcPris = ihBrowse:GET-BROWSE-COLUMN(ix)
          hbfPris = ihBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('fPris')
        .
      WHEN 'cSanert' THEN /*Navn på kolonnen*/
        ASSIGN
          hbcSanert = ihBrowse:GET-BROWSE-COLUMN(ix)
          hbfSanert = ihBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('cSanert')
        .
      WHEN 'ArtikkelNr' THEN /*Navn på kolonnen*/
        ASSIGN
          hbcArtikkelNr = ihBrowse:GET-BROWSE-COLUMN(ix)
          hbfArtikkelNr = ihBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('ArtikkelNr')
        .
      WHEN 'BildNr' THEN /*Navn på kolonnen*/
        ASSIGN
          hbcBilde = ihBrowse:GET-BROWSE-COLUMN(ix)
          hbfBilde = ihBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('BildNr')
        .
    END CASE.
  END.

  RETURN TRUE.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getControlFrameHandle Dialog-Frame 
FUNCTION getControlFrameHandle RETURNS HANDLE
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
RETURN image-sko:HANDLE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getFrameHandle Dialog-Frame 
FUNCTION getFrameHandle RETURNS HANDLE
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

/*RETURN FRAME Default-Frame:HANDLE.*/

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

