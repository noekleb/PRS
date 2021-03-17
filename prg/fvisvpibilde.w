&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS fFrameWin 
/*------------------------------------------------------------------------

  File: 

  Description: from cntnrfrm.w - ADM2 SmartFrame Template

  Input Parameters:
      <none>

  Output Parameters:
      <none>

 
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
DEFINE VARIABLE hDataSource AS HANDLE     NO-UNDO.
DEFINE VARIABLE cKatalog AS CHARACTER  NO-UNDO.
DEFINE STREAM Stream1.

{syspara.i 10 1 2 cKatalog}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartFrame
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER FRAME

&Scoped-define ADM-SUPPORTED-LINKS Data-Target,Data-Source,Page-Target,Update-Source,Update-Target

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME fMain

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getFilename fFrameWin 
FUNCTION getFilename RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of handles for OCX Containers                            */
DEFINE VARIABLE IMAGE-Sko AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chIMAGE-Sko AS COMPONENT-HANDLE NO-UNDO.

/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_dvpibildedata AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dvpibilderegister AS HANDLE NO-UNDO.

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 28.2 BY 4.81.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartFrame
   Allow: Basic,Browse,DB-Fields,Query,Smart
   Container Links: Data-Target,Data-Source,Page-Target,Update-Source,Update-Target
   Other Settings: PERSISTENT-ONLY COMPILE
 */

/* This procedure should always be RUN PERSISTENT.  Report the error,  */
/* then cleanup and return.                                            */
IF NOT THIS-PROCEDURE:PERSISTENT THEN DO:
  MESSAGE "{&FILE-NAME} should only be RUN PERSISTENT.":U
          VIEW-AS ALERT-BOX ERROR BUTTONS OK.
  RETURN.
END.

&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW fFrameWin ASSIGN
         HEIGHT             = 4.81
         WIDTH              = 28.2.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB fFrameWin 
/* ************************* Included-Libraries *********************** */

{src/adm2/containr.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW fFrameWin
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME fMain
   NOT-VISIBLE FRAME-NAME                                               */
ASSIGN 
       FRAME fMain:HIDDEN           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME fMain
/* Query rebuild information for FRAME fMain
     _Options          = ""
     _Query            is NOT OPENED
*/  /* FRAME fMain */
&ANALYZE-RESUME

 


/* **********************  Create OCX Containers  ********************** */

&ANALYZE-SUSPEND _CREATE-DYNAMIC

&IF "{&OPSYS}" = "WIN32":U AND "{&WINDOW-SYSTEM}" NE "TTY":U &THEN

CREATE CONTROL-FRAME IMAGE-Sko ASSIGN
       FRAME           = FRAME fMain:HANDLE
       ROW             = 1
       COLUMN          = 1
       HEIGHT          = 4.81
       WIDTH           = 27
       HIDDEN          = no
       SENSITIVE       = yes.

PROCEDURE adm-create-controls:
      IMAGE-Sko:NAME = "IMAGE-Sko":U .
/* IMAGE-Sko OCXINFO:CREATE-CONTROL from: {9A93B740-C96B-11D0-8883-444553540000} type: Picbuf */

END PROCEDURE.

&ENDIF

&ANALYZE-RESUME /* End of _CREATE-DYNAMIC */


/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME IMAGE-Sko
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL IMAGE-Sko fFrameWin OCX.DblClick
PROCEDURE IMAGE-Sko.Picbuf.DblClick .
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  None required for OCX.
  Notes:       
------------------------------------------------------------------------------*/

  if available vpiArtBas then
    do:
      find vpibilderegister WHERE
          vpibilderegister.EkstVPILevNr = VPIArtBas.EkstVPILevNr AND
           vpibilderegister.VareNr = VPIArtBas.VareNr NO-LOCK NO-ERROR.
      if available vpibilderegister then
        run d-visbil.w (input recid(vpibilderegister)).
    end.
  return no-apply.    


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK fFrameWin 


/* ***************************  Main Block  *************************** */

&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN
   /* Now enable the interface  if in test mode - otherwise this happens when
      the object is explicitly initialized from its container. */
   RUN initializeObject.
&ENDIF

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects fFrameWin  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/
  DEFINE VARIABLE currentPage  AS INTEGER NO-UNDO.

  ASSIGN currentPage = getCurrentPage().

  CASE currentPage: 

    WHEN 0 THEN DO:
       RUN constructObject (
             INPUT  'sdo/dvpibilderegister.wDB-AWARE':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'AppServiceASInfoASUsePrompt?CacheDuration0CheckCurrentChangedyesDestroyStatelessnoDisconnectAppServernoServerOperatingModeNONEShareDatanoUpdateFromSourcenoForeignFieldsObjectNamedvpibilderegisterOpenOnInityesPromptColumns(NONE)PromptOnDeleteyesRowsToBatch200RebuildOnReposnoToggleDataTargetsyes':U ,
             OUTPUT h_dvpibilderegister ).
       RUN repositionObject IN h_dvpibilderegister ( 1.48 , 17.00 ) NO-ERROR.
       /* Size in AB:  ( 1.86 , 9.20 ) */

       RUN constructObject (
             INPUT  'sdo/dvpibildedata.wDB-AWARE':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'AppServiceASInfoASUsePrompt?CacheDuration0CheckCurrentChangedyesDestroyStatelessnoDisconnectAppServernoServerOperatingModeNONEShareDatanoUpdateFromSourcenoForeignFieldsObjectNamedvpibildedataOpenOnInityesPromptColumns(NONE)PromptOnDeleteyesRowsToBatch200RebuildOnReposnoToggleDataTargetsyes':U ,
             OUTPUT h_dvpibildedata ).
       RUN repositionObject IN h_dvpibildedata ( 3.38 , 7.00 ) NO-ERROR.
       /* Size in AB:  ( 1.86 , 9.00 ) */

       /* Adjust the tab order of the smart objects. */
    END. /* Page 0 */

  END CASE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE BildeTilDisk fFrameWin 
PROCEDURE BildeTilDisk :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT-OUTPUT PARAMETER cBildeFil AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE               rawData   AS RAW        NO-UNDO.
    DEFINE VARIABLE               cRowIdent1 AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE               cRowIdent2 AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE               cTellerStr AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE iBildeNr  AS INTEGER    NO-UNDO.
    ASSIGN iBildeNr = DYNAMIC-FUNCTION("getBildNr" IN h_dvpibilderegister).
    IF iBildeNr = ? OR iBildeNr = 0 THEN
        ASSIGN cBildeFil = "".
    ELSE DO:
        ASSIGN cTellerStr = " AND Teller " +
        IF ENTRY(NUM-ENTRIES(cBildeFil,"/"),cBildeFil,"/") BEGINS "mini" THEN
           ">= 200" ELSE "< 200".
        DYNAMIC-FUNCTION('setQueryWhere':U IN h_dvpibildedata,
     INPUT "BildNr = " + STRING(iBildeNr) + cTellerStr /* CHARACTER */).
        DYNAMIC-FUNCTION('openQuery':U IN h_dvpibildedata).                     
     RUN fetchFirst IN h_dvpibildedata.
     LENGTH(rawData) = 30000.
     ASSIGN rawData    = DYNAMIC-FUNCTION("getRawData" IN h_dvpibildedata)
            cRowIdent1 = DYNAMIC-FUNCTION('getRowIdent':U IN h_dvpibildedata).
     IF rawData = ? THEN
         RETURN.
     ELSE DO:
        OUTPUT STREAM Stream1 TO VALUE(cBildeFil) NO-MAP NO-CONVERT.
        PUT STREAM Stream1 CONTROL rawData.
        HENT:
        REPEAT:
            RUN fetchNext IN h_dvpibildedata.
            ASSIGN cRowIdent2 = DYNAMIC-FUNCTION('getRowIdent':U IN h_dvpibildedata).
            IF cRowIdent1 = cRowIdent2 THEN
                LEAVE.
            ASSIGN cRowIdent1 = cRowIdent2
                   rawData = DYNAMIC-FUNCTION("getRawData" IN h_dvpibildedata).
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE control_load fFrameWin  _CONTROL-LOAD
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

OCXFile = SEARCH( "fvisvpibilde.wrx":U ).
IF OCXFile = ? THEN
  OCXFile = SEARCH(SUBSTRING(THIS-PROCEDURE:FILE-NAME, 1,
                     R-INDEX(THIS-PROCEDURE:FILE-NAME, ".":U), "CHARACTER":U) + "wrx":U).

IF OCXFile <> ? THEN
DO:
  ASSIGN
    chIMAGE-Sko = IMAGE-Sko:COM-HANDLE
    UIB_S = chIMAGE-Sko:LoadControls( OCXFile, "IMAGE-Sko":U)
  .
  RUN initialize-controls IN THIS-PROCEDURE NO-ERROR.
END.
ELSE MESSAGE "fvisvpibilde.wrx":U SKIP(1)
             "The binary control file could not be found. The controls cannot be loaded."
             VIEW-AS ALERT-BOX TITLE "Controls Not Loaded".

&ENDIF

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE destroyObject fFrameWin 
PROCEDURE destroyObject :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  RUN SUPER.

  /* Code placed here will execute AFTER standard behavior.    */
/*   IF VALID-HANDLE(IMAGE-Sko) THEN       */
/*       DELETE OBJECT IMAGE-Sko NO-ERROR. */
  IF VALID-HANDLE(chIMAGE-Sko) THEN
      RELEASE OBJECT chIMAGE-Sko NO-ERROR.
  IF VALID-HANDLE(IMAGE-Sko) THEN
      DELETE OBJECT IMAGE-Sko.
  ASSIGN chIMAGE-Sko = ?.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI fFrameWin  _DEFAULT-DISABLE
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
  HIDE FRAME fMain.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI fFrameWin  _DEFAULT-ENABLE
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
  {&OPEN-BROWSERS-IN-QUERY-fMain}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initialize-controls fFrameWin 
PROCEDURE initialize-controls :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    ASSIGN chIMAGE-Sko = chIMAGE-Sko:Picbuf
           chIMAGE-Sko:BackColor = 13554646
           chIMAGE-Sko:BorderStyle = 1.
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initializeObject fFrameWin 
PROCEDURE initializeObject :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  RUN SUPER.

  ASSIGN hDataSource = DYNAMIC-FUNCTION('getDataSource':U).
  RUN addLink (hDataSource,'DATA',h_dvpibilderegister).

  /* Code placed here will execute AFTER standard behavior.    */
  IF VALID-HANDLE(h_dvpibilderegister) THEN DO:
      DYNAMIC-FUNCTION('setForeignFields':U IN h_dvpibilderegister,
        INPUT "RowObject.BildNr,BildNr" /* CHARACTER */).
/*     ASSIGN cKatalog = DYNAMIC-FUNCTION("getKatalog" IN h_dvpibilderegister). */
    SUBSCRIBE TO "VisBilde" IN h_dvpibilderegister.
    SUBSCRIBE TO "NyttBilde" ANYWHERE.

    RUN dataAvailable IN h_dvpibilderegister ("SAME").

/*     RUN refreshRow IN h_dvpibilderegister. */
/*       DYNAMIC-FUNCTION('openQuery':U IN h_dvpibilderegister). */
  END.
  SUBSCRIBE TO "NyttBilde" ANYWHERE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE NyttBilde fFrameWin 
PROCEDURE NyttBilde :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER cBildeFilNavn AS CHARACTER  NO-UNDO.

    RUN VisBilde (cKatalog + "\" + cBildeFilNavn).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE VisBilde fFrameWin 
PROCEDURE VisBilde :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT  PARAMETER cBildeFil AS CHARACTER  NO-UNDO.
   chIMAGE-Sko:clear(2).
   IF cBildeFil = "" THEN
       RETURN.
   IF search(cBildeFil) = ? THEN DO:
       RUN BildeTilDisk (INPUT-OUTPUT cBildeFil).
   END.
   IF search(cBildeFil) <> ? THEN DO:
       ASSIGN chIMAGE-Sko:FILENAME = cBildeFil
              chIMAGE-Sko:AutoScale = TRUE.
              chIMAGE-Sko:LOAD NO-ERROR.

   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getFilename fFrameWin 
FUNCTION getFilename RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  RETURN  chIMAGE-Sko:FILENAME. /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

