&ANALYZE-SUSPEND _VERSION-NUMBER AB_v9r12 GUI ADM2
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

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartFrame
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER FRAME

&Scoped-define ADM-SUPPORTED-LINKS Data-Target,Data-Source,Page-Target,Update-Source,Update-Target

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME fMain

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */


/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_bbonglinje AS HANDLE NO-UNDO.
DEFINE VARIABLE h_bbonglinjeoriginaldata AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dbonghode AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dbonglinje AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dbonglinje-2 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_folder AS HANDLE NO-UNDO.
DEFINE VARIABLE h_vbonghode AS HANDLE NO-UNDO.
DEFINE VARIABLE h_vbonghodeutskriftskopi AS HANDLE NO-UNDO.
DEFINE VARIABLE h_vbonghodlogg AS HANDLE NO-UNDO.

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 158.2 BY 21.48.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartFrame
   Allow: Basic,Browse,DB-Fields,Query,Smart
   Container Links: Data-Target,Data-Source,Page-Target,Update-Source,Update-Target
   Design Page: 1
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
         HEIGHT             = 21.48
         WIDTH              = 158.2.
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
   NOT-VISIBLE                                                          */
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
             INPUT  'sdo/dbonghode.wDB-AWARE':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'AppServiceASUsePromptASInfoForeignFieldsBongHode.ButikkNr,ButikkNr,BongHode.GruppeNr,GruppeNr,BongHode.KasseNr,KasseNr,BongHode.Dato,Dato,BongHode.BongNr,BongNrRowsToBatch200CheckCurrentChangedyesRebuildOnReposnoServerOperatingModeNONEDestroyStatelessnoDisconnectAppServernoObjectNamedbonghodeUpdateFromSourcenoToggleDataTargetsyesOpenOnInityesPromptOnDeleteyesPromptColumns(NONE)':U ,
             OUTPUT h_dbonghode ).
       RUN repositionObject IN h_dbonghode ( 1.00 , 121.00 ) NO-ERROR.
       /* Size in AB:  ( 2.14 , 16.00 ) */

       RUN constructObject (
             INPUT  'sdo/dbonglinje.wDB-AWARE':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'AppServiceASUsePromptASInfoForeignFieldsBongLinje.ButikkNr,ButikkNr,BongLinje.GruppeNr,GruppeNr,BongLinje.KasseNr,KasseNr,BongLinje.Dato,Dato,BongLinje.BongNr,BongNrRowsToBatch200CheckCurrentChangedyesRebuildOnReposnoServerOperatingModeNONEDestroyStatelessnoDisconnectAppServernoObjectNamedbonglinjeUpdateFromSourcenoToggleDataTargetsyesOpenOnInityesPromptOnDeleteyesPromptColumns(NONE)':U ,
             OUTPUT h_dbonglinje ).
       RUN repositionObject IN h_dbonglinje ( 1.00 , 146.00 ) NO-ERROR.
       /* Size in AB:  ( 2.38 , 13.00 ) */

       RUN constructObject (
             INPUT  'prg/vbonghode.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'EnabledObjFldsToDisable(None)DataSourceNamesUpdateTargetNamesLogicalObjectNameLogicalObjectNamePhysicalObjectNameDynamicObjectnoRunAttributeHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_vbonghode ).
       RUN repositionObject IN h_vbonghode ( 1.00 , 1.00 ) NO-ERROR.
       /* Size in AB:  ( 5.24 , 158.00 ) */

       RUN constructObject (
             INPUT  'adm2/folder.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'FolderLabels':U + 'Kvitteringslinjer|Utskriftskopi|Grunndata|Feilmeldinger' + 'FolderTabWidth0FolderFont-1HideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_folder ).
       RUN repositionObject IN h_folder ( 6.24 , 1.00 ) NO-ERROR.
       RUN resizeObject IN h_folder ( 16.19 , 158.00 ) NO-ERROR.

       /* Links to SmartDataObject h_dbonghode. */
       RUN addLink ( THIS-PROCEDURE , 'Data':U , h_dbonghode ).

       /* Links to SmartDataObject h_dbonglinje. */
       RUN addLink ( h_dbonghode , 'Data':U , h_dbonglinje ).

       /* Links to SmartDataViewer h_vbonghode. */
       RUN addLink ( h_dbonghode , 'Data':U , h_vbonghode ).
       RUN addLink ( h_vbonghode , 'Update':U , h_dbonghode ).

       /* Links to SmartFolder h_folder. */
       RUN addLink ( h_folder , 'Page':U , THIS-PROCEDURE ).

    END. /* Page 0 */

    WHEN 1 THEN DO:
       RUN constructObject (
             INPUT  'prg/bbonglinje.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'ScrollRemotenoNumDown0CalcWidthnoMaxWidth80FetchOnReposToEndyesDataSourceNamesUpdateTargetNamesLogicalObjectNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_bbonglinje ).
       RUN repositionObject IN h_bbonglinje ( 7.67 , 2.00 ) NO-ERROR.
       RUN resizeObject IN h_bbonglinje ( 14.52 , 156.00 ) NO-ERROR.

       /* Links to SmartDataBrowser h_bbonglinje. */
       RUN addLink ( h_dbonglinje , 'Data':U , h_bbonglinje ).
       RUN addLink ( h_bbonglinje , 'Update':U , h_dbonglinje ).

    END. /* Page 1 */

    WHEN 2 THEN DO:
       RUN constructObject (
             INPUT  'prg/vbonghodeutskriftskopi.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'EnabledObjFldsToDisable(None)DataSourceNamesUpdateTargetNamesLogicalObjectNameLogicalObjectNamePhysicalObjectNameDynamicObjectnoRunAttributeHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_vbonghodeutskriftskopi ).
       RUN repositionObject IN h_vbonghodeutskriftskopi ( 7.67 , 3.00 ) NO-ERROR.
       /* Size in AB:  ( 13.81 , 96.00 ) */

       /* Links to SmartDataViewer h_vbonghodeutskriftskopi. */
       RUN addLink ( h_dbonghode , 'Data':U , h_vbonghodeutskriftskopi ).
       RUN addLink ( h_vbonghode , 'GroupAssign':U , h_vbonghodeutskriftskopi ).

    END. /* Page 2 */

    WHEN 3 THEN DO:
       RUN constructObject (
             INPUT  'sdo/dbonglinje.wDB-AWARE':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'AppServiceASUsePromptASInfoForeignFieldsBongLinje.ButikkNr,ButikkNr,BongLinje.GruppeNr,GruppeNr,BongLinje.KasseNr,KasseNr,BongLinje.Dato,Dato,BongLinje.BongNr,BongNrRowsToBatch200CheckCurrentChangedyesRebuildOnReposnoServerOperatingModeNONEDestroyStatelessnoDisconnectAppServernoObjectNamedbonglinjeUpdateFromSourcenoToggleDataTargetsyesOpenOnInityesPromptOnDeleteyesPromptColumns(NONE)':U ,
             OUTPUT h_dbonglinje-2 ).
       RUN repositionObject IN h_dbonglinje-2 ( 1.48 , 136.00 ) NO-ERROR.
       /* Size in AB:  ( 1.86 , 10.80 ) */

       RUN constructObject (
             INPUT  'prg/bbonglinjeoriginaldata.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'ScrollRemotenoNumDown0CalcWidthnoMaxWidth80FetchOnReposToEndyesDataSourceNamesUpdateTargetNamesLogicalObjectNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_bbonglinjeoriginaldata ).
       RUN repositionObject IN h_bbonglinjeoriginaldata ( 7.67 , 2.00 ) NO-ERROR.
       RUN resizeObject IN h_bbonglinjeoriginaldata ( 14.05 , 156.00 ) NO-ERROR.

       /* Links to SmartDataObject h_dbonglinje-2. */
       RUN addLink ( h_dbonghode , 'Data':U , h_dbonglinje-2 ).

       /* Links to SmartDataBrowser h_bbonglinjeoriginaldata. */
       RUN addLink ( h_dbonglinje-2 , 'Data':U , h_bbonglinjeoriginaldata ).
       RUN addLink ( h_bbonglinjeoriginaldata , 'Update':U , h_dbonglinje-2 ).

    END. /* Page 3 */

    WHEN 4 THEN DO:
       RUN constructObject (
             INPUT  'prg/vbonghodlogg.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'EnabledObjFldsToDisable(None)DataSourceNamesUpdateTargetNamesLogicalObjectNameLogicalObjectNamePhysicalObjectNameDynamicObjectnoRunAttributeHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_vbonghodlogg ).
       RUN repositionObject IN h_vbonghodlogg ( 7.43 , 2.00 ) NO-ERROR.
       /* Size in AB:  ( 13.81 , 96.00 ) */

       /* Links to SmartDataViewer h_vbonghodlogg. */
       RUN addLink ( h_dbonghode , 'Data':U , h_vbonghodlogg ).
       RUN addLink ( h_vbonghode , 'GroupAssign':U , h_vbonghodlogg ).

       /* Adjust the tab order of the smart objects. */
    END. /* Page 4 */

  END CASE.
  /* Select a Startup page. */
  IF currentPage eq 0
  THEN RUN selectPage IN THIS-PROCEDURE ( 1 ).

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setFirstNextHandle fFrameWin 
PROCEDURE setFirstNextHandle :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER iphFirstNext AS HANDLE     NO-UNDO.
    RUN setFirstNextHandle IN h_vbonghode (iphFirstNext).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

