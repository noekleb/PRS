&ANALYZE-SUSPEND _VERSION-NUMBER AB_v9r12 GUI ADM2
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME gDialog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS gDialog 
/*------------------------------------------------------------------------

  File: 

  Description: from cntnrdlg.w - ADM2 SmartDialog Template

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

&IF "{&UIB_is_Running}" = ""  &THEN
  DEF INPUT-OUTPUT PARAMETER cColValues AS char NO-UNDO.
  DEF INPUT        PARAMETER cFelt      as char NO-UNDO.
  DEF INPUT        PARAMETER cVerdier   as char NO-UNDO.
  DEF INPUT        PARAMETER cStart   as char NO-UNDO.
&else
  DEF VAR cColValues AS char NO-UNDO.
  DEF VAR cFelt             AS CHAR NO-UNDO.
  DEF VAR cVerdier          AS CHAR NO-UNDO.
  DEF VAR cStart          AS CHAR NO-UNDO.
&ENDIF  

/* Local Variable Definitions ---                                       */

DEF VAR cReturn-Value AS CHAR INITIAL "AVBRYT" NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartDialog
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER DIALOG-BOX

&Scoped-define ADM-SUPPORTED-LINKS Data-Target,Data-Source,Page-Target,Update-Source,Update-Target

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME gDialog

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BtnCancel 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_dkassererkontanter AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dyntoolbar AS HANDLE NO-UNDO.
DEFINE VARIABLE h_vkassererkontanter AS HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BtnCancel AUTO-END-KEY DEFAULT 
     LABEL "Avslutt" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME gDialog
     BtnCancel AT ROW 15.05 COL 35
     SPACE(0.39) SKIP(0.04)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Kontanter"
         CANCEL-BUTTON BtnCancel.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartDialog
   Allow: Basic,Browse,DB-Fields,Query,Smart
   Container Links: Data-Target,Data-Source,Page-Target,Update-Source,Update-Target
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB gDialog 
/* ************************* Included-Libraries *********************** */

{src/adm2/containr.i}
{dproclibstart.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX gDialog
                                                                        */
ASSIGN 
       FRAME gDialog:SCROLLABLE       = FALSE
       FRAME gDialog:HIDDEN           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX gDialog
/* Query rebuild information for DIALOG-BOX gDialog
     _Options          = "SHARE-LOCK"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX gDialog */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME gDialog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL gDialog gDialog
ON GO OF FRAME gDialog /* Kontanter */
DO:
    ASSIGN
  cReturn-Value = "OK"
  .



END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL gDialog gDialog
ON WINDOW-CLOSE OF FRAME gDialog /* Kontanter */
DO:  
  /* Add Trigger to equate WINDOW-CLOSE to END-ERROR. */
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BtnCancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnCancel gDialog
ON CHOOSE OF BtnCancel IN FRAME gDialog /* Avslutt */
DO:
  IF DYNAMIC-FUNCTION('getDataModified':U IN h_dkassererkontanter) THEN
  DO:
      MESSAGE "Registrerte/endrede data må lagres før det kan avsluttes." 
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      RETURN NO-APPLY.
  END.
  ASSIGN
      cReturn-Value = "OK"
      .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK gDialog 


/* ***************************  Main Block  *************************** */

{lng.i &SDO = 'SDO'}

{src/adm2/dialogmn.i}

RETURN cReturn-Value.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects gDialog  _ADM-CREATE-OBJECTS
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
             INPUT  'sdo/dkassererkontanter.wDB-AWARE':U ,
             INPUT  FRAME gDialog:HANDLE ,
             INPUT  'AppServiceASUsePromptASInfoForeignFieldsRowsToBatch200CheckCurrentChangedyesRebuildOnReposnoServerOperatingModeNONEDestroyStatelessnoDisconnectAppServernoObjectNamedkassererkontanterUpdateFromSourcenoToggleDataTargetsyesOpenOnInityesPromptOnDeleteyesPromptColumns(NONE)':U ,
             OUTPUT h_dkassererkontanter ).
       RUN repositionObject IN h_dkassererkontanter ( 14.10 , 20.00 ) NO-ERROR.
       /* Size in AB:  ( 1.86 , 10.80 ) */

       RUN constructObject (
             INPUT  'prg/vkassererkontanter.w':U ,
             INPUT  FRAME gDialog:HANDLE ,
             INPUT  'EnabledObjFldsToDisable(None)DataSourceNamesUpdateTargetNamesLogicalObjectNameLogicalObjectNamePhysicalObjectNameDynamicObjectnoRunAttributeHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_vkassererkontanter ).
       RUN repositionObject IN h_vkassererkontanter ( 2.43 , 2.00 ) NO-ERROR.
       /* Size in AB:  ( 12.38 , 48.00 ) */

       RUN constructObject (
             INPUT  'adm2/dyntoolbar.w':U ,
             INPUT  FRAME gDialog:HANDLE ,
             INPUT  'FlatButtonsyesMenunoShowBorderyesToolbaryesActionGroupsTableioSubModulesTableIOTypeSaveSupportedLinksTableio-sourceToolbarBandsToolbarParentMenuToolbarAutoSizenoToolbarDrawDirectionHorizontalToolbarInitialStateLogicalObjectNameAutoResizeDisabledActionsHiddenActionsUpdate,First,Prev,Next,Last,Add,Copy,DeleteHiddenToolbarBandsHiddenMenuBandsMenuMergeOrder0EdgePixels2PanelTypeToolbarDeactivateTargetOnHidenoDisabledActionsNavigationTargetNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_dyntoolbar ).
       RUN repositionObject IN h_dyntoolbar ( 1.00 , 2.00 ) NO-ERROR.
       RUN resizeObject IN h_dyntoolbar ( 1.24 , 48.00 ) NO-ERROR.

       /* Links to SmartDataViewer h_vkassererkontanter. */
       RUN addLink ( h_dkassererkontanter , 'Data':U , h_vkassererkontanter ).
       RUN addLink ( h_vkassererkontanter , 'Update':U , h_dkassererkontanter ).
       RUN addLink ( h_dyntoolbar , 'TableIo':U , h_vkassererkontanter ).

       /* Adjust the tab order of the smart objects. */
       RUN adjustTabOrder ( h_dyntoolbar ,
             BtnCancel:HANDLE , 'BEFORE':U ).
       RUN adjustTabOrder ( h_vkassererkontanter ,
             h_dyntoolbar , 'AFTER':U ).
    END. /* Page 0 */

  END CASE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE createObjects gDialog 
PROCEDURE createObjects :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  RUN SUPER.

  /* Code placed here will execute AFTER standard behavior.    */
  
  /* Setter opp Where sats i Query.  */
  /* Legges inn der hvor det skal gjøres avgrensning I datasett for søk. */
  IF cFelt <> "" THEN
  DO:
    DYNAMIC-FUNCTION('assignQuerySelection':U IN h_dkassererkontanter,
      INPUT cFelt,    /* Comma separerte verdier  */
      INPUT cVerdier, /* CHR(1) separerte verdier */
      INPUT "EQ").
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE destroyObject gDialog 
PROCEDURE destroyObject :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR piLoop AS INT NO-UNDO.
  DEF VAR plSum  AS INT NO-UNDO.

  /* Code placed here will execute PRIOR to standard behavior. */
  IF cReturn-Value <> "AVBRYT" THEN  
  DO:
      ASSIGN
        cColValues    = DYNAMIC-FUNCTION('colValues':U IN h_dkassererkontanter,
                          INPUT "Belop1,Belop2,Belop3,Belop4,Belop5,Belop6,Belop7,Belop8,Belop9,Belop10" /* CHARACTER */)
        .
  END.
  
  RUN SUPER.

  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI gDialog  _DEFAULT-DISABLE
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
  HIDE FRAME gDialog.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI gDialog  _DEFAULT-ENABLE
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
  ENABLE BtnCancel 
      WITH FRAME gDialog.
  VIEW FRAME gDialog.
  {&OPEN-BROWSERS-IN-QUERY-gDialog}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initializeObject gDialog 
PROCEDURE initializeObject :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  RUN SwitchLng.
  RUN SUPER.

  /* Code placed here will execute AFTER standard behavior.    */
  DYNAMIC-FUNCTION('findRowWhere':U IN h_dkassererkontanter,
     INPUT cFelt    + (IF cFelt = "" THEN "" ELSE ","),
     INPUT cVerdier + (IF cVerdier = "" THEN "" ELSE CHR(1)),
     INPUT "EQ,EQ,EQ,EQ,EQ" /* CHARACTER */).
  /* Signalerer til de andre objektene. */
  RUN dataAvailable IN h_dkassererkontanter
    ( INPUT "SAME" /* CHARACTER */).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

