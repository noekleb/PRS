&ANALYZE-SUSPEND _VERSION-NUMBER AB_v9r12 GUI ADM2
&ANALYZE-RESUME
/* Connected Databases 
          skotex           PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW

/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE RowObject
       {"dkjedensbutikker.i"}.


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS vTableWin 
/*------------------------------------------------------------------------

  File:

  Description: from viewer.w - Template for SmartDataViewer objects

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
DEF VAR cTekst AS CHAR NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartDataViewer
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER FRAME

&Scoped-define ADM-SUPPORTED-LINKS Data-Target,Update-Source,TableIO-Target,GroupAssign-Source,GroupAssign-Target

/* Include file with RowObject temp-table definition */
&Scoped-define DATA-FIELD-DEFS "dkjedensbutikker.i"

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME F-Main

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS RowObject.ButikkNavn RowObject.Firmanavn ~
RowObject.Adresse1 RowObject.Adresse2 RowObject.PostNr RowObject.Telefon ~
RowObject.Telefaks RowObject.Mobil RowObject.E-Mail RowObject.DagligLeder ~
RowObject.Kontaktperson RowObject.OrganisasjonsNr ~
RowObject.OppstartButikkdata RowObject.Medlemsstatus RowObject.UtmeldtDato 
&Scoped-define ENABLED-TABLES RowObject
&Scoped-define FIRST-ENABLED-TABLE RowObject
&Scoped-Define ENABLED-OBJECTS B-Post BUTTON-1 RECT-4 RECT-5 RECT-6 
&Scoped-Define DISPLAYED-FIELDS RowObject.ButikkNr RowObject.ButikkNavn ~
RowObject.Firmanavn RowObject.KjedeNr RowObject.fuKjedenavn ~
RowObject.Adresse1 RowObject.Adresse2 RowObject.RegionNr ~
RowObject.fuRegionavn RowObject.PostNr RowObject.fuPostSted ~
RowObject.DistriktNr RowObject.fuDistriktnavn RowObject.Telefon ~
RowObject.Telefaks RowObject.Mobil RowObject.E-Mail RowObject.DagligLeder ~
RowObject.Kontaktperson RowObject.OrganisasjonsNr ~
RowObject.OppstartButikkdata RowObject.Medlemsstatus RowObject.UtmeldtDato 
&Scoped-define DISPLAYED-TABLES RowObject
&Scoped-define FIRST-DISPLAYED-TABLE RowObject


/* Custom List Definitions                                              */
/* ADM-ASSIGN-FIELDS,List-2,List-3,List-4,List-5,List-6                 */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */


/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_dbeliggenhet AS HANDLE NO-UNDO.
DEFINE VARIABLE h_ddriftsform AS HANDLE NO-UNDO.
DEFINE VARIABLE h_ddriftstype AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dynsBeliggenhet AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dynselform AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dynseltype AS HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON B-Post 
     LABEL "..." 
     SIZE 4.6 BY 1.

DEFINE BUTTON BUTTON-1 
     LABEL "Bytt tilhørighet" 
     SIZE 17 BY 1.14.

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 41 BY 7.62.

DEFINE RECTANGLE RECT-5
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 106.2 BY 7.62.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 147 BY 3.57.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     RowObject.ButikkNr AT ROW 1.24 COL 20.2 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
     RowObject.ButikkNavn AT ROW 2.24 COL 20.2 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 32 BY 1
     RowObject.Firmanavn AT ROW 2.24 COL 65 COLON-ALIGNED FORMAT "X(60)"
          VIEW-AS FILL-IN 
          SIZE 32 BY 1
     RowObject.KjedeNr AT ROW 2.24 COL 115.8 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 4.8 BY 1
     RowObject.fuKjedenavn AT ROW 2.24 COL 122 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
     RowObject.Adresse1 AT ROW 3.24 COL 20.2 COLON-ALIGNED FORMAT "X(60)"
          VIEW-AS FILL-IN 
          SIZE 32 BY 1
     RowObject.Adresse2 AT ROW 3.24 COL 65 COLON-ALIGNED FORMAT "X(60)"
          VIEW-AS FILL-IN 
          SIZE 32 BY 1
     RowObject.RegionNr AT ROW 3.24 COL 115.8 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 4.8 BY 1
     RowObject.fuRegionavn AT ROW 3.24 COL 122 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
     RowObject.PostNr AT ROW 4.24 COL 20.2 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 15.6 BY 1
     B-Post AT ROW 4.24 COL 38
     RowObject.fuPostSted AT ROW 4.24 COL 65 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 32 BY 1
     RowObject.DistriktNr AT ROW 4.24 COL 115.8 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 6.2 BY 1
     RowObject.fuDistriktnavn AT ROW 4.24 COL 122 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
     RowObject.Telefon AT ROW 5.24 COL 20.2 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 27 BY 1
     RowObject.Telefaks AT ROW 5.24 COL 65 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 27 BY 1
     BUTTON-1 AT ROW 5.57 COL 117.8
     RowObject.Mobil AT ROW 6.24 COL 20.2 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 27 BY 1
     RowObject.E-Mail AT ROW 6.24 COL 65 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 32 BY 1
     RowObject.DagligLeder AT ROW 7.24 COL 20.2 COLON-ALIGNED FORMAT "X(60)"
          VIEW-AS FILL-IN 
          SIZE 32 BY 1
     RowObject.Kontaktperson AT ROW 7.24 COL 65 COLON-ALIGNED FORMAT "X(60)"
          VIEW-AS FILL-IN 
          SIZE 32 BY 1
     RowObject.OrganisasjonsNr AT ROW 8.76 COL 20.2 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 17.6 BY 1
     RowObject.OppstartButikkdata AT ROW 8.76 COL 120.8 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 13.2 BY 1
     RowObject.Medlemsstatus AT ROW 9.76 COL 20.2 COLON-ALIGNED
          VIEW-AS COMBO-BOX INNER-LINES 25
          LIST-ITEM-PAIRS "Medlem",1,
                     "Observatør",8,
                     "Tidligere medlem",9
          DROP-DOWN-LIST
          SIZE 32 BY 1
     RowObject.UtmeldtDato AT ROW 9.76 COL 120.8 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 13.2 BY 1
     RECT-4 AT ROW 1 COL 107
     RECT-5 AT ROW 1 COL 1
     RECT-6 AT ROW 8.52 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY USE-DICT-EXPS 
         SIDE-LABELS NO-UNDERLINE THREE-D NO-AUTO-VALIDATE 
         AT COL 1 ROW 1 SCROLLABLE .


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartDataViewer
   Data Source: "dkjedensbutikker.w"
   Allow: Basic,DB-Fields,Smart
   Container Links: Data-Target,Update-Source,TableIO-Target,GroupAssign-Source,GroupAssign-Target
   Frames: 1
   Add Fields to: Neither
   Other Settings: PERSISTENT-ONLY COMPILE
   Temp-Tables and Buffers:
      TABLE: RowObject D "?" ?  
      ADDITIONAL-FIELDS:
          {dkjedensbutikker.i}
      END-FIELDS.
   END-TABLES.
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
  CREATE WINDOW vTableWin ASSIGN
         HEIGHT             = 11.1
         WIDTH              = 147.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB vTableWin 
/* ************************* Included-Libraries *********************** */

{src/adm2/viewer.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW vTableWin
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F-Main
   NOT-VISIBLE Size-to-Fit                                              */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN RowObject.Adresse1 IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN RowObject.Adresse2 IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN RowObject.ButikkNr IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN RowObject.DagligLeder IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN RowObject.DistriktNr IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN RowObject.Firmanavn IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN RowObject.fuDistriktnavn IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       RowObject.fuDistriktnavn:READ-ONLY IN FRAME F-Main        = TRUE.

/* SETTINGS FOR FILL-IN RowObject.fuKjedenavn IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       RowObject.fuKjedenavn:READ-ONLY IN FRAME F-Main        = TRUE.

/* SETTINGS FOR FILL-IN RowObject.fuPostSted IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       RowObject.fuPostSted:READ-ONLY IN FRAME F-Main        = TRUE.

/* SETTINGS FOR FILL-IN RowObject.fuRegionavn IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       RowObject.fuRegionavn:READ-ONLY IN FRAME F-Main        = TRUE.

/* SETTINGS FOR FILL-IN RowObject.KjedeNr IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN RowObject.Kontaktperson IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN RowObject.RegionNr IN FRAME F-Main
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F-Main
/* Query rebuild information for FRAME F-Main
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* FRAME F-Main */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME B-Post
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Post vTableWin
ON CHOOSE OF B-Post IN FRAME F-Main /* ... */
OR "F10" OF RowObject.PostNr
DO:
  /* Kaller søkerutine */
  RUN gPost.w (
    INPUT-OUTPUT cTekst, /* Returstreng - chr(1) separert */
    "", /* Feltliste avgrensningsfelt (kommaseparert) */
    "", /* Feltverdier (chr(1) sep) */ 
    RowObject.PostNr:SCREEN-VALUE /* Post markøren skal stå på */
    ).
  IF RETURN-VALUE = "AVBRYT" THEN
      RETURN NO-APPLY.
  IF NUM-ENTRIES(cTekst,CHR(1)) >= 3 THEN
  DO:
      /* Legger opp verdier I de aktuelle feltene */
      ASSIGN
        RowOBject.PostNr:SCREEN-VALUE      = ENTRY(2,cTekst,CHR(1))
        RowObject.PostNr:MODIFIED          = TRUE
        RowOBject.fuPostSted:SCREEN-VALUE  = ENTRY(3,cTekst,CHR(1))
/*         RowObject.PostNr:MODIFIED          = FALSE */
        .

        /* Flagger at det er gjort endringer på recorden og trigger toolbar. */
        APPLY "VALUE-CHANGED":U TO RowObject.PostNr.
        RETURN NO-APPLY.

      /* Flagger at et felt er endret. */
      dynamic-function('assignLinkProperty', 
                       'GroupAssign-Target':U, 
                       'DataModified':U, 'yes':U).
  END.
  MESSAGE RowObject.PostNr:MODIFIED
      VIEW-AS ALERT-BOX INFO BUTTONS OK.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-1 vTableWin
ON CHOOSE OF BUTTON-1 IN FRAME F-Main /* Bytt tilhørighet */
DO:
    DEFINE VARIABLE iKjede    AS INTEGER    NO-UNDO.
    DEFINE VARIABLE iRegion   AS INTEGER    NO-UNDO.
    DEFINE VARIABLE iDistrikt AS INTEGER    NO-UNDO.
    DEFINE VARIABLE cKjedeNavn    AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cRegionNavn   AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cDistriktNavn AS CHARACTER  NO-UNDO.
    ASSIGN iKjede    = INPUT RowObject.KjedeNr
           iRegion   = INPUT RowObject.RegionNr
           iDistrikt = INPUT RowObject.DistriktNr.
    RUN gByttKjede.w (INPUT-OUTPUT iKjede,INPUT-OUTPUT iRegion,INPUT-OUTPUT iDistrikt,
                      OUTPUT cKjedeNavn,OUTPUT cRegionNavn, OUTPUT cDistriktNavn).
    IF NOT RETURN-VALUE = "AVBRYT" THEN DO:
        ASSIGN RowObject.KjedeNr:SCREEN-VALUE    = STRING(iKjede)
               RowObject.RegionNr:SCREEN-VALUE   = STRING(iRegion)
               RowObject.DistriktNr:SCREEN-VALUE = STRING(iDistrikt)
               RowObject.fuKjedenavn:SCREEN-VALUE    = cKjedeNavn  
               RowObject.fuRegionavn:SCREEN-VALUE    = cRegionNavn
               RowObject.fuDistriktnavn:SCREEN-VALUE = cDistriktNavn
               RowObject.fuKjedenavn:MODIFIED    = FALSE
               RowObject.fuRegionavn:MODIFIED    = FALSE
               RowObject.fuDistriktnavn:MODIFIED = FALSE.
        APPLY "VALUE-CHANGED" TO FRAME {&FRAME-NAME}.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RowObject.PostNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RowObject.PostNr vTableWin
ON TAB OF RowObject.PostNr IN FRAME F-Main /* PostNr */
OR "RETURN":U OF Rowobject.PostNr
DO:
  RUN SjekkPostNr IN 
           DYNAMIC-FUNCTION('getDataSource':U) 
           (INPUT INPUT RowObject.PostNr).
  IF RETURN-VALUE = "AVBRYT" THEN
  DO:
      MESSAGE "Ugyldig postnummer."
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      RETURN NO-APPLY.
  END.
  ELSE DO:
      ASSIGN
          RowObject.fuPostSted:SCREEN-VALUE = RETURN-VALUE
          RowObject.fuPostSted:MODIFIED     = FALSE
          .
  END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK vTableWin 


/* ***************************  Main Block  *************************** */

  &IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
    RUN initializeObject.
  &ENDIF         
  
  /************************ INTERNAL PROCEDURES ********************/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE addRecord vTableWin 
PROCEDURE addRecord :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  RUN SUPER.

  /* Code placed here will execute AFTER standard behavior.    */
  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN 
      RowObject.ButikkNr:SENSITIVE = TRUE
      .
  END.
  RUN SetFokus. /* Setter fokus i ønsket felt. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects vTableWin  _ADM-CREATE-OBJECTS
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
             INPUT  'sdo/dbeliggenhet.wDB-AWARE':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'AppServiceASUsePromptASInfoForeignFieldsRowsToBatch200CheckCurrentChangedyesRebuildOnReposnoServerOperatingModeNONEDestroyStatelessnoDisconnectAppServernoObjectNamedbeliggenhetUpdateFromSourcenoToggleDataTargetsyesOpenOnInityesPromptOnDeleteyesPromptColumns(NONE)':U ,
             OUTPUT h_dbeliggenhet ).
       RUN repositionObject IN h_dbeliggenhet ( 1.71 , 100.00 ) NO-ERROR.
       /* Size in AB:  ( 1.86 , 10.80 ) */

       RUN constructObject (
             INPUT  'adm2/dynselect.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'AutoRefreshnoChangedEventDisplayedFieldBeliggenhetNavnKeyFieldBeliggenhetIdDataSourceFilterNumRows5OptionalnoOptionalString':U + '<none>' + 'LabelBeliggenhetSortyesViewAsCombo-box:drop-down-listToolTipFormatHelpId0BrowseTitleBrowseFieldsExitBrowseOnActionnoCancelBrowseOnExitnoRepositionDataSourcenoDefineAnyKeyTriggeryesStartBrowseKeysFieldNameBeliggenhetIdDisplayFieldyesEnableFieldyesHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_dynsBeliggenhet ).
       RUN repositionObject IN h_dynsBeliggenhet ( 10.76 , 22.20 ) NO-ERROR.
       RUN resizeObject IN h_dynsBeliggenhet ( 1.00 , 32.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'sdo/ddriftsform.wDB-AWARE':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'AppServiceASUsePromptASInfoForeignFieldsRowsToBatch200CheckCurrentChangedyesRebuildOnReposnoServerOperatingModeNONEDestroyStatelessnoDisconnectAppServernoObjectNameddriftsformUpdateFromSourcenoToggleDataTargetsyesOpenOnInityesPromptOnDeleteyesPromptColumns(NONE)':U ,
             OUTPUT h_ddriftsform ).
       RUN repositionObject IN h_ddriftsform ( 4.10 , 100.00 ) NO-ERROR.
       /* Size in AB:  ( 1.86 , 10.80 ) */

       RUN constructObject (
             INPUT  'sdo/ddriftstype.wDB-AWARE':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'AppServiceASUsePromptASInfoForeignFieldsDriftsType.DriftsFormId,DriftsFormIdRowsToBatch200CheckCurrentChangedyesRebuildOnReposnoServerOperatingModeNONEDestroyStatelessnoDisconnectAppServernoObjectNameddriftstypeUpdateFromSourcenoToggleDataTargetsyesOpenOnInityesPromptOnDeleteyesPromptColumns(NONE)':U ,
             OUTPUT h_ddriftstype ).
       RUN repositionObject IN h_ddriftstype ( 6.00 , 100.00 ) NO-ERROR.
       /* Size in AB:  ( 1.86 , 10.80 ) */

       RUN constructObject (
             INPUT  'adm2/dynselect.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'AutoRefreshnoChangedEventDisplayedFieldDriftsTypeNavnKeyFieldDriftsTypeIdDataSourceFilterNumRows5OptionalnoOptionalString':U + '<none>' + 'Label?SortyesViewAsCombo-box:drop-down-listToolTipFormatHelpId0BrowseTitleBrowseFieldsExitBrowseOnActionnoCancelBrowseOnExitnoRepositionDataSourcenoDefineAnyKeyTriggeryesStartBrowseKeysFieldNameDriftsTypeIdDisplayFieldyesEnableFieldyesHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_dynseltype ).
       RUN repositionObject IN h_dynseltype ( 9.76 , 67.20 ) NO-ERROR.
       RUN resizeObject IN h_dynseltype ( 1.00 , 31.80 ) NO-ERROR.

       RUN constructObject (
             INPUT  'adm2/dynselect.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'AutoRefreshnoChangedEventDriftsFormSelectedDisplayedFieldDriftsFormNavnKeyFieldDriftsFormIdDataSourceFilterNumRows5OptionalnoOptionalString':U + '<none>' + 'Label?SortyesViewAsCombo-box:drop-down-listToolTipFormatHelpId0BrowseTitleBrowseFieldsExitBrowseOnActionnoCancelBrowseOnExitnoRepositionDataSourcenoDefineAnyKeyTriggeryesStartBrowseKeysFieldNameDriftsFormIdDisplayFieldyesEnableFieldyesHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_dynselform ).
       RUN repositionObject IN h_dynselform ( 8.76 , 67.20 ) NO-ERROR.
       RUN resizeObject IN h_dynselform ( 1.00 , 31.80 ) NO-ERROR.

       /* Links to SmartDataField h_dynsBeliggenhet. */
       RUN addLink ( h_dbeliggenhet , 'Data':U , h_dynsBeliggenhet ).

       /* Links to SmartDataObject h_ddriftstype. */
       RUN addLink ( h_ddriftsform , 'Data':U , h_ddriftstype ).

       /* Links to SmartDataField h_dynseltype. */
       RUN addLink ( h_ddriftstype , 'Data':U , h_dynseltype ).

       /* Links to SmartDataField h_dynselform. */
       RUN addLink ( h_ddriftsform , 'Data':U , h_dynselform ).

       /* Adjust the tab order of the smart objects. */
       RUN adjustTabOrder ( h_dynselform ,
             RowObject.OrganisasjonsNr:HANDLE IN FRAME F-Main , 'AFTER':U ).
       RUN adjustTabOrder ( h_dynseltype ,
             RowObject.Medlemsstatus:HANDLE IN FRAME F-Main , 'AFTER':U ).
       RUN adjustTabOrder ( h_dynsBeliggenhet ,
             RowObject.UtmeldtDato:HANDLE IN FRAME F-Main , 'AFTER':U ).
    END. /* Page 0 */

  END CASE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE cancelRecord vTableWin 
PROCEDURE cancelRecord :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  RUN SUPER.

  /* Code placed here will execute AFTER standard behavior.    */
  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN
      RowObject.ButikkNr:SENSITIVE = FALSE
      .
  END.
  RUN SetFokus. /* Setter fokus i ønsket felt. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE confirmExit vTableWin 
PROCEDURE confirmExit :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

  DEFINE INPUT-OUTPUT PARAMETER plCancel AS LOGICAL NO-UNDO.

  /* Code placed here will execute PRIOR to standard behavior. */
  DEF VAR pbDataModified AS LOG NO-UNDO.
  ASSIGN
    pbDataModified = DYNAMIC-FUNCTION('getDataModified':U)
      .
  DO WITH FRAME {&FRAME-NAME}:
    IF pbDataModified THEN
    DO:
        MESSAGE "Det er gjort endringer på posten." SKIP
                "Disse må lagres eller kanseleres før programmet kan avsluttes."
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        ASSIGN
            plCancel = TRUE /* Flagger at avsluttning skal avbrytes */
            .
        RETURN NO-APPLY.
    END.
  END.

  RUN SUPER( INPUT-OUTPUT plCancel).

  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE copyRecord vTableWin 
PROCEDURE copyRecord :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  RUN SUPER.

  /* Code placed here will execute AFTER standard behavior.    */
  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN 
      RowObject.ButikkNr:SENSITIVE = TRUE
      .
  END.
  RUN SetFokus. /* Setter fokus i ønsket felt. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE dataAvailable vTableWin 
PROCEDURE dataAvailable :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

  DEFINE INPUT PARAMETER pcRelative AS CHARACTER NO-UNDO.

  /* Code placed here will execute PRIOR to standard behavior. */

  RUN SUPER( INPUT pcRelative).
  /* Code placed here will execute AFTER standard behavior.    */
  IF VALID-HANDLE(h_ddriftstype) THEN DO:
      DYNAMIC-FUNCTION('findRowWhere':U IN h_ddriftsform,
          INPUT "DriftsFormId" /* CHARACTER */,
          INPUT DYNAMIC-FUNCTION('getKeyFieldValue':U IN h_dynselform) /* CHARACTER */,
          INPUT "=" /* CHARACTER */).
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI vTableWin  _DEFAULT-DISABLE
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
  HIDE FRAME F-Main.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DriftsFormSelected vTableWin 
PROCEDURE DriftsFormSelected :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT  PARAMETER cSelected AS CHARACTER  NO-UNDO.
   DYNAMIC-FUNCTION('findRowWhere':U IN h_ddriftsform,
     INPUT "DriftsFormId" /* CHARACTER */,
      INPUT cSelected /* CHARACTER */,
      INPUT "=" /* CHARACTER */).
/*    DYNAMIC-FUNCTION('openQuery':U IN h_ddriftstype). */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initializeObject vTableWin 
PROCEDURE initializeObject :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  RUN SUPER.

  /* Code placed here will execute AFTER standard behavior.    */
  SUBSCRIBE TO "DriftsFormSelected" IN h_dynselform.
  ASSIGN hDataSource =   WIDGET-HANDLE(DYNAMIC-FUNCTION('linkHandles':U,
       INPUT "DATA-SOURCE" /* CHARACTER */)).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SetFokus vTableWin 
PROCEDURE SetFokus :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DO WITH FRAME {&FRAME-NAME}:
  IF RowObject.ButikkNr:SENSITIVE = TRUE THEN
    RUN ApplyEntry ("ButikkNr").
  ELSE
    RUN ApplyEntry ("ButikkNavn").
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE updateRecord vTableWin 
PROCEDURE updateRecord :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  ASSIGN RowObject.fuPostSted:MODIFIED IN FRAME {&FRAME-NAME} = FALSE.
  RUN SUPER.

  /* Code placed here will execute AFTER standard behavior.    */
  IF RETURN-VALUE <> "ADM-ERROR" THEN
  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN
      RowObject.ButikkNr:SENSITIVE = FALSE
      .
    DYNAMIC-FUNCTION('openQuery':U IN hDataSource).

  END.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

