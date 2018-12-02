&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
/* Connected Databases 
          skotex           PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW


/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE RowObject
       {"dselger.i"}.



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
DEF VAR cTekst AS CHAR NO-UNDO.
DEF VAR piLoop AS INT NO-UNDO.

DEFINE VARIABLE cSkoModus AS CHARACTER   NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartDataViewer
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER FRAME

&Scoped-define ADM-SUPPORTED-LINKS Data-Target,Update-Source,TableIO-Target,GroupAssign-Source,GroupAssign-Target

/* Include file with RowObject temp-table definition */
&Scoped-define DATA-FIELD-DEFS "dselger.i"

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS RowObject.ForNavn RowObject.Navn ~
RowObject.NavnIKasse RowObject.AnsattNr RowObject.PersonNr ~
RowObject.Adresse1 RowObject.Adresse2 RowObject.PostNr RowObject.Telefon ~
RowObject.Mobiltelefon RowObject.ButikkNr RowObject.FodtDato ~
RowObject.AnsattDato RowObject.SluttetDato RowObject.JobTittel ~
RowObject.ArbeidsProsent RowObject.LonnProfil RowObject.TimeLonn ~
RowObject.FastLonn RowObject.deciPWD 
&Scoped-define ENABLED-TABLES RowObject
&Scoped-define FIRST-ENABLED-TABLE RowObject
&Scoped-Define ENABLED-OBJECTS BUTTON-Reset RECT-59 BUTTON-Sokeknapp-2 ~
BUTTON-SokPost 
&Scoped-Define DISPLAYED-FIELDS RowObject.SelgerNr RowObject.ForNavn ~
RowObject.Navn RowObject.NavnIKasse RowObject.AnsattNr RowObject.PersonNr ~
RowObject.Adresse1 RowObject.Adresse2 RowObject.PostNr RowObject.Telefon ~
RowObject.Mobiltelefon RowObject.ButikkNr RowObject.fuPostSted ~
RowObject.FodtDato RowObject.AnsattDato RowObject.SluttetDato ~
RowObject.JobTittel RowObject.ArbeidsProsent RowObject.LonnProfil ~
RowObject.TimeLonn RowObject.FastLonn RowObject.deciPWD 
&Scoped-define DISPLAYED-TABLES RowObject
&Scoped-define FIRST-DISPLAYED-TABLE RowObject


/* Custom List Definitions                                              */
/* ADM-ASSIGN-FIELDS,List-2,List-3,List-4,List-5,List-6                 */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE BUTTON BUTTON-Reset  NO-FOCUS
     LABEL "Återställ" 
     SIZE 15 BY 1.

DEFINE BUTTON BUTTON-Sokeknapp-2 
     IMAGE-UP FILE "icon\e-sokpr":U NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON BUTTON-SokPost 
     IMAGE-UP FILE "icon\e-sokpr":U NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE RECTANGLE RECT-59
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 126 BY 13.81.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     BUTTON-Reset AT ROW 9.19 COL 109
     RowObject.SelgerNr AT ROW 1.19 COL 16 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 20.2 BY 1
     RowObject.ForNavn AT ROW 2.14 COL 16 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 32 BY 1
     RowObject.Navn AT ROW 3.14 COL 16 COLON-ALIGNED
          LABEL "Etternavn"
          VIEW-AS FILL-IN 
          SIZE 32 BY 1
     RowObject.NavnIKasse AT ROW 4.1 COL 16 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
     RowObject.AnsattNr AT ROW 5.1 COL 16 COLON-ALIGNED FORMAT "X(10)"
          VIEW-AS FILL-IN 
          SIZE 20 BY 1
     RowObject.PersonNr AT ROW 6.05 COL 16 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 20 BY 1
     RowObject.Adresse1 AT ROW 7 COL 16 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 32 BY 1
     RowObject.Adresse2 AT ROW 7.95 COL 16 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 32 BY 1
     RowObject.PostNr AT ROW 8.91 COL 16 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 20 BY 1
     RowObject.Telefon AT ROW 9.86 COL 16 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 20 BY 1
     RowObject.Mobiltelefon AT ROW 10.81 COL 16 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 20 BY 1
     RowObject.ButikkNr AT ROW 11.81 COL 16 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
     BUTTON-Sokeknapp-2 AT ROW 11.81 COL 28
     RowObject.fuPostSted AT ROW 8.91 COL 40 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN NATIVE 
          SIZE 32 BY 1
     RowObject.FodtDato AT ROW 1.19 COL 91.4 COLON-ALIGNED FORMAT "99/99/9999"
          VIEW-AS FILL-IN 
          SIZE 16.6 BY 1
     RowObject.AnsattDato AT ROW 2.14 COL 91.4 COLON-ALIGNED FORMAT "99/99/9999"
          VIEW-AS FILL-IN 
          SIZE 16.6 BY 1
     RowObject.SluttetDato AT ROW 3.1 COL 91.4 COLON-ALIGNED FORMAT "99/99/9999"
          VIEW-AS FILL-IN 
          SIZE 16.6 BY 1
     RowObject.JobTittel AT ROW 4.1 COL 91.4 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 32 BY 1
     RowObject.ArbeidsProsent AT ROW 5.05 COL 91.4 COLON-ALIGNED FORMAT ">>9"
          VIEW-AS FILL-IN 
          SIZE 9.6 BY 1
     RowObject.LonnProfil AT ROW 6 COL 91.4 COLON-ALIGNED FORMAT "X(25)"
          VIEW-AS COMBO-BOX INNER-LINES 25
          LIST-ITEM-PAIRS "Item 1","Item 1"
          DROP-DOWN-LIST
          SIZE 32 BY 1
     RowObject.TimeLonn AT ROW 7 COL 91.4 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     RowObject.FastLonn AT ROW 8 COL 91.4 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 17.4 BY 1
     RowObject.deciPWD AT ROW 9.1 COL 91.4 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 14.6 BY 1
     BUTTON-SokPost AT ROW 8.91 COL 37.8
     RECT-59 AT ROW 1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY USE-DICT-EXPS 
         SIDE-LABELS NO-UNDERLINE THREE-D NO-AUTO-VALIDATE 
         AT COL 1 ROW 1 SCROLLABLE .


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartDataViewer
   Data Source: "dselger.w"
   Allow: Basic,DB-Fields,Smart
   Container Links: Data-Target,Update-Source,TableIO-Target,GroupAssign-Source,GroupAssign-Target
   Frames: 1
   Add Fields to: Neither
   Other Settings: PERSISTENT-ONLY COMPILE
   Temp-Tables and Buffers:
      TABLE: RowObject D "?" ?  
      ADDITIONAL-FIELDS:
          {dselger.i}
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
         HEIGHT             = 13.86
         WIDTH              = 126.2.
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
   NOT-VISIBLE FRAME-NAME Size-to-Fit L-To-R,COLUMNS                    */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN RowObject.AnsattDato IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN RowObject.AnsattNr IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN RowObject.ArbeidsProsent IN FRAME F-Main
   EXP-FORMAT                                                           */
ASSIGN 
       RowObject.deciPWD:READ-ONLY IN FRAME F-Main        = TRUE.

/* SETTINGS FOR FILL-IN RowObject.FodtDato IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN RowObject.fuPostSted IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       RowObject.fuPostSted:READ-ONLY IN FRAME F-Main        = TRUE.

/* SETTINGS FOR COMBO-BOX RowObject.LonnProfil IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN RowObject.Navn IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN RowObject.SelgerNr IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN RowObject.SluttetDato IN FRAME F-Main
   EXP-FORMAT                                                           */
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

&Scoped-define SELF-NAME BUTTON-Reset
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Reset vTableWin
ON CHOOSE OF BUTTON-Reset IN FRAME F-Main /* Återställ */
DO:
    APPLY "ENTRY" TO Rowobject.deciPWD.
/*     Rowobject.deciPWD:READ-ONLY = FALSE. */
    Rowobject.deciPWD:SCREEN-VALUE = "0".
    APPLY "VALUE-CHANGED" TO Rowobject.deciPWD.
    PROCESS EVENTS.
/*     Rowobject.deciPWD:READ-ONLY = TRUE.  */
/*     Rowobject.deciPWD:MODIFIED = TRUE.    */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Sokeknapp-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Sokeknapp-2 vTableWin
ON CHOOSE OF BUTTON-Sokeknapp-2 IN FRAME F-Main /* ... */
OR "F10" OF RowObject.ButikkNr DO:
  DEF VAR cButikerFieldList    AS CHAR NO-UNDO.
  DEF VAR bOk                  AS LOG  NO-UNDO.

  DO WITH FRAME {&FRAME-NAME}:
      THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
      RUN JBoxLookup.w (THIS-PROCEDURE,1000,
                        "Butiker"
                         + ";Butik"
                         + ";ButNamn"
                         ,
                       "WHERE true"
                        ,""
                        ,"Butik,ButNamn",
                        OUTPUT cButikerFieldList,
                        OUTPUT bOK).
      THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.

      IF bOk AND cButikerFieldList NE "" THEN DO:
        ASSIGN 
           RowObject.ButikkNr:SCREEN-VALUE = ENTRY(1,cButikerFieldList,"|")
           .
        APPLY "any-printable" TO RowObject.ButikkNr.
        APPLY "VALUE-CHANGED":U TO RowObject.ButikkNr.

        /* Flagger at et felt er endret. */
        dynamic-function('assignLinkProperty', 
                       'GroupAssign-Target':U, 
                       'DataModified':U, 'yes':U).


      END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-SokPost
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-SokPost vTableWin
ON CHOOSE OF BUTTON-SokPost IN FRAME F-Main /* ... */
or F10 of RowObject.PostNr
DO:
  DO WITH FRAME {&FRAME-NAME}:
  /* Kaller søkerutine */
    RUN gpost.w (
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
          RowObject.PostNr:SCREEN-VALUE     = ENTRY(2,cTekst,CHR(1))
          RowObject.fuPostSted:SCREEN-VALUE = ENTRY(3,cTekst,CHR(1))
          RowObject.fuPostSted:MODIFIED     = FALSE 
          .
        /* Flagger at det er gjort endringer på recorden og trigger toolbar. */
        APPLY "VALUE-CHANGED":U TO RowObject.PostNr.

        /* Flagger at et felt er endret. */
        dynamic-function('assignLinkProperty', 
                       'GroupAssign-Target':U, 
                       'DataModified':U, 'yes':U).

    END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RowObject.PostNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RowObject.PostNr vTableWin
ON TAB OF RowObject.PostNr IN FRAME F-Main /* PostNr */
OR "RETURN" OF RowObject.PostNr
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
          RowObject.fuPoststed:MODIFIED     = FALSE
          .
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK vTableWin 


/* ***************************  Main Block  *************************** */
  {syspara.i 1 1 54 cSkomodus}
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
  IF cSkomodus = "1" THEN DO WITH FRAME {&FRAME-NAME}:
    ASSIGN 
      RowObject.SelgerNr:SENSITIVE = TRUE
      .
  END.
  RUN SetFokus. /* Setter fokus i ønsket felt. */

  PUBLISH 'SelgerNaDis' ('1').

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
      RowObject.SelgerNr:SENSITIVE = FALSE
      .
  END.
  RUN SetFokus. /* Setter fokus i ønsket felt. */
  PUBLISH 'SelgerNaDis' ('2').

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
  /*
  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN 
      RowObject.SelgerNr:SENSITIVE = TRUE
      .
  END.
  */
  RUN SetFokus. /* Setter fokus i ønsket felt. */

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE displayRecord vTableWin 
PROCEDURE displayRecord :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  RUN SUPER.
 DO WITH FRAME {&FRAME-NAME}:
/*      MESSAGE rowobject.deciPWD:SCREEN-VALUE                                                */
/*          VIEW-AS ALERT-BOX INFO BUTTONS OK.                                                */
     IF rowobject.deciPWD:SCREEN-VALUE <> "0" THEN
        rowobject.deciPWD:SCREEN-VALUE = FILL("9",9).
 END.
  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initializeObject vTableWin 
PROCEDURE initializeObject :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DO WITH FRAME F-Main:
      cTekst = '<Ikke angitt>|'.
      RowObject.LonnProfil:DELIMITER = '|'.
      FOR EACH SysPara EXCLUSIVE-LOCK WHERE
        SysPara.SysHId =  50 AND
        SysPara.SysGr  =  21 AND
        SysPara.ParaNr >= 100 AND
        SysPara.ParaNr <= 110 AND
        SysPara.Parameter1 > '' AND
        SysPara.Parameter2 > '':
        cTekst =   cTekst 
                 + (IF cTekst <> '' THEN '|' ELSE '') 
                 + SysPara.Parameter1 + '|' + SysPara.Parameter2.
      END.
      RowObject.LonnProfil:LIST-ITEM-PAIRS = cTekst.

    RUN SUPER.

    FIND Bruker NO-LOCK WHERE
      Bruker.Brukerid = userid('SkoTex') NO-ERROR.
    IF NOT AVAILABLE Bruker OR Bruker.BrukerType > 1 THEN 
    DO:
      ASSIGN
          RowObject.LonnProfil:HIDDEN     = TRUE
          RowObject.ArbeidsProsent:HIDDEN = TRUE
          RowObject.TimeLonn:HIDDEN       = TRUE
          RowObject.FastLonn:HIDDEN       = TRUE
          .
    END.

  END.

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
  IF RowObject.SelgerNr:SENSITIVE = TRUE THEN
    RUN ApplyEntry ("SelgerNr").
  ELSE
    RUN ApplyEntry ("ForNavn").
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
  DEFINE VARIABLE dSelgerNr AS DECIMAL         NO-UNDO.
  DEFINE VARIABLE iButik    LIKE Butiker.butik NO-UNDO.
  DEF VAR lDec AS DEC NO-UNDO.

  DO WITH FRAME {&FRAME-NAME}:

      ASSIGN dSelgerNr = DECI(RowObject.SelgerNr:SCREEN-VALUE).
      ASSIGN lDec = DEC(AnsattNr:SCREEN-VALUE) NO-ERROR.
      IF ERROR-STATUS:ERROR THEN
      DO:
          MESSAGE 'Ugyldige tegn i ansattnr. Kun siffer er tillatt.'
              VIEW-AS ALERT-BOX INFO BUTTONS OK.
          RETURN NO-APPLY.
          APPLY 'ENTRY' TO AnsattNr.
      END.
  END.
  RUN SUPER.

  /* Code placed here will execute AFTER standard behavior.    */
  IF RETURN-VALUE <> "ADM-ERROR" THEN
  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN
      RowObject.SelgerNr:SENSITIVE = FALSE
      .
    IF dSelgerNr <> DECI(RowObject.SelgerNr:SCREEN-VALUE) THEN DO:
        /* Ny selger upplagd och då skapar vi en kobbling till butik */
        /* om systemet bara har en butik, anrop i SDO */
        IF DYNAMIC-FUNCTION('UnikButik':U IN DYNAMIC-FUNCTION('getDataSource':U),
                  OUTPUT iButik /* INTEGER */) THEN
            PUBLISH "NyButSelger" (iButik,DECI(RowObject.SelgerNr:SCREEN-VALUE),0,TRUE).
    END.
  END.

  PUBLISH 'SelgerNaDis' ('2').

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

