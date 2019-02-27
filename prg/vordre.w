&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
/* Connected Databases 
          skotex           PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW


/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE RowObject
       {"sdo/dordre.i"}.



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

DEFINE VARIABLE wLapTop        AS LOGICAL    NO-UNDO.
DEFINE VARIABLE h_dordre       AS HANDLE     NO-UNDO.
DEFINE VARIABLE cOrdreStatList AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cTekst         AS CHAR       NO-UNDO.
DEFINE VAR      iCl            AS INT        NO-UNDO.

DEF BUFFER clButiker FOR Butiker.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartDataViewer
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER FRAME

&Scoped-define ADM-SUPPORTED-LINKS Data-Target,Update-Source,TableIO-Target,GroupAssign-Source,GroupAssign-Target

/* Include file with RowObject temp-table definition */
&Scoped-define DATA-FIELD-DEFS "sdo/dordre.i"

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS RowObject.Hasteordre RowObject.BekreftetOrdre ~
RowObject.Merknad RowObject.LevAdresse1 RowObject.LevAdresse2 ~
RowObject.LevPostBoks RowObject.LevPostNr RowObject.LevTelefon ~
RowObject.LevKontakt RowObject.LevMerknad RowObject.Notat 
&Scoped-define ENABLED-TABLES RowObject
&Scoped-define FIRST-ENABLED-TABLE RowObject
&Scoped-Define ENABLED-OBJECTS B-Post B-SendOrdre FILL-IN-Tekst RECT-2 ~
RECT-3 
&Scoped-Define DISPLAYED-FIELDS RowObject.Hasteordre RowObject.fraERP ~
RowObject.HkOrdre RowObject.BekreftetDato RowObject.BekreftetOrdre ~
RowObject.LapTop RowObject.OrdreNr RowObject.LevNr RowObject.Merknad ~
RowObject.SendtDato RowObject.fLevNamn RowObject.LevAdresse1 ~
RowObject.LevAdresse2 RowObject.LevPostBoks RowObject.LevPostNr ~
RowObject.LevTelefon RowObject.LevKontakt RowObject.LevMerknad ~
RowObject.Notat RowObject.OrdreStatus RowObject.fStatusTxt ~
RowObject.fuLevPostSted RowObject.EkstId RowObject.VareBehNr 
&Scoped-define DISPLAYED-TABLES RowObject
&Scoped-define FIRST-DISPLAYED-TABLE RowObject
&Scoped-Define DISPLAYED-OBJECTS FILL-IN-Tekst 

/* Custom List Definitions                                              */
/* ADM-ASSIGN-FIELDS,List-2,List-3,List-4,List-5,List-6                 */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE BUTTON B-Post  NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON B-SendOrdre 
     LABEL "&Send ordre" 
     SIZE 30 BY 1.14.

DEFINE BUTTON B-SokLevNr  NO-FOCUS
     LABEL "..." 
     SIZE 4.6 BY 1.1.

DEFINE VARIABLE FILL-IN-Tekst AS CHARACTER FORMAT "X(256)":U INITIAL "Leveringsadresse" 
      VIEW-AS TEXT 
     SIZE 42 BY .62
     FONT 6 NO-UNDO.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 150 BY 4.52.

DEFINE RECTANGLE RECT-3
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 150 BY 10.76.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     RowObject.Hasteordre AT ROW 1.48 COL 131.2
          LABEL "Hasteordre"
          VIEW-AS TOGGLE-BOX
          SIZE 17.8 BY .81 NO-TAB-STOP 
     RowObject.fraERP AT ROW 3.24 COL 131
          LABEL "Fra ERP"
          VIEW-AS TOGGLE-BOX
          SIZE 13.4 BY .81 NO-TAB-STOP 
     RowObject.HkOrdre AT ROW 2.33 COL 131
          LABEL "HK ordre"
          VIEW-AS TOGGLE-BOX
          SIZE 13.4 BY .81 NO-TAB-STOP 
     RowObject.BekreftetDato AT ROW 4.05 COL 85.8 COLON-ALIGNED
          LABEL "Bekreftet"
          VIEW-AS FILL-IN 
          SIZE 18.8 BY 1
     RowObject.BekreftetOrdre AT ROW 4.14 COL 107.4
          LABEL ""
          VIEW-AS TOGGLE-BOX
          SIZE 4.4 BY .81
     RowObject.LapTop AT ROW 4.19 COL 131
          LABEL "LapTop"
          VIEW-AS TOGGLE-BOX
          SIZE 13.4 BY .81 NO-TAB-STOP 
     B-Post AT ROW 10.62 COL 30.6
     B-SokLevNr AT ROW 2.95 COL 27.8 NO-TAB-STOP 
     RowObject.OrdreNr AT ROW 1.95 COL 12.8 COLON-ALIGNED FORMAT "zzzzzzz9"
          VIEW-AS FILL-IN 
          SIZE 13.2 BY 1
     RowObject.LevNr AT ROW 3 COL 12.8 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 13.2 BY 1
     RowObject.Merknad AT ROW 1.95 COL 30.4 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 42 BY 1
     RowObject.SendtDato AT ROW 3 COL 85.8 COLON-ALIGNED FORMAT "99/99/99"
          VIEW-AS FILL-IN 
          SIZE 18.8 BY 1
     RowObject.fLevNamn AT ROW 3 COL 30.4 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 42 BY 1
     RowObject.LevAdresse1 AT ROW 7.62 COL 12.8 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 52.2 BY 1
     RowObject.LevAdresse2 AT ROW 8.62 COL 12.8 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 52.2 BY 1
     RowObject.LevPostBoks AT ROW 9.62 COL 12.8 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 52.2 BY 1
     RowObject.LevPostNr AT ROW 10.62 COL 12.8 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 15.6 BY 1
     RowObject.LevTelefon AT ROW 11.62 COL 12.8 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
     RowObject.LevKontakt AT ROW 12.62 COL 12.8 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 32 BY 1
     RowObject.LevMerknad AT ROW 13.62 COL 12.8 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 52 BY 1
     RowObject.Notat AT ROW 7.67 COL 67.6 NO-LABEL
          VIEW-AS EDITOR NO-WORD-WRAP SCROLLBAR-HORIZONTAL SCROLLBAR-VERTICAL
          SIZE 82 BY 6.91
     B-SendOrdre AT ROW 17.43 COL 2.2
     FILL-IN-Tekst AT ROW 6.62 COL 14 COLON-ALIGNED NO-LABEL
     RowObject.OrdreStatus AT ROW 1.95 COL 85.8 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 4.8 BY 1
     RowObject.fStatusTxt AT ROW 1.95 COL 91 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 13.6 BY 1
     RowObject.fuLevPostSted AT ROW 10.62 COL 33 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN NATIVE 
          SIZE 32 BY 1
     RowObject.EkstId AT ROW 4.1 COL 12.8 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
     RowObject.VareBehNr AT ROW 4.1 COL 52.2 COLON-ALIGNED
          LABEL "Vareboknr"
          VIEW-AS FILL-IN 
          SIZE 20.2 BY 1
     RECT-2 AT ROW 1 COL 1
     RECT-3 AT ROW 5.71 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY USE-DICT-EXPS 
         SIDE-LABELS NO-UNDERLINE THREE-D NO-AUTO-VALIDATE 
         AT COL 1 ROW 1 SCROLLABLE .


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartDataViewer
   Data Source: "sdo/dordre.w"
   Allow: Basic,DB-Fields,Smart
   Container Links: Data-Target,Update-Source,TableIO-Target,GroupAssign-Source,GroupAssign-Target
   Frames: 1
   Add Fields to: Neither
   Other Settings: PERSISTENT-ONLY COMPILE
   Temp-Tables and Buffers:
      TABLE: RowObject D "?" ?  
      ADDITIONAL-FIELDS:
          {sdo/dordre.i}
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
         HEIGHT             = 18.33
         WIDTH              = 150.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB vTableWin 
/* ************************* Included-Libraries *********************** */

{src/adm2/viewer.i}
{dproclibstart.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW vTableWin
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F-Main
   NOT-VISIBLE FRAME-NAME Size-to-Fit Custom                            */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

/* SETTINGS FOR BUTTON B-SokLevNr IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN RowObject.BekreftetDato IN FRAME F-Main
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR TOGGLE-BOX RowObject.BekreftetOrdre IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN RowObject.EkstId IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN RowObject.fLevNamn IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       RowObject.fLevNamn:READ-ONLY IN FRAME F-Main        = TRUE.

/* SETTINGS FOR TOGGLE-BOX RowObject.fraERP IN FRAME F-Main
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN RowObject.fStatusTxt IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       RowObject.fStatusTxt:READ-ONLY IN FRAME F-Main        = TRUE.

/* SETTINGS FOR FILL-IN RowObject.fuLevPostSted IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       RowObject.fuLevPostSted:READ-ONLY IN FRAME F-Main        = TRUE.

/* SETTINGS FOR TOGGLE-BOX RowObject.Hasteordre IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR TOGGLE-BOX RowObject.HkOrdre IN FRAME F-Main
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR TOGGLE-BOX RowObject.LapTop IN FRAME F-Main
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN RowObject.LevNr IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR EDITOR RowObject.Notat IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN RowObject.OrdreNr IN FRAME F-Main
   NO-ENABLE EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN RowObject.OrdreStatus IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN RowObject.SendtDato IN FRAME F-Main
   NO-ENABLE EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN RowObject.VareBehNr IN FRAME F-Main
   NO-ENABLE EXP-LABEL                                                  */
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
OR F10 OF RowOBject.LevPostNr
DO:
  /* Kaller søkerutine */
  RUN gpost.w (
    INPUT-OUTPUT cTekst, /* Returstreng - chr(1) separert */
    "", /* Feltliste avgrensningsfelt (kommaseparert) */
    "", /* Feltverdier (chr(1) sep) */ 
    RowObject.LevPostNr:SCREEN-VALUE /* Post markøren skal stå på */
    ).
  IF RETURN-VALUE = "AVBRYT" THEN
      RETURN NO-APPLY.
  IF NUM-ENTRIES(cTekst,CHR(1)) >= 3 THEN
  DO:
      /* Legger opp verdier I de aktuelle feltene */
      /* KundeNr,Navn,Adresse1,PostNr,Telefon,MobilTlf,KontE-Post */
      ASSIGN
        RowObject.LevPostNr:SCREEN-VALUE = ENTRY(2,cTekst,CHR(1))
        RowObject.fuLevPostSted:SCREEN-VALUE = ENTRY(3,cTekst,CHR(1))
        RowObject.fuLevPostSted:MODIFIED     = FALSE
        .
      APPLY "VALUE-CHANGED":U TO RowObject.LevPostNr.

      /* Flagger at det er gjort endringer på posten. */
      dynamic-function('assignLinkProperty', 
                       'GroupAssign-Target':U, 
                       'DataModified':U, 'yes':U).
  END.

  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-SendOrdre
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-SendOrdre vTableWin
ON CHOOSE OF B-SendOrdre IN FRAME F-Main /* Send ordre */
DO:
    DEFINE VARIABLE iLevNr      AS INTEGER    NO-UNDO.
    DEFINE VARIABLE iOrdreNr    AS INTEGER    NO-UNDO.
    DEFINE VARIABLE cColValues  AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cBest-Liste AS CHAR       NO-UNDO.
    
    ASSIGN cColValues = DYNAMIC-FUNCTION('colValues':U IN h_dordre,
       INPUT "LevNr,OrdreNr" /* CHARACTER */).
    IF NUM-ENTRIES(cColValues,CHR(1)) <> 3 THEN
        RETURN NO-APPLY.
    ASSIGN iLevNr   = INT(ENTRY(2,cColValues,CHR(1)))
           iOrdreNr = INT(ENTRY(3,cColValues,CHR(1))).
    /*Bygger strengen med bestillinger.*/
    ASSIGN cBest-Liste = DYNAMIC-FUNCTION('getKobledeBest':U IN h_dordre,
                          INPUT iLevNr /* INTEGER */,
                          INPUT iOrdreNr /* INTEGER */).
    /*
    IF cBest-Liste = "" THEN DO:
        MESSAGE "Ingen bestilling er koblet til denne ordre." SKIP
                "Eller alle bestillinger på ordren er allerede sendt."
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        RETURN NO-APPLY.
    END.
    */
    MESSAGE "Skal ordren markeres som 'Sendt til leverandør' ?" SKIP(1)
            "Bestillinger: " cBest-Liste
            VIEW-AS ALERT-BOX BUTTONS YES-NO
            TITLE "Bekreftelse"
            UPDATE lSvar AS LOG.
    IF lSvar = TRUE THEN DO:
        RUN SetSendtStatus IN h_dordre (cBest-Liste) NO-ERROR.
        IF NOT ERROR-STATUS:ERROR THEN DO WITH FRAME {&FRAME-NAME}:
            ASSIGN RowObject.OrdreStatus:SCREEN-VALUE = "2"
                   RowObject.OrdreStatus:MODIFIED     = TRUE
                   RowObject.SendtDato:SCREEN-VALUE   = STRING(TODAY)
                   RowObject.SendtDato:MODIFIED       = TRUE.
            RUN updateRecord.
        END.
    END.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-SokLevNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-SokLevNr vTableWin
ON CHOOSE OF B-SokLevNr IN FRAME F-Main /* ... */
OR F10 OF RowObject.LevNr
DO:
  DEFINE VARIABLE cTekst AS CHARACTER  NO-UNDO.
  ASSIGN
      cTekst = "".

  /* Kaller søkerutine */
  RUN gLevbas.w (
    INPUT-OUTPUT cTekst, /* Returstreng - chr(1) separert */
    "", /* Feltliste avgrensningsfelt (kommaseparert) */
    "", /* Feltverdier (chr(1) sep) */ 
    RowObject.LevNr:SCREEN-VALUE /* Post markøren skal stå på */
    ).
  IF RETURN-VALUE = "AVBRYT" THEN
      RETURN NO-APPLY.
  IF NUM-ENTRIES(cTekst,CHR(1)) = 3 THEN
  DO:
      /* Legger opp verdier I de aktuelle feltene */
      ASSIGN
        RowObject.LevNr:SCREEN-VALUE    = ENTRY(2,cTekst,CHR(1))
        RowObject.LevNr:MODIFIED        = TRUE
        RowObject.fLevNamn:SCREEN-VALUE = ENTRY(3,cTekst,CHR(1))
        RowObject.fLevNamn:MODIFIED     = FALSE.
        .

        /* Flagger at det er gjort endringer på recorden og trigger toolbar. */
        APPLY "TAB":U TO RowObject.LevNr.
        RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RowObject.BekreftetOrdre
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RowObject.BekreftetOrdre vTableWin
ON VALUE-CHANGED OF RowObject.BekreftetOrdre IN FRAME F-Main
DO:
    IF INPUT RowObject.BekreftetOrdre = TRUE THEN
        ASSIGN
        BekreftetDato:SCREEN-VALUE = STRING(TODAY).
    ELSE
        ASSIGN
        BekreftetDato:SCREEN-VALUE = ?.
      .
    ASSIGN
        RowObject.BekreftetDato:MODIFIED = TRUE.
    DYNAMIC-FUNCTION('setDataModified':U,
     INPUT true /* LOGICAL */).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RowObject.LevNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RowObject.LevNr vTableWin
ON TAB OF RowObject.LevNr IN FRAME F-Main /* Leverandør */
DO:
  ASSIGN RowObject.fLevNamn:SCREEN-VALUE = DYNAMIC-FUNCTION('getLevNavn2':U IN h_dordre,
                    INPUT INT(RowObject.LevNr:SCREEN-VALUE) /* INTEGER */)
         RowObject.fLevNamn:MODIFIED = FALSE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RowObject.LevPostNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RowObject.LevPostNr vTableWin
ON TAB OF RowObject.LevPostNr IN FRAME F-Main /* PostNr */
OR "RETURN":U OF RowObject.LevPostNr
DO:
    RUN PostSted IN 
             DYNAMIC-FUNCTION('getDataSource':U) 
             (INPUT INPUT RowObject.LevPostNr).
    ASSIGN
        cTekst = RETURN-VALUE
        .
    IF cTekst = "" THEN
    DO:
        MESSAGE "Ugyldig postnummer."
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        RETURN NO-APPLY.
    END.
    ELSE DO:
        /* Legger opp verdier I de aktuelle feltene */
        /* KundeNr,Navn,Adresse1,PostNr,Telefon,MobilTlf,KontE-Post */
        ASSIGN
          RowOBject.fuLevPostSted:SCREEN-VALUE = cTekst
          RowObject.fuLevPostSted:MODIFIED     = FALSE
          .

        /* Flagger at det er gjort endringer på posten. */
        dynamic-function('assignLinkProperty', 
                         'GroupAssign-Target':U, 
                         'DataModified':U, 'yes':U).
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
  if valid-handle(h_dproclib) then
    run SjekkLApTop in h_dproclib (output wLapTop).

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
      IF dynamic-function('linkHandles':U,'TableIO-Source':U) <> "" THEN
          ASSIGN RowObject.LapTop:CHECKED  = wLapTop
                 RowObject.Laptop:MODIFIED = TRUE
                 RowObject.LevNr:SENSITIVE = TRUE
                 B-SokLevNr:SENSITIVE      = TRUE
                 B-SendOrdre:SENSITIVE     = TRUE /* FALSE */. 
/*       APPLY "ENTRY" TO RowObject.Merknad. */
/*       ELSE                                                      */
/*           /* vordre blir brukt vid nyreg från d-nyordre.w   */  */
/*           /* den kör vi utan toolbar och addrecord anropas  */  */
/*           /* genom att d-nyordre kör NyOrdre i denna rutinen */ */
/*           ASSIGN RowObject.LapTop:CHECKED  = wLapTop            */
/*                  RowObject.LevNr:MODIFIED  = TRUE               */
/*                  B-SendOrdre:SENSITIVE     = FALSE.             */

      if available clButiker THEN DO:
          assign
            RowObject.LevAdresse1:screen-value  = clButiker.LevAdresse1
            RowObject.LevAdresse2:screen-value  = clButiker.LevAdresse2
            RowObject.LevKontakt:screen-value   = clButiker.LevKontakt
            RowObject.LevMerknad:screen-value   = clButiker.LevMerknad
            RowObject.LevPostBoks:screen-value  = clButiker.LevPostBoks
            RowObject.LevPostNr:screen-value    = clButiker.LevPostNr
            RowObject.LevTelefon:screen-value   = clButiker.LevTelefon
            .
          APPLY "TAB" TO RowObject.LevPostNr.
      END.
      APPLY "ENTRY" TO RowObject.LevNr.
  END.
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
      ASSIGN RowObject.LevNr:SENSITIVE = FALSE
             B-SokLevNr:SENSITIVE      = FALSE. 
  END.

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
  IF VALID-HANDLE(h_dordre) THEN DO:
      ASSIGN B-SendOrdre:SENSITIVE IN FRAME {&FRAME-NAME} = TRUE /*
                                 RowObject.OrdreStatus:SCREEN-VALUE = "1" */.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE deleteRecord vTableWin 
PROCEDURE deleteRecord :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  MESSAGE "Skall orderen slettes?"
      VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE lCancel AS LOGICAL.
  IF lCancel = FALSE THEN
      RETURN.
  RUN SUPER.

  /* Code placed here will execute AFTER standard behavior.    */

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DisalbSend vTableWin 
PROCEDURE DisalbSend :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 DO WITH FRAME {&FRAME-NAME}:
     ASSIGN
         B-SendOrdre:HIDDEN = TRUE.
 END.
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
  /* Henter sentrallager. */
  {syspara.i 5 1 1 iCl "INT"}
  FIND clButiker NO-LOCK WHERE
      clButiker.Butik = iCl.

  ASSIGN 
      h_dordre = DYNAMIC-FUNCTION('getDataSource':U)
      .
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE NyOrdre vTableWin 
PROCEDURE NyOrdre :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER iLevNr AS INTEGER  NO-UNDO.
    ASSIGN RowObject.LevNr:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(iLevNr)
           RowObject.LevNr:MODIFIED  = TRUE
           RowObject.LevNr:SENSITIVE = FALSE
           B-SokLevNr:SENSITIVE      = FALSE.
    APPLY "TAB" TO RowObject.LevNr.
    APPLY "ENTRY" TO RowObject.Merknad.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SetLevadresse vTableWin 
PROCEDURE SetLevadresse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER rButikk AS ROWID      NO-UNDO.
    FIND Butiker WHERE ROWID(Butiker) = rButikk NO-LOCK NO-ERROR.
    DO WITH FRAME {&FRAME-NAME}:
        ASSIGN RowObject.LevAdresse1:SCREEN-VALUE = Butiker.LevAdresse1
               RowObject.LevAdresse2:SCREEN-VALUE = Butiker.LevAdresse2
               RowObject.LevPostBoks:SCREEN-VALUE = Butiker.LevPostBoks
               RowObject.LevPostNr:SCREEN-VALUE   = Butiker.LevPostNr
               RowObject.LevTelefon:SCREEN-VALUE  = Butiker.LevTelefon
               RowObject.LevKontakt:SCREEN-VALUE  = Butiker.LevKontakt
               RowObject.LevMerknad:SCREEN-VALUE  = Butiker.LevMerknad.
        APPLY "TAB" TO RowObject.LevPostNr.
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
  ASSIGN RowObject.LevNr:MODIFIED IN FRAME {&FRAME-NAME} = TRUE.

  RUN SUPER.

  /* Code placed here will execute AFTER standard behavior.    */
  IF RETURN-VALUE = "ADM-ERROR" THEN
      RETURN.
  DO WITH FRAME {&FRAME-NAME}:
      ASSIGN RowObject.LevNr:SENSITIVE = FALSE
             B-SokLevNr:SENSITIVE      = FALSE. 
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

