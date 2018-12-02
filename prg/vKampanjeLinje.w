&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
/* Connected Databases 
          skotex           PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW


/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE RowObject
       {"sdo/dkampanjelinje.i"}.



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
  DEFINE VARIABLE hContainerSource AS HANDLE     NO-UNDO.
  DEFINE VARIABLE hDataSource      AS HANDLE     NO-UNDO.
  DEFINE VARIABLE hNavigation      AS HANDLE     NO-UNDO.
  DEFINE VARIABLE lNyModus         AS LOGICAL    NO-UNDO.

  DEFINE VARIABLE cSkomodus AS CHARACTER  NO-UNDO.

  DEFINE VARIABLE iCL              AS INT                  NO-UNDO.
  DEFINE VARIABLE iProfilNr        AS INT FORMAT ">>>>>>9" NO-UNDO.

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
&Scoped-define DATA-FIELD-DEFS "sdo/dkampanjelinje.i"

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME F-Main

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS RowObject.Pris2 RowObject.VareKost 
&Scoped-define ENABLED-TABLES RowObject
&Scoped-define FIRST-ENABLED-TABLE RowObject
&Scoped-Define DISPLAYED-FIELDS RowObject.KampanjeId RowObject.Behandlet ~
RowObject.Beskr RowObject.LevKod RowObject.NormalPris RowObject.LevFargKod ~
RowObject.Pris2 RowObject.Vg RowObject.VareKost RowObject.LopNr 
&Scoped-define DISPLAYED-TABLES RowObject
&Scoped-define FIRST-DISPLAYED-TABLE RowObject
&Scoped-Define DISPLAYED-OBJECTS FI-Kamp% 

/* Custom List Definitions                                              */
/* ADM-ASSIGN-FIELDS,List-2,List-3,List-4,List-5,List-6                 */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE VARIABLE FI-Kamp% AS DECIMAL FORMAT "->9.9":U INITIAL 0 
     LABEL "%Endring" 
     VIEW-AS FILL-IN 
     SIZE 20.2 BY 1 NO-UNDO.

DEFINE VARIABLE TOGGLE-Tilbud AS LOGICAL INITIAL yes 
     LABEL "Tilbud" 
     VIEW-AS TOGGLE-BOX
     SIZE 18 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     RowObject.KampanjeId AT ROW 1 COL 13.8 COLON-ALIGNED
          LABEL "Endringsnr"
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     RowObject.Behandlet AT ROW 1.1 COL 58
          LABEL "Behandlet"
          VIEW-AS TOGGLE-BOX
          SIZE 18 BY .81
     TOGGLE-Tilbud AT ROW 1.14 COL 34
     RowObject.Beskr AT ROW 2.05 COL 14 COLON-ALIGNED
          LABEL "Varetekst"
          VIEW-AS FILL-IN 
          SIZE 63 BY 1
     RowObject.LevKod AT ROW 3.14 COL 14 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 27 BY 1
     RowObject.NormalPris AT ROW 3.14 COL 56.8 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 20.2 BY 1
     RowObject.LevFargKod AT ROW 4.24 COL 14 COLON-ALIGNED
          LABEL "Lev.fargekode" FORMAT "x(20)"
          VIEW-AS FILL-IN 
          SIZE 27 BY 1
     RowObject.Pris2 AT ROW 4.24 COL 56.8 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 20.2 BY 1
     RowObject.Vg AT ROW 5.29 COL 14 COLON-ALIGNED
          LABEL "Varegruppe"
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     RowObject.VareKost AT ROW 5.33 COL 56.8 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 20.2 BY 1
     RowObject.LopNr AT ROW 6.38 COL 14 COLON-ALIGNED
          LABEL "Løpenr"
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     FI-Kamp% AT ROW 6.48 COL 56.8 COLON-ALIGNED
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY USE-DICT-EXPS 
         SIDE-LABELS NO-UNDERLINE THREE-D NO-AUTO-VALIDATE 
         AT COL 1 ROW 1 SCROLLABLE .


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartDataViewer
   Data Source: "sdo/dkampanjelinje.w"
   Allow: Basic,DB-Fields,Smart
   Container Links: Data-Target,Update-Source,TableIO-Target,GroupAssign-Source,GroupAssign-Target
   Frames: 1
   Add Fields to: Neither
   Other Settings: PERSISTENT-ONLY COMPILE
   Temp-Tables and Buffers:
      TABLE: RowObject D "?" ?  
      ADDITIONAL-FIELDS:
          {sdo/dkampanjelinje.i}
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
         HEIGHT             = 6.62
         WIDTH              = 78.6.
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

/* SETTINGS FOR TOGGLE-BOX RowObject.Behandlet IN FRAME F-Main
   NO-ENABLE EXP-LABEL                                                  */
ASSIGN 
       RowObject.Behandlet:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN RowObject.Beskr IN FRAME F-Main
   NO-ENABLE EXP-LABEL                                                  */
ASSIGN 
       RowObject.Beskr:READ-ONLY IN FRAME F-Main        = TRUE.

/* SETTINGS FOR FILL-IN FI-Kamp% IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN RowObject.KampanjeId IN FRAME F-Main
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN RowObject.LevFargKod IN FRAME F-Main
   NO-ENABLE EXP-LABEL EXP-FORMAT EXP-HELP                              */
ASSIGN 
       RowObject.LevFargKod:READ-ONLY IN FRAME F-Main        = TRUE.

/* SETTINGS FOR FILL-IN RowObject.LevKod IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       RowObject.LevKod:READ-ONLY IN FRAME F-Main        = TRUE.

/* SETTINGS FOR FILL-IN RowObject.LopNr IN FRAME F-Main
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN RowObject.NormalPris IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       RowObject.NormalPris:READ-ONLY IN FRAME F-Main        = TRUE.

/* SETTINGS FOR TOGGLE-BOX TOGGLE-Tilbud IN FRAME F-Main
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       TOGGLE-Tilbud:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN RowObject.Vg IN FRAME F-Main
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

&Scoped-define SELF-NAME RowObject.LopNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RowObject.LopNr vTableWin
ON RETURN OF RowObject.LopNr IN FRAME F-Main /* Løpenr */
DO:
    APPLY "TAB" TO SELF.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RowObject.LopNr vTableWin
ON TAB OF RowObject.LopNr IN FRAME F-Main /* Løpenr */
DO:
    DEF VAR dArtikkelNr LIKE ArtBas.ArtikkelNr NO-UNDO.
    DEF VAR cMsgStr     AS CHARACTER           NO-UNDO.
    DEF VAR dPrisStr     AS CHARACTER          NO-UNDO.
    IF VALID-HANDLE(hDataSource) THEN DO:
        IF DYNAMIC-FUNCTION('getRegistrert':U IN hDataSource,
           INPUT INT(RowObject.KampanjeId:SCREEN-VALUE),
           INPUT INPUT RowObject.Vg, INPUT INPUT RowObject.LopNr) THEN
            ASSIGN cMsgStr = "Allerede registrert".
        IF cMsgStr = "" THEN DO:
            ASSIGN dArtikkelNr = DYNAMIC-FUNCTION('getArtikkelNr':U IN hDataSource,
               INPUT INPUT RowObject.Vg, INPUT INPUT RowObject.LopNr).
            IF dArtikkelNr = ? THEN
                ASSIGN cMsgStr = "Artikkel finnes ikke!".
            ELSE DO:
                ASSIGN dPrisStr = DYNAMIC-FUNCTION('getArtPris':U IN hDataSource,
                    INPUT dArtikkelNr).
                IF ENTRY(1,dPrisStr,";") = "?" THEN
                    ASSIGN cMsgStr =  "Artikkelpris finnes ikke!".
            END.
        END.
        IF cMsgStr <> "" THEN DO:
            MESSAGE cMsgStr
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
            RUN cancelRecord.
            RUN addRecord.
            RETURN NO-APPLY.
        END.
        ELSE DO:
                ASSIGN RowObject.Pris2:SCREEN-VALUE = ENTRY(1,dPrisStr,";")
                       TOGGLE-Tilbud:CHECKED = ENTRY(2,dPrisStr,";") = "J"
                       TOGGLE-Tilbud:HIDDEN = ENTRY(2,dPrisStr,";") = "N".
        END.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RowObject.Pris2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RowObject.Pris2 vTableWin
ON ALT-L OF RowObject.Pris2 IN FRAME F-Main /* Pris */
DO:
  IF SELF:MODIFIED THEN
      RUN updateRecord.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RowObject.Pris2 vTableWin
ON LEAVE OF RowObject.Pris2 IN FRAME F-Main /* Pris */
DO:
  IF DECI(SELF:SCREEN-VALUE) = 0 THEN
      BELL.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RowObject.VareKost
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RowObject.VareKost vTableWin
ON ALT-L OF RowObject.VareKost IN FRAME F-Main /* Varekost */
DO:
    IF SELF:MODIFIED THEN
        RUN updateRecord.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RowObject.VareKost vTableWin
ON RETURN OF RowObject.VareKost IN FRAME F-Main /* Varekost */
OR 'TAB' OF RowObject.Varekost
DO:
    RUN updateRecord.
    IF RowObject.Vg:SENSITIVE THEN
        APPLY "ENTRY" TO RowObject.Vg.
/*     RUN addRecord. */
    RETURN NO-APPLY.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RowObject.Vg
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RowObject.Vg vTableWin
ON RETURN OF RowObject.Vg IN FRAME F-Main /* Varegruppe */
DO:
  APPLY "TAB" TO SELF.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RowObject.Vg vTableWin
ON TAB OF RowObject.Vg IN FRAME F-Main /* Varegruppe */
DO:
    RUN kontrollerVG (INT(RowObject.Vg:SCREEN-VALUE)).
    IF RETURN-VALUE <> "OK" THEN DO:
          MESSAGE RETURN-VALUE
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        RETURN NO-APPLY.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK vTableWin 


/* ***************************  Main Block  *************************** */
/*   ON F2,ALT-S OF FRAME F-Main ANYWHERE DO: */
/*     IF NOT B-SokArtbas:SENSITIVE THEN      */
/*       RETURN NO-APPLY.                     */
/*     ELSE                                   */
/*        APPLY "CHOOSE" TO B-SokArtbas.      */
/*   END.                                     */

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
  DO WITH FRAME {&FRAME-NAME}:
/*       ASSIGN RowObject.Vg:SENSITIVE    = TRUE  */
/*              RowObject.Lopnr:SENSITIVE = TRUE  */
/*              B-SokArtBas:SENSITIVE     = TRUE. */
/*       APPLY "CHOOSE" TO B-SokArtbas. */
  END.
  RUN SUPER.
  /* Code placed here will execute AFTER standard behavior.    */
  ASSIGN lNyModus = TRUE.
  RUN NyPost.
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
/*       DYNAMIC-FUNCTION('setDataModified':U, */
/*          INPUT FALSE /* LOGICAL */).        */
  DO WITH FRAME {&FRAME-NAME}:
      ASSIGN RowObject.Vg:SENSITIVE    = FALSE
             RowObject.Lopnr:SENSITIVE = FALSE
             TOGGLE-Tilbud:HIDDEN      = TRUE
/*              B-SokArtBas:SENSITIVE     = FALSE */
             lNyModus                  = FALSE.
/*       IF NUM-ENTRIES(PROGRAM-NAME(2),"/") > 1 THEN                */
/*       DO:                                                         */
/*           IF ENTRY(2,PROGRAM-NAME(2),"/") BEGINS "toolbar" THEN   */
/*               DYNAMIC-FUNCTION('disableActions':U IN hNavigation, */
/*                   INPUT "Copy" /* CHARACTER */).                  */
/*       END.                                                        */
  END.
  RowObject.Pris2:SET-SELECTION(1,1).
  RUN SetBrowseFocus IN hContainerSource.

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

  RUN SUPER( INPUT-OUTPUT plCancel).
  
  /* Code placed here will execute AFTER standard behavior.    */

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
  IF RowObject.Behandlet:CHECKED IN FRAME {&FRAME-NAME} THEN
      RUN disableObject.
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

  RUN SUPER.

  /* Code placed here will execute AFTER standard behavior.    */
  IF DYNAMIC-FUNCTION('getRecordState':U) = "NoRecordAvailable" THEN DO:
      DYNAMIC-FUNCTION('openQuery':U IN hDataSource).
       RUN disableObject.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE displayFields vTableWin 
PROCEDURE displayFields :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

  DEFINE INPUT PARAMETER pcColValues AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cAktivertKomplett  AS CHARACTER  NO-UNDO.
  /* Code placed here will execute PRIOR to standard behavior. */

  RUN SUPER( INPUT pcColValues).

  /* Code placed here will execute AFTER standard behavior.    */

  IF VALID-HANDLE(hContainerSource) THEN
      ASSIGN cAktivertKomplett = DYNAMIC-FUNCTION('getHodeStatus':U IN hContainerSource).
  
  IF VALID-HANDLE(hNavigation) THEN DO:
      IF NUM-ENTRIES(cAktivertKomplett,CHR(1)) = 3 THEN DO: 
          IF ENTRY(3,cAktivertKomplett,CHR(1)) = "yes" THEN DO:
              DYNAMIC-FUNCTION('disableActions':U IN hNavigation,
                  INPUT "Add,Delete,Save" /* CHARACTER */).
              RUN disableObject.
          END.
          ELSE IF ENTRY(2,cAktivertKomplett,CHR(1)) = "yes" THEN DO:
              DYNAMIC-FUNCTION('disableActions':U IN hNavigation,
                  INPUT "Add" /* CHARACTER */).
              IF RowObject.Behandlet:CHECKED IN FRAME {&FRAME-NAME} = TRUE THEN
                  DYNAMIC-FUNCTION('disableActions':U IN hNavigation,
                      INPUT "Add,Delete" /* CHARACTER */).
          END.
      END.
  END.
  
  /* NB: Dette virker ikke. Ingen RowObject til gjengelig. */
  IF AVAILABLE RowObject THEN
  DO WITH FRAME F-Main:
      FIND ArtBas NO-LOCK WHERE
          ArtBas.ArtikkelNr = RowObject.ArtikkelNr NO-ERROR.
      IF AVAILABLE ArtBas THEN
          RowObject.LevKod:SCREEN-VALUE = ArtBas.LevKod.
      ELSE
          RowObject.LevKod:SCREEN-VALUE = ''.
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
 {syspara.i 5 1 1 iCL INT}
 FIND clButiker NO-LOCK WHERE
     clButiker.Butik = iCL NO-ERROR.
 IF NOT AVAILABLE clButiker THEN
     MESSAGE 'Det er ikke satt opp gyldig sentrallager.'
         VIEW-AS ALERT-BOX INFO BUTTONS OK.
 
 {syspara.i 1 1 54 cSkomodus}
     IF cSkomodus = "1" THEN DO WITH FRAME {&FRAME-NAME}:
         ASSIGN RowObject.Vg:HIDDEN = FALSE
                RowObject.LopNr:HIDDEN = FALSE.
     END.
  RUN SUPER.

  /* Code placed here will execute AFTER standard behavior.    */
  ASSIGN hDataSource      = DYNAMIC-FUNCTION('getDataSource':U)
         hNavigation      = DYNAMIC-FUNCTION('getTableIOSource':U).
  IF VALID-HANDLE(hDataSource) THEN
      RUN refreshRow IN hDataSource.

  PUBLISH 'hentProfilNr' (OUTPUT iProfilNr).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE KampProcent vTableWin 
PROCEDURE KampProcent :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipKamp% AS DECIMAL    NO-UNDO.
    DO WITH FRAME {&FRAME-NAME}:
        ASSIGN FI-Kamp% = ipKamp%
               FI-Kamp%:SCREEN-VALUE = STRING(FI-Kamp%).
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE kontrollerVG vTableWin 
PROCEDURE kontrollerVG :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER ipVg LIKE ArtBas.Vg NO-UNDO.
  RETURN IF NOT DYNAMIC-FUNCTION('VarGrFinnes':U IN hDataSource,
          INPUT ipVg) THEN
          "Feil varegruppe" ELSE "OK".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE NyPost vTableWin 
PROCEDURE NyPost :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR wArtikkelNr AS   DEC NO-UNDO.
  DEF VAR dArtikkelNr LIKE ArtBas.ArtikkelNr NO-UNDO.
  DEF VAR cMsgStr     AS CHARACTER           NO-UNDO.
  DEF VAR dPrisStr     AS CHARACTER          NO-UNDO.
  DEF VAR dVarekostSTr AS CHAR               NO-UNDO.
  DEFINE VARIABLE dDeci AS DECIMAL    NO-UNDO.

  DEF BUFFER bufArtPris FOR ArtPris.

  PUBLISH 'hentProfilNr' (OUTPUT iProfilNr).

  DO WITH FRAME {&FRAME-NAME}:
      FIND KampanjeHode NO-LOCK WHERE
          KampanjeHode.KampanjeId = int(RowObject.KampanjeId:SCREEN-VALUE) NO-ERROR.
      /*
      IF AVAILABLE KampanjeHode THEN
          RowObject.Varekost:SENSITIVE = NOT KampanjeHode.LeverandorKampanje.
      ELSE
          RowObject.Varekost:SENSITIVE = TRUE.
      */
      RUN d-hsok.w (OUTPUT wArtikkelNr,"").
      IF wArtikkelNr = ? THEN DO:
  /*         APPLY "ENTRY" TO RowObject.Vg. */
          RUN cancelRecord.
          RETURN.
      END.
      ELSE DO:
          FIND ArtBas WHERE 
              ArtBas.ArtikkelNr = wArtikkelNr NO-LOCK NO-ERROR.
          FIND ArtPris OF ArtBas NO-LOCK WHERE
              ArtPris.ProfilNr = iProfilNr NO-ERROR.
          IF NOT AVAILABLE ArtPris THEN
          DO:
              FIND ArtPris OF ArtBas NO-LOCK WHERE
                  ArtPris.ProfilNr = clButiker.ProfilNr NO-ERROR.
              IF AVAILABLE ArtPris THEN
                  BUFFER-COPY ArtPris 
                    EXCEPT ProfilNr
                    TO bufArtPris
                    ASSIGN
                      bufArtPris.ProfilNr = iProfilNr.
          END.

          IF Artbas.sanertdato <> ? THEN DO:
              MESSAGE "Artikkel er sanert!"
                  VIEW-AS ALERT-BOX INFO BUTTONS OK.
              RUN cancelRecord.
              RETURN NO-APPLY.
          END.
          IF AVAIL ArtBas AND ArtBas.Opris = TRUE THEN DO:
              MESSAGE "PLU-artikkel!"
                  VIEW-AS ALERT-BOX INFO BUTTONS OK.
  /*             APPLY "ENTRY" TO RowObject.Vg. */
              RUN cancelRecord.
              RETURN NO-APPLY.
          END.
          IF AVAIL ArtBas AND ArtBas.LopNr = ? THEN DO:
              MESSAGE "Artikkel mangler løpenummer!"
                  VIEW-AS ALERT-BOX INFO BUTTONS OK.
  /*             APPLY "ENTRY" TO RowObject.Vg. */
              RUN cancelRecord.
              RETURN NO-APPLY.
          END.
          ELSE IF AVAIL ArtBas THEN DO:
              ASSIGN RowObject.Vg:SCREEN-VALUE = STRING(ArtBas.Vg)
                     RowObject.LopNr:SCREEN-VALUE = STRING(ArtBas.LopNr)
                     RowObject.Beskr:SCREEN-VALUE = ArtBas.Beskr
                     RowObject.Beskr:MODIFIED = FALSE
                     RowObject.LevKod:SCREEN-VALUE = ArtBas.LevKod
                     RowObject.LevKod:MODIFIED = FALSE
                     RowObject.Vg:MODIFIED = TRUE
                     RowObject.LopNr:MODIFIED = TRUE.
              IF DYNAMIC-FUNCTION('getRegistrert':U IN hDataSource,
                 INPUT INT(RowObject.KampanjeId:SCREEN-VALUE),
                 INPUT INPUT RowObject.Vg, INPUT INPUT RowObject.LopNr) THEN DO:
                  ASSIGN cMsgStr = "Allerede registrert".
              END.
              ELSE DO:
                  ASSIGN dPrisStr = DYNAMIC-FUNCTION('getArtPris':U IN hDataSource,
                      INPUT ArtBas.ArtikkelNr).
                  IF ENTRY(1,dPrisStr,";") = "?" THEN
                      ASSIGN cMsgStr =  "Artikkelpris finnes ikke!".
                  ASSIGN dVarekostSTr = entry(1,DYNAMIC-FUNCTION('getVarekost':U IN hDataSource,
                      INPUT ArtBas.ArtikkelNr),';').    
              END.
              IF cMsgStr <> "" THEN DO:
                  MESSAGE cMsgStr
                      VIEW-AS ALERT-BOX INFO BUTTONS OK.
                  RUN cancelRecord.
                  RUN addRecord.
              END.
              ELSE DO:
                  IF ABS(FI-Kamp%) <> 0 THEN
                  DO:
                      ASSIGN dDeci = ROUND(DECI(ENTRY(1,dPrisStr,";")) * (1 - ((FI-Kamp% * -1) / 100)),1).
                      IF dDeci > 50 THEN
                          ASSIGN dDeci = TRUNC(dDeci,0).
                      ELSE
                         ASSIGN dDeci = IF dDeci - TRUNC(dDeci,0) > 0.5 THEN TRUNC(dDeci,0) + 0.5 ELSE TRUNC(dDeci,0).
                  END.
                  ELSE 
                      ASSIGN dDeci = DECI(ENTRY(1,dPrisStr,";")).
                  ASSIGN 
/*                          RowObject.Vg:SENSITIVE = FALSE    */
/*                          RowObject.Lopnr:SENSITIVE = FALSE */
                         RowObject.NormalPris:SCREEN-VALUE = ENTRY(1,dPrisStr,";")
                         RowObject.NormalPris:MODIFIED     = FALSE
                         RowObject.Pris2:SCREEN-VALUE      = STRING(dDeci)
                         RowObject.Varekost:SCREEN-VALUE   = STRING(dVarekostSTr)
                         TOGGLE-Tilbud:CHECKED             = ENTRY(2,dPrisStr,";") = "J"
                         TOGGLE-Tilbud:HIDDEN              = ENTRY(2,dPrisStr,";") = "N".
                  APPLY "ENTRY" TO RowObject.Pris2.
                  RUN VisBildeKalkyle IN hContainerSource (Artbas.ArtikkelNr,Artbas.Bildnr).
              END.
          END.
      END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setContainerHandle vTableWin 
PROCEDURE setContainerHandle :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER ipContainerHandle AS HANDLE     NO-UNDO.
ASSIGN hContainerSource = ipContainerHandle.
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
  DO WITH FRAME {&FRAME-NAME}:
/*       IF INPUT EowObject.Pris2 = 0 THEN DO:      */
/*           MESSAGE "Registrer pris!"              */
/*               VIEW-AS ALERT-BOX INFO BUTTONS OK. */
/*           APPLY "TAB" TO RowObject.LopNr.        */
/*           RETURN.                                */
/*       END.                                       */
      IF DECI(RowObject.Pris2:SCREEN-VALUE) = 0 THEN DO:
          MESSAGE "Skal artikkelen registreres med pris 0,00?" VIEW-AS
              ALERT-BOX QUESTION BUTTONS YES-NO UPDATE lchoice AS LOGICAL.
          IF NOT lchoice THEN
              RETURN.
      END.
      /*RowObject.Varekost:SENSITIVE = TRUE.*/
  END.
  
  RUN SUPER.
  /* Code placed here will execute AFTER standard behavior.    */
  IF RETURN-VALUE = "" THEN DO:
      ASSIGN TOGGLE-Tilbud:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
/*       IF RowObject.Vg:SENSITIVE IN FRAME {&FRAME-NAME} THEN */
      IF lNyModus = TRUE THEN
         RUN addRecord.
/*       ELSE                                                        */
/*           IF VALID-HANDLE(hNavigation) THEN                       */
/*               DYNAMIC-FUNCTION('disableActions':U IN hNavigation, */
/*                   INPUT "Copy" /* CHARACTER */).                  */
  END.
  ELSE DO:
      RUN cancelRecord.
      RUN addRecord.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

