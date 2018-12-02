&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
/* Connected Databases 
          data             PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
{adecomm/appserv.i}
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS dTables 
/*------------------------------------------------------------------------

  File:  

  Description: from DATA.W - Template For SmartData objects in the ADM

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Modified:     February 24, 1999
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

&glob DATA-LOGIC-PROCEDURE .p

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartDataObject
&Scoped-define DB-AWARE yes

&Scoped-define ADM-SUPPORTED-LINKS Data-Source,Data-Target,Navigation-Target,Update-Target,Commit-Target,Filter-Target


/* Db-Required definitions. */
&IF DEFINED(DB-REQUIRED) = 0 &THEN
    &GLOBAL-DEFINE DB-REQUIRED TRUE
&ENDIF
&GLOBAL-DEFINE DB-REQUIRED-START   &IF {&DB-REQUIRED} &THEN
&GLOBAL-DEFINE DB-REQUIRED-END     &ENDIF


&Scoped-define QUERY-NAME Query-Main

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES priskontroll

/* Definitions for QUERY Query-Main                                     */
&Scoped-Define ENABLED-FIELDS  Antall ArtikkelNr BongTekst BongPris ButikkNr b_id Dato Feilkode GruppeNr~
 KasseNr KasseVarekost SEPris SEVarekost Strekkode
&Scoped-define ENABLED-FIELDS-IN-priskontroll Antall ArtikkelNr BongTekst ~
BongPris ButikkNr b_id Dato Feilkode GruppeNr KasseNr KasseVarekost SEPris ~
SEVarekost Strekkode 
&Scoped-Define DATA-FIELDS  Antall ArtikkelNr BongTekst BongPris ButikkNr b_id Dato Feilkode GruppeNr~
 KasseNr KasseVarekost SEPris SEVarekost Strekkode
&Scoped-define DATA-FIELDS-IN-priskontroll Antall ArtikkelNr BongTekst ~
BongPris ButikkNr b_id Dato Feilkode GruppeNr KasseNr KasseVarekost SEPris ~
SEVarekost Strekkode 
&Scoped-Define MANDATORY-FIELDS  ButikkNr GruppeNr KasseNr
&Scoped-Define APPLICATION-SERVICE 
&Scoped-Define ASSIGN-LIST 
&Scoped-Define DATA-FIELD-DEFS "dpriskontroll.i"
&Scoped-define QUERY-STRING-Query-Main FOR EACH priskontroll NO-LOCK INDEXED-REPOSITION
{&DB-REQUIRED-START}
&Scoped-define OPEN-QUERY-Query-Main OPEN QUERY Query-Main FOR EACH priskontroll NO-LOCK INDEXED-REPOSITION.
{&DB-REQUIRED-END}
&Scoped-define TABLES-IN-QUERY-Query-Main priskontroll
&Scoped-define FIRST-TABLE-IN-QUERY-Query-Main priskontroll


/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

{&DB-REQUIRED-START}

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Query-Main FOR 
      priskontroll SCROLLING.
&ANALYZE-RESUME
{&DB-REQUIRED-END}


/* ************************  Frame Definitions  *********************** */


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartDataObject
   Allow: Query
   Frames: 0
   Add Fields to: Neither
   Other Settings: PERSISTENT-ONLY COMPILE APPSERVER DB-AWARE
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
  CREATE WINDOW dTables ASSIGN
         HEIGHT             = 1.62
         WIDTH              = 46.6.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB dTables 
/* ************************* Included-Libraries *********************** */

{src/adm2/data.i}
{sdo\soksdo.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW dTables
  VISIBLE,,RUN-PERSISTENT                                               */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK QUERY Query-Main
/* Query rebuild information for SmartDataObject Query-Main
     _TblList          = "data.priskontroll"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > data.priskontroll.Antall
"Antall" "Antall" ? ? "decimal" ? ? ? ? ? ? yes ? no 12.6 yes ""
     _FldNameList[2]   > data.priskontroll.ArtikkelNr
"ArtikkelNr" "ArtikkelNr" ? ? "decimal" ? ? ? ? ? ? yes ? no 15.6 yes ""
     _FldNameList[3]   > data.priskontroll.BongTekst
"BongTekst" "BongTekst" ? ? "character" ? ? ? ? ? ? yes ? no 10.4 yes ""
     _FldNameList[4]   > data.priskontroll.BongPris
"BongPris" "BongPris" ? ? "decimal" ? ? ? ? ? ? yes ? no 14.4 yes ""
     _FldNameList[5]   > data.priskontroll.ButikkNr
"ButikkNr" "ButikkNr" ? ? "integer" ? ? ? ? ? ? yes ? yes 7.2 yes ""
     _FldNameList[6]   > data.priskontroll.b_id
"b_id" "b_id" ? ? "decimal" ? ? ? ? ? ? yes ? no 14.4 yes ""
     _FldNameList[7]   > data.priskontroll.Dato
"Dato" "Dato" ? ? "date" ? ? ? ? ? ? yes ? no 9.2 yes ""
     _FldNameList[8]   > data.priskontroll.Feilkode
"Feilkode" "Feilkode" ? ? "integer" ? ? ? ? ? ? yes ? no 8 yes ""
     _FldNameList[9]   > data.priskontroll.GruppeNr
"GruppeNr" "GruppeNr" ? ? "integer" ? ? ? ? ? ? yes ? yes 4.4 yes ""
     _FldNameList[10]   > data.priskontroll.KasseNr
"KasseNr" "KasseNr" ? ? "integer" ? ? ? ? ? ? yes ? yes 8 yes ""
     _FldNameList[11]   > data.priskontroll.KasseVarekost
"KasseVarekost" "KasseVarekost" ? ? "decimal" ? ? ? ? ? ? yes ? no 14.6 yes ""
     _FldNameList[12]   > data.priskontroll.SEPris
"SEPris" "SEPris" ? ? "decimal" ? ? ? ? ? ? yes ? no 14.4 yes ""
     _FldNameList[13]   > data.priskontroll.SEVarekost
"SEVarekost" "SEVarekost" ? ? "decimal" ? ? ? ? ? ? yes ? no 13.2 yes ""
     _FldNameList[14]   > data.priskontroll.Strekkode
"Strekkode" "Strekkode" ? ? "character" ? ? ? ? ? ? yes ? no 20 yes ""
     _Design-Parent    is WINDOW dTables @ ( 1.14 , 2.6 )
*/  /* QUERY Query-Main */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK dTables 


/* ***************************  Main Block  *************************** */

  &IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
    RUN initializeObject.
  &ENDIF

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI dTables  _DEFAULT-DISABLE
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
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

