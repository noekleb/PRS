&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
/* Connected Databases 
          skotex           PROGRESS
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
&Scoped-define INTERNAL-TABLES KassererKontanter

/* Definitions for QUERY Query-Main                                     */
&Scoped-Define ENABLED-FIELDS  ButikkNr Dato KassererNr z_nummer Belop1 Belop2 Belop3 Belop4 Belop5 Belop6~
 Belop7 Belop8 Belop9 Belop10 AntallValor1 AntallValor2 AntallValor3~
 AntallValor4 AntallValor5 AntallValor6 AntallValor7 AntallValor8~
 AntallValor9 AntallValor10
&Scoped-define ENABLED-FIELDS-IN-KassererKontanter ButikkNr Dato KassererNr ~
z_nummer Belop1 Belop2 Belop3 Belop4 Belop5 Belop6 Belop7 Belop8 Belop9 ~
Belop10 AntallValor1 AntallValor2 AntallValor3 AntallValor4 AntallValor5 ~
AntallValor6 AntallValor7 AntallValor8 AntallValor9 AntallValor10 
&Scoped-Define DATA-FIELDS  ButikkNr Dato KassererNr z_nummer Belop1 Belop2 Belop3 Belop4 Belop5 Belop6~
 Belop7 Belop8 Belop9 Belop10 AntallValor1 AntallValor2 AntallValor3~
 AntallValor4 AntallValor5 AntallValor6 AntallValor7 AntallValor8~
 AntallValor9 AntallValor10
&Scoped-define DATA-FIELDS-IN-KassererKontanter ButikkNr Dato KassererNr ~
z_nummer Belop1 Belop2 Belop3 Belop4 Belop5 Belop6 Belop7 Belop8 Belop9 ~
Belop10 AntallValor1 AntallValor2 AntallValor3 AntallValor4 AntallValor5 ~
AntallValor6 AntallValor7 AntallValor8 AntallValor9 AntallValor10 
&Scoped-Define MANDATORY-FIELDS  ButikkNr
&Scoped-Define APPLICATION-SERVICE 
&Scoped-Define ASSIGN-LIST   rowObject.Belop1 = KassererKontanter.Belop[1]~
  rowObject.Belop2 = KassererKontanter.Belop[2]~
  rowObject.Belop3 = KassererKontanter.Belop[3]~
  rowObject.Belop4 = KassererKontanter.Belop[4]~
  rowObject.Belop5 = KassererKontanter.Belop[5]~
  rowObject.Belop6 = KassererKontanter.Belop[6]~
  rowObject.Belop7 = KassererKontanter.Belop[7]~
  rowObject.Belop8 = KassererKontanter.Belop[8]~
  rowObject.Belop9 = KassererKontanter.Belop[9]~
  rowObject.Belop10 = KassererKontanter.Belop[10]~
  rowObject.AntallValor1 = KassererKontanter.AntallValor[1]~
  rowObject.AntallValor2 = KassererKontanter.AntallValor[2]~
  rowObject.AntallValor3 = KassererKontanter.AntallValor[3]~
  rowObject.AntallValor4 = KassererKontanter.AntallValor[4]~
  rowObject.AntallValor5 = KassererKontanter.AntallValor[5]~
  rowObject.AntallValor6 = KassererKontanter.AntallValor[6]~
  rowObject.AntallValor7 = KassererKontanter.AntallValor[7]~
  rowObject.AntallValor8 = KassererKontanter.AntallValor[8]~
  rowObject.AntallValor9 = KassererKontanter.AntallValor[9]~
  rowObject.AntallValor10 = KassererKontanter.AntallValor[10]
&Scoped-Define DATA-FIELD-DEFS "dkassererkontanter.i"
&Scoped-define QUERY-STRING-Query-Main FOR EACH KassererKontanter NO-LOCK INDEXED-REPOSITION
{&DB-REQUIRED-START}
&Scoped-define OPEN-QUERY-Query-Main OPEN QUERY Query-Main FOR EACH KassererKontanter NO-LOCK INDEXED-REPOSITION.
{&DB-REQUIRED-END}
&Scoped-define TABLES-IN-QUERY-Query-Main KassererKontanter
&Scoped-define FIRST-TABLE-IN-QUERY-Query-Main KassererKontanter


/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

{&DB-REQUIRED-START}

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Query-Main FOR 
      KassererKontanter SCROLLING.
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
     _TblList          = "skotex.KassererKontanter"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > skotex.KassererKontanter.ButikkNr
"ButikkNr" "ButikkNr" ? ? "integer" ? ? ? ? ? ? yes ? yes 7.2 yes ""
     _FldNameList[2]   > skotex.KassererKontanter.Dato
"Dato" "Dato" ? ? "date" ? ? ? ? ? ? yes ? no 9.2 yes ""
     _FldNameList[3]   > skotex.KassererKontanter.KassererNr
"KassererNr" "KassererNr" ? ? "integer" ? ? ? ? ? ? yes ? no 10.4 yes ""
     _FldNameList[4]   > skotex.KassererKontanter.z_nummer
"z_nummer" "z_nummer" ? ? "integer" ? ? ? ? ? ? yes ? no 9.4 yes ""
     _FldNameList[5]   > skotex.KassererKontanter.Belop[1]
"Belop[1]" "Belop1" ? ? "decimal" ? ? ? ? ? ? yes ? no 13.2 yes ""
     _FldNameList[6]   > skotex.KassererKontanter.Belop[2]
"Belop[2]" "Belop2" ? ? "decimal" ? ? ? ? ? ? yes ? no 13.2 yes ""
     _FldNameList[7]   > skotex.KassererKontanter.Belop[3]
"Belop[3]" "Belop3" ? ? "decimal" ? ? ? ? ? ? yes ? no 13.2 yes ""
     _FldNameList[8]   > skotex.KassererKontanter.Belop[4]
"Belop[4]" "Belop4" ? ? "decimal" ? ? ? ? ? ? yes ? no 13.2 yes ""
     _FldNameList[9]   > skotex.KassererKontanter.Belop[5]
"Belop[5]" "Belop5" ? ? "decimal" ? ? ? ? ? ? yes ? no 13.2 yes ""
     _FldNameList[10]   > skotex.KassererKontanter.Belop[6]
"Belop[6]" "Belop6" ? ? "decimal" ? ? ? ? ? ? yes ? no 13.2 yes ""
     _FldNameList[11]   > skotex.KassererKontanter.Belop[7]
"Belop[7]" "Belop7" ? ? "decimal" ? ? ? ? ? ? yes ? no 13.2 yes ""
     _FldNameList[12]   > skotex.KassererKontanter.Belop[8]
"Belop[8]" "Belop8" ? ? "decimal" ? ? ? ? ? ? yes ? no 13.2 yes ""
     _FldNameList[13]   > skotex.KassererKontanter.Belop[9]
"Belop[9]" "Belop9" ? ? "decimal" ? ? ? ? ? ? yes ? no 13.2 yes ""
     _FldNameList[14]   > skotex.KassererKontanter.Belop[10]
"Belop[10]" "Belop10" ? ? "decimal" ? ? ? ? ? ? yes ? no 13.2 yes ""
     _FldNameList[15]   > skotex.KassererKontanter.AntallValor[1]
"AntallValor[1]" "AntallValor1" ? ? "integer" ? ? ? ? ? ? yes ? no 7.2 yes ""
     _FldNameList[16]   > skotex.KassererKontanter.AntallValor[2]
"AntallValor[2]" "AntallValor2" ? ? "integer" ? ? ? ? ? ? yes ? no 7.2 yes ""
     _FldNameList[17]   > skotex.KassererKontanter.AntallValor[3]
"AntallValor[3]" "AntallValor3" ? ? "integer" ? ? ? ? ? ? yes ? no 7.2 yes ""
     _FldNameList[18]   > skotex.KassererKontanter.AntallValor[4]
"AntallValor[4]" "AntallValor4" ? ? "integer" ? ? ? ? ? ? yes ? no 7.2 yes ""
     _FldNameList[19]   > skotex.KassererKontanter.AntallValor[5]
"AntallValor[5]" "AntallValor5" ? ? "integer" ? ? ? ? ? ? yes ? no 7.2 yes ""
     _FldNameList[20]   > skotex.KassererKontanter.AntallValor[6]
"AntallValor[6]" "AntallValor6" ? ? "integer" ? ? ? ? ? ? yes ? no 7.2 yes ""
     _FldNameList[21]   > skotex.KassererKontanter.AntallValor[7]
"AntallValor[7]" "AntallValor7" ? ? "integer" ? ? ? ? ? ? yes ? no 7.2 yes ""
     _FldNameList[22]   > skotex.KassererKontanter.AntallValor[8]
"AntallValor[8]" "AntallValor8" ? ? "integer" ? ? ? ? ? ? yes ? no 7.2 yes ""
     _FldNameList[23]   > skotex.KassererKontanter.AntallValor[9]
"AntallValor[9]" "AntallValor9" ? ? "integer" ? ? ? ? ? ? yes ? no 7.2 yes ""
     _FldNameList[24]   > skotex.KassererKontanter.AntallValor[10]
"AntallValor[10]" "AntallValor10" ? ? "integer" ? ? ? ? ? ? yes ? no 7.2 yes ""
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

