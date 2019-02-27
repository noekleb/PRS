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
DEF VAR iTotalCount             AS INT NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Global-define DATA-LOGIC-PROCEDURE .p

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
&Scoped-define INTERNAL-TABLES Bokforingsbilag

/* Definitions for QUERY Query-Main                                     */
&Scoped-Define ENABLED-FIELDS  Aar BokforingsNr BrukerID ButikkNr EDato ETid OmsetningsDato RegistrertAv~
 RegistrertDato RegistrertTid SendAv SendtDato SendtRegnskap SendtTid~
 GodkjentAv GodkjentDato GodkjentFlagg GodkjentTid EODDato EODMottatt
&Scoped-define ENABLED-FIELDS-IN-Bokforingsbilag Aar BokforingsNr BrukerID ~
ButikkNr EDato ETid OmsetningsDato RegistrertAv RegistrertDato ~
RegistrertTid SendAv SendtDato SendtRegnskap SendtTid GodkjentAv ~
GodkjentDato GodkjentFlagg GodkjentTid EODDato EODMottatt 
&Scoped-Define DATA-FIELDS  Aar fuEndretKl fuSendtKl fuRegistrertKl fuGodkjentKl BokforingsNr BrukerID~
 ButikkNr EDato ETid OmsetningsDato RegistrertAv RegistrertDato~
 RegistrertTid SendAv SendtDato SendtRegnskap SendtTid GodkjentAv~
 GodkjentDato GodkjentFlagg GodkjentTid EODDato EODMottatt
&Scoped-define DATA-FIELDS-IN-Bokforingsbilag Aar BokforingsNr BrukerID ~
ButikkNr EDato ETid OmsetningsDato RegistrertAv RegistrertDato ~
RegistrertTid SendAv SendtDato SendtRegnskap SendtTid GodkjentAv ~
GodkjentDato GodkjentFlagg GodkjentTid EODDato EODMottatt 
&Scoped-Define MANDATORY-FIELDS 
&Scoped-Define APPLICATION-SERVICE 
&Scoped-Define ASSIGN-LIST 
&Scoped-Define DATA-FIELD-DEFS "dbokforingsbilag.i"
&Scoped-define QUERY-STRING-Query-Main FOR EACH Bokforingsbilag NO-LOCK ~
    BY Bokforingsbilag.ButikkNr ~
       BY Bokforingsbilag.Aar DESCENDING ~
        BY Bokforingsbilag.BokforingsNr DESCENDING INDEXED-REPOSITION
{&DB-REQUIRED-START}
&Scoped-define OPEN-QUERY-Query-Main OPEN QUERY Query-Main FOR EACH Bokforingsbilag NO-LOCK ~
    BY Bokforingsbilag.ButikkNr ~
       BY Bokforingsbilag.Aar DESCENDING ~
        BY Bokforingsbilag.BokforingsNr DESCENDING INDEXED-REPOSITION.
{&DB-REQUIRED-END}
&Scoped-define TABLES-IN-QUERY-Query-Main Bokforingsbilag
&Scoped-define FIRST-TABLE-IN-QUERY-Query-Main Bokforingsbilag


/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD EndretKl dTables  _DB-REQUIRED
FUNCTION EndretKl RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getRecordCount dTables  _DB-REQUIRED
FUNCTION getRecordCount RETURNS INTEGER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD GodkjentKl dTables  _DB-REQUIRED
FUNCTION GodkjentKl RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD RegistrertKl dTables  _DB-REQUIRED
FUNCTION RegistrertKl RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD SendtKl dTables  _DB-REQUIRED
FUNCTION SendtKl RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}


/* ***********************  Control Definitions  ********************** */

{&DB-REQUIRED-START}

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Query-Main FOR 
      Bokforingsbilag SCROLLING.
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
{soksdo.i}

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
     _TblList          = "skotex.Bokforingsbilag"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _OrdList          = "skotex.Bokforingsbilag.ButikkNr|yes,skotex.Bokforingsbilag.Aar|no,skotex.Bokforingsbilag.BokforingsNr|no"
     _FldNameList[1]   > skotex.Bokforingsbilag.Aar
"Aar" "Aar" ? ? "integer" ? ? ? ? ? ? yes ? no 4.8 yes ""
     _FldNameList[2]   > "_<CALC>"
"EndretKl()" "fuEndretKl" "Kl" "x(8)" "character" ? ? ? ? ? ? no ? no 8 no ?
     _FldNameList[3]   > "_<CALC>"
"SendtKl()" "fuSendtKl" "Kl" "x(8)" "character" ? ? ? ? ? ? no ? no 8 no ?
     _FldNameList[4]   > "_<CALC>"
"RegistrertKl()" "fuRegistrertKl" "Kl" "x(8)" "character" ? ? ? ? ? ? no ? no 8 no ?
     _FldNameList[5]   > "_<CALC>"
"GodkjentKl()" "fuGodkjentKl" "Kl" "x(8)" "character" ? ? ? ? ? ? no ? no 8 no ?
     _FldNameList[6]   > skotex.Bokforingsbilag.BokforingsNr
"BokforingsNr" "BokforingsNr" ? ? "integer" ? ? ? ? ? ? yes ? no 12.2 yes ""
     _FldNameList[7]   > skotex.Bokforingsbilag.BrukerID
"BrukerID" "BrukerID" ? ? "character" ? ? ? ? ? ? yes ? no 10 yes ""
     _FldNameList[8]   > skotex.Bokforingsbilag.ButikkNr
"ButikkNr" "ButikkNr" ? ? "integer" ? ? ? ? ? ? yes ? no 8.2 yes ""
     _FldNameList[9]   > skotex.Bokforingsbilag.EDato
"EDato" "EDato" ? ? "date" ? ? ? ? ? ? yes ? no 11.6 yes ""
     _FldNameList[10]   > skotex.Bokforingsbilag.ETid
"ETid" "ETid" ? ? "integer" ? ? ? ? ? ? yes ? no 10.2 yes ""
     _FldNameList[11]   > skotex.Bokforingsbilag.OmsetningsDato
"OmsetningsDato" "OmsetningsDato" ? ? "date" ? ? ? ? ? ? yes ? no 15.2 yes ""
     _FldNameList[12]   > skotex.Bokforingsbilag.RegistrertAv
"RegistrertAv" "RegistrertAv" ? ? "character" ? ? ? ? ? ? yes ? no 10 yes ""
     _FldNameList[13]   > skotex.Bokforingsbilag.RegistrertDato
"RegistrertDato" "RegistrertDato" ? ? "date" ? ? ? ? ? ? yes ? no 11.6 yes ""
     _FldNameList[14]   > skotex.Bokforingsbilag.RegistrertTid
"RegistrertTid" "RegistrertTid" ? ? "integer" ? ? ? ? ? ? yes ? no 20.8 yes ""
     _FldNameList[15]   > skotex.Bokforingsbilag.SendAv
"SendAv" "SendAv" ? ? "character" ? ? ? ? ? ? yes ? no 15 yes ""
     _FldNameList[16]   > skotex.Bokforingsbilag.SendtDato
"SendtDato" "SendtDato" ? ? "date" ? ? ? ? ? ? yes ? no 9.2 yes ""
     _FldNameList[17]   > skotex.Bokforingsbilag.SendtRegnskap
"SendtRegnskap" "SendtRegnskap" ? "*~~/" "logical" ? ? ? ? ? ? yes ? no 5.6 yes ""
     _FldNameList[18]   > skotex.Bokforingsbilag.SendtTid
"SendtTid" "SendtTid" ? ? "integer" ? ? ? ? ? ? yes ? no 10.2 yes ""
     _FldNameList[19]   > skotex.Bokforingsbilag.GodkjentAv
"GodkjentAv" "GodkjentAv" ? ? "character" ? ? ? ? ? ? yes ? no 15 yes ""
     _FldNameList[20]   > skotex.Bokforingsbilag.GodkjentDato
"GodkjentDato" "GodkjentDato" ? ? "date" ? ? ? ? ? ? yes ? no 9.2 yes ""
     _FldNameList[21]   > skotex.Bokforingsbilag.GodkjentFlagg
"GodkjentFlagg" "GodkjentFlagg" ? ? "logical" ? ? ? ? ? ? yes ? no 8.6 yes ""
     _FldNameList[22]   > skotex.Bokforingsbilag.GodkjentTid
"GodkjentTid" "GodkjentTid" ? ? "integer" ? ? ? ? ? ? yes ? no 12.8 yes ""
     _FldNameList[23]   > skotex.Bokforingsbilag.EODDato
"EODDato" "EODDato" ? ? "date" ? ? ? ? ? ? yes ? no 9.2 yes ?
     _FldNameList[24]   > skotex.Bokforingsbilag.EODMottatt
"EODMottatt" "EODMottatt" ? ? "logical" ? ? ? ? ? ? yes ? no 11.2 yes ?
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

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DATA.CALCULATE dTables  DATA.CALCULATE _DB-REQUIRED
PROCEDURE DATA.CALCULATE :
/*------------------------------------------------------------------------------
  Purpose:     Calculate all the Calculated Expressions found in the
               SmartDataObject.
  Parameters:  <none>
------------------------------------------------------------------------------*/
      ASSIGN 
         rowObject.fuEndretKl = (EndretKl())
         rowObject.fuGodkjentKl = (GodkjentKl())
         rowObject.fuRegistrertKl = (RegistrertKl())
         rowObject.fuSendtKl = (SendtKl())
      .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

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

/* ************************  Function Implementations ***************** */

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION EndretKl dTables  _DB-REQUIRED
FUNCTION EndretKl RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  RETURN string(RowObject.ETid,"HH:MM:SS").   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getRecordCount dTables  _DB-REQUIRED
FUNCTION getRecordCount RETURNS INTEGER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
RETURN iTotalCount.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION GodkjentKl dTables  _DB-REQUIRED
FUNCTION GodkjentKl RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  RETURN string(RowObject.GodkjentTid,"HH:MM:SS").   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION RegistrertKl dTables  _DB-REQUIRED
FUNCTION RegistrertKl RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  RETURN string(RowObject.RegistrertTid,"HH:MM:SS").   /* Function return value. */


END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION SendtKl dTables  _DB-REQUIRED
FUNCTION SendtKl RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  RETURN string(RowObject.SendtTid,"HH:MM:SS").   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

