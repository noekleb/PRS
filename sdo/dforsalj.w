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
&Scoped-define INTERNAL-TABLES Forsalj

/* Definitions for QUERY Query-Main                                     */
&Scoped-Define ENABLED-FIELDS  AnsattNr BrukerID EDato ETid FoAdr FoAnstNr FoNamn FoPadr FoPersNr FoPoNr~
 ForsNr FoTel LevNr RegistrertAv RegistrertDato RegistrertTid BrukerId2~
 ButikkNr FodtDato ForsaljAktiv navnikasse passord Prisendring Rabatt Retur~
 SlettBong SletteForste slettTidligere
&Scoped-define ENABLED-FIELDS-IN-Forsalj AnsattNr BrukerID EDato ETid FoAdr ~
FoAnstNr FoNamn FoPadr FoPersNr FoPoNr ForsNr FoTel LevNr RegistrertAv ~
RegistrertDato RegistrertTid BrukerId2 ButikkNr FodtDato ForsaljAktiv ~
navnikasse passord Prisendring Rabatt Retur SlettBong SletteForste ~
slettTidligere 
&Scoped-Define DATA-FIELDS  AnsattNr BrukerID EDato ETid FoAdr FoAnstNr FoNamn FoPadr FoPersNr FoPoNr~
 ForsNr FoTel LevNr RegistrertAv RegistrertDato RegistrertTid BrukerId2~
 ButikkNr FodtDato ForsaljAktiv navnikasse passord Prisendring Rabatt Retur~
 SlettBong SletteForste slettTidligere
&Scoped-define DATA-FIELDS-IN-Forsalj AnsattNr BrukerID EDato ETid FoAdr ~
FoAnstNr FoNamn FoPadr FoPersNr FoPoNr ForsNr FoTel LevNr RegistrertAv ~
RegistrertDato RegistrertTid BrukerId2 ButikkNr FodtDato ForsaljAktiv ~
navnikasse passord Prisendring Rabatt Retur SlettBong SletteForste ~
slettTidligere 
&Scoped-Define MANDATORY-FIELDS 
&Scoped-Define APPLICATION-SERVICE 
&Scoped-Define ASSIGN-LIST 
&Scoped-Define DATA-FIELD-DEFS "dforsalj.i"
&Scoped-define QUERY-STRING-Query-Main FOR EACH Forsalj NO-LOCK INDEXED-REPOSITION
{&DB-REQUIRED-START}
&Scoped-define OPEN-QUERY-Query-Main OPEN QUERY Query-Main FOR EACH Forsalj NO-LOCK INDEXED-REPOSITION.
{&DB-REQUIRED-END}
&Scoped-define TABLES-IN-QUERY-Query-Main Forsalj
&Scoped-define FIRST-TABLE-IN-QUERY-Query-Main Forsalj


/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

{&DB-REQUIRED-START}

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Query-Main FOR 
      Forsalj SCROLLING.
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
     _TblList          = "skotex.Forsalj"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > skotex.Forsalj.AnsattNr
"AnsattNr" "AnsattNr" ? ? "character" ? ? ? ? ? ? yes ? no 15 yes ""
     _FldNameList[2]   > skotex.Forsalj.BrukerID
"BrukerID" "BrukerID" ? ? "character" ? ? ? ? ? ? yes ? no 10 yes ""
     _FldNameList[3]   > skotex.Forsalj.EDato
"EDato" "EDato" ? ? "date" ? ? ? ? ? ? yes ? no 11.6 yes ""
     _FldNameList[4]   > skotex.Forsalj.ETid
"ETid" "ETid" ? ? "integer" ? ? ? ? ? ? yes ? no 10.2 yes ""
     _FldNameList[5]   > skotex.Forsalj.FoAdr
"FoAdr" "FoAdr" ? ? "character" ? ? ? ? ? ? yes ? no 30 yes ""
     _FldNameList[6]   > skotex.Forsalj.FoAnstNr
"FoAnstNr" "FoAnstNr" ? ? "integer" ? ? ? ? ? ? yes ? no 18.2 yes ""
     _FldNameList[7]   > skotex.Forsalj.FoNamn
"FoNamn" "FoNamn" ? ? "character" ? ? ? ? ? ? yes ? no 30 yes ""
     _FldNameList[8]   > skotex.Forsalj.FoPadr
"FoPadr" "FoPadr" ? ? "character" ? ? ? ? ? ? yes ? no 30 yes ""
     _FldNameList[9]   > skotex.Forsalj.FoPersNr
"FoPersNr" "FoPersNr" ? ? "decimal" ? ? ? ? ? ? yes ? no 11.2 yes ""
     _FldNameList[10]   > skotex.Forsalj.FoPoNr
"FoPoNr" "FoPoNr" ? ? "character" ? ? ? ? ? ? yes ? no 11.6 yes ""
     _FldNameList[11]   > skotex.Forsalj.ForsNr
"ForsNr" "ForsNr" ? ">>>>>>>9" "integer" ? ? ? ? ? ? yes ? no 9.6 yes ""
     _FldNameList[12]   > skotex.Forsalj.FoTel
"FoTel" "FoTel" ? ? "character" ? ? ? ? ? ? yes ? no 15 yes ""
     _FldNameList[13]   > skotex.Forsalj.LevNr
"LevNr" "LevNr" ? ? "integer" ? ? ? ? ? ? yes ? no 10.8 yes ""
     _FldNameList[14]   > skotex.Forsalj.RegistrertAv
"RegistrertAv" "RegistrertAv" ? ? "character" ? ? ? ? ? ? yes ? no 10 yes ""
     _FldNameList[15]   > skotex.Forsalj.RegistrertDato
"RegistrertDato" "RegistrertDato" ? ? "date" ? ? ? ? ? ? yes ? no 11.6 yes ""
     _FldNameList[16]   > skotex.Forsalj.RegistrertTid
"RegistrertTid" "RegistrertTid" ? ? "integer" ? ? ? ? ? ? yes ? no 20.8 yes ""
     _FldNameList[17]   > skotex.Forsalj.BrukerId2
"BrukerId2" "BrukerId2" ? ? "character" ? ? ? ? ? ? yes ? no 10 yes ""
     _FldNameList[18]   > skotex.Forsalj.ButikkNr
"ButikkNr" "ButikkNr" ? ? "integer" ? ? ? ? ? ? yes ? no 7.2 yes ""
     _FldNameList[19]   > skotex.Forsalj.FodtDato
"FodtDato" "FodtDato" ? ? "date" ? ? ? ? ? ? yes ? no 9.2 yes ""
     _FldNameList[20]   > skotex.Forsalj.ForsaljAktiv
"ForsaljAktiv" "ForsaljAktiv" ? ? "logical" ? ? ? ? ? ? yes ? no 4.8 yes ""
     _FldNameList[21]   > skotex.Forsalj.navnikasse
"navnikasse" "navnikasse" ? ? "character" ? ? ? ? ? ? yes ? no 15 yes ""
     _FldNameList[22]   > skotex.Forsalj.passord
"passord" "passord" ? ? "integer" ? ? ? ? ? ? yes ? no 7.6 yes ""
     _FldNameList[23]   > skotex.Forsalj.Prisendring
"Prisendring" "Prisendring" ? ? "integer" ? ? ? ? ? ? yes ? no 10.4 yes ""
     _FldNameList[24]   > skotex.Forsalj.Rabatt
"Rabatt" "Rabatt" ? ? "integer" ? ? ? ? ? ? yes ? no 6.4 yes ""
     _FldNameList[25]   > skotex.Forsalj.Retur
"Retur" "Retur" ? ? "integer" ? ? ? ? ? ? yes ? no 5.2 yes ""
     _FldNameList[26]   > skotex.Forsalj.SlettBong
"SlettBong" "SlettBong" ? ? "integer" ? ? ? ? ? ? yes ? no 5 yes ""
     _FldNameList[27]   > skotex.Forsalj.SletteForste
"SletteForste" "SletteForste" ? ? "integer" ? ? ? ? ? ? yes ? no 11.2 yes ""
     _FldNameList[28]   > skotex.Forsalj.slettTidligere
"slettTidligere" "slettTidligere" ? ? "integer" ? ? ? ? ? ? yes ? no 13.4 yes ""
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

