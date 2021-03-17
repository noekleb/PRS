&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
/* Connected Databases 
          vpi              PROGRESS
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
&Scoped-define INTERNAL-TABLES VPIStrekkode

/* Definitions for QUERY Query-Main                                     */
&Scoped-Define ENABLED-FIELDS  BrukerID EDato EkstStorl EkstVPILevNr ETid HovedNr Kode KodeType~
 RegistrertAv RegistrertDato RegistrertTid StrKode VareId VareNr~
 Bestillingsnummer Storl
&Scoped-define ENABLED-FIELDS-IN-VPIStrekkode BrukerID EDato EkstStorl ~
EkstVPILevNr ETid HovedNr Kode KodeType RegistrertAv RegistrertDato ~
RegistrertTid StrKode VareId VareNr Bestillingsnummer Storl 
&Scoped-Define DATA-FIELDS  BrukerID EDato EkstStorl EkstVPILevNr ETid HovedNr Kode KodeType~
 RegistrertAv RegistrertDato RegistrertTid StrKode VareId VareNr~
 Bestillingsnummer Storl
&Scoped-define DATA-FIELDS-IN-VPIStrekkode BrukerID EDato EkstStorl ~
EkstVPILevNr ETid HovedNr Kode KodeType RegistrertAv RegistrertDato ~
RegistrertTid StrKode VareId VareNr Bestillingsnummer Storl 
&Scoped-Define MANDATORY-FIELDS 
&Scoped-Define APPLICATION-SERVICE 
&Scoped-Define ASSIGN-LIST 
&Scoped-Define DATA-FIELD-DEFS "dvpistrekkode.i"
&Scoped-define QUERY-STRING-Query-Main FOR EACH VPIStrekkode NO-LOCK INDEXED-REPOSITION
{&DB-REQUIRED-START}
&Scoped-define OPEN-QUERY-Query-Main OPEN QUERY Query-Main FOR EACH VPIStrekkode NO-LOCK INDEXED-REPOSITION.
{&DB-REQUIRED-END}
&Scoped-define TABLES-IN-QUERY-Query-Main VPIStrekkode
&Scoped-define FIRST-TABLE-IN-QUERY-Query-Main VPIStrekkode


/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

{&DB-REQUIRED-START}

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Query-Main FOR 
      VPIStrekkode SCROLLING.
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
     _TblList          = "Vpi.VPIStrekkode"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > Vpi.VPIStrekkode.BrukerID
"BrukerID" "BrukerID" ? ? "character" ? ? ? ? ? ? yes ? no 10 yes ""
     _FldNameList[2]   > Vpi.VPIStrekkode.EDato
"EDato" "EDato" ? ? "date" ? ? ? ? ? ? yes ? no 11.6 yes ""
     _FldNameList[3]   > Vpi.VPIStrekkode.EkstStorl
"EkstStorl" "EkstStorl" ? ? "character" ? ? ? ? ? ? yes ? no 15 yes ""
     _FldNameList[4]   > Vpi.VPIStrekkode.EkstVPILevNr
"EkstVPILevNr" "EkstVPILevNr" ? ? "integer" ? ? ? ? ? ? yes ? no 6.2 yes ""
     _FldNameList[5]   > Vpi.VPIStrekkode.ETid
"ETid" "ETid" ? ? "integer" ? ? ? ? ? ? yes ? no 10.2 yes ""
     _FldNameList[6]   > Vpi.VPIStrekkode.HovedNr
"HovedNr" "HovedNr" ? ? "logical" ? ? ? ? ? ? yes ? no 3.8 yes ""
     _FldNameList[7]   > Vpi.VPIStrekkode.Kode
"Kode" "Kode" ? ? "character" ? ? ? ? ? ? yes ? no 20 yes ""
     _FldNameList[8]   > Vpi.VPIStrekkode.KodeType
"KodeType" "KodeType" ? ? "integer" ? ? ? ? ? ? yes ? no 2.8 yes ""
     _FldNameList[9]   > Vpi.VPIStrekkode.RegistrertAv
"RegistrertAv" "RegistrertAv" ? ? "character" ? ? ? ? ? ? yes ? no 10 yes ""
     _FldNameList[10]   > Vpi.VPIStrekkode.RegistrertDato
"RegistrertDato" "RegistrertDato" ? ? "date" ? ? ? ? ? ? yes ? no 11.6 yes ""
     _FldNameList[11]   > Vpi.VPIStrekkode.RegistrertTid
"RegistrertTid" "RegistrertTid" ? ? "integer" ? ? ? ? ? ? yes ? no 20.8 yes ""
     _FldNameList[12]   > Vpi.VPIStrekkode.StrKode
"StrKode" "StrKode" ? ? "integer" ? ? ? ? ? ? yes ? no 7.6 yes ""
     _FldNameList[13]   > Vpi.VPIStrekkode.VareId
"VareId" "VareId" ? ? "integer" ? ? ? ? ? ? yes ? no 8.4 yes ""
     _FldNameList[14]   > Vpi.VPIStrekkode.VareNr
"VareNr" "VareNr" ? ? "character" ? ? ? ? ? ? yes ? no 20 yes ""
     _FldNameList[15]   > Vpi.VPIStrekkode.Bestillingsnummer
"Bestillingsnummer" "Bestillingsnummer" ? ? "character" ? ? ? ? ? ? yes ? no 25 yes ""
     _FldNameList[16]   > Vpi.VPIStrekkode.Storl
"Storl" "Storl" ? ? "character" ? ? ? ? ? ? yes ? no 4 yes ""
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

