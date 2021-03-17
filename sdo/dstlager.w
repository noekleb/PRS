&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
/* Connected Databases 
          skotex           PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
{adecomm/appserv.i}


/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE TT_StLager NO-UNDO LIKE StLager
       FIELD UtSolgt% AS DECI
       FIELD Vg LIKE VarGr.Vg
       FIELD VgBeskr LIKE VarGr.VgBeskr
       FIELD HgBeskr LIKE HuvGr.HgBeskr
       FIELD AvdelingNr LIKE Avdeling.AvdelingNr
       FIELD AvdelingNavn LIKE Avdeling.AvdelingNavn
       FIELD LevNr LIKE LevBas.LevNr
       FIELD LevNamn LIKE LevBas.levnamn
       FIElD ForsNr LIKE Forsalj.ForsNr
       FIELD FoNamn LIKE Forsalj.FoNamn
       FIELD SelgerNr LIKE Selger.SelgerNr
       FIELD SelgerNavn LIKE Selger.Navn
       FIELD Beskrivelse AS CHARACTER
       FIELD CharButik AS CHARACTER
       FIELD DbKr LIKE StLinje.DbKr
       FIELD Db% LIKE StLinje.Db%
       FIELD LagerVerdi LIKE StLager.VVarekost
       FIELD Butnamn LIKE Butiker.Butnamn
       FIELD Solgt% AS DECI
       FIELD DBandel% AS DECI
       FIELD Rabandel% AS DECI
       FIELD Kjopandel% AS DECI
       .



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

DEFINE TEMP-TABLE TT_StLagerTMP NO-UNDO LIKE TT_StLager.

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
&Scoped-define INTERNAL-TABLES StLager

/* Definitions for QUERY Query-Main                                     */
&Scoped-Define ENABLED-FIELDS  AntRab AntSolgt BrekkAnt BrekkVerdi BrukerID Butik DataObjekt EDato ETid~
 GjenkjopAnt GjenkjopVerdi IntAnt IntVerdi JustAnt JustVerdi KjopAnt~
 KjopVerdi LagAnt NedAnt NedVerdi OvAnt OvVerdi RegistrertAv RegistrertDato~
 RegistrertTid ReklAnt ReklLAnt ReklLVerdi ReklVerdi RetLAnt SistInnlevert~
 StTypeId SvinnAnt SvinnVerdi VerdiRabatt VerdiSolgt VVarekost SVK~
 vSnittKostPris
&Scoped-define ENABLED-FIELDS-IN-StLager AntRab AntSolgt BrekkAnt ~
BrekkVerdi BrukerID Butik DataObjekt EDato ETid GjenkjopAnt GjenkjopVerdi ~
IntAnt IntVerdi JustAnt JustVerdi KjopAnt KjopVerdi LagAnt NedAnt NedVerdi ~
OvAnt OvVerdi RegistrertAv RegistrertDato RegistrertTid ReklAnt ReklLAnt ~
ReklLVerdi ReklVerdi RetLAnt SistInnlevert StTypeId SvinnAnt SvinnVerdi ~
VerdiRabatt VerdiSolgt VVarekost SVK vSnittKostPris 
&Scoped-Define DATA-FIELDS  AntRab AntSolgt BrekkAnt BrekkVerdi BrukerID Butik DataObjekt EDato ETid~
 GjenkjopAnt GjenkjopVerdi IntAnt IntVerdi JustAnt JustVerdi KjopAnt~
 KjopVerdi LagAnt NedAnt NedVerdi OvAnt OvVerdi RegistrertAv RegistrertDato~
 RegistrertTid ReklAnt ReklLAnt ReklLVerdi ReklVerdi RetLAnt SistInnlevert~
 StTypeId SvinnAnt SvinnVerdi VerdiRabatt VerdiSolgt VVarekost SVK~
 vSnittKostPris
&Scoped-define DATA-FIELDS-IN-StLager AntRab AntSolgt BrekkAnt BrekkVerdi ~
BrukerID Butik DataObjekt EDato ETid GjenkjopAnt GjenkjopVerdi IntAnt ~
IntVerdi JustAnt JustVerdi KjopAnt KjopVerdi LagAnt NedAnt NedVerdi OvAnt ~
OvVerdi RegistrertAv RegistrertDato RegistrertTid ReklAnt ReklLAnt ~
ReklLVerdi ReklVerdi RetLAnt SistInnlevert StTypeId SvinnAnt SvinnVerdi ~
VerdiRabatt VerdiSolgt VVarekost SVK vSnittKostPris 
&Scoped-Define MANDATORY-FIELDS  StTypeId
&Scoped-Define APPLICATION-SERVICE 
&Scoped-Define ASSIGN-LIST 
&Scoped-Define DATA-FIELD-DEFS "dstlager.i"
&Scoped-define QUERY-STRING-Query-Main FOR EACH StLager NO-LOCK INDEXED-REPOSITION
{&DB-REQUIRED-START}
&Scoped-define OPEN-QUERY-Query-Main OPEN QUERY Query-Main FOR EACH StLager NO-LOCK INDEXED-REPOSITION.
{&DB-REQUIRED-END}
&Scoped-define TABLES-IN-QUERY-Query-Main StLager
&Scoped-define FIRST-TABLE-IN-QUERY-Query-Main StLager


/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getBeskr dTables  _DB-REQUIRED
FUNCTION getBeskr RETURNS CHARACTER
  ( INPUT cStTypeId AS CHARACTER, INPUT cObjekt AS CHARACTER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}


/* ***********************  Control Definitions  ********************** */

{&DB-REQUIRED-START}

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Query-Main FOR 
      StLager SCROLLING.
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
   Temp-Tables and Buffers:
      TABLE: TT_StLager T "?" NO-UNDO skotex StLager
      ADDITIONAL-FIELDS:
          FIELD UtSolgt% AS DECI
          FIELD Vg LIKE VarGr.Vg
          FIELD VgBeskr LIKE VarGr.VgBeskr
          FIELD HgBeskr LIKE HuvGr.HgBeskr
          FIELD AvdelingNr LIKE Avdeling.AvdelingNr
          FIELD AvdelingNavn LIKE Avdeling.AvdelingNavn
          FIELD LevNr LIKE LevBas.LevNr
          FIELD LevNamn LIKE LevBas.levnamn
          FIElD ForsNr LIKE Forsalj.ForsNr
          FIELD FoNamn LIKE Forsalj.FoNamn
          FIELD SelgerNr LIKE Selger.SelgerNr
          FIELD SelgerNavn LIKE Selger.Navn
          FIELD Beskrivelse AS CHARACTER
          FIELD CharButik AS CHARACTER
          FIELD DbKr LIKE StLinje.DbKr
          FIELD Db% LIKE StLinje.Db%
          FIELD LagerVerdi LIKE StLager.VVarekost
          FIELD Butnamn LIKE Butiker.Butnamn
          FIELD Solgt% AS DECI
          FIELD DBandel% AS DECI
          FIELD Rabandel% AS DECI
          FIELD Kjopandel% AS DECI
          
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
     _TblList          = "skotex.StLager"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > skotex.StLager.AntRab
"AntRab" "AntRab" ? ? "decimal" ? ? ? ? ? ? yes ? no 11.8 yes ""
     _FldNameList[2]   > skotex.StLager.AntSolgt
"AntSolgt" "AntSolgt" ? ? "decimal" ? ? ? ? ? ? yes ? no 11.4 yes ""
     _FldNameList[3]   > skotex.StLager.BrekkAnt
"BrekkAnt" "BrekkAnt" ? ? "decimal" ? ? ? ? ? ? yes ? no 9.4 yes ""
     _FldNameList[4]   > skotex.StLager.BrekkVerdi
"BrekkVerdi" "BrekkVerdi" ? ? "decimal" ? ? ? ? ? ? yes ? no 16.4 yes ""
     _FldNameList[5]   > skotex.StLager.BrukerID
"BrukerID" "BrukerID" ? ? "character" ? ? ? ? ? ? yes ? no 10 yes ""
     _FldNameList[6]   > skotex.StLager.Butik
"Butik" "Butik" ? ? "integer" ? ? ? ? ? ? yes ? no 7.2 yes ""
     _FldNameList[7]   > skotex.StLager.DataObjekt
"DataObjekt" "DataObjekt" ? ? "character" ? ? ? ? ? ? yes ? no 15 yes ""
     _FldNameList[8]   > skotex.StLager.EDato
"EDato" "EDato" ? ? "date" ? ? ? ? ? ? yes ? no 11.6 yes ""
     _FldNameList[9]   > skotex.StLager.ETid
"ETid" "ETid" ? ? "integer" ? ? ? ? ? ? yes ? no 10.2 yes ""
     _FldNameList[10]   > skotex.StLager.GjenkjopAnt
"GjenkjopAnt" "GjenkjopAnt" ? ? "decimal" ? ? ? ? ? ? yes ? no 10.4 yes ""
     _FldNameList[11]   > skotex.StLager.GjenkjopVerdi
"GjenkjopVerdi" "GjenkjopVerdi" ? ? "decimal" ? ? ? ? ? ? yes ? no 23.6 yes ""
     _FldNameList[12]   > skotex.StLager.IntAnt
"IntAnt" "IntAnt" ? ? "decimal" ? ? ? ? ? ? yes ? no 13.2 yes ""
     _FldNameList[13]   > skotex.StLager.IntVerdi
"IntVerdi" "IntVerdi" ? ? "decimal" ? ? ? ? ? ? yes ? no 21.4 yes ""
     _FldNameList[14]   > skotex.StLager.JustAnt
"JustAnt" "JustAnt" ? ? "decimal" ? ? ? ? ? ? yes ? no 11.8 yes ""
     _FldNameList[15]   > skotex.StLager.JustVerdi
"JustVerdi" "JustVerdi" ? ? "decimal" ? ? ? ? ? ? yes ? no 14.4 yes ""
     _FldNameList[16]   > skotex.StLager.KjopAnt
"KjopAnt" "KjopAnt" ? ? "decimal" ? ? ? ? ? ? yes ? no 13.2 yes ""
     _FldNameList[17]   > skotex.StLager.KjopVerdi
"KjopVerdi" "KjopVerdi" ? ? "decimal" ? ? ? ? ? ? yes ? no 13 yes ""
     _FldNameList[18]   > skotex.StLager.LagAnt
"LagAnt" "LagAnt" ? ? "decimal" ? ? ? ? ? ? yes ? no 11.4 yes ""
     _FldNameList[19]   > skotex.StLager.NedAnt
"NedAnt" "NedAnt" ? ? "decimal" ? ? ? ? ? ? yes ? no 16.6 yes ""
     _FldNameList[20]   > skotex.StLager.NedVerdi
"NedVerdi" "NedVerdi" ? ? "decimal" ? ? ? ? ? ? yes ? no 16 yes ""
     _FldNameList[21]   > skotex.StLager.OvAnt
"OvAnt" "OvAnt" ? ? "decimal" ? ? ? ? ? ? yes ? no 13.2 yes ""
     _FldNameList[22]   > skotex.StLager.OvVerdi
"OvVerdi" "OvVerdi" ? ? "decimal" ? ? ? ? ? ? yes ? no 22.2 yes ""
     _FldNameList[23]   > skotex.StLager.RegistrertAv
"RegistrertAv" "RegistrertAv" ? ? "character" ? ? ? ? ? ? yes ? no 10 yes ""
     _FldNameList[24]   > skotex.StLager.RegistrertDato
"RegistrertDato" "RegistrertDato" ? ? "date" ? ? ? ? ? ? yes ? no 11.6 yes ""
     _FldNameList[25]   > skotex.StLager.RegistrertTid
"RegistrertTid" "RegistrertTid" ? ? "integer" ? ? ? ? ? ? yes ? no 20.8 yes ""
     _FldNameList[26]   > skotex.StLager.ReklAnt
"ReklAnt" "ReklAnt" ? ? "decimal" ? ? ? ? ? ? yes ? no 8.2 yes ""
     _FldNameList[27]   > skotex.StLager.ReklLAnt
"ReklLAnt" "ReklLAnt" ? ? "decimal" ? ? ? ? ? ? yes ? no 13.4 yes ""
     _FldNameList[28]   > skotex.StLager.ReklLVerdi
"ReklLVerdi" "ReklLVerdi" ? ? "decimal" ? ? ? ? ? ? yes ? no 32.6 yes ""
     _FldNameList[29]   > skotex.StLager.ReklVerdi
"ReklVerdi" "ReklVerdi" ? ? "decimal" ? ? ? ? ? ? yes ? no 24.4 yes ""
     _FldNameList[30]   > skotex.StLager.RetLAnt
"RetLAnt" "RetLAnt" ? ? "decimal" ? ? ? ? ? ? yes ? no 15.8 yes ""
     _FldNameList[31]   > skotex.StLager.SistInnlevert
"SistInnlevert" "SistInnlevert" ? ? "date" ? ? ? ? ? ? yes ? no 11.6 yes ""
     _FldNameList[32]   > skotex.StLager.StTypeId
"StTypeId" "StTypeId" ? ? "character" ? ? ? ? ? ? yes ? yes 10 yes ""
     _FldNameList[33]   > skotex.StLager.SvinnAnt
"SvinnAnt" "SvinnAnt" ? ? "decimal" ? ? ? ? ? ? yes ? no 10.8 yes ""
     _FldNameList[34]   > skotex.StLager.SvinnVerdi
"SvinnVerdi" "SvinnVerdi" ? ? "decimal" ? ? ? ? ? ? yes ? no 14.4 yes ""
     _FldNameList[35]   > skotex.StLager.VerdiRabatt
"VerdiRabatt" "VerdiRabatt" ? ? "decimal" ? ? ? ? ? ? yes ? no 12 yes ""
     _FldNameList[36]   > skotex.StLager.VerdiSolgt
"VerdiSolgt" "VerdiSolgt" ? ? "decimal" ? ? ? ? ? ? yes ? no 13 yes ""
     _FldNameList[37]   > skotex.StLager.VVarekost
"VVarekost" "VVarekost" ? ? "decimal" ? ? ? ? ? ? yes ? no 12 yes ""
     _FldNameList[38]   > skotex.StLager.SVK
"SVK" "SVK" ? ? "decimal" ? ? ? ? ? ? yes ? no 13 yes ""
     _FldNameList[39]   > skotex.StLager.vSnittKostPris
"vSnittKostPris" "vSnittKostPris" ? ? "decimal" ? ? ? ? ? ? yes ? no 17.8 yes ""
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

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initializeObject dTables  _DB-REQUIRED
PROCEDURE initializeObject :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  RUN SUPER.

  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE StLagerToTT dTables  _DB-REQUIRED
PROCEDURE StLagerToTT :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes: 
         
------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER TTH AS HANDLE NO-UNDO.
    DEFINE INPUT  PARAMETER cStTypeId AS CHARACTER  NO-UNDO.
    DEFINE INPUT  PARAMETER cButiker  AS CHARACTER  NO-UNDO.
    DEFINE INPUT  PARAMETER lVisBut   AS LOGICAL    NO-UNDO.
    DEFINE INPUT  PARAMETER cXFilter  AS CHARACTER  NO-UNDO.
    DEFINE INPUT  PARAMETER cXParam   AS CHARACTER  NO-UNDO.
    DEFINE        VARIABLE  lFirst    AS LOGICAL    NO-UNDO.
    DEFINE        VARIABLE  rRowId    AS ROWID      NO-UNDO.
    DEFINE        VARIABLE  dTotSum   AS DECIMAL    NO-UNDO.
    DEFINE        VARIABLE  hBufferField AS HANDLE     NO-UNDO.
    DEFINE        VARIABLE  pcFeltListe  AS CHARACTER  NO-UNDO.
    DEFINE        VARIABLE  pcVerdier    AS CHARACTER  NO-UNDO.
    DEFINE        VARIABLE  iCount       AS INTEGER    NO-UNDO.
    DEFINE        VARIABLE  lUtvidetFilter AS LOGICAL    NO-UNDO.
    DEFINE        VARIABLE  lIkkeTreff     AS LOGICAL    NO-UNDO.
    DEFINE        VARIABLE  cQRYstring     AS CHARACTER  NO-UNDO.
    DEFINE        VARIABLE  iButikLoop     AS INTEGER    NO-UNDO.
    DEFINE        VARIABLE  dSolgtTot      AS DECIMAL    NO-UNDO.
    DEFINE        VARIABLE  dDBTot         AS DECIMAL    NO-UNDO.
    DEFINE        VARIABLE  dRabTot        AS DECIMAL    NO-UNDO.
    DEFINE        VARIABLE  dKjopTot       AS DECIMAL    NO-UNDO.


    EMPTY TEMP-TABLE TT_StLager.
    EMPTY TEMP-TABLE TT_StLagerTMP.
    CREATE TT_StLagerTMP. /* en temporär record för att kunna summera */
    IF NUM-ENTRIES(cXFilter,";") = 2 THEN DO:
        ASSIGN pcFeltListe = ENTRY(1,cXFilter,";")
               pcVerdier   = ENTRY(2,cXFilter,";").
        IF pcFeltListe <> pcVerdier THEN  /* */
            ASSIGN lUtvidetFilter = TRUE.
    END.
    ASSIGN cQRYstring = DYNAMIC-FUNCTION('getQueryString':U).
    DO iButikLoop = 1 TO NUM-ENTRIES(cButiker):
        DYNAMIC-FUNCTION('closeQuery':U).
        DYNAMIC-FUNCTION('setQueryString':U,
        INPUT REPLACE(cQRYstring,"SUBSTBUTIK","Butik = " + ENTRY(iButikLoop,cButiker)) /* CHARACTER */).
        DYNAMIC-FUNCTION('openQuery':U).
        ASSIGN lFirst = FALSE
               rRowId = ?.
        REPEAT:
            IF lFirst = FALSE THEN DO:
                RUN fetchFirst.
                ASSIGN lFirst = TRUE.
            END.
            ELSE
                RUN fetchNext.
            IF rRowId = ROWID(RowObject) THEN
                LEAVE.
            ELSE
                ASSIGN rRowId = ROWID(RowObject).
            IF lUtvidetFilter = TRUE THEN DO:
                IF cStTypeId = "AVDELING" THEN DO:
                    FIND Avdeling WHERE Avdeling.AvdelingNr = INT(RowObject.DataObjekt) NO-LOCK NO-ERROR.
                    IF NOT AVAIL Avdeling OR NOT CAN-DO(pcVerdier,STRING(Avdeling.AvdelingNr)) THEN
                        NEXT.
                END.
                IF cStTypeId = "HOVEDGR" THEN DO:
                    IF pcFeltListe = "HuvGr" AND NOT CAN-DO(pcVerdier,STRING(INT(RowObject.DataObjekt))) THEN
                        NEXT.
                    ELSE IF pcFeltListe = "AvdelingNr" THEN DO: /* Här har vi test på AvdelingNr kvar */
                        FIND HuvGr WHERE HuvGr.Hg = INT(RowObject.DataObjekt) NO-LOCK NO-ERROR.
                        IF NOT AVAIL HuvGr OR NOT CAN-DO(pcVerdier,STRING(HuvGr.AvdelingNr)) THEN
                            NEXT.
                    END.
                END.
                IF cStTypeId = "VAREGR" THEN DO:
                    IF pcFeltListe = "VarGr" THEN DO: 
                        IF NOT CAN-DO(pcVerdier,STRING(INT(RowObject.DataObjekt))) THEN
                            NEXT.
                    END.
                    ELSE IF pcFeltListe = "HuvGr" OR pcFeltListe = "AvdelingNr" THEN DO:
                        FIND VarGr WHERE VarGr.Vg = INT(RowObject.DataObjekt) NO-LOCK NO-ERROR.
                        IF NOT AVAIL VarGr THEN
                            NEXT.
                        ELSE IF pcFeltListe = "HuvGr" THEN DO: 
                            IF NOT CAN-DO(pcVerdier,STRING(VarGr.Hg)) THEN
                                NEXT.
                        END.
                        ELSE IF pcFeltListe = "AvdelingNr" THEN DO: /* Här har vi AvdelingNr kvar */
                            FIND HuvGr OF VarGr NO-LOCK NO-ERROR.
                            IF NOT AVAIL HuvGr OR NOT CAN-DO(pcVerdier,STRING(HuvGr.AvdelingNr)) THEN
                                NEXT.
                        END.
                    END.
                END.
                ELSE IF cStTypeId = "LEVERAN" THEN DO:
                    FIND LevBas WHERE LevBas.LevNr = INT(RowObject.DataObjekt) NO-LOCK NO-ERROR.
                    IF NOT AVAIL LevBas OR NOT CAN-DO(pcVerdier,STRING(LevBas.LevNr)) THEN
                        NEXT.
                END.
            END.
            BUFFER-COPY RowObject TO TT_StLagerTMP.
            ASSIGN dSolgtTot = dSolgtTot + TT_StLagerTMP.VerdiSolgt
                   dDBTot    = dDBTot    + TT_StLagerTMP.VerdiSolgt - TT_StLagerTMP.Svk
                   dRabTot   = dRabTot   + TT_StLagerTMP.VerdiRabatt
                   dKjopTot  = dKjopTot  + TT_StLagerTMP.KjopVerdi.
            IF NOT lVisBut THEN
                ASSIGN TT_StLagerTMP.Butik = 0.
            IF NOT lVisBut THEN
                FIND TT_StLager WHERE TT_StLager.Butik      = 0 AND
                                      TT_StLager.StTypeId   = cStTypeId AND
                                      TT_StLager.DataObjekt = TT_StLagerTMP.DataObjekt NO-ERROR.
            IF AVAIL TT_StLager THEN DO:
                RUN SummeraTT.
            END.
            ELSE DO:
                CREATE TT_StLager.
                BUFFER-COPY TT_StLagerTMP TO TT_StLager.
                ASSIGN TT_StLager.Beskrivelse = getBeskr(cStTypeId,TT_StLager.DataObjekt)
                       TT_StLager.CharButik = IF lVisBut THEN STRING(TT_StLager.Butik) ELSE ""
    /*  org                  TT_StLager.DbKr      = TT_StLager.VerdiSolgt - (TT_StLager.AntSolgt * TT_StLager.VVarekost) */
                           TT_StLager.DbKr      = TT_StLager.VerdiSolgt - TT_StLager.Svk /*(TT_StLager.AntSolgt * TT_StLager.Nedverdi)*/
                      /* Här sätter vi den totala lagervärdet utifrån LagAnt och VVarekost */
                      /* vid flera butiker summeras den och längre ner delar vi det totala lagervärdet med totala LagAnt */
                      /* som ger vvarekost/st */
                       /* !!! Vi har ändrat så att VVarekost innehåller totala lagervärdet */
                       TT_StLager.LagerVerdi     = /* TT_StLager.LagAnt */ TT_StLager.VVarekost.
                IF lVisBut THEN
                    ASSIGN TT_StLager.Butnamn = getBeskr("GETBUTNAVN",STRING(TT_StLager.Butik)).
                RELEASE TT_StLager.
            END.
            RELEASE TT_StLager.
        END.
    END.
    FOR EACH TT_StLager:
        ASSIGN TT_StLager.UtSolgt%  = IF TT_StLager.KjopAnt <> 0 THEN ROUND(TT_StLager.AntSolgt / TT_StLager.KjopAnt * 100,1)
                                      ELSE 0
               TT_StLager.Db%       = IF TT_StLager.VerdiSolgt <> 0 THEN ROUND(TT_StLager.DbKr / TT_StLager.VerdiSolgt * 100,1)
                                      ELSE 0
               TT_StLager.Solgt%    = IF dSolgtTot <> 0 THEN ROUND(TT_StLager.VerdiSolgt / dSolgtTot * 100,1)
                                       ELSE 0
               TT_StLager.DBandel%  = IF dDBTot <> 0 THEN ROUND(TT_StLager.DbKr / dDBTot * 100,1)
                                      ELSE 0
               TT_StLager.Rabandel% = IF dRabTot <> 0 THEN ROUND(TT_StLager.VerdiRabatt / dRabTot * 100,1)
                                      ELSE 0
               TT_StLager.Kjopandel% = IF dKjopTot <> 0 THEN ROUND(TT_StLager.KjopVerdi / dKjopTot * 100,1)
                                      ELSE 0.

    END.
    DYNAMIC-FUNCTION('closeQuery':U).
    ASSIGN TTH = BUFFER TT_StLager:HANDLE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SummeraTT dTables  _DB-REQUIRED
PROCEDURE SummeraTT :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
ASSIGN TT_StLager.LagAnt        = TT_StLager.LagAnt        + TT_StLagerTMP.LagAnt       
       TT_StLager.AntSolgt      = TT_StLager.AntSolgt      + TT_StLagerTMP.AntSolgt     
       TT_StLager.VerdiSolgt    = TT_StLager.VerdiSolgt    + TT_StLagerTMP.VerdiSolgt   
       TT_StLager.AntRab        = TT_StLager.AntRab        + TT_StLagerTMP.AntRab
/* /**/   TT_StLager.DbKr          = TT_StLager.DbKr          + TT_StLagerTMP.VerdiSolgt - (TT_StLagerTMP.AntSolgt * TT_StLagerTMP.VVarekost) */
/**/   TT_StLager.DbKr          = TT_StLager.DbKr          + TT_StLagerTMP.VerdiSolgt - TT_StLagerTMP.Svk /*(TT_StLagerTMP.AntSolgt * TT_StLagerTMP.Nedverdi)*/
       TT_StLager.VerdiRabatt   = TT_StLager.VerdiRabatt   + TT_StLagerTMP.VerdiRabatt
    /* Här summerar vi varekosten för att få totala lagervärdet, */
    /* Innan vi kör visning delas den med LagAnt och ger VVarekost */
    /* !!! Vi har ändrat så att VVarekost innehåller totala lagervärdet */
/**/   TT_StLager.LagerVerdi    = TT_StLager.LagerVerdi    + TT_StLagerTMP.VVarekost
/* /**/   TT_StLager.LagerVerdi    = TT_StLager.LagerVerdi    + (TT_StLagerTMP.LagAnt * TT_StLagerTMP.VVarekost) */
       TT_StLager.ReklAnt       = TT_StLager.ReklAnt       + TT_StLagerTMP.ReklAnt      
       TT_StLager.ReklVerdi     = TT_StLager.ReklVerdi     + TT_StLagerTMP.ReklVerdi    
       TT_StLager.ReklLAnt      = TT_StLager.ReklLAnt      + TT_StLagerTMP.ReklLAnt     
       TT_StLager.ReklLVerdi    = TT_StLager.ReklLVerdi    + TT_StLagerTMP.ReklLVerdi   
       TT_StLager.RetLAnt       = TT_StLager.RetLAnt       + TT_StLagerTMP.RetLAnt      
       TT_StLager.SvinnAnt      = TT_StLager.SvinnAnt      + TT_StLagerTMP.SvinnAnt     
       TT_StLager.SvinnVerdi    = TT_StLager.SvinnVerdi    + TT_StLagerTMP.SvinnVerdi   
       TT_StLager.GjenkjopAnt   = TT_StLager.GjenkjopAnt   + TT_StLagerTMP.GjenkjopAnt  
       TT_StLager.GjenkjopVerdi = TT_StLager.GjenkjopVerdi + TT_StLagerTMP.GjenkjopVerdi
       TT_StLager.KjopAnt       = TT_StLager.KjopAnt       + TT_StLagerTMP.KjopAnt      
       TT_StLager.KjopVerdi     = TT_StLager.KjopVerdi     + TT_StLagerTMP.KjopVerdi    
       TT_StLager.BrekkAnt      = TT_StLager.BrekkAnt      + TT_StLagerTMP.BrekkAnt     
       TT_StLager.BrekkVerdi    = TT_StLager.BrekkVerdi    + TT_StLagerTMP.BrekkVerdi   
       TT_StLager.IntAnt        = TT_StLager.IntAnt        + TT_StLagerTMP.IntAnt       
       TT_StLager.IntVerdi      = TT_StLager.IntVerdi      + TT_StLagerTMP.IntVerdi     
       TT_StLager.JustAnt       = TT_StLager.JustAnt       + TT_StLagerTMP.JustAnt      
       TT_StLager.JustVerdi     = TT_StLager.JustVerdi     + TT_StLagerTMP.JustVerdi    
       TT_StLager.NedAnt        = TT_StLager.NedAnt        + TT_StLagerTMP.NedAnt       
       TT_StLager.NedVerdi      = TT_StLager.NedVerdi      + TT_StLagerTMP.NedVerdi     
       TT_StLager.OvAnt         = TT_StLager.OvAnt         + TT_StLagerTMP.OvAnt        
       TT_StLager.OvVerdi       = TT_StLager.OvVerdi       + TT_StLagerTMP.OvVerdi.      

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

/* ************************  Function Implementations ***************** */

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getBeskr dTables  _DB-REQUIRED
FUNCTION getBeskr RETURNS CHARACTER
  ( INPUT cStTypeId AS CHARACTER, INPUT cObjekt AS CHARACTER ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cBeskrivelse AS CHARACTER  NO-UNDO.
  CASE cStTypeId:
      WHEN "GETBUTNAVN" THEN DO:
          FIND Butiker WHERE Butiker.Butik = INT(cObjekt) NO-LOCK NO-ERROR.
          RETURN IF AVAIL Butiker THEN Butiker.Butnamn ELSE "Ukjent".
      END.
      WHEN "AVDELING" THEN DO:
          FIND Avdeling WHERE Avdeling.AvdelingNr = INT(cObjekt) NO-LOCK NO-ERROR.
          RETURN IF AVAIL Avdeling THEN Avdeling.AvdelingNavn ELSE "Ukjent".
      END.
      WHEN "HOVEDGR" THEN DO:
          FIND HuvGr WHERE HuvGr.Hg = INT(cObjekt) NO-LOCK NO-ERROR.
          RETURN IF AVAIL HuvGr THEN HuvGr.HgBeskr ELSE "Ukjent".
      END.
      WHEN "BUTSTAT" THEN DO:
          FIND Butiker WHERE Butiker.Butik = INT(cObjekt) NO-LOCK NO-ERROR.
          RETURN IF AVAIL Butiker THEN Butiker.ButNamn ELSE "Ukjent".
      END.
      WHEN "VAREGR" THEN DO:
          FIND VarGr WHERE VarGr.Vg = INT(cObjekt) NO-LOCK NO-ERROR.
          RETURN IF AVAIL VarGr THEN VarGr.VgBeskr ELSE "Ukjent".
      END.
      WHEN "LEVERAN" THEN DO:
          FIND LevBas WHERE LevBas.LevNr = INT(cObjekt) NO-LOCK NO-ERROR.
          RETURN IF AVAIL Levbas THEN LevBas.levnamn ELSE "Ukjent".
      END.
/*       WHEN "ARTIKKEL" THEN DO:                                             */
/*           FIND ArtBas WHERE ArtBas.ForsNr = INT(cObjekt) NO-LOCK NO-ERROR. */
/*           RETURN IF AVAIL Forsalj THEN Forsalj.FoNamn ELSE "Ukjent".       */
/*       END.                                                                 */
      OTHERWISE RETURN "".
  END CASE.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

