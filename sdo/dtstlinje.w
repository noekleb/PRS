&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
/* Connected Databases 
          skotex           PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
{adecomm/appserv.i}


/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE tStLinje NO-UNDO LIKE StLinje.



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

DEF VAR cStTypeId AS CHAR NO-UNDO.

def VAR wDataObjekt as char no-undo.
def VAR wPerId      as char no-undo.
def VAR wStTypeId   as char no-undo.
DEF VAR wNullstill  as LOG  NO-UNDO.
DEF VAR wKriterier  as CHAR NO-UNDO.

DEF var wAAr1       as int  NO-UNDO.
DEF var wAAr2       as int  NO-UNDO.
DEF var wPerLin1    as int  NO-UNDO.
DEF var wPerLin2    as int  NO-UNDO.
DEF VAR wButik      as INT  NO-UNDO.
DEF VAR wPeriodeTot as log  NO-UNDO.
DEF VAR wTidsavgr   as LOG  NO-UNDO.
DEF VAR wSettLager  as LOG  NO-UNDO.
DEF VAR wHg         as INT  NO-UNDO.
DEF VAR wStartTid   as int  NO-UNDO.
DEF VAR wLoop1      as INT  NO-UNDO.
DEF VAR wLoop2      as INT  NO-UNDO.
DEF VAR wFra        as INT  NO-UNDO.
DEF VAR wTil        as INT  NO-UNDO.
DEF VAR wWeekNum    as INT  NO-UNDO.
DEF VAR wLayout     as CHAR NO-UNDO.
DEF VAR wCL         AS INT  NO-UNDO.

DEF VAR wBevegelse  as DEC  NO-UNDO.
DEF VAR wLagerVerdi as DEC  NO-UNDO.

DEF BUFFER ttStLinje FOR tStLinje.

DEF STREAM Ut.

{htmlwrapperdef.i}

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
&Scoped-define INTERNAL-TABLES tStLinje

/* Definitions for QUERY Query-Main                                     */
&Scoped-Define ENABLED-FIELDS  Aar AntRabatt AntSolgt AntTilbSolgt Beskrivelse BrekkAnt BrekkVerdi~
 BrukerID Butik DataObjekt Db% DbKr Diverse DiverseAnt Diverseverdi EDato~
 ETid GjenkjopAnt GjenkjopVerdi Hg IntAnt IntVerdi JustAnt JustVerdi KjopAnt~
 KjopVerdi LagerAnt LagerVerdi MvaVerdi NedAnt NedVerdi OmlHast OvAnt~
 OvVerdi PerId PerLinNr PerLinTxt PrimoAnt Primoverdi RegistrertAv~
 RegistrertDato RegistrertTid ReklAnt ReklLAnt ReklLVerdi ReklVerdi StTypeId~
 SvinnAnt SvinnVerdi TilbMvaVerdi TilbVVarekost Utsolgt% VerdiRabatt~
 VerdiSolgt VerdiTilbSolgt VisBut VVarekost TotalPost
&Scoped-define ENABLED-FIELDS-IN-tStLinje Aar AntRabatt AntSolgt ~
AntTilbSolgt Beskrivelse BrekkAnt BrekkVerdi BrukerID Butik DataObjekt Db% ~
DbKr Diverse DiverseAnt Diverseverdi EDato ETid GjenkjopAnt GjenkjopVerdi ~
Hg IntAnt IntVerdi JustAnt JustVerdi KjopAnt KjopVerdi LagerAnt LagerVerdi ~
MvaVerdi NedAnt NedVerdi OmlHast OvAnt OvVerdi PerId PerLinNr PerLinTxt ~
PrimoAnt Primoverdi RegistrertAv RegistrertDato RegistrertTid ReklAnt ~
ReklLAnt ReklLVerdi ReklVerdi StTypeId SvinnAnt SvinnVerdi TilbMvaVerdi ~
TilbVVarekost Utsolgt% VerdiRabatt VerdiSolgt VerdiTilbSolgt VisBut ~
VVarekost TotalPost 
&Scoped-Define DATA-FIELDS  Aar AntRabatt AntSolgt AntTilbSolgt Beskrivelse BrekkAnt BrekkVerdi~
 BrukerID Butik DataObjekt Db% DbKr Diverse DiverseAnt Diverseverdi EDato~
 ETid GjenkjopAnt GjenkjopVerdi Hg IntAnt IntVerdi JustAnt JustVerdi KjopAnt~
 KjopVerdi LagerAnt LagerVerdi MvaVerdi NedAnt NedVerdi OmlHast OvAnt~
 OvVerdi PerId PerLinNr PerLinTxt PrimoAnt Primoverdi RegistrertAv~
 RegistrertDato RegistrertTid ReklAnt ReklLAnt ReklLVerdi ReklVerdi StTypeId~
 SvinnAnt SvinnVerdi TilbMvaVerdi TilbVVarekost Utsolgt% VerdiRabatt~
 VerdiSolgt VerdiTilbSolgt VisBut VVarekost TotalPost
&Scoped-define DATA-FIELDS-IN-tStLinje Aar AntRabatt AntSolgt AntTilbSolgt ~
Beskrivelse BrekkAnt BrekkVerdi BrukerID Butik DataObjekt Db% DbKr Diverse ~
DiverseAnt Diverseverdi EDato ETid GjenkjopAnt GjenkjopVerdi Hg IntAnt ~
IntVerdi JustAnt JustVerdi KjopAnt KjopVerdi LagerAnt LagerVerdi MvaVerdi ~
NedAnt NedVerdi OmlHast OvAnt OvVerdi PerId PerLinNr PerLinTxt PrimoAnt ~
Primoverdi RegistrertAv RegistrertDato RegistrertTid ReklAnt ReklLAnt ~
ReklLVerdi ReklVerdi StTypeId SvinnAnt SvinnVerdi TilbMvaVerdi ~
TilbVVarekost Utsolgt% VerdiRabatt VerdiSolgt VerdiTilbSolgt VisBut ~
VVarekost TotalPost 
&Scoped-Define MANDATORY-FIELDS  PerId StTypeId
&Scoped-Define APPLICATION-SERVICE 
&Scoped-Define ASSIGN-LIST 
&Scoped-Define DATA-FIELD-DEFS "dtstlinje.i"
&Scoped-define QUERY-STRING-Query-Main FOR EACH tStLinje NO-LOCK ~
    BY tStLinje.DataObjekt ~
       BY tStLinje.StTypeId ~
        BY tStLinje.PerId ~
         BY tStLinje.Butik ~
          BY tStLinje.Aar ~
           BY tStLinje.PerLinNr INDEXED-REPOSITION
{&DB-REQUIRED-START}
&Scoped-define OPEN-QUERY-Query-Main OPEN QUERY Query-Main FOR EACH tStLinje NO-LOCK ~
    BY tStLinje.DataObjekt ~
       BY tStLinje.StTypeId ~
        BY tStLinje.PerId ~
         BY tStLinje.Butik ~
          BY tStLinje.Aar ~
           BY tStLinje.PerLinNr INDEXED-REPOSITION.
{&DB-REQUIRED-END}
&Scoped-define TABLES-IN-QUERY-Query-Main tStLinje
&Scoped-define FIRST-TABLE-IN-QUERY-Query-Main tStLinje


/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

{&DB-REQUIRED-START}

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Query-Main FOR 
      tStLinje SCROLLING.
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
      TABLE: tStLinje T "?" NO-UNDO SkoTex StLinje
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
{dproclibstart.i}
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
     _TblList          = "Temp-Tables.tStLinje"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _OrdList          = "Temp-Tables.tStLinje.DataObjekt|yes,Temp-Tables.tStLinje.StTypeId|yes,Temp-Tables.tStLinje.PerId|yes,Temp-Tables.tStLinje.Butik|yes,Temp-Tables.tStLinje.Aar|yes,Temp-Tables.tStLinje.PerLinNr|yes"
     _FldNameList[1]   > Temp-Tables.tStLinje.Aar
"Aar" "Aar" ? ? "integer" ? ? ? ? ? ? yes ? no 4.8 no ""
     _FldNameList[2]   > Temp-Tables.tStLinje.AntRabatt
"AntRabatt" "AntRabatt" ? ? "decimal" ? ? ? ? ? ? yes ? no 11.4 no ""
     _FldNameList[3]   > Temp-Tables.tStLinje.AntSolgt
"AntSolgt" "AntSolgt" ? ? "decimal" ? ? ? ? ? ? yes ? no 11.4 no ""
     _FldNameList[4]   > Temp-Tables.tStLinje.AntTilbSolgt
"AntTilbSolgt" "AntTilbSolgt" ? ? "decimal" ? ? ? ? ? ? yes ? no 11.4 no ""
     _FldNameList[5]   > Temp-Tables.tStLinje.Beskrivelse
"Beskrivelse" "Beskrivelse" ? ? "character" ? ? ? ? ? ? yes ? no 30 no ""
     _FldNameList[6]   > Temp-Tables.tStLinje.BrekkAnt
"BrekkAnt" "BrekkAnt" ? ? "decimal" ? ? ? ? ? ? yes ? no 9.4 no ""
     _FldNameList[7]   > Temp-Tables.tStLinje.BrekkVerdi
"BrekkVerdi" "BrekkVerdi" ? "-zz,zzz,zz9" "decimal" ? ? ? ? ? ? yes ? no 16.4 no ""
     _FldNameList[8]   > Temp-Tables.tStLinje.BrukerID
"BrukerID" "BrukerID" ? ? "character" ? ? ? ? ? ? yes ? no 10 no ""
     _FldNameList[9]   > Temp-Tables.tStLinje.Butik
"Butik" "Butik" ? ">>>>>>9" "integer" ? ? ? ? ? ? yes ? no 8.4 no ""
     _FldNameList[10]   > Temp-Tables.tStLinje.DataObjekt
"DataObjekt" "DataObjekt" ? ? "character" ? ? ? ? ? ? yes ? no 15 no ""
     _FldNameList[11]   > Temp-Tables.tStLinje.Db%
"Db%" "Db%" ? ? "decimal" ? ? ? ? ? ? yes ? no 7.8 no ""
     _FldNameList[12]   > Temp-Tables.tStLinje.DbKr
"DbKr" "DbKr" ? ? "decimal" ? ? ? ? ? ? yes ? no 10 no ""
     _FldNameList[13]   > Temp-Tables.tStLinje.Diverse
"Diverse" "Diverse" ? ? "character" ? ? ? ? ? ? yes ? no 15 no ""
     _FldNameList[14]   > Temp-Tables.tStLinje.DiverseAnt
"DiverseAnt" "DiverseAnt" ? ? "decimal" ? ? ? ? ? ? yes ? no 12.6 no ""
     _FldNameList[15]   > Temp-Tables.tStLinje.Diverseverdi
"Diverseverdi" "Diverseverdi" ? "-zz,zzz,zz9" "decimal" ? ? ? ? ? ? yes ? no 12.6 no ""
     _FldNameList[16]   > Temp-Tables.tStLinje.EDato
"EDato" "EDato" ? ? "date" ? ? ? ? ? ? yes ? no 11.6 no ""
     _FldNameList[17]   > Temp-Tables.tStLinje.ETid
"ETid" "ETid" ? ? "integer" ? ? ? ? ? ? yes ? no 10.2 no ""
     _FldNameList[18]   > Temp-Tables.tStLinje.GjenkjopAnt
"GjenkjopAnt" "GjenkjopAnt" ? ? "decimal" ? ? ? ? ? ? yes ? no 10.4 no ""
     _FldNameList[19]   > Temp-Tables.tStLinje.GjenkjopVerdi
"GjenkjopVerdi" "GjenkjopVerdi" ? "-zz,zzz,zz9" "decimal" ? ? ? ? ? ? yes ? no 23.6 no ""
     _FldNameList[20]   > Temp-Tables.tStLinje.Hg
"Hg" "Hg" ? ? "integer" ? ? ? ? ? ? yes ? no 4.8 no ""
     _FldNameList[21]   > Temp-Tables.tStLinje.IntAnt
"IntAnt" "IntAnt" ? ? "decimal" ? ? ? ? ? ? yes ? no 13.2 no ""
     _FldNameList[22]   > Temp-Tables.tStLinje.IntVerdi
"IntVerdi" "IntVerdi" ? "-zz,zzz,zz9" "decimal" ? ? ? ? ? ? yes ? no 21.4 no ""
     _FldNameList[23]   > Temp-Tables.tStLinje.JustAnt
"JustAnt" "JustAnt" ? ? "decimal" ? ? ? ? ? ? yes ? no 11.8 no ""
     _FldNameList[24]   > Temp-Tables.tStLinje.JustVerdi
"JustVerdi" "JustVerdi" ? "-zz,zzz,zz9" "decimal" ? ? ? ? ? ? yes ? no 14.4 no ""
     _FldNameList[25]   > Temp-Tables.tStLinje.KjopAnt
"KjopAnt" "KjopAnt" ? ? "decimal" ? ? ? ? ? ? yes ? no 13.2 no ""
     _FldNameList[26]   > Temp-Tables.tStLinje.KjopVerdi
"KjopVerdi" "KjopVerdi" ? "-zz,zzz,zz9" "decimal" ? ? ? ? ? ? yes ? no 13 no ""
     _FldNameList[27]   > Temp-Tables.tStLinje.LagerAnt
"LagerAnt" "LagerAnt" ? ? "integer" ? ? ? ? ? ? yes ? no 10.2 no ""
     _FldNameList[28]   > Temp-Tables.tStLinje.LagerVerdi
"LagerVerdi" "LagerVerdi" ? "-zz,zzz,zz9" "decimal" ? ? ? ? ? ? yes ? no 15.6 no ""
     _FldNameList[29]   > Temp-Tables.tStLinje.MvaVerdi
"MvaVerdi" "MvaVerdi" ? "-zz,zzz,zz9" "decimal" ? ? ? ? ? ? yes ? no 13 no ""
     _FldNameList[30]   > Temp-Tables.tStLinje.NedAnt
"NedAnt" "NedAnt" ? ? "decimal" ? ? ? ? ? ? yes ? no 16.6 no ""
     _FldNameList[31]   > Temp-Tables.tStLinje.NedVerdi
"NedVerdi" "NedVerdi" ? "-zz,zzz,zz9" "decimal" ? ? ? ? ? ? yes ? no 16 no ""
     _FldNameList[32]   > Temp-Tables.tStLinje.OmlHast
"OmlHast" "OmlHast" ? ? "decimal" ? ? ? ? ? ? yes ? no 10.2 no ""
     _FldNameList[33]   > Temp-Tables.tStLinje.OvAnt
"OvAnt" "OvAnt" ? ? "decimal" ? ? ? ? ? ? yes ? no 13.2 no ""
     _FldNameList[34]   > Temp-Tables.tStLinje.OvVerdi
"OvVerdi" "OvVerdi" ? "-zz,zzz,zz9" "decimal" ? ? ? ? ? ? yes ? no 22.2 no ""
     _FldNameList[35]   > Temp-Tables.tStLinje.PerId
"PerId" "PerId" ? ? "character" ? ? ? ? ? ? yes ? yes 10 no ""
     _FldNameList[36]   > Temp-Tables.tStLinje.PerLinNr
"PerLinNr" "PerLinNr" ? ">>>>>>>>>9" "integer" ? ? ? ? ? ? yes ? no 12 no ""
     _FldNameList[37]   > Temp-Tables.tStLinje.PerLinTxt
"PerLinTxt" "PerLinTxt" ? ? "character" ? ? ? ? ? ? yes ? no 20 no ""
     _FldNameList[38]   > Temp-Tables.tStLinje.PrimoAnt
"PrimoAnt" "PrimoAnt" ? ? "integer" ? ? ? ? ? ? yes ? no 10.2 no ""
     _FldNameList[39]   > Temp-Tables.tStLinje.Primoverdi
"Primoverdi" "Primoverdi" ? "-zz,zzz,zz9" "decimal" ? ? ? ? ? ? yes ? no 15.6 no ""
     _FldNameList[40]   > Temp-Tables.tStLinje.RegistrertAv
"RegistrertAv" "RegistrertAv" ? ? "character" ? ? ? ? ? ? yes ? no 10 no ""
     _FldNameList[41]   > Temp-Tables.tStLinje.RegistrertDato
"RegistrertDato" "RegistrertDato" ? ? "date" ? ? ? ? ? ? yes ? no 11.6 no ""
     _FldNameList[42]   > Temp-Tables.tStLinje.RegistrertTid
"RegistrertTid" "RegistrertTid" ? ? "integer" ? ? ? ? ? ? yes ? no 20.8 no ""
     _FldNameList[43]   > Temp-Tables.tStLinje.ReklAnt
"ReklAnt" "ReklAnt" ? ? "decimal" ? ? ? ? ? ? yes ? no 8.2 no ""
     _FldNameList[44]   > Temp-Tables.tStLinje.ReklLAnt
"ReklLAnt" "ReklLAnt" ? ? "decimal" ? ? ? ? ? ? yes ? no 13.4 no ""
     _FldNameList[45]   > Temp-Tables.tStLinje.ReklLVerdi
"ReklLVerdi" "ReklLVerdi" ? "-zz,zzz,zz9" "decimal" ? ? ? ? ? ? yes ? no 32.6 no ""
     _FldNameList[46]   > Temp-Tables.tStLinje.ReklVerdi
"ReklVerdi" "ReklVerdi" ? "-zz,zzz,zz9" "decimal" ? ? ? ? ? ? yes ? no 24.4 no ""
     _FldNameList[47]   > Temp-Tables.tStLinje.StTypeId
"StTypeId" "StTypeId" ? ? "character" ? ? ? ? ? ? yes ? yes 10 no ""
     _FldNameList[48]   > Temp-Tables.tStLinje.SvinnAnt
"SvinnAnt" "SvinnAnt" ? ? "decimal" ? ? ? ? ? ? yes ? no 10.8 no ""
     _FldNameList[49]   > Temp-Tables.tStLinje.SvinnVerdi
"SvinnVerdi" "SvinnVerdi" ? "-zz,zzz,zz9" "decimal" ? ? ? ? ? ? yes ? no 14.4 no ""
     _FldNameList[50]   > Temp-Tables.tStLinje.TilbMvaVerdi
"TilbMvaVerdi" "TilbMvaVerdi" ? "-zz,zzz,zz9" "decimal" ? ? ? ? ? ? yes ? no 13.4 no ""
     _FldNameList[51]   > Temp-Tables.tStLinje.TilbVVarekost
"TilbVVarekost" "TilbVVarekost" ? "-zz,zzz,zz9" "decimal" ? ? ? ? ? ? yes ? no 13.2 no ""
     _FldNameList[52]   > Temp-Tables.tStLinje.Utsolgt%
"Utsolgt%" "Utsolgt%" ? ? "decimal" ? ? ? ? ? ? yes ? no 8.2 no ""
     _FldNameList[53]   > Temp-Tables.tStLinje.VerdiRabatt
"VerdiRabatt" "VerdiRabatt" ? "-zz,zzz,zz9" "decimal" ? ? ? ? ? ? yes ? no 13 no ""
     _FldNameList[54]   > Temp-Tables.tStLinje.VerdiSolgt
"VerdiSolgt" "VerdiSolgt" ? "-zz,zzz,zz9" "decimal" ? ? ? ? ? ? yes ? no 10 no ""
     _FldNameList[55]   > Temp-Tables.tStLinje.VerdiTilbSolgt
"VerdiTilbSolgt" "VerdiTilbSolgt" ? "-zz,zzz,zz9" "decimal" ? ? ? ? ? ? yes ? no 13 no ""
     _FldNameList[56]   > Temp-Tables.tStLinje.VisBut
"VisBut" "VisBut" ? ? "character" ? ? ? ? ? ? yes ? no 7 no ""
     _FldNameList[57]   > Temp-Tables.tStLinje.VVarekost
"VVarekost" "VVarekost" ? "-zz,zzz,zz9" "decimal" ? ? ? ? ? ? yes ? no 12 no ""
     _FldNameList[58]   > Temp-Tables.tStLinje.TotalPost
"TotalPost" "TotalPost" ? ? "integer" ? ? ? ? ? ? yes ? no 9 no ""
     _Design-Parent    is WINDOW dTables @ ( 1.14 , 2.6 )
*/  /* QUERY Query-Main */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK dTables 


/* ***************************  Main Block  *************************** */
  {syspara.i 5 1 1 wCL INT}

  &IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
    RUN initializeObject.
  &ENDIF

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ByggTabell dTables  _DB-REQUIRED
PROCEDURE ByggTabell :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

IF wKriterier = "" THEN
    RETURN.

/* Flagger setting av inn/utgående lager.                           */
/* Layout som ikke viser lager, skal ikke kjøre beregning av lager. */
IF NUM-ENTRIES(wKriterier) >= 9 then
DO:
  IF CAN-DO("Nei,250,301,401,501,601,701",ENTRY(9,wKriterier)) then
    assign
      wSettLager = FALSE.
  else
    assign
      wSettLager = TRUE.
END.
ELSE
  assign
    wSettLager = TRUE.
/* Disse skal ikke ha visning av lager. */
IF can-do("KUNDSTAT,MEDLEM,MEDLEMTOT,SELGERSTAT,SELGER",wStTypeId) THEN
    wSettLager = FALSE.

/* Flagger bygging av periodetotaler. */
/* Default verdi er Ja.               */
/* 0-Nei, 1-Ja                        */
IF num-entries(wKriterier) >= 7 then
    wPeriodeTot   = if INT(ENTRY(7,wKriterier)) = 1
                      THEN true
                      ELSE FALSE.
ELSE wPeriodeTot = TRUE.
/* Flagger at avgrensing skal benyttes */
/* Default verdi er Nei                */
/* 0-Nei, 1-Ja                         */
IF num-entries(wKriterier) >= 8 then
    wTidsavgr   = if INT(ENTRY(8,wKriterier)) = 1
                      THEN true
                      ELSE FALSE.
ELSE wTidsavgr = false.

/* Ekstra info. Sender med varegruppens HG n†r HG stat skal plukkes ut. */
IF num-entries(wKriterier) >= 6 then
  wHg = INT(ENTRY(6,wKriterier)).
ELSE wHg = 0.

/* Nullstiller. */
IF wNullstill then
  DO:
    EMPTY TEMP-TABLE tStLinje  NO-ERROR.
    EMPTY TEMP-TABLE ttStLinje NO-ERROR.
  END.

/* Leser avgrensinger hvis avgrensing skal benyttes. */
/* NB: Dette gjøres om igjen etter byggstlinje.i.    */
IF wTidsavgr then
  assign
    wAAr1    = INT(ENTRY(1,wKriterier))
    wAAr2    = INT(ENTRY(2,wKriterier))
    wPerLin1 = INT(ENTRY(3,wKriterier))
    wPerLin2 = INT(ENTRY(4,wKriterier))
    wButik   = INT(ENTRY(5,wKriterier)).

/*
MESSAGE "Byggtabell i dtstlinje" SKIP
    "StTypeId:" wStTypeId SKIP
    "PeriodeId:" wPerId SKIP
    "Dataobjekt:" wDataObjekt SKIP
    "År:" wAAr1 wAAr2   SKIP
    "Periodelinje:" wPerLin1 wPerLin2 SKIP
    "Butikk:" wButik SKIP  
    "Periodetotal:" wPeriodeTot SKIP
    "Kriterier:" wKriterier SKIP
    "Settlager:" wSettLager SKIP
    "Tidsavgrensing:" wTidsavgr SKIP
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
*/

/* Bygger temp-table.                                */
/* Kopierer alle postene fra StLinje inn i tStLinje. */
/* Kopieres her ved hjelp av Buffer-Copy.            */
IF wButik = 0 then {byggstlinje.i &Butikk = " "}
else {byggstlinje.i &Butikk = "and stLinje.Butik = wButik"}

/* Frisker opp igjen fordi variablene er blitt endret i byggstlinje.i */
IF wTidsavgr then
  assign
    wAAr1    = INT(ENTRY(1,wKriterier))
    wAAr2    = INT(ENTRY(2,wKriterier))
    wPerLin1 = INT(ENTRY(3,wKriterier))
    wPerLin2 = INT(ENTRY(4,wKriterier))
    wButik   = INT(ENTRY(5,wKriterier)).

/* For å få med alle artikkler, må det opprettes en tom post innenfor */
/* den perioden det spørres om.                                       */
/* FIX for å få med post på utskrift.                                 */
IF wTidsAvgr THEN
DO:
  IF wButik = 0 THEN
  DO:
    IF NOT CAN-FIND(FIRST tStLinje where
                    tStLinje.DataObjekt = wDataObjekt and
                    tStLinje.StTypeId   = wStTypeId and
                    tStLinje.PerId      = wPerId and
                    tStLinje.AAr        = wAAr2 and
                    tStLinje.PerLinNr   = wPerLin2 AND
                    tStLinje.Diverse    = "") THEN
    DO:
      CREATE tStLinje.
      assign
        tStLinje.StTypeId   = wStTypeId 
        tStLinje.PerId      = wPerId 
        tStLinje.DataObjekt = wDataObjekt 
        tStLinje.Diverse    = ""
        tStLinje.Butik      = wCL
        tStLinje.Aar        = wAAr2
        tStLinje.PerLinNr   = wPerLin2.      
    END.
  END.
  else
  DO:
    IF NOT CAN-FIND(FIRST tStLinje where
                    tStLinje.DataObjekt = wDataObjekt and
                    tStLinje.StTypeId   = wStTypeId and
                    tStLinje.PerId      = wPerId and
                    tStLinje.AAr        = wAAr2 and
                    tStLinje.PerLinNr   = wPerLin2 AND
                    tStLinje.Butik      = wButik AND
                    tStLinje.Diverse    = "") THEN
    DO:
      CREATE tStLinje.
      assign
        tStLinje.StTypeId   = wStTypeId 
        tStLinje.PerId      = wPerId 
        tStLinje.DataObjekt = wDataObjekt 
        tStLinje.Diverse    = ""
        tStLinje.Butik      = wButik
        tStLinje.Aar        = wAAr2
        tStLinje.PerLinNr   = wPerLin2.      
    END.
  END.

END. /* FIX for å få med post på utskrift. */

/* Legger inn utgående lager for alle perioder.             */
/* Lagerverdien "rulles" opp egjennom alle periodenlinjene. */
IF wSettLager then /* TEST */
FOR EACH tStLinje where
  tStLinje.DataObjekt = wDataObjekt and
  tStLinje.StTypeId   = wStTypeId and
  tStLinje.PerId      = wPerId and
  tStLinje.PerLinNr   < 1000000
  break
  by tStLinje.DataObjekt
  by tStLinje.StTypeId
  by tStLinje.Butik
  by tStLinje.Aar descending
  by tStLinje.PerId descending
  by tStLinje.PerLinNr descending:

  IF first-of(tStLinje.Butik) then
    RUN FinnLager.
  ELSE assign
    tStLinje.LagerAnt   = wBevegelse
    tStLinje.LagerVerdi = wLagerVerdi. /* Verdi fra forrige post. */

  assign
    wBevegelse = tStLinje.LagerAnt

                 + tStLinje.AntSolgt
                 + tStLinje.BrekkAnt
                 + tStLinje.IntAnt
                 - tStLinje.OvAnt
                   /* - tStLinje.ReklAnt  */
                 + tStLinje.ReklLAnt
                   /* - tStLinje.GjenkjopAnt */
                 - tStLinje.KjopAnt
                 - tStLinje.JustAnt
                 - tStLinje.SvinnAnt
    wLagerVerdi = tStLinje.LagerVerdi

                 + tStLinje.VVareKost
                 + tStLinje.BrekkVerdi
                 + tStLinje.IntVerdi
                 - tStLinje.OvVerdi
                   /* - tStLinje.ReklVerdi  */
                 + tStLinje.ReklLAnt
                   /* - tStLinje.GjenkjopVerdi */
                 - tStLinje.KjopVerdi
                 - tStLinje.JustVerdi
                 - tStLinje.SvinnVerdi
    tStLinje.PrimoAnt     = wBevegelse
    tStLinje.PrimoVerdi   = wLagerVerdi
    tStLinje.OmlHast      = tStLinje.AntSolgt / ((tStLinje.LagerAnt + tStLinje.KjopAnt + tStLinje.OvAnt) / 2)
    tStLinje.OmlHast      = IF tStLinje.OmlHast = ? THEN 0 ELSE tStLinje.OmlHast
    tStLinje.DiverseAnt   =   tStLinje.SvinnAnt
                            + tStLinje.JustAnt
                            + tStLinje.IntAnt
                            + tStLinje.BrekkAnt
    tStLinje.DiverseVerdi =   tStLinje.SvinnVerdi
                            + tStLinje.JustVerdi
                            + tStLinje.IntVerdi
                            + tStLinje.BrekkVerdi
    .
END.

RUN Tidsavgr.
RUN PeriodeTot.
RUN G-Total.
RUN PeriodeTotal.
RUN KalkFelt.

/* Renser bort butikk 0. */
FOR EACH tStLinje WHERE
    tStLinje.Butik = 0:
    DELETE tStLinje.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE dataAvailable dTables  _DB-REQUIRED
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
  IF wDataObjekt <> DYNAMIC-FUNCTION('getForeignValues':U) THEN
  DO:
      RUN GenStLinje.
      DYNAMIC-FUNCTION('openQuery':U). 
  END.

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

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE EksporterTabell dTables  _DB-REQUIRED
PROCEDURE EksporterTabell :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR wLabel as CHAR.

  OUTPUT to t.sdv.

  EXPORT DELIMITER ";"
    "DataObjekt"
    "StTypeId"
    "Beskrivelse"
    "PerId"
    "Aar"
    "PerLinNr"
    "Butik"

    "KjopAnt"
    "KjopVerdi"
    "AntSolgt"
    "VerdiSolgt"
    "VVarekost"
    "LagerAnt"
    "LagerVerdi"
    "BrekkAnt"
    "IntAnt"
    "ReklAnt"
    "ReklLAnt"
    "GjenkjopAnt"
    "OvAnt"
    "JustAnt"
    "JustVerdi"
    "SvinnAnt"
    "SvinnVerdi"
    "NedAnt"
    "NedVerdi"
    "BrekkVerdi"
    "IntVerdi"
    "ReklVerdi"
    "ReklLVerdi"
    "GjenkjopVerdi"
    "OvVerdi"
    "MvaVerdi"
    "Diverse"
    "AntTilbSolgt"
    "VerdiTilbSolgt"
    "TilbVVarekost"
    "TilbMvaVerdi"
    "AntRabatt"
    "VerdiRabatt"
    "PrimoAnt"
    "OmlHast"
    "Hg"
    "But"
    "DbKr"
    "Utsolgt%"
    "DiverseAnt"
    "DiverseVerdi"
    "EDato"
    "ETid"
    "BrukerID"
    "RegistrertDato"
    "RegistrertTid"
    "RegistrertAv"
    .

  FOR EACH tStLinje NO-LOCK:
    EXPORT DELIMITER ";"
    tStLinje.DataObjekt
    tStLinje.StTypeId
    tStLinje.Beskrivelse
    tStLinje.PerId
    tStLinje.Aar
    tStLinje.PerLinNr
    tStLinje.Butik

    tStLinje.KjopAnt
    tStLinje.KjopVerdi
    tStLinje.AntSolgt
    tStLinje.VerdiSolgt
    tStLinje.VVarekost
    tStLinje.LagerAnt
    tStLinje.LagerVerdi
    tStLinje.BrekkAnt
    tStLinje.IntAnt
    tStLinje.ReklAnt
    tStLinje.ReklLAnt
    tStLinje.GjenkjopAnt
    tStLinje.OvAnt
    tStLinje.JustAnt
    tStLinje.JustVerdi
    tStLinje.SvinnAnt
    tStLinje.SvinnVerdi
    tStLinje.NedAnt
    tStLinje.NedVerdi
    tStLinje.BrekkVerdi
    tStLinje.IntVerdi
    tStLinje.ReklVerdi
    tStLinje.ReklLVerdi
    tStLinje.GjenkjopVerdi
    tStLinje.OvVerdi
    tStLinje.MvaVerdi
    tStLinje.Diverse
    tStLinje.AntTilbSolgt
    tStLinje.VerdiTilbSolgt
    tStLinje.TilbVVarekost
    tStLinje.TilbMvaVerdi
    tStLinje.AntRabatt
    tStLinje.VerdiRabatt
    tStLinje.PrimoAnt
    tStLinje.OmlHast
    tStLinje.Hg
    tStLinje.But
    tStLinje.DbKr
    tStLinje.Utsolgt%
    tStLinje.DiverseAnt
    tStLinje.DiverseVerdi
    tStLinje.EDato
    tStLinje.ETid
    tStLinje.BrukerID
    tStLinje.RegistrertDato
    tStLinje.RegistrertTid
    tStLinje.RegistrertAv
    .
  END.
  OUTPUT CLOSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE FinnLager dTables  _DB-REQUIRED
PROCEDURE FinnLager :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  case wStTypeId:
    when "ARTIKKEL" then
      /* ARTIKKEL */
      do:
        FIND Lager NO-LOCK where
          Lager.ArtikkelNr = INT(tStLinje.DataObjekt) and
          Lager.Butik      = tStLinje.Butik NO-ERROR.
        IF AVAILABLE Lager then
          assign
            tStLinje.LagerAnt   = Lager.LagAnt
            wBevegelse          = Lager.LagAnt
            tStLinje.LagerVerdi = Lager.LagAnt * Lager.VVareKost
            wLagerVerdi         = Lager.LagAnt * Lager.VVareKost.
        else
          assign
            tStLinje.LagerAnt   = 0
            wBevegelse          = 0
            tStLinje.LagerVerdi = 0
            wLagerVerdi         = 0.
/*
MESSAGE "Butikk:" tStLinje.Butik skip
        "StLinje.LAgerAnt:" tStLinje.LagerAnt skip
        "wBevegelse:" wBevegelse skip
        "StLinje.LAgerVerdi:" tStLinje.LagerVerdi skip
        "sLAgerVerdi:" wLagerVerdi VIEW-AS ALERT-BOX.
*/

      end. /* ARTIKKEL */
    when "KUNDSTAT" then
      do:
        assign
          tStLinje.LagerAnt   = 0
          wBevegelse          = 0
          tStLinje.LagerVerdi = 0
          wLagerVerdi         = 0.
      end. /* KUNDSTAT */
    when "SELGERSTAT" then
      do:
        assign
          tStLinje.LagerAnt   = 0
          wBevegelse          = 0
          tStLinje.LagerVerdi = 0
          wLagerVerdi         = 0.
      end. /* SELGERSTAT */
    when "SELGER" then
      do:
        assign
          tStLinje.LagerAnt   = 0
          wBevegelse          = 0
          tStLinje.LagerVerdi = 0
          wLagerVerdi         = 0.
      end. /* SELGERSTAT */
    OTHERWISE DO: /* "VAREGR","LEVERAN","HOVEDGR","BUTSTAT"  */
        FIND StLager NO-LOCK where
          StLager.StTypeId   = wStTypeId and
          StLager.DataObjekt = tStLinje.DataObjekt and
          StLager.Butik      = tStLinje.Butik NO-ERROR.
        IF AVAILABLE StLager then
          assign
            tStLinje.LagerAnt   = StLager.LagAnt
            wBevegelse          = StLager.LagAnt
            tStLinje.LagerVerdi = StLager.LagAnt * StLager.VVareKost
            wLagerVerdi         = StLager.LagAnt * StLager.VVareKost.
        else
          assign
            tStLinje.LagerAnt   = 0
            wBevegelse          = 0
            tStLinje.LagerVerdi = 0
            wLagerVerdi         = 0.
    END.
  end case. /* STATISTIKKTYPE */    

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE G-Total dTables  _DB-REQUIRED
PROCEDURE G-Total :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/* Genererer G-Total pr. periodeID - Utføres kun for ALLE butikker.*/
IF (wButik = 0 and wPeriodeTot) then
FOR EACH tStLinje where
  tStLinje.DataObjekt = wDataObjekt and
  tStLinje.StTypeId   = wStTypeId and
  tStLinje.PerId      = wPerId and
  tStLinje.PerLinNr   < 1000000
  break
  by tStLinje.DataObjekt
  by tStLinje.StTypeId
  by tStLinje.Aar descending
  by tStLinje.PerId descending
  by tStLinje.PerLinNr descending
  by tStLinje.Butik DESCENDING:

  IF FIRST-of(tStLinje.PerLinNr) then
    DO:
      FIND FIRST ttStLinje where
                 ttStLinje.DataObjekt = wDataObjekt and
                 ttStLinje.StTypeId   = wStTypeId   and
                 ttStLinje.PerId      = wPerId      and
                 ttStLinje.butik      = 9999998      and
                 ttStLinje.PerLinNr   = tStLinje.PerLinNr and
                 ttStLinje.Aar        = tStLinje.Aar no-error.
      IF AVAILABLE ttStLinje then
        DELETE ttStLinje.
      CREATE ttStLinje.
      assign
        ttStLinje.DataObjekt = wDataObjekt
        ttStLinje.StTypeId   = wStTypeId
        ttStLinje.PerId      = wPerId
        ttStLinje.butik      = 9999998
        ttStLinje.PerLinNr   = tStLinje.PerLinNr
        ttStLinje.Aar        = tStLinje.Aar
        ttStLinje.Hg         = tStLinje.Hg
        ttStLinje.TotalPost  = 1
        .

    END.

  IF AVAILABLE ttStLinje then
  ASSIGN
    ttStLinje.LagerAnt       = ttStLinje.LagerAnt       + tStLinje.LagerAnt
    ttStLinje.LagerVerdi     = ttStLinje.LagerVerdi     + tStLinje.LagerVerdi
    ttStLinje.PrimoAnt       = ttStLinje.PrimoAnt       + tStLinje.PrimoAnt
    ttStLinje.PrimoVerdi     = ttStLinje.PrimoVerdi     + tStLinje.PrimoVerdi
    ttStLinje.VVarekost      = ttStLinje.VVarekost      + tStLinje.VVareKost
    ttStLinje.AntSolgt       = ttStLinje.AntSolgt       + tStLinje.AntSolgt
    ttStLinje.AntRab         = ttStLinje.AntRab         + tStLinje.AntRab
    ttStLinje.BrekkAnt       = ttStLinje.BrekkAnt       + tStLinje.BrekkAnt
    ttStLinje.IntAnt         = ttStLinje.IntAnt         + tStLinje.IntAnt
    ttStLinje.ReklAnt        = ttStLinje.ReklAnt        + tStLinje.ReklAnt
    ttStLinje.ReklLAnt       = ttStLinje.ReklLAnt       + tStLinje.ReklLAnt
    ttStLinje.GjenkjopAnt    = ttStLinje.GjenkjopAnt    + tStLinje.GjenkjopAnt
    ttStLinje.KjopAnt        = ttStLinje.KjopAnt        + tStLinje.KjopAnt
    ttStLinje.OvAnt          = ttStLinje.OvAnt          + tStLinje.OvAnt
    ttStLinje.JustAnt        = ttStLinje.JustAnt        + tStLinje.JustAnt
    ttStLinje.JustVerdi      = ttStLinje.JustVerdi      + tStLinje.JustVerdi
    ttStLinje.SvinnAnt       = ttStLinje.SvinnAnt       + tStLinje.SvinnAnt
    ttStLinje.SvinnVerdi     = ttStLinje.SvinnVerdi     + tStLinje.SvinnVerdi
    ttStLinje.NedAnt         = ttStLinje.NedAnt         + tStLinje.NedAnt
    ttStLinje.NedVerdi       = ttStLinje.NedVerdi       + tStLinje.NedVerdi
    ttStLinje.VerdiSolgt     = ttStLinje.VerdiSolgt     + tStLinje.VerdiSolgt
    ttStLinje.VerdiRabatt    = ttStLinje.VerdiRabatt    + tStLinje.VerdiRabatt
    ttStLinje.KjopVerdi      = ttStLinje.KjopVerdi      + tStLinje.KjopVerdi
    ttStLinje.BrekkVerdi     = ttStLinje.BrekkVerdi     + tStLinje.BrekkVerdi
    ttStLinje.IntVerdi       = ttStLinje.IntVerdi       + tStLinje.IntVerdi
    ttStLinje.ReklVerdi      = ttStLinje.ReklVerdi      + tStLinje.ReklVerdi
    ttStLinje.ReklLVerdi     = ttStLinje.ReklLVerdi     + tStLinje.ReklLVerdi
    ttStLinje.GjenkjopVerdi  = ttStLinje.GjenkjopVerdi  + tStLinje.GjenkjopVerdi
    ttStLinje.OvVerdi        = ttStLinje.OvVerdi        + tStLinje.OvVerdi
    ttStLinje.MvaVerdi       = ttStLinje.MvaVerdi       + tStLinje.MvaVerdi
    ttStLinje.OmlHast        = ttStLinje.AntSolgt / ((ttStLinje.LagerAnt + ttStLinje.KjopAnt + ttStLinje.OvAnt) / 2)
    ttStLinje.OmlHast        = IF ttStLinje.OmlHast = ? THEN 0 ELSE ttStLinje.OmlHast
    ttStLinje.DiverseAnt     =   ttStLinje.SvinnAnt
                               + ttStLinje.JustAnt
                               + ttStLinje.IntAnt
                               + ttStLinje.BrekkAnt
    ttStLinje.DiverseVerdi   =   ttStLinje.SvinnVerdi
                               + ttStLinje.JustVerdi
                               + ttStLinje.IntVerdi
                               + ttStLinje.BrekkVerdi
    .
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GenStLinje dTables  _DB-REQUIRED
PROCEDURE GenStLinje :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
  
     Kriterier:
     ----------
     ÅrFra,ÅrTil,
     PerFra,PerTil,
     Butikk,          0-Alle, N-Angitt butikk
     Hg,              0
     ByggTotaler,     0-Nei, 1-Ja
     Bruk avgrensning 0-Nei, 1-Ja
     
------------------------------------------------------------------------------*/

  ASSIGN 
      wDataObjekt = DYNAMIC-FUNCTION('getForeignValues':U)
      wNullstill  = TRUE
      .

  IF wDataObjekt <> ? AND wPerId <> "" AND wStTypeId <> "" THEN
  BYGGTABELL:
  DO:
    /*
    /* Sjekker om det allerede er bygget liste. */
    IF CAN-FIND(FIRST tStLinje WHERE
                tStLinje.DataObjekt = wDataObjekt  AND
                tStLinje.PerId      = wPerId       AND
                tStLinje.StTypeId   = wStTypeId)
                 THEN
        LEAVE BYGGTABELL.
    */
    /* Bygger ny liste */
    RUN ByggTabell.
  END. /* BYGGTABELL */
  ELSE DO:
      /*
      MESSAGE "Feil i variabeloppsettet:" SKIP
          "Dataobjekt:" wDataobjekt SKIP
          "PerId:" wPerId SKIP
          "StTypeId:" wStTypeId
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      */
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Historikk dTables  _DB-REQUIRED
PROCEDURE Historikk :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  def input parameter ipDataObjekt        as char   no-undo.
  def input parameter ipPerId             as char   no-undo.
  def input parameter ipStTypeId          as char   no-undo.
  DEF INPUT PARAMETER ipNullstill         as LOG    NO-UNDO.
  DEF INPUT PARAMETER ipKriterier         as CHAR   NO-UNDO.

  assign
    wDataObjekt = ipDataObjekt
    wPerId      = ipPerId
    wStTypeId   = ipStTypeId
    wNullstill  = ipNullstill
    wKriterier  = ipKriterier.

  RUN ByggTabell.

  /* RUN EksporterTabell.*/


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE htmlstlinje dTables  _DB-REQUIRED
PROCEDURE htmlstlinje :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
&Scoped UtStream Stream Ut

DEF VAR wLinje AS CHAR NO-UNDO.
DEF VAR wLnNr  AS INT  NO-UNDO.
DEF VAR wt     AS CHAR NO-UNDO.
DEF VAR j      AS INT  NO-UNDO.
DEF VAR wNr    AS INT  NO-UNDO.
DEF VAR wHit AS LOGI NO-UNDO.
DEF VAR ii AS INTE NO-UNDO.
DEF VAR hField AS HANDLE NO-UNDO.
DEF VAR hBuffer AS HANDLE NO-UNDO.
DEFINE VAR wHead2       AS CHAR NO-UNDO.

DEFINE INPUT PARAMETER wDokTyp      AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER wSep         AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER wHead1Set    AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER wColHead     AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER wFields      AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER wFormat      AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER wColHeadForm AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER wTabell      AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER wTot         AS LOGI NO-UNDO.
DEFINE INPUT PARAMETER wtmpFileName AS CHAR NO-UNDO.

DEF    VAR pcFelt       AS CHAR NO-UNDO.
DEFINE VAR pcColValues  AS CHAR NO-UNDO.
DEFINE VAR wHead1       AS CHAR NO-UNDO.
DEFINE VAR wTitle       AS CHAR NO-UNDO.
DEFINE VAR piAntLinjer  AS INT  NO-UNDO.

DO ii = 1 TO num-entries(wFields):
    ASSIGN
        pcFelt = pcFelt + 
                 (IF pcFelt = ""
                    THEN ""
                    ELSE wSep) + 
                 ENTRY(ii,wFields).
END.

ASSIGN wTitle = "Rapport " + wTabell
       wHead1 = wTitle
       wHead2 = STRING(TODAY,"99/99/9999")
       .

Output STREAM Ut to VALUE(wtmpFileName).

IF wDokTyp = "EX" THEN DO:
    PUT STREAM Ut Unformatted IF wSep <> ";" THEN REPLACE(wColHead,wSep,";") ELSE wColHead SKIP.   
END.
ELSE DO:
    PUT STREAM Ut Unformatted
      HTML;Start (wSep,wTitle,"")
      HTML;Head1 (wHead1,ENTRY(1,wHead1Set),ENTRY(2,wHead1Set),INT(ENTRY(3,wHead1Set)),INT(ENTRY(4,wHead1Set)),INT(ENTRY(5,wHead1Set)),INT(ENTRY(6,wHead1Set)))
      HTML;Head2 (wHead2)
      HTML;ColHead (wColHead,"" /* wColHeadForm */). 
END.
ASSIGN 
    wLnNr       = 0
    piAntLinjer = 0
    .
REPEAT:
    IF piAntLinjer = 0 
    THEN RUN fetchFirst.
    ELSE RUN fetchNext.

    Assign piAntLinjer = piAntLinjer + 1
           wLnNr       = 1
           wLinje      = ""
           wLinje      = FILL(wSep,NUM-ENTRIES(wFields) - 1)
           wHit        = FALSE
           pcColValues = DYNAMIC-FUNCTION('colValues':U,INPUT wFields)
           .

    DO ii = 1 TO num-entries(wFields):
        IF ENTRY(ii,wFields) = "PerLinNr" AND int(entry(11 + 1,pcColValues,chr(1))) > 999999 THEN DO:
            IF wTot AND INT(entry(11 + 1,pcColValues,chr(1))) > 1000000 THEN 
                ASSIGN wLnNr = 2. /* ger färglagd rad */
            ELSE IF NOT wTot THEN
                ASSIGN wLnNr = 2. 
        END.
        ASSIGN wHit = TRUE
               ENTRY(ii,wLinje,wSep) = entry(ii + 1,pcColValues,chr(1)).
    END.

    IF wHit THEN
       IF wDokTyp = "EX" THEN
          PUT STREAM Ut Unformatted IF wSep <> ";" THEN REPLACE(wLinje,wSep,";") ELSE wLinje SKIP.
       ELSE
          PUT STREAM Ut Unformatted HTML;Col(wLinje, "",wLnNr).

    IF DYNAMIC-FUNCTION("getQueryPosition") = "OnlyRecord" OR 
       DYNAMIC-FUNCTION("getQueryPosition") = "LastRecord" 
    THEN LEAVE.
END.
IF wDokTyp = "HTM" THEN
  PUT STREAM Ut Unformatted
      HTML;Footer2 ("")
      HTML;Footer1 ("")
      HTML;SKIP    (20)
      HTML;END     ().

Output STREAM Ut CLOSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitVar dTables  _DB-REQUIRED
PROCEDURE InitVar :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER pcStTypeId  AS CHAR NO-UNDO.
  DEF INPUT PARAMETER pcPerId     AS CHAR NO-UNDO.
  DEF INPUT PARAMETER pT-Tot      AS LOG  NO-UNDO.
  DEF INPUT PARAMETER piButik     AS INT  NO-UNDO.
  DEF INPUT PARAMETER pcKriterier AS CHAR NO-UNDO.

  ASSIGN
      wStTypeId  = pcStTypeId
      wKriterier = pcKriterier
      wPerId     = pcPerId
      .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE KalkFelt dTables  _DB-REQUIRED
PROCEDURE KalkFelt :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
FOR EACH tStLinje where
  tStLinje.DataObjekt = wDataObjekt and
  tStLinje.StTypeId   = wStTypeId and
  tStLinje.PerId      = wPerId:

  assign
    tStLinje.VerdiSolgt = tStLinje.VerdiSolgt /*- (tStLinje.ReklVerdi + tStLinje.GjenkjopVerdi)*/
    tStLinje.DbKr       = tStLinje.VerdiSolgt - tStLinje.VVareKost
    tStLinje.Db%        = (tStLinje.DbKr * 100) / tStLinje.VerdiSolgt
    tStLinje.Db%        = if tStLinje.Db% = ? then 0 else tStLinje.Db%
    tStLinje.LagerAnt   = tStLinje.LagerAnt
    tStLinje.Utsolgt%   = (tStLinje.AntSolgt / (tStLinje.KjopAnt + tStLinje.OvAnt)) * 100
    tStLinje.Utsolgt%   = if tStLinje.Utsolgt% = ? then 0 else tStLinje.Utsolgt%
    tStLinje.OmlHast    = tStLinje.OmlHast
    tStLinje.PrimoAnt   = tStLinje.PrimoAnt
    tStLinje.VisBut     = if tStLinje.Butik <= 999999
                    then string(tStLinje.Butik)
                    else ""
    tStLinje.VisBut     = fill(" ",7 - length(tStLinje.VisBut)) + tStLinje.VisBut.
    
  /* Hånterer sumlinjer */
  if tStLinje.PerLinNr = 1999999 then
    tStLinje.PerLinTxt = "G-Tot".
  else if tStLinje.PerLinNr = 1000000 then
    tStLinje.PerLinTxt = "ButikkTot".
  else  
  case tStLinje.PerId:
    when "AAR"   then tStLinje.PerLinTxt = string(tStLinje.PerLinNr).
    when "MANED" then tStLinje.PerLinTxt = string(tStLinje.PerLinNr).
    when "UKE"   then tStLinje.PerLinTxt = string(tStLinje.PerLinNr).
    when "DAG"   then 
      do:
        find PerLin no-lock where
          PerLin.PerId    = tStLinje.PerId and
          PerLin.PerLinNr = tStLinje.PerLinNr no-error.
        if available PerLin then
          tStLinje.PerLinTxt = string(date(1,1,tStLinje.Aar) + (tStLinje.PerLinNr - 1)).
        else tStLinje.PerLinTxt = string(tStLinje.PerLinNr).
      end.
    otherwise    
      do:
        find PerLin no-lock where
          PerLin.PerId    = tStLinje.PerId and
          PerLin.PerLinNr = tStLinje.PerLinNr no-error.
        if available PerLin then
          tStLinje.PerLinTxt = string(PerLin.FraDato) + "-" + string(PerLin.TilDato).
        else tStLinje.PerLinTxt = string(tStLinje.PerLinNr).
      end.
  end case.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Kriterier dTables  _DB-REQUIRED
PROCEDURE Kriterier :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER pcKriterier AS CHAR NO-UNDO.
  DEF INPUT PARAMETER pcPerId     AS CHAR NO-UNDO.

  ASSIGN
      wKriterier = pcKriterier
      wPerId     = pcPerId
      .
  RUN GenStLinje.
  DYNAMIC-FUNCTION('openQuery':U).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PeriodeTot dTables  _DB-REQUIRED
PROCEDURE PeriodeTot :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
if wPeriodeTot then
FOR EACH tStLinje where
  tStLinje.DataObjekt = wDataObjekt and
  tStLinje.StTypeId   = wStTypeId and
  tStLinje.PerId      = wPerId and
  tStLinje.PerLinNr   < 1000000
  break
  by tStLinje.DataObjekt
  by tStLinje.StTypeId
  by tStLinje.Butik
  by tStLinje.Aar descending
  by tStLinje.PerId descending
  by tStLinje.PerLinNr descending:

  IF FIRST-OF(tStLinje.Butik) then
    DO:
      if not CAN-FIND(FIRST ttStLinje where
                 ttStLinje.DataObjekt = wDataObjekt and
                 ttStLinje.StTypeId   = wStTypeId   and
                 ttStLinje.PerId      = wPerId      and
                 ttStLinje.butik      = tStLinje.Butik  and
                 ttStLinje.PerLinNr   = 1000000     and
                 ttStLinje.Aar        = tStLinje.Aar) then
      DO:
        CREATE ttStLinje.
        assign
          ttStLinje.DataObjekt = wDataObjekt
          ttStLinje.StTypeId   = wStTypeId
          ttStLinje.PerId      = wPerId
          ttStLinje.butik      = tStLinje.Butik
          ttStLinje.PerLinNr   = 1000000
          ttStLinje.Aar        = tStLinje.Aar
          ttStLinje.Hg         = tStLinje.Hg
          ttStLinje.TotalPost  = 1
          .
      END.
    END.

  if AVAILABLE ttStLinje then
  ASSIGN
    ttStLinje.LagerAnt       = if FIRST-OF(tStLinje.butik)
                                 then tStLinje.LagerAnt
                                 ELSE ttStLinje.LagerAnt
    ttStLinje.LagerVerdi     = if FIRST-OF(tStLinje.butik)
                                 then tStLinje.LagerVerdi
                                 ELSE ttStLinje.LagerVerdi
    ttStLinje.PrimoAnt       = if LAST-OF(tStLinje.Butik)
                                 then tStLinje.PrimoAnt
                                 ELSE ttStLinje.PrimoAnt
    ttStLinje.PrimoVerdi     = if LAST-OF(tStLinje.Butik)
                                 then tStLinje.PrimoVerdi
                                 ELSE ttStLinje.PrimoVerdi
    ttStLinje.VVarekost      = ttStLinje.VVarekost      + tStLinje.VVareKost
    ttStLinje.AntSolgt       = ttStLinje.AntSolgt       + tStLinje.AntSolgt
    ttStLinje.AntRab         = ttStLinje.AntRab         + tStLinje.AntRab
    ttStLinje.BrekkAnt       = ttStLinje.BrekkAnt       + tStLinje.BrekkAnt
    ttStLinje.IntAnt         = ttStLinje.IntAnt         + tStLinje.IntAnt
    ttStLinje.ReklAnt        = ttStLinje.ReklAnt        + tStLinje.ReklAnt
    ttStLinje.ReklLAnt       = ttStLinje.ReklLAnt       + tStLinje.ReklLAnt
    ttStLinje.GjenkjopAnt    = ttStLinje.GjenkjopAnt    + tStLinje.GjenkjopAnt
    ttStLinje.KjopAnt        = ttStLinje.KjopAnt        + tStLinje.KjopAnt
    ttStLinje.OvAnt          = ttStLinje.OvAnt          + tStLinje.OvAnt
    ttStLinje.JustAnt        = ttStLinje.JustAnt        + tStLinje.JustAnt
    ttStLinje.JustVerdi      = ttStLinje.JustVerdi      + tStLinje.JustVerdi
    ttStLinje.SvinnAnt       = ttStLinje.SvinnAnt       + tStLinje.SvinnAnt
    ttStLinje.SvinnVerdi     = ttStLinje.SvinnVerdi     + tStLinje.SvinnVerdi
    ttStLinje.NedAnt         = ttStLinje.NedAnt         + tStLinje.NedAnt
    ttStLinje.NedVerdi       = ttStLinje.NedVerdi       + tStLinje.NedVerdi
    ttStLinje.VerdiSolgt     = ttStLinje.VerdiSolgt     + tStLinje.VerdiSolgt
    ttStLinje.VerdiRabatt    = ttStLinje.VerdiRabatt    + tStLinje.VerdiRabatt
    ttStLinje.KjopVerdi      = ttStLinje.KjopVerdi      + tStLinje.KjopVerdi
    ttStLinje.BrekkVerdi     = ttStLinje.BrekkVerdi     + tStLinje.BrekkVerdi
    ttStLinje.IntVerdi       = ttStLinje.IntVerdi       + tStLinje.IntVerdi
    ttStLinje.ReklVerdi      = ttStLinje.ReklVerdi      + tStLinje.ReklVerdi
    ttStLinje.ReklLVerdi     = ttStLinje.ReklLVerdi     + tStLinje.ReklLVerdi
    ttStLinje.GjenkjopVerdi  = ttStLinje.GjenkjopVerdi  + tStLinje.GjenkjopVerdi
    ttStLinje.OvVerdi        = ttStLinje.OvVerdi        + tStLinje.OvVerdi
    ttStLinje.MvaVerdi       = ttStLinje.MvaVerdi       + tStLinje.MvaVerdi
    ttStLinje.OmlHast        = ttStLinje.AntSolgt / ((ttStLinje.LagerAnt + ttStLinje.KjopAnt + ttStLinje.OvAnt) / 2)
    ttStLinje.OmlHast        = IF ttStLinje.OmlHast = ? THEN 0 ELSE ttStLinje.OmlHast
    ttStLinje.DiverseAnt     =   ttStLinje.SvinnAnt
                               + ttStLinje.JustAnt
                               + ttStLinje.IntAnt
                               + ttStLinje.BrekkAnt
    ttStLinje.DiverseVerdi   =   ttStLinje.SvinnVerdi
                               + ttStLinje.JustVerdi
                               + ttStLinje.IntVerdi
                               + ttStLinje.BrekkVerdi
    .
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PeriodeTotal dTables  _DB-REQUIRED
PROCEDURE PeriodeTotal :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/* Genererer G-Total.                                   */
/* G-Total finnes ved † summere alle butikktotallinjer. */
IF wPeriodeTot THEN /* TEST */
FOR EACH tStLinje where
  tStLinje.DataObjekt = wDataObjekt and
  tStLinje.StTypeId   = wStTypeId and
  tStLinje.PerId      = wPerId and
  tStLinje.PerLinNr   = 1000000
  break
  by tStLinje.DataObjekt
  by tStLinje.StTypeId
  by tStLinje.Butik descending
  by tStLinje.Aar descending
  by tStLinje.PerId descending
  by tStLinje.PerLinNr descending:

  IF FIRST(tStLinje.Butik) then
    DO:
      FIND FIRST ttStLinje where
                 ttStLinje.DataObjekt = wDataObjekt and
                 ttStLinje.StTypeId   = wStTypeId   and
                 ttStLinje.PerId      = wPerId      and
                 ttStLinje.butik      = 9999999     and
                 ttStLinje.PerLinNr   = 1999999     and
                 ttStLinje.Aar        = tStLinje.Aar no-error.
      IF AVAILABLE ttStLinje then
        DELETE ttStLinje.
      CREATE ttStLinje.
      assign
        ttStLinje.DataObjekt = wDataObjekt
        ttStLinje.StTypeId   = wStTypeId
        ttStLinje.PerId      = wPerId
        ttStLinje.butik      = 9999999
        ttStLinje.PerLinNr   = 1999999
        ttStLinje.Aar        = tStLinje.Aar
        ttStLinje.Hg         = tStLinje.Hg
        ttStLinje.TotalPost  = 1
        .
    END.

  IF AVAILABLE ttStLinje then
  ASSIGN
    ttStLinje.LagerAnt       = ttStLinje.LagerAnt       + tStLinje.LagerAnt
    ttStLinje.LagerVerdi     = ttStLinje.LagerVerdi     + tStLinje.LagerVerdi
    ttStLinje.PrimoAnt       = ttStLinje.PrimoAnt       + tStLinje.PrimoAnt
    ttStLinje.PrimoVerdi     = ttStLinje.PrimoVerdi     + tStLinje.PrimoVerdi
    ttStLinje.VVarekost      = ttStLinje.VVarekost      + tStLinje.VVareKost
    ttStLinje.AntSolgt       = ttStLinje.AntSolgt       + tStLinje.AntSolgt
    ttStLinje.AntRab         = ttStLinje.AntRab         + tStLinje.AntRab
    ttStLinje.BrekkAnt       = ttStLinje.BrekkAnt       + tStLinje.BrekkAnt
    ttStLinje.IntAnt         = ttStLinje.IntAnt         + tStLinje.IntAnt
    ttStLinje.ReklAnt        = ttStLinje.ReklAnt        + tStLinje.ReklAnt
    ttStLinje.ReklLAnt       = ttStLinje.ReklLAnt       + tStLinje.ReklLAnt
    ttStLinje.GjenkjopAnt    = ttStLinje.GjenkjopAnt    + tStLinje.GjenkjopAnt
    ttStLinje.KjopAnt        = ttStLinje.KjopAnt        + tStLinje.KjopAnt
    ttStLinje.OvAnt          = ttStLinje.OvAnt          + tStLinje.OvAnt
    ttStLinje.JustAnt        = ttStLinje.JustAnt        + tStLinje.JustAnt
    ttStLinje.JustVerdi      = ttStLinje.JustVerdi      + tStLinje.JustVerdi
    ttStLinje.SvinnAnt       = ttStLinje.SvinnAnt       + tStLinje.SvinnAnt
    ttStLinje.SvinnVerdi     = ttStLinje.SvinnVerdi     + tStLinje.SvinnVerdi
    ttStLinje.NedAnt         = ttStLinje.NedAnt         + tStLinje.NedAnt
    ttStLinje.NedVerdi       = ttStLinje.NedVerdi       + tStLinje.NedVerdi
    ttStLinje.VerdiSolgt     = ttStLinje.VerdiSolgt     + tStLinje.VerdiSolgt
    ttStLinje.VerdiRabatt    = ttStLinje.VerdiRabatt    + tStLinje.VerdiRabatt
    ttStLinje.KjopVerdi      = ttStLinje.KjopVerdi      + tStLinje.KjopVerdi
    ttStLinje.BrekkVerdi     = ttStLinje.BrekkVerdi     + tStLinje.BrekkVerdi
    ttStLinje.IntVerdi       = ttStLinje.IntVerdi       + tStLinje.IntVerdi
    ttStLinje.ReklVerdi      = ttStLinje.ReklVerdi      + tStLinje.ReklVerdi
    ttStLinje.ReklLVerdi     = ttStLinje.ReklLVerdi     + tStLinje.ReklLVerdi
    ttStLinje.GjenkjopVerdi  = ttStLinje.GjenkjopVerdi  + tStLinje.GjenkjopVerdi
    ttStLinje.OvVerdi        = ttStLinje.OvVerdi        + tStLinje.OvVerdi
    ttStLinje.MvaVerdi       = ttStLinje.MvaVerdi       + tStLinje.MvaVerdi
    ttStLinje.OmlHast        = ttStLinje.AntSolgt / ((ttStLinje.LagerAnt + ttStLinje.KjopAnt + ttStLinje.OvAnt) / 2)
    ttStLinje.OmlHast        = IF ttStLinje.OmlHast = ? THEN 0 ELSE ttStLinje.OmlHast
                                                    /* Det blir her feil å regne oms.hast fra primo, da */
                                                    /* denne alltid er 0 eller mindre enn null.         */
    ttStLinje.DiverseAnt     =   ttStLinje.SvinnAnt
                               + ttStLinje.JustAnt
                               + ttStLinje.IntAnt
                               + ttStLinje.BrekkAnt
    ttStLinje.DiverseVerdi   =   ttStLinje.SvinnVerdi
                               + ttStLinje.JustVerdi
                               + ttStLinje.IntVerdi
                               + ttStLinje.BrekkVerdi
    .
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SettOppStatInfo dTables  _DB-REQUIRED
PROCEDURE SettOppStatInfo :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT  PARAMETER wInput        AS CHAR  NO-UNDO.
  DEF OUTPUT PARAMETER wVindustittel AS CHAR  NO-UNDO.
  DEF OUTPUT PARAMETER wTitle        AS CHAR  NO-UNDO.
  DEF OUTPUT PARAMETER wPris         AS CHAR  NO-UNDO.

  /* STATISTIKKTYPE */
  case wStTypeId:
    when "ARTIKKEL" then
      /* ARTIKKEL */
      do:
        /* Henter artikkelinformasjonen. */
        find ArtBas no-lock where 
          ArtBas.ArtikkelNr = dec(wInput) no-error.
        if not available ArtBAs then
          return no-apply.
        find LevBas of ArtBas no-lock no-error.
        find ArtPris no-lock where
          ArtPris.ArtikkelNr = ArtBas.ArtikkelNr and
          ArtPris.ProfilNr   = Butiker.ProfilNr no-error.
        if available ArtPris then
          do:
            assign
              wPris = "Pris: " + 
                      (if ArtPris.Tilbud = true
                         then string(ArtPris.Pris[2])
                         else string(ArtPris.Pris[1])).
          end.
  
        /* Setter vindu, graf tittler og dataobjekt. */
        assign
          wDataObjekt   = string(ArtBas.ArtikkelNr,"9999999999999")
          wVindusTittel = "Artikkelstatistikk for periode: " + wPerId
          wTitle        = "Artikkel: " + string(ArtBas.Vg) + "/" + string(ArtBas.LopNr) + "  " + 
                          ArtBas.LevKod + "/" + ArtBas.LevFarg + 
                          " Leverandør: " + string(ArtBas.LevNr) + " " + 
                          (if available LevBas then LevBas.LevNamn else "") + " " +
                          wPris.
      end. /* ARTIKKEL */
    when "BUTSTAT" then
      do:
        /* Henter Butikkinformasjonen. */
        find Butiker no-lock where 
          Butiker.Butik = int(wInput) no-error.
        if not available Butiker then
          return no-apply.
        assign
           wPris = "".
        /* Setter vindu, graf tittler og dataobjekt. */
        assign
          wDataObjekt   = string(Butiker.Butik,"999999")
          wVindusTittel = "Butikkstatistikk for periode: " + wPerId
          wTitle        = "Butikk: " + string(Butiker.Butik) + " " + string(Butiker.butNamn).
      end. /* BUTSTAT */
    when "HOVEDGR" then
      do:
        /* Henter Hovedgruppeinformasjonen. */
        find HuvGr no-lock where 
          HuvGr.Hg = int(wInput) no-error.
        if not available HuvGr then
          return no-apply.
        assign
           wPris = "".
        /* Setter vindu, graf tittler og dataobjekt. */
        assign
          wDataObjekt   = string(HuvGr.Hg,"9999")
          wVindusTittel = "Hovedgruppestatistikk for periode: " + wPerId
          wTitle        = "Hovedgruppe: " + string(HuvGr.Hg) + " " + string(HuvGr.HgBeskr).
      end. /* HUVEDGR */
    when "KUNDSTAT" then
      do:
        /* Henter kundeinformasjonen. */
        find Kunde no-lock where
          Kunde.KundeNr = dec(wInput) no-error.
        if not available Kunde then
          return no-apply.
        assign
           wPris = "".
        /* Setter vindu, graf tittler og dataobjekt. */
        assign
          wDataObjekt   = string(Kunde.KundeNr,"9999999999999")
          wVindusTittel = "Kundestatistikk for periode: " + wPerId
          wTitle        = "Kunde: " + string(Kunde.KundeNr) + " " + string(Kunde.Navn).
      end. /* KUNDSTAT */
    when "LEVERAN" then
      do:      
        /* Henter leverandørinformasjonen. */
        find LevBas no-lock where 
          LevBas.LevNr = int(wInput) no-error.
        if not available LevBas then
          return no-apply.
        assign
           wPris = "".
        /* Setter vindu, graf tittler og dataobjekt. */
        assign
          wDataObjekt   = string(LevBas.LevNr,"999999")
          wVindusTittel = "Leverandørstatistikk for periode: " + wPerId
          wTitle        = "Leverandør: " + string(LevBas.LevNr) + " " + string(LevBas.LevNamn).
      end. /* LEVERAN */
    when "SELGERSTAT" then
      do:
        /* Henter Selgerinformasjonen. */
        find Forsalj no-lock where 
          Forsalj.ForsNr = int(wInput) no-error.
        if not available Forsalj then
          return no-apply.
        assign
           wPris = "".
        /* Setter vindu, graf tittler og dataobjekt. */
        assign
          wDataObjekt   = string(Forsalj.ForsNr,"9999999999999")
          wVindusTittel = "Selgerstatistikk for periode: " + wPerId
          wTitle        = "Selger: " + string(Forsalj.ForsNr) + " " + string(Forsalj.FoNamn).
      end. /* SELGERSTAT */
    when "VAREGR" then
      do:
        /* Henter Varegruppeinformasjonen. */
        find VarGr no-lock where 
          VarGr.Vg = int(wInput) no-error.
        if not available VarGr then
          return no-apply.
        assign
           wPris = "".
        /* Setter vindu, graf tittler og dataobjekt. */
        assign
          wDataObjekt   = string(VarGr.Vg,"9999")
          wVindusTittel = "Varegruppestatistikk for periode: " + wPerId
          wTitle        = "Varegruppe: " + string(VarGr.Vg) + " " + string(VarGr.VgBeskr).
      end. /* VAREGR */
      when "SELGER" then
        do:
          /* Henter Selgerinformasjonen. */
          find Selger no-lock where 
            Selger.SelgerNr = dec(wInput) no-error.
          if not available Selger then
            return no-apply.
          assign
             wPris = "".
          /* Setter vindu, graf tittler og dataobjekt. */
          assign
            wDataObjekt   = string(Selger.SelgerNr,"9999999999999")
            wVindusTittel = "Selgerstatistikk for periode: " + wPerId
            wTitle        = "Selger: " + string(Selger.SelgerNr) + " " + string(Selger.Navn).
        end. /* VAREGR */
      when "AVDELING" then
        do:
          /* Henter Selgerinformasjonen. */
          find Avdeling no-lock where 
            Avdeling.AvdelingNr = int(wInput) no-error.
          if not available Avdeling then
            return no-apply.
          assign
             wPris = "".
          /* Setter vindu, graf tittler og dataobjekt. */
          assign
            wDataObjekt   = string(Avdeling.AvdelingNr,"99")
            wVindusTittel = "Avdelingsstatistikk for periode: " + wPerId
            wTitle        = "Avdeling: " + string(Avdeling.AvdelingNr) + " " + string(Avdeling.AvdelingNavn).
        end. /* VAREGR */
  end case. /* STATISTIKKTYPE */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE StatGrafikk dTables  _DB-REQUIRED
PROCEDURE StatGrafikk :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  def var wVindusTittel as char  no-undo.
  DEF VAR wTitle        AS CHAR  NO-UNDO.
  DEF VAR wRowLabel     AS CHAR  NO-UNDO.
  def var wRowKrit      as char  no-undo.
  DEF VAR wColLabel     AS CHAR  NO-UNDO.
  DEF VAR wFColValues   AS CHAR  no-undo.
  def var wColEnable    as char  no-undo.      
  def var wAntRowLabel  as int   no-undo.
  def var wLoop         as int   no-undo.    
  def var w2Loop        as int   no-undo.
  def var wDato         as date  no-undo.
  def var wWDato        as date  no-undo.
  def var wPris         as char  no-undo.    
  def var wYYYYWW       as int   no-undo.
  def var wFraUke       as int   no-undo.
  def var wTilUke       as int   no-undo.
  def var wAar          as int   no-undo.
  def var wPerLinNr     as int   no-undo.
  DEF VAR w2Tekst       AS CHAR  NO-UNDO.
  DEF VAR wRecid        AS RECID NO-UNDO.

  def var wKjopt      as int  extent 53 no-undo. 
  def var wSolgt      as int  extent 53 no-undo.
  def var wGjenkjop   as int  extent 53 no-undo.
  def var wKReklam    as int  extent 53 no-undo.
  def var wLagerRekl  as int  extent 53 no-undo.
  def var wBrekkasje  as int  extent 53 no-undo.
  def var wOverfort   as int  extent 53 no-undo.
  def var wJustert    as int  extent 53 no-undo.
  def var wSvinn      as int  extent 53 no-undo.
  def var wIntforbruk as int  extent 53 no-undo.
  def var wNedskrevet as int  extent 53 no-undo.
  def var wRabatt     as int  extent 53 no-undo. 
  def var wTekst      as char extent 12 no-undo.
  DEF VAR wIniTxt     AS CHAR           NO-UNDO.

  DEF VAR wUkeNr AS INT NO-UNDO.
    
  assign
    wVindusTittel = ""
    wTitle        = ""
    wRowLabel     = ""
    wRowKrit      = ""
    wColLabel     = ""
    wFColValues   = ""
    wColEnable    = ""      
    wAntRowLabel  = 0
    wPris         = ""
    wTekst        = "".    
    
    /* -------------
    assign
    *  wVindusTittel = "Vindustittel"
    *  wTitle        = "Artikkel 11/702 XL-13/94 Testgraf"
    *  wRowLabel     = "Kvartal 1,Kvartal 2,Kvartal 3,Kvartal 4"
    *  wColLabel     = "Kjøpt,Solgt,Gjenkjøp,K.Reklam,Lager.Rekl,Brekkasje,Overført,Justert,Svinn,Int.forbruk,Nedskrevet,Rabatt"
      wFColValues   = "3000,600,1100,1900" + ";" + 
                      "2000,100,200,2100" + ";" + 
                      "4000,800,350,1100" + ";" + 
                      "4500,800,350,1900" + ";" + 
                      "3200,800,350,1600" + ";" + 
                      "3000,600,450,1000" + ";" + 
                      "3000,600,450,1000" + ";" + 
                      "3000,600,450,1000" + ";" + 
                      "3000,600,450,1000" + ";" + 
                      "3000,600,450,1000" + ";" + 
                      "3000,600,450,1000"
      wColEnable    = "1,1,0,0,0,0,0,0,0,0,0".      
    ---------------*/

  /* Sentrallager butikk */
  find butiker no-lock where
    Butiker.butik = wCl no-error.
  if not available butiker then
    do:
      message "Sentrallager er ikke satt opp!"
        view-as alert-box error title "Feil".
      return no-apply.
    end.

  /* Setter opp statistikkinformasjonen */
  run SettOppStatInfo (DYNAMIC-FUNCTION('getForeignValues':U),
                       OUTPUT wVindustittel,
                       OUTPUT wTitle,       
                       OUTPUT wPris).        

  /* LabelListe */
  {syspara.i 2 2 1 wColLabel}
  if wColLabel = "" then
    wColLabel = "Kjøpt,Solgt,Gjenkjøp,K.Reklam,Lager.Rekl,Brekkasje,Overført,Justert,Svinn,Int.forbruk,Nedskrevet,Rabatt".

  /* Bygger  RowLabel */
  
  assign
    wAntRowLabel = 0
    wRowLabel    = ""
    wRowKrit     = "".
  case wPerId:
    when "AAR" then
      do:
        do wLoop = int(entry(1,wKriterier)) to int(entry(2,wKriterier)):
          assign
            wRowLabel    = wRowLabel + 
                           (if wRowLabel = ""
                             then ""
                             else ",") + 
                           string(wLoop)
            wRowKrit     = wRowKrit + 
                           (if wRowKrit = ""
                              then ""
                              else "|") +
                           string(wLoop,"9999") + ",1".        
        end.
      end.
    when "MANED" then
      do:
        /* Forespurt periode er innen for ett år. */
        if int(entry(1,wKriterier)) = int(entry(2,wKriterier)) then
          do:           
            do wLoop = int(entry(3,wKriterier)) to int(entry(4,wKriterier)):
              assign
                wRowLabel    = wRowLabel + 
                               (if wRowLabel = ""
                                 then ""
                                 else ",") + 
                               string(wLoop)
                wRowKrit     = wRowKrit + 
                               (if wRowKrit = ""
                                  then ""
                                  else "|") +
                               entry(1,wKriterier) + "," + string(wLoop).        
             end.
          end.
        /* Forespurt periode går over flere år. */
        else do:
            /* Første år */
            do wLoop = int(entry(3,wKriterier)) to 12:
              assign
                wRowLabel    = wRowLabel + 
                               (if wRowLabel = ""
                                 then ""
                                 else ",") + 
                               string(wLoop)        
                wRowKrit     = wRowKrit + 
                               (if wRowKrit = ""
                                  then ""
                                  else "|") +
                               entry(1,wKriterier) + "," + string(wLoop).        
            end.
            /*  Mellomliggende år. */
            do wLoop = int(entry(1,wKriterier)) to int(entry(2,wKriterier)):
              if wLoop = int(entry(1,wKriterier)) or wLoop = int(entry(2,wKriterier)) then
                next.
              do w2Loop = 1 to 12:
                assign
                  wRowLabel    = wRowLabel + 
                               (if wRowLabel = ""
                                 then ""
                                 else ",") + 
                               string(wLoop)        
                  wRowKrit     = wRowKrit + 
                                 (if wRowKrit = ""
                                    then ""
                                    else "|") +
                                 string(int(entry(1,wKriterier)) + wLoop,"9999") + "," + string(w2Loop).        
              end.
            end. 
            /* Siste år. */
            do wLoop = 1 to int(entry(4,wKriterier)):
              assign
                wRowLabel    = wRowLabel + 
                               (if wRowLabel = ""
                                 then ""
                                 else ",") + 
                               string(wLoop)        
                wRowKrit     = wRowKrit + 
                               (if wRowKrit = ""
                                  then ""
                                  else "|") +
                               entry(2,wKriterier) + "," + string(wLoop).        
            end.
        
        end.
      end.
    when "UKE" then
      do:        
        /* Forespurt periode er innen for ett år. */
        if int(entry(1,wKriterier)) = int(entry(2,wKriterier)) then
          do:            
            assign
              wFraUke = int(entry(3,wKriterier))
              wTilUke = int(entry(4,wKriterier)).                    
            do wLoop = wFraUke to wTilUke:
              assign
                wRowLabel    = wRowLabel + 
                               (if wRowLabel = ""
                                 then ""
                                 else ",") + 
                               string(wLoop)        
                wRowKrit     = wRowKrit + 
                               (if wRowKrit = ""
                                  then ""
                                  else "|") +
                               entry(1,wKriterier) + "," + string(wLoop).        
             end.
          end.
        /* Forespurt periode går over flere år. */
        else do:
            /* Fra - til uke første år.   */
            /* Til ukenummer må beregnes. */
            RUN weeknum.p (DATE(12,31,INT(entry(1,wKriterier))),OUTPUT wUkeNr).
            assign
              wFraUke = int(entry(3,wKriterier))
              wTilUke = int(SUBstring(STRING(wUkeNr,"999999"),5,2)).
              /*wTilUke = int(entry(4,wKriterier))*/                    
            /* Første år */
            do wLoop = wFraUke to wTilUke:
              assign
                wRowLabel    = wRowLabel + 
                               (if wRowLabel = ""
                                 then ""
                                 else ",") + 
                               string(wLoop)        
                wRowKrit     = wRowKrit + 
                               (if wRowKrit = ""
                                  then ""
                                  else "|") +
                               entry(1,wKriterier) + "," + string(wLoop).        
            end.
            /*  Mellomliggende år er ikke aktuelt. Maks 53 perioders oppløsning. */

            /* Finner siste ukenummer siste år.*/         
            ASSIGN
              wFraUke = 1
              wTilUke = int(entry(4,wKriterier)).                    
            /* Siste år. */
            do wLoop = 1 to wTilUke:
              assign
                wRowLabel    = wRowLabel + 
                               (if wRowLabel = ""
                                 then ""
                                 else ",") + 
                               string(wLoop)        
                wRowKrit     = wRowKrit + 
                               (if wRowKrit = ""
                                  then ""
                                  else "|") +
                               entry(2,wKriterier) + "," + string(wLoop).        
            end.        
        end.
      end. /* UKE */
    when "DAG" then
      do:        
        /* Forespurt periode er innen for ett år. */
        if int(entry(1,wKriterier)) = int(entry(2,wKriterier)) then
          do:            
            do wLoop = int(entry(3,wKriterier)) to int(entry(4,wKriterier)):
              assign
                wRowLabel    = wRowLabel + 
                               (if wRowLabel = ""
                                 then ""
                                 else ",") + 
                               STRING(date(1,1,INT(ENTRY(1,wKriterier))) - 1 + wLoop)        
                wRowKrit     = wRowKrit + 
                               (if wRowKrit = ""
                                  then ""
                                  else "|") +
                               entry(1,wKriterier) + "," + string(wLoop).        
             end.
          end.
        /* Forespurt periode går over flere år. */
        else do:
            do wLoop = int(entry(3,wKriterier)) to 
                       (date(12,31,int(entry(1,wKriterier))) - date(1,1,int(entry(1,wKriterier))) + 1):
              assign
                wRowLabel    = wRowLabel + 
                               (if wRowLabel = ""
                                 then ""
                                 else ",") + 
                               STRING(date(1,1,INT(ENTRY(1,wKriterier))) - 1 + wLoop)        
                wRowKrit     = wRowKrit + 
                               (if wRowKrit = ""
                                  then ""
                                  else "|") +
                               entry(1,wKriterier) + "," + string(wLoop).        
            end.
            /*  Mellomliggende år er ikke aktuelt. Maks 53 perioders oppløsning. */

            /* Siste år. */
            do wLoop = 1 to int(entry(4,wKriterier)):
              assign
                wRowLabel    = wRowLabel + 
                               (if wRowLabel = ""
                                 then ""
                                 else ",") + 
                               STRING(date(1,1,INT(ENTRY(2,wKriterier))) - 1 + wLoop)        
                wRowKrit     = wRowKrit + 
                               (if wRowKrit = ""
                                  then ""
                                  else "|") +
                               entry(2,wKriterier) + "," + string(wLoop).        
            end.        
        end.
      end.
  end case.
  assign 
    wAntRowLabel = num-entries(wRowLabel).
  /* Grafen kan ikke vise mer med 53 labler. */
  if wAntRowLabel > 53 then
    do:
      message "Grafen kan ikke vise med enn 53 perioder!"
        view-as alert-box message title "Melding".
      return no-apply.
    end.
  
  /* Bygger ColEnable liste */
  wColEnable = "".
  do wLoop = 1 to num-entries(wColLabel):
    wColEnable = wColEnable + 
                 (if wColEnable <> "" 
                    then ","
                    else "") + 
                 (if wLoop <= 2 
                    then "1"
                    else "0").
  end.

  /* Nullstiller sumvariabler. */
  assign
    wKjopt      = 0 
    wSolgt      = 0
    wGjenkjop   = 0
    wKReklam    = 0
    wLagerRekl  = 0
    wBrekkasje  = 0
    wOverfort   = 0
    wJustert    = 0
    wSvinn      = 0
    wIntforbruk = 0
    wNedskrevet = 0
    wRabatt     = 0.

  /*
  MESSAGE 
  "wAar:" wAar SKIP
  "wRowLabel:" wRowLabel SKIP
  "wRowKrit:" wRowKrit SKIP
  "wKriterier:" wKriterier SKIP
  VIEW-AS ALERT-BOX.
  */

  /* Henter verdier fra statistikken */
  STATISTIK:
  do wLoop = 1 to num-entries(wRowLabel): 
    /* Henter kriterier for hver periode fra kriterielisten */
    /* Setter Kriterier */
    assign
      wAar      = int(entry(1,entry(wLoop,wRowKrit,"|"))) 
      wPerLinNr = int(entry(2,entry(wLoop,wRowKrit,"|"))).
  
    /* Sumerer opp for alle butikker */
    for each tStLinje no-lock where
       tStLinje.StTypeId   = wStTypeId and
       tStLinje.PerId      = wPerId and
       tStLinje.DataObjekt = wDataObjekt and
       tStLinje.Diverse    = "" and
       tStLinje.Butik      > 0 AND 
       tStLinje.Butik      < 1000000 AND
       tStLinje.Aar        = wAar and
       tStLinje.PerLinNr   = wPerLinNr:
        
      assign
        wKjopt[wLoop]      = wKjopt[wLoop]      + tStLinje.KjopAnt
        wSolgt[wLoop]      = wSolgt[wLoop]      + tStLinje.AntSolgt
        wGjenkjop[wLoop]   = wGjenkjop[wLoop]   + tStLinje.GjenkjopAnt
        wKReklam [wLoop]   = wKReklam [wLoop]   + tStLinje.ReklAnt
        wLagerRekl[wLoop]  = wLagerRekl[wLoop]  + tStLinje.ReklLAnt
        wBrekkasje[wLoop]  = wBrekkasje[wLoop]  + tStLinje.BrekkAnt
        wOverfort[wLoop]   = wOverfort[wLoop]   + tStLinje.OvAnt
        wJustert[wLoop]    = wJustert[wLoop]    + tStLinje.JustAnt
        wSvinn[wLoop]      = wSvinn[wLoop]      + tStLinje.SvinnAnt
        wIntforbruk[wLoop] = wIntforbruk[wLoop] + tStLinje.IntAnt
        wNedskrevet[wLoop] = wNedSkrevet[wLoop] + tStLinje.NedAnt
        wRabatt[wLoop]     = wRabatt[wLoop]     + tStLinje.AntRab.
    end.
    /*
    MESSAGE 
        wStTypeId skip
        wPerId SKIP
        wDataObjekt skip
        wAar SKIP
        wPerLinNr SKIP
        "wRowKrit:" wRowKrit SKIP
        "wRowLabel:" wRowLabel SKIP
        wKjopt[wLoop]     
        wSolgt[wLoop]     
        wGjenkjop[wLoop]  
        wKReklam [wLoop]  
        wLagerRekl[wLoop] 
        wBrekkasje[wLoop] 
        wOverfort[wLoop]  
        wJustert[wLoop]   
        wSvinn[wLoop]     
        wIntforbruk[wLoop]
        wNedskrevet[wLoop]
        wRabatt[wLoop]    
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
    */
  end. /* STATISTIK */
  
  /* Bygger verdiliste pr. element.*/
  do wLoop = 1 to num-entries(wRowLabel):  
    assign
      wTekst[ 1] = wTekst[ 1] + (if wTekst[ 1] = "" then "" else ",") + string(wKjopt[wLoop])
      wTekst[ 2] = wTekst[ 2] + (if wTekst[ 2] = "" then "" else ",") + string(wSolgt[wLoop])
      wTekst[ 3] = wTekst[ 3] + (if wTekst[ 3] = "" then "" else ",") + string(wGjenkjop[wLoop])
      wTekst[ 4] = wTekst[ 4] + (if wTekst[ 4] = "" then "" else ",") + string(wKReklam [wLoop])
      wTekst[ 5] = wTekst[ 5] + (if wTekst[ 5] = "" then "" else ",") + string(wLagerRekl[wLoop])
      wTekst[ 6] = wTekst[ 6] + (if wTekst[ 6] = "" then "" else ",") + string(wBrekkasje[wLoop])
      wTekst[ 7] = wTekst[ 7] + (if wTekst[ 7] = "" then "" else ",") + string(wOverfort[wLoop])
      wTekst[ 8] = wTekst[ 8] + (if wTekst[ 8] = "" then "" else ",") + string(wJustert[wLoop])
      wTekst[ 9] = wTekst[ 9] + (if wTekst[ 9] = "" then "" else ",") + string(wSvinn[wLoop])
      wTekst[10] = wTekst[10] + (if wTekst[10] = "" then "" else ",") + string(wIntforbruk[wLoop])
      wTekst[11] = wTekst[11] + (if wTekst[11] = "" then "" else ",") + string(wNedSkrevet[wLoop]).   
      wTekst[12] = wTekst[12] + (if wTekst[12] = "" then "" else ",") + string(wRabatt[wLoop]).   
  end.
  
  /* Setter sammen elementverdistrengen. */
  assign
    wFColValues =  wTekst[ 1] + ";" + 
                   wTekst[ 2] + ";" +
                   wTekst[ 3] + ";" +
                   wTekst[ 4] + ";" +
                   wTekst[ 5] + ";" +
                   wTekst[ 6] + ";" +
                   wTekst[ 7] + ";" +
                   wTekst[ 8] + ";" +
                   wTekst[ 9] + ";" +
                   wTekst[10] + ";" +
                   wTekst[11] + ";" +
                   wTekst[12].

  /* TEST */
  /*
  message  "wVindusTittel" wVindusTittel skip
           "wTitle" wTitle skip
           "wRowLabel" wRowLabel skip
           "wColLabel" wColLabel skip
           "wColEnable" wColEnable skip
           "wFColValues" wFColValues skip 
           " wTekst[ 1]" wTekst[ 1] skip
           " wTekst[ 2]" wTekst[ 2]  skip
           " wTekst[ 3]" wTekst[ 3]  skip
           " wTekst[ 4]" wTekst[ 4]  skip
           " wTekst[ 5]" wTekst[ 5]       skip  
           " wTekst[ 6]" wTekst[ 6]  skip
           " wTekst[ 7]" wTekst[ 7]  skip
           " wTekst[ 8]" wTekst[ 8]  skip
           " wTekst[ 9]" wTekst[ 9]  skip
           " wTekst[10]" wTekst[10]  skip
           " wTekst[11]" wTekst[11]  skip
           " wTekst[12]" wTekst[12] skip
  view-as alert-box.
  */
  /* TEST slutt */

  /* Starter grafisk visning. */
  run d-mschart.w (wVindusTittel,
                   wTitle,
                   wRowLabel,
                   wColLabel,
                   wFColValues,
                   wColEnable).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE TidsAvgr dTables  _DB-REQUIRED
PROCEDURE TidsAvgr :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/* Tar bort periodelinjer som ligger utenfor forespurt tidsområde. */
IF wTidsavgr then
  KRITERIER:
  DO:
    IF NUM-ENTRIES(wKriterier) < 5 then
      LEAVE KRITERIER.

    FOR EACH tStLinje where
      tStLinje.DataObjekt = wDataObjekt and
      tStLinje.StTypeId   = wStTypeId and
      tStLinje.PerId      = wPerId and
      tStLinje.PerLinNr   < 1000000 and
      (if wButik = 0
        THEN true
        ELSE tStLinje.Butik = wButik)
      break
      by tStLinje.DataObjekt
      by tStLinje.StTypeId
      by tStLinje.Butik
      by tStLinje.Aar descending
      by tStLinje.PerId descending
      by tStLinje.PerLinNr descending:

      /* Ligger i †ret f›r. */
      if tStLinje.Aar < wAAr1 then
        DELETE tStLinje.

      /* Ligger i †ret etter. */
      if AVAILABLE tStLinje then
        DO:
         if tStLinje.Aar > wAAr2 then
           DELETE tStLinje.
        END.

      /* Fase 2 */
      if AVAILABLE tStLinje then
        DO:
          /* F›ste †r */
          if tStLinje.Aar = wAAr1 then
            DO:
              if tStLinje.PerLinNr < wPerLin1 then
                DELETE tStLinje.
            END.

          /* rene imellom er greie */

          /* Siste †r. */
          if AVAILABLE tStLinje then
            DO:
              if tStLinje.Aar = wAAr2 then
                DO:
                  if tStLinje.PerLinNr > wPerLin2 then
                    DELETE tStLinje.
                END.
            END.
        END.

      /* Tar bort butikkene som ikke skal v‘re med. */
      if wButik <> 0 and available tStLinje then
        DO:
          if tStLinje.Butik <> wButik then
            DELETE tStLinje.
        end.

    END.
  END. /* KRITERIER */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

