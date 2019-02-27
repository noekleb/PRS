&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
/* Connected Databases 
          skotex           PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
{adecomm/appserv.i}


/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE TT_StLinje NO-UNDO LIKE StLinje
       FIELD Solgt% AS DECI
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
       FIELD Rab% AS DECI
       FIELD VgLopNr as CHAR
       FIELD LevKod LIKE ArtBas.LevKod
       FIELD Sasong LIKE SaSong.Sasong
       FIELD SasBeskr LIKE SaSong.SasBeskr
       FIELD Farg LIKE Farg.Farg
       FIELD FarBeskr LIKE Farg.FarBeskr
       FIELD MatKod LIKE Material.MatKod
       FIELD MatBeskr LIKE Material.MatBeskr
       FIELD VMId LIKE Varemerke.VMId
       FIELD VMBeskr LIKE Varemerke.Beskrivelse
       FIELD ArtikkelNr LIKE ArtBas.ArtikkelNr
       FIELD ArtBeskr LIKE ArtBas.Beskr
       FIELD BruttoSolgt LIKE StLinje.VerdiSolgt
       FIELD Butnamn LIKE Butiker.butnamn
       FIELD Fornavn AS CHAR
       FIELD Etternavn AS CHAR
       FIELD Adresse1 AS CHAR
       FIELD Adresse2 AS CHAR
       FIELD PostNr AS CHAR
       FIELD PostAdr AS CHAR
       FIELD Telefon AS CHAR
       FIELD EMail AS CHAR
       FIELD KampTilbId AS INTEGER
       FIELD KampTilbbeskr as CHAR
       FIELD Kamptypebeskr as char
       FIELD DBandel% AS DECI
       FIELD Rabandel% AS DECI
       FIELD Kjopandel% AS DECI
       FIELD Kilde AS CHAR
       FIELD TilgKilde AS CHAR
       FIELD MedType AS INTE
       FIELD MedGruppe AS INTE
       field KundeNr AS DECI
       FIELD MobilTlf LIKE Medlem.MobilTlf
       FIELD MKlubbId LIKE Medlem.MKlubbid.



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

DEF VAR cHKinst        AS CHAR   NO-UNDO.

DEFINE TEMP-TABLE TT_StLinjeTMP NO-UNDO LIKE TT_StLinje.

DEFINE TEMP-TABLE TT_StLinjeJmf NO-UNDO LIKE TT_StLinje
/* FIELD DataObjekt   LIKE StLinje.DataObjekt */
/* FIELD Beskrivelse  AS CHAR                 */
/* FIELD PerLinTxt    LIKE StLinje.PerLinTxt  */
/* FIELD AntSolgt1    LIKE StLinje.AntSolgt    */
/* FIELD BruttoSolgt1 AS DECIMAL               */
/* FIELD VerdiSolgt1  LIKE StLinje.VerdiSolgt  */
/* FIELD Solgt%1      AS DECIMAL               */
/* FIELD MvaVerdi1    LIKE StLinje.MvaVerdi    */
/* FIELD DbKr1        LIKE StLinje.DbKr        */
/* FIELD Db%1         LIKE StLinje.Db%         */
/* FIELD AntRabatt1   LIKE StLinje.AntRabatt   */
/* FIELD VerdiRabatt1 LIKE StLinje.VerdiRabatt */
/* FIELD Rab%1        AS DECIMAL               */
FIELD AntSolgt2    LIKE StLinje.AntSolgt   
FIELD BruttoSolgt2 AS DECIMAL
FIELD VerdiSolgt2  LIKE StLinje.VerdiSolgt 
FIELD Solgt%2      AS DECIMAL
FIELD MvaVerdi2    LIKE StLinje.MvaVerdi   
FIELD DbKr2        LIKE StLinje.DbKr       
FIELD Db%2         AS DECIMAL
FIELD AntRabatt2   LIKE StLinje.AntRabatt  
FIELD VerdiRabatt2 LIKE StLinje.VerdiRabatt
FIELD Rab%2        AS DECIMAL
FIELD AntDiff%     AS DECIMAL
FIELD SolgtDiff%   AS DECIMAL
FIELD DbKrDiff%    AS DECIMAL
    INDEX DataObjekt Dataobjekt.
DEFINE TEMP-TABLE TT_StLinjeJmfTMP NO-UNDO LIKE TT_StLinjeJmf.

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
&Scoped-define INTERNAL-TABLES StLinje

/* Definitions for QUERY Query-Main                                     */
&Scoped-Define ENABLED-FIELDS  Aar AntRabatt AntSolgt AntTilbSolgt Beskrivelse BrekkAnt BrekkVerdi~
 BrukerID Butik DataObjekt Db% DbKr Diverse DiverseAnt Diverseverdi EDato~
 ETid GjenkjopAnt GjenkjopVerdi Hg IntAnt IntVerdi JustAnt JustVerdi KjopAnt~
 KjopVerdi LagerAnt LagerVerdi MvaVerdi NedAnt NedVerdi OmlHast OvAnt~
 OvVerdi PerId PerLinNr PerLinTxt PrimoAnt Primoverdi RegistrertAv~
 RegistrertDato RegistrertTid ReklAnt ReklLAnt ReklLVerdi ReklVerdi StTypeId~
 SvinnAnt SvinnVerdi TilbMvaVerdi TilbVVarekost TotalPost Utsolgt%~
 VerdiRabatt VerdiSolgt VerdiTilbSolgt VisBut VVarekost AarPerLinNr
&Scoped-define ENABLED-FIELDS-IN-StLinje Aar AntRabatt AntSolgt ~
AntTilbSolgt Beskrivelse BrekkAnt BrekkVerdi BrukerID Butik DataObjekt Db% ~
DbKr Diverse DiverseAnt Diverseverdi EDato ETid GjenkjopAnt GjenkjopVerdi ~
Hg IntAnt IntVerdi JustAnt JustVerdi KjopAnt KjopVerdi LagerAnt LagerVerdi ~
MvaVerdi NedAnt NedVerdi OmlHast OvAnt OvVerdi PerId PerLinNr PerLinTxt ~
PrimoAnt Primoverdi RegistrertAv RegistrertDato RegistrertTid ReklAnt ~
ReklLAnt ReklLVerdi ReklVerdi StTypeId SvinnAnt SvinnVerdi TilbMvaVerdi ~
TilbVVarekost TotalPost Utsolgt% VerdiRabatt VerdiSolgt VerdiTilbSolgt ~
VisBut VVarekost AarPerLinNr 
&Scoped-Define DATA-FIELDS  Aar AntRabatt AntSolgt AntTilbSolgt Beskrivelse BrekkAnt BrekkVerdi~
 BrukerID Butik DataObjekt Db% DbKr Diverse DiverseAnt Diverseverdi EDato~
 ETid GjenkjopAnt GjenkjopVerdi Hg IntAnt IntVerdi JustAnt JustVerdi KjopAnt~
 KjopVerdi LagerAnt LagerVerdi MvaVerdi NedAnt NedVerdi OmlHast OvAnt~
 OvVerdi PerId PerLinNr PerLinTxt PrimoAnt Primoverdi RegistrertAv~
 RegistrertDato RegistrertTid ReklAnt ReklLAnt ReklLVerdi ReklVerdi StTypeId~
 SvinnAnt SvinnVerdi TilbMvaVerdi TilbVVarekost TotalPost Utsolgt%~
 VerdiRabatt VerdiSolgt VerdiTilbSolgt VisBut VVarekost AarPerLinNr
&Scoped-define DATA-FIELDS-IN-StLinje Aar AntRabatt AntSolgt AntTilbSolgt ~
Beskrivelse BrekkAnt BrekkVerdi BrukerID Butik DataObjekt Db% DbKr Diverse ~
DiverseAnt Diverseverdi EDato ETid GjenkjopAnt GjenkjopVerdi Hg IntAnt ~
IntVerdi JustAnt JustVerdi KjopAnt KjopVerdi LagerAnt LagerVerdi MvaVerdi ~
NedAnt NedVerdi OmlHast OvAnt OvVerdi PerId PerLinNr PerLinTxt PrimoAnt ~
Primoverdi RegistrertAv RegistrertDato RegistrertTid ReklAnt ReklLAnt ~
ReklLVerdi ReklVerdi StTypeId SvinnAnt SvinnVerdi TilbMvaVerdi ~
TilbVVarekost TotalPost Utsolgt% VerdiRabatt VerdiSolgt VerdiTilbSolgt ~
VisBut VVarekost AarPerLinNr 
&Scoped-Define MANDATORY-FIELDS  PerId StTypeId
&Scoped-Define APPLICATION-SERVICE 
&Scoped-Define ASSIGN-LIST 
&Scoped-Define DATA-FIELD-DEFS "dstlinje.i"
&Scoped-define QUERY-STRING-Query-Main FOR EACH StLinje NO-LOCK INDEXED-REPOSITION
{&DB-REQUIRED-START}
&Scoped-define OPEN-QUERY-Query-Main OPEN QUERY Query-Main FOR EACH StLinje NO-LOCK INDEXED-REPOSITION.
{&DB-REQUIRED-END}
&Scoped-define TABLES-IN-QUERY-Query-Main StLinje
&Scoped-define FIRST-TABLE-IN-QUERY-Query-Main StLinje


/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getBeskr dTables  _DB-REQUIRED
FUNCTION getBeskr RETURNS CHARACTER
  ( INPUT cStTypeId AS CHARACTER,INPUT cObjekt AS CHARACTER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD kildeOK dTables  _DB-REQUIRED
FUNCTION kildeOK RETURNS LOGICAL
  ( INPUT cStTypeId AS CHARACTER,INPUT cObjekt AS CHARACTER, INPUT cKilde AS CHARACTER, INPUT cTilgKilde AS CHARACTER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}


/* ***********************  Control Definitions  ********************** */

{&DB-REQUIRED-START}

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Query-Main FOR 
      StLinje SCROLLING.
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
      TABLE: TT_StLinje T "?" NO-UNDO skotex StLinje
      ADDITIONAL-FIELDS:
          FIELD Solgt% AS DECI
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
          FIELD Rab% AS DECI
          FIELD VgLopNr as CHAR
          FIELD LevKod LIKE ArtBas.LevKod
          FIELD Sasong LIKE SaSong.Sasong
          FIELD SasBeskr LIKE SaSong.SasBeskr
          FIELD Farg LIKE Farg.Farg
          FIELD FarBeskr LIKE Farg.FarBeskr
          FIELD MatKod LIKE Material.MatKod
          FIELD MatBeskr LIKE Material.MatBeskr
          FIELD VMId LIKE Varemerke.VMId
          FIELD VMBeskr LIKE Varemerke.Beskrivelse
          FIELD ArtikkelNr LIKE ArtBas.ArtikkelNr
          FIELD ArtBeskr LIKE ArtBas.Beskr
          FIELD BruttoSolgt LIKE StLinje.VerdiSolgt
          FIELD Butnamn LIKE Butiker.butnamn
          FIELD Fornavn AS CHAR
          FIELD Etternavn AS CHAR
          FIELD Adresse1 AS CHAR
          FIELD Adresse2 AS CHAR
          FIELD PostNr AS CHAR
          FIELD PostAdr AS CHAR
          FIELD Telefon AS CHAR
          FIELD EMail AS CHAR
          FIELD KampTilbId AS INTEGER
          FIELD KampTilbbeskr as CHAR
          FIELD Kamptypebeskr as char
          FIELD DBandel% AS DECI
          FIELD Rabandel% AS DECI
          FIELD Kjopandel% AS DECI
          FIELD Kilde AS CHAR
          FIELD TilgKilde AS CHAR
          FIELD MedType AS INTE
          FIELD MedGruppe AS INTE
          field KundeNr AS DECI
          FIELD MobilTlf LIKE Medlem.MobilTlf
          FIELD MKlubbId LIKE Medlem.MKlubbid
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
         HEIGHT             = 1.57
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
     _TblList          = "skotex.StLinje"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > skotex.StLinje.Aar
"Aar" "Aar" ? ? "integer" ? ? ? ? ? ? yes ? no 4.8 yes ""
     _FldNameList[2]   > skotex.StLinje.AntRabatt
"AntRabatt" "AntRabatt" ? ? "decimal" ? ? ? ? ? ? yes ? no 11.4 yes ""
     _FldNameList[3]   > skotex.StLinje.AntSolgt
"AntSolgt" "AntSolgt" ? ? "decimal" ? ? ? ? ? ? yes ? no 11.4 yes ""
     _FldNameList[4]   > skotex.StLinje.AntTilbSolgt
"AntTilbSolgt" "AntTilbSolgt" ? ? "decimal" ? ? ? ? ? ? yes ? no 11.4 yes ""
     _FldNameList[5]   > skotex.StLinje.Beskrivelse
"Beskrivelse" "Beskrivelse" ? ? "character" ? ? ? ? ? ? yes ? no 30 yes ""
     _FldNameList[6]   > skotex.StLinje.BrekkAnt
"BrekkAnt" "BrekkAnt" ? ? "decimal" ? ? ? ? ? ? yes ? no 9.4 yes ""
     _FldNameList[7]   > skotex.StLinje.BrekkVerdi
"BrekkVerdi" "BrekkVerdi" ? ? "decimal" ? ? ? ? ? ? yes ? no 16.4 yes ""
     _FldNameList[8]   > skotex.StLinje.BrukerID
"BrukerID" "BrukerID" ? ? "character" ? ? ? ? ? ? yes ? no 10 yes ""
     _FldNameList[9]   > skotex.StLinje.Butik
"Butik" "Butik" ? ? "integer" ? ? ? ? ? ? yes ? no 7.2 yes ""
     _FldNameList[10]   > skotex.StLinje.DataObjekt
"DataObjekt" "DataObjekt" ? ? "character" ? ? ? ? ? ? yes ? no 15 yes ""
     _FldNameList[11]   > skotex.StLinje.Db%
"Db%" "Db%" ? ? "decimal" ? ? ? ? ? ? yes ? no 7.8 yes ""
     _FldNameList[12]   > skotex.StLinje.DbKr
"DbKr" "DbKr" ? ? "decimal" ? ? ? ? ? ? yes ? no 10 yes ""
     _FldNameList[13]   > skotex.StLinje.Diverse
"Diverse" "Diverse" ? ? "character" ? ? ? ? ? ? yes ? no 15 yes ""
     _FldNameList[14]   > skotex.StLinje.DiverseAnt
"DiverseAnt" "DiverseAnt" ? ? "decimal" ? ? ? ? ? ? yes ? no 12.6 yes ""
     _FldNameList[15]   > skotex.StLinje.Diverseverdi
"Diverseverdi" "Diverseverdi" ? ? "decimal" ? ? ? ? ? ? yes ? no 15.6 yes ""
     _FldNameList[16]   > skotex.StLinje.EDato
"EDato" "EDato" ? ? "date" ? ? ? ? ? ? yes ? no 11.6 yes ""
     _FldNameList[17]   > skotex.StLinje.ETid
"ETid" "ETid" ? ? "integer" ? ? ? ? ? ? yes ? no 10.2 yes ""
     _FldNameList[18]   > skotex.StLinje.GjenkjopAnt
"GjenkjopAnt" "GjenkjopAnt" ? ? "decimal" ? ? ? ? ? ? yes ? no 10.4 yes ""
     _FldNameList[19]   > skotex.StLinje.GjenkjopVerdi
"GjenkjopVerdi" "GjenkjopVerdi" ? ? "decimal" ? ? ? ? ? ? yes ? no 23.6 yes ""
     _FldNameList[20]   > skotex.StLinje.Hg
"Hg" "Hg" ? ? "integer" ? ? ? ? ? ? yes ? no 4.8 yes ""
     _FldNameList[21]   > skotex.StLinje.IntAnt
"IntAnt" "IntAnt" ? ? "decimal" ? ? ? ? ? ? yes ? no 13.2 yes ""
     _FldNameList[22]   > skotex.StLinje.IntVerdi
"IntVerdi" "IntVerdi" ? ? "decimal" ? ? ? ? ? ? yes ? no 21.4 yes ""
     _FldNameList[23]   > skotex.StLinje.JustAnt
"JustAnt" "JustAnt" ? ? "decimal" ? ? ? ? ? ? yes ? no 11.8 yes ""
     _FldNameList[24]   > skotex.StLinje.JustVerdi
"JustVerdi" "JustVerdi" ? ? "decimal" ? ? ? ? ? ? yes ? no 14.4 yes ""
     _FldNameList[25]   > skotex.StLinje.KjopAnt
"KjopAnt" "KjopAnt" ? ? "decimal" ? ? ? ? ? ? yes ? no 13.2 yes ""
     _FldNameList[26]   > skotex.StLinje.KjopVerdi
"KjopVerdi" "KjopVerdi" ? ? "decimal" ? ? ? ? ? ? yes ? no 13 yes ""
     _FldNameList[27]   > skotex.StLinje.LagerAnt
"LagerAnt" "LagerAnt" ? ? "integer" ? ? ? ? ? ? yes ? no 10.2 yes ""
     _FldNameList[28]   > skotex.StLinje.LagerVerdi
"LagerVerdi" "LagerVerdi" ? ? "decimal" ? ? ? ? ? ? yes ? no 15.6 yes ""
     _FldNameList[29]   > skotex.StLinje.MvaVerdi
"MvaVerdi" "MvaVerdi" ? ? "decimal" ? ? ? ? ? ? yes ? no 13 yes ""
     _FldNameList[30]   > skotex.StLinje.NedAnt
"NedAnt" "NedAnt" ? ? "decimal" ? ? ? ? ? ? yes ? no 16.6 yes ""
     _FldNameList[31]   > skotex.StLinje.NedVerdi
"NedVerdi" "NedVerdi" ? ? "decimal" ? ? ? ? ? ? yes ? no 16 yes ""
     _FldNameList[32]   > skotex.StLinje.OmlHast
"OmlHast" "OmlHast" ? ? "decimal" ? ? ? ? ? ? yes ? no 10.2 yes ""
     _FldNameList[33]   > skotex.StLinje.OvAnt
"OvAnt" "OvAnt" ? ? "decimal" ? ? ? ? ? ? yes ? no 13.2 yes ""
     _FldNameList[34]   > skotex.StLinje.OvVerdi
"OvVerdi" "OvVerdi" ? ? "decimal" ? ? ? ? ? ? yes ? no 22.2 yes ""
     _FldNameList[35]   > skotex.StLinje.PerId
"PerId" "PerId" ? ? "character" ? ? ? ? ? ? yes ? yes 10 yes ""
     _FldNameList[36]   > skotex.StLinje.PerLinNr
"PerLinNr" "PerLinNr" ? ? "integer" ? ? ? ? ? ? yes ? no 8.2 yes ""
     _FldNameList[37]   > skotex.StLinje.PerLinTxt
"PerLinTxt" "PerLinTxt" ? ? "character" ? ? ? ? ? ? yes ? no 20 yes ""
     _FldNameList[38]   > skotex.StLinje.PrimoAnt
"PrimoAnt" "PrimoAnt" ? ? "integer" ? ? ? ? ? ? yes ? no 10.2 yes ""
     _FldNameList[39]   > skotex.StLinje.Primoverdi
"Primoverdi" "Primoverdi" ? ? "decimal" ? ? ? ? ? ? yes ? no 15.6 yes ""
     _FldNameList[40]   > skotex.StLinje.RegistrertAv
"RegistrertAv" "RegistrertAv" ? ? "character" ? ? ? ? ? ? yes ? no 10 yes ""
     _FldNameList[41]   > skotex.StLinje.RegistrertDato
"RegistrertDato" "RegistrertDato" ? ? "date" ? ? ? ? ? ? yes ? no 11.6 yes ""
     _FldNameList[42]   > skotex.StLinje.RegistrertTid
"RegistrertTid" "RegistrertTid" ? ? "integer" ? ? ? ? ? ? yes ? no 20.8 yes ""
     _FldNameList[43]   > skotex.StLinje.ReklAnt
"ReklAnt" "ReklAnt" ? ? "decimal" ? ? ? ? ? ? yes ? no 8.2 yes ""
     _FldNameList[44]   > skotex.StLinje.ReklLAnt
"ReklLAnt" "ReklLAnt" ? ? "decimal" ? ? ? ? ? ? yes ? no 13.4 yes ""
     _FldNameList[45]   > skotex.StLinje.ReklLVerdi
"ReklLVerdi" "ReklLVerdi" ? ? "decimal" ? ? ? ? ? ? yes ? no 32.6 yes ""
     _FldNameList[46]   > skotex.StLinje.ReklVerdi
"ReklVerdi" "ReklVerdi" ? ? "decimal" ? ? ? ? ? ? yes ? no 24.4 yes ""
     _FldNameList[47]   > skotex.StLinje.StTypeId
"StTypeId" "StTypeId" ? ? "character" ? ? ? ? ? ? yes ? yes 10 yes ""
     _FldNameList[48]   > skotex.StLinje.SvinnAnt
"SvinnAnt" "SvinnAnt" ? ? "decimal" ? ? ? ? ? ? yes ? no 10.8 yes ""
     _FldNameList[49]   > skotex.StLinje.SvinnVerdi
"SvinnVerdi" "SvinnVerdi" ? ? "decimal" ? ? ? ? ? ? yes ? no 14.4 yes ""
     _FldNameList[50]   > skotex.StLinje.TilbMvaVerdi
"TilbMvaVerdi" "TilbMvaVerdi" ? ? "decimal" ? ? ? ? ? ? yes ? no 13.4 yes ""
     _FldNameList[51]   > skotex.StLinje.TilbVVarekost
"TilbVVarekost" "TilbVVarekost" ? ? "decimal" ? ? ? ? ? ? yes ? no 13.2 yes ""
     _FldNameList[52]   > skotex.StLinje.TotalPost
"TotalPost" "TotalPost" ? ? "integer" ? ? ? ? ? ? yes ? no 9 yes ""
     _FldNameList[53]   > skotex.StLinje.Utsolgt%
"Utsolgt%" "Utsolgt%" ? ? "decimal" ? ? ? ? ? ? yes ? no 8.2 yes ""
     _FldNameList[54]   > skotex.StLinje.VerdiRabatt
"VerdiRabatt" "VerdiRabatt" ? ? "decimal" ? ? ? ? ? ? yes ? no 13 yes ""
     _FldNameList[55]   > skotex.StLinje.VerdiSolgt
"VerdiSolgt" "VerdiSolgt" ? ? "decimal" ? ? ? ? ? ? yes ? no 13 yes ""
     _FldNameList[56]   > skotex.StLinje.VerdiTilbSolgt
"VerdiTilbSolgt" "VerdiTilbSolgt" ? ? "decimal" ? ? ? ? ? ? yes ? no 13 yes ""
     _FldNameList[57]   > skotex.StLinje.VisBut
"VisBut" "VisBut" ? ? "character" ? ? ? ? ? ? yes ? no 7 yes ""
     _FldNameList[58]   > skotex.StLinje.VVarekost
"VVarekost" "VVarekost" ? ? "decimal" ? ? ? ? ? ? yes ? no 12 yes ""
     _FldNameList[59]   > skotex.StLinje.AarPerLinNr
"AarPerLinNr" "AarPerLinNr" ? ? "integer" ? ? ? ? ? ? yes ? no 11.4 yes ""
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE AssignTT_StLinje dTables  _DB-REQUIRED
PROCEDURE AssignTT_StLinje :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
        ASSIGN 
          TT_StLinje.AntSolgt    = TT_StLinje.AntSolgt    + TT_StLinjeTMP.AntSolgt
          TT_StLinje.VerdiSolgt  = TT_StLinje.VerdiSolgt  + TT_StLinjeTMP.VerdiSolgt
          TT_StLinje.MvaVerdi    = TT_StLinje.MvaVerdi    + TT_StLinjeTMP.MvaVerdi
          TT_StLinje.DbKr        = TT_StLinje.DbKr        + TT_StLinjeTMP.DbKr
          TT_StLinje.AntRab      = TT_StLinje.AntRab      + TT_StLinjeTMP.AntRab
          TT_StLinje.VerdiRabatt = TT_StLinje.VerdiRabatt + TT_StLinjeTMP.VerdiRabatt
          TT_StLinje.AntTilbSolgt   = TT_StLinje.AntTilbSolgt  + TT_StLinjeTMP.AntTilbSolgt  
          TT_StLinje.BrekkAnt       = TT_StLinje.BrekkAnt      + TT_StLinjeTMP.BrekkAnt      
          TT_StLinje.BrekkVerdi     = TT_StLinje.BrekkVerdi    + TT_StLinjeTMP.BrekkVerdi    
          TT_StLinje.GjenkjopAnt    = TT_StLinje.GjenkjopAnt   + TT_StLinjeTMP.GjenkjopAnt   
          TT_StLinje.GjenkjopVerdi  = TT_StLinje.GjenkjopVerdi + TT_StLinjeTMP.GjenkjopVerdi 
          TT_StLinje.IntAnt         = TT_StLinje.IntAnt        + TT_StLinjeTMP.IntAnt        
          TT_StLinje.IntVerdi       = TT_StLinje.IntVerdi      + TT_StLinjeTMP.IntVerdi      
          TT_StLinje.JustAnt        = TT_StLinje.JustAnt       + TT_StLinjeTMP.JustAnt       
          TT_StLinje.JustVerdi      = TT_StLinje.JustVerdi     + TT_StLinjeTMP.JustVerdi     
          TT_StLinje.KjopAnt        = TT_StLinje.KjopAnt       + TT_StLinjeTMP.KjopAnt       
          TT_StLinje.KjopVerdi      = TT_StLinje.KjopVerdi     + TT_StLinjeTMP.KjopVerdi     
          TT_StLinje.LagerAnt       = TT_StLinje.LagerAnt      + TT_StLinjeTMP.LagerAnt      
          TT_StLinje.LagerVerdi     = TT_StLinje.LagerVerdi    + TT_StLinjeTMP.LagerVerdi    
          TT_StLinje.ReklAnt        = TT_StLinje.ReklAnt        + TT_StLinjeTMP.ReklAnt       
          TT_StLinje.ReklLAnt       = TT_StLinje.ReklLAnt       + TT_StLinjeTMP.ReklLAnt      
          TT_StLinje.ReklLVerdi     = TT_StLinje.ReklLVerdi     + TT_StLinjeTMP.ReklLVerdi    
          TT_StLinje.ReklVerdi      = TT_StLinje.ReklVerdi      + TT_StLinjeTMP.ReklVerdi     
          TT_StLinje.OvAnt       = TT_StLinje.OvAnt       + TT_StLinjeTMP.OvAnt       
          TT_StLinje.OvVerdi     = TT_StLinje.OvVerdi     + TT_StLinjeTMP.OvVerdi     
          TT_StLinje.SvinnAnt       = TT_StLinje.SvinnAnt       + TT_StLinjeTMP.SvinnAnt       
          TT_StLinje.SvinnVerdi     = TT_StLinje.SvinnVerdi     + TT_StLinjeTMP.SvinnVerdi     
          TT_StLinje.VerdiTilbSolgt = TT_StLinje.VerdiTilbSolgt + TT_StLinjeTMP.VerdiTilbSolgt
          TT_StLinje.VVarekost      = TT_StLinje.VVarekost      + TT_StLinjeTMP.VVarekost     
          TT_StLinje.BruttoSolgt    = TT_StLinje.BruttoSolgt    + TT_StLinjeTMP.BruttoSolgt.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DATA.CALCULATE dTables  DATA.CALCULATE _DB-REQUIRED
PROCEDURE DATA.CALCULATE :
/*------------------------------------------------------------------------------
  Purpose:     Calculate all the Calculated Expressions found in the
               SmartDataObject.
  Parameters:  <none>
------------------------------------------------------------------------------*/

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE FillKund_Medl dTables  _DB-REQUIRED
PROCEDURE FillKund_Medl :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER cStTypeId AS CHARACTER  NO-UNDO.
  DEFINE INPUT  PARAMETER cObjekt AS CHARACTER  NO-UNDO.
  IF cStTypeId = "KUNDSTAT" THEN DO:
      FIND Kunde WHERE Kunde.KundeNr = DECI(cObjekt) NO-LOCK NO-ERROR.
      IF AVAIL Kunde THEN DO:
          FIND Post WHERE Post.Postnr = Kunde.PostNr NO-LOCK NO-ERROR.
          ASSIGN TT_StLinje.Beskrivelse = Kunde.Navn
                 TT_StLinje.Adresse1    = Kunde.Adresse1
                 TT_StLinje.Adresse2    = Kunde.Adresse2
                 TT_StLinje.PostNr      = Kunde.PostNr
                 TT_StLinje.PostAdr     = IF AVAIL Post THEN Post.Beskrivelse ELSE ""
                 TT_StLinje.Telefon     = Kunde.Telefon
                 TT_StLinje.EMail       = Kunde.ePostAdresse
                 TT_StLinje.Kilde       = Kunde.Kilde
                 TT_StLinje.TilgKilde   = Kunde.TilgKilde
                 .

      END.
      ELSE 
          ASSIGN TT_StLinje.Beskrivelse = "Ukjent".
  END.
  ELSE IF cStTypeId = "MEDLEM" THEN DO:
      FIND Medlem WHERE Medlem.MedlemsNr = DECI(cObjekt) NO-LOCK NO-ERROR.
      IF AVAIL Medlem THEN DO:
          FIND Post WHERE Post.Postnr = Medlem.PostNr NO-LOCK NO-ERROR.
          ASSIGN TT_StLinje.Beskrivelse = Medlem.EtterNavn + "," + Medlem.ForNavn
                 TT_StLinje.Fornavn     = Medlem.ForNavn
                 TT_StLinje.EtterNavn   = Medlem.EtterNavn
                 TT_StLinje.Adresse1    = Medlem.Adresse1
                 TT_StLinje.Adresse2    = Medlem.Adresse2
                 TT_StLinje.PostNr      = Medlem.PostNr
                 TT_StLinje.PostAdr     = IF AVAIL Post THEN Post.Beskrivelse ELSE ""
                 TT_StLinje.Telefon     = Medlem.Telefon
                 TT_StLinje.EMail       = Medlem.ePostAdresse
                 TT_StLinje.Kilde       = Medlem.Kilde
                 TT_StLinje.TilgKilde   = Medlem.TilgKilde
                 TT_StLinje.MedType     = Medlem.MedType
                 TT_StLinje.MedGruppe   = Medlem.MedGruppe
                 TT_StLinje.MobilTlf    = Medlem.MobilTlf
                 TT_StLinje.MKlubbId    = Medlem.MKlubbId
                 .
      END.
      ELSE 
          ASSIGN TT_StLinje.Beskrivelse = "Ukjent".
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initializeObject dTables  _DB-REQUIRED
PROCEDURE initializeObject :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  {syspara.i 1 1 18 cHKinst}

  RUN SUPER.

  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE StLinjeJmfToTT dTables  _DB-REQUIRED
PROCEDURE StLinjeJmfToTT :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE OUTPUT PARAMETER TTH AS HANDLE NO-UNDO.
  DEFINE INPUT  PARAMETER cQry1     AS CHARACTER  NO-UNDO.
  DEFINE INPUT  PARAMETER cQry2     AS CHARACTER  NO-UNDO.
  DEFINE INPUT  PARAMETER cStTypeId AS CHARACTER  NO-UNDO.
  DEFINE INPUT  PARAMETER cButiker  AS CHARACTER  NO-UNDO.
  DEFINE INPUT  PARAMETER cXFilter  AS CHARACTER  NO-UNDO.
  DEFINE INPUT  PARAMETER cXParam   AS CHARACTER  NO-UNDO.
  DEFINE INPUT  PARAMETER lVisPeriode AS LOGICAL    NO-UNDO.
  DEFINE        VARIABLE iQueries AS INTEGER    NO-UNDO.
  DEFINE        VARIABLE  cQuery AS CHARACTER  NO-UNDO.
  DEFINE        VARIABLE  lFirst    AS LOGICAL    NO-UNDO.
  DEFINE        VARIABLE  rRowId    AS ROWID      NO-UNDO.
  DEFINE        VARIABLE  dTotSum1   AS DECIMAL    NO-UNDO.
  DEFINE        VARIABLE  dTotSum2   AS DECIMAL    NO-UNDO.
  DEFINE        VARIABLE  hBufferField AS HANDLE     NO-UNDO.
  DEFINE        VARIABLE  pcFeltListe  AS CHARACTER  NO-UNDO.
  DEFINE        VARIABLE  pcVerdier    AS CHARACTER  NO-UNDO.
  DEFINE        VARIABLE  iButikloop  AS INTEGER    NO-UNDO.
  DEFINE        VARIABLE  lUtvidetFilter AS LOGICAL    NO-UNDO.
  DEFINE        VARIABLE  lIkkeTreff     AS LOGICAL    NO-UNDO.
  DEFINE        VARIABLE  dDato AS DATE       NO-UNDO.
  DEFINE        VARIABLE lVisPerBut AS LOGICAL    NO-UNDO.
  EMPTY TEMP-TABLE TT_StLinje.
  EMPTY TEMP-TABLE TT_StLinjeJmfTMP.
  EMPTY TEMP-TABLE TT_StLinjeJmf.
  EMPTY TEMP-TABLE TT_StLinjeJmfTMP.
  IF NUM-ENTRIES(cXFilter,";") = 2 THEN DO:
      ASSIGN pcFeltListe = ENTRY(1,cXFilter,";")
             pcVerdier   = ENTRY(2,cXFilter,";").
      IF pcFeltListe <> pcVerdier THEN  /* */
          ASSIGN lUtvidetFilter = TRUE.
  END.
  ASSIGN lVisPerBut = IF NUM-ENTRIES(cXParam,CHR(1)) = 2 THEN ENTRY(2,cXParam,CHR(1)) = "J" ELSE FALSE
         cXParam    = ENTRY(1,cXParam,CHR(1)).

  DO iQueries = 1 TO 2:
      ASSIGN cQuery = IF iQueries = 1 THEN cQry1 ELSE cQry2.
      DO iButikLoop = 1 TO NUM-ENTRIES(cButiker):
          DYNAMIC-FUNCTION('closeQuery':U).
          DYNAMIC-FUNCTION('setQueryString':U,
             INPUT REPLACE(cQuery,"SUBSTBUTIK","Butik = " + ENTRY(iButikLoop,cButiker) + " ") /* CHARACTER */).
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
              IF NOT AVAIL RowObject OR rRowId = ROWID(RowObject) THEN
                  LEAVE.
              ELSE
                  ASSIGN rRowId = ROWID(RowObject).
              IF lUtvidetFilter = TRUE THEN DO:
                  IF cStTypeId = "AVDELING" THEN DO:
                      IF pcFeltListe = "AvdelingNr" THEN DO:
                          IF NOT CAN-DO(pcVerdier,LEFT-TRIM(RowObject.DataObjekt,"0")) THEN
                              NEXT.
                      END.
                  END.
                  ELSE IF cStTypeId = "HOVEDGR" THEN DO:
                      IF pcFeltListe = "Hg" AND NOT CAN-DO(pcVerdier,LEFT-TRIM(RowObject.DataObjekt,"0")) THEN
                          NEXT.
                      ELSE IF pcFeltListe = "AvdelingNr" THEN DO:
                          FIND HuvGr WHERE HuvGr.Hg = INT(RowObject.DataObjekt) NO-LOCK NO-ERROR.
                          IF NOT AVAIL HuvGr OR (pcFeltListe = "AvdelingNr" AND NOT CAN-DO(pcVerdier,STRING(HuvGr.AvdelingNr))) THEN
                              NEXT.
                      END.
                  END.
                  ELSE IF cStTypeId = "VAREGR" THEN DO:
                      IF pcFeltListe = "Vg" AND NOT CAN-DO(pcVerdier,LEFT-TRIM(RowObject.DataObjekt,"0")) THEN
                          NEXT.
                      ELSE IF pcFeltListe <> "Vg" THEN DO:
                          FIND VarGr WHERE VarGr.Vg = INT(RowObject.DataObjekt) NO-LOCK NO-ERROR.
                          IF NOT AVAIL VarGr OR (pcFeltListe = "Hg" AND NOT CAN-DO(pcVerdier,STRING(VarGr.Hg))) THEN
                              NEXT.
                          ELSE IF pcFeltListe = "AvdelingNr" THEN DO:
                              FIND HuvGr OF VarGr NO-LOCK NO-ERROR.
                              IF NOT AVAIL HuvGr OR (pcFeltListe = "AvdelingNr" AND NOT CAN-DO(pcVerdier,STRING(HuvGr.AvdelingNr))) THEN
                                  NEXT.
                          END.
                      END.
                  END.
                  ELSE IF cStTypeId = "LEVERAN" OR cStTypeId = "LEVERAN-VG" THEN DO:
                    /*  */
                      IF NOT CAN-DO(ENTRY(1,pcVerdier,CHR(1)),STRING(INT(ENTRY(1,RowObject.DataObjekt,CHR(1))))) THEN
                          NEXT.
                      IF NUM-ENTRIES(pcFeltListe) = 2 AND ENTRY(2,pcFeltListe) <> "" THEN DO:
                          IF ENTRY(2,pcFeltListe) = "Vg" AND NOT CAN-DO(ENTRY(2,pcVerdier,CHR(1)),LEFT-TRIM(ENTRY(2,RowObject.DataObjekt,CHR(1)),"0")) THEN
                              NEXT.
                          ELSE IF ENTRY(2,pcFeltListe) <> "Vg" THEN DO:
                              FIND VarGr WHERE VarGr.Vg = INT(ENTRY(2,RowObject.DataObjekt,CHR(1))) NO-LOCK NO-ERROR.
                              IF NOT AVAIL VarGr OR (ENTRY(2,pcFeltListe) = "Hg" AND NOT CAN-DO(ENTRY(2,pcVerdier,CHR(1)),STRING(VarGr.Hg))) THEN
                                  NEXT.
                              ELSE IF ENTRY(2,pcFeltListe) = "AvdelingNr" THEN DO:
                                  FIND HuvGr OF VarGr NO-LOCK NO-ERROR.
                                  IF NOT AVAIL HuvGr OR (ENTRY(2,pcFeltListe) = "AvdelingNr" AND NOT CAN-DO(ENTRY(2,pcVerdier,CHR(1)),STRING(HuvGr.AvdelingNr))) THEN
                                      NEXT.
                              END.
                          END.
                      END.
                  END.
                  ELSE IF cStTypeId = "LEVERAN-SA" THEN DO:
                      IF NOT CAN-DO(ENTRY(1,pcVerdier,CHR(1)),STRING(INT(ENTRY(1,RowObject.DataObjekt,CHR(1))))) THEN
                        NEXT.
                      IF NUM-ENTRIES(pcFeltListe) = 2 AND ENTRY(2,pcFeltListe) <> "" THEN DO:
                          IF NOT CAN-DO(ENTRY(2,pcVerdier,CHR(1)),LEFT-TRIM(ENTRY(2,RowObject.DataObjekt,CHR(1)),"0")) THEN
                                NEXT.
                      END.
                  END.
              END. /* utvidet filter */
              FIND FIRST TT_StLinjeJmfTMP NO-ERROR.
              BUFFER-COPY RowObject TO TT_StLinjeJmfTMP.
              ASSIGN TT_StLinjeJmfTMP.DbKr  = TT_StLinjeJmfTMP.VerdiSolgt - TT_StLinjeJmfTMP.VVareKost
                     TT_StLinjeJmfTMP.Butik = IF lVisPerBut = TRUE THEN TT_StLinjeJmfTMP.Butik ELSE 0
/*                      TT_StLinjeJmfTMP.Butik = 0 */
                     TT_StLinjeJmfTMP.AarPerLinNr = IF lVisPeriode = TRUE THEN TT_StLinjeJmfTMP.AarPerLinNr ELSE 0
                     TT_StLinjeJmfTMP.BruttoSolgt = TT_StLinjeJmfTMP.VerdiSolgt + TT_StLinjeJmfTMP.MvaVerdi.
/*               IF cStTypeId = "LEVERAN-VG" AND  THEN                                                 */
/*                   ASSIGN TT_StLinjeJmfTMP.DataObjekt = ENTRY(1,TT_StLinjeJmfTMP.DataObjekt,CHR(1)). */
             IF cStTypeId = "LEVERAN-SA" THEN DO:
                 ASSIGN TT_StLinjeTMP.Sasong      = INT(ENTRY(2,TT_StLinjeTMP.DataObjekt,CHR(1))) NO-ERROR.
                 FIND sasong WHERE sasong.sasong = TT_StLinjeTMP.Sasong NO-LOCK NO-ERROR.
                 TT_StLinjeTMP.SasBeskr = IF AVAIL Sasong THEN Sasong.sasbeskr ELSE "Ukjent".
                 RELEASE sasong.
             END.

             ELSE IF cXParam <> "" THEN DO:
                  FIND VarGr WHERE VarGr.Vg = INT(ENTRY(2,TT_StLinjeJmfTMP.DataObjekt,CHR(1))) NO-LOCK NO-ERROR.
                  IF AVAIL VarGr THEN
                      FIND HuvGr OF VarGr NO-LOCK NO-ERROR.
                  IF AVAIL HuvGr THEN
                      FIND Avdeling OF HuvGr NO-LOCK NO-ERROR.
                  CASE cXParam:
                    WHEN "Avd" THEN DO:
                      ASSIGN TT_StLinjeJmfTMP.DataObjekt = ENTRY(1,TT_StLinjeJmfTMP.DataObjekt,CHR(1)) + CHR(1) +
                           IF AVAIL Avdeling THEN STRING(Avdeling.AvdelingNr,"9999") ELSE "0000"
                         TT_StLinjeJmfTMP.AvdelingNr   = IF AVAIL Avdeling THEN Avdeling.AvdelingNr ELSE 0
                         TT_StLinjeJmfTMP.AvdelingNavn = IF AVAIL Avdeling THEN Avdeling.AvdelingNavn ELSE "Ukjent".
                    END.
                    WHEN "Hg" THEN DO:
                      ASSIGN TT_StLinjeJmfTMP.DataObjekt = ENTRY(1,TT_StLinjeJmfTMP.DataObjekt,CHR(1)) + CHR(1) +
                                  IF AVAIL HuvGr THEN STRING(HuvGr.Hg,"9999") ELSE "0000"
                         TT_StLinjeJmfTMP.Hg      = IF AVAIL HuvGr THEN HuvGr.Hg ELSE 0
                         TT_StLinjeJmfTMP.HgBeskr = IF AVAIL HuvGr THEN HuvGr.HgBeskr ELSE "Ukjent".
                    END.
                    WHEN "Vg" THEN DO:
                      ASSIGN TT_StLinjeJmfTMP.DataObjekt = ENTRY(1,TT_StLinjeJmfTMP.DataObjekt,CHR(1)) + CHR(1) +
                                  IF AVAIL VarGr THEN STRING(VarGr.Vg,"999999") ELSE "000000"
                         TT_StLinjeJmfTMP.Vg      = IF AVAIL VarGr THEN VarGr.Vg ELSE 0
                         TT_StLinjeJmfTMP.VgBeskr = IF AVAIL VarGr THEN VarGr.VgBeskr ELSE "Ukjent".
                    END.
                  END CASE.
                  RELEASE Avdeling.
                  RELEASE HuvGr.
                  RELEASE VarGr.
              END.
              FIND CURRENT TT_StLinjeJmfTMP.
              IF iQueries = 1 THEN
                  ASSIGN dTotSum1 = dTotSum1 + TT_StLinjeJmfTMP.VerdiSolgt.
              ELSE
                  ASSIGN dTotSum2 = dTotSum2 + TT_StLinjeJmfTMP.VerdiSolgt.
              DO:
                  FIND TT_StLinjeJmf WHERE TT_StLinjeJmf.Butik   = TT_StLinjeJmfTMP.Butik      AND /* allt på en butik = 0 */
                                        TT_StLinjeJmf.StTypeId   = TT_StLinjeJmfTMP.StTypeId   AND
                                        TT_StLinjeJmf.PerId      = TT_StLinjeJmfTMP.PerId      AND
                                        TT_StLinjeJmf.AarPerLinNr = TT_StLinjeJmfTMP.AarPerLinNr AND
                                        TT_StLinjeJmf.DataObjekt = TT_StLinjeJmfTMP.DataObjekt USE-INDEX AarPerLinNr NO-ERROR.
                  IF NOT AVAIL TT_StLinjeJmf THEN DO:
                      IF lVisPeriode = TRUE THEN
                          ASSIGN dDato = IF TT_StLinjeJmfTMP.PerId = "DAG" THEN DATE(1,1,TT_StLinjeJmfTMP.Aar) + TT_StLinjeJmfTMP.PerLinNr - 1 ELSE ?
                                 TT_StLinjeJmfTMP.PerLinTxt = IF TT_StLinjeJmfTMP.PerId = "DAG" THEN
                                   STRING(YEAR(dDato)) + "/" + STRING(MONTH(dDato),"99") + "/" + STRING(DAY(dDato),"99") ELSE 
                                    IF TT_StLinjeJmfTMP.PerId = "AAR" THEN
                                        STRING(TT_StLinjeJmfTMP.Aar) ELSE
                                    STRING(TT_StLinjeJmfTMP.Aar) + "-" + STRING(TT_StLinjeJmfTMP.PerLinNr,"99").
                      CREATE TT_StLinjeJmf.
                      IF iQueries = 1 THEN DO:
                          BUFFER-COPY TT_StLinjeJmfTMP TO TT_StLinjeJmf.
                      END.
                      ELSE DO:
                          BUFFER-COPY TT_StLinjeJmfTMP USING Butik StTypeId PerId AarPerLinNr DataObjekt PerLinTxt Vg VgBeskr hG HgBeskr AvdelingNr AvdelingNavn TO TT_StLinjeJmf.
                          ASSIGN TT_StLinjeJmf.AntSolgt2    = TT_StLinjeJmfTMP.AntSolgt   
                                 TT_StLinjeJmf.VerdiSolgt2  = TT_StLinjeJmfTMP.VerdiSolgt 
                                 TT_StLinjeJmf.MvaVerdi2    = TT_StLinjeJmfTMP.MvaVerdi   
                                 TT_StLinjeJmf.DbKr2        = TT_StLinjeJmfTMP.DbKr       
                                 TT_StLinjeJmf.AntRabatt2   = TT_StLinjeJmfTMP.AntRabatt  
                                 TT_StLinjeJmf.VerdiRabatt2 = TT_StLinjeJmfTMP.VerdiRabatt
                                 TT_StLinjeJmf.BruttoSolgt2 = TT_StLinjeJmfTMP.BruttoSolgt.
                      END.
                      ASSIGN TT_StLinjeJmf.Beskrivelse = getBeskr(cStTypeId,TT_StLinjeJmf.DataObjekt).
                  END.
                  ELSE DO:
                      IF iQueries = 1 THEN
                          ASSIGN TT_StLinjeJmf.AntSolgt    = TT_StLinjeJmf.AntSolgt    + TT_StLinjeJmfTMP.AntSolgt
                                 TT_StLinjeJmf.VerdiSolgt  = TT_StLinjeJmf.VerdiSolgt  + TT_StLinjeJmfTMP.VerdiSolgt
                                 TT_StLinjeJmf.MvaVerdi    = TT_StLinjeJmf.MvaVerdi    + TT_StLinjeJmfTMP.MvaVerdi
                                 TT_StLinjeJmf.DbKr        = TT_StLinjeJmf.DbKr        + TT_StLinjeJmfTMP.DbKr
                                 TT_StLinjeJmf.AntRabatt   = TT_StLinjeJmf.AntRabatt   + TT_StLinjeJmfTMP.AntRabatt
                                 TT_StLinjeJmf.VerdiRabatt = TT_StLinjeJmf.VerdiRabatt + TT_StLinjeJmfTMP.VerdiRabatt
                                 TT_StLinjeJmf.BruttoSolgt = TT_StLinjeJmf.BruttoSolgt + TT_StLinjeJmfTMP.BruttoSolgt.
                       ELSE
                           ASSIGN TT_StLinjeJmf.AntSolgt2    = TT_StLinjeJmf.AntSolgt2    + TT_StLinjeJmfTMP.AntSolgt
                                  TT_StLinjeJmf.VerdiSolgt2  = TT_StLinjeJmf.VerdiSolgt2  + TT_StLinjeJmfTMP.VerdiSolgt
                                  TT_StLinjeJmf.MvaVerdi2    = TT_StLinjeJmf.MvaVerdi2    + TT_StLinjeJmfTMP.MvaVerdi
                                  TT_StLinjeJmf.DbKr2        = TT_StLinjeJmf.DbKr2        + TT_StLinjeJmfTMP.DbKr
                                  TT_StLinjeJmf.AntRabatt2   = TT_StLinjeJmf.AntRabatt2   + TT_StLinjeJmfTMP.AntRabatt
                                  TT_StLinjeJmf.VerdiRabatt2 = TT_StLinjeJmf.VerdiRabatt2 + TT_StLinjeJmfTMP.VerdiRabatt
                                  TT_StLinjeJmf.BruttoSolgt2 = TT_StLinjeJmf.BruttoSolgt2 + TT_StLinjeJmfTMP.BruttoSolgt.
/*                              TT_StLinjeJmf.AntTilbSolgt   = TT_StLinjeJmf.AntTilbSolgt  + TT_StLinjeJmfTMP.AntTilbSolgt    */
/*                              TT_StLinjeJmf.BrekkAnt       = TT_StLinjeJmf.BrekkAnt      + TT_StLinjeJmfTMP.BrekkAnt        */
/*                              TT_StLinjeJmf.BrekkVerdi     = TT_StLinjeJmf.BrekkVerdi    + TT_StLinjeJmfTMP.BrekkVerdi      */
/*                              TT_StLinjeJmf.GjenkjopAnt    = TT_StLinjeJmf.GjenkjopAnt   + TT_StLinjeJmfTMP.GjenkjopAnt     */
/*                              TT_StLinjeJmf.GjenkjopVerdi  = TT_StLinjeJmf.GjenkjopVerdi + TT_StLinjeJmfTMP.GjenkjopVerdi   */
/*                              TT_StLinjeJmf.IntAnt         = TT_StLinjeJmf.IntAnt        + TT_StLinjeJmfTMP.IntAnt          */
/*                              TT_StLinjeJmf.IntVerdi       = TT_StLinjeJmf.IntVerdi      + TT_StLinjeJmfTMP.IntVerdi        */
/*                              TT_StLinjeJmf.JustAnt        = TT_StLinjeJmf.JustAnt       + TT_StLinjeJmfTMP.JustAnt         */
/*                              TT_StLinjeJmf.JustVerdi      = TT_StLinjeJmf.JustVerdi     + TT_StLinjeJmfTMP.JustVerdi       */
/*                              TT_StLinjeJmf.KjopAnt        = TT_StLinjeJmf.KjopAnt       + TT_StLinjeJmfTMP.KjopAnt         */
/*                              TT_StLinjeJmf.KjopVerdi      = TT_StLinjeJmf.KjopVerdi     + TT_StLinjeJmfTMP.KjopVerdi       */
/*                              TT_StLinjeJmf.LagerAnt       = TT_StLinjeJmf.LagerAnt      + TT_StLinjeJmfTMP.LagerAnt        */
/*                              TT_StLinjeJmf.LagerVerdi     = TT_StLinjeJmf.LagerVerdi    + TT_StLinjeJmfTMP.LagerVerdi      */
/*                              TT_StLinjeJmf.ReklAnt        = TT_StLinjeJmf.ReklAnt        + TT_StLinjeJmfTMP.ReklAnt        */
/*                              TT_StLinjeJmf.ReklLAnt       = TT_StLinjeJmf.ReklLAnt       + TT_StLinjeJmfTMP.ReklLAnt       */
/*                              TT_StLinjeJmf.ReklLVerdi     = TT_StLinjeJmf.ReklLVerdi     + TT_StLinjeJmfTMP.ReklLVerdi     */
/*                              TT_StLinjeJmf.ReklVerdi      = TT_StLinjeJmf.ReklVerdi      + TT_StLinjeJmfTMP.ReklVerdi      */
/*                              TT_StLinjeJmf.SvinnAnt       = TT_StLinjeJmf.SvinnAnt       + TT_StLinjeJmfTMP.SvinnAnt       */
/*                              TT_StLinjeJmf.SvinnVerdi     = TT_StLinjeJmf.SvinnVerdi     + TT_StLinjeJmfTMP.SvinnVerdi     */
/*                              TT_StLinjeJmf.VerdiTilbSolgt = TT_StLinjeJmf.VerdiTilbSolgt + TT_StLinjeJmfTMP.VerdiTilbSolgt */
/*                              TT_StLinjeJmf.VVarekost      = TT_StLinjeJmf.VVarekost      + TT_StLinjeJmfTMP.VVarekost      */
                  END.
              END.
          END.
      END.
      FOR EACH TT_StLinjeJmf:
          ASSIGN 
                 TT_StLinjeJmf.AntDiff%   = IF TT_StLinjeJmf.antsolgt2 <= 0 THEN 0 ELSE (TT_StLinjeJmf.antsolgt - TT_StLinjeJmf.antsolgt2) * 100 / TT_StLinjeJmf.antsolgt2
                 TT_StLinjeJmf.SolgtDiff% = IF TT_StLinjeJmf.verdisolgt2 <= 0 THEN 0 ELSE (TT_StLinjeJmf.verdisolgt - TT_StLinjeJmf.verdisolgt2) * 100 / TT_StLinjeJmf.verdisolgt2
                 TT_StLinjeJmf.Db%    = IF TT_StLinjeJmf.VerdiSolgt <= 0 THEN 0 ELSE (TT_StLinjeJmf.DbKr * 100) / TT_StLinjeJmf.VerdiSolgt
                 TT_StLinjeJmf.Solgt% = ROUND(TT_StLinjeJmf.VerdiSolgt / dTotsum1 * 100,1)
                 TT_StLinjeJmf.Rab%   = IF TT_StLinjeJmf.VerdiSolgt + TT_StLinjeJmf.VerdiRabatt = 0 THEN 0 ELSE 
                                   ((TT_StLinjeJmf.VerdiRabatt) * 100) / (TT_StLinjeJmf.VerdiSolgt + TT_StLinjeJmf.VerdiRabatt)
                 TT_StLinjeJmf.Db%2    = IF TT_StLinjeJmf.VerdiSolgt2 <= 0 THEN 0 ELSE (TT_StLinjeJmf.DbKr2 * 100) / TT_StLinjeJmf.VerdiSolgt2
                 TT_StLinjeJmf.Solgt%2 = ROUND(TT_StLinjeJmf.VerdiSolgt2 / dTotsum2 * 100,1)
                 TT_StLinjeJmf.Rab%2   = IF TT_StLinjeJmf.VerdiSolgt2 + TT_StLinjeJmf.VerdiRabatt2 = 0 THEN 0 ELSE 
                                                            ((TT_StLinjeJmf.VerdiRabatt2) * 100) / (TT_StLinjeJmf.VerdiSolgt2 + TT_StLinjeJmf.VerdiRabatt2)
                 TT_StLinjeJmf.DbKrDiff% = IF TT_StLinjeJmf.DbKr2 <= 0 THEN 0 ELSE (TT_StLinjeJmf.DbKr - TT_StLinjeJmf.DbKr2) * 100 / TT_StLinjeJmf.DbKr2
                   .
          IF lVisPerBut = TRUE THEN
              TT_StLinjeJmf.Butnamn  = getBeskr("GETBUTNAVN",STRING(TT_StLinjeJmf.Butik)).

          IF cStTypeId = "LEVERAN" OR cStTypeId = "LEVERAN-VG" OR cStTypeId = "LEVERAN-SA" THEN DO:
              ASSIGN TT_StLinjeJmf.LevNr   = INT(ENTRY(1,TT_StLinjeJmf.DataObjekt,CHR(1)))
                     TT_StLinjeJmf.LevNamn = getBeskr(cStTypeId,ENTRY(1,TT_StLinjeJmf.DataObjekt,CHR(1))).
          END.
      END.
      DYNAMIC-FUNCTION('closeQuery':U).
      ASSIGN TTH = BUFFER TT_StLinjeJmf:HANDLE.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE StLinjeToTT dTables  _DB-REQUIRED
PROCEDURE StLinjeToTT :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes: 
------------------------------------------------------------------------------*/
DEF OUTPUT PARAMETER TTH AS HANDLE NO-UNDO.
DEF INPUT  PARAMETER cStTypeId AS CHARACTER  NO-UNDO.
DEF INPUT  PARAMETER cButiker  AS CHARACTER  NO-UNDO.
DEF INPUT  PARAMETER cXFilter  AS CHARACTER  NO-UNDO.
DEF INPUT  PARAMETER cXParam   AS CHARACTER  NO-UNDO.
DEF INPUT  PARAMETER lVisPeriode AS LOGICAL    NO-UNDO.
DEF VARIABLE  cQuery AS CHARACTER  NO-UNDO.
DEF VARIABLE  lFirst    AS LOGICAL    NO-UNDO.
DEF VARIABLE  rRowId    AS ROWID      NO-UNDO.
DEF VARIABLE  dTotSum   AS DECIMAL    NO-UNDO.
DEF VARIABLE  hBufferField AS HANDLE     NO-UNDO.
DEF VARIABLE  pcFeltListe  AS CHARACTER  NO-UNDO.
DEF VARIABLE  pcVerdier    AS CHARACTER  NO-UNDO.
DEF VARIABLE  iButikloop  AS INTEGER    NO-UNDO.
DEF VARIABLE  lUtvidetFilter AS LOGICAL    NO-UNDO.
DEF VARIABLE  lIkkeTreff     AS LOGICAL    NO-UNDO.
DEF VARIABLE  dDato          AS DATE       NO-UNDO.
DEF VARIABLE  cOmsVerdier AS CHARACTER  NO-UNDO.
DEF VARIABLE  dBruttoFra  AS DECIMAL    NO-UNDO.
DEF VARIABLE  dBruttoTil  AS DECIMAL    NO-UNDO.
DEF VARIABLE  cKilde AS CHARACTER  NO-UNDO.
DEF VARIABLE  cTilgKilde AS CHARACTER  NO-UNDO.
DEF VARIABLE  ii         AS INTE NO-UNDO.
DEF VAR hQuery AS HANDLE NO-UNDO.  
DEF VARIABLE  dDBTot   AS DECIMAL    NO-UNDO.
DEF VARIABLE  dRabTot  AS DECIMAL    NO-UNDO.
DEF VARIABLE  dKjopTot AS DECIMAL    NO-UNDO.
DEF VARIABLE lVisPerBut AS LOGICAL    NO-UNDO.
DEFINE VARIABLE iMKlubbId AS INTEGER     NO-UNDO.
EMPTY TEMP-TABLE TT_StLinjeJmf.
EMPTY TEMP-TABLE TT_StLinjeJmfTMP.
EMPTY TEMP-TABLE TT_StLinje.
EMPTY TEMP-TABLE TT_StLinjeTMP.

CREATE TT_StLinjeTMP. /* en temporär record för att kunna summera */
  IF NUM-ENTRIES(cXFilter,";") = 2 THEN DO:
    ASSIGN pcFeltListe = ENTRY(1,cXFilter,";")
           pcVerdier   = ENTRY(2,cXFilter,";").
    IF cStTypeId = "MEDLEM" AND pcFeltListe <> "" THEN DO:
      IF NUM-ENTRIES(pcVerdier,CHR(2)) = 2 THEN do:
        ASSIGN 
         cOmsVerdier = ENTRY(2,pcVerdier,CHR(2))
         dBruttoFra  = DECI(ENTRY(1,cOmsVerdier))
         dBruttoTil  = DECI(ENTRY(2,cOmsVerdier)).
      end.
      pcVerdier   = ENTRY(1,pcVerdier,CHR(2)).
      iMKlubbId = ?. /* det finns klubbnr med 0 */
      DO ii = 2 TO NUM-ENTRIES(pcFeltListe):
        IF ENTRY(ii,pcFeltListe) = "Kilde" then
          ASSIGN cKilde = ENTRY(ii,pcVerdier,chr(1)).
        ELSE IF ENTRY(ii,pcFeltListe) = "TilgKilde" then
          ASSIGN cTilgKilde = ENTRY(ii,pcVerdier,chr(1)).
        ELSE IF ENTRY(ii,pcFeltListe) = "Medlemsklubb" then
            ASSIGN iMKlubbId = INT(ENTRY(ii,pcVerdier,chr(1))).
      END.
      pcVerdier = entry(1,pcVerdier,chr(1)).
    END.
    ELSE IF cStTypeId = "KUNDSTAT" AND pcFeltListe <> pcVerdier THEN DO:
      DO ii = 2 TO NUM-ENTRIES(pcFeltListe):
        IF ENTRY(ii,pcFeltListe) = "Kilde" then
          ASSIGN cKilde = ENTRY(ii,pcVerdier,CHR(1)).
        ELSE IF ENTRY(ii,pcFeltListe) = "TilgKilde" then
          ASSIGN cTilgKilde = ENTRY(ii,pcVerdier,CHR(1)).
      END.
      pcFeltListe = ENTRY(1,pcFeltListe).
      pcVerdier   = ENTRY(1,pcVerdier,CHR(1)).
    END.
    IF pcFeltListe <> pcVerdier THEN  /* */
        ASSIGN lUtvidetFilter = TRUE.
  END.
  /* Specialhantering av Vis/But  */
  /* Parameter cXParam används för att ge möjlighet att visa per butik */
  /* I de statistiktyper som vi skall 'Vis/But' läggs CHR(1)J */
  ASSIGN lVisPerBut = IF NUM-ENTRIES(cXParam,CHR(1)) = 2 THEN ENTRY(2,cXParam,CHR(1)) = "J" ELSE FALSE
         cXParam    = ENTRY(1,cXParam,CHR(1)).

  ASSIGN cQuery = DYNAMIC-FUNCTION('getQueryString':U).
/*   MESSAGE cQuery                         */
/*       VIEW-AS ALERT-BOX INFO BUTTONS OK. */
  DO iButikLoop = 1 TO NUM-ENTRIES(cButiker):
      DYNAMIC-FUNCTION('closeQuery':U).
      DYNAMIC-FUNCTION('setQueryString':U,
     INPUT REPLACE(cQuery,"SUBSTBUTIK","Butik = " + ENTRY(iButikLoop,cButiker) + " ") /* CHARACTER */).
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
    IF NOT AVAIL RowObject OR rRowId = ROWID(RowObject) THEN
      LEAVE.
    ELSE
      ASSIGN rRowId = ROWID(RowObject).
    IF lUtvidetFilter = TRUE THEN DO:
      IF cStTypeId = "AVDELING" THEN DO:
        IF pcFeltListe = "AvdelingNr" THEN DO:
          IF NOT CAN-DO(pcVerdier,LEFT-TRIM(RowObject.DataObjekt,"0")) THEN
            NEXT.
        END.
      END.
      ELSE IF cStTypeId = "HOVEDGR" THEN DO:
        IF pcFeltListe = "Hg" AND NOT CAN-DO(pcVerdier,LEFT-TRIM(RowObject.DataObjekt,"0")) THEN
              NEXT.
        ELSE IF pcFeltListe = "AvdelingNr" THEN DO:
            FIND HuvGr WHERE HuvGr.Hg = INT(RowObject.DataObjekt) NO-LOCK NO-ERROR.
            IF NOT AVAIL HuvGr OR (pcFeltListe = "AvdelingNr" AND NOT CAN-DO(pcVerdier,STRING(HuvGr.AvdelingNr))) THEN
                NEXT.
        END.
      END.
      ELSE IF cStTypeId = "VAREGR" THEN DO:
        IF pcFeltListe = "Vg" AND NOT CAN-DO(pcVerdier,LEFT-TRIM(RowObject.DataObjekt,"0")) THEN
              NEXT.
        ELSE IF pcFeltListe <> "Vg" THEN DO:
            FIND VarGr WHERE VarGr.Vg = INT(RowObject.DataObjekt) NO-LOCK NO-ERROR.
            IF NOT AVAIL VarGr OR (pcFeltListe = "Hg" AND NOT CAN-DO(pcVerdier,STRING(VarGr.Hg))) THEN
                NEXT.
            ELSE IF pcFeltListe = "AvdelingNr" THEN DO:
                FIND HuvGr OF VarGr NO-LOCK NO-ERROR.
                IF NOT AVAIL HuvGr OR (pcFeltListe = "AvdelingNr" AND NOT CAN-DO(pcVerdier,STRING(HuvGr.AvdelingNr))) THEN
                    NEXT.
            END.
        END.
      END.
      ELSE IF (cStTypeId = "LEVERAN" OR cStTypeId = "LEVERAN-VG") THEN DO:
        IF NOT CAN-DO(ENTRY(1,pcVerdier,CHR(1)),STRING(INT(ENTRY(1,RowObject.DataObjekt,CHR(1))))) THEN
          NEXT.
        IF NUM-ENTRIES(pcFeltListe) = 2 AND ENTRY(2,pcFeltListe) <> "" THEN DO:
            IF ENTRY(2,pcFeltListe) = "Vg" AND NOT CAN-DO(ENTRY(2,pcVerdier,CHR(1)),LEFT-TRIM(ENTRY(2,RowObject.DataObjekt,CHR(1)),"0")) THEN
                  NEXT.
            ELSE IF ENTRY(2,pcFeltListe) <> "Vg" THEN DO:
                FIND VarGr WHERE VarGr.Vg = INT(ENTRY(2,RowObject.DataObjekt,CHR(1))) NO-LOCK NO-ERROR.
                IF NOT AVAIL VarGr OR (ENTRY(2,pcFeltListe) = "Hg" AND NOT CAN-DO(ENTRY(2,pcVerdier,CHR(1)),STRING(VarGr.Hg))) THEN
                    NEXT.
                ELSE IF ENTRY(2,pcFeltListe) = "AvdelingNr" THEN DO:
                    FIND HuvGr OF VarGr NO-LOCK NO-ERROR.
                    IF NOT AVAIL HuvGr OR (ENTRY(2,pcFeltListe) = "AvdelingNr" AND NOT CAN-DO(ENTRY(2,pcVerdier,CHR(1)),STRING(HuvGr.AvdelingNr))) THEN
                        NEXT.
                END.
            END.
        END.
      END.
      ELSE IF cStTypeId = "LEVERAN-SA" THEN DO:
        IF NOT CAN-DO(ENTRY(1,pcVerdier,CHR(1)),STRING(INT(ENTRY(1,RowObject.DataObjekt,CHR(1))))) THEN
          NEXT.
        IF NUM-ENTRIES(pcFeltListe) = 2 AND ENTRY(2,pcFeltListe) <> "" THEN DO:
            IF NOT CAN-DO(ENTRY(2,pcVerdier,CHR(1)),LEFT-TRIM(ENTRY(2,RowObject.DataObjekt,CHR(1)),"0")) THEN
                  NEXT.
        END.
      END.
      ELSE IF (cStTypeId = "SELGERSTAT" OR cStTypeId = "KASS-VG" OR cStTypeId = "KASS-ART") THEN DO:
        IF NOT CAN-DO(ENTRY(1,pcVerdier,CHR(1)),STRING(INT(ENTRY(1,RowObject.DataObjekt,CHR(1))))) THEN
          NEXT.
        ASSIGN TT_StLinjeTMP.ForsNr = INT(ENTRY(1,RowObject.DataObjekt,CHR(1))).
        IF NUM-ENTRIES(pcFeltListe) = 2 AND ENTRY(2,pcFeltListe) <> "" THEN DO:
            IF cStTypeId = "KASS-ART" THEN DO:
                FIND ArtBas WHERE ArtBas.ArtikkelNr = DECI(ENTRY(2,TT_StLinjeTMP.DataObjekt,CHR(1))) NO-LOCK NO-ERROR.
                IF NOT AVAIL ArtBas THEN
                    NEXT.
                IF ENTRY(2,pcFeltListe) = "Vg" AND NOT CAN-DO(ENTRY(2,pcVerdier,CHR(1)),STRING(ArtBas.Vg)) THEN
                      NEXT.
            END.
            IF cStTypeId <> "KASS-ART" AND ENTRY(2,pcFeltListe) = "Vg" AND NOT CAN-DO(ENTRY(2,pcVerdier,CHR(1)),LEFT-TRIM(ENTRY(2,RowObject.DataObjekt,CHR(1)),"0")) THEN
                  NEXT.
            ELSE IF ENTRY(2,pcFeltListe) <> "Vg" THEN DO:
                FIND VarGr WHERE VarGr.Vg = INT(ENTRY(2,RowObject.DataObjekt,CHR(1))) NO-LOCK NO-ERROR.
                IF NOT AVAIL VarGr OR (ENTRY(2,pcFeltListe) = "Hg" AND NOT CAN-DO(ENTRY(2,pcVerdier,CHR(1)),STRING(VarGr.Hg))) THEN
                    NEXT.
                ELSE IF ENTRY(2,pcFeltListe) = "AvdelingNr" THEN DO:
                    FIND HuvGr OF VarGr NO-LOCK NO-ERROR.
                    IF NOT AVAIL HuvGr OR (ENTRY(2,pcFeltListe) = "AvdelingNr" AND NOT CAN-DO(ENTRY(2,pcVerdier,CHR(1)),STRING(HuvGr.AvdelingNr))) THEN
                        NEXT.
                END.
            END.
        END.
      END.
      ELSE IF (cStTypeId = "SELGER" OR cStTypeId = "SELGER-VG" OR cStTypeId = "SELGER-ART") THEN DO:
        IF NOT CAN-DO(ENTRY(1,pcVerdier,CHR(1)),STRING(INT(ENTRY(1,RowObject.DataObjekt,CHR(1))))) THEN
          NEXT.
        ASSIGN TT_StLinjeTMP.SelgerNr = DECI(ENTRY(1,RowObject.DataObjekt,CHR(1))).
        IF NUM-ENTRIES(pcFeltListe) = 2 AND ENTRY(2,pcFeltListe) <> "" THEN DO:
          IF cStTypeId = "SELGER-ART" THEN DO:
            FIND ArtBas WHERE ArtBas.ArtikkelNr = DECI(ENTRY(2,TT_StLinjeTMP.DataObjekt,CHR(1))) NO-LOCK NO-ERROR.
            IF NOT AVAIL ArtBas THEN
              NEXT.
            IF ENTRY(2,pcFeltListe) = "Vg" AND NOT CAN-DO(ENTRY(2,pcVerdier,CHR(1)),STRING(ArtBas.Vg)) THEN
              NEXT.
          END.
          IF cStTypeId <> "SELGER-ART" AND ENTRY(2,pcFeltListe) = "Vg" AND NOT CAN-DO(ENTRY(2,pcVerdier,CHR(1)),LEFT-TRIM(ENTRY(2,RowObject.DataObjekt,CHR(1)),"0")) THEN
            NEXT.
          ELSE IF ENTRY(2,pcFeltListe) <> "Vg" THEN DO:
            IF cStTypeId = "SELGER-ART" THEN
              FIND VarGr OF ArtBas NO-LOCK NO-ERROR.
            ELSE
              FIND VarGr WHERE VarGr.Vg = INT(ENTRY(2,RowObject.DataObjekt,CHR(1))) NO-LOCK NO-ERROR.
              IF NOT AVAIL VarGr OR (ENTRY(2,pcFeltListe) = "Hg" AND NOT CAN-DO(ENTRY(2,pcVerdier,CHR(1)),STRING(VarGr.Hg))) THEN
                NEXT.
              ELSE IF ENTRY(2,pcFeltListe) = "AvdelingNr" THEN DO:
                FIND HuvGr OF VarGr NO-LOCK NO-ERROR.
                IF NOT AVAIL HuvGr OR (ENTRY(2,pcFeltListe) = "AvdelingNr" AND NOT CAN-DO(ENTRY(2,pcVerdier,CHR(1)),STRING(HuvGr.AvdelingNr))) THEN
                  NEXT.
            END.
          END.
        END.
      END.
      ELSE IF cStTypeId = "KUNDSTAT" THEN DO:
        IF pcFeltListe = "KundeNr" AND NOT CAN-DO(pcVerdier,LEFT-TRIM(RowObject.DataObjekt,"0")) THEN
              NEXT.
        IF (cKilde <> "" OR cTilgKilde <> "")  then do:
          if NOT kildeOK(cStTypeId,RowObject.DataObjekt,cKilde,cTilgKilde) then
           NEXT.
        end.
      END.
      ELSE IF cStTypeId = "MEDLEM" THEN DO:
        IF pcFeltListe = "MedlemsNr" AND NOT CAN-DO(pcVerdier,LEFT-TRIM(RowObject.DataObjekt,"0")) THEN
          NEXT.
        IF (cKilde <> "" OR cTilgKilde <> "") then do:
          if not kildeOK(cStTypeId,RowObject.DataObjekt,cKilde,cTilgKilde) then
           NEXT.
        end.
        IF iMKlubbId <> ? THEN DO:
            FIND medlem WHERE medlem.medlemsnr = DECI(RowObject.DataObjekt) NO-LOCK NO-ERROR.
            IF NOT AVAIL medlem THEN
                NEXT.
            ELSE IF medlem.MKlubbId <> iMKlubbId THEN
                NEXT.
        END.
      END.
    END.
    BUFFER-COPY RowObject TO TT_StLinjeTMP.
    ASSIGN
      TT_StLinjeTMP.DbKr  = TT_StLinjeTMP.VerdiSolgt - TT_StLinjeTMP.VVareKost
      TT_StLinjeTMP.Butik = IF lVisPerBut = TRUE THEN TT_StLinjeTMP.Butik ELSE 0
      TT_StLinjeTMP.AarPerLinNr = IF lVisPeriode = TRUE THEN TT_StLinjeTMP.AarPerLinNr ELSE 0
      TT_StLinjeTMP.BruttoSolgt = TT_StLinjeTMP.VerdiSolgt + TT_StLinjeTMP.MvaVerdi.

    IF cStTypeId = "LEVERAN-VG" OR cStTypeId = "LEVERAN-SA" OR cStTypeId = "SELGER-VG" OR cStTypeId = "KASS-VG" OR cStTypeId = "SELGER-ART" OR cStTypeId = "KASS-ART" THEN VGBLOCK: DO:
      IF cXParam = "ENTRY1" THEN DO:
          ASSIGN TT_StLinjeTMP.DataObjekt = ENTRY(1,TT_StLinjeTMP.DataObjekt,CHR(1)).
          LEAVE VGBLOCK.
      END.
      IF cStTypeId = "SELGER-ART"  OR cStTypeId = "KASS-ART" THEN DO:
          FIND ArtBas WHERE ArtBas.ArtikkelNr = DECI(ENTRY(2,TT_StLinjeTMP.DataObjekt,CHR(1))) NO-LOCK NO-ERROR.
      END.
      ELSE IF cStTypeId = "LEVERAN-SA" THEN DO:
          ASSIGN TT_StLinjeTMP.Sasong      = INT(ENTRY(2,TT_StLinjeTMP.DataObjekt,CHR(1))) NO-ERROR.
          FIND sasong WHERE sasong.sasong = TT_StLinjeTMP.Sasong NO-LOCK NO-ERROR.
          TT_StLinjeTMP.SasBeskr = IF AVAIL Sasong THEN Sasong.sasbeskr ELSE "Ukjent".
          RELEASE sasong.
      END.
      ELSE DO:
          FIND VarGr WHERE VarGr.Vg = INT(ENTRY(2,TT_StLinjeTMP.DataObjekt,CHR(1))) NO-LOCK NO-ERROR.
          IF AVAIL VarGr THEN
              FIND HuvGr OF VarGr NO-LOCK NO-ERROR.
          IF AVAIL HuvGr THEN
              FIND Avdeling OF HuvGr NO-LOCK NO-ERROR.
          END.
      CASE cXParam:
        WHEN "Avd" THEN DO:
          ASSIGN TT_StLinjeTMP.DataObjekt = ENTRY(1,TT_StLinjeTMP.DataObjekt,CHR(1)) + CHR(1) +
               IF AVAIL Avdeling THEN STRING(Avdeling.AvdelingNr,"9999") ELSE "0000"
             TT_StLinjeTMP.AvdelingNr   = IF AVAIL Avdeling THEN Avdeling.AvdelingNr ELSE 0
             TT_StLinjeTMP.AvdelingNavn = IF AVAIL Avdeling THEN Avdeling.AvdelingNavn ELSE "Ukjent".
        END.
        WHEN "Hg" THEN DO:
          ASSIGN TT_StLinjeTMP.DataObjekt = ENTRY(1,TT_StLinjeTMP.DataObjekt,CHR(1)) + CHR(1) +
                      IF AVAIL HuvGr THEN STRING(HuvGr.Hg,"9999") ELSE "0000"
             TT_StLinjeTMP.Hg      = IF AVAIL HuvGr THEN HuvGr.Hg ELSE 0
             TT_StLinjeTMP.HgBeskr = IF AVAIL HuvGr THEN HuvGr.HgBeskr ELSE "Ukjent".
        END.
        WHEN "Vg" THEN DO:
          ASSIGN TT_StLinjeTMP.DataObjekt = ENTRY(1,TT_StLinjeTMP.DataObjekt,CHR(1)) + CHR(1) +
                      IF AVAIL VarGr THEN STRING(VarGr.Vg,"999999") ELSE "000000"
             TT_StLinjeTMP.Vg      = IF AVAIL VarGr THEN VarGr.Vg ELSE 0
             TT_StLinjeTMP.VgBeskr = IF AVAIL VarGr THEN VarGr.VgBeskr ELSE "Ukjent".
        END.
          WHEN "Artikkel" THEN DO:
            ASSIGN TT_StLinjeTMP.DataObjekt = ENTRY(1,TT_StLinjeTMP.DataObjekt,CHR(1)) + CHR(1) +
                        IF AVAIL ArtBas THEN STRING(ArtBas.ArtikkelNr,"9999999999999") ELSE "0000000000000"
               TT_StLinjeTMP.ArtikkelNr = IF AVAIL ArtBas THEN ArtBas.ArtikkelNr ELSE 0
               TT_StLinjeTMP.ArtBeskr = IF AVAIL ArtBas THEN ArtBas.Beskr ELSE "Ukjent".
          END.
      END CASE.
      RELEASE Avdeling.
      RELEASE HuvGr.
      RELEASE VarGr.
    END.
    FIND CURRENT TT_StLinjeTMP.
    ASSIGN 
      dTotSum = dTotSum + TT_StLinjeTMP.VerdiSolgt
      dDBTot    = dDBTot    + TT_StLinjeTMP.DbKr
      dRabTot   = dRabTot   + TT_StLinjeTMP.VerdiRabatt
      dKjopTot  = dKjopTot  + TT_StLinjeTMP.KjopVerdi.
    DO:
      FIND TT_StLinje 
        WHERE TT_StLinje.Butik      = TT_StLinjeTMP.Butik      AND /* allt på en butik = 0 */
              TT_StLinje.StTypeId   = TT_StLinjeTMP.StTypeId   AND
              TT_StLinje.PerId      = TT_StLinjeTMP.PerId      AND
              TT_StLinje.AarPerLinNr = TT_StLinjeTMP.AarPerLinNr AND
              TT_StLinje.DataObjekt = TT_StLinjeTMP.DataObjekt USE-INDEX AarPerLinNr NO-ERROR.
      IF NOT AVAIL TT_Stlinje THEN DO:
        IF lVisPeriode = TRUE THEN
          ASSIGN dDato = IF TT_StLinjeTMP.PerId = "DAG" THEN DATE(1,1,TT_StLinjeTMP.Aar) + TT_StLinjeTMP.PerLinNr - 1 ELSE ?
                 TT_StLinjeTMP.PerLinTxt = IF TT_StLinjeTMP.PerId = "DAG" THEN
                   STRING(YEAR(dDato)) + "/" + STRING(MONTH(dDato),"99") + "/" + STRING(DAY(dDato),"99") ELSE 
                 IF TT_StLinjeTMP.PerId = "AAR" THEN STRING(TT_StLinjeTMP.Aar) ELSE
                     STRING(TT_StLinjeTMP.Aar) + "-" + STRING(TT_StLinjeTMP.PerLinNr,"99").
        CREATE TT_StLinje.
        BUFFER-COPY TT_StLinjeTMP TO TT_StLinje.
        IF CAN-DO("KUNDSTAT,MEDLEM",cStTypeId) THEN
            RUN FillKund_Medl(cStTypeId,TT_StLinje.DataObjekt).
        ELSE
            ASSIGN TT_StLinje.Beskrivelse = getBeskr(cStTypeId,TT_StLinje.DataObjekt).
        ASSIGN TT_StLinje.Butnamn     = IF lVisPerBut = TRUE THEN getBeskr("GETBUTNAVN",STRING(TT_StLinje.Butik)) ELSE "".
      END.
      ELSE
          RUN AssignTT_StLinje.
    END.
  END.
  END.
  FOR EACH TT_StLinje:
      IF cStTypeId = "MEDLEM" AND (dBruttoFra <> 0 OR dBruttoTil <> 0) THEN DO:
          IF TT_StLinje.BruttoSolgt < dBruttoFra OR TT_StLinje.BruttoSolgt > dBruttoTil THEN DO:
            DELETE TT_StLinje.
            NEXT.
          END.
      END.                         
      ASSIGN TT_StLinje.Db%    = IF TT_StLinje.VerdiSolgt <= 0 THEN 0 ELSE (TT_StLinje.DbKr * 100) / TT_StLinje.VerdiSolgt
             TT_StLinje.Solgt% = ROUND(TT_StLinje.VerdiSolgt / dTotsum * 100,1)
             TT_StLinje.Rab%   = IF TT_StLinje.VerdiSolgt + TT_StLinje.VerdiRabatt = 0 THEN 0 ELSE 
                                 ((TT_StLinje.VerdiRabatt) * 100) / (TT_StLinje.VerdiSolgt + TT_StLinje.VerdiRabatt)
             TT_StLinje.DBandel%   = IF dDBTot = 0 THEN 0 ELSE ROUND(TT_StLinje.DbKr / dDBTot * 100,1)
             TT_StLinje.Rabandel%  = IF dRabTot = 0 THEN 0 ELSE ROUND(TT_StLinje.VerdiRabatt / dRabTot * 100,1)
             TT_StLinje.Kjopandel% = IF dKjopTot = 0 THEN 0 ELSE ROUND(TT_StLinje.KjopVerdi / dKjopTot * 100,1).
      IF cStTypeId = "LEVERAN-VG" OR cStTypeId = "LEVERAN-SA" THEN DO:
          ASSIGN TT_StLinje.LevNr   = INT(ENTRY(1,TT_StLinje.DataObjekt,CHR(1)))
                 TT_StLinje.LevNamn = getBeskr(cStTypeId,ENTRY(1,TT_StLinje.DataObjekt,CHR(1))).
      END.
      ELSE IF cStTypeId = "KASS-VG" THEN DO:
          ASSIGN TT_StLinje.ForsNr   = INT(ENTRY(1,TT_StLinje.DataObjekt,CHR(1)))
                 TT_StLinje.FoNamn = getBeskr(cStTypeId,ENTRY(1,TT_StLinje.DataObjekt,CHR(1))).
      END.
      ELSE IF cStTypeId = "SELGER-VG" OR cStTypeId = "SELGER-ART" THEN DO:
          ASSIGN TT_StLinje.SelgerNavn = getBeskr(cStTypeId,ENTRY(1,TT_StLinje.DataObjekt,CHR(1))).
      END.
  END.
  DYNAMIC-FUNCTION('closeQuery':U).
  ASSIGN TTH = BUFFER TT_StLinje:HANDLE.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE StLinjeToTTKampanje dTables  _DB-REQUIRED
PROCEDURE StLinjeToTTKampanje :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes: 
         
------------------------------------------------------------------------------*/
/*   DEFINE OUTPUT PARAMETER TABLE-HANDLE TTH. */
  DEFINE OUTPUT PARAMETER TTH AS HANDLE NO-UNDO.
  DEFINE INPUT  PARAMETER cStTypeId AS CHARACTER  NO-UNDO.
  DEFINE INPUT  PARAMETER cButiker  AS CHARACTER  NO-UNDO.
  DEFINE INPUT  PARAMETER cXFilter  AS CHARACTER  NO-UNDO.
  DEFINE INPUT  PARAMETER cXParam   AS CHARACTER  NO-UNDO.
  DEFINE INPUT  PARAMETER lVisPeriode  AS LOGICAL    NO-UNDO.
  DEFINE INPUT  PARAMETER cKampId      AS CHARACTER  NO-UNDO.
  DEFINE INPUT  PARAMETER cKampTilbType AS CHARACTER  NO-UNDO.
  DEFINE INPUT  PARAMETER hTTArtBuf    AS HANDLE     NO-UNDO.
  DEFINE        VARIABLE iKampanjeLoop AS INTEGER    NO-UNDO.
  DEFINE        VARIABLE  cQuery       AS CHARACTER  NO-UNDO.
  DEFINE        VARIABLE  cQueryUpdate AS CHARACTER  NO-UNDO.
  DEFINE        VARIABLE  lFirst       AS LOGICAL    NO-UNDO.
  DEFINE        VARIABLE  rRowId       AS ROWID      NO-UNDO.
  DEFINE        VARIABLE  dTotSum      AS DECIMAL    NO-UNDO.
  DEFINE        VARIABLE  hBufferField AS HANDLE     NO-UNDO.
  DEFINE        VARIABLE  pcFeltListe  AS CHARACTER  NO-UNDO.
  DEFINE        VARIABLE  pcVerdier    AS CHARACTER  NO-UNDO.
  DEFINE        VARIABLE  iButikloop  AS INTEGER    NO-UNDO.
  DEFINE        VARIABLE  lUtvidetFilter AS LOGICAL    NO-UNDO.
  DEFINE        VARIABLE  lIkkeTreff     AS LOGICAL    NO-UNDO.
  DEFINE        VARIABLE  dDato          AS DATE       NO-UNDO.
  DEFINE        VARIABLE  cOmsVerdier AS CHARACTER  NO-UNDO.
  DEFINE        VARIABLE  dBruttoFra  AS DECIMAL    NO-UNDO.
  DEFINE        VARIABLE  dBruttoTil  AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE bOK AS LOGICAL    NO-UNDO.
  DEFINE VARIABLE lVisPerBut AS LOGICAL    NO-UNDO.

  EMPTY TEMP-TABLE TT_StLinjeJmf.
  EMPTY TEMP-TABLE TT_StLinjeJmfTMP.
  EMPTY TEMP-TABLE TT_StLinje.
  EMPTY TEMP-TABLE TT_StLinjeTMP.
  CREATE TT_StLinjeTMP. /* en temporär record för att kunna summera */
  IF NUM-ENTRIES(cXFilter,";") = 2 THEN DO:
      ASSIGN pcFeltListe = ENTRY(1,cXFilter,";")
             pcVerdier   = ENTRY(2,cXFilter,";").
      IF cStTypeId = "MEDLEM" THEN DO:
          IF NUM-ENTRIES(pcVerdier,CHR(2)) = 2 THEN
              ASSIGN cOmsVerdier = ENTRY(2,pcVerdier,CHR(2))
                     pcVerdier   = ENTRY(1,pcVerdier,CHR(2))
                     dBruttoFra  = DECI(ENTRY(1,cOmsVerdier))
                     dBruttoTil  = DECI(ENTRY(2,cOmsVerdier)).
      END.
      IF pcFeltListe <> pcVerdier THEN  /* */
          ASSIGN lUtvidetFilter = TRUE.
  END.
  /* Specialhantering av Vis/But                                       */
  /* Parameter cXParam används för att ge möjlighet att visa per butik */
  /* I de statistiktyper som vi skall 'Vis/But' läggs CHR(1)J          */
  ASSIGN lVisPerBut = IF NUM-ENTRIES(cXParam,CHR(1)) = 2 THEN ENTRY(2,cXParam,CHR(1)) = "J" ELSE FALSE
         cXParam    = ENTRY(1,cXParam,CHR(1)).

  /* SUBSTKAMPANJE */
  ASSIGN cQuery = DYNAMIC-FUNCTION('getQueryString':U).
  DO iButikLoop = 1 TO NUM-ENTRIES(cButiker):

    DO iKampanjeLoop = 1 TO NUM-ENTRIES(cKampId):
      cQueryUpdate = REPLACE(cQuery,"SUBSTBUTIK","Butik = " + ENTRY(iButikLoop,cButiker) + " ").
      IF cKampId <> "*" AND cQueryUpdate MATCHES "*SUBSTKAMPANJE*" THEN
          cQueryUpdate = REPLACE(cQueryUpdate,"SUBSTKAMPANJE",STRING(DECI(ENTRY(iKampanjeLoop,cKampId)),"9999999999999")).
      IF cKampId <> "*" AND cKampTilbType <> "*" THEN DO:
          FIND KampanjeTilbud WHERE KampanjeTilbud.KampId = DECI(ENTRY(iKampanjeLoop,cKampId)) NO-LOCK NO-ERROR.
          IF NOT AVAIL Kampanjetilbud OR NOT CAN-DO(cKampTilbType,STRING(Kampanjetilbud.KampTilbTypeId)) THEN
              NEXT.
      END.
      DYNAMIC-FUNCTION('closeQuery':U).
      DYNAMIC-FUNCTION('setQueryString':U,
     INPUT cQueryUpdate /* CHARACTER */).
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
        IF NOT AVAIL RowObject OR rRowId = ROWID(RowObject) THEN
          LEAVE.
        ELSE
          ASSIGN rRowId = ROWID(RowObject).
        IF cKampId = "*" AND cKampTilbType <> "*" THEN DO:
            FIND KampanjeTilbud WHERE KampanjeTilbud.KampId = DECI(ENTRY(1,RowObject.Dataobjekt,CHR(1))) NO-LOCK NO-ERROR.
            IF NOT AVAIL Kampanjetilbud OR NOT CAN-DO(cKampTilbType,STRING(Kampanjetilbud.KampTilbTypeId)) THEN
                NEXT.
        END.
        IF cStTypeId = "KAMPART" AND VALID-HANDLE(hTTArtBuf) THEN DO:
            bOK = hTTArtBuf:FIND-FIRST("WHERE artikkelnr = " + LEFT-TRIM(ENTRY(2,RowObject.Dataobjekt,CHR(1)),"0")) NO-ERROR.
            IF NOT bOK THEN
                NEXT.
        END.
        BUFFER-COPY RowObject TO TT_StLinjeTMP.

        ASSIGN TT_StLinjeTMP.DbKr  = TT_StLinjeTMP.VerdiSolgt - TT_StLinjeTMP.VVareKost
               TT_StLinjeTMP.Butik = IF lVisPerBut = TRUE THEN TT_StLinjeTMP.Butik ELSE 0
               TT_StLinjeTMP.AarPerLinNr = IF lVisPeriode = TRUE THEN TT_StLinjeTMP.AarPerLinNr ELSE 0
               TT_StLinjeTMP.BruttoSolgt = TT_StLinjeTMP.VerdiSolgt + TT_StLinjeTMP.MvaVerdi.

        FIND CURRENT TT_StLinjeTMP.
        ASSIGN dTotSum = dTotSum + TT_StLinjeTMP.VerdiSolgt.
        DO:
            FIND TT_StLinje WHERE TT_StLinje.Butik      = TT_StLinjeTMP.Butik      AND /* allt på en butik = 0 */
                                  TT_StLinje.StTypeId   = TT_StLinjeTMP.StTypeId   AND
                                  TT_StLinje.PerId      = TT_StLinjeTMP.PerId      AND
                                  TT_StLinje.AarPerLinNr = TT_StLinjeTMP.AarPerLinNr AND
                                  TT_StLinje.DataObjekt = TT_StLinjeTMP.DataObjekt USE-INDEX AarPerLinNr NO-ERROR.
            IF NOT AVAIL TT_Stlinje THEN DO:
                IF lVisPeriode = TRUE THEN
                    ASSIGN    dDato = IF TT_StLinjeTMP.PerId = "DAG" THEN DATE(1,1,TT_StLinjeTMP.Aar) + TT_StLinjeTMP.PerLinNr - 1 ELSE ?
                              TT_StLinjeTMP.PerLinTxt = IF TT_StLinjeTMP.PerId = "DAG" THEN
                              STRING(YEAR(dDato)) + "/" + STRING(MONTH(dDato),"99") + "/" + STRING(DAY(dDato),"99") ELSE 
                              IF TT_StLinjeTMP.PerId = "AAR" THEN
                                  STRING(TT_StLinjeTMP.Aar) ELSE
                              STRING(TT_StLinjeTMP.Aar) + "-" + STRING(TT_StLinjeTMP.PerLinNr,"99").
                IF cSttypeid = "KAMPART" THEN DO:
                    FIND artbas WHERE artbas.artikkelnr = DECI(ENTRY(3,TT_StLinjeTmp.dataobjekt,CHR(1))) NO-LOCK NO-ERROR.
                    FIND KampanjeTilbud WHERE KampanjeTilbud.KampId = DECI(ENTRY(1,TT_StLinjeTmp.dataobjekt,CHR(1))) AND 
                                              KampanjeTilbud.KampTilbId = INT(ENTRY(2,TT_StLinjeTmp.dataobjekt,CHR(1))) NO-LOCK NO-ERROR.
                    IF AVAIL Kampanjetilbud THEN
                        FIND Kampanjetilbtype OF kampanjetilbud NO-LOCK NO-ERROR.
                    ELSE
                        RELEASE kampanjetilbtype.
                    ASSIGN TT_StLinjeTMP.ArtikkelNr = DECI(ENTRY(3,TT_StLinjeTmp.dataobjekt,CHR(1)))
                           TT_StLinjeTMP.ArtBeskr = IF AVAIL ArtBas THEN ArtBas.Beskr ELSE "Ukjent"
                           TT_StLinjeTMP.KampTilbid = DECI(ENTRY(2,TT_StLinjeTmp.dataobjekt,CHR(1)))
                           TT_StLinjeTMP.Kamptilbbeskr = IF AVAIL KampanjeTilbud THEN KampanjeTilbud.KampTilbKvitteringstekst ELSE "Ukjent"
                           TT_StLinjeTMP.Kamptypebeskr = (IF AVAIL Kampanjetilbud THEN string(Kampanjetilbud.KampTilbTypeId) + " " ELSE "") +
                               (IF AVAIL Kampanjetilbtype THEN Kampanjetilbtype.KampTilbTypeNavn ELSE "Ukjent").
                END.
                ELSE IF cSttypeid = "KAMPTILB" THEN DO:
                    FIND KampanjeTilbud WHERE KampanjeTilbud.KampId = DECI(ENTRY(1,TT_StLinjeTmp.dataobjekt,CHR(1))) AND 
                                              KampanjeTilbud.KampTilbId = INT(ENTRY(2,TT_StLinjeTmp.dataobjekt,CHR(1))) NO-LOCK NO-ERROR.
                    IF AVAIL Kampanjetilbud THEN
                        FIND Kampanjetilbtype OF kampanjetilbud NO-LOCK NO-ERROR.
                    ELSE
                        RELEASE kampanjetilbtype.
                    ASSIGN TT_StLinjeTMP.KampTilbid = DECI(ENTRY(2,TT_StLinjeTmp.dataobjekt,CHR(1)))
                           TT_StLinjeTMP.Kamptilbbeskr = IF AVAIL KampanjeTilbud THEN KampanjeTilbud.KampTilbKvitteringstekst ELSE "Ukjent"
                           TT_StLinjeTMP.Kamptypebeskr = (IF AVAIL Kampanjetilbud THEN string(Kampanjetilbud.KampTilbTypeId) + " " ELSE "") +
                               (IF AVAIL Kampanjetilbtype THEN Kampanjetilbtype.KampTilbTypeNavn ELSE "Ukjent")
                           .
                END.
                CREATE TT_StLinje.
                BUFFER-COPY TT_StLinjeTMP TO TT_StLinje.
                IF CAN-DO("KUNDSTAT,MEDLEM",cStTypeId) THEN
                    RUN FillKund_Medl(cStTypeId,TT_StLinje.DataObjekt).
                ELSE
                    ASSIGN TT_StLinje.Beskrivelse = getBeskr(cStTypeId,TT_StLinje.DataObjekt).
                ASSIGN TT_StLinje.Butnamn     = IF lVisPerBut = TRUE THEN getBeskr("GETBUTNAVN",STRING(TT_StLinje.Butik)) ELSE "".
            END.
            ELSE
                ASSIGN TT_StLinje.AntSolgt    = TT_StLinje.AntSolgt    + TT_StLinjeTMP.AntSolgt
                       TT_StLinje.VerdiSolgt  = TT_StLinje.VerdiSolgt  + TT_StLinjeTMP.VerdiSolgt
                       TT_StLinje.MvaVerdi    = TT_StLinje.MvaVerdi    + TT_StLinjeTMP.MvaVerdi
                       TT_StLinje.DbKr        = TT_StLinje.DbKr        + TT_StLinjeTMP.DbKr
                       TT_StLinje.AntRab      = TT_StLinje.AntRab      + TT_StLinjeTMP.AntRab
                       TT_StLinje.VerdiRabatt = TT_StLinje.VerdiRabatt + TT_StLinjeTMP.VerdiRabatt
                       TT_StLinje.AntTilbSolgt   = TT_StLinje.AntTilbSolgt  + TT_StLinjeTMP.AntTilbSolgt  
                       TT_StLinje.BrekkAnt       = TT_StLinje.BrekkAnt      + TT_StLinjeTMP.BrekkAnt      
                       TT_StLinje.BrekkVerdi     = TT_StLinje.BrekkVerdi    + TT_StLinjeTMP.BrekkVerdi    
                       TT_StLinje.GjenkjopAnt    = TT_StLinje.GjenkjopAnt   + TT_StLinjeTMP.GjenkjopAnt   
                       TT_StLinje.GjenkjopVerdi  = TT_StLinje.GjenkjopVerdi + TT_StLinjeTMP.GjenkjopVerdi 
                       TT_StLinje.IntAnt         = TT_StLinje.IntAnt        + TT_StLinjeTMP.IntAnt        
                       TT_StLinje.IntVerdi       = TT_StLinje.IntVerdi      + TT_StLinjeTMP.IntVerdi      
                       TT_StLinje.JustAnt        = TT_StLinje.JustAnt       + TT_StLinjeTMP.JustAnt       
                       TT_StLinje.JustVerdi      = TT_StLinje.JustVerdi     + TT_StLinjeTMP.JustVerdi     
                       TT_StLinje.KjopAnt        = TT_StLinje.KjopAnt       + TT_StLinjeTMP.KjopAnt       
                       TT_StLinje.KjopVerdi      = TT_StLinje.KjopVerdi     + TT_StLinjeTMP.KjopVerdi     
                       TT_StLinje.LagerAnt       = TT_StLinje.LagerAnt      + TT_StLinjeTMP.LagerAnt      
                       TT_StLinje.LagerVerdi     = TT_StLinje.LagerVerdi    + TT_StLinjeTMP.LagerVerdi    
                       TT_StLinje.ReklAnt        = TT_StLinje.ReklAnt        + TT_StLinjeTMP.ReklAnt       
                       TT_StLinje.ReklLAnt       = TT_StLinje.ReklLAnt       + TT_StLinjeTMP.ReklLAnt      
                       TT_StLinje.ReklLVerdi     = TT_StLinje.ReklLVerdi     + TT_StLinjeTMP.ReklLVerdi    
                       TT_StLinje.ReklVerdi      = TT_StLinje.ReklVerdi      + TT_StLinjeTMP.ReklVerdi     
                       TT_StLinje.OvAnt       = TT_StLinje.OvAnt       + TT_StLinjeTMP.OvAnt       
                       TT_StLinje.OvVerdi     = TT_StLinje.OvVerdi     + TT_StLinjeTMP.OvVerdi     
                       TT_StLinje.SvinnAnt       = TT_StLinje.SvinnAnt       + TT_StLinjeTMP.SvinnAnt       
                       TT_StLinje.SvinnVerdi     = TT_StLinje.SvinnVerdi     + TT_StLinjeTMP.SvinnVerdi     
                       TT_StLinje.VerdiTilbSolgt = TT_StLinje.VerdiTilbSolgt + TT_StLinjeTMP.VerdiTilbSolgt
                       TT_StLinje.VVarekost      = TT_StLinje.VVarekost      + TT_StLinjeTMP.VVarekost     
                       TT_StLinje.BruttoSolgt    = TT_StLinje.BruttoSolgt    + TT_StLinjeTMP.BruttoSolgt.
                       .
        END.
      END.
    END.
  END.
  FOR EACH TT_StLinje:
      IF cStTypeId = "MEDLEM" AND (dBruttoFra <> 0 OR dBruttoTil <> 0) THEN DO:
          IF TT_StLinje.BruttoSolgt < dBruttoFra OR TT_StLinje.BruttoSolgt > dBruttoTil THEN DO:
              DELETE TT_StLinje.
              NEXT.
          END.
      END.                         
      ASSIGN TT_StLinje.Db%    = IF TT_StLinje.VerdiSolgt <= 0 THEN 0 ELSE (TT_StLinje.DbKr * 100) / TT_StLinje.VerdiSolgt
             TT_StLinje.Solgt% = ROUND(TT_StLinje.VerdiSolgt / dTotsum * 100,1)
             TT_StLinje.Rab%   = IF TT_StLinje.VerdiSolgt + TT_StLinje.VerdiRabatt = 0 THEN 0 ELSE 
                                 ((TT_StLinje.VerdiRabatt) * 100) / (TT_StLinje.VerdiSolgt + TT_StLinje.VerdiRabatt).
      IF cStTypeId = "LEVERAN-VG" THEN DO:
          ASSIGN TT_StLinje.LevNr   = INT(ENTRY(1,TT_StLinje.DataObjekt,CHR(1)))
                 TT_StLinje.LevNamn = getBeskr(cStTypeId,ENTRY(1,TT_StLinje.DataObjekt,CHR(1))).
      END.
      ELSE IF cStTypeId = "KASS-VG" THEN DO:
          ASSIGN TT_StLinje.ForsNr   = INT(ENTRY(1,TT_StLinje.DataObjekt,CHR(1)))
                 TT_StLinje.FoNamn = getBeskr(cStTypeId,ENTRY(1,TT_StLinje.DataObjekt,CHR(1))).
      END.
      ELSE IF cStTypeId = "SELGER-VG" OR cStTypeId = "SELGER-ART" THEN DO:
          ASSIGN /* TT_StLinje.SelgerNr   = INT(ENTRY(1,TT_StLinje.DataObjekt,CHR(1))) */
                 TT_StLinje.SelgerNavn = getBeskr(cStTypeId,ENTRY(1,TT_StLinje.DataObjekt,CHR(1))).
      END.
  END.
  DYNAMIC-FUNCTION('closeQuery':U).
  ASSIGN TTH = BUFFER TT_StLinje:HANDLE.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE StLinjeToTT_Vg dTables  _DB-REQUIRED
PROCEDURE StLinjeToTT_Vg :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes: 
------------------------------------------------------------------------------*/
DEF OUTPUT PARAMETER TTH AS HANDLE NO-UNDO.
DEF INPUT  PARAMETER cStTypeId AS CHARACTER  NO-UNDO.
DEF INPUT  PARAMETER cButiker  AS CHARACTER  NO-UNDO.
DEF INPUT  PARAMETER cXFilter  AS CHARACTER  NO-UNDO.
DEF INPUT  PARAMETER cXParam   AS CHARACTER  NO-UNDO.
DEF INPUT  PARAMETER lVisPeriode AS LOGICAL    NO-UNDO.
DEF VARIABLE  cQuery AS CHARACTER  NO-UNDO.
DEF VARIABLE  lFirst    AS LOGICAL    NO-UNDO.
DEF VARIABLE  rRowId    AS ROWID      NO-UNDO.
DEF VARIABLE  dTotSum   AS DECIMAL    NO-UNDO.
DEF VARIABLE  hBufferField AS HANDLE     NO-UNDO.
DEF VARIABLE  pcFeltListe  AS CHARACTER  NO-UNDO.
DEF VARIABLE  pcVerdier    AS CHARACTER  NO-UNDO.
DEF VARIABLE  iButikloop  AS INTEGER    NO-UNDO.
DEF VARIABLE  lUtvidetFilter AS LOGICAL    NO-UNDO.
DEF VARIABLE  lIkkeTreff     AS LOGICAL    NO-UNDO.
DEF VARIABLE  dDato          AS DATE       NO-UNDO.
DEF VARIABLE  cOmsVerdier AS CHARACTER  NO-UNDO.
DEF VARIABLE  dBruttoFra  AS DECIMAL    NO-UNDO.
DEF VARIABLE  dBruttoTil  AS DECIMAL    NO-UNDO.
DEF VARIABLE  cKilde AS CHARACTER  NO-UNDO.
DEF VARIABLE  cTilgKilde AS CHARACTER  NO-UNDO.
DEF VARIABLE  ii         AS INTE NO-UNDO.
DEF VAR hQuery AS HANDLE NO-UNDO.  
DEF VARIABLE  dDBTot   AS DECIMAL    NO-UNDO.
DEF VARIABLE  dRabTot  AS DECIMAL    NO-UNDO.
DEF VARIABLE  dKjopTot AS DECIMAL    NO-UNDO.
DEF VARIABLE lVisPerBut AS LOGICAL    NO-UNDO.
EMPTY TEMP-TABLE TT_StLinjeJmf.
EMPTY TEMP-TABLE TT_StLinjeJmfTMP.
EMPTY TEMP-TABLE TT_StLinje.
EMPTY TEMP-TABLE TT_StLinjeTMP.
CREATE TT_StLinjeTMP. /* en temporär record för att kunna summera */
  IF NUM-ENTRIES(cXFilter,";") = 2 THEN DO:
    ASSIGN pcFeltListe = ENTRY(1,cXFilter,";")
           pcVerdier   = ENTRY(2,cXFilter,";").
    IF pcFeltListe <> pcVerdier THEN  /* */
        ASSIGN lUtvidetFilter = TRUE.
  END.
  /* Specialhantering av Vis/But  */
  /* Parameter cXParam används för att ge möjlighet att visa per butik */
  /* I de statistiktyper som vi skall 'Vis/But' läggs CHR(1)J */
  ASSIGN lVisPerBut = IF NUM-ENTRIES(cXParam,CHR(1)) = 2 THEN ENTRY(2,cXParam,CHR(1)) = "J" ELSE FALSE
         cXParam    = ENTRY(1,cXParam,CHR(1)).
  ASSIGN cQuery = DYNAMIC-FUNCTION('getQueryString':U).
  DO iButikLoop = 1 TO NUM-ENTRIES(cButiker):
      DYNAMIC-FUNCTION('closeQuery':U).
      DYNAMIC-FUNCTION('setQueryString':U,
     INPUT REPLACE(cQuery,"SUBSTBUTIK","Butik = " + ENTRY(iButikLoop,cButiker) + " ") /* CHARACTER */).
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
    IF NOT AVAIL RowObject OR rRowId = ROWID(RowObject) THEN
      LEAVE.
    ELSE
      ASSIGN rRowId = ROWID(RowObject).
    IF lUtvidetFilter = TRUE THEN DO:
      IF cStTypeId = "VAREGR" THEN DO:
        IF pcFeltListe = "Vg" AND NOT CAN-DO(pcVerdier,LEFT-TRIM(RowObject.DataObjekt,"0")) THEN
              NEXT.
        ELSE IF pcFeltListe <> "Vg" THEN DO:
            FIND VarGr WHERE VarGr.Vg = INT(RowObject.DataObjekt) NO-LOCK NO-ERROR.
            IF NOT AVAIL VarGr OR (pcFeltListe = "Hg" AND NOT CAN-DO(pcVerdier,STRING(VarGr.Hg))) THEN
                NEXT.
            ELSE IF pcFeltListe = "AvdelingNr" THEN DO:
                FIND HuvGr OF VarGr NO-LOCK NO-ERROR.
                IF NOT AVAIL HuvGr OR (pcFeltListe = "AvdelingNr" AND NOT CAN-DO(pcVerdier,STRING(HuvGr.AvdelingNr))) THEN
                    NEXT.
            END.
        END.
      END.
    END.
    BUFFER-COPY RowObject TO TT_StLinjeTMP.
    ASSIGN
      TT_StLinjeTMP.DbKr  = TT_StLinjeTMP.VerdiSolgt - TT_StLinjeTMP.VVareKost
      TT_StLinjeTMP.Butik = IF lVisPerBut = TRUE THEN TT_StLinjeTMP.Butik ELSE 0
      TT_StLinjeTMP.AarPerLinNr = IF lVisPeriode = TRUE THEN TT_StLinjeTMP.AarPerLinNr ELSE 0
      TT_StLinjeTMP.BruttoSolgt = TT_StLinjeTMP.VerdiSolgt + TT_StLinjeTMP.MvaVerdi.

    FIND CURRENT TT_StLinjeTMP.
    ASSIGN 
      dTotSum = dTotSum + TT_StLinjeTMP.VerdiSolgt
      dDBTot    = dDBTot    + TT_StLinjeTMP.DbKr
      dRabTot   = dRabTot   + TT_StLinjeTMP.VerdiRabatt
      dKjopTot  = dKjopTot  + TT_StLinjeTMP.KjopVerdi.
    DO:
      FIND TT_StLinje 
        WHERE TT_StLinje.Butik      = TT_StLinjeTMP.Butik      AND /* allt på en butik = 0 */
              TT_StLinje.StTypeId   = TT_StLinjeTMP.StTypeId   AND
              TT_StLinje.PerId      = TT_StLinjeTMP.PerId      AND
              TT_StLinje.AarPerLinNr = TT_StLinjeTMP.AarPerLinNr AND
              TT_StLinje.DataObjekt = TT_StLinjeTMP.DataObjekt USE-INDEX AarPerLinNr NO-ERROR.
      IF NOT AVAIL TT_Stlinje THEN DO:
        IF lVisPeriode = TRUE THEN
          ASSIGN dDato = IF TT_StLinjeTMP.PerId = "DAG" THEN DATE(1,1,TT_StLinjeTMP.Aar) + TT_StLinjeTMP.PerLinNr - 1 ELSE ?
                 TT_StLinjeTMP.PerLinTxt = IF TT_StLinjeTMP.PerId = "DAG" THEN
                   STRING(YEAR(dDato)) + "/" + STRING(MONTH(dDato),"99") + "/" + STRING(DAY(dDato),"99") ELSE 
                 IF TT_StLinjeTMP.PerId = "AAR" THEN STRING(TT_StLinjeTMP.Aar) ELSE
                     STRING(TT_StLinjeTMP.Aar) + "-" + STRING(TT_StLinjeTMP.PerLinNr,"99").
        CREATE TT_StLinje.
        BUFFER-COPY TT_StLinjeTMP TO TT_StLinje.
        IF CAN-DO("KUNDSTAT,MEDLEM",cStTypeId) THEN
            RUN FillKund_Medl(cStTypeId,TT_StLinje.DataObjekt).
        ELSE
            ASSIGN TT_StLinje.Beskrivelse = getBeskr(cStTypeId,TT_StLinje.DataObjekt).
        ASSIGN TT_StLinje.Butnamn     = IF lVisPerBut = TRUE THEN getBeskr("GETBUTNAVN",STRING(TT_StLinje.Butik)) ELSE "".
      END.
      ELSE
          RUN AssignTT_StLinje.
    END.
  END.
  END.
  FOR EACH TT_StLinje:
      IF cStTypeId = "MEDLEM" AND (dBruttoFra <> 0 OR dBruttoTil <> 0) THEN DO:
          IF TT_StLinje.BruttoSolgt < dBruttoFra OR TT_StLinje.BruttoSolgt > dBruttoTil THEN DO:
            DELETE TT_StLinje.
            NEXT.
          END.
      END.                         
      ASSIGN TT_StLinje.Db%    = IF TT_StLinje.VerdiSolgt <= 0 THEN 0 ELSE (TT_StLinje.DbKr * 100) / TT_StLinje.VerdiSolgt
             TT_StLinje.Solgt% = ROUND(TT_StLinje.VerdiSolgt / dTotsum * 100,1)
             TT_StLinje.Rab%   = IF TT_StLinje.VerdiSolgt + TT_StLinje.VerdiRabatt = 0 THEN 0 ELSE 
                                 ((TT_StLinje.VerdiRabatt) * 100) / (TT_StLinje.VerdiSolgt + TT_StLinje.VerdiRabatt)
             TT_StLinje.DBandel%   = IF dDBTot = 0 THEN 0 ELSE ROUND(TT_StLinje.DbKr / dDBTot * 100,1)
             TT_StLinje.Rabandel%  = IF dRabTot = 0 THEN 0 ELSE ROUND(TT_StLinje.VerdiRabatt / dRabTot * 100,1)
             TT_StLinje.Kjopandel% = IF dKjopTot = 0 THEN 0 ELSE ROUND(TT_StLinje.KjopVerdi / dKjopTot * 100,1).
      IF cStTypeId = "LEVERAN-VG" THEN DO:
          ASSIGN TT_StLinje.LevNr   = INT(ENTRY(1,TT_StLinje.DataObjekt,CHR(1)))
                 TT_StLinje.LevNamn = getBeskr(cStTypeId,ENTRY(1,TT_StLinje.DataObjekt,CHR(1))).
      END.
      ELSE IF cStTypeId = "KASS-VG" THEN DO:
          ASSIGN TT_StLinje.ForsNr   = INT(ENTRY(1,TT_StLinje.DataObjekt,CHR(1)))
                 TT_StLinje.FoNamn = getBeskr(cStTypeId,ENTRY(1,TT_StLinje.DataObjekt,CHR(1))).
      END.
      ELSE IF cStTypeId = "SELGER-VG" OR cStTypeId = "SELGER-ART" THEN DO:
          ASSIGN TT_StLinje.SelgerNavn = getBeskr(cStTypeId,ENTRY(1,TT_StLinje.DataObjekt,CHR(1))).
      END.
  END.
  DYNAMIC-FUNCTION('closeQuery':U).
  ASSIGN TTH = BUFFER TT_StLinje:HANDLE.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SummerTT_StLinje dTables  _DB-REQUIRED
PROCEDURE SummerTT_StLinje :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

/* ************************  Function Implementations ***************** */

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getBeskr dTables  _DB-REQUIRED
FUNCTION getBeskr RETURNS CHARACTER
  ( INPUT cStTypeId AS CHARACTER,INPUT cObjekt AS CHARACTER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
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
        WHEN "VAREGR" THEN DO:
            FIND VarGr WHERE VarGr.Vg = INT(cObjekt) NO-LOCK NO-ERROR.
            RETURN IF AVAIL VarGr THEN VarGr.VgBeskr ELSE "Ukjent".
        END.
        WHEN "LEVERAN" OR WHEN "LEVERAN-VG" OR WHEN "LEVERAN-SA" THEN DO:
            FIND LevBas WHERE LevBas.LevNr = INT(cObjekt) NO-LOCK NO-ERROR.
            RETURN IF AVAIL Levbas THEN LevBas.levnamn ELSE "Ukjent".
        END.
        WHEN "SELGER" OR WHEN "SELGER-VG" THEN DO:
/*             IF cHKinst = "yes" THEN */
/*                 RELEASE Selger.     */
/*             ELSE                    */
                FIND Selger WHERE Selger.SelgerNr = DECI(cObjekt) NO-LOCK NO-ERROR.
            RETURN IF AVAIL Selger THEN Selger.Navn ELSE "Ukjent".
        END.
        WHEN "SELGERSTAT" OR WHEN "KASS-VG" THEN DO:
/*             IF cHKinst = "yes" THEN */
/*                 RELEASE Forsalj.    */
/*             ELSE                    */
                FIND Forsalj WHERE Forsalj.ForsNr = INT(cObjekt) NO-LOCK NO-ERROR.
            RETURN IF AVAIL Forsalj THEN Forsalj.FoNamn ELSE "Ukjent".
        END.
        WHEN "BUTSTAT" THEN DO:
            FIND Butiker WHERE Butiker.Butik = INT(cObjekt) NO-LOCK NO-ERROR.
            RETURN IF AVAIL Butiker THEN Butiker.ButNamn ELSE "Ukjent".
        END.
        WHEN "KUNDSTAT" THEN DO:
            FIND Kunde WHERE Kunde.KundeNr = DECI(cObjekt) NO-LOCK NO-ERROR.
            RETURN IF AVAIL Kunde THEN Kunde.Navn ELSE "Ukjent".
        END.
        WHEN "MEDLEM" THEN DO:
            FIND Medlem WHERE Medlem.MedlemsNr = DECI(cObjekt) NO-LOCK NO-ERROR.
            RETURN IF AVAIL Medlem THEN  Medlem.EtterNavn + "," + Medlem.ForNavn ELSE "Ukjent".
        END.
        WHEN "KAMPANJE" OR WHEN "KAMPART" OR WHEN "KAMPTILB" THEN DO:
            FIND KampanjeMixMatch WHERE KampanjeMixMatch.KampId = DECI(ENTRY(1,cObjekt,CHR(1))) NO-LOCK NO-ERROR.
            RETURN IF AVAIL KampanjeMixMatch THEN KampanjeMixMatch.KampNavn ELSE "Ukjent".
        END.
        OTHERWISE RETURN "".
    END CASE.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION kildeOK dTables  _DB-REQUIRED
FUNCTION kildeOK RETURNS LOGICAL
  ( INPUT cStTypeId AS CHARACTER,INPUT cObjekt AS CHARACTER, INPUT cKilde AS CHARACTER, INPUT cTilgKilde AS CHARACTER ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  IF cStTypeId = "KUNDSTAT" THEN DO:
      FIND Kunde WHERE Kunde.KundeNr = DECI(cObjekt) NO-LOCK NO-ERROR.
      IF NOT AVAIL Kunde THEN
          RETURN FALSE.
      IF cKilde <> "" AND Kunde.Kilde <> cKilde THEN
          RETURN FALSE.
      IF cTilgKilde <> "" AND Kunde.TilgKilde <> cTilgKilde THEN
          RETURN FALSE.
  END.
  ELSE IF cStTypeId = "MEDLEM" THEN DO:
      FIND Medlem WHERE Medlem.MedlemsNr = DECI(cObjekt) NO-LOCK NO-ERROR.
      IF NOT AVAIL Medlem THEN
          RETURN FALSE.
      IF cKilde <> "" AND Medlem.Kilde <> cKilde THEN
          RETURN FALSE.
      IF cTilgKilde <> "" AND Medlem.TilgKilde <> cTilgKilde THEN
          RETURN FALSE.
  END.
  RETURN TRUE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

