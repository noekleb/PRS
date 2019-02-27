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
DEF VAR cTransTypeTekster  AS CHAR NO-UNDO.

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
&Scoped-define INTERNAL-TABLES BongLinje

/* Definitions for QUERY Query-Main                                     */
&Scoped-Define ENABLED-FIELDS  Antall ArtikkelNr BongNr BongPris BongTekst ButikkNr Dato EAv EDato ETid~
 GruppeNr KasseNr LinjeNr LinjeRab LinjeSum MButikkNr Mva% MvaGr~
 MvaGruppeNavn MvaKr OAv ODato OriginalData OTid Storrelse SubtotalRab TBId~
 TransDato TransTid TTId Type VareGr VareGruppeNavn LopeNr FeilKode~
 FeilKodeTekst NotatKode NotatKodeTekst Makulert HovedGr HovedGrBeskrivelse~
 ReturButikk ReturKasserer ReturKassererNavn b_id RefNr RefTekst SeqNr~
 Strekkode TransNr VVarekost AaaaMmDd GenerellRabatt KampanjeId KampEierId~
 KampId KampTilbId Kunderabatt LevNavn LevNr Medlemsrabatt OrgVareGr~
 Personalrabatt PrisPrSalgsenhet ProduktType SalgsType SkiftNr~
 ForKonvertering
&Scoped-define ENABLED-FIELDS-IN-BongLinje Antall ArtikkelNr BongNr ~
BongPris BongTekst ButikkNr Dato EAv EDato ETid GruppeNr KasseNr LinjeNr ~
LinjeRab LinjeSum MButikkNr Mva% MvaGr MvaGruppeNavn MvaKr OAv ODato ~
OriginalData OTid Storrelse SubtotalRab TBId TransDato TransTid TTId Type ~
VareGr VareGruppeNavn LopeNr FeilKode FeilKodeTekst NotatKode ~
NotatKodeTekst Makulert HovedGr HovedGrBeskrivelse ReturButikk ~
ReturKasserer ReturKassererNavn b_id RefNr RefTekst SeqNr Strekkode TransNr ~
VVarekost AaaaMmDd GenerellRabatt KampanjeId KampEierId KampId KampTilbId ~
Kunderabatt LevNavn LevNr Medlemsrabatt OrgVareGr Personalrabatt ~
PrisPrSalgsenhet ProduktType SalgsType SkiftNr ForKonvertering 
&Scoped-Define DATA-FIELDS  Antall fuAntall ArtikkelNr BongNr fuTransTypeTekst BongPris BongTekst~
 ButikkNr Dato EAv EDato ETid GruppeNr KasseNr LinjeNr LinjeRab LinjeSum~
 MButikkNr Mva% MvaGr MvaGruppeNavn MvaKr OAv ODato OriginalData OTid~
 Storrelse SubtotalRab TBId TransDato TransTid TTId Type VareGr~
 VareGruppeNavn LopeNr FeilKode FeilKodeTekst NotatKode NotatKodeTekst~
 Makulert HovedGr HovedGrBeskrivelse ReturButikk ReturKasserer~
 ReturKassererNavn fuTransKl b_id RefNr RefTekst SeqNr Strekkode TransNr~
 VVarekost AaaaMmDd GenerellRabatt KampanjeId KampEierId KampId KampTilbId~
 Kunderabatt LevNavn LevNr Medlemsrabatt OrgVareGr Personalrabatt~
 PrisPrSalgsenhet ProduktType SalgsType SkiftNr ForKonvertering
&Scoped-define DATA-FIELDS-IN-BongLinje Antall ArtikkelNr BongNr BongPris ~
BongTekst ButikkNr Dato EAv EDato ETid GruppeNr KasseNr LinjeNr LinjeRab ~
LinjeSum MButikkNr Mva% MvaGr MvaGruppeNavn MvaKr OAv ODato OriginalData ~
OTid Storrelse SubtotalRab TBId TransDato TransTid TTId Type VareGr ~
VareGruppeNavn LopeNr FeilKode FeilKodeTekst NotatKode NotatKodeTekst ~
Makulert HovedGr HovedGrBeskrivelse ReturButikk ReturKasserer ~
ReturKassererNavn b_id RefNr RefTekst SeqNr Strekkode TransNr VVarekost ~
AaaaMmDd GenerellRabatt KampanjeId KampEierId KampId KampTilbId Kunderabatt ~
LevNavn LevNr Medlemsrabatt OrgVareGr Personalrabatt PrisPrSalgsenhet ~
ProduktType SalgsType SkiftNr ForKonvertering 
&Scoped-Define MANDATORY-FIELDS  ArtikkelNr BongNr ButikkNr GruppeNr KasseNr LinjeNr MvaGr TTId VareGr~
 ReturKasserer OrgVareGr
&Scoped-Define APPLICATION-SERVICE 
&Scoped-Define ASSIGN-LIST 
&Scoped-Define DATA-FIELD-DEFS "dbonglinje.i"
&Scoped-define QUERY-STRING-Query-Main FOR EACH BongLinje NO-LOCK INDEXED-REPOSITION
{&DB-REQUIRED-START}
&Scoped-define OPEN-QUERY-Query-Main OPEN QUERY Query-Main FOR EACH BongLinje NO-LOCK INDEXED-REPOSITION.
{&DB-REQUIRED-END}
&Scoped-define TABLES-IN-QUERY-Query-Main BongLinje
&Scoped-define FIRST-TABLE-IN-QUERY-Query-Main BongLinje


/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD TransKl dTables  _DB-REQUIRED
FUNCTION TransKl RETURNS CHARACTER
  ( INPUT piTid AS int )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD TransTypeTekst dTables  _DB-REQUIRED
FUNCTION TransTypeTekst RETURNS CHARACTER
  ( INPUT piTTId AS INT )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}


/* ***********************  Control Definitions  ********************** */

{&DB-REQUIRED-START}

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Query-Main FOR 
      BongLinje SCROLLING.
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
     _TblList          = "Data.BongLinje"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > Data.BongLinje.Antall
"Antall" "Antall" ? ? "decimal" ? ? ? ? ? ? yes ? no 12.6 yes ""
     _FldNameList[2]   > "_<CALC>"
"round(RowObject.Antall,0)" "fuAntall" "Antall" "->>>9" "Integer" ? ? ? ? ? ? no ? no 5.4 no ?
     _FldNameList[3]   > Data.BongLinje.ArtikkelNr
"ArtikkelNr" "ArtikkelNr" ? ? "character" ? ? ? ? ? ? yes ? yes 20 yes ""
     _FldNameList[4]   > Data.BongLinje.BongNr
"BongNr" "BongNr" ? ? "integer" ? ? ? ? ? ? yes ? yes 15.6 yes ""
     _FldNameList[5]   > "_<CALC>"
"TransTypeTekst(RowObject.TTId)" "fuTransTypeTekst" "TransTypeTekst" "x(30)" "character" ? ? ? ? ? ? no ? no 30 no ?
     _FldNameList[6]   > Data.BongLinje.BongPris
"BongPris" "BongPris" ? ? "decimal" ? ? ? ? ? ? yes ? no 14.4 yes ""
     _FldNameList[7]   > Data.BongLinje.BongTekst
"BongTekst" "BongTekst" ? ? "character" ? ? ? ? ? ? yes ? no 30 yes ""
     _FldNameList[8]   > Data.BongLinje.ButikkNr
"ButikkNr" "ButikkNr" ? ? "integer" ? ? ? ? ? ? yes ? yes 7.2 yes ""
     _FldNameList[9]   > Data.BongLinje.Dato
"Dato" "Dato" ? ? "date" ? ? ? ? ? ? yes ? no 9.2 yes ""
     _FldNameList[10]   > Data.BongLinje.EAv
"EAv" "EAv" ? ? "character" ? ? ? ? ? ? yes ? no 15 yes ""
     _FldNameList[11]   > Data.BongLinje.EDato
"EDato" "EDato" ? ? "date" ? ? ? ? ? ? yes ? no 9.2 yes ""
     _FldNameList[12]   > Data.BongLinje.ETid
"ETid" "ETid" ? ? "integer" ? ? ? ? ? ? yes ? no 10.2 yes ""
     _FldNameList[13]   > Data.BongLinje.GruppeNr
"GruppeNr" "GruppeNr" ? ? "integer" ? ? ? ? ? ? yes ? yes 4.4 yes ""
     _FldNameList[14]   > Data.BongLinje.KasseNr
"KasseNr" "KasseNr" ? ? "integer" ? ? ? ? ? ? yes ? yes 8 yes ""
     _FldNameList[15]   > Data.BongLinje.LinjeNr
"LinjeNr" "LinjeNr" ? ? "integer" ? ? ? ? ? ? yes ? yes 6.6 yes ""
     _FldNameList[16]   > Data.BongLinje.LinjeRab
"LinjeRab" "LinjeRab" ? ? "decimal" ? ? ? ? ? ? yes ? no 13.2 yes ""
     _FldNameList[17]   > Data.BongLinje.LinjeSum
"LinjeSum" "LinjeSum" ? ? "decimal" ? ? ? ? ? ? yes ? no 17.4 yes ""
     _FldNameList[18]   > Data.BongLinje.MButikkNr
"MButikkNr" "MButikkNr" ? ? "integer" ? ? ? ? ? ? yes ? no 7.2 yes ""
     _FldNameList[19]   > Data.BongLinje.Mva%
"Mva%" "Mva%" ? ? "decimal" ? ? ? ? ? ? yes ? no 10.2 yes ""
     _FldNameList[20]   > Data.BongLinje.MvaGr
"MvaGr" "MvaGr" ? ? "integer" ? ? ? ? ? ? yes ? yes 4.2 yes ""
     _FldNameList[21]   > Data.BongLinje.MvaGruppeNavn
"MvaGruppeNavn" "MvaGruppeNavn" ? ? "character" ? ? ? ? ? ? yes ? no 30 yes ""
     _FldNameList[22]   > Data.BongLinje.MvaKr
"MvaKr" "MvaKr" ? ? "decimal" ? ? ? ? ? ? yes ? no 14.4 yes ""
     _FldNameList[23]   > Data.BongLinje.OAv
"OAv" "OAv" ? ? "character" ? ? ? ? ? ? yes ? no 15 yes ""
     _FldNameList[24]   > Data.BongLinje.ODato
"ODato" "ODato" ? ? "date" ? ? ? ? ? ? yes ? no 9.2 yes ""
     _FldNameList[25]   > Data.BongLinje.OriginalData
"OriginalData" "OriginalData" ? "X(200)" "character" ? ? ? ? ? ? yes ? no 200 yes ""
     _FldNameList[26]   > Data.BongLinje.OTid
"OTid" "OTid" ? ? "integer" ? ? ? ? ? ? yes ? no 10.2 yes ""
     _FldNameList[27]   > Data.BongLinje.Storrelse
"Storrelse" "Storrelse" ? ? "character" ? ? ? ? ? ? yes ? no 4 yes ""
     _FldNameList[28]   > Data.BongLinje.SubtotalRab
"SubtotalRab" "SubtotalRab" ? ? "decimal" ? ? ? ? ? ? yes ? no 13.2 yes ""
     _FldNameList[29]   > Data.BongLinje.TBId
"TBId" "TBId" "TBId" ">>9" "integer" ? ? ? ? ? ? yes ? no 27.8 yes ""
     _FldNameList[30]   > Data.BongLinje.TransDato
"TransDato" "TransDato" ? ? "date" ? ? ? ? ? ? yes ? no 9.2 yes ""
     _FldNameList[31]   > Data.BongLinje.TransTid
"TransTid" "TransTid" ? ? "integer" ? ? ? ? ? ? yes ? no 10.2 yes ""
     _FldNameList[32]   > Data.BongLinje.TTId
"TTId" "TTId" ? ? "integer" ? ? ? ? ? ? yes ? yes 4.8 yes ""
     _FldNameList[33]   > Data.BongLinje.Type
"Type" "Type" ? ? "integer" ? ? ? ? ? ? yes ? no 4.8 yes ""
     _FldNameList[34]   > Data.BongLinje.VareGr
"VareGr" "VareGr" ? ? "integer" ? ? ? ? ? ? yes ? yes 7.2 yes ""
     _FldNameList[35]   > Data.BongLinje.VareGruppeNavn
"VareGruppeNavn" "VareGruppeNavn" ? ? "character" ? ? ? ? ? ? yes ? no 30 yes ""
     _FldNameList[36]   > Data.BongLinje.LopeNr
"LopeNr" "LopeNr" ? ">>>>>9" "integer" ? ? ? ? ? ? yes ? no 7.2 yes ""
     _FldNameList[37]   > Data.BongLinje.FeilKode
"FeilKode" "FeilKode" ? ? "integer" ? ? ? ? ? ? yes ? no 2.6 yes ""
     _FldNameList[38]   > Data.BongLinje.FeilKodeTekst
"FeilKodeTekst" "FeilKodeTekst" ? ? "character" ? ? ? ? ? ? yes ? no 30 yes ""
     _FldNameList[39]   > Data.BongLinje.NotatKode
"NotatKode" "NotatKode" ? ">>9" "integer" ? ? ? ? ? ? yes ? no 3.6 yes ""
     _FldNameList[40]   > Data.BongLinje.NotatKodeTekst
"NotatKodeTekst" "NotatKodeTekst" ? ? "character" ? ? ? ? ? ? yes ? no 30 yes ""
     _FldNameList[41]   > Data.BongLinje.Makulert
"Makulert" "Makulert" ? "*~~/" "logical" ? ? ? ? ? ? yes ? no 4.2 yes ""
     _FldNameList[42]   > Data.BongLinje.HovedGr
"HovedGr" "HovedGr" ? ? "integer" ? ? ? ? ? ? yes ? no 4.8 yes ""
     _FldNameList[43]   > Data.BongLinje.HovedGrBeskrivelse
"HovedGrBeskrivelse" "HovedGrBeskrivelse" ? ? "character" ? ? ? ? ? ? yes ? no 30 yes ""
     _FldNameList[44]   > Data.BongLinje.ReturButikk
"ReturButikk" "ReturButikk" ? ? "integer" ? ? ? ? ? ? yes ? no 11.2 yes ""
     _FldNameList[45]   > Data.BongLinje.ReturKasserer
"ReturKasserer" "ReturKasserer" ? ? "decimal" ? ? ? ? ? ? yes ? yes 15.6 yes ""
     _FldNameList[46]   > Data.BongLinje.ReturKassererNavn
"ReturKassererNavn" "ReturKassererNavn" ? ? "character" ? ? ? ? ? ? yes ? no 30 yes ""
     _FldNameList[47]   > "_<CALC>"
"TransKl(RowObject.TransTid)" "fuTransKl" "Kl" "x(8)" "character" ? ? ? ? ? ? no ? no 8 no ?
     _FldNameList[48]   > Data.BongLinje.b_id
"b_id" "b_id" ? "->>>>>>>>>>>>>>>>>>>>9" "decimal" ? ? ? ? ? ? yes ? no 25.8 yes ""
     _FldNameList[49]   > Data.BongLinje.RefNr
"RefNr" "RefNr" ? "->>>>>>>>9" "integer" ? ? ? ? ? ? yes ? no 11.4 yes ""
     _FldNameList[50]   > Data.BongLinje.RefTekst
"RefTekst" "RefTekst" ? ? "character" ? ? ? ? ? ? yes ? no 40 yes ""
     _FldNameList[51]   > Data.BongLinje.SeqNr
"SeqNr" "SeqNr" ? ? "integer" ? ? ? ? ? ? yes ? no 6 yes ""
     _FldNameList[52]   > Data.BongLinje.Strekkode
"Strekkode" "Strekkode" ? "X(30)" "character" ? ? ? ? ? ? yes ? no 30 yes ""
     _FldNameList[53]   > Data.BongLinje.TransNr
"TransNr" "TransNr" ? ? "integer" ? ? ? ? ? ? yes ? no 11.4 yes ""
     _FldNameList[54]   > Data.BongLinje.VVarekost
"VVarekost" "VVarekost" ? ? "decimal" ? ? ? ? ? ? yes ? no 13.2 yes ""
     _FldNameList[55]   > Data.BongLinje.AaaaMmDd
"AaaaMmDd" "AaaaMmDd" ? ? "character" ? ? ? ? ? ? yes ? no 10.2 yes ""
     _FldNameList[56]   > Data.BongLinje.GenerellRabatt
"GenerellRabatt" "GenerellRabatt" ? ? "decimal" ? ? ? ? ? ? yes ? no 13.8 yes ""
     _FldNameList[57]   > Data.BongLinje.KampanjeId
"KampanjeId" "KampanjeId" ? ? "integer" ? ? ? ? ? ? yes ? no 11 yes ""
     _FldNameList[58]   > Data.BongLinje.KampEierId
"KampEierId" "KampEierId" ? ? "integer" ? ? ? ? ? ? yes ? no 12.8 yes ""
     _FldNameList[59]   > Data.BongLinje.KampId
"KampId" "KampId" ? ? "decimal" ? ? ? ? ? ? yes ? no 11 yes ""
     _FldNameList[60]   > Data.BongLinje.KampTilbId
"KampTilbId" "KampTilbId" ? ? "integer" ? ? ? ? ? ? yes ? no 14.4 yes ""
     _FldNameList[61]   > Data.BongLinje.Kunderabatt
"Kunderabatt" "Kunderabatt" ? ? "decimal" ? ? ? ? ? ? yes ? no 13.2 yes ""
     _FldNameList[62]   > Data.BongLinje.LevNavn
"LevNavn" "LevNavn" ? ? "character" ? ? ? ? ? ? yes ? no 30 yes ""
     _FldNameList[63]   > Data.BongLinje.LevNr
"LevNr" "LevNr" ? ? "integer" ? ? ? ? ? ? yes ? no 18.2 yes ""
     _FldNameList[64]   > Data.BongLinje.Medlemsrabatt
"Medlemsrabatt" "Medlemsrabatt" ? ? "decimal" ? ? ? ? ? ? yes ? no 13.8 yes ""
     _FldNameList[65]   > Data.BongLinje.OrgVareGr
"OrgVareGr" "OrgVareGr" ? ? "integer" ? ? ? ? ? ? yes ? yes 7.2 yes ""
     _FldNameList[66]   > Data.BongLinje.Personalrabatt
"Personalrabatt" "Personalrabatt" ? ? "decimal" ? ? ? ? ? ? yes ? no 13.6 yes ""
     _FldNameList[67]   > Data.BongLinje.PrisPrSalgsenhet
"PrisPrSalgsenhet" "PrisPrSalgsenhet" ? ? "decimal" ? ? ? ? ? ? yes ? no 17.4 yes ""
     _FldNameList[68]   > Data.BongLinje.ProduktType
"ProduktType" "ProduktType" ? ? "integer" ? ? ? ? ? ? yes ? no 11.4 yes ""
     _FldNameList[69]   > Data.BongLinje.SalgsType
"SalgsType" "SalgsType" ? ? "logical" ? ? ? ? ? ? yes ? no 9.2 yes ""
     _FldNameList[70]   > Data.BongLinje.SkiftNr
"SkiftNr" "SkiftNr" ? ? "integer" ? ? ? ? ? ? yes ? no 7.2 yes ""
     _FldNameList[71]   > Data.BongLinje.ForKonvertering
"ForKonvertering" "ForKonvertering" ? ? "character" ? ? ? ? ? ? yes ? no 40 yes ""
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
         rowObject.fuAntall = (round(RowObject.Antall,0))
         rowObject.fuTransKl = (TransKl(RowObject.TransTid))
         rowObject.fuTransTypeTekst = (TransTypeTekst(RowObject.TTId))
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

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initializeObject dTables  _DB-REQUIRED
PROCEDURE initializeObject :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  {dproclibstart.i}
  RUN GetTransTypeTekster IN h_dproclib (OUTPUT cTransTypeTekster).

  RUN SUPER.

  /* Code placed here will execute AFTER standard behavior.    */
  DYNAMIC-FUNCTION('setRebuildOnRepos':U,
     INPUT TRUE /* LOGICAL */).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

/* ************************  Function Implementations ***************** */

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION TransKl dTables  _DB-REQUIRED
FUNCTION TransKl RETURNS CHARACTER
  ( INPUT piTid AS int ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  RETURN string(piTid,"HH:MM:SS").   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION TransTypeTekst dTables  _DB-REQUIRED
FUNCTION TransTypeTekst RETURNS CHARACTER
  ( INPUT piTTId AS INT ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEF VAR pcReturTekst AS CHAR NO-UNDO.
  DEF VAR piEntry      AS INT  NO-UNDO.

  ASSIGN
      piEntry = lookup(string(piTTId,"999"),cTransTypeTekster) - 1
      .

  IF piEntry >= 0 THEN
    ASSIGN
      pcReturTekst = ENTRY(
                           piEntry,
                           cTransTypeTekster)
      .
  ELSE 
      pcReturTekst = "*Ukjent (" + STRING(piTTId,"999") + ")*"
      .

  RETURN pcReturTekst.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

