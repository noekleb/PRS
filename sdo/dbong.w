&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
/* Connected Databases 
          data             PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
{adecomm/appserv.i}


/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE TT_BongHode NO-UNDO LIKE BongHode
       FIELD EkstOrdreNr AS CHAR.
DEFINE TEMP-TABLE TT_BongLinje NO-UNDO LIKE BongLinje
       FIELD Nettokr LIKE BongLinje.LinjeSum
       FIELD DBKr LIKE BongLinje.LinjeSum
       FIELD DB% AS DECIMAL
       FIELD SelgerNr like BongHode.SelgerNr
       field SelgerNavn like BongHode.SelgerNavn
       FIELD Kundenr LIKE Bonghode.Kundenr
       FIELD Kundenavn LIKE Bonghode.Kundenavn
       FIELD Rabattkommentar as char
       FIELD IdLinjeNr as char.



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
&Scoped-define INTERNAL-TABLES BongHode

/* Definitions for QUERY Query-Main                                     */
&Scoped-Define ENABLED-FIELDS  BongNr BongStatus ButikkNr DataSettId Dato EAv EDato ETid GruppeNr KasseNr~
 KassererNavn KassererNr Konvertert KundeKort KundeNr MedlemNavn MedlemsKort~
 MedlemsNr OAv ODato OpdKvit OpdUtskKopi OTid OverforingsNr SelgerNavn~
 SelgerNr Tid UtskriftsKopi Logg KundeNavn Belop KortType Gradering b_id~
 EksportertDato flBankkort flBetalingskort flGavekort flKreditkort flKupong1~
 flRabatt flRekvisisasjon flSjekk flSlKort flSystemkort pfFlagg Systemkort~
 SkiftNr KOrdre_Id
&Scoped-define ENABLED-FIELDS-IN-BongHode BongNr BongStatus ButikkNr ~
DataSettId Dato EAv EDato ETid GruppeNr KasseNr KassererNavn KassererNr ~
Konvertert KundeKort KundeNr MedlemNavn MedlemsKort MedlemsNr OAv ODato ~
OpdKvit OpdUtskKopi OTid OverforingsNr SelgerNavn SelgerNr Tid ~
UtskriftsKopi Logg KundeNavn Belop KortType Gradering b_id EksportertDato ~
flBankkort flBetalingskort flGavekort flKreditkort flKupong1 flRabatt ~
flRekvisisasjon flSjekk flSlKort flSystemkort pfFlagg Systemkort SkiftNr ~
KOrdre_Id 
&Scoped-Define DATA-FIELDS  BongNr fuKl BongStatus ButikkNr fuStatusTekst DataSettId Dato EAv EDato~
 ETid GruppeNr KasseNr KassererNavn KassererNr Konvertert KundeKort KundeNr~
 MedlemNavn MedlemsKort MedlemsNr OAv ODato OpdKvit OpdUtskKopi OTid~
 OverforingsNr SelgerNavn SelgerNr Tid UtskriftsKopi Logg KundeNavn Belop~
 KortType Gradering b_id EksportertDato flBankkort flBetalingskort~
 flGavekort flKreditkort flKupong1 flRabatt flRekvisisasjon flSjekk flSlKort~
 flSystemkort pfFlagg Systemkort SkiftNr KOrdre_Id
&Scoped-define DATA-FIELDS-IN-BongHode BongNr BongStatus ButikkNr ~
DataSettId Dato EAv EDato ETid GruppeNr KasseNr KassererNavn KassererNr ~
Konvertert KundeKort KundeNr MedlemNavn MedlemsKort MedlemsNr OAv ODato ~
OpdKvit OpdUtskKopi OTid OverforingsNr SelgerNavn SelgerNr Tid ~
UtskriftsKopi Logg KundeNavn Belop KortType Gradering b_id EksportertDato ~
flBankkort flBetalingskort flGavekort flKreditkort flKupong1 flRabatt ~
flRekvisisasjon flSjekk flSlKort flSystemkort pfFlagg Systemkort SkiftNr ~
KOrdre_Id 
&Scoped-Define MANDATORY-FIELDS  BongNr ButikkNr GruppeNr KasseNr KassererNr SelgerNr
&Scoped-Define APPLICATION-SERVICE 
&Scoped-Define ASSIGN-LIST 
&Scoped-Define DATA-FIELD-DEFS "dbong.i"
&Scoped-define QUERY-STRING-Query-Main FOR EACH BongHode NO-LOCK INDEXED-REPOSITION
{&DB-REQUIRED-START}
&Scoped-define OPEN-QUERY-Query-Main OPEN QUERY Query-Main FOR EACH BongHode NO-LOCK INDEXED-REPOSITION.
{&DB-REQUIRED-END}
&Scoped-define TABLES-IN-QUERY-Query-Main BongHode
&Scoped-define FIRST-TABLE-IN-QUERY-Query-Main BongHode


/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD GETEkstOrdreNr dTables  _DB-REQUIRED
FUNCTION GETEkstOrdreNr RETURNS CHARACTER
  ( INPUT dKordre_ID AS DECI )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD Kl dTables  _DB-REQUIRED
FUNCTION Kl RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD StatusTekst dTables  _DB-REQUIRED
FUNCTION StatusTekst RETURNS CHARACTER
  ( INPUT piStatus AS INT )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}


/* ***********************  Control Definitions  ********************** */

{&DB-REQUIRED-START}

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Query-Main FOR 
      BongHode SCROLLING.
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
      TABLE: TT_BongHode T "?" NO-UNDO data BongHode
      ADDITIONAL-FIELDS:
          FIELD EkstOrdreNr AS CHAR
      END-FIELDS.
      TABLE: TT_BongLinje T "?" NO-UNDO data BongLinje
      ADDITIONAL-FIELDS:
          FIELD Nettokr LIKE BongLinje.LinjeSum
          FIELD DBKr LIKE BongLinje.LinjeSum
          FIELD DB% AS DECIMAL
          FIELD SelgerNr like BongHode.SelgerNr
          field SelgerNavn like BongHode.SelgerNavn
          FIELD Kundenr LIKE Bonghode.Kundenr
          FIELD Kundenavn LIKE Bonghode.Kundenavn
          FIELD Rabattkommentar as char
          FIELD IdLinjeNr as char
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
         HEIGHT             = 1.67
         WIDTH              = 46.6.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB dTables 
/* ************************* Included-Libraries *********************** */

{src/adm2/data.i}
{sdo/dproclibstart.i}

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
     _TblList          = "Data.BongHode"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > Data.BongHode.BongNr
"BongNr" "BongNr" ? ? "integer" ? ? ? ? ? ? yes ? yes 9.6 yes ""
     _FldNameList[2]   > "_<CALC>"
"Kl()" "fuKl" "Kl" "x(8)" "character" ? ? ? ? ? ? no ? no 8 no ?
     _FldNameList[3]   > Data.BongHode.BongStatus
"BongStatus" "BongStatus" ? ? "integer" ? ? ? ? ? ? yes ? no 6 yes ""
     _FldNameList[4]   > Data.BongHode.ButikkNr
"ButikkNr" "ButikkNr" ? ? "integer" ? ? ? ? ? ? yes ? yes 7.2 yes ""
     _FldNameList[5]   > "_<CALC>"
"StatusTekst(RowObject.BongStatus)" "fuStatusTekst" "Status" "x(15)" "character" ? ? ? ? ? ? no ? no 15 no ?
     _FldNameList[6]   > Data.BongHode.DataSettId
"DataSettId" "DataSettId" ? ? "decimal" ? ? ? ? ? ? yes ? no 16.8 yes ""
     _FldNameList[7]   > Data.BongHode.Dato
"Dato" "Dato" ? ? "date" ? ? ? ? ? ? yes ? no 9.2 yes ""
     _FldNameList[8]   > Data.BongHode.EAv
"EAv" "EAv" ? ? "character" ? ? ? ? ? ? yes ? no 15 yes ""
     _FldNameList[9]   > Data.BongHode.EDato
"EDato" "EDato" ? ? "date" ? ? ? ? ? ? yes ? no 9.2 yes ""
     _FldNameList[10]   > Data.BongHode.ETid
"ETid" "ETid" ? ? "integer" ? ? ? ? ? ? yes ? no 10.2 yes ""
     _FldNameList[11]   > Data.BongHode.GruppeNr
"GruppeNr" "GruppeNr" ? ? "integer" ? ? ? ? ? ? yes ? yes 4.4 yes ""
     _FldNameList[12]   > Data.BongHode.KasseNr
"KasseNr" "KasseNr" ? ? "integer" ? ? ? ? ? ? yes ? yes 8 yes ""
     _FldNameList[13]   > Data.BongHode.KassererNavn
"KassererNavn" "KassererNavn" ? ? "character" ? ? ? ? ? ? yes ? no 30 yes ""
     _FldNameList[14]   > Data.BongHode.KassererNr
"KassererNr" "KassererNr" ? ? "decimal" ? ? ? ? ? ? yes ? yes 15.6 yes ""
     _FldNameList[15]   > Data.BongHode.Konvertert
"Konvertert" "Konvertert" ? "*~~/" "logical" ? ? ? ? ? ? yes ? no 9.8 yes ""
     _FldNameList[16]   > Data.BongHode.KundeKort
"KundeKort" "KundeKort" ? ? "character" ? ? ? ? ? ? yes ? no 22 yes ""
     _FldNameList[17]   > Data.BongHode.KundeNr
"KundeNr" "KundeNr" ? ? "decimal" ? ? ? ? ? ? yes ? no 15.6 yes ""
     _FldNameList[18]   > Data.BongHode.MedlemNavn
"MedlemNavn" "MedlemNavn" ? ? "character" ? ? ? ? ? ? yes ? no 30 yes ""
     _FldNameList[19]   > Data.BongHode.MedlemsKort
"MedlemsKort" "MedlemsKort" ? ? "character" ? ? ? ? ? ? yes ? no 16 yes ""
     _FldNameList[20]   > Data.BongHode.MedlemsNr
"MedlemsNr" "MedlemsNr" ? ? "decimal" ? ? ? ? ? ? yes ? no 15.6 yes ""
     _FldNameList[21]   > Data.BongHode.OAv
"OAv" "OAv" ? ? "character" ? ? ? ? ? ? yes ? no 15 yes ""
     _FldNameList[22]   > Data.BongHode.ODato
"ODato" "ODato" ? ? "date" ? ? ? ? ? ? yes ? no 9.2 yes ""
     _FldNameList[23]   > Data.BongHode.OpdKvit
"OpdKvit" "OpdKvit" ? "*~~/" "logical" ? ? ? ? ? ? yes ? no 7.6 yes ""
     _FldNameList[24]   > Data.BongHode.OpdUtskKopi
"OpdUtskKopi" "OpdUtskKopi" ? "*~~/" "logical" ? ? ? ? ? ? yes ? no 11.6 yes ""
     _FldNameList[25]   > Data.BongHode.OTid
"OTid" "OTid" ? ? "integer" ? ? ? ? ? ? yes ? no 10.2 yes ""
     _FldNameList[26]   > Data.BongHode.OverforingsNr
"OverforingsNr" "OverforingsNr" ? ? "decimal" ? ? ? ? ? ? yes ? no 9.6 yes ""
     _FldNameList[27]   > Data.BongHode.SelgerNavn
"SelgerNavn" "SelgerNavn" ? ? "character" ? ? ? ? ? ? yes ? no 30 yes ""
     _FldNameList[28]   > Data.BongHode.SelgerNr
"SelgerNr" "SelgerNr" ? ? "integer" ? ? ? ? ? ? yes ? yes 15.6 yes ""
     _FldNameList[29]   > Data.BongHode.Tid
"Tid" "Tid" ? ? "integer" ? ? ? ? ? ? yes ? no 10.2 yes ""
     _FldNameList[30]   > Data.BongHode.UtskriftsKopi
"UtskriftsKopi" "UtskriftsKopi" ? ? "character" ? ? ? ? ? ? yes ? no 60 yes ""
     _FldNameList[31]   > Data.BongHode.Logg
"Logg" "Logg" ? ? "character" ? ? ? ? ? ? yes ? no 60 yes ""
     _FldNameList[32]   > Data.BongHode.KundeNavn
"KundeNavn" "KundeNavn" ? ? "character" ? ? ? ? ? ? yes ? no 30 yes ""
     _FldNameList[33]   > Data.BongHode.Belop
"Belop" "Belop" ? ? "decimal" ? ? ? ? ? ? yes ? no 13.2 yes ""
     _FldNameList[34]   > Data.BongHode.KortType
"KortType" "KortType" ? ? "integer" ? ? ? ? ? ? yes ? no 8.6 yes ""
     _FldNameList[35]   > Data.BongHode.Gradering
"Gradering" "Gradering" ? ? "integer" ? ? ? ? ? ? yes ? no 9.2 yes ""
     _FldNameList[36]   > Data.BongHode.b_id
"b_id" "b_id" ? ? "decimal" ? ? ? ? ? ? yes ? no 14.4 yes ""
     _FldNameList[37]   > Data.BongHode.EksportertDato
"EksportertDato" "EksportertDato" ? ? "date" ? ? ? ? ? ? yes ? no 9.6 yes ""
     _FldNameList[38]   > Data.BongHode.flBankkort
"flBankkort" "flBankkort" ? ? "logical" ? ? ? ? ? ? yes ? no 8.6 yes ""
     _FldNameList[39]   > Data.BongHode.flBetalingskort
"flBetalingskort" "flBetalingskort" ? ? "logical" ? ? ? ? ? ? yes ? no 12.2 yes ""
     _FldNameList[40]   > Data.BongHode.flGavekort
"flGavekort" "flGavekort" ? ? "logical" ? ? ? ? ? ? yes ? no 8.8 yes ""
     _FldNameList[41]   > Data.BongHode.flKreditkort
"flKreditkort" "flKreditkort" ? ? "logical" ? ? ? ? ? ? yes ? no 9 yes ""
     _FldNameList[42]   > Data.BongHode.flKupong1
"flKupong1" "flKupong1" ? ? "logical" ? ? ? ? ? ? yes ? no 8.6 yes ""
     _FldNameList[43]   > Data.BongHode.flRabatt
"flRabatt" "flRabatt" ? ? "logical" ? ? ? ? ? ? yes ? no 6.4 yes ""
     _FldNameList[44]   > Data.BongHode.flRekvisisasjon
"flRekvisisasjon" "flRekvisisasjon" ? ? "logical" ? ? ? ? ? ? yes ? no 13 yes ""
     _FldNameList[45]   > Data.BongHode.flSjekk
"flSjekk" "flSjekk" ? ? "logical" ? ? ? ? ? ? yes ? no 5.4 yes ""
     _FldNameList[46]   > Data.BongHode.flSlKort
"flSlKort" "flSlKort" ? ? "integer" ? ? ? ? ? ? yes ? no 6.6 yes ""
     _FldNameList[47]   > Data.BongHode.flSystemkort
"flSystemkort" "flSystemkort" ? ? "logical" ? ? ? ? ? ? yes ? no 10.4 yes ""
     _FldNameList[48]   > Data.BongHode.pfFlagg
"pfFlagg" "pfFlagg" ? ? "integer" ? ? ? ? ? ? yes ? no 17.8 yes ""
     _FldNameList[49]   > Data.BongHode.Systemkort
"Systemkort" "Systemkort" ? ? "character" ? ? ? ? ? ? yes ? no 30 yes ""
     _FldNameList[50]   > Data.BongHode.SkiftNr
"SkiftNr" "SkiftNr" ? ? "integer" ? ? ? ? ? ? yes ? no 7.2 yes ""
     _FldNameList[51]   > Data.BongHode.KOrdre_Id
"KOrdre_Id" "KOrdre_Id" ? ? "decimal" ? ? ? ? ? ? yes ? no 15.6 yes ?
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE BongToTT dTables  _DB-REQUIRED
PROCEDURE BongToTT :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 DEFINE OUTPUT PARAMETER TTH AS HANDLE NO-UNDO.
 DEFINE INPUT  PARAMETER pcHodeVerdier  AS CHARACTER  NO-UNDO.
 DEFINE INPUT  PARAMETER pcAndQuery     AS CHARACTER  NO-UNDO.
 DEFINE INPUT  PARAMETER pcBongLinje    AS CHARACTER  NO-UNDO.
 DEFINE INPUT  PARAMETER qh             AS HANDLE     NO-UNDO.
 DEFINE VARIABLE cTTId     AS CHARACTER  NO-UNDO.
 DEFINE VARIABLE dFraBel   AS DECIMAL    NO-UNDO.
 DEFINE VARIABLE dTilBel   AS DECIMAL    NO-UNDO.
 DEFINE VARIABLE lVisLinje AS LOGICAL    NO-UNDO.
 DEFINE VARIABLE lSokNetto AS LOGICAL    NO-UNDO.
 DEFINE VARIABLE iGruppe     AS INTEGER    NO-UNDO.
 DEFINE VARIABLE dFra        AS DATE       NO-UNDO.
 DEFINE VARIABLE dTil        AS DATE       NO-UNDO.
 DEFINE VARIABLE cButListe   AS CHARACTER  NO-UNDO.
 DEFINE VARIABLE cKasseListe AS CHARACTER  NO-UNDO.
 DEFINE VARIABLE cDatoListe AS CHARACTER  NO-UNDO.
 DEFINE VARIABLE cQry        AS CHARACTER  NO-UNDO.
 DEFINE VARIABLE iCount      AS INTEGER    NO-UNDO.
 DEFINE VARIABLE iCount2     AS INTEGER    NO-UNDO.
 DEFINE VARIABLE dDato      AS DATE       NO-UNDO.
 DEFINE VARIABLE hQry       AS HANDLE     NO-UNDO.
 DEFINE VARIABLE lFirst     AS LOGICAL    NO-UNDO.
 DEFINE VARIABLE rRowId     AS ROWID      NO-UNDO.
 DEFINE VARIABLE lMakulerte AS LOGICAL    NO-UNDO.
 EMPTY TEMP-TABLE TT_BongHode.
 EMPTY TEMP-TABLE TT_BongLinje.

 IF pcBongLinje <> "" THEN DO:
     ASSIGN cTTId      = ENTRY(1,pcBongLinje,CHR(1))
            lVisLinje  = ENTRY(3,pcBongLinje,CHR(1)) = "J"
            lSokNetto  = ENTRY(4,pcBongLinje,CHR(1)) = "J"
            lMakulerte = ENTRY(5,pcBongLinje,CHR(1)) = "J".
     IF ENTRY(2,pcBongLinje,CHR(1)) <> "" THEN
         ASSIGN dFraBel      = DECI(ENTRY(1,ENTRY(2,pcBongLinje,CHR(1)),";"))
                dTilBel      = DECI(ENTRY(2,ENTRY(2,pcBongLinje,CHR(1)),";")).
 END.
 ASSIGN cButListe   = ENTRY(1,pcHodeVerdier,CHR(1))
        cKasseListe = ENTRY(2,pcHodeVerdier,CHR(1))
        cDatoListe  = ENTRY(3,pcHodeVerdier,CHR(1))
        dFra        = DATE(ENTRY(1,cDatoListe))
        dTil        = DATE(ENTRY(2,cDatoListe)).
 ASSIGN hQry = DYNAMIC-FUNCTION('getQueryHandle':U).
 DO iCount = 1 TO NUM-ENTRIES(cButListe).
     DO iCount2 = 1 TO NUM-ENTRIES(cKasseListe).
         DO dDato = dFra TO dTil:
             ASSIGN cQry = "FOR EACH BongHode NO-LOCK WHERE BongHode.ButikkNr = " + ENTRY(iCount,cButListe) +
                           " AND BongHode.GruppeNr = 1 AND BongHode.KasseNr = " + ENTRY(iCount2,cKasseListe) + 
                           " AND BongHode.dato = " + "'" + STRING(dDato) + "'" + IF pcAndQuery = "" THEN "" ELSE pcAndQuery.
             DYNAMIC-FUNCTION('closeQuery':U).
             DYNAMIC-FUNCTION('setQueryString':U,
                    INPUT cQry /* CHARACTER */).
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
                 IF pcBongLinje <> "" THEN DO:
                     IF lMakulerte = TRUE AND NOT CAN-FIND(FIRST BongLinje WHERE BongLinje.b_id = RowObject.b_id AND Bonglinje.Makulert = TRUE) THEN
                         NEXT.
                     IF ENTRY(2,pcBongLinje,CHR(1)) = "" THEN DO:
                         IF NOT CAN-FIND(FIRST BongLinje WHERE BongLinje.b_id = RowObject.b_id AND Bonglinje.TTId = INT(cTTId) USE-INDEX b_id) THEN
                             NEXT.
                         IF lVisLinje = TRUE THEN DO:
                             FOR EACH BongLinje NO-LOCK WHERE BongLinje.b_id = RowObject.b_id AND Bonglinje.TTId = INT(cTTId) USE-INDEX b_id:
                                 CREATE TT_BongLinje.
                                 BUFFER-COPY BongLinje TO TT_BongLinje.
                                 ASSIGN
                                     tt_BongLinje.SelgerNr   = RowObject.SelgerNr
                                     tt_BongLinje.SelgerNavn = RowObject.SelgerNavn
                                     tt_BongLinje.KundeNr    = RowObject.KundeNr
                                     tt_BongLinje.KundeNavn  = RowObject.KundeNavn.
                                 RUN KalkulerNetto.
                             END.
                         END.
                     END.
                     ELSE DO:
                         IF NOT CAN-FIND(FIRST BongLinje WHERE BongLinje.b_id = RowObject.b_id AND Bonglinje.TTId = INT(cTTId) AND
                                         (IF lSokNetto THEN BongLinje.LinjeSum - BongLinje.LinjeRab - BongLinje.SubtotalRab ELSE BongLinje.LinjeSum) >= dFraBel AND
                                          (IF lSokNetto THEN BongLinje.LinjeSum - BongLinje.LinjeRab - BongLinje.SubtotalRab ELSE BongLinje.LinjeSum) <= dTilBel USE-INDEX b_id) THEN
                             NEXT.
                         IF lVisLinje = TRUE THEN DO:
                             FOR EACH BongLinje NO-LOCK WHERE BongLinje.b_id = RowObject.b_id AND Bonglinje.TTId = INT(cTTId) AND
                                             (IF lSokNetto THEN BongLinje.LinjeSum - BongLinje.LinjeRab - BongLinje.SubtotalRab ELSE BongLinje.LinjeSum) >= dFraBel AND
                                             (IF lSokNetto THEN BongLinje.LinjeSum - BongLinje.LinjeRab - BongLinje.SubtotalRab ELSE BongLinje.LinjeSum) <= dTilBel USE-INDEX b_id:
                                 CREATE TT_BongLinje.
                                 BUFFER-COPY BongLinje TO TT_BongLinje.
                                 ASSIGN
                                     tt_BongLinje.SelgerNr   = RowObject.SelgerNr
                                     tt_BongLinje.SelgerNavn = RowObject.SelgerNavn
                                     tt_BongLinje.KundeNr    = RowObject.KundeNr
                                     tt_BongLinje.KundeNavn  = RowObject.KundeNavn.
                                 RUN KalkulerNetto.
                             END.
                         END.
                     END.
                 END.
                 IF NOT lVisLinje THEN DO:
                     CREATE TT_BongHode.
                     BUFFER-COPY RowObject TO TT_BongHode.
                     IF TT_BongHode.KOrdre_Id > 0 THEN
                         TT_BongHode.EkstOrdreNr = GETEkstOrdreNr(TT_BongHode.KOrdre_Id).
                 END.
             END.
         END.
     END.
 END.
 DYNAMIC-FUNCTION('closeQuery':U).
 IF lVisLinje = TRUE THEN
     ASSIGN TTH = BUFFER TT_BongLinje:HANDLE.
 ELSE
     ASSIGN TTH = BUFFER TT_BongHode:HANDLE.

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
      ASSIGN 
         rowObject.fuKl = (Kl())
         rowObject.fuStatusTekst = (StatusTekst(RowObject.BongStatus))
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

  RUN SUPER.

  /* Code placed here will execute AFTER standard behavior.    */
  DYNAMIC-FUNCTION('setRebuildOnRepos':U,
     INPUT TRUE /* LOGICAL */).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE KalkulerNetto dTables  _DB-REQUIRED
PROCEDURE KalkulerNetto :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    ASSIGN 
/*            TT_BongLinje.VVarekost = TT_BongLinje.Antall * TT_BongLinje.VVarekost */
           TT_BongLinje.Nettokr = TT_BongLinje.LinjeSum - TT_BongLinje.LinjeRab - TT_BongLinje.SubtotalRab
           TT_BongLinje.DBKr    = TT_BongLinje.Nettokr - TT_BongLinje.MvaKr - TT_BongLinje.VVarekost
           TT_BongLinje.DB%     = IF TT_BongLinje.Nettokr - TT_BongLinje.MvaKr <> 0 THEN 
               ROUND(100 * (TT_BongLinje.DBKr / (TT_BongLinje.Nettokr - TT_BongLinje.MvaKr)),1) ELSE 0.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE RabBongLinjeToTT dTables  _DB-REQUIRED
PROCEDURE RabBongLinjeToTT :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 DEFINE OUTPUT PARAMETER TTH AS HANDLE NO-UNDO.
 DEFINE INPUT  PARAMETER cButListe  AS CHARACTER  NO-UNDO.
 DEFINE INPUT  PARAMETER cDatoListe     AS CHARACTER  NO-UNDO.
 DEFINE INPUT  PARAMETER cTTIdlista    AS CHARACTER  NO-UNDO.
 DEFINE INPUT  PARAMETER cRabattLista AS CHARACTER   NO-UNDO.
 DEFINE INPUT  PARAMETER qh             AS HANDLE     NO-UNDO.
 DEFINE VARIABLE cTTId     AS CHARACTER  NO-UNDO.
 DEFINE VARIABLE dFraBel   AS DECIMAL    NO-UNDO.
 DEFINE VARIABLE dTilBel   AS DECIMAL    NO-UNDO.
 DEFINE VARIABLE lVisLinje AS LOGICAL    NO-UNDO.
 DEFINE VARIABLE lSokNetto AS LOGICAL    NO-UNDO.
 DEFINE VARIABLE iGruppe     AS INTEGER    NO-UNDO.
 DEFINE VARIABLE dFra        AS DATE       NO-UNDO.
 DEFINE VARIABLE dTil        AS DATE       NO-UNDO.
 DEFINE VARIABLE cQry        AS CHARACTER  NO-UNDO.
 DEFINE VARIABLE iCount      AS INTEGER    NO-UNDO.
 DEFINE VARIABLE iCount2     AS INTEGER    NO-UNDO.
 DEFINE VARIABLE dDato      AS DATE       NO-UNDO.
 DEFINE VARIABLE hQry       AS HANDLE     NO-UNDO.
 DEFINE VARIABLE lFirst     AS LOGICAL    NO-UNDO.
 DEFINE VARIABLE rRowId     AS ROWID      NO-UNDO.
 DEFINE VARIABLE lMakulerte AS LOGICAL    NO-UNDO.
 DEFINE VARIABLE cRabattkommentar AS CHARACTER   NO-UNDO.
 DEFINE BUFFER bufBonglinje FOR Bonglinje.
 EMPTY TEMP-TABLE TT_BongHode.
 EMPTY TEMP-TABLE TT_BongLinje.

 lVisLinje = TRUE. /* Alltid */

/*  IF pcBongLinje <> "" THEN DO:                                                 */
/*      ASSIGN cTTId      = ENTRY(1,pcBongLinje,CHR(1))                           */
/*             lVisLinje  = ENTRY(3,pcBongLinje,CHR(1)) = "J"                     */
/*             lSokNetto  = ENTRY(4,pcBongLinje,CHR(1)) = "J"                     */
/*             lMakulerte = ENTRY(5,pcBongLinje,CHR(1)) = "J".                    */
/*      IF ENTRY(2,pcBongLinje,CHR(1)) <> "" THEN                                 */
/*          ASSIGN dFraBel      = DECI(ENTRY(1,ENTRY(2,pcBongLinje,CHR(1)),";"))  */
/*                 dTilBel      = DECI(ENTRY(2,ENTRY(2,pcBongLinje,CHR(1)),";")). */
/*  END.                                                                          */
 ASSIGN dFra        = DATE(ENTRY(1,cDatoListe))
        dTil        = DATE(ENTRY(2,cDatoListe)).
 ASSIGN hQry = DYNAMIC-FUNCTION('getQueryHandle':U).
 DO iCount = 1 TO NUM-ENTRIES(cButListe).
     DO dDato = dFra TO dTil:
         ASSIGN cQry = "FOR EACH BongHode NO-LOCK WHERE BongHode.ButikkNr = " + ENTRY(iCount,cButListe) +
                       " AND BongHode.dato = " + "'" + STRING(dDato) + "'".
         DYNAMIC-FUNCTION('closeQuery':U).
         DYNAMIC-FUNCTION('setQueryString':U,
                INPUT cQry /* CHARACTER */).
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
             FOR EACH BongLinje NO-LOCK WHERE BongLinje.b_id = RowObject.b_id USE-INDEX b_id:
                 IF NOT CAN-DO(cTTIdlista,STRING(Bonglinje.TTId)) OR bonglinje.makulert = TRUE OR bonglinje.linjerab = 0 OR bonglinje.linjerab = ? THEN
                     NEXT.
                 IF NOT CAN-DO(cRabattLista,STRING(bonglinje.feilkode)) THEN
                     NEXT.
                 RELEASE levbas.
                 IF bonglinje.levnr <> 0 AND bonglinje.levnavn = "" THEN DO:
                     FIND artbas WHERE artbas.artikkelnr = DECI(bonglinje.artikkelnr) NO-LOCK NO-ERROR.
                     FIND levbas WHERE levbas.levnr = artbas.levnr NO-LOCK NO-ERROR.
                 END.
                 cRabattkommentar = "".
                 IF bonglinje.ttid = 1 THEN DO:
                     FIND bufBonglinje WHERE bufBonglinje.butikknr = bonglinje.butikknr AND
                                             bufBonglinje.gruppenr = Bonglinje.gruppenr AND
                                             bufBonglinje.kassenr  = Bonglinje.kassenr  AND
                                             bufBonglinje.dato     = Bonglinje.dato     AND
                                             bufBonglinje.bongnr   = Bonglinje.bongnr   AND
                                             bufBonglinje.linjenr  = Bonglinje.linjenr + 1 NO-LOCK NO-ERROR.
                     IF AVAIL bufBonglinje AND bufBonglinje.ttid = 95 THEN
                         cRabattkommentar = bufBonglinje.bongtekst.
                 END.
                 CREATE TT_BongLinje.
                 BUFFER-COPY BongLinje TO TT_BongLinje.
                 ASSIGN
                     tt_BongLinje.SelgerNr   = RowObject.SelgerNr
                     tt_BongLinje.SelgerNavn = RowObject.SelgerNavn
                     tt_BongLinje.KundeNr    = RowObject.KundeNr
                     tt_BongLinje.KundeNavn  = RowObject.KundeNavn
                     tt_BongLinje.levnr      = IF AVAIL levbas THEN levbas.levnr   ELSE tt_BongLinje.levnr
                     tt_BongLinje.levnavn    = IF AVAIL levbas THEN levbas.levnamn ELSE tt_BongLinje.levnavn
                     tt_BongLinje.IdLinjeNr  = STRING(Bonglinje.b_id) + ";" + STRING(Bonglinje.Linjenr)
                     tt_Bonglinje.Rabattkommentar = cRabattkommentar.
                 RUN KalkulerNetto.
             END.
         END.
     END.
 END.
 DYNAMIC-FUNCTION('closeQuery':U).
     ASSIGN TTH = BUFFER TT_BongLinje:HANDLE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SokSdo dTables  _DB-REQUIRED
PROCEDURE SokSdo :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER pcSokFelt    AS CHAR NO-UNDO.
  DEF INPUT PARAMETER pcSokValues  AS CHAR NO-UNDO.
  DEF INPUT PARAMETER pcSokSort    AS CHAR NO-UNDO.
  DEF INPUT PARAMETER pcOperators  AS CHAR NO-UNDO.
  DEF INPUT PARAMETER pbFKeys      AS LOG  NO-UNDO.
  
  DEF VAR pcWhere  AS CHAR NO-UNDO.
  DEF VAR piLoop1  AS INT  NO-UNDO.
  DEF VAR piLoop2  AS INT  NO-UNDO.
  DEF VAR pcFelt   AS CHAR NO-UNDO.
  DEF VAR pcValues AS CHAR NO-UNDO.
  DEF VAR pcOp     AS CHAR NO-UNDO.


  /* Setter opp betingelse for fremmednøkler. */
  IF pbFKeys = FALSE THEN
  FKEYS:
  DO:
      ASSIGN
          pcFelt   = DYNAMIC-FUNCTION('getForeignFields':U)
          pcValues = DYNAMIC-FUNCTION('getForeignValues':U)
          pcWhere  = ""
          piLoop2  = 1
          .

      /* Legger på ForegnKeys */
      IF pcFelt <> "" THEN
      DO piLoop1 = 1 TO (NUM-ENTRIES(pcFelt) / 2):
        ASSIGN
          pcWhere  = pcWhere +
                     (IF pcWhere <> "" 
                         THEN " and "
                         ELSE "") +
                     ENTRY(piLoop2,pcFelt) + " = " + ENTRY(piLoop1,pcValues,CHR(1))
          piLoop2  = piLoop2 + 2 
          .
      END.
  END. /* FKEYS */

  /* Legger på søkemasken */
  IF pcSokFelt <> "" THEN
  DO piLoop1 = 1 TO (NUM-ENTRIES(pcSokFelt)):
    ASSIGN
      pcOp     = (IF pcOperators = ""
                    THEN "="
                    ELSE ENTRY(piLoop1,pcOperators))
      pcWhere  = pcWhere +
                 (IF pcWhere <> "" 
                     THEN " and "
                     ELSE "") +
                 ENTRY(piLoop1,pcSokFelt) + 
                 " " + pcOp + " " + 
                 ENTRY(piLoop1,pcSokValues,CHR(1))
      .
  END.

  /* Setter tilbake BASE Query */
  DYNAMIC-FUNCTION('setQueryWhere':U,
     INPUT "" /* CHARACTER */).

  /* Legger inn ny filterverdi */
  IF pcWhere <> "" THEN
    DYNAMIC-FUNCTION('addQueryWhere':U,
       INPUT pcWhere /* CHARACTER */,
        INPUT "",
        INPUT "").

  /* Legger inn valgt sortering */
  IF pcSokSort <> "" THEN
    DYNAMIC-FUNCTION('setQuerySort':U,
       INPUT pcSokSort /* CHARACTER */).

  /* Åpner query med ny filterverdi. */
  DYNAMIC-FUNCTION('openQuery':U).
         
 /*
 MESSAGE  DYNAMIC-FUNCTION('getQueryWhere':U)
     VIEW-AS ALERT-BOX INFO BUTTONS OK.
 */

  /* ?????
  /* For at ikke query skal forstyrres hvis man endrer på andre */
  /* fliker, settes query tilbake til standard qyery.           */
  /* Dette gjøres uten å kjøre OpenQuery. Det blir gjort når    */
  /* det f.eks byttes datasett på fliken foran.                 */
  /* Setter tilbake BASE Query */
  DYNAMIC-FUNCTION('setQueryWhere':U,
     INPUT "" /* CHARACTER */).
  */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

/* ************************  Function Implementations ***************** */

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION GETEkstOrdreNr dTables  _DB-REQUIRED
FUNCTION GETEkstOrdreNr RETURNS CHARACTER
  ( INPUT dKordre_ID AS DECI ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cTxt AS CHARACTER   NO-UNDO.
  FIND KOrdreHode WHERE KOrdreHode.Kordre_id = dKordre_ID NO-LOCK NO-ERROR.
  IF AVAIL KOrdreHode THEN DO:
      cTxt = TRIM(REPLACE(KOrdreHode.EkstOrdreNr,"RETUR","")).
  END.
  OUTPUT TO c:\tmp\ekst.txt APPEND.
  PUT UNFORMATTED "- "cTxt SKIP.
  OUTPUT CLOSE.
  RETURN cTxt.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION Kl dTables  _DB-REQUIRED
FUNCTION Kl RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  RETURN STRING(RowObject.Tid,"HH:MM:SS").

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION StatusTekst dTables  _DB-REQUIRED
FUNCTION StatusTekst RETURNS CHARACTER
  ( INPUT piStatus AS INT ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEF VAR pcTekst       AS CHAR NO-UNDO.
  DEF VAR pcStatusListe AS CHAR NO-UNDO.


  RUN BongStatsTekst IN h_dproclib (INPUT piStatus).

  RETURN RETURN-VALUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

