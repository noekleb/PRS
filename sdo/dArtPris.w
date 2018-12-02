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
&Scoped-define INTERNAL-TABLES ArtPris

/* Definitions for QUERY Query-Main                                     */
&Scoped-Define ENABLED-FIELDS  AktivFraDato AktivFraTid ArtikkelNr BrukerID DB%1 DB%2 DBKr1 DBKr2~
 DivKost%1 DivKost%2 DivKostKr1 DivKostKr2 EDato ETid EuroManuel EuroPris1~
 EuroPris2 Frakt1 Frakt2 Frakt%1 Frakt%2 InnkjopsPris1 InnkjopsPris2 LevNr~
 Mva%1 Mva%2 MvaKr1 MvaKr2 Pris1 Pris2 ProfilNr Rab1%1 Rab1%2 Rab1Kr1~
 Rab1Kr2 Rab2%1 Rab2%2 Rab2Kr1 Rab2Kr2 Rab3%1 Rab3%2 Rab3Kr1 Rab3Kr2~
 RegistrertAv RegistrertDato RegistrertTid Tilbud TilbudFraDato TilbudFraTid~
 TilbudTilDato TilbudTilTid TilbudTimeStyrt ValPris1 ValPris2 VareKost1~
 VareKost2
&Scoped-define ENABLED-FIELDS-IN-ArtPris AktivFraDato AktivFraTid ~
ArtikkelNr BrukerID DB%1 DB%2 DBKr1 DBKr2 DivKost%1 DivKost%2 DivKostKr1 ~
DivKostKr2 EDato ETid EuroManuel EuroPris1 EuroPris2 Frakt1 Frakt2 Frakt%1 ~
Frakt%2 InnkjopsPris1 InnkjopsPris2 LevNr Mva%1 Mva%2 MvaKr1 MvaKr2 Pris1 ~
Pris2 ProfilNr Rab1%1 Rab1%2 Rab1Kr1 Rab1Kr2 Rab2%1 Rab2%2 Rab2Kr1 Rab2Kr2 ~
Rab3%1 Rab3%2 Rab3Kr1 Rab3Kr2 RegistrertAv RegistrertDato RegistrertTid ~
Tilbud TilbudFraDato TilbudFraTid TilbudTilDato TilbudTilTid ~
TilbudTimeStyrt ValPris1 ValPris2 VareKost1 VareKost2 
&Scoped-Define DATA-FIELDS  AktivFraDato AktivFraTid ArtikkelNr BrukerID DB%1 DB%2 DBKr1 DBKr2~
 DivKost%1 DivKost%2 DivKostKr1 DivKostKr2 EDato ETid EuroManuel EuroPris1~
 EuroPris2 Frakt1 Frakt2 Frakt%1 Frakt%2 InnkjopsPris1 InnkjopsPris2 LevNr~
 Mva%1 Mva%2 MvaKr1 MvaKr2 Pris1 Pris2 ProfilNr Rab1%1 Rab1%2 Rab1Kr1~
 Rab1Kr2 Rab2%1 Rab2%2 Rab2Kr1 Rab2Kr2 Rab3%1 Rab3%2 Rab3Kr1 Rab3Kr2~
 RegistrertAv RegistrertDato RegistrertTid Tilbud TilbudFraDato TilbudFraTid~
 TilbudTilDato TilbudTilTid TilbudTimeStyrt ValPris1 ValPris2 VareKost1~
 VareKost2 FuInnkjopsPris FuRab1Kr FuRab1% FuRab2Kr FuRab2% FuFrakt FuFrakt%~
 FuDivKostKr FuDivKost% FuRab3Kr FuRab3% FuVareKost FuDBKr FuDB% FuMvaKr~
 FuMva% FuPris FuEuroPris FuValPris FuHarTilbudsPris
&Scoped-define DATA-FIELDS-IN-ArtPris AktivFraDato AktivFraTid ArtikkelNr ~
BrukerID DB%1 DB%2 DBKr1 DBKr2 DivKost%1 DivKost%2 DivKostKr1 DivKostKr2 ~
EDato ETid EuroManuel EuroPris1 EuroPris2 Frakt1 Frakt2 Frakt%1 Frakt%2 ~
InnkjopsPris1 InnkjopsPris2 LevNr Mva%1 Mva%2 MvaKr1 MvaKr2 Pris1 Pris2 ~
ProfilNr Rab1%1 Rab1%2 Rab1Kr1 Rab1Kr2 Rab2%1 Rab2%2 Rab2Kr1 Rab2Kr2 Rab3%1 ~
Rab3%2 Rab3Kr1 Rab3Kr2 RegistrertAv RegistrertDato RegistrertTid Tilbud ~
TilbudFraDato TilbudFraTid TilbudTilDato TilbudTilTid TilbudTimeStyrt ~
ValPris1 ValPris2 VareKost1 VareKost2 
&Scoped-Define MANDATORY-FIELDS 
&Scoped-Define APPLICATION-SERVICE 
&Scoped-Define ASSIGN-LIST   rowObject.DB%1 = ArtPris.DB%[1]  rowObject.DB%2 = ArtPris.DB%[2]~
  rowObject.DBKr1 = ArtPris.DBKr[1]  rowObject.DBKr2 = ArtPris.DBKr[2]~
  rowObject.DivKost%1 = ArtPris.DivKost%[1]~
  rowObject.DivKost%2 = ArtPris.DivKost%[2]~
  rowObject.DivKostKr1 = ArtPris.DivKostKr[1]~
  rowObject.DivKostKr2 = ArtPris.DivKostKr[2]~
  rowObject.EuroPris1 = ArtPris.EuroPris[1]~
  rowObject.EuroPris2 = ArtPris.EuroPris[2]~
  rowObject.Frakt1 = ArtPris.Frakt[1]  rowObject.Frakt2 = ArtPris.Frakt[2]~
  rowObject.Frakt%1 = ArtPris.Frakt%[1]~
  rowObject.Frakt%2 = ArtPris.Frakt%[2]~
  rowObject.InnkjopsPris1 = ArtPris.InnkjopsPris[1]~
  rowObject.InnkjopsPris2 = ArtPris.InnkjopsPris[2]~
  rowObject.Mva%1 = ArtPris.Mva%[1]  rowObject.Mva%2 = ArtPris.Mva%[2]~
  rowObject.MvaKr1 = ArtPris.MvaKr[1]  rowObject.MvaKr2 = ArtPris.MvaKr[2]~
  rowObject.Pris1 = ArtPris.Pris[1]  rowObject.Pris2 = ArtPris.Pris[2]~
  rowObject.Rab1%1 = ArtPris.Rab1%[1]  rowObject.Rab1%2 = ArtPris.Rab1%[2]~
  rowObject.Rab1Kr1 = ArtPris.Rab1Kr[1]~
  rowObject.Rab1Kr2 = ArtPris.Rab1Kr[2]  rowObject.Rab2%1 = ArtPris.Rab2%[1]~
  rowObject.Rab2%2 = ArtPris.Rab2%[2]  rowObject.Rab2Kr1 = ArtPris.Rab2Kr[1]~
  rowObject.Rab2Kr2 = ArtPris.Rab2Kr[2]  rowObject.Rab3%1 = ArtPris.Rab3%[1]~
  rowObject.Rab3%2 = ArtPris.Rab3%[2]  rowObject.Rab3Kr1 = ArtPris.Rab3Kr[1]~
  rowObject.Rab3Kr2 = ArtPris.Rab3Kr[2]~
  rowObject.ValPris1 = ArtPris.ValPris[1]~
  rowObject.ValPris2 = ArtPris.ValPris[2]~
  rowObject.VareKost1 = ArtPris.VareKost[1]~
  rowObject.VareKost2 = ArtPris.VareKost[2]
&Scoped-Define DATA-FIELD-DEFS "dArtPris.i"
&Scoped-define QUERY-STRING-Query-Main FOR EACH ArtPris NO-LOCK INDEXED-REPOSITION
{&DB-REQUIRED-START}
&Scoped-define OPEN-QUERY-Query-Main OPEN QUERY Query-Main FOR EACH ArtPris NO-LOCK INDEXED-REPOSITION.
{&DB-REQUIRED-END}
&Scoped-define TABLES-IN-QUERY-Query-Main ArtPris
&Scoped-define FIRST-TABLE-IN-QUERY-Query-Main ArtPris


/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

{&DB-REQUIRED-START}

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Query-Main FOR 
      ArtPris SCROLLING.
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
     _TblList          = "skotex.ArtPris"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > skotex.ArtPris.AktivFraDato
"AktivFraDato" "AktivFraDato" ? ? "date" ? ? ? ? ? ? yes ? no 11.6 yes ""
     _FldNameList[2]   > skotex.ArtPris.AktivFraTid
"AktivFraTid" "AktivFraTid" ? ? "integer" ? ? ? ? ? ? yes ? no 16.4 yes ""
     _FldNameList[3]   > skotex.ArtPris.ArtikkelNr
"ArtikkelNr" "ArtikkelNr" ? ? "decimal" ? ? ? ? ? ? yes ? no 14.4 yes ""
     _FldNameList[4]   > skotex.ArtPris.BrukerID
"BrukerID" "BrukerID" ? ? "character" ? ? ? ? ? ? yes ? no 10 yes ""
     _FldNameList[5]   > skotex.ArtPris.DB%[1]
"DB%[1]" "DB%1" ? ? "decimal" ? ? ? ? ? ? yes ? no 10.2 yes ""
     _FldNameList[6]   > skotex.ArtPris.DB%[2]
"DB%[2]" "DB%2" ? ? "decimal" ? ? ? ? ? ? yes ? no 10.2 yes ""
     _FldNameList[7]   > skotex.ArtPris.DBKr[1]
"DBKr[1]" "DBKr1" ? ? "decimal" ? ? ? ? ? ? yes ? no 10.2 yes ""
     _FldNameList[8]   > skotex.ArtPris.DBKr[2]
"DBKr[2]" "DBKr2" ? ? "decimal" ? ? ? ? ? ? yes ? no 10.2 yes ""
     _FldNameList[9]   > skotex.ArtPris.DivKost%[1]
"DivKost%[1]" "DivKost%1" ? ? "decimal" ? ? ? ? ? ? yes ? no 10.2 yes ""
     _FldNameList[10]   > skotex.ArtPris.DivKost%[2]
"DivKost%[2]" "DivKost%2" ? ? "decimal" ? ? ? ? ? ? yes ? no 10.2 yes ""
     _FldNameList[11]   > skotex.ArtPris.DivKostKr[1]
"DivKostKr[1]" "DivKostKr1" ? ? "decimal" ? ? ? ? ? ? yes ? no 10.2 yes ""
     _FldNameList[12]   > skotex.ArtPris.DivKostKr[2]
"DivKostKr[2]" "DivKostKr2" ? ? "decimal" ? ? ? ? ? ? yes ? no 10.2 yes ""
     _FldNameList[13]   > skotex.ArtPris.EDato
"EDato" "EDato" ? ? "date" ? ? ? ? ? ? yes ? no 11.6 yes ""
     _FldNameList[14]   > skotex.ArtPris.ETid
"ETid" "ETid" ? ? "integer" ? ? ? ? ? ? yes ? no 10.2 yes ""
     _FldNameList[15]   > skotex.ArtPris.EuroManuel
"EuroManuel" "EuroManuel" ? ? "logical" ? ? ? ? ? ? yes ? no 11.4 yes ""
     _FldNameList[16]   > skotex.ArtPris.EuroPris[1]
"EuroPris[1]" "EuroPris1" ? ? "decimal" ? ? ? ? ? ? yes ? no 13.2 yes ""
     _FldNameList[17]   > skotex.ArtPris.EuroPris[2]
"EuroPris[2]" "EuroPris2" ? ? "decimal" ? ? ? ? ? ? yes ? no 13.2 yes ""
     _FldNameList[18]   > skotex.ArtPris.Frakt[1]
"Frakt[1]" "Frakt1" ? ? "decimal" ? ? ? ? ? ? yes ? no 10.2 yes ""
     _FldNameList[19]   > skotex.ArtPris.Frakt[2]
"Frakt[2]" "Frakt2" ? ? "decimal" ? ? ? ? ? ? yes ? no 10.2 yes ""
     _FldNameList[20]   > skotex.ArtPris.Frakt%[1]
"Frakt%[1]" "Frakt%1" ? ? "decimal" ? ? ? ? ? ? yes ? no 10.2 yes ""
     _FldNameList[21]   > skotex.ArtPris.Frakt%[2]
"Frakt%[2]" "Frakt%2" ? ? "decimal" ? ? ? ? ? ? yes ? no 10.2 yes ""
     _FldNameList[22]   > skotex.ArtPris.InnkjopsPris[1]
"InnkjopsPris[1]" "InnkjopsPris1" ? ? "decimal" ? ? ? ? ? ? yes ? no 11.2 yes ""
     _FldNameList[23]   > skotex.ArtPris.InnkjopsPris[2]
"InnkjopsPris[2]" "InnkjopsPris2" ? ? "decimal" ? ? ? ? ? ? yes ? no 11.2 yes ""
     _FldNameList[24]   > skotex.ArtPris.LevNr
"LevNr" "LevNr" ? ? "integer" ? ? ? ? ? ? yes ? no 5.8 yes ""
     _FldNameList[25]   > skotex.ArtPris.Mva%[1]
"Mva%[1]" "Mva%1" ? ? "decimal" ? ? ? ? ? ? yes ? no 10.2 yes ""
     _FldNameList[26]   > skotex.ArtPris.Mva%[2]
"Mva%[2]" "Mva%2" ? ? "decimal" ? ? ? ? ? ? yes ? no 10.2 yes ""
     _FldNameList[27]   > skotex.ArtPris.MvaKr[1]
"MvaKr[1]" "MvaKr1" ? ? "decimal" ? ? ? ? ? ? yes ? no 10.2 yes ""
     _FldNameList[28]   > skotex.ArtPris.MvaKr[2]
"MvaKr[2]" "MvaKr2" ? ? "decimal" ? ? ? ? ? ? yes ? no 10.2 yes ""
     _FldNameList[29]   > skotex.ArtPris.Pris[1]
"Pris[1]" "Pris1" ? ? "decimal" ? ? ? ? ? ? yes ? no 13.2 yes ""
     _FldNameList[30]   > skotex.ArtPris.Pris[2]
"Pris[2]" "Pris2" ? ? "decimal" ? ? ? ? ? ? yes ? no 13.2 yes ""
     _FldNameList[31]   > skotex.ArtPris.ProfilNr
"ProfilNr" "ProfilNr" ? ? "integer" ? ? ? ? ? ? yes ? no 7.8 yes ""
     _FldNameList[32]   > skotex.ArtPris.Rab1%[1]
"Rab1%[1]" "Rab1%1" ? ? "decimal" ? ? ? ? ? ? yes ? no 9.8 yes ""
     _FldNameList[33]   > skotex.ArtPris.Rab1%[2]
"Rab1%[2]" "Rab1%2" ? ? "decimal" ? ? ? ? ? ? yes ? no 9.8 yes ""
     _FldNameList[34]   > skotex.ArtPris.Rab1Kr[1]
"Rab1Kr[1]" "Rab1Kr1" ? ? "decimal" ? ? ? ? ? ? yes ? no 10.2 yes ""
     _FldNameList[35]   > skotex.ArtPris.Rab1Kr[2]
"Rab1Kr[2]" "Rab1Kr2" ? ? "decimal" ? ? ? ? ? ? yes ? no 10.2 yes ""
     _FldNameList[36]   > skotex.ArtPris.Rab2%[1]
"Rab2%[1]" "Rab2%1" ? ? "decimal" ? ? ? ? ? ? yes ? no 10.2 yes ""
     _FldNameList[37]   > skotex.ArtPris.Rab2%[2]
"Rab2%[2]" "Rab2%2" ? ? "decimal" ? ? ? ? ? ? yes ? no 10.2 yes ""
     _FldNameList[38]   > skotex.ArtPris.Rab2Kr[1]
"Rab2Kr[1]" "Rab2Kr1" ? ? "decimal" ? ? ? ? ? ? yes ? no 10.2 yes ""
     _FldNameList[39]   > skotex.ArtPris.Rab2Kr[2]
"Rab2Kr[2]" "Rab2Kr2" ? ? "decimal" ? ? ? ? ? ? yes ? no 10.2 yes ""
     _FldNameList[40]   > skotex.ArtPris.Rab3%[1]
"Rab3%[1]" "Rab3%1" ? ? "decimal" ? ? ? ? ? ? yes ? no 10.2 yes ""
     _FldNameList[41]   > skotex.ArtPris.Rab3%[2]
"Rab3%[2]" "Rab3%2" ? ? "decimal" ? ? ? ? ? ? yes ? no 10.2 yes ""
     _FldNameList[42]   > skotex.ArtPris.Rab3Kr[1]
"Rab3Kr[1]" "Rab3Kr1" ? ? "decimal" ? ? ? ? ? ? yes ? no 10.2 yes ""
     _FldNameList[43]   > skotex.ArtPris.Rab3Kr[2]
"Rab3Kr[2]" "Rab3Kr2" ? ? "decimal" ? ? ? ? ? ? yes ? no 10.2 yes ""
     _FldNameList[44]   > skotex.ArtPris.RegistrertAv
"RegistrertAv" "RegistrertAv" ? ? "character" ? ? ? ? ? ? yes ? no 10 yes ""
     _FldNameList[45]   > skotex.ArtPris.RegistrertDato
"RegistrertDato" "RegistrertDato" ? ? "date" ? ? ? ? ? ? yes ? no 11.6 yes ""
     _FldNameList[46]   > skotex.ArtPris.RegistrertTid
"RegistrertTid" "RegistrertTid" ? ? "integer" ? ? ? ? ? ? yes ? no 20.8 yes ""
     _FldNameList[47]   > skotex.ArtPris.Tilbud
"Tilbud" "Tilbud" ? ? "logical" ? ? ? ? ? ? yes ? no 5.8 yes ""
     _FldNameList[48]   > skotex.ArtPris.TilbudFraDato
"TilbudFraDato" "TilbudFraDato" ? ? "date" ? ? ? ? ? ? yes ? no 11.6 yes ""
     _FldNameList[49]   > skotex.ArtPris.TilbudFraTid
"TilbudFraTid" "TilbudFraTid" ? ? "integer" ? ? ? ? ? ? yes ? no 18 yes ""
     _FldNameList[50]   > skotex.ArtPris.TilbudTilDato
"TilbudTilDato" "TilbudTilDato" ? ? "date" ? ? ? ? ? ? yes ? no 13 yes ""
     _FldNameList[51]   > skotex.ArtPris.TilbudTilTid
"TilbudTilTid" "TilbudTilTid" ? ? "integer" ? ? ? ? ? ? yes ? no 10.6 yes ""
     _FldNameList[52]   > skotex.ArtPris.TilbudTimeStyrt
"TilbudTimeStyrt" "TilbudTimeStyrt" ? ? "logical" ? ? ? ? ? ? yes ? no 8.8 yes ""
     _FldNameList[53]   > skotex.ArtPris.ValPris[1]
"ValPris[1]" "ValPris1" ? ? "decimal" ? ? ? ? ? ? yes ? no 15.6 yes ""
     _FldNameList[54]   > skotex.ArtPris.ValPris[2]
"ValPris[2]" "ValPris2" ? ? "decimal" ? ? ? ? ? ? yes ? no 15.6 yes ""
     _FldNameList[55]   > skotex.ArtPris.VareKost[1]
"VareKost[1]" "VareKost1" ? ? "decimal" ? ? ? ? ? ? yes ? no 10.2 yes ""
     _FldNameList[56]   > skotex.ArtPris.VareKost[2]
"VareKost[2]" "VareKost2" ? ? "decimal" ? ? ? ? ? ? yes ? no 10.2 yes ""
     _FldNameList[57]   > "_<CALC>"
"RowObject.InnkjopsPris1" "FuInnkjopsPris" "Innkjøpspris" "->,>>>,>>9.99" "Decimal" ? ? ? ? ? ? no ? no 13.2 no ?
     _FldNameList[58]   > "_<CALC>"
"RowObject.Rab1Kr1" "FuRab1Kr" "Rabatt 1 (-)" "->>,>>9.99" "Decimal" ? ? ? ? ? ? no ? no 10.6 no ?
     _FldNameList[59]   > "_<CALC>"
"RowObject.Rab1%1" "FuRab1%" "%Rabatt 1" "->>9.99" "Decimal" ? ? ? ? ? ? no ? no 7 no ?
     _FldNameList[60]   > "_<CALC>"
"RowObject.Rab2Kr1" "FuRab2Kr" "Rabatt 2 (-)" "->>,>>9.99" "Decimal" ? ? ? ? ? ? no ? no 10.6 no ?
     _FldNameList[61]   > "_<CALC>"
"RowObject.Rab2%1" "FuRab2%" "%Rabatt 2" "->>9.99" "Decimal" ? ? ? ? ? ? no ? no 9.8 no ?
     _FldNameList[62]   > "_<CALC>"
"RowObject.Frakt1" "FuFrakt" "Frakt (+)" "->>,>>9.99" "Decimal" ? ? ? ? ? ? no ? no 10.2 no ?
     _FldNameList[63]   > "_<CALC>"
"RowObject.Frakt%1" "FuFrakt%" "Frakt%" "->>,>>9.99" "Decimal" ? ? ? ? ? ? no ? no 10.2 no ?
     _FldNameList[64]   > "_<CALC>"
"RowObject.DivKostKr1" "FuDivKostKr" "Div.kost (+)" "->>,>>9.99" "Decimal" ? ? ? ? ? ? no ? no 10.2 no ?
     _FldNameList[65]   > "_<CALC>"
"RowObject.DivKost%1" "FuDivKost%" "Div.kost%" "->>,>>9.99" "Decimal" ? ? ? ? ? ? no ? no 10.2 no ?
     _FldNameList[66]   > "_<CALC>"
"RowObject.Rab3Kr1" "FuRab3Kr" "Rabatt 3 (-)" "->>,>>9.99" "Decimal" ? ? ? ? ? ? no ? no 10.2 no ?
     _FldNameList[67]   > "_<CALC>"
"RowObject.Rab3%1" "FuRab3%" "%Rabatt 3" "->>9.99" "Decimal" ? ? ? ? ? ? no ? no 9.8 no ?
     _FldNameList[68]   > "_<CALC>"
"RowObject.VareKost1" "FuVareKost" "VareKost" "->>,>>9.99" "Decimal" ? ? ? ? ? ? no ? no 10.2 no ?
     _FldNameList[69]   > "_<CALC>"
"RowObject.DBKr1" "FuDBKr" "Db (+)" "->>,>>9.99" "Decimal" ? ? ? ? ? ? no ? no 10.2 no ?
     _FldNameList[70]   > "_<CALC>"
"RowObject.DB%1" "FuDB%" "DB%" "->>,>>9.99" "Decimal" ? ? ? ? ? ? no ? no 10.2 no ?
     _FldNameList[71]   > "_<CALC>"
"RowObject.MvaKr1" "FuMvaKr" "Mva (+)" "->>,>>9.99" "Decimal" ? ? ? ? ? ? no ? no 10.2 no ?
     _FldNameList[72]   > "_<CALC>"
"RowObject.Mva%1" "FuMva%" "Mva%" "->>,>>9.99" "Decimal" ? ? ? ? ? ? no ? no 10.2 no ?
     _FldNameList[73]   > "_<CALC>"
"RowObject.Pris1" "FuPris" "Pris" "->,>>>,>>9.99" "Decimal" ? ? ? ? ? ? no ? no 13.2 no ?
     _FldNameList[74]   > "_<CALC>"
"RowObject.EuroPris1" "FuEuroPris" "Pris (Euro)" "->,>>>,>>9.99" "Decimal" ? ? ? ? ? ? no ? no 13.2 no ?
     _FldNameList[75]   > "_<CALC>"
"RowObject.ValPris1" "FuValPris" "Valutapris" "->>>,>>>,>>9.99" "Decimal" ? ? ? ? ? ? no ? no 15 no ?
     _FldNameList[76]   > "_<CALC>"
"RowObject.InnkjopsPris2 <> 0 AND RowObject.Pris2 <> 0" "FuHarTilbudsPris" "HarTilbudpris" "J/N" "Logical" ? ? ? ? ? ? no ? no 1.6 no ?
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
         rowObject.FuDB% = (RowObject.DB%1)
         rowObject.FuDBKr = (RowObject.DBKr1)
         rowObject.FuDivKost% = (RowObject.DivKost%1)
         rowObject.FuDivKostKr = (RowObject.DivKostKr1)
         rowObject.FuEuroPris = (RowObject.EuroPris1)
         rowObject.FuFrakt = (RowObject.Frakt1)
         rowObject.FuFrakt% = (RowObject.Frakt%1)
         rowObject.FuHarTilbudsPris = (RowObject.InnkjopsPris2 <> 0 AND RowObject.Pris2 <> 0)
         rowObject.FuInnkjopsPris = (RowObject.InnkjopsPris1)
         rowObject.FuMva% = (RowObject.Mva%1)
         rowObject.FuMvaKr = (RowObject.MvaKr1)
         rowObject.FuPris = (RowObject.Pris1)
         rowObject.FuRab1% = (RowObject.Rab1%1)
         rowObject.FuRab1Kr = (RowObject.Rab1Kr1)
         rowObject.FuRab2% = (RowObject.Rab2%1)
         rowObject.FuRab2Kr = (RowObject.Rab2Kr1)
         rowObject.FuRab3% = (RowObject.Rab3%1)
         rowObject.FuRab3Kr = (RowObject.Rab3Kr1)
         rowObject.FuValPris = (RowObject.ValPris1)
         rowObject.FuVareKost = (RowObject.VareKost1)
      .

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
  DEFINE VARIABLE lSpecial AS LOGICAL    NO-UNDO.
  IF pcRelative = "SPECIAL" THEN
      ASSIGN lSpecial  = TRUE
             pcRelative = "SAME".
  /* Code placed here will execute PRIOR to standard behavior. */

  RUN SUPER( INPUT pcRelative).
  IF NOT lSpecial THEN
      PUBLISH "ArtPrisOk".
  /* Code placed here will execute AFTER standard behavior.    */

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE FindArtPris dTables  _DB-REQUIRED
PROCEDURE FindArtPris :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT  PARAMETER ipArtikkelNr LIKE ArtPris.Artikkelnr NO-UNDO.
   DEFINE INPUT  PARAMETER ipProfilNr AS INTEGER    NO-UNDO.

   DEFINE VARIABLE h_vartpris AS HANDLE     NO-UNDO.

   ASSIGN h_vartpris = WIDGET-HANDLE(DYNAMIC-FUNCTION('getDataTarget':U)).

   IF NOT VALID-HANDLE(h_vartpris) THEN
       RETURN.

   DYNAMIC-FUNCTION('setQueryWhere':U,
      INPUT "ArtPris.ArtikkelNr = '" + STRING(ipArtikkelNr) + "' AND ArtPris.ProfilNr = '"
                                     + STRING(ipProfilNr) + "'" /* CHARACTER */).

    IF DYNAMIC-FUNCTION('openQuery':U) THEN DO:
        FIND ArtPris NO-LOCK WHERE
            ArtPris.ArtikkelNR = ipArtikkelNr AND
            ArtPris.ProfilNr   = ipProfilNr NO-ERROR.
/*         RUN KampanjVerdier IN h_vartpris                                                      */
/*          (INPUT IF RowObject.Tilbud THEN RowObject.Pris2 ELSE RowObject.Pris1 /* DECIMAL */). */
        RUN KampanjVerdier IN h_vartpris
         (INPUT IF ArtPris.Tilbud THEN Artpris.Pris[2] ELSE ArtPris.Pris[1] /* DECIMAL */).
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

