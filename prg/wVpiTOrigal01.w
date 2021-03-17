&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 

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

DEFINE VARIABLE iSortCol   AS INTEGER  NO-UNDO.
DEFINE VARIABLE iSortOrder AS INTEGER  NO-UNDO.

DEFINE VARIABLE iMouseDownRow AS INTEGER    NO-UNDO.
DEFINE VARIABLE cSumWhatSave  AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cColLabelSave AS CHARACTER  NO-UNDO.
DEFINE VARIABLE dCtrlWidth    AS DECIMAL    NO-UNDO.
DEFINE VARIABLE cXSolgt%      AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cTitle        AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cKundenavn    AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cPolygon      AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cExtraInfo    AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cPreSelectCols AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cPrintKunCols AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cPrintNoHiddCols AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cVisSave      AS CHARACTER  NO-UNDO.
DEFINE VARIABLE h_Window      AS HANDLE     NO-UNDO.
DEFINE VARIABLE iCurrPost     AS INTE       NO-UNDO.
DEFINE VARIABLE iCurrRow      AS INTE       NO-UNDO.
                
DEFINE VARIABLE cSekvensFil  AS CHARACTER INIT "Rigalsekvens.txt"  NO-UNDO.

DEF VAR cRigalversion AS CHAR INIT "RIGAL02,8.0" NO-UNDO.
DEF VAR cFlyttDir       AS CHAR  NO-UNDO.
DEF VAR cFlyttKommando  AS CHAR  NO-UNDO.
DEF VAR cFinansProDir   AS CHAR  INIT "E:\LRS\ankommet" NO-UNDO.
/* DEF VAR cFinansProDir   AS CHAR  INIT "C:\home\lindbak\sendes" NO-UNDO. */
DEF VAR cTekst AS CHAR NO-UNDO.
DEF VAR ctmpTxt AS CHAR NO-UNDO.
DEF VAR lFinansPro     AS LOGICAL    NO-UNDO.
DEF VAR piLevNr        AS INT INIT 400 NO-UNDO.
DEF VAR cSalgsEnhListe AS CHAR NO-UNDO.
DEF VAR rStatus        AS CHAR.
DEFINE VARIABLE lNyFil AS LOGICAL     NO-UNDO.
DEFINE TEMP-TABLE TT_Error NO-UNDO
    FIELD RadNr  AS INTE
    FIELD cError AS CHAR FORMAT "x(40)" LABEL "Felinfo"
    INDEX radnr IS PRIMARY UNIQUE radnr.

DEFINE TEMP-TABLE TT_VareOrgInfo NO-UNDO
    FIELD RadNr  AS INTE
    FIELD cTTfelt AS CHAR
    FIELD cFelt  AS CHAR FORMAT "x(40)" LABEL "Fält"
    FIELD cVerdi AS CHAR FORMAT "x(40)" LABEL "Värde"
    INDEX radnr IS PRIMARY radnr.


/* {methodexcel.i} */

DEF VAR chExcelApplication    AS COM-HANDLE.  
DEF VAR chWorkbooks           AS COM-HANDLE.
DEF VAR chWorksheets          AS COM-HANDLE.


DEFINE TEMP-TABLE TT_Prisfil NO-UNDO
    FIELD Radnr           AS INTE FORMAT ">>>>9"  LABEL "Radnr"
    FIELD lNy             AS LOG  FORMAT "J/"     LABEL "Ny"
    FIELD lNyPris         AS LOG  FORMAT "J/"     LABEL "NyPris"
    FIELD cTandem         AS CHAR FORMAT "x(15)"  LABEL "Tandem"
    FIELD cStatus         AS CHAR FORMAT "x(4)"  LABEL "Anm."
    FIELD Artbas_levnr      AS DECI FORMAT ">>>>>>>>>>>9" LABEL "Levnr"
    FIELD Artbas_levkod      AS DECI FORMAT ">>>>>>>>>>>>9" LABEL "Varunr"
    FIELD Strekkode_kode        AS DECI FORMAT ">>>>>>>>>>>>9" LABEL "EAN"
    FIELD Strekkode_bestnr     AS INTE FORMAT ">>>>>>>9"      LABEL "Bestnr"
    FIELD Artbas_beskr  AS CHAR FORMAT "x(40)"         LABEL "Varutext"
    FIELD Artbas_mengde     AS DECI FORMAT ">>,>>9.999"    LABEL "Mängd"
    FIELD Artbas_enhet      AS INTE FORMAT ">9"            LABEL "Enhet"
    FIELD Artbas_Cenhet     AS CHAR FORMAT "x(4)" LABEL  "Text" /* = 1 THEN "st" ELSE IF vare.enhet = 2 THEN "kg" ELSE IF vare.enhet = 3 THEN "l" ELSE "m") */
    FIELD Artbas_hg        AS INTE FORMAT ">>>9" LABEL "Varugr"
    FIELD pris_engrosn    AS DECI FORMAT ">>,>>9.99"   LABEL "Inpris"
    FIELD rabatt          AS DECI FORMAT "9"           LABEL "Rab"
    FIELD pris_utprisn    AS DECI FORMAT ">>,>>9.99"   LABEL "Utpris"
    FIELD dbpris_utprisn  AS DECI FORMAT ">>,>>9.99" 
    FIELD pris_veilpris   AS DECI FORMAT ">>,>>9.99"   LABEL "Riktpris"
    FIELD vare_mva        AS DECI FORMAT ">9.99"       LABEL "Moms%"
    FIELD vare_mvagr      AS INTE FORMAT "9" LABEL "Momsgr" /* konv vid inläsning */
    FIELD vare_antpkn     AS DECI FORMAT ">,>>9.999" LABEL "Förp"
    FIELD Artbas_linkvarenr       AS DECI FORMAT ">>>>>>>>>>>>9" LABEL "Link"
    FIELD fil_tbproc      AS DECI FORMAT "->>9.99" LABEL "TB%"
    FIELD hgrprof_brutto% AS DECI FORMAT "->>9.99" LABEL "VG TB%"
    FIELD orgdata         AS CHAR
    INDEX Radnr IS PRIMARY UNIQUE Radnr.

DEFINE TEMP-TABLE TT_Moms NO-UNDO LIKE moms.

/* {RigalVare.i} */ /* har utförligare beskrivelse */

DEFINE TEMP-TABLE TT_RigalVare NO-UNDO
/* Feltnr     Betegnelse                       Feltinnhold                     Format  Kommentar                                                      */
/* 1   "VAR"     */ FIELD Kode         AS CHAR /* "VAR" */
/* 2   "E"       */ FIELD AID_kode     AS CHAR /* "E" eller "P" */
/* 3   130200004 */ FIELD Nummer       AS DECI /* EAN/PLU-nummer  I(13) */
/* 4   13020000  */ FIELD Artnr        AS DECI /* Internt artikkelnr */
/* 5   "N"       */ FIELD Flag         AS CHAR /* Funksjonskode "N" = "Normal",  vare/prisendring"K" = Kampanje"M" =  Medlemstilbud "U" = Utmelding (tolkes som sletting) "A" =  Slett ikke påbegynt kampanje, avslutt påbegynt kamåpanje     */
/* 6   1         */ FIELD Grossist     AS INTE /* Leverandørnummer I(8) */
/* 7   0         */ FIELD Bestnr       AS INTE /* Lev artnumr I(8)  Bestillingsnummer */
/* 8   2002061825*/ FIELD Fradato      AS DECI /* Dato for prisendring,evt startdato tilbudDato  */
/* 9             */ FIELD Tildato      AS DECI /* Sluttdato tilbudDato */
/* 10  "ORIGINAL */ FIELD Varetek      AS CHAR /* Varetekst C(30) */
/* 11  "ORIGINAL */ FIELD Bongtek      AS CHAR /* Bongtekst C(20) */
/* 12  "ORIGINAL */ FIELD Etitekst1    AS CHAR /* Etikettekst 1 C(30)   Må i fylles ut med kunderettede tekster                      */
/* 13  ""        */ FIELD Etitekst2    AS CHAR /* Etikettekst 2 C(40)   Må i fylles ut med utfyllende kunderettede tekster (se over) */
/* 14  1         */ FIELD Pakn         AS INTE /* Salgsenhet  I 1=stk, 2=kg, 3=liter, 4=meter osv.*/
/* 15  1         */ FIELD Antpkn       AS INTE /* Antall i pakning */
/* 16  0         */ FIELD Storrelse    AS INTE /* Pakningsstørrelse*/
/* 17  6         */ FIELD Avdeling     AS INTE /* Avdeling      */
/* 18  6330      */ FIELD Hgr          AS INTE /* HovedgruppeI  */
/* 19  1         */ FIELD Ugr          AS INTE /* Undergruppe   */
/* 20  47        */ FIELD Engros       AS DECI /* EngrosprisD   */
/* 21  47        */ FIELD Netto        AS DECI /* Nettopris     */
/* 22  102       */ FIELD Utpris       AS DECI /* UtsalgsprisD  */
/* 23  0         */ FIELD Veil         AS DECI /* Veiledende pris */
/* 24  0         */ FIELD Avt_sperre   AS INTE /* Avtalesperre */
/* 25  "0E"      */ FIELD Rabtyp1      AS CHAR /* Rabattype    */
/* 26  "0E"      */ FIELD Rabtyp2      AS CHAR
/* 27  "0E"      */ FIELD Rabtyp3      AS CHAR
/* 28  "0N"      */ FIELD Rabtyp4      AS CHAR
/* 29  "0N"      */ FIELD Rabtyp5      AS CHAR
/* 30  "0N"      */ FIELD Rabtyp6      AS CHAR
/* 31  "0E"      */ FIELD Rabtyp7      AS CHAR
/* 32  "0E"      */ FIELD Rabtyp8      AS CHAR
/* 33  "0E"      */ FIELD Rabtyp9      AS CHAR
/* 34  "0E"      */ FIELD Rabtyp10     AS CHAR
/* 35  0         */ FIELD Rab1         AS INTE
/* 36  0         */ FIELD Rab2         AS INTE
/* 37  0         */ FIELD Rab3         AS INTE
/* 38  0         */ FIELD Rab4         AS INTE
/* 39  0         */ FIELD Rab5         AS INTE
/* 40  0         */ FIELD Rab6         AS INTE
/* 41  0         */ FIELD Rab7         AS INTE
/* 42  0         */ FIELD Rab8         AS INTE
/* 43  0         */ FIELD Rab9         AS INTE
/* 44  0         */ FIELD Rab10        AS INTE
/* 45  24        */ FIELD Mva%         AS DECI
/* 46  1         */ FIELD Bonusg       AS INTE
/* 47  "G"       */ FIELD Sorti        AS CHAR
/* 48  "N"       */ FIELD Vtype        AS CHAR
/* 49  0         */ FIELD Mm-nr        AS INTE
/* 50  1         */ FIELD Etikett      AS INTE
/* 51  0         */ FIELD Lag          AS INTE
/* 52  0         */ FIELD Pall         AS INTE
/* 53  628       */ FIELD Leverandor   AS INTE
/* 54  228       */ FIELD Filial       AS INTE
/* 55  0         */ FIELD Pant         AS DECI
/* 56  0         */ FIELD DUN_Velosity AS INTE
/* 57  ""        */ FIELD AID_kode2    AS CHAR
/* 58  0         */ FIELD Linknr       AS DECI
/* 59  ""        */ FIELD Statflag     AS CHAR
/* 60  1         */ FIELD Produsent    AS INTE
/* 61  ""        */ FIELD AID_kode3    AS CHAR
/* 62  0         */ FIELD Salgskode3   AS DECI
/* 63  ""        */ FIELD AID_kode4    AS CHAR
/* 64  0         */ FIELD Salgskode4   AS DECI
/* 65  ""        */ FIELD Bnrbesk      AS CHAR
/* 66  0         */ FIELD Kampid       AS INTE
/* 67  ""        */ FIELD Kampnavn     AS CHAR
/* 68  0         */ FIELD ENVAnr       AS INTE
/* 69  ""        */ FIELD Selektering1 AS CHAR
/* 70  ""        */ FIELD Selektering2 AS CHAR
/* 71  ""        */ FIELD Selektering3 AS CHAR
/* 72  N         */ FIELD Vardek       AS CHAR
/* 73  J         */ FIELD Laggros      AS CHAR
/* 74  N         */ FIELD Miljokode    AS CHAR
/* 75  N         */ FIELD Genmod       AS CHAR
/* 76  N         */ FIELD Okologisk    AS CHAR
/* 77  0         */ FIELD Erstvar      AS DECI
/* 78  0         */ FIELD Fabrikat     AS INTE
/* 79  0         */ FIELD Konvfak      AS DECI
/* 80  0         */ FIELD Konvpri      AS DECI
/* 81  ""        */ FIELD Lok_i_butikk AS CHAR
/* 82  ""        */ FIELD Planogram    AS CHAR
/* 83  0         */ FIELD Bredde       AS DECI
/* 84  0         */ FIELD Dybde        AS DECI
/* 85  0         */ FIELD Hoyde        AS DECI
/* 86  0         */ FIELD Antbredde    AS INTE
/* 87  0         */ FIELD Antdybde     AS INTE
/* 88  0         */ FIELD Anthoyde     AS INTE
/* 89  N         */ FIELD Anbrekk      AS CHAR
/* 90            */ FIELD Bestilles_fra_datoDato AS DATE
/* 91            */ FIELD Bestilles_til_datoDato AS DATE
/* 92  0         */ FIELD Konvenh      AS  INTE  /* Konverteringsenhet Angir enhet som brukes ifm. enhetspris. Samme koder som i  felt 7.1.14                                                  */
/* 93  0         */ FIELD Garantiklasse AS INTE /* Garantiklassenummer Jfr. "Salg av vare til kunde", pkt. 4                        */
/* 94  0         */ FIELD Idkrav        AS INTE /* I (1)   Angir at det er aldersgrense på salg av varen, jfr. "Varer i butikk", pkt. 5.0=ingen, 1=alkohol, 2=tobakk)                */
/* 95  ""        */ FIELD Bilde        AS CHAR /* Navn på billedfil C(20)   Jfr. "VPI", pkt. 8 */
/* 96  ""        */ FIELD Mersalg      AS CHAR /* EAN/PLUnummer Tilleggsvarer, forslag til mersalg for kasserer adskilt  med; mellom hvert varenr                                     */
/* 97  J         */ FIELD Lokalpris    AS CHAR /* LOGICAL J/N Tillatt å endre pris lokalt i InfoPOS i butikk (frukt&grønt) */
/* 98  0         */ FIELD Holdbarhet   AS INTE           /*  I (2)   Antall dager holdbarhet                                      */
/* 99  0         */ FIELD Taranetto    AS DECI           /* Tara eller netto D       Antall gram tara for vektvarer eller nettovekt for stkvarer  */
/* 100 0         */ FIELD Trykkbilde   AS INTE           /* I (1)   Valg av trykkbilde på ferskvarevekt                          */
/* 101 ""        */ FIELD Etikettenhet AS CHAR           /* C (10)  Tekst for salgsenehet som skal skrives på etikett/plakat     */
/* 102 13020     */ FIELD Modellnr     AS INTE           /* I (8)   Modellnr*/
/* 103 0         */ FIELD Variantnr    AS INTE           /* I (3)   Sortering innen en modell                                    */
/* 104 ""        */ FIELD Farge        AS CHAR           /* Fargetekst                      C(10)   Fargetekst                                                   */
/* 105 ""        */ FIELD Storrelsetekst AS CHAR           /* Størrelsestekst                 C(10)   Størrelsestekst                                              */
/* 106 N         */ FIELD Kunlot       AS CHAR /* LOGICAL Bestilles kun i lot             J/N     Bestilling kun i LOT                                         */
/* 107 ""        */ FIELD FabrikatNavn AS CHAR           /* Fabrikatnavn                    C(30)   Navn på fabrikat - erstatter fabrikatnr (7.1.78)             */
/* 108 ""        */ FIELD Levvnr       AS CHAR           /* Underleverandørs varenr         C(20)   Underleverandørs varenr (dennes bestillingsnr)               */
/* 109 ""        */ FIELD Individtype  AS CHAR           /* Type individhåndtering          C(1)    E=elektro, V=våpen osv.                                      */
/* 110 ""        */ FIELD Artnr2       AS CHAR           /* Alfanumerisk internt artikkelnr C(20)                                                                */
/* 111 0         */ FIELD Matrialkode  AS INTE           /* I2 */
/* 112 0         */ FIELD Sesongkode     AS IN             /* I3 */
INDEX nummer IS PRIMARY nummer
.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME BROWSE-TT_Error

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES TT_Error TT_Prisfil TT_VareOrgInfo

/* Definitions for BROWSE BROWSE-TT_Error                               */
&Scoped-define FIELDS-IN-QUERY-BROWSE-TT_Error tt_Error.Radnr tt_Error.cError   
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-TT_Error   
&Scoped-define SELF-NAME BROWSE-TT_Error
&Scoped-define QUERY-STRING-BROWSE-TT_Error FOR EACH TT_Error
&Scoped-define OPEN-QUERY-BROWSE-TT_Error OPEN QUERY {&SELF-NAME} FOR EACH TT_Error.
&Scoped-define TABLES-IN-QUERY-BROWSE-TT_Error TT_Error
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-TT_Error TT_Error


/* Definitions for BROWSE BROWSE-TT_Prisfil                             */
&Scoped-define FIELDS-IN-QUERY-BROWSE-TT_Prisfil TT_Prisfil.Radnr tt_Prisfil.lNy tt_Prisfil.lNyPris tt_prisfil.ctandem tt_Prisfil.cStatus tt_Prisfil.Artbas_levnr tt_Prisfil.Artbas_levkod tt_Prisfil.Strekkode_kode tt_Prisfil.Strekkode_bestnr tt_Prisfil.Artbas_beskr tt_Prisfil.Artbas_mengde tt_Prisfil.Artbas_enhet tt_Prisfil.Artbas_Cenhet tt_Prisfil.Artbas_hg tt_Prisfil.pris_engrosn /* tt_Prisfil.rabatt */ tt_Prisfil.pris_utprisn tt_Prisfil.fil_tbproc tt_Prisfil.hgrprof_brutto% tt_Prisfil.pris_veilpris tt_Prisfil.vare_mva tt_Prisfil.vare_mvagr tt_Prisfil.vare_antpkn tt_Prisfil.Artbas_linkvarenr   
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-TT_Prisfil   
&Scoped-define SELF-NAME BROWSE-TT_Prisfil
&Scoped-define OPEN-QUERY-BROWSE-TT_Prisfil IF RS-Avvik:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "N" THEN     OPEN QUERY {&SELF-NAME} FOR EACH TT_Prisfil WHERE TT_Prisfil.lNy = TRUE. ELSE IF RS-Avvik:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "P" THEN         OPEN QUERY {&SELF-NAME} FOR EACH TT_Prisfil WHERE TT_Prisfil.lNyPris = TRUE. ELSE     OPEN QUERY {&SELF-NAME} FOR EACH TT_Prisfil WHERE TT_Prisfil.cStatus MATCHES RS-Avvik.
&Scoped-define TABLES-IN-QUERY-BROWSE-TT_Prisfil TT_Prisfil
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-TT_Prisfil TT_Prisfil


/* Definitions for BROWSE BROWSE-TT_VareOrgInfo                         */
&Scoped-define FIELDS-IN-QUERY-BROWSE-TT_VareOrgInfo TT_VareOrgInfo.RadNr TT_VareOrgInfo.cFelt TT_VareOrgInfo.cVerdi   
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-TT_VareOrgInfo   
&Scoped-define SELF-NAME BROWSE-TT_VareOrgInfo
&Scoped-define QUERY-STRING-BROWSE-TT_VareOrgInfo FOR EACH TT_VareOrgInfo WHERE TT_VareOrgInfo.Rad = TT_prisfil.Radnr
&Scoped-define OPEN-QUERY-BROWSE-TT_VareOrgInfo OPEN QUERY {&SELF-NAME} FOR EACH TT_VareOrgInfo WHERE TT_VareOrgInfo.Rad = TT_prisfil.Radnr.
&Scoped-define TABLES-IN-QUERY-BROWSE-TT_VareOrgInfo TT_VareOrgInfo
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-TT_VareOrgInfo TT_VareOrgInfo


/* Definitions for FRAME DEFAULT-FRAME                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME ~
    ~{&OPEN-QUERY-BROWSE-TT_Error}~
    ~{&OPEN-QUERY-BROWSE-TT_Prisfil}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-51 RECT-52 B-Fil B-Refresh BUTTON-2 ~
BROWSE-TT_Error BROWSE-TT_VareOrgInfo RS-Avvik BROWSE-TT_Prisfil ~
FI-FelbrowseTxt FI-AvvikbrowseTxt FI-FilterTxt 
&Scoped-Define DISPLAYED-OBJECTS FI-Rigalnr FI-Filnavn FI-Filnamn RS-Avvik ~
FI-FelbrowseTxt FI-AvvikbrowseTxt FI-FilterTxt 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON B-Fil 
     LABEL "Droppa fil här" 
     SIZE 15 BY 1.14 DROP-TARGET.

DEFINE BUTTON B-Refresh  NO-FOCUS
     LABEL "Uppdatera" 
     SIZE 20 BY 1.14.

DEFINE BUTTON BUTTON-2 
     LABEL "Exportera varufil" 
     SIZE 26 BY 1.14.

DEFINE VARIABLE FI-AvvikbrowseTxt AS CHARACTER FORMAT "X(256)":U INITIAL "Rader med ändringar" 
      VIEW-AS TEXT 
     SIZE 63 BY 1.05
     FONT 6 NO-UNDO.

DEFINE VARIABLE FI-FelbrowseTxt AS CHARACTER FORMAT "X(256)":U INITIAL "Rader med identifierade fel" 
      VIEW-AS TEXT 
     SIZE 42.2 BY 1.05
     FONT 6 NO-UNDO.

DEFINE VARIABLE FI-Filnamn AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 80 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Filnavn AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 60.2 BY 1 NO-UNDO.

DEFINE VARIABLE FI-FilterTxt AS CHARACTER FORMAT "X(256)":U INITIAL "Ändrade/nya rader exporteras" 
      VIEW-AS TEXT 
     SIZE 42 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE FI-Rigalnr AS INTEGER FORMAT "999":U INITIAL 0 
     LABEL "Rigalnr" 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1 NO-UNDO.

DEFINE VARIABLE RS-Avvik AS CHARACTER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Alla", "*",
"Ändringar", "J",
"Nytt pris", "P",
"Nya", "N"
     SIZE 54 BY 1.19 NO-UNDO.

DEFINE RECTANGLE RECT-51
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 230 BY .05.

DEFINE RECTANGLE RECT-52
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 230 BY .05.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-TT_Error FOR 
      TT_Error SCROLLING.

DEFINE QUERY BROWSE-TT_Prisfil FOR 
      TT_Prisfil SCROLLING.

DEFINE QUERY BROWSE-TT_VareOrgInfo FOR 
      TT_VareOrgInfo SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-TT_Error
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-TT_Error C-Win _FREEFORM
  QUERY BROWSE-TT_Error DISPLAY
      tt_Error.Radnr
tt_Error.cError
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 103 BY 8.24 ROW-HEIGHT-CHARS .62 FIT-LAST-COLUMN.

DEFINE BROWSE BROWSE-TT_Prisfil
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-TT_Prisfil C-Win _FREEFORM
  QUERY BROWSE-TT_Prisfil DISPLAY
      TT_Prisfil.Radnr
 tt_Prisfil.lNy
 tt_Prisfil.lNyPris
          tt_prisfil.ctandem
 tt_Prisfil.cStatus
 tt_Prisfil.Artbas_levnr    
 tt_Prisfil.Artbas_levkod    
 tt_Prisfil.Strekkode_kode      
 tt_Prisfil.Strekkode_bestnr   
 tt_Prisfil.Artbas_beskr
 tt_Prisfil.Artbas_mengde   
 tt_Prisfil.Artbas_enhet    
 tt_Prisfil.Artbas_Cenhet   
 tt_Prisfil.Artbas_hg      
 tt_Prisfil.pris_engrosn  
/*  tt_Prisfil.rabatt */
 tt_Prisfil.pris_utprisn  
tt_Prisfil.fil_tbproc
tt_Prisfil.hgrprof_brutto%
 tt_Prisfil.pris_veilpris 
 tt_Prisfil.vare_mva      
 tt_Prisfil.vare_mvagr    
 tt_Prisfil.vare_antpkn
 tt_Prisfil.Artbas_linkvarenr
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 228 BY 23.91 ROW-HEIGHT-CHARS .62 FIT-LAST-COLUMN.

DEFINE BROWSE BROWSE-TT_VareOrgInfo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-TT_VareOrgInfo C-Win _FREEFORM
  QUERY BROWSE-TT_VareOrgInfo DISPLAY
      TT_VareOrgInfo.RadNr 
    TT_VareOrgInfo.cFelt 
    TT_VareOrgInfo.cVerdi
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 91 BY 8.24 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     B-Fil AT ROW 1.43 COL 3.8
     B-Refresh AT ROW 1.48 COL 80.8 NO-TAB-STOP 
     BUTTON-2 AT ROW 1.48 COL 103
     FI-Rigalnr AT ROW 1.48 COL 220 COLON-ALIGNED
     FI-Filnavn AT ROW 1.52 COL 18.4 COLON-ALIGNED NO-LABEL
     FI-Filnamn AT ROW 1.52 COL 129 COLON-ALIGNED NO-LABEL
     BROWSE-TT_Error AT ROW 4.1 COL 3
     BROWSE-TT_VareOrgInfo AT ROW 4.1 COL 109
     RS-Avvik AT ROW 12.57 COL 3 NO-LABEL
     BROWSE-TT_Prisfil AT ROW 13.86 COL 3
     FI-FelbrowseTxt AT ROW 2.81 COL 1.8 COLON-ALIGNED NO-LABEL
     FI-AvvikbrowseTxt AT ROW 2.91 COL 107 COLON-ALIGNED NO-LABEL
     FI-FilterTxt AT ROW 12.91 COL 57.2 COLON-ALIGNED NO-LABEL
     RECT-51 AT ROW 1.24 COL 1.6
     RECT-52 AT ROW 2.76 COL 1.6
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 231 BY 36.95.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Prisfilbehandling"
         HEIGHT             = 36.95
         WIDTH              = 231
         MAX-HEIGHT         = 38.57
         MAX-WIDTH          = 231
         VIRTUAL-HEIGHT     = 38.57
         VIRTUAL-WIDTH      = 231
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */
/* BROWSE-TAB BROWSE-TT_Error FI-Filnamn DEFAULT-FRAME */
/* BROWSE-TAB BROWSE-TT_VareOrgInfo BROWSE-TT_Error DEFAULT-FRAME */
/* BROWSE-TAB BROWSE-TT_Prisfil RS-Avvik DEFAULT-FRAME */
/* SETTINGS FOR FILL-IN FI-Filnamn IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-Filnavn IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-Rigalnr IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-TT_Error
/* Query rebuild information for BROWSE BROWSE-TT_Error
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH TT_Error.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE BROWSE-TT_Error */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-TT_Prisfil
/* Query rebuild information for BROWSE BROWSE-TT_Prisfil
     _START_FREEFORM
IF RS-Avvik:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "N" THEN
    OPEN QUERY {&SELF-NAME} FOR EACH TT_Prisfil WHERE TT_Prisfil.lNy = TRUE.
ELSE IF RS-Avvik:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "P" THEN
        OPEN QUERY {&SELF-NAME} FOR EACH TT_Prisfil WHERE TT_Prisfil.lNyPris = TRUE.
ELSE
    OPEN QUERY {&SELF-NAME} FOR EACH TT_Prisfil WHERE TT_Prisfil.cStatus MATCHES RS-Avvik.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE BROWSE-TT_Prisfil */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-TT_VareOrgInfo
/* Query rebuild information for BROWSE BROWSE-TT_VareOrgInfo
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH TT_VareOrgInfo WHERE TT_VareOrgInfo.Rad = TT_prisfil.Radnr.
     _END_FREEFORM
     _Query            is NOT OPENED
*/  /* BROWSE BROWSE-TT_VareOrgInfo */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Prisfilbehandling */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Prisfilbehandling */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Fil
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Fil C-Win
ON CHOOSE OF B-Fil IN FRAME DEFAULT-FRAME /* Droppa fil här */
DO:
    DEFINE VARIABLE cCSVNavn AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cKonvFil AS CHARACTER   NO-UNDO.
    IF search(FI-Filnavn) <> ? THEN DO:
        SESSION:SET-WAIT-STATE("GENERAL").
        cCSVNavn = SESSION:TEMP-DIR + "tmp.csv".
        cKonvFil = SESSION:TEMP-DIR + "tmp.txt".
        OS-DELETE VALUE(cCSVNavn) NO-ERROR.
        OS-DELETE VALUE(cKonvFil) NO-ERROR.
        iCurrPost = tt_prisfil.radnr NO-ERROR.
        iCurrRow  = BROWSE BROWSE-TT_Prisfil:FOCUSED-ROW NO-ERROR.
        RUN LesEXCEL(FI-Filnavn,cCSVNavn). /* vid uppdatera så skall inte temptabeller tömmas */
        lNyFil = FALSE. 
        IF search(cCsvNavn) <> ? THEN
            RUN importera (cCSVNavn,cKonvFil).
        SESSION:SET-WAIT-STATE("").
    END.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Fil C-Win
ON DROP-FILE-NOTIFY OF B-Fil IN FRAME DEFAULT-FRAME /* Droppa fil här */
DO:
    DEFINE VARIABLE cFilNavn AS CHARACTER   NO-UNDO.
    IF SELF:NUM-DROPPED-FILES = 1 THEN DO:
        ASSIGN cFilNavn = SELF:GET-DROPPED-FILE(1).
        IF NOT CAN-DO("xls,xlsx",ENTRY(NUM-ENTRIES(cFilNavn,"."),cFilNavn,".")) THEN
            MESSAGE "Tillåtna filtyper: '.xls,.xlsx'"
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
        ELSE DO:
            lNyFil = TRUE. /* hantering av temptablar i LesExcel */
            FI-FIlnavn = cFilNavn.
            FI-FIlnavn:SCREEN-VALUE = FI-FIlnavn.
            APPLY "CHOOSE" TO B-Fil.
        END.
    END.
    ELSE DO:
        MESSAGE "Endast 1 fil tillåten!"
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        RETURN NO-APPLY.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Refresh
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Refresh C-Win
ON CHOOSE OF B-Refresh IN FRAME DEFAULT-FRAME /* Uppdatera */
DO:
    APPLY "CHOOSE" TO B-Fil.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-TT_Error
&Scoped-define SELF-NAME BROWSE-TT_Error
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-TT_Error C-Win
ON VALUE-CHANGED OF BROWSE-TT_Error IN FRAME DEFAULT-FRAME
DO:
    FIND TT_Prisfil WHERE TT_Prisfil.Radnr = tt_error.radnr.
    REPOSITION BROWSE-TT_Prisfil TO ROWID ROWID(tt_Prisfil).
    BROWSE BROWSE-TT_Prisfil:DESELECT-FOCUSED-ROW().
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-TT_Prisfil
&Scoped-define SELF-NAME BROWSE-TT_Prisfil
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-TT_Prisfil C-Win
ON ROW-DISPLAY OF BROWSE-TT_Prisfil IN FRAME DEFAULT-FRAME
DO:
/*     TT_Prisfil.Radnr  */
/*     tt_Prisfil.cStatus */
    IF tt_Prisfil.cStatus = "" THEN DO:
        tt_Prisfil.Artbas_levnr:BGCOLOR     IN BROWSE {&BROWSE-NAME} = IF tt_Prisfil.Artbas_levnr = ? THEN 12 ELSE ?.
        tt_Prisfil.Artbas_levkod:BGCOLOR     IN BROWSE {&BROWSE-NAME} = IF tt_Prisfil.Artbas_levkod = ? THEN 12 ELSE ?.
        tt_Prisfil.Strekkode_kode:BGCOLOR       IN BROWSE {&BROWSE-NAME} = IF tt_Prisfil.Strekkode_kode    = ? THEN 12 ELSE ?.   
        tt_Prisfil.Strekkode_bestnr:BGCOLOR    IN BROWSE {&BROWSE-NAME} = IF tt_Prisfil.Strekkode_bestnr  = ? THEN 12 ELSE ?.  
        tt_Prisfil.Artbas_beskr:BGCOLOR IN BROWSE {&BROWSE-NAME} = IF tt_Prisfil.Artbas_beskr = ? THEN 12 ELSE ?.
        tt_Prisfil.Artbas_mengde:BGCOLOR    IN BROWSE {&BROWSE-NAME} = IF tt_Prisfil.Artbas_mengde  = ? THEN 12 ELSE ?.
        tt_Prisfil.Artbas_enhet:BGCOLOR     IN BROWSE {&BROWSE-NAME} = IF tt_Prisfil.Artbas_enhet   = ? THEN 12 ELSE ?. 
        tt_Prisfil.Artbas_Cenhet:BGCOLOR    IN BROWSE {&BROWSE-NAME} = IF tt_Prisfil.Artbas_Cenhet  = ? THEN 12 ELSE ?. 
        tt_Prisfil.Artbas_hg:BGCOLOR       IN BROWSE {&BROWSE-NAME} = IF tt_Prisfil.Artbas_hg     = ? THEN 12 ELSE ?.
        tt_Prisfil.pris_engrosn:BGCOLOR   IN BROWSE {&BROWSE-NAME} = IF tt_Prisfil.pris_engrosn = ? OR 
                                                                        tt_Prisfil.pris_engrosn = 0 THEN 12 ELSE ?. 
/*         tt_Prisfil.rabatt:BGCOLOR         IN BROWSE {&BROWSE-NAME} = IF tt_Prisfil.rabatt       = ? THEN 12 ELSE ?. */
        tt_Prisfil.pris_utprisn:BGCOLOR   IN BROWSE {&BROWSE-NAME} = IF tt_Prisfil.pris_utprisn = ? THEN 12 ELSE ?. 
        tt_Prisfil.pris_veilpris:BGCOLOR  IN BROWSE {&BROWSE-NAME} = IF tt_Prisfil.pris_veilpris = ? THEN 12 ELSE ?.
        tt_Prisfil.vare_mva:BGCOLOR       IN BROWSE {&BROWSE-NAME} = IF tt_Prisfil.vare_mva       = ? THEN 12 ELSE ?.
        tt_Prisfil.vare_mvagr:BGCOLOR     IN BROWSE {&BROWSE-NAME} = IF tt_Prisfil.vare_mvagr     = ? THEN 12 ELSE ?.
        tt_Prisfil.vare_antpkn:BGCOLOR    IN BROWSE {&BROWSE-NAME} = IF tt_Prisfil.vare_antpkn   = ? THEN 12 ELSE ?. 
        tt_Prisfil.fil_tbproc:BGCOLOR    IN BROWSE {&BROWSE-NAME}  = IF tt_Prisfil.fil_tbproc - tt_Prisfil.hgrprof_brutto% >= 20 THEN 11 ELSE 
                                                                     IF tt_Prisfil.hgrprof_brutto% - tt_Prisfil.fil_tbproc >= 10 THEN 14 ELSE ?. 
    END.
    ELSE DO:
        tt_Prisfil.lNy:BGCOLOR     IN BROWSE {&BROWSE-NAME} = IF CAN-FIND(TT_VareOrgInfo WHERE TT_VareOrgInfo.Rad = tt_prisfil.Radnr AND TT_VareOrgInfo.cTTfelt = "ny_vara") THEN 10 ELSE ?.
        tt_Prisfil.cTandem:BGCOLOR     IN BROWSE {&BROWSE-NAME} = IF CAN-FIND(TT_VareOrgInfo WHERE TT_VareOrgInfo.Rad = tt_prisfil.Radnr AND TT_VareOrgInfo.cTTfelt = "ny_tandem") THEN 10 ELSE ?.
        tt_Prisfil.Artbas_levnr:BGCOLOR     IN BROWSE {&BROWSE-NAME} = IF CAN-FIND(TT_VareOrgInfo WHERE TT_VareOrgInfo.Rad = tt_prisfil.Radnr AND TT_VareOrgInfo.cTTfelt = "Artbas_levnr") THEN 10 ELSE ?.
        tt_Prisfil.Artbas_levkod:BGCOLOR     IN BROWSE {&BROWSE-NAME} = IF CAN-FIND(TT_VareOrgInfo WHERE TT_VareOrgInfo.Rad = tt_prisfil.Radnr AND TT_VareOrgInfo.cTTfelt = "Artbas_levkod") THEN 10 ELSE ?.
        tt_Prisfil.Strekkode_kode:BGCOLOR       IN BROWSE {&BROWSE-NAME} = IF CAN-FIND(TT_VareOrgInfo WHERE TT_VareOrgInfo.Rad = tt_prisfil.Radnr AND TT_VareOrgInfo.cTTfelt = "Strekkode_kode") THEN 10 ELSE ?.   
        tt_Prisfil.Strekkode_bestnr:BGCOLOR    IN BROWSE {&BROWSE-NAME} = IF CAN-FIND(TT_VareOrgInfo WHERE TT_VareOrgInfo.Rad = tt_prisfil.Radnr AND TT_VareOrgInfo.cTTfelt = "Strekkode_bestnr") THEN 10 ELSE ?.  
        tt_Prisfil.Artbas_beskr:BGCOLOR IN BROWSE {&BROWSE-NAME} = IF CAN-FIND(TT_VareOrgInfo WHERE TT_VareOrgInfo.Rad = tt_prisfil.Radnr AND TT_VareOrgInfo.cTTfelt = "Artbas_beskr") THEN 10 ELSE ?.
        tt_Prisfil.Artbas_mengde:BGCOLOR    IN BROWSE {&BROWSE-NAME} = IF CAN-FIND(TT_VareOrgInfo WHERE TT_VareOrgInfo.Rad = tt_prisfil.Radnr AND TT_VareOrgInfo.cTTfelt = "Artbas_mengde") THEN 10 ELSE ?.
        tt_Prisfil.Artbas_enhet:BGCOLOR     IN BROWSE {&BROWSE-NAME} = IF CAN-FIND(TT_VareOrgInfo WHERE TT_VareOrgInfo.Rad = tt_prisfil.Radnr AND TT_VareOrgInfo.cTTfelt = "Artbas_enhet") THEN 10 ELSE ?. 
        tt_Prisfil.Artbas_Cenhet:BGCOLOR    IN BROWSE {&BROWSE-NAME} = IF CAN-FIND(TT_VareOrgInfo WHERE TT_VareOrgInfo.Rad = tt_prisfil.Radnr AND TT_VareOrgInfo.cTTfelt = "Artbas_enhet") THEN 10 ELSE ?. 
        tt_Prisfil.Artbas_hg:BGCOLOR       IN BROWSE {&BROWSE-NAME} = IF CAN-FIND(TT_VareOrgInfo WHERE TT_VareOrgInfo.Rad = tt_prisfil.Radnr AND TT_VareOrgInfo.cTTfelt = "Artbas_hg") THEN 10 ELSE ?.
        tt_Prisfil.pris_engrosn:BGCOLOR   IN BROWSE {&BROWSE-NAME} = IF CAN-FIND(TT_VareOrgInfo WHERE TT_VareOrgInfo.Rad = tt_prisfil.Radnr AND TT_VareOrgInfo.cTTfelt = "pris_engrosn") THEN 10 ELSE ?. 
/*         tt_Prisfil.rabatt:BGCOLOR         IN BROWSE {&BROWSE-NAME} = IF tt_Prisfil.rabatt       = ? THEN 12 ELSE ?. */
        tt_Prisfil.pris_utprisn:BGCOLOR   IN BROWSE {&BROWSE-NAME} = IF CAN-FIND(TT_VareOrgInfo WHERE TT_VareOrgInfo.Rad = tt_prisfil.Radnr AND TT_VareOrgInfo.cTTfelt = "pris_utprisn") THEN 10 ELSE ?. 
        tt_Prisfil.pris_veilpris:BGCOLOR  IN BROWSE {&BROWSE-NAME} = IF CAN-FIND(TT_VareOrgInfo WHERE TT_VareOrgInfo.Rad = tt_prisfil.Radnr AND TT_VareOrgInfo.cTTfelt = "pris_veilpris") THEN 10 ELSE ?.
        tt_Prisfil.vare_mva:BGCOLOR       IN BROWSE {&BROWSE-NAME} = IF CAN-FIND(TT_VareOrgInfo WHERE TT_VareOrgInfo.Rad = tt_prisfil.Radnr AND TT_VareOrgInfo.cTTfelt = "vare_mvagr") THEN 10 ELSE ?.
        tt_Prisfil.vare_mvagr:BGCOLOR     IN BROWSE {&BROWSE-NAME} = IF CAN-FIND(TT_VareOrgInfo WHERE TT_VareOrgInfo.Rad = tt_prisfil.Radnr AND TT_VareOrgInfo.cTTfelt = "vare_mvagr") THEN 10 ELSE ?.
        tt_Prisfil.vare_antpkn:BGCOLOR    IN BROWSE {&BROWSE-NAME} = IF CAN-FIND(TT_VareOrgInfo WHERE TT_VareOrgInfo.Rad = tt_prisfil.Radnr AND TT_VareOrgInfo.cTTfelt = "vare_antpkn") THEN 10 ELSE ?. 
        tt_Prisfil.fil_tbproc:BGCOLOR    IN BROWSE {&BROWSE-NAME}  = IF tt_Prisfil.fil_tbproc - tt_Prisfil.hgrprof_brutto% >= 20 THEN 11 ELSE
                                                                     IF tt_Prisfil.hgrprof_brutto% - tt_Prisfil.fil_tbproc >= 10 THEN 14 ELSE ?.

    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-TT_Prisfil C-Win
ON VALUE-CHANGED OF BROWSE-TT_Prisfil IN FRAME DEFAULT-FRAME
DO:
  {&OPEN-QUERY-BROWSE-TT_VareOrgInfo}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-2 C-Win
ON CHOOSE OF BUTTON-2 IN FRAME DEFAULT-FRAME /* Exportera varufil */
DO:
    RUN ExportRigal.
    CLOSE QUERY BROWSE-TT_Error. 
    CLOSE QUERY BROWSE-TT_Prisfil. 
    CLOSE QUERY BROWSE-TT_VareOrgInfo. 
    EMPTY TEMP-TABLE TT_Prisfil.
    EMPTY TEMP-TABLE TT_Error.
    EMPTY TEMP-TABLE TT_VareOrgInfo.
    FI-Filnavn = "".
    FI-Filnavn:SCREEN-VALUE = "".
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RS-Avvik
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RS-Avvik C-Win
ON VALUE-CHANGED OF RS-Avvik IN FRAME DEFAULT-FRAME
DO:
    ASSIGN RS-Avvik.
  {&OPEN-QUERY-BROWSE-TT_Prisfil}
      IF BROWSE BROWSE-TT_Prisfil:FOCUSED-ROW <> ? THEN
          APPLY "VALUE-CHANGED" TO BROWSE-TT_Prisfil.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-TT_Error
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE DO:
    RUN disable_UI.
    QUIT.
END.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
    FIND FIRST Butikk NO-LOCK.
    FI-Rigalnr = butikk.rigalnr.
    RUN initTTMoms.
/*                                                                  */
/* /*     {syspara.i 2 4 10 cSalgsEnhListe} */                      */
/*     /* Programmet förutsätter att vi lägger ut till finanspro */ */
/*     ASSIGN cTekst = "".                                          */
/* /*     {syspara.i 200 2 100 cTekst} */                           */
/*     IF CAN-DO("1,yes,true,ja",cTekst) THEN                       */
/*         lFinansPro = TRUE.                                       */
/*     ELSE                                                         */
/*         lFinansPro = FALSE.                                      */
/*     IF lFinansPro = TRUE THEN DO:                                */
/*         {syspar2.i 200 2 100 cFinansProDir}                      */
/*         /* Skall flyttning av filer ske */                       */
/*         ASSIGN cTekst = "".                                      */
/*         {syspara.i 200 2 102 cTekst}                             */
/*         IF CAN-DO("1,yes,true,ja",cTekst) THEN DO:               */
/*             /* Hämta kommando */                                 */
/*             {syspar2.i 200 2 102 cTmpTxt}                        */
/*             IF NUM-ENTRIES(cTmpTxt,";") = 2 THEN DO:             */
/*                 ASSIGN cFlyttDir      = ENTRY(1,cTmpTxt,";")     */
/*                        cFlyttKommando = ENTRY(2,cTmpTxt,";").    */
/*                 IF SEARCH(cFlyttKommando) = ? THEN               */
/*                     ASSIGN cFlyttDir      = ""                   */
/*                            cFlyttKommando = "".                  */
/*             END.                                                 */
/*         END.                                                     */
/*     END.                                                         */
/*                                                                  */

  IF SEARCH(cFinansProDir + "\" + cSekvensfil) = ? THEN DO:
      OUTPUT TO VALUE(cFinansProDir + "\" + cSekvensfil).
      PUT UNFORMATTED "0" SKIP.
      OUTPUT CLOSE.
  END.
  RUN enable_UI.
  ASSIGN RS-Avvik.
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI C-Win  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Delete the WINDOW we created */
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
  THEN DELETE WIDGET C-Win.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI C-Win  _DEFAULT-ENABLE
PROCEDURE enable_UI :
/*------------------------------------------------------------------------------
  Purpose:     ENABLE the User Interface
  Parameters:  <none>
  Notes:       Here we display/view/enable the widgets in the
               user-interface.  In addition, OPEN all queries
               associated with each FRAME and BROWSE.
               These statements here are based on the "Other 
               Settings" section of the widget Property Sheets.
------------------------------------------------------------------------------*/
  DISPLAY FI-Rigalnr FI-Filnavn FI-Filnamn RS-Avvik FI-FelbrowseTxt 
          FI-AvvikbrowseTxt FI-FilterTxt 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE RECT-51 RECT-52 B-Fil B-Refresh BUTTON-2 BROWSE-TT_Error 
         BROWSE-TT_VareOrgInfo RS-Avvik BROWSE-TT_Prisfil FI-FelbrowseTxt 
         FI-AvvikbrowseTxt FI-FilterTxt 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ExportRigal C-Win 
PROCEDURE ExportRigal :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cRigalEntries AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cRigalstr     AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cUtfil        AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cFilnamn      AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE iSekvens      AS INTEGER     NO-UNDO.
    DEFINE VARIABLE cStr          AS CHARACTER   NO-UNDO.
    INPUT FROM VALUE(cFinansProDir + "\" + cSekvensfil).
    IMPORT UNFORMATTED cStr.
    INPUT CLOSE.
    iSekvens = INT(cStr) + 1.
    OUTPUT TO VALUE(cFinansProDir + "\" + cSekvensfil).
    PUT UNFORMATTED iSekvens SKIP.
    OUTPUT CLOSE.

/*     ASSIGN cFilnamn = "v" + SUBSTR(STRING(YEAR(TODAY)),3) + STRING(MONTH(TODAY),"99") + STRING(DAY(TODAY),"99") + STRING(TIME,"99999") + "1." + string(FI-Rigalnr,"999") */
    ASSIGN cFilnamn = "v" + STRING(iSekvens,"999999") + "0." + string(FI-Rigalnr,"999")
           cUtfil = RIGHT-TRIM(TRIM(cFinansProDir),"\") + "\" + cFilnamn
           FI-Filnamn:SCREEN-VALUE IN FRAME {&FRAME-NAME} = cUtfil.
     
/*     MESSAGE cFinansProDir SKIP cUtfil      */
/*         VIEW-AS ALERT-BOX INFO BUTTONS OK. */
    ASSIGN cRigalEntries = FILL(",",109)
           ENTRY(1,cRigalEntries) = "VAR"
           ENTRY(2,cRigalEntries) = "E".
           ENTRY(5,cRigalEntries) = "N".
    OUTPUT TO VALUE(cUtfil).
    PUT UNFORMATTED cRigalversion SKIP.
    FOR EACH TT_Prisfil: /*  Om NY så använd tandem som origvare  */
        IF RS-Avvik:SCREEN-VALUE = "N" AND TT_Prisfil.lNy = FALSE THEN
            NEXT.
        IF RS-Avvik:SCREEN-VALUE = "J" AND TT_Prisfil.lNy = TRUE THEN
            NEXT.
        IF RS-Avvik:SCREEN-VALUE = "P" AND TT_Prisfil.lNyPris = FALSE THEN
            NEXT.
        IF TT_Prisfil.lNy = FALSE AND NOT CAN-FIND(FIRST TT_VareOrgInfo OF tt_prisfil) THEN
            NEXT.
        ASSIGN cRigalstr = cRigalEntries
               ENTRY(3,cRigalstr)   = STRING(TT_Prisfil.Strekkode_kode) /* tänk på tandem "40000" + TT_Input.Artnr  */
               ENTRY(4,cRigalstr)   = STRING(TT_Prisfil.Artbas_levkod)
               ENTRY(6,cRigalstr)   = STRING(TT_Prisfil.Artbas_levnr) /* STRING(piLevNr) */
/* finns char */ ENTRY(7,cRigalstr)   = STRING(TT_Prisfil.Strekkode_bestnr) /* TT_Input.Artnr */
               ENTRY(10,cRigalstr)  = '"' + TRIM(SUBSTR(TT_Prisfil.Artbas_beskr,1,30)) + '"'
               ENTRY(11,cRigalstr)  = '"' + TRIM(SUBSTR(TT_Prisfil.Artbas_beskr,1,20)) + '"'
               ENTRY(12,cRigalstr)  = '"' + TRIM(SUBSTR(TT_Prisfil.Artbas_beskr,1,30)) + '"'
               ENTRY(14,cRigalstr)  = "1" /* STRING(TT_Prisfil.Artbas_enhet) */ /* "1" */
               ENTRY(15,cRigalstr)  = STRING(TT_Prisfil.vare_antpkn,">>>9")  /* "1" */
               ENTRY(18,cRigalstr)  = STRING(TT_Prisfil.Artbas_hg) /* TT_Input.Varuklasskod */
               ENTRY(19,cRigalstr)  = "1"
               ENTRY(20,cRigalstr)  = REPLACE(STRING(TT_Prisfil.pris_engrosn),",",".")  /* TT_Input.Inpris */
               ENTRY(22,cRigalstr)  = REPLACE(STRING(TT_Prisfil.pris_utprisn),",",".")
               ENTRY(45,cRigalstr)  = REPLACE(STRING(TT_Prisfil.vare_mva),",",".")
               ENTRY(48,cRigalstr)  = "N" /* Varetype  "N" = vanlig vare "K" = vektvare (veies i kassen)"O" = åpen   pris "I" = Ikke pris i kassen */
/* linknr */   ENTRY(58,cRigalstr)  = IF DECI(TT_Prisfil.Artbas_linkvarenr) > 0 THEN STRING(TT_Prisfil.Artbas_linkvarenr) ELSE ""
/* tandem */   ENTRY(61,cRigalstr)  = IF DECI(TT_Prisfil.cTandem) > 0 THEN "E" ELSE ""
               ENTRY(62,cRigalstr)  = IF DECI(TT_Prisfil.cTandem) > 0 THEN TT_Prisfil.cTandem ELSE ""
               ENTRY(79,cRigalstr)  = IF TT_Prisfil.Artbas_mengde = 1 THEN "1" ELSE REPLACE(STRING(TT_Prisfil.Artbas_mengde),",",".")
               ENTRY(92,cRigalstr)  = STRING(TT_Prisfil.Artbas_enhet)
/* ???            ENTRY(110,cRigalstr) = TT_Input.Levart. */
                   .
        PUT UNFORMATTED cRigalStr SKIP.
    END.
    OUTPUT CLOSE.
    IF cFlyttkommando <> "" THEN
        RUN flyttprofiler.p ("cFlyttDir","cFlyttKommando",cUtfil) NO-ERROR.


/*                                                                                                                                                                                                                                                                                                                                */
/* /*   1 */ TT_RigalVare.Kode       = "VAR"                                                                                                                                                                                                                                                                                      */
/* /*   2 */ TT_RigalVare.AID_kode   = IF LENGTH(cNummer) < 6 THEN "P" ELSE "E"     /* "E" eller "P" Neste felt EAN eller PLU */                                                                                                                                                                                                  */
/* /*   3 */ TT_RigalVare.Nummer     = DECI(cNummer) /* EAN/PLU-nummer  I(13) */                                                                                                                                                                                                                                                  */
/* /*   4 */ TT_RigalVare.Artnr      = ArtBas.ArtikkelNr /* ??? Internt artikkelnr */                                                                                                                                                                                                                                             */
/* /*   5 */ TT_RigalVare.Flag       = /* IF NOT ArtBas.Aktivert OR NOT Artbas.IKasse THEN "U" ELSE */ "N" /* Funksjonskode "N" = "Normal", dvs.  vare/prisendring"K" = Kampanje"M" =    Medlemstilbud "U" = Utmelding (tolkes som sletting)"A" =  Slett ikke påbegynt kampanje, avslutt påbegynt kamåpanje */ */
/* /*   6 */ TT_RigalVare.Grossist   = ArtBas.LevNr         /* ?? */                                                                                                                                                                                                                                                              */
/* /*   7 */ TT_RigalVare.Bestnr     = 0                    /* Leverandørens artikkelnummer I(8) Bestillingsnummer */                                                                                                                                                                                                       */
/* /*  10 */ TT_RigalVare.Varetek    = SUBSTR(ArtBas.Bongtekst,1,30) /* Varetekst C(30) */                                                                                                                                                                                                                                        */
/* /*  11 */ TT_RigalVare.Bongtek    = SUBSTR(ArtBas.Bongtekst,1,20) /* Bongtekst C(20) */                                                                                                                                                                                                                                        */
/* /*  12 */ TT_RigalVare.Etitekst1  = SUBSTR(ArtBas.Bongtekst,1,30) /* Etikettekst 1 C(30) Må i fylles ut med kunderettede tekster */                                                                                                                                                                                            */
/* /*  14 */ TT_RigalVare.Pakn       = IF ArtBas.SalgsEnhet = "Kg" THEN 2 ELSE 1 /* Salgsenhet I     1=stk, 2=kg, 3=liter, 4=meter osv.                           */                                                                                                                                         */
/* /*  18 */ TT_RigalVare.Hgr        = ArtBas.Vg            /* HovedgruppeI      */                                                                                                                                                                                                                                               */
/* /*  19 */ TT_RigalVare.Ugr        = 1                    /* 0/1 Undergruppe       */                                                                                                                                                                                                                                           */
/* /*  20 */ TT_RigalVare.Engros     = ArtPris.VareKost[iPrisEntry] /* ? */  /*   47 EngrosprisD  */                                                                                                                                                                                                                              */
/* /*  22 */ TT_RigalVare.Utpris     = ArtPris.Pris[iPrisEntry]     /* ? */  /*  102 UtsalgsprisD */                                                                                                                                                                                                                              */
/* /*  45 */ TT_RigalVare.Mva%       = Moms.MomsProc        /* Mva-prosent D */                                                                                                                                                                                                                                                   */
/* /*  48 */ TT_RigalVare.Vtype      = IF ArtBas.OPris THEN "O" ELSE IF ArtBas.Salgsenhet = "Kg" THEN "K" ELSE "N" /* IF NOT ArtBas.Aktivert THEN "I" ELSE "N" = vanlig vare"K" = vektvare (veies i kassen)"O" = åpen   pris "I" = Ikke pris i kassen */                                                                          */
/* /*  61  */ FIELD AID_kode3    AS CHARACTER /* "E" eller "P"                           Angir om neste felt er EAN eller PLU                         */ */
/* /*  62  */ FIELD Salgskode3   AS DECIMAL   /* EAN/PLU-nummer                  I(13)   Alternativt varenummer (tandem)                              */ */
/* /*  79 */ TT_RigalVare.Konvfak      = IF ArtBas.Mengde = 0 THEN 1 ELSE ArtBas.Mengde      /* Konverteringsfaktor             D       Brukes ved beregning av enhetspris      */                                                                                                                                                */
/* /*  92 */ TT_RigalVare.Konvenh       = 0                 /* Konverteringsenhet              I       Angir enhet som brukes ifm. enhetspris. Samme koder som i  felt 7.1.14                                     */                                                                                                              */
/* /* 110 */ TT_RigalVare.Artnr2         = ""               /* Alfanumerisk internt artikkelnr C(20) */                                                                                                                                                                                                                           */
/*                                                                                                                                                                                                                                                                                                                                */


/* 
DEFINE TEMP-TABLE TT_Input NO-UNDO
    FIELD Anbnr        AS CHAR
    FIELD Vgp          AS CHAR
    FIELD Artnr        AS CHAR
    FIELD Chk          AS CHAR
    FIELD Lev          AS CHAR
    FIELD Levart       AS CHAR
    FIELD Benamning    AS CHAR
    FIELD Frp          AS CHAR
    FIELD Ean          AS CHAR
    FIELD Varuklasskod AS CHAR
    FIELD AAFpris      AS CHAR
    FIELD Utpris       AS CHAR
    FIELD Dummy1       AS CHAR
    FIELD Dummy2       AS CHAR
    FIELD Dummy3       AS CHAR
    FIELD Dummy4       AS CHAR
    FIELD Dummy5       AS CHAR
    FIELD Moms%        AS CHAR
    FIELD Inpris       AS CHAR.
 
 */




END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Importera C-Win 
PROCEDURE Importera :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEF INPUT  PARAMETER cFilnamn AS CHARACTER   NO-UNDO.
   DEF INPUT  PARAMETER cKonvnamn AS CHARACTER   NO-UNDO.
   DEF VAR cStr AS CHARACTER   NO-UNDO.
   DEF VAR ii AS INTEGER     NO-UNDO.
   DEF VAR cTmp AS CHARACTER   NO-UNDO.
   DEF VAR dTst AS DECIMAL     NO-UNDO.
   DEF VAR iAntRows AS INTEGER     NO-UNDO.
   DEF VAR cEntryList  AS CHARACTER   NO-UNDO.
   DEF VAR cError  AS CHARACTER   NO-UNDO.
   DEF VAR cErrorFelt AS CHARACTER   NO-UNDO.
   DEF VAR dArtbas_levnr     AS DECI NO-UNDO.
   DEF VAR dArtbas_levkod     AS DECI NO-UNDO.
   DEF VAR dStrekkode_kode       AS DECI NO-UNDO.
   DEF VAR iStrekkode_bestnr    AS INTE NO-UNDO.
   DEF VAR cArtbas_beskr AS CHAR NO-UNDO.
   DEF VAR dArtbas_mengde    AS DECI NO-UNDO.
   DEF VAR iArtbas_enhet     AS INTE NO-UNDO.
   DEF VAR cArtbas_Cenhet    AS CHAR NO-UNDO.
   DEF VAR iArtbas_hg       AS INTE NO-UNDO.
   DEF VAR dpris_engrosn   AS DECI NO-UNDO.
   DEF VAR drabatt         AS DECI NO-UNDO.
   DEF VAR dpris_utprisn   AS DECI NO-UNDO.
   DEF VAR dpris_veilpris  AS DECI NO-UNDO.
   DEF VAR dvare_mva       AS DECI NO-UNDO.
   DEF VAR ivare_mvagr     AS INTE NO-UNDO.
   DEF VAR dvare_antpkn    AS DECI NO-UNDO.
   DEF VAR iRad            AS INTE NO-UNDO.
   DEF VAR cColNamn        AS CHAR NO-UNDO.
   DEF VAR dInExMva        AS DECI NO-UNDO.
   DEF VAR dOrigVareEAN    AS DECI NO-UNDO.
   DEF VAR dArtbas_linkvarenr      AS DECI NO-UNDO.
   DEF VAR dTandemEAN      AS DECI NO-UNDO.
   DEF VAR dUtNetto AS DECI NO-UNDO.
   DEF VAR dTBkr    AS DECI NO-UNDO.
   DEF VAR dOren    AS DECI NO-UNDO.
   DEF VAR dhgrprof_brutto% AS DECI NO-UNDO.
   PROCESS EVENTS.

   cColNamn = "Levnr,Varunr,EAN,Bestnr,Beskr,Strl,Enhet,Varugr,Inpris,Rabatt,Utpris,Rekpris,Moms,Förp,Link".
   cEntryList = FILL(",",NUM-ENTRIES(cColNamn) - 1).
   INPUT FROM VALUE(cFilnamn).
   IMPORT UNFORMATTED cStr.
   IF NUM-ENTRIES(cStr,CHR(9)) <> NUM-ENTRIES(cColnamn) THEN DO:
       MESSAGE "Fel antal fält (" + STRING(NUM-ENTRIES(cStr,CHR(9))) + ")" SKIP
               "Skall vara " + STRING(NUM-ENTRIES(cColnamn))
           VIEW-AS ALERT-BOX ERROR BUTTONS OK.
       INPUT CLOSE.
       RETURN.
   END.
   iRad = iRad + 1.
   LESRAD:
   REPEAT:
       IMPORT UNFORMATTED cStr.
       iRad = iRad + 1.
       cStr = TRIM(cStr).
       IF cStr = "" THEN
           NEXT.
       FIND tt_prisfil WHERE tt_prisfil.radnr = iRad NO-ERROR.
       IF AVAIL tt_prisfil AND tt_prisfil.orgdata = cstr THEN
           NEXT.
       ELSE IF AVAIL tt_prisfil THEN
           DELETE tt_prisfil.
       FOR EACH TT_VareOrgInfo WHERE TT_VareOrgInfo.radnr = iRad:
           DELETE TT_VareOrgInfo.
       END.
       FOR EACH tt_Error WHERE tt_error.radnr = iRad:
           DELETE tt_error.
       END.
       cError = cEntryList.
/*        DO ii = 1 TO MIN(NUM-ENTRIES(cStr,CHR(9)),NUM-ENTRIES(cColnamn)): */
       DO ii = 1 TO NUM-ENTRIES(cColnamn):
         CASE ii:
           WHEN 1 THEN DO:
               dArtbas_levnr = DECI(ENTRY(ii,cStr,CHR(9))) NO-ERROR.
               IF ERROR-STATUS:ERROR THEN
                   ASSIGN dArtbas_levnr = ?
                          ENTRY(ii,cError) = ENTRY(ii,cColNamn).
/* !!!! Detta skall testas om vi har ny vara annars hämtar vi från befintlig */
/*                ELSE DO:                                                     */
/*                    FIND lev WHERE lev.levnr = dArtbas_levnr NO-LOCK NO-ERROR. */
/*                    IF NOT AVAIL lev THEN                                    */
/*                        ASSIGN dArtbas_levnr = ?                               */
/*                               ENTRY(ii,cError) = ENTRY(ii,cColNamn).        */
/*                END.                                                         */
           END.
           WHEN 2 THEN DO:
               dArtbas_levkod = DECI(ENTRY(ii,cStr,CHR(9))) NO-ERROR.
               IF ERROR-STATUS:ERROR THEN
                   ASSIGN dArtbas_levkod = ?
                          ENTRY(ii,cError) = ENTRY(ii,cColNamn).
           END.
           WHEN 3 THEN DO:
               dStrekkode_kode   = DECI(ENTRY(ii,cStr,CHR(9))) NO-ERROR.
               IF ERROR-STATUS:ERROR THEN
                   ASSIGN dStrekkode_kode   = ?
                          ENTRY(ii,cError) = ENTRY(ii,cColNamn).
           END.
           WHEN 4 THEN DO:
               iStrekkode_bestnr   = INT(ENTRY(ii,cStr,CHR(9))) NO-ERROR.
               IF ERROR-STATUS:ERROR THEN
                   ASSIGN iStrekkode_bestnr   = ?
                          ENTRY(ii,cError) = ENTRY(ii,cColNamn).
           END.
           WHEN 5 THEN DO:
               cArtbas_beskr = TRIM(TRIM(ENTRY(ii,cStr,CHR(9)),'"')) NO-ERROR.
               IF ERROR-STATUS:ERROR THEN
                   ASSIGN cArtbas_beskr = ?
                          ENTRY(ii,cError) = ENTRY(ii,cColNamn).
           END.
           WHEN 6 THEN DO:
               dArtbas_mengde = DECI(REPLACE(ENTRY(ii,cStr,CHR(9)),".",",")) NO-ERROR.
               IF ERROR-STATUS:ERROR THEN
                   ASSIGN dArtbas_mengde = ?
                          ENTRY(ii,cError) = ENTRY(ii,cColNamn).
           END.
           WHEN 7 THEN DO:
               cArtbas_Cenhet  = TRIM(REPLACE(ENTRY(ii,cStr,CHR(9)),'"','')) NO-ERROR.
               IF ERROR-STATUS:ERROR THEN
                   ASSIGN cArtbas_Cenhet  = ?
                          ENTRY(ii,cError) = ENTRY(ii,cColNamn).
           END.
           WHEN 8 THEN DO:
               iArtbas_hg    = INT(ENTRY(ii,cStr,CHR(9))) NO-ERROR.
               IF ERROR-STATUS:ERROR THEN
                   ASSIGN iArtbas_hg    = ?
                          ENTRY(ii,cError) = ENTRY(ii,cColNamn).
           END.
           WHEN 9 THEN DO:
               dpris_engrosn = DECI(REPLACE(ENTRY(ii,cStr,CHR(9)),".",",")) NO-ERROR.
               IF ERROR-STATUS:ERROR THEN
                   ASSIGN dpris_engrosn = ?
                          ENTRY(ii,cError) = ENTRY(ii,cColNamn).
               ELSE IF dpris_engrosn = 0 THEN
                   ENTRY(ii,cError) = ENTRY(ii,cColNamn).
           END.
           WHEN 10 THEN DO:
               .
           END.
           WHEN 11 THEN DO:
               dpris_utprisn = DECI(REPLACE(ENTRY(ii,cStr,CHR(9)),".",",")) NO-ERROR.
               IF ERROR-STATUS:ERROR THEN
                   ASSIGN dpris_utprisn = ?
                          ENTRY(ii,cError) = ENTRY(ii,cColNamn).
           END.
           WHEN 12 THEN DO:
               dpris_veilpris = DECI(REPLACE(ENTRY(ii,cStr,CHR(9)),".",",")) NO-ERROR.
               IF ERROR-STATUS:ERROR THEN
                   ASSIGN dpris_veilpris = ?
                          ENTRY(ii,cError) = ENTRY(ii,cColNamn).
           END.
           WHEN 13 THEN DO:
               dvare_mva      = DECI(REPLACE(ENTRY(ii,cStr,CHR(9)),".",",")) NO-ERROR.
               IF ERROR-STATUS:ERROR THEN
                   ASSIGN dvare_mva      = ?
                          ENTRY(ii,cError) = ENTRY(ii,cColNamn).
           END.
           WHEN 14 THEN DO:
               dvare_antpkn   = DECI(REPLACE(ENTRY(ii,cStr,CHR(9)),".",",")) NO-ERROR.
               IF ERROR-STATUS:ERROR THEN
                   ASSIGN dvare_antpkn   = ?
                          ENTRY(ii,cError) = ENTRY(ii,cColNamn).
           END.
           WHEN 15 THEN DO:
               dArtbas_linkvarenr   = DECI(REPLACE(ENTRY(ii,cStr,CHR(9)),".",",")) NO-ERROR.
               IF ERROR-STATUS:ERROR THEN
                   ASSIGN dArtbas_linkvarenr   = 0.
               IF dArtbas_linkvarenr > 0 AND NOT CAN-FIND(FIRST vare WHERE vare.ean = dArtbas_linkvarenr) THEN
                          ENTRY(ii,cError) = ENTRY(ii,cColNamn).
           END.
         END CASE.
/*          IF ERROR-STATUS:ERROR THEN DO:                                      */
/*              CREATE tt_Prisfil.                                              */
/*              ASSIGN TT_Prisfil.Radnr         = iRad                         */
/*                     tt_PrisFil.cStatus        = "FEL " + entry(ii,cColNamn). */
/*              NEXT LESRAD.                                                    */
/*          END.                                                                */

       END.
       iArtbas_enhet = IF cArtbas_Cenhet = "ST" THEN 1 ELSE IF cArtbas_Cenhet = "KG" THEN 2 ELSE IF cArtbas_Cenhet = "L" THEN 3
                                                   ELSE IF cArtbas_Cenhet = "M" THEN 4 ELSE IF cArtbas_Cenhet = "" THEN 0 ELSE ?.
       FIND TT_moms WHERE TT_moms.momsproc = dvare_mva NO-ERROR.
       ivare_mvagr = IF AVAIL TT_moms THEN TT_moms.momsprocgr ELSE ?.
       IF ivare_mvagr = ? THEN
           ENTRY(13,cError) = ENTRY(13,cColNamn).
/* om varan finns, hämta beskr ?? i vareje fall vid tandem ?? */
       dOrigVareEAN = 0.
       dTandemEAN   = 0.
       FIND vare WHERE vare.ean = dStrekkode_kode NO-LOCK NO-ERROR.
       IF NOT AVAIL vare THEN DO:
           FIND tandem WHERE tandem.tandemean = dStrekkode_kode NO-LOCK NO-ERROR.
           IF AVAIL tandem THEN DO:
               FIND vare OF tandem NO-LOCK NO-ERROR.
               IF AVAIL Vare THEN
                   ASSIGN dTandemEAN = dStrekkode_kode   /* vi har fått ny tandem */
                          dStrekkode_kode  = tandem.ean. /* befintlig vara */
           END.
           ELSE DO:
               IF dArtbas_levnr <> ? AND iStrekkode_bestnr <> ? THEN DO:
                   FIND FIRST pris WHERE pris.levnr  = dArtbas_levnr  AND
                                         pris.bestnr = iStrekkode_bestnr AND
                                         pris.profnr = 1 AND
                                         pris.butnr = 0 NO-LOCK NO-ERROR.
                   IF AVAIL pris THEN DO:
                       FIND vare OF pris NO-LOCK NO-ERROR.
                       IF AVAIL Vare THEN
                           ASSIGN dTandemEAN = dStrekkode_kode
                                  dStrekkode_kode  = pris.ean.
                   END.
               END.
           END.
       END.
       IF AVAIL vare THEN DO:
           IF iArtbas_enhet = 0 THEN DO:
               iArtbas_enhet = vare.enhet.
               CASE iArtbas_enhet:
                   WHEN 1 THEN cArtbas_Cenhet = "ST".
                   WHEN 2 THEN cArtbas_Cenhet = "KG".
                   WHEN 3 THEN cArtbas_Cenhet = "L".
                   WHEN 4 THEN cArtbas_Cenhet = "M".
                   OTHERWISE cArtbas_Cenhet = "".
               END CASE.
           END.
           IF iArtbas_enhet = 0 THEN DO:
               iArtbas_enhet = ?.
               ENTRY(7,cError) = ENTRY(7,cColNamn).
           END.
       END.
       /* testa om varan är ny eller huvudvara(prisändring) ellse r tandem */
/*        RELEASE tandem.                                                            */
/*        IF dOrigVareEAN > 0 AND dOrigVareEAN <> dStrekkode_kode THEN                     */
/*            FIND FIRST tandem WHERE tandem.tandemean = dStrekkode_kode NO-LOCK NO-ERROR. */
       /* testa om inpris > utpris */
/*        IF  THEN */
       RELEASE hovedgr.
       RELEASE paslag.
/*        FIND hovedgr WHERE hovedgr.hgr = iArtbas_hg NO-LOCK NO-ERROR. */
       FIND hgrprof WHERE hgrprof.hgr = iArtbas_hg AND hgrprof.profnr = 1 NO-LOCK NO-ERROR.
       IF AVAIL hgrprof THEN DO:
               dhgrprof_brutto% = hgrprof.brutto%.
       END.
       ELSE
           dhgrprof_brutto% = 0.
       IF iArtbas_hg = 0 AND AVAIL vare THEN
           iArtbas_hg = vare.hgr.
       CREATE tt_Prisfil.
       ASSIGN TT_Prisfil.Radnr         = iRad
              tt_Prisfil.lNy            = NOT AVAIL vare
              tt_Prisfil.ctandem        = IF dTandemEAN <> 0 THEN STRING(dTandemEAN) ELSE ""
              tt_Prisfil.Artbas_levnr     = dArtbas_levnr    
              tt_Prisfil.Artbas_levkod     = dArtbas_levkod    
              tt_Prisfil.Strekkode_kode       = dStrekkode_kode      
              tt_Prisfil.Strekkode_bestnr    = iStrekkode_bestnr   
              tt_Prisfil.Artbas_beskr = cArtbas_beskr
              tt_Prisfil.Artbas_mengde    = dArtbas_mengde   
              tt_Prisfil.Artbas_enhet     = iArtbas_enhet    
              tt_Prisfil.Artbas_Cenhet    = cArtbas_Cenhet   
              tt_Prisfil.Artbas_hg       = iArtbas_hg      
              tt_Prisfil.pris_engrosn   = dpris_engrosn  
              tt_Prisfil.rabatt         = drabatt        
              tt_Prisfil.pris_utprisn   = dpris_utprisn
              tt_Prisfil.pris_veilpris  = IF dpris_veilpris = 0 THEN dpris_utprisn ELSE dpris_veilpris
              tt_Prisfil.vare_mva       = dvare_mva      
              tt_Prisfil.vare_mvagr     = ivare_mvagr  
              tt_Prisfil.vare_antpkn    = IF dvare_antpkn = 0 AND AVAIL vare THEN vare.antpkn ELSE dvare_antpkn
              tt_Prisfil.Artbas_linkvarenr      = dArtbas_linkvarenr
              tt_Prisfil.hgrprof_brutto% = dhgrprof_brutto%
              tt_Prisfil.orgdata        = cStr.

       IF dpris_utprisn = 0 AND dpris_engrosn > 0 AND AVAIL hgrprof AND hgrprof_brutto% > 0 THEN DO:
           dpris_utprisn = dpris_engrosn * ( 100 / (100 - hgrprof_brutto%)) * (1 + dvare_mva / 100).
           IF dpris_utprisn = ? THEN
              dpris_utprisn = 0.
           ELSE DO:
               dOren = dpris_utprisn - TRUNC(dpris_utprisn,0).
               IF dOren <> 0 THEN DO:
                   IF dOren < .5 THEN
                       dOren = .5.
                   ELSE IF dOren < .9 THEN
                       dOren = .9.
                   ELSE dOren = 1.
                   dpris_utprisn = TRUNC(dpris_utprisn,0) + dOren.
               END.
               tt_prisfil.pris_utprisn = dpris_utprisn.
           END.
       END.
       IF dpris_engrosn > 0 THEN DO:
           ASSIGN dUtNetto = ROUND(dpris_utprisn / (1 + dvare_mva / 100),2)
                  dTBkr    = dUtNetto - dpris_engrosn.
           ASSIGN tt_Prisfil.fil_tbproc = ROUND(dTBkr / dUtNetto * 100,2) NO-ERROR.
                  tt_Prisfil.fil_tbproc = IF tt_Prisfil.fil_tbproc = ? THEN 0 ELSE tt_Prisfil.fil_tbproc.
       END.
       cErrorFelt = "".
       IF cError <> cEntryList THEN DO:
         DO ii = 1 TO NUM-ENTRIES(cError):
             IF ENTRY(ii,cError) <> "" THEN
                 cErrorFelt = cErrorFelt + (IF cErrorFelt <> "" THEN " " ELSE "") + ENTRY(ii,cError).
         END.
         CREATE TT_Error.
         ASSIGN tt_Error.RadNr  = iRad
                tt_Error.cError = cErrorFelt.
         RELEASE TT_Error.
       END.
       ELSE DO:
         /* när vi inte har några fel på raden så skall vi jämföra mot db */
         IF NOT CAN-FIND(tt_error WHERE tt_error.radnr = iRad) THEN DO:
           IF tt_Prisfil.lNy = FALSE THEN DO:
             FIND vare WHERE vare.ean = dStrekkode_kode NO-LOCK NO-ERROR.
             IF NOT AVAIL vare THEN
                 NEXT.
             FIND pris WHERE pris.ean = dStrekkode_kode AND
                             pris.profnr = 1 AND
                             pris.butnr = 0 NO-LOCK NO-ERROR.
             IF NOT AVAIL pris THEN
                 NEXT.
             IF AVAIL vare THEN DO:
               IF vare.varetekst <> cArtbas_beskr THEN DO:
                   RUN SkapaTT_VareOrgInfo(iRad,"Artbas_beskr",ENTRY(5,cColNamn),vare.varetekst).
               END.
               IF pris.bestnr <> ? AND pris.bestnr <> iStrekkode_bestnr THEN DO:
                   RUN SkapaTT_VareOrgInfo(iRad,"Strekkode_bestnr",ENTRY(4,cColNamn),string(pris.bestnr)).
               END.
               IF vare.hgr <> iArtbas_hg THEN DO:
                   RUN SkapaTT_VareOrgInfo(iRad,"Artbas_hg",ENTRY(8,cColNamn),string(vare.hgr)).
               END.
               IF vare.mengde <> dArtbas_mengde THEN DO:
                   IF dArtbas_mengde = 0 THEN
                       ASSIGN tt_prisfil.Artbas_mengde = vare.mengde.
                   RUN SkapaTT_VareOrgInfo(iRad,"Artbas_mengde",ENTRY(6,cColNamn),string(vare.mengde,">>,>>9.999")).
               END.
               IF vare.enhet <> iArtbas_enhet THEN DO:
                   RUN SkapaTT_VareOrgInfo(iRad,"Artbas_enhet",ENTRY(7,cColNamn),string(vare.enhet)).
               END.
               IF pris.engrosn <> dpris_engrosn THEN DO:
                   tt_Prisfil.lNypris = TRUE.
                   RUN SkapaTT_VareOrgInfo(iRad,"pris_engrosn",ENTRY(9,cColNamn),string(pris.engrosn,">>,>>9.99")).
               END.
               IF pris.utprisn <> dpris_utprisn THEN DO:
                   RUN SkapaTT_VareOrgInfo(iRad,"pris_utprisn",ENTRY(11,cColNamn),string(pris.utprisn,">>,>>9.99")).
               END.
/*                tt_Prisfil.Artbas_Cenhet    = cArtbas_Cenhet */
               IF pris.veilpris <> dpris_veilpris THEN DO:
                   RUN SkapaTT_VareOrgInfo(iRad,"pris_veilpris",ENTRY(12,cColNamn),string(pris.veilpris,">>,>>9.99")).
               END.
/*                tt_Prisfil.vare_mva       = dvare_mva */
               IF vare.momsprocgr <> ivare_mvagr THEN DO:
                   RUN SkapaTT_VareOrgInfo(iRad,"vare_mvagr",ENTRY(13,cColNamn),string(vare.momsprocgr)).
               END.
               IF vare.link <> dArtbas_linkvarenr THEN DO:
                   RUN SkapaTT_VareOrgInfo(iRad,"Artbas_linkvarenr",ENTRY(15,cColNamn),string(vare.link)).
               END.
             END.
             IF tt_Prisfil.cTandem <> "" THEN DO:
                  FIND tandem WHERE tandem.ean = tt_Prisfil.Strekkode_kode AND tandem.tandemean = DECI(tt_Prisfil.cTandem) NO-LOCK NO-ERROR.
                  IF NOT AVAIL tandem THEN DO:
                      RUN SkapaTT_VareOrgInfo(iRad,"ny_tandem","Ny tandem","").
                  END.
             END.
           END.
/*            ELSE DO: /* ny vara */                        */
/*                CREATE TT_VareOrgInfo.                    */
/*                ASSIGN TT_VareOrgInfo.Radnr = iRad        */
/*                       TT_VareOrgInfo.cTTfelt = "ny_vara" */
/*                       TT_VareOrgInfo.cFelt = "Ny vara"   */
/*                       TT_VareOrgInfo.cVerdi = "".        */
/*            END.                                          */
         END.
         
         IF CAN-FIND(FIRST TT_VareOrgInfo WHERE TT_VareOrgInfo.Radnr = iRad) THEN DO:
/*              FIND tt_Prisfil WHERE TT_Prisfil.Radnr = iRad. */
                 tt_Prisfil.cStatus = "J".
         END.
       END.
/*        PUT UNFORMATTED (IF iAntRows > 0 THEN string(iAntrows) ELSE "") "|" REPLACE(cStr,CHR(9),"|") SKIP. */
/*        iAntRows = iAntRows + 1.                                                                           */
   END.
   INPUT CLOSE.
/*    OUTPUT TO "CLIPBOARD". */
/*    FOR EACH tt_prisfil:   */
/*        EXPORT tt_prisfil. */
/*    END.                   */
/*    OUTPUT CLOSE.          */
{&OPEN-BROWSERS-IN-QUERY-{&FRAME-NAME}}
IF iCurrPost > 0 THEN DO:
    FIND tt_prisfil WHERE tt_prisfil.radnr = iCurrPost NO-ERROR.
    IF AVAIL tt_prisfil THEN DO:
        BROWSE BROWSE-TT_Prisfil:SET-REPOSITIONED-ROW(iCurrRow,"ALWAYS").
        REPOSITION BROWSE-TT_Prisfil TO ROWID ROWID(tt_prisfil) NO-ERROR.
        APPLY "VALUE-CHANGED" TO BROWSE BROWSE-TT_Prisfil.
    END.
END.
ELSE IF BROWSE BRowse-TT_Error:FOCUSED-ROW <> ? THEN DO:
    FIND TT_Prisfil WHERE TT_Prisfil.Radnr = tt_error.radnr.
    REPOSITION BROWSE-TT_Prisfil TO ROWID ROWID(tt_Prisfil).
END.
    BROWSE-TT_Prisfil:SELECT-FOCUSED-ROW() NO-ERROR.
/* BROWSE BROWSE-TT_Prisfil:DESELECT-FOCUSED-ROW(). */
  PROCESS EVENTS.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initTTMoms C-Win 
PROCEDURE initTTMoms :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    FOR EACH moms NO-LOCK.
        CREATE tt_moms.
        BUFFER-COPY moms TO tt_moms.
        RELEASE tt_moms.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LesEXCEL C-Win 
PROCEDURE LesEXCEL :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER cFileName  AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER cUtfilnamn AS CHARACTER   NO-UNDO.
  CLOSE QUERY BROWSE-TT_Error. 
  CLOSE QUERY BROWSE-TT_Prisfil. 
  CLOSE QUERY BROWSE-TT_VareOrgInfo.
  IF lNyfil = TRUE THEN DO:
      iCurrPost = 0.
      EMPTY TEMP-TABLE TT_Prisfil.
      EMPTY TEMP-TABLE TT_Error.
      EMPTY TEMP-TABLE TT_VareOrgInfo.
      RS-Avvik:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "*".
      FI-Filnamn:SCREEN-VALUE = "".
      ASSIGN RS-Avvik.
  END.
  PROCESS EVENTS.
  CREATE "Excel.Application" chExcelApplication.  
  chExcelApplication:Visible = FALSE.                                     
  chExcelApplication:DisplayAlerts = FALSE.
/*   chWorkbooks = chExcelApplication:Workbooks:OpenText(cFileName). */
  chWorkbooks = chExcelApplication:Workbooks:OpenText(cFileName,2,1,1,1,1,FALSE,TRUE,FALSE,FALSE,FALSE).
 
  STATUS DEFAULT "Setter aktivt ark...".
  chWorkSheets = chExcelApplication:Sheets:Item(1).
  chWorkSheets:SaveAs(cUtfilnamn,42).

  RELEASE OBJECT chWorksheets NO-ERROR.            /* release com-handles */
  RELEASE OBJECT chWorkbooks NO-ERROR.             /* release com-handles */
  chExcelApplication:Workbooks:CLOSE().
  RELEASE OBJECT chExcelApplication NO-ERROR.      /* release com-handles */
  ASSIGN chWorksheets       = ?
         chWorkbooks        = ?
         chExcelApplication = ?.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SkapaTT_VareOrgInfo C-Win 
PROCEDURE SkapaTT_VareOrgInfo :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT  PARAMETER iRad    AS INTEGER     NO-UNDO.
   DEFINE INPUT  PARAMETER cTTfelt AS CHARACTER   NO-UNDO.
   DEFINE INPUT  PARAMETER cFelt   AS CHARACTER   NO-UNDO.
   DEFINE INPUT  PARAMETER cVerdi AS CHARACTER   NO-UNDO.
   CREATE TT_VareOrgInfo.
   ASSIGN TT_VareOrgInfo.Radnr = iRad
          TT_VareOrgInfo.cTTfelt = cTTfelt
          TT_VareOrgInfo.cFelt = cFelt
          TT_VareOrgInfo.cVerdi = cVerdi.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

