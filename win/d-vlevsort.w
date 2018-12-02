&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          skotex           PROGRESS
          temp-db          PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame

/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE tmpStrDef NO-UNDO LIKE tmpStrDef.


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame 
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
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */
&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
  def var wRecid       as recid no-undo.
  def var wModus       as char  no-undo.
  def var wLevBasRecid as RECID no-undo.
  def var wStrTypeID   like StrType.StrTypeID no-undo.
  find first LevSort no-lock.
  find LevBas of LevSort no-lock.
  assign wRecid       = recid(LevSort)
         wModus       = "ENDRE"
         wLevBasRecid = RECID(LevBas).
&ELSE
  def input-output parameter wRecid       as recid  no-undo.
  def input        parameter wModus       as char   no-undo.
  def input        parameter wLevBasRecid as RECID  no-undo.
  def input        parameter wStrTypeID   like StrType.StrTypeID no-undo.
&ENDIF

/* Preprossessor direktiver ---                                         */
&scoped-define br-tabell LevSort
&scoped-define KeyFelt SortId
&scoped-define DataType STRING /* INT STRING DEC Datatype på nøkkelfelt*/
/* Henter eventuelle relaterte poster - før mainblokk */
&scoped-define FinnRelatertePoster if available LevSort then ~
      do: ~
        Find StrType of LevSort no-lock no-error. ~
        find LevBas  of LevSort no-lock no-error. ~
      end.
/* Ekstra informasjon i find/where når det er flere ledd i indeks */
&scoped-define OptFind 
/* Felter som skal vises frem når rutinen VisPost kjøres */
&scoped-define VisPoster LevSort.SortId when available LevSort ~
                         LevSort.Beskrivelse when available LevSort ~
                         LevSort.Merknad when available LevSort ~
                         LevSort.Fri when available LevSort ~
                         LevSort.StrTypeId when available LevSort ~
                         StrType.Beskrivelse when available StrType
/* Alternative poster som skal vises når VisPost kjøres */
&scoped-define VisAndreData 
/* Ved sletting - Sjekker om posten kan slettes */
&scoped-define SjekkOmPostFinnes ~
  find bLevSort no-lock where ~
  bLevSort.LevNr = LevBas.LevNr and ~
  bLevSort.SortId = input LevSort.SortID AND ~
  bLevSort.StrTypeId = input LevSort.StrTypeId no-error. ~
  if available bLevSort then ~
    do: ~
      message "Det finnes allerede et leverandørsortiment med dette ID!" ~
      view-as alert-box title "Lagringsfeil". ~
      return no-apply "AVBRYT". ~
    end.
  
/* Felter som assign'es når rutinen LagrePost kjøres */        
&scoped-define AssignFelter LevSort.Beskrivelse ~
                            LevSort.Merknad ~
                            LevSort.Fri ~
                            LevSort.StrTypeId

/* Tilleggs felter som assign'es når rutinen Lagre post kjøres. */
&scoped-define TillegsAssign

/* Local Variable Definitions ---                                       */
def var wRetur-Verdi as char initial "AVBRYT" no-undo.

/* Local Variable Definitions ---                                       */
def var wtmpStrDefRecid as   recid             no-undo.
def var wLoop           as   int               no-undo.

def temp-table tLevSAnt LIKE LevSAnt.

def buffer bLevSort for LevSort.

{runlib.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME Dialog-Frame

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tmpStrDef LevSort StrType

/* Definitions for DIALOG-BOX Dialog-Frame                              */
&Scoped-define FIELDS-IN-QUERY-Dialog-Frame LevSort.SortID ~
LevSort.Beskrivelse LevSort.Fri LevSort.Merknad LevSort.StrTypeID ~
StrType.KortNavn StrType.Beskrivelse 
&Scoped-define ENABLED-FIELDS-IN-QUERY-Dialog-Frame LevSort.Beskrivelse ~
LevSort.Merknad 
&Scoped-define ENABLED-TABLES-IN-QUERY-Dialog-Frame LevSort
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-Dialog-Frame LevSort
&Scoped-define QUERY-STRING-Dialog-Frame FOR EACH tmpStrDef SHARE-LOCK, ~
      EACH LevSort WHERE TRUE /* Join to tmpStrDef incomplete */ SHARE-LOCK, ~
      EACH StrType OF LevSort SHARE-LOCK
&Scoped-define OPEN-QUERY-Dialog-Frame OPEN QUERY Dialog-Frame FOR EACH tmpStrDef SHARE-LOCK, ~
      EACH LevSort WHERE TRUE /* Join to tmpStrDef incomplete */ SHARE-LOCK, ~
      EACH StrType OF LevSort SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-Dialog-Frame tmpStrDef LevSort StrType
&Scoped-define FIRST-TABLE-IN-QUERY-Dialog-Frame tmpStrDef
&Scoped-define SECOND-TABLE-IN-QUERY-Dialog-Frame LevSort
&Scoped-define THIRD-TABLE-IN-QUERY-Dialog-Frame StrType


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS LevSort.Beskrivelse LevSort.Merknad 
&Scoped-define ENABLED-TABLES LevSort
&Scoped-define FIRST-ENABLED-TABLE LevSort
&Scoped-Define ENABLED-OBJECTS Btn_OK Btn_Cancel BUTTON-Storrelser Btn_Help ~
RECT-3 RECT-4 
&Scoped-Define DISPLAYED-FIELDS LevSort.SortID LevSort.Beskrivelse ~
LevSort.Fri LevSort.Merknad LevSort.StrTypeID StrType.KortNavn ~
StrType.Beskrivelse tmpStrDef.Str[1] tmpStrDef.Str[2] tmpStrDef.Str[3] ~
tmpStrDef.Str[4] tmpStrDef.Str[5] tmpStrDef.Str[6] tmpStrDef.Str[7] ~
tmpStrDef.Str[8] tmpStrDef.Str[9] tmpStrDef.Str[10] tmpStrDef.Str[11] ~
tmpStrDef.Str[12] tmpStrDef.Ant[1] tmpStrDef.Ant[2] tmpStrDef.Ant[3] ~
tmpStrDef.Ant[4] tmpStrDef.Ant[5] tmpStrDef.Ant[6] tmpStrDef.Ant[7] ~
tmpStrDef.Ant[8] tmpStrDef.Ant[9] tmpStrDef.Ant[10] tmpStrDef.Ant[11] ~
tmpStrDef.Ant[12] tmpStrDef.Str[13] tmpStrDef.Str[14] tmpStrDef.Str[15] ~
tmpStrDef.Str[16] tmpStrDef.Str[17] tmpStrDef.Str[18] tmpStrDef.Str[19] ~
tmpStrDef.Str[20] tmpStrDef.Str[21] tmpStrDef.Str[22] tmpStrDef.Str[23] ~
tmpStrDef.Str[24] tmpStrDef.Ant[13] tmpStrDef.Ant[14] tmpStrDef.Ant[15] ~
tmpStrDef.Ant[16] tmpStrDef.Ant[17] tmpStrDef.Ant[18] tmpStrDef.Ant[19] ~
tmpStrDef.Ant[20] tmpStrDef.Ant[21] tmpStrDef.Ant[22] tmpStrDef.Ant[23] ~
tmpStrDef.Ant[24] tmpStrDef.Str[25] tmpStrDef.Str[26] tmpStrDef.Str[27] ~
tmpStrDef.Str[28] tmpStrDef.Str[29] tmpStrDef.Str[30] tmpStrDef.Str[31] ~
tmpStrDef.Str[32] tmpStrDef.Str[33] tmpStrDef.Str[34] tmpStrDef.Str[35] ~
tmpStrDef.Str[36] tmpStrDef.Ant[25] tmpStrDef.Ant[26] tmpStrDef.Ant[27] ~
tmpStrDef.Ant[28] tmpStrDef.Ant[29] tmpStrDef.Ant[30] tmpStrDef.Ant[31] ~
tmpStrDef.Ant[32] tmpStrDef.Ant[33] tmpStrDef.Ant[34] tmpStrDef.Ant[35] ~
tmpStrDef.Ant[36] tmpStrDef.Str[37] tmpStrDef.Str[38] tmpStrDef.Str[39] ~
tmpStrDef.Str[40] tmpStrDef.Str[41] tmpStrDef.Str[42] tmpStrDef.Str[43] ~
tmpStrDef.Str[44] tmpStrDef.Str[45] tmpStrDef.Str[46] tmpStrDef.Str[47] ~
tmpStrDef.Str[48] tmpStrDef.Ant[37] tmpStrDef.Ant[38] tmpStrDef.Ant[39] ~
tmpStrDef.Ant[40] tmpStrDef.Ant[41] tmpStrDef.Ant[42] tmpStrDef.Ant[43] ~
tmpStrDef.Ant[44] tmpStrDef.Ant[45] tmpStrDef.Ant[46] tmpStrDef.Ant[47] ~
tmpStrDef.Ant[48] 
&Scoped-define DISPLAYED-TABLES LevSort StrType tmpStrDef
&Scoped-define FIRST-DISPLAYED-TABLE LevSort
&Scoped-define SECOND-DISPLAYED-TABLE StrType
&Scoped-define THIRD-DISPLAYED-TABLE tmpStrDef
&Scoped-Define DISPLAYED-OBJECTS FI-Total 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD FinnAntall Dialog-Frame 
FUNCTION FinnAntall RETURNS INTEGER
  ( input wModus as char,
    input wLoop  as int )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Cancel AUTO-END-KEY 
     LABEL "Avbryt" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON Btn_Help 
     LABEL "&Hjelp" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON Btn_OK AUTO-GO 
     LABEL "OK" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON BUTTON-SokStrType 
     IMAGE-UP FILE "icon\e-sokpr":U NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON BUTTON-Storrelser 
     LABEL "Hent størrelser..." 
     SIZE 19 BY 1.14.

DEFINE VARIABLE FI-Total AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     LABEL "Total" 
     VIEW-AS FILL-IN 
     SIZE 12.8 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-3
     EDGE-PIXELS 4 GRAPHIC-EDGE  NO-FILL 
     SIZE 90.4 BY 14.1.

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 88 BY .1.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Dialog-Frame FOR 
      tmpStrDef, 
      LevSort, 
      StrType SCROLLING.
&ANALYZE-RESUME

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     LevSort.SortID AT ROW 2.19 COL 12 COLON-ALIGNED
          LABEL "Sortiment"
          VIEW-AS FILL-IN 
          SIZE 17.6 BY 1
     LevSort.Beskrivelse AT ROW 2.19 COL 30 COLON-ALIGNED NO-LABEL FORMAT "X(200)"
          VIEW-AS FILL-IN 
          SIZE 38.2 BY 1
     LevSort.Fri AT ROW 1.95 COL 73
          LABEL "Fri inndeling"
          VIEW-AS TOGGLE-BOX
          SIZE 15.4 BY .81
     LevSort.Merknad AT ROW 3.19 COL 12 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 56.2 BY 1
     LevSort.StrTypeID AT ROW 4.24 COL 12 COLON-ALIGNED
          LABEL "Type" FORMAT "zzzzz9"
          VIEW-AS FILL-IN 
          SIZE 11 BY 1
     StrType.KortNavn AT ROW 4.24 COL 27 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 12.2 BY 1
     BUTTON-SokStrType AT ROW 4.24 COL 24.6
     StrType.Beskrivelse AT ROW 4.24 COL 39.4 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 28.8 BY 1
     FI-Total AT ROW 4.24 COL 74.6 COLON-ALIGNED
     tmpStrDef.Str[1] AT ROW 6.24 COL 15.8 COLON-ALIGNED NO-LABEL FORMAT "X(4)"
          VIEW-AS FILL-IN 
          SIZE 6 BY 1
     tmpStrDef.Str[2] AT ROW 6.24 COL 21.8 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 6 BY 1
     tmpStrDef.Str[3] AT ROW 6.24 COL 27.8 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 6 BY 1
     tmpStrDef.Str[4] AT ROW 6.24 COL 33.8 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 6 BY 1
     tmpStrDef.Str[5] AT ROW 6.24 COL 39.8 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 6 BY 1
     tmpStrDef.Str[6] AT ROW 6.24 COL 45.8 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 6 BY 1
     tmpStrDef.Str[7] AT ROW 6.24 COL 51.8 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 6 BY 1
     tmpStrDef.Str[8] AT ROW 6.24 COL 57.8 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 6 BY 1
     tmpStrDef.Str[9] AT ROW 6.24 COL 63.8 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 6 BY 1
     tmpStrDef.Str[10] AT ROW 6.24 COL 69.8 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 6 BY 1
     tmpStrDef.Str[11] AT ROW 6.24 COL 75.8 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 6 BY 1
     tmpStrDef.Str[12] AT ROW 6.24 COL 81.8 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 6 BY 1
     tmpStrDef.Ant[1] AT ROW 7.29 COL 15.8 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 6 BY 1
     tmpStrDef.Ant[2] AT ROW 7.29 COL 21.8 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 6 BY 1
     tmpStrDef.Ant[3] AT ROW 7.29 COL 27.8 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 6 BY 1
     tmpStrDef.Ant[4] AT ROW 7.29 COL 33.8 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 6 BY 1
     tmpStrDef.Ant[5] AT ROW 7.29 COL 39.8 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 6 BY 1
     tmpStrDef.Ant[6] AT ROW 7.29 COL 45.8 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 6 BY 1
     tmpStrDef.Ant[7] AT ROW 7.29 COL 51.8 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 6 BY 1
     tmpStrDef.Ant[8] AT ROW 7.29 COL 57.8 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 6 BY 1
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE .

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME Dialog-Frame
     tmpStrDef.Ant[9] AT ROW 7.29 COL 63.8 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 6 BY 1
     tmpStrDef.Ant[10] AT ROW 7.29 COL 69.8 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 6 BY 1
     tmpStrDef.Ant[11] AT ROW 7.29 COL 75.8 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 6 BY 1
     tmpStrDef.Ant[12] AT ROW 7.29 COL 81.8 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 6 BY 1
     tmpStrDef.Str[13] AT ROW 8.62 COL 15.8 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 6 BY 1
     tmpStrDef.Str[14] AT ROW 8.62 COL 21.8 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 6 BY 1
     tmpStrDef.Str[15] AT ROW 8.62 COL 27.8 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 6 BY 1
     tmpStrDef.Str[16] AT ROW 8.62 COL 33.8 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 6 BY 1
     tmpStrDef.Str[17] AT ROW 8.62 COL 39.8 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 6 BY 1
     tmpStrDef.Str[18] AT ROW 8.62 COL 45.8 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 6 BY 1
     tmpStrDef.Str[19] AT ROW 8.62 COL 51.8 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 6 BY 1
     tmpStrDef.Str[20] AT ROW 8.62 COL 57.8 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 6 BY 1
     tmpStrDef.Str[21] AT ROW 8.62 COL 63.8 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 6 BY 1
     tmpStrDef.Str[22] AT ROW 8.62 COL 69.8 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 6 BY 1
     tmpStrDef.Str[23] AT ROW 8.62 COL 75.8 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 6 BY 1
     tmpStrDef.Str[24] AT ROW 8.62 COL 81.8 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 6 BY 1
     tmpStrDef.Ant[13] AT ROW 9.62 COL 15.8 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 6 BY 1
     tmpStrDef.Ant[14] AT ROW 9.62 COL 21.8 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 6 BY 1
     tmpStrDef.Ant[15] AT ROW 9.62 COL 27.8 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 6 BY 1
     tmpStrDef.Ant[16] AT ROW 9.62 COL 33.8 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 6 BY 1
     tmpStrDef.Ant[17] AT ROW 9.62 COL 39.8 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 6 BY 1
     tmpStrDef.Ant[18] AT ROW 9.62 COL 45.8 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 6 BY 1
     tmpStrDef.Ant[19] AT ROW 9.62 COL 51.8 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 6 BY 1
     tmpStrDef.Ant[20] AT ROW 9.62 COL 57.8 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 6 BY 1
     tmpStrDef.Ant[21] AT ROW 9.62 COL 63.8 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 6 BY 1
     tmpStrDef.Ant[22] AT ROW 9.62 COL 69.8 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 6 BY 1
     tmpStrDef.Ant[23] AT ROW 9.62 COL 75.8 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 6 BY 1
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE .

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME Dialog-Frame
     tmpStrDef.Ant[24] AT ROW 9.62 COL 81.8 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 6 BY 1
     tmpStrDef.Str[25] AT ROW 11 COL 15.8 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 6 BY 1
     tmpStrDef.Str[26] AT ROW 11 COL 21.8 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 6 BY 1
     tmpStrDef.Str[27] AT ROW 11 COL 27.8 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 6 BY 1
     tmpStrDef.Str[28] AT ROW 11 COL 33.8 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 6 BY 1
     tmpStrDef.Str[29] AT ROW 11 COL 39.8 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 6 BY 1
     tmpStrDef.Str[30] AT ROW 11 COL 45.8 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 6 BY 1
     tmpStrDef.Str[31] AT ROW 11 COL 51.8 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 6 BY 1
     tmpStrDef.Str[32] AT ROW 11 COL 57.8 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 6 BY 1
     tmpStrDef.Str[33] AT ROW 11 COL 63.8 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 6 BY 1
     tmpStrDef.Str[34] AT ROW 11 COL 69.8 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 6 BY 1
     tmpStrDef.Str[35] AT ROW 11 COL 75.8 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 6 BY 1
     tmpStrDef.Str[36] AT ROW 11 COL 81.8 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 6 BY 1
     tmpStrDef.Ant[25] AT ROW 11.95 COL 15.8 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 6 BY 1
     tmpStrDef.Ant[26] AT ROW 11.95 COL 21.8 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 6 BY 1
     tmpStrDef.Ant[27] AT ROW 11.95 COL 27.8 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 6 BY 1
     tmpStrDef.Ant[28] AT ROW 11.95 COL 33.8 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 6 BY 1
     tmpStrDef.Ant[29] AT ROW 11.95 COL 39.8 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 6 BY 1
     tmpStrDef.Ant[30] AT ROW 11.95 COL 45.8 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 6 BY 1
     tmpStrDef.Ant[31] AT ROW 11.95 COL 51.8 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 6 BY 1
     tmpStrDef.Ant[32] AT ROW 11.95 COL 57.8 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 6 BY 1
     tmpStrDef.Ant[33] AT ROW 11.95 COL 63.8 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 6 BY 1
     tmpStrDef.Ant[34] AT ROW 11.95 COL 69.8 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 6 BY 1
     tmpStrDef.Ant[35] AT ROW 11.95 COL 75.8 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 6 BY 1
     tmpStrDef.Ant[36] AT ROW 11.95 COL 81.8 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 6 BY 1
     tmpStrDef.Str[37] AT ROW 13.29 COL 15.8 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 6 BY 1
     tmpStrDef.Str[38] AT ROW 13.29 COL 21.8 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 6 BY 1
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE .

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME Dialog-Frame
     tmpStrDef.Str[39] AT ROW 13.29 COL 27.8 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 6 BY 1
     tmpStrDef.Str[40] AT ROW 13.29 COL 33.8 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 6 BY 1
     tmpStrDef.Str[41] AT ROW 13.29 COL 39.8 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 6 BY 1
     tmpStrDef.Str[42] AT ROW 13.29 COL 45.8 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 6 BY 1
     tmpStrDef.Str[43] AT ROW 13.29 COL 51.8 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 6 BY 1
     tmpStrDef.Str[44] AT ROW 13.29 COL 57.8 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 6 BY 1
     tmpStrDef.Str[45] AT ROW 13.29 COL 63.8 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 6 BY 1
     tmpStrDef.Str[46] AT ROW 13.29 COL 69.8 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 6 BY 1
     tmpStrDef.Str[47] AT ROW 13.29 COL 75.8 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 6 BY 1
     tmpStrDef.Str[48] AT ROW 13.29 COL 81.8 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 6 BY 1
     tmpStrDef.Ant[37] AT ROW 14.24 COL 15.8 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 6 BY 1
     tmpStrDef.Ant[38] AT ROW 14.24 COL 21.8 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 6 BY 1
     tmpStrDef.Ant[39] AT ROW 14.24 COL 27.8 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 6 BY 1
     tmpStrDef.Ant[40] AT ROW 14.24 COL 33.8 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 6 BY 1
     tmpStrDef.Ant[41] AT ROW 14.24 COL 39.8 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 6 BY 1
     tmpStrDef.Ant[42] AT ROW 14.24 COL 45.8 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 6 BY 1
     tmpStrDef.Ant[43] AT ROW 14.24 COL 51.8 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 6 BY 1
     tmpStrDef.Ant[44] AT ROW 14.24 COL 57.8 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 6 BY 1
     tmpStrDef.Ant[45] AT ROW 14.24 COL 63.8 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 6 BY 1
     tmpStrDef.Ant[46] AT ROW 14.24 COL 69.8 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 6 BY 1
     tmpStrDef.Ant[47] AT ROW 14.24 COL 75.8 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 6 BY 1
     tmpStrDef.Ant[48] AT ROW 14.24 COL 81.8 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 6 BY 1
     Btn_OK AT ROW 15.86 COL 1.4
     Btn_Cancel AT ROW 15.86 COL 16.8
     BUTTON-Storrelser AT ROW 15.86 COL 46.4
     Btn_Help AT ROW 15.86 COL 77.4
     "Antall:" VIEW-AS TEXT
          SIZE 10.8 BY .62 AT ROW 14.52 COL 6.8
     "Størrelse:" VIEW-AS TEXT
          SIZE 10.4 BY .62 AT ROW 13.57 COL 6.8
     "Antall:" VIEW-AS TEXT
          SIZE 10.8 BY .62 AT ROW 12.24 COL 6.8
     "Størrelse:" VIEW-AS TEXT
          SIZE 10.4 BY .62 AT ROW 11.29 COL 6.8
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE .

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME Dialog-Frame
     "Antall:" VIEW-AS TEXT
          SIZE 10.8 BY .62 AT ROW 9.95 COL 6.8
     "Størrelse:" VIEW-AS TEXT
          SIZE 10.4 BY .62 AT ROW 8.91 COL 6.8
     "Antall:" VIEW-AS TEXT
          SIZE 10.8 BY .62 AT ROW 7.48 COL 6.8
     "Størrelse:" VIEW-AS TEXT
          SIZE 10.4 BY .62 AT ROW 6.52 COL 6.8
     RECT-3 AT ROW 1.57 COL 1.8
     RECT-4 AT ROW 5.76 COL 3.2
     SPACE(1.20) SKIP(11.14)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Vedlikehold størrelsesinndeling størrelsestyper".


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: DIALOG-BOX
   Allow: Basic,Browse,DB-Fields,Query
   Other Settings: COMPILE
   Temp-Tables and Buffers:
      TABLE: tmpStrDef T "?" NO-UNDO Temp-DB tmpStrDef
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX Dialog-Frame
   NOT-VISIBLE Custom                                                   */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE.

/* SETTINGS FOR FILL-IN tmpStrDef.Ant[10] IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN tmpStrDef.Ant[11] IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN tmpStrDef.Ant[12] IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN tmpStrDef.Ant[13] IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN tmpStrDef.Ant[14] IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN tmpStrDef.Ant[15] IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN tmpStrDef.Ant[16] IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN tmpStrDef.Ant[17] IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN tmpStrDef.Ant[18] IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN tmpStrDef.Ant[19] IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN tmpStrDef.Ant[1] IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN tmpStrDef.Ant[20] IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN tmpStrDef.Ant[21] IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN tmpStrDef.Ant[22] IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN tmpStrDef.Ant[23] IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN tmpStrDef.Ant[24] IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN tmpStrDef.Ant[25] IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN tmpStrDef.Ant[26] IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN tmpStrDef.Ant[27] IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN tmpStrDef.Ant[28] IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN tmpStrDef.Ant[29] IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN tmpStrDef.Ant[2] IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN tmpStrDef.Ant[30] IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN tmpStrDef.Ant[31] IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN tmpStrDef.Ant[32] IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN tmpStrDef.Ant[33] IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN tmpStrDef.Ant[34] IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN tmpStrDef.Ant[35] IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN tmpStrDef.Ant[36] IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN tmpStrDef.Ant[37] IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN tmpStrDef.Ant[38] IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN tmpStrDef.Ant[39] IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN tmpStrDef.Ant[3] IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN tmpStrDef.Ant[40] IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN tmpStrDef.Ant[41] IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN tmpStrDef.Ant[42] IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN tmpStrDef.Ant[43] IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN tmpStrDef.Ant[44] IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN tmpStrDef.Ant[45] IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN tmpStrDef.Ant[46] IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN tmpStrDef.Ant[47] IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN tmpStrDef.Ant[48] IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN tmpStrDef.Ant[4] IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN tmpStrDef.Ant[5] IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN tmpStrDef.Ant[6] IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN tmpStrDef.Ant[7] IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN tmpStrDef.Ant[8] IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN tmpStrDef.Ant[9] IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN LevSort.Beskrivelse IN FRAME Dialog-Frame
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN StrType.Beskrivelse IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON BUTTON-SokStrType IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-Total IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX LevSort.Fri IN FRAME Dialog-Frame
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN StrType.KortNavn IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN LevSort.SortID IN FRAME Dialog-Frame
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN LevSort.StrTypeID IN FRAME Dialog-Frame
   NO-ENABLE EXP-LABEL EXP-FORMAT                                       */
/* SETTINGS FOR FILL-IN tmpStrDef.Str[10] IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN tmpStrDef.Str[11] IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN tmpStrDef.Str[12] IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN tmpStrDef.Str[13] IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN tmpStrDef.Str[14] IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN tmpStrDef.Str[15] IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN tmpStrDef.Str[16] IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN tmpStrDef.Str[17] IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN tmpStrDef.Str[18] IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN tmpStrDef.Str[19] IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN tmpStrDef.Str[1] IN FRAME Dialog-Frame
   NO-ENABLE EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN tmpStrDef.Str[20] IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN tmpStrDef.Str[21] IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN tmpStrDef.Str[22] IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN tmpStrDef.Str[23] IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN tmpStrDef.Str[24] IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN tmpStrDef.Str[25] IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN tmpStrDef.Str[26] IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN tmpStrDef.Str[27] IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN tmpStrDef.Str[28] IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN tmpStrDef.Str[29] IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN tmpStrDef.Str[2] IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN tmpStrDef.Str[30] IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN tmpStrDef.Str[31] IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN tmpStrDef.Str[32] IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN tmpStrDef.Str[33] IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN tmpStrDef.Str[34] IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN tmpStrDef.Str[35] IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN tmpStrDef.Str[36] IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN tmpStrDef.Str[37] IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN tmpStrDef.Str[38] IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN tmpStrDef.Str[39] IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN tmpStrDef.Str[3] IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN tmpStrDef.Str[40] IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN tmpStrDef.Str[41] IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN tmpStrDef.Str[42] IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN tmpStrDef.Str[43] IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN tmpStrDef.Str[44] IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN tmpStrDef.Str[45] IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN tmpStrDef.Str[46] IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN tmpStrDef.Str[47] IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN tmpStrDef.Str[48] IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN tmpStrDef.Str[4] IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN tmpStrDef.Str[5] IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN tmpStrDef.Str[6] IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN tmpStrDef.Str[7] IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN tmpStrDef.Str[8] IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN tmpStrDef.Str[9] IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX Dialog-Frame
/* Query rebuild information for DIALOG-BOX Dialog-Frame
     _TblList          = "SkoTex.tmpStrDef,SkoTex.LevSort WHERE SkoTex.tmpStrDef ...,SkoTex.StrType OF SkoTex.LevSort"
     _Options          = "SHARE-LOCK"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX Dialog-Frame */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Vedlikehold størrelsesinndeling størrelsestyper */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Help
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Help Dialog-Frame
ON CHOOSE OF Btn_Help IN FRAME Dialog-Frame /* Hjelp */
OR HELP OF FRAME {&FRAME-NAME}
DO: /* Call Help Function (or a simple message). */
  {diahelp.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK Dialog-Frame
ON CHOOSE OF Btn_OK IN FRAME Dialog-Frame /* OK */
DO:
  DEF VAR wChoice AS LOGI NO-UNDO.
  IF NOT CAN-FIND(FIRST tLevSAnt) THEN DO:
      IF wModus = "NY" THEN
         MESSAGE "Ingen størrelser er valgt." SKIP
                 "Posten lagres ikke." SKIP
                 "Skal du avslutte?" VIEW-AS ALERT-BOX INFORMATION BUTTONS YES-NO 
                                     TITLE "" UPDATE wChoice.
      ELSE IF wModus = "ENDRE" THEN
         MESSAGE "Alle størrelser er tatt bort." SKIP
                 "Endringen lagres ikke." SKIP
                 "Skal du avslutte?" VIEW-AS ALERT-BOX INFORMATION BUTTONS YES-NO 
                                     TITLE "" UPDATE wChoice.
      IF wChoice THEN
          ASSIGN wretur-verdi = "AVBRYT".
      ELSE
          RETURN NO-APPLY.
  END.
  ELSE DO:
      RUN LagraTmpSoAnt.
      IF wStrTypeID <> ? AND NOT CAN-FIND(FIRST tLevSAnt WHERE tLevSant.SoAnt > 0) THEN DO:
         MESSAGE "Alle storleker har 0 i antall." SKIP
                 "Posten lagres ikke." SKIP
                 "Skal du avslutte?" VIEW-AS ALERT-BOX INFORMATION BUTTONS YES-NO 
                                     TITLE "" UPDATE wChoice.
          IF wChoice THEN
              ASSIGN wretur-verdi = "AVBRYT".
          ELSE
              RETURN NO-APPLY.
          LEAVE.
      END.
      /* Lagrer LevSort. */
      run LagrePost.
      if return-value = "AVBRYT" then
        return no-apply.  

      /* Lagrer størrelsene */
      run LagreStorrelser.
      if return-value = "AVBRYT" then
        return no-apply.  
      ASSIGN wretur-verdi = "OK".
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-SokStrType
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-SokStrType Dialog-Frame
ON CHOOSE OF BUTTON-SokStrType IN FRAME Dialog-Frame /* ... */
or F10 of LevSort.StrTypeId
DO:
  /* Start søkeprogram */
  {soek.i
    &Felt        = LevSort.StrTypeId
    &Program     = d-bstrtype.w
    &Frame       = Dialog-Frame
    &PostRun     = "find StrType no-lock where
                    recid(StrType) = int(return-value) no-error."
    &OptDisp     = "StrType.KortNavn when available StrType
                    StrType.Beskrivelse when available StrType"
  }   
  IF RETURN-VALUE = "AVBRYT" THEN
     RETURN.
    ASSIGN tmpStrDef.Str = ""
           tmpStrDef.Ant = 0.
    FOR EACH tLevSAnt:
        DELETE tLevSAnt.
    END.
    RUN VisPost.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Storrelser
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Storrelser Dialog-Frame
ON CHOOSE OF BUTTON-Storrelser IN FRAME Dialog-Frame /* Hent størrelser... */
DO:
  def var wTagListe as char no-undo.
  def var wTekst    as char no-undo.
  def var wLoop     as int  no-undo.
  
DO WITH FRAME {&FRAME-NAME}:
  if input LevSort.SortId = "" or
     input SkoTex.LevSort.StrTypeID = "" then
    do:
      message "SortimentsID og størrelsestype må være angitt!"
        view-as alert-box title "Melding".
      return no-apply.
    end.
    ASSIGN tmpStrDef.Str[1 for 48]
           tmpStrDef.Ant[1 for 48].

  FOR EACH tLevSAnt:
      tLevSAnt.SoAnt = tmpStrDef.Ant[tLevSAnt.SeqNr].
    assign wTagListe = wTagListe +
                  (if wTagListe <> ""
                     then "!"
                     else "") +
                  LevSort.StrTypeId:SCREEN-VALUE + ";" + 
                  LevSort.SortID:SCREEN-VALUE + ";" +
                  string(tLevSAnt.SeqNr) + ";" +
                  tLevSAnt.SoStorl + ";" +
                  string(tLevSAnt.SoAnt).
  END.

  /* Starter tagRutine. */
  run d-tagstrtstr.w (input-output wTagListe, INT(LevSort.StrTypeId:SCREEN-VALUE)).
  if return-value = "AVBRYT" then
    return no-apply.
  FOR EACH tLevSAnt:
      ASSIGN tLevSAnt.SeqNr = 99.
  END.
  LAGRE-TAG:
  do TRANSACTION:
    /* Legger opp nye størrelser og retter opp sekvensnummer */
    NYE-STR:
    do wLoop = 1 to num-entries(wTagListe,"!"):
      assign
        wTekst = entry(wLoop, wTagListe, "!").
        
      find first tLevSAnt exclusive-lock where
        tLevSAnt.LevNr   = LevBas.LevNr and
        tLevSAnt.SortId  = INPUT LevSort.SortId and
        tLevSAnt.SoStorl = entry(3, wTekst, ";") no-error.
      if not available tLevSAnt then
        do:
          create tLevSAnt.
          assign
            tLevSAnt.LevNr   = LevBas.LevNr
            tLevSAnt.SortId  = INPUT LevSort.SortId
            tLevSAnt.SoStorl = entry(3, wTekst, ";") 
            tLevSAnt.SoAnt   = int(entry(4, wTekst, ";"))
            tLevSAnt.SeqNr   = int(entry(2, wTekst, ";")).
        end.
      else 
        assign
          tLevSAnt.SeqNr = int(entry(2, wTekst, ";")).
    end. /* NYE-STR */

    /* Sletter størrelser som ikke finnes lengre. */
    /* og retter opp sekvensnummeret.             */
    assign wLoop = 0.
    for each tLevSAnt by tLevSAnt.SeqNr:
       
      /* Kaster størrelsersom ikke lenger skal benyttes */
      if tLevSAnt.SeqNr = 99 then
        delete tLevSAnt.
      else do:
        /* Dette går nå bra fordi vi vet at sekvensnummer alltid */
        /* er likt eller høyere enn wLoop.                       */
        assign 
          wLoop         = wLoop + 1
          tLevSAnt.SeqNr = wLoop.
      end.
    end. 
    RUN InittmpStr.
    run VisPost.
  end. /* TRANSACTION */
END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME LevSort.SortID
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL LevSort.SortID Dialog-Frame
ON LEAVE OF LevSort.SortID IN FRAME Dialog-Frame /* Sortiment */
or "RETURN":U of LevSort.SortId or "TAB":U of LevSort.SortId
DO:
    IF CAN-FIND(bLevSort OF LevBas WHERE bLevSort.SortID = INPUT LevSort.SortID) THEN DO:
      message "Det finnes allerede et leverandørsortiment med dette ID!"
          view-as alert-box title "Lagringsfeil".
      APPLY "ENTRY" TO LevSort.SortID.
      return no-apply.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME LevSort.StrTypeID
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL LevSort.StrTypeID Dialog-Frame
ON LEAVE OF LevSort.StrTypeID IN FRAME Dialog-Frame /* Type */
or "RETURN":U of LevSort.StrTypeId or "TAB":U of LevSort.StrTypeId
DO:
  find StrType no-lock where
    StrType.StrTypeId = input LevSort.StrTypeId no-error.
  if available StrType then
    do:
      display 
        StrType.KortNavn
        StrType.Beskrivelse
      with frame Dialog-Frame.
    end.
  else do:
    message "Ugyldig størrelsestype!" view-as alert-box title "Melding".
    APPLY "ENTRY" TO LevSort.StrTypeID.
    return no-apply.
  end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dialog-Frame 


/* ***************************  Main Block  *************************** */

/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT eq ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.

/* Henter post */
IF wModus = "ENDRE" THEN DO:
find {&br-tabell} no-lock where
  recid({&br-tabell}) = wRecid no-error.
if available {&br-tabell} then 
  do: 
    {&FinnRelatertePoster}  
  end.
END.
IF NOT AVAIL LevBas THEN
    find LevBas no-lock where RECID(LevBas) = wLevBasRecid no-error.
on "TAB":U, "RETURN":U anywhere
  do:
    if Frame-Field = "Ant" then
      run VisTotal (input "I").
  end.
  
/* skärmrecorden */
create tmpStrDef.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN enable_UI.
  {lng.i} 
  if wModus = "Ny" then do:
    assign
      {&br-tabell}.{&KeyFelt}:sensitive  = true
      BUTTON-SokStrType:sensitive        = wStrTypeID = ?
      LevSort.StrTypeID:sensitive        = wStrTypeID = ?
      LevSort.Fri:sensitive              = FALSE.
      IF wStrTypeID <> ?  THEN LevSort.StrTypeID:SCREEN-VALUE = 
                                STRING(wStrTypeID).
  end.
  else do:
    assign
      {&br-tabell}.{&KeyFelt}:sensitive  = false
      BUTTON-SokStrType:sensitive        = false
      LevSort.StrTypeID:sensitive        = false
      LevSort.Fri:sensitive              = false.
      RUN InitTLevSAnt.
      RUN InittmpStr.
  end.
  run VisPost.
/*  run LastStorrelser. */
  view frame Dialog-Frame.
  WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

&IF DEFINED(UIB_IS_RUNNING) EQ 0 &THEN
 return wretur-verdi.
&else
 message wretur-verdi view-as alert-box.
&endif

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI Dialog-Frame  _DEFAULT-DISABLE
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
  HIDE FRAME Dialog-Frame.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI Dialog-Frame  _DEFAULT-ENABLE
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
  DISPLAY FI-Total 
      WITH FRAME Dialog-Frame.
  IF AVAILABLE LevSort THEN 
    DISPLAY LevSort.SortID LevSort.Beskrivelse LevSort.Fri LevSort.Merknad 
          LevSort.StrTypeID 
      WITH FRAME Dialog-Frame.
  IF AVAILABLE StrType THEN 
    DISPLAY StrType.KortNavn StrType.Beskrivelse 
      WITH FRAME Dialog-Frame.
  IF AVAILABLE tmpStrDef THEN 
    DISPLAY tmpStrDef.Str[1] tmpStrDef.Str[2] tmpStrDef.Str[3] tmpStrDef.Str[4] 
          tmpStrDef.Str[5] tmpStrDef.Str[6] tmpStrDef.Str[7] tmpStrDef.Str[8] 
          tmpStrDef.Str[9] tmpStrDef.Str[10] tmpStrDef.Str[11] tmpStrDef.Str[12] 
          tmpStrDef.Ant[1] tmpStrDef.Ant[2] tmpStrDef.Ant[3] tmpStrDef.Ant[4] 
          tmpStrDef.Ant[5] tmpStrDef.Ant[6] tmpStrDef.Ant[7] tmpStrDef.Ant[8] 
          tmpStrDef.Ant[9] tmpStrDef.Ant[10] tmpStrDef.Ant[11] tmpStrDef.Ant[12] 
          tmpStrDef.Str[13] tmpStrDef.Str[14] tmpStrDef.Str[15] 
          tmpStrDef.Str[16] tmpStrDef.Str[17] tmpStrDef.Str[18] 
          tmpStrDef.Str[19] tmpStrDef.Str[20] tmpStrDef.Str[21] 
          tmpStrDef.Str[22] tmpStrDef.Str[23] tmpStrDef.Str[24] 
          tmpStrDef.Ant[13] tmpStrDef.Ant[14] tmpStrDef.Ant[15] 
          tmpStrDef.Ant[16] tmpStrDef.Ant[17] tmpStrDef.Ant[18] 
          tmpStrDef.Ant[19] tmpStrDef.Ant[20] tmpStrDef.Ant[21] 
          tmpStrDef.Ant[22] tmpStrDef.Ant[23] tmpStrDef.Ant[24] 
          tmpStrDef.Str[25] tmpStrDef.Str[26] tmpStrDef.Str[27] 
          tmpStrDef.Str[28] tmpStrDef.Str[29] tmpStrDef.Str[30] 
          tmpStrDef.Str[31] tmpStrDef.Str[32] tmpStrDef.Str[33] 
          tmpStrDef.Str[34] tmpStrDef.Str[35] tmpStrDef.Str[36] 
          tmpStrDef.Ant[25] tmpStrDef.Ant[26] tmpStrDef.Ant[27] 
          tmpStrDef.Ant[28] tmpStrDef.Ant[29] tmpStrDef.Ant[30] 
          tmpStrDef.Ant[31] tmpStrDef.Ant[32] tmpStrDef.Ant[33] 
          tmpStrDef.Ant[34] tmpStrDef.Ant[35] tmpStrDef.Ant[36] 
          tmpStrDef.Str[37] tmpStrDef.Str[38] tmpStrDef.Str[39] 
          tmpStrDef.Str[40] tmpStrDef.Str[41] tmpStrDef.Str[42] 
          tmpStrDef.Str[43] tmpStrDef.Str[44] tmpStrDef.Str[45] 
          tmpStrDef.Str[46] tmpStrDef.Str[47] tmpStrDef.Str[48] 
          tmpStrDef.Ant[37] tmpStrDef.Ant[38] tmpStrDef.Ant[39] 
          tmpStrDef.Ant[40] tmpStrDef.Ant[41] tmpStrDef.Ant[42] 
          tmpStrDef.Ant[43] tmpStrDef.Ant[44] tmpStrDef.Ant[45] 
          tmpStrDef.Ant[46] tmpStrDef.Ant[47] tmpStrDef.Ant[48] 
      WITH FRAME Dialog-Frame.
  ENABLE LevSort.Beskrivelse LevSort.Merknad Btn_OK Btn_Cancel 
         BUTTON-Storrelser Btn_Help RECT-3 RECT-4 
      WITH FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitTLevSAnt Dialog-Frame 
PROCEDURE InitTLevSAnt :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
FOR EACH LevSAnt OF LevSort:
    CREATE tLevSAnt.
    BUFFER-COPY LevSAnt TO tLevSAnt.
    RELEASE tLevSant.
END.
    /*        LevSAnt.LevNr     = LevSort.LevNr
            LevSAnt.SortId    = LevSort.SortId
            LevSAnt.SeqNr     = wLoop
            LevSAnt.SoStorl   = tmpStrDef.Str[wLoop] /* Oppdateres automatisk */
            LevSAnt.SoAnt     = input tmpStrDef.Ant[wLoop]. /* Brukerinput */
*/
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InittmpStr Dialog-Frame 
PROCEDURE InittmpStr :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    ASSIGN tmpStrDef.Str = ""
           tmpStrDef.Ant = 0.
    FOR EACH tLevSAnt:
        ASSIGN tmpStrDef.Str[tLevSAnt.SeqNr] = tLevSAnt.SoStorl
               tmpStrDef.Ant[tLevSAnt.SeqNr] = tLevSAnt.SoAnt.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LagraTmpSoAnt Dialog-Frame 
PROCEDURE LagraTmpSoAnt :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DO WITH FRAME {&FRAME-NAME}:
      ASSIGN tmpStrDef.Str[1 for 48]
             tmpStrDef.Ant[1 for 48].

      FOR EACH tLevSAnt:
          ASSIGN tLevSAnt.SoAnt = tmpStrDef.Ant[tLevSAnt.SeqNr].
      END.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LagrePost Dialog-Frame 
PROCEDURE LagrePost :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
do with frame Dialog-Frame transaction:
  /* Sjekker nye poster. */
  if wModus = "Ny" then
    do:
      {&SjekkOmPostFinnes}
      create {&br-tabell}.
      assign 
        {&OptFind}
        {&br-tabell}.LevNr      = LevBas.LevNr /* wLevNr */
        {&br-tabell}.{&KeyFelt} = {&DataType}({&br-tabell}.{&KeyFelt}:screen-value)
        wRecid   = recid({&br-tabell}).
    end.
  else 
    find {&br-tabell} Exclusive-lock where
      recid({&br-tabell}) = wRecid no-error.

  assign 
   {&AssignFelter}.

  {&TillegsAssign}
end. /* TRANSACTION */    

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LagreStorrelser Dialog-Frame 
PROCEDURE LagreStorrelser :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  def var wRetur-Verdi as char initial "<avbryt>" no-undo.
  
  do WITH FRAME {&FRAME-NAME}:
    /* Først kaster vi alle de gamle størrelsene */
    for each LevSAnt of LevSort exclusive-lock:
      delete LevSAnt.
    end.

  ASSIGN tmpStrDef.Str[1 for 48]
         tmpStrDef.Ant[1 for 48].

  FOR EACH tLevSAnt:
      BUFFER-COPY tLevSAnt TO LevSAnt.
      RELEASE LevSAnt.
  END.
    assign wRetur-Verdi = "OK".
  end.
  
  return wRetur-Verdi.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LastStorrelser Dialog-Frame 
PROCEDURE LastStorrelser :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  def var wLoop as int no-undo.
  for each LevSAnt of LevSort no-lock
    by LevSAnt.LevNr
    by LevSAnt.SortId
    by LevSAnt.SeqNr:
    assign 
      tmpStrDef.Str[LevSAnt.SeqNr] = LevSAnt.SoStorl
      tmpStrDef.Ant[LevSAnt.SeqNr] = LevSAnt.SoAnt
      FI-Total                     = FI-Total + LevSAnt.Soant.
    display 
      tmpStrDef.Str[LevSAnt.SeqNr]
      tmpStrDef.Ant[LevSAnt.SeqNr]
    with frame Dialog-Frame.
    ENABLE tmpStrDef.Ant[LevSAnt.SeqNr] with frame Dialog-Frame.
  end.    
  
  run VisTotal (input "B").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE VisPost Dialog-Frame 
PROCEDURE VisPost :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  disable tmpStrDef.Ant[1 for 48] with frame Dialog-Frame.
  display tmpStrDef.Str[1 for 48]
          tmpStrDef.Ant[1 for 48] with frame Dialog-Frame.

  for each tLevSAnt
    by tLevSAnt.SeqNr:
    ENABLE tmpStrDef.Ant[tLevSAnt.SeqNr] with frame Dialog-Frame.
  end.    
  
  run VisTotal (input "B").

/*  display 
    {&VisPoster}
  with frame Dialog-Frame.
  {&VisAndreData}
*/
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE VisTotal Dialog-Frame 
PROCEDURE VisTotal :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  def input parameter wModus as char.
  
  def var wLoop as int no-undo.

  if not available tmpStrDef then
    find tmpStrDef where
      recid(tmpStrDef) = wtmpStrDefRecid exclusive-lock.

  assign
    FI-Total = 0.
    
  do wLoop = 1 to 48:
    assign 
      FI-Total = FI-Total + FinnAntall(input wModus, input wLoop).

  end.
  display 
    FI-Total
    "" when FI-Total = 0 @ FI-Total
  with frame Dialog-Frame.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION FinnAntall Dialog-Frame 
FUNCTION FinnAntall RETURNS INTEGER
  ( input wModus as char,
    input wLoop  as int ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  def var wRetur-Verdi as int no-undo.
  
  if wModus = "B" then
    case wLoop:
      when  1 then wRetur-Verdi = tmpStrDef.Ant[ 1].
      when  2 then wRetur-Verdi = tmpStrDef.Ant[ 2].
      when  3 then wRetur-Verdi = tmpStrDef.Ant[ 3].
      when  4 then wRetur-Verdi = tmpStrDef.Ant[ 4].
      when  5 then wRetur-Verdi = tmpStrDef.Ant[ 5].
      when  6 then wRetur-Verdi = tmpStrDef.Ant[ 6].
      when  7 then wRetur-Verdi = tmpStrDef.Ant[ 7].
      when  8 then wRetur-Verdi = tmpStrDef.Ant[ 8].
      when  9 then wRetur-Verdi = tmpStrDef.Ant[ 9].
      when 10 then wRetur-Verdi = tmpStrDef.Ant[10].
      when 11 then wRetur-Verdi = tmpStrDef.Ant[11].
      when 12 then wRetur-Verdi = tmpStrDef.Ant[12].
      when 13 then wRetur-Verdi = tmpStrDef.Ant[13].
      when 14 then wRetur-Verdi = tmpStrDef.Ant[14].
      when 15 then wRetur-Verdi = tmpStrDef.Ant[15].
      when 16 then wRetur-Verdi = tmpStrDef.Ant[16].
      when 17 then wRetur-Verdi = tmpStrDef.Ant[17].
      when 18 then wRetur-Verdi = tmpStrDef.Ant[18].
      when 19 then wRetur-Verdi = tmpStrDef.Ant[19].
      when 20 then wRetur-Verdi = tmpStrDef.Ant[20].
      when 21 then wRetur-Verdi = tmpStrDef.Ant[21].
      when 22 then wRetur-Verdi = tmpStrDef.Ant[22].
      when 23 then wRetur-Verdi = tmpStrDef.Ant[23].
      when 24 then wRetur-Verdi = tmpStrDef.Ant[24].
      when 25 then wRetur-Verdi = tmpStrDef.Ant[25].
      when 26 then wRetur-Verdi = tmpStrDef.Ant[26].
      when 27 then wRetur-Verdi = tmpStrDef.Ant[27].
      when 28 then wRetur-Verdi = tmpStrDef.Ant[28].
      when 29 then wRetur-Verdi = tmpStrDef.Ant[29].
      when 30 then wRetur-Verdi = tmpStrDef.Ant[30].
      when 31 then wRetur-Verdi = tmpStrDef.Ant[31].
      when 32 then wRetur-Verdi = tmpStrDef.Ant[32].
      when 33 then wRetur-Verdi = tmpStrDef.Ant[33].
      when 34 then wRetur-Verdi = tmpStrDef.Ant[34].
      when 35 then wRetur-Verdi = tmpStrDef.Ant[35].
      when 36 then wRetur-Verdi = tmpStrDef.Ant[36].
      when 37 then wRetur-Verdi = tmpStrDef.Ant[37].
      when 38 then wRetur-Verdi = tmpStrDef.Ant[38].
      when 39 then wRetur-Verdi = tmpStrDef.Ant[39].
      when 40 then wRetur-Verdi = tmpStrDef.Ant[40].
      when 41 then wRetur-Verdi = tmpStrDef.Ant[41].
      when 42 then wRetur-Verdi = tmpStrDef.Ant[42].
      when 43 then wRetur-Verdi = tmpStrDef.Ant[43].
      when 44 then wRetur-Verdi = tmpStrDef.Ant[44].
      when 45 then wRetur-Verdi = tmpStrDef.Ant[45].
      when 46 then wRetur-Verdi = tmpStrDef.Ant[46].
      when 47 then wRetur-Verdi = tmpStrDef.Ant[47].
      when 48 then wRetur-Verdi = tmpStrDef.Ant[48].
    end case.
  
  else 
    case wLoop:
      when  1 then wRetur-Verdi = int(tmpStrDef.Ant[ 1]:screen-value in frame Dialog-Frame).    
      when  2 then wRetur-Verdi = int(tmpStrDef.Ant[ 2]:screen-value in frame Dialog-Frame).    
      when  3 then wRetur-Verdi = int(tmpStrDef.Ant[ 3]:screen-value in frame Dialog-Frame).    
      when  4 then wRetur-Verdi = int(tmpStrDef.Ant[ 4]:screen-value in frame Dialog-Frame).    
      when  5 then wRetur-Verdi = int(tmpStrDef.Ant[ 5]:screen-value in frame Dialog-Frame).    
      when  6 then wRetur-Verdi = int(tmpStrDef.Ant[ 6]:screen-value in frame Dialog-Frame).    
      when  7 then wRetur-Verdi = int(tmpStrDef.Ant[ 7]:screen-value in frame Dialog-Frame).    
      when  8 then wRetur-Verdi = int(tmpStrDef.Ant[ 8]:screen-value in frame Dialog-Frame).    
      when  9 then wRetur-Verdi = int(tmpStrDef.Ant[ 9]:screen-value in frame Dialog-Frame).    
      when 10 then wRetur-Verdi = int(tmpStrDef.Ant[10]:screen-value in frame Dialog-Frame).    
      when 11 then wRetur-Verdi = int(tmpStrDef.Ant[11]:screen-value in frame Dialog-Frame).    
      when 12 then wRetur-Verdi = int(tmpStrDef.Ant[12]:screen-value in frame Dialog-Frame).    
      when 13 then wRetur-Verdi = int(tmpStrDef.Ant[13]:screen-value in frame Dialog-Frame).    
      when 14 then wRetur-Verdi = int(tmpStrDef.Ant[14]:screen-value in frame Dialog-Frame).    
      when 15 then wRetur-Verdi = int(tmpStrDef.Ant[15]:screen-value in frame Dialog-Frame).    
      when 16 then wRetur-Verdi = int(tmpStrDef.Ant[16]:screen-value in frame Dialog-Frame).    
      when 17 then wRetur-Verdi = int(tmpStrDef.Ant[17]:screen-value in frame Dialog-Frame).    
      when 18 then wRetur-Verdi = int(tmpStrDef.Ant[18]:screen-value in frame Dialog-Frame).    
      when 19 then wRetur-Verdi = int(tmpStrDef.Ant[19]:screen-value in frame Dialog-Frame).    
      when 20 then wRetur-Verdi = int(tmpStrDef.Ant[20]:screen-value in frame Dialog-Frame).    
      when 21 then wRetur-Verdi = int(tmpStrDef.Ant[21]:screen-value in frame Dialog-Frame).    
      when 22 then wRetur-Verdi = int(tmpStrDef.Ant[22]:screen-value in frame Dialog-Frame).    
      when 23 then wRetur-Verdi = int(tmpStrDef.Ant[23]:screen-value in frame Dialog-Frame).    
      when 24 then wRetur-Verdi = int(tmpStrDef.Ant[24]:screen-value in frame Dialog-Frame).    
      when 25 then wRetur-Verdi = int(tmpStrDef.Ant[25]:screen-value in frame Dialog-Frame).    
      when 26 then wRetur-Verdi = int(tmpStrDef.Ant[26]:screen-value in frame Dialog-Frame).    
      when 27 then wRetur-Verdi = int(tmpStrDef.Ant[27]:screen-value in frame Dialog-Frame).    
      when 28 then wRetur-Verdi = int(tmpStrDef.Ant[28]:screen-value in frame Dialog-Frame).    
      when 29 then wRetur-Verdi = int(tmpStrDef.Ant[29]:screen-value in frame Dialog-Frame).    
      when 30 then wRetur-Verdi = int(tmpStrDef.Ant[30]:screen-value in frame Dialog-Frame).    
      when 31 then wRetur-Verdi = int(tmpStrDef.Ant[31]:screen-value in frame Dialog-Frame).    
      when 32 then wRetur-Verdi = int(tmpStrDef.Ant[32]:screen-value in frame Dialog-Frame).    
      when 33 then wRetur-Verdi = int(tmpStrDef.Ant[33]:screen-value in frame Dialog-Frame).    
      when 34 then wRetur-Verdi = int(tmpStrDef.Ant[34]:screen-value in frame Dialog-Frame).    
      when 35 then wRetur-Verdi = int(tmpStrDef.Ant[35]:screen-value in frame Dialog-Frame).    
      when 36 then wRetur-Verdi = int(tmpStrDef.Ant[36]:screen-value in frame Dialog-Frame).    
      when 37 then wRetur-Verdi = int(tmpStrDef.Ant[37]:screen-value in frame Dialog-Frame).    
      when 38 then wRetur-Verdi = int(tmpStrDef.Ant[38]:screen-value in frame Dialog-Frame).    
      when 39 then wRetur-Verdi = int(tmpStrDef.Ant[39]:screen-value in frame Dialog-Frame).    
      when 40 then wRetur-Verdi = int(tmpStrDef.Ant[40]:screen-value in frame Dialog-Frame).    
      when 41 then wRetur-Verdi = int(tmpStrDef.Ant[41]:screen-value in frame Dialog-Frame).    
      when 42 then wRetur-Verdi = int(tmpStrDef.Ant[42]:screen-value in frame Dialog-Frame).    
      when 43 then wRetur-Verdi = int(tmpStrDef.Ant[43]:screen-value in frame Dialog-Frame).    
      when 44 then wRetur-Verdi = int(tmpStrDef.Ant[44]:screen-value in frame Dialog-Frame).    
      when 45 then wRetur-Verdi = int(tmpStrDef.Ant[45]:screen-value in frame Dialog-Frame).    
      when 46 then wRetur-Verdi = int(tmpStrDef.Ant[46]:screen-value in frame Dialog-Frame).    
      when 47 then wRetur-Verdi = int(tmpStrDef.Ant[47]:screen-value in frame Dialog-Frame).    
      when 48 then wRetur-Verdi = int(tmpStrDef.Ant[48]:screen-value in frame Dialog-Frame).    
    end case.

  RETURN wRetur-Verdi. /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

