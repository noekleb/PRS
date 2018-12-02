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
  DEF VAR wStrTypeID LIKE StrType.StrTypeID NO-UNDO.
  FIND FIRST StrType NO-LOCK NO-ERROR.
  ASSIGN
   wStrTypeID = StrType.StrTypeId.
&ELSE
  DEF INPUT PARAMETER wStrTypeID LIKE StrType.StrTypeID NO-UNDO.
&ENDIF

/* Local Variable Definitions ---                                       */
DEF VAR wLoop  AS INT   NO-UNDO.
DEF VAR wRecid AS RECID NO-UNDO.
DEF VAR wStorl AS CHAR  NO-UNDO.

{runlib.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES StrType tmpStrDef

/* Definitions for DIALOG-BOX Dialog-Frame                              */
&Scoped-define FIELDS-IN-QUERY-Dialog-Frame StrType.StrTypeID ~
StrType.KortNavn StrType.Beskrivelse 
&Scoped-define QUERY-STRING-Dialog-Frame FOR EACH StrType SHARE-LOCK, ~
      EACH tmpStrDef WHERE TRUE /* Join to StrType incomplete */ SHARE-LOCK
&Scoped-define OPEN-QUERY-Dialog-Frame OPEN QUERY Dialog-Frame FOR EACH StrType SHARE-LOCK, ~
      EACH tmpStrDef WHERE TRUE /* Join to StrType incomplete */ SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-Dialog-Frame StrType tmpStrDef
&Scoped-define FIRST-TABLE-IN-QUERY-Dialog-Frame StrType
&Scoped-define SECOND-TABLE-IN-QUERY-Dialog-Frame tmpStrDef


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-3 RECT-4 Btn_OK B-Tag 
&Scoped-Define DISPLAYED-FIELDS StrType.StrTypeID StrType.KortNavn ~
StrType.Beskrivelse tmpStrDef.Str[1] tmpStrDef.Str[2] tmpStrDef.Str[3] ~
tmpStrDef.Str[4] tmpStrDef.Str[5] tmpStrDef.Str[6] tmpStrDef.Str[7] ~
tmpStrDef.Str[8] tmpStrDef.Str[9] tmpStrDef.Str[10] tmpStrDef.Str[11] ~
tmpStrDef.Str[12] tmpStrDef.Str[13] tmpStrDef.Str[14] tmpStrDef.Str[15] ~
tmpStrDef.Str[16] tmpStrDef.Str[17] tmpStrDef.Str[18] tmpStrDef.Str[19] ~
tmpStrDef.Str[20] tmpStrDef.Str[21] tmpStrDef.Str[22] tmpStrDef.Str[23] ~
tmpStrDef.Str[24] tmpStrDef.Str[25] tmpStrDef.Str[26] tmpStrDef.Str[27] ~
tmpStrDef.Str[28] tmpStrDef.Str[29] tmpStrDef.Str[30] tmpStrDef.Str[31] ~
tmpStrDef.Str[32] tmpStrDef.Str[33] tmpStrDef.Str[34] tmpStrDef.Str[35] ~
tmpStrDef.Str[36] tmpStrDef.Str[37] tmpStrDef.Str[38] tmpStrDef.Str[39] ~
tmpStrDef.Str[40] tmpStrDef.Str[41] tmpStrDef.Str[42] tmpStrDef.Str[43] ~
tmpStrDef.Str[44] tmpStrDef.Str[45] tmpStrDef.Str[46] tmpStrDef.Str[47] ~
tmpStrDef.Str[48] tmpStrDef.Str[49] tmpStrDef.Str[50] tmpStrDef.Str[51] ~
tmpStrDef.Str[52] tmpStrDef.Str[53] tmpStrDef.Str[54] tmpStrDef.Str[55] ~
tmpStrDef.Str[56] tmpStrDef.Str[57] tmpStrDef.Str[58] tmpStrDef.Str[59] ~
tmpStrDef.Str[60] tmpStrDef.Str[61] tmpStrDef.Str[62] tmpStrDef.Str[63] ~
tmpStrDef.Str[64] tmpStrDef.Str[65] tmpStrDef.Str[66] tmpStrDef.Str[67] ~
tmpStrDef.Str[68] tmpStrDef.Str[69] tmpStrDef.Str[70] tmpStrDef.Str[71] ~
tmpStrDef.Str[72] tmpStrDef.Str[73] tmpStrDef.Str[74] tmpStrDef.Str[75] ~
tmpStrDef.Str[76] tmpStrDef.Str[77] tmpStrDef.Str[78] tmpStrDef.Str[79] ~
tmpStrDef.Str[80] tmpStrDef.Str[81] tmpStrDef.Str[82] tmpStrDef.Str[83] ~
tmpStrDef.Str[84] tmpStrDef.Str[85] tmpStrDef.Str[86] tmpStrDef.Str[87] ~
tmpStrDef.Str[88] tmpStrDef.Str[89] tmpStrDef.Str[90] tmpStrDef.Str[91] ~
tmpStrDef.Str[92] tmpStrDef.Str[93] tmpStrDef.Str[94] tmpStrDef.Str[95] ~
tmpStrDef.Str[96] tmpStrDef.Str[97] tmpStrDef.Str[98] tmpStrDef.Str[99] ~
tmpStrDef.Str[100] 
&Scoped-define DISPLAYED-TABLES StrType tmpStrDef
&Scoped-define FIRST-DISPLAYED-TABLE StrType
&Scoped-define SECOND-DISPLAYED-TABLE tmpStrDef


/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD HentStr Dialog-Frame 
FUNCTION HentStr RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON B-Tag 
     LABEL "Velg størrelser" 
     SIZE 26.8 BY 1.14.

DEFINE BUTTON Btn_Help 
     LABEL "&Hjelp" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON Btn_OK AUTO-GO 
     LABEL "OK" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE RECTANGLE RECT-3
     EDGE-PIXELS 4 GRAPHIC-EDGE  NO-FILL   
     SIZE 206 BY 24.33.

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 203.6 BY .1.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Dialog-Frame FOR 
      StrType, 
      tmpStrDef SCROLLING.
&ANALYZE-RESUME

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     StrType.StrTypeID AT ROW 1.57 COL 16.2 COLON-ALIGNED FORMAT ">>>>>>9"
          VIEW-AS FILL-IN 
          SIZE 10.8 BY 1
     StrType.KortNavn AT ROW 1.57 COL 27 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 13.6 BY 1
     StrType.Beskrivelse AT ROW 1.57 COL 41 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 78 BY 1
     tmpStrDef.Str[1] AT ROW 3.29 COL 16 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 15.6 BY 1
     tmpStrDef.Str[2] AT ROW 3.29 COL 31.6 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 15.6 BY 1
     tmpStrDef.Str[3] AT ROW 3.29 COL 47.2 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 15.6 BY 1
     tmpStrDef.Str[4] AT ROW 3.29 COL 62.8 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 15.6 BY 1
     tmpStrDef.Str[5] AT ROW 3.29 COL 78.4 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 15.6 BY 1
     tmpStrDef.Str[6] AT ROW 3.29 COL 94 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 15.6 BY 1
     tmpStrDef.Str[7] AT ROW 3.29 COL 109.6 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 15.6 BY 1
     tmpStrDef.Str[8] AT ROW 3.29 COL 125.2 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 15.6 BY 1
     tmpStrDef.Str[9] AT ROW 3.29 COL 140.8 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 15.6 BY 1
     tmpStrDef.Str[10] AT ROW 3.29 COL 156.4 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 15.6 BY 1
     tmpStrDef.Str[11] AT ROW 3.29 COL 172 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 15.6 BY 1
     tmpStrDef.Str[12] AT ROW 3.29 COL 187.6 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 15.6 BY 1
     tmpStrDef.Str[13] AT ROW 5.67 COL 16 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 15.6 BY 1
     tmpStrDef.Str[14] AT ROW 5.67 COL 31.6 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 15.6 BY 1
     tmpStrDef.Str[15] AT ROW 5.67 COL 47.2 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 15.6 BY 1
     tmpStrDef.Str[16] AT ROW 5.67 COL 62.8 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 15.6 BY 1
     tmpStrDef.Str[17] AT ROW 5.67 COL 78.4 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 15.6 BY 1
     tmpStrDef.Str[18] AT ROW 5.67 COL 94 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 15.6 BY 1
     tmpStrDef.Str[19] AT ROW 5.67 COL 109.6 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 15.6 BY 1
     tmpStrDef.Str[20] AT ROW 5.67 COL 125.2 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 15.6 BY 1
     tmpStrDef.Str[21] AT ROW 5.67 COL 140.8 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 15.6 BY 1
     tmpStrDef.Str[22] AT ROW 5.67 COL 156.4 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 15.6 BY 1
     tmpStrDef.Str[23] AT ROW 5.67 COL 172 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 15.6 BY 1
     tmpStrDef.Str[24] AT ROW 5.67 COL 187.6 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 15.6 BY 1
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE .

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME Dialog-Frame
     tmpStrDef.Str[25] AT ROW 8.05 COL 16 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 15.6 BY 1
     tmpStrDef.Str[26] AT ROW 8.05 COL 31.6 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 15.6 BY 1
     tmpStrDef.Str[27] AT ROW 8.05 COL 47.2 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 15.6 BY 1
     tmpStrDef.Str[28] AT ROW 8.05 COL 62.8 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 15.6 BY 1
     tmpStrDef.Str[29] AT ROW 8.05 COL 78.4 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 15.6 BY 1
     tmpStrDef.Str[30] AT ROW 8.05 COL 94 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 15.6 BY 1
     tmpStrDef.Str[31] AT ROW 8.05 COL 109.6 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 15.6 BY 1
     tmpStrDef.Str[32] AT ROW 8.05 COL 125.2 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 15.6 BY 1
     tmpStrDef.Str[33] AT ROW 8.05 COL 140.8 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 15.6 BY 1
     tmpStrDef.Str[34] AT ROW 8.05 COL 156.4 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 15.6 BY 1
     tmpStrDef.Str[35] AT ROW 8.05 COL 172 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 15.6 BY 1
     tmpStrDef.Str[36] AT ROW 8.05 COL 187.6 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 15.6 BY 1
     tmpStrDef.Str[37] AT ROW 10.33 COL 16 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 15.6 BY 1
     tmpStrDef.Str[38] AT ROW 10.33 COL 31.6 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 15.6 BY 1
     tmpStrDef.Str[39] AT ROW 10.33 COL 47.2 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 15.6 BY 1
     tmpStrDef.Str[40] AT ROW 10.33 COL 62.8 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 15.6 BY 1
     tmpStrDef.Str[41] AT ROW 10.33 COL 78.4 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 15.6 BY 1
     tmpStrDef.Str[42] AT ROW 10.33 COL 94 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 15.6 BY 1
     tmpStrDef.Str[43] AT ROW 10.33 COL 109.6 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 15.6 BY 1
     tmpStrDef.Str[44] AT ROW 10.33 COL 125.2 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 15.6 BY 1
     tmpStrDef.Str[45] AT ROW 10.33 COL 140.8 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 15.6 BY 1
     tmpStrDef.Str[46] AT ROW 10.33 COL 156.4 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 15.6 BY 1
     tmpStrDef.Str[47] AT ROW 10.33 COL 172 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 15.6 BY 1
     tmpStrDef.Str[48] AT ROW 10.33 COL 187.6 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 15.6 BY 1
     tmpStrDef.Str[49] AT ROW 12.19 COL 18 NO-LABEL 
          VIEW-AS FILL-IN 
          SIZE 15.6 BY 1
     tmpStrDef.Str[50] AT ROW 12.19 COL 31.6 COLON-ALIGNED NO-LABEL 
          VIEW-AS FILL-IN 
          SIZE 15.6 BY 1
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE .

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME Dialog-Frame
     tmpStrDef.Str[51] AT ROW 12.19 COL 47.2 COLON-ALIGNED NO-LABEL 
          VIEW-AS FILL-IN 
          SIZE 15.6 BY 1
     tmpStrDef.Str[52] AT ROW 12.19 COL 62.8 COLON-ALIGNED NO-LABEL 
          VIEW-AS FILL-IN 
          SIZE 15.6 BY 1
     tmpStrDef.Str[53] AT ROW 12.19 COL 78.4 COLON-ALIGNED NO-LABEL 
          VIEW-AS FILL-IN 
          SIZE 15.6 BY 1
     tmpStrDef.Str[54] AT ROW 12.19 COL 94 COLON-ALIGNED NO-LABEL 
          VIEW-AS FILL-IN 
          SIZE 15.6 BY 1
     tmpStrDef.Str[55] AT ROW 12.19 COL 109.6 COLON-ALIGNED NO-LABEL 
          VIEW-AS FILL-IN 
          SIZE 15.6 BY 1
     tmpStrDef.Str[56] AT ROW 12.19 COL 125.2 COLON-ALIGNED NO-LABEL 
          VIEW-AS FILL-IN 
          SIZE 15.6 BY 1
     tmpStrDef.Str[57] AT ROW 12.19 COL 140.8 COLON-ALIGNED NO-LABEL 
          VIEW-AS FILL-IN 
          SIZE 15.6 BY 1
     tmpStrDef.Str[58] AT ROW 12.19 COL 156.4 COLON-ALIGNED NO-LABEL 
          VIEW-AS FILL-IN 
          SIZE 15.6 BY 1
     tmpStrDef.Str[59] AT ROW 12.19 COL 172 COLON-ALIGNED NO-LABEL 
          VIEW-AS FILL-IN 
          SIZE 15.6 BY 1
     tmpStrDef.Str[60] AT ROW 12.19 COL 187.6 COLON-ALIGNED NO-LABEL 
          VIEW-AS FILL-IN 
          SIZE 15.6 BY 1
     tmpStrDef.Str[61] AT ROW 14.1 COL 18 NO-LABEL 
          VIEW-AS FILL-IN 
          SIZE 15.6 BY 1
     tmpStrDef.Str[62] AT ROW 14.1 COL 31.6 COLON-ALIGNED NO-LABEL 
          VIEW-AS FILL-IN 
          SIZE 15.6 BY 1
     tmpStrDef.Str[63] AT ROW 14.1 COL 47.2 COLON-ALIGNED NO-LABEL 
          VIEW-AS FILL-IN 
          SIZE 15.6 BY 1
     tmpStrDef.Str[64] AT ROW 14.1 COL 62.8 COLON-ALIGNED NO-LABEL 
          VIEW-AS FILL-IN 
          SIZE 15.6 BY 1
     tmpStrDef.Str[65] AT ROW 14.1 COL 78.4 COLON-ALIGNED NO-LABEL 
          VIEW-AS FILL-IN 
          SIZE 15.6 BY 1
     tmpStrDef.Str[66] AT ROW 14.1 COL 94 COLON-ALIGNED NO-LABEL 
          VIEW-AS FILL-IN 
          SIZE 15.6 BY 1
     tmpStrDef.Str[67] AT ROW 14.1 COL 109.6 COLON-ALIGNED NO-LABEL 
          VIEW-AS FILL-IN 
          SIZE 15.6 BY 1
     tmpStrDef.Str[68] AT ROW 14.1 COL 125.2 COLON-ALIGNED NO-LABEL 
          VIEW-AS FILL-IN 
          SIZE 15.6 BY 1
     tmpStrDef.Str[69] AT ROW 14.1 COL 140.8 COLON-ALIGNED NO-LABEL 
          VIEW-AS FILL-IN 
          SIZE 15.6 BY 1
     tmpStrDef.Str[70] AT ROW 14.1 COL 156.4 COLON-ALIGNED NO-LABEL 
          VIEW-AS FILL-IN 
          SIZE 15.6 BY 1
     tmpStrDef.Str[71] AT ROW 14.1 COL 172 COLON-ALIGNED NO-LABEL 
          VIEW-AS FILL-IN 
          SIZE 15.6 BY 1
     tmpStrDef.Str[72] AT ROW 14.1 COL 187.6 COLON-ALIGNED NO-LABEL 
           VIEW-AS FILL-IN 
          SIZE 15.6 BY 1
     tmpStrDef.Str[73] AT ROW 16 COL 18 NO-LABEL 
          VIEW-AS FILL-IN 
          SIZE 15.6 BY 1
     tmpStrDef.Str[74] AT ROW 16 COL 31.6 COLON-ALIGNED NO-LABEL 
          VIEW-AS FILL-IN 
          SIZE 15.6 BY 1
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE .

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME Dialog-Frame
     tmpStrDef.Str[75] AT ROW 16 COL 47.2 COLON-ALIGNED NO-LABEL 
          VIEW-AS FILL-IN 
          SIZE 15.6 BY 1
     tmpStrDef.Str[76] AT ROW 16 COL 62.8 COLON-ALIGNED NO-LABEL 
          VIEW-AS FILL-IN 
          SIZE 15.6 BY 1
     tmpStrDef.Str[77] AT ROW 16 COL 78.4 COLON-ALIGNED NO-LABEL 
          VIEW-AS FILL-IN 
          SIZE 15.6 BY 1
     tmpStrDef.Str[78] AT ROW 16 COL 94 COLON-ALIGNED NO-LABEL 
          VIEW-AS FILL-IN 
          SIZE 15.6 BY 1
     tmpStrDef.Str[79] AT ROW 16 COL 109.6 COLON-ALIGNED NO-LABEL 
          VIEW-AS FILL-IN 
          SIZE 15.6 BY 1
     tmpStrDef.Str[80] AT ROW 16 COL 125.2 COLON-ALIGNED NO-LABEL 
          VIEW-AS FILL-IN 
          SIZE 15.6 BY 1
     tmpStrDef.Str[81] AT ROW 16 COL 140.8 COLON-ALIGNED NO-LABEL 
          VIEW-AS FILL-IN 
          SIZE 15.6 BY 1
     tmpStrDef.Str[82] AT ROW 16 COL 156.4 COLON-ALIGNED NO-LABEL 
          VIEW-AS FILL-IN 
          SIZE 15.6 BY 1
     tmpStrDef.Str[83] AT ROW 16 COL 172 COLON-ALIGNED NO-LABEL 
          VIEW-AS FILL-IN 
          SIZE 15.6 BY 1
     tmpStrDef.Str[84] AT ROW 16 COL 187.6 COLON-ALIGNED NO-LABEL 
          VIEW-AS FILL-IN 
          SIZE 15.6 BY 1
     tmpStrDef.Str[85] AT ROW 17.91 COL 18 NO-LABEL 
          VIEW-AS FILL-IN 
          SIZE 15.6 BY 1
     tmpStrDef.Str[86] AT ROW 17.91 COL 31.6 COLON-ALIGNED NO-LABEL 
          VIEW-AS FILL-IN 
          SIZE 15.6 BY 1
     tmpStrDef.Str[87] AT ROW 17.91 COL 47.2 COLON-ALIGNED NO-LABEL 
          VIEW-AS FILL-IN 
          SIZE 15.6 BY 1
     tmpStrDef.Str[88] AT ROW 17.91 COL 62.8 COLON-ALIGNED NO-LABEL 
          VIEW-AS FILL-IN 
          SIZE 15.6 BY 1
     tmpStrDef.Str[89] AT ROW 17.91 COL 78.4 COLON-ALIGNED NO-LABEL 
          VIEW-AS FILL-IN 
          SIZE 15.6 BY 1
     tmpStrDef.Str[90] AT ROW 17.91 COL 94 COLON-ALIGNED NO-LABEL 
          VIEW-AS FILL-IN 
          SIZE 15.6 BY 1
     tmpStrDef.Str[91] AT ROW 17.91 COL 109.6 COLON-ALIGNED NO-LABEL 
          VIEW-AS FILL-IN 
          SIZE 15.6 BY 1
     tmpStrDef.Str[92] AT ROW 17.91 COL 125.2 COLON-ALIGNED NO-LABEL 
          VIEW-AS FILL-IN 
          SIZE 15.6 BY 1
     tmpStrDef.Str[93] AT ROW 17.91 COL 140.8 COLON-ALIGNED NO-LABEL 
          VIEW-AS FILL-IN 
          SIZE 15.6 BY 1
     tmpStrDef.Str[94] AT ROW 17.91 COL 156.4 COLON-ALIGNED NO-LABEL 
          VIEW-AS FILL-IN 
          SIZE 15.6 BY 1
     tmpStrDef.Str[95] AT ROW 17.91 COL 172 COLON-ALIGNED NO-LABEL 
          VIEW-AS FILL-IN 
          SIZE 15.6 BY 1
     tmpStrDef.Str[96] AT ROW 17.91 COL 187.6 COLON-ALIGNED NO-LABEL 
          VIEW-AS FILL-IN 
          SIZE 15.6 BY 1
     tmpStrDef.Str[97] AT ROW 19.81 COL 18 NO-LABEL 
          VIEW-AS FILL-IN 
          SIZE 15.6 BY 1
     tmpStrDef.Str[98] AT ROW 19.81 COL 31.6 COLON-ALIGNED NO-LABEL 
          VIEW-AS FILL-IN 
          SIZE 15.6 BY 1
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE .

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME Dialog-Frame
     tmpStrDef.Str[99] AT ROW 19.81 COL 47.2 COLON-ALIGNED NO-LABEL 
          VIEW-AS FILL-IN 
          SIZE 15.6 BY 1
     tmpStrDef.Str[100] AT ROW 19.81 COL 62.8 COLON-ALIGNED NO-LABEL 
          VIEW-AS FILL-IN 
          SIZE 15.6 BY 1
     Btn_OK AT ROW 25.76 COL 2.6
     B-Tag AT ROW 25.76 COL 20.2
     Btn_Help AT ROW 25.76 COL 192
     "Størrelse:" VIEW-AS TEXT
          SIZE 10.4 BY .62 AT ROW 20 COL 7 
     "Størrelse:" VIEW-AS TEXT
          SIZE 10.4 BY .62 AT ROW 18.05 COL 7 
     "Størrelse:" VIEW-AS TEXT
          SIZE 10.4 BY .62 AT ROW 16.24 COL 7 
     "Størrelse:" VIEW-AS TEXT
          SIZE 10.4 BY .62 AT ROW 14.33 COL 7 
     "Størrelse:" VIEW-AS TEXT
          SIZE 10.4 BY .62 AT ROW 12.38 COL 7 
     "Størrelse:" VIEW-AS TEXT
          SIZE 10.4 BY .62 AT ROW 3.57 COL 7
     "Størrelse:" VIEW-AS TEXT
          SIZE 10.4 BY .62 AT ROW 8.33 COL 7
     "Størrelse:" VIEW-AS TEXT
          SIZE 10.4 BY .62 AT ROW 10.62 COL 7
     "Størrelse:" VIEW-AS TEXT
          SIZE 10.4 BY .62 AT ROW 5.95 COL 7
     RECT-3 AT ROW 1.19 COL 2
     RECT-4 AT ROW 2.81 COL 3.4
     SPACE(1.39) SKIP(24.22)
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
   FRAME-NAME                                                           */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN StrType.Beskrivelse IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON Btn_Help IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN StrType.KortNavn IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN StrType.StrTypeID IN FRAME Dialog-Frame
   NO-ENABLE EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN tmpStrDef.Str[100] IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN tmpStrDef.Str[10] IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
ASSIGN 
       tmpStrDef.Str[10]:READ-ONLY IN FRAME Dialog-Frame        = TRUE.

/* SETTINGS FOR FILL-IN tmpStrDef.Str[11] IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
ASSIGN 
       tmpStrDef.Str[11]:READ-ONLY IN FRAME Dialog-Frame        = TRUE.

/* SETTINGS FOR FILL-IN tmpStrDef.Str[12] IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
ASSIGN 
       tmpStrDef.Str[12]:READ-ONLY IN FRAME Dialog-Frame        = TRUE.

/* SETTINGS FOR FILL-IN tmpStrDef.Str[13] IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
ASSIGN 
       tmpStrDef.Str[13]:READ-ONLY IN FRAME Dialog-Frame        = TRUE.

/* SETTINGS FOR FILL-IN tmpStrDef.Str[14] IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
ASSIGN 
       tmpStrDef.Str[14]:READ-ONLY IN FRAME Dialog-Frame        = TRUE.

/* SETTINGS FOR FILL-IN tmpStrDef.Str[15] IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
ASSIGN 
       tmpStrDef.Str[15]:READ-ONLY IN FRAME Dialog-Frame        = TRUE.

/* SETTINGS FOR FILL-IN tmpStrDef.Str[16] IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
ASSIGN 
       tmpStrDef.Str[16]:READ-ONLY IN FRAME Dialog-Frame        = TRUE.

/* SETTINGS FOR FILL-IN tmpStrDef.Str[17] IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
ASSIGN 
       tmpStrDef.Str[17]:READ-ONLY IN FRAME Dialog-Frame        = TRUE.

/* SETTINGS FOR FILL-IN tmpStrDef.Str[18] IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
ASSIGN 
       tmpStrDef.Str[18]:READ-ONLY IN FRAME Dialog-Frame        = TRUE.

/* SETTINGS FOR FILL-IN tmpStrDef.Str[19] IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
ASSIGN 
       tmpStrDef.Str[19]:READ-ONLY IN FRAME Dialog-Frame        = TRUE.

/* SETTINGS FOR FILL-IN tmpStrDef.Str[1] IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
ASSIGN 
       tmpStrDef.Str[1]:READ-ONLY IN FRAME Dialog-Frame        = TRUE.

/* SETTINGS FOR FILL-IN tmpStrDef.Str[20] IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
ASSIGN 
       tmpStrDef.Str[20]:READ-ONLY IN FRAME Dialog-Frame        = TRUE.

/* SETTINGS FOR FILL-IN tmpStrDef.Str[21] IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
ASSIGN 
       tmpStrDef.Str[21]:READ-ONLY IN FRAME Dialog-Frame        = TRUE.

/* SETTINGS FOR FILL-IN tmpStrDef.Str[22] IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
ASSIGN 
       tmpStrDef.Str[22]:READ-ONLY IN FRAME Dialog-Frame        = TRUE.

/* SETTINGS FOR FILL-IN tmpStrDef.Str[23] IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
ASSIGN 
       tmpStrDef.Str[23]:READ-ONLY IN FRAME Dialog-Frame        = TRUE.

/* SETTINGS FOR FILL-IN tmpStrDef.Str[24] IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
ASSIGN 
       tmpStrDef.Str[24]:READ-ONLY IN FRAME Dialog-Frame        = TRUE.

/* SETTINGS FOR FILL-IN tmpStrDef.Str[25] IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
ASSIGN 
       tmpStrDef.Str[25]:READ-ONLY IN FRAME Dialog-Frame        = TRUE.

/* SETTINGS FOR FILL-IN tmpStrDef.Str[26] IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
ASSIGN 
       tmpStrDef.Str[26]:READ-ONLY IN FRAME Dialog-Frame        = TRUE.

/* SETTINGS FOR FILL-IN tmpStrDef.Str[27] IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
ASSIGN 
       tmpStrDef.Str[27]:READ-ONLY IN FRAME Dialog-Frame        = TRUE.

/* SETTINGS FOR FILL-IN tmpStrDef.Str[28] IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
ASSIGN 
       tmpStrDef.Str[28]:READ-ONLY IN FRAME Dialog-Frame        = TRUE.

/* SETTINGS FOR FILL-IN tmpStrDef.Str[29] IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
ASSIGN 
       tmpStrDef.Str[29]:READ-ONLY IN FRAME Dialog-Frame        = TRUE.

/* SETTINGS FOR FILL-IN tmpStrDef.Str[2] IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
ASSIGN 
       tmpStrDef.Str[2]:READ-ONLY IN FRAME Dialog-Frame        = TRUE.

/* SETTINGS FOR FILL-IN tmpStrDef.Str[30] IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
ASSIGN 
       tmpStrDef.Str[30]:READ-ONLY IN FRAME Dialog-Frame        = TRUE.

/* SETTINGS FOR FILL-IN tmpStrDef.Str[31] IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
ASSIGN 
       tmpStrDef.Str[31]:READ-ONLY IN FRAME Dialog-Frame        = TRUE.

/* SETTINGS FOR FILL-IN tmpStrDef.Str[32] IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
ASSIGN 
       tmpStrDef.Str[32]:READ-ONLY IN FRAME Dialog-Frame        = TRUE.

/* SETTINGS FOR FILL-IN tmpStrDef.Str[33] IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
ASSIGN 
       tmpStrDef.Str[33]:READ-ONLY IN FRAME Dialog-Frame        = TRUE.

/* SETTINGS FOR FILL-IN tmpStrDef.Str[34] IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
ASSIGN 
       tmpStrDef.Str[34]:READ-ONLY IN FRAME Dialog-Frame        = TRUE.

/* SETTINGS FOR FILL-IN tmpStrDef.Str[35] IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
ASSIGN 
       tmpStrDef.Str[35]:READ-ONLY IN FRAME Dialog-Frame        = TRUE.

/* SETTINGS FOR FILL-IN tmpStrDef.Str[36] IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
ASSIGN 
       tmpStrDef.Str[36]:READ-ONLY IN FRAME Dialog-Frame        = TRUE.

/* SETTINGS FOR FILL-IN tmpStrDef.Str[37] IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
ASSIGN 
       tmpStrDef.Str[37]:READ-ONLY IN FRAME Dialog-Frame        = TRUE.

/* SETTINGS FOR FILL-IN tmpStrDef.Str[38] IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
ASSIGN 
       tmpStrDef.Str[38]:READ-ONLY IN FRAME Dialog-Frame        = TRUE.

/* SETTINGS FOR FILL-IN tmpStrDef.Str[39] IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
ASSIGN 
       tmpStrDef.Str[39]:READ-ONLY IN FRAME Dialog-Frame        = TRUE.

/* SETTINGS FOR FILL-IN tmpStrDef.Str[3] IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
ASSIGN 
       tmpStrDef.Str[3]:READ-ONLY IN FRAME Dialog-Frame        = TRUE.

/* SETTINGS FOR FILL-IN tmpStrDef.Str[40] IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
ASSIGN 
       tmpStrDef.Str[40]:READ-ONLY IN FRAME Dialog-Frame        = TRUE.

/* SETTINGS FOR FILL-IN tmpStrDef.Str[41] IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
ASSIGN 
       tmpStrDef.Str[41]:READ-ONLY IN FRAME Dialog-Frame        = TRUE.

/* SETTINGS FOR FILL-IN tmpStrDef.Str[42] IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
ASSIGN 
       tmpStrDef.Str[42]:READ-ONLY IN FRAME Dialog-Frame        = TRUE.

/* SETTINGS FOR FILL-IN tmpStrDef.Str[43] IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
ASSIGN 
       tmpStrDef.Str[43]:READ-ONLY IN FRAME Dialog-Frame        = TRUE.

/* SETTINGS FOR FILL-IN tmpStrDef.Str[44] IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
ASSIGN 
       tmpStrDef.Str[44]:READ-ONLY IN FRAME Dialog-Frame        = TRUE.

/* SETTINGS FOR FILL-IN tmpStrDef.Str[45] IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
ASSIGN 
       tmpStrDef.Str[45]:READ-ONLY IN FRAME Dialog-Frame        = TRUE.

/* SETTINGS FOR FILL-IN tmpStrDef.Str[46] IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
ASSIGN 
       tmpStrDef.Str[46]:READ-ONLY IN FRAME Dialog-Frame        = TRUE.

/* SETTINGS FOR FILL-IN tmpStrDef.Str[47] IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
ASSIGN 
       tmpStrDef.Str[47]:READ-ONLY IN FRAME Dialog-Frame        = TRUE.

/* SETTINGS FOR FILL-IN tmpStrDef.Str[48] IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
ASSIGN 
       tmpStrDef.Str[48]:READ-ONLY IN FRAME Dialog-Frame        = TRUE.

/* SETTINGS FOR FILL-IN tmpStrDef.Str[49] IN FRAME Dialog-Frame
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN tmpStrDef.Str[4] IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
ASSIGN 
       tmpStrDef.Str[4]:READ-ONLY IN FRAME Dialog-Frame        = TRUE.

/* SETTINGS FOR FILL-IN tmpStrDef.Str[50] IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN tmpStrDef.Str[51] IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN tmpStrDef.Str[52] IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN tmpStrDef.Str[53] IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN tmpStrDef.Str[54] IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN tmpStrDef.Str[55] IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN tmpStrDef.Str[56] IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN tmpStrDef.Str[57] IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN tmpStrDef.Str[58] IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN tmpStrDef.Str[59] IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN tmpStrDef.Str[5] IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
ASSIGN 
       tmpStrDef.Str[5]:READ-ONLY IN FRAME Dialog-Frame        = TRUE.

/* SETTINGS FOR FILL-IN tmpStrDef.Str[60] IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN tmpStrDef.Str[61] IN FRAME Dialog-Frame
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN tmpStrDef.Str[62] IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN tmpStrDef.Str[63] IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN tmpStrDef.Str[64] IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN tmpStrDef.Str[65] IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN tmpStrDef.Str[66] IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN tmpStrDef.Str[67] IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN tmpStrDef.Str[68] IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN tmpStrDef.Str[69] IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN tmpStrDef.Str[6] IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
ASSIGN 
       tmpStrDef.Str[6]:READ-ONLY IN FRAME Dialog-Frame        = TRUE.

/* SETTINGS FOR FILL-IN tmpStrDef.Str[70] IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN tmpStrDef.Str[71] IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN tmpStrDef.Str[72] IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN tmpStrDef.Str[73] IN FRAME Dialog-Frame
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN tmpStrDef.Str[74] IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN tmpStrDef.Str[75] IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN tmpStrDef.Str[76] IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN tmpStrDef.Str[77] IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN tmpStrDef.Str[78] IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN tmpStrDef.Str[79] IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN tmpStrDef.Str[7] IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
ASSIGN 
       tmpStrDef.Str[7]:READ-ONLY IN FRAME Dialog-Frame        = TRUE.

/* SETTINGS FOR FILL-IN tmpStrDef.Str[80] IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN tmpStrDef.Str[81] IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN tmpStrDef.Str[82] IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN tmpStrDef.Str[83] IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN tmpStrDef.Str[84] IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN tmpStrDef.Str[85] IN FRAME Dialog-Frame
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN tmpStrDef.Str[86] IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN tmpStrDef.Str[87] IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN tmpStrDef.Str[88] IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN tmpStrDef.Str[89] IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN tmpStrDef.Str[8] IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
ASSIGN 
       tmpStrDef.Str[8]:READ-ONLY IN FRAME Dialog-Frame        = TRUE.

/* SETTINGS FOR FILL-IN tmpStrDef.Str[90] IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN tmpStrDef.Str[91] IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN tmpStrDef.Str[92] IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN tmpStrDef.Str[93] IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN tmpStrDef.Str[94] IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN tmpStrDef.Str[95] IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN tmpStrDef.Str[96] IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN tmpStrDef.Str[97] IN FRAME Dialog-Frame
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN tmpStrDef.Str[98] IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN tmpStrDef.Str[99] IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN tmpStrDef.Str[9] IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
ASSIGN 
       tmpStrDef.Str[9]:READ-ONLY IN FRAME Dialog-Frame        = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX Dialog-Frame
/* Query rebuild information for DIALOG-BOX Dialog-Frame
     _TblList          = "SkoTex.StrType,SkoTex.tmpStrDef WHERE SkoTex.StrType ..."
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


&Scoped-define SELF-NAME B-Tag
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Tag Dialog-Frame
ON CHOOSE OF B-Tag IN FRAME Dialog-Frame /* Velg størrelser */
DO:
    DEFINE VARIABLE cStrListe AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE iCount    AS INTEGER    NO-UNDO.
    RUN d-tagstrkonv.w (INPUT wStrTypeID,INPUT StrType.Beskrivelse,OUTPUT cStrListe).
    IF cStrListe <> "" THEN DO:
        ASSIGN tmpStrDef.Str[1]:SCREEN-VALUE  = ENTRY(1,cStrListe,CHR(1))
               tmpStrDef.Str[2]:SCREEN-VALUE  = ENTRY(2,cStrListe,CHR(1))
               tmpStrDef.Str[3]:SCREEN-VALUE  = ENTRY(3,cStrListe,CHR(1))
               tmpStrDef.Str[4]:SCREEN-VALUE  = ENTRY(4,cStrListe,CHR(1))
               tmpStrDef.Str[5]:SCREEN-VALUE  = ENTRY(5,cStrListe,CHR(1))
               tmpStrDef.Str[6]:SCREEN-VALUE  = ENTRY(6,cStrListe,CHR(1))
               tmpStrDef.Str[7]:SCREEN-VALUE  = ENTRY(7,cStrListe,CHR(1))
               tmpStrDef.Str[8]:SCREEN-VALUE  = ENTRY(8,cStrListe,CHR(1))
               tmpStrDef.Str[9]:SCREEN-VALUE  = ENTRY(9,cStrListe,CHR(1))
               tmpStrDef.Str[10]:SCREEN-VALUE = ENTRY(10,cStrListe,CHR(1))
               tmpStrDef.Str[11]:SCREEN-VALUE = ENTRY(11,cStrListe,CHR(1))
               tmpStrDef.Str[12]:SCREEN-VALUE = ENTRY(12,cStrListe,CHR(1))
               tmpStrDef.Str[13]:SCREEN-VALUE = ENTRY(13,cStrListe,CHR(1))
               tmpStrDef.Str[14]:SCREEN-VALUE = ENTRY(14,cStrListe,CHR(1))
               tmpStrDef.Str[15]:SCREEN-VALUE = ENTRY(15,cStrListe,CHR(1))
               tmpStrDef.Str[16]:SCREEN-VALUE = ENTRY(16,cStrListe,CHR(1))
               tmpStrDef.Str[17]:SCREEN-VALUE = ENTRY(17,cStrListe,CHR(1))
               tmpStrDef.Str[18]:SCREEN-VALUE = ENTRY(18,cStrListe,CHR(1))
               tmpStrDef.Str[19]:SCREEN-VALUE = ENTRY(19,cStrListe,CHR(1))
               tmpStrDef.Str[20]:SCREEN-VALUE = ENTRY(20,cStrListe,CHR(1))
               tmpStrDef.Str[21]:SCREEN-VALUE = ENTRY(21,cStrListe,CHR(1))
               tmpStrDef.Str[22]:SCREEN-VALUE = ENTRY(22,cStrListe,CHR(1))
               tmpStrDef.Str[23]:SCREEN-VALUE = ENTRY(23,cStrListe,CHR(1))
               tmpStrDef.Str[24]:SCREEN-VALUE = ENTRY(24,cStrListe,CHR(1))
               tmpStrDef.Str[25]:SCREEN-VALUE = ENTRY(25,cStrListe,CHR(1))
               tmpStrDef.Str[26]:SCREEN-VALUE = ENTRY(26,cStrListe,CHR(1))
               tmpStrDef.Str[27]:SCREEN-VALUE = ENTRY(27,cStrListe,CHR(1))
               tmpStrDef.Str[28]:SCREEN-VALUE = ENTRY(28,cStrListe,CHR(1))
               tmpStrDef.Str[29]:SCREEN-VALUE = ENTRY(29,cStrListe,CHR(1))
               tmpStrDef.Str[30]:SCREEN-VALUE = ENTRY(30,cStrListe,CHR(1))
               tmpStrDef.Str[31]:SCREEN-VALUE = ENTRY(31,cStrListe,CHR(1))
               tmpStrDef.Str[32]:SCREEN-VALUE = ENTRY(32,cStrListe,CHR(1))
               tmpStrDef.Str[33]:SCREEN-VALUE = ENTRY(33,cStrListe,CHR(1))
               tmpStrDef.Str[34]:SCREEN-VALUE = ENTRY(34,cStrListe,CHR(1))
               tmpStrDef.Str[35]:SCREEN-VALUE = ENTRY(35,cStrListe,CHR(1))
               tmpStrDef.Str[36]:SCREEN-VALUE = ENTRY(36,cStrListe,CHR(1))
               tmpStrDef.Str[37]:SCREEN-VALUE = ENTRY(37,cStrListe,CHR(1))
               tmpStrDef.Str[38]:SCREEN-VALUE = ENTRY(38,cStrListe,CHR(1))
               tmpStrDef.Str[39]:SCREEN-VALUE = ENTRY(39,cStrListe,CHR(1))
               tmpStrDef.Str[40]:SCREEN-VALUE = ENTRY(40,cStrListe,CHR(1))
               tmpStrDef.Str[41]:SCREEN-VALUE = ENTRY(41,cStrListe,CHR(1))
               tmpStrDef.Str[42]:SCREEN-VALUE = ENTRY(42,cStrListe,CHR(1))
               tmpStrDef.Str[43]:SCREEN-VALUE = ENTRY(43,cStrListe,CHR(1))
               tmpStrDef.Str[44]:SCREEN-VALUE = ENTRY(44,cStrListe,CHR(1))
               tmpStrDef.Str[45]:SCREEN-VALUE = ENTRY(45,cStrListe,CHR(1))
               tmpStrDef.Str[46]:SCREEN-VALUE = ENTRY(46,cStrListe,CHR(1))
               tmpStrDef.Str[47]:SCREEN-VALUE = ENTRY(47,cStrListe,CHR(1))
               tmpStrDef.Str[48]:SCREEN-VALUE = ENTRY(48,cStrListe,CHR(1))
            
            tmpStrDef.Str[49]:SCREEN-VALUE = ENTRY(49,cStrListe,CHR(1))
            
            tmpStrDef.Str[50]:SCREEN-VALUE = ENTRY(50,cStrListe,CHR(1))
            tmpStrDef.Str[51]:SCREEN-VALUE = ENTRY(51,cStrListe,CHR(1))
            tmpStrDef.Str[52]:SCREEN-VALUE = ENTRY(52,cStrListe,CHR(1))
            tmpStrDef.Str[53]:SCREEN-VALUE = ENTRY(53,cStrListe,CHR(1))
            tmpStrDef.Str[54]:SCREEN-VALUE = ENTRY(54,cStrListe,CHR(1))
            tmpStrDef.Str[55]:SCREEN-VALUE = ENTRY(55,cStrListe,CHR(1))
            tmpStrDef.Str[56]:SCREEN-VALUE = ENTRY(56,cStrListe,CHR(1))
            tmpStrDef.Str[57]:SCREEN-VALUE = ENTRY(57,cStrListe,CHR(1))
            tmpStrDef.Str[58]:SCREEN-VALUE = ENTRY(58,cStrListe,CHR(1))
            tmpStrDef.Str[59]:SCREEN-VALUE = ENTRY(59,cStrListe,CHR(1))

            tmpStrDef.Str[60]:SCREEN-VALUE = ENTRY(60,cStrListe,CHR(1))
            tmpStrDef.Str[61]:SCREEN-VALUE = ENTRY(61,cStrListe,CHR(1))
            tmpStrDef.Str[62]:SCREEN-VALUE = ENTRY(62,cStrListe,CHR(1))
            tmpStrDef.Str[63]:SCREEN-VALUE = ENTRY(63,cStrListe,CHR(1))
            tmpStrDef.Str[64]:SCREEN-VALUE = ENTRY(64,cStrListe,CHR(1))
            tmpStrDef.Str[65]:SCREEN-VALUE = ENTRY(65,cStrListe,CHR(1))
            tmpStrDef.Str[66]:SCREEN-VALUE = ENTRY(66,cStrListe,CHR(1))
            tmpStrDef.Str[67]:SCREEN-VALUE = ENTRY(67,cStrListe,CHR(1))
            tmpStrDef.Str[68]:SCREEN-VALUE = ENTRY(68,cStrListe,CHR(1))
            tmpStrDef.Str[69]:SCREEN-VALUE = ENTRY(69,cStrListe,CHR(1))

            tmpStrDef.Str[70]:SCREEN-VALUE = ENTRY(70,cStrListe,CHR(1))
            tmpStrDef.Str[71]:SCREEN-VALUE = ENTRY(71,cStrListe,CHR(1))
            tmpStrDef.Str[72]:SCREEN-VALUE = ENTRY(72,cStrListe,CHR(1))
            tmpStrDef.Str[73]:SCREEN-VALUE = ENTRY(73,cStrListe,CHR(1))
            tmpStrDef.Str[74]:SCREEN-VALUE = ENTRY(74,cStrListe,CHR(1))
            tmpStrDef.Str[75]:SCREEN-VALUE = ENTRY(75,cStrListe,CHR(1))
            tmpStrDef.Str[76]:SCREEN-VALUE = ENTRY(76,cStrListe,CHR(1))
            tmpStrDef.Str[77]:SCREEN-VALUE = ENTRY(77,cStrListe,CHR(1))
            tmpStrDef.Str[78]:SCREEN-VALUE = ENTRY(78,cStrListe,CHR(1))
            tmpStrDef.Str[79]:SCREEN-VALUE = ENTRY(79,cStrListe,CHR(1))

            tmpStrDef.Str[80]:SCREEN-VALUE = ENTRY(80,cStrListe,CHR(1))
            tmpStrDef.Str[81]:SCREEN-VALUE = ENTRY(81,cStrListe,CHR(1))
            tmpStrDef.Str[82]:SCREEN-VALUE = ENTRY(82,cStrListe,CHR(1))
            tmpStrDef.Str[83]:SCREEN-VALUE = ENTRY(83,cStrListe,CHR(1))
            tmpStrDef.Str[84]:SCREEN-VALUE = ENTRY(84,cStrListe,CHR(1))
            tmpStrDef.Str[85]:SCREEN-VALUE = ENTRY(85,cStrListe,CHR(1))
            tmpStrDef.Str[86]:SCREEN-VALUE = ENTRY(86,cStrListe,CHR(1))
            tmpStrDef.Str[87]:SCREEN-VALUE = ENTRY(87,cStrListe,CHR(1))
            tmpStrDef.Str[88]:SCREEN-VALUE = ENTRY(88,cStrListe,CHR(1))
            tmpStrDef.Str[89]:SCREEN-VALUE = ENTRY(89,cStrListe,CHR(1))

            tmpStrDef.Str[90]:SCREEN-VALUE = ENTRY(90,cStrListe,CHR(1))
            tmpStrDef.Str[91]:SCREEN-VALUE = ENTRY(91,cStrListe,CHR(1))
            tmpStrDef.Str[92]:SCREEN-VALUE = ENTRY(92,cStrListe,CHR(1))
            tmpStrDef.Str[93]:SCREEN-VALUE = ENTRY(93,cStrListe,CHR(1))
            tmpStrDef.Str[94]:SCREEN-VALUE = ENTRY(94,cStrListe,CHR(1))
            tmpStrDef.Str[95]:SCREEN-VALUE = ENTRY(95,cStrListe,CHR(1))
            tmpStrDef.Str[96]:SCREEN-VALUE = ENTRY(96,cStrListe,CHR(1))
            tmpStrDef.Str[97]:SCREEN-VALUE = ENTRY(97,cStrListe,CHR(1))
            tmpStrDef.Str[98]:SCREEN-VALUE = ENTRY(98,cStrListe,CHR(1))
            tmpStrDef.Str[99]:SCREEN-VALUE = ENTRY(99,cStrListe,CHR(1))

            tmpStrDef.Str[100]:SCREEN-VALUE = ENTRY(100,cStrListe,CHR(1))
            .
        RUN LagreStorrelser.
    END.
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


&Scoped-define SELF-NAME tmpStrDef.Str[1]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tmpStrDef.Str[1] Dialog-Frame
ON LEAVE OF tmpStrDef.Str[1] IN FRAME Dialog-Frame /* Str */
OR LEAVE OF tmpStrDef.Str[ 2]
OR LEAVE OF tmpStrDef.Str[ 3]
OR LEAVE OF tmpStrDef.Str[ 4]
OR LEAVE OF tmpStrDef.Str[ 5]
OR LEAVE OF tmpStrDef.Str[ 6]
OR LEAVE OF tmpStrDef.Str[ 7]
OR LEAVE OF tmpStrDef.Str[ 8]
OR LEAVE OF tmpStrDef.Str[ 9]
OR LEAVE OF tmpStrDef.Str[10]
OR LEAVE OF tmpStrDef.Str[11]
OR LEAVE OF tmpStrDef.Str[12]
OR LEAVE OF tmpStrDef.Str[13]
OR LEAVE OF tmpStrDef.Str[14]
OR LEAVE OF tmpStrDef.Str[15]
OR LEAVE OF tmpStrDef.Str[16]
OR LEAVE OF tmpStrDef.Str[17]
OR LEAVE OF tmpStrDef.Str[18]
OR LEAVE OF tmpStrDef.Str[19]
OR LEAVE OF tmpStrDef.Str[20]
OR LEAVE OF tmpStrDef.Str[21]
OR LEAVE OF tmpStrDef.Str[22]
OR LEAVE OF tmpStrDef.Str[23]
OR LEAVE OF tmpStrDef.Str[24]
OR LEAVE OF tmpStrDef.Str[25]
OR LEAVE OF tmpStrDef.Str[26]
OR LEAVE OF tmpStrDef.Str[27]
OR LEAVE OF tmpStrDef.Str[28]
OR LEAVE OF tmpStrDef.Str[29]
OR LEAVE OF tmpStrDef.Str[30]
OR LEAVE OF tmpStrDef.Str[31]
OR LEAVE OF tmpStrDef.Str[32]
OR LEAVE OF tmpStrDef.Str[33]
OR LEAVE OF tmpStrDef.Str[34]
OR LEAVE OF tmpStrDef.Str[35]
OR LEAVE OF tmpStrDef.Str[36]
OR LEAVE OF tmpStrDef.Str[37]
OR LEAVE OF tmpStrDef.Str[38]
OR LEAVE OF tmpStrDef.Str[39]
OR LEAVE OF tmpStrDef.Str[40]
OR LEAVE OF tmpStrDef.Str[41]
OR LEAVE OF tmpStrDef.Str[42]
OR LEAVE OF tmpStrDef.Str[43]
OR LEAVE OF tmpStrDef.Str[44]
OR LEAVE OF tmpStrDef.Str[45]
OR LEAVE OF tmpStrDef.Str[46]
OR LEAVE OF tmpStrDef.Str[47]
OR LEAVE OF tmpStrDef.Str[48]
DO:
  {vstrtstr.i}  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dialog-Frame 


/* ***************************  Main Block  *************************** */

/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT EQ ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.

/* Henter record. */
FIND StrType NO-LOCK WHERE
  StrType.StrTypeId = wStrTypeID NO-ERROR.
/*
DO wLoop = 1 TO 100:
  RUN EnableStr (INPUT wLoop).
END.
*/
FIND LAST tmpStrDef EXCLUSIVE-LOCK NO-ERROR.
IF NOT AVAILABLE tmpStrDef
  THEN ASSIGN wLoop= 1.
ELSE
  ASSIGN wLoop = tmpStrDef.Id + 1.
CREATE tmpStrDef.
  ASSIGN
    tmpStrDef.Id = wLoop.
ASSIGN
  wRecid = RECID(tmpStrDef).
RUN LastStorrelser.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  {lng.i} RUN enable_UI.
  WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE EnableStr Dialog-Frame 
PROCEDURE EnableStr :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER wInt AS INT NO-UNDO.
  
  CASE wInt:
    WHEN  1 THEN tmpStrDef.Str[ 1]:sensitive IN FRAME Dialog-Frame = TRUE.
    WHEN  2 THEN tmpStrDef.Str[ 2]:sensitive IN FRAME Dialog-Frame = TRUE.
    WHEN  3 THEN tmpStrDef.Str[ 3]:sensitive IN FRAME Dialog-Frame = TRUE.
    WHEN  4 THEN tmpStrDef.Str[ 4]:sensitive IN FRAME Dialog-Frame = TRUE.
    WHEN  5 THEN tmpStrDef.Str[ 5]:sensitive IN FRAME Dialog-Frame = TRUE.
    WHEN  6 THEN tmpStrDef.Str[ 6]:sensitive IN FRAME Dialog-Frame = TRUE.
    WHEN  7 THEN tmpStrDef.Str[ 7]:sensitive IN FRAME Dialog-Frame = TRUE.
    WHEN  8 THEN tmpStrDef.Str[ 8]:sensitive IN FRAME Dialog-Frame = TRUE.
    WHEN  9 THEN tmpStrDef.Str[ 9]:sensitive IN FRAME Dialog-Frame = TRUE.
    WHEN 10 THEN tmpStrDef.Str[10]:sensitive IN FRAME Dialog-Frame = TRUE.
    WHEN 11 THEN tmpStrDef.Str[11]:sensitive IN FRAME Dialog-Frame = TRUE.
    WHEN 12 THEN tmpStrDef.Str[12]:sensitive IN FRAME Dialog-Frame = TRUE.
    WHEN 13 THEN tmpStrDef.Str[13]:sensitive IN FRAME Dialog-Frame = TRUE.
    WHEN 14 THEN tmpStrDef.Str[14]:sensitive IN FRAME Dialog-Frame = TRUE.
    WHEN 15 THEN tmpStrDef.Str[15]:sensitive IN FRAME Dialog-Frame = TRUE.
    WHEN 16 THEN tmpStrDef.Str[16]:sensitive IN FRAME Dialog-Frame = TRUE.
    WHEN 17 THEN tmpStrDef.Str[17]:sensitive IN FRAME Dialog-Frame = TRUE.
    WHEN 18 THEN tmpStrDef.Str[18]:sensitive IN FRAME Dialog-Frame = TRUE.
    WHEN 19 THEN tmpStrDef.Str[19]:sensitive IN FRAME Dialog-Frame = TRUE.
    WHEN 20 THEN tmpStrDef.Str[20]:sensitive IN FRAME Dialog-Frame = TRUE.
    WHEN 21 THEN tmpStrDef.Str[21]:sensitive IN FRAME Dialog-Frame = TRUE.
    WHEN 22 THEN tmpStrDef.Str[22]:sensitive IN FRAME Dialog-Frame = TRUE.
    WHEN 23 THEN tmpStrDef.Str[23]:sensitive IN FRAME Dialog-Frame = TRUE.
    WHEN 24 THEN tmpStrDef.Str[24]:sensitive IN FRAME Dialog-Frame = TRUE.
    WHEN 25 THEN tmpStrDef.Str[25]:sensitive IN FRAME Dialog-Frame = TRUE.
    WHEN 26 THEN tmpStrDef.Str[26]:sensitive IN FRAME Dialog-Frame = TRUE.
    WHEN 27 THEN tmpStrDef.Str[27]:sensitive IN FRAME Dialog-Frame = TRUE.
    WHEN 28 THEN tmpStrDef.Str[28]:sensitive IN FRAME Dialog-Frame = TRUE.
    WHEN 29 THEN tmpStrDef.Str[29]:sensitive IN FRAME Dialog-Frame = TRUE.
    WHEN 30 THEN tmpStrDef.Str[30]:sensitive IN FRAME Dialog-Frame = TRUE.
    WHEN 31 THEN tmpStrDef.Str[31]:sensitive IN FRAME Dialog-Frame = TRUE.
    WHEN 32 THEN tmpStrDef.Str[32]:sensitive IN FRAME Dialog-Frame = TRUE.
    WHEN 33 THEN tmpStrDef.Str[33]:sensitive IN FRAME Dialog-Frame = TRUE.
    WHEN 34 THEN tmpStrDef.Str[34]:sensitive IN FRAME Dialog-Frame = TRUE.
    WHEN 35 THEN tmpStrDef.Str[35]:sensitive IN FRAME Dialog-Frame = TRUE.
    WHEN 36 THEN tmpStrDef.Str[36]:sensitive IN FRAME Dialog-Frame = TRUE.
    WHEN 37 THEN tmpStrDef.Str[37]:sensitive IN FRAME Dialog-Frame = TRUE.
    WHEN 38 THEN tmpStrDef.Str[38]:sensitive IN FRAME Dialog-Frame = TRUE.
    WHEN 39 THEN tmpStrDef.Str[39]:sensitive IN FRAME Dialog-Frame = TRUE.
    WHEN 40 THEN tmpStrDef.Str[40]:sensitive IN FRAME Dialog-Frame = TRUE.
    WHEN 41 THEN tmpStrDef.Str[41]:sensitive IN FRAME Dialog-Frame = TRUE.
    WHEN 42 THEN tmpStrDef.Str[42]:sensitive IN FRAME Dialog-Frame = TRUE.
    WHEN 43 THEN tmpStrDef.Str[43]:sensitive IN FRAME Dialog-Frame = TRUE.
    WHEN 44 THEN tmpStrDef.Str[44]:sensitive IN FRAME Dialog-Frame = TRUE.
    WHEN 45 THEN tmpStrDef.Str[45]:sensitive IN FRAME Dialog-Frame = TRUE.
    WHEN 46 THEN tmpStrDef.Str[46]:sensitive IN FRAME Dialog-Frame = TRUE.
    WHEN 47 THEN tmpStrDef.Str[47]:sensitive IN FRAME Dialog-Frame = TRUE.
    WHEN 48 THEN tmpStrDef.Str[48]:sensitive IN FRAME Dialog-Frame = TRUE.
  
      WHEN 49 THEN tmpStrDef.Str[49]:sensitive IN FRAME Dialog-Frame = TRUE.
      WHEN 50 THEN tmpStrDef.Str[50]:sensitive IN FRAME Dialog-Frame = TRUE.
      WHEN 51 THEN tmpStrDef.Str[51]:sensitive IN FRAME Dialog-Frame = TRUE.
      WHEN 52 THEN tmpStrDef.Str[52]:sensitive IN FRAME Dialog-Frame = TRUE.
      WHEN 53 THEN tmpStrDef.Str[53]:sensitive IN FRAME Dialog-Frame = TRUE.
      WHEN 54 THEN tmpStrDef.Str[54]:sensitive IN FRAME Dialog-Frame = TRUE.
      WHEN 55 THEN tmpStrDef.Str[55]:sensitive IN FRAME Dialog-Frame = TRUE.
      WHEN 56 THEN tmpStrDef.Str[56]:sensitive IN FRAME Dialog-Frame = TRUE.
      WHEN 57 THEN tmpStrDef.Str[57]:sensitive IN FRAME Dialog-Frame = TRUE.
      WHEN 58 THEN tmpStrDef.Str[58]:sensitive IN FRAME Dialog-Frame = TRUE.
      WHEN 59 THEN tmpStrDef.Str[59]:sensitive IN FRAME Dialog-Frame = TRUE.

      WHEN 60 THEN tmpStrDef.Str[60]:sensitive IN FRAME Dialog-Frame = TRUE.
      WHEN 61 THEN tmpStrDef.Str[61]:sensitive IN FRAME Dialog-Frame = TRUE.
      WHEN 62 THEN tmpStrDef.Str[62]:sensitive IN FRAME Dialog-Frame = TRUE.
      WHEN 63 THEN tmpStrDef.Str[63]:sensitive IN FRAME Dialog-Frame = TRUE.
      WHEN 64 THEN tmpStrDef.Str[64]:sensitive IN FRAME Dialog-Frame = TRUE.
      WHEN 65 THEN tmpStrDef.Str[65]:sensitive IN FRAME Dialog-Frame = TRUE.
      WHEN 66 THEN tmpStrDef.Str[66]:sensitive IN FRAME Dialog-Frame = TRUE.
      WHEN 67 THEN tmpStrDef.Str[67]:sensitive IN FRAME Dialog-Frame = TRUE.
      WHEN 68 THEN tmpStrDef.Str[68]:sensitive IN FRAME Dialog-Frame = TRUE.
      WHEN 69 THEN tmpStrDef.Str[69]:sensitive IN FRAME Dialog-Frame = TRUE.

      WHEN 70 THEN tmpStrDef.Str[70]:sensitive IN FRAME Dialog-Frame = TRUE.
      WHEN 71 THEN tmpStrDef.Str[71]:sensitive IN FRAME Dialog-Frame = TRUE.
      WHEN 72 THEN tmpStrDef.Str[72]:sensitive IN FRAME Dialog-Frame = TRUE.
      WHEN 73 THEN tmpStrDef.Str[73]:sensitive IN FRAME Dialog-Frame = TRUE.
      WHEN 74 THEN tmpStrDef.Str[74]:sensitive IN FRAME Dialog-Frame = TRUE.
      WHEN 75 THEN tmpStrDef.Str[75]:sensitive IN FRAME Dialog-Frame = TRUE.
      WHEN 76 THEN tmpStrDef.Str[76]:sensitive IN FRAME Dialog-Frame = TRUE.
      WHEN 77 THEN tmpStrDef.Str[77]:sensitive IN FRAME Dialog-Frame = TRUE.
      WHEN 78 THEN tmpStrDef.Str[78]:sensitive IN FRAME Dialog-Frame = TRUE.
      WHEN 79 THEN tmpStrDef.Str[79]:sensitive IN FRAME Dialog-Frame = TRUE.

      WHEN 80 THEN tmpStrDef.Str[80]:sensitive IN FRAME Dialog-Frame = TRUE.
      WHEN 81 THEN tmpStrDef.Str[81]:sensitive IN FRAME Dialog-Frame = TRUE.
      WHEN 82 THEN tmpStrDef.Str[82]:sensitive IN FRAME Dialog-Frame = TRUE.
      WHEN 83 THEN tmpStrDef.Str[83]:sensitive IN FRAME Dialog-Frame = TRUE.
      WHEN 84 THEN tmpStrDef.Str[84]:sensitive IN FRAME Dialog-Frame = TRUE.
      WHEN 85 THEN tmpStrDef.Str[85]:sensitive IN FRAME Dialog-Frame = TRUE.
      WHEN 86 THEN tmpStrDef.Str[86]:sensitive IN FRAME Dialog-Frame = TRUE.
      WHEN 87 THEN tmpStrDef.Str[87]:sensitive IN FRAME Dialog-Frame = TRUE.
      WHEN 88 THEN tmpStrDef.Str[88]:sensitive IN FRAME Dialog-Frame = TRUE.
      WHEN 89 THEN tmpStrDef.Str[89]:sensitive IN FRAME Dialog-Frame = TRUE.


      WHEN 90 THEN tmpStrDef.Str[90]:sensitive IN FRAME Dialog-Frame = TRUE.
      WHEN 91 THEN tmpStrDef.Str[91]:sensitive IN FRAME Dialog-Frame = TRUE.
      WHEN 92 THEN tmpStrDef.Str[92]:sensitive IN FRAME Dialog-Frame = TRUE.
      WHEN 93 THEN tmpStrDef.Str[93]:sensitive IN FRAME Dialog-Frame = TRUE.
      WHEN 94 THEN tmpStrDef.Str[94]:sensitive IN FRAME Dialog-Frame = TRUE.
      WHEN 95 THEN tmpStrDef.Str[95]:sensitive IN FRAME Dialog-Frame = TRUE.
      WHEN 96 THEN tmpStrDef.Str[96]:sensitive IN FRAME Dialog-Frame = TRUE.
      WHEN 97 THEN tmpStrDef.Str[97]:sensitive IN FRAME Dialog-Frame = TRUE.
      WHEN 98 THEN tmpStrDef.Str[98]:sensitive IN FRAME Dialog-Frame = TRUE.
      WHEN 99 THEN tmpStrDef.Str[99]:sensitive IN FRAME Dialog-Frame = TRUE.
      WHEN 100 THEN tmpStrDef.Str[100]:sensitive IN FRAME Dialog-Frame = TRUE.

  END CASE.

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
  IF AVAILABLE StrType THEN 
    DISPLAY StrType.StrTypeID StrType.KortNavn StrType.Beskrivelse 
      WITH FRAME Dialog-Frame.
  IF AVAILABLE tmpStrDef THEN 
    DISPLAY tmpStrDef.Str[1] tmpStrDef.Str[2] tmpStrDef.Str[3] tmpStrDef.Str[4] 
          tmpStrDef.Str[5] tmpStrDef.Str[6] tmpStrDef.Str[7] tmpStrDef.Str[8] 
          tmpStrDef.Str[9] tmpStrDef.Str[10] tmpStrDef.Str[11] tmpStrDef.Str[12] 
          tmpStrDef.Str[13] tmpStrDef.Str[14] tmpStrDef.Str[15] 
          tmpStrDef.Str[16] tmpStrDef.Str[17] tmpStrDef.Str[18] 
          tmpStrDef.Str[19] tmpStrDef.Str[20] tmpStrDef.Str[21] 
          tmpStrDef.Str[22] tmpStrDef.Str[23] tmpStrDef.Str[24] 
          tmpStrDef.Str[25] tmpStrDef.Str[26] tmpStrDef.Str[27] 
          tmpStrDef.Str[28] tmpStrDef.Str[29] tmpStrDef.Str[30] 
          tmpStrDef.Str[31] tmpStrDef.Str[32] tmpStrDef.Str[33] 
          tmpStrDef.Str[34] tmpStrDef.Str[35] tmpStrDef.Str[36] 
          tmpStrDef.Str[37] tmpStrDef.Str[38] tmpStrDef.Str[39] 
          tmpStrDef.Str[40] tmpStrDef.Str[41] tmpStrDef.Str[42] 
          tmpStrDef.Str[43] tmpStrDef.Str[44] tmpStrDef.Str[45] 
          tmpStrDef.Str[46] tmpStrDef.Str[47] tmpStrDef.Str[48] 
          tmpStrDef.Str[49] tmpStrDef.Str[50] tmpStrDef.Str[51] 
          tmpStrDef.Str[52] tmpStrDef.Str[53] tmpStrDef.Str[54] 
          tmpStrDef.Str[55] tmpStrDef.Str[56] tmpStrDef.Str[57] 
          tmpStrDef.Str[58] tmpStrDef.Str[59] tmpStrDef.Str[60] 
          tmpStrDef.Str[61] tmpStrDef.Str[62] tmpStrDef.Str[63] 
          tmpStrDef.Str[64] tmpStrDef.Str[65] tmpStrDef.Str[66] 
          tmpStrDef.Str[67] tmpStrDef.Str[68] tmpStrDef.Str[69] 
          tmpStrDef.Str[70] tmpStrDef.Str[71] tmpStrDef.Str[72] 
          tmpStrDef.Str[73] tmpStrDef.Str[74] tmpStrDef.Str[75] 
          tmpStrDef.Str[76] tmpStrDef.Str[77] tmpStrDef.Str[78] 
          tmpStrDef.Str[79] tmpStrDef.Str[80] tmpStrDef.Str[81] 
          tmpStrDef.Str[82] tmpStrDef.Str[83] tmpStrDef.Str[84] 
          tmpStrDef.Str[85] tmpStrDef.Str[86] tmpStrDef.Str[87] 
          tmpStrDef.Str[88] tmpStrDef.Str[89] tmpStrDef.Str[90] 
          tmpStrDef.Str[91] tmpStrDef.Str[92] tmpStrDef.Str[93] 
          tmpStrDef.Str[94] tmpStrDef.Str[95] tmpStrDef.Str[96] 
          tmpStrDef.Str[97] tmpStrDef.Str[98] tmpStrDef.Str[99] 
          tmpStrDef.Str[100] 
      WITH FRAME Dialog-Frame.
  ENABLE RECT-3 RECT-4 Btn_OK B-Tag 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
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
  DEF VAR wRetur-Verdi AS CHAR INITIAL "<avbryt>" NO-UNDO.
  DO:
    /* Først kaster vi alle de gamle størrelsene */
    FOR EACH StrTStr OF StrType EXCLUSIVE-LOCK:
      DELETE StrTStr.
    END.

    /* Så oppretter vi nye poster for alle de nye */
    DO wLoop = 1 TO 100 WITH FRAME Dialog-Frame:
    
      IF INPUT tmpStrDef.Str[wLoop] <> "" THEN
        DO:
          CREATE StrTStr.
          ASSIGN
            StrTStr.StrTypeId = StrType.StrTypeID
            StrTSTr.SeqNr     = wLoop
            StrTStr.SoStorl   = INPUT tmpStrDef.Str[wLoop].
        END.
    END.
    
    IF AVAILABLE tmpStrDef THEN
      DELETE tmpStrDef.
    RUN settStrTypeFelt.p (StrType.StrTypeID).
    ASSIGN wRetur-Verdi = "OK".
  END.
  
  RETURN wRetur-Verdi.
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
  DEF VAR wLoop AS INT NO-UNDO.

  IF NOT AVAILABLE tmpStrDef THEN
    FIND tmpStrDef WHERE
      RECID(tmpStrDef) = wRecid EXCLUSIVE-LOCK.
  IF NOT AVAILABLE StrType THEN
    RETURN NO-APPLY.
  
  BYGG:
  FOR EACH StrTStr OF StrType NO-LOCK
    BY StrTStr.StrTypeId
    BY StrTStr.SeqNr:
    ASSIGN 
      wLoop = wLoop + 1
      tmpStrDef.Str[StrTStr.SeqNr] = StrTStr.SoStorl.
    IF wLoop > 100 THEN
      LEAVE BYGG.
  END. /* BYGG */
  
  IF wLoop > 100 THEN
    DO:
      MESSAGE "Det er flere enn 100 størrelser på denne størrelsestypen." SKIP
              "Kan ikke vedlikeholdes i dette programmet."
              VIEW-AS ALERT-BOX TITLE "Melding".
      APPLY "close":U TO THIS-PROCEDURE.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION HentStr Dialog-Frame 
FUNCTION HentStr RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEF VAR wSoStorl AS CHAR NO-UNDO.        
  
  CASE wLoop:
    WHEN  1 THEN wSoStorl = tmpStrDef.Str[ 1]:screen-value IN FRAME Dialog-Frame.
  END CASE.
  
  RETURN wSoStorl.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

