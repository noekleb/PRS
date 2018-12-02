&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME F-Main
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS F-Main 
/*------------------------------------------------------------------------

  File: kalend1.w

  Description: Kalender dialog
               RETURN-VALUE ved OK: dagnavn, dagnr. månedsnavn år (f.eks. mandag 24. august 1997)
               RETURN-VALUE ved avbrryt: teksten "<avbryt>".

  Input Parameters:
      INPUT-OUTPUT dato (initiell verdi og retur)

  Output Parameters:
      <none>

  Author: Sturla Johnsen (C)

  Created: Oktober 1997
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */
&IF "{&UIB_is_Running}" <> "" &THEN
   DEF VAR wio-Dato AS DATE NO-UNDO INIT 5/21/1998.
   DEF VAR wTittel  AS CHAR NO-UNDO INIT "Velg dato".
   
&ELSE
   DEF INPUT-OUTPUT PARAMETER wio-Dato AS DATE NO-UNDO.
   DEF INPUT PARAMETER wTittel AS CHAR NO-UNDO.
&ENDIF   


/* Local Variable Definitions ---                                       */
DEF VAR wDag     AS INTE FORMAT "zz" EXTENT 37 NO-UNDO. /* Dagene på skjermen */
DEF VAR wDHdl    AS HANDLE           EXTENT 37 NO-UNDO. /* Handle til fill-in-feltene */
DEF VAR wCurrDag AS INTE NO-UNDO. /* Dagen som er merket */
DEF VAR wCurrExt AS INTE NO-UNDO. /* Extentnr. som er merket */
DEF VAR wNyExt   AS INTE NO-UNDO. /* Ny ext som skal merkes */
DEF VAR wMnd     AS INTE NO-UNDO. /* Valgt måned         */
DEF VAR wAar     AS INTE NO-UNDO. /* Valgt år            */

/* Farger */
DEF VAR wD-FgColor AS INTE NO-UNDO. /* Forgrunnsfarge for dager */
DEF VAR wS-FgColor AS INTE INIT 12 NO-UNDO. /* Forgrunn for søn- og helgedager */
DEF VAR wM-FgColor AS INTE INIT 15 NO-UNDO. /* Forgrunn for merkede dager untatt søn- og helgedager */
DEF VAR wM-BgColor AS INTE INIT 1  NO-UNDO. /* Bakgrunn for merkede dager */

DEF VAR wHDag AS CHAR NO-UNDO. /* Årests helgedager   */
DEF VAR wHMnd AS CHAR NO-UNDO. /* Korrsponderende mnd */

DEF VAR wRet-Verdi AS CHAR NO-UNDO. /* Return-verdi */

{runlib.i}
{windows.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME F-Main

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BUTTON-DUMMY BUTTON-OK BUTTON-Avbryt ~
BUTTON-ForrigeMnd BUTTON-NesteMnd COMBO-BOX-Mnd COMBO-BOX-Aar BUTTON-Idag ~
BUTTON-GoTo wU-1 wD-1 wD-2 wD-3 wD-4 wD-5 wD-6 wD-7 wU-2 wD-8 wD-9 wD-10 ~
wD-11 wD-12 wD-13 wD-14 wU-3 wD-15 wD-16 wD-17 wD-18 wD-19 wD-20 wD-21 wU-4 ~
wD-22 wD-23 wD-24 wD-25 wD-26 wD-27 wD-28 wU-5 wD-29 wD-30 wD-31 wD-32 ~
wD-33 wD-34 wD-35 wU-6 wD-36 wD-37 
&Scoped-Define DISPLAYED-OBJECTS COMBO-BOX-Mnd COMBO-BOX-Aar wU-1 wD-1 wD-2 ~
wD-3 wD-4 wD-5 wD-6 wD-7 wU-2 wD-8 wD-9 wD-10 wD-11 wD-12 wD-13 wD-14 wU-3 ~
wD-15 wD-16 wD-17 wD-18 wD-19 wD-20 wD-21 wU-4 wD-22 wD-23 wD-24 wD-25 ~
wD-26 wD-27 wD-28 wU-5 wD-29 wD-30 wD-31 wD-32 wD-33 wD-34 wD-35 wU-6 wD-36 ~
wD-37 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Menu Definitions                                                     */
DEFINE MENU POPUP-MENU-F-Main TITLE "Gå til"
       MENU-ITEM m_Idag         LABEL "&I dag"        
       RULE
       MENU-ITEM m_Julen        LABEL "&Julen"        
       MENU-ITEM m_Paasken      LABEL "&Påsken"       
       MENU-ITEM m_KrHfart      LABEL "&Kr. himmelfartsdag"
       MENU-ITEM m_Pinsen       LABEL "Pi&nsen"       .


/* Definitions of the field level widgets                               */
DEFINE BUTTON BUTTON-Avbryt AUTO-END-KEY 
     LABEL "Avbryt" 
     SIZE 8.2 BY 1.

DEFINE BUTTON BUTTON-DUMMY 
     LABEL "OK" 
     SIZE 1.4 BY .33.

DEFINE BUTTON BUTTON-ForrigeMnd 
     LABEL "<" 
     SIZE-PIXELS 18 BY 17.

DEFINE BUTTON BUTTON-GoTo 
     LABEL "&Gå til" 
     SIZE 7.6 BY 1.1.

DEFINE BUTTON BUTTON-Idag 
     LABEL "&I dag" 
     SIZE-PIXELS 41 BY 21.

DEFINE BUTTON BUTTON-NesteMnd 
     LABEL ">" 
     SIZE-PIXELS 18 BY 17.

DEFINE BUTTON BUTTON-OK AUTO-GO DEFAULT 
     LABEL "OK" 
     SIZE 8.2 BY 1.

DEFINE VARIABLE COMBO-BOX-Aar AS CHARACTER FORMAT "x(4)":U 
     LABEL "Combo 3" 
     VIEW-AS COMBO-BOX INNER-LINES 10
     LIST-ITEMS "","" 
     SIZE 11 BY 1 NO-UNDO.

DEFINE VARIABLE COMBO-BOX-Mnd AS CHARACTER FORMAT "X(10)":U INITIAL "September" 
     LABEL "Combo 2" 
     VIEW-AS COMBO-BOX INNER-LINES 12
     LIST-ITEMS "Januar","Februar","Mars","April","Mai","Juni","Juli","August","September","Oktober","November","Desember" 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE wD-1 AS INTEGER FORMAT "zz":U INITIAL 0 
     LABEL "Fill 1" 
      VIEW-AS TEXT 
     SIZE 2.6 BY .62 NO-UNDO.

DEFINE VARIABLE wD-10 AS INTEGER FORMAT "zz":U INITIAL 0 
     LABEL "Fill 1" 
      VIEW-AS TEXT 
     SIZE 2.6 BY .62 NO-UNDO.

DEFINE VARIABLE wD-11 AS INTEGER FORMAT "zz":U INITIAL 0 
     LABEL "Fill 1" 
      VIEW-AS TEXT 
     SIZE 2.6 BY .62 NO-UNDO.

DEFINE VARIABLE wD-12 AS INTEGER FORMAT "zz":U INITIAL 0 
     LABEL "Fill 1" 
      VIEW-AS TEXT 
     SIZE 2.6 BY .62 NO-UNDO.

DEFINE VARIABLE wD-13 AS INTEGER FORMAT "zz":U INITIAL 0 
     LABEL "Fill 1" 
      VIEW-AS TEXT 
     SIZE 2.6 BY .62 NO-UNDO.

DEFINE VARIABLE wD-14 AS INTEGER FORMAT "zz":U INITIAL 0 
     LABEL "Fill 1" 
      VIEW-AS TEXT 
     SIZE 2.6 BY .62
     FGCOLOR 12  NO-UNDO.

DEFINE VARIABLE wD-15 AS INTEGER FORMAT "zz":U INITIAL 0 
     LABEL "Fill 1" 
      VIEW-AS TEXT 
     SIZE 2.6 BY .62 NO-UNDO.

DEFINE VARIABLE wD-16 AS INTEGER FORMAT "zz":U INITIAL 0 
     LABEL "Fill 1" 
      VIEW-AS TEXT 
     SIZE 2.6 BY .62 NO-UNDO.

DEFINE VARIABLE wD-17 AS INTEGER FORMAT "zz":U INITIAL 0 
     LABEL "Fill 1" 
      VIEW-AS TEXT 
     SIZE 2.6 BY .62 NO-UNDO.

DEFINE VARIABLE wD-18 AS INTEGER FORMAT "zz":U INITIAL 0 
     LABEL "Fill 1" 
      VIEW-AS TEXT 
     SIZE 2.6 BY .62 NO-UNDO.

DEFINE VARIABLE wD-19 AS INTEGER FORMAT "zz":U INITIAL 0 
     LABEL "Fill 1" 
      VIEW-AS TEXT 
     SIZE 2.6 BY .62 NO-UNDO.

DEFINE VARIABLE wD-2 AS INTEGER FORMAT "zz":U INITIAL 0 
     LABEL "Fill 1" 
      VIEW-AS TEXT 
     SIZE 2.6 BY .62 NO-UNDO.

DEFINE VARIABLE wD-20 AS INTEGER FORMAT "zz":U INITIAL 0 
     LABEL "Fill 1" 
      VIEW-AS TEXT 
     SIZE 2.6 BY .62 NO-UNDO.

DEFINE VARIABLE wD-21 AS INTEGER FORMAT "zz":U INITIAL 0 
     LABEL "Fill 1" 
      VIEW-AS TEXT 
     SIZE 2.6 BY .62
     FGCOLOR 12  NO-UNDO.

DEFINE VARIABLE wD-22 AS INTEGER FORMAT "zz":U INITIAL 0 
     LABEL "Fill 1" 
      VIEW-AS TEXT 
     SIZE-PIXELS 13 BY 13 NO-UNDO.

DEFINE VARIABLE wD-23 AS INTEGER FORMAT "zz":U INITIAL 0 
     LABEL "Fill 1" 
      VIEW-AS TEXT 
     SIZE 2.6 BY .62 NO-UNDO.

DEFINE VARIABLE wD-24 AS INTEGER FORMAT "zz":U INITIAL 0 
     LABEL "Fill 1" 
      VIEW-AS TEXT 
     SIZE 2.6 BY .62 NO-UNDO.

DEFINE VARIABLE wD-25 AS INTEGER FORMAT "zz":U INITIAL 0 
     LABEL "Fill 1" 
      VIEW-AS TEXT 
     SIZE 2.6 BY .62 NO-UNDO.

DEFINE VARIABLE wD-26 AS INTEGER FORMAT "zz":U INITIAL 0 
     LABEL "Fill 1" 
      VIEW-AS TEXT 
     SIZE 2.6 BY .62 NO-UNDO.

DEFINE VARIABLE wD-27 AS INTEGER FORMAT "zz":U INITIAL 0 
     LABEL "Fill 1" 
      VIEW-AS TEXT 
     SIZE 2.6 BY .62 NO-UNDO.

DEFINE VARIABLE wD-28 AS INTEGER FORMAT "zz":U INITIAL 0 
     LABEL "Fill 1" 
      VIEW-AS TEXT 
     SIZE 2.6 BY .62
     FGCOLOR 12  NO-UNDO.

DEFINE VARIABLE wD-29 AS INTEGER FORMAT "zz":U INITIAL 0 
     LABEL "Fill 1" 
      VIEW-AS TEXT 
     SIZE 2.6 BY .62 NO-UNDO.

DEFINE VARIABLE wD-3 AS INTEGER FORMAT "zz":U INITIAL 0 
     LABEL "Fill 1" 
      VIEW-AS TEXT 
     SIZE 2.6 BY .62 NO-UNDO.

DEFINE VARIABLE wD-30 AS INTEGER FORMAT "zz":U INITIAL 0 
     LABEL "Fill 1" 
      VIEW-AS TEXT 
     SIZE 2.6 BY .62 NO-UNDO.

DEFINE VARIABLE wD-31 AS INTEGER FORMAT "zz":U INITIAL 0 
     LABEL "Fill 1" 
      VIEW-AS TEXT 
     SIZE 2.6 BY .62 NO-UNDO.

DEFINE VARIABLE wD-32 AS INTEGER FORMAT "zz":U INITIAL 0 
     LABEL "Fill 1" 
      VIEW-AS TEXT 
     SIZE 2.6 BY .62 NO-UNDO.

DEFINE VARIABLE wD-33 AS INTEGER FORMAT "zz":U INITIAL 0 
     LABEL "Fill 1" 
      VIEW-AS TEXT 
     SIZE 2.6 BY .62 NO-UNDO.

DEFINE VARIABLE wD-34 AS INTEGER FORMAT "zz":U INITIAL 0 
     LABEL "Fill 1" 
      VIEW-AS TEXT 
     SIZE 2.6 BY .62 NO-UNDO.

DEFINE VARIABLE wD-35 AS INTEGER FORMAT "zz":U INITIAL 0 
     LABEL "Fill 1" 
      VIEW-AS TEXT 
     SIZE 2.6 BY .62
     FGCOLOR 12  NO-UNDO.

DEFINE VARIABLE wD-36 AS INTEGER FORMAT "zz":U INITIAL 0 
     LABEL "Fill 1" 
      VIEW-AS TEXT 
     SIZE 2.6 BY .62 NO-UNDO.

DEFINE VARIABLE wD-37 AS INTEGER FORMAT "zz":U INITIAL 0 
     LABEL "Fill 1" 
      VIEW-AS TEXT 
     SIZE 2.6 BY .62 NO-UNDO.

DEFINE VARIABLE wD-4 AS INTEGER FORMAT "zz":U INITIAL 0 
     LABEL "Fill 1" 
      VIEW-AS TEXT 
     SIZE 2.6 BY .62 NO-UNDO.

DEFINE VARIABLE wD-5 AS INTEGER FORMAT "zz":U INITIAL 0 
     LABEL "Fill 1" 
      VIEW-AS TEXT 
     SIZE 2.6 BY .62 NO-UNDO.

DEFINE VARIABLE wD-6 AS INTEGER FORMAT "zz":U INITIAL 0 
     LABEL "Fill 1" 
      VIEW-AS TEXT 
     SIZE 2.6 BY .62 NO-UNDO.

DEFINE VARIABLE wD-7 AS INTEGER FORMAT "zz":U INITIAL 0 
     LABEL "Fill 1" 
      VIEW-AS TEXT 
     SIZE 2.6 BY .62
     FGCOLOR 12  NO-UNDO.

DEFINE VARIABLE wD-8 AS INTEGER FORMAT "zz":U INITIAL 0 
     LABEL "Fill 1" 
      VIEW-AS TEXT 
     SIZE 2.6 BY .62 NO-UNDO.

DEFINE VARIABLE wD-9 AS INTEGER FORMAT "zz":U INITIAL 0 
     LABEL "Fill 1" 
      VIEW-AS TEXT 
     SIZE 2.6 BY .62 NO-UNDO.

DEFINE VARIABLE wDagInfo AS CHARACTER FORMAT "x(40)":U INITIAL "mandag 24. september 1997" 
      VIEW-AS TEXT 
     SIZE 29.2 BY .62 NO-UNDO.

DEFINE VARIABLE wU-1 AS INTEGER FORMAT "zz":U INITIAL 53 
     LABEL "Fill 1" 
      VIEW-AS TEXT 
     SIZE 2.6 BY .62 NO-UNDO.

DEFINE VARIABLE wU-2 AS INTEGER FORMAT "zz":U INITIAL 53 
     LABEL "Fill 1" 
      VIEW-AS TEXT 
     SIZE 2.6 BY .62 NO-UNDO.

DEFINE VARIABLE wU-3 AS INTEGER FORMAT "zz":U INITIAL 53 
     LABEL "Fill 1" 
      VIEW-AS TEXT 
     SIZE-PIXELS 13 BY 13 NO-UNDO.

DEFINE VARIABLE wU-4 AS INTEGER FORMAT "zz":U INITIAL 53 
     LABEL "Fill 1" 
      VIEW-AS TEXT 
     SIZE-PIXELS 13 BY 13 NO-UNDO.

DEFINE VARIABLE wU-5 AS INTEGER FORMAT "zz":U INITIAL 53 
     LABEL "Fill 1" 
      VIEW-AS TEXT 
     SIZE-PIXELS 13 BY 13 NO-UNDO.

DEFINE VARIABLE wU-6 AS INTEGER FORMAT "zz":U INITIAL 53 
     LABEL "Fill 1" 
      VIEW-AS TEXT 
     SIZE-PIXELS 13 BY 13 NO-UNDO.

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 30.4 BY 4.67.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 4.8 BY 4.67.

DEFINE RECTANGLE RECT-8
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 39.8 BY 7.86.

DEFINE RECTANGLE RECT-9
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 9.8 BY 6.76.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     BUTTON-DUMMY AT ROW 1.67 COL 35.8
     BUTTON-OK AT ROW 5.52 COL 32.6
     BUTTON-Avbryt AT ROW 6.67 COL 32.6
     BUTTON-ForrigeMnd AT Y 147 X 25 RIGHT-ALIGNED
     BUTTON-NesteMnd AT Y 147 X 198 RIGHT-ALIGNED
     COMBO-BOX-Mnd AT ROW 1.33 COL 2.4
     COMBO-BOX-Aar AT ROW 1.33 COL 20.2
     BUTTON-Idag AT Y 8 X 158
     wDagInfo AT ROW 8.1 COL 35.2 RIGHT-ALIGNED NO-LABEL
     BUTTON-GoTo AT ROW 2.43 COL 33
     wU-1 AT ROW 3.43 COL 2.8
     wD-1 AT ROW 3.43 COL 7
     wD-2 AT ROW 3.43 COL 10.6
     wD-3 AT ROW 3.43 COL 14.2
     wD-4 AT ROW 3.43 COL 17.8
     wD-5 AT ROW 3.43 COL 21.4
     wD-6 AT ROW 3.43 COL 24.6
     wD-7 AT ROW 3.43 COL 28.2
     wU-2 AT ROW 4.14 COL 2.8
     wD-8 AT ROW 4.14 COL 7
     wD-9 AT ROW 4.14 COL 10.6
     wD-10 AT ROW 4.14 COL 14.2
     wD-11 AT ROW 4.14 COL 17.8
     wD-12 AT ROW 4.14 COL 21.4
     wD-13 AT ROW 4.14 COL 24.6
     wD-14 AT ROW 4.14 COL 28.2
     wU-3 AT Y 81 X 9
     wD-15 AT ROW 4.86 COL 7
     wD-16 AT ROW 4.86 COL 10.6
     wD-17 AT ROW 4.86 COL 14.2
     wD-18 AT ROW 4.86 COL 17.8
     wD-19 AT ROW 4.86 COL 21.4
     wD-20 AT ROW 4.86 COL 24.6
     wD-21 AT ROW 4.86 COL 28.2
     wU-4 AT Y 96 X 9
     wD-22 AT Y 96 X 30
     wD-23 AT ROW 5.57 COL 10.6
     wD-24 AT ROW 5.57 COL 14.2
     wD-25 AT ROW 5.57 COL 17.8
     wD-26 AT ROW 5.57 COL 21.4
     wD-27 AT ROW 5.57 COL 24.6
     wD-28 AT ROW 5.57 COL 28.2
     wU-5 AT Y 111 X 9
     wD-29 AT ROW 6.29 COL 7
     wD-30 AT ROW 6.29 COL 10.6
     wD-31 AT ROW 6.29 COL 14.2
     wD-32 AT ROW 6.29 COL 17.8
     wD-33 AT ROW 6.29 COL 21.4
     wD-34 AT ROW 6.29 COL 24.6
     wD-35 AT ROW 6.29 COL 28.2
     wU-6 AT Y 126 X 9
     wD-36 AT ROW 7 COL 7
     wD-37 AT ROW 7 COL 10.6
     RECT-4 AT ROW 3.24 COL 1.8
     RECT-6 AT ROW 3.24 COL 1.8
     RECT-8 AT ROW 1.14 COL 1.8
     RECT-9 AT ROW 1.14 COL 31.8
     " m    t    o    t     f      l    s" VIEW-AS TEXT
          SIZE 24 BY .48 AT ROW 2.57 COL 7.2
     SPACE(10.40) SKIP(5.95)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         NO-LABELS NO-UNDERLINE NO-VALIDATE THREE-D  SCROLLABLE 
         TITLE "Kalender"
         DEFAULT-BUTTON BUTTON-OK CANCEL-BUTTON BUTTON-Avbryt.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: DIALOG-BOX
   Allow: Basic,Browse,DB-Fields,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX F-Main
   Custom                                                               */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE
       FRAME F-Main:POPUP-MENU       = MENU POPUP-MENU-F-Main:HANDLE.

ASSIGN 
       BUTTON-DUMMY:SELECTABLE IN FRAME F-Main       = TRUE.

/* SETTINGS FOR BUTTON BUTTON-ForrigeMnd IN FRAME F-Main
   ALIGN-R                                                              */
/* SETTINGS FOR BUTTON BUTTON-NesteMnd IN FRAME F-Main
   ALIGN-R                                                              */
/* SETTINGS FOR COMBO-BOX COMBO-BOX-Aar IN FRAME F-Main
   ALIGN-L LABEL "Combo 3:"                                             */
/* SETTINGS FOR COMBO-BOX COMBO-BOX-Mnd IN FRAME F-Main
   ALIGN-L LABEL "Combo 2:"                                             */
/* SETTINGS FOR RECTANGLE RECT-4 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE RECT-6 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE RECT-8 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE RECT-9 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN wD-1 IN FRAME F-Main
   ALIGN-L LABEL "Fill 1:"                                              */
ASSIGN 
       wD-1:SELECTABLE IN FRAME F-Main       = TRUE.

/* SETTINGS FOR FILL-IN wD-10 IN FRAME F-Main
   ALIGN-L LABEL "Fill 1:"                                              */
ASSIGN 
       wD-10:SELECTABLE IN FRAME F-Main       = TRUE.

/* SETTINGS FOR FILL-IN wD-11 IN FRAME F-Main
   ALIGN-L LABEL "Fill 1:"                                              */
ASSIGN 
       wD-11:SELECTABLE IN FRAME F-Main       = TRUE.

/* SETTINGS FOR FILL-IN wD-12 IN FRAME F-Main
   ALIGN-L LABEL "Fill 1:"                                              */
ASSIGN 
       wD-12:SELECTABLE IN FRAME F-Main       = TRUE.

/* SETTINGS FOR FILL-IN wD-13 IN FRAME F-Main
   ALIGN-L LABEL "Fill 1:"                                              */
ASSIGN 
       wD-13:SELECTABLE IN FRAME F-Main       = TRUE.

/* SETTINGS FOR FILL-IN wD-14 IN FRAME F-Main
   ALIGN-L LABEL "Fill 1:"                                              */
ASSIGN 
       wD-14:SELECTABLE IN FRAME F-Main       = TRUE.

/* SETTINGS FOR FILL-IN wD-15 IN FRAME F-Main
   ALIGN-L LABEL "Fill 1:"                                              */
ASSIGN 
       wD-15:SELECTABLE IN FRAME F-Main       = TRUE.

/* SETTINGS FOR FILL-IN wD-16 IN FRAME F-Main
   ALIGN-L LABEL "Fill 1:"                                              */
ASSIGN 
       wD-16:SELECTABLE IN FRAME F-Main       = TRUE.

/* SETTINGS FOR FILL-IN wD-17 IN FRAME F-Main
   ALIGN-L LABEL "Fill 1:"                                              */
ASSIGN 
       wD-17:SELECTABLE IN FRAME F-Main       = TRUE.

/* SETTINGS FOR FILL-IN wD-18 IN FRAME F-Main
   ALIGN-L LABEL "Fill 1:"                                              */
ASSIGN 
       wD-18:SELECTABLE IN FRAME F-Main       = TRUE.

/* SETTINGS FOR FILL-IN wD-19 IN FRAME F-Main
   ALIGN-L LABEL "Fill 1:"                                              */
ASSIGN 
       wD-19:SELECTABLE IN FRAME F-Main       = TRUE.

/* SETTINGS FOR FILL-IN wD-2 IN FRAME F-Main
   ALIGN-L LABEL "Fill 1:"                                              */
ASSIGN 
       wD-2:SELECTABLE IN FRAME F-Main       = TRUE.

/* SETTINGS FOR FILL-IN wD-20 IN FRAME F-Main
   ALIGN-L LABEL "Fill 1:"                                              */
ASSIGN 
       wD-20:SELECTABLE IN FRAME F-Main       = TRUE.

/* SETTINGS FOR FILL-IN wD-21 IN FRAME F-Main
   ALIGN-L LABEL "Fill 1:"                                              */
ASSIGN 
       wD-21:SELECTABLE IN FRAME F-Main       = TRUE.

/* SETTINGS FOR FILL-IN wD-22 IN FRAME F-Main
   ALIGN-L LABEL "Fill 1:"                                              */
ASSIGN 
       wD-22:SELECTABLE IN FRAME F-Main       = TRUE.

/* SETTINGS FOR FILL-IN wD-23 IN FRAME F-Main
   ALIGN-L LABEL "Fill 1:"                                              */
ASSIGN 
       wD-23:SELECTABLE IN FRAME F-Main       = TRUE.

/* SETTINGS FOR FILL-IN wD-24 IN FRAME F-Main
   ALIGN-L LABEL "Fill 1:"                                              */
ASSIGN 
       wD-24:SELECTABLE IN FRAME F-Main       = TRUE.

/* SETTINGS FOR FILL-IN wD-25 IN FRAME F-Main
   ALIGN-L LABEL "Fill 1:"                                              */
ASSIGN 
       wD-25:SELECTABLE IN FRAME F-Main       = TRUE.

/* SETTINGS FOR FILL-IN wD-26 IN FRAME F-Main
   ALIGN-L LABEL "Fill 1:"                                              */
ASSIGN 
       wD-26:SELECTABLE IN FRAME F-Main       = TRUE.

/* SETTINGS FOR FILL-IN wD-27 IN FRAME F-Main
   ALIGN-L LABEL "Fill 1:"                                              */
ASSIGN 
       wD-27:SELECTABLE IN FRAME F-Main       = TRUE.

/* SETTINGS FOR FILL-IN wD-28 IN FRAME F-Main
   ALIGN-L LABEL "Fill 1:"                                              */
ASSIGN 
       wD-28:SELECTABLE IN FRAME F-Main       = TRUE.

/* SETTINGS FOR FILL-IN wD-29 IN FRAME F-Main
   ALIGN-L LABEL "Fill 1:"                                              */
ASSIGN 
       wD-29:SELECTABLE IN FRAME F-Main       = TRUE.

/* SETTINGS FOR FILL-IN wD-3 IN FRAME F-Main
   ALIGN-L LABEL "Fill 1:"                                              */
ASSIGN 
       wD-3:SELECTABLE IN FRAME F-Main       = TRUE.

/* SETTINGS FOR FILL-IN wD-30 IN FRAME F-Main
   ALIGN-L LABEL "Fill 1:"                                              */
ASSIGN 
       wD-30:SELECTABLE IN FRAME F-Main       = TRUE.

/* SETTINGS FOR FILL-IN wD-31 IN FRAME F-Main
   ALIGN-L LABEL "Fill 1:"                                              */
ASSIGN 
       wD-31:SELECTABLE IN FRAME F-Main       = TRUE.

/* SETTINGS FOR FILL-IN wD-32 IN FRAME F-Main
   ALIGN-L LABEL "Fill 1:"                                              */
ASSIGN 
       wD-32:SELECTABLE IN FRAME F-Main       = TRUE.

/* SETTINGS FOR FILL-IN wD-33 IN FRAME F-Main
   ALIGN-L LABEL "Fill 1:"                                              */
ASSIGN 
       wD-33:SELECTABLE IN FRAME F-Main       = TRUE.

/* SETTINGS FOR FILL-IN wD-34 IN FRAME F-Main
   ALIGN-L LABEL "Fill 1:"                                              */
ASSIGN 
       wD-34:SELECTABLE IN FRAME F-Main       = TRUE.

/* SETTINGS FOR FILL-IN wD-35 IN FRAME F-Main
   ALIGN-L LABEL "Fill 1:"                                              */
ASSIGN 
       wD-35:SELECTABLE IN FRAME F-Main       = TRUE.

/* SETTINGS FOR FILL-IN wD-36 IN FRAME F-Main
   ALIGN-L LABEL "Fill 1:"                                              */
ASSIGN 
       wD-36:SELECTABLE IN FRAME F-Main       = TRUE.

/* SETTINGS FOR FILL-IN wD-37 IN FRAME F-Main
   ALIGN-L LABEL "Fill 1:"                                              */
ASSIGN 
       wD-37:SELECTABLE IN FRAME F-Main       = TRUE.

/* SETTINGS FOR FILL-IN wD-4 IN FRAME F-Main
   ALIGN-L LABEL "Fill 1:"                                              */
ASSIGN 
       wD-4:SELECTABLE IN FRAME F-Main       = TRUE.

/* SETTINGS FOR FILL-IN wD-5 IN FRAME F-Main
   ALIGN-L LABEL "Fill 1:"                                              */
ASSIGN 
       wD-5:SELECTABLE IN FRAME F-Main       = TRUE.

/* SETTINGS FOR FILL-IN wD-6 IN FRAME F-Main
   ALIGN-L LABEL "Fill 1:"                                              */
ASSIGN 
       wD-6:SELECTABLE IN FRAME F-Main       = TRUE.

/* SETTINGS FOR FILL-IN wD-7 IN FRAME F-Main
   ALIGN-L LABEL "Fill 1:"                                              */
ASSIGN 
       wD-7:SELECTABLE IN FRAME F-Main       = TRUE.

/* SETTINGS FOR FILL-IN wD-8 IN FRAME F-Main
   ALIGN-L LABEL "Fill 1:"                                              */
ASSIGN 
       wD-8:SELECTABLE IN FRAME F-Main       = TRUE.

/* SETTINGS FOR FILL-IN wD-9 IN FRAME F-Main
   ALIGN-L LABEL "Fill 1:"                                              */
ASSIGN 
       wD-9:SELECTABLE IN FRAME F-Main       = TRUE.

/* SETTINGS FOR FILL-IN wDagInfo IN FRAME F-Main
   NO-DISPLAY NO-ENABLE ALIGN-R                                         */
/* SETTINGS FOR FILL-IN wU-1 IN FRAME F-Main
   ALIGN-L LABEL "Fill 1:"                                              */
/* SETTINGS FOR FILL-IN wU-2 IN FRAME F-Main
   ALIGN-L LABEL "Fill 1:"                                              */
/* SETTINGS FOR FILL-IN wU-3 IN FRAME F-Main
   ALIGN-L LABEL "Fill 1:"                                              */
/* SETTINGS FOR FILL-IN wU-4 IN FRAME F-Main
   ALIGN-L LABEL "Fill 1:"                                              */
/* SETTINGS FOR FILL-IN wU-5 IN FRAME F-Main
   ALIGN-L LABEL "Fill 1:"                                              */
/* SETTINGS FOR FILL-IN wU-6 IN FRAME F-Main
   ALIGN-L LABEL "Fill 1:"                                              */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX F-Main
/* Query rebuild information for DIALOG-BOX F-Main
     _Options          = "SHARE-LOCK"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX F-Main */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME F-Main
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL F-Main F-Main
ON CURSOR-DOWN OF FRAME F-Main /* Kalender */
ANYWHERE DO:
  DEF VAR i AS INTE NO-UNDO.
  IF wCurrExt < 30 THEN ASSIGN i = wCurrExt + 7.
  ELSE                  ASSIGN i = 37.
  IF wDag[i] = 0 OR wCurrExt = 37 THEN RETURN NO-APPLY.
    
  APPLY "MOUSE-SELECT-DOWN" TO wDhdl[i].
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL F-Main F-Main
ON CURSOR-LEFT OF FRAME F-Main /* Kalender */
ANYWHERE DO:
   DEF VAR i AS INTE NO-UNDO.
   IF wCurrExt = 1 OR wDag[wCurrExt - 1] = 0 THEN DO WITH FRAME {&FRAME-NAME}:
      APPLY "CHOOSE" TO BUTTON-ForrigeMnd.
      RUN UnMarkDay.
      DO i = 37 TO 1 BY -1:
         If wDag[i] > 0 then do:
            ASSIGN wCurrExt = i + 1.
            LEAVE.
         END.
      END.
   END.
   APPLY "MOUSE-SELECT-DOWN" TO wDhdl[IF wCurrExt > 1 THEN wCurrExt - 1 ELSE 37].
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL F-Main F-Main
ON CURSOR-RIGHT OF FRAME F-Main /* Kalender */
ANYWHERE DO:
   DEF VAR i AS INTE NO-UNDO.
   IF wCurrExt = 37 OR wDag[wCurrExt + 1] = 0 THEN DO WITH FRAME {&FRAME-NAME}:
      APPLY "CHOOSE" TO BUTTON-NesteMnd.
      RUN UnMarkDay.
      DO i = 1 TO 7:
         If wDag[i] > 0 then do:
            ASSIGN wCurrExt = i - 1.
            LEAVE.
         END.
      END.
   END.
   APPLY "MOUSE-SELECT-DOWN" TO wDhdl[IF wCurrExt < 37 THEN wCurrExt + 1 ELSE 1].
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL F-Main F-Main
ON CURSOR-UP OF FRAME F-Main /* Kalender */
ANYWHERE DO:
  DEF VAR i AS INTE NO-UNDO.
  IF wCurrExt > 7 THEN ASSIGN i = wCurrExt - 7.
  ELSE                 ASSIGN i = 1.
  IF wDag[i] = 0 OR wCurrExt = 1 THEN RETURN NO-APPLY.
    
  APPLY "MOUSE-SELECT-DOWN" TO wDhdl[i].
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL F-Main F-Main
ON GO OF FRAME F-Main /* Kalender */
DO:
   ASSIGN 
      wio-Dato   = DATE(wMnd,wCurrDag,wAar)
      wRet-Verdi = ENTRY(WEEKDAY(DATE(wMnd,wCurrDag,wAar)),"søndag,mandag,tirsdag,onsdag,torsdag,fredag,lørdag") +
                          ", " + STRING(wCurrDag) + ". " + LC(COMBO-BOX-Mnd:SCREEN-VALUE IN FRAME {&FRAME-NAME}) + " " + STRING(wAar).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL F-Main F-Main
ON HELP OF FRAME F-Main /* Kalender */
DO:
  {diahelp.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL F-Main F-Main
ON PAGE-DOWN OF FRAME F-Main /* Kalender */
ANYWHERE DO:
  APPLY "CHOOSE" TO BUTTON-NesteMnd.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL F-Main F-Main
ON PAGE-UP OF FRAME F-Main /* Kalender */
ANYWHERE DO:
  APPLY "CHOOSE" TO BUTTON-ForrigeMnd.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL F-Main F-Main
ON WINDOW-CLOSE OF FRAME F-Main /* Kalender */
DO:
   ASSIGN wRet-Verdi = "<avbryt>".
   APPLY "END-ERROR":U TO SELF.
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Avbryt
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Avbryt F-Main
ON CHOOSE OF BUTTON-Avbryt IN FRAME F-Main /* Avbryt */
DO:
   ASSIGN wRet-Verdi = "<avbryt>".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-DUMMY
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-DUMMY F-Main
ON CHOOSE OF BUTTON-DUMMY IN FRAME F-Main /* OK */
DO:
     
   APPLY "CHOOSE" TO BUTTON-OK IN FRAME {&FRAME-NAME}.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-ForrigeMnd
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-ForrigeMnd F-Main
ON CHOOSE OF BUTTON-ForrigeMnd IN FRAME F-Main /* < */
DO:
  DEF VAR wSaveAar AS INTE NO-UNDO.
  APPLY "ENTRY" TO BUTTON-DUMMY. /* Fjern fokus (fordi knappen er så liten) */
  ASSIGN 
    wSaveAar = wAar
    wAar = wAar - INT(wMnd = 1)
    wMnd = IF wMnd = 1 THEN 12 ELSE (wMnd - 1).
  IF NOT CAN-DO(COMBO-BOX-Aar:LIST-ITEMS IN FRAME {&FRAME-NAME},STRING(wAar)) THEN
    IF COMBO-BOX-Aar:ADD-FIRST(STRING(wAar)) THEN.  
  RUN InitNyMnd(wSaveAar <> wAar).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-GoTo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-GoTo F-Main
ON CHOOSE OF BUTTON-GoTo IN FRAME F-Main /* Gå til */
DO:
  APPLY "ENTRY" TO BUTTON-DUMMY. /* Fjern fokus (fordi knappen er så liten) */
  run CenterMouseCursor(self:handle).
  run Apply-mouse-menu-click(self:handle).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Idag
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Idag F-Main
ON CHOOSE OF BUTTON-Idag IN FRAME F-Main /* I dag */
DO:
  APPLY "ENTRY" TO BUTTON-DUMMY. /* Fjern fokus (fordi knappen er så liten) */
  
  IF wAar <> YEAR(TODAY) OR wMnd <> MONTH(TODAY) OR DAY(TODAY) <> wCurrDag THEN DO:
     ASSIGN 
       wAar = YEAR(TODAY)
       wMnd = MONTH(TODAY).
     RUN FillAarCombo.  
     RUN UnMarkDay.
     ASSIGN wCurrDag = DAY(TODAY).
     RUN FillDag.
     RUN VisDag.
     APPLY "VALUE-CHANGED" TO COMBO-BOX-Aar.
     PROCESS EVENTS.
     IF wNyExt <> wCurrExt THEN DO:
        RUN ForceFlyttMark(wNyExt).
        RUN MarkDay.
     END.   
   END.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-NesteMnd
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-NesteMnd F-Main
ON CHOOSE OF BUTTON-NesteMnd IN FRAME F-Main /* > */
DO:
  DEF VAR wSaveAar AS INTE NO-UNDO.
  APPLY "ENTRY" TO BUTTON-DUMMY. /* Fjerner fokus (fordi knappen er så liten) */
  
  ASSIGN 
    wSaveAar = wAar 
    wAar = wAar + INT(wMnd = 12)
    wMnd = IF wMnd = 12 THEN 1 ELSE (wMnd + 1).
  IF NOT CAN-DO(COMBO-BOX-Aar:LIST-ITEMS IN FRAME {&FRAME-NAME},STRING(wAar)) THEN
     IF COMBO-BOX-Aar:ADD-LAST(STRING(wAar)) THEN.
  RUN InitNyMnd(wSaveAar <> wAar).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME COMBO-BOX-Aar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL COMBO-BOX-Aar F-Main
ON VALUE-CHANGED OF COMBO-BOX-Aar IN FRAME F-Main /* Combo 3 */
DO:
  ASSIGN wAar = INT(SELF:SCREEN-VALUE).
  RUN FixPopupMeny.
  RUN FinnHellig(OUTPUT wHdag, OUTPUT wHmnd).
  /* Hvis det er prosedyren InitNyMnd som har "forårsaket" 
     at nytt år er satt, skal den ikke kjøres. */
  IF NOT PROGRAM-NAME(2) BEGINS "InitNyMnd" THEN RUN InitNyMnd(NO).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME COMBO-BOX-Mnd
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL COMBO-BOX-Mnd F-Main
ON VALUE-CHANGED OF COMBO-BOX-Mnd IN FRAME F-Main /* Combo 2 */
DO:
  ASSIGN wMnd = LOOKUP(SELF:SCREEN-VALUE,SELF:LIST-ITEMS).
  RUN SettNyMnd.
  RUN SettHellig.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Idag
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Idag F-Main
ON CHOOSE OF MENU-ITEM m_Idag /* I dag */
DO:
  APPLY "CHOOSE" TO BUTTON-Idag IN FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Julen
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Julen F-Main
ON CHOOSE OF MENU-ITEM m_Julen /* Julen */
DO:
    ASSIGN 
       wMnd = 12 
       wCurrDag = 25.
    RUN InitNyMnd(NO). /* Året er ikke endret */  
       
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_KrHfart
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_KrHfart F-Main
ON CHOOSE OF MENU-ITEM m_KrHfart /* Kr. himmelfartsdag */
DO:
  IF wAar < 1700 OR wAar > 2299 THEN 
     MESSAGE "Kjenner ikke de bevegelige helligdagen før år 1700 og etter år 2299." VIEW-AS ALERT-BOX ERROR TITLE "Gå til...".
  ELSE DO:
     ASSIGN 
       wMnd     = INT(ENTRY(11,wHMnd))
       wCurrDag = INT(ENTRY(11,wHDag)).
    RUN InitNyMnd(NO). /* Året er ikke endret */  
  END.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Paasken
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Paasken F-Main
ON CHOOSE OF MENU-ITEM m_Paasken /* Påsken */
DO:
  IF wAar < 1700 OR wAar > 2299 THEN 
     MESSAGE "Kjenner ikke de bevegelige helligdagen før år 1700 og etter år 2299." VIEW-AS ALERT-BOX ERROR TITLE "Gå til...".
  ELSE DO:
     ASSIGN 
       wMnd     = INT(ENTRY(6,wHMnd))
       wCurrDag = INT(ENTRY(6,wHDag)).
    RUN InitNyMnd(NO). /* Året er ikke endret */  
  END.      
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Pinsen
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Pinsen F-Main
ON CHOOSE OF MENU-ITEM m_Pinsen /* Pinsen */
DO:
  IF wAar < 1700 OR wAar > 2299 THEN 
     MESSAGE "Kjenner ikke de bevegelige helligdagen før år 1700 og etter år 2299." VIEW-AS ALERT-BOX ERROR TITLE "Gå til...".
  ELSE DO:
     ASSIGN 
       wMnd     = INT(ENTRY(12,wHMnd))
       wCurrDag = INT(ENTRY(12,wHDag)).
    RUN InitNyMnd(NO). /* Året er ikke endret */  
  END.      
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME wD-27
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wD-27 F-Main
ON MOUSE-SELECT-DOWN OF wD-27 IN FRAME F-Main /* Fill 1 */
DO:
   RUN ByttMark.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK F-Main 


/* ***************************  Main Block  *************************** */

/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT eq ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.


/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
DO WITH FRAME {&FRAME-NAME}:
   ON "MOUSE-SELECT-DBLCLICK" OF 
       wD-1 , wD-2 , wD-3 , wD-4 , wD-5 , wD-6 , wD-7 , 
       wD-8 , wD-9 , wD-10, wD-11, wD-12, wD-13, wD-14,
       wD-15, wD-16, wD-17, wD-18, wD-19, wD-20, wD-21,
       wD-22, wD-23, wD-24, wD-25, wD-26, wD-27, wD-28,
       wD-29, wD-30, wD-31, wD-32, wD-33, wD-34, wD-35,
       wD-36, wD-37
   DO:
      RUN DblKlikk. 
   END.    
   ON "MOUSE-SELECT-DOWN" OF 
       wD-1 , wD-2 , wD-3 , wD-4 , wD-5 , wD-6 , wD-7 , 
       wD-8 , wD-9 , wD-10, wD-11, wD-12, wD-13, wD-14,
       wD-15, wD-16, wD-17, wD-18, wD-19, wD-20, wD-21,
       wD-22, wD-23, wD-24, wD-25, wD-26, wD-27, wD-28,
       wD-29, wD-30, wD-31, wD-32, wD-33, wD-34, wD-35,
       wD-36, wD-37
   DO:
      RUN ByttMark. 
   END.    
   
END.
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

  RUN enable_UI.
    
  ASSIGN 
     FRAME {&FRAME-NAME}:TITLE  = IF wTittel <> "" THEN wTittel ELSE "Kalender".

  RUN InitierOppstart.
  
  WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.
RETURN wRet-Verdi.
  /************************ INTERNAL PROCEDURES ********************/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Apply-Mouse-Menu-Click F-Main 
PROCEDURE Apply-Mouse-Menu-Click :
/*------------------------------------------------------------------------------
Purpose:     Programatic click the right mouse button on a widget
Parameters:  Widget-handle on which you want to click
------------------------------------------------------------------------------*/
   DEFINE INPUT PARAMETER  p-wh   AS WIDGET-HANDLE  NO-UNDO.
   DEF VAR ReturnValue AS INTEGER NO-UNDO.
   RUN SendMessage{&A} in hpApi (INPUT p-wh:HWND, 
                                 INPUT {&WM_RBUTTONDOWN},
                                 INPUT {&MK_RBUTTON},
                                 INPUT 0,
                                 OUTPUT ReturnValue).
   RUN SendMessage{&A} in hpApi (INPUT p-wh:HWND, 
                                 INPUT {&WM_RBUTTONUP},
                                 INPUT 0, 
                                 INPUT 0,
                                 OUTPUT ReturnValue).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ByttMark F-Main 
PROCEDURE ByttMark :
/*------------------------------------------------------------------------------
  Purpose:     Sjekker om avmerket dag skal byttes.
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DO WITH FRAME {&FRAME-NAME}:
      IF SELF:SCREEN-VALUE <> "" THEN DO: /* Er dagen utfylt */
         RUN UnMarkDay.
         RUN MarkDay.
         ASSIGN 
           wCurrExt = INT(SUBSTR(SELF:NAME,4))
           wNyExt   = wCurrExt.
      END.   
   END.   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CenterMouseCursor F-Main 
PROCEDURE CenterMouseCursor :
/*------------------------------------------------------------------------------
Purpose:     Move the mouse cursor to the middle of a widget
Parameters:  the widget-handle
------------------------------------------------------------------------------*/
   DEFINE INPUT PARAMETER  p-wh   AS WIDGET-HANDLE  NO-UNDO.
   DEF VAR lppoint     AS MEMPTR  NO-UNDO.  /* POINT FAR*  */
   DEF VAR ReturnValue AS INTEGER NO-UNDO.   SET-SIZE(lppoint)= 2 * {&INTSIZE}.
   PUT-{&INT}(lppoint,1 + 0 * {&INTSIZE})=INTEGER(p-wh:WIDTH-PIXELS / 2).
   PUT-{&INT}(lppoint,1 + 1 * {&INTSIZE})=INTEGER(p-wh:HEIGHT-PIXELS / 2).
   RUN ClientToScreen in hpApi (INPUT p-wh:HWND, 
                                INPUT GET-POINTER-VALUE(lppoint),
                                OUTPUT ReturnValue).
   RUN SetCursorPos in hpApi   (INPUT GET-{&INT}(lppoint,1 + 0 * {&INTSIZE}), 
                                INPUT GET-{&INT}(lppoint,1 + 1 * {&INTSIZE}),
                                OUTPUT ReturnValue).   SET-SIZE(lppoint)= 0.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ClearFarge F-Main 
PROCEDURE ClearFarge :
/*------------------------------------------------------------------------------
  Purpose:     Fjerner evt. røde helligdager
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  &SCOP NyFarge :FGCOLOR = wM-FgColor THEN wM-FgColor ELSE ?
  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN
      wD-1:FGCOLOR  = IF wD-1{&NyFarge}
      wD-2:FGCOLOR  = IF wD-2{&NyFarge}
      wD-3:FGCOLOR  = IF wD-3{&NyFarge}
      wD-4:FGCOLOR  = IF wD-4{&NyFarge}
      wD-5:FGCOLOR  = IF wD-5{&NyFarge}
      wD-6:FGCOLOR  = IF wD-6{&NyFarge}
      wD-8:FGCOLOR  = IF wD-8{&NyFarge}
      wD-9:FGCOLOR  = IF wD-9{&NyFarge}
      wD-10:FGCOLOR = IF wD-10{&NyFarge}
      wD-11:FGCOLOR = IF wD-11{&NyFarge}
      wD-12:FGCOLOR = IF wD-12{&NyFarge}
      wD-13:FGCOLOR = IF wD-13{&NyFarge}
      wD-15:FGCOLOR = IF wD-15{&NyFarge}
      wD-16:FGCOLOR = IF wD-16{&NyFarge}
      wD-17:FGCOLOR = IF wD-17{&NyFarge}
      wD-18:FGCOLOR = IF wD-18{&NyFarge}
      wD-19:FGCOLOR = IF wD-19{&NyFarge}
      wD-20:FGCOLOR = IF wD-20{&NyFarge}
      wD-22:FGCOLOR = IF wD-22{&NyFarge}
      wD-23:FGCOLOR = IF wD-23{&NyFarge}
      wD-24:FGCOLOR = IF wD-24{&NyFarge}
      wD-25:FGCOLOR = IF wD-25{&NyFarge}
      wD-26:FGCOLOR = IF wD-26{&NyFarge}
      wD-27:FGCOLOR = IF wD-27{&NyFarge}
      wD-29:FGCOLOR = IF wD-29{&NyFarge}
      wD-30:FGCOLOR = IF wD-30{&NyFarge}
      wD-31:FGCOLOR = IF wD-31{&NyFarge}
      wD-32:FGCOLOR = IF wD-32{&NyFarge}
      wD-33:FGCOLOR = IF wD-33{&NyFarge}
      wD-34:FGCOLOR = IF wD-34{&NyFarge}
      wD-36:FGCOLOR = IF wD-36{&NyFarge}
      wD-37:FGCOLOR = IF wD-37{&NyFarge}.
   END.   

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DblKlikk F-Main 
PROCEDURE DblKlikk :
/*------------------------------------------------------------------------------
  Purpose: Sjekker dobbelklikk på en dag.    
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DO WITH FRAME {&FRAME-NAME}:
      IF SELF:SCREEN-VALUE <> "" THEN /* Er dagen utfylt */
         APPLY "CHOOSE" TO BUTTON-OK.
   END.   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI F-Main _DEFAULT-DISABLE
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
  HIDE FRAME F-Main.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI F-Main _DEFAULT-ENABLE
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
  DISPLAY COMBO-BOX-Mnd COMBO-BOX-Aar wU-1 wD-1 wD-2 wD-3 wD-4 wD-5 wD-6 wD-7 
          wU-2 wD-8 wD-9 wD-10 wD-11 wD-12 wD-13 wD-14 wU-3 wD-15 wD-16 wD-17 
          wD-18 wD-19 wD-20 wD-21 wU-4 wD-22 wD-23 wD-24 wD-25 wD-26 wD-27 wD-28 
          wU-5 wD-29 wD-30 wD-31 wD-32 wD-33 wD-34 wD-35 wU-6 wD-36 wD-37 
      WITH FRAME F-Main.
  ENABLE BUTTON-DUMMY BUTTON-OK BUTTON-Avbryt BUTTON-ForrigeMnd BUTTON-NesteMnd 
         COMBO-BOX-Mnd COMBO-BOX-Aar BUTTON-Idag BUTTON-GoTo wU-1 wD-1 wD-2 
         wD-3 wD-4 wD-5 wD-6 wD-7 wU-2 wD-8 wD-9 wD-10 wD-11 wD-12 wD-13 wD-14 
         wU-3 wD-15 wD-16 wD-17 wD-18 wD-19 wD-20 wD-21 wU-4 wD-22 wD-23 wD-24 
         wD-25 wD-26 wD-27 wD-28 wU-5 wD-29 wD-30 wD-31 wD-32 wD-33 wD-34 wD-35 
         wU-6 wD-36 wD-37 
      WITH FRAME F-Main.
  VIEW FRAME F-Main.
  {&OPEN-BROWSERS-IN-QUERY-F-Main}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE FillAarCombo F-Main 
PROCEDURE FillAarCombo :
/*------------------------------------------------------------------------------
  Purpose:     Fyller combo-box for år
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR wAar-i   AS INTE NO-UNDO.
  DEF VAR wAar-j   AS INTE NO-UNDO.
  DEF VAR wAarList AS CHAR NO-UNDO.
  ASSIGN 
     wAar-j = wAar - 19
     COMBO-BOX-AAR:LIST-ITEMS IN FRAME {&FRAME-NAME} = "".
  
  DO wAar-i = 1 TO 40:
     ASSIGN
       wAarList = (wAarList + STRING(wAar-j)) + IF wAar-i < 40 THEN "," ELSE "".
       wAar-j = wAar-j + 1.      
  END.

  COMBO-BOX-AAR:LIST-ITEMS IN FRAME {&FRAME-NAME} = wAarList.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE FillDag F-Main 
PROCEDURE FillDag :
/*------------------------------------------------------------------------------
  Purpose: Fyll inn dagnummer i variabel wDag (ext 35).     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEF VAR wUke# AS INTE NO-UNDO.
   DEF VAR wExt# AS INTE NO-UNDO.
   DEF VAR wDag# AS INTE NO-UNDO.
   
   ASSIGN 
      wDag  = 0  /* Alle 37 extentene */
      wU-5  = 0  /* Trenger ikke å bli utfylt */
      wU-6  = 0  /* Trenger ikke å bli utfylt */ 
      wNyExt = 0.   
   FyllExtenter:
   DO wExt# = IF WEEKDAY(DATE(wMnd,1,wAar)) = 1 THEN 7 ELSE WEEKDAY(DATE(wMnd,1,wAar)) - 1 
          TO DAY(DATE(MONTH(DATE(wMnd,20,wAar) + 15),1,YEAR(DATE(wMnd,20,wAar) + 15)) - 1) + 
             (IF WEEKDAY(DATE(wMnd,1,wAar)) = 1 THEN 7 ELSE WEEKDAY(DATE(wMnd,1,wAar)) - 1) - 1:
      ASSIGN 
         wDag#       = wDag# + 1
         wDag[wExt#] = wDag#.
      IF wDag# = wCurrDag THEN ASSIGN wNyExt = wExt# wCurrExt = 0.
      IF wDag# = 1 OR CAN-DO("29,36",STRING(wExt#)) THEN 
      BestemUke: DO:
         RUN FinnUke(DATE(wMnd,IF wDag# = 1 THEN 1 ELSE wDag[wExt#],wAar),OUTPUT wUke#).  
         IF wDag# = 1 AND (wUke# = 1 OR wUke# = 53) AND wExt# <> 1
         THEN RUN FinnUke(DATE(12,31,wAar - 1),OUTPUT wUke#).  
          
         IF wDag# = 1  THEN ASSIGN wU-1 = wUke#. ELSE
         IF wExt# = 29 THEN ASSIGN wU-5 = wUke#. ELSE
         IF wExt# = 36 THEN ASSIGN wU-6 = wUke#.
      END. /* BestemUke */   
   END. /* FyllExtenter */
   ASSIGN 
      wU-2 = (IF wU-1 < 52 THEN wU-1 ELSE 0) + 1
      wU-3 = wU-2 + 1
      wU-4 = wU-3 + 1.
   IF wNyExt = 0 THEN
   ASSIGN 
      wCurrDag = DAY(DATE(MONTH(DATE(wMnd,20,wAar) + 15),1,YEAR(DATE(wMnd,20,wAar) + 15)) - 1)
      wNyExt   =  wExt# - 1
      wCurrExt = 0.
           
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE FinnHellig F-Main 
PROCEDURE FinnHellig :
/*------------------------------------------------------------------------------
  Purpose:     Finner årets helgedager. 
               Påskedag ut fra formel av C. Fr. Gauss.
  Parameters:  Outp hdag kommasep sting med helligdager.
               Outp hmnd kommasep string med korresponderende måneder.
  Notes:       
------------------------------------------------------------------------------*/
 
   DEF OUTPUT PARAMETER wHDag AS CHAR NO-UNDO. /* Årests helgedager   */
   DEF OUTPUT PARAMETER wHMnd AS CHAR NO-UNDO. /* Korrsponderende mnd */ 

   DEF VAR wPdag AS INTE NO-UNDO. /* Påskedag */
   DEF VAR wPmnd AS INTE NO-UNDO. /* Måneden til påskedagen */

   DEF VAR a AS INTE NO-UNDO.
   DEF VAR b AS INTE NO-UNDO.
   DEF VAR c AS INTE NO-UNDO.    
   DEF VAR d AS INTE NO-UNDO.
   DEF VAR e AS INTE NO-UNDO.

   DEF VAR x AS INTE init 24  NO-UNDO.
   DEF VAR y AS INTE init 5   NO-UNDO.

   /* Faste helgedager */
   ASSIGN 
     wHDag = "1,1,17,25,26"
     wHMnd = "1,5,5,12,12".

   IF wAar >= 1700 AND wAar <= 1799 THEN ASSIGN x = 23 y = 3. ELSE
   IF wAar >= 1800 AND wAar <= 1899 THEN ASSIGN x = 23 y = 4. ELSE
   IF wAar >= 1900 AND wAar <= 2099 THEN ASSIGN x = 24 y = 5. ELSE
   IF wAar >= 2100 AND wAar <= 2199 THEN ASSIGN x = 24 y = 6. ELSE
   IF wAar >= 2200 AND wAar <= 2299 THEN ASSIGN x = 25 y = 0. 
   ELSE DO:
      ASSIGN 
         wHDag = wHDag + ",,,,,,,,"
         wHMnd = wHMnd + ",,,,,,,,".
      RETURN.
   END.   

   ASSIGN 
      a = wAar MOD 19
      b = wAar MOD 4
      c = wAar MOD 7
      d = (19 * a + x) MOD 30
      e = (2 * b + 4 * c + 6 * d + y) MOD 7
      wPdag = d + e + 22
      wPmnd = 3. 

   IF wPdag > 31 THEN 
      ASSIGN wPdag = (d + e - 9) wPmnd = 4.
   
   /* Unntagelse ihht. Gauss' formel */   
   IF wPmnd = 4 THEN DO:   
      /* Unntak 1? */
      IF wPdag = 26 THEN 
         ASSIGN wPdag = 19. 
      ELSE       
      /* Unntak 2? */
      IF wPdag = 25 AND (a > 10 AND d = 28 AND e = 6) THEN
         ASSIGN wPdag = 18.
   END.      

   ASSIGN 
     wHDag = wHDag 
      + "," + STRING(DAY(DATE(wPmnd,wPdag,wAar) - 7))     /* Palmesøndag    */
      + "," + STRING(DAY(DATE(wPmnd,wPdag,wAar) - 3))     /* Skjærtorsdag   */
      + "," + STRING(DAY(DATE(wPmnd,wPdag,wAar) - 2))     /* Langfredag     */
      + "," + STRING(wPdag)                               /* Påskedag       */
      + "," + STRING(DAY(DATE(wPmnd,wPdag,wAar) + 1))     /* 2. påskedag    */
      + "," + STRING(DAY(DATE(wPmnd,wPdag,wAar) + 39))    /* Kr. himmelfart */
      + "," + STRING(DAY(DATE(wPmnd,wPdag,wAar) + 49))    /* 1. pinsedag    */
      + "," + STRING(DAY(DATE(wPmnd,wPdag,wAar) + 50))    /* 2. pinsedag    */
     wHmnd = wHmnd
      + "," + STRING(MONTH(DATE(wPmnd,wPdag,wAar) - 7))   /* Palmesøndag    */
      + "," + STRING(MONTH(DATE(wPmnd,wPdag,wAar) - 3))   /* Skjærtorsdag   */
      + "," + STRING(MONTH(DATE(wPmnd,wPdag,wAar) - 2))   /* Langfredag     */
      + "," + STRING(wPmnd)                               /* Påskedag       */
      + "," + STRING(MONTH(DATE(wPmnd,wPdag,wAar) + 1))   /* 2. påskedag    */
      + "," + STRING(MONTH(DATE(wPmnd,wPdag,wAar) + 39))  /* Kr. himmelfart */
      + "," + STRING(MONTH(DATE(wPmnd,wPdag,wAar) + 49))  /* 1. pinsedag    */
      + "," + STRING(MONTH(DATE(wPmnd,wPdag,wAar) + 50)). /* 2. pinsedag    */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE FinnUke F-Main 
PROCEDURE FinnUke :
/*------------------------------------------------------------------------------
  Purpose:     Finner ukenummer for en gitt dato.
  Parameters:  Input dato, output ukenummer
  Notes:       
------------------------------------------------------------------------------*/
   DEF INPUT  PARAMETER wDato AS DATE NO-UNDO.
   DEF OUTPUT PARAMETER wUke  AS INTE NO-UNDO.
    
   ASSIGN 
      wUke = (((wDato - DATE(1,1,YEAR(wDato)) + 1) +
              ((WEEKDAY(DATE(1,1,YEAR(wDato))) + 5) MOD 7)) -
              ((WEEKDAY(wDato) + 5) MOD 7 + 1) - 1) / 7 + 1
      wUke = IF WEEKDAY(DATE(1,3,YEAR(wDato))) <= 3 THEN (IF wUke = 1 THEN 53 ELSE wUke - 1) ELSE wUke.
   
   IF wUke = 53 AND MONTH(wDato) = 12 AND 
      (IF WEEKDAY(DATE(1,1,YEAR(wDato) + 1)) = 1 THEN 7 ELSE WEEKDAY(DATE(1,1,YEAR(wDato) + 1)) - 1) < 5  
   THEN ASSIGN wUke = 1.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE FixPopupMeny F-Main 
PROCEDURE FixPopupMeny :
/*------------------------------------------------------------------------------
  Purpose:     Fikser popup-menyen slik at den inneholder årstallet.
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR wMaxLabelLen AS INTE NO-UNDO.
  DEF VAR wSpaceLen    AS INTE NO-UNDO.
  ASSIGN
    wMaxLabelLen = FONT-TABLE:GET-TEXT-WIDTH-PIXELS("Kr.himmefartsdag ") 
    wSpaceLen    = FONT-TABLE:GET-TEXT-WIDTH-PIXELS(" "). 
      
  DO WITH FRAME {&FRAME-NAME}:
     ASSIGN 
        MENU-ITEM m_Julen:LABEL   IN MENU POPUP-MENU-F-Main = "&Julen"  + FILL(" ",INT((wMaxLabelLen - FONT-TABLE:GET-TEXT-WIDTH-PIXELS("Julen" )) / wSpaceLen)) + STRING(wAar)
        MENU-ITEM m_Paasken:LABEL IN MENU POPUP-MENU-F-Main = "&Påsken" + FILL(" ",INT((wMaxLabelLen - FONT-TABLE:GET-TEXT-WIDTH-PIXELS("Påsken")) / wSpaceLen)) + STRING(wAar)
        MENU-ITEM m_KrHFart:LABEL IN MENU POPUP-MENU-F-Main = "&Kr.himmefartsdag " + STRING(wAar)
        MENU-ITEM m_Pinsen:LABEL  IN MENU POPUP-MENU-F-Main = "Pi&nsen" + FILL(" ",INT((wMaxLabelLen - FONT-TABLE:GET-TEXT-WIDTH-PIXELS("Pinsen")) / wSpaceLen)) + STRING(wAar).
  END.      
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ForceFlyttMark F-Main 
PROCEDURE ForceFlyttMark :
/*------------------------------------------------------------------------------
  Purpose:     Flytter avmerket dag (ved initering og ved valg av annen mnd/år).
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEF INPUT PARAMETER wNyExt# AS INTE NO-UNDO.
   DO WITH FRAME {&FRAME-NAME}:
      CASE wNyExt#:  
         WHEN 1  THEN DO: APPLY "MOUSE-SELECT-DOWN" TO wD-1.  wD-1:SELECTED  = TRUE. END. 
         WHEN 2  THEN DO: APPLY "MOUSE-SELECT-DOWN" TO wD-2.  wD-2:SELECTED  = TRUE. END. 
         WHEN 3  THEN DO: APPLY "MOUSE-SELECT-DOWN" TO wD-3.  wD-3:SELECTED  = TRUE. END. 
         WHEN 4  THEN DO: APPLY "MOUSE-SELECT-DOWN" TO wD-4.  wD-4:SELECTED  = TRUE. END. 
         WHEN 5  THEN DO: APPLY "MOUSE-SELECT-DOWN" TO wD-5.  wD-5:SELECTED  = TRUE. END. 
         WHEN 6  THEN DO: APPLY "MOUSE-SELECT-DOWN" TO wD-6.  wD-6:SELECTED  = TRUE. END. 
         WHEN 7  THEN DO: APPLY "MOUSE-SELECT-DOWN" TO wD-7.  wD-7:SELECTED  = TRUE. END. 
         WHEN 8  THEN DO: APPLY "MOUSE-SELECT-DOWN" TO wD-8.  wD-8:SELECTED  = TRUE. END. 
         WHEN 9  THEN DO: APPLY "MOUSE-SELECT-DOWN" TO wD-9.  wD-9:SELECTED  = TRUE. END. 
         WHEN 10 THEN DO: APPLY "MOUSE-SELECT-DOWN" TO wD-10. wD-10:SELECTED = TRUE. END. 
         WHEN 11 THEN DO: APPLY "MOUSE-SELECT-DOWN" TO wD-11. wD-11:SELECTED = TRUE. END. 
         WHEN 12 THEN DO: APPLY "MOUSE-SELECT-DOWN" TO wD-12. wD-12:SELECTED = TRUE. END. 
         WHEN 13 THEN DO: APPLY "MOUSE-SELECT-DOWN" TO wD-13. wD-13:SELECTED = TRUE. END. 
         WHEN 14 THEN DO: APPLY "MOUSE-SELECT-DOWN" TO wD-14. wD-14:SELECTED = TRUE. END. 
         WHEN 15 THEN DO: APPLY "MOUSE-SELECT-DOWN" TO wD-15. wD-15:SELECTED = TRUE. END. 
         WHEN 16 THEN DO: APPLY "MOUSE-SELECT-DOWN" TO wD-16. wD-16:SELECTED = TRUE. END. 
         WHEN 17 THEN DO: APPLY "MOUSE-SELECT-DOWN" TO wD-17. wD-17:SELECTED = TRUE. END. 
         WHEN 18 THEN DO: APPLY "MOUSE-SELECT-DOWN" TO wD-18. wD-18:SELECTED = TRUE. END. 
         WHEN 19 THEN DO: APPLY "MOUSE-SELECT-DOWN" TO wD-19. wD-19:SELECTED = TRUE. END. 
         WHEN 20 THEN DO: APPLY "MOUSE-SELECT-DOWN" TO wD-20. wD-20:SELECTED = TRUE. END. 
         WHEN 21 THEN DO: APPLY "MOUSE-SELECT-DOWN" TO wD-21. wD-21:SELECTED = TRUE. END. 
         WHEN 22 THEN DO: APPLY "MOUSE-SELECT-DOWN" TO wD-22. wD-22:SELECTED = TRUE. END. 
         WHEN 23 THEN DO: APPLY "MOUSE-SELECT-DOWN" TO wD-23. wD-23:SELECTED = TRUE. END. 
         WHEN 24 THEN DO: APPLY "MOUSE-SELECT-DOWN" TO wD-24. wD-24:SELECTED = TRUE. END. 
         WHEN 25 THEN DO: APPLY "MOUSE-SELECT-DOWN" TO wD-25. wD-25:SELECTED = TRUE. END. 
         WHEN 26 THEN DO: APPLY "MOUSE-SELECT-DOWN" TO wD-26. wD-26:SELECTED = TRUE. END. 
         WHEN 27 THEN DO: APPLY "MOUSE-SELECT-DOWN" TO wD-27. wD-27:SELECTED = TRUE. END. 
         WHEN 28 THEN DO: APPLY "MOUSE-SELECT-DOWN" TO wD-28. wD-28:SELECTED = TRUE. END. 
         WHEN 29 THEN DO: APPLY "MOUSE-SELECT-DOWN" TO wD-29. wD-29:SELECTED = TRUE. END. 
         WHEN 30 THEN DO: APPLY "MOUSE-SELECT-DOWN" TO wD-30. wD-30:SELECTED = TRUE. END. 
         WHEN 31 THEN DO: APPLY "MOUSE-SELECT-DOWN" TO wD-31. wD-31:SELECTED = TRUE. END. 
         WHEN 32 THEN DO: APPLY "MOUSE-SELECT-DOWN" TO wD-32. wD-32:SELECTED = TRUE. END. 
         WHEN 33 THEN DO: APPLY "MOUSE-SELECT-DOWN" TO wD-33. wD-33:SELECTED = TRUE. END. 
         WHEN 34 THEN DO: APPLY "MOUSE-SELECT-DOWN" TO wD-34. wD-34:SELECTED = TRUE. END. 
         WHEN 35 THEN DO: APPLY "MOUSE-SELECT-DOWN" TO wD-35. wD-35:SELECTED = TRUE. END. 
         WHEN 36 THEN DO: APPLY "MOUSE-SELECT-DOWN" TO wD-36. wD-36:SELECTED = TRUE. END. 
         WHEN 37 THEN DO: APPLY "MOUSE-SELECT-DOWN" TO wD-37. wD-37:SELECTED = TRUE. END. 
      END CASE.
      PROCESS EVENTS.
   END.   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitierOppstart F-Main 
PROCEDURE InitierOppstart :
/*------------------------------------------------------------------------------
  Purpose:     Initiering av defaultverier m.m.
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
  IF wio-Dato = ? THEN
       ASSIGN 
           wMnd     = MONTH(TODAY)
           wCurrDag = DAY(TODAY)
           wAar     = YEAR(TODAY).
  ELSE ASSIGN 
           wMnd     = MONTH(wio-Dato)
           wAar     = YEAR(wio-Dato) 
           wCurrDag = DAY(wio-Dato).
  
  RUN FillDag.
  RUN FillAarCombo.
  RUN FinnHellig(OUTPUT wHdag, OUTPUT wHmnd).
  RUN VisDag.
  RUN SettHellig.
  RUN ForceFlyttMark(wNyExt).
  RUN FixPopupMeny.
  
  DO WITH FRAME {&FRAME-NAME}:
     ASSIGN wDhdl[01] = wD-1:HANDLE
            wDhdl[02] = wD-2:HANDLE 
            wDhdl[03] = wD-3:HANDLE
            wDhdl[04] = wD-4:HANDLE
            wDhdl[05] = wD-5:HANDLE
            wDhdl[06] = wD-6:HANDLE
            wDhdl[07] = wD-7:HANDLE
            wDhdl[08] = wD-8:HANDLE
            wDhdl[09] = wD-9:HANDLE
            wDhdl[10] = wD-10:HANDLE
            wDhdl[11] = wD-11:HANDLE
            wDhdl[12] = wD-12:HANDLE
            wDhdl[13] = wD-13:HANDLE
            wDhdl[14] = wD-14:HANDLE
            wDhdl[15] = wD-15:HANDLE
            wDhdl[16] = wD-16:HANDLE
            wDhdl[17] = wD-17:HANDLE
            wDhdl[18] = wD-18:HANDLE
            wDhdl[19] = wD-19:HANDLE
            wDhdl[20] = wD-20:HANDLE
            wDhdl[21] = wD-21:HANDLE
            wDhdl[22] = wD-22:HANDLE
            wDhdl[23] = wD-23:HANDLE
            wDhdl[24] = wD-24:HANDLE
            wDhdl[25] = wD-25:HANDLE
            wDhdl[26] = wD-26:HANDLE
            wDhdl[27] = wD-27:HANDLE
            wDhdl[28] = wD-28:HANDLE
            wDhdl[29] = wD-29:HANDLE
            wDhdl[30] = wD-30:HANDLE
            wDhdl[31] = wD-31:HANDLE
            wDhdl[32] = wD-32:HANDLE
            wDhdl[33] = wD-33:HANDLE
            wDhdl[34] = wD-34:HANDLE
            wDhdl[35] = wD-35:HANDLE
            wDhdl[36] = wD-36:HANDLE
            wDhdl[37] = wD-37:HANDLE.
  END.      
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitNyMnd F-Main 
PROCEDURE InitNyMnd :
/*------------------------------------------------------------------------------
  Purpose:     Sørger for at ny måned blir satt og at combo-boxen 
               skjønner at verdien er endret.
  Parameters:  Inp årendret logi
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER wAarEndret AS LOGI NO-UNDO.

  RUN SettNyMnd.
  IF wAarEndret THEN
       APPLY "VALUE-CHANGED" TO COMBO-BOX-Aar IN FRAME {&FRAME-NAME}.
  RUN SettHellig.     
  PROCESS EVENTS.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE MarkDay F-Main 
PROCEDURE MarkDay :
/*------------------------------------------------------------------------------
  Purpose:   Highligth ny valgt dag og vis litt info om dagen.  
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEF VAR wi AS INTE NO-UNDO.
   
   ASSIGN wDagInfo = "".
   
   DO WITH FRAME {&FRAME-NAME}:
      IF SELF:TYPE = "TEXT"  AND SELF:SCREEN-VALUE <> "" THEN 
         ASSIGN 
            SELF:FGCOLOR = IF SELF:FGCOLOR = wS-FgColor THEN wS-FgColor ELSE wM-FgColor 
            SELF:BGCOLOR = wM-BgColor
            wCurrDag     = INT(SELF:SCREEN-VALUE).
      /* Sjekker om det er en høytidsdag (untatt 1. jan, 1. mai og 17. mai */
      IF CAN-DO(wHmnd,STRING(wMnd)) THEN 
      DO wi = 4 TO NUM-ENTRIES(wHmnd):
         IF STRING(wMnd) = ENTRY(wi,wHmnd) AND
            STRING(wCurrDag) = ENTRY(wi,wHDag) 
         THEN DO:
            CASE wi:
              WHEN 4  THEN ASSIGN wDagInfo = "Første juledag".
              WHEN 5  THEN ASSIGN wDagInfo = "Andre juledag".
              WHEN 6  THEN ASSIGN wDagInfo = "Palmesøndag".
              WHEN 7  THEN ASSIGN wDagInfo = "Skjærtorsdag".
              WHEN 8  THEN ASSIGN wDagInfo = "Langfredag".
              WHEN 9  THEN ASSIGN wDagInfo = "Påskedag".
              WHEN 10 THEN ASSIGN wDagInfo = "Andre påskedag".
              WHEN 11 THEN ASSIGN wDagInfo = "Kristi himmelfartsdag".
              WHEN 12 THEN ASSIGN wDagInfo = "Første pinsedag".
              WHEN 13 THEN ASSIGN wDagInfo = "Andre pinsedag".
            END CASE.
            LEAVE.
         END.   
      END.   
      
      IF wDagInfo = "" THEN
        ASSIGN      
            wDagInfo = ENTRY(WEEKDAY(DATE(wMnd,wCurrDag,wAar)),"søndag,mandag,tirsdag,onsdag,torsdag,fredag,lørdag") +
                          ", " + STRING(wCurrDag) + ". " + LC(COMBO-BOX-Mnd:SCREEN-VALUE) + " " + STRING(wAar).
      /* Sentrerer */
      ASSIGN
         wDagInfo = FILL(" ",INT((wDagInfo:WIDTH-PIXELS - FONT-TABLE:GET-TEXT-WIDTH-PIXELS (wDagInfo)) / 2 / FONT-TABLE:GET-TEXT-WIDTH-PIXELS (" "))) + wDagInfo.
              
      DISPL wDagInfo.
   END.   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SettHellig F-Main 
PROCEDURE SettHellig :
/*------------------------------------------------------------------------------
  Purpose:     Setter helligdager for aktuell måned.
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   
   DEF VAR wi AS INTE NO-UNDO. 

   IF CAN-DO(wHmnd,STRING(wMnd)) THEN 
   DO wi = 1 TO NUM-ENTRIES(wHmnd):
      IF STRING(wMnd) = ENTRY(wi,wHmnd) THEN 
         RUN SettHelligFarge(INT(ENTRY(wi,wHdag)) +
              (IF WEEKDAY(DATE(wMnd,1,wAar)) = 1 THEN 7 ELSE WEEKDAY(DATE(wMnd,1,wAar)) - 1) - 1).
   END.   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SettHelligFarge F-Main 
PROCEDURE SettHelligFarge :
/*------------------------------------------------------------------------------
  Purpose:     Fargelegger helgedager.
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEF INPUT PARAMETER wHelgedag AS INTE NO-UNDO.
   DO WITH FRAME {&FRAME-NAME}:
      CASE wHelgedag:
         WHEN 1  THEN ASSIGN wD-1:FGCOLOR  = wS-FgColor. 
         WHEN 2  THEN ASSIGN wD-2:FGCOLOR  = wS-FgColor. 
         WHEN 3  THEN ASSIGN wD-3:FGCOLOR  = wS-FgColor.
         WHEN 4  THEN ASSIGN wD-4:FGCOLOR  = wS-FgColor.
         WHEN 5  THEN ASSIGN wD-5:FGCOLOR  = wS-FgColor.
         WHEN 6  THEN ASSIGN wD-6:FGCOLOR  = wS-FgColor.
         WHEN 7  THEN ASSIGN wD-7:FGCOLOR  = wS-FgColor.
         WHEN 8  THEN ASSIGN wD-8:FGCOLOR  = wS-FgColor.
         WHEN 9  THEN ASSIGN wD-9:FGCOLOR  = wS-FgColor.
         WHEN 10 THEN ASSIGN wD-10:FGCOLOR = wS-FgColor.
         WHEN 11 THEN ASSIGN wD-11:FGCOLOR = wS-FgColor.
         WHEN 12 THEN ASSIGN wD-12:FGCOLOR = wS-FgColor.
         WHEN 13 THEN ASSIGN wD-13:FGCOLOR = wS-FgColor.
         WHEN 14 THEN ASSIGN wD-14:FGCOLOR = wS-FgColor.
         WHEN 15 THEN ASSIGN wD-15:FGCOLOR = wS-FgColor.
         WHEN 16 THEN ASSIGN wD-16:FGCOLOR = wS-FgColor.
         WHEN 17 THEN ASSIGN wD-17:FGCOLOR = wS-FgColor.
         WHEN 18 THEN ASSIGN wD-18:FGCOLOR = wS-FgColor.
         WHEN 19 THEN ASSIGN wD-19:FGCOLOR = wS-FgColor.
         WHEN 20 THEN ASSIGN wD-20:FGCOLOR = wS-FgColor.
         WHEN 21 THEN ASSIGN wD-21:FGCOLOR = wS-FgColor.
         WHEN 22 THEN ASSIGN wD-22:FGCOLOR = wS-FgColor.
         WHEN 23 THEN ASSIGN wD-23:FGCOLOR = wS-FgColor.
         WHEN 24 THEN ASSIGN wD-24:FGCOLOR = wS-FgColor.
         WHEN 25 THEN ASSIGN wD-25:FGCOLOR = wS-FgColor.
         WHEN 26 THEN ASSIGN wD-26:FGCOLOR = wS-FgColor.
         WHEN 27 THEN ASSIGN wD-27:FGCOLOR = wS-FgColor.
         WHEN 28 THEN ASSIGN wD-28:FGCOLOR = wS-FgColor.
         WHEN 29 THEN ASSIGN wD-29:FGCOLOR = wS-FgColor.
         WHEN 30 THEN ASSIGN wD-30:FGCOLOR = wS-FgColor.
         WHEN 31 THEN ASSIGN wD-31:FGCOLOR = wS-FgColor.
         WHEN 32 THEN ASSIGN wD-32:FGCOLOR = wS-FgColor.
         WHEN 33 THEN ASSIGN wD-33:FGCOLOR = wS-FgColor.
         WHEN 34 THEN ASSIGN wD-34:FGCOLOR = wS-FgColor.
         WHEN 35 THEN ASSIGN wD-35:FGCOLOR = wS-FgColor.
         WHEN 36 THEN ASSIGN wD-36:FGCOLOR = wS-FgColor.
         WHEN 37 THEN ASSIGN wD-37:FGCOLOR = wS-FgColor.
      END CASE.
     
   END.   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SettNyMnd F-Main 
PROCEDURE SettNyMnd :
/*------------------------------------------------------------------------------
  Purpose:     Initierer ny måned.
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   RUN UnMarkDay.
   RUN ClearFarge.
   RUN FillDag.
   RUN VisDag.
   IF wNyExt <> wCurrExt THEN DO:
      RUN ForceFlyttMark(wNyExt).
      RUN MarkDay.
   END.   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SettSondagFarge F-Main 
PROCEDURE SettSondagFarge :
/*------------------------------------------------------------------------------
  Purpose:     Setter valgt farge på alle søndager.  
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 DO WITH FRAME {&FRAME-NAME}:
    ASSIGN
      wD-7:FGCOLOR  = wS-FgColor
      wD-14:FGCOLOR = wS-FgColor
      wD-21:FGCOLOR = wS-FgColor
      wD-28:FGCOLOR = wS-FgColor
      wD-35:FGCOLOR = wS-FgColor.
   END.   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE UnMarkDay F-Main 
PROCEDURE UnMarkDay :
/*------------------------------------------------------------------------------
  Purpose:   Fjern highligth fra sist valgt dag  
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   &SCOP NyFarge :FGCOLOR = wS-FgColor THEN wS-FgColor ELSE ?
   DO WITH FRAME {&FRAME-NAME}:
      /* Bruker dummy-knappen til å fjerne fokus */
      APPLY "MOUSE-SELECT-DOWN" TO BUTTON-DUMMY.
      PROCESS EVENTS.
      CASE wCurrExt:
         WHEN 1  THEN ASSIGN wD-1:FGCOLOR = IF wD-1{&NyFarge} wD-1:BGCOLOR  = ?. 
         WHEN 2  THEN ASSIGN wD-2:FGCOLOR = IF wD-2{&NyFarge} wD-2:BGCOLOR  = ?. 
         WHEN 3  THEN ASSIGN wD-3:FGCOLOR = IF wD-3{&NyFarge} wD-3:BGCOLOR  = ?. 
         WHEN 4  THEN ASSIGN wD-4:FGCOLOR = IF wD-4{&NyFarge} wD-4:BGCOLOR  = ?. 
         WHEN 5  THEN ASSIGN wD-5:FGCOLOR = IF wD-5{&NyFarge} wD-5:BGCOLOR  = ?. 
         WHEN 6  THEN ASSIGN wD-6:FGCOLOR = IF wD-6{&NyFarge} wD-6:BGCOLOR  = ?. 
         WHEN 7  THEN ASSIGN wD-7:FGCOLOR = wS-FgColor wD-7:BGCOLOR  = ?. 
         WHEN 8  THEN ASSIGN wD-8:FGCOLOR = IF wD-8{&NyFarge} wD-8:BGCOLOR  = ?. 
         WHEN 9  THEN ASSIGN wD-9:FGCOLOR = IF wD-9{&NyFarge} wD-9:BGCOLOR  = ?. 
         WHEN 10 THEN ASSIGN wD-10:FGCOLOR = IF wD-10{&NyFarge} wD-10:BGCOLOR = ?. 
         WHEN 11 THEN ASSIGN wD-11:FGCOLOR = IF wD-11{&NyFarge} wD-11:BGCOLOR = ?. 
         WHEN 12 THEN ASSIGN wD-12:FGCOLOR = IF wD-12{&NyFarge} wD-12:BGCOLOR = ?. 
         WHEN 13 THEN ASSIGN wD-13:FGCOLOR = IF wD-13{&NyFarge} wD-13:BGCOLOR = ?. 
         WHEN 14 THEN ASSIGN wD-14:FGCOLOR  = wS-FgColor wD-14:BGCOLOR = ?. 
         WHEN 15 THEN ASSIGN wD-15:FGCOLOR = IF wD-15{&NyFarge} wD-15:BGCOLOR = ?. 
         WHEN 16 THEN ASSIGN wD-16:FGCOLOR = IF wD-16{&NyFarge} wD-16:BGCOLOR = ?. 
         WHEN 17 THEN ASSIGN wD-17:FGCOLOR = IF wD-17{&NyFarge} wD-17:BGCOLOR = ?. 
         WHEN 18 THEN ASSIGN wD-18:FGCOLOR = IF wD-18{&NyFarge} wD-18:BGCOLOR = ?. 
         WHEN 19 THEN ASSIGN wD-19:FGCOLOR = IF wD-19{&NyFarge} wD-19:BGCOLOR = ?. 
         WHEN 20 THEN ASSIGN wD-20:FGCOLOR = IF wD-20{&NyFarge} wD-20:BGCOLOR = ?. 
         WHEN 21 THEN ASSIGN wD-21:FGCOLOR  = wS-FgColor wD-21:BGCOLOR = ?. 
         WHEN 22 THEN ASSIGN wD-22:FGCOLOR = IF wD-22{&NyFarge} wD-22:BGCOLOR = ?. 
         WHEN 23 THEN ASSIGN wD-23:FGCOLOR = IF wD-23{&NyFarge} wD-23:BGCOLOR = ?. 
         WHEN 24 THEN ASSIGN wD-24:FGCOLOR = IF wD-24{&NyFarge} wD-24:BGCOLOR = ?. 
         WHEN 25 THEN ASSIGN wD-25:FGCOLOR = IF wD-25{&NyFarge} wD-25:BGCOLOR = ?. 
         WHEN 26 THEN ASSIGN wD-26:FGCOLOR = IF wD-26{&NyFarge} wD-26:BGCOLOR = ?. 
         WHEN 27 THEN ASSIGN wD-27:FGCOLOR = IF wD-27{&NyFarge} wD-27:BGCOLOR = ?. 
         WHEN 28 THEN ASSIGN wD-28:FGCOLOR = wS-FgColor wD-28:BGCOLOR = ?. 
         WHEN 29 THEN ASSIGN wD-29:FGCOLOR = IF wD-29{&NyFarge} wD-29:BGCOLOR = ?. 
         WHEN 30 THEN ASSIGN wD-30:FGCOLOR = IF wD-30{&NyFarge} wD-30:BGCOLOR = ?. 
         WHEN 31 THEN ASSIGN wD-31:FGCOLOR = IF wD-31{&NyFarge} wD-31:BGCOLOR = ?. 
         WHEN 32 THEN ASSIGN wD-32:FGCOLOR = IF wD-32{&NyFarge} wD-32:BGCOLOR = ?. 
         WHEN 33 THEN ASSIGN wD-33:FGCOLOR = IF wD-33{&NyFarge} wD-33:BGCOLOR = ?. 
         WHEN 34 THEN ASSIGN wD-34:FGCOLOR = IF wD-34{&NyFarge} wD-34:BGCOLOR = ?. 
         WHEN 35 THEN ASSIGN wD-35:FGCOLOR = wS-FgColor wD-35:BGCOLOR = ?. 
         WHEN 36 THEN ASSIGN wD-36:FGCOLOR = IF wD-36{&NyFarge} wD-36:BGCOLOR = ?. 
         WHEN 37 THEN ASSIGN wD-37:FGCOLOR = IF wD-37{&NyFarge} wD-37:BGCOLOR = ?. 
      END CASE.
     
   END.   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE VisDag F-Main 
PROCEDURE VisDag :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  ASSIGN 
     COMBO-BOX-Mnd:SCREEN-VALUE IN FRAME {&FRAME-NAME} = COMBO-BOX-MND:ENTRY(wMnd) IN FRAME {&FRAME-NAME}.

  IF COMBO-BOX-Aar:SCREEN-VALUE IN FRAME {&FRAME-NAME} <> STRING(wAar) THEN
     ASSIGN COMBO-BOX-Aar:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(wAar). 
  DISPLAY 
    wU-1 
    wU-2
    wU-3 
    wU-4
    wU-5 
    wU-6 
    wDag[1]  @ wD-1  wDag[2]  @ wD-2  wDag[3]  @ wD-3  wDag[4]  @ wD-4  wDag[5]  @ wD-5  wDag[6]  @ wD-6  wDag[7]  @ wD-7 
    wDag[8]  @ wD-8  wDag[9]  @ wD-9  wDag[10] @ wD-10 wDag[11] @ wD-11 wDag[12] @ wD-12 wDag[13] @ wD-13 wDag[14] @ wD-14 
    wDag[15] @ wD-15 wDag[16] @ wD-16 wDag[17] @ wD-17 wDag[18] @ wD-18 wDag[19] @ wD-19 wDag[20] @ wD-20 wDag[21] @ wD-21 
    wDag[22] @ wD-22 wDag[23] @ wD-23 wDag[24] @ wD-24 wDag[25] @ wD-25 wDag[26] @ wD-26 wDag[27] @ wD-27 wDag[28] @ wD-28 
    wDag[29] @ wD-29 wDag[30] @ wD-30 wDag[31] @ wD-31 wDag[32] @ wD-32 wDag[33] @ wD-33 wDag[34] @ wD-34 wDag[35] @ wD-35 
    wDag[36] @ wD-36 wDag[37] @ wD-37 
  /* IN WINDOW CURRENT-WINDOW */
  WITH FRAME {&FRAME-NAME}.       
  
  ASSIGN    
      wD-1:SELECTABLE IN FRAME {&FRAME-NAME} = wD-1:SCREEN-VALUE IN FRAME {&FRAME-NAME} <> "" 
      wD-2:SELECTABLE IN FRAME {&FRAME-NAME} = wD-2:SCREEN-VALUE IN FRAME {&FRAME-NAME} <> "" 
      wD-3:SELECTABLE IN FRAME {&FRAME-NAME} = wD-3:SCREEN-VALUE IN FRAME {&FRAME-NAME} <> "" 
      wD-4:SELECTABLE IN FRAME {&FRAME-NAME} = wD-4:SCREEN-VALUE IN FRAME {&FRAME-NAME} <> "" 
      wD-5:SELECTABLE IN FRAME {&FRAME-NAME} = wD-5:SCREEN-VALUE IN FRAME {&FRAME-NAME} <> "" 
      wD-6:SELECTABLE IN FRAME {&FRAME-NAME} = wD-6:SCREEN-VALUE IN FRAME {&FRAME-NAME} <> "" 
      wD-29:SELECTABLE IN FRAME {&FRAME-NAME} = wD-29:SCREEN-VALUE IN FRAME {&FRAME-NAME} <> "" 
      wD-30:SELECTABLE IN FRAME {&FRAME-NAME} = wD-30:SCREEN-VALUE IN FRAME {&FRAME-NAME} <> "" 
      wD-31:SELECTABLE IN FRAME {&FRAME-NAME} = wD-31:SCREEN-VALUE IN FRAME {&FRAME-NAME} <> "" 
      wD-32:SELECTABLE IN FRAME {&FRAME-NAME} = wD-32:SCREEN-VALUE IN FRAME {&FRAME-NAME} <> "" 
      wD-33:SELECTABLE IN FRAME {&FRAME-NAME} = wD-33:SCREEN-VALUE IN FRAME {&FRAME-NAME} <> "" 
      wD-34:SELECTABLE IN FRAME {&FRAME-NAME} = wD-34:SCREEN-VALUE IN FRAME {&FRAME-NAME} <> "" 
      wD-35:SELECTABLE IN FRAME {&FRAME-NAME} = wD-35:SCREEN-VALUE IN FRAME {&FRAME-NAME} <> "" 
      wD-36:SELECTABLE IN FRAME {&FRAME-NAME} = wD-36:SCREEN-VALUE IN FRAME {&FRAME-NAME} <> "" 
      wD-37:SELECTABLE IN FRAME {&FRAME-NAME} = wD-37:SCREEN-VALUE IN FRAME {&FRAME-NAME} <> "". 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

