&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          skotex           PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame
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
/*          This .W file was created with the Progress AppBuilder.       */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */
DEFINE INPUT  PARAMETER rPakkeLinje      AS ROWID      NO-UNDO.
DEFINE INPUT  PARAMETER dArtikkelNr LIKE PakkeLinje.ArtikkelNr  NO-UNDO.
DEFINE INPUT  PARAMETER hParent          AS HANDLE     NO-UNDO.
/* Local Variable Definitions ---                                       */

DEFINE VARIABLE cHovedStrekKode AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cNoStrStrekKode AS CHARACTER  NO-UNDO.
DEFINE VARIABLE hSokKnapp       AS HANDLE     NO-UNDO.
DEFINE BUFFER bArtBas FOR ArtBas.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES PakkeLinje

/* Definitions for DIALOG-BOX Dialog-Frame                              */
&Scoped-define FIELDS-IN-QUERY-Dialog-Frame PakkeLinje.ArtikkelNr ~
PakkeLinje.PkArtikkelNr PakkeLinje.Antall 
&Scoped-define ENABLED-FIELDS-IN-QUERY-Dialog-Frame PakkeLinje.Antall 
&Scoped-define ENABLED-TABLES-IN-QUERY-Dialog-Frame PakkeLinje
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-Dialog-Frame PakkeLinje
&Scoped-define QUERY-STRING-Dialog-Frame FOR EACH PakkeLinje SHARE-LOCK
&Scoped-define OPEN-QUERY-Dialog-Frame OPEN QUERY Dialog-Frame FOR EACH PakkeLinje SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-Dialog-Frame PakkeLinje
&Scoped-define FIRST-TABLE-IN-QUERY-Dialog-Frame PakkeLinje


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS PakkeLinje.Antall 
&Scoped-define ENABLED-TABLES PakkeLinje
&Scoped-define FIRST-ENABLED-TABLE PakkeLinje
&Scoped-Define ENABLED-OBJECTS BUTTON-SokVg Btn_OK B-LagreNy Btn_Cancel ~
BUTTON-Strekkode 
&Scoped-Define DISPLAYED-FIELDS PakkeLinje.ArtikkelNr ~
PakkeLinje.PkArtikkelNr PakkeLinje.Antall 
&Scoped-define DISPLAYED-TABLES PakkeLinje
&Scoped-define FIRST-DISPLAYED-TABLE PakkeLinje
&Scoped-Define DISPLAYED-OBJECTS FI-PakkeNavn FI-PkArtikkelNavn CB-StrKode 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD AlleredeReg Dialog-Frame 
FUNCTION AlleredeReg RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON B-LagreNy 
     LABEL "Lagre/Ny" 
     SIZE 15 BY 1.14.

DEFINE BUTTON Btn_Cancel AUTO-END-KEY 
     LABEL "Avbryt" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON Btn_OK AUTO-GO 
     LABEL "Lagre" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON BUTTON-SokVg  NO-FOCUS
     LABEL "&Søk artikkel" 
     SIZE 15 BY 1.14 TOOLTIP "Søk v h a vg-løpnr / artikkelnr".

DEFINE BUTTON BUTTON-Strekkode 
     IMAGE-UP FILE "icon\e-sokpr":U NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1 TOOLTIP "Søk artikkel v h a strekkode".

DEFINE VARIABLE CB-StrKode AS CHARACTER FORMAT "X(256)":U 
     LABEL "Størrelse" 
     VIEW-AS COMBO-BOX INNER-LINES 25
     DROP-DOWN-LIST
     SIZE 22 BY 1 NO-UNDO.

DEFINE VARIABLE FI-PakkeNavn AS CHARACTER FORMAT "X(256)":U 
     LABEL "Navn" 
     VIEW-AS FILL-IN 
     SIZE 32 BY 1 NO-UNDO.

DEFINE VARIABLE FI-PkArtikkelNavn AS CHARACTER FORMAT "X(256)":U 
     LABEL "Navn" 
     VIEW-AS FILL-IN 
     SIZE 32 BY 1 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Dialog-Frame FOR 
      PakkeLinje SCROLLING.
&ANALYZE-RESUME

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     BUTTON-SokVg AT ROW 1.43 COL 52
     PakkeLinje.ArtikkelNr AT ROW 1.52 COL 12.6 COLON-ALIGNED
          LABEL "Pakke"
          VIEW-AS FILL-IN 
          SIZE 20.2 BY 1
     FI-PakkeNavn AT ROW 2.52 COL 12.6 COLON-ALIGNED
     Btn_OK AT ROW 3.33 COL 52
     PakkeLinje.PkArtikkelNr AT ROW 3.52 COL 12.6 COLON-ALIGNED
          LABEL "Artikkelnr"
          VIEW-AS FILL-IN 
          SIZE 20.2 BY 1
     FI-PkArtikkelNavn AT ROW 4.52 COL 12.6 COLON-ALIGNED
     B-LagreNy AT ROW 4.52 COL 52
     CB-StrKode AT ROW 5.52 COL 12.6 COLON-ALIGNED
     PakkeLinje.Antall AT ROW 6.52 COL 12.6 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11.8 BY 1
     Btn_Cancel AT ROW 6.57 COL 52
     BUTTON-Strekkode AT ROW 5.52 COL 36.8
     SPACE(26.79) SKIP(1.28)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Registrer pakkelinje"
         DEFAULT-BUTTON Btn_OK CANCEL-BUTTON Btn_Cancel.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Dialog-Box
   Allow: Basic,Browse,DB-Fields,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX Dialog-Frame
   FRAME-NAME                                                           */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN PakkeLinje.ArtikkelNr IN FRAME Dialog-Frame
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR COMBO-BOX CB-StrKode IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-PakkeNavn IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-PkArtikkelNavn IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN PakkeLinje.PkArtikkelNr IN FRAME Dialog-Frame
   NO-ENABLE EXP-LABEL                                                  */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX Dialog-Frame
/* Query rebuild information for DIALOG-BOX Dialog-Frame
     _TblList          = "skotex.PakkeLinje"
     _Options          = "SHARE-LOCK"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX Dialog-Frame */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Registrer pakkelinje */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-LagreNy
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-LagreNy Dialog-Frame
ON CHOOSE OF B-LagreNy IN FRAME Dialog-Frame /* Lagre/Ny */
DO:
    IF int(CB-StrKode:SCREEN-VALUE) = 0  OR 
        int(CB-StrKode:SCREEN-VALUE) = ? THEN
    DO:
        MESSAGE "Størrelse må angis før lagring."
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        RETURN NO-APPLY.
    END.
    IF AlleredeReg() THEN DO:
        MESSAGE "Artikkel/størrelse er allerede registrert som pakkemedlem." SKIP
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        RETURN NO-APPLY.
    END.

    RUN LagrePakkeLinje.
    IF RETURN-VALUE = "AVBRYT" THEN
        RETURN NO-APPLY.
    RUN OpenQueryPakkelinje IN hParent.
    DO WITH FRAME {&FRAME-NAME}:
      ASSIGN PakkeLinje.PkArtikkelNr:SCREEN-VALUE = "0"
             CB-StrKode:SENSITIVE = FALSE
             FI-PkArtikkelNavn:SCREEN-VALUE = ""
             PakkeLinje.Antall:SCREEN-VALUE = "1"
             Btn_OK:SENSITIVE    = FALSE
             B-LagreNy:SENSITIVE = FALSE.
    END.
    APPLY "CHOOSE" TO hSokKnapp.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK Dialog-Frame
ON CHOOSE OF Btn_OK IN FRAME Dialog-Frame /* Lagre */
DO:
    IF int(CB-StrKode:SCREEN-VALUE) = 0  OR 
        int(CB-StrKode:SCREEN-VALUE) = ? THEN
    DO:
        MESSAGE "Størrelse må angis før lagring."
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        RETURN NO-APPLY.
    END.
    IF AlleredeReg() THEN DO:
        MESSAGE "Artikkel/størrelse er allerede registrert som pakkemedlem." SKIP
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        RETURN NO-APPLY.
    END.
    RUN LagrePakkeLinje.
    IF RETURN-VALUE = "AVBRYT" THEN
        RETURN NO-APPLY.
    RUN OpenQueryPakkelinje IN hParent.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-SokVg
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-SokVg Dialog-Frame
ON CHOOSE OF BUTTON-SokVg IN FRAME Dialog-Frame /* Søk artikkel */
DO:
  DEFINE VARIABLE lArtikkelNr AS DEC  NO-UNDO.

  ASSIGN hSokKnapp = SELF.
  run d-hsok.w (output lArtikkelNr,"").

  IF lArtikkelNr <> ? THEN DO:
      FIND ArtBas WHERE 
          ArtBas.ArtikkelNr = lArtikkelNr NO-LOCK NO-ERROR.

      IF AVAILABLE ArtBas AND ArtBas.Pakke = FALSE THEN DO:
          IF ArtBas.OPris THEN DO:
              MESSAGE "PLU/Åpen pris artikler kan ikke legges inn på pakke."
                  VIEW-AS ALERT-BOX INFO BUTTONS OK.
          END.
          ELSE IF Artbas.Sanertdato <> ? THEN DO:
              MESSAGE "Artikkelen er sanert. Kan ikke legges inn i en pakke."
                  VIEW-AS ALERT-BOX INFO BUTTONS OK.
          END.
          ELSE IF NOT CAN-FIND(FIRST StrekKode OF ArtBas) THEN DO:
              MESSAGE "Artikkel mangler strekkode/størrelser"
                  VIEW-AS ALERT-BOX INFO BUTTONS OK.
              RELEASE ArtBas.
          END.
          ELSE DO:
              RUN PopulateCB-Kode.
              RUN DispPkArtikkel("").
              APPLY "ENTRY" TO PakkeLinje.Antall.
          END.
      END.
      ELSE DO:
          MESSAGE "Artikkelen er lagt opp som pakke og kan ikke legges inn som pakkelinje."
              VIEW-AS ALERT-BOX INFO BUTTONS OK.
      END.
  END.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Strekkode
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Strekkode Dialog-Frame
ON CHOOSE OF BUTTON-Strekkode IN FRAME Dialog-Frame /* ... */
DO:
  DEFINE VARIABLE cStrekkode   LIKE Strekkode.Kode NO-UNDO.
  DEFINE VARIABLE iArtBasRecid AS RECID            NO-UNDO.
  DO WITH FRAME {&FRAME-NAME}:
     ASSIGN hSokKnapp = SELF.
    run d-streksok.w (INPUT dArtikkelNr,OUTPUT iArtBasRecid,OUTPUT cStrekkode).
    IF iArtBasRecid <> ? THEN DO:
        FIND ArtBas WHERE RECID(ArtBas) = iArtBasRecid NO-LOCK.
        IF ArtBas.Pakke = FALSE THEN DO:
            IF AlleredeReg() THEN DO:
                MESSAGE "Allerede registrert"
                    VIEW-AS ALERT-BOX INFO BUTTONS OK.
            END.
            ELSE DO:
                RUN PopulateCB-Kode.
                RUN DispPkArtikkel(cStrekkode).
            END.
        END.
        ELSE DO:
            MESSAGE "Pakkeartikkel kann ikke brukes."
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
        END.
    END.
  END.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dialog-Frame 


/* ***************************  Main Block  *************************** */

/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT eq ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.

on F2,ALT-S anywhere 
DO:
    APPLY "CHOOSE" TO BUTTON-SokVg IN FRAME {&FRAME-NAME}.
END.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
    FIND bArtBas WHERE bArtBas.ArtikkelNr = dArtikkelNr NO-LOCK NO-ERROR.
/*   IF NOT dArtikkelNr = ? THEN DO:                                         */
/*       FIND ArtBas WHERE ArtBas.ArtikkelNr = dArtikkelNr NO-LOCK NO-ERROR. */
/*       IF NOT AVAIL ArtBas THEN DO:                                        */
/*           MESSAGE "Fant ikke artikkel."                                   */
/*               VIEW-AS ALERT-BOX INFO BUTTONS OK.                          */
/*           RETURN NO-APPLY.                                                */
/*       END.                                                                */
/*   END.                                                                    */
  IF rPakkeLinje <> ? THEN DO:
      FIND PakkeLinje WHERE ROWID(PakkeLinje) = rPakkeLinje NO-LOCK.
  END.
  {lng.i}
  RUN enable_UI.
  RUN FixView.
  IF BUTTON-SokVg:SENSITIVE THEN
      APPLY "CHOOSE" TO BUTTON-SokVg.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DispPkArtikkel Dialog-Frame 
PROCEDURE DispPkArtikkel :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER cStrekKode AS CHARACTER  NO-UNDO.
  DO WITH FRAME {&FRAME-NAME}:

/*       IF NOT AVAIL PakkeLinje THEN DO:                                 */
/*           IF cHovedStrekKode <> "" THEN                                */
/*               ASSIGN cStrekKode = cHovedStrekKode.                     */
/*           ELSE IF cHovedStrekKode = "" AND cNoStrStrekKode <> "" THEN  */
/*               ASSIGN cStrekKode = cNoStrStrekKode.                     */
/*           ELSE IF cStrekKode = "" THEN ENTRY(1,CB-StrKode:LIST-ITEMS). */
/*       END.                                                             */

      IF AVAILABLE PakkeLinje THEN
          FIND FIRST StrKonv NO-LOCK WHERE
              StrKonv.StrKode = PakkeLinje.StrKode NO-ERROR.

      ASSIGN PakkeLinje.PkArtikkelNr:SCREEN-VALUE = STRING(ArtBas.ArtikkelNr)
             FI-PkArtikkelNavn:SCREEN-VALUE       = IF ArtBas.Beskr <> "" 
                                                      THEN ArtBas.Beskr 
                                                      ELSE ArtBas.BongTekst
             PakkeLinje.Antall:SCREEN-VALUE       = IF AVAIL PakkeLinje 
                                                      THEN STRING(PakkeLinje.Antall) 
                                                      ELSE "1"
             CB-StrKode:SCREEN-VALUE = IF (AVAIL PakkeLinje AND AVAIL StrKonv) 
                                         THEN string(StrKonv.StrKode)
                                         ELSE entry(2,CB-StrKode:LIST-ITEM-PAIRS)
             CB-StrKode:SENSITIVE = TRUE
             Btn_OK:SENSITIVE  = TRUE
             B-LagreNy:SENSITIVE = NOT AVAIL PakkeLinje.
             .
  END.
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
  DISPLAY FI-PakkeNavn FI-PkArtikkelNavn CB-StrKode 
      WITH FRAME Dialog-Frame.
  IF AVAILABLE PakkeLinje THEN 
    DISPLAY PakkeLinje.ArtikkelNr PakkeLinje.PkArtikkelNr PakkeLinje.Antall 
      WITH FRAME Dialog-Frame.
  ENABLE BUTTON-SokVg Btn_OK B-LagreNy PakkeLinje.Antall Btn_Cancel 
         BUTTON-Strekkode 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE FixView Dialog-Frame 
PROCEDURE FixView :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN PakkeLinje.ArtikkelNr:SCREEN-VALUE = STRING(bArtBas.ArtikkelNr)
           FI-PakkeNavn:SCREEN-VALUE = bArtBas.Beskr.

    IF AVAIL PakkeLinje THEN DO:
        FIND ArtBas WHERE ArtBas.ArtikkelNr = PakkeLinje.PkArtikkelNr NO-LOCK.
        RUN PopulateCB-Kode.
        RUN DispPkArtikkel(PakkeLinje.StrKode).
        ASSIGN BUTTON-SokVg:SENSITIVE     = FALSE
               Button-Strekkode:SENSITIVE = FALSE
               B-LagreNy:SENSITIVE        = FALSE
               PakkeLinje.PkArtikkelNr:SENSITIVE = FALSE.
               .
    END.
    ELSE
        ASSIGN Btn_OK:SENSITIVE    = FALSE
               B-LagreNy:SENSITIVE = FALSE.
    RELEASE ArtBas.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LagrePakkeLinje Dialog-Frame 
PROCEDURE LagrePakkeLinje :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DO WITH FRAME {&FRAME-NAME}:
    IF NOT AVAIL PakkeLinje AND NOT AVAIL ArtBas THEN DO:
        MESSAGE "Søk artikkel"
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        RETURN "AVBRYT".
    END.
    ELSE IF INPUT PakkeLinje.Antall = 0 THEN DO:
        MESSAGE "Antall = 0, ikke tillatt."
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        RETURN "AVBRYT".
    END.
    ELSE IF AVAIL PakkeLinje THEN DO:
        FIND FIRST StrKonv NO-LOCK WHERE
            StrKonv.StrKode = PakkeLinje.StrKode NO-ERROR.
        IF PakkeLinje.StrKode <> int(CB-StrKode:SCREEN-VALUE) OR
           PakkeLinje.Antall <> INPUT PakkeLinje.Antal THEN DO:
            FIND CURRENT PakkeLinje EXCLUSIVE.
            ASSIGN PakkeLinje.StrKode = int(CB-StrKode:SCREEN-VALUE)
                   PakkeLinje.Antall = INPUT PakkeLinje.Antall.
            RELEASE PakkeLinje.
        END.
    END.
    ELSE DO:
        CREATE PakkeLinje.
        ASSIGN PakkeLinje.ArtikkelNr   = INPUT PakkeLinje.ArtikkelNr 
               PakkeLinje.PakkeNr      = bArtBas.PakkeNr 
               PakkeLinje.PkArtikkelNr = INPUT PakkeLinje.PkArtikkelNr 
               PakkeLinje.StrKode      = INT(CB-StrKode:SCREEN-VALUE IN FRAME {&FRAME-NAME})
               PakkeLinje.Antall       = INPUT PakkeLinje.Antall.
        RELEASE PakkeLinje.
        RELEASE ArtBas.
    END.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PopulateCB-Kode Dialog-Frame 
PROCEDURE PopulateCB-Kode :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cItems AS CHARACTER  NO-UNDO.

    DO WITH FRAME Dialog-Frame:
        ASSIGN cHovedStrekKode = ""
               cNoStrStrekKode = ""
               cItems = ",".


        IF DEC(Pakkelinje.PkArtikkelNr:SCREEN-VALUE) > 0 THEN
            FIND ArtBas NO-LOCK WHERE
                 ArtBas.ArtikkelNr = DEC(Pakkelinje.PkArtikkelNr:SCREEN-VALUE) NO-ERROR.
        IF AVAILABLE ArtBas THEN
        FOR EACH StrekKode OF ArtBas NO-LOCK
            BREAK BY Strekkode.ArtikkelNr
                  BY Strekkode.StrKode:
          IF first-of(Strekkode.StrKode) THEN
          DO:
              FIND FIRST StrKonv NO-LOCK WHERE
                  StrKonv.StrKode = Strekkode.StrKode NO-ERROR.
              ASSIGN 
                  cItems = cItems 
                           + (IF cItems = "" THEN "" ELSE ",") 
                           + ((IF AVAILABLE StrKonv
                                 THEN StrKonv.Storl
                                 ELSE '*') + "," + string(StrekKode.StrKode))
                           .
          END.
        END.
        ASSIGN 
            CB-StrKode:LIST-ITEM-PAIRS IN FRAME {&FRAME-NAME} = cItems
            CB-StrKode:SCREEN-VALUE = entry(2,CB-StrKode:LIST-ITEM-PAIRS).
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION AlleredeReg Dialog-Frame 
FUNCTION AlleredeReg RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DO WITH FRAME {&FRAME-NAME}:
    RETURN CAN-FIND(FIRST PakkeLinje WHERE 
                    PakkeLinje.ArtikkelNr   = INPUT PakkeLinje.ArtikkelNr AND 
                    PakkeLinje.PkArtikkelNr = ArtBas.ArtikkelNr AND
                    PakkeLinje.StrKode      = INT(CB-StrKode:SCREEN-VALUE IN FRAME {&FRAME-NAME})).   /* Function return value. */
  END.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

