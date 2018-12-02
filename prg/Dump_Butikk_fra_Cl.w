&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
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
DEFINE VARIABLE iBut AS INTEGER     NO-UNDO.
{runlib.i}


DEF STREAM Ut1.
DEF STREAM Ut2.
DEF STREAM Ut3.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS B-But FI-Butik FI-butnamn B-Dir FI-Katalog ~
B-Starta FI-ButikTxt 
&Scoped-Define DISPLAYED-OBJECTS FI-Butik FI-butnamn FI-Katalog FI-ButikTxt 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON B-But 
     IMAGE-UP FILE "icon/e-search.bmp":U
     LABEL "Button 3" 
     SIZE 4.6 BY 1.24.

DEFINE BUTTON B-Dir 
     IMAGE-UP FILE "icon/e-open.bmp":U
     LABEL "Button 2" 
     SIZE 4.6 BY 1.24.

DEFINE BUTTON B-Starta 
     LABEL "Start" 
     SIZE 15 BY 1.14.

DEFINE VARIABLE FI-Butik AS INTEGER FORMAT ">>>>9":U INITIAL 0 
     LABEL "Butikk" 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1 NO-UNDO.

DEFINE VARIABLE FI-ButikTxt AS CHARACTER FORMAT "X(256)":U INITIAL "Eksporter data for butikk" 
      VIEW-AS TEXT 
     SIZE 55 BY 1.1
     FONT 8 NO-UNDO.

DEFINE VARIABLE FI-butnamn AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 29 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Katalog AS CHARACTER FORMAT "X(256)":U 
     LABEL "Katalog" 
     VIEW-AS FILL-IN 
     SIZE 30 BY 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     B-But AT ROW 3.33 COL 20.6
     FI-Butik AT ROW 3.43 COL 11 COLON-ALIGNED
     FI-butnamn AT ROW 3.43 COL 23.8 COLON-ALIGNED NO-LABEL
     B-Dir AT ROW 4.91 COL 43.8
     FI-Katalog AT ROW 5.05 COL 11 COLON-ALIGNED
     B-Starta AT ROW 6.76 COL 13
     FI-ButikTxt AT ROW 1.81 COL 4 COLON-ALIGNED NO-LABEL
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 70.14 BY 12.27.


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
         TITLE              = "Eksport av data for en butikk fra et sentrallager"
         HEIGHT             = 12.29
         WIDTH              = 70.2
         MAX-HEIGHT         = 23.19
         MAX-WIDTH          = 118.8
         VIRTUAL-HEIGHT     = 23.19
         VIRTUAL-WIDTH      = 118.8
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
ASSIGN 
       FI-Butik:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

ASSIGN 
       FI-butnamn:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

ASSIGN 
       FI-Katalog:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Eksport av data for en butikk fra et sentrallager */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Eksport av data for en butikk fra et sentrallager */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-But
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-But C-Win
ON CHOOSE OF B-But IN FRAME DEFAULT-FRAME /* Button 3 */
DO:
  RUN d-bbutiker.w (INPUT-OUTPUT FI-Butik).
  ASSIGN FI-Butik:SCREEN-VALUE = STRING(FI-Butik).
  IF FI-Butik > 0 THEN DO:
      FIND butiker WHERE butiker.butik = FI-Butik NO-LOCK NO-ERROR.
      IF AVAIL butiker THEN
          ASSIGN FI-butnamn = butiker.butnamn
                 FI-butnamn:SCREEN-VALUE = butiker.butnamn.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Dir
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Dir C-Win
ON CHOOSE OF B-Dir IN FRAME DEFAULT-FRAME /* Button 2 */
DO:
    SYSTEM-DIALOG GET-DIR FI-Katalog INITIAL-DIR "c:\".
    IF FI-Katalog <> "" THEN
        ASSIGN FI-Katalog = FI-Katalog + "\"
               FI-Katalog:SCREEN-VALUE = FI-Katalog.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Starta
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Starta C-Win
ON CHOOSE OF B-Starta IN FRAME DEFAULT-FRAME /* Start */
DO:
    ASSIGN
        FI-butik
        FI-Katalog
        .

    IF INT(FI-Butik) = 0 THEN DO:
        MESSAGE "Välj butik"
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        RETURN NO-APPLY.
    END.
    IF FI-Katalog = "" THEN DO:
        MESSAGE "Välj katalog"
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        RETURN NO-APPLY.
    END.
  RUN DumpaSkotex.
  RUN DumpaData.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-Butik
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-Butik C-Win
ON ENTRY OF FI-Butik IN FRAME DEFAULT-FRAME /* Butikk */
DO:
  FI-Butnamn = "".
  FI-Butnamn:SCREEN-VALUE = "".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
   RUN disable_UI.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  {lng.i}
  RUN enable_UI.

  DO WITH FRAME DEFAULT-FRAME:
      ASSIGN
          FI-butik:SCREEN-VALUE   = '1'
          FI-Katalog:SCREEN-VALUE = 'c:\temp'
          .
  END.
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Artikelrelaterat C-Win 
PROCEDURE Artikelrelaterat :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/*    ArtBas                             */
/*    ArtPris                            */
/*    StrekKode                          */
/*    HPrisKo                            */
/*    PrisKo                             */
/*    Lager                        Butik */
/*    ArtLag                       butik */

    DEFINE INPUT  PARAMETER cTable AS CHARACTER   NO-UNDO.
    DEFINE INPUT  PARAMETER cButfield AS CHARACTER   NO-UNDO.

    DEF VAR iProfilNr AS INT NO-UNDO.
    DEF BUFFER b_File FOR SkoTex._File.

    FIND skotex._file WHERE skotex._file._file-name = cTable NO-LOCK.
    FIND Butiker NO-LOCK WHERE Butiker.Butik = INT(FI-Butik).

    iProfilNr = Butiker.ProfilNr.

    IF ("' + FI-Katalog + '\' + skotex._file._dump-name '.d") = ? THEN
    DO:
        MESSAGE 'Feil ved eksport av tabell ' cTable
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        RETURN.
    END.
    
    OUTPUT TO "wrk\dumpaArtikelrelaterat.p".
      PUT UNFORMATTED 'DEF STREAM Ut1.' SKIP.
      PUT UNFORMATTED 'DEF STREAM Ut2.' SKIP.

      PUT UNFORMATTED 'OUTPUT STREAM Ut1 TO value("' + FI-Katalog + '\' + skotex._file._dump-name '.d").' SKIP.


      PUT UNFORMATTED 'FOR EACH ' cTable ':' SKIP.
      PUT UNFORMATTED '    EXPORT STREAM Ut1 ' cTable '.' SKIP.
      
      FIND b_file WHERE b_file._file-name = 'ArtPris' NO-LOCK.
      PUT UNFORMATTED '      OUTPUT STREAM Ut2 TO value("' + FI-Katalog + '\' + b_file._dump-name '.d") APPEND.' SKIP.      
      PUT UNFORMATTED '        find first ArtPris of ArtBas where ArtPris.ProfilNr = ' + STRING(iProfilNr) + ' NO-LOCK NO-ERROR.' SKIP.
      PUT UNFORMATTED '        if not available ArtPris then find first ArtPris of ArtBas NO-LOCK NO-ERROR.' SKIP.                                
      PUT UNFORMATTED '        if available ArtPris then EXPORT STREAM Ut2 ArtPris.' SKIP.
      PUT UNFORMATTED '      OUTPUT STREAM Ut2 CLOSE.' SKIP.

      FIND b_file WHERE b_file._file-name = 'PrisKo' NO-LOCK.
      PUT UNFORMATTED '      OUTPUT STREAM Ut2 TO value("' + FI-Katalog + '\' + b_file._dump-name '.d") APPEND.' SKIP.      
      PUT UNFORMATTED '        For EACH PrisKo of ARTBAS NO-LOCK where PrisKo.ProfilNr = ' + STRING(iProfilNr) + ':' SKIP.
      PUT UNFORMATTED '            EXPORT STREAM Ut2 PrisKo.' SKIP.
      PUT UNFORMATTED '        END.' SKIP.
      PUT UNFORMATTED '      OUTPUT STREAM Ut2 CLOSE.' SKIP.
      
      FIND b_file WHERE b_file._file-name = 'HPrisKo' NO-LOCK.
      PUT UNFORMATTED '      OUTPUT STREAM Ut2 TO value("' + FI-Katalog + '\' + b_file._dump-name '.d") APPEND.' SKIP.      
      PUT UNFORMATTED '        For EACH HPrisKo of ARTBAS NO-LOCK where HPrisKo.ProfilNr = ' + STRING(iProfilNr) + ':' SKIP.
      PUT UNFORMATTED '            EXPORT STREAM Ut2 HPrisKo.' SKIP.
      PUT UNFORMATTED '        END.' SKIP.
      PUT UNFORMATTED '      OUTPUT STREAM Ut2 CLOSE.' SKIP.

      FIND b_file WHERE b_file._file-name = 'Strekkode' NO-LOCK.
      PUT UNFORMATTED '      OUTPUT STREAM Ut2 TO value("' + FI-Katalog + '\' + b_file._dump-name '.d") APPEND.' SKIP.      
      PUT UNFORMATTED '        For EACH Strekkode of ARTBAS NO-LOCK.' SKIP.
      PUT UNFORMATTED '            EXPORT STREAM Ut2 Strekkode.' SKIP.
      PUT UNFORMATTED '        END.' SKIP.
      PUT UNFORMATTED '      OUTPUT STREAM Ut2 CLOSE.' SKIP.

      FIND b_file WHERE b_file._file-name = 'Lager' NO-LOCK.
      PUT UNFORMATTED '      OUTPUT STREAM Ut2 TO value("' + FI-Katalog + '\' + b_file._dump-name '.d") APPEND.' SKIP.      
      PUT UNFORMATTED '        For EACH Lager of ArtBas where Lager.Butik = ' + STRING(FI-Butik) + ' NO-LOCK:' SKIP.
      PUT UNFORMATTED '            EXPORT STREAM Ut2 Lager.' SKIP.
      PUT UNFORMATTED '        END.' SKIP.
      PUT UNFORMATTED '      OUTPUT STREAM Ut2 CLOSE.' SKIP.

      FIND b_file WHERE b_file._file-name = 'ArtLag' NO-LOCK.
      PUT UNFORMATTED '      OUTPUT STREAM Ut2 TO value("' + FI-Katalog + '\' + b_file._dump-name '.d") APPEND.' SKIP.      
      PUT UNFORMATTED '        For EACH ArtLag where ArtLag.ArtikkelNr = ArtBas.ArtikkelNr and ArtLag.Butik = ' + STRING(FI-Butik) + ' NO-LOCK:' SKIP.
      PUT UNFORMATTED '            EXPORT STREAM Ut2 ArtLag.' SKIP.
      PUT UNFORMATTED '        END.' SKIP.
      PUT UNFORMATTED '      OUTPUT STREAM Ut2 CLOSE.' SKIP.

      PUT UNFORMATTED 'END.' SKIP.

      PUT UNFORMATTED 'OUTPUT STREAM Ut1 CLOSE.' SKIP.
    OUTPUT CLOSE.
    
    RUN VALUE("wrk\dumpaArtikelrelaterat.p").


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DumpaAlla C-Win 
PROCEDURE DumpaAlla :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER cTable AS CHARACTER   NO-UNDO.

    FIND skotex._file WHERE skotex._file._file-name = cTable NO-LOCK.

    IF ("wrk\dumpa_" + skotex._file._dump-name + ".p") = ? THEN
    DO:
        MESSAGE 'Feil ved eksport av tabell ' cTable
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        RETURN.
    END.

    OUTPUT TO value("wrk\dumpa_" + skotex._file._dump-name + ".p").
      PUT UNFORMATTED 'IF NOT CAN-FIND(FIRST ' + cTable + ') THEN RETURN.' SKIP.      
      PUT UNFORMATTED 'OUTPUT TO value("' + FI-Katalog + '\' + skotex._file._dump-name '.d").' SKIP.
      PUT UNFORMATTED 'FOR EACH ' cTable ':' SKIP.
      PUT UNFORMATTED '    EXPORT ' cTable '.' SKIP.
      PUT UNFORMATTED 'END.' SKIP.
      PUT UNFORMATTED 'OUTPUT CLOSE.' SKIP.
    OUTPUT CLOSE.
    
    RUN VALUE("wrk\dumpa_" + skotex._file._dump-name + ".p").
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DumpaButik C-Win 
PROCEDURE DumpaButik :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER cTable AS CHARACTER   NO-UNDO.
    DEFINE INPUT  PARAMETER cButfield AS CHARACTER   NO-UNDO.

    FIND skotex._file WHERE skotex._file._file-name = cTable NO-LOCK NO-ERROR.
    IF NOT AVAILABLE SkoTex._File THEN
    DO:
        MESSAGE 'Feil tabellangivelse. Finner ikke _File for angitt tabell.' SKIP
                'cTable: ' cTable
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        RETURN.
    END.

    IF ("' + FI-Katalog + '\' + skotex._file._dump-name '.d") = ? THEN
    DO:
        MESSAGE 'Feil ved eksport av tabell ' cTable
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        RETURN.
    END.
    
    OUTPUT TO value("wrk\dumpa_" + skotex._file._dump-name + ".p").

      PUT UNFORMATTED 'IF NOT CAN-FIND(FIRST ' + cTable + ' WHERE ' + cTable + '.' + cButfield + ' = ' + STRING(FI-Butik) + ') THEN RETURN.' SKIP.      
      PUT UNFORMATTED 'OUTPUT TO value("' + FI-Katalog + '\' + skotex._file._dump-name '.d").' SKIP.
      PUT UNFORMATTED 'FOR EACH ' cTable ' WHERE ' cButfield ' = ' FI-Butik ':' SKIP.
      PUT UNFORMATTED '    EXPORT ' cTable '.' SKIP.
      PUT UNFORMATTED 'END.' SKIP.
      PUT UNFORMATTED 'OUTPUT CLOSE.' SKIP.

    OUTPUT CLOSE.
    
    RUN VALUE("wrk\dumpa_" + skotex._file._dump-name + ".p").
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DumpaData C-Win 
PROCEDURE DumpaData :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    /*
    FIND data._file WHERE data._file._file-name = "priskontroll"  NO-LOCK.
    OUTPUT TO VALUE(FI-Katalog + data._file._dump-name + ".d").
    FOR EACH priskontroll WHERE priskontroll.butikknr = FI-Butik NO-LOCK.
        EXPORT priskontroll.
    END.
    OUTPUT CLOSE.
    */

    FIND data._file WHERE data._file._file-name = "Datasett" NO-LOCK.
    OUTPUT TO VALUE(FI-Katalog + data._file._dump-name + ".d").
    FOR EACH Datasett WHERE Datasett.butikknr = FI-Butik NO-LOCK.
        EXPORT Datasett.
    END.
    OUTPUT CLOSE.
    
    FIND data._file WHERE data._file._file-name = "BongHode"      NO-LOCK.
    OUTPUT TO VALUE(FI-Katalog + data._file._dump-name + ".d").
    FOR EACH bonghode WHERE bonghode.butikknr = FI-Butik NO-LOCK.
        EXPORT bonghode.
    END.
    OUTPUT CLOSE.
    
    FIND data._file WHERE data._file._file-name = "BongLinje"     NO-LOCK.
    OUTPUT TO VALUE(FI-Katalog + data._file._dump-name + ".d").
    FOR EACH bonglinje WHERE bonglinje.butikknr = FI-Butik NO-LOCK.
        EXPORT bonglinje.
    END.
    OUTPUT CLOSE.
    
    FIND data._file WHERE data._file._file-name = "Bokforingsdag" NO-LOCK.
    OUTPUT TO VALUE(FI-Katalog + data._file._dump-name + ".d").
    FOR EACH bokforingsdag WHERE bokforingsdag.butikknr = FI-Butik NO-LOCK.
        EXPORT bokforingsdag.
    END.
    OUTPUT CLOSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DumpaFaktura C-Win 
PROCEDURE DumpaFaktura :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER cTable AS CHARACTER   NO-UNDO.
    DEFINE INPUT  PARAMETER cButfield AS CHARACTER   NO-UNDO.

    DEF BUFFER b_File FOR SkoTex._File.

    FIND skotex._file WHERE skotex._file._file-name = cTable NO-LOCK.
    
    OUTPUT TO "wrk\dumpaFaktura.p".
      PUT UNFORMATTED 'IF NOT CAN-FIND(FIRST ' + cTable + ' WHERE ' + cTable + '.' + cButfield + ' = ' + STRING(FI-Butik) + ') THEN RETURN.' SKIP.      
      PUT UNFORMATTED 'DEF STREAM Ut1.' SKIP.
      PUT UNFORMATTED 'DEF STREAM Ut2.' SKIP(1).

      PUT UNFORMATTED 'OUTPUT STREAM Ut1 TO value("' + FI-Katalog + '\' + skotex._file._dump-name '.d").' SKIP(1).

      PUT UNFORMATTED 'FOR EACH ' cTable ' WHERE ' cButfield ' = ' FI-Butik ':' SKIP.
      PUT UNFORMATTED '    EXPORT STREAM Ut1 ' cTable '.' SKIP.

          FIND b_file WHERE b_file._file-name = 'FakturaLinje' NO-LOCK.
          PUT UNFORMATTED '    OUTPUT STREAM Ut2 TO value("' + FI-Katalog + '\' + b_file._dump-name '.d") APPEND.' SKIP.
          PUT UNFORMATTED '    FOR EACH FakturaLinje OF FakturaHode NO-LOCK:' SKIP.
          PUT UNFORMATTED '        EXPORT STREAM Ut2 FakturaLinje.' SKIP.
          PUT UNFORMATTED '    END.' SKIP.
          PUT UNFORMATTED '    OUTPUT STREAM Ut2 CLOSE.' SKIP(1).

      PUT UNFORMATTED 'END.' SKIP.
      
      PUT UNFORMATTED 'OUTPUT STREAM Ut1 CLOSE.' SKIP.
    OUTPUT CLOSE.
    
    RUN VALUE("wrk\dumpaFaktura.p").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DumpaKunde C-Win 
PROCEDURE DumpaKunde :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/*     KundeBetTrans                betButik              */
/*     KundeKommentar                                     */
/*     KundeKort                                          */
/*     Kundeprosjekt                                      */
/*     KundeResKobling                                    */
/*     Kundereskontr                                      */
/*     KundeSaldo                   ButikkNr              */
/*     KundeTrans                   Butik                 */

    DEFINE INPUT  PARAMETER cTable AS CHARACTER   NO-UNDO.
    DEFINE INPUT  PARAMETER cButfield AS CHARACTER   NO-UNDO.

    DEF VAR iProfilNr AS INT NO-UNDO.
    DEF BUFFER b_File FOR SkoTex._File.

    FIND skotex._file WHERE skotex._file._file-name = cTable NO-LOCK.
    FIND Butiker NO-LOCK WHERE Butiker.Butik = INT(FI-Butik).

    iProfilNr = Butiker.ProfilNr.

    IF ("' + FI-Katalog + '\' + skotex._file._dump-name '.d") = ? THEN
    DO:
        MESSAGE 'Feil ved eksport av tabell ' cTable
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        RETURN.
    END.
    
    OUTPUT TO "wrk\dumpaKunde.p".
      PUT UNFORMATTED 'DEF STREAM Ut1.' SKIP.
      PUT UNFORMATTED 'DEF STREAM Ut2.' SKIP(1).

      PUT UNFORMATTED 'OUTPUT STREAM Ut1 TO value("' + RIGHT-TRIM(FI-Katalog,'\') + '\' + skotex._file._dump-name '.d").' SKIP(1).
      PUT UNFORMATTED 'FOR EACH Kunde where Kunde.ButikkNr = ' + STRING(FI-Butik) + ':' SKIP.
      PUT UNFORMATTED '    EXPORT STREAM Ut1 Kunde.' SKIP(1).
      
          /* Eksport av relaterte kundedata */
          FIND b_file WHERE b_file._file-name = 'KundeKommentar' NO-LOCK.
          PUT UNFORMATTED '      OUTPUT STREAM Ut2 TO value("' + FI-Katalog + '\' + b_file._dump-name '.d") APPEND.' SKIP.      
          PUT UNFORMATTED '        For EACH KundeKommentar NO-LOCK where KundeKommentar.KundeNr = Kunde.KundeNr:' SKIP.
          PUT UNFORMATTED '            EXPORT STREAM Ut2 KundeKommentar.' SKIP.
          PUT UNFORMATTED '        END.' SKIP.
          PUT UNFORMATTED '      OUTPUT STREAM Ut2 CLOSE.' SKIP.
          
          FIND b_file WHERE b_file._file-name = 'KundeKort' NO-LOCK.
          PUT UNFORMATTED '      OUTPUT STREAM Ut2 TO value("' + FI-Katalog + '\' + b_file._dump-name '.d") APPEND.' SKIP.      
          PUT UNFORMATTED '        For EACH KundeKort NO-LOCK where KundeKort.KundeNr = Kunde.KundeNr:' SKIP.
          PUT UNFORMATTED '            EXPORT STREAM Ut2 KundeKort.' SKIP.
          PUT UNFORMATTED '        END.' SKIP.
          PUT UNFORMATTED '      OUTPUT STREAM Ut2 CLOSE.' SKIP(1).
                    
          PUT UNFORMATTED '      FOR EACH KundeReskontr OF Kunde NO-LOCK:' SKIP.
          
          FIND b_file WHERE b_file._file-name = 'KundeReskontr' NO-LOCK.
          PUT UNFORMATTED '      OUTPUT STREAM Ut2 TO value("' + FI-Katalog + '\' + b_file._dump-name '.d") APPEND.' SKIP.
          PUT UNFORMATTED '      EXPORT STREAM Ut2 Kundereskontr.' SKIP.
          PUT UNFORMATTED '      OUTPUT STREAM Ut2 CLOSE.' SKIP.

          FIND b_file WHERE b_file._file-name = 'KundeResKobling' NO-LOCK.
          PUT UNFORMATTED '          OUTPUT STREAM Ut2 TO value("' + FI-Katalog + '\' + b_file._dump-name '.d") APPEND.' SKIP.
                  
          PUT UNFORMATTED '          FOR EACH KundeResKobling NO-LOCK WHERE' SKIP. 
          PUT UNFORMATTED '            KundeREsKobling.KReskontro_id = Kundereskontr.Reskontro_id:' SKIP. 
          PUT UNFORMATTED '            EXPORT STREAM Ut2 KundeResKobling.' SKIP.      
          PUT UNFORMATTED '          END.' SKIP(1).

          PUT UNFORMATTED '          FOR EACH KundeResKobling NO-LOCK WHERE' SKIP. 
          PUT UNFORMATTED '            KundeREsKobling.DReskontro_id = Kundereskontr.Reskontro_id:' SKIP. 
          PUT UNFORMATTED '            EXPORT STREAM Ut2 KundeResKobling.' SKIP.      
          PUT UNFORMATTED '          END.' SKIP(1).

          PUT UNFORMATTED '          OUTPUT STREAM Ut2 CLOSE.' SKIP.
          PUT UNFORMATTED '      END.' SKIP(1).
          /* Relaterte kundedata slutt */

      PUT UNFORMATTED 'END.' SKIP.
      PUT UNFORMATTED 'OUTPUT STREAM Ut1 CLOSE.' SKIP(1).

      FIND b_file WHERE b_file._file-name = 'KundeBetTrans' NO-LOCK.
      PUT UNFORMATTED 'OUTPUT STREAM Ut2 TO value("' + FI-Katalog + '\' + b_file._dump-name '.d") APPEND.' SKIP.      
      PUT UNFORMATTED '  For EACH KundeBetTrans NO-LOCK where KundeBetTrans.Butik = ' + STRING(FI-Butik) + ':' SKIP.
      PUT UNFORMATTED '      EXPORT STREAM Ut2 KundeBetTrans.' SKIP.
      PUT UNFORMATTED '  END.' SKIP.
      PUT UNFORMATTED 'OUTPUT STREAM Ut2 CLOSE.' SKIP(1).
      
      FIND b_file WHERE b_file._file-name = 'KundeSaldo' NO-LOCK.
      PUT UNFORMATTED 'OUTPUT STREAM Ut2 TO value("' + FI-Katalog + '\' + b_file._dump-name '.d") APPEND.' SKIP.      
      PUT UNFORMATTED '  For EACH KundeSaldo NO-LOCK where KundeSaldo.ButikkNr = ' + STRING(FI-Butik) + ':' SKIP.
      PUT UNFORMATTED '      EXPORT STREAM Ut2 KundeSaldo.' SKIP.
      PUT UNFORMATTED '  END.' SKIP.
      PUT UNFORMATTED 'OUTPUT STREAM Ut2 CLOSE.' SKIP(1).
      
      FIND b_file WHERE b_file._file-name = 'KundeTrans' NO-LOCK.
      PUT UNFORMATTED 'OUTPUT STREAM Ut2 TO value("' + FI-Katalog + '\' + b_file._dump-name '.d") APPEND.' SKIP.      
      PUT UNFORMATTED '  For EACH KundeTrans NO-LOCK where KundeTrans.Butik = ' + STRING(FI-Butik) + ':' SKIP.
      PUT UNFORMATTED '      EXPORT STREAM Ut2 KundeTrans.' SKIP.
      PUT UNFORMATTED '  END.' SKIP.
      PUT UNFORMATTED 'OUTPUT STREAM Ut2 CLOSE.' SKIP.
      
    OUTPUT CLOSE.
    
    RUN VALUE("wrk\dumpaKunde.p").


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DumpaSkotex C-Win 
PROCEDURE DumpaSkotex :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    RUN Artikelrelaterat("ArtBas","Butik").
    RUN DumpaAlla ("Aktivitet").

    RUN DumpaAlla ("akt_mal").
    RUN DumpaButik ("akt_rapp","butik").
    RUN DumpaAlla ("AlfaLandKode").
/*    RUN DumpaAlla ("AltLevBas"). */
    RUN DumpaAlla ("Anv-Kod").
    RUN DumpaButik ("ApnSkjema","ButikkNr").
/*    RUN DumpaButik ("ArtBestPkt","ButikkNr"). */
/*    RUN DumpaAlla ("ArtikkelNrSerie").        */
    RUN DumpaButik ("ArtLok","ButikkNr").
    RUN DumpaAlla ("ArtSort").
    RUN DumpaAlla ("Avdeling").
/*     BatchLogg                                          */
    RUN DumpaAlla ("Behandlingskode").
    RUN DumpaAlla ("Beliggenhet").
/*     BestHLev                                           */
/*     /* BestHode                     sendtButikkDato */ */
/*     /* BestKasse                    Butik           */ */
/*     /* BestLevert                   Butik           */ */
/*     /* BestLevIndivid               Butik           */ */
/*     /* BestLinje                    Butik           */ */
/*     /* BestPris                                     */ */
/*     /* BestSort                                     */ */
/*     /* BestStr                      Butik           */ */
/*     /* Fributik                     Butik           */ */
/*     /* Ordre                        CL              */ */
    RUN DumpaAlla ("Betalingsbetingelser").
    RUN DumpaAlla ("Bilagsart").
    RUN DumpaAlla ("Bilagstype").
    RUN DumpaAlla ("BildeData").
    RUN DumpaAlla ("Bilderegister").
    RUN DumpaButik ("BokforingsBilag","ButikkNr").
    RUN DumpaButik ("Bruker","ButikkNr").
    RUN Dumpa_User ("_user").
    RUN DumpaAlla ("BrukerGrp").
/*     BrukerLev                                          */
    RUN DumpaButik ("Butiker","butik").
    RUN DumpaButik ("butikkforsalj","Butik").
    RUN DumpaButik ("ButikkKobling","Butik").
    RUN DumpaButik ("ButikkSelger","ButikkNr").
/*     RUN DumpaButikkTeam */
/*     ButikkTilgang                Butik                 */
    RUN DumpaButik ("ButLok","ButikkNr").
    RUN DumpaAlla ("dags_lbl").
    RUN DumpaButik ("dags_rap","butikk").
    RUN DumpaButik ("DefaultLevDato","ButikkNr").
    RUN DumpaAlla ("DriftsForm").
    RUN DumpaAlla ("DriftsType").
/*     EANNrListe                                         */
/*     EANNrSerie                                         */
    RUN DumpaAlla ("EDBDataType").
    RUN DumpaAlla ("EkstEDBSystem").
    RUN DumpaAlla ("EkstVPIFil").
    RUN DumpaAlla ("EkstVPILev").
    RUN DumpaAlla ("EkstVPITabell").
/*     ELogg                                              */
    RUN DumpaAlla ("Erstattningsvare").
    RUN DumpaButik ("etikett","butik").
    RUN DumpaButik ("Etikettko","ButikkNr").
    RUN DumpaFaktura ("FakturaHode","ButikkNr").
/*     FakturaLinje                                       */
    RUN DumpaAlla ("FakturaTekst").
/*     /* Falck_Sykkelregister         ButikkNr */        */
    RUN DumpaAlla ("Farg").
    RUN DumpaAlla ("Feilkode").
    RUN DumpaAlla ("FilArkiv").
    RUN DumpaAlla ("foder").
/*     Forsalj                      ButikkNr              */
    RUN DumpaAlla ("Fylke").
    RUN DumpaAlla ("Garanti").
    RUN DumpaButik ("Gavekort","butnr").
    RUN DumpaAlla ("GaveKType").
    RUN DumpaButik ("Gruppe","ButikkNr").
    RUN DumpaAlla ("Handtering").
/*     HappyHourHode                                      */
/*     HappyHourPeriode                                   */
    RUN DumpaAlla ("HjelpMap").
    RUN DumpaAlla ("HT-Type").
    RUN DumpaAlla ("HuvGr").
/*     ImpHode                                            */
/*     ImpKonv                                            */
/*     Individ                      butnr                 */
    RUN DumpaAlla ("IndType").
    RUN DumpaAlla ("InnBetType").
    RUN DumpaAlla ("InnerSula").
    RUN DumpaAlla ("Innkjopsgrupper").
    RUN DumpaAlla ("JamforEnhet").
    RUN DumpaAlla ("JBoxCompany").
    RUN DumpaAlla ("JBoxCompanyUser").
/*     /* JBoxEventCategory                           */  */
/*     /* JBoxEventGroup                              */  */
/*     /* JBoxEventLine                               */  */
/*     /* JBoxEventSource                             */  */
/*     /* JBoxEventType                               */  */
/*     /* JBoxFunction                                */  */
/*     /* JBoxFunctionAccess                          */  */
/*     /* JBoxGenCode                                 */  */
/*     /* JBoxGenCodeType                             */  */
/*     /* JBoxLoginSession                            */  */
    RUN DumpaAlla ("JBoxMenu").
    RUN DumpaAlla ("JBoxMenuToMenu").
/*     /* JBoxQCriteria                               */  */
/*     /* JBoxQEntity                                 */  */
/*     /* JBoxQForeignKey                             */  */
/*     /* JBoxQLog                                    */  */
/*     /* JBoxQRelation                               */  */
    RUN DumpaAlla ("JBoxTranslation").
    RUN DumpaAlla ("JBoxUser").
    RUN DumpaAlla ("JBoxUserGroup").
    RUN DumpaAlla ("JBoxUserGroupMembers").
/*     /* JBoxUserMenu                                */  */
/*     /* JBoxUserSetting                             */  */
    RUN DumpaAlla ("Jobb").
/*     KampanjeButikker             Butik                 */
/*     KampanjeButMottak            Butik                 */
/*     KampanjeEier                                       */
/*     KampanjeHode                                       */
/*     KampanjeLinje                                      */
/*     KampanjeMixMatch                                   */
/*     KampanjeTilbArtikkel                               */
/*     KampanjeTilbType                                   */
/*     KampanjeTilbud                                     */
/*     KampRabattType                                     */
/*     KampTilbTemplate                                   */
    RUN DumpaButik ("Kasse","ButikkNr").
    RUN DumpaButik ("KassererBilag","ButikkNr").
    RUN DumpaButik ("kassererDag","ButikkNr").
    RUN DumpaButik ("KassererDat","ButikkNr").
    RUN DumpaButik ("KassererKontanter","ButikkNr").
    RUN DumpaButik ("KassererOppgj","ButikkNr").
    RUN DumpaButik ("KassererValuta","ButikkNr").
    RUN DumpaButik ("KasseTrans","butikk").
    RUN DumpaAlla ("KasValuta").
    RUN DumpaButik ("kas_konter","butikk").
    RUN DumpaButik ("kas_logg","butikk").
    RUN DumpaButik ("kas_rap","butikk").
    RUN DumpaAlla ("KatalogArkiv").
    RUN DumpaAlla ("Kategori").
/*     Kjede                                              */
/*     KjedeDistrikt                                      */
/*     KjedensButikker              ButikkNavn            */
/*     KjedeRegion                                        */
    RUN DumpaAlla ("klack").
    RUN DumpaAlla ("Kommune").
    RUN DumpaButik ("konto","butikk").
    RUN DumpaAlla ("KontoTabell").
    RUN DumpaButik ("kont_mal","butikk").
    RUN DumpaAlla ("KonvReg").
/*     KOrdreHode                   Butik                 */
/*     KOrdreLinje                  PlukkButikk           */
/*     KortNrListe                                        */
/*     Kort_Spes                    butikk                */
    RUN DumpaAlla ("Kravkode").
    RUN DumpaButik ("Kravkode","ButikkNr").
    RUN DumpaAlla ("KundeGruppe").
    RUN DumpaAlla ("KundeType").
/*     Kupong                                             */
/*     KupongEier                                         */
/*     KupongTransLogg                                    */
/*     KupongType                                         */
/*     KupongVarekode                                     */
    RUN DumpaAlla ("LapTop").
    RUN DumpaAlla ("Last-Sko").
    RUN DumpaAlla ("LevBas").
    RUN DumpaAlla ("LeveringsForm").
    RUN DumpaAlla ("LevKontakt").
/*     LevLager                                           */
/*     LevPris                                            */
/*     LevSAnt                                            */
/*     LevSort                                            */
/*     ListeLinje                                         */
/*     Lister                                             */
    RUN DumpaAlla ("Lng").
/*     LokalGruppering                                    */
    RUN DumpaAlla ("Material").
/*     Medlem                       ButikkNr              */
/*     MedlemBetTrans               betButik              */
/*     MedlemSaldo                  ButikkNr              */
/*     MedlemsGruppe                                      */
/*     MedlemsKort                                        */
/*     MedlemsType                                        */
/*     MedTrans                     Butik                 */
    RUN DumpaAlla ("Meny").
/*     Messe                                              */
/*     MesseForButikk               Butikknr              */
/*     MixMatchHode                                       */
/*     MixMatchRad                                        */
    RUN DumpaAlla ("Moms").
/*     NON_Sale_Spes                butikk                */
    RUN DumpaAlla ("Notatkoder").
    RUN DumpaAlla ("NumLandKode").
    RUN DumpaAlla ("Ovandel").
/*     /* OvArt                                    */     */
/*     /* OvBuffer                     ButikkNrFra */     */
/*     /* OvBunt                                   */     */
/*     /* OvLinje                      FraButik    */     */
/*     /* OvLink                                   */     */
/*     /* OvOrdre                      FraButikk   */     */
/*     PakkeLinje                                         */
    RUN DumpaAlla ("Periode").
    RUN DumpaAlla ("PerLin").
/*     /* PkSdlHode                                */     */
/*     /* PkSdlLinje                   ButikkNr    */     */
/*     /* PkSdlMottak                              */     */
/*     /* PkSdlPris                                */     */
/*     /* plListeArtikkel                          */     */
/*     /* PlListeHode                  FraButikkNr */     */
/*     /* PlListeLinje                             */     */
/*     /* PlListeModell                            */     */
/*     /* plListeType                              */     */
    RUN DumpaAlla ("Post").
/*     PrintLogg                                          */
/*     PrintLoggType                                      */
    RUN DumpaAlla ("PrisGruppe").
/*     PrisListeHode                                      */
/*     PrisListeKunde                                     */
/*     PrisListeLinje                                     */
/*     Prisprofil                                         */
/*     ProduktFamilie                                     */
/*     ProduktFamMedlem                                   */
    RUN DumpaAlla ("Produsent").
/*     ProgBrGrp                                          */
    RUN DumpaAlla ("ProgramListe").
    RUN DumpaAlla ("Prov").
    RUN DumpaAlla ("Purretrinn").
    RUN DumpaAlla ("Rabatt").
    RUN DumpaAlla ("RappType").
/*     Region                                             */
    RUN DumpaAlla ("Regnskapsavdeling").
/*     ReklamasjonsLinje            Butik                 */
/*     ReklamasjonsLogg             ButikkNr              */
    RUN DumpaAlla ("Salgsenhet").
    RUN DumpaAlla ("SaSong").
    RUN DumpaAlla ("SegmentRegister").
    RUN DumpaButik ("Selger","ButikkNr").
    RUN DumpaAlla ("SlitSula").
    RUN DumpaAlla ("sprak").
    RUN DumpaAlla ("StDef").
    RUN DumpaAlla ("StHode").
    RUN DumpaButik ("StLager","Butik").
    /*RUN DumpaButik ("StLagerHist","ButikkNr").*/
                                  
    RUN DumpaAlla ("StrKonv").
    RUN DumpaAlla ("StrTStr").
    RUN DumpaAlla ("StrType").
    RUN DumpaAlla ("StType").
    RUN DumpaAlla ("SysGruppe").
    RUN DumpaAlla ("SysHode").
    RUN DumpaAlla ("SysPara").
    RUN DumpaAlla ("TeamType").
    RUN DumpaAlla ("Tekst").
/*     /* TelleHode                    ButikkListe */     */
/*     /* TelleLinje                   Butik       */     */
    RUN DumpaButik ("Tilgode","butnr").
    RUN DumpaAlla ("TransType").
    RUN DumpaAlla ("TransBeskr").
    RUN DumpaAlla ("UtbetType").
    RUN DumpaAlla ("Valuta").
/*     /* VareBehBestHode              Butikkliste     */ */
/*     /* VareBehBestLinje             BestiltButikkNr */ */
/*     /* VareBehHode                  ButikkListe     */ */
/*     /* VareBehLinje                                 */ */
/*     /* VareBehLinjeTHode            ButikkNr        */ */
/*     /* VareBehLinjeTrans            ButikkNr        */ */
/*     /* VareBehPris                                  */ */
/*     /* VareBehType                                  */ */
/*     /* VareBhBrukerGrp                              */ */
/*     /* VareBokHode                                  */ */
/*     /* VareBokLinje                                 */ */
/*     /* VarebokTemaHode                              */ */
/*     /* VareBokTemaLinje                             */ */
/*     /* VareBokType                                  */ */
    RUN DumpaAlla ("Varemerke").
/*     Varetrans                    ButikkNr              */
    RUN DumpaAlla ("VarGr").
    RUN DumpaAlla ("VgAkt").
    RUN DumpaAlla ("VgKat").
    RUN DumpaAlla ("VgKundeGrpRabatt").
    RUN DumpaAlla ("VPIFilType").
/*     VPIMottak                                          */


    RUN DumpaKunde ("Kunde","ButikkNr").

    RUN DumpaButik ("z_nummer","butikk").
    RUN DumpaButik ("StLinje","Butik").
    RUN DumpaButik ("TransLogg","Butik").


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Dumpa_User C-Win 
PROCEDURE Dumpa_User :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER cTable AS CHARACTER   NO-UNDO.

    FIND skotex._file WHERE skotex._file._file-name = '_User' NO-LOCK.

    OUTPUT TO value("wrk\dumpa_User.p").
      PUT UNFORMATTED 'IF NOT CAN-FIND(FIRST SkoTex._User) THEN RETURN.' SKIP.      
      PUT UNFORMATTED 'OUTPUT TO value("' + FI-Katalog + '\' + '_User.d").' SKIP.
      PUT UNFORMATTED 'FOR EACH SkoTex._User:' SKIP.
      PUT UNFORMATTED '    EXPORT SkoTex._User.' SKIP.
      PUT UNFORMATTED 'END.' SKIP.
      PUT UNFORMATTED 'OUTPUT CLOSE.' SKIP.
    OUTPUT CLOSE.
    
    RUN VALUE("wrk\dumpa_User.p").
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
  DISPLAY FI-Butik FI-butnamn FI-Katalog FI-ButikTxt 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE B-But FI-Butik FI-butnamn B-Dir FI-Katalog B-Starta FI-ButikTxt 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

