&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: 

  Description:        JukeBox suppressed window template
                      This template is for use with a tabfolder or viewer object
                      to parent the suppressed window

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author:            brynjar@chemistry.no

  Created:           30.march.2008

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

DEF VAR bOk                 AS LOG    NO-UNDO.
DEF VAR ix                  AS INT    NO-UNDO.
DEF VAR hBrowse             AS HANDLE NO-UNDO.
DEF VAR hToolbar            AS HANDLE NO-UNDO.
DEF VAR hBuffer             AS HANDLE NO-UNDO.
DEF VAR hParent             AS HANDLE NO-UNDO.
DEF VAR hParentQueryObject  AS HANDLE NO-UNDO.
DEF VAR iReturn             AS INT    NO-UNDO.
DEF VAR cButikkNr           AS CHAR   NO-UNDO.

DEF VAR cSprak                   AS CHAR        NO-UNDO.
DEF VAR cSprakLst                AS CHAR  INITIAL 'SE,SVE' NO-UNDO.

{etikettlogg.i &NEW = "NEW"}

DEF TEMP-TABLE ttEtikettKoLogging
    FIELD RowIdent1 AS CHAR.
DEF VAR httEtikettKoLogging AS HANDLE NO-UNDO.
httEtikettKoLogging = BUFFER ttEtikettkoLogging:HANDLE:TABLE-HANDLE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS brwEtikett tbEtikett tbTilbudspris 
&Scoped-Define DISPLAYED-OBJECTS tbTilbudspris 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD AdjustBrowseColumns C-Win 
FUNCTION AdjustBrowseColumns RETURNS LOGICAL
  ( INPUT ihBrowse AS HANDLE,
    INPUT icBrowseName AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getFrameHandle C-Win 
FUNCTION getFrameHandle RETURNS HANDLE
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD InitializeResize C-Win 
FUNCTION InitializeResize RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setParent C-Win 
FUNCTION setParent RETURNS LOGICAL
  ( INPUT ihParent AS HANDLE )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setQuery C-Win 
FUNCTION setQuery RETURNS LOGICAL
  ( INPUT ihParentQueryObject AS HANDLE )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE RECTANGLE brwEtikett
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 80 BY 16.19.

DEFINE RECTANGLE tbEtikett
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 9.8 BY .91.

DEFINE VARIABLE tbTilbudspris AS LOGICAL INITIAL no 
     LABEL "Skriv ut tilbudspris" 
     VIEW-AS TOGGLE-BOX
     SIZE 25 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     tbTilbudspris AT ROW 1.33 COL 51
     brwEtikett AT ROW 2.43 COL 1
     tbEtikett AT ROW 1.24 COL 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 80.8 BY 17.91.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* SUPPRESS Window definition (used by the UIB) 
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Etikett browse"
         HEIGHT             = 17.76
         WIDTH              = 80.6
         MAX-HEIGHT         = 28.57
         MAX-WIDTH          = 160
         VIRTUAL-HEIGHT     = 28.57
         VIRTUAL-WIDTH      = 160
         SHOW-IN-TASKBAR    = no
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
                                                                        */
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME
ASSIGN C-Win = CURRENT-WINDOW.


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB C-Win 
/* ************************* Included-Libraries *********************** */

{incl/devmode.i}
{incl/custdevmode.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Etikett browse */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Etikett browse */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
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
ON CLOSE OF THIS-PROCEDURE DO:
  /* The viewer or tabfolder object will see to that the Disable_UI is run
     and the container will make sure that all dynamic objects + resize settings are deleted
     also for the suppressed windows */
  PUBLISH "InvalidateHandle".
END.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN enable_UI.

  &IF DEFINED(UIB_is_Running) NE 0 &THEN
    RUN InitializeObject.
  &ENDIF

  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

{incl/supptrigg.i hBrowse}  /* <- To be able to capture keys (insert, delete..) on the browse */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ButikkNrChanged C-Win 
PROCEDURE ButikkNrChanged :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM icButikkNr AS CHAR NO-UNDO.

cButikkNr = icButikkNr.
  
DYNAMIC-FUNCTION("ClearQueryFilter",hBrowse).
DYNAMIC-FUNCTION("InitDynFilter",hBrowse,"Butikknr,Utskriftsnr","=,=",cButikknr + "|0","STATIC").

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
  /* Hide all frames. */
  HIDE FRAME DEFAULT-FRAME.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DisplayRecord C-Win 
PROCEDURE DisplayRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
RUN SUPER.

DYNAMIC-FUNCTION("VisArtBilde" IN hParent,hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("Bildnr"):BUFFER-VALUE) NO-ERROR.

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
  DISPLAY tbTilbudspris 
      WITH FRAME DEFAULT-FRAME.
  ENABLE brwEtikett tbEtikett tbTilbudspris 
      WITH FRAME DEFAULT-FRAME.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE FindRecord C-Win 
PROCEDURE FindRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR cLookupValue  AS CHAR NO-UNDO.

cLookupValue = "UtskriftsNr".

RUN JBoxDLookup.w ("Etikettko"
                 + ";DISTINCT Utskriftsnr"
                 + ";Utskriftsdato"
                 + ";+2ph_Antall|INTEGER|>>>9|AntEtikett|Antall"
                ,"WHERE FALSE"
                ,INPUT-OUTPUT cLookupValue).

IF cLookupValue NE "" THEN DO:
  DYNAMIC-FUNCTION("ClearQueryFilter",hBrowse).
  DYNAMIC-FUNCTION("InitDynFilter",hBrowse,"Butikknr,Utskriftsnr","=,=",cButikknr + "|" + cLookupValue,"STATIC").
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitializeObject C-Win 
PROCEDURE InitializeObject :
/*------------------------------------------------------------------------------
  Purpose:     Create any dynamic objects, initialize drop-down lists etc
  Parameters:  <none>
  Notes:       The dynamic query object must be linked to the THIS-PROCEDURE
------------------------------------------------------------------------------*/
DEF VAR hFieldAnt           AS HANDLE NO-UNDO.
DEF VAR hFieldEti1          AS HANDLE NO-UNDO.
DEF VAR hFieldEti2          AS HANDLE NO-UNDO.

DO WITH FRAME {&FRAME-NAME}:
  cSprak      = DYNAMIC-FUNCTION("getFieldValues","Bruker","WHERE brukerid = '" + DYNAMIC-FUNCTION("getASuserId") + "'","Lng").
  DYNAMIC-FUNCTION("NewObject",THIS-PROCEDURE:CURRENT-WINDOW,THIS-PROCEDURE,"procedure").

  SUBSCRIBE TO "ButikkNrChanged" IN hParent.

  hBrowse = DYNAMIC-FUNCTION("NewBrowse"
          ,brwEtikett:HANDLE
          ,10000
          ,"multiple"
          ,"Etikettko"
           + ";ArtikkelNr|Art.nr"
           + ";EtikettAntHylleplasser|Ant.eti@7"
           + ";Etikettekst1"
           + ";Etikettekst2"
           + ";Pris[1]|Pris@8"
           + ";Pris[2]|Tilb.pris@9"
           + ";+JamforPris|DECIMAL|->><>>><>>9.99|jamforpris|Jamfør pris@10"
           + ";EtiLayout"
           + ";JamforEnhet"
           + ";Mengde"
           + ";!StrKode"
           + ";Kode"
           + ";UtskriftsDato"
           + ";Utskriftsnr|Nr@1"
           + ";ButikkNr"
           + ";RegistrertAv"
           + ";RegistrertDato"
           + ";BrukerID"
           + ";EDato"
         + ",ArtBas"
           + ";Beskr@4"
           + ";LevKod|Lev.artnr@3"
           + ";LevFargKod|Lev.farge@5"
           + ";!Bildnr"
           + ";!Vg"
           + ";!Lopnr"
         + ",StrKonv"
           + ";Storl|Str@6"
          ,"WHERE false"
         + ",FIRST ArtBas NO-LOCK OF Etikettko"
         + ",FIRST StrKonv NO-LOCK OF Etikettko OUTER-JOIN"
          ,"").

  DYNAMIC-FUNCTION("CreateObjectLink",hBrowse,THIS-PROCEDURE).

  ASSIGN hBrowse:HELP = "Dobbeltklikk for å redigere antall etiketter eller tekst"
         hBuffer      = hBrowse:QUERY:GET-BUFFER-HANDLE(1)
         .

  DYNAMIC-FUNCTION("setSortString",hBrowse,"ButikkNr,ArtikkelNr,Storl").

  DYNAMIC-FUNCTION("setAttribute",hBrowse,"getRecordCount","yes").
  DYNAMIC-FUNCTION("setAttribute",hBrowse,"calcFieldProc","etikettko_brwcalc.p").
  DYNAMIC-FUNCTION("setAttribute",hBrowse,"enableOnDblClick","yes").


/*   DYNAMIC-FUNCTION("setAttribute",hBrowse,"filtervalue_Utskriftsnr","0").   */
/*   DYNAMIC-FUNCTION("setAttribute",hBrowse,"OperatorInUse_Utskriftsnr","="). */

  hFieldAnt = DYNAMIC-FUNCTION("NewBrowseFillIn",hBrowse,"EtikettAntHylleplasser","EtikettAntHylleplasser"
            ,"","",""
            ,"").
  DYNAMIC-FUNCTION("CreateOverlayLink",hBrowse,hFieldAnt,"EtikettAntHylleplasser").

  hFieldEti1 = DYNAMIC-FUNCTION("NewBrowseFillIn",hBrowse,"Etikettekst1","Etikettekst1"
           ,"","",""
           ,"").
  DYNAMIC-FUNCTION("CreateOverlayLink",hBrowse,hFieldEti1,"Etikettekst1").

  hFieldEti2 = DYNAMIC-FUNCTION("NewBrowseFillIn",hBrowse,"Etikettekst2","Etikettekst2"
           ,"","",""
           ,"").
  DYNAMIC-FUNCTION("CreateOverlayLink",hBrowse,hFieldEti1,"Etikettekst2").

  hToolbar = DYNAMIC-FUNCTION("NewToolBar"
           ,tbEtikett:HANDLE
           ,""
           ,"Refresh,Utskrift"
          + ",Find;Tidligere utskrifter¤enable"
          + ",delete;Slett"
/*           + ",Filter"  */
          + ",browseConfig;Kolonneoppsett"
          + ",excel;Eksporter til E&xcel"
           ,"maxborder").

  DYNAMIC-FUNCTION("CreateObjectLink",hBrowse,hToolbar).

  DYNAMIC-FUNCTION("setAttribute",hBrowse,"executeDynFilter","NO").
  DYNAMIC-FUNCTION("InitDynFilter",hBrowse,"Utskriftsnr,Butikknr","=,=","0|" + DYNAMIC-FUNCTION("getButikkNr" IN hParent),"STATIC").
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE MoveToTop C-Win 
PROCEDURE MoveToTop :
/*------------------------------------------------------------------------------
  Purpose:     Mak
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
FRAME {&FRAME-NAME}:MOVE-TO-TOP().

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE RefreshRecord C-Win 
PROCEDURE RefreshRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DYNAMIC-FUNCTION("ClearQueryFilter",hBrowse).
DYNAMIC-FUNCTION("InitDynFilter",hBrowse,"Butikknr,Utskriftsnr","=,=",cButikknr + "|0","STATIC").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ScannTilKoRecord C-Win 
PROCEDURE ScannTilKoRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM icProfilNr AS CHAR NO-UNDO.
DEF INPUT PARAM icButikkNr AS CHAR NO-UNDO.

THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = NO.
RUN ScannTilEtikettKo.w (hBrowse,icProfilNr,icButikkNr).
THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = YES.
THIS-PROCEDURE:CURRENT-WINDOW:MOVE-TO-TOP().
APPLY "entry" TO hBrowse.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setLookupAttributes C-Win 
PROCEDURE setLookupAttributes :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM ihBrowse AS HANDLE NO-UNDO.
DEF INPUT PARAM ihDummy  AS HANDLE NO-UNDO.
DEF INPUT PARAM ihDummy2 AS HANDLE NO-UNDO.

DYNAMIC-FUNCTION("setSortString",ihBrowse,"Utskriftsnr;desc").
DYNAMIC-FUNCTION("setAttribute",ihBrowse,"baseQuery","WHERE ButikkNr = " + cButikkNr).
DYNAMIC-FUNCTION("setAttribute",ihBrowse,"calcFieldProc","etikett_find_brwcalc.p").
RUN InvokeMethod(ihBrowse,"OpenQuery").
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE UtskriftRecord C-Win 
PROCEDURE UtskriftRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE iCount        AS INTEGER    NO-UNDO.
DEFINE VARIABLE iStartEtikett AS INTEGER    NO-UNDO.
DEFINE VARIABLE iPrinterValg  AS INTEGER    NO-UNDO.

DEF VAR iReturn    AS INT    NO-UNDO.
DEF VAR cButList   AS CHAR   NO-UNDO.
DEF VAR hQuery     AS HANDLE NO-UNDO.


  IF CAN-DO(cSprakLst,cSprak) THEN DO:
      RUN JBoxBrowseSelectMsg.w ("Skall etiketter skrivas ut?",
                                    hBrowse:NUM-SELECTED-ROWS,
                                    IF INT(DYNAMIC-FUNCTION("getAttribute",hBrowse,"Totalcount")) LT INT(DYNAMIC-FUNCTION("getAttribute",hBrowse,"rowstobatch")) THEN
                                      INT(DYNAMIC-FUNCTION("getAttribute",hBrowse,"Totalcount"))
                                    ELSE 99999,
                                    OUTPUT iReturn).  /*1=Alle,2=Valgte*/
  END.
  ELSE DO:
      RUN JBoxBrowseSelectMsg.w ("Skal etiketter skrives ut?",
                                    hBrowse:NUM-SELECTED-ROWS,
                                    IF INT(DYNAMIC-FUNCTION("getAttribute",hBrowse,"Totalcount")) LT INT(DYNAMIC-FUNCTION("getAttribute",hBrowse,"rowstobatch")) THEN
                                      INT(DYNAMIC-FUNCTION("getAttribute",hBrowse,"Totalcount"))
                                    ELSE 99999,
                                    OUTPUT iReturn).  /*1=Alle,2=Valgte*/
  END.

IF iReturn >= 1 AND iReturn <= 2 THEN
DO:
    RUN d-skrivervalg.w (OUTPUT iPrinterValg,OUTPUT iStartEtikett).
    IF iPrinterValg = 0 THEN
        RETURN.

    EMPTY TEMP-TABLE Etikettlogg.
    EMPTY TEMP-TABLE ttEtikettKoLogging.

    IF iStartEtikett > 1 THEN DO:
      CREATE EtikettLogg.
      ASSIGN EtikettLogg.Butik = 0
             EtikettLogg.SeqNr = 0
             EtikettLogg.Storl  = "STARTETIKETT"
             EtikettLogg.Ant = iStartEtikett.
      /* Ant avänds för att ange startetikett */
    END.
END.
ELSE 
    RETURN.

IF iReturn = 1 THEN 
SEND_ALLE:
DO:
    CREATE QUERY hQuery.
    hQuery:SET-BUFFERS(hBuffer).
    hQuery:QUERY-PREPARE("FOR EACH " + hBuffer:NAME + " NO-LOCK").
    hQuery:QUERY-OPEN().

    hQuery:GET-FIRST().
    REPEAT WHILE NOT hQuery:QUERY-OFF-END:
        create EtikettLogg.
        ASSIGN iCount = iCount + 1
               EtikettLogg.Butik     = iCount /* Det skal skrives ut i seqnr ordning. */
               EtikettLogg.Vg        = hBuffer:BUFFER-FIELD("Vg"):BUFFER-VALUE 
               EtikettLogg.LopNr     = hBuffer:BUFFER-FIELD("Lopnr"):BUFFER-VALUE
               EtikettLogg.Ant       = hBuffer:BUFFER-FIELD("EtikettAntHylleplasser"):BUFFER-VALUE
               EtikettLogg.Storl     = hBuffer:BUFFER-FIELD("Kode"):BUFFER-VALUE /* Strekkode skal inn i størrelsen */
               EtikettLogg.Bongtekst = hBuffer:BUFFER-FIELD("Etikettekst1"):BUFFER-VALUE 
                                     + (IF hBuffer:BUFFER-FIELD("Etikettekst2"):BUFFER-VALUE NE "" THEN
                                         CHR(1) + hBuffer:BUFFER-FIELD("Etikettekst2"):BUFFER-VALUE
                                        ELSE "")
               EtikettLogg.Pris      = IF NOT tbTilbudspris:CHECKED IN FRAME {&FRAME-NAME}
                                         THEN hBuffer:BUFFER-FIELD("Pris[1]"):BUFFER-VALUE  
                                         ELSE hBuffer:BUFFER-FIELD("Pris[2]"):BUFFER-VALUE
               EtikettLogg.Individ   = 0
               EtikettLogg.SeqNr     = iCount.

        CREATE ttEtikettKoLogging.
        ttEtikettKoLogging.RowIdent1 = hBuffer:BUFFER-FIELD("RowIdent1"):BUFFER-VALUE. 

        hQuery:GET-NEXT().
    END.
END. /* SEND_ALLE */

ELSE IF iReturn = 2 THEN
SEND_VALGTE:
DO ix = 1 TO hBrowse:NUM-SELECTED-ROWS:
  IF hBrowse:FETCH-SELECTED-ROW(ix) THEN
  DO:
      create EtikettLogg.
      ASSIGN iCount = iCount + 1
             EtikettLogg.Butik     = iCount /* Det skal skrives ut i seqnr ordning. */
             EtikettLogg.Vg        = hBuffer:BUFFER-FIELD("Vg"):BUFFER-VALUE 
             EtikettLogg.LopNr     = hBuffer:BUFFER-FIELD("Lopnr"):BUFFER-VALUE
             EtikettLogg.Ant       = hBuffer:BUFFER-FIELD("EtikettAntHylleplasser"):BUFFER-VALUE
             EtikettLogg.Storl     = hBuffer:BUFFER-FIELD("Kode"):BUFFER-VALUE /* Strekkode skal inn i størrelsen */
             EtikettLogg.Bongtekst = hBuffer:BUFFER-FIELD("Etikettekst1"):BUFFER-VALUE 
                                   + (IF hBuffer:BUFFER-FIELD("Etikettekst2"):BUFFER-VALUE NE "" THEN
                                       CHR(1) + hBuffer:BUFFER-FIELD("Etikettekst2"):BUFFER-VALUE
                                      ELSE "")
             EtikettLogg.Pris      = IF NOT tbTilbudspris:CHECKED IN FRAME {&FRAME-NAME}
                                       THEN hBuffer:BUFFER-FIELD("Pris[1]"):BUFFER-VALUE  
                                       ELSE hBuffer:BUFFER-FIELD("Pris[2]"):BUFFER-VALUE
             EtikettLogg.Individ   = 0
             EtikettLogg.SeqNr     = iCount.

      CREATE ttEtikettKoLogging.
      ttEtikettKoLogging.RowIdent1 = hBuffer:BUFFER-FIELD("RowIdent1"):BUFFER-VALUE. 
  END.
END. /* SEND_VALGTE */

RUN x-etikettsend.w (iPrinterValg).

DYNAMIC-FUNCTION("runProc","etikettko_sett_utskrnr.p","",httEtikettKoLogging).
RUN InvokeMethod(hBrowse,"OpenQuery").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION AdjustBrowseColumns C-Win 
FUNCTION AdjustBrowseColumns RETURNS LOGICAL
  ( INPUT ihBrowse AS HANDLE,
    INPUT icBrowseName AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
ASSIGN 
       ihBrowse:GET-BROWSE-COLUMN(1):WIDTH-PIXELS = 10
       ihBrowse:GET-BROWSE-COLUMN(2):WIDTH-PIXELS = 75
       ihBrowse:GET-BROWSE-COLUMN(3):WIDTH-PIXELS = 40
       ihBrowse:GET-BROWSE-COLUMN(4):WIDTH-PIXELS = 80
       ihBrowse:GET-BROWSE-COLUMN(5):WIDTH-PIXELS = 40 
       ihBrowse:GET-BROWSE-COLUMN(6):WIDTH-PIXELS = 20
       ihBrowse:GET-BROWSE-COLUMN(7):WIDTH-PIXELS = 20
       ihBrowse:GET-BROWSE-COLUMN(10):WIDTH-PIXELS = 55
       .
  
RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getFrameHandle C-Win 
FUNCTION getFrameHandle RETURNS HANDLE
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose: Make the frame of the child procedure known to the tabfolder or viewer object
    Notes: The procedure is mandatory
------------------------------------------------------------------------------*/

  RETURN FRAME {&FRAME-NAME}:HANDLE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION InitializeResize C-Win 
FUNCTION InitializeResize RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose: This function is invoked automatically from the viewer or tabfolder object before
           InitializeObject and also before the suppressed window is resized to fit the size of the 
           graphics placeholder.
  Notes:   Note that if the suppressed window contains a searchfield or toolbar object
           the placeholders for these must have resize settings here since the objects themselves have not been 
           initialized. (When a searchfield or tabfolder is created the resize is automatically taken care of).
           
           This function is not mandatory but is called if it exists
------------------------------------------------------------------------------*/

DYNAMIC-FUNCTION("setNoResizeX", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE,"SearchField").
DYNAMIC-FUNCTION("setNoResizeY", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE,"SearchField,rectToolbar").
  
RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setParent C-Win 
FUNCTION setParent RETURNS LOGICAL
  ( INPUT ihParent AS HANDLE ) :
/*------------------------------------------------------------------------------
  Purpose: Set the handle to the parent procedure for the suppressed window
    Notes: This function is not mandatory but is called if it exists
------------------------------------------------------------------------------*/
hParent = ihParent.

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setQuery C-Win 
FUNCTION setQuery RETURNS LOGICAL
  ( INPUT ihParentQueryObject AS HANDLE ) :
/*------------------------------------------------------------------------------
  Purpose: Set the handle of the parent (or oneToOne navigation query/browse) so
           it is available for the child object 
    Notes: This function is not mandatory but is called if it exists 
------------------------------------------------------------------------------*/
hParentQueryObject = ihParentQueryObject.

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

