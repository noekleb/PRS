&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS wWin 
/*------------------------------------------------------------------------

  File: 

  Description: from cntnrwin.w - ADM SmartWindow Template

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  History: New V9 Version - January 15, 1998
          
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AB.              */
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

DEF VAR iBatchNr AS INT NO-UNDO.
DEF VAR cErrFil AS CHAR NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartWindow
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

&Scoped-define ADM-SUPPORTED-LINKS Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME fMain

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-56 RECT-57 FI-FraDato FI-TilDato ~
FI-ButikkNr BtnOK BtnCancel 
&Scoped-Define DISPLAYED-OBJECTS FI-FraDato FI-TilDato FI-ButikkNr ~
FI-Status 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR wWin AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BtnCancel AUTO-END-KEY DEFAULT 
     LABEL "Avbryt" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON BtnOK AUTO-GO DEFAULT 
     LABEL "OK" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE VARIABLE FI-ButikkNr AS INTEGER FORMAT ">>>>>9":U INITIAL 0 
     LABEL "ButikkNr" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FI-FraDato AS DATE FORMAT "99/99/99":U 
     LABEL "Fra/til dato" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Status AS CHARACTER FORMAT "X(256)":U 
     LABEL "Status" 
     VIEW-AS FILL-IN NATIVE 
     SIZE 52 BY 1 NO-UNDO.

DEFINE VARIABLE FI-TilDato AS DATE FORMAT "99/99/99":U 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-56
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 67 BY 5.24.

DEFINE RECTANGLE RECT-57
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 62 BY 1.91.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
     FI-FraDato AT ROW 3.86 COL 15.2 COLON-ALIGNED
     FI-TilDato AT ROW 3.86 COL 29.2 COLON-ALIGNED NO-LABEL
     FI-ButikkNr AT ROW 4.95 COL 15.2 COLON-ALIGNED
     FI-Status AT ROW 6 COL 15.2 COLON-ALIGNED
     BtnOK AT ROW 8.14 COL 4
     BtnCancel AT ROW 8.14 COL 56
     "  Sletter salgsdata for periode" VIEW-AS TEXT
          SIZE 60 BY 1.43 AT ROW 1.71 COL 8
          BGCOLOR 9 FGCOLOR 15 FONT 12
     RECT-56 AT ROW 2.43 COL 4
     RECT-57 AT ROW 1.48 COL 7
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 73.4 BY 8.67
         DEFAULT-BUTTON BtnOK CANCEL-BUTTON BtnCancel.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartWindow
   Allow: Basic,Browse,DB-Fields,Query,Smart,Window
   Container Links: Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW wWin ASSIGN
         HIDDEN             = YES
         TITLE              = ""
         HEIGHT             = 8.67
         WIDTH              = 73.4
         MAX-HEIGHT         = 28.81
         MAX-WIDTH          = 146.2
         VIRTUAL-HEIGHT     = 28.81
         VIRTUAL-WIDTH      = 146.2
         RESIZE             = no
         SCROLL-BARS        = no
         STATUS-AREA        = yes
         BGCOLOR            = ?
         FGCOLOR            = ?
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB wWin 
/* ************************* Included-Libraries *********************** */

{src/adm2/containr.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW wWin
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME fMain
   FRAME-NAME                                                           */
/* SETTINGS FOR FILL-IN FI-Status IN FRAME fMain
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
THEN wWin:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON END-ERROR OF wWin
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-CLOSE OF wWin
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BtnCancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnCancel wWin
ON CHOOSE OF BtnCancel IN FRAME fMain /* Avbryt */
DO:
   APPLY "CLOSE":U TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BtnOK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnOK wWin
ON CHOOSE OF BtnOK IN FRAME fMain /* OK */
DO:
  DEF VAR bOk AS LOG NO-UNDO.

  ASSIGN
      bOk = NO
      .
  MESSAGE "Skal sletting av salgsdata for angitt periode startes?"
      VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO
      UPDATE bOk.
  IF bOk = TRUE THEN
      RUN SlettData.
  ELSE
      RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK wWin 


/* ***************************  Main Block  *************************** */

/* Include custom  Main Block code for SmartWindows. */
{src/adm2/windowmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects wWin  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI wWin  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
  THEN DELETE WIDGET wWin.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI wWin  _DEFAULT-ENABLE
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
  DISPLAY FI-FraDato FI-TilDato FI-ButikkNr FI-Status 
      WITH FRAME fMain IN WINDOW wWin.
  ENABLE RECT-56 RECT-57 FI-FraDato FI-TilDato FI-ButikkNr BtnOK BtnCancel 
      WITH FRAME fMain IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-fMain}
  VIEW wWin.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Errlogg wWin 
PROCEDURE Errlogg :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAMETER cTekst AS CHAR NO-UNDO.

ASSIGN
    cErrFil = "SlettSalgsdata.log".

OUTPUT TO VALUE(cErrFil) APPEND.

  PUT UNFORMATTED
      string(today) " " string(time,"HH:MM:SS") " " cTekst SKIP.

OUTPUT CLOSE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE exitObject wWin 
PROCEDURE exitObject :
/*------------------------------------------------------------------------------
  Purpose:  Window-specific override of this procedure which destroys 
            its contents and itself.
    Notes:  
------------------------------------------------------------------------------*/

  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initializeObject wWin 
PROCEDURE initializeObject :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  ASSIGN
      FI-FraDato = TODAY
      FI-TilDato = TODAY
      .

  RUN SUPER.

  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SlettData wWin 
PROCEDURE SlettData :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR piTransNr AS INT NO-UNDO.
  DEF VAR piSeqNr   AS INT NO-UNDO.
  DEF VAR dDato     AS DATE NO-UNDO.

  DEF BUFFER bDatasett  FOR DataSett.
  DEF BUFFER bTranslogg FOR Translogg.
  DEF BUFFER bBongHode  FOR BongHode.
  DEF BUFFER bArtBas    FOR Artbas.
  /* Batch for TransLogg */
  run batchlogg.w (program-name(1),
                   "Sletting av salgsdata " +
                   string(today) +
                   " " +
                   string(time,"HH:MM") +
                   " " +
                   userid("dictdb"),
                   output iBatchNr).

  DO WITH FRAME {&FRAME-NAME}:
      ASSIGN
          FI-FraDato
          FI-TilDato
          FI-ButikkNr
          .

      IF FI-FraDato = ? OR
         FI-TilDato = ? OR
         FI-ButikkNr = 0 OR 
         NOT can-find(Butiker WHERE Butiker.Butik = FI-ButikkNr) THEN
      DO:
          MESSAGE "Fra, tildato og butikknummer må angis."
              VIEW-AS ALERT-BOX INFO BUTTONS OK.
          RETURN NO-APPLY.
      END.

      /* Bonger. */
      ASSIGN
          FI-Status:SCREEN-VALUE = "Sletter datasett, bonger og filer.".
      DATASETT:
      FOR EACH DataSett NO-LOCK WHERE
          Datasett.ButikkNr = FI-ButikkNr AND
          Datasett.Dato >= FI-FraDato AND
          DataSett.Dato <= FI-TilDato:
          /* Bonger. */
          ASSIGN
              FI-Status:SCREEN-VALUE = "Sletter datasett, bonger og filer. " + 
              STRING(Datasett.ButikkNr) + " " + STRING(DataSEtt.DAto) + ".".

          STATUS DEFAULT "Sletter fillinjer...".
          RUN errlogg ("Sletter fillinjer...").
          FOR EACH Filer OF DataSett EXCLUSIVE-LOCK:
              FOR EACH Fillinjer OF Filer EXCLUSIVE-LOCK:
                  DELETE Fillinjer.
              END.
              DELETE Filer.
          END.

          STATUS DEFAULT "Sletter bonger...".
          RUN errlogg ("Sletter bonger...").
          /* Bonglinjer med tilhørende motposteres/poster slettes. */
          FOR EACH BongHode OF DataSett NO-LOCK:
              FOR EACH BongLinje NO-LOCK WHERE
                  BongLinje.B_Id = BongHode.B_Id:

                  /* Translogg - Salg          */
                  /* Translogg - Motpostering. */
                  IF BongLinje.TTId > 0 and
                     BongLinje.TTID < 12 AND /* Bare varetransaksjoner */
                     BongLinje.TransNr > 0 THEN
                  DO:
                      IF AVAILABLE bArtBas
                          THEN RELEASE bArtBas.
                      FIND bArtBas NO-LOCK WHERE
                          bArtBas.ArtikkelNr = dec(BongLinje.ArtikkelNr) NO-ERROR.

                      IF AVAILABLE bArtBas AND dec(BongLinje.ArtikkelNr) > 0 THEN
                      TRANSLOGG:
                      FOR EACH TransLogg EXCLUSIVE-LOCK WHERE
                          Translogg.ArtikkelNr  = bArtBas.ArtikkelNr AND
                          Translogg.Dato        = BongLinje.TransDato AND
                          Translogg.Tid         = BongLinje.TransTid AND
                          TransLogg.Butik       = BongLinje.ButikkNr AND
                          Translogg.TTID        = BongLinje.TTId:

                          IF TransLogg.BongId <> BongLinje.BongNr OR
                             TransLogg.BongLinjeNr <> BongLinje.LinjeNr OR
                             TransLogg.BatchNr     >= iBatchNr THEN 
                            NEXT TRANSLOGG.

                          /* Setter transaksjonsnummer  */
                          if piTransNr = 0 then
                            DO:
                              find last bTransLogg where
                                bTransLogg.Butik = BongLinje.ButikkNr
                                use-index TransLogg no-error.
                              if available bTransLogg then
                                ASSIGN
                                  piTransNr = bTransLogg.TransNr + 1
                                  piSeqNr   = bTransLogg.SeqNr   + 1
                                  .
                              else
                                ASSIGN
                                    piTransNr = 1
                                    piSeqNr   = 1
                                    .
                            END.
                          else
                            piTransNr = piTransNr + 1.

                          /* Oppretter TransLogg */
                          CREATE bTransLogg.
                          BUFFER-COPY TransLogg 
                            EXCEPT BatchNr TransNr SeqNr Postert PostertDato PostertTid Plukket
                            TO bTranslogg
                            assign 
                              bTransLogg.BatchNr      = iBatchNr
                              bTransLogg.TransNr      = piTransNr
                              bTransLogg.SeqNr        = piSeqNr
                              /* Alltid lagerjustering */
                              bTransLogg.Antall       = TransLogg.Antall * -1
                              bTransLogg.Postert      = FALSE
                              bTransLogg.PostertDato  = ?
                              bTransLogg.PostertTid   = 0
                              bTransLogg.Plukket      = TRUE
                              .

                          /* Slipper posten */
                          RELEASE bTranslogg.

                          /*DELETE TransLogg.*/
                      END. /* TRANSLOGG */
                      FOR EACH ReklamasjonsLinje EXCLUSIVE-LOCK WHERE
                              Reklamasjonslinje.Butik   = BongLinje.ButikkNr AND
                              Reklamasjonslinje.KassaNr = BongLinje.KasseNr  AND
                              Reklamasjonslinje.BongId  = BongLinje.BongNr   AND
                              Reklamasjonslinje.BongLinjeNr > 0              AND
                              Reklamasjonslinje.Dato    = BongLinje.TransDato:
                          DELETE ReklamasjonsLinje.
                      END.
                  END.
              END.
              FOR EACH BongLinje EXCLUSIVE-LOCK WHERE
                  BongLinje.B_Id = BongHode.B_Id:
                  DELETE BongLinje.
              END.
              DO TRANSACTION:
                  FIND bBongHode WHERE
                      RECID(bBongHode) = RECID(BongHode).
                  DELETE bBongHode.
              END.
          END.
          DO TRANSACTION:
              FIND bDataSett EXCLUSIVE-LOCK WHERE
                  RECID(bDataSett) = RECID(DataSett).
              DELETE bDataSett.
          END.
      END. /* DATASETT */

      STATUS DEFAULT "Sletter KupongTranslogg...".
      RUN errlogg ("Sletter KupongTransLogg...").
      DO TRANSACTION:
          FOR EACH KupongTransLogg EXCLUSIVE-LOCK WHERE
              KupongTransLogg.ButikkNr   = FI-ButikkNr AND 
              KupongTransLogg.SalgsDato >= FI-FraDato AND
              KupongTransLogg.SalgsDato <= FI-TilDato:
              DELETE KupongTransLogg.
          END.
      END.

      STATUS DEFAULT "Sletter BOKFØRINGSBILAG...".
      RUN errlogg ("Sletter BOKFØRINGSBILAG...").

      BOKFORINGSBILAG:
      DO:
          ASSIGN
              FI-Status:SCREEN-VALUE = "Sletter KASSERERBILAG"
              .
          STATUS DEFAULT "Sletter KassererBilag...".
          RUN errlogg ("Sletter KassererBilag...").
          FOR EACH KassererBilag EXCLUSIVE-LOCK WHERE
              KassererBilag.ButikkNr = FI-ButikkNr AND
              KassererBilag.Dato >= FI-FraDato AND
              KassererBilag.Dato <= FI-TilDato:
              DELETE KassererBilag.
          END.
          ASSIGN
              FI-Status:SCREEN-VALUE = "Sletter KASSERERDAG"
              .
          STATUS DEFAULT "Sletter KassererDag...".
          RUN errlogg ("Sletter KassererDag...").
          FOR EACH KassererDag EXCLUSIVE-LOCK WHERE
              KassererDag.ButikkNr = FI-ButikkNR AND
              KassererDag.Dato >= FI-FraDato AND
              KassererDag.Dato <= FI-TilDato:
              DELETE KassererDag.
          END.
          ASSIGN
              FI-Status:SCREEN-VALUE = "Sletter KASSERERDAG"
              .
          STATUS DEFAULT "Sletter KassererOppgj...".
          RUN errlogg ("Sletter KassererOppgj...").
          FOR EACH KassererOppgj EXCLUSIVE-LOCK WHERE
            KassererOppgj.ButikkNr = FI-ButikkNR AND
            KassererOppgj.Dato    >= FI-FraDato AND
            KassererOppgj.Dato    <= FI-TilDato:
            DELETE KassererOppgj.
          END.
          ASSIGN
              FI-Status:SCREEN-VALUE = "Sletter KASSERERKONTANT"
              .
          STATUS DEFAULT "Sletter KassererKontanter...".
          RUN errlogg ("Sletter KassererKontanter...").
          FOR EACH KassererKontanter EXCLUSIVE-LOCK WHERE
            KassererKontant.ButikkNR   = FI-ButikkNR AND
            KassererKontanter.Dato    >= FI-FraDato AND
            KassererKontanter.Dato    <= FI-TilDato:
            DELETE KassererKontanter.
          END.
          ASSIGN
              FI-Status:SCREEN-VALUE = "Sletter KASSERERVALUTA"
              .
          STATUS DEFAULT "Sletter KassererValuta...".
          RUN errlogg ("Sletter KassererValuta...").
          FOR EACH KassererValuta EXCLUSIVE-LOCK WHERE
            KassererValuta.ButikkNr = FI-ButikkNr AND
            KassererValuta.KasseNr  > 0 AND
            KassererValuta.Dato     >= FI-FraDato AND
            KassererValuta.Dato     <= FI-TilDato:
            DELETE KassererValuta.
          END.

          ASSIGN
              FI-Status:SCREEN-VALUE = "Sletter BOKFØRINGSBILAG"
              .
          STATUS DEFAULT "Sletter bokføringsbilag...".
          RUN errlogg ("Sletter bokføringsbilag...").
          FOR EACH Bokforingsbilag EXCLUSIVE-LOCK WHERE
              Bokforingsbilag.ButikkNr        = FI-ButikkNr AND
              Bokforingsbilag.Omsetningsdato >= FI-FraDato AND
              Bokforingsbilag.Omsetningsdato <= FI-TilDato:
              DELETE Bokforingsbilag.
          END.
      END. /* BOKFORINGSBILAG */
      ASSIGN
          FI-Status:SCREEN-VALUE = "Sletter AKTIVITETSRAPPORT"
          .
      STATUS DEFAULT "Sletter Akt_Rapp...".
      RUN errlogg ("Sletter Akt_Rapp...").
      DO dDato = FI-FraDato TO FI-TilDato:
          FOR EACH Akt_Rapp where
              Akt_Rapp.dato  = dDato and
              Akt_Rapp.Butik = FI-ButikkNr:
              DELETE Akt_Rapp.
          END.
      END.
      PAUSE 1 NO-MESSAGE.

      ASSIGN
          FI-Status:SCREEN-VALUE = "Sletter HOVEDGRUPPERAPPORT"
          .
      STATUS DEFAULT "Sletter Dags_Rap...".
      RUN errlogg ("Sletter Dags_Rap...").
      FOR EACH dags_rap where 
          Dags_Rap.Butik  = FI-ButikkNr AND
          dags_rap.dato  >= FI-FraDato AND
          Dags_Rap.DAto  <= FI-TilDato:
          DELETE Dags_Rap.
      END.
      PAUSE 1 NO-MESSAGE.

      ASSIGN
          FI-Status:SCREEN-VALUE = "Sletter KASSARAPPORT"
          .
      STATUS DEFAULT "Sletter Kas_Rap...".
      RUN errlogg ("Sletter Kas_Rap...").
      DO dDato = FI-FraDato TO FI-TilDato:
          FOR EACH Kas_Rap where 
              Kas_Rap.dato   = dDato AND
              Kas_Rap.Butikk  = FI-ButikkNr:
              DELETE Kas_Rap.
          END.
      END.
      ASSIGN
          FI-Status:SCREEN-VALUE = "Sletter KONTO"
          .
      STATUS DEFAULT "Sletter Konto...".
      RUN errlogg ("Sletter Konto...").
      DO dDato = FI-FraDato TO FI-TilDato:
          FOR EACH Konto where 
              Konto.dato   = dDato AND
              Konto.Butik  = FI-ButikkNr:
              DELETE Konto.
          END.
      END.
      PAUSE 1 NO-MESSAGE.

      ASSIGN
          FI-Status:SCREEN-VALUE = "Sletter KUNDESALG"
          .
      STATUS DEFAULT "Sletter KundeTrans...".
      RUN errlogg ("Sletter KundeTrans...IKKE AKTIV...").
      /*
      FOR EACH KundeTrans where 
          KundeTrans.Butik   = FI-ButikkNr AND
          KundeTrans.Dato   >= FI-FraDato AND
          KundeTrans.Dato   <= FI-TilDato: 
          DELETE KundeTrans.
      END.
      */
      ASSIGN
          FI-Status:SCREEN-VALUE = "Sletter KUNDEBETALING"
          .
      STATUS DEFAULT "Sletter KundeBetTrans...".
      RUN errlogg ("Sletter KundeBetTrans...IKKE AKTIV...").
      /*
      FOR EACH KundeBetTrans where 
          KundeBetTrans.Butik   = FI-ButikkNr AND
          KundeBetTrans.Dato   >= FI-FraDato AND
          KundeBetTrans.Dato   <= FI-TilDato AND
          KundeBetTrans.BongId > 0: /* Tar ikke med innbet. opprettet fra bakrom */
          DELETE KundeBetTrans.
      END.
      */
      PAUSE 1 NO-MESSAGE.
   
      ASSIGN
          FI-Status:SCREEN-VALUE = "Sletter MEDLEMSSALG"
          .
      STATUS DEFAULT "Sletter MEdTrans...".
      RUN errlogg ("Sletter MedTrans...IKKE AKTIV...").
      /*
      FOR EACH MedTrans where 
          MEdTrans.Butik   = FI-ButikkNr AND
          MedTrans.Dato   >= FI-FraDato AND
          MedTrans.Dato   <= FI-TilDato /*AND
          MedTrans.BongId > 0*/:
          DELETE MedTrans.
      END.
      */
      PAUSE 1 NO-MESSAGE.
      
      ASSIGN
          FI-Status:SCREEN-VALUE = "Sletter MEDLEMBETALING"
          .
      STATUS DEFAULT "Sletter MedlemBetTrans...".
      RUN errlogg ("Sletter MedlemBetTrans......IKKE AKTIVT...").
      /*
      FOR EACH MedlemBetTrans where 
          MedlemBetTrans.Butik   = FI-ButikkNr AND
          MedlemBetTrans.Dato   >= FI-FraDato AND
          MedlemBetTrans.Dato   <= FI-TilDato AND
          MedlemBetTrans.BongId > 0: /* Tar ikke med innbet. opprettet fra bakrom */
          DELETE MedlemBetTrans.
      END.
      */
      PAUSE 1 NO-MESSAGE.

      ASSIGN
          FI-Status:SCREEN-VALUE = "Sletter KORT_SPES"
          .
      STATUS DEFAULT "Sletter Kort_Spes...".
      RUN errlogg ("Sletter Kort_Spes...").
      DO dDato = FI-FraDato TO  FI-TilDato: 
          FOR EACH Kort_Spes where
              Kort_Spes.Dato    = dDato AND
              Kort_Spes.Butik   = FI-ButikkNr:
              DELETE Kort_Spes.
          END.
      END.
      PAUSE 1 NO-MESSAGE.

      STATUS DEFAULT "Sletter NonSale...".
      RUN errlogg ("Sletter NonSale...").
      DO dDato = FI-FraDato TO  FI-TilDato:
          FOR EACH Kasse NO-LOCK WHERE Kasse.ButikkNr = FI-ButikkNr:
              FOR EACH Non_Sale_Spes where
                  Non_Sale_Spes.Butik   = FI-ButikkNr AND 
                  Non_Sale_Spes.Kasse   = Kasse.KasseNr AND
                  Non_Sale_Spes.Dato    = dDato:
                  DELETE Non_Sale_Spes.
              END.
          END.
      END.
      PAUSE 1 NO-MESSAGE.
      
      ASSIGN
          FI-Status:SCREEN-VALUE = ""
          .
  END.

  /* Flagger batchen klar for oppdatering. */
  run batchstatus.p (iBatchNr, 2).

  STATUS DEFAULT "Sletting ferdig.".
  RUN errlogg ("Sletting ferdig.").
  MESSAGE "Data er slettet for perioden " 
          FI-FraDato "til" FI-TilDato " FOR butikk " FI-ButikkNr "."
  VIEW-AS ALERT-BOX INFO BUTTONS OK.

  APPLY "choose" TO BtnCancel.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

