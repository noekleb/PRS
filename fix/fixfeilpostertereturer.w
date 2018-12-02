&ANALYZE-SUSPEND _VERSION-NUMBER AB_v9r12 GUI ADM2
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

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartWindow
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

&Scoped-define ADM-SUPPORTED-LINKS Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME fMain

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS FI-FraDato FI-TilDato BtnDone BtnOK RECT-56 
&Scoped-Define DISPLAYED-OBJECTS FI-FraDato FI-TilDato FI-Status 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR wWin AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BtnDone DEFAULT 
     LABEL "&Avslutt" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON BtnOK AUTO-GO DEFAULT 
     LABEL "Start korreksjon" 
     SIZE 22 BY 1.14
     BGCOLOR 8 .

DEFINE VARIABLE FI-FraDato AS DATE FORMAT "99/99/99":U 
     LABEL "Fra dato" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Status AS CHARACTER FORMAT "X(256)":U 
     LABEL "Status" 
     VIEW-AS FILL-IN NATIVE 
     SIZE 52 BY 1 NO-UNDO.

DEFINE VARIABLE FI-TilDato AS DATE FORMAT "99/99/99":U 
     LABEL "Til dato" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-56
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 67 BY 5.24.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
     FI-FraDato AT ROW 3.86 COL 14 COLON-ALIGNED
     FI-TilDato AT ROW 4.81 COL 14 COLON-ALIGNED
     FI-Status AT ROW 6 COL 14 COLON-ALIGNED
     BtnDone AT ROW 8.14 COL 4
     BtnOK AT ROW 8.14 COL 49
     RECT-56 AT ROW 2.43 COL 4
     "  Fiks feilposterte returer" VIEW-AS TEXT
          SIZE 42 BY 1.43 AT ROW 1.71 COL 10
          BGCOLOR 12 FGCOLOR 9 FONT 12
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 73.4 BY 8.48
         DEFAULT-BUTTON BtnDone.


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
         TITLE              = "Fiks returer i dagsrapportene"
         HEIGHT             = 8.48
         WIDTH              = 73.4
         MAX-HEIGHT         = 28.81
         MAX-WIDTH          = 146.2
         VIRTUAL-HEIGHT     = 28.81
         VIRTUAL-WIDTH      = 146.2
         RESIZE             = no
         SCROLL-BARS        = no
         STATUS-AREA        = no
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
                                                                        */
/* SETTINGS FOR FILL-IN FI-Status IN FRAME fMain
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
THEN wWin:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON END-ERROR OF wWin /* Fiks returer i dagsrapportene */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-CLOSE OF wWin /* Fiks returer i dagsrapportene */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BtnDone
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnDone wWin
ON CHOOSE OF BtnDone IN FRAME fMain /* Avslutt */
DO:
  &IF "{&PROCEDURE-TYPE}" EQ "SmartPanel" &THEN
    &IF "{&ADM-VERSION}" EQ "ADM1.1" &THEN
      RUN dispatch IN THIS-PROCEDURE ('exit').
    &ELSE
      RUN exitObject.
    &ENDIF
  &ELSE
      APPLY "CLOSE":U TO THIS-PROCEDURE.
  &ENDIF
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BtnOK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnOK wWin
ON CHOOSE OF BtnOK IN FRAME fMain /* Start korreksjon */
DO:
  RUN FiksData.
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
  DISPLAY FI-FraDato FI-TilDato FI-Status 
      WITH FRAME fMain IN WINDOW wWin.
  ENABLE FI-FraDato FI-TilDato BtnDone BtnOK RECT-56 
      WITH FRAME fMain IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-fMain}
  VIEW wWin.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE FiksData wWin 
PROCEDURE FiksData :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DO WITH FRAME {&FRAME-NAME}:
      ASSIGN
          FI-FraDato
          FI-TilDato
          .

      ASSIGN
          FI-Status:SCREEN-VALUE = "Korrigerer feilposteringer i dagsrapporten"
          .
      
      ASSIGN
          FI-Status:SCREEN-VALUE = "Korrigerer kassarapport med returbong."
          .
      PAUSE 1 NO-MESSAGE.
      FOR EACH BongHode NO-LOCK WHERE
          BongHode.Dato >= FI-FraDato AND
          BongHode.Dato <= FI-TilDato AND
          CAN-FIND(FIRST BongLinje OF BongHode WHERE
                   CAN-DO("10",STRING(BongLinje.TTId,"99"))):

          RUN KorrKassRapp.
      END.

      ASSIGN
          FI-Status:SCREEN-VALUE = ""
          .
  END.

  MESSAGE "Data er korrigert" SKIP
  VIEW-AS ALERT-BOX INFO BUTTONS OK.

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
  APPLY "ENTRY":U TO FI-FraDato IN FRAME {&FRAME-NAME}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE KorrKassRapp wWin 
PROCEDURE KorrKassRapp :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

/* Lokale variabler */
def var plSum      as dec  no-undo.
def var piLoop     as int  no-undo.
DEF VAR pcPOS      AS CHAR NO-UNDO.

DEF BUFFER bBongLinje FOR BongLinje.

/* Henter kassarapporten og z_nummer. */
HENT:
do:
    /* Henter kassarapporten. */
    find FIRST kas_rap where 
        kas_rap.dato             = BongHode.Dato     and
                kas_rap.butikk   = BongHode.ButikkNr and
                kas_rap.kasse    = BongHode.KasseNr
                exclusive-lock no-error.

    /* Finnes den ikke, er det ingenting å korrigere. */
    if not available kas_rap then
        RETURN.
end. /* HENT */

/* Leser kvittering i workfile. */
LES:
for each BongLinje OF BongHode NO-LOCK WHERE 
    BongLinje.Makulert = FALSE:
    ASSIGN
        pcPOS = SUBSTRING(BongLinje.OriginalData,21,2)
        .

    /* Postering av betalingstransaksjoner. */
    POSTER:
    do:
        /* Kontant.                                                        */
        /* Se notat i toppen av filen for transkod 20, rabatt p} subtotal. */
        if int(pcPOS) =  4 then
        do:
            kas_rap.kontant = kas_rap.kontant - (BongLinje.LinjeSum).
        end.

        /* Sjekk */
        if int(pcPOS) = 5 then
           kas_rap.sjekk      =   kas_rap.sjekk - BongLinje.LinjeSum.

        /* Kort */
        if int(pcPOS) = 6 then
           kas_rap.kort       =   kas_rap.kort - BongLinje.LinjeSum.

        /* Tilgode som betalingsmiddel. */
        if int(pcPOS) = 7 then
            kas_rap.tilgode    =   kas_rap.tilgode - BongLinje.LinjeSum.

        /* Konto.                                                */
        /* Det forutsettes at det kun kommer en 08 trans p} en   */
        /* kvittering. Det forutsettes videre at alle artikkler  */
        /* som er solgt p} kvitteringen, skal posteres p} konto. */
        if int(pcPOS) = 8 then
        do:
            kas_rap.kredit = kas_rap.kredit + BongLinje.LinjeSum.

            /* Legger alle salgstransaksjoner inn p} konto */
            for each bBongLinje OF BongHode where 
                bBongLinje.Makulert = FALSE AND
            can-do("00,01,02,17",SUBSTRING(bBongLinje.OriginalData,21,2)) no-lock:
                FIND FIRST Konto EXCLUSIVE-LOCK WHERE
                    konto.butikk      = bBongLinje.ButikkNr AND
                    konto.kasse       = bBongLinje.kassenr  AND
                    konto.dato        = bBongLinje.dato     AND
                    konto.kontonummer = int(BongHode.kundenr)    AND
                    konto.Vg          = bBongLinje.VareGr   AND
                    konto.lopnr       = bBongLinje.lopenr   AND
                    konto.storl       = bBongLinje.Storrelse AND
                    konto.antal       = bBongLinje.Antall    AND
                    konto.kvitto      = bBongLinje.BongNr NO-ERROR.
                IF AVAILABLE Konto THEN
                    DELETE Konto.

            end.
        end.

        /* Veksel */
        if int(pcPOS) = 9 then
           kas_rap.kontant    =   kas_rap.kontant + BongLinje.LinjeSum.

        /* Kupong 1 (Gavekort) */
        if int(pcPOS) = 12 then
           kas_rap.kupong1    = kas_rap.kupong1 - BongLinje.LinjeSum.

        /* Kupong 2 */
        if int(pcPOS) = 13 then
           kas_rap.kupong2    = kas_rap.kupong2 - BongLinje.LinjeSum.

    end. /* POSTER */
end. /* LES */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PosterReturer wWin 
PROCEDURE PosterReturer :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

/* Lokale variabler */
def var plSum      as dec  no-undo.
def var piLoop     as int  no-undo.
DEF VAR pcPOS      AS CHAR NO-UNDO.

DEF BUFFER bBongLinje FOR BongLinje.

/* Henter kassarapporten og z_nummer. */
HENT:
do:
    /* Henter kassarapporten. */
    find FIRST kas_rap where 
        kas_rap.dato             = DataSett.Dato     and
                kas_rap.butikk   = DataSett.ButikkNr and
                kas_rap.kasse    = DataSett.KasseNr
                exclusive-lock no-error.

    /* Skaper den hvis den ikke finnes. */
    if not available kas_rap then
        RETURN.
end. /* HENT */

/* Leser kvittering i workfile. */
LES:
for each BongLinje OF BongHode NO-LOCK WHERE 
    BongLinje.Makulert = FALSE:
    ASSIGN
        pcPOS = SUBSTRING(BongLinje.OriginalData,21,2)
        .

    /* Postering av betalingstransaksjoner. */
    POSTER:
    do:
        /* Kontant.                                                        */
        /* Se notat i toppen av filen for transkod 20, rabatt p} subtotal. */
        if int(pcPOS) =  4 then
        do:
            kas_rap.kontant = kas_rap.kontant + (BongLinje.LinjeSum).
        end.

        /* Sjekk */
        if int(pcPOS) = 5 then
           kas_rap.sjekk      =   kas_rap.sjekk + BongLinje.LinjeSum.

        /* Kort */
        if int(pcPOS) = 6 then
           kas_rap.kort       =   kas_rap.kort + BongLinje.LinjeSum.

        /* Tilgode som betalingsmiddel. */
        if int(pcPOS) = 7 then
            kas_rap.tilgode    =   kas_rap.tilgode + BongLinje.LinjeSum.

        /* Konto.                                                */
        /* Det forutsettes at det kun kommer en 08 trans p} en   */
        /* kvittering. Det forutsettes videre at alle artikkler  */
        /* som er solgt p} kvitteringen, skal posteres p} konto. */
        if int(pcPOS) = 8 then
        do:
            kas_rap.kredit = kas_rap.kredit + BongLinje.LinjeSum.

            /* Legger alle salgstransaksjoner inn p} konto */
            for each bBongLinje OF BongHode where 
                bBongLinje.Makulert = FALSE AND
            can-do("00,01,02,17",SUBSTRING(bBongLinje.OriginalData,21,2)) no-lock:
                /* Skaper konto post. */
                create konto.

                /* Setter opp index. */
                assign konto.butikk      = bBongLinje.ButikkNr
                       konto.kasse       = bBongLinje.kassenr
                       konto.dato        = bBongLinje.dato
                       konto.kontonummer = BongHode.kundenr.

                /* Legger over informasjonen. */
                assign konto.Vg          = bBongLinje.VareGr
                       konto.lopnr       = bBongLinje.lopenr
                       konto.storl       = bBongLinje.Storrelse
                       konto.pris        = bBongLinje.LinjeSum - bBongLinje.LinjeRab - bBongLinje.SubtotalRab
                       konto.antal       = bBongLinje.Antall
                       konto.kvitto      = bBongLinje.BongNr.
                /*
                /* Snur fortegn */
                IF can-do("00,01,02,17",SUBSTRING(bBongLinje.OriginalData,21,2)) THEN
                  ASSIGN
                    konto.pris  = Konto.Pris  * -1
                    konto.antal = Konto.Antal * -1
                    .
                */
            end.
        end.

        /* Veksel */
        if int(pcPOS) = 9 then
           kas_rap.kontant    =   kas_rap.kontant - BongLinje.LinjeSum.

        /* Inn og utbetalinger.                                        */
        /* Disse skal posteres for seg selv i tillegg.                 */
        /* Dette gj|res for } kunne finne netto salg og utg}ende moms. */
        /* ved kontering av kassarapporten.                            */
        if int(pcPOS) = 10 or
           int(pcPOS) = 11 then
        do:
            if int(pcPOS) = 11 then
               kas_rap.kont_ut  = kas_rap.kont_ut + (BongLinje.LinjeSum).
            else
               kas_rap.kont_inn = kas_rap.kont_inn  + (BongLinje.LinjeSum).
        end.

        /* Kupong 1 (Gavekort) */
        if int(pcPOS) = 12 then
           kas_rap.kupong1    = kas_rap.kupong1 + BongLinje.LinjeSum.

        /* Kupong 2 */
        if int(pcPOS) = 13 then
           kas_rap.kupong2    = kas_rap.kupong2 + BongLinje.LinjeSum.



    end. /* POSTER */
end. /* LES */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SnuForTegn wWin 
PROCEDURE SnuForTegn :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR plSum AS DEC NO-UNDO.

  ASSIGN
      plSum = 0
      .
  /* Nullstiller fortegn */
  FOR EACH BongLinje OF BongHode:
      IF BongLinje.Antall < 0 THEN
          BongLinje.Antall = bongLinje.Antall * -1.
      IF BongLinje.LinjeSum < 0 THEN
          BongLinje.LinjeSum = bongLinje.LinjeSum * -1.
  END.

  /* Snur fortegn på de linjene som skal være negative.                        */                                                        
  /* Bongen lagres med fortegn. På varelinjer settes fortegnet i beløpsfeltet. */
  /* På betalingstransaksjoner, settes det direkte i beløpsfeltet.             */
  FOR EACH BongLinje OF BongHode EXCLUSIVE-LOCK:
      /* Snur fortegn på Reklamasjon, Retur og Makulering. */
      /* NB: 04 trans blir slettet. Den er bare til info.  */
      IF CAN-DO("03,10,12",STRING(BongLinje.TTId,"99")) THEN
          ASSIGN
          BongLinje.Antall = BongLinje.Antall * -1
          .
      /* Snur ALLTID fortegn på Vekseltransaksjonen */
      IF CAN-DO("70",STRING(BongLinje.TTId,"99")) THEN
          ASSIGN
          BongLinje.LinjeSum = BongLinje.LinjeSum * -1
          .
      /* Sumerer opp varelinjer + veksel for å finne beløp som skal dekkes */
      /* av de resterende betalingstransaksjonene.                         */
      IF CAN-DO("01,03,10,12",STRING(BongLinje.TTId,"99")) THEN
          plSum = plSum + (IF BongLinje.Antall < 0
                             THEN (BongLinje.LinjeSum * -1)
                             ELSE BongLinje.LinjeSum)
          .
  END.

  /* Hvis summen er større eller lik 0, skal ingenting gjøres. Bongen er korrekt.     */
  /* Er summen mindre enn 0, skal fortegnet snus på de andre betalingstransaksjonene. */
  IF plSum < 0 THEN
  FOR EACH BongLinje OF BongHode EXCLUSIVE-LOCK:
      /* Snur fortegn på betalingstransaksjonene. */
      IF CAN-DO("50,52,54,56,65,66,71",STRING(BongLinje.TTId,"99")) THEN
          ASSIGN
          BongLinje.LinjeSum = BongLinje.LinjeSum * -1
          .
  END.
  ELSE
  FOR EACH BongLinje OF BongHode EXCLUSIVE-LOCK:
      IF CAN-DO("63",STRING(BongLinje.TTId,"99")) THEN
          ASSIGN
          BongLinje.LinjeSum = BongLinje.LinjeSum * -1
          .

  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

