&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Manedsrapport
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Manedsrapport 
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

DEF VAR cButListe   AS CHAR NO-UNDO.
DEF VAR wExcEkstent AS CHAR NO-UNDO.
DEF VAR wKriterier  AS CHAR NO-UNDO.
DEF VAR wSkoTex     AS CHAR NO-UNDO.
DEF VAR wKunde      AS CHAR NO-UNDO.
DEF VAR iCL         AS INT  NO-UNDO.

{manedsrapport_tmptabell.i &NEW = "NEW" &SHARED = "SHARED"}

DEF BUFFER tmptotManedsrap FOR tmpManedsrap.

DEF STREAM sExportFile.
{runlib.i}
{methodexcel.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-55 B-Butikker FI-FraDato FI-TilDato ~
B-Excel B-Exit Btn_Help BUTTON-SokDato2 BUTTON-SokDato 
&Scoped-Define DISPLAYED-OBJECTS FI-ButListe FI-FraDato FI-TilDato 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Manedsrapport AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON B-Butikker 
     IMAGE-UP FILE "icon/e-sokpr.ico":U
     LABEL "&Merk..." 
     SIZE 4.4 BY 1 TOOLTIP "Valg av butikker".

DEFINE BUTTON B-Excel 
     LABEL "Start eksport til Excel..." 
     SIZE 42 BY 1.14 TOOLTIP "Eksporter alle eller merkede tellelinjer til X-Print. Alt-P.".

DEFINE BUTTON B-Exit 
     IMAGE-UP FILE "icon/e-exit":U NO-FOCUS FLAT-BUTTON
     LABEL "Button 10" 
     SIZE 4.6 BY 1.14 TOOLTIP "Avslutt".

DEFINE BUTTON Btn_Help DEFAULT 
     IMAGE-UP FILE "icon/e-help":U NO-FOCUS FLAT-BUTTON
     LABEL "&Hjelp" 
     SIZE 4.6 BY 1.14 TOOLTIP "Hjelp"
     BGCOLOR 8 .

DEFINE BUTTON BUTTON-SokDato 
     IMAGE-UP FILE "icon\e-sokpr":U NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON BUTTON-SokDato2 
     IMAGE-UP FILE "icon\e-sokpr":U NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE VARIABLE FI-ButListe AS CHARACTER FORMAT "X(256)":U 
     LABEL "Butikker" 
     VIEW-AS FILL-IN 
     SIZE 37.6 BY 1 NO-UNDO.

DEFINE VARIABLE FI-FraDato AS DATE FORMAT "99/99/99":U 
     LABEL "Fra - Til Dato" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE FI-TilDato AS DATE FORMAT "99/99/99":U 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-55
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 77 BY .14.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     FI-ButListe AT ROW 4.57 COL 16 COLON-ALIGNED HELP
          "Butikker som skal være med i rapporten"
     B-Butikker AT ROW 4.57 COL 55.8 HELP
          "Valg av butikker"
     FI-FraDato AT ROW 6.24 COL 16 COLON-ALIGNED
     FI-TilDato AT ROW 6.24 COL 37.6 COLON-ALIGNED NO-LABEL
     B-Excel AT ROW 8.14 COL 18
     B-Exit AT ROW 1.24 COL 73
     Btn_Help AT ROW 1.24 COL 68
     BUTTON-SokDato2 AT ROW 6.24 COL 55.8
     BUTTON-SokDato AT ROW 6.24 COL 34.2
     RECT-55 AT ROW 2.43 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 77.2 BY 9.33.


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
  CREATE WINDOW C-Manedsrapport ASSIGN
         HIDDEN             = YES
         TITLE              = "Månedsrapport"
         HEIGHT             = 9.38
         WIDTH              = 77.2
         MAX-HEIGHT         = 16
         MAX-WIDTH          = 80
         VIRTUAL-HEIGHT     = 16
         VIRTUAL-WIDTH      = 80
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = yes
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
/* SETTINGS FOR WINDOW C-Manedsrapport
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */
/* SETTINGS FOR FILL-IN FI-ButListe IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Manedsrapport)
THEN C-Manedsrapport:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Manedsrapport
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Manedsrapport C-Manedsrapport
ON END-ERROR OF C-Manedsrapport /* Månedsrapport */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Manedsrapport C-Manedsrapport
ON WINDOW-CLOSE OF C-Manedsrapport /* Månedsrapport */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Butikker
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Butikker C-Manedsrapport
ON CHOOSE OF B-Butikker IN FRAME DEFAULT-FRAME /* Merk... */
DO:
  
 DO WITH FRAME {&FRAME-NAME}:
     ASSIGN
       cButListe = FI-ButListe.
     IF cButListe = "[Alle]" THEN
     DO:
         ASSIGN
             cButListe = "".
         FOR EACH Butiker NO-LOCK WHERE
              CAN-FIND(FIRST Kasse WHERE
                       Kasse.Butik = Butiker.Butik AND
                       Kasse.Aktiv = TRUE):
           ASSIGN
             cButListe = cButListe +
                           (IF cButListe = ""
                               THEN ""
                               ELSE ",") +
                            STRING(Butiker.Butik).
         END.
     END.

     RUN d-tagbutikerBgrp.w (INPUT-OUTPUT cButListe).
     IF RETURN-VALUE = "Avbryt" THEN
           RETURN NO-APPLY.

     IF cButListe = "" THEN
         ASSIGN
         cButListe = FI-butListe:SCREEN-VALUE.
     ASSIGN
         FI-ButListe:SCREEN-VALUE = IF cButListe = ""
                                      THEN FI-ButListe:SCREEN-VALUE
                                      ELSE cButListe.
 END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Excel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Excel C-Manedsrapport
ON CHOOSE OF B-Excel IN FRAME DEFAULT-FRAME /* Start eksport til Excel... */
DO:
    DO WITH FRAME Default-frame:
      ASSIGN INPUT FI-FraDato
             INPUT FI-TilDato
             FI-ButListe
             wKriterier = "Butikker: " + FI-ButListe + " Periode: " + STRING(FI-FraDato) + " - " + STRING(FI-TilDato)
             .
      RUN ValiderKrit.
      IF RETURN-VALUE = "AVBRYT" THEN
        RETURN NO-APPLY.

      RUN manedsrapport_bygg_tmptabell.p (FI-ButListe:SCREEN-VALUE,FI-FraDato:SCREEN-VALUE, FI-TilDato:SCREEN-VALUE).
      IF RETURN-VALUE = "AVBRYT" THEN
        RETURN NO-APPLY.
    END.

    RUN EksporterData.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Exit
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Exit C-Manedsrapport
ON CHOOSE OF B-Exit IN FRAME DEFAULT-FRAME /* Button 10 */
DO:
  APPLY "close":U TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Help
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Help C-Manedsrapport
ON CHOOSE OF Btn_Help IN FRAME DEFAULT-FRAME /* Hjelp */
OR HELP OF FRAME {&FRAME-NAME}
DO: /* Call Help Function (or a simple message). */
  /*{winhlp.i}*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-SokDato
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-SokDato C-Manedsrapport
ON CHOOSE OF BUTTON-SokDato IN FRAME DEFAULT-FRAME /* ... */
OR F10 OF FI-FraDato
DO:

  DEF VAR wTittel AS CHAR NO-UNDO.
  ASSIGN FI-FraDato = DATE(FI-FraDato:screen-value IN FRAME {&FRAME-NAME}).

  DO WITH FRAME {&FRAME-NAME}:  

  wTittel = "Datosøk".

  /* Start søkeprogram */
  {soek.i
    &Felt        = FI-FraDato
    &Program     = kalender.w
    &Frame       = {&FRAME-NAME}
    &ExtraParam  = "input wTittel"
  }   
  
  END. /* FRAME */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-SokDato2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-SokDato2 C-Manedsrapport
ON CHOOSE OF BUTTON-SokDato2 IN FRAME DEFAULT-FRAME /* ... */
OR F10 OF FI-TilDato
DO:

  DEF VAR wTittel AS CHAR NO-UNDO.
  ASSIGN FI-TilDato = DATE(FI-FraDato:screen-value IN FRAME {&FRAME-NAME}).

  DO WITH FRAME {&FRAME-NAME}:  

  wTittel = "Datosøk".

  /* Start søkeprogram */
  {soek.i
    &Felt        = FI-TilDato
    &Program     = kalender.w
    &Frame       = {&FRAME-NAME}
    &ExtraParam  = "input wTittel"
  }   
  
  END. /* FRAME */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Manedsrapport 


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

{syspara.i 5 1 1 iCL INT}
{syspara.i 1 1 100 wKunde}
{syspara.i 1 1 101 wSkoTex}
{syspara.i 1 4 1 wExcEkstent}
wExcEkstent = IF wExcEkstent = "" THEN "sdv" ELSE wExcEkstent.   

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

  RUN enable_UI.

  ASSIGN
      FI-ButListe:SCREEN-VALUE = STRING(iCL).
  APPLY "ENTRY" TO FI-FraDato.

  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI C-Manedsrapport  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Manedsrapport)
  THEN DELETE WIDGET C-Manedsrapport.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE EksporterData C-Manedsrapport 
PROCEDURE EksporterData :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR wFileName  AS CHAR NO-UNDO.
  DEF VAR wAntPoster AS INT  NO-UNDO.
  DEF VAR cMvaTekst  AS CHAR NO-UNDO.

  FOR EACH Moms NO-LOCK:
      ASSIGN
          cMvaTekst = cMvaTekst + 
                      (IF cMvaTekst <> '' THEN '|' ELSE '') + 
                      Moms.Beskrivelse.
  END.
  cMvaTekst = cMvaTekst + "|||".

  {sww.i}
  
  /* Finner temporært filnavn. */
  IF VALID-HANDLE(wLibHandle) THEN
    RUN GetTempFileName IN wLibHandle ("Manedsrapport", wExcEkstent, OUTPUT wFileName). 
  
  /* Åpner stream */
  OUTPUT STREAM sExportFile TO VALUE(wFileName) NO-ECHO.
  
  /* Legger ut overskrifter. */
  STATUS DEFAULT "Eksporterer data...".
  EXPORT STREAM sExportFile DELIMITER ";"
    "MÅNEDSRAPPORT"
    SKIP.                                 
  EXPORT STREAM sExportFile DELIMITER ";"
    /* A  */ "Butikk"          
    /* B  */ ""              
    /* C  */ "Bokf."                 
    /* D  */ "Opptalt"         
    /* E  */ "Bank"               
    /* F  */ "Bank"          
    /* G  */ ""               
    /* H  */ ""              
    /* I  */ ""                 
    /* J  */ ""               
    /* K  */ "Senter"             
    /* L  */ "Diverse"       
    /* M  */ "Tilbake bet."   
    /* N  */ ""              
    /* O  */ "Tilgode"      
    /* P  */ "Tilgode"       
    /* Q  */ "Gavekort"     
    /* R  */ "Gavekort"             
    /* S  */ "Tilgode"              
    /* T  */ "Gavekort"      
    /* U  */ "Innbetalt"       
    /* V  */ "Sum"           
    /* W  */ "Omsetning" 
    /* X  */ ""          
    /* Y  */ ""   
    /* Z  */ (IF ENTRY(1,cMvaTekst,'|') = '' THEN "Oms.MvaGrp1" ELSE ENTRY(1,cMvaTekst,'|'))  
    /* AA */ (IF ENTRY(2,cMvaTekst,'|') = '' THEN "Oms.MvaGrp2" ELSE ENTRY(2,cMvaTekst,'|'))     
    /* AB */ (IF ENTRY(3,cMvaTekst,'|') = '' THEN "Oms.MvaGrpDiv" ELSE ENTRY(3,cMvaTekst,'|'))     
    /* AC */ "Omsetning"   

    SKIP.                                 
  EXPORT STREAM sExportFile DELIMITER ";"
    /* A  */ "nr."          
    /* B  */ "Dato"              
    /* C  */ "nr."                 
    /* D  */ "Kasse"        
    /* E  */ "Pose"               
    /* F  */ "Kort"              
    /* G  */ "Visa"                   
    /* H  */ "Eurocard"          
    /* I  */ "Amex"                   
    /* J  */ "Diners"           
    /* K  */ "gavekort"        
    /* L  */ "Kort"       
    /* M  */ "Kunde"                  
    /* N  */ "Beskrivelse"      
    /* O  */ "brukt egne"       
    /* P  */ "brukt andre"     
    /* Q  */ "brukt egne"      
    /* R  */ "brukt andre"     
    /* S  */ "ut"             
    /* T  */ "ut"                   
    /* U  */ "Kunde"      
    /* V  */ "inn butikk"        
    /* W  */ "Ekskl.kred"             
    /* X  */ "Diff"       
    /* Y  */ "Kreditsalg" 
    /* AA */ "Inkl.kred"   
    /* AB */ "Inkl.kred"   
    /* AC */ "Inkl.kred"   
    /* AD */ "Inkl.kred"   
    SKIP.                                 
                                  
  ASSIGN
      wAntPoster = 3.
  /* Eksporterer data */
  EKSPORT:
  FOR EACH tmpManedsrap WHERE
      tmpManedsrap.ButikkNr > 0
      BREAK BY tmpManedsrap.ButikkNr
            BY tmpManedsrap.Dato:
      
      ASSIGN
          wAntPoster = wAntPoster + 1.
      EXPORT STREAM sExportFile DELIMITER ";"
          /* A  */ tmpManedsrap.ButikkNr          
          /* B  */ tmpManedsrap.Dato              
          /* C  */ tmpManedsrap.BokfNr                 
          /* D  */ tmpManedsrap.Kontant               
          /* E  */ tmpManedsrap.BankPose               
          /* F  */ tmpManedsrap.BankKort               
          /* G  */ tmpManedsrap.Visa              
          /* H  */ tmpManedsrap.Eurocard               
          /* I  */ tmpManedsrap.Amex              
          /* J  */ tmpManedsrap.Diners                 
          /* K  */ tmpManedsrap.SenterGavekort         
          /* L  */ tmpManedsrap.DiverseKort            
          /* M  */ tmpManedsrap.KontKjopKasse     
          /* N  */ tmpManedsrap.Beskrivelse            
          /* O  */ tmpManedsrap.TilgodeBruktEgne       
          /* P  */ tmpManedsrap.TilgodeBruktAndre      
          /* Q  */ tmpManedsrap.GavekortBruktEgne      
          /* R  */ tmpManedsrap.GavekortBruktAndre     
          /* S  */ tmpManedsrap.TilgodeUt              
          /* T  */ tmpManedsrap.GavekortUt             
          /* U  */ tmpManedsrap.InnbetaltKunde      
          /* V  */ tmpManedsrap.SumInnbutikk           
          /* W  */ tmpManedsrap.OmsetningEksKred       
          /* X  */ tmpManedsrap.DiffKasse        
          /* Y  */ tmpManedsrap.Kreditsalg       
          /* Z */ tmpManedsrap.OmsetningMvaGrp1   
          /* AA */ tmpManedsrap.OmsetningMvaGrp2   
          /* AB */ tmpManedsrap.OmsetningMvaGrpDiv   
          /* AC */ (tmpManedsrap.OmsetningMvaGrp1 + 
                    tmpManedsrap.OmsetningMvaGrp2 +  
                    tmpManedsrap.OmsetningMvaGrpDiv)
          .             
  END.

  FIND FIRST tmptotManedsrap NO-ERROR.
  IF AVAILABLE tmptotManedsrap THEN
  DO:
      ASSIGN
          wAntPoster = wAntPoster + 1.
      EXPORT STREAM sExportFile DELIMITER ";"
          /* A  */ "Total:"          
          /* B  */ " "              
          /* C  */ " "                 
          /* D  */ tmptotManedsrap.Kontant               
          /* E  */ tmptotManedsrap.BankPose               
          /* F  */ tmptotManedsrap.BankKort               
          /* G  */ tmptotManedsrap.Visa              
          /* H  */ tmptotManedsrap.Eurocard               
          /* I  */ tmptotManedsrap.Amex              
          /* J  */ tmptotManedsrap.Diners                 
          /* K  */ tmptotManedsrap.SenterGavekort         
          /* L  */ tmptotManedsrap.DiverseKort            
          /* M  */ tmptotManedsrap.KontKjopKasse     
          /* N  */ tmptotManedsrap.Beskrivelse            
          /* O  */ tmptotManedsrap.TilgodeBruktEgne       
          /* P  */ tmptotManedsrap.TilgodeBruktAndre      
          /* Q  */ tmptotManedsrap.GavekortBruktEgne      
          /* R  */ tmptotManedsrap.GavekortBruktAndre     
          /* S  */ tmptotManedsrap.TilgodeUt              
          /* T  */ tmptotManedsrap.GavekortUt             
          /* U  */ tmpTotManedsrap.InnbetaltKunde       
          /* V  */ tmptotManedsrap.SumInnbutikk           
          /* W  */ tmptotManedsrap.OmsetningEksKred       
          /* X  */ tmptotManedsrap.DiffKasse        
          /* Y  */ tmptotManedsrap.Kreditsalg       
          /* Z  */ tmptotManedsrap.OmsetningMvaGrp1   
          /* AA */ tmptotManedsrap.OmsetningMvaGrp2   
          /* AB */ tmptotManedsrap.OmsetningMvaGrpDiv   
          /* AC */ (tmptotManedsrap.OmsetningMvaGrp1 + 
                    tmptotManedsrap.OmsetningMvaGrp2 +  
                    tmptotManedsrap.OmsetningMvaGrpDiv)
          .             
  END.

  /* Lukker stream */
  OUTPUT STREAM sExportFile CLOSE.
  
  STATUS DEFAULT "Importerer data i Excel...".
  CREATE "Excel.Application" chExcelApplication.  
  chExcelApplication:Visible = FALSE.                                     
  chWorkbooks = chExcelApplication:Workbooks:OpenText(wFileName,2,1,1,1,1,FALSE,TRUE,FALSE,FALSE,FALSE).
 
  STATUS DEFAULT "Setter aktivt ark...".
  chWorkSheets = chExcelApplication:Sheets:Item(1).
 
  STATUS DEFAULT "Setter overskrift...".
  chWorkSheets:Range("A1:AC3"):Font:Bold = TRUE.
  chWorkSheets:Range("A1:AC3"):Font:Italic = TRUE.

  STATUS DEFAULT "Blokkinndeling...".
  chWorkSheets:Range("C:C"):borders(10):LineStyle   = 9. /*** Dobbelt linje ****/
  chWorkSheets:Range("U:U"):borders(10):LineStyle   = 9. /*** Dobbelt linje ****/
  chWorkSheets:Range("X:X"):borders(10):LineStyle   = 9. /*** Dobbelt linje ****/
  chWorkSheets:Range("Y:Y"):borders(10):LineStyle   = 9. /*** Dobbelt linje ****/
  chWorkSheets:Range("AB:AB"):borders(10):LineStyle   = 9. /*** Dobbelt linje ****/
  chWorkSheets:Range("AC:AC"):borders(10):LineStyle   = 9. /*** Dobbelt linje ****/
  
  STATUS DEFAULT "Setter nummerformat...".
  chWorkSheets:Range("A:A"):NumberFormat = "# ##0".
  chWorkSheets:Range("B:B"):NumberFormat = "[$-414]d mmm.;@".
  chWorkSheets:Range("D:M"):NumberFormat = "# ##0,00".
  chWorkSheets:Range("O:AC"):NumberFormat = "# ##0,00".
  
  STATUS DEFAULT "Setter overskrift...".
  chWorkSheets:Range("A1:AC1"):Merge().
  chWorkSheets:Range("A2:AC3"):HorizontalAlignment = 3.
  
  STATUS DEFAULT "Fetstil på sumrad...".
  chWorkSheets:Range("A" + string(wAntPoster) + ":AC" + string(wAntPoster)):Font:Bold = TRUE.

  /*
  STATUS DEFAULT "Setter AutoFit...".
  chWorkSheets:Columns("A1:A2"):AutoFit().
  */

  STATUS DEFAULT "Setter Kriterier...".
  chWorkSheets:PageSetup:PrintTitleRows = "A1:AC3".  
  chWorkSheets:PageSetup:LeftHeader     = "Kriterier: " + wKriterier. 
  chWorkSheets:PageSetup:RightHeader    = "Antall poster: " + String(wAntPoster).
  chWorkSheets:PageSetup:LeftFooter     = wKunde.
  chWorkSheets:PageSetup:RightFooter    = wSkoTex.
  chWorksheets:PageSetup:PrintArea      = "$A$1:$AC$" + string(wAntPoster).
  chWorkSheets:PageSetup:Orientation    = 2.
  chWorkSheets:PageSetup:FitToPagesWide = 1.
  
  STATUS DEFAULT "Setter FreezePanes...".
  chWorkSheets:Range("D4"):Select().
  chExcelApplication:ActiveWindow:FreezePanes = TRUE.
  
  STATUS DEFAULT "Setter summeringer...".
  /*chWorkSheets:Range("E4:M50"):Subtotal(1 ,1 ,"5 , 7, 9" ,TRUE ,TRUE ,TRUE ).*/   
  
  chExcelApplication:Visible = TRUE.
  
  RELEASE OBJECT chWorksheets NO-ERROR.            /* release com-handles */
  RELEASE OBJECT chWorkbooks NO-ERROR.             /* release com-handles */
  RELEASE OBJECT chExcelApplication NO-ERROR.      /* release com-handles */
  ASSIGN chWorksheets       = ?
         chWorkbooks        = ?
         chExcelApplication = ?.
  
  {swn.i}

  STATUS DEFAULT "".
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI C-Manedsrapport  _DEFAULT-ENABLE
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
  DISPLAY FI-ButListe FI-FraDato FI-TilDato 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Manedsrapport.
  ENABLE RECT-55 B-Butikker FI-FraDato FI-TilDato B-Excel B-Exit Btn_Help 
         BUTTON-SokDato2 BUTTON-SokDato 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Manedsrapport.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Manedsrapport.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ValiderKrit C-Manedsrapport 
PROCEDURE ValiderKrit :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR ipStatus AS CHAR INITIAL "AVBRYT" NO-UNDO.

DO WITH FRAME Default-Frame:                   
/*     ASSIGN FI-Tildato = INPUT FI-FraDato. /* detta gäller version 1  */ */
  IF INPUT FI-FraDato >
     INPUT FI-TilDato THEN
  DO:
    MESSAGE "FraDato er større enn TilDato!"
      VIEW-AS ALERT-BOX MESSAGE TITLE "Melding".
    RETURN "AVBRYT".
  END.
  ELSE IF INPUT FI-FraDato = ? OR INPUT FI-TilDato = ? THEN
  DO:
    MESSAGE "Både fra og til dato må angis!"
      VIEW-AS ALERT-BOX MESSAGE TITLE "Melding".
    RETURN "AVBRYT".
  END.
/*   ELSE IF (INPUT T-Accum:CHECKED AND INPUT T-Kasse  = FALSE AND */
/*       INPUT T-Butikk = FALSE AND                                */
/*       INPUT T-Total  = FALSE) THEN                              */
/*       MESSAGE "Det er ikke valgt totaler."                      */
/*           VIEW-AS ALERT-BOX INFO BUTTONS OK TITLE "Melding".    */
/*   ELSE ipStatus = "OK".                                         */

END.

/* RETURN ipStatus. */
RETURN "OK".
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

