&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        :  xinnutskrkopipos.p
    Purpose     :  Innlesning av utskriftskopifil fra kasse.

    Syntax      :

    Description :  

    Author(s)   :  Tom Nøkleby
    Created     :  23/10-01
    Notes       :
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEF INPUT  PARAMETER lFilId      AS DEC    NO-UNDO.
DEF INPUT  PARAMETER h_Parent    AS HANDLE NO-UNDO.
DEF INPUT  PARAMETER h_Telleverk AS HANDLE NO-UNDO.
DEF OUTPUT PARAMETER iAntLinjer  AS INT    NO-UNDO.

DEF VAR cError          AS CHAR NO-UNDO.
DEF VAR piLoop1         AS INT  NO-UNDO.
DEF VAR cLinje         AS CHAR NO-UNDO.
DEF VAR cFilNavn        AS CHAR NO-UNDO.

DEF VAR iButikkNr       AS INT  NO-UNDO.
DEF VAR iGruppeNr       AS INT  NO-UNDO.
DEF VAR iKasseNr        AS INT  NO-UNDO.
DEF VAR cInnKvittering  AS CHAR NO-UNDO.
DEF VAR iTotAntLinjer   AS INT  NO-UNDO.
DEF VAR cDatoListe      AS CHAR NO-UNDO.

DEF STREAM InnFil.

{ttUtskrKopi.i &NEW="NEW"}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Procedure
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: CODE-ONLY COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Procedure ASSIGN
         HEIGHT             = 15
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

RUN NyFilLogg IN h_Parent (INPUT lFilId, STRING(TODAY) + " " + 
          STRING(TIME,"HH:MM:SS") + " " + USERID("skotex") + 
         " - xinnutskrkopipos.p startet.").

FIND Filer NO-LOCK WHERE
    Filer.FilId = lFilId NO-ERROR.
IF NOT AVAILABLE Filer THEN
DO:
    RUN NyFilLogg IN h_Parent (INPUT lFilId, STRING(TODAY) + " " + 
              STRING(TIME,"HH:MM:SS") + " " + USERID("skotex") + 
             " -  Ukjent post 'filer' (" + STRING(lFilId) + ")." + 
             CHR(1) + "1") NO-ERROR.

    RETURN " ** Ukjent Filer post (" + STRING(lFilId) + ").".
END.
ASSIGN
    cFilNavn = Filer.Katalog + "~\" + Filer.FilNavn.
   
/* Finner kasse som filen er koblet til. */
RUN koblekasse.p (INPUT lFilId,
                  INPUT h_Parent,
                  INPUT 1,
                  INPUT Filer.FilType,
                  INPUT "", /* brukes i KobleKvittering */
                  OUTPUT iButikkNr,
                  OUTPUT iGruppeNr,
                  OUTPUT iKasseNr
                 ).
IF RETURN-VALUE <> "" THEN
    RETURN RETURN-VALUE.

RUN TellOppLinjer.

RUN InnLesFil.

RETURN "".

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-InnLesFil) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InnLesFil Procedure 
PROCEDURE InnLesFil :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
  
  iButikkNr 
  iGruppeNr 
  iKasseNr  
  cFilNavn  
  lFilId    
  iAntLinjer
------------------------------------------------------------------------------*/
  DEF VAR piLinjeNr    AS INT   NO-UNDO.
  DEF VAR piSettNr     AS INT   NO-UNDO.
  DEF VAR plDataSettId AS DEC   NO-UNDO.
  DEF VAR prRowId      AS ROWID NO-UNDO.
  DEF VAR piAntISett   AS INT   NO-UNDO.
  DEF VAR pcTekst      AS CHAR  NO-UNDO.
  DEF VAR pcFilNavn    AS CHAR  NO-UNDO.
  DEF VAR piLoop       AS INT   NO-UNDO.

  ASSIGN
      iantLinjer   = 0
      cDatoListe   = ""
      prRowId      = ?
      plDataSettId = 0
      pcFilNavn    = Filer.Katalog + "~\" + Filer.FilNavn
      .
  
  RUN Telleverk IN h_Telleverk ("Tømmer temp-table. Vent litt..") NO-ERROR.
  /* Tømmer temp-table */
  FOR EACH ttUtskrKopi WHERE
      ttUtskrKopi.FilNavn = pcFilNavn:
      DELETE ttUtskrKopi.
  END.

  RUN Telleverk IN h_Telleverk ("Leser inn fil i temp-table. Vent litt..") NO-ERROR.
  /* Leser inn filen t temp-table. */
  RUN xinn2utskriftskopipos.p (input pcFilNavn).
  IF RETURN-VALUE <> "" THEN
  DO:
      RUN NyFilLogg IN h_parent (INPUT lFilId, STRING(TODAY) + " " + 
                      STRING(TIME,"HH:MM:SS") + " " + USERID("skotex") + 
                     " - Innlesning av fil " + pcFilNavn + 
                     " i temp-file avbrutt. [" + RETURN-VALUE + "]" + 
                     CHR(1) + "1").
      RETURN "** Innlesning av fil " + pcFilNavn + 
             " i temp-file avbrutt. [" + RETURN-VALUE + "]".
  END.

  FIND LAST FilLinjer OF Filer NO-LOCK NO-ERROR.
  IF AVAILABLE FilLinjer THEN
      piLinjeNr = FilLinjer.LinjeNr + 1.
  ELSE
      piLinjeNr = 1.

  LESERLINJER:
  FOR EACH ttUtskrKopi WHERE
      ttUtskrKopi.FilNavn = pcFilNavn
      BREAK BY ttUtskrKopi.FilNavn
            BY ttUtskrKopi.ButikkNr
            BY ttUtskrKopi.GruppeNr
            BY ttUtskrKopi.Kasse
            BY ttUtskrKopi.Dato
            BY ttUtskrKopi.BongNr:

    /* Oppretter datasett */
    IF FIRST-OF(ttUtskrKopi.Dato) THEN
        OPPRETTDATASETT:
        DO:
            /* Oppdaterer det ferdigbehandlede datasettet, før det nye skapes. */
            IF prRowId <> ? THEN
            DO:
                FIND DataSett EXCLUSIVE-LOCK WHERE
                    ROWID(Datasett) = prRowid.
                ASSIGN
                  DataSett.AntallLinjer = piAntISett
                  DataSett.SettStatus   = (IF DataSett.SettStatus > 1
                                            THEN 3 /* Ekstra  */
                                            ELSE 2 /* Mottatt */)
                  DataSett.SettStatus   = (IF DataSett.ButikkNr <> 0
                                            THEN DataSett.SettStatus
                                            ELSE 9  /* Ikke koblet */)
                  piAntISett             = 0
                  prRowId                = ?
                  .
                RELEASE DataSett.
            END.

          /* Finner neste DataSettId */
          FIND LAST DataSett NO-LOCK
              USE-INDEX DataSettId NO-ERROR.
          IF AVAILABLE DataSett THEN 
              plDataSettId = DataSett.DataSettId + 1.
          ELSE
              plDataSettId = 1.
   
          /* Finner neste SettNr */
          FIND LAST Datasett NO-LOCK WHERE
              Datasett.ButikkNr = ttUtskrKopi.ButikkNr AND
              Datasett.GruppeNr = ttUtskrKopi.GruppeNr AND
              Datasett.KasseNr  = ttUtskrKopi.KasseNr  AND
              Datasett.Dato     = ttUtskrKopi.Dato    AND
              DataSett.FilType  = 3 /* Utskriftskopi */
              USE-INDEX DataSett NO-ERROR.
          IF AVAILABLE DataSett THEN
              ASSIGN piSettNr = IF DataSett.SettStatus = 1 THEN DataSett.SettNr ELSE DataSett.SettNr + 1
                     plDataSettId = IF DataSett.SettStatus = 1 THEN DataSett.DataSettId ELSE plDataSettId.
          ELSE 
              piSettNr = 1.
          /* Er dette det første settet og det har status 1, skal denne */
          /* posten benyttes. Hvis ikke skapes en ny.                   */
          IF AVAILABLE DataSett THEN
          DO:
            IF DataSett.SettNr = 1 AND DataSett.SettStatus = 1 THEN. /* Gjør ingenting */
            ELSE RELEASE DataSett. /* Ny post skal skapes. */
          END.

          /* Oppretter datasettet hvis det ikke finnes. */
          IF NOT AVAILABLE DataSett THEN
          DO:
            CREATE DataSett.
            ASSIGN
                DataSett.DataSettId = plDataSettId
                SettStatus          = 8 /* Innlesning avbrutt */
                .
            IF NOT CAN-DO(cDatoListe,STRING(ttUtskrKopi.Dato)) THEN
                ASSIGN
                  cDatoListe = cDatoListe + 
                               (IF cDatoListe = ""
                                  THEN ""
                                  ELSE ",") +
                                STRING(ttUtskrKopi.Dato)
                                .
          END.
          ELSE
              FIND CURRENT DataSett EXCLUSIVE-LOCK.
          ASSIGN
            prRowId             = ROWID(DataSett)
            DataSett.ButikkNr   = ttUtskrKopi.ButikkNr 
            DataSett.GruppeNr   = ttUtskrKopi.GruppeNr
            DataSett.KasseNr    = ttUtskrKopi.KasseNr
            DataSett.Dato       = ttUtskrKopi.Dato
            DataSett.SettNr     = piSettNr
            DataSett.Tid        = 0
            DataSett.FilId      = lFilId
            DataSett.FilType    = 3 /* Utskriftskopi */
            .
    END. /* OPPRETTDATASETT */
        
    /* Posterer linjen */
    CREATE FilLinjer.
    ASSIGN
        FilLinjer.FilId      = lFilId
        FilLinjer.LinjeNr    = piLinjeNr
        FilLinjer.Tekst      = string(ttUtskrKopi.ButikkNr)  + "|" + 
                               STRING(ttUtskrKopi.GruppeNr)  + "|" +
                               STRING(ttUtskrKopi.KasseNr)   + "|" + 
                               STRING(ttUtskrKopi.Dato)      + "|" + 
                               STRING(ttUtskrKopi.BongNr)    + "|" + 
                               STRING(ttUtskrKopi.MedlemsNr)
        FilLinjer.StorTekst  = ttUtskrKopi.Tekst
        FilLinjer.Datasett   = IF plDataSettId <> 0
                                 THEN TRUE
                                 ELSE FALSE
        FilLinjer.DataSettId = plDataSettId
        iAntLinjer           = iAntLinjer + 1
        piLinjeNr            = piLinjeNr  + 1
        piAntISett           = piAntISett + 1
        .

    IF iAntLinjer MODULO 25 = 0 THEN
    DO:
      RUN Telleverk IN h_Telleverk 
          ("Leser " + STRING(iAntLinjer) +
           " av " + STRING(iTotAntLinjer) + ".") NO-ERROR.
    END.

  END. /* LESERLINJER */
  INPUT STREAM InnFil CLOSE.

  RUN Telleverk IN h_Telleverk (" ") NO-ERROR.

  /* Stempler posten som innlest. */
  IF AVAILABLE DataSett THEN
  DO TRANSACTION:
      FIND CURRENT DataSett EXCLUSIVE-LOCK.
      ASSIGN
          DataSett.AntallLinjer = piAntISett
          DataSett.SettStatus   = (IF DataSett.SettStatus > 1
                                    THEN 3 /* Ekstra  */
                                    ELSE 2 /* Mottatt */)
          DataSett.SettStatus   = (IF DataSett.ButikkNr <> 0
                                    THEN DataSett.SettStatus
                                    ELSE 9  /* Ikke koblet */)
          .
  END.
  IF AVAILABLE Filer THEN
  DO TRANSACTION:
      FIND CURRENT Filer EXCLUSIVE-LOCK.
      ASSIGN
          Filer.Innlest = TRUE
          Filer.InnlestDato = TODAY
          Filer.InnlestKl   = TIME
          Filer.InnlestAv   = USERID("SkoTex")
          .
  END.

  IF AVAILABLE DataSett THEN
      FIND CURRENT DataSett NO-LOCK.
  IF AVAILABLE Filer THEN
      FIND CURRENT Filer    NO-LOCK.

  RUN Telleverk IN h_Telleverk ("Kontrollerer at alle datasett er mottatt. Vent litt... ") NO-ERROR.
  RUN sjekkdatasett.p (INPUT lFilId, INPUT cDatoListe).
  RUN Telleverk IN h_Telleverk (" ") NO-ERROR.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-TellOppLinjer) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE TellOppLinjer Procedure 
PROCEDURE TellOppLinjer :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  ASSIGN
      iTotAntLinjer = 0
      .
  INPUT STREAM InnFil FROM VALUE(Filer.Katalog + "~\" + Filer.FilNavn) NO-ECHO.
  repeat:
    IMPORT STREAM InnFil UNFORMATTED cLinje.
    ASSIGN
        iTotAntLinjer = iTotAntLinjer + 1
        .
  END.
  INPUT STREAM InnFil CLOSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

