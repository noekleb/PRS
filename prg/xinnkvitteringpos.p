&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        :  xinnkvitteringpos.p
    Purpose     :  Innlesning av kvitteringsfil fra kasse.

    Syntax      :
                    RUN VALUE(pcInnKvittering + ".p") 
                        (INPUT  piButikkNr,
                         INPUT  piGruppeNr,
                         INPUT  piKasseNr,
                         INPUT  Filer.Katalog + "~\" + Filer.FilNavn,
                         INPUT  plFilId,
                         OUTPUT piAntLinjer
                        ).

    Description :  

    Author(s)   :  Tom Nøkleby
    Created     :  9/10-01
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
         " - xinnkvitteringpos.p startet.").

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
   
/* Leser første linjen i filen. */
INPUT STREAM InnFil FROM VALUE(Filer.Katalog + "~\" + Filer.FilNavn) NO-ECHO.
  IMPORT STREAM InnFil UNFORMATTED cLinje.
INPUT STREAM InnFil CLOSE.

RUN koblekasse.p (INPUT lFilId,
                  INPUT h_Parent,
                  INPUT 1,
                  INPUT Filer.FilType,
                  INPUT cLinje,
                  OUTPUT iButikkNr,
                  OUTPUT iGruppeNr,
                  OUTPUT iKasseNr
                 ).
IF (iButikkNr = 0 AND iGruppeNr = 0 AND iKasseNr = 0) THEN
    RETURN "** Kobling av kasse misslykkes.".

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
  DEF VAR pcLinje      AS CHAR  NO-UNDO.
  DEF VAR pcOLinje     AS CHAR  NO-UNDO.
  DEF VAR piSettNr     AS INT   NO-UNDO.
  DEF VAR pdDato       AS DATE  NO-UNDO.
  DEF VAR plDataSettId AS DEC   NO-UNDO.
  DEF VAR pcSokMaske   AS CHAR  NO-UNDO.
  DEF VAR pbKoblet     AS LOG   NO-UNDO.
  DEF VAR prRowId      AS ROWID NO-UNDO.
  DEF VAR piAntISett   AS INT   NO-UNDO.
  DEF VAR piLoop       AS INT   NO-UNDO.
  DEF VAR pcTekst      AS CHAR  NO-UNDO.

  ASSIGN
      iantLinjer  = 0
      pcSokMaske  = "*000009900000000000000*"
      cDatoListe  = ""
      prRowId     = ?
      pcOLinje    = ""
      .
  FIND LAST FilLinjer OF Filer NO-LOCK NO-ERROR.
  IF AVAILABLE FilLinjer THEN
      piLinjeNr = FilLinjer.LinjeNr + 1.
  ELSE
      piLinjeNr = 1.

  INPUT STREAM InnFil FROM VALUE(cFilNavn) NO-ECHO.
  LESERLINJER:
  REPEAT:
    /* Leser linje fra filen */
    IMPORT STREAM InnFil UNFORMATTED pcLinje.

    /* Sjekker om det kommer doble datorecord. */
    /* Denne testen slår til når det kommer to eller flere like datorecord etter hverandre. */
    IF (pcLinje MATCHES pcSokMaske) AND (pcLinje = pcOLinje) THEN
    DO:
        assign
          iAntLinjer = iAntLinjer + 1
          .
        NEXT LESERLINJER.
    END.
    ASSIGN
        pcOLinje = pcLinje
        .

    /* Feil legde */
    IF LENGTH(pcLinje) < 48 OR LENGTH(pcLinje) > 49 THEN
    DO:
        assign
          iAntLinjer = iAntLinjer + 1
          .
        RUN NyFilLogg IN h_parent (INPUT lFilId, STRING(TODAY) + " " + 
                        STRING(TIME,"HH:MM:SS") + " " + USERID("skotex") + 
                       " - Feil recordlengde " + STRING(LENGTH(pcLinje)) + 
                       " på linje " + STRING(iAntLinjer) + ": " + pcLinje + "." + 
                       CHR(1) + "3").
        RUN NyFilLogg IN h_parent (INPUT lFilId, STRING(TODAY) + " " + 
                        STRING(TIME,"HH:MM:SS") + " " + USERID("skotex") + 
                       " - Recorden med feil lengde er ikke lest inn." + 
                       CHR(1) + "3").
        NEXT LESERLINJER.
    END.

    /* Plukker ut eventuelle ikke numeriske */
    pcTekst = "".
    DO piLoop = 1 TO LENGTH(pcLinje):
      IF NOT CAN-DO("0,1,2,3,4,5,6,7,8,9",SUBSTRING(pcLinje,piLoop,1)) THEN
          ASSIGN
            pcTekst = pcTekst + 
                      (IF pcTekst = "" THEN "" ELSE ",") + 
                      SUBSTRING(pcLinje,piLoop,1).
    END.
    /* Mapper ugyldige tegn til 0 */
    IF pcTekst <> "" THEN
    DO piLoop = 1 TO NUM-ENTRIES(pcTekst):
      DO WHILE INDEX(pcLinje,entry(piLoop,pcTekst)) <> 0:      
        RUN NyFilLogg IN h_parent (INPUT lFilId, STRING(TODAY) + " " + 
                        STRING(TIME,"HH:MM:SS") + " " + USERID("skotex") + 
                       " - Feil i data linje " + STRING(iAntLinjer) + " '" + 
                       entry(piLoop,pcTekst) + "' er mappet til '0': " + pcLinje + "." + 
                       CHR(1) + "1").
        OVERLAY(pcLinje,INDEX(pcLinje,entry(piLoop,pcTekst))) = "0".
      END.
    END.

    /* Oppretter datasett */
    IF pcLinje MATCHES pcSokMaske THEN
        OPPRETTDATASETT:
        DO:
          IF prRowId <> ? THEN
          DO:
              FIND DataSett EXCLUSIVE-LOCK WHERE
                  ROWID(Datasett) = prRowid.
              ASSIGN
                DataSett.AntallLinjer = piAntISett
                DataSett.SettStatus   = (IF DataSett.SettNr > 1
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
          RUN koblekasse.p (INPUT  lFilId,
                            INPUT  h_Parent,
                            INPUT  iAntLinjer,
                            INPUT  Filer.FilType,
                            INPUT  pcLinje,
                            OUTPUT iButikkNr,
                            OUTPUT iGruppeNr,
                            OUTPUT iKasseNr
                           ).
          /* Kobling misslykkes. */
          IF (iButikkNr = 0 AND
              iGruppeNr = 0 AND
              iKasseNr  = 0) THEN
            pbKoblet  = FALSE.
          ELSE
            pbKoblet = TRUE.

          /* Setter transaksjonsdato */
          ASSIGN
              pdDato = DATE(int(SUBSTRING(pcLinje,7,2)),
                            int(SUBSTRING(pcLinje,9,2)),
                            (IF int(SUBSTRING(pcLinje,5,2)) <= 50
                               THEN 2000 + int(SUBSTRING(pcLinje,5,2))
                               ELSE 1900 + int(SUBSTRING(pcLinje,5,2)))
                           )
              NO-ERROR.
          /* Er det feil i dato, skal gårsdagens dato benyttes */
          IF ERROR-STATUS:ERROR THEN
          DO:
              RUN NyFilLogg IN h_parent (INPUT lFilId, STRING(TODAY) + " " + 
                              STRING(TIME,"HH:MM:SS") + " " + USERID("skotex") + 
                             " - Feil i dato llinje " + STRING(iAntLinjer + 1) + ". Dato satt til dagens dato: " + substring(pcLinje,5,6) + "." + 
                             CHR(1) + "2").
              ASSIGN
                pdDato = TODAY
                .
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
              Datasett.ButikkNr = iButikkNr AND
              Datasett.GruppeNr = iGruppeNr AND
              Datasett.KasseNr  = iKasseNr  AND
              Datasett.Dato     = pdDato    AND
              DataSett.FilType  = 2 /* Kvittering */
              USE-INDEX DataSett NO-ERROR.
          IF AVAILABLE DataSett THEN
              piSettNr = DataSett.SettNr + 1.
          ELSE 
              piSettNr = 1.
   
          /* Er dette det første settet og det har status 1, skal denne */
          /* posten benyttes. Hvis ikke skapes en ny.                   */
          IF AVAILABLE DataSett THEN
          DO:
            IF DataSett.SettNr = 1 AND DataSett.SettStatus = 1 THEN
                ASSIGN
                  plDataSettId = DataSett.DataSettId
                  piSettNr     = DataSett.SettNr
                  .
            ELSE RELEASE DataSett. /* Ny post skal skapes. */
          END.
   
          IF NOT AVAILABLE DataSett THEN
          DO:
            CREATE DataSett.
            ASSIGN
                DataSett.DataSettId = plDataSettId
                DataSett.SettStatus = 8 /* Innlesning avbrutt */
                .
            IF NOT CAN-DO(cDatoListe,STRING(pdDato)) THEN
                ASSIGN
                  cDatoListe = cDatoListe + 
                               (IF cDatoListe = ""
                                  THEN ""
                                  ELSE ",") +
                                STRING(pdDato)
                                .
          END.
          ELSE  /* Bruker det vi fant. */
              FIND CURRENT DataSett EXCLUSIVE-LOCK.

          ASSIGN
            prRowId             = ROWID(DataSett)
            DataSett.ButikkNr   = iButikkNr 
            DataSett.GruppeNr   = iGruppeNr
            DataSett.KasseNr    = iKasseNr
            DataSett.Dato       = pdDato
            DataSett.SettNr     = piSettNr
            DataSett.Tid        = 0
            DataSett.FilId      = lFilId
            DataSett.FilType    = 2 /* Kvittering */
            .
    END. /* OPPRETTDATASETT */
        
    /* Posterer linjen */
    CREATE FilLinjer.
    ASSIGN
        FilLinjer.FilId      = lFilId
        FilLinjer.LinjeNr    = piLinjeNr
        FilLinjer.Tekst      = pcLinje
        FilLinjer.Datasett   = IF pcLinje MATCHES pcSokMaske
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
          ("Fil: " + Filer.Katalog + "\" + Filer.FilNavn + 
           " Leser " + STRING(iAntLinjer) +
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
          DataSett.SettStatus   = (IF DataSett.SettNr > 1
                                    THEN 3 /* Ekstra  */
                                    ELSE 2 /* Mottatt */)
          DataSett.SettStatus   = (IF DataSett.ButikkNr <> 0
                                    THEN DataSett.SettStatus
                                    ELSE 9  /* Ikke koblet */)
          .
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

  RUN Telleverk IN h_Parent ("Kontrollerer at alle datasett er mottatt. Vent litt... ") NO-ERROR.
  RUN sjekkdatasett.p (INPUT lFilId, INPUT cDatoListe).
  RUN Telleverk IN h_Parent (" ") NO-ERROR.

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

