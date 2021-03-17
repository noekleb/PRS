&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        :  xinnipeljournalpos.p
    Purpose     :  Innlesning av kvitteringsfil fra InfoPOS kasse.

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
    Created     :  14/02-02
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
DEFINE VARIABLE dDato AS DATE NO-UNDO.

DEFINE VARIABLE iDatoFormat AS INTEGER NO-UNDO.
DEF VAR iButikkNr       AS INT  NO-UNDO.
DEF VAR iGruppeNr       AS INT  NO-UNDO.
DEF VAR iKasseNr        AS INT  NO-UNDO.
DEF VAR cInnKvittering  AS CHAR NO-UNDO.
DEF VAR iTotAntLinjer   AS INT  NO-UNDO.
DEF VAR cDatoListe      AS CHAR NO-UNDO.
DEF VAR cKontrolltabell AS CHARACTER  NO-UNDO. /* MottaksKontroll av vilken data vi skall testa mot */

DEF STREAM InnFil.

DEF TEMP-TABLE tmpHGRDAG    LIKE HGRDAG.
DEF TEMP-TABLE tmpTIMEDAG   LIKE TIMEDAG.
DEF TEMP-TABLE tmpVAREDAG   LIKE VAREDAG.
DEF TEMP-TABLE tmpFilLinjer LIKE FilLinjer
    FIELD ButikkNr  AS INT
    FIELD KasseNr   AS INT
    FIELD Dato      AS DATE
    FIELD BongNr    AS INT
    FIELD BongLinje AS INT
    .

{syspara.i 1 1 55 iDatoFormat INT}

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
         " - xinnipeljournalpos.p startet.").

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
    
/* Oppretter kasse automatisk hvis den ikke finnes fra før. */
IF Filer.FilType = 1 /* InfoPOS POS */ THEN RUN SjekkKasseNrInfoPOS (INPUT cLinje).

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
{syspara.i 1 1 25 cKontrolltabell}
IF NOT CAN-DO("1,2",cKontrolltabell) THEN
    ASSIGN cKontrolltabell = "1".

RUN TellOppLinjer.

RUN InnLesFil.    /* El-Journal. */


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
  DEF VAR pcDato       AS CHAR  NO-UNDO.
  DEF VAR plDataSettId AS DEC   NO-UNDO.
  DEF VAR pcSokMaske   AS CHAR  NO-UNDO.
  DEF VAR pcOSokMaske  AS CHAR  NO-UNDO.
  DEF VAR pbKoblet     AS LOG   NO-UNDO.
  DEF VAR prRowId      AS ROWID NO-UNDO.
  DEF VAR piAntISett   AS INT   NO-UNDO.
  DEF VAR piLoop       AS INT   NO-UNDO.
  DEF VAR pcTekst      AS CHAR  NO-UNDO.
  DEF VAR pcButKasLst  AS CHAR  NO-UNDO.
  DEF VAR pc2Tekst     AS CHAR  NO-UNDO.
  DEF VAR piBongLinje  AS INT   NO-UNDO.
  DEF VAR piBongNr     AS INT   NO-UNDO.
  DEF VAR d31DecFgAr AS DATE NO-UNDO.
  DEFINE VARIABLE pcKonv  AS CHARACTER NO-UNDO.
  DEFINE VARIABLE p2cKonv AS CHARACTER NO-UNDO.
  
  ASSIGN
      iantLinjer  = 0
      pcSokMaske  = ""
      pcOSokMaske = ""
      cDatoListe  = ""
      prRowId     = ?
      pcOLinje    = ""
      .

  /* Leser inn linjer i temp-file for sortering. */
  FOR EACH tmpFilLinjer: 
      DELETE tmpFilLinjer.
  END.
  piLinjeNr = 1.
  INPUT STREAM InnFil FROM VALUE(cFilNavn) NO-ECHO.
  LESINNBUFFER:
  REPEAT:
    ASSIGN
        pcLinje = ""
        .

    /* Leser linje fra filen */
    IMPORT STREAM InnFil UNFORMATTED pcLinje.
    IF TRIM(pcLinje) = "" THEN
        NEXT LESINNBUFFER.
        
    /* Trimmer bort semikolon fra vareteksten. */    
    IF ENTRY(7,pcLinje,';') = '3' AND NUM-ENTRIES(pcLinje,'"') = 3 THEN 
      ENTRY(2,pcLinje,'"') = REPLACE(ENTRY(2,pcLinje,'"'),";"," ").

    /* Mapper bort tekst */
    pcLinje = REPLACE(pcLinje,'Godkännes för debitering \nav mitt konto enligt ovan','').

    /* Norsk eller svensk datoformat */
    IF iDatoFormat = 1 /* YY/MM/DD */ AND NUM-ENTRIES(pcLinje,';') >= 3 THEN 
      DO:
        /* TN 8/2-13 Endret 
        ASSIGN
          pcKonv  = ENTRY(3,pcLinje,";")
          p2cKonv = ENTRY(3,pcLinje,";") NO-ERROR.
        
        ENTRY(1,pcKonv,'/') = ENTRY(3,p2cKonv,'/').
        ENTRY(3,pcKonv,'/') = ENTRY(1,p2cKonv,'/').        
        ENTRY(3,pcLinje,';') = pcKonv.
        */
        ASSIGN 
            pcKonv = ENTRY(3,pcLinje,";")
            dDato  = DATE(INT(ENTRY(2,pcKonv,'/')),
                          INT(ENTRY(3,pcKonv,'/')),
                          2000 + INT(ENTRY(1,pcKonv,'/')))
            ENTRY(3,pcLinje,';') = STRING(dDato).
      END.
    ELSE DO:

    END.
    IF piBongLinje = 0 THEN
        piBongLinje = 1.
    IF piBongNr <> int(ENTRY(5,pcLinje,";")) THEN
    DO:
        ASSIGN
            piBongLinje = 1
            piBongNr    = int(ENTRY(5,pcLinje,";"))
            .
    END.
    CREATE tmpFilLinjer.
    ASSIGN
        tmpFilLinjer.FilId      = lFilId
        tmpFilLinjer.LinjeNr    = piLinjeNr
        tmpFilLinjer.Tekst      = pcLinje 
        tmpFilLinjer.ButikkNr   = int(ENTRY(1,pcLinje,";"))
        tmpFilLinjer.KasseNr    = int(ENTRY(2,pcLinje,";"))
        tmpFilLinjer.Dato       = DATE(ENTRY(3,pcLinje,";"))
        tmpFilLinjer.BongNr     = int(ENTRY(5,pcLinje,";"))
        tmpFilLinjer.BongLinje  = piBongLinje
        piLinjeNr               = piLinjeNr + 1
        piBongLinje             = piBongLinje + 1
        .
  END. /* LESINNBUFFER */
  INPUT STREAM InnFil CLOSE.

  FIND LAST FilLinjer OF Filer NO-LOCK NO-ERROR.
  IF AVAILABLE FilLinjer THEN
      piLinjeNr = FilLinjer.LinjeNr + 1.
  ELSE
      piLinjeNr = 1.

  LESERLINJER:
  FOR EACH tmpFilLinjer
      BREAK BY tmpFilLinjer.ButikkNr
            BY tmpFilLinjer.KasseNr
            BY tmpFilLinjer.Dato
            BY tmpFilLinjer.BongNr
            BY tmpFilLinjer.BongLinje:
    ASSIGN
        pcLinje = tmpFilLinjer.Tekst
        .

/*     OUTPUT TO VALUE("Gurre.txt") APPEND. */
/*     EXPORT DELIMITER ";"                 */
/*         tmpFilLinjer.ButikkNr            */
/*         tmpFilLinjer.KasseNr             */
/*         tmpFilLinjer.Dato                */
/*         tmpFilLinjer.BongNr              */
/*         tmpFilLinjer.BongLinje           */
/*         .                                */
/*     OUTPUT CLOSE.                        */

    /* Blanke linjer skippes */
    IF pcLinje = "" THEN
        NEXT LESERLINJER.

    /* Setter søkemaske og brytpunkt. */
    ASSIGN
        pcSokMaske  = ENTRY(1,pcLinje,";") + ";" + /* Butikk */ 
                      ENTRY(2,pcLinje,";") + ";" + /* Kasse  */
                      ENTRY(3,pcLinje,";") + ";"   /* Dato   */
        pcOLinje    = pcLinje
        pcDato      = ENTRY(3,pcLinje,";")
        .

    /* Oppretter datasett */
    /* IF pcSokMaske <> pcOSokMaske THEN */
    IF FIRST-OF(tmpFilLinjer.Dato) THEN
        OPPRETTDATASETT:
        DO:
          /* Ferdigstempler den vi hold på med. */
          IF prRowId <> ? THEN
          DO:
              FIND DataSett EXCLUSIVE-LOCK WHERE
                  ROWID(Datasett) = prRowid.
              ASSIGN
                DataSett.AntallLinjer = DataSett.AntallLinjer + piAntISett
                DataSett.SettStatus   = (IF DataSett.SettNr > 1
                                          THEN 3 /* Ekstra  */
                                          ELSE 2 /* Mottatt */)
                DataSett.SettStatus   = (IF DataSett.ButikkNr <> 0
                                          THEN DataSett.SettStatus
                                          ELSE 9  /* Ikke koblet */)
                piAntISett            = 0
                prRowId               = ?
                pcOSokMaske           = pcSokMaske
                .
              /* Åpningsskjemahantering */
              IF cKontrolltabell = "1" THEN DO:
                  FIND ApnSkjema WHERE ApnSkjema.ButikkNr = DataSett.ButikkNr AND
                                       ApnSkjema.Ar       = YEAR(DataSett.Dato) NO-ERROR.  
                  IF AVAIL ApnSkjema THEN DO:
                      ASSIGN d31DecFgAr = DATE(12,31,ApnSkjema.Ar - 1)
                             ENTRY(DataSett.Dato - d31DecFgAr,ApnSkjema.OpenClosed) = "4" NO-ERROR.
                  END.
              END.
              /* Åpnings.... SLUTT      */
              RELEASE DataSett.
          END.
          ELSE
              IF pcOSokMaske = "" THEN
                  pcOSokMaske = pcSokMaske.

          /* Oppretter kasse automatisk hvis den ikke finnes fra før. */
          IF Filer.FilType = 1 /* InfoPOS POS */ THEN RUN SjekkKasseNrInfoPOS (INPUT pcLinje).


          RUN koblekasse.p (INPUT  lFilId,
                            INPUT  h_Parent,
                            INPUT  iAntLinjer,
                            INPUT  Filer.FilType,
                            INPUT  pcLinje,
                            OUTPUT iButikkNr,
                            OUTPUT iGruppeNr,
                            OUTPUT iKasseNr
                           ).
          /* BUG ovverride */
          ASSIGN
              iButikkNr = tmpFilLinjer.ButikkNr
              iGruppeNr = 1 /*tmpFilLinjer.GruppeNr*/
              iKasseNr  = tmpFilLinjer.KasseNr
              .
          /* Kobling misslykkes. */
          IF (iButikkNr = 0 AND
              iGruppeNr = 0 AND
              iKasseNr  = 0) THEN
            pbKoblet  = FALSE.
          ELSE
            pbKoblet = TRUE.

          /* ------------------------------
          /* Setter svensk transaksjonsdato */
          IF iDatoFormat = 1 /* YMD  ;09/09/28; */ THEN 
            ASSIGN pdDato = DATE(
                                 int(ENTRY(2,pcDato,'/')),
                                 int(ENTRY(1,pcDato,'/')),
                                 (IF int(ENTRY(3,pcDato,'/')) < 50
                                   THEN 2000 + int(ENTRY(3,pcDato,'/'))
                                   ELSE 1900 + int(ENTRY(3,pcDato,'/'))
                                 )
                                 )
                             .
          /* Setter norsk dato */
          ELSE ASSIGN /* DMY */
                 pdDato = DATE(ENTRY(3,pcLinje,";"))
            NO-ERROR.
          ------------------------------ */
          ASSIGN pdDato = DATE(ENTRY(3,pcLinje,";")) NO-ERROR.

          
          /* Er det feil i dato, skal dagens dato benyttes */
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
              DataSett.FilType  = 1 /* EL-Journal */
              USE-INDEX DataSett NO-ERROR.
          IF AVAILABLE DataSett THEN
              piSettNr = DataSett.SettNr + 1.
          ELSE DO:
              piSettNr = 1.
          END.

          /* Alle kvitteringer som kommer inn på samme dag skal kobles  */
          /* til det samme datasettet. Forutsetning er at settet ikke   */
          /* har behandlingsstatus > 1.                                 */
/*           IF AVAILABLE DataSett THEN                             */
/*           DO:                                                    */
/*             IF DataSett.SettNr <= 3 AND DataSett.SettStatus <= 2 */
/*                 AND DataSett.Behandlet <= 1 THEN                 */
/*                 ASSIGN                                           */
/*                   plDataSettId = DataSett.DataSettId             */
/*                   piSettNr     = DataSett.SettNr                 */
/*                   .                                              */
/*             ELSE                                                 */
/*                 RELEASE DataSett. /* Ny post skal skapes. */     */
/*           END.                                                   */
          RELEASE DataSett. /* Ny post skal skapes. */

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
          DO:
              FIND CURRENT DataSett EXCLUSIVE-LOCK.
          END.

          ASSIGN
            prRowId             = ROWID(DataSett)
            DataSett.ButikkNr   = iButikkNr 
            DataSett.GruppeNr   = iGruppeNr
            DataSett.KasseNr    = iKasseNr
            DataSett.Dato       = pdDato
            DataSett.SettNr     = piSettNr
            DataSett.Tid        = 0
            DataSett.FilId      = lFilId
            DataSett.FilType    = 1 /* EL-Journal */
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
          ("Fil: " + Filer.Katalog + "\" + Filer.Filnavn +  
           " Leser " + STRING(iAntLinjer) +
           " av " + STRING(iTotAntLinjer) + ".") NO-ERROR.
    END.

  END. /* LESERLINJER */

  RUN Telleverk IN h_Telleverk (" ") NO-ERROR.

  /* Stempler posten som innlest. */
  IF AVAILABLE DataSett THEN
  DO TRANSACTION:
      FIND CURRENT DataSett EXCLUSIVE-LOCK.
      ASSIGN
          DataSett.AntallLinjer = DataSett.AntallLinjer + piAntISett
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
      /* Åpningsskjemahantering */
      IF cKontrolltabell = "1" THEN DO:
          FIND ApnSkjema WHERE ApnSkjema.ButikkNr = DataSett.ButikkNr AND
                               ApnSkjema.Ar       = YEAR(DataSett.Dato) NO-ERROR.  
          IF AVAIL ApnSkjema THEN DO:
              ASSIGN d31DecFgAr = DATE(12,31,ApnSkjema.Ar - 1)
                     ENTRY(DataSett.Dato - d31DecFgAr,ApnSkjema.OpenClosed) = "4" NO-ERROR.
          END.
      END.
      /* Åpnings.... SLUTT      */
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

&IF DEFINED(EXCLUDE-SjekkKasseNrInfoPOS) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SjekkKasseNrInfoPOS Procedure 
PROCEDURE SjekkKasseNrInfoPOS :
/*------------------------------------------------------------------------------
  Purpose:     Oppretter kasserecord for de kassenummer som er ukjente.
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEF INPUT PARAMETER pcLinje AS CHAR NO-UNDO.


DEF VAR piButikkNr AS INT  NO-UNDO.
DEF VAR piKasseNr  AS INT  NO-UNDO.

DEF BUFFER bKasse FOR Kasse.

ASSIGN
  piButikkNr = int(ENTRY(1,pcLinje,";"))
  piKasseNr  = int(ENTRY(2,pcLinje,";"))
  NO-ERROR.

IF ERROR-STATUS:ERROR THEN
    RETURN.

/* Oppretter kassen hvis den mangler. */
IF NOT CAN-FIND(Kasse WHERE
                Kasse.ButikkNr = pibutikkNr AND
                Kasse.GruppeNr = 1  AND
                Kasse.KasseNr  = piKasseNr) THEN
  DO FOR bKasse TRANSACTION:
    /* Kasse 1 skal ALLTID være lagt opp på alle butikker. */
    FIND Kasse NO-LOCK WHERE
        Kasse.ButikkNr = piButikkNr AND
        Kasse.Gruppe   = 1 AND
        Kasse.KasseNr  = 1 NO-ERROR.
    IF AVAILABLE Kasse THEN
    DO:
        CREATE bKasse.
        BUFFER-COPY Kasse TO bKasse
            ASSIGN
            bKasse.KasseNr        = piKasseNr
            bKasse.Navn           = "Kasse " + string(piKasseNr) + " - Butikk " + string(piButikkNr)
            bKasse.Aktiv          = TRUE
            bKasse.ElJournal[2]   = STRING(pibutikkNr)
            bKasse.ElJournalId    = STRING(pibutikkNr) + ";" + string(piKasseNr) + ";"
            bKasse.ElJournalAktiv = TRUE
            .
    END.

    IF AVAILABLE bKasse THEN
        RELEASE bKasse.
  END. /* TRANSACTION */

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
  REPEAT:
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

