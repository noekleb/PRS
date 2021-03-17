&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        :  xbehipeljournalpos.p
    Purpose     :  Innlesning av kvitteringsfil fra kasse.

    Syntax      :

    Description :  

    Author(s)   :  Tom Nøkleby
    Created     :  18/2-02
    Notes       :
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEF INPUT  PARAMETER lDataSettId AS DEC    NO-UNDO.
DEF INPUT  PARAMETER h_Parent    AS HANDLE NO-UNDO.
DEF INPUT  PARAMETER h_Logg      AS HANDLE NO-UNDO.
DEF OUTPUT PARAMETER iAntLinjer  AS INT    NO-UNDO.

DEF VAR cError          AS CHAR   NO-UNDO.
DEF VAR piLoop1         AS INT    NO-UNDO.

DEF VAR iButikkNr       AS INT    NO-UNDO.
DEF VAR iGruppeNr       AS INT    NO-UNDO.
DEF VAR iKasseNr        AS INT    NO-UNDO.
DEF VAR cBehKvittering  AS CHAR   NO-UNDO.
DEF VAR iTotAntLinjer   AS INT    NO-UNDO.
DEF VAR cDatoListe      AS CHAR   NO-UNDO.
DEF VAR lFilId          AS CHAR   NO-UNDO.
DEF VAR iStart          AS INT    NO-UNDO.
DEF VAR h_PrisKo        AS HANDLE NO-UNDO.

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

FIND DataSett NO-LOCK WHERE
    DataSett.DataSettId = lDataSettId NO-ERROR.
IF NOT AVAILABLE Datasett THEN
    RETURN " ** Ukjent datasett (" + STRING(lDataSettId) + ").".

FIND Filer OF DataSett NO-LOCK NO-ERROR.
IF NOT AVAILABLE Filer THEN
    RETURN " ** Ukjent filkobling på datasett (" + STRING(lDataSettId) + ").".
  
/* Probrambiliotek */
IF NOT VALID-HANDLE(h_PrisKo) THEN
    RUN PrisKo.p PERSISTENT SET h_PrisKo.
   
RUN TellOppLinjer.

RUN OppdaterDatasett.

IF VALID-HANDLE(h_PrisKo) THEN
    DELETE PROCEDURE h_PrisKo.

RETURN cError.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-OppdaterDatasett) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OppdaterDatasett Procedure 
PROCEDURE OppdaterDatasett :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR piLinjeNr     AS INT   NO-UNDO.
  DEF VAR piBongNr      AS INT   NO-UNDO.
  DEF VAR piOldBongNr   AS INT   NO-UNDO.
  DEF VAR pbDobbel      AS LOG   NO-UNDO.
  DEF VAR piBongLinje   AS INT   NO-UNDO.
  DEF VAR piLoop1       AS INT   NO-UNDO.
  DEF VAR prBongRowId   AS ROWID NO-UNDO.
  DEF VAR pcError       AS CHAR  NO-UNDO.
  DEF VAR pcFilError    AS CHAR  NO-UNDO.
  DEF VAR plLinjeSum    AS DEC   NO-UNDO.
  DEF VAR pcButKas      AS CHAR  NO-UNDO.
  DEF VAR pcOButKas     AS CHAR  NO-UNDO.
  
  ASSIGN
      iAntLinjer  = 0
      iStart      = TIME
      pbDobbel    = FALSE
      prBongRowId = ?
      piLinjeNr   = 0
      piOldBongNr = -1
      .


  /* Markerer Datasettet som under oppdatering/delhvis oppdatert */
  DO TRANSACTION:
      FIND CURRENT DataSett EXCLUSIVE-LOCK.
      ASSIGN
          DataSett.Behandlet = 2
          .
  END.
  FIND CURRENT DataSett NO-LOCK.

  /* Renser opp i tidliger gammal dr... */
  /* Delhvis oppdaterte bonghoder.      */
  RENS:
  DO TRANSACTION:
    FOR EACH BongHode OF DataSett EXCLUSIVE-LOCK:
        FOR EACH BongLinje EXCLUSIVE-LOCK WHERE
            BongLinje.B_Id = BongHode.B_Id:
            DELETE BongLinje.
        END.
        DELETE BongHode.
    END.
  END. /* RENS */

  /* Leser alle fillinjene som hører til datasettet.                         */
  /* Transaksjon rundt hver enkelt post.                                     */
  /* Er innlesning avbrutt, skal linjene alikevel leses, men ikke behandles. */
  DATASETT:
  FOR EACH FilLinjer OF DataSett EXCLUSIVE-LOCK WHERE /*
      FilLinjer.Behandlet = FALSE */:

    ASSIGN
        iAntLinjer  = iAntLinjer + 1
        piLinjeNr   = piLinjeNr  + 1
        .
    /* Setter informasjonen i faste felt */
    ASSIGN
        piBongNr    = int(ENTRY(5,FilLinje.Tekst,";"))
        pcButKas    = STRING(int(ENTRY(1,FilLinje.Tekst,";")),">>>999") + 
                      string(int(ENTRY(2,FilLinje.Tekst,";")),">>>999")
        .

    /* Flagger linjer som skal ignoreres.     */
    /* 83-Feilkorr. finansrapport.            */
    /* 1000 --> 1999 Makulerte transaksjoner. */
    MAKULERING:
    DO:
      IF int(ENTRY(7,FilLinje.Tekst,";")) > 999 THEN
       ASSIGN
       FilLinje.Behandlet = TRUE
       .
      ELSE IF CAN-DO("083",STRING(int(ENTRY(7,FilLinje.Tekst,";")),"999") ) THEN
          ASSIGN
          FilLinje.Behandlet = TRUE
          .
    END. /* MAKULERING */

    IF iAntLinjer MODULO 25 = 0 THEN
    DO:
      RUN Telleverk IN h_Parent ("Fil: " + Filer.FilNavn + 
                                 " Datasett: " + 
                                 STRING(FilLinje.DataSettId) + 
                                 " LinjeNr: " +
                                 STRING(iAntLinjer) +
                                 " av " +
                                 string(iTotAntLinjer) + ".") NO-ERROR.
    END.
    
    /* Henter eller oppretter BongHode.                                  */
    /* Bongen kan eksistere fra før, hvis f.eks utskriftskopi er innlest */ 
    IF piOldBongNr <> piBongNr THEN
    BONGHODE:
    DO:
      ASSIGN /* Linjenummer på bong. */
          piLinjeNr = 1
          pbDobbel  = FALSE
          .

      /* Setter status på siste klargjorte kvittering.                      */
      /* NB: Dette gjøres også utenfor loppen for å få med siste kvittering */
      IF prBongRowId <> ? THEN
      KONVBONG:
      DO:
        {xbehipeljournal.i}
      END. /* KONVBONG */

      /* Sjekker 10 dager tilbake i tid for dobbeloppdatering */
      /* TN 20/11-02 Dette gir problemer og kobles ut. */
      SJEKKDOBBEL:
      DO /* piLoop1 = 0 TO 9*/:
          FIND BongHode EXCLUSIVE-LOCK WHERE
              BongHode.ButikkNr = DataSett.ButikkNr AND
              BongHode.GruppeNr = DataSett.GruppeNr AND
              BongHode.KasseNr  = DataSett.KasseNr  AND
              BongHode.Dato     = DataSett.Dato /*(DataSett.Dato - piLoop1)*/ AND
              BongHode.BongNr   = piBongNr NO-ERROR.
          IF AVAILABLE BongHode THEN
              LEAVE SJEKKDOBBEL.
      END. /* SJEKKDOBBEL */
      
      /* Sjekker og eventuelt flagger dobbelinnlesning */
      IF AVAILABLE BongHode THEN
      DO:

          /* Flagger dobbeloppdatert kvittering.                  */
          /* Det er ikke dobbeloppdatering hvis det er ny kjøring */
          /* etter en avbrutt kjøring.                            */
          IF BongHode.BongStatus >= 5 THEN
          DO:
            ASSIGN
              pbDobbel = TRUE
              .
            IF Filer.Dobbel = FALSE THEN
            DO:
                FIND CURRENT Filer EXCLUSIVE-LOCK.
                ASSIGN
                    Filer.Dobbel = TRUE
                    .
                FIND CURRENT filer NO-LOCK.
            END.
          END.

          ASSIGN
              piOldBongNr            = piBongNr
              BongHode.OpdKvit       = TRUE
              Bonghode.DataSettId    = IF pbDobbel = FALSE
                                         THEN DataSett.DataSettId
                                         ELSE BongHode.DataSettId
              prBongRowId            = ROWID(BongHode)
              .
          LEAVE BONGHODE.
      END.
      ELSE DO:
        /* Dobble kvitteringer skal ikke legges opp, selv om de tilhører et annet datasett. */
        IF pbDobbel = FALSE THEN
        DO:
          CREATE BongHode.
          ASSIGN
            piBongLinje            = 1
            BongHode.ButikkNr      = DataSett.ButikkNr 
            BongHode.GruppeNr      = DataSett.GruppeNr 
            BongHode.KasseNr       = DataSett.KasseNr  
            BongHode.Dato          = DataSett.Dato
            BongHode.BongNr        = piBongNr
            BongHode.BongStatus    = 3 /* Under oppdatering */
            BongHode.OpdKvit       = TRUE
            Bonghode.DataSettId    = DataSett.DataSettId
            BongHode.Utskriftskopi = "Utskriftskopi ikke mottat for kvittering " + 
                                     STRING(piBongNr) + "."
            pbDobbel               = FALSE
            prBongRowId            = ROWID(BongHode)
            .
        END.
      END.

      ASSIGN
          piOldBongNr = piBongNr
          .
    END. /* BONGHODE */

    /* Skipper behandling av alle linjer på den dobbelinnleste bongen. */
    IF pbDobbel THEN
    DO:
        ASSIGN
            piBongLinje = piBongLinje + 1
            .
        IF cError = "" THEN
            cError  = cError + 
                     (IF cError = ""
                        THEN ""
                        ELSE "|") +
                     STRING(TODAY) + " " + 
                              STRING(TIME,"HH:MM:SS") + " " + USERID("skotex") +
                     " - Datasett " + 
                     string(DataSett.DataSettId) + 
                     " inneholder en eller flere dobbeloppdatering av bonger. " + 
                     " Deriblant bongnr: " + STRING(piBongNr) + "." + CHR(1) + "2".
            .
        ASSIGN
            FilLinje.Behandlet = TRUE
            .
        NEXT DATASETT.
    END.

    /* TN 4/1-02 Kassereroppgjør kommer på bongnr 0
    /* Skipper behandling av alle linjer med bongnr = 0. */
    IF piBongNr = 0 THEN
    DO:
        ASSIGN
            piBongLinje        = piBongLinje + 1
            FilLinje.Behandlet = TRUE
            .
        IF cError = "" THEN
            cError  = cError + 
                     (IF cError = ""
                        THEN ""
                        ELSE "|") +
                     STRING(TODAY) + " " + 
                              STRING(TIME,"HH:MM:SS") + " " + userid("skotex") +
                     " - Datasett " + 
                     string(DataSett.DataSettId) + 
                     " Bonglinje med BongNr = 0. " + CHR(1) + "2".
            .
        NEXT DATASETT.
    END.
    */

    FIND BongLinje EXCLUSIVE-LOCK WHERE
         BongLinje.ButikkNr = BongHode.ButikkNr AND
         BongLinje.GruppeNr = BongHode.GruppeNr AND
         BongLinje.KasseNr  = BongHode.KasseNr  AND
         BongLinje.Dato     = BongHode.Dato     AND
         BongLinje.BongNr   = BongHode.BongNr   AND
         BongLinje.LinjeNr  = piBongLinje NO-ERROR.
    IF NOT AVAILABLE BongLinje THEN
    DO:
      CREATE BongLinje. /* */
      ASSIGN
          BongLinje.B_Id         = BongHode.B_Id
          BongLinje.ButikkNr     = BongHode.ButikkNr 
          BongLinje.GruppeNr     = BongHode.GruppeNr 
          BongLinje.KasseNr      = BongHode.KasseNr  
          BongLinje.Dato         = BongHode.Dato     
          BongLinje.BongNr       = BongHode.BongNr   
          BongLinje.LinjeNr      = piLinjeNr /*BongLinje*/
          FilLinje.Behandlet     = TRUE
          piBongLinje            = piBongLinje + 1
          .
    END.

    /* Legger på datastreng fra kassen. */
    ASSIGN
        BongLinje.OriginalData = FilLinje.Tekst
        .
  END. /* DATSETT */

  /* Markerer Datasettet som oppdatert */
  DO TRANSACTION:
      IF prBongRowId <> ? THEN
      KONVBLOKK:
      DO:
        {xbehipeljournal.i}
      END.

      IF NOT CAN-FIND(FIRST BongHode OF DataSett WHERE
                      BongHode.BongStatus < 5) AND
         NOT CAN-FIND(FIRST FilLinje OF DataSett WHERE
                      FilLinje.Behandlet = FALSE)
          THEN
      DO:
        FIND CURRENT DataSett EXCLUSIVE-LOCK.
        ASSIGN
          DataSett.Behandlet = 3
          .
      END.
  END.
  FIND CURRENT DataSett NO-LOCK.
  
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
      iStart        = TIME
      .
  FOR EACH FilLinjer NO-LOCK WHERE
    FilLinjer.DataSettId = lDataSettId:  
    ASSIGN
        iTotAntLinjer = iTotAntLinjer + 1
        .
  END.

  /*
  ASSIGN
    cError = cError + 
             (IF cError = ""
                THEN ""
                ELSE CHR(1)) +
             STRING(TODAY) + " " + 
             STRING(TIME,"HH:MM:SS") + " " + userid("skotex") + 
             " - Tidsbruk i xbehkvitteringspos.p - TellOppLinjer: " + 
             STRING(TIME - iStart,"HH:MM:SS").
  */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

