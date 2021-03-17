&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : 
    Purpose     :

    Syntax      :

    Description : 

    Author(s)   : Tom Nøkleby
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEF INPUT  PARAMETER lFilId      AS DEC    NO-UNDO.
DEF INPUT  PARAMETER h_Parent    AS HANDLE NO-UNDO.
DEF OUTPUT PARAMETER iAntLinjer  AS INT    NO-UNDO.

DEF VAR iTotAntLinjer AS INT  NO-UNDO.
DEF VAR cLinje        AS CHAR NO-UNDO.
DEF VAR cFilNavn      AS CHAR NO-UNDO.
DEF VAR ctmpKatalog   AS CHAR NO-UNDO.
DEF VAR pcLinje       AS CHAR NO-UNDO.
DEF VAR h_PrisKo      AS HANDLE NO-UNDO.
DEF VAR iLevNr        AS INT NO-UNDO.
DEF VAR bHK           AS LOG NO-UNDO.
DEF VAR cTekst        AS CHAR NO-UNDO.
DEF VAR cEkspKatalog  AS CHAR NO-UNDO.

DEF VAR hField1       AS HANDLE NO-UNDO.
DEF VAR lArtikkelNr   AS DEC  NO-UNDO.
DEF VAR iStrKode      AS INT  NO-UNDO.

DEF STREAM InnFil.
DEF STREAM UtFil.

DEFINE TEMP-TABLE TT_Lagerstatus 
    FIELD ArticleNo AS DEC 
    FIELD EANNo AS CHAR
    FIELD ArtName AS CHAR 
    FIELD AktivWeb AS LOG
    FIELD SendtTilWeb AS LOG
    FIELD lArtikkelNr AS DEC 
    FIELD iStrKode AS INT 
    INDEX ArtNr
        lArtikkelNr
    .

DEFINE TEMP-TABLE tt_Error NO-UNDO
  FIELD LinjeNr AS INT
  FIELD Tekst   AS CHAR
  .

{windows.i}

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

{syspara.i 1 1 18 cTekst}
IF CAN-DO('J,Y,Ja,YES,TRUE,1',cTekst) THEN
    bHK = TRUE.
{syspara.i 1 1 51 cEkspKatalog}

FIND VPIFilHode NO-LOCK WHERE
    VPIFilHode.FilId = lFilId NO-ERROR.
IF NOT AVAILABLE VPIFilHode THEN
DO:
    RETURN " ** Ukjent VPIFilHode post (" + STRING(lFilId) + ").".
END.
FIND EkstVPILev OF VPIFilHode.
FIND LevBas NO-LOCK WHERE
    LevBas.LevNr = EkstVPILev.LevNr NO-ERROR.
ASSIGN
    cFilNavn = VPIFilHode.Katalog + "~\" + VPIFilHode.FilNavn
    ctmpKatalog = SESSION:TEMP-DIRECTORY
    iLevNr      = IF AVAILABLE LevBas THEN LevBas.LevNr ELSE 0
    .

RUN LesInnFil.
IF CAN-FIND(FIRST TT_Lagerstatus) THEN
    RUN NullstillKjedevare.
RUN OppdaterKjedevare.

RETURN.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-ErrorLogg) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ErrorLogg Procedure 
PROCEDURE ErrorLogg :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/*   OUTPUT TO VALUE("Error.Txt").                                                 */
/*     PUT UNFORMATTED                                                             */
/*       "Innlesning " + STRING(TODAY) + "  " + STRING(TIME,"HH:MM:SS") + "." SKIP */
/*       "Feil i fil: " + VPIFilHode.Katalog + "~\" + VPIFilHode.FilNavn skip      */
/*       .                                                                         */
/*     FOR EACH tt_Error:                                                          */
/*       PUT UNFORMATTED tt_Error.Tekst SKIP.                                      */
/*     END.                                                                        */
/*   OUTPUT CLOSE.                                                                 */
  IF SEARCH("Error.Txt") <> ? THEN
  DO:
    DEF VAR hInstance AS INT.

    RUN ShellExecute{&A} IN hpApi(0,
                                  "open",
                                  "notepad.exe",
                                  SEARCH("Error.Txt"),
                                  "",
                                  1,
                                  OUTPUT hInstance).

  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-LesInnFil) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LesInnFil Procedure 
PROCEDURE LesInnFil :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR piLinjeNr    AS INT  NO-UNDO.
  DEF VAR piAntFeil    AS INT  NO-UNDO.
  DEF VAR pcBkuFil     AS CHAR NO-UNDO.
  DEF VAR piLoop       AS INT  NO-UNDO.
  DEF VAR pcLinje      AS CHAR NO-UNDO.
  DEF VAR dDato        AS DATE NO-UNDO.
  DEF VAR cArticleNo   AS CHAR NO-UNDO.
  DEF VAR cEANNo       AS CHAR NO-UNDO.
  DEF VAR cArtName     AS CHAR NO-UNDO.
  DEF VAR cAkticWeb    AS CHAR NO-UNDO.
  DEF VAR cSendtTilWeb AS CHAR NO-UNDO.
  DEF VAR cButikkFil   AS CHAR NO-UNDO.
  DEF VAR cButikkFil2  AS CHAR NO-UNDO.

  /* Tømmer feillogg. */
  FOR EACH tt_Error:
    DELETE tt_Error.
  END.
  FOR EACH TT_Lagerstatus:
      DELETE TT_Lagerstatus.
  END.

  RUN TellOppLinjer.

  IF AVAILABLE TT_Lagerstatus THEN
      DELETE TT_Lagerstatus.

  ASSIGN
      piLinjeNr  = 1.
      iAntLinjer = 0
      .
  INPUT STREAM InnFil FROM VALUE(cFilNavn) NO-ECHO.
  LESERLINJER:
  REPEAT TRANSACTION:
    /* Leser linje fra filen */
    IMPORT STREAM InnFil UNFORMATTED pcLinje NO-ERROR.
    IF ERROR-STATUS:ERROR THEN
    DO:
        ASSIGN iAntLinjer = iAntLinjer + 1.
      DO piLoop = 1 TO ERROR-STATUS:NUM-MESSAGES:
          ASSIGN piAntFeil = piAntFeil + 1.
          ERROR-STATUS:GET-NUMBER(piLoop).          
          CREATE tt_Error.
          ASSIGN
            tt_Error.LinjeNr = piAntFeil
            tt_Error.Tekst   = "** Feil på linje " + STRING(iAntLinjer) + " " + ERROR-STATUS:GET-MESSAGE(piLoop).
            .
      END.
      NEXT LESERLINJER.
    END.
    /* Skipper Heading */
    IF iantLinjer = 0 THEN
    DO:
        iAntLinjer = 1.
        NEXT LESERLINJER.
    END.
    
    STATUS DEFAULT "Leser linje " + 
                   STRING(iAntLinjer) + 
                   " av " + 
                   STRING(iTotAntLinjer) + 
                   ".".
    
    IF NUM-ENTRIES(pcLinje,";") <> 7 THEN
    DO:
        ASSIGN piAntFeil = piAntFeil + 1.
        ERROR-STATUS:GET-NUMBER(1).          
        CREATE tt_Error.
        ASSIGN
          tt_Error.LinjeNr = piAntFeil
          tt_Error.Tekst   = "** Feil antall entries på linje " + STRING(iAntLinjer) + " " + ERROR-STATUS:GET-MESSAGE(1).
          .
        NEXT LESERLINJER.
    END.

    /* Det holder å logge artikkelen en gang. */
    IF CAN-FIND(FIRST TT_LagerStatus WHERE
                TT_LagerStatus.lArtikkelNr = dec(SUBstring(STRING(DEC(trim(ENTRY(1,pcLinje,";"),'"')),"9999999999999"),1,10))) THEN
        NEXT LESERLINJER.

    CREATE TT_Lagerstatus.
    
    ASSIGN 
        iAntLinjer                  = iAntLinjer + 1
        TT_Lagerstatus.ArticleNo    = dec(trim(trim(ENTRY(1,pcLinje,";"),'"')))                             
        TT_Lagerstatus.EANNo        = trim(trim(ENTRY(2,pcLinje,";"),'"'))                             
        TT_Lagerstatus.ArtName      = trim(trim(ENTRY(3,pcLinje,";"),'"'))                             
        TT_Lagerstatus.AktivWeb     = can-do('1,true,yes,ja,y,j',trim(trim(ENTRY(4,pcLinje,";"),'"')))                             
        TT_Lagerstatus.SendtTilWeb  = can-do('1,true,yes,ja,y,j',trim(trim(ENTRY(5,pcLinje,";"),'"')))
        TT_Lagerstatus.lArtikkelNr  = dec(SUBstring(STRING(DEC(trim(ENTRY(1,pcLinje,";"),'"')),"9999999999999"),1,10))
        TT_Lagerstatus.iStrKode     = int(SUBstring(STRING(DEC(trim(ENTRY(1,pcLinje,";"),'"')),"9999999999999"),11,3))
        NO-ERROR.
    
    /* Logger feil som følge av alfanumeriske verdier i de numeriske feltene. */
    IF ERROR-STATUS:ERROR THEN
    DO:
      DO piLoop = 1 TO ERROR-STATUS:NUM-MESSAGES:
          ASSIGN piAntFeil = piAntFeil + 1.
          ERROR-STATUS:GET-NUMBER(piLoop).          
          CREATE tt_Error.
          ASSIGN
            tt_Error.LinjeNr = piAntFeil
            tt_Error.Tekst   = "** Feil på linje - Alfanumeriske verdier i artikkelnr/strkode." + STRING(iAntLinjer) + " " + ERROR-STATUS:GET-MESSAGE(piLoop).
            .
      END.
      NEXT LESERLINJER.
    END.

    /* Denne størrelseskoden skal ikke ha lager. */
    IF TT_Lagerstatus.AktivWeb = FALSE THEN
    DO:
       DELETE TT_Lagerstatus.
       NEXT LESERLINJER.
    END.

    /* Artikkel = 0 skal ikke leses inn. */
    IF TT_Lagerstatus.lArtikkelNr = 0 THEN
    DO:
       DELETE TT_Lagerstatus.
       NEXT LESERLINJER.
    END.

    /* Henter artikkel */
    IF NOT CAN-find(ArtBas WHERE
        ArtBas.ArtikkelNr = TT_Lagerstatus.lArtikkelNr) THEN 
    DO:
       DELETE TT_Lagerstatus.
       NEXT LESERLINJER.
    END.
  END. /* LESERLINJER */
  INPUT STREAM InnFil CLOSE.

  /* Stempler posten som behandlet. */
  DO TRANSACTION:
      FIND CURRENT VPIFilHode EXCLUSIVE-LOCK.
      ASSIGN
          VPIFilHode.VPIFilStatus = 5
          .
  END.
  IF AVAILABLE VPIFilHode THEN
      FIND CURRENT VPIFilHode    NO-LOCK.

  IF CAN-FIND(FIRST tt_Error) THEN
    RUN ErrorLogg.

  ASSIGN
  pcBkuFil   = VPIFilHode.Katalog + "~\bku" + "\" + 
               VPIFilHode.FilNavn
  .

  /* LAGERFILEN */
  /* Sikrer at backup katalog finnes. */
  OS-CREATE-DIR value(VPIFilHode.Katalog + "~\bku").
  /* Flytter filen til backup katalog. */
  OS-COPY value(VPIFilHode.Katalog + "~\" + VPIFilHode.FilNavn) 
          value(pcBkuFil).
  /* Renser bort fil */
  IF SEARCH(pcBkuFil) <> ? THEN
  DO:
      /* Filen tas bort fra katalogen. */
      IF SEARCH(VPIFilHode.Katalog + "~\" + VPIFilHode.FilNavn) <> ? THEN
          OS-DELETE VALUE(VPIFilHode.Katalog + "~\" + VPIFilHode.FilNavn).
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-NullstillKjedevare) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE NullstillKjedevare Procedure 
PROCEDURE NullstillKjedevare :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  /* Alle varer skal nullstilles. Deretter leses lagerstatus filen inn. */                                   
  NULLSTILL:
  FOR EACH ArtBas EXCLUSIVE-LOCK WHERE
      ArtBas.KjedeVare = TRUE
      USE-INDEX kjedeVare:

      ASSIGN
          ArtBas.Kjedevare = FALSE.
  END. /* NULLSTILL */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-OppdaterKjedevare) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OppdaterKjedevare Procedure 
PROCEDURE OppdaterKjedevare :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  FOR EACH TT_Lagerstatus WHERE
      TT_Lagerstatus.lArtikkelNr > 0
    USE-INDEX  ArtNr:

    IF CAN-FIND(ArtBas WHERE
                ArtBas.ArtikkelNr = TT_LagerStatus.lArtikkelNr) THEN
    DO TRANSACTION:
        FIND ArtBas EXCLUSIVE-LOCK WHERE
            ArtBas.ArtikkelNr = TT_LagerStatus.lArtikkelNr NO-ERROR.
        IF AVAILABLE ArtBas THEN DO:
            ArtBas.KjedeVare = TT_LagerStatus.AktivWeb.

            STATUS DEFAULT "Behandler artikkel " + 
                           STRING(ArtBas.ArtikkelNr) + ' ' +
                           ArtBas.Beskr +
                           ".".

        END.

    END. /* TRANSACTION */
  END.

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
  INPUT STREAM InnFil FROM VALUE(VPIFilHode.Katalog + "~\" + VPIFilHode.FilNavn) NO-ECHO.
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

