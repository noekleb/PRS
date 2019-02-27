&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : xplukklisteinnles.p
    Purpose     : Innlesning av plukkliste fil fra 

    Syntax      :

    Description :

    Author(s)   :
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
DEF VAR cTekst        AS CHAR NO-UNDO.
DEF VAR bOk           AS LOG  NO-UNDO.
DEF VAR iButikkNr     AS INT  NO-UNDO.
DEF VAR iCL           AS INT  NO-UNDO.
DEF VAR iOppdOverf    AS INT  NO-UNDO. 
DEF VAR iEan          AS INT  NO-UNDO.

DEF STREAM InnFil.

{windows.i}

DEF {&New} TEMP-TABLE tt_plukkliste
    FIELD iRecType   AS INT
    FIELD ButikkNr   AS INT
    FIELD EanNr      AS CHAR FORMAT "x(30)"
    FIELD Dato       AS date
    FIELD Tid        AS INT FORMAT ">>>>>>>>9"
    FIELD LoggNr     AS INT FORMAT "->>>>>9"
    FIELD TransType  AS INT FORMAT ">9"
    FIELD TransTekst AS CHAR FORMAT "x(40)"
    FIELD BrukerId   AS CHAR FORMAT "x(15)"
    FIELD Antall     AS DEC  FORMAT "->,>>>,>>9.999"
    FIELD Kostpris   AS DEC  FORMAT "->>,>>>,>>9.999"
    FIELD Salgssum   AS DEC  FORMAT "->>,>>>,>>9.999"
    FIELD NyLagant   AS DEC  FORMAT "->,>>>,>>9.999"
    FIELD GmlLagAnt  AS DEC  FORMAT "->,>>>,>>9.999"
    FIELD Antall2    AS DEC  FORMAT "->,>>>,>>9.999"
    FIELD Varetekst  AS CHAR FORMAT "x(40)"
    FIELD Storl      AS CHAR FORMAT "x(10)"
    FIELD PlListeId  AS DEC 
    INDEX PLLISTEID PlListeId.

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

{syspara.i 5 1 1 iCL INT}
{syspara.i 11 5 2 iOppdOverf INT}
{syspara.i 11 5 5 iEan INT}

FIND VPIFilHode NO-LOCK WHERE
    VPIFilHode.FilId = lFilId NO-ERROR.
IF NOT AVAILABLE VPIFilHode THEN
DO:
    RETURN " ** Ukjent VPIFilHode post (" + STRING(lFilId) + ").".
END.
ASSIGN
    cFilNavn  = VPIFilHode.Katalog + "~\" + VPIFilHode.FilNavn
    .

RUN LesInnFil.

IF CAN-FIND(FIRST tt_Plukkliste) AND bOk = TRUE THEN
    RUN OppdaterPlukkliste.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-LesInnFil) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LesInnFil Procedure 
PROCEDURE LesInnFil :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes: 

Eksempel på fil. 3 linjer med sjekk på linjeantall:  
1 2 7028560300913 01/01/03 135000 0 8 "PLUKK" "tomn" 1 0.0 0 0.0 0 0 "2711/7 10" "10"
1 2 7028560300913 01/01/03 135009 0 8 "PLUKK" "tomn" 1 0.0 0 0.0 0 0 "NORDKAPP undertÃ¸y -" "10"
1 2 7028567004203 01/01/03 135028 0 8 "PLUKK" "tomn" 1 0.0 0 0.0 0 0 "2712/6  S" " S"
99 3
      
------------------------------------------------------------------------------*/
  DEF VAR piLinjeNr  AS INT  NO-UNDO.
  DEF VAR pcLinje    AS CHAR NO-UNDO.
  DEF VAR piAntFeil  AS INT  NO-UNDO.
  DEF VAR pcSep      AS CHAR NO-UNDO.
  DEF VAR iButikkNr  AS INT  NO-UNDO.

  ASSIGN
      pcSep = ";"
      .
  
  RUN TellOppLinjer.

  FIND LAST VPIFilLinje OF VPIFilHode NO-LOCK NO-ERROR.
  IF AVAILABLE VPIFilLinje THEN
      piLinjeNr = VPIFilLinje.LinjeNr + 1.
  ELSE
      piLinjeNr = 1.

  ASSIGN
      iAntLinjer = 0
      .

  INPUT STREAM InnFil FROM VALUE(cFilNavn) NO-ECHO.
  LESERLINJER:
  REPEAT:
    /* Leser linje fra filen */
    IMPORT STREAM InnFil UNFORMATTED pcLinje.

    ASSIGN
      iAntLinjer = iAntLinjer + 1.

    /* Håndterer kontrollrecord. Sjekker at antall linjer i filen er korrekt. */
    IF pcLinje BEGINS "99" /*int(ENTRY( 1,pcLinje,pcSep)) = 99*/ THEN
    DO:
        /* Feltseparator kan være feil åp siste record. */
        IF NUM-ENTRIES(pcLinje," ") = 2 THEN
            pcLinje = REPLACE(pcLinje," ",";").
        IF (iTotAntLinjer) = int(ENTRY( 2,pcLinje,pcSep))
            THEN bOk = TRUE.
        LEAVE LESERLINJER.
    END.

    IF NUM-ENTRIES(pcLinje,pcSep) < 18 THEN
    DO:
      ASSIGN
        piAntFeil = piAntFeil + 1
        cTekst = "Feil antall elementer på linje " + 
                 string(iAntLinjer) + "." +
                 " (Skal være 17, det er " + string(NUM-ENTRIES(pcLinje,pcSep)) + ").".
      PUBLISH "VPIFilLogg" (cTekst + chr(1) + "3").
    END.

    /* Posterer linjen */
    CREATE tt_Plukkliste.
    ASSIGN
        tt_Plukkliste.iRecType   = int(ENTRY( 1,pcLinje,pcSep))
        tt_Plukkliste.ButikkNr   = int(ENTRY( 2,pcLinje,pcSep))
        tt_Plukkliste.EanNr      = ENTRY( 3,pcLinje,pcSep)
        tt_Plukkliste.Dato       = DATE(ENTRY( 4,pcLinje,pcSep))
        tt_Plukkliste.Tid        = int(ENTRY( 5,pcLinje,pcSep))
        tt_Plukkliste.LoggNr     = int(ENTRY( 6,pcLinje,pcSep))
        tt_Plukkliste.TransType  = int(ENTRY( 7,pcLinje,pcSep))
        tt_Plukkliste.TransTekst = ENTRY( 8,pcLinje,pcSep)
        tt_Plukkliste.BrukerId   = ENTRY( 9,pcLinje,pcSep)
        tt_Plukkliste.Antall     = dec(ENTRY(10,pcLinje,pcSep))
        tt_Plukkliste.Kostpris   = dec(ENTRY(11,pcLinje,pcSep))
        tt_Plukkliste.Salgssum   = dec(ENTRY(12,pcLinje,pcSep))
        tt_Plukkliste.NyLagant   = dec(ENTRY(13,pcLinje,pcSep))
        tt_Plukkliste.GmlLagAnt  = dec(ENTRY(14,pcLinje,pcSep))
        tt_Plukkliste.Antall2    = dec(ENTRY(15,pcLinje,pcSep))
        tt_Plukkliste.Varetekst  = ENTRY(16,pcLinje,pcSep)
        tt_Plukkliste.Storl      = ENTRY(17,pcLinje,pcSep)
        tt_Plukkliste.PlListeId  = DEC(ENTRY(18,pcLinje,pcSep)) 
        piLinjeNr                = piLinjeNr  + 1
        .

    STATUS DEFAULT "Lese linje " + 
                   STRING(iAntLinjer) + 
                   " av " + 
                   STRING(iTotAntLinjer) + 
                   ".".
  END. /* LESERLINJER */
  INPUT STREAM InnFil CLOSE.

  /* Stempler posten som oppdatert. */
  DO TRANSACTION:
      FIND CURRENT VPIFilHode EXCLUSIVE-LOCK.
      ASSIGN
          VPIFilHode.VPIFilStatus = (IF bOk = TRUE 
                                       THEN 3
                                       ELSE 2)
          .
  END.
  IF AVAILABLE VPIFilHode THEN
      FIND CURRENT VPIFilHode    NO-LOCK.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-OppdaterPlukkliste) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OppdaterPlukkliste Procedure 
PROCEDURE OppdaterPlukkliste :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR dFDato      AS DATE NO-UNDO.
DEF VAR cKortNr     AS CHAR NO-UNDO.
DEF VAR plKundeNr   AS DEC  NO-UNDO.
DEF VAR plMedlemsNr AS DEC  NO-UNDO.
DEF VAR bOk         AS LOG  NO-UNDO.
DEF VAR piGruppeId  AS INT  NO-UNDO.
DEF VAR cMsgs       AS CHAR NO-UNDO.
DEF VAR cMedlemsNr  AS CHAR NO-UNDO.

FIND FIRST tt_Plukkliste NO-ERROR.
IF AVAILABLE tt_PlukkListe THEN
    ASSIGN
      iButikkNr = tt_Plukkliste.ButikkNr
      dFDato    = tt_Plukkliste.Dato.
ELSE RETURN.

PLUKKLISTE:
FOR EACH tt_Plukkliste
  BREAK BY tt_Plukkliste.PlListeId:
    IF FIRST-OF(tt_Plukkliste.PlListeId) THEN 
    DO:
      FIND FIRST PlListeHode NO-LOCK WHERE
        PlListeHode.PlListeId   = tt_PlukkListe.PlListeId
        NO-ERROR.
    END.

    IF AVAILABLE PlListeHode THEN 
    DO TRANSACTION:
        IF iEan = 0 THEN
            FIND Strekkode NO-LOCK WHERE
                Strekkode.Kode = tt_Plukkliste.EanNr NO-ERROR.
        ELSE IF iEan = 1 THEN
            FIND FIRST Strekkode NO-LOCK WHERE
                Strekkode.Bestillingsnummer = tt_Plukkliste.EanNr.

        IF NOT AVAILABLE Strekkode THEN
            NEXT PLUKKLISTE. /* Må logges */

        FIND PlListeLinje EXCLUSIVE-LOCK WHERE
            PlListeLinje.PlListeId  = PlListeHode.PlListeId AND
            PlListeLinje.ArtikkelNr = Strekkode.ArtikkelNr AND
            PlListeLinje.StrKode    = Strekkode.StrKode NO-ERROR.
        IF NOT AVAILABLE PlListeLinje THEN
            NEXT PLUKKLISTE. /* Må logges */

        /* Oppdaterer med plukket antall */
        ASSIGN
            PlListeLinje.AntallPlukket = PlListeLinje.AntallPlukket + tt_Plukkliste.Antall
            .

    END. /* TRANSACTION */

    DELETE tt_Plukkliste.
END. /* PLUKKLISTE */

/* Oppdaterer plukklistehode. */
DO TRANSACTION:
   FIND CURRENT PlListeHode EXCLUSIVE-LOCK.
   ASSIGN
       PlListeHode.DatoPlukket   = dFDato
       PlListeHode.AntallPlukket = 0
       .
   FOR EACH PlListeLinje OF PlListeHode NO-LOCK:
       ASSIGN
           PlListeHode.AntallPlukket = PlListeHode.AntallPlukket + PlListeLinje.AntallPlukket
           .
   END.
   FIND CURRENT PlListeHode NO-LOCK.
END. /* TRANSACTION */

/* Overfører plukklisten til overføringsordre. */
IF iOppdOverf = 0 THEN
OVERFOR:
DO:
   IF plListeHode.DatoPlukket <> ? AND
       PlListeHode.OverfortDato = ? THEN
   DO:
       NOT DYNAMIC-FUNCTION("RunProc","pllistehode_send_overforingsordre.p",    
                             STRING(PlListeHode.PlListeId)
                            ,?).
   END.
END. /* OVERFOR */


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

