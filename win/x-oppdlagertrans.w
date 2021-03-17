&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
/* Connected Databases 
          skotex           PROGRESS
*/


/* Temp-Table and Buffer definitions                                    */
DEFINE NEW SHARED TEMP-TABLE TT_OvBuffer NO-UNDO LIKE OvBuffer.



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : x-oppdlagertrans.w
    Purpose     : Oppdaterer lagertransaksjoner ilager og statistikker.

    Syntax      : 

    Description : 

    Author(s)   : Tom Nøkleby
    Created     : 
    Notes       :
    
    TN  12/3-99  Redusert til en loggfil pr. dag.
    TN  27/4-99  - Retuerer teller opp lageret.
                 - Returer påvirker ikke lenger internt forbruk.
    TN  4/6-99   Lagt inn kontroll på at varen har varekost.
    
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEF INPUT PARAMETER wProgram-Handle AS HANDLE NO-UNDO.
DEF INPUT PARAMETER wBatchNr        AS INT    NO-UNDO.

DEF VAR iBatchNr      AS INT  NO-UNDO.
DEF VAR iPlukkBatchNr AS INT  NO-UNDO.
DEF VAR wLoop1        AS INT  NO-UNDO.
DEF VAR wStDataObjekt AS CHAR NO-UNDO.
DEF VAR wStTypeListe  AS CHAR NO-UNDO.
DEF VAR wTotAntall AS DEC FORMAT "zzz,zzz,zz9" NO-UNDO.
DEF VAR wWork            AS DEC  NO-UNDO.
DEF VAR wWork1        AS DEC  NO-UNDO.
DEF VAR wWork2           AS DEC  NO-UNDO.
DEF VAR wWork3           AS DEC  NO-UNDO.
DEF VAR wStop            AS LOG  INITIAL FALSE NO-UNDO.
DEF VAR wDato            AS DATE NO-UNDO.
DEF VAR wTid             AS INT  NO-UNDO.
DEF VAR wSkjerm          AS CHAR NO-UNDO.
DEF VAR wTilbud          AS LOG  NO-UNDO.
DEF VAR wOk              AS LOG  NO-UNDO.
DEF VAR wOppdatertAntall AS INT  NO-UNDO.
DEF VAR wStartTid        AS INT  NO-UNDO.
DEF VAR wFerdigTid       AS INT  NO-UNDO.
DEF VAR wBruktTid        AS INT  NO-UNDO.
DEF VAR wImpFil          AS CHAR NO-UNDO.
DEF VAR wLogFil          AS CHAR NO-UNDO.
DEF VAR wOldPluFil       AS CHAR NO-UNDO.
DEF VAR wKatalog         AS CHAR NO-UNDO.
DEF VAR wLogKatalog      AS CHAR NO-UNDO.
DEF VAR wOkStatus        AS CHAR NO-UNDO.
DEF VAR wAntBest         AS INT  NO-UNDO.
DEF VAR wAntFeil         AS INT  NO-UNDO.
DEF VAR wEDB-System      AS CHAR NO-UNDO.
DEF VAR wTabell          AS CHAR NO-UNDO.
DEF VAR wVareKost        AS DEC  NO-UNDO.
DEF VAR wMva%            AS DEC  NO-UNDO.
DEF VAR wDefMva%         AS DEC  NO-UNDO.
DEF VAR wOpprettMedlem   AS LOG  NO-UNDO.
DEF VAR wTekst           AS CHAR NO-UNDO.
DEF VAR wMedlemsNr       AS DEC  NO-UNDO.
DEF VAR h_PrisKo         AS HANDLE NO-UNDO.
DEF VAR iTransNr         AS INT  NO-UNDO.
DEF VAR cBatchListe      AS CHAR NO-UNDO.
DEF VAR iLoop2           AS INT  NO-UNDO.
DEF VAR c2BatchListe     AS CHAR NO-UNDO.
DEF VAR piPris           AS DEC  NO-UNDO.
DEF VAR iCl              AS INT  NO-UNDO.
DEF VAR piStrKode        AS INT NO-UNDO.    
                             

/* Definerer stream */
DEF STREAM InnData.
DEF STREAM LoggData.

/* Definerer buffere */
DEF BUFFER bufLager     FOR Lager.
DEF BUFFER bufStLager   FOR StLager.
DEF BUFFER bufArtLag    FOR ArtLag.
DEF BUFFER lagTransLogg FOR TransLogg.
DEFINE BUFFER bufTransLogg FOR TransLogg.
DEF BUFFER bButiker     FOR Butiker.
DEF BUFFER bclButiker   FOR Butiker.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&IF DEFINED(EXCLUDE-RabUMva) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD RabUMva Procedure 
FUNCTION RabUMva RETURNS DECIMAL
  ( wMva% AS DEC )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SjekkStreng) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD SjekkStreng Procedure 
FUNCTION SjekkStreng RETURNS LOGICAL
  ( INPUT wTekst AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Procedure
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: CODE-ONLY COMPILE
   Temp-Tables and Buffers:
      TABLE: TT_OvBuffer T "NEW SHARED" NO-UNDO skotex OvBuffer
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Procedure ASSIGN
         HEIGHT             = 14.81
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

{runlib.i}

/* Kode for låsing av artikkelnummer ved overføring. */
{syspara.i 1 2 3 wEDB-System}
IF wEDB-System = "" THEN
  wEDB-System = "OVERFOR-LOCK".
{syspar2.i 1 2 3 wTabell}
IF wEDB-System = "" THEN
  wEDB-System = "ArtBas".

/* Opprette medlemmer automatisk for ukjente medlemskort. */
{syspara.i 3 3 10 wTekst}
IF CAN-DO("Ja,Yes,True,1",wTekst) THEN
  wOpprettMedlem = TRUE.
ELSE
  wOpprettMedlem = FALSE.

/* Default MVA% */
{syspara.i 2 1 4 wDefMva% DEC}

/* Sentrallager. */
{syspara.i 5 1 1 iCl INT}

/* Klargjørings dato og tid. */
ASSIGN
  wDato = TODAY
  wTid  = TIME.

RUN ByggLoggFilNavn.

RUN OppdaterLagerOgStat.

/* Posterer overføringstransaksjonene i en overføringsordre. */
IF iPlukkBatchNr > 0 AND
    CAN-FIND(FIRST Translogg WHERE
             TransLogg.BatchNr = iPlukkBatchNr) THEN
    RUN PosterOverfOrdre.

/* viser resultat til bruker */
/* if valid-handle(wProgram-Handle) and terminal <> "" then      */
/*   do:                                                         */
/*     if search(wLogFil) <> ? then                              */
/*       os-command no-wait value("notepad.exe") value(wLogFil). */
/*   end.                                                        */

IF VALID-HANDLE(h_PrisKo) THEN
    DELETE PROCEDURE h_prisko.

RETURN.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-ByggLoggFilNavn) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ByggLoggFilNavn Procedure 
PROCEDURE ByggLoggFilNavn :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR wDatoTid AS CHAR NO-UNDO.

/* Filnavn */
IF AVAILABLE SysPara THEN RELEASE SysPara.
{syspara.i 50 1 110 wLogFil}
IF wLogFil = "" THEN
  ASSIGN
    wLogFil = "oppdtransDDMMAAHHMM.log".

/* Ugyldig filnavn i parameteroppsett. */
IF INDEX(wLogFil,"DDMMAA") = 0 THEN
  RETURN "02".

/* Lager Dato og Tid strengen */
ASSIGN
  wDatoTid = STRING(DAY(TODAY),"99") + 
             string(MONTH(TODAY),"99") +
             substring(STRING(YEAR(TODAY),"9999"),3,2) /* + 
             substring(string(time,"HH:MM"),1,2) +
             substring(string(time,"HH:MM"),4,2) */.

/* Setter inn dato og klokkeslett i filnavnet. */
/*
OVERLAY(wLogFil, index(wLogFil,"DDMMAAHHMM"), 10, "CHARACTER") = wDatoTid.
*/
OVERLAY(wLogFil, INDEX(wLogFil,"DDMMAA"), 6, "CHARACTER") = wDatoTid.

/* /* Katalognavn */                                                */
/* if available SysPara then release SysPara.                       */
/* if opsys = "unix"                                                */
/*   then {syspar2.i 50 1 100 wLogKatalog}                          */
/* else {syspara.i 50 1 100 wLogKatalog}                            */
/*                                                                  */
/* if wLogKatalog = "" then                                         */
/*   wLogKatalog = if opsys = "unix" then "." else ".".             */
/* if substring(wLogKatalog,length(wLogKatalog),1) = "/" or         */
/*    substring(wLogKatalog,length(wLogKatalog),1) = "\" then       */
/*  wLogKatalog = substring(wLogKatalog,1,length(wLogKatalog) - 1). */
/*                                                                  */
/* /* Bygger full path til fil */                                   */
/* assign                                                           */
/*   wLogFil = wLogKatalog +                                        */
/*             (if opsys = "unix"                                   */
/*                then "/"                                          */
/*                else "\") +                                       */
/*             wLogFil.                                             */

RETURN "OK".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-KundeSalg) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE KundeSalg Procedure 
PROCEDURE KundeSalg :
/*------------------------------------------------------------------------------
  Purpose:     Posterer Kundessalg i KundeTrans.
               Dato for første og siste kjøp settes også.
  Parameters:  Forutsetter at det er en Translogg post tilgjengelig
               hvor Kundesnummeret er <> 0.
               
  Notes:       
------------------------------------------------------------------------------*/
  DEF BUFFER bKundeTrans FOR KundeTrans.
  DEF BUFFER bKundeSaldo FOR KundeSaldo.

  /* Sjekker at det er et gyldig Kunde.   */
  /* Salg på Kundemet posteres uansett... */
  FIND Kunde NO-LOCK WHERE
      Kunde.KundeNr = TransLogg.KundNr NO-ERROR.
  IF NOT AVAILABLE Kunde THEN
      RETURN "AVBRYT". /* Ingen Kunde å postere på */
                                            
  /* Sjekker om den er postert fra før. */
  IF CAN-FIND(bKundeTrans WHERE
              bKundeTrans.KundeNr      = TransLogg.KundNr  AND
              bKundeTrans.Butik        = TransLogg.Butik   AND
              bKundeTrans.TransNr      = TransLogg.TransNr AND
              bKundeTrans.SeqNr        = TransLogg.SeqNr) THEN
    RETURN "AVBRYT".

  POSTERING-TRANS:
  DO FOR bKundeTrans TRANSACTION:
      CREATE bKundeTrans.
    ASSIGN
        bKundeTrans.KundeNr      = TransLogg.KundNr
        bKundeTrans.Butik        = TransLogg.Butik
        bKundeTrans.TransNr      = TransLogg.TransNr
        bKundeTrans.SeqNr        = TransLogg.SeqNr
        .
    ASSIGN
      bKundeTrans.BatchNr        = TransLogg.BatchNr
      bKundeTrans.TTId           = TransLogg.TTId
      bKundeTrans.TBId           = TransLogg.TBId
      bKundeTrans.ArtikkelNr     = TransLogg.ArtikkelNr
      bKundeTrans.LevNr          = TransLogg.LevNr
      bKundeTrans.BongId         = TransLogg.BongId
      bKundeTrans.BongLinjeNr    = TransLogg.BongLinjeNr
      bKundeTrans.KassaNr        = TransLogg.KassaNr
      bKundeTrans.Vg             = TransLogg.Vg
      bKundeTrans.LopNr          = TransLogg.LopNr
      bKundeTrans.Storl          = TransLogg.Storl
      bKundeTrans.Antall         = TransLogg.Antall
      bKundeTrans.Pris           = TransLogg.Pris
      bKundeTrans.RabKr          = TransLogg.RabKr
      bKundeTrans.Mva            = TransLogg.Mva
      bKundeTrans.Dato           = TransLogg.Dato
      bKundeTrans.Tid            = TransLogg.Tid
      bKundeTrans.VVarekost      = TransLogg.VVarekost
      bKundeTrans.SattVVareKost  = TransLogg.SattVVareKost
      bKundeTrans.KortNr         = IF TransLogg.KortType = 1 /* Kundeskort */
                                   THEN TransLogg.KortNr
                                   ELSE ""
      bKundeTrans.ForsNr         = TransLogg.ForsNr
      .

  END. /* POSTERING-TRANS */

  POSTERING-SALDO:
  DO FOR bKundeSaldo WHILE TRUE TRANSACTION:
    FIND bKundeSaldo EXCLUSIVE-LOCK WHERE
        bKundeSaldo.KundeNr = TransLogg.KundNr AND
        bKundeSaldo.Butik     = TransLogg.Butik
        NO-ERROR NO-WAIT.
    /* Posten holdes av en annen, vi forsøker igjen. */
    IF LOCKED bKundeSaldo THEN
        NEXT POSTERING-SALDO.
    /* Oppretter ny post. */
    IF NOT AVAILABLE bKundeSaldo THEN
    DO:
        CREATE bKundeSaldo.
        ASSIGN
            bKundeSaldo.KundeNr = TransLogg.KundNr
            bKundeSaldo.Butik   = TransLogg.Butik
            .
    END.
    /* Posterer 1.gangs kjøp. */
    IF bKundeSaldo.ForsteDato = ? THEN
      ASSIGN
        bKundeSaldo.ForsteDato = TransLogg.Dato
        bKundeSaldo.ForsteTid  = TransLogg.Tid
        .
    /* Posterer siste gangs kjøp. */
    ASSIGN
        bKundeSaldo.DatoSiste  = TransLogg.Dato
        bKundeSaldo.SisteTid   = TransLogg.Tid
        .
    /* Oppdaterer saldo. */
    ASSIGN
        bKundeSaldo.Saldo      = bKundeSaldo.Saldo      + 
                                  ((TransLogg.Pris - TransLogg.RabKr) * TransLogg.Antall) 
        bKundeSaldo.TotaltKjop = bKundeSaldo.TotaltKjop + 
                                  ((TransLogg.Pris - TransLogg.RabKr) * TransLogg.Antall)
        .

    LEAVE POSTERING-SALDO.    
  END. /* POSTERING-SALDO */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-LoggFeilITrans) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LoggFeilITrans Procedure 
PROCEDURE LoggFeilITrans :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER ipFeilKode AS INT.
  
  DEF VAR wTekst AS CHAR NO-UNDO.
  
  ASSIGN
    wAntFeil = wAntFeil + 1.

  {syspara.i 100 1 ipFeilKode wTekst}
      
  IF wTekst = "" AND ipFeilKode = 20 THEN
    wTekst = "Overføringstransaksjon på vare som IKKE er lagerstyrt.".

/*   OUTPUT STREAM LoggData to value(wLogFil) no-echo append.                */
/*   put stream LoggData unformatted                                         */
/*   " Batch: " TransLogg.BatchNr                                            */
/*   " TransNr: " TransLogg.TransNr                                          */
/*   " ArtikkelNr: " TransLogg.ArtikkelNr                                    */
/*   " Vg/LøpeNr: " string(TransLogg.Vg) + "/" + string(TransLogg.LopNr) " " */
/*   wTekst skip.                                                            */
/*   OUTPUT STREAM LoggData close.                                           */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-MedlemsSalg) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE MedlemsSalg Procedure 
PROCEDURE MedlemsSalg :
/*------------------------------------------------------------------------------
  Purpose:     Posterer medlemssalg i MedTrans.
               Dato for første og siste kjøp settes også.
  Parameters:  Forutsetter at det er en Translogg post tilgjengelig
               hvor medlemsnummeret er <> 0.
               
  Notes:       
------------------------------------------------------------------------------*/
  DEF BUFFER bMedTrans    FOR MedTrans.
  DEF BUFFER bMedlemSaldo FOR MedlemSaldo.

  /* Sjekker at det er et gyldig medlem.   */
  /* Salg på medlemmet posteres uansett... */
  FIND Medlem NO-LOCK WHERE
      Medlem.MedlemsNr = TransLogg.MedlemsNr NO-ERROR.
  IF NOT AVAILABLE Medlem THEN
      RETURN "AVBRYT". /* Ingen medlem å postere på */
                                            
  /* Transaksjonen kan ha blitt oppdatert tidligere. */
  IF CAN-FIND(bMedTrans WHERE
              bMedTrans.MedlemsNr = TransLogg.MedlemsNr AND
              bMedTrans.Butik     = TransLogg.Butik     AND
              bMedTrans.TransNr   = TransLogg.TransNr   AND
              bMedTrans.SeqNr     = TransLogg.SeqNr) THEN
    RETURN "AVBRYT".

  POSTERING-TRANS:
  DO FOR bMedTrans TRANSACTION:
    CREATE bMedTrans.
    ASSIGN
        bMedTrans.MedlemsNr      = TransLogg.MedlemsNr
        bMedTrans.Butik          = TransLogg.Butik
        bMedTrans.TransNr        = TransLogg.TransNr
        bMedTrans.SeqNr          = TransLogg.SeqNr
        .
    ASSIGN
      bMedTrans.BatchNr        = TransLogg.BatchNr
      bMedTrans.TTId           = TransLogg.TTId
      bMedTrans.TBId           = TransLogg.TBId
      bMedTrans.ArtikkelNr     = TransLogg.ArtikkelNr
      bMedTrans.LevNr          = TransLogg.LevNr
      bMedTrans.BongId         = TransLogg.BongId
      bMedTrans.BongLinjeNr    = TransLogg.BongLinjeNr
      bMedTrans.KassaNr        = TransLogg.KassaNr
      bMedTrans.Vg             = TransLogg.Vg
      bMedTrans.LopNr          = TransLogg.LopNr
      bMedTrans.Storl          = TransLogg.Storl
      bMedTrans.Antall         = TransLogg.Antall
      bMedTrans.Pris           = TransLogg.Pris
      bMedTrans.RabKr          = TransLogg.RabKr
      bMedTrans.Mva            = TransLogg.Mva
      bMedTrans.Dato           = TransLogg.Dato
      bMedTrans.Tid            = TransLogg.Tid
      bMedTrans.VVarekost      = TransLogg.VVarekost
      bMedTrans.SattVVareKost  = TransLogg.SattVVareKost
      bMedTrans.KortNr         = IF TransLogg.KortType = 3 /* Medlemskort */
                                   THEN TransLogg.KortNr
                                   ELSE ""
      bMedTrans.ForsNr         = TransLogg.ForsNr
      .

  END. /* POSTERING-TRANS */

  POSTERING-SALDO:
  DO FOR bMedlemSaldo WHILE TRUE TRANSACTION:
    FIND bMedlemSaldo EXCLUSIVE-LOCK WHERE
        bMedlemSaldo.MedlemsNr = TransLogg.MedlemsNr AND
        bMedlemSaldo.Butik     = TransLogg.Butik
        NO-ERROR NO-WAIT.
    /* Posten holdes av en annen, vi forsøker igjen. */
    IF LOCKED bMedlemSaldo THEN
        NEXT POSTERING-SALDO.
    /* Oppretter ny post. */
    IF NOT AVAILABLE bMedlemSaldo THEN
    DO:
        CREATE bMedlemSaldo.
        ASSIGN
            bMedlemSaldo.MedlemsNr = TransLogg.MedlemsNr
            bMedlemSaldo.Butik     = TransLogg.Butik
            .
    END.
    /* Posterer 1.gangs kjøp. */
    IF bMedlemSaldo.ForsteDato = ? THEN
      ASSIGN
        bMedlemSaldo.ForsteDato = TransLogg.Dato
        bMedlemSaldo.ForsteTid  = TransLogg.Tid
        .
    /* Posterer siste gangs kjøp. */
    ASSIGN
        bMedlemSaldo.DatoSiste  = TransLogg.Dato
        bMedlemSaldo.SisteTid   = TransLogg.Tid
        .
    /* Oppdaterer saldo. */
    ASSIGN
        bMedlemSaldo.Saldo      = bMedlemSaldo.Saldo      + 
                                  ((TransLogg.Pris - TransLogg.RabKr) * TransLogg.Antall) 
        bMedlemSaldo.TotaltKjop = bMedlemSaldo.TotaltKjop + 
                                  ((TransLogg.Pris - TransLogg.RabKr) * TransLogg.Antall)
        .

    LEAVE POSTERING-SALDO.    
  END. /* POSTERING-SALDO */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-OppdaterLagerOgStat) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OppdaterLagerOgStat Procedure 
PROCEDURE OppdaterLagerOgStat :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR wOppdatertAntall AS INT  NO-UNDO.
DEF VAR wLestAntall      AS INT  NO-UNDO.
DEF VAR wStartTid        AS INT  NO-UNDO.
DEF VAR wFerdigTid       AS INT  NO-UNDO.
DEF VAR wBruktTid        AS INT  NO-UNDO.
DEF VAR wDataObjekt      AS CHAR NO-UNDO.

DEF BUFFER bufBatchLogg FOR BatchLogg.

ASSIGN 
  wAntBest  = 0
  wOkStatus = "AVBRYT".

/* Åpner stream til datafil */
/* OUTPUT STREAM LoggData to value(wLogFil) no-echo append.                              */
/* /* Åpner ningsmelding i loggfil */                                                    */
/* put stream LoggData unformatted                                                       */
/*   " " skip                                                                            */
/*   "Batch: " + (if wBatchNr = ? then "?" else string(wBatchNr)) skip                   */
/*   " -----------------------------------------------------------------------" skip     */
/*   " Oppdaterer lager og statistikker  " skip                                          */
/*   " " string(today) " " string(time,"HH:MM:SS") " " userid("dictdb") skip             */
/*   " -----------------------------------------------------------------------" skip(1). */
/* OUTPUT STREAM LoggData close.                                                         */

DO WITH FRAME DEFAULT-FRAME:
  IF NOT VALID-HANDLE(h_PrisKo) THEN
      RUN PrisKo.p PERSISTENT SET h_PrisKo.

  /* Startet info */
  ASSIGN
    wStartTid = TIME.
  IF VALID-HANDLE(wProgram-Handle) THEN
    RUN StartInfo IN wProgram-Handle (INPUT TODAY, INPUT wStartTid).

  /* Alle ikke ferdigbehandlede batchloger */
  IF wBatchNr = ? THEN
  BATCHLOGG1:
  FOR EACH BatchLogg NO-LOCK WHERE
    BatchLogg.OppdStatus > 1 AND
    BatchLogg.OppdStatus < 4:

    /* Sjekker at batchen 100% sikker ikke er låst. */
    DO FOR bufBatchLogg TRANSACTION:
      FIND bufBatchLogg EXCLUSIVE-LOCK WHERE
        bufBatchLogg.BatchNr = BatchLogg.BatchNr NO-WAIT NO-ERROR.

      /* Er den låst, tar vi den i neste runde. */
      IF LOCKED(bufBatchLogg) THEN 
          LEAVE BATCHLOGG1.
    END. /* TRANSACTION */

    wAntFeil  = 0.
 
    OPPDAT_TRANS1:
    FOR EACH bufTransLogg NO-LOCK WHERE
      bufTransLogg.BatchNr = BatchLogg.BatchNr AND
      bufTransLogg.Postert = FALSE
      BREAK BY bufTransLogg.BatchNr
            BY bufTransLogg.Postert
            BY bufTransLogg.Butik
            BY bufTransLogg.TransNr TRANSACTION:
        
        /* Er posten låset, eller behandles i et annet program, skal den ikke behandles nå, men tas tak i senere. */
        FIND TransLogg EXCLUSIVE-LOCK WHERE
          RECID(TransLogg) = recid(bufTransLogg) NO-ERROR NO-WAIT.
        IF NOT AVAILABLE TransLogg THEN
            NEXT OPPDAT_TRANS1.
            
        /* Kampanje MixMatch */
        IF TransLogg.TTId = 109 THEN
        DO:
            /* Posterer i statistikkene.*/
            RUN PosterStatistikk.
            IF RETURN-VALUE = "UNDO" THEN
              UNDO OPPDAT_TRANS1, NEXT OPPDAT_TRANS1.
            ELSE IF RETURN-VALUE = "NEXT" THEN
              NEXT OPPDAT_TRANS1.      
            /* Kvitterer ut transaksjonen                   */
            ASSIGN
              TransLogg.Postert     = TRUE
              TransLogg.PostertDato = TODAY
              TransLogg.PostertTid  = TIME
              TransLogg.FeilKode    = 0.
        END. ELSE 
        DO:
            {x-oppdlagertrans.i &Blokk = 1}  
        END.
    END. /* OPPDAT_TRANS TRANSATION */
    
    IF CAN-FIND(FIRST TransLogg WHERE
                  TransLogg.BatchNr = BatchLogg.BatchNr AND
                  TransLogg.Postert = FALSE) THEN
      RUN batchstatus.p(BatchLogg.BatchNr, 3). /* Ikke behandlede poster ligger igjen */
    ELSE
      RUN batchstatus.p(BatchLogg.BatchNr, 4). /* Alle poster er oppdatert */
  END. /* BATCHLOGG1 */
  ELSE 
    BATCHLOGG2:
    FOR EACH BatchLogg NO-LOCK WHERE
      BatchLogg.BatchNr = wBatchNr:
     
      wAntFeil  = 0.
  
      OPPDAT_TRANS2:
      FOR EACH bufTransLogg NO-LOCK WHERE
        bufTransLogg.BatchNr = BatchLogg.BatchNr AND
        bufTransLogg.Postert = FALSE
        BREAK BY bufTransLogg.BatchNr
              BY bufTransLogg.Postert
              BY bufTransLogg.Butik
              BY bufTransLogg.TransNr TRANSACTION:
                
        /* Er posten låset, eller behandles i et annet program, skal den ikke behandles nå, men tas tak i senere. */
        FIND TransLogg EXCLUSIVE-LOCK WHERE
          RECID(TransLogg) = recid(bufTransLogg) NO-ERROR NO-WAIT.
        IF NOT AVAILABLE TransLogg THEN
            NEXT OPPDAT_TRANS2.
                
        /* Kampanje MixMatch */
        IF TransLogg.TTId = 109 THEN
        DO:
            /* Posterer i statistikkene.*/
            RUN PosterStatistikk.
            IF RETURN-VALUE = "UNDO" THEN
              UNDO OPPDAT_TRANS2, NEXT OPPDAT_TRANS2.
            ELSE IF RETURN-VALUE = "NEXT" THEN
              NEXT OPPDAT_TRANS2.      
            /* Kvitterer ut transaksjonen                   */
            ASSIGN
              TransLogg.Postert     = TRUE
              TransLogg.PostertDato = TODAY
              TransLogg.PostertTid  = TIME
              TransLogg.FeilKode    = 0.
        END. ELSE DO:
            {x-oppdlagertrans.i &Blokk = 2}  
        END.
      END. /* OPPDAT_TRANS TRANSATION */

      IF CAN-FIND(FIRST TransLogg WHERE
                    TransLogg.BatchNr = wBatchNr AND
                    TransLogg.Postert = FALSE) THEN
        RUN batchstatus.p(BatchLogg.BatchNr, 3). /* Ikke behandlede poster ligger igjen */
      ELSE
        RUN batchstatus.p(BatchLogg.BatchNr, 4). /* Alle poster er oppdatert */
    END. /* BATCHLOGG2 */

  /* Løser eventuelle problemer med locking. */
  IF AVAILABLE TransLogg THEN
    RELEASE TransLog.
  IF AVAILABLE ArtBas THEN
    RELEASE ArtBas.
  IF AVAILABLE ArtLag THEN
    RELEASE Artlag.
  IF AVAILABLE Lager THEN
    RELEASE Lager.
  IF AVAILABLE StHode THEN
    RELEASE StHode.
  IF AVAILABLE StLinje THEN
    RELEASE StLinje.    
  IF AVAILABLE KonvReg THEN
    RELEASE KonvReg.
  IF AVAILABLE MedTrans THEN
    RELEASE MedTrans.
  IF AVAILABLE KundeTrans THEN
    RELEASE KundeTrans.

  /* Brukt info */
  ASSIGN
    wOkStatus  = "OK"
    wFerdigTid = TIME
    wBruktTid  = wFerdigTid - wStartTid.
  IF VALID-HANDLE(wProgram-Handle) THEN
    RUN BruktInfo IN wProgram-Handle (INPUT TODAY, INPUT wFerdigTid, INPUT wBruktTid).

  IF VALID-HANDLE(h_PrisKo) THEN
      DELETE PROCEDURE h_PrisKo.
END. /* FRAME */  

/* Flagger batchen med overføringer fra plukklager for oppdatering. */
/* Eller kaster den tomme batchen.                                  */
IF iPlukkBatchNr > 0 THEN
DO:
    FIND FIRST TransLogg NO-LOCK WHERE
      Translogg.BatchNr = iPlukkBatchNr NO-ERROR.
    IF NOT AVAILABLE TRANSLOGG THEN
    DO TRANSACTION:
      FIND BatchLogg EXCLUSIVE-LOCK WHERE
        BatchLogg.BAtchNr = iPlukkBatchNr NO-ERROR.
      IF AVAILABLE BatchLogg THEN
        DELETE BAtchLogg.
    END. /* TRANSACTION */
    ELSE
      RUN batchstatus.p (iPlukkBatchNr, 2).
END.

/* Ferdig melding i loggfil */
/* Åpner stream til datafil */
/* OUTPUT STREAM LoggData to value(wLogFil) no-echo append.                              */
/* put stream LoggData unformatted                                                       */
/*   " " skip                                                                            */
/*   " -----------------------------------------------------------------------" skip     */
/*   " Antall transksjoner: " string(wTotAntall,"zzzzzzz9") skip                         */
/*   " Antall oppdatert   : " string(wOppdatertAntall,"zzzzzzz9") skip                   */
/*   " Antall med feilkode: " string(wTotAntall - wOppdatertAntall,"zzzzzzz9") skip      */
/*   " Ferdigstatus       : " wOkStatus skip                                             */
/*   " Brukt tid          : " string(wBruktTid,"HH:MM:SS") skip                          */
/*   " Ferdig             : " string(today) " " string(time,"HH:MM:SS") skip             */
/*   " -----------------------------------------------------------------------" skip(1). */
/* OUTPUT STREAM LoggData close.                                                         */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Plukklager) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Plukklager Procedure 
PROCEDURE Plukklager :
/*------------------------------------------------------------------------------
  Purpose:     Oppretter overføringsposter til butikken det selges fra, 
               fra den butikk som er satt opp som plukkbutikk.
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR piSeqNr AS INT NO-UNDO.

  IF iPlukkBatchNr = 0 THEN
      RUN batchlogg.p (PROGRAM-NAME(1),
                 "Plukklager'er " +
                 string(TODAY) +
                 " " +
                 string(TIME,"HH:MM") +
                 " " +
                 USERID("dictdb"),
                 OUTPUT iPlukkBatchNr).

  /* Setter transaksjonsnummer  */
  IF iTransNr = 0 THEN
    DO:
      FIND LAST lagTransLogg WHERE
        lagTransLogg.Butik = (IF Translogg.Antall > 0
                                THEN Butiker.PlukkButik
                                ELSE Translogg.Butik)
        USE-INDEX TransLogg NO-ERROR.
      IF AVAILABLE lagTransLogg THEN
        iTransNr = lagTransLogg.TransNr + 1.
      ELSE
        iTransNr = 1.
    END.
  ELSE
    iTransNr = iTransNr + 1.

  piSeqNr = 1.
  SEQNR:
  DO WHILE TRUE: 
    piSeqNr = piSeqNr + 1.
    IF NOT CAN-FIND(TransLogg WHERE
                    TransLogg.Butik   = (IF Translogg.Antall > 0
                                          THEN Butiker.PlukkButik
                                          ELSE Translogg.Butik) AND
                    TransLogg.TransNr = TransLogg.TransNr AND
                    TransLogg.SeqNr   = piSeqNr) THEN
        LEAVE SEQNR.
  END. /* SEQNR */

  /* Oppretter TransLogg */
  CREATE lagTransLogg.

  ASSIGN lagTransLogg.Butik         = IF Translogg.Antall > 0
                                        THEN Butiker.PlukkButik
                                        ELSE Translogg.Butik
         lagTransLogg.TransNr       = iTransNr
         lagTransLogg.SeqNr         = piSeqNr.
  ASSIGN lagTransLogg.BatchNr       = iPlukkBatchNr
         lagTransLogg.BongTekst     = TransLogg.BongTekst
         lagTransLogg.Kode          = TransLogg.Kode
         lagTransLogg.vVarekost     = Translogg.VVareKost
         lagTransLogg.SattVVareKost = TRUE
         lagTransLogg.KundNr        = 0
         lagTransLogg.TTId          = 6
         lagTransLogg.TBId          = 1
         lagTransLogg.ArtikkelNr    = TransLogg.ArtikkelNr
         lagTransLogg.LevNr         = Translogg.LevNr
         lagTransLogg.BongId        = 0
         lagTransLogg.BongLinjeNr   = 0
         lagTransLogg.KassaNr       = 0
         lagTransLogg.Vg            = TransLogg.Vg
         lagTransLogg.LopNr         = TransLogg.LopNr
         lagTransLogg.Storl         = TransLogg.Storl
         lagTransLogg.TilStorl      = TransLogg.Storl
         lagTransLogg.Dato          = TransLogg.Dato
         lagTransLogg.Tid           = TransLogg.Tid
         lagTransLogg.BestNr        = 0
         lagTransLogg.Postert       = FALSE
         lagTransLogg.OvButik       = IF Translogg.Antall > 0
                                        THEN TransLogg.butik
                                        ELSE Butiker.PlukkButik
         lagTransLogg.Plukket       = TRUE /* Skal ikke ut på plukkliste */
         lagTransLogg.Antall        = abs(Translogg.Antall)
         lagTransLogg.Pris          = Translogg.VVareKost
         lagTransLogg.RabKr         = 0
         lagTransLogg.Mva           = 0.
         .                                             

  /* Slipper posten */
  RELEASE lagTranslogg.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-PosterArtLag) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PosterArtLag Procedure 
PROCEDURE PosterArtLag PRIVATE :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
      OPPDAT_ARTLAG:
      DO:
        /* Er det en gyldig størrelse */
        FIND FIRST StrTStr NO-LOCK WHERE
          StrTStr.StrTypeId = ArtBas.StrTypeId AND
          StrTStr.SoStorl   = TransLogg.Storl NO-ERROR.
        IF NOT AVAILABLE StrTStr THEN
          DO:
            ASSIGN TransLogg.FeilKode = 5. /* Størrelsen ligger ikke i størrelsesdefinisjonen. */
            RUN LoggFeilITrans (INPUT TransLogg.FeilKode).
            /* return "UNDO". -- Oppdateres alikevel ---*/
          END.
        
        FIND StrKonv NO-LOCK WHERE
          StrKonv.Storl = TransLogg.Storl NO-ERROR.
        IF NOT AVAILABLE StrKonv THEN
          DO:
            /* Gjøres også før transen behandles. Dvs unødvendig her, men... */
            FIND LAST StrKonv NO-LOCK USE-INDEX StrKode.
            IF AVAILABLE StrKonv THEN
                piStrKode = StrKonv.StrKode + 1.
            ELSE
                piStrKode = 1.
            CREATE StrKonv.
            ASSIGN
                StrKonv.Storl   = Translogg.Storl
                StrKonv.StrKode = piStrKode
                StrKonv.Merknad = "Opprettet fra x-oppdlagertrans.w"
                .
          END.

        /* Hvis ikke ArtLag posten finnes opprettes den. Men transaksjonen */
        /* flagges med en kode for at lagerpost ble opprettet.             */
        FIND ArtLag EXCLUSIVE-LOCK WHERE
            Artlag.ArtikkelNr = dec(Translogg.ArtikkelNr) AND
            ArtLag.Butik      = TransLogg.Butik AND
            ArtLag.StrKode    = StrKonv.StrKode NO-ERROR.

        /* Alternativt oppslag */    
        IF NOT AVAILABLE ArtLAg THEN
        FIND ArtLag EXCLUSIVE-LOCK WHERE
          Artlag.artikkelnr = ArtBas.artikkelnr AND
          ArtLag.Storl  = TransLogg.Storl AND
          Artlag.Butik  = Translogg.Butik NO-ERROR.
        /* Retter StrKode feltet hvis det er feil. */
        IF AVAILABLE ArtLag AND AVAILABLE StrKonv AND ArtLag.StrKode = ? THEN
          ArtLag.StrKode = StrKonv.StrKode.        

        IF NOT AVAILABLE ArtLag THEN
          DO:
            ASSIGN TransLogg.FeilKode = 6. /* Ukjent artlag. Opprettet ved postering på lager! */
            RUN LoggFeilITrans (INPUT TransLogg.FeilKode).
            CREATE ArtLag.
            ASSIGN
              ArtLag.Butik      = TransLogg.Butik
              Artlag.ArtikkelNr = TransLogg.ArtikkelNr
              ArtLag.StrKode    = IF AVAILABLE StrKonv
                                    THEN StrKonv.StrKode
                                    ELSE 0
              ArtLag.Storl      = TransLogg.Storl
              ArtLag.Vg         = TransLogg.Vg 
              ArtLag.LopNr      = TransLogg.LopNr 
              NO-ERROR.     
            /* Blir det alikevel feil, skal ikke transen oppdateres men feilmerkes. */
            IF ERROR-STATUS:ERROR THEN
            DO:
                ASSIGN TransLogg.FeilKode = 16. /* Ukjent artlag. Opprettet ved postering på lager! */
                RUN LoggFeilITrans (INPUT TransLogg.FeilKode).
                IF AVAILABLE Artlag AND NEW Artlag THEN
                    DELETE Artlag.
                RETURN "UNDO Create".
            END.
          END.
          
        CASE TransLogg.TTId:
          WHEN 1 THEN /* Varesalg */
          DO:
              ASSIGN
                ArtLag.LagAnt   = ArtLag.LagAnt     - TransLogg.Antall
                ArtLag.AntSolgt = ArtLag.AntSolgt + TransLogg.Antall
                ArtLag.AntRab   = ArtLAg.AntRab +
                                  (IF TransLogg.RabKr <> 0
                                     THEN TransLogg.Antall 
                                     ELSE 0).
          END.
          WHEN 2 THEN /* Brekkasje */
            ASSIGN
              ArtLag.LagAnt   = ArtLag.LagAnt   - TransLogg.Antall
              ArtLag.BrekkAnt = ArtLag.BrekkAnt + TransLogg.Antall.
          WHEN 3 THEN /* Kundereklamasjon */
            ASSIGN
              /* TN 14/5-02 ArtLag.LagAnt  = ArtLag.LagAnt  + TransLogg.Antall */
              ArtLag.ReklAnt = ArtLag.ReklAnt + TransLogg.Antall
              /* Korrigerer salget ved reklamasjon */
              ArtLag.AntSolgt = ArtLag.AntSolgt + TransLogg.Antall.
          WHEN 4 THEN /* Lagerreklamasjon */
            ASSIGN
              ArtLag.LagAnt   = ArtLag.LagAnt   - TransLogg.Antall
              ArtLag.ReklLAnt = ArtLag.ReklLAnt + TransLogg.Antall.
          WHEN 5 THEN /* Varekjøp */
            ASSIGN
              ArtLag.LagAnt  = ArtLag.LagAnt  + TransLogg.Antall
              ArtLag.KjopAnt = ArtLag.KjopAnt + TransLogg.Antall.          
          WHEN 6 THEN /* Overføring */
            DO:
              ASSIGN  /*Inn i butikk. NB: Translogg.Antall er negativ postert i Translogg. */
                ArtLag.LagAnt  = ArtLag.LagAnt - TransLogg.Antall
                ArtLag.OvAnt   = ArtLag.OvAnt  - TransLogg.Antall.
              
              IF TransLogg.TBId <> 2 THEN
              MOTTAGENDE_BUT:
              DO:
                  /* Justerer fra butikken. */  
                  /* NB: i w-gridlager.w lagres fra størrelsen i TransLogg.TilStorl. */
                  FIND bufArtLag EXCLUSIVE-LOCK WHERE
                      bufArtlag.ArtikkelNr = ArtLag.ArtikkelNr AND
                      bufArtLag.Butik      = TransLogg.OvButik AND
                      bufArtLag.StrKode    = ArtLag.StrKode NO-ERROR NO-WAIT.
                  IF NOT AVAILABLE bufArtLag AND NOT LOCKED bufArtLag THEN 
                    FIND FIRST bufArtLag EXCLUSIVE-LOCK WHERE
                      bufArtlag.Vg         = ArtLag.Vg AND
                      bufArtLag.LopNr      = ArtLag.LopNr AND 
                      bufArtLag.Storl      = ArtLag.Storl AND 
                      bufArtLag.Butik      = TransLogg.OvButik  NO-ERROR NO-WAIT.                                                
                  IF LOCKED bufArtLag THEN
                    DO:
                      ASSIGN TransLogg.FeilKode = 8. /* Artlag låst i mottagende butikk! */
                      RUN LoggFeilITrans (INPUT TransLogg.FeilKode).
                      RETURN "UNDO Overfor".
                    END.
                  IF NOT AVAILABLE bufArtLag THEN
                    DO:
                      FIND StrKonv NO-LOCK WHERE
                        StrKonv.Storl = (IF Translogg.tilStorl = ""
                                           THEN Translogg.Storl
                                           ELSE TransLogg.TilStorl) NO-ERROR.
                      ASSIGN TransLogg.FeilKode = 7. /* Ukjent artlag i mottagende butikk (Opprettet)! */
                      RUN LoggFeilITrans (INPUT TransLogg.FeilKode).
                      CREATE bufArtLag.
                      ASSIGN
                        bufArtLag.Butik      = TransLogg.OvButik
                        bufArtLag.Vg         = TransLogg.Vg 
                        bufArtLag.LopNr      = TransLogg.LopNr 
                        bufArtLag.Storl      = (IF Translogg.TilStorl = ""
                                                  THEN Translogg.Storl
                                                  ELSE TransLogg.TilStorl)
                        bufArtLag.ArtikkelNr = TransLogg.ArtikkelNr
                        bufArtLag.StrKode    = (IF AVAILABLE StrKonv
                                                 THEN StrKonv.StrKode
                                                 ELSE bufArtLag.StrKode)
                        NO-ERROR.
                      IF ERROR-STATUS:ERROR THEN 
                      DO:
                        ASSIGN TransLogg.FeilKode = 8. /* Artlag låst i mottagende butikk! */
                        RUN LoggFeilITrans (INPUT TransLogg.FeilKode).
                        RETURN "UNDO Overfor".
                      END.              
                    END.
                    
                  ASSIGN  /*Trekke ned i fra butikken. Husk at TransLogg.Antall er negativ. */
                    bufArtLag.LagAnt  = bufArtLag.LagAnt + TransLogg.Antall
                    bufArtLag.OvAnt   = bufArtLag.OvAnt  + TransLogg.Antall.
              END. /* MOTTAGENDE_BUT */
            END.
          WHEN 7 THEN /* Lagerjustering */
            ASSIGN
              ArtLag.LagAnt  = ArtLag.LagAnt  - TransLogg.Antall
              ArtLag.JustAnt = ArtLag.JustAnt + TransLogg.Antall.
          WHEN 8 THEN. /* Nedskrivning - Påvirker ikke ArtLag. */
          WHEN 9 THEN /* Svinn */        
            ASSIGN
              ArtLag.LagAnt   = ArtLag.LagAnt   - TransLogg.Antall
              ArtLag.SvinnAnt = ArtLag.SvinnAnt + TransLogg.Antall.
          WHEN 10 THEN /* Gjennkjøp */        
            ASSIGN
              ArtLag.LagAnt      = ArtLag.LagAnt      + (TransLogg.Antall * -1) /* Negativt antall i TransLogg */
              ArtLag.GjenkjopAnt = ArtLag.GjenkjopAnt + TransLogg.Antall
              /* Korrigerer rabatter */
              ArtLag.AntRab   = ArtLAg.AntRab +
                                (IF TransLogg.RabKr <> 0
                                   THEN TransLogg.Antall 
                                   ELSE 0)
              /* Korrigerer salget ved gjenkjøp */
              ArtLag.AntSolgt = ArtLag.AntSolgt + TransLogg.Antall.
          WHEN 11 THEN /* Internt forbruk */        
            ASSIGN
              ArtLag.LagAnt = ArtLag.LagAnt - TransLogg.Antall
              ArtLag.IntAnt = ArtLag.IntAnt + TransLogg.Antall.        
        END CASE.
        
        /* Flagger at transen trekker lager på størrelsen. */
        IF (ArtLag.LagAnt < 0 AND ArtBas.Lager = TRUE AND ArtBas.OPris = FALSE) THEN 
            TransLogg.NegLager = 1.
        
        /* Ikke lagerstyrte artikler */
        IF ArtBas.Lager = FALSE OR ArtBAs.OPris = TRUE THEN
        IKKE_LAGERSTYRT:
        DO:
            ASSIGN
                ArtLag.LagAnt = 0.
        END. /* IKKE_LAGERSTYRT */

      END. /* OPPDAT_ARTLAG */

  RETURN "OK".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-PosterLager) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PosterLager Procedure 
PROCEDURE PosterLager :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  /* Oppdater lagerpost hvis artikkelen har lagerstyring. */
  {x-oppdlagertran2.i
     &FinnLager = "EVIGHET:
                   DO WHILe TRUE:
                     find bufLager exclusive-lock where
                       bufLager.ArtikkelNr = TransLogg.ArtikkelNr and
                       bufLager.Butik      = TransLogg.OvButik NO-ERROR NO-WAIT.
                    if LOCKED bufLager then
                    DO:
                        PAUSE 1 MESSAGE 'Venter på bufLager artikkelnr/butikk '.
                        next EVIGHET.
                    END.
                    if not available bufLager then
                      do:
                        assign
                          TransLogg.FeilKode = 2. /* Lagerpost finnes ikke for mottagende butikk.*/
                        run LoggFeilITrans (input TransLogg.FeilKode).
                        create bufLager.
                        assign
                          bufLager.ArtikkelNr = TransLogg.ArtikkelNr
                          bufLager.Butik      = TransLogg.OvButik.
                        leave EVIGHET.
                      end.
                    if AVAILABLE bufLager then leave EVIGHET. 
                   END. /* EVIGHET */"
  }
  
  RETURN "OK".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-PosterOverfOrdre) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PosterOverfOrdre Procedure 
PROCEDURE PosterOverfOrdre :
/*------------------------------------------------------------------------------
  Purpose:    Posterer genererte overføringstransaksjoner på en overførings-
              ordre. En ov.ordre pr. måned samler opp alle overøfringer.
              Ov.ordren er ferdig postert.
              
              Bygger opp temp-tabell med tt_ovbuffer. Deretter sendes disse
              til LagraOvBuffer.p. -2 i parameterverdi, forteller at det
              er overføringer som er ferdig postert av type plukklager.
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DEF VAR piLinjeNr AS INT  NO-UNDO.
  DEF VAR iBuntNr   AS INT  NO-UNDO.
  DEF VAR ocReturn  AS CHAR NO-UNDO.
  DEF VAR obOk      AS LOG  NO-UNDO.

  /* Tømmer */
  FOR EACH TT_OvBuffer:
      DELETE TT_OvBuffer. /* ev metod empty-temp-table */
  END.
  ASSIGN
      piLinjeNr = 1
      .

  OVBUFFER:
  FOR EACH TransLogg NO-LOCK WHERE
      TransLogg.BatchNr = iPlukkBatchNr:

      IF TransLogg.TTId <> 6 THEN
          NEXT OVBUFFER.

      /* Ukjente artikler kan ikke behandles */
      IF TransLogg.ArtikkelNr = 0 THEN
          NEXT OVBUFFER.
  
      /* Henter Artikkel */
      FIND ArtBas NO-LOCK WHERE
          ArtBas.ArtikkelNr = TransLogg.ArtikkelNr NO-ERROR.
      IF NOT AVAILABLE ArtBas THEN
        NEXT OVBUFFER.
      /* Varer med åpen pris har ikke lagerstyring. */
      IF ArtBas.OPris = TRUE THEN
        NEXT OVBUFFER.
      /* Tar ikke med varer uten lagerstyring */
      IF ArtBas.Lager = FALSE THEN
        NEXT OVBUFFER.

      OVERFORINGSPOST:
      DO:
        /* Logger overføringstransaksjonen */
        CREATE TT_OvBuffer.
        ASSIGN TT_OvBuffer.BuntNr      = 997 /* dummy, kan vara vad som helst */
               TT_OvBuffer.LinjeNr     = piLinjeNr
               TT_OvBuffer.ButikkNrFra = TransLogg.Butik
               TT_OvBuffer.ButikkNrTil = TransLogg.OvButik        
               TT_OvBuffer.ArtikkelNr  = Translogg.ArtikkelNr
               TT_OvBuffer.Vg          = ArtBas.Vg   
               TT_OvBuffer.LopNr       = ArtBas.LopNr
               TT_OvBuffer.Antall      = Translogg.Antall
               TT_OvBuffer.Merknad     = "Plukklager"
               TT_OvBuffer.Storl       = Translogg.Storl
               TT_OvBuffer.TilStorl    = TransLogg.TilStorl
               TT_OvBuffer.Varekost    = Translogg.VVarekost
               piLinjeNr               = piLinjeNr + 1
               /* Setter datoinfo i registrert dato og tid. */
               TT_OvBuffer.RegistrertDato = Translogg.Dato
               TT_OvBuffer.RegistrertTid  = Translogg.Tid
               TT_OvBuffer.RegistrertAv   = USERID("SkoTex")
               .
      END. /* OVERFORINGSPOST */
  END. /* OVBUFFER */

  IF CAN-FIND(FIRST TT_OvBuffer) THEN DO:
      ASSIGN iBuntNr = -2.
      RUN LagraOvBuffer.p (INPUT-OUTPUT iBuntNr,
                           0,
                           "Overføring plukklager " + 
                             STRING(TODAY) + " " + 
                             STRING(TIME,"HH:MM"),
                           wEDB-System,
                           wTabell,
                           7).
      EMPTY TEMP-TABLE TT_OvBuffer NO-ERROR.
  END.
  /* Utskrift av reservasjon, men bare for Web butikk*/
  /*
  .. printreservasjonX.p
  
  IF CAN-FIND(OvBuffer WHERE
              OvBuffer.BuntNr = iBuntNr) THEN
  DO:
    UTSKRIFT_PR_TRANS:
    FOR EACH TT_OvBuffer:
        RUN faktura_fakturaskriver.p (STRING(TT_OvBuffer.ButikkNrTil) + "|1|1",
                                      ?,
                                      " ",
                                      OUTPUT ocReturn,
                                      OUTPUT obOk).
        IF obOk THEN DO:
            IF ocReturn <> "" THEN DO:
                RUN printreservasjonX.p (iBuntNr).
                /*
                RUN skrivfaktura.p (STRING(plfaktura_Id) + "|",ENTRY(1,pcTekst,"|"),ENTRY(2,pcTekst,"|"),ENTRY(3,pcTekst,"|"),ENTRY(4,pcTekst,"|"),ENTRY(5,pcTekst,"|")). 
                */
            END.
        END.
    END. /* UTSKRIFT_PR_TRANS */
  END.
  */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-PosterStatistikk) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PosterStatistikk Procedure 
PROCEDURE PosterStatistikk :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR wDataObjekt AS CHAR NO-UNDO.

  DEF BUFFER bufStLinje FOR StLinje.

    /* Oppdaterer statistikker */
    STATISTIKK_DEF:
    FOR EACH stdef NO-LOCK
      BREAK BY StDef.StTypeId:

      /* Kampanjetrans skal bare oppdatere kampanje og kampanjetilbud */
      IF TransLogg.TTId = 109 THEN
      DO: 
          IF NOT CAN-DO("KAMPANJE,KAMPTILB",StDef.StTypeId) THEN
              NEXT.              
      END.
      /* Vanlige transaksjoner skal også oppdatere kampanjeartikkel. */
      ELSE DO:
          IF CAN-DO("KAMPANJE,KAMPTILB",StDef.StTypeId) THEN
              NEXT.
          /* For vanlige artikler skal ... */
          IF AVAILABLE ArtBas AND ArtBas.Non_Sale = FALSE THEN DO:
            IF CAN-DO("SELGER-ART,KASS-ART,NONSALE",StDef.StTypeId) THEN
              NEXT.
          END.
          ELSE DO:
            /* For nonsale skal bare nonsale oppdateres. */      
            IF NOT CAN-DO("NONSALE",StDef.StTypeId) THEN
              NEXT.
          END.
      END.
    
      /* Henter og kontrollerer perioden. */
      FIND Periode NO-LOCK WHERE
       Periode.PerId = StDef.PerId NO-ERROR.
      IF NOT AVAILABLE Periode THEN 
        RETURN "NEXT".
    
      /* Henter PeriodeLinje */
      /* Faste perioder.     */
      IF Periode.Fast THEN
        DO:
          CASE Periode.PerId:
            WHEN "AAR"   THEN /* Kun en linje i et år. */
              wWork = 1. 
            WHEN "MANED" THEN /* 12 Linjer pr. år */
              wWork = MONTH(TransLogg.Dato).
            WHEN "UKE"   THEN /* Opptil 53 linjer pr. år */
              DO:
                RUN weeknum.p (TransLogg.Dato, OUTPUT wWork).
                wWork = int(SUBSTRING(STRING(wWork,"999999"),5,2)).
              END.
            WHEN "DAG"   THEN /* Opptil 366 dager pr. år. */
              wWork = (TransLogg.Dato + 1) - date(01,01,YEAR(TransLogg.Dato)).
          END.
        END.
      /* Fri periode         */
      ELSE DO:
        FIND FIRST PerLin NO-LOCK WHERE
          PerLin.PerId   =  Periode.PerId  AND
          PerLin.FraDato <= TransLogg.Dato AND 
          PerLin.TilDato >= TransLogg.Dato NO-ERROR.
        IF NOT AVAILABLE PerLin THEN
          FIND LAST PerLin WHERE
            PerLin.PerId = Periode.PerId NO-ERROR.
        IF AVAILABLE PerLin THEN
          wWork = PerLin.PerLinNr.
        ELSE
          wWork = ?. /* Her er ingenting mer å gjøre. */
      END.
      /* Håndtering av ukjente frie statistikkdefineisjoner. */
      IF wWork = 0 OR 
         wWork = ? THEN 
        NEXT STATISTIKK_DEF.
      
      /* Henter/oppretter og stempler statistikkhode */      
      FIND StHode EXCLUSIVE-LOCK WHERE
        StHode.StTypeId = StDef.StTypeId AND
        StHode.PerId    = Periode.PerId NO-ERROR.
      IF NOT AVAILABLE StHode THEN
        DO:
          CREATE StHode.
          ASSIGN
            StHode.StTypeId = StDef.StTypeId
            StHode.PerId    = Periode.PerId.
          ASSIGN
            StHode.RegistrertDato = TODAY
            StHode.RegistrertTid  = TIME
            StHode.RegistrertAv   = USERID("dictdb").            
        END.
      ASSIGN
        StHode.EDato    = TODAY
        StHode.ETid     = TIME
        StHode.BrukerId = USERID("dictdb").            
        
      /* Setter dataobjekt */
      CASE StDef.StTypeId:
        WHEN "ARTIKKEL" THEN          
          wDataObjekt = STRING(TransLogg.ArtikkelNr,"9999999999999").
        WHEN "NONSALE" THEN          
          wDataObjekt = STRING(TransLogg.ArtikkelNr,"9999999999999").
        WHEN "HOVEDGR"  THEN
          DO:
            FIND VarGr OF ArtBas NO-LOCK NO-ERROR.
            IF NOT AVAILABLE VarGr THEN NEXT.
            IF CAN-FIND(HuvGr OF VarGr) THEN
              wDataObjekt = STRING(VarGr.Hg,"9999").
            ELSE DO:
              ASSIGN TransLogg.FeilKode = 9. /* Ukjent hovedgruppe på atikkelen. */
              RUN LoggFeilITrans (INPUT TransLogg.FeilKode).
              RETURN "UNDO".
            END.
          END.
        WHEN "VAREGR"   THEN
          wDataObjekt = STRING(TransLogg.Vg,"999999").
        WHEN "AVDELING"   THEN
        DO:
            FIND VarGr OF ArtBas NO-LOCK NO-ERROR.
            IF NOT AVAILABLE VarGr THEN NEXT.
            FIND HuvGr OF VarGr NO-LOCK NO-ERROR.
            IF AVAILABLE HuvGr THEN
                FIND Avdeling NO-LOCK WHERE
                  Avdeling.AvdelingNr = HuvGr.AvdelingNr NO-ERROR.
            IF AVAILABLE Avdeling THEN
                wDataObjekt = STRING(Avdeling.AvdelingNr,"9999").
            ELSE DO:
              ASSIGN TransLogg.FeilKode = 15. /* Ukjent Avdeling på atikkelens hovedgruppe. */
              RUN LoggFeilITrans (INPUT TransLogg.FeilKode).
              RETURN "UNDO".
            END.
        END.
        WHEN "LEVERAN"  THEN
          wDataObjekt = TRIM(STRING(ArtBas.LevNr,">>999999")).
        WHEN "LEVERAN-VG"  THEN
              wDataObjekt = TRIM(STRING(ArtBas.LevNr,">>999999")) + CHR(1) + 
                  string(TransLogg.Vg,"999999").
        WHEN "LEVERAN-SA"  THEN
        DO:
            wDataObjekt = TRIM(STRING(ArtBas.LevNr,">>999999")) + CHR(1) + 
                        string(ArtBas.Sasong,"999999").
        END.
        WHEN "BUTSTAT" THEN
          wDataObjekt = STRING(TransLogg.Butik,"999999").
        WHEN "KUNDSTAT" THEN
          wDataObjekt = STRING(TransLogg.KundNr,"9999999999999").
        WHEN "SELGERSTAT" THEN
          wDataObjekt = STRING(TransLogg.ForsNr,"9999999999999").
        WHEN "KASS-VG" THEN
          wDataObjekt = STRING(TransLogg.ForsNr,"9999999999999") + CHR(1) + 
                        string(TransLogg.Vg,"999999").
        WHEN "KASS-ART" THEN
          wDataObjekt = STRING(TransLogg.ForsNr,"9999999999999") + CHR(1) + 
                        string(TransLogg.ArtikkelNr,"9999999999999").
        WHEN "SELGER" THEN
          wDataObjekt = STRING(TransLogg.SelgerNr,"9999999999999").
        WHEN "SELGER-VG" THEN
          wDataObjekt = STRING(TransLogg.SelgerNr,"9999999999999") + CHR(1) + 
                        string(TransLogg.Vg,"999999").
        WHEN "SELGER-ART" THEN
          wDataObjekt = STRING(TransLogg.SelgerNr,"9999999999999") + CHR(1) + 
                        string(TransLogg.ArtikkelNr,"9999999999999").
        WHEN "MEDLEM" THEN
          wDataObjekt = IF TransLogg.MedlemsNr <> 0
                          THEN STRING(TransLogg.MedlemsNr,"9999999999999")
                          ELSE "9999999999999".
        WHEN "MEDLEMTOT" THEN
          wDataObjekt = "9999999999999". /* Statistikk totalt for kundeklubb. */
        WHEN "KAMPANJE" THEN
          wDataObjekt = IF TransLogg.KampId > 0
                          THEN STRING(TransLogg.KampId,"9999999999999")
                          ELSE "".
        WHEN "KAMPART" THEN
          wDataObjekt = IF (TransLogg.KampId > 0 AND Translogg.ArtikkelNr > 0)
                          THEN string(TransLogg.KampId,"9999999999999") + CHR(1) + 
                               string(Translogg.KampTilbId,"999999999") + CHR(1) + 
                               string(TransLogg.ArtikkelNr,"9999999999999")
                          ELSE "".
        WHEN "KAMPTILB" THEN
          wDataObjekt = IF (TransLogg.KampId > 0 AND Translogg.KampTilbId > 0)
                          THEN string(TransLogg.KampId,"9999999999999") + CHR(1) + 
                               string(Translogg.KampTilbId,"999999999")
                          ELSE "".
      END CASE.

      /* Blanktt objekt skal ikke oppdateres. */
      IF wDataObjekt = "" THEN NEXT STATISTIKK_DEF.

      /* Medlemsstatistikk skal bare oppdateres hvis det er et gyldig medlemsnummer. */
      IF StDef.StTypeId = "MEDLEM" THEN
      DO:
        IF TransLogg.MedlemsNr = 0 THEN
          NEXT STATISTIKK_DEF.
      END.
      
      /* Medlemsklubbstatistikk skal bare oppdateres hvis det er et gyldig medlemsnummer. */
      IF StDef.StTypeId = "MEDLEMTOT" THEN
      DO:
        IF TransLogg.MedlemsNr = 0 THEN
          NEXT STATISTIKK_DEF.
      END.
      
      /* Kundestatistikk skal bare oppdateres hvis det er et gyldig kundenummer. */
      IF StDef.StTypeId = "KUNDSTAT" THEN
      DO:
        IF TransLogg.KundNr = 0 THEN
          NEXT STATISTIKK_DEF.
      END.
      
      /* Selgerstatiatikk skal bare oppdateres hvis det er et gyldig selgernr. */
      IF StDef.StTypeId = "SELGER" THEN
      DO:
        IF TransLogg.SelgerNr = 0 THEN
          NEXT STATISTIKK_DEF.
      END.

      IF StDef.StTypeId = "SELGER-VG" THEN
      DO:
        IF TransLogg.SelgerNr = 0 THEN
          NEXT STATISTIKK_DEF.
        IF TransLogg.Vg = 0 THEN
          NEXT STATISTIKK_DEF.
      END.

      /* Oppdaterer statistikklinjen. */
      FIND StLinje EXCLUSIVE-LOCK WHERE
        StLinje.StTypeId   = StDef.StTypeId AND
        StLinje.PerId      = Periode.PerId AND
        StLinje.DataObjekt = wDataObjekt AND
        StLinje.Diverse    = "" AND
        StLinje.Butik      = TransLogg.Butik AND
        StLinje.Aar        = year(TransLogg.Dato) AND
        StLinje.PerLinNr   = int(wWork) NO-ERROR.
      IF NOT AVAILABLE StLinje THEN
        DO:
          CREATE StLinje.
          ASSIGN
            StLinje.StTypeId   = StDef.StTypeId 
            StLinje.PerId      = Periode.PerId 
            StLinje.DataObjekt = wDataObjekt 
            StLinje.Diverse    = ""
            StLinje.Butik      = TransLogg.Butik
            StLinje.Aar        = YEAR(TransLogg.Dato)
            StLinje.PerLinNr   = int(wWork). 

        END.

      /* Henter Lager posten */
      IF TransLogg.TTId <> 109 THEN 
      DO:
          FIND Lager NO-LOCK WHERE
            Lager.ArtikkelNr = TransLogg.ArtikkelNr AND
            Lager.Butik      = TransLogg.Butik NO-ERROR. /* Denne skal finnes */
          IF NOT AVAILABLE Lager THEN
            RETURN "UNDO".
      END.
      /* Oppdaterer statistikkfeltene */
      CASE TransLogg.TTId:      
        WHEN 1 OR WHEN 109 THEN
          DO:
          ASSIGN /* Varesalg */
            StLinje.AntSolgt   = StLinje.AntSolgt   + TransLogg.Antall  
            StLinje.VerdiSolgt = StLinje.VerdiSolgt + 
                               (
                                (TransLogg.Pris - Translogg.RabKr) - 
                                TransLogg.Mva
                               ) * TransLogg.Antall
            StLinje.VVareKost  = StLinje.VVareKost + 
                                 (TransLogg.VVareKost * TransLogg.Antall)
            StLinje.MvaVerdi   = StLinje.MvaVerdi + 
                                 (TransLogg.Mva * TransLogg.Antall)
            /* Posterer rabatt. */
            StLinje.AntRab        = StLinje.AntRab +
                                    (IF TransLogg.RabKr <> 0
                                      THEN TransLogg.Antall 
                                      ELSE 0)
            StLinje.VerdiRabatt   = StLinje.VerdiRabatt +
                                    (IF TransLogg.RabKr <> 0
                                      THEN TransLogg.Antall * (Translogg.RabKr - RabUMva(wMva%))
                                      ELSE 0).
          END.                                      
        WHEN 2 THEN
            ASSIGN  /* Brekkasje */
              StLinje.BrekkAnt   = StLinje.BrekkAnt   + TransLogg.Antall  
              StLinje.BrekkVerdi = StLinje.BrekkVerdi + 
                                   (wVareKost * TransLogg.Antall).
        WHEN 3 THEN 
          DO:
            ASSIGN  /* Kundereklamasjon */
              StLinje.ReklAnt    = StLinje.ReklAnt    + TransLogg.Antall  
              StLinje.ReklVerdi  = StLinje.ReklVerdi  + 
                                   (wVareKost * TransLogg.Antall)  
              /* Korrigerer rabatt. */
              StLinje.AntRab        = StLinje.AntRab +
                                      (IF TransLogg.RabKr <> 0
                                        THEN TransLogg.Antall 
                                        ELSE 0)
              StLinje.VerdiRabatt   = StLinje.VerdiRabatt +
                                      (IF TransLogg.RabKr <> 0
                                        THEN TransLogg.Antall * (Translogg.RabKr - RabUMva(wMva%))
                                        ELSE 0)
              /* Korrigerer salget. */
              StLinje.AntSolgt   = StLinje.AntSolgt   + TransLogg.Antall  
              StLinje.VerdiSolgt = StLinje.VerdiSolgt + 
                                   (
                                    (TransLogg.Pris - Translogg.RabKr) - 
                                    TransLogg.Mva
                                   ) * TransLogg.Antall
              StLinje.VVareKost  = StLinje.VVareKost + 
                                   (TransLogg.VVareKost * TransLogg.Antall)
              StLinje.MvaVerdi   = StLinje.MvaVerdi + 
                                 (TransLogg.Mva * TransLogg.Antall).
          END. 
        WHEN 4 THEN
            ASSIGN  /* Lagerreklamasjon */
              StLinje.ReklLAnt   = StLinje.ReklLAnt   + TransLogg.Antall  
              StLinje.ReklLVerdi = StLinje.ReklLVerdi + 
                                   (wVareKost * TransLogg.Antall).
        WHEN 5 THEN
          DO: /* Varekjøp m/vektet vareverdi */
            ASSIGN  
              wWork  = Lager.Lagant   * wVareKost   /* Gammel lagerverdi */
              wWork2 = TransLogg.Pris * TransLogg.Antall. /* Verdi av innkjøp  */
            
            ASSIGN          
              StLinje.KjopAnt   = StLinje.KjopAnt    + TransLogg.Antall  
              StLinje.KjopVerdi = StLinje.KjopVerdi  + wWork2.
          END.
        WHEN 6 THEN
          DO: /* Overføring */                
            /* Henter statistikk for mottagende butikk.            */
            /* Litt spesiell håndtering må til for å kunne postere */
            /* overføringer i butikkstatistikken.                  */
            FIND bufStLinje EXCLUSIVE-LOCK WHERE
              bufStLinje.StTypeId   = StDef.StTypeId AND
              bufStLinje.PerId      = Periode.PerId AND
              bufStLinje.DataObjekt = (IF StDef.StTypeId = "BUTSTAT"
                                        THEN STRING(TransLogg.OvButik,"999999")
                                        ELSE wDataObjekt) AND
              bufStLinje.Diverse    = "" AND
              bufStLinje.Butik      = TransLogg.OvButik AND
              bufStLinje.Aar        = year(TransLogg.Dato) AND
              bufStLinje.PerLinNr   = int(wWork) NO-ERROR.
            IF NOT AVAILABLE bufStLinje THEN
              DO:
                CREATE bufStLinje.
                ASSIGN
                  bufStLinje.StTypeId   = StDef.StTypeId 
                  bufStLinje.PerId      = Periode.PerId 
                  bufStLinje.DataObjekt = (IF StDef.StTypeId = "BUTSTAT"
                                            THEN STRING(TransLogg.OvButik,"999999")
                                            ELSE wDataObjekt)  
                  bufStLinje.Diverse    = ""
                  bufStLinje.Butik      = TransLogg.OvButik
                  bufStLinje.Aar        = YEAR(TransLogg.Dato)
                  bufStLinje.PerLinNr   = int(wWork).      
              END.
            
            ASSIGN  /* Trekker ned på FRA butikk.            */
                    /* Posteringer skjer med vektet varekost i fra butikken. */
              StLinje.OvAnt   = StLinje.OvAnt      - TransLogg.Antall  
              StLinje.OvVerdi = StLinje.OvVerdi    - 
                                (wVareKost * TransLogg.Antall).
   
            /* Henter Lager posten til mottagende butikk */
            /* Forutsetter her at VVarekost er satt, fordi varen er lagerstyrt. */
            FIND bufLager NO-LOCK WHERE
              bufLager.ArtikkelNr = TransLogg.ArtikkelNr AND
              bufLager.Butik      = TransLogg.Butik NO-ERROR. /* Denne skal finnes */
            IF NOT AVAILABLE bufLager THEN
              RETURN "UNDO".
              
            ASSIGN  /* Posterer i TIL (mottagende) butikk.  */
                    /* Posteringer skjer med vektet varekost i mottagende butikk. */
              bufStLinje.OvAnt     = bufStLinje.OvAnt      + TransLogg.Antall  
              bufStLinje.OvVerdi   = bufStLinje.OvVerdi    + 
                                    (TransLogg.VVareKost * TransLogg.Antall).
          END.
          
        WHEN 7 THEN
            ASSIGN  /* Lagerjustering */
              StLinje.JustAnt   = StLinje.JustAnt   + TransLogg.Antall  
              StLinje.JustVerdi = StLinje.JustVerdi + 
                                  (TransLogg.VVareKost * TransLogg.Antall).
        WHEN 8 THEN
          DO:
            FIND bufLager NO-LOCK WHERE
              bufLager.ArtikkelNr = TransLogg.ArtikkelNr AND
              bufLager.Butik      = TransLogg.Butik NO-ERROR. /* Denne skal finnes */
            ASSIGN  /* Nedskriving */
              /* Ingen endring i lagerantall. Kun VVarekost. */
              StLinje.NedAnt    = StLinje.NedAnt   + ((IF AVAILABLE bufLager 
                                                        THEN Lager.LagAnt  
                                                        ELSE 0) *
                                                       (IF TransLogg.Antall < 0
                                                         THEN -1
                                                         ELSE 1))
              StLinje.NedVerdi  = StLinje.NedVerdi + 
                                  (TransLogg.Pris * ((IF AVAILABLE bufLager 
                                                        THEN Lager.LagAnt  
                                                        ELSE 0)) *
                                                       (IF TransLogg.Antall < 0
                                                         THEN -1
                                                         ELSE 1)).
          END.
        WHEN 9 THEN
            ASSIGN  /* Svinn */
              StLinje.SvinnAnt   = StLinje.SvinnAnt   + TransLogg.Antall  
              StLinje.SvinnVerdi = StLinje.SvinnVerdi + 
                                   (TransLogg.Pris * TransLogg.Antall).
        WHEN 10 THEN
            ASSIGN  /* Gjennkjøp */
              StLinje.GjenkjopAnt   = StLinje.GjenkjopAnt    + (TransLogg.Antall * -1) /* Negativt antall i TransLogg */  
              StLinje.GjenkjopVerdi = StLinje.GjenkjopVerdi  + 
                                      (
                                       (TransLogg.Pris - Translogg.RabKr) - 
                                       TransLogg.Mva
                                      ) * TransLogg.Antall
              /* Korrigerer rabatt ved gjennkjøp. */
              StLinje.AntRab        = StLinje.AntRab +
                                      (IF TransLogg.RabKr <> 0
                                        THEN TransLogg.Antall 
                                        ELSE 0)
              StLinje.VerdiRabatt   = StLinje.VerdiRabatt +
                                      (IF TransLogg.RabKr <> 0
                                        THEN TransLogg.Antall * (Translogg.RabKr - RabUMva(wMva%))
                                        ELSE 0)
              /* Korrigerer salget ved gjenkjøp */
              StLinje.AntSolgt   = StLinje.AntSolgt   + TransLogg.Antall  
              StLinje.VerdiSolgt = StLinje.VerdiSolgt + 
                                   (
                                    (TransLogg.Pris - Translogg.RabKr) - 
                                    TransLogg.Mva
                                   ) * TransLogg.Antall
              StLinje.VVareKost  = StLinje.VVareKost + 
                                   (TransLogg.VVareKost * TransLogg.Antall)
              StLinje.MvaVerdi   = StLinje.MvaVerdi + 
                                 (TransLogg.Mva * TransLogg.Antall).
        WHEN 11 THEN
            ASSIGN  /* Internt forbruk */
              StLinje.IntAnt   = StLinje.IntAnt    + TransLogg.Antall  
              StLinje.IntVerdi = StLinje.IntVerdi  + 
                                 (TransLogg.VVareKost * TransLogg.Antall).
       END CASE.
    
    END. /* STATISTIKK_DEF */      

  RETURN "OK".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

/* ************************  Function Implementations ***************** */

&IF DEFINED(EXCLUDE-RabUMva) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION RabUMva Procedure 
FUNCTION RabUMva RETURNS DECIMAL
  ( wMva% AS DEC ) :
/*------------------------------------------------------------------------------
  Purpose:  Beregner RABATT - MVA(På rabatten). Dvs nettorabatt.
    Notes:  
------------------------------------------------------------------------------*/

  DEF VAR wWork AS DEC NO-UNDO.

  wWork = (TransLogg.RabKr) -
          ((TransLogg.RabKr) / (1 + (wMva% / 100))).
  IF wWork = ? THEN wWork = 0.

  RETURN wWork.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SjekkStreng) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION SjekkStreng Procedure 
FUNCTION SjekkStreng RETURNS LOGICAL
  ( INPUT wTekst AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  DEF VAR wGyldigeChar AS CHAR INITIAL "0,1,2,3,4,5,6,7,8,9" NO-UNDO.
  DEF VAR wOk          AS LOG  INITIAL TRUE   NO-UNDO.
  DEF VAR wLoop        AS INT                 NO-UNDO.
  
  /* Alle karrakterer i strengen skal finnes i sjekklisten. */  
  SJEKKLOOP:
  DO wLoop = 1 TO LENGTH(wTekst):
    IF CAN-DO(wGyldigeChar,SUBSTRING(wTekst,wLoop,1)) THEN
      NEXT.
    ELSE DO:
      ASSIGN wOk = FALSE.
      LEAVE SJEKKLOOP.
    END.
  END. /* SJEKKLOOP */  

  RETURN wOk.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

