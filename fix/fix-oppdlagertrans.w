&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
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
DEF INPUT PARAMETER iAar            AS INT    NO-UNDO.

DEFINE VARIABLE dFraDato AS DATE NO-UNDO.
DEFINE VARIABLE dTilDato AS DATE NO-UNDO.
DEF VAR iBatchNr      AS INT  NO-UNDO.
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

/* Definerer stream */
DEF STREAM InnData.
DEF STREAM LoggData.

/* Definerer buffere */
DEF BUFFER bufLager     FOR Lager.
DEF BUFFER bufStLager   FOR StLager.
DEF BUFFER bufArtLag    FOR ArtLag.
DEF BUFFER lagTransLogg FOR TransLogg.
DEFINE BUFFER bufButiker FOR Butiker.
DEFINE BUFFER bufArtBas  FOR ArtBas.
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

/* Klargjørings dato og tid. */
ASSIGN
  wDato = TODAY
  wTid  = TIME
  dFraDato = DATE(1,1,iAar)
  dTilDato = DATE(12,31,iAar).

RUN ByggLoggFilNavn.

BUTIKKLOOP:
FOR EACH bufButiker NO-LOCK WHERE 
  CAN-FIND(FIRST TransLogg WHERE 
           Translogg.Dato >= dFraDato AND TransLogg.Dato <= dTilDato AND TransLogg.TTId > 0 AND TransLogg.Butik = bufButiker.Butik):
           
  /* viser butikk som oppdateres. */  
  DO:
    OUTPUT STREAM LoggData to value(wLogFil) no-echo append.
    PUT STREAM LoggData UNFORMATTED 
    " Oppdaterer for butikk: " STRING(bufButiker.Butik) + " " + bufButiker.ButNamn SKIP. 
    OUTPUT STREAM LoggData close.        
  END.      
           
  /* Oppdaterer lager og statistikker. */
  RUN OppdaterLagerOgStat.
END. /* BUTIKKLOOP */

/* viser resultat til bruker */
IF VALID-HANDLE(wProgram-Handle) AND TERMINAL <> "" THEN
  DO:
    IF SEARCH(wLogFil) <> ? THEN
      OS-COMMAND NO-WAIT VALUE("notepad.exe") VALUE(wLogFil).
  END.

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

  OUTPUT STREAM LoggData to value(wLogFil) no-echo append.
  PUT STREAM LoggData UNFORMATTED 
  " Batch: " TransLogg.BatchNr 
  " TransNr: " TransLogg.TransNr 
  " ArtikkelNr: " TransLogg.ArtikkelNr
  " Vg/LøpeNr: " STRING(TransLogg.Vg) + "/" + string(TransLogg.LopNr) " " 
  wTekst SKIP.
  OUTPUT STREAM LoggData close.

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
OUTPUT STREAM LoggData to value(wLogFil) no-echo append.
/* Åpner ningsmelding i loggfil */
PUT STREAM LoggData UNFORMATTED 
  " " SKIP
  "Batch: " + (IF wBatchNr = ? THEN "?" ELSE STRING(wBatchNr)) SKIP
  " -----------------------------------------------------------------------" SKIP
  " Oppdaterer lager og statistikker  " SKIP
  " " STRING(TODAY) " " STRING(TIME,"HH:MM:SS") " " USERID("dictdb") SKIP
  " -----------------------------------------------------------------------" SKIP(1).
OUTPUT STREAM LoggData close.

ASSIGN
  cBatchListe = cBatchListe +
                (IF cBatchListe = ""
                   THEN ""
                   ELSE ",") + 
                STRING(iBatchNr).
  .

DO WITH FRAME DEFAULT-FRAME:
  IF NOT VALID-HANDLE(h_PrisKo) THEN
      RUN PrisKo.p PERSISTENT SET h_PrisKo.

  /* Startet info */
  ASSIGN
    wStartTid = TIME.
  IF VALID-HANDLE(wProgram-Handle) THEN
    RUN StartInfo IN wProgram-Handle (INPUT TODAY, INPUT wStartTid).

  OUTPUT TO value('regenstat.log') append.
    PUT UNFORMATTED TODAY ' ' STRING(TIME,'HH:MM:SS') " Sletter statistikk for butikk: " STRING(bufButiker.Butik) + "." SKIP.
  OUTPUT CLOSE.

  wAntFeil  = 0.
  STATUS DEFAULT "Sletter statistikk for butikk: " + STRING(bufButiker.Butik) + ".".
  
  FOR EACH StDef NO-LOCK:
    FOR EACH StLinje WHERE 
      StLinje.Butik = bufButiker.Butik AND 
      StLinje.StTypeId = StDef.StTypeId AND
      StLinje.PerId    = StDef.PerId AND
      StLinje.AarPerLinNr >= INTEGER(STRING(iAar,'9999') + '000') AND
      StLinje.AarPerLinNr <= INTEGER(STRING(iAar,'9999') + '999'):
      DELETE StLinje.
    END.
  END.

  OUTPUT TO value('regenstat.log') append.
    PUT UNFORMATTED TODAY ' ' STRING(TIME,'HH:MM:SS') " Behandler translogg for butikk: " STRING(bufButiker.Butik) + ". " dFraDato ' ' dTilDato SKIP.
  OUTPUT CLOSE.
  STATUS DEFAULT "Behandler translogg for butikk: " + STRING(bufButiker.Butik) + ".".

  OPPDAT_TRANS1:
  FOR EACH bufArtBas NO-LOCK WHERE 
    CAN-FIND(FIRST TransLogg WHERE Translogg.ArtikkelNr = bufArtBas.ArtikkelNr AND
           Translogg.Dato >= dFraDato AND
           TransLogg.Dato <= dTilDato AND
           TransLogg.TTId > 0 AND 
           TransLogg.Butik = bufButiker.Butik),  
    EACH TransLogg NO-LOCK WHERE Translogg.ArtikkelNr = bufArtBas.ArtikkelNr AND
       Translogg.Dato >= dFraDato AND
       TransLogg.Dato <= dTilDato AND
       TransLogg.TTId >= 0 AND
       TransLogg.Butik = bufButiker.Butik:

    OUTPUT TO value('regenstat.log') append.
      PUT UNFORMATTED TODAY ' ' STRING(TIME,'HH:MM:SS') ' FØR ' Translogg.ArtikkelNr Translogg.Postert SKIP.
    OUTPUT CLOSE.

    IF TransLogg.Postert = FALSE THEN 
      NEXT OPPDAT_TRANS1.

    OUTPUT TO value('regenstat.log') append.
      PUT UNFORMATTED TODAY ' ' STRING(TIME,'HH:MM:SS') ' ETTER ' Translogg.ArtikkelNr Translogg.Postert SKIP.
    OUTPUT CLOSE.
    
    FIND BatchLogg OF TransLogg NO-LOCK NO-ERROR.
    {fix-oppdlagertrans.i &Blokk = 1}      
  END. /* OPPDAT_TRANS TRANSATION */

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

/* Ferdig melding i loggfil */
/* Åpner stream til datafil */
OUTPUT STREAM LoggData to value(wLogFil) no-echo append.
PUT STREAM LoggData UNFORMATTED 
  " " SKIP
  " -----------------------------------------------------------------------" SKIP
  " Antall transksjoner: " STRING(wTotAntall,"zzzzzzz9") SKIP
  " Antall oppdatert   : " STRING(wOppdatertAntall,"zzzzzzz9") SKIP
  " Antall med feilkode: " STRING(wTotAntall - wOppdatertAntall,"zzzzzzz9") SKIP
  " Ferdigstatus       : " wOkStatus SKIP
  " Brukt tid          : " STRING(wBruktTid,"HH:MM:SS") SKIP
  " Ferdig             : " STRING(TODAY) " " STRING(TIME,"HH:MM:SS") SKIP
  " -----------------------------------------------------------------------" SKIP(1).
OUTPUT STREAM LoggData close.

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
        WHEN "HOVEDGR"  THEN
          DO:
            FIND VarGr OF ArtBas NO-LOCK NO-ERROR.
            IF NOT AVAILABLE VarGr THEN NEXT.
            IF CAN-FIND(HuvGr OF VarGr) THEN
              wDataObjekt = STRING(VarGr.Hg,"9999").
            ELSE DO:
/*               assign TransLogg.FeilKode = 9. /* Ukjent hovedgruppe på atikkelen. */ */
/*               run LoggFeilITrans (input TransLogg.FeilKode).                        */
              RETURN "UNDO".
            END.
/*                                                      */
/*             find HuvGr no-lock where                 */
/*               HuvGr.Hg = ArtBas.Hg no-error.         */
/*             if available HuvGr then                  */
/*               wDataObjekt = string(HuvGr.Hg,"9999"). */
/*             else do:                                 */
/*               return "UNDO".                         */
/*             end.                                     */
/*                                                      */
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
/*               assign TransLogg.FeilKode = 15. /* Ukjent Avdeling på atikkelens hovedgruppe. */ */
/*               run LoggFeilITrans (input TransLogg.FeilKode).                                   */
              RETURN "UNDO".
            END.

/*             find HuvGr no-lock where                               */
/*               HuvGr.Hg = ArtBas.Hg no-error.                       */
/*             IF AVAILABLE HuvGr THEN                                */
/*                 FIND Avdeling NO-LOCK WHERE                        */
/*                   Avdeling.AvdelingNr = HuvGr.AvdelingNr NO-ERROR. */
/*             IF AVAILABLE Avdeling THEN                             */
/*                 wDataObjekt = string(Avdeling.AvdelingNr,"9999").  */
/*             else do:                                               */
/*               return "UNDO".                                       */
/*             end.                                                   */
        END.
        WHEN "LEVERAN"  THEN
          wDataObjekt = STRING(ArtBas.LevNr,"999999").
        WHEN "LEVERAN-VG"  THEN
          wDataObjekt = STRING(ArtBas.LevNr,"999999") + CHR(1) + 
                        string(TransLogg.Vg,"999999").
        WHEN "BUTSTAT" THEN
          wDataObjekt = STRING(TransLogg.Butik,"999999").
        WHEN "KUNDSTAT" THEN
          wDataObjekt = STRING(TransLogg.KundNr,"9999999999999").
        WHEN "SELGERSTAT" THEN
          wDataObjekt = STRING(TransLogg.ForsNr,"9999999999999").
        WHEN "KASS-VG" THEN
          wDataObjekt = STRING(TransLogg.ForsNr,"9999999999999") + CHR(1) + 
                        string(TransLogg.Vg,"999999").
        WHEN "SELGER" THEN
          wDataObjekt = STRING(TransLogg.SelgerNr,"9999999999999").
        WHEN "SELGER-VG" THEN
          wDataObjekt = STRING(TransLogg.SelgerNr,"9999999999999") + CHR(1) + 
                        string(TransLogg.Vg,"999999").
        WHEN "MEDLEM" THEN
          wDataObjekt = IF TransLogg.MedlemsNr <> 0
                          THEN STRING(TransLogg.MedlemsNr,"9999999999999")
                          ELSE "9999999999999".
        WHEN "MEDLEMTOT" THEN
          wDataObjekt = "9999999999999". /* Statistikk totalt for kundeklubb. */
      END CASE.

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
      
      /* Kundestatistikk skal bare oppdateres hvis det er et gyldig kundenummer. */
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
      FIND Lager NO-LOCK WHERE
        Lager.ArtikkelNr = TransLogg.ArtikkelNr AND
        Lager.Butik      = TransLogg.Butik NO-ERROR. /* Denne skal finnes */
      IF NOT AVAILABLE Lager THEN
        RETURN "UNDO".

      /* Oppdaterer statistikkfeltene */
      CASE TransLogg.TTId:      
        WHEN 1 THEN
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
/*
message "Rabatt:" RabUMva(wMva%) skip
        "wMva%:" wMva% skip
        "Translogg.RabKr:" TransLogg.RabKr skip
        "Translogg.AntRab:" TransLogg.Antall skip
        "StLinje.VerdiRabatt:" StLinje.VerdiRabatt
view-as alert-box.                                              
*/
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

