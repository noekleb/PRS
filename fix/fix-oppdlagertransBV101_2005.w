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

def VAR wProgram-Handle as handle no-undo.
def VAR wBatchNr        as int    no-undo.
def VAR iAar            as int  INIT 2005  no-undo.

DEF VAR iBatchNr      AS INT  NO-UNDO.
DEF VAR wLoop1        AS INT  NO-UNDO.
DEF VAR wStDataObjekt AS CHAR NO-UNDO.
DEF VAR wStTypeListe  AS CHAR NO-UNDO.
def var wTotAntall as dec format "zzz,zzz,zz9" no-undo.
def var wWork            as dec  no-undo.
DEF VAR wWork1        AS DEC  NO-UNDO.
def var wWork2           as dec  no-undo.
def var wWork3           as dec  no-undo.
def var wStop            as log  initial false no-undo.
def var wDato            as date no-undo.
def var wTid             as int  no-undo.
def var wSkjerm          as char no-undo.
def var wTilbud          as log  no-undo.
def var wOk              as log  no-undo.
def var wOppdatertAntall as int  no-undo.
def var wStartTid        as int  no-undo.
def var wFerdigTid       as int  no-undo.
def var wBruktTid        as int  no-undo.
def var wImpFil          as char no-undo.
def var wLogFil          as char no-undo.
def var wOldPluFil       as char no-undo.
def var wKatalog         as char no-undo.
def var wLogKatalog      as char no-undo.
def var wOkStatus        as char no-undo.
def var wAntBest         as int  no-undo.
def var wAntFeil         as int  no-undo.
def var wEDB-System      as char no-undo.
def var wTabell          as char no-undo.
def var wVareKost        as dec  no-undo.
def var wMva%            as dec  no-undo.
def var wDefMva%         as dec  no-undo.
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
def stream InnData.
def stream LoggData.

/* Definerer buffere */
def buffer bufLager     for Lager.
def buffer bufStLager   for StLager.
def buffer bufArtLag    for ArtLag.
DEF BUFFER lagTransLogg FOR TransLogg.

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
  ( wMva% as dec )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SjekkStreng) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD SjekkStreng Procedure 
FUNCTION SjekkStreng RETURNS LOGICAL
  ( input wTekst as char )  FORWARD.

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
if wEDB-System = "" then
  wEDB-System = "OVERFOR-LOCK".
{syspar2.i 1 2 3 wTabell}
if wEDB-System = "" then
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
assign
  wDato = today
  wTid  = time.

run ByggLoggFilNavn.

/* Klargjør priskøen. */
RUN OppdaterLagerOgStat.

/* viser resultat til bruker */
if valid-handle(wProgram-Handle) and terminal <> "" then
  do:
    if search(wLogFil) <> ? then
      os-command no-wait value("notepad.exe") value(wLogFil).
  end.

IF VALID-HANDLE(h_PrisKo) THEN
    DELETE PROCEDURE h_prisko.

return.

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
def var wDatoTid as char no-undo.

/* Filnavn */
if available SysPara then release SysPara.
{syspara.i 50 1 110 wLogFil}
if wLogFil = "" then
  assign
    wLogFil = "oppdtransDDMMAAHHMM.log".

/* Ugyldig filnavn i parameteroppsett. */
if index(wLogFil,"DDMMAA") = 0 then
  return "02".

/* Lager Dato og Tid strengen */
assign
  wDatoTid = string(day(today),"99") + 
             string(month(today),"99") +
             substring(string(year(today),"9999"),3,2) /* + 
             substring(string(time,"HH:MM"),1,2) +
             substring(string(time,"HH:MM"),4,2) */.

/* Setter inn dato og klokkeslett i filnavnet. */
/*
OVERLAY(wLogFil, index(wLogFil,"DDMMAAHHMM"), 10, "CHARACTER") = wDatoTid.
*/
OVERLAY(wLogFil, index(wLogFil,"DDMMAA"), 6, "CHARACTER") = wDatoTid.

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

return "OK".

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
  def input parameter ipFeilKode as int.
  
  def var wTekst as char no-undo.
  
  assign
    wAntFeil = wAntFeil + 1.

  {syspara.i 100 1 ipFeilKode wTekst}
      
  IF wTekst = "" AND ipFeilKode = 20 THEN
    wTekst = "Overføringstransaksjon på vare som IKKE er lagerstyrt.".

  OUTPUT STREAM LoggData to value(wLogFil) no-echo append.
  put stream LoggData unformatted 
  " Batch: " TransLogg.BatchNr 
  " TransNr: " TransLogg.TransNr 
  " ArtikkelNr: " TransLogg.ArtikkelNr
  " Vg/LøpeNr: " string(TransLogg.Vg) + "/" + string(TransLogg.LopNr) " " 
  wTekst skip.
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
def var wOppdatertAntall as int  no-undo.
def var wLestAntall      as int  no-undo.
def var wStartTid        as int  no-undo.
def var wFerdigTid       as int  no-undo.
def var wBruktTid        as int  no-undo.
def var wDataObjekt      as char no-undo.

def buffer bufBatchLogg for BatchLogg.

assign 
  wAntBest  = 0
  wOkStatus = "AVBRYT".

/* Åpner stream til datafil */
OUTPUT STREAM LoggData to value(wLogFil) no-echo append.
/* Åpner ningsmelding i loggfil */
put stream LoggData unformatted 
  " " skip
  "Batch: " + (if wBatchNr = ? then "?" else string(wBatchNr)) skip
  " -----------------------------------------------------------------------" skip
  " Oppdaterer lager og statistikker  " skip
  " " string(today) " " string(time,"HH:MM:SS") " " userid("dictdb") skip
  " -----------------------------------------------------------------------" skip(1).
OUTPUT STREAM LoggData close.

ASSIGN
  cBatchListe = cBatchListe +
                (IF cBatchListe = ""
                   THEN ""
                   ELSE ",") + 
                STRING(iBatchNr).
  .

do with frame DEFAULT-FRAME:
  IF NOT VALID-HANDLE(h_PrisKo) THEN
      RUN PrisKo.p PERSISTENT SET h_PrisKo.

  /* Startet info */
  assign
    wStartTid = time.
  if valid-handle(wProgram-Handle) then
    run StartInfo in wProgram-Handle (input today, input wStartTid).

  /* Alle ikke ferdigbehandlede batchloger */
  BATCHLOGG1:
  for each BatchLogg NO-LOCK /*WHERE
      BatchLogg.BatchNr = 4817*/:
    wAntFeil  = 0.
    STATUS DEFAULT "Translogg " + STRING(BatchLogg.BatchNr) + ".".

    OPPDAT_TRANS1:
    for each TransLogg NO-LOCK where
      TransLogg.BatchNr = BatchLogg.BatchNr
      break by TransLogg.BatchNr
            by TransLogg.Postert
            by TransLogg.Butik
            by TransLogg.TransNr TRANSACTION:
        IF Translogg.butik <> 101 THEN
            NEXT.
        IF YEAR(TransLogg.Dato) <> iAar THEN
            NEXT.
      {fix-oppdlagertrans.i &Blokk = 1}      
    end. /* OPPDAT_TRANS TRANSATION */

  end. /* BATCHLOGG1 */

  /* Løser eventuelle problemer med locking. */
  if available TransLogg then
    release TransLog.
  if available ArtBas then
    release ArtBas.
  if available ArtLag then
    release Artlag.
  if available Lager then
    release Lager.
  if available StHode then
    release StHode.
  if Available StLinje then
    release StLinje.    
  if available KonvReg then
    release KonvReg.
  IF AVAILABLE MedTrans THEN
    RELEASE MedTrans.
  IF AVAILABLE KundeTrans THEN
    RELEASE KundeTrans.

  /* Brukt info */
  assign
    wOkStatus  = "OK"
    wFerdigTid = time
    wBruktTid  = wFerdigTid - wStartTid.
  if valid-handle(wProgram-Handle) then
    run BruktInfo in wProgram-Handle (input today, input wFerdigTid, input wBruktTid).

  IF VALID-HANDLE(h_PrisKo) THEN
      DELETE PROCEDURE h_PrisKo.
end. /* FRAME */  

/* Ferdig melding i loggfil */
/* Åpner stream til datafil */
OUTPUT STREAM LoggData to value(wLogFil) no-echo append.
put stream LoggData unformatted 
  " " skip
  " -----------------------------------------------------------------------" skip
  " Antall transksjoner: " string(wTotAntall,"zzzzzzz9") skip
  " Antall oppdatert   : " string(wOppdatertAntall,"zzzzzzz9") skip
  " Antall med feilkode: " string(wTotAntall - wOppdatertAntall,"zzzzzzz9") skip
  " Ferdigstatus       : " wOkStatus skip
  " Brukt tid          : " string(wBruktTid,"HH:MM:SS") skip
  " Ferdig             : " string(today) " " string(time,"HH:MM:SS") skip
  " -----------------------------------------------------------------------" skip(1).
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
  def var wDataObjekt as char no-undo.

  def buffer bufStLinje for StLinje.

    /* Oppdaterer statistikker */
    STATISTIKK_DEF:
    for each stdef no-lock
      break by StDef.StTypeId:
    
      /* Henter og kontrollerer perioden. */
      find Periode no-lock where
       Periode.PerId = StDef.PerId no-error.
      if not available Periode then 
        return "NEXT".
    
      /* Henter PeriodeLinje */
      /* Faste perioder.     */
      if Periode.Fast then
        do:
          case Periode.PerId:
            when "AAR"   then /* Kun en linje i et år. */
              wWork = 1. 
            when "MANED" then /* 12 Linjer pr. år */
              wWork = month(TransLogg.Dato).
            when "UKE"   then /* Opptil 53 linjer pr. år */
              do:
                run weeknum.p (TransLogg.Dato, output wWork).
                wWork = int(substring(string(wWork,"999999"),5,2)).
              end.
            when "DAG"   then /* Opptil 366 dager pr. år. */
              wWork = (TransLogg.Dato + 1) - date(01,01,year(TransLogg.Dato)).
          end.
        end.
      /* Fri periode         */
      else do:
        find first PerLin no-lock where
          PerLin.PerId   =  Periode.PerId  and
          PerLin.FraDato <= TransLogg.Dato and 
          PerLin.TilDato >= TransLogg.Dato no-error.
        if not available PerLin then
          find last PerLin where
            PerLin.PerId = Periode.PerId no-error.
        if available PerLin then
          wWork = PerLin.PerLinNr.
        else
          wWork = ?. /* Her er ingenting mer å gjøre. */
      end.
      /* Håndtering av ukjente frie statistikkdefineisjoner. */
      if wWork = 0 or 
         wWork = ? then 
        next STATISTIKK_DEF.
      
      /* Henter/oppretter og stempler statistikkhode */      
      find StHode exclusive-lock where
        StHode.StTypeId = StDef.StTypeId and
        StHode.PerId    = Periode.PerId no-error.
      if not available StHode then
        do:
          create StHode.
          assign
            StHode.StTypeId = StDef.StTypeId
            StHode.PerId    = Periode.PerId.
          assign
            StHode.RegistrertDato = today
            StHode.RegistrertTid  = time
            StHode.RegistrertAv   = userid("dictdb").            
        end.
      assign
        StHode.EDato    = today
        StHode.ETid     = time
        StHode.BrukerId = userid("dictdb").            
        
      /* Setter dataobjekt */
      case StDef.StTypeId:
        when "ARTIKKEL" then          
          wDataObjekt = string(TransLogg.ArtikkelNr,"9999999999999").
        when "HOVEDGR"  then
          do:
            FIND VarGr OF ArtBas NO-LOCK NO-ERROR.
            IF NOT AVAILABLE VarGr THEN NEXT.
            IF CAN-FIND(HuvGr OF VarGr) THEN
              wDataObjekt = string(VarGr.Hg,"9999").
            else do:
/*               assign TransLogg.FeilKode = 9. /* Ukjent hovedgruppe på atikkelen. */ */
/*               run LoggFeilITrans (input TransLogg.FeilKode).                        */
              return "UNDO".
            end.
/*                                                      */
/*             find HuvGr no-lock where                 */
/*               HuvGr.Hg = ArtBas.Hg no-error.         */
/*             if available HuvGr then                  */
/*               wDataObjekt = string(HuvGr.Hg,"9999"). */
/*             else do:                                 */
/*               return "UNDO".                         */
/*             end.                                     */
/*                                                      */
          end.
        when "VAREGR"   then
          wDataObjekt = string(TransLogg.Vg,"999999").
        when "AVDELING"   then
        DO:

            FIND VarGr OF ArtBas NO-LOCK NO-ERROR.
            IF NOT AVAILABLE VarGr THEN NEXT.
            find HuvGr OF VarGr no-lock no-error.
            IF AVAILABLE HuvGr THEN
                FIND Avdeling NO-LOCK WHERE
                  Avdeling.AvdelingNr = HuvGr.AvdelingNr NO-ERROR.
            IF AVAILABLE Avdeling THEN
                wDataObjekt = string(Avdeling.AvdelingNr,"9999").
            else do:
/*               assign TransLogg.FeilKode = 15. /* Ukjent Avdeling på atikkelens hovedgruppe. */ */
/*               run LoggFeilITrans (input TransLogg.FeilKode).                                   */
              return "UNDO".
            end.

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
        when "LEVERAN"  then
          wDataObjekt = string(ArtBas.LevNr,"999999").
        when "LEVERAN-VG"  then
          wDataObjekt = string(ArtBas.LevNr,"999999") + CHR(1) + 
                        string(TransLogg.Vg,"999999").
        when "BUTSTAT" then
          wDataObjekt = string(TransLogg.Butik,"999999").
        when "KUNDSTAT" then
          wDataObjekt = string(TransLogg.KundNr,"9999999999999").
        when "SELGERSTAT" then
          wDataObjekt = string(TransLogg.ForsNr,"9999999999999").
        when "KASS-VG" then
          wDataObjekt = string(TransLogg.ForsNr,"9999999999999") + CHR(1) + 
                        string(TransLogg.Vg,"999999").
        when "SELGER" then
          wDataObjekt = string(TransLogg.SelgerNr,"9999999999999").
        when "SELGER-VG" then
          wDataObjekt = string(TransLogg.SelgerNr,"9999999999999") + CHR(1) + 
                        string(TransLogg.Vg,"999999").
        when "MEDLEM" then
          wDataObjekt = IF TransLogg.MedlemsNr <> 0
                          THEN string(TransLogg.MedlemsNr,"9999999999999")
                          ELSE "9999999999999".
        when "MEDLEMTOT" then
          wDataObjekt = "9999999999999". /* Statistikk totalt for kundeklubb. */
      end case.

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
      find StLinje exclusive-lock where
        StLinje.StTypeId   = StDef.StTypeId and
        StLinje.PerId      = Periode.PerId and
        StLinje.DataObjekt = wDataObjekt and
        StLinje.Diverse    = "" and
        StLinje.Butik      = TransLogg.Butik and
        StLinje.Aar        = year(TransLogg.Dato) and
        StLinje.PerLinNr   = int(wWork) no-error.
      if not available StLinje then
        do:
          create StLinje.
          assign
            StLinje.StTypeId   = StDef.StTypeId 
            StLinje.PerId      = Periode.PerId 
            StLinje.DataObjekt = wDataObjekt 
            StLinje.Diverse    = ""
            StLinje.Butik      = TransLogg.Butik
            StLinje.Aar        = year(TransLogg.Dato)
            StLinje.PerLinNr   = int(wWork).      
        end.

      /* Henter Lager posten */
      find Lager no-lock where
        Lager.ArtikkelNr = TransLogg.ArtikkelNr and
        Lager.Butik      = TransLogg.Butik no-error. /* Denne skal finnes */
      if not available Lager then
        return "UNDO".

      /* Oppdaterer statistikkfeltene */
      case TransLogg.TTId:      
        when 1 then
          do:
          assign /* Varesalg */
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
                                    (if TransLogg.RabKr <> 0
                                      then TransLogg.Antall 
                                      else 0)
            StLinje.VerdiRabatt   = StLinje.VerdiRabatt +
                                    (if TransLogg.RabKr <> 0
                                      then TransLogg.Antall * (Translogg.RabKr - RabUMva(wMva%))
                                      else 0).
/*
message "Rabatt:" RabUMva(wMva%) skip
        "wMva%:" wMva% skip
        "Translogg.RabKr:" TransLogg.RabKr skip
        "Translogg.AntRab:" TransLogg.Antall skip
        "StLinje.VerdiRabatt:" StLinje.VerdiRabatt
view-as alert-box.                                              
*/
          end.                                      
        when 2 then
            assign  /* Brekkasje */
              StLinje.BrekkAnt   = StLinje.BrekkAnt   + TransLogg.Antall  
              StLinje.BrekkVerdi = StLinje.BrekkVerdi + 
                                   (wVareKost * TransLogg.Antall).
        when 3 then 
          do:
            assign  /* Kundereklamasjon */
              StLinje.ReklAnt    = StLinje.ReklAnt    + TransLogg.Antall  
              StLinje.ReklVerdi  = StLinje.ReklVerdi  + 
                                   (wVareKost * TransLogg.Antall)  
              /* Korrigerer rabatt. */
              StLinje.AntRab        = StLinje.AntRab +
                                      (if TransLogg.RabKr <> 0
                                        then TransLogg.Antall 
                                        else 0)
              StLinje.VerdiRabatt   = StLinje.VerdiRabatt +
                                      (if TransLogg.RabKr <> 0
                                        then TransLogg.Antall * (Translogg.RabKr - RabUMva(wMva%))
                                        else 0)
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
          end. 
        when 4 then
            assign  /* Lagerreklamasjon */
              StLinje.ReklLAnt   = StLinje.ReklLAnt   + TransLogg.Antall  
              StLinje.ReklLVerdi = StLinje.ReklLVerdi + 
                                   (wVareKost * TransLogg.Antall).
        when 5 then
          do: /* Varekjøp m/vektet vareverdi */
            assign  
              wWork  = Lager.Lagant   * wVareKost   /* Gammel lagerverdi */
              wWork2 = TransLogg.Pris * TransLogg.Antall. /* Verdi av innkjøp  */
            
            assign          
              StLinje.KjopAnt   = StLinje.KjopAnt    + TransLogg.Antall  
              StLinje.KjopVerdi = StLinje.KjopVerdi  + wWork2.
          end.
        when 6 then
          do: /* Overføring */                
            /* Henter statistikk for mottagende butikk.            */
            /* Litt spesiell håndtering må til for å kunne postere */
            /* overføringer i butikkstatistikken.                  */
            find bufStLinje exclusive-lock where
              bufStLinje.StTypeId   = StDef.StTypeId and
              bufStLinje.PerId      = Periode.PerId and
              bufStLinje.DataObjekt = (IF StDef.StTypeId = "BUTSTAT"
                                        THEN STRING(TransLogg.OvButik,"999999")
                                        ELSE wDataObjekt) and
              bufStLinje.Diverse    = "" and
              bufStLinje.Butik      = TransLogg.OvButik and
              bufStLinje.Aar        = year(TransLogg.Dato) and
              bufStLinje.PerLinNr   = int(wWork) no-error.
            if not available bufStLinje then
              do:
                create bufStLinje.
                assign
                  bufStLinje.StTypeId   = StDef.StTypeId 
                  bufStLinje.PerId      = Periode.PerId 
                  bufStLinje.DataObjekt = (IF StDef.StTypeId = "BUTSTAT"
                                            THEN STRING(TransLogg.OvButik,"999999")
                                            ELSE wDataObjekt)  
                  bufStLinje.Diverse    = ""
                  bufStLinje.Butik      = TransLogg.OvButik
                  bufStLinje.Aar        = year(TransLogg.Dato)
                  bufStLinje.PerLinNr   = int(wWork).      
              end.
            
            assign  /* Trekker ned på FRA butikk.            */
                    /* Posteringer skjer med vektet varekost i fra butikken. */
              StLinje.OvAnt   = StLinje.OvAnt      - TransLogg.Antall  
              StLinje.OvVerdi = StLinje.OvVerdi    - 
                                (wVareKost * TransLogg.Antall).
   
            /* Henter Lager posten til mottagende butikk */
            /* Forutsetter her at VVarekost er satt, fordi varen er lagerstyrt. */
            find bufLager no-lock where
              bufLager.ArtikkelNr = TransLogg.ArtikkelNr and
              bufLager.Butik      = TransLogg.Butik no-error. /* Denne skal finnes */
            if not available bufLager then
              return "UNDO".
              
            assign  /* Posterer i TIL (mottagende) butikk.  */
                    /* Posteringer skjer med vektet varekost i mottagende butikk. */
              bufStLinje.OvAnt     = bufStLinje.OvAnt      + TransLogg.Antall  
              bufStLinje.OvVerdi   = bufStLinje.OvVerdi    + 
                                    (TransLogg.VVareKost * TransLogg.Antall).
          end.
          
        when 7 then
            assign  /* Lagerjustering */
              StLinje.JustAnt   = StLinje.JustAnt   + TransLogg.Antall  
              StLinje.JustVerdi = StLinje.JustVerdi + 
                                  (TransLogg.VVareKost * TransLogg.Antall).
        when 8 then
          do:
            find bufLager no-lock where
              bufLager.ArtikkelNr = TransLogg.ArtikkelNr and
              bufLager.Butik      = TransLogg.Butik no-error. /* Denne skal finnes */
            assign  /* Nedskriving */
                    /* Ingen endring i lagerantall. Kun VVarekost. */
              StLinje.NedAnt    = StLinje.NedAnt   + ((if available bufLager 
                                                        then Lager.LagAnt  
                                                        else 0) *
                                                       (if TransLogg.Antall < 0
                                                         then -1
                                                         else 1))
              StLinje.NedVerdi  = StLinje.NedVerdi + 
                                  (TransLogg.Pris * ((if available bufLager 
                                                        then Lager.LagAnt  
                                                        else 0)) *
                                                       (if TransLogg.Antall < 0
                                                         then -1
                                                         else 1)).
          end.
        when 9 then
            assign  /* Svinn */
              StLinje.SvinnAnt   = StLinje.SvinnAnt   + TransLogg.Antall  
              StLinje.SvinnVerdi = StLinje.SvinnVerdi + 
                                   (TransLogg.Pris * TransLogg.Antall).
        when 10 then
            assign  /* Gjennkjøp */
              StLinje.GjenkjopAnt   = StLinje.GjenkjopAnt    + (TransLogg.Antall * -1) /* Negativt antall i TransLogg */  
              StLinje.GjenkjopVerdi = StLinje.GjenkjopVerdi  + 
                                      (
                                       (TransLogg.Pris - Translogg.RabKr) - 
                                       TransLogg.Mva
                                      ) * TransLogg.Antall
              /* Korrigerer rabatt ved gjennkjøp. */
              StLinje.AntRab        = StLinje.AntRab +
                                      (if TransLogg.RabKr <> 0
                                        then TransLogg.Antall 
                                        else 0)
              StLinje.VerdiRabatt   = StLinje.VerdiRabatt +
                                      (if TransLogg.RabKr <> 0
                                        then TransLogg.Antall * (Translogg.RabKr - RabUMva(wMva%))
                                        else 0)
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
        when 11 then
            assign  /* Internt forbruk */
              StLinje.IntAnt   = StLinje.IntAnt    + TransLogg.Antall  
              StLinje.IntVerdi = StLinje.IntVerdi  + 
                                 (TransLogg.VVareKost * TransLogg.Antall).
       end case.
    
    end. /* STATISTIKK_DEF */      

  return "OK".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

/* ************************  Function Implementations ***************** */

&IF DEFINED(EXCLUDE-RabUMva) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION RabUMva Procedure 
FUNCTION RabUMva RETURNS DECIMAL
  ( wMva% as dec ) :
/*------------------------------------------------------------------------------
  Purpose:  Beregner RABATT - MVA(På rabatten). Dvs nettorabatt.
    Notes:  
------------------------------------------------------------------------------*/

  DEF VAR wWork as DEC NO-UNDO.

  wWork = (TransLogg.RabKr) -
          ((TransLogg.RabKr) / (1 + (wMva% / 100))).
  if wWork = ? THEN wWork = 0.

  RETURN wWork.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SjekkStreng) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION SjekkStreng Procedure 
FUNCTION SjekkStreng RETURNS LOGICAL
  ( input wTekst as char ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  def var wGyldigeChar as char initial "0,1,2,3,4,5,6,7,8,9" no-undo.
  def var wOk          as log  initial true   no-undo.
  def var wLoop        as int                 no-undo.
  
  /* Alle karrakterer i strengen skal finnes i sjekklisten. */  
  SJEKKLOOP:
  do wLoop = 1 to length(wTekst):
    if can-do(wGyldigeChar,substring(wTekst,wLoop,1)) then
      next.
    else do:
      assign wOk = false.
      leave SJEKKLOOP.
    end.
  end. /* SJEKKLOOP */  

  RETURN wOk.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

