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

def input parameter wProgram-Handle as handle no-undo.
def input parameter wBatchNr        as int    no-undo.

def var wTotAntall as dec format "zzz,zzz,zz9" no-undo.
def var wWork            as dec  no-undo.
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
  
/* Definerer stream */
def stream InnData.
def stream LoggData.

/* Definerer buffere */
def buffer bufLager     for Lager.
def buffer bufArtLag    for ArtLag.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD RabUMva Procedure 
FUNCTION RabUMva RETURNS DECIMAL
  ( wMva% as dec )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD SjekkStreng Procedure 
FUNCTION SjekkStreng RETURNS LOGICAL
  ( input wTekst as char )  FORWARD.

/* _UIB-CODE-BLOCK-END */
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

{runlib.i}

/* Kode for låsing av artikkelnummer ved overføring. */
{syspara.i 1 2 3 wEDB-System}
if wEDB-System = "" then
  wEDB-System = "OVERFOR-LOCK".
{syspar2.i 1 2 3 wTabell}
if wEDB-System = "" then
  wEDB-System = "ArtBas".

/* Klargjørings dato og tid. */
assign
  wDato = today
  wTid  = time.

run ByggLoggFilNavn.

/* Klargjør priskøen. */
RUN OppdaterLagerOgStat.

return.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ByggLoggFilNavn Procedure 
PROCEDURE ByggLoggFilNavn :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
def var wDatoTid as char no-undo.

/* Filnavn */
assign
  wLogFil = "FixRabDDMMAAHHMM.log".

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

/* Katalognavn */
if available SysPara then release SysPara.
if opsys = "unix" 
  then {syspar2.i 50 1 100 wLogKatalog}
else {syspara.i 50 1 100 wLogKatalog}

if wLogKatalog = "" then
  wLogKatalog = if opsys = "unix" then "." else ".".
if substring(wLogKatalog,length(wLogKatalog),1) = "/" or
   substring(wLogKatalog,length(wLogKatalog),1) = "\" then
 wLogKatalog = substring(wLogKatalog,1,length(wLogKatalog) - 1).
    
/* Bygger full path til fil */
assign
  wLogFil = wLogKatalog +
            (if opsys = "unix" 
               then "/"
               else "\") +
            wLogFil.

return "OK".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

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
def var wOldButNR        as int no-undo.

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

do with frame DEFAULT-FRAME:

  /* Startet info */
  assign
    wStartTid = time.
  if valid-handle(wProgram-Handle) then
    run StartInfo in wProgram-Handle (input today, input wStartTid).

  /* Alle ikke ferdigbehandlede batchloger */
  if wBatchNr = ? then
  BATCHLOGG1:
  do:

    wAntFeil  = 0.
 
    OPPDAT_TRANS1:
    for each translogg where RAbKr <> 0 and
      can-do("1,3,10",string(TransLogg.TTid)) and
      TransLogg.Postert = true no-lock:      
      {x-fixrab.i &Blokk = 1}      
    end. /* OPPDAT_TRANS */
  end. /* BATCHLOGG1 */
  else 
    BATCHLOGG2:
    for each BatchLogg no-lock where
      BatchLogg.BatchNr = wBatchNr:
     
      wAntFeil  = 0.
  
      OPPDAT_TRANS2:
      for each translogg of BatchLogg where RAbKr <> 0 and
        can-do("1,3,10",string(TransLogg.TTid)) and
        TransLogg.Postert = true no-lock:      
        {x-fixrab.i &Blokk = 2}  
      end. /* OPPDAT_TRANS TRANSATION */
    end. /* BATCHLOGG2 */

  /* Løser problemer med locking. */
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

  /* Brukt info */
  assign
    wOkStatus  = "OK"
    wFerdigTid = time
    wBruktTid  = wFerdigTid - wStartTid.
  if valid-handle(wProgram-Handle) then
    run BruktInfo in wProgram-Handle (input today, input wFerdigTid, input wBruktTid).
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

/* viser resultat til bruker */
if valid-handle(wProgram-Handle) and terminal <> "" then
  do:
    if search(wLogFil) <> ? then
      os-command no-wait value("notepad.exe") value(wLogFil).
  end.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PosterLager Procedure 
PROCEDURE PosterLager :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    /* Oppdater lagerpost hvis artikkelen har lagerstyring. */
    if ArtBas.Lager then
    case TransLogg.TTId:
      when 1 then
          assign /* Varesalg */
            Lager.VerdiSolgt = Lager.VerdiSolgt + 
                                (RabUMva(wMva%) * TransLogg.Antall)* -1.
                                
      when 3 then 
        do:
          assign  /* Kundereklamasjon */
            Lager.ReklVerdi  = Lager.ReklVerdi  + 
                               (Lager.VVareKost * TransLogg.Antall)
            /* Korrigerer salget ved retur - NB: Antallet er negativt i transaksjonen. */
            Lager.VerdiSolgt = Lager.VerdiSolgt + 
                               (RabUMva(wMva%) * TransLogg.Antall) * -1.
        end. 
      when 10 then
          assign  /* Gjennkjøp */
            Lager.GjenkjopVerdi = Lager.GjenkjopVerdi  + 
                               (RabUMva(wMva%) * TransLogg.Antall) * -1
            /* Korrigerer salget ved retur - NB: Antallet er negativt i transaksjonen. */
            Lager.VerdiSolgt = Lager.VerdiSolgt + 
                               (RabUMva(wMva%) * TransLogg.Antall) * -1.
    end case.
    
  return "OK".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

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
    for each stdef no-lock where
      StDef.StTypeId = "ARTIKKEL"    
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
            find HuvGr no-lock where
              HuvGr.Hg = ArtBas.Hg no-error.
            if available HuvGr then
              wDataObjekt = string(HuvGr.Hg,"9999").
            else do:
              /*assign TransLogg.FeilKode = 9. /* Ukjent hovedgruppe på artikkelen. */*/
              run LoggFeilITrans (input TransLogg.FeilKode).
              return "UNDO".
            end.
          end.
        when "VAREGR"   then
          wDataObjekt = string(TransLogg.Vg,"9999").
        when "LEVERAN"  then
          wDataObjekt = string(ArtBas.LevNr,"999999").
        when "BUTSTAT" then
          wDataObjekt = string(TransLogg.Butik,"999999").
        when "KUNDSTAT" then
          wDataObjekt = string(TransLogg.KundNr,"9999999999999").
        when "SELGERSTAT" then
          wDataObjekt = string(TransLogg.ForsNr,"9999999999999").
      end case.
      
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
/*
message "Verdi av rabatt:" RabUMva(wMva%) wMva% TransLogg.Antall skip
        "Sumrabatt:" (RabUMva(wMva%) * TransLogg.Antall) * -1 view-as alert-box.
*/        
      /* Oppdaterer statistikkfeltene */
      case TransLogg.TTId:      
        when 1 then
          assign /* Varesalg */
            StLinje.VerdiSolgt = StLinje.VerdiSolgt + 
                               (RabUMva(wMva%) * TransLogg.Antall) * -1
            StLinje.MvaVerdi   = StLinje.MvaVerdi + 
                                 (TransLogg.Mva * TransLogg.Antall).
        when 3 then 
          do:
            assign  /* Kundereklamasjon */
              /* Korrigerer salget ved reklamasjon */
              StLinje.VerdiSolgt = StLinje.VerdiSolgt + 
                                 (RabUMva(wMva%) * TransLogg.Antall) * -1
              StLinje.MvaVerdi   = StLinje.MvaVerdi + 
                                 (TransLogg.Mva * TransLogg.Antall).
          end. 
        when 10 then
            assign  /* Gjennkjøp */
              StLinje.GjenkjopVerdi = StLinje.GjenkjopVerdi  + 
                                      (RabUMva(wMva%) * TransLogg.Antall) * -1

              /* Korrigerer salget ved gjenkjøp */
              StLinje.VerdiSolgt = StLinje.VerdiSolgt + 
                                 (RabUMva(wMva%) * TransLogg.Antall) * -1.
       end case.
    
    end. /* STATISTIKK_DEF */      

  return "OK".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION RabUMva Procedure 
FUNCTION RabUMva RETURNS DECIMAL
  ( wMva% as dec ) :
/*------------------------------------------------------------------------------
  Purpose:  Beregner rabatt eksklusive mva.
    Notes:  
------------------------------------------------------------------------------*/

  DEF VAR wWork as DEC NO-UNDO.

  wWork = ((TransLogg.RabKr) / (1 + (wMva% / 100))).
  if wWork = ? THEN wWork = 0.

  RETURN wWork.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

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

