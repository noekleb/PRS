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

do with frame DEFAULT-FRAME:

  /* Startet info */
  assign
    wStartTid = time.
  if valid-handle(wProgram-Handle) then
    run StartInfo in wProgram-Handle (input today, input wStartTid).

  /* Alle ikke ferdigbehandlede batchloger */
  if wBatchNr = ? then
  BATCHLOGG1:
  for each BatchLogg no-lock where
    BatchLogg.OppdStatus > 1 and
    BatchLogg.OppdStatus < 4:
    
    /* Sjekker at batchen 100% sikker ikke er låst. */
    do FOR bufBatchLogg TRANSACTION:
      FIND bufBatchLogg exclusive-lock WHERE
        bufBatchLogg.BatchNr = BatchLogg.BatchNr NO-WAIT NO-ERROR.

      IF LOCKED(bufBatchLogg) THEN 
        next BATCHLOGG1.
    END. /* TRANSACTION */

    wAntFeil  = 0.
 
    OPPDAT_TRANS1:
    for each TransLogg exclusive-lock where
      TransLogg.BatchNr = BatchLogg.BatchNr and
      TransLogg.Postert = false
      break by TransLogg.BatchNr
            by TransLogg.Postert
            by TransLogg.Butik
            by TransLogg.TransNr TRANSACTION:     
      {fix-jfxi-oppdlagertrans.i &Blokk = 1}      
    end. /* OPPDAT_TRANS TRANSATION */

    if can-find(first TransLogg where
                  TransLogg.BatchNr = BatchLogg.BatchNr and
                  TransLogg.Postert = false) then
      run batchstatus(BatchLogg.BatchNr, 3). /* Ikke behandlede poster ligger igjen */
    else
      run batchstatus(BatchLogg.BatchNr, 4). /* Alle poster er oppdatert */
  end. /* BATCHLOGG1 */
  else 
    BATCHLOGG2:
    for each BatchLogg no-lock where
      BatchLogg.BatchNr = wBatchNr:
     
      wAntFeil  = 0.
  
      OPPDAT_TRANS2:
      for each TransLogg exclusive-lock where
        TransLogg.BatchNr = BatchLogg.BatchNr and
        TransLogg.Postert = false
        break by TransLogg.BatchNr
              by TransLogg.Postert
              by TransLogg.Butik
              by TransLogg.TransNr TRANSACTION:
        {fix-jfxi-oppdlagertrans.i &Blokk = 2}  
      end. /* OPPDAT_TRANS TRANSATION */

      if can-find(first TransLogg where
                    TransLogg.BatchNr = wBatchNr and
                    TransLogg.Postert = false) then
        run batchstatus(BatchLogg.BatchNr, 3). /* Ikke behandlede poster ligger igjen */
      else
        run batchstatus(BatchLogg.BatchNr, 4). /* Alle poster er oppdatert */
    end. /* BATCHLOGG2 */

  /* Løser problemer med locking. */
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

&ENDIF

&IF DEFINED(EXCLUDE-PosterArtLag) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PosterArtLag Procedure 
PROCEDURE PosterArtLag :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    
    if (ArtBas.Lager = true and 
        ArtBas.Storrelser = true) then
      OPPDAT_ARTLAG:
      do:
        /* Er det en gyldig størrelse */
        find first StrTStr no-lock where
          StrTStr.StrTypeId = ArtBas.StrTypeId and
          StrTStr.SoStorl   = TransLogg.Storl no-error.
        if not available StrTStr then
          do:
            assign TransLogg.FeilKode = 5. /* Størrelsen ligger ikke i størrelsesdefinisjonen. */
            run LoggFeilITrans (input TransLogg.FeilKode).
            /* return "UNDO". -- Oppdateres alikevel ---*/
          end.
        
        /* Hvis ikke ArtLag posten finnes opprettes den. Men transaksjonen */
        /* flagges med en kode for at lagerpost ble opprettet.             */
        find ArtLag exclusive-lock where
          ArtLag.Butik = TransLogg.Butik and
          ArtLag.Vg    = TransLogg.Vg and
          ArtLag.LopNr = TransLogg.LopNr and
          ArtLag.Storl = TransLogg.Storl no-error no-wait.
        if not available ArtLag then
          do:
            assign TransLogg.FeilKode = 6. /* Ukjent artlag. Opprettet ved postering på lager! */
            run LoggFeilITrans (input TransLogg.FeilKode).
            create ArtLag.
            assign
              ArtLag.Butik = TransLogg.Butik
              ArtLag.Vg    = TransLogg.Vg 
              ArtLag.LopNr = TransLogg.LopNr 
              ArtLag.Storl = TransLogg.Storl.              
          end.
          
        case TransLogg.TTId:
          when 1 then /* Varesalg */
            assign
              ArtLag.LagAnt   = ArtLag.LagAnt     - TransLogg.Antall
              ArtLag.AntSolgt = ArtLag.AntSolgt + TransLogg.Antall
              ArtLag.AntRab   = ArtLAg.AntRab +
                                (if TransLogg.RabKr <> 0
                                   then TransLogg.Antall 
                                   else 0).
          when 2 then /* Brekkasje */
            assign
              ArtLag.LagAnt   = ArtLag.LagAnt   - TransLogg.Antall
              ArtLag.BrekkAnt = ArtLag.BrekkAnt + TransLogg.Antall.
          when 3 then /* Kundereklamasjon */
            assign
              ArtLag.LagAnt  = ArtLag.LagAnt  + TransLogg.Antall
              ArtLag.ReklAnt = ArtLag.ReklAnt + TransLogg.Antall
              /* Korrigerer salget ved reklamasjon */
              /* TN 25/11-99 ArtLag.AntSolgt = ArtLag.AntSolgt + TransLogg.Antall */.
          when 4 then /* Lagerreklamasjon */
            assign
              ArtLag.LagAnt   = ArtLag.LagAnt   - TransLogg.Antall
              ArtLag.ReklLAnt = ArtLag.ReklLAnt + TransLogg.Antall.
          when 5 then /* Varekjøp */
            assign
              ArtLag.LagAnt  = ArtLag.LagAnt  + TransLogg.Antall
              ArtLag.KjopAnt = ArtLag.KjopAnt + TransLogg.Antall.          
          when 6 then /* Overføring */
            do:
              assign  /*Inn i butikk. NB: Translogg.Antall er negativ postert i Translogg. */
                ArtLag.LagAnt  = ArtLag.LagAnt - TransLogg.Antall
                ArtLag.OvAnt   = ArtLag.OvAnt  - TransLogg.Antall.
                
              /* Justerer fra butikken. */  
              /* NB: i w-gridlager.w lagres fra størrelsen i TransLogg.TilStorl. */
              find bufArtLag exclusive-lock where
                bufArtLag.Butik = TransLogg.OvButik and
                bufArtLag.Vg    = TransLogg.Vg and
                bufArtLag.LopNr = TransLogg.LopNr and
                bufArtLag.Storl = TransLogg.TilStorl no-error no-wait.
              if locked bufArtLag then
                do:
                  assign TransLogg.FeilKode = 8. /* Artlag låst i mottagende butikk! */
                  run LoggFeilITrans (input TransLogg.FeilKode).
                  return "UNDO".
                end.
              if not available bufArtLag then
                do:
                  assign TransLogg.FeilKode = 7. /* Ukjent artlag i mottagende butikk (Opprettet)! */
                  run LoggFeilITrans (input TransLogg.FeilKode).
                  create bufArtLag.
                  assign
                    bufArtLag.Butik = TransLogg.OvButik
                    bufArtLag.Vg    = TransLogg.Vg 
                    bufArtLag.LopNr = TransLogg.LopNr 
                    bufArtLag.Storl = TransLogg.TilStorl.              
                end.
                
              assign  /*Trekke ned i fra butikken. Husk at TransLogg.Antall er negativ. */
                bufArtLag.LagAnt  = bufArtLag.LagAnt + TransLogg.Antall
                bufArtLag.OvAnt   = bufArtLag.OvAnt  + TransLogg.Antall.
            end.
          when 7 then /* Lagerjustering */
            assign
              ArtLag.LagAnt  = ArtLag.LagAnt  - TransLogg.Antall
              ArtLag.JustAnt = ArtLag.JustAnt + TransLogg.Antall.
          when 8 then. /* Nedskrivning - Påvirker ikke ArtLag. */
          when 9 then /* Svinn */        
            assign
              ArtLag.LagAnt   = ArtLag.LagAnt   - TransLogg.Antall
              ArtLag.SvinnAnt = ArtLag.SvinnAnt + TransLogg.Antall.
          when 10 then /* Gjennkjøp */        
            assign
              ArtLag.LagAnt      = ArtLag.LagAnt      + (TransLogg.Antall * -1) /* Negativt antall i TransLogg */
              ArtLag.GjenkjopAnt = ArtLag.GjenkjopAnt + TransLogg.Antall
              /* Korrigerer rabatter */
              ArtLag.AntRab   = ArtLAg.AntRab +
                                (if TransLogg.RabKr <> 0
                                   then TransLogg.Antall 
                                   else 0)
              /* Korrigerer salget ved gjenkjøp */
              ArtLag.AntSolgt = ArtLag.AntSolgt + TransLogg.Antall.
          when 11 then /* Internt forbruk */        
            assign
              ArtLag.LagAnt = ArtLag.LagAnt - TransLogg.Antall
              ArtLag.IntAnt = ArtLag.IntAnt + TransLogg.Antall.        
        end case.
        
      end. /* OPPDAT_ARTLAG */

  return "OK".

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
    if ArtBas.Lager then
    case TransLogg.TTId:
      when 1 then
          assign /* Varesalg */
            Lager.Lagant     = Lager.Lagant     - TransLogg.Antall   
            Lager.AntSolgt   = Lager.AntSolgt   + TransLogg.Antall  
            Lager.VerdiSolgt = Lager.VerdiSolgt +                                
                               (
                                (TransLogg.Pris - Translogg.RabKr) - 
                                TransLogg.Mva
                               ) * TransLogg.Antall
            Lager.AntRab     = Lager.AntRab +
                               (if TransLogg.RabKr <> 0
                                  then TransLogg.Antall 
                                  else 0)
            Lager.VerdiRabatt = Lager.VerdiRabatt +
                               (if TransLogg.RabKr <> 0
                                  then TransLogg.Antall * (Translogg.RabKr - RabUMva(wMva%))
                                  else 0).
      when 2 then
          assign  /* Brekkasje */
            Lager.Lagant     = Lager.Lagant     - TransLogg.Antall   
            Lager.BrekkAnt   = Lager.BrekkAnt   + TransLogg.Antall  
            Lager.BrekkVerdi = Lager.BrekkVerdi + 
                               (TransLogg.VVareKost * TransLogg.Antall).
      when 3 then 
        do:
          assign  /* Kundereklamasjon */
            Lager.Lagant     = Lager.Lagant     + TransLogg.Antall   
            Lager.ReklAnt    = Lager.ReklAnt    + TransLogg.Antall  
            Lager.ReklVerdi  = Lager.ReklVerdi  + 
                               (TransLogg.VVareKost * TransLogg.Antall).
        end. 
      when 4 then
          assign  /* Lagerreklamasjon */
            Lager.Lagant     = Lager.Lagant     - TransLogg.Antall   
            Lager.ReklLAnt   = Lager.ReklLAnt   + TransLogg.Antall  
            Lager.ReklLVerdi = Lager.ReklLVerdi + 
                               (TransLogg.VVareKost * TransLogg.Antall).
      when 5 then
        do: /* Varekjøp m/vektet vareverdi */
            /* TN 5/4-00                                                    */
            /* Ved slasking av varer blir verdien av "innleveransen" lik    */
            /* verdien av hva som ligger på lager fra før i "Fra" butikken. */
            /* Derfor må wWork3 settes lik Lager.VVarekost hvis den nye     */
            /* VVareKost beregnes lik ?.                                    */
          assign  
            wWork  = ABSOLUTE(Lager.Lagant   * Lager.VVareKost)  /* Gammel lagerverdi */
            wWork2 = ABSOLUTE(TransLogg.Pris * TransLogg.Antall) /* Verdi av innkjøp  */
            wWork3 = (wWork + wWork2) / (ABSOLUTE(Lager.LagAnt) + ABSOLUTE(TransLogg.Antall))
            wWork3 = if wWork3 = ? then Lager.VVareKost else wWork3.
            
          assign          
            Lager.Lagant    = Lager.Lagant     + TransLogg.Antall   
            Lager.VVareKost = wWork3 /* Setter ny vektet snittpris */
            Lager.KjopAnt   = Lager.KjopAnt    + TransLogg.Antall  
            Lager.KjopVerdi = Lager.KjopVerdi  + wWork2.
        end.
      when 6 then
        do: /* Overføring */                
          /* Henter lager for mottagende butikk */
          find bufLager exclusive-lock where
            bufLager.ArtikkelNr = TransLogg.ArtikkelNr and
            bufLager.Butik      = TransLogg.OvButik no-error.
          if not available bufLager then
            do:
              /* Det opprettes artlagposter på alle butikker når artikkelen opprettes. */
              /* Hvis det kommer nye butikker, opprettes det alikevel poster her.      */
              assign 
                TransLogg.FeilKode = 2. /* Lagerpost finnes ikke for mottagende butikk.*/
              run LoggFeilITrans (input TransLogg.FeilKode).
              /* return "NEXT". */
              create bufLager.
              assign
                bufLager.ArtikkelNr = TransLogg.ArtikkelNr
                bufLager.Butik      = TransLogg.OvButik.
            end.

          assign  /* Trekker ned lager på fra butikk.            */
                  /* Alle posteringer skjer med vektet varekost. */
            Lager.Lagant  = Lager.Lagant     - TransLogg.Antall   
            Lager.OvAnt   = Lager.OvAnt      - TransLogg.Antall  
            Lager.OvVerdi = Lager.OvVerdi    - 
                               (TransLogg.VVareKost * TransLogg.Antall).
   
          /* Innleveranse medfører ny vekting av varekost i mottagende butikk */
          assign  
            wWork  = ABSOLUTE(bufLager.Lagant * bufLager.VVareKost)   /* Gammel lagerverdi */
            wWork2 = ABSOLUTE(TransLogg.VVareKost * TransLogg.Antall) /* Verdi av overføring  */
            wWork3 = (wWork + wWork2) / (ABSOLUTE(bufLager.LagAnt) + ABSOLUTE(TransLogg.Antall))
            wWork3 = if wWork3 = ? then bufLager.VVareKost else wWork3.
            
          assign  /* Posterer i mottagende butikk.  */
                  /* Alle posteringer skjer med vektet varekost. */
            bufLager.Lagant    = bufLager.Lagant     + TransLogg.Antall   
            bufLager.VVareKost = wWork3 /* Setter ny vektet snittpris */
            bufLager.OvAnt     = bufLager.OvAnt      + TransLogg.Antall  
            bufLager.OvVerdi   = bufLager.OvVerdi    + 
                                  (TransLogg.VVareKost * TransLogg.Antall).
/*
message 
"wWork:" wWork skip            
"wWork2:" wWork2 skip            
"wWork3:" wWork3 skip            
"buflager.lagant:" buflager.lagant skip
"buflager.vvarekost:" buflager.vvarekost skip
"lager.lagant:" lager.lagant skip
"lager.vvarekost:" lager.vvarekost skip
view-as alert-box.
*/
        end.
      when 7 then
          assign  /* Lagerjustering */
            Lager.Lagant    = Lager.Lagant    - TransLogg.Antall   
            Lager.JustAnt   = Lager.JustAnt   + TransLogg.Antall  
            Lager.JustVerdi = Lager.JustVerdi + 
                               (TransLogg.VVareKost * TransLogg.Antall).
      when 8 then
        do:
          assign  /* Nedskrivning */
                  /* Ingen endring i lagerantall. Kun VVarekost. */
            Lager.NedVerdi  = Lager.NedVerdi  + 
                              (TransLogg.Pris * TransLogg.Antall)
            Lager.VVareKost = Lager.VVareKost - (TransLogg.Pris * if TransLogg.Antall < 0
                                                                    then -1
                                                                    else 1)   
            Lager.NedAnt    = Lager.NedAnt    + TransLogg.Antall.
        end.    
      when 9 then
          assign  /* Svinn */
            Lager.LagAnt     = Lager.LagAnt    - TransLogg.Antall
            Lager.SvinnAnt   = Lager.SvinnAnt  + TransLogg.Antall  
            Lager.SvinnVerdi = Lager.SvinnVerdi  + 
                               (TransLogg.Pris * TransLogg.Antall).
      when 10 then
          assign  /* Gjennkjøp */
            Lager.LagAnt        = Lager.LagAnt         + (TransLogg.Antall * -1) /* Negativt antall i TransLogg */
            Lager.GjenkjopAnt   = Lager.GjenkjopAnt    + TransLogg.Antall  
            Lager.GjenkjopVerdi = Lager.GjenkjopVerdi  + 
                               (
                                (TransLogg.Pris - Translogg.RabKr) - 
                                TransLogg.Mva
                               ) * TransLogg.Antall
            Lager.AntRab     = Lager.AntRab +
                               (if TransLogg.RabKr <> 0
                                  then TransLogg.Antall 
                                  else 0)
            Lager.VerdiRabatt = Lager.VerdiRabatt +
                               (if TransLogg.RabKr <> 0
                                  then TransLogg.Antall * (Translogg.RabKr - RabUMva(wMva%))
                                  else 0)
            /* Korrigerer salget ved retur - NB: Antallet er negativt i transaksjonen. */
            Lager.AntSolgt   = Lager.AntSolgt   + TransLogg.Antall           
            Lager.VerdiSolgt = Lager.VerdiSolgt + 
                               (
                                (TransLogg.Pris - Translogg.RabKr) - 
                                TransLogg.Mva
                               ) * TransLogg.Antall.
      when 11 then
          assign  /* Internt forbruk */
            Lager.LagAnt   = Lager.LagAnt    - TransLogg.Antall
            Lager.IntAnt   = Lager.IntAnt    + TransLogg.Antall  
            Lager.IntVerdi = Lager.IntVerdi  + 
                               (TransLogg.VVareKost * TransLogg.Antall).
    end case.
    
  return "OK".

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
            find HuvGr no-lock where
              HuvGr.Hg = ArtBas.Hg no-error.
            if available HuvGr then
              wDataObjekt = string(HuvGr.Hg,"9999").
            else do:
              assign TransLogg.FeilKode = 9. /* Ukjent hovedgruppe på artikkelen. */
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
            /* Korrigerer rabatt ved gjennkjøp. */
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
        /*
        when 2 then
            assign  /* Brekkasje */
              StLinje.BrekkAnt   = StLinje.BrekkAnt   + TransLogg.Antall  
              StLinje.BrekkVerdi = StLinje.BrekkVerdi + 
                                   (wVareKost * TransLogg.Antall).
        when 3 then 
          do:
            assign  /* Kundereklamasjon */
                    /* Skal salget trekkes ned? - I tilfelle hvilken verdi skal benyttes? */
              StLinje.ReklAnt    = StLinje.ReklAnt    + TransLogg.Antall  
              StLinje.ReklVerdi  = StLinje.ReklVerdi  + 
                                   (wVareKost * TransLogg.Antall).           
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
            /* Henter statistikk for mottagende butikk */
            find bufStLinje exclusive-lock where
              bufStLinje.StTypeId   = StDef.StTypeId and
              bufStLinje.PerId      = Periode.PerId and
              bufStLinje.DataObjekt = wDataObjekt and
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
                  bufStLinje.DataObjekt = wDataObjekt 
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
        */
        when 10 then
            assign  /* Gjennkjøp */
              StLinje.GjenkjopAnt   = StLinje.GjenkjopAnt    + (TransLogg.Antall * -1) /* Negativt antall i TransLogg */  
              StLinje.GjenkjopVerdi = StLinje.GjenkjopVerdi  + 
                                      (
                                       (TransLogg.Pris - Translogg.RabKr) - 
                                       TransLogg.Mva
                                      ) * TransLogg.Antall
              /* Korrigerer rabatt ved gjennkjøp. */
              StLinje.AntRab        = Lager.AntRab +
                                      (if TransLogg.RabKr <> 0
                                        then TransLogg.Antall 
                                        else 0)
              StLinje.VerdiRabatt   = Lager.VerdiRabatt +
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
        /*
        when 11 then
            assign  /* Internt forbruk */
              StLinje.IntAnt   = StLinje.IntAnt    + TransLogg.Antall  
              StLinje.IntVerdi = StLinje.IntVerdi  + 
                                 (TransLogg.VVareKost * TransLogg.Antall).
        */
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

