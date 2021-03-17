&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : 
    Purpose     :

    Syntax      :

    Description :

    Author(s)   :
    Created     :
    Notes       :
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

def input parameter wProgram-Handle as handle no-undo.

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
  
/* Importvariabler */
def var wLinje     as char no-undo.
def var wFLevKod   as char no-undo.
def var wFSortKod  as char no-undo.
def var wFStlkKod  as char no-undo.
def var wFBeskr    as char no-undo.
def var wSeqNr     as int  no-undo.
def var wFStAnt    as int extent 30 no-undo.
def var wStrTypeId as int  no-undo.

/* Definerer stream */
def stream InnData.
def stream LoggData.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

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

/* Klargjørings dato og tid. */
assign
  wDato = today
  wTid  = time.

RUN ImporterLeverandorsortiment.

/* Sletter procedyrebibloteket hvis det kjøres i batch modus. */
if terminal = "" then
  do:
    if valid-handle(wLibHandle) then
      delete procedure wLibHandle no-error.
  end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ByggFilNavn Procedure 
PROCEDURE ByggFilNavn :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/* Filnavn */
if available SysPara then release SysPara.
{syspara.i 50 10 8 wImpFil}
if wImpFil = "" then
  assign
    wImpFil = "levreg.dat".

/* Katalognavn */
if available SysPara then release SysPara.
if opsys = "unix" 
  then {syspar2.i 50 10 1 wKatalog}
else {syspara.i 50 10 1 wKatalog}
if wKatalog = "" then
  wKatalog = if opsys = "unix" then "." else ".".
if substring(wKatalog,length(wKatalog),1) = "/" or
   substring(wKatalog,length(wKatalog),1) = "\" then
 wKatalog = substring(wKatalog,1,length(wKatalog) - 1).
    
/* Bygger full path til fil */
assign
  wImpFil = wKatalog +
            (if opsys = "unix" 
               then "/"
               else "\") +
            wImpFil.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

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
{syspara.i 50 10 10 wLogFil}
if wLogFil = "" then
  assign
    wLogFil = "impflexMMDDAAHHMM.log".

/* Ugyldig filnavn i parameteroppsett. */
if index(wLogFil,"DDMMAAHHMM") = 0 then
  return "02".

/* Lager Dato og Tid strengen */
assign
  wDatoTid = string(day(today),"99") + 
             string(month(today),"99") +
             substring(string(year(today),"9999"),3,2) + 
             substring(string(time,"HH:MM"),1,2) +
             substring(string(time,"HH:MM"),4,2).

/* Setter inn dato og klokkeslett i filnavnet. */
OVERLAY(wLogFil, index(wLogFil,"DDMMAAHHMM"), 10, "CHARACTER") = wDatoTid.

/* Katalognavn */
if available SysPara then release SysPara.
if opsys = "unix" 
  then {syspar2.i 50 10 9 wLogKatalog}
else {syspara.i 50 10 9 wLogKatalog}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ImporterLeverandorsortiment Procedure 
PROCEDURE ImporterLeverandorsortiment :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
def var wLoop        as int  no-undo.
def var wFeilKode    as int  no-undo.
def var wFeilTekst   as char no-undo.
def var wMomsKod     as int  no-undo.
def var wOk          as log  no-undo.
def var wTekst       as char no-undo.

def buffer bufLevSort for LevSort.
def buffer bufLevSAnt for LevSAnt.

run ByggFilNavn.
run ByggLoggFilNavn.

assign 
  wAntBest  = 0
  wOkStatus = "AVBRYT".

/* Åpner stream til datafil */
INPUT STREAM InnData from value(wImpFil) no-echo.

/* Åpner stream til datafil */
OUTPUT STREAM LoggData to value(wLogFil) no-echo append.
/* Åpner ningsmelding i loggfil */
put stream LoggData unformatted 
  " " skip
  " -----------------------------------------------------------------------" skip
  " Import av leverandørsortiment fra eXcellenc PRO Innkjøpssystem startet         " skip
  " " string(today) " " string(time,"HH:MM:SS") " " userid("dictdb") skip
  " -----------------------------------------------------------------------" skip(1).
OUTPUT STREAM LoggData close.

MAINLOOP:
do with frame DEFAULT-FRAME:
  /* Startet info */
  assign
    wStartTid = time.
  if valid-handle(wProgram-Handle) then
    run StartInfo in wProgram-Handle (input today, input wStartTid).


  IMPORT_AV_DATA:  
  repeat on error undo IMPORT_AV_DATA, leave MAINLOOP TRANSACTION:
     
    /* Leser en og en linje fra filen */
    import stream InnData unformatted wLinje.

    /* Antall poster importert */
    assign
      wTotAntall = wTotAntall + 1.

      /* Ny bestilling. Pakker ut hode, oppretter bestilling og leverandør. */
      DATA:
      do:
        assign
          wAntBest     = wAntBest + 1
          wFLevKod     = trim(substring(wLinje,5,4))
          wFSortKod    = trim(substring(wLinje,9,5))
          wFStlkKod    = trim(substring(wLinje,14,3))
          wFBeskr      = trim(substring(wLinje,17,25)).
          
        /* Sjekker numeriske felt */
        if SjekkStreng(wFLevKod) = false then
          do:
            OUTPUT STREAM LoggData to value(wLogFil) no-echo append.
            put stream LoggData unformatted 
              " Feil i levkode i levsort: " wFLevKod " Linje:" wTotAntall skip. 
            OUTPUT STREAM LoggData close.
            next IMPORT_AV_DATA. 
          end.          
          
        /* Leser inn antall pr. størrelsene */
        /* For at dette skal treffe riktig, må import av størrelesdefinisjoner */
        /* være gjordt på forhånd.                                             */
        /* Størrelsen konverteres til SkoTex format ettersom den leses inn.    */
        do wLoop = 1 to 30:
          assign
            wFStAnt[wLoop] = int(substring(wLinje,31 + (wLoop * 6),6)).
          if valid-handle(wLibHandle) then
            run FiksStorl in wLibHandle (input-output wFStant[wLoop]).                   
        end.  

        /* Konverterer Størrelestypekoden */
        find first StrType no-lock where
          StrType.KortNavn = wFStlkKod no-error.
        if not available StrType then
          do:
            OUTPUT STREAM LoggData to value(wLogFil) no-echo append.
            put stream LoggData unformatted 
              " Finner ikke StrType med kortnavn (Ikke importert): " wFStlkKod 
              " " wFBeskr " ".
            OUTPUT STREAM LoggData close.
            next IMPORT_AV_DATA.
          end.
        else
          wStrTypeId = StrType.StrTypeId.
          
        /* Sjekker at leverandøren finnes */
        find LevBas no-lock where
          LevBas.LevNr = int(wFLevKod) no-error.
        if not available LevBas then
          do:
            OUTPUT STREAM LoggData to value(wLogFil) no-echo append.
            put stream LoggData unformatted 
              " Ukjent leverandør (Ikke importert): " wFLevKod " LevSort: " wFStlkKod 
              " " wFBeskr " ".
            OUTPUT STREAM LoggData close.
            next IMPORT_AV_DATA.
          end.

        /* Oppretter eller oppdaterer leverandørsortimentet.      */
        find LevSort exclusive-lock where
          LevSort.LevNr     = LevBas.LevNr and
          /* LevSort.StrTypeId = wStrTypeId and */
          LevSort.SortId    = wFSortKod no-error.
        if not available LevSort then
          OPPRETT_LEVSORT:
          do:
            create LevSort.
            assign
              LevSort.LevNr     = LevBas.LevNr 
              /* LevSort.StrTypeId = wStrTypeId  */
              LevSort.SortId    = wFSortKod.

            assign
              LevSort.StrTypeId   = wStrTypeId  
              LevSort.Beskrivelse = wFBeskr
              LevSort.Fri         = false.

            /* Oppretter LevSAnt.                                               */          
            /* Dette forutsetter at det er samsvar mellom SeqNr i StrTStr og    */
            /* LevSAnt. Det er bare sant hvis ikke størrelsesredigeringsrutinen */
            /* som benyttes mot StrTStr i SkoTex ikke er kjørt siden import av  */
            /* størrelsesdefinisjoner fra eXcellence PRO.                       */        
            /* Den rutinen kaster om på sekvensnummerne, hvis det tas bort noen */
            /* størrelser. Men det er lite sansynlig at så er tilfelle.         */
            assign wSeqNr = 0.
            do wLoop = 1 to 30:          
              if wFStAnt[wLoop] <> 0 then
                do:
                  assign wSeqNr = wSeqNr + 1.

                  /* Henter størrelsen fra størrelsesdefinisjonen. */
                  find StrTStr no-lock where
                    StrTStr.StrTypeId = wStrTypeId and
                    StrTStr.SeqNr     = wSeqNr no-error.
              
                  /* Oppretter antallsposten. */  
                  find LevSAnt exclusive-lock where
                    LevSAnt.LevNr     = LevBas.LevNr and
                    LevSAnt.SortId    = wFSortKod and
                    LevSAnt.SeqNr     = wSeqNr no-error.
                  if not available LevSAnt then
                    do:
                      create LevSAnt.
                      assign
                        LevSAnt.LevNr     = LevBas.LevNr 
                        LevSAnt.SortId    = wFSortKod
                        LevSAnt.SeqNr     = wSeqNr.
                    end.
                  assign
                    LevSAnt.SoAnt   = wFStAnt[wLoop]
                    LevSAnt.SoStorl = if available StrTStr 
                                        then StrTStr.SoStorl
                                        else "".
                end.
            end.
                
            OUTPUT STREAM LoggData to value(wLogFil) no-echo append.
            put stream LoggData unformatted 
              " LevNr: " wFLevKod
              " StlkKod: " wFStlkKod 
              " " wFBeskr "  ".
              do wLoop = 1 to 30: 
                put stream LoggData wFStAnt[wLoop] " ".       
              end.
            put stream LoggData skip.
            OUTPUT STREAM LoggData close.

          end. /* OPPRETT_LEVSORT */

        /* Info om Bestillingen */
        if valid-handle(wProgram-Handle) then
          run ProfilInfo in wProgram-Handle (input string(wFStlkKod) + " " +
                                        string(wFBeskr)).
      end. /* DATA */    


    /* info om transaksjon */
    assign
      wOppdatertAntall = wOppdatertAntall + 1.
    if valid-handle(wProgram-Handle) then
      run TransInfo in wProgram-Handle
                    (input " ", 
                     input "Importert " + 
                           string(wOppdatertAntall)
                    ).

      
  end. /* IMPORT_AV_DATA */

  /* Brukt info */
  assign
    wOkStatus  = "OK"
    wFerdigTid = time
    wBruktTid  = wFerdigTid - wStartTid.
  if valid-handle(wProgram-Handle) then
    run BruktInfo in wProgram-Handle (input today, input wFerdigTid, input wBruktTid).
    
  if available LevSort then release LevSort.
  if available LevSAnt then release LevSAnt.
  
end. /* FRAME */  

/* Ferdig melding i loggfil */
/* Åpner stream til datafil */
OUTPUT STREAM LoggData to value(wLogFil) no-echo append.
put stream LoggData unformatted 
  " " skip
  " -----------------------------------------------------------------------" skip
  " Antall bestillinger: " string(wAntBest,"zzzzzzz9") skip
  " Antall linjer      : " string(wOppdatertAntall,"zzzzzzz9") skip
  " Ferdigstatus       : " wOkStatus skip
  " Brukt tid          : " string(wBruktTid,"HH:MM:SS") skip
  " Ferdig             : " string(today) " " string(time,"HH:MM:SS") skip
  " -----------------------------------------------------------------------" skip(1).
OUTPUT STREAM LoggData close.

/* Lukker data stream */
INPUT STREAM InnData close.

/* viser resultat til bruker */
if valid-handle(wProgram-Handle) then
  do:
    if search(wLogFil) <> ? then
      os-command no-wait value("notepad.exe") value(wLogFil).
  end.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

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

