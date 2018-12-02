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
def var wLinje           as char no-undo.
def var wFPostTyp        as char no-undo.
def var wFVgNr           as char no-undo. 
def var wFTekst          as char no-undo.
def var wFDb%            as char no-undo.
def var wMomsKod         as int  no-undo.

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

/* Klargjør priskøen. */
run ImporterVaregrupper.

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
{syspara.i 50 10 3 wImpFil}
if wImpFil = "" then
  assign
    wImpFil = "artgrp.dat".

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ImporterVaregrupper Procedure 
PROCEDURE ImporterVaregrupper :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
def var wLoop        as int  no-undo.
def var wFeilKode    as int  no-undo.
def var wFeilTekst   as char no-undo.

if available SysPara then release SysPara.
{syspara.i 50 10 100 wMomsKod INT}
  
run ByggFilNavn.
run ByggLoggFilNavn.

assign 
  wAntBest  = 0
  wOkStatus = "AVBRYT".

/* Åpner stream til datafil */
INPUT STREAM InnData from value(wImpFil) no-echo.

/* Åpner ningsmelding i loggfil */
OUTPUT STREAM LoggData to value(wLogFil) no-echo append.
put stream LoggData unformatted 
  " " skip
  " -----------------------------------------------------------------------" skip
  " Import av varegrupper fra eXcellenc PRO Innkjøpssystem startet         " skip
  " " string(today) " " string(time,"HH:MM:SS") " " userid("dictdb") skip
  " -----------------------------------------------------------------------" skip(1).
OUTPUT STREAM LoggData close.

TRANSBLOKK:
do with frame DEFAULT-FRAME TRANSACTION:
  /* Startet info */
  assign
    wStartTid = time.
  if valid-handle(wProgram-Handle) then
    run StartInfo in wProgram-Handle (input today, input wStartTid).


  IMPORT_AV_DATA:  
  repeat on error undo TRANSBLOKK, leave TRANSBLOKK:
     
    /* Leser en og en linje fra filen */
    import stream InnData unformatted wLinje.

    /* Antall poster importert */
    assign
      wTotAntall = wTotAntall + 1.

    /* Ny bestilling. Pakker ut hode, oppretter bestilling og leverandør. */
    if substring(wLinje,1,1) = "1" then
      BESTILLINGS-HODE:
      do:
        assign
          wAntBest     = wAntBest + 1
          wFVgNr       = trim(substring(wLinje,5,3))
          wFTekst      = trim(substring(wLinje,10,27))
          wFDb%        = trim(substring(wLinje,39,6)).
           
        /* Oppretter eller oppdaterer varegruppen.      */
        find VarGr exclusive-lock where
          VarGr.Vg = int(wFVgNr) no-error.
        if not available VarGr then
          do:
            create VarGr.
            assign
              wAntBest      = wAntBest + 1
              VarGr.Vg      = int(wFVgNr)
              VarGr.MomsKod = wMomsKod.
          end.
        assign
          VarGr.VgBeskr = wFTekst.
          
        OUTPUT STREAM LoggData to value(wLogFil) no-echo append.
        put stream LoggData unformatted 
          " " string(int(wFVgNr),"zz9") 
          " " wFTekst skip. 
        OUTPUT STREAM LoggData close.

        /* Info om Bestillingen */
        if valid-handle(wProgram-Handle) then
          run ProfilInfo in wProgram-Handle (input string(wFVgNr) + " " +
                                        string(wFTekst)).
      end. /* BESTILLINGS-HODE */    


    /* info om transaksjon */
    assign
      wOppdatertAntall = wOppdatertAntall + 1.
    /*
    if valid-handle(wProgram-Handle) then
      run TransInfo in wProgram-Handle
                    (input "Artikkel: " +
                           string(ArtBas.Vg) + "/" +
                           string(ArtBas.LopNr) + " " + 
                           string(ArtBas.Beskr), 
                     input "Importert " + 
                           string(wOppdatertAntall)
                    ).
     */

      
  end. /* IMPORT_AV_DATA */

  /* Brukt info */
  assign
    wOkStatus  = "OK"
    wFerdigTid = time
    wBruktTid  = wFerdigTid - wStartTid.
  if valid-handle(wProgram-Handle) then
    run BruktInfo in wProgram-Handle (input today, input wFerdigTid, input wBruktTid).
  
end. /* FRAME */  

/* Ferdig melding i loggfil */
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

/*
  /* Tar bort gammele plu-filer. */
  if search(wPluFil) <> ? then 
    os-delete value(wPluFil).
*/    

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

