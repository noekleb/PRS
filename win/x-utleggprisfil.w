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
def var wOk              as int  no-undo.

/* Definerer stream */
DEF STREAM ut_pp.
def stream ut_plu.

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

{runlib.i}

/* Klargjørings dato og tid. */
assign
  wDato = today
  wTid  = time.

/* Klargjør priskøen. */
run EksporterPriser.

/* Sletter procedyrebibloteket hvis det kjøres i batch modus. */
if terminal = "" then
  do:
    if valid-handle(wLibHandle) then
      delete procedure wLibHandle no-error.
  end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-EksporterPriser) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE EksporterPriser Procedure 
PROCEDURE EksporterPriser :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  def var wOppdatertAntall as int  no-undo.
  def var wStartTid        as int  no-undo.
  def var wFerdigTid       as int  no-undo.
  def var wBruktTid        as int  no-undo.
  def var wTekst           as char no-undo.
  def var wEkstent         as int  no-undo.
  def var wPluFil          as char no-undo.
  def var wOldPluFil       as char no-undo.
  def var wKatalog         as char no-undo.
  def var wProfilListe     as char no-undo.
  def var wPrisTxt         as char no-undo.
  def var wBongtekst       as char no-undo.
  DEF VAR wCl              AS INT  NO-UNDO.

DEF BUFFER clButiker FOR Butiker.

/* Sentrallageret */
{syspara.i 5 1 1 wCl INT}
FIND clButiker NO-LOCK WHERE
    clButiker.Butik = wCl NO-ERROR.

/* Filnavn */
if available SysPara then
  release SysPara.
{syspara.i 50 1 10 wPluFil}
if wPluFil = "" then
  assign
    wPluFil = "dataout.plu".

/* Katalognavn */
if available SysPara then
  release SysPara.
if opsys = "unix" 
  then {syspar2.i 50 1 11 wKatalog}
else {syspara.i 50 1 11 wKatalog}
if wKatalog = "" then
  wKatalog = if opsys = "unix" then "." else ".".
if substring(wKatalog,length(wKatalog),1) = "/" or
   substring(wKatalog,length(wKatalog),1) = "\" then
 wKatalog = substring(wKatalog,1,length(wKatalog) - 1).
    
/* Bygger full path til fil */
assign
  wOldPluFil = wKatalog +
               (if opsys = "unix" 
                  then "/"
                  else "\") +
               wPluFil.

/* Bygger profilliste */
for each Butiker no-lock where
  Butiker.Butik > 0:
  if not can-do(wProfilListe,string(Butiker.ProfilNr)) then
    assign
      wProfilListe = wProfilListe +
                     (if wProfilListe <> ""
                        then ","
                        else "") + 
                     string(Butiker.ProfilNr). 
end.

/* Feil i filnavn */
if index(wOldPluFil,"N") = 0 then
  return.

PROFIL:  
for each PrisProfil no-lock 
  break by PrisProfil.ProfilNr:

  /* Setter inn profilnummer i filnavnet. */
  assign wPluFil = wOldPluFil.
  OVERLAY(wPluFil, index(wPluFil,"N"), 3, "CHARACTER") = string(PrisProfil.ProfilNr,"999").

  /* Tar bort gammele plu-filer. */
  if search(wPluFil) <> ? then 
    os-delete value(wPluFil).
end.

do with frame DEFAULT-FRAME:
  /* Startet info */
  assign
    wStartTid = time.
  if valid-handle(wProgram-Handle) then
    run StartInfo in wProgram-Handle (input today, input wStartTid).

  PROFIL:  
  for each PrisProfil no-lock 
    break by PrisProfil.ProfilNr:
  
    /* Setter inn profilnummer i filnavnet. */
    assign wPluFil = wOldPluFil.
    OVERLAY(wPluFil, index(wPluFil,"N"), 3, "CHARACTER") = string(PrisProfil.ProfilNr,"999").

    /* Åpner stream */
    OUTPUT STREAM ut_plu TO value(wPluFil) no-echo.

    /* Kun aktive profiler */  
    if not can-do(wProfilListe,string(PrisProfil.ProfilNr)) then
       next PROFIL.
    
    /* Info om butikk */
    if valid-handle(wProgram-Handle) then
      run ProfilInfo in wProgram-Handle (input string(PrisProfil.ProfilNr) + " " +
                                    PrisProfil.KortNavn + " " + 
                                    PrisProfil.Beskrivelse).

    /* For hver profil klarjøres PrisKøen. */
    OPPDAT_TRANS:
    for each ArtBas no-lock where
      ArtBas.Aktivert = true
      by ArtBas.Aktivert
      by ArtBas.Vg
      by ArtBas.LopNr:
      
      /* Bruker avbryter. */
      if TERMINAL <> "" then
        do:
          PROCESS EVENTS.
          if wStop then
            do:
              message "Skal eksportrutinen? " view-as alert-box buttons YES-NO 
                       set wStop.
              if wStop = true then
                run AvbrytOppdatering.
            end.
        end.

      /* Artikkler som ikke er tildelt løpenummer, skal ikke ut. */
      if ArtBas.LopNr = 0 or 
         ArtBas.LopNr = ? then
        next OPPDAT_TRANS.      

      /* Henter prisfilen */
      find ArtPris of ArtBas no-lock where
        ArtPris.ProfilNr = PrisProfil.ProfilNr no-error.
      /* Finnes ikke kalkylen for butikk og profil, benyttes */
      /* sentrallagerets kalkyle.                            */
      IF NOT AVAILABLE ArtPris OR Artpris.Pris[IF ArtPris.Tilbud THEN 2 ELSE 1] = 0 THEN
      DO:
        FIND ArtPris OF ArtBas NO-LOCK WHERE
            ArtPris.ProfilNr = clButiker.ProfilNr NO-ERROR.
      END.
      if not available ArtPris then
        next OPPDAT_TRANS.      

      /* info om transaksjon */
      assign
        wOppdatertAntall = wOppdatertAntall + 1.
      if valid-handle(wProgram-Handle) then
        run TransInfo in wProgram-Handle
                      (input "Artikkel: " +
                             string(ArtBas.Vg) + "/" +
                             string(ArtBas.LopNr) + " " + 
                             string(ArtBas.Beskr), 
                       input "Eksportert " + 
                             string(wOppdatertAntall)
                      ).

      
      /* Tilbud er aktivt */
      if ArtPris.Tilbud then
        wEkstent = 2.
      else
        wEkstent = 1.
        
      /* Bughider - Nullpriser skal ikke ut! */
      if wEkstent = 2 and ArtPris.Pris[2] <= 0 then
        wEkstent = 1.
      if ArtPris.Pris[wEkstent] <= 0 then
        next OPPDAT_TRANS.
        
      /* Bygger eksportstrengen */
      
      IF artbas.bongtekst = "" THEN 
      DO:    
          FIND vargr OF artbas NO-LOCK NO-ERROR.
          IF AVAILABLE vargr THEN wBongtekst = vargr.VgBeskr.
      END.
      ELSE wBongtekst = ArtBas.BongTekst.

      assign
        wPrisTxt = (if ArtPris.Pris[wEkstent] > 99999 
                    then STRING(ArtPris.Pris[wEkstent] / 1000,"zzzz9.99") 
                    else STRING(ArtPris.Pris[wEkstent],"zzzz9.99")).
      OVERLAY(wPrisTxt, index(wPrisTxt,","), 1, "CHARACTER") = ".".
      assign
        wTekst   = "1A" + 
                 "      " + 
                 STRING(ArtBas.Vg * 10000 + ArtBas.LopNr,"9999999") + 
                 "0" + 
                 substring(wBongTekst,1,21) + 
                 fill(" ",21 - length(substring(wBongTekst,1,21))) + 
                 wPrisTxt.
              
      /* Eksporterer posten */
      PUT STREAM ut_plu unformatted wTekst skip.

      /* Antall poster som skal klagjøres. */
      assign
        wTotAntall = wTotAntall + 1.

    end. /* OPPDAT_TRANS */
  
    /* Lukker stream */
    OUTPUT STREAM ut_plu close.

  end. /* PROFIL */

  /* Brukt info */
  assign
    wFerdigTid = time
    wBruktTid  = wFerdigTid - wStartTid.
  if valid-handle(wProgram-Handle) then
    run BruktInfo in wProgram-Handle (input today, input wFerdigTid, input wBruktTid).
  
end. /* FRAME */  

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

