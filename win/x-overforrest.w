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
def input parameter wRecid1         as recid  no-undo.
def input parameter wRecid2         as recid  no-undo.

def var wTotAntall as dec format "zzz,zzz,zz9" no-undo.
def var wWork            as dec  no-undo.
def var wWork2           as dec  no-undo.
def var wWork3           as dec  no-undo.
def var wStop            as log  initial false no-undo.
def var wDato            as date no-undo.
def var wTid             as int  no-undo.
def var wOk              as int  no-undo.
DEFINE VARIABLE dFraArtikkelnr AS DECIMAL     NO-UNDO.
DEFINE VARIABLE dTilArtikkelnr AS DECIMAL     NO-UNDO.
{etikettlogg.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&IF DEFINED(EXCLUDE-FiksStorl) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD FiksStorl Procedure 
FUNCTION FiksStorl RETURNS CHARACTER
  ( input wStorl as char )  FORWARD.

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

/* Klargjørings dato og tid. */
assign
  wDato = today
  wTid  = time.

/* Klargjør priskøen. */
run SkapTransLogg.
RUN RegitreraKonvreg.
/* Sletter procedyrebibloteket hvis det kjøres i batch modus. */
if terminal = "" then
  do:
    if valid-handle(wLibHandle) then
      delete procedure wLibHandle no-error.
  end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-RegitreraKonvreg) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE RegitreraKonvreg Procedure 
PROCEDURE RegitreraKonvreg :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE BUFFER bufKonv FOR KonvReg.
/* dFraArtikkelnr */
/* dTilArtikkelnr */
/* om artikeln som det flyttas från har fått artiklar flyttade till sig så skall de flyttas till nya */
FOR EACH KonvReg WHERE KonvReg.EDB-System = "RESTPAR" AND
                       Konvreg.Tabell     = ""        AND
                       KonvReg.InterntId   = STRING(dFraArtikkelnr):
         KonvReg.InterntId   = STRING(dTilArtikkelnr) NO-ERROR.
         IF ERROR-STATUS:ERROR THEN
             DELETE KonvReg.
END.
FIND FIRST KonvReg WHERE
     KonvReg.EDB-System = "RESTPAR" AND
     KonvReg.Tabell     = "" AND
     KonvReg.EkstId     = STRING(dFraArtikkelnr) AND
     KonvReg.InterntId  = STRING(dTilArtikkelnr) NO-LOCK NO-ERROR.
IF NOT AVAILABLE KonvReg THEN DO:
    CREATE bufKonv.
    ASSIGN bufKonv.EDB-System = "RESTPAR"
           bufKonv.Tabell     = ""
           bufKonv.InterntId  = STRING(dTilArtikkelnr)
           bufKonv.EkstId     = STRING(dFraArtikkelnr) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN
        DELETE bufKonv.
END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SkapTransLogg) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SkapTransLogg Procedure 
PROCEDURE SkapTransLogg :
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
  def var wBatchNr         as int  no-undo.
  def var wTransNr         as int  no-undo.
  def var wSeqNr           as int  no-undo.

def buffer bArtBas for ArtBas.

/* Fra Artikkel */
find ArtBas no-lock where
  recid(ArtBas) = wRecid1.
/* Til Artikkel */
find bArtBas no-lock where
  recid(bArtBas) = wRecid2.
/* Oppretter batchlogg og setter inn kommentar */
run batchlogg.w (program-name(1), 
                 "Overfør restpar: " + 
                 string(ArtBas.Vg) + 
                 "/" +
                 string(ArtBas.LopNr) + " -> " +
                 string(bArtBas.Vg) + 
                 "/" +
                 string(bArtBas.LopNr),
                 output wBatchNr).

FLYTT_BLOKK:
do TRANSACTION:

find current ArtBas exclusive-lock.
FIND CURRENT bArtBas EXCLUSIVE-LOCK.

do with frame DEFAULT-FRAME:
  /* Startet info */
  assign
    wTransNr  = 0
    wStartTid = time.
  if valid-handle(wProgram-Handle) then
    run StartInfo in wProgram-Handle (input today, input wStartTid).

  LAGER_LOOP:  
  for each Lager of ArtBas no-lock: 
  
    /* Butikk og ArtPris skal finnes! */
    find Butiker of Lager no-lock.
    find ArtPris no-lock where
      ArtPris.ArtikkelNr = bArtBas.ArtikkelNr and
      ArtPris.ProfilNr   = Butiker.ProfilNr.

    ARTLAG_LOOP:
    for each ArtLag no-lock where
      ArtLag.artikkelnr = ArtBas.artikkelnr and
      ArtLag.Butik = Lager.butik and
      ArtLag.LagAnt > 0
      break
      by ArtLag.Butik
      by ArtLag.artikkelnr:
      
      /* Bruker avbryter. */
      PROCESS EVENTS.
      if wStop then
        do:
          message "Skal klagjøringsrutinen stoppes? " view-as alert-box buttons YES-NO 
                   set wStop.
          if wStop = true then
            run AvbrytOppdatering.
        end.

      /* info om transaksjon */
      assign
        wOppdatertAntall = wOppdatertAntall + 1.        
      if valid-handle(wProgram-Handle) then
        run TransInfo in wProgram-Handle
                      (input "Artikkel: " +
                             string(ArtBas.Vg) + "/" +
                             string(ArtBas.LopNr) + " " + 
                             string(ArtBas.Beskr), 
                       input "Lagerposter " + 
                             string(wOppdatertAntall)
                      ).

      /* Vi tuter og kjører med + og - verdier. */
      if ArtLag.LagAnt <> 0 then
      OPPRETTRANS:
      do on error undo FLYTT_BLOKK, leave FLYTT_BLOKK :
        if wTransNr = 0 then
          do:
            find last TransLogg no-lock where
              TransLogg.Butik = ArtLag.Butik use-index TransLogg no-error.
            if available TransLogg then
              wTransNr = TransLogg.TransNr + 1.
            else 
              wTransNr = 1.
          end.
              
        /* Sjekker at transnr er ledig */
        if can-find(first TransLogg where
                    TransLogg.Butik = ArtLag.Butik and
                    TransLogg.TransNr = wTransNr) then
          NESTE_NR:
          do while true:
            wTransNr = wTransNr + 1.
            if can-find(first TransLogg where
                        TransLogg.Butik   = ArtLag.Butik and
                        TransLogg.TransNr = wTransNr) then
              next NESTE_NR.
            else
              leave NESTE_NR.
          end. /* NESTE_NR */
        
        /* FRA Artikkel */      
        create TransLogg.
        assign TransLogg.Butik        = ArtLag.Butik
               TransLogg.TransNr      = wTransNr
               TransLogg.SeqNr        = 1.
        assign TransLogg.BatchNr      = wBatchNr
               TransLogg.KundNr       = 0
               TransLogg.TTId         = 5 /* Varekjøp */
               TransLogg.TBId         = 1
               TransLogg.ArtikkelNr   = ArtBas.ArtikkelNr
               TransLogg.LevNr        = ArtBas.LevNr
               TransLogg.BongId       = 0
               TransLogg.BongLinjeNr  = 0
               TransLogg.KassaNr      = 0
               TransLogg.Vg           = ArtBas.Vg
               TransLogg.LopNr        = ArtBas.LopNr
               TransLogg.Antall       = ArtLag.LagAnt * -1 /* Trekkes ned på fra artikkel */
               TransLogg.Pris         = Lager.VVareKost               
               TransLogg.RabKr        = 0
               TransLogg.Mva          = 0
               TransLogg.Plukket      = true /* Skal ikke ut på plukkliste */
               TransLogg.Dato         = today
               TransLogg.Tid          = time
               TransLogg.BestNr       = 0
               TransLogg.Postert      = false                               
               wTransNr               = wTransNr + 1.
        assign  
               TransLogg.Storl        = FiksStorl(ArtLag.Storl).
               
        /* Sjekker at transnr er ledig */
        if can-find(first TransLogg where
                    TransLogg.Butik   = ArtLag.Butik and
                    TransLogg.TransNr = wTransNr) then
          NESTE_NR:
          do while true:
            wTransNr = wTransNr + 1.
            if can-find(first TransLogg where
                        TransLogg.Butik   = ArtLag.Butik and
                        TransLogg.TransNr = wTransNr) then
              next NESTE_NR.
            else
              leave NESTE_NR.
          end. /* NESTE_NR */

        /* TIL Artikkel */ 
        create TransLogg.
        assign TransLogg.Butik        = ArtLag.Butik
               TransLogg.TransNr      = wTransNr
               TransLogg.SeqNr        = 1.
        assign TransLogg.BatchNr      = wBatchNr
               TransLogg.KundNr       = 0
               TransLogg.TTId         = 5 /* Varekjøp */
               TransLogg.TBId         = 1
               TransLogg.ArtikkelNr   = bArtBas.ArtikkelNr
               TransLogg.LevNr        = bArtBas.LevNr
               TransLogg.BongId       = 0
               TransLogg.BongLinjeNr  = 0
               TransLogg.KassaNr      = 0
               TransLogg.Vg           = bArtBas.Vg
               TransLogg.LopNr        = bArtBas.LopNr
               TransLogg.Antall       = ArtLag.LagAnt 
               TransLogg.Pris         = Lager.VVareKost
               TransLogg.RabKr        = 0
               TransLogg.Mva          = 0
               TransLogg.Plukket      = true /* Skal ikke ut på plukkliste */
               TransLogg.Dato         = today
               TransLogg.Tid          = time
               TransLogg.BestNr       = 0
               TransLogg.Postert      = false                               
               wTransNr               = wTransNr + 1.
        assign  
               TransLogg.Storl        = FiksStorl(ArtLag.Storl).
        
        /* Det legges kun ut etiketter på positivt antall */
        if ArtLag.LagAnt > 0 AND bArtBas.Etikett > 0 then
        do:
          create EtikettLogg.
          assign
            wSeqNr                = wSeqNr + 1
            EtikettLogg.Butik     = wSeqNr /* Det skal skrives ut i seqnr ordning. */
            EtikettLogg.Vg        = bArtBas.Vg
            EtikettLogg.LopNr     = bArtBas.LopNr
            EtikettLogg.Ant       = IF bArtBas.Etikett = 1 
                                         THEN ArtLag.LagAnt ELSE 1
            EtikettLogg.Storl     = TransLogg.Storl
            EtikettLogg.Bongtekst = if available ArtBas 
                                      then ArtBas.Bongtekst
                                      else ""
            EtikettLogg.Pris      = (if ArtPris.Tilbud 
                                       then ArtPris.Pris[2]
                                       else ArtPris.Pris[1])
            EtikettLogg.SeqNr     = wSeqNr.
        end.

        release TransLogg.
      end. /* OPPRETTRANS */
    
      /* Antall poster som skal klagjøres. */
      assign
        wTotAntall = wTotAntall + 1.
    
    end. /* ARTLAG_LOOP */
  end. /* LAGER_LOOP */

  /* Brukt info */
  assign
    wFerdigTid = time
    wBruktTid  = wFerdigTid - wStartTid.
  if valid-handle(wProgram-Handle) then
    run BruktInfo in wProgram-Handle (input today, input wFerdigTid, input wBruktTid).
  
end. /* FRAME */  


/* Stempler FRA artikkel som ikke aktiv. */
assign
  ArtBas.Utgatt          = true
  ArtBas.UtgattDato      = today
  ArtBas.Aktivert        = false
  ArtBas.AktivDato       = today
  ArtBas.AktivAv         = userid("dictdb")
  ArtBas.Slasket         = FALSE
  ArtBas.SlaskArtikkelNr = ArtBas.ArtikkelNr
  ArtBas.Notat     = ArtBas.Notat + 
                     (IF ArtBas.Notat <> ""
                        THEN CHR(10)
                        ELSE "") + 
                     "Overført restpar til : " + 
                     string(bArtBas.Vg) + 
                     "/" +
                     string(bArtBas.LopNr) + "   Batch:" +
                     string(wBatchNr) + "."

  bArtBas.Notat    = bArtBas.Notat + 
                      (IF bArtBas.Notat <> ""
                         THEN chr(10)
                         ELSE "") + 
                      "Mottat restpar fra : " + 
                      string(ArtBas.Vg) + 
                      "/" +
                      string(ArtBas.LopNr) + "   Batch:" +
                      string(wBatchNr) + "."
  .
    /* Används för att skapa riktiga KonvReg */
    dFraArtikkelnr = Artbas.artikkelnr.
    dTilArtikkelnr = bArtbas.artikkelnr.

end. /* FLYTT_BLOKK TRANSACTION */

/* Oppdaterer status på BAtchLogg. */
run batchstatus.p (wBatchNr, 2).

if available ArtBas then
  release ArtBAs.
/* Hær måste vi skapa nya strekkoder på TIL artikkelen */
IF CAN-FIND(FIRST EtikettLogg OF bArtBas) THEN
    RUN genStrekKode.p (bArtBas.ArtikkelNr,wBatchNr,"TRANSLOGG").
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

/* ************************  Function Implementations ***************** */

&IF DEFINED(EXCLUDE-FiksStorl) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION FiksStorl Procedure 
FUNCTION FiksStorl RETURNS CHARACTER
  ( input wStorl as char ) :
/*------------------------------------------------------------------------------
  Purpose:  Formaterer størrelsen korrekt etter SkoTex standard.
    Notes:  
------------------------------------------------------------------------------*/

 assign
    wStorl = trim(wStorl)
    wStorl = caps(wStorl)
    wStorl = if (length(wStorl) = 1 or 
                 length(wStorl) = 3
                 ) 
                then " " + wStorl
                else wStorl.          

  /* Bytter ut eventuelle comma med punkt. */
  if index(wStorl,",") <> 0 then
    OVERLAY(wStorl, index(wStorl,","), 1, "CHARACTER") = ".".

  RETURN wStorl.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

