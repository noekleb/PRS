&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*
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

def var wBildeKatFlexi   as char no-undo.
def var wBildeKatSkoTex  as char no-undo.
def var wBildefil        as char no-undo.
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
def var wOrdreRecid      as recid no-undo.
def var wLoop            as int  no-undo.
def var wFeilKode        as int  no-undo.
def var wFeilTekst       as char no-undo.
def var wOldFBestNr      as char no-undo.    
def var wNeste           as log  no-undo.
def var wAntLinjer       as int  no-undo.
def var wArtBasRecid     as recid no-undo.
def var wCl              as int  no-undo.
def var wOrdreNr         as int  no-undo.
def var wEDB2-System      as char no-undo.
def var w2Tabell          as char no-undo.
  
/* Importvariabler ordre */
def var wLinje           as char no-undo.
def var wFPostTyp        as char no-undo.
def var wFBestNr         as char no-undo. /* FlexiCon bestillingsnummer */
def var wFBDato          as date no-undo.
def var wBestNr          as int  no-undo. /* SkoTex bestillingsnummer   */
def var wFLevNr          as char no-undo. /* FlexiCon leverandørnummer  */
def var wLevNr           as int  no-undo. /* SkoTex   leverandørnummer  */
def var wFLevAdrKode     as char no-undo.  
def var wPosterlinjer    as log  no-undo.
def var wEDB-System      as char no-undo.
def var wTabell          as char no-undo.

/* importvariabler - Bestilling + artikkel */
def var wFDokumentNr     as char no-undo.  
def var wFUnderNr        as char no-undo.
def var wFSasong         as char no-undo.
def var wFLevTid         as char no-undo.
def var wFArtGrp         as char no-undo.
def var wFKateg          as char no-undo.
def var wFLevArtKod      as char no-undo.
def var wEgenArtKod      as char no-undo.
def var wFEFarg          as char no-undo.
def var wFLevFarg        as char no-undo.
def var wFMaterial       as char no-undo.
def var wFVarum          as char no-undo.
def var wFInnPrisValuta  as char no-undo.
def var wFValKod         as char no-undo.
def var wFValKurs        as char no-undo.
def var wFRab%           as char no-undo.
def var wFFrakt          as char no-undo.
def var wFTull%          as char no-undo.
def var wFNettoInnpris   as char no-undo.
def var wFPris           as char no-undo.
def var wFAnnons         as char no-undo.
def var wFPostFlagg      as char no-undo.
def var wFFriTekst       as char no-undo.

/* Importvariabler for linjene */
def var wFSalgsSted      as char no-undo.
def var wFLevSort        as char no-undo.
def var wFStrId          as char no-undo.
def var wFAntSort        as char no-undo.
def var wFAntPar         as int  extent 30 no-undo.
def var wFStorl          as char extent 30 no-undo.

/* Definerer stream */
def stream InnData.
def stream LoggData.

def temp-table BestListe 
  field tmpBestNr as int.

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

/* Henter centrallager. */
{syspara.i 5 1 1 wCl INT}

/* Bildekatalog Flexicon */
{syspara.i 50 10 50 wBildeKatFlexi}

/* Kode for konvertering av artikkelnummer ved import. */
{syspara.i 1 2 1 wEDB-System}
if wEDB-System = "" then
  wEDB-System = "FLEXICON".
{syspar2.i 1 2 1 wTabell}
if wEDB-System = "" then
  wEDB-System = "ArtBas".
  
/* Henter parametre for konvertering av leverandør. */
{syspara.i 1 2 1000 wEDB2-System}
{syspar2.i 1 2 1000 w2Tabell}
w2Tabell = entry(1,w2Tabell). /* FlexiCon ligger som første entry i listen. */  

/* Klargjørings dato og tid. */
assign
  wDato = today
  wTid  = time.

/* Klargjør priskøen. */
run ImporterBestillinger.

/* Sletter procedyrebibloteket hvis det kjøres i batch modus. */
if terminal = "" then
  do:
    if valid-handle(wLibHandle) then
      delete procedure wLibHandle no-error.
  end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-BestillingsHode) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE BestillingsHode Procedure 
PROCEDURE BestillingsHode :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  /* Pakker ut linjen */
  assign
    wFDokumentNr      = trim(substring(wLinje,11,6))
    wFUnderNr         = trim(substring(wLinje,17,2))
    wFSasong          = trim(substring(wLinje,19,3))
    wFLevTid          = trim(substring(wLinje,22,3))
    wFArtGrp          = trim(substring(wLinje,25,3))
    wFKateg           = trim(substring(wLinje,28,1)) 
    wFLevArtKod       = trim(substring(wLinje,29,15))
    wEgenArtKod       = trim(substring(wLinje,44,18))
    wFEFarg           = trim(substring(wLinje,62,2))
    wFLevFarg         = trim(substring(wLinje,64,15))
    wFMaterial        = trim(substring(wLinje,79,25))
    wFVarum           = trim(substring(wLinje,104,25))
    wFInnPrisValuta   = trim(substring(wLinje,129,10))
    wFValKod          = trim(substring(wLinje,139,3))
    wFValKurs         = trim(substring(wLinje,142,10))
    wFRab%            = trim(substring(wLinje,152,6))
    wFFrakt           = trim(substring(wLinje,158,6))
    wFTull%           = trim(substring(wLinje,164,5))
    wFNettoInnpris    = trim(substring(wLinje,169,10))
    wFPris            = trim(substring(wLinje,179,10))
    wFAnnons          = trim(substring(wLinje,189,1))
    wFPostFlagg       = trim(substring(wLinje,190,1))
    wFFriTekst        = trim(substring(wLinje,191,200)).   

  BESTHODE-ARTIKKEL:
  do TRANSACTION:

    run OpprettArtBas. /* Her oppretter BestHode og artikkel */
    if return-value = "AVBRYT" then
      return "AVBRYT".
    find ArtBas no-lock where
      recid(ArtBas) = wArtBasRecid no-error.        

  end. /* BESTHODE-ARTIKKEL */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-BestillingsLinjer) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE BestillingsLinjer Procedure 
PROCEDURE BestillingsLinjer :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

def output parameter wReturStatus as char no-undo.

def var wLoop          as int  no-undo.
def var wStrIntervall  as char no-undo.
def var wStrlIntv      as char no-undo.
def var wStrTmp        as char no-undo.
def var wFordeling     as char no-undo.
def var wAntal         as int  no-undo.
def var wStrTypeId     as int  no-undo.
def var wBestIntervall as char no-undo.
def var wFri           as log  no-undo.
def var wTmpVar        as char no-undo.

assign
  wReturStatus = "OK".

/* Gjør leverandøren tilgjengelig. */
find LevBas no-lock where
  LevBas.LevNr = int(wFLevNr) no-error.  
if not available LevBAs then
  return.
  
BESTLINJE:
do TRANSACTION:  
  assign
    wFSalgsSted    = trim(substring(wLinje,2,3))
    wFLevSort      = trim(substring(wLinje,14,4))
    wFStrId        = trim(substring(wLinje,18,2))
    wFAntSort      = trim(substring(wLinje,20,2))
    wFAntPar       = 0
    wFStorl        = ""
    wBestIntervall = ""
    wFri           = false.

  /* Leser inn størrelser m/m i variablene */
  do wLoop = 1 to 30:
    assign
      wFAntPar[wLoop] = int(substring(wLinje,22 + (wLoop * 7),2))
      wFStorl[wLoop]  = trim(substring(wLinje,17 + (wLoop * 7),4)).
    assign
      wBestIntervall  = wBestIntervall + 
                        (if wBestIntervall = ""
                           then ""
                           else " ") +
                        string(wFAntPar[wLoop]).      
  end. 

  /* Sjekker at størrelsesdefinisjonen finnes */
  find first StrType no-lock where
    StrType.KortNavn = wFStrId no-error.
  if not available StrType then
    OPPRETT-STRTYPEID:
    do:
      wFeilKode     = 19.
      run FeilKode (wFeilKode, " StrTypeId: " + string(wFStrId) + " (Opprettet)").

      find last StrType no-lock use-index StrType no-error.
      if available StrType
        then wStrTypeId = StrType.StrTypeId + 1.
        else wStrTypeId = 1.
      create StrType.
      assign
        StrType.StrTypeId = wStrTypeId
        StrType.KortNavn  = wFStrId
        StrType.Beskr     = wFStrId.      
        
      /* Oppretter størrelsene */
      LOOPEN:
      do wLoop = 1 to 30:
        if length(trim(wFStorl[wLoop])) = 0 then 
          next LOOPEN. /* Blanke størrelser legger vi ikke opp. */
        find StrTStr exclusive-lock where
          StrTStr.StrTypeId = StrType.StrTypeId and
          StrTStr.SeqNr     = wLoop no-error.
        if not available StrTStr then
          do:
            create STrTStr.
            assign
              StrTStr.StrTypeId = StrType.StrTypeId 
              StrTStr.SeqNr     = wLoop.
          end.
        assign
          StrTStr.SoStorl = wFStorl[wLoop].

        /* Bytter ut eventuelle comma med punkt. */
        if index(StrTStr.SoStorl,",") <> 0 then
          OVERLAY(StrTStr.SoStorl, index(StrTStr.SoStorl,","), 1, "CHARACTER") = ".".
          
        /* Fikser størrelsesplassering med space. */
        assign
          StrTStr.SoStorl = trim(StrTStr.SoStorl)
          StrTStr.SoStorl = if (length(StrTStr.SoStorl) = 1 or 
                               length(StrTStr.SoStorl) = 3) 
                              then " " + StrTStr.SoStorl
                              else StrTStr.SoStorl.                   
      end. /* LOOPEN */
    end. /* OPPRETT-STRTYPEID */
  assign
    wStrTypeId = StrType.StrTypeId.

  /* Henter leverandørsortimentet */
  find LevSort no-lock where
    LevSort.LevNr     = LevBas.LevNr and
    LevSort.SortId    = (if wFLevSort = "FRI" 
                           then wFLevSort + "-" + wFStrId
                           else wFLevSort) and
    LevSort.StrTypeId = wStrTypeId no-error.
  if not available LevSort then
    do:
      wFeilKode     = 18.
      run FeilKode (wFeilKode, " LevNr/LevSort/StrType: " + 
                                string(LevBas.LevNr) + "/" + 
                                (if wFLevSort = "FRI" 
                                  then wFLevSort + "-" + wFStrId
                                  else wFLevSort) + "/" + 
                                wFStrId + " (" +
                                string(wStrTypeId) + ")").
    end.
  else 
    assign
      wFri = LevSort.Fri.

  /* Leser Størrelsestypen og definisjonen. */
  for each StrTstr no-lock where
    StrTStr.StrTypeId = wStrTypeId
    by StrTStr.SeqNr:
    assign
      wStrIntervall = wStrIntervall +
                      (if wStrIntervall = ""
                         then ""
                         else " ") +
                      trim(StrTStr.SoStorl).
  end.

  /* Legger opp default sortlinjer hvis det ikke er gjort.        */
  /* Det legges opp en linje pr. definert leverandøsortiment med  */
  /* samme størrelsestype.                                        */
  /* Legger også opp størrelsesintervallet på hver bestsortlinje. */
  /* NB: Dette gjøres en gang pr. bestilling.                     */
  find first BestSort no-lock where
    BestSort.BestNr = wBestNr no-error.
  if not available BestSort then
    DEFAULT-BEST-SORT:
    do:

      /* Legger opp de faste sortimentene.              */
      /* Leser alle levsort som ikke har fri inndeling. */
      LEV-SORT:
      FOR EACH LevSort OF LevBas no-lock WHERE 
        LevSort.StrType = wStrTypeId AND
        LevSort.Fri     = false:
    
        ASSIGN 
          wAntal     = 0
          wFordeling = ""
          wStrlIntv  = ""
          wStrTmp    = "".
          
        /* Bygger fordelingsliste på størrelser og antall. */
        FOR EACH LevsAnt OF LevSort NO-LOCK:
          ASSIGN 
            wAntal     = wAntal + LevsAnt.SoAnt
            wFordeling = IF wFordeling = "" 
                           THEN String(LevSant.SoAnt) + " "
                           ELSE wFordeling + String(LevSant.SoAnt) + " "
            wStrTmp = IF wStrTmp = "" 
                        THEN left-trim(LevSant.SoStorl)
                        ELSE wStrTmp + " " + left-trim(LevSant.SoStorl).
        END.
        IF wStrTmp = "" THEN 
          NEXT LEV-SORT.
          
        /* Føste og siste størrelse i intervallet. */
        ASSIGN 
          wStrlIntv = ENTRY(1,wStrTmp," ") + " - " +
                      ENTRY(NUM-ENTRIES(wStrTmp," "),wStrTmp," ").

        /* Oppretter linjene med default verdier. */
        create BestSort.
        assign
          BestSort.BestNr = wBestNr
          BestSort.SortId = LevSort.SortId.
        assign  
          BestSort.Fri         = LevSort.Fri
          BestSort.AntSort     = 0
          BestSort.Antall      = wAntal
          BestSort.Strinterval = wStrlIntv
          BestSort.Fordeling   = wFordeling
          BestSort.Storrelser  = wStrTmp.
      END. /* LEV-SORT */
            
      /* Legger opp det første fri sortimentet for størrelsestypen */.
      LEV-FRI-SORT:
      do:
        ASSIGN 
          wFordeling = ""
          wStrlIntv  = ""
          wStrTmp    = "".
          
        /* Bygger fordelingsliste på størrelser og antall. */
        do wLoop = 1 to 30:
          if wFAntPar[wLoop] = 0 then
            next. /* Tar ikke med blanke */
             
          ASSIGN 
            wTmpVar    = wFStorl[wLoop].

          /* Bytter ut eventuelle comma med punkt. */
          if index(wTmpVar,",") <> 0 then
            OVERLAY(wTmpVar, index(wTmpVar,","), 1, "CHARACTER") = ".".
              
          assign
            wFordeling = IF wFordeling = "" 
                           THEN String(wFAntPar[wLoop]) + " "
                           ELSE wFordeling + String(wFAntPar[wLoop]) + " "
            wStrTmp = IF wStrTmp = "" 
                        THEN left-trim(wTmpVar)
                        ELSE wStrTmp + " " + left-trim(wTmpVar).
        end.
        IF wStrTmp = "" THEN 
          leave LEV-FRI-SORT.
        
        /* Føste og siste størrelse i intervallet. */
        ASSIGN 
          wStrlIntv = ENTRY(1,wStrTmp," ") + " - " +
                      ENTRY(NUM-ENTRIES(wStrTmp," "),wStrTmp," ").
         /* Oppretter linjene med default verdier. */
        create BestSort.
        assign
          BestSort.BestNr = wBestNr
          BestSort.SortId = wFLevSort + "-" + wFStrId.
        assign  
          BestSort.Fri         = true
          BestSort.AntSort     = 0
          BestSort.Antall      = 0
          BestSort.Strinterval = ""
          BestSort.Fordeling   = wFordeling
          BestSort.Storrelser  = wStrTmp.      
      end. /* LEV-FRI-SORT */
    end. /* DEFAULT-BEST-SORT */
    
  /* Oppdaterer BesSort med hva vi har funnet. */
  find first BestSort exclusive-lock where
    BestSort.BestNr = wBestNr and
    BestSort.SortId = (if wFLevSort = "FRI" 
                         then wFLevSort + "-" + wFStrId
                         else wFLevSort) no-error.
  /* Her mangler default sort som er opprettet ovenfor. */
  if not available BestSort then
    do:
      wFeilKode     = 20.
      run FeilKode (wFeilKode, " Bestilling: " + 
                               string(wBestNr) + " LevSort: " + 
                               (if wFLevSort = "FRI" 
                                  then wFLevSort + "-" + wFStrId
                                  else wFLevSort)).
      wReturStatus = "AVBRYT".
      leave BESTLINJE.
    end.
  /* Oppdaterer bestSort med antall sortiment bestilt. */
  assign
    BestSort.AntSort   = int(wFAntSort)
    BestSort.Fordeling = if wFri then wBestIntervall else BestSort.Fordeling.         
  
  /* Oppdaterer ArtBas med StørrelsesTypen. */
  find ArtBas no-lock where
    recid(ArtBas) = wArtBasRecid no-error.
  if ArtBas.StrTypeId = 0 then
    do:
      find current ArtBas exclusive-lock no-error.
      assign 
        ArtBas.StrTypeId = wStrTypeId.
    end.
  /* Sjekker at artbas har samme størrelsestype som artikkelen. */
  if ArtBas.StrTypeId <> wStrTypeId then
    do:
      wFeilKode     = 21.
      run FeilKode (wFeilKode, " StrTypeId på bestilling/Artikkel: " + 
                                 string(wStrTypeId) + "/" + 
                                 string(ArtBas.StrTypeId)).
      /* --Dette er tillatt
      wReturStatus = "AVBRYT".
      leave BESTLINJE.
      */
    end.
    
  /* Legger opp default bestillingslinjer. */
  find first BestLinje no-lock where
    BestLinje.BestNr = wBestNr no-error.
  if not available BestLinje then
    do:
      for each Butiker no-lock where
        Butiker.Butik > 0:
        create BestLinje.
        assign
          BestLinje.BestNr = wBestNr
          BestLinje.Butik  = Butiker.Butik.        
      end.          
    end.

   /* Legger opp størrelsene som er bestillt. */
   STORRELSENE:
   do wLoop = 1 to 30:
     if wFAntPar[wLoop] = 0 then 
       next STORRELSENE.
     
     /* Fikser på størrelsene */
     /* NB: Dette kan ikke gjøres før ordreprogrammene er korrigert!
     wFStorl[wLoop]  = FiksStorl(wFStorl[wLoop]).
     */
       
     find BestSTr exclusive-lock where
       BestStr.BestNr   = wBestNr and
       BestStr.Butik    = int(wFSalgsSted) and
       BestStr.Storl    = wFStorl[wLoop] and
       BestStr.BestStat = BestHode.BestStat no-error.
     if not available BestStr then
       do:
         create BestStr.
         assign
           BestStr.BestNr   = wBestNr 
           BestStr.Butik    = int(wFSalgsSted) 
           BestStr.Storl    = wFStorl[wLoop] 
           BestStr.BestStat = BestHode.BestStat.

       end.       
     assign
       BestStr.Bestilt    = wFAntPar[wLoop]  
       BestHode.TotAntPar = BestHode.TotantPar + wFAntPar[wLoop]
       BestHode.StrTypeId = wStrTypeId. /* Fra FlexiCon. */
   end. /* STORRELSENE */

   /* Henter prisene (For sentrallager */
   find Butiker no-lock where
     Butiker.Butik = wCl no-error.
   if available Butiker then
     find BestPris no-lock where
       BestPris.BestNr   = BestHode.BestNr and
       BestPris.ProfilNr = Butiker.ProfilNr and
       BestPris.BestStat = BestHode.BestStat no-error.   

   /* Oppdaterer totalene. */
   if available BestPris then
     ASSIGN 
       BestHode.TotInnKjVerdi = BestHode.TotAntPar * BestPris.Varekost
       BestHode.TotDbKr       = BestHode.TotAntPar * BestPris.DbKr
       BestHode.TotSalgsVerdi = BestHode.TotAntPar * BestPris.Pris.

end. /* BESTLINJE - TRANSACTION */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-BestStatus) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE BestStatus Procedure 
PROCEDURE BestStatus :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  def input parameter ipBestNr as int no-undo.
  
  def var wBestHodeRecid as recid no-undo.

  find BestHode no-lock where 
    BestHode.BestNr = ipBestNr no-error.
  if available BestHode then
    do transaction:
      /* Setter riktig status. */
      assign wBestHodeRecid = recid(BestHode).
      if BestHode.BestStat = 1 then
        RUN bytbeststatus.p (wBestHodeRecid,"+3",?).
      else if BestHode.BestStat = 2 then
        RUN bytbeststatus.p (wBestHodeRecid,"+2",?).
      else if BestHode.BestStat = 3 then
        RUN bytbeststatus.p (wBestHodeRecid,"+1",?).
      wFeilKode = 23.
      run FeilKode (wFeilKode, "* LevNr/BestNr/OrdreNr/ArtikkelNr/: " + 
                               string(BestHode.LevNr) + "/" + 
                               string(BestHode.BestNr) + "/" +
                               string(BestHOde.ORdreNr) + "/" +
                               string(BestHode.ArtikkelNr)).      
      
      /* Setter riktig fordeling på det fri sortimentet. */
      run SumFriSort (ipBestNr).
      
      release BestHode.
      
    end.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ByggFilNavn) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ByggFilNavn Procedure 
PROCEDURE ByggFilNavn :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/* Filnavn */
if available SysPara then release SysPara.
{syspara.i 50 10 2 wImpFil}
if wImpFil = "" then
  assign
    wImpFil = "excbest.exp".

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

&ENDIF

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

&ENDIF

&IF DEFINED(EXCLUDE-FeilKode) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE FeilKode Procedure 
PROCEDURE FeilKode :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  def input parameter wFeilKode as int.
  def input parameter wTekst    as char.

  def var wStdTekst             as char.  
  def var wFeilTekst as char no-undo.

  if substring(wTekst,1,1) = "*" then
    assign
      wStdTekst = " Linje: "
      wTekst    = substring(wTekst,2).
  else
    assign
      wStdTekst = " *Feil på linje*: "
      wTekst    = wTekst.
  
  OUTPUT STREAM LoggData to value(wLogFil) no-echo append.
  {syspara.i 50 11 wFeilKode wFeilTekst}
  put stream LoggData unformatted  wStdTekst string(wTotAntall,"999999") + " " + wFeilTekst + wTekst skip.
  OUTPUT STREAM LoggData close.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ImporterBestillinger) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ImporterBestillinger Procedure 
PROCEDURE ImporterBestillinger :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

def var wReturStatus as char no-undo.
def var wUndoOrdre   as log  no-undo.
def var wOldBestNr   as int  no-undo.

run ByggFilNavn.
run ByggLoggFilNavn.

assign 
  wAntBest       = 0
  wNeste         = false
  wPosterlinjer  = true
  wOkStatus      = "AVBRYT".

/* Åpner stream til datafil */
INPUT STREAM InnData from value(wImpFil) no-echo.

/* Åpner stream til datafil */
OUTPUT STREAM LoggData to value(wLogFil) no-echo append.
/* Åpner ningsmelding i loggfil */
put stream LoggData unformatted 
  " " skip
  " -----------------------------------------------------------------------" skip
  " Import av ordre fra eXcellenc PRO Innkjøpssystem startet         " skip
  " " string(today) " " string(time,"HH:MM:SS") " " userid("dictdb") skip
  " -----------------------------------------------------------------------" skip(1).
OUTPUT STREAM LoggData close.

/* Startet info */
assign
  wStartTid = time
  wBestNr   = ?.
if valid-handle(wProgram-Handle) then
  run StartInfo in wProgram-Handle (input today, input wStartTid).

/* Hovedblokk - repeterende */
MAIN-BLOCK:
do while true:

/* Det styres me en transaksjon rund hver bestilling. */
/* Etter hver bestilling gjøres det en commit, før    */
/* neste bestilling behandles.                        */
TRANSBLOKK:
do with frame DEFAULT-FRAME
  on error undo, retry:

  IMPORT_AV_DATA:  
  repeat TRANSACTION:
    assign wFeilKode = 0.
    
    /* Leser en og en linje fra filen */
    if wNeste = false then
      import stream InnData unformatted wLinje.
    else
      wNeste = false.

    /* Antall poster importert.                          */
    /* Linjer som Committes telles to ganger! (Bieffekt) */
    assign
      wAntLinjer = wAntLinjer + 1
      wTotAntall = wTotAntall + 1.

    /* Ny bestilling. Pakker ut hode, oppretter bestilling og leverandør. */
    if substring(wLinje,1,1) = "1" then
      ORDRE-HODE:
      do:
        assign
          wPosterlinjer = true
          wUndoOrdre    = false.

        /* Håndterer Commit pr. bestilling. */
        if wAntLinjer = 1 then. /* Gjør ingenting. */
        else do:
          assign
            wAntLinjer = 0
            wNeste     = true. 
          leave TRANSBLOKK. /* Commit av transaksjonen rund bestillingen. */
        end.
      
        assign
          wAntBest      = wAntBest + 1
          wFPostTyp     = substring(wLinje,1,1)
          wFBestNr      = substring(wLinje,2,6)
          wFBDato       = date(
                              int(substring(wLinje,10,2)),
                              int(substring(wLinje,12,2)),
                              int(substring(wLinje,8,2)) +
                              (if int(substring(wLinje,8,2)) < 50
                                then 2000
                                else 1900)
                              )
          wFLevNr       = substring(wLinje,14,4) 
          wFLevAdrKode  = substring(wLinje,18,4).
           
        /* Sjekker leverandørnummer.                                    */
        /* Hvis det er feil på leverandørnummer, hopper vi over posten. */
        wOk = SjekkStreng(wFLevNr).
        if wOk = false then
          do:
             assign wFeilKode = 1.  
             run FeilKode (wFeilKode, " LevNr: " + wFLevNr).             
             next IMPORT_AV_DATA.
          end.
          
        /* Konverterer leverandørnummeret */
        find KonvReg no-lock where
          KonvReg.EDB-System = wEDB2-System and
          KonvReg.Tabell     = w2Tabell     and
          KonvReg.EkstId     = string(int(wFLevNr)) no-error.
        if available KonvReg then
          KONVERTER:
          do:
            assign
              wFLevNr = KonvReg.InterntId.
          
            OUTPUT STREAM LoggData to value(wLogFil) no-echo append.
            put stream LoggData unformatted 
              "Konverterer " KonvReg.EkstId 
              " til " KonvReg.InterntId skip. 
            OUTPUT STREAM LoggData close.
          
            release KonvReg.
          end. /* KONVERTER */          
          
        /* Finnes leverandøren? */
        find LevBas no-lock where
          LevBas.LevNr = int(wFLevNr) no-error.
        if not available LevBas then
          do:
             assign wFeilKode = 2.  
             run FeilKode (wFeilKode, " Ekst. BestNr/LevNr: " + 
                                      wFBestNr + "/" + wFLevNr ).
             next IMPORT_AV_DATA.
          end.
          
        /* Oppretter og gjør ordren tilgjengelig */
        RUN OpprettOrdre.      
        if return-value = "AVBRYT" then
          next IMPORT_AV_DATA.          
        find Ordre no-lock where
          recid(Ordre) = wOrdreRecid no-error.
        if not available Ordre then
          next IMPORT_AV_DATA.

        if available Ordre then
          do:
            OUTPUT STREAM LoggData to value(wLogFil) no-echo append.
            put stream LoggData unformatted 
              " BestNr: " string(int(wFBestNr),"zz9999") 
              " (OrdreNr: " + string(Ordre.OrdreNr) + ")"
              " Dato: " wFBDato 
              " Leverandør: " wFLevNr 
              " oppdatert." skip.
            OUTPUT STREAM LoggData close.

            /* Info om Bestillingen */
            if valid-handle(wProgram-Handle) then
              run ProfilInfo in wProgram-Handle (input string(wFBestNr) + " BestDato: " +
                                            string(wFBDato) + " LevNr: " + 
                                            wFLevNr + " Adr: " +
                                            wFLevAdrKode).
          end.
      end. /* ORDRE-HODE */    

    /* info om transaksjon */
    assign
      wOppdatertAntall = wOppdatertAntall + 1.      

    /* Posterer Transkode 2 og 3.                             */
    /* Er ordren importert fra før, skal disse ikke posteres. */
    if wPosterlinjer and wFeilKode = 0 then
      POSTER-LINJER:
      do:
        /* Oppdaterer dokument, dvs bestilling. */
        /* Alltid opprette ny bestilling.       */
        if substring(wLinje,1,1) = "2" then
          DOKUMENT:
          do:
            RUN BestillingsHode.  
            if return-value = "AVBRYT" then
              undo TRANSBLOKK, leave TRANSBLOKK.

            if valid-handle(wProgram-Handle) then
            run TransInfo in wProgram-Handle
                    (input " ", 
                     input "Importert " + 
                           string(wOppdatertAntall)
                    ).
          end. /* DOKUMENT */

        /* Oppdaterer størelsene */
        if substring(wLinje,1,1) = "3" then
          STORLEKAR:
          do:
            RUN BestillingsLinjer (output wReturStatus).  
            if wReturStatus = "AVBRYT" then
              assign wUndoOrdre = true.

          end. /* STORLEKAR */
      end. /* POSTER-LINJER */
      
  end. /* IMPORT_AV_DATA */
  
  /* Her er vi ferdig på normal måte */
  assign
    wNeste = false.

  /* Brukt info */
  assign
    wOkStatus  = "OK"
    wFerdigTid = time
    wBruktTid  = wFerdigTid - wStartTid.
  if valid-handle(wProgram-Handle) then
    run BruktInfo in wProgram-Handle (input today, input wFerdigTid, input wBruktTid).
  
end. /* TRANSBLOKK TRANSACTION*/  

/* Slipper diverse record. */
if available Ordre then
  release Ordre.
if available BestHode then
release BestHode.
if available BestSort then
release BestSort.
if available BestPris then
release BestPris.
if available BestLinje then
release BestLinje.
if available BestStr then
release BestStr.
if available KonvReg then
  release KonvReg.

/* Er wNeste satt, skal vi opp og ta neste bestilling. */
if wNeste = true then
  next MAIN-BLOCK.
else 
  leave MAIN-BLOCK. /* Forhindrer evig loop */
end. /* MAIN-BLOCK */

/* Oppdaterer bestillingsstatus . */
if can-find(first BestListe) then
  do:
    for each BestListe:
      run BestStatus (BestListe.tmpBestNr).
    end.
  end.

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

/* viser resultat til bruker */
if valid-handle(wProgram-Handle) then
  do:
    if search(wLogFil) <> ? then
      os-command no-wait value("notepad.exe") value(wLogFil).
  end.

/*
  /* Tar bort gammele plu-filer. */
  if search(wPluFil) <> ? then 
    os-delete value(wPluFil).
*/    

/* Slipper diverse record. */
if available Ordre         then release Ordre.
if available BestHode      then release BestHode.
if available BestSort      then release BestSort.
if available BestPris      then release BestPris.
if available BestLinje     then release BestLinje.
if available BestStr       then release BestStr.
if available KonvReg       then release KonvReg.
if available BildeRegister then release BildeRegister.
if available StrTStr       then release StrTStr.
if available StrType       then release StrType.  

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Kalkulasjon) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Kalkulasjon Procedure 
PROCEDURE Kalkulasjon :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  def input parameter wNyArtikkel as char no-undo.

  DEF VAR h_PrisKo AS HANDLE.

  IF NOT VALID-HANDLE(h_PrisKo) THEN
      RUN prisko.p PERSISTENT SET h_PrisKo.

    /* Bygger kalkulasjonsstreng */
    assign
      wSkjerm = string(dec(wFInnPrisValuta) / 100) + ";" +
                " " + ";" +
                "0" + ";" +
                string(dec(wFRab%) / 100) + ";" +
                "0" + ";" +
                "0" + ";" +
                string(dec(wFFrakt) / 100) + ";" +
                "0" + ";" +
                string(dec(wFTull%) / 100) + ";" +
                "0" + ";" +
                "0" + ";" +
                "0" + ";" +
                string(dec(wFNettoInnpris) / 100) + ";" + /* Denne blir beregnet om igjen! */
                "0" + ";" +
                string(Moms.MomsProc) + ";" +
                "0" + ";" +
                "0" + ";" +
                string(dec(wFPris) / 100) + ";" +
                "0" + ";" +
                "False" + ";" +
                string(today) + ";" +
                string(today) + ";" +
                "0"  + ";" +
                "0"  + ";" +
                "false".        

  /* Pris og lagerinformasjon */
  BUTIKKLOOP:
  for each Butiker no-lock where
    Butiker.Butik > 0
    break by Butiker.ProfilNr:
    /* En prosrecord pr. profil. */
    if first-of(Butiker.ProfilNr) then
      KALKULASJON:
      do:              
        /* Henter prisprofilen */
        find PrisProfil no-lock where
          PrisProfil.ProfilNr = Butiker.ProfilNr no-error.
        if not available PrisProfil then
          do:  
            wFeilKode = 16.
            run FeilKode (wFeilKode, " (For butikk:" + string(Butiker.Butik) + ")").  
            leave KALKULASJON.
          end.         
          
        /* Starter omkalkulering.                         */
        /* NB: Kalkulasjonen skjer i prosedyrebilboteket. */
        
        /* Fra valutaprisfeltet. */
        if valid-handle(h_PrisKo) then
          run Omregning in h_PrisKo
               (input wArtBasRecid, 
                input PrisProfil.ProfilNr,
                input-output wSkjerm,
                input Moms.MomsProc,
                input Valuta.ValKurs, 
                input 1, 
                input false).
        /* Fikser kalkulasjonsstrengen */
        wSkjerm = wSkjerm + ";" + 
                  "False" + ";" +
                  string(today) + ";" +
                  string(today) + ";" +
                  "0"  + ";" +
                  "0"  + ";" +
                  "false".        

        /* Fra Rabatt-1% feltet. */
        if valid-handle(h_PrisKo) then
          run Omregning in h_PrisKo
               (input wArtBasRecid, 
                input PrisProfil.ProfilNr,
                input-output wSkjerm,
                input Moms.MomsProc,
                input Valuta.ValKurs, 
                input 4, 
                input false).
        /* Fikser kalkulasjonsstrengen */
        wSkjerm = wSkjerm + ";" + 
                  "False" + ";" +
                  string(today) + ";" +
                  string(today) + ";" +
                  "0"  + ";" +
                  "0"  + ";" +
                  "false".        

        /* Fra FraktKr feltet. */
        if valid-handle(h_PrisKo) then
          run Omregning in h_PrisKo
               (input wArtBasRecid, 
                input PrisProfil.ProfilNr,
                input-output wSkjerm,
                input Moms.MomsProc,
                input Valuta.ValKurs, 
                input 7, 
                input false).
        /* Fikser kalkulasjonsstrengen */
        wSkjerm = wSkjerm + ";" + 
                  "False" + ";" +
                  string(today) + ";" +
                  string(today) + ";" +
                  "0"  + ";" +
                  "0"  + ";" +
                  "false".        

        /* Fra DivKost% feltet. */
        if valid-handle(h_PrisKo) then
          run Omregning in h_PrisKo
               (input wArtBasRecid, 
                input PrisProfil.ProfilNr,
                input-output wSkjerm,
                input Moms.MomsProc,
                input Valuta.ValKurs, 
                input 10, 
                input false).
        /* Fikser kalkulasjonsstrengen */
        wSkjerm = wSkjerm + ";" + 
                  "False" + ";" +
                  string(today) + ";" +
                  string(today) + ";" +
                  "0"  + ";" +
                  "0"  + ";" +
                  "false".        

        /* Simulerer intasting av salspris. */
        assign
          wSkjerm = entry(1,wSkjerm,";") + ";" +
                    entry(2,wSkjerm,";") + ";" +
                    entry(3,wSkjerm,";") + ";" +
                    entry(4,wSkjerm,";") + ";" +
                    entry(5,wSkjerm,";") + ";" +
                    entry(6,wSkjerm,";") + ";" +
                    entry(7,wSkjerm,";") + ";" +
                    entry(8,wSkjerm,";") + ";" +
                    entry(9,wSkjerm,";") + ";" +
                    entry(10,wSkjerm,";") + ";" +
                    entry(11,wSkjerm,";") + ";" +
                    entry(12,wSkjerm,";") + ";" +
                    entry(13,wSkjerm,";") + ";" +
                    entry(14,wSkjerm,";") + ";" +
                    entry(15,wSkjerm,";") + ";" +
                    entry(16,wSkjerm,";") + ";" +
                    entry(17,wSkjerm,";") + ";" +
                    string(dec(wFPris) / 100) + ";" +
                    entry(19,wSkjerm,";") + ";" +
                    "False" + ";" +
                    string(today) + ";" +
                    string(today) + ";" +
                    "0"  + ";" +
                    "0"  + ";" +
                    "false".        

        /* Fra Pris feltet. */
        if valid-handle(h_PrisKo) then
          run Omregning in h_PrisKo
               (input wArtBasRecid, 
                input PrisProfil.ProfilNr,
                input-output wSkjerm,
                input Moms.MomsProc,
                input Valuta.ValKurs, 
                input 18, 
                input false).
        /* Fikser kalkulasjonsstrengen */
        wSkjerm = wSkjerm + ";" + 
                  "False" + ";" +
                  string(today) + ";" +
                  string(today) + ";" +
                  "0"  + ";" +
                  "0"  + ";" +
                  "false".        
                  
        /* Oppdatering av priser på artikkelen gjøres kun for ny artikkel */
        if wNyArtikkel = "Ja" then 
          do:
            /* Oppdaterer ordinærkalkyle */
            if valid-handle(h_PrisKo) then
              run LagreArtPris in h_PrisKo
                  (input recid(ArtBas),
                   input PrisProfil.ProfilNr,
                   input-output wSkjerm,
                   input false,  /* wTilbud = false - Dvs ordinær kalkyle.          */
                   input true,   /* Direkte oppdatering av prisene som er kalkulert */
                   input false). /* Ingen oppdatering av prisko.                    */
          end.
          
        /* Oppretter kalkyle på bestilling for sentrallager. */        
        /* NB: Her opprettes pr. prisprofil.                 */
        /* if Butiker.Butik = wCl then */
          CENTRALLAGER:
          do:          
            /* Henter posten */
            find BestPris exclusive-lock where
              BestPris.BestNr   = BestHode.BestNr and
              BestPris.BestStat = BestHode.BestStat no-error.
            if not available BestPris then
              do:
                create BestPris.
                assign
                  BestPris.BestNr   = BestHode.BestNr 
                  BestPris.BestStat = BestHode.BestStat
                  BestPris.ProfilNr = Butiker.ProfilNr.
              end.
    
            assign          
              BestPris.ValPris      = dec(entry(1,wSkjerm,";"))
              BestPris.InnkjopsPris = dec(entry(2,wSkjerm,";"))
              BestPris.Rab1Kr       = dec(entry(3,wSkjerm,";"))
              BestPris.Rab1%        = dec(entry(4,wSkjerm,";"))
              BestPris.Rab2Kr       = dec(entry(5,wSkjerm,";"))
              BestPris.Rab2%        = dec(entry(6,wSkjerm,";"))
              BestPris.Frakt        = dec(entry(7,wSkjerm,";"))
              BestPris.Frakt%       = dec(entry(8,wSkjerm,";"))
              BestPris.DivKostKr    = dec(entry(9,wSkjerm,";"))
              BestPris.DivKost%     = dec(entry(10,wSkjerm,";"))
              BestPris.Rab3Kr       = dec(entry(11,wSkjerm,";"))
              BestPris.Rab3%        = dec(entry(12,wSkjerm,";"))
              BestPris.VareKost     = dec(entry(13,wSkjerm,";"))
              BestPris.MvaKr        = dec(entry(14,wSkjerm,";"))
              BestPris.Mva%         = dec(entry(15,wSkjerm,";"))
              BestPris.DbKr         = dec(entry(16,wSkjerm,";"))
              BestPris.DB%          = dec(entry(17,wSkjerm,";"))
              BestPris.Pris         = dec(entry(18,wSkjerm,";"))
              BestPris.EuroPris     = dec(entry(19,wSkjerm,";"))
              BestPris.EuroManuel   = false.  
          end. /* CENTRALLAGER */
                  
      end. /* KALKULASJON */
      
    /* En lagerpost pr.. butikk, hvis det er lagerstyring */
    if ArtBas.Lager then
      do:
        find Lager no-lock where
          Lager.ArtikkelNr = ArtBas.ArtikkelNr and
          Lager.Butik      = Butiker.Butik no-error.
        if not available Lager then
          do:
            create Lager.
            assign
              Lager.ArtikkelNr = ArtBas.ArtikkelNr
              Lager.Butik      = Butiker.Butik.                    
          end.
      end.
                
    /* Artlag poster opprettes der hvor behovet oppstår. */
  end. /* BUTIKKLOOP */

  IF VALID-HANDLE(h_PrisKo) THEN
      DELETE PROCEDURE h_PrisKo.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-OpprettArtBas) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OpprettArtBas Procedure 
PROCEDURE OpprettArtBas :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/* Oppretter artikkel og utfører følgende:        */
/* - Oppretter eller oppdaterer artbas            */
/* - Oppdaterer valutakode på leverandør.         */
/* - Oppretter valuta.                            */
/* - Oppretter sesong hvis den ikke finnes        */
/* - Oppretter ArtPris hvis den ikke finnes.      */
/* - Bytter varegruppe, hvis den er forskjelling. */
/* Leverandør er tilgjengelig her.                */

def var wNyArtikkel as char initial "   " no-undo.
def var wBildNr     as int                no-undo. 

/* Gjør leverandøren tilgjengelig. */
find LevBas no-lock where
  LevBas.LevNr = int(wFLevNr) no-error.  
if not available LevBAs then
  return.

ARTIKKEL:
do TRANSACTION:  
  /* Flagger artikkel default. */
  assign
    wNyArtikkel = "Nei".
    
  /* Henter eller oppretter valutakode. */  
  find first Valuta no-lock where
    Valuta.ValKod = wFValKod no-error.
  if not available Valuta then
    do:
      create Valuta.
      assign
        Valuta.ValKod = wFValKod.
      assign
        Valuta.ValKurs  = dec(wFValKurs) / 100
        Valuta.ValLand  = ""
        Valuta.ValDatum = today
        wFeilKode       = 5.  
      run FeilKode (wFeilKode, "").
    end.
    
  /* Henter eller oppretter sesong */
  find first SaSong no-lock where
    SaSong.SasBeskr = wFSasong no-error.
  if not available SaSong then
    do:
      find Last SaSong no-lock no-error.
      if not available Sasong then
        wWork = 1.
      else wWork = SaSong.SaSong + 1.
      create SaSong.
      assign
        SaSong.SaSong   = wWork
        SaSong.SasBeskr = wFSaSong.
      find current SaSong no-lock.

      wFeilKode     = 7.
      run FeilKode (wFeilKode, "").
    end.
  
  /* Sjekker om valutakoden på artikkelen er endret. */  
  if LevBas.ValKod <> wFValKod then
    do:
      find current LevBas exclusive-lock.
      assign
        LevBas.ValKod = wFValKod
        wFeilKode     = 6.
      run FeilKode (wFeilKode, "").
      find current LevBas no-lock.
    end.
    
  /* Henter varegruppen */
  find VarGr no-lock where
    VarGr.Vg = int(wFArtGrp) no-error.

  /* Henter/oppretter artikkelen. */
  find first KonvReg no-lock where
    KonvReg.EDB-System = wEDB-System and
    KonvReg.Tabell     = wTabell   and
    KonvReg.EkstId     = wFDokumentNr no-error.
  if not available KonvReg then
    do:
      /* NB: ArtikkelNr settes i trigger. */
      create ArtBas.
      assign
        wNyArtikkel  = "Ja"
        wArtBasRecid = recid(ArtBas)
        ArtBas.Vg    = int(wFArtGrp)
        ArtBas.LopNr = ?.    
        
      assign
        ArtBas.HG = if available VarGr then VarGr.Hg else ArtBas.Hg.
    
      create KonvReg.
      assign
        KonvReg.EDB-System = wEDB-System 
        KonvReg.Tabell     = wTabell   
        KonvReg.EkstId     = wFDokumentNr
        KonvReg.InterntID  = string(ArtBas.ArtikkelNr).
    end.
  else 
    find ArtBas exclusive-lock where
      ArtBas.ArtikkelNr = dec(KonvReg.InterntId) no-error.
  if not available ArtBas then
    do:
      wFeilKode     = 4.  
      run FeilKode (wFeilKode,wFDokumentNr + "/" + wFUnderNr).  
      undo ARTIKKEL.
      return "AVBRYT".
    end.
  else
    wArtBasRecid = recid(ArtBas).
    
  /* Flagger at varegruppe byttes */
  if ArtBas.Vg <> int(wFArtGrp) then
    do:
      wFeilKode = 8.
      run FeilKode (wFeilKode, "(" + string(ArtBas.Vg) + " --> " + wFArtGrp + ")").  
    end.

  /* Flagger at inkjøpskategorien ikke er definert for varegruppen. */
  if not can-find(first VgKat where
                    VgKat.Vg    = int(wFArtGrp) and
                    VgKat.VgKat = int(wFKateg)) then
    do:
      wFeilKode = 9.
      run FeilKode (wFeilKode, "(" + wFArtGrp + "/" + wFKateg + ")").  
    end.

  /* Flagger at leverandør er byttet på artikkelen. */
  if (ArtBas.LevNr <> LevBas.LevNr and ArtBas.LevNr <> 0) then
    do:
      wFeilKode = 9.
      run FeilKode (wFeilKode, "(" + string(ArtBas.LevNr) + " --> " + string(LevBas.LevNr) + ")").  
    end.
    
  /* Her får vi beskrivelsen, ikke varemerkekoden. Dvs oppslag på beskrivelse. */  
  find first Varemerke no-lock where
    VareMerke.Beskrivelse = wFVarum no-error.
  if not available VareMerke then
    do:
      find last VareMerke no-lock no-error.
      wWork = if available Varemerke
                then Varemerke.VMId + 1
                else 1.
      create VareMerke.
      assign
        VareMerke.VMId        = wWork
        VareMerke.KortNavn    = wFVarum
        Varemerke.Beskrivelse = wFVarum.
    
      wFeilKode = 10.
      run FeilKode (wFeilKode, " (" + wFVarum + ")").  
    end.

  /* Henter varegruppen. */
  find VarGr no-lock where
    VarGr.Vg = int(wFArtGrp) no-error.
  if not available VarGr then
    do:
      wFeilKode = 11.
      run FeilKode (wFeilKode, " (" + wFVarum + ")").      
      undo ARTIKKEL.
      return "AVBRYT".
    end.
  find Moms of VarGr no-lock no-error.
  if not available Moms then
    do:
      wFeilKode = 15.
      run FeilKode (wFeilKode, " (" + string(VarGr.MomsKod) + ")").      
      undo ARTIKKEL.
      return "AVBRYT".
    end.

  /* Fargekode */
  find first Farg no-lock where
    Farg.Farg = int(wFEFarg) no-error.
  if not available Farg then
    do:
      create Farg.
      assign
        Farg.Farg     = int(wFEFarg)
        Farg.FarBeskr = "**Fra eXcellence PRO**".
    
      wFeilKode = 12.
      run FeilKode (wFeilKode, " (" + wFEFarg + ")").  
    end.

  /* Materialkode */
  find first Material no-lock where
    Material.MatBeskr = wFMaterial no-error.
  if not available Material then
    do:
      find last Material no-lock no-error.
      wWork = if available Material
                then Material.MatKod + 1
                else 1.
      create Material.
      assign
        Material.MatKod   = wWork
        Material.MatBeskr = wFMaterial.
    
      wFeilKode = 13.
      run FeilKode (wFeilKode, " (" + wFMaterial + ")").  
    end.
  
  /* Kopierer bilde på artikkelen til SkoTex BildeKatalog. */
  find Bilderegister no-lock where
    ArtBas.BildNr = BildeRegister.BildNr no-error.    
  /* Finnes artikkelen, men den har ikke bilde, fikser vi dette */  
  If available BildeRegister and BildeRegister.BildNr = 0 then
    release BildeRegister.    
  if not available Bilderegister then
    do:
      if search(trim(wBildeKatFlexi + "\" + wFDokumentNr) + ".bmp") <> ? then
        do:
          if valid-handle(wLibHandle) then
          do:
          run bildenummer in wLibHandle (input "I",output wBildNr,output wBildeFil,output wBildeKatSkoTex).
          OVERLAY(wBildeFil, index(wBildeFil,".jpg"), 4, "CHARACTER") = ".bmp".
          os-copy value(wBildeKatFlexi + "\" + wFDokumentNr + ".bmp") 
                  value(wBildeKatSkoTex + "\" + wBildeFil).                              
          find BildeRegister exclusive-lock where
            Bilderegister.BildNr = wBildNr.
          if available BildeRegister then
            BildeRegister.FilNavn = wBildeFil.
          release BildeRegister.
        end.
      end.       
    end.
  if available Bilderegister then
    do:
      find current BildeRegister exclusive-lock.
      assign
        Bilderegister.Merknad = wFDokumentNr.
    end.

  /* Logger artikkelen */
  wFeilKode = 14.
  run FeilKode (wFeilKode, "* " + wNyArtikkel + "/" +
                wFDokumentNr + "/" + wFUnderNr + 
                " (ArtikkelNr: " + string(ArtBas.ArtikkelNr)).  

  /* Oppdaterer artikkelinformasjonen. */    
  assign
    ArtBas.Vg         = int(wFArtGrp)
    ArtBas.Lager      = true
    ArtBas.Aktivert   = true
    ArtBas.Storrelser = true
    ArtBas.SaSong     = SaSong.SaSong
    ArtBas.Notat      = wFDokumentNr + "/" + wFUnderNr + chr(13) + wFFriTekst
    /*ArtBas.LevTid-1   = wFLevTid*/
    ArtBas.VgKat      = int(wFKateg)
    /*ArtBas.Korr_Dato  = today*/
    ArtBas.Beskr      = if wEgenArtKod <> ""
                          then wEgenArtKod
                          else ArtBas.Beskr
    ArtBas.LevNr      = LevBas.LevNr 
    ArtBas.LevKod     = wFLevArtKod
    ArtBas.LevFargKod = wFLevFarg
    ArtBas.AktivAv    = userid("dictdb")
    ArtBas.AktivDato  = today
    ArtBas.ValKod     = wFValKod
    ArtBas.VMId       = if available Varemerke
                          then VareMerke.VMId
                          else ArtBas.VMId
    ArtBas.BongTekst  = if ArtBas.BongTekst <> ""
                          then ArtBas.BongTekst
                          else VarGr.VgBeskr
    ArtBas.AnonseArtikkel = if wFAnnons = "N" 
                          then false
                          else true
    ArtBas.Farg       = if available Farg 
                         then Farg.Farg
                         else ArtBas.Farg
    ArtBas.MatKod     = if available Material
                          then Material.MatKod
                          else ArtBas.MatKod
    ArtBas.BildNr     = wBildNr.

    /* Oppretter eller oppdaterer bestilling i importfil.      */
    CREATE BestHode.
    ASSIGN 
      BestHode.ArtikkelNr      = ArtBas.ArtikkelNr
      BestHode.Beskrivelse     = "Dokument: " + wFDokumentNr + "/" + wFUnderNr
      BestHode.BestStat        = 1
      BestHode.StrTypeID       = ArtBas.StrTypeID 
      BestHode.LevFargKod      = wFLevFarg
      BestHode.LevKod          = wFLevArtKod
      BestHode.LevNr           = LevBas.LevNr
      BestHode.LevTid          = wFLevTid
      BestHode.OrdreNr         = wOrdreNr
      BestHode.BestillingsDato = wFBDato
      wBestNr                  = BestHode.BestNr. /* Tildelt av trigger */

    /* Logger bestilling for opdpatering av status på bestillingen.  */
    /* Bestillinger med feil, blir ikke oppdatert. */
    find BestListe where 
      BestListe.tmpBestNr = BestHode.BestNr no-error.
    if not available BestListe then
      do:
        create BestListe.
        assign
          BestListe.tmpBestNr = BestHode.BestNr.
      end.

    /* Melder bestillingsnummer. */
    wFeilKode = 17.
    run FeilKode (wFeilKode, "* " + string(ArtBas.ArtikkelNr) + "/" + string(BestHode.BestNr) +   "").  
  
    /* Kalkulerer artikkelen. */
    run Kalkulasjon (wNyArtikkel).  
end. /* ARTIKKEL TRANSACTION */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-OpprettOrdre) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OpprettOrdre Procedure 
PROCEDURE OpprettOrdre :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
def buffer bufOrdre for Ordre.
  
do TRANSACTION:  
  find first Ordre exclusive-lock where
    Ordre.LevNr  = int(wFLevNr) and
    Ordre.EkstId = wFBestNr no-error.  
  if not available Ordre then
    OPPSTANDELSEN:
    do:
      assign wPosterlinjer = true.
      find Last bufOrdre no-lock no-error.
      create Ordre.
      assign
        Ordre.OrdreNr = if available bufOrdre
                          then bufOrdre.OrdreNr + 1
                          else 1
        wOrdreNr      = Ordre.OrdreNr.    
      assign
        Ordre.EkstId      = wFBestNr   
        Ordre.LevNr       = int(wFLevNr)
        Ordre.SendtDato   = wFBDato
        Ordre.OrdreStatus = 2
        wOrdreRecid       = recid(Ordre).
        Ordre.Merknad     = "eXcellence PRO " + string(today) + " (" +
                            trim(wFBestNr) + ") " + 
                            userid("dictdb").
    
      /*    
        Ordre.LevAdresse1 ~
        Ordre.LevAdresse2 ~
        Ordre.LevKontakt ~
        Ordre.LevMerknad ~
        Ordre.LevPostBoks ~
        Ordre.LevPostNr ~
        Ordre.LevTelefon ~
        Ordre.Notat ~
      */
      release Ordre.
    end. /* OPPSTANDELSEN */
  else do:
    assign 
      wOrdreNr      = Ordre.OrdreNr    
      wOrdreRecid   = recid(Ordre)
      wPosterlinjer = false
      wFeilKode     = 3.  
    run FeilKode (wFeilKode,"  LevNr/BestNr: " + wFLevNr + "/" + wFBestNr).  
    wFeilKode = 0.
  end.    
  
end. /* TRANSACTION */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SumFriSort) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SumFriSort Procedure 
PROCEDURE SumFriSort :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 def input parameter ipBestNr as int no-undo.

  def var wAntPrStr     as int extent 48 no-undo.
  def var wStorrelser   as char no-undo.
  def var wFordeling    as char no-undo.
  def var wLoop         as int no-undo.

  find first BestSort where 
    BestSort.BestNr = ipBestNr and
    BestSort.Fri    = true no-error.
  if not available BestSort then 
    return.
  find BestHode no-lock where
    BestHode.BestNr = ipBestNr no-error.

  if not available BestHode then
    return.
  
  run x-korrbestilling.p (input BestHode.BestNr).
  
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

