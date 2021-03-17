&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Include 
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

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Include
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: INCLUDE-ONLY
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Include ASSIGN
         HEIGHT             = 15
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Include 


/* ***************************  Main Block  *************************** */


  DEF VAR pcTabell  AS CHAR NO-UNDO.
  DEF VAR pcDato    AS CHAR NO-UNDO.
  DEF VAR pcTid     AS CHAR NO-UNDO.
  DEF VAR pcFilNavn AS CHAR NO-UNDO.

  DEF VAR piAntArt  AS INT  NO-UNDO.

  DEF BUFFER bImportHode FOR ImportHode.

  /* Nullstiller variant */
  {&Nullstill} 

  DO with FRAME default-frame:
  
  ASSIGN
      cErrLst  = IF lBekreft = TRUE
                 THEN ""
                 ELSE cErrLst
      pcTabell = "{&Tabell}"
      wLoop    = 0
      .

  APPLY "Choose" TO B-SjekkFil.
  IF {&Sjekk} = FALSE THEN
  DO:
      MESSAGE "Ugyldig filnavn " pcFilNavn
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      RETURN "AVBRYT".
  END.

  IF NOT AVAILABLE ImportHode THEN
  DO:
      MESSAGE "Ingen rapporthode tilgjengelig"
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      RETURN "AVBRYT".
  END.

  ASSIGN
      pcFilNavn = INPUT FI-Katalog + "/" + INPUT {&FilNavn}.

  STATUS DEFAULT "Kontroll av filnavn....".
  IF SEARCH(pcFilNavn) = ? THEN
  DO:
    MESSAGE "Ugyldig filnavn"
      VIEW-AS ALERT-BOX ERROR BUTTONS OK TITLE "Importfeil".
    RETURN "AVBRYT".
  END.

  /* Kontrollerer om import allerede er utført. */
  IF lBekreft = TRUE THEN
  DO:
    IF CAN-FIND(FIRST ImportLinje WHERE
      ImportLinje.BatchNr = ImportHode.BatchNr AND
      ImportLinje.Tabell  = pcTabell) THEN
    DO:
        lSvar = FALSE.
        MESSAGE "Filen er allerede lest inn i importbufferet." SKIP
                "Skal ny import startes alikevel?"
            VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "Bekreft"
            UPDATE lSvar.
        IF lSvar = FALSE THEN
            RETURN "AVBRYT".
    END.
  END.
  STATUS DEFAULT "Teller antall linjer i importfilen. Vent litt.....".
  INPUT STREAM Inn FROM VALUE(pcFilNavn).
  IMP-LOOP:
  REPEAT: 
    IMPORT STREAM Inn UNFORMATTED wLinje.
    
    ASSIGN
      wLoop = wLoop + 1.
  END. /* IMP-LOOP */
  INPUT STREAM Inn CLOSE.
  
  ASSIGN
    wTotAnt = wLoop - 2. /* Tar bort hode posten */
  
  /* Informerer om antall poster og krever bekreftelse for å starte. */
  ASSIGN
      lSvar = TRUE.
  IF lBekreft = TRUE THEN
  DO:
    MESSAGE "Skal import starte? (Antall linjer: " + STRING(wLoop) + ")" SKIP(1)
      VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "Bekreft start av import"
      UPDATE lSvar AS LOG.
    IF lSvar <> TRUE THEN
    DO:
      STATUS DEFAULT " ".
      RETURN NO-APPLY "AVBRYT".
    END.
  END.
  
  IF wTotAnt > 5 THEN
  DO:
    ASSIGN
      chCtrlFrame:Visible             = true
      chCtrlFrame:ProgressBar:Min     = 1
      chCtrlFrame:ProgressBar:Max     = wTotAnt
      chCtrlFrame:ProgressBar:Value   = 1.
  END.
  
  ASSIGN
    wLoop = 0.                            
                              
  STATUS DEFAULT "Import startet.....".
  INPUT STREAM Inn FROM VALUE(pcFilNavn).
  IMP-LOOP:
  REPEAT TRANSACTION: 
    IMPORT STREAM Inn UNFORMATTED wLinje.
    
    {&PreSjekk}

    ASSIGN
      wLoop = wLoop + 1.

    CASE entry(1,wLinje," "):
        {&Case1}
    END CASE.

    /* De tre første postene er hode record. */
    IF wLoop <= 2 THEN
    HODE:
    DO FOR bImportHode:
      IF wLoop = 2 THEN 
      DO:
        FIND bImportHode EXCLUSIVE-LOCK WHERE
            bImportHode.BatchNr = INPUT FI-BatchNr NO-ERROR.
        IF AVAILABLE bImportHode THEN
        ASSIGN
            bImportHode.KundeKode    = wKundKod
            bImportHode.Bunt         = wBunt
            bImportHode.ImportDato   = TODAY /*wDatum*/
            bImportHode.ImportTid    = TIME  /*wTid*/
            .
        IF AVAILABLE bImportHode THEN 
          RELEASE bImportHode.
      END.

      NEXT IMP-LOOP.
    END. /* HODE */
      
    IF wLoop > 5 THEN
    DO:
      if wLoop MODULO 10 = 0 then
      DO:
        chCtrlFrame:ProgressBar:Value = IF wLoop > wTotAnt
                                          THEN wTotAnt
                                          ELSE wLoop.      
      END.
    END.
        
    /* Oppretter linjen */
    IF entry(1,wLinje," ") = "{&KODE}" THEN
    DO:
      FIND ImportLinje EXCLUSIVE-LOCK WHERE
        ImportLinje.BatchNr = ImportHode.BatchNr AND
        ImportLinje.Tabell  = pcTabell           AND 
        ImportLinje.LinjeNr = wLoop - 2 NO-ERROR.
      IF NOT AVAILABLE ImportLinje then
        CREATE ImportLinje.
      ASSIGN
        piAntArt = piAntArt + 1
        ImportLinje.BatchNr = ImportHode.BatchNr
        ImportLinje.Tabell  = pcTabell
        ImportLinje.LinjeNr = wLoop - 2
        .
    END.
    
    {&Variant}   /* Oppretter variant */
    {&Pris}      /* Oppretter pris.   */
    {&Sortiment} /* Opprett sortiment */  
    
    /* Oppdaterer feltene. Hopper over blanke linjer. */
    IF ENTRY(1,wLinje," ") <> "" THEN
    CASE ENTRY(1,wLinje," "):
      {&Case2}
      {&Case3}
      {&Case4}
      OTHERWISE
          DO:
              /* Gir feilmelding fordi verdi blir for stor.
              IF NOT CAN-DO(cErrLst,trim(SUBSTRING(wLinje,INDEX(wLinje,' ') + 1))) THEN
              ASSIGN
                  cErrLst = cErrLst + 
                            (IF cErrLst = ""
                               THEN ""
                               ELSE "|") + 
                            trim(SUBSTRING(wLinje,INDEX(wLinje,' ') + 1)).
              */
          END.
    END CASE.
    
    /*
    MESSAGE 
        ImportLinje.Felt[1]
        ImportLinje.Felt[2]
        ImportLinje.Felt[3]
        ImportLinje.Felt[4]
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
    */

    /* Bruker avbryter importen. */
    PROCESS EVENTS.
    if wStopp then
      do:
        message "Skal import stoppes? " view-as alert-box buttons YES-NO 
                 set lSvar.
        if lSvar = true then
          LEAVE IMP-LOOP.
        ELSE 
          wStopp = FALSE.
      end.
     
  END. /* IMP-LOOP */
  INPUT STREAM Inn CLOSE.
    
  IF lBekreft THEN
  DO:
    IF cErrLst <> "" THEN
        MESSAGE "Ukjente felt:" SKIP
                cErrLst
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
  END.

  STATUS DEFAULT "Import av " + STRING(piAntArt) + " poster ferdig.".
  IF lBekreft THEN
  MESSAGE "Import av " STRING(wLoop) " er ferdig."
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
  
  STATUS DEFAULT " ".
  ASSIGN
    chCtrlFrame:Visible             = false
    chCtrlFrame:ProgressBar:Min     = 1
    chCtrlFrame:ProgressBar:Max     = 100
    chCtrlFrame:ProgressBar:Value   = 1.
  END. /* Frame Scoop */

  /* Sikrer at postene slippes. */
  IF AVAILABLE ImportLinje THEN
      RELEASE ImportLinje.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


