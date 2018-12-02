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

DEF INPUT PARAMETER iTelleNr AS INT    NO-UNDO.
DEF INPUT PARAMETER h_Parent AS HANDLE NO-UNDO.

DEF VAR wNedSkriv     AS LOG  NO-UNDO.
DEF VAR wTekst        AS CHAR NO-UNDO.
DEF VAR wBatchNr      AS INT  NO-UNDO.
DEF VAR wEDB-System   AS CHAR NO-UNDO.
DEF VAR wTabell       AS CHAR NO-UNDO.  
DEF VAR wCl           AS INT  NO-UNDO.
DEF VAR wTransNr      AS INT  NO-UNDO.

DEF BUFFER clButiker FOR Butiker.

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


FIND TelleHode NO-LOCK WHERE
    TelleHode.TelleNr = iTelleNr NO-ERROR.
IF NOT AVAILABLE TelleHode THEN
    RETURN "AVBRYT".
                    
{syspara.i 4 1 2 wTekst}
IF TelleHode.TTID = int(wTekst) 
  THEN wNedskriv = TRUE.
ELSE wNedskriv = FALSE.

{syspara.i 1 2 4 wEDB-System}
ASSIGN wTabell = "ArtBas".

{syspara.i 5 1 1 wCl INT}
FIND clButiker NO-LOCK WHERE
  clButiker.Butik = wCl NO-ERROR.

IF TelleHode.TTId = 1 THEN
    RUN OppdatTelleHode (INPUT TODAY). /* Varesalg */
ELSE
    RUN OppdaterTelling. /* Allt Annet. */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-OppdaterDagsrapport) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OppdaterDagsrapport Procedure 
PROCEDURE OppdaterDagsrapport :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

/*   {dagsrapport.i} */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-OppdaterTelling) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OppdaterTelling Procedure 
PROCEDURE OppdaterTelling :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR wAntLinjer AS INT NO-UNDO.

  FIND TransType NO-LOCK WHERE
    TransType.TTId = TelleHode.TTId NO-ERROR.
  /*
  find TransBeskr no-lock where
    TransBeskr.TTID = TelleHode.TTId and
    TransBeskr.TBId = TelleHode.TBID no-error.
  */    
  {sww.i}
  RUN batchlogg.w (PROGRAM-NAME(1), 
                   "Varetelling - Type: " + TransType.Beskrivelse,
                    OUTPUT wBatchNr).

/*   if TelleHode.AntLinjer >= 5 then                */
/*     do:                                           */
/*       assign                                      */
/*         chProgressBar:Min   = 1                   */
/*         chProgressBar:Max   = TelleHode.AntLinjer */
/*         chProgressBar:Value = 1.                  */
/*       view frame FRAME-Progress.                  */
/*     end.                                          */

  /* Leser alle tellerader. */
  TELLELINJE:
  FOR EACH TelleLinje OF TelleHode EXCLUSIVE-LOCK WHERE
    TelleLinje.Oppdatert = FALSE
    BREAK
    BY TelleLinje.TelleNr
    BY TelleLinje.Vg
    BY TelleLinje.LopNr
    BY TelleLinje.Butik 
    BY TelleLinje.Storl TRANSACTION:
    
    IF FIRST-OF(TelleLinje.Butik) THEN
      FIND Butiker NO-LOCK WHERE
        Butiker.Butik = TelleLinje.Butik NO-ERROR.
    
    ASSIGN
      wAntLinjer = wAntLinjer + 1
      TelleLinje.Oppdatert = TRUE.      
      
    IF wAntLinjer >= 5 THEN
      DO:
        IF wAntLinjer MODULO 10 = 0 AND valid-handle(h_Parent) THEN
          RUN Teller IN h_Parent (INPUT wAntLinjer) NO-ERROR.
      END.

    /* Tar bort tellelås på artikkel/butikk. */
    IF LAST-OF(TelleLinje.Butik) THEN
      DO:
        FIND FIRST KonvReg EXCLUSIVE-LOCK WHERE
          KonvReg.EDB-System = wEDB-System AND
          KonvReg.Tabell     = wTabell     AND
          KonvReg.EkstId     = STRING(TelleLinje.ArtikkelNr) + "," + 
                               string(TelleLinje.Butik) AND
          KonvReg.InterntId  = STRING(TelleLinje.ArtikkelNr) + "," + 
                               string(TelleLinje.Butik) NO-ERROR.
        IF AVAILABLE KonvReg THEN
          DELETE KonvReg.      
      END.
    
    /* Er diff lik 0, er et ingenting å oppdatere på en vanlig telling. */
    IF CAN-DO('9',STRING(TelleHode.TTId)) AND TelleLinje.AntallDiff = 0 THEN 
      DO:
        ASSIGN
          TelleLinje.Oppdatert = TRUE.
        NEXT TELLELINJE.
      END.
      
    /* Er det nedskrivning, skal det kun opprettes transaksjoner der hvor det er gjordt nedskrivning . */
    /* Dvs der hvor Nedskrevet er forsjkellig fra VVareKost.                                           */
    IF CAN-DO("8",STRING(TelleHode.TTId)) THEN
      DO:
        IF TelleLinje.Nedskrevet = TelleLinje.VVareKost THEN
          NEXT TELLELINJE.
      END.

    FIND ArtBas NO-LOCK WHERE
        ArtBas.ArtikkelNr = TelleLinje.ArtikkelNr NO-ERROR.

    IF AVAILABLE ArtBas AND
        ArtBas.OPris = FALSE AND
        ArtBas.Pakke = FALSE THEN
    LAGERSTYRTE:
    DO:
        /* Oppretter transaksjoner */
        IF wNedskriv THEN
          RUN OppdatNedskriv.
        ELSE
          RUN OppdatTrans.
    END. /* LAGERSTYRTE */
  END. /* TELLELINJE */

  RUN OppdatTelleHode (INPUT TODAY).

/*   /* Oppdaterer dagsrapporten for varesalg. */  */
/*   IF TelleHode.TTId = 1 THEN                    */
/*       RUN OppdaterDagsrapport (INPUT wBatchNr). */

  RUN batchstatus.p (wBatchNr, 2).
  {swn.i}
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-OppdatNedskriv) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OppdatNedskriv Procedure 
PROCEDURE OppdatNedskriv :
/*------------------------------------------------------------------------------
  Purpose:     Oppretter en transaksjon pr. butikk/vg og løpenummer.
               Denne transaksjonen posterer mot lager. I LAger påvirkes feltet
               VVAreKost som er den vektede varekost for butikken.
               Det er nok med en transaksjon for å oppnå det ønskede
               resultat.
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  POSTER_BLOKK:
  DO:
    /* Transaksjonsnummer for butikken. */
    FIND LAST TransLogg NO-LOCK WHERE
      TransLogg.Butik = TelleLinje.Butik USE-INDEX TransLogg NO-ERROR.
    IF AVAILABLE TransLogg THEN
      wTransNr = TransLogg.TransNr + 1.
    ELSE 
      wTransNr = 1.

    /* Oppretter transaksjon */
    LAG_TRANS:
    DO:
      /* Sjekker at transnr er ledig */
      IF CAN-FIND(TransLogg WHERE
                  TransLogg.Butik   = TelleLinje.Butik AND
                  TransLogg.TransNr = wTransNr) THEN
      NESTE_NR:
      DO WHILE TRUE:
        wTransNr = wTransNr + 1.
        IF CAN-FIND(TransLogg WHERE
                    TransLogg.Butik   = TelleLinje.Butik AND
                    TransLogg.TransNr = wTransNr) THEN
          NEXT NESTE_NR.
       ELSE
          LEAVE NESTE_NR.
      END. /* NESTE_NR */

      CREATE TransLogg.
      ASSIGN TransLogg.Butik        = TelleLinje.Butik
             TransLogg.TransNr      = wTransNr
             TransLogg.SeqNr        = 1.
      ASSIGN TransLogg.BatchNr      = wBatchNr
             TransLogg.KundNr       = 0
             TransLogg.TTId         = TelleHode.TTId
             TransLogg.TBId         = TelleHode.TBId
             TransLogg.ArtikkelNr   = TelleLinje.ArtikkelNr
             TransLogg.LevNr        = TelleLinje.LevNr
             TransLogg.BongId       = 0
             TransLogg.BongLinjeNr  = 0
             TransLogg.KassaNr      = 0
             TransLogg.Vg           = TelleLinje.Vg
             TransLogg.LopNr        = TelleLinje.LopNr
             TransLogg.Storl        = ""
             TransLogg.Dato         = TODAY
             TransLogg.Tid          = TIME
             TransLogg.BestNr       = 0
             TransLogg.Postert      = FALSE                                             
             TransLogg.Plukket      = TRUE /* Skal ikke ut på plukkliste */
             TransLogg.Antall       = IF TelleLinje.AntallPar <= 0 /* For at transene skal kunne bakkes ut */
                                        THEN 1
                                        ELSE TelleLinje.AntallPar
             TransLogg.Pris         = TelleLinje.VVareKost - TelleLinje.Nedskrevet
             TransLogg.RabKr        = 0
             TransLogg.Mva          = 0.
/*             
message "Fra OppdatNedskriv i w-bTelleHode:" skip(1)
        "TransLogg.Antall:" TransLogg.Antall skip
        "TransLogg.Pris:" TransLogg.Pris skip
        "TelleLinje.VVareKost:" TelleLinje.VVareKost skip
        "TelleLinje.Nedskrevet:" TelleLinje.Nedskrevet
        view-as alert-box.
*/        

                     
    END. /* LAG_TRANS */
    
  END. /* POSTER_BLOKK */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-OppdatTelleHode) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OppdatTelleHode Procedure 
PROCEDURE OppdatTelleHode :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER wToday AS DATE.
   
  DEF BUFFER bTelleHode FOR TelleHode.

  DO TRANSACTION:
    FIND bTelleHode EXCLUSIVE-LOCK WHERE
      ROWID(bTelleHode) = rowid(TelleHode).
    ASSIGN
      bTelleHode.Oppdatert  = IF wToday <> ? 
                                THEN wToday
                                ELSE bTelleHode.Oppdatert
      bTelleHode.BatchNr    = wBatchNr
      bTelleHode.AntallPar  = 0
      bTelleHode.AntallTalt = 0
      bTelleHode.OpptVerdi  = 0
      bTelleHode.VerdiDiff  = 0
      bTelleHode.AntallDiff = 0
      bTelleHode.OpprVerdi  = 0
      bTelleHode.AntLinjer  = 0.
    FOR EACH TelleLinje OF bTelleHode NO-LOCK:

      ASSIGN
        bTelleHode.AntallPar  = bTelleHode.AntallPar  + TelleLinje.AntallPar
        bTelleHode.AntallTalt = bTelleHode.AntallTalt + TelleLinje.AntallTalt
        bTelleHode.OpprVerdi  = bTelleHode.OpprVerdi  + TelleLinje.OpprVerdi      
        bTelleHode.OpptVerdi  = bTelleHode.OpptVerdi  + TelleLinje.OpptVerdi
        bTelleHode.AntLinjer  = bTelleHode.AntLinjer  + 1.      

      IF wNedSkriv THEN
        DO:
          ASSIGN
            bTelleHode.VerdiDiff  = bTelleHode.OpprVerdi  - bTelleHode.OpptVerdi
            bTelleHode.AntallDiff = bTelleHode.AntallPar  - bTelleHode.AntallTalt.      
        END.
      ELSE DO:
        ASSIGN
          bTelleHode.VerdiDiff  = bTelleHode.OpprVerdi  - bTelleHode.OpptVerdi
          bTelleHode.AntallDiff = bTelleHode.AntallPar  - bTelleHode.AntallTalt.      
      END.

    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-OppdatTrans) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OppdatTrans Procedure 
PROCEDURE OppdatTrans :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF BUFFER bufTransLogg FOR TransLogg.

DO FOR bufTransLogg:
    /* Henter artpris */
    IF AVAILABLE Butiker THEN
      DO:
        FIND ArtPris NO-LOCK WHERE
          ArtPris.ArtikkelNr = TelleLinje.ArtikkelNr AND
          ArtPris.ProfilNr   = Butiker.ProfilNr NO-ERROR.
        IF NOT AVAILABLE ArtPris THEN
          FIND ArtPris NO-LOCK WHERE
            ArtPris.ArtikkelNr = TelleLinje.ArtikkelNr AND
            ArtPris.ProfilNr   = clButiker.ProfilNr NO-ERROR.
      END.
    ELSE 
      FIND ArtPris NO-LOCK WHERE
        ArtPris.ArtikkelNr = TelleLinje.ArtikkelNr AND
        ArtPris.ProfilNr   = clButiker.ProfilNr NO-ERROR.

    /* Henter lager */
    FIND Lager NO-LOCK WHERE
         Lager.ArtikkelNr = ArtPris.ArtikkelNr AND
        Lager.Butik = TelleLinje.Butik NO-ERROR.
            
    /* Transaksjonsnummer for butikken. */
    FIND LAST bufTransLogg NO-LOCK WHERE
      bufTransLogg.Butik = TelleLinje.Butik USE-INDEX TransLogg NO-ERROR.
    IF AVAILABLE bufTransLogg THEN
      wTransNr = bufTransLogg.TransNr + 1.
    ELSE 
      wTransNr = 1.

    /* Oppretter transaksjon */
    LAG_TRANS:
    DO:
      /* Sjekker at transnr er ledig */
      IF CAN-FIND(bufTransLogg WHERE
                  bufTransLogg.Butik   = TelleLinje.Butik AND
                  bufTransLogg.TransNr = wTransNr) THEN
      NESTE_NR:
      DO WHILE TRUE:
        wTransNr = wTransNr + 1.
        IF CAN-FIND(bufTransLogg WHERE
                    bufTransLogg.Butik   = TelleLinje.Butik AND
                    bufTransLogg.TransNr = wTransNr) THEN
          NEXT NESTE_NR.
       ELSE
          LEAVE NESTE_NR.
      END. /* NESTE_NR */

      CREATE bufTransLogg.
      ASSIGN bufTransLogg.Butik         = TelleLinje.Butik
             bufTransLogg.TransNr       = wTransNr
             bufTransLogg.SeqNr         = 1.
      ASSIGN bufTransLogg.BatchNr       = wBatchNr
             bufTranslogg.BongTekst     = Tellelinje.Beskr
             bufTransLogg.Kode          = TelleLinje.Kode
             bufTranslogg.vVarekost     = 0 /* Settes ved oppdatering av transaksjonen. */
             bufTransLogg.SattVVareKost = FALSE
             bufTransLogg.KundNr        = 0
             bufTransLogg.TTId          = TelleHode.TTId
             bufTransLogg.TBId          = TelleHode.TBId
             bufTransLogg.ArtikkelNr    = TelleLinje.ArtikkelNr
             bufTransLogg.LevNr         = TelleLinje.LevNr
             bufTransLogg.BongId        = 0
             bufTransLogg.BongLinjeNr   = 0
             bufTransLogg.KassaNr       = 0
             bufTransLogg.Vg            = TelleLinje.Vg
             bufTransLogg.LopNr         = TelleLinje.LopNr
             bufTransLogg.Storl         = TelleLinje.Storl
             bufTransLogg.Dato          = TelleHode.StartDato
             bufTransLogg.Tid           = TIME
             bufTransLogg.BestNr        = 0
             bufTransLogg.Postert       = FALSE
             bufTransLogg.OvButik       = TelleHode.TilButikk
             .                                             
             
      /* Tillatte transaksjonstyper 1,2,3,5,7,8,9,10,11 */       
      CASE TelleHode.TTId:
/*         when 1  /* Varesalg        */ then                                                                                                   */
/*           assign                                                                                                                             */
/*             bufTransLogg.Plukket = true /* Skal ikke ut på plukkliste */                                                                     */
/*             bufTransLogg.Antall = TelleLinje.AntallTalt                                                                                      */
/*             bufTransLogg.Pris   = TelleLinje.VVareKost                                                                                       */
/*             bufTransLogg.RabKr  = TelleLinje.RabKr                                                                                           */
/*             bufTransLogg.Mva    = (TelleLinje.VVareKost - TelleLinje.RabKr) -                                                                */
/*                                   ((TelleLinje.VVAreKost - TelleLinje.RabKr) / (1 + (ArtPris.Mva%[if ArtPris.Tilbud then 2 else 1] / 100))). */
        WHEN 2  /* Brekkasje       */ THEN 
          ASSIGN
            bufTransLogg.Plukket = TRUE /* Skal ikke ut på plukkliste */
            bufTransLogg.Antall = TelleLinje.AntallDiff * -1
            bufTransLogg.Pris   = TelleLinje.VVareKost
            bufTransLogg.RabKr  = 0
            bufTransLogg.Mva    = 0.
        WHEN 3  /* Kundereklamasjon */ THEN 
          ASSIGN
            bufTransLogg.Plukket = TRUE /* Skal ikke ut på plukkliste */
            bufTransLogg.Antall  = TelleLinje.AntallDiff
            bufTransLogg.Pris    = TelleLinje.VVareKost 
            bufTransLogg.RabKr   = TelleLinje.RabKr
            bufTransLogg.Mva     = (TelleLinje.VVareKost - TelleLinje.RabKr) - 
                                  ((TelleLinje.VVAreKost - TelleLinje.RabKr) / (1 + (ArtPris.Mva%[IF ArtPris.Tilbud THEN 2 ELSE 1] / 100))).
        WHEN 5  /* Varekjøp        */ THEN 
          ASSIGN
            bufTransLogg.Plukket = TRUE /* Skal ikke ut på plukkliste */
            bufTransLogg.Antall = TelleLinje.AntallTalt /*TelleLinje.AntallDiff * -1*/
            bufTransLogg.Pris   = TelleLinje.VVareKost
            bufTransLogg.RabKr  = 0
            bufTransLogg.Mva    = 0.
/*         when 6  /* Overføring       */ then                              */
/*           assign                                                         */
/*             bufTransLogg.Plukket = true /* Skal ikke ut på plukkliste */ */
/*             bufTransLogg.Antall = TelleLinje.AntallDiff * -1             */
/*             bufTransLogg.Pris   = TelleLinje.VVareKost                   */
/*             bufTransLogg.RabKr  = 0                                      */
/*             bufTransLogg.Mva    = 0.                                     */
        WHEN 7  /* Lagerjustering  */ THEN 
          ASSIGN
            bufTransLogg.Plukket = TRUE /* Skal ikke ut på plukkliste */
            bufTransLogg.Antall = TelleLinje.AntallTalt
            bufTransLogg.Pris   = TelleLinje.VVareKost
            bufTransLogg.RabKr  = 0
            bufTransLogg.Mva    = 0.
        WHEN 8  /* Nedskriving     */ THEN 
          ASSIGN
            bufTransLogg.Plukket = TRUE /* Skal ikke ut på plukkliste */
            bufTransLogg.Antall = TelleLinje.AntallDiff
            bufTransLogg.Pris   = TelleLinje.Nedskrevet
            bufTransLogg.RabKr  = 0
            bufTransLogg.Mva    = 0.
        WHEN 9  /* Svinn           */ THEN 
        DO:
            ASSIGN
              bufTransLogg.Plukket = TRUE /* Skal ikke ut på plukkliste */
              bufTransLogg.Antall = TelleLinje.AntallDiff
              bufTransLogg.Pris   = TelleLinje.VVareKost
              bufTransLogg.RabKr  = 0
              bufTransLogg.Mva    = 0.
            IF AVAILABLE Lager THEN
            DO:
                IF Lager.VVareKost = 0 THEN
                DO:
                    FIND CURRENT Lager EXCLUSIVE-LOCK.
                    ASSIGN
                        Lager.VVareKost = TelleLinje.VVareKost
                        .
                    RELEASE Lager.
                END.
            END.

        END.
        WHEN 10  /* Gjennkjøp */ THEN
          ASSIGN
            bufTransLogg.Plukket = TRUE /* Skal ikke ut på plukkliste */
            bufTransLogg.Antall  = TelleLinje.AntallDiff * -1
            bufTransLogg.Pris    = TelleLinje.VVareKost 
            bufTransLogg.RabKr   = TelleLinje.RabKr
            bufTransLogg.Mva     = (TelleLinje.VVareKost - TelleLinje.RabKr) - 
                                  ((TelleLinje.VVAreKost - TelleLinje.RabKr) / (1 + (ArtPris.Mva%[IF ArtPris.Tilbud THEN 2 ELSE 1] / 100))).
        WHEN 11 /* Internt forbruk */ THEN 
          ASSIGN
            bufTransLogg.Plukket = TRUE /* Skal ikke ut på plukkliste */
            bufTransLogg.Antall = TelleLinje.AntallDiff * -1
            bufTransLogg.Pris   = TelleLinje.VVareKost
            bufTransLogg.RabKr  = 0
            bufTransLogg.Mva    = 0.
      END CASE.
    END. /* LAG_TRANS */
    
  IF AVAILABLE bufTransLogg THEN
    RELEASE bufTransLogg.    
END. /* TRANSLOGG */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

