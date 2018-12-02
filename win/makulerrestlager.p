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

DEF INPUT PARAMETER rArtBasRecid AS RECID NO-UNDO.

DEF VAR lReklamasjonsNr AS DEC NO-UNDO.
DEF VAR iBatchNr        AS INT NO-UNDO.
DEF VAR iCl             AS INT NO-UNDO.
DEFINE VARIABLE bPrButikk AS LOG       NO-UNDO.
DEFINE VARIABLE cTekst    AS CHARACTER NO-UNDO.

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

{syspara.i  5  1 1 iCl INT}
{syspara.i 15 14 1 cTekst}
IF CAN-DO('1,J,Y,Ja,Yes,True',cTekst) 
  THEN bPrButikk = TRUE.
  ELSE bPrButikk = FALSE.  

RUN MakulerRestlager.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-MakulerRestlager) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE MakulerRestlager Procedure 
PROCEDURE MakulerRestlager :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR piTransNr AS INT  NO-UNDO.
  DEF VAR piLinjeNr AS INT  NO-UNDO.
  DEF VAR ocValue   AS CHAR NO-UNDO.
  DEF VAR obOk      AS LOG  NO-UNDO.

  FIND ArtBas NO-LOCK WHERE
      RECID(ArtBas) = rArtBasRecid NO-ERROR.
  IF NOT AVAILABLE ArtBas THEN
      RETURN.

  FIND Butiker NO-LOCK WHERE
      Butiker.Butik = iCl NO-ERROR.
  
  BUTIKKLOOP:    
  FOR EACH Butiker NO-LOCK:  
  
    /* Skal reklamasjon legges opp pr. butikk, må disse variablene nullstilles for hver butikk. */
    IF bPrButikk THEN 
    ASSIGN
      iBatchNr        = 0
      piLinjeNr       = 0
      lReklamasjonsNr = 0
      .  

  /* Leser størrelser */
  RESTLAGER:
  FOR EACH ArtLag NO-LOCK WHERE
      ArtLag.Artikkelnr = ArtBas.artikkelnr AND
      ArtLag.Butik      = Butiker.Butik AND  
      ArtLag.LagAnt > 0:

      FIND ArtPris NO-LOCK WHERE
          ArtPris.ArtikkelNr = ArtBas.ArtikkelNr AND
          ArtPris.ProfilNr   = Butiker.ProfilNr NO-ERROR.

      /* Oppretter batch */
      IF iBatchNr = 0 THEN
          RUN batchlogg.p (PROGRAM-NAME(1),
                           "Lev.rekl. restlager but. " + string(Butiker.Butik) + ' ' + Butiker.ButNamn + ' ' +
                           string(TODAY) +
                           " " +
                           string(TIME,"HH:MM") +
                           " " +
                           USERID("dictdb"),
                           OUTPUT iBatchNr).

      /* Setter transaksjonsnummer  */
      TRANSNRLOOP:
      DO WHILE TRUE:
        IF piTransNr = 0 THEN
          DO:
            FIND LAST TransLogg WHERE
              TransLogg.Butik = ArtLag.Butik
              USE-INDEX TransLogg NO-ERROR.
            IF AVAILABLE TransLogg THEN
              piTransNr = TransLogg.TransNr + 1.
            ELSE
              piTransNr = 1.
          END.
        ELSE
            piTransNr = piTransNr + 1.
            
        /* Sjekker at transnr er ledig. */
        IF CAN-FIND(Translogg WHERE
                    TransLogg.Butik   = ArtLag.butik AND
                    Translogg.TransNr = piTransNr AND
                    TransLogg.SeqNr   = 1) THEN 
          NEXT TRANSNRLOOP.
        ELSE
          LEAVE TRANSNRLOOP.
      END. /* TRANSNRLOOP */
      
      /* Skaper reklamasjonshode. */
      IF lReklamasjonsNr = 0 THEN
          RUN OpprettReklamasjon.
      FIND Reklamasjonslogg NO-LOCK WHERE
              Reklamasjonslogg.ReklamasjonsNr = lReklamasjonsNr NO-ERROR.

      /* Linjeteller */
      IF piLinjeNr = 0 THEN
      DO:
          FIND LAST ReklamasjonsLinje OF Reklamasjonslogg NO-ERROR.
          IF AVAILABLE Reklamasjonslinje THEN
              piLinjeNr = Reklamasjonslinje.LinjeNr + 1.
          ELSE
              piLinjeNr = 1.
      END.
      ELSE
          piLinjeNr = piLinjeNr + 1.

      DO TRANSACTION:
          /* Oppretter TransLogg */
          CREATE TransLogg.
          ASSIGN TransLogg.Butik        = ArtLag.Butik
                 TransLogg.TransNr      = piTransNr
                 TransLogg.SeqNr        = 1
                 .

          ASSIGN
                 TransLogg.BatchNr      = iBatchNr
                 TransLogg.TTId         = 4 /* Lagerreklamasjon */
                 TransLogg.TBId         = 1
                 TransLogg.ArtikkelNr   = IF AVAILABLE ArtBas
                                            THEN ArtBas.ArtikkelNr
                                            ELSE 0
                 TransLogg.Vg           = ArtBas.Vg
                 TransLogg.LopNr        = ArtBas.LopNr
                 TransLogg.Antall       = Artlag.LagAnt
                 TransLogg.Pris         = ArtPris.InnkjopsPris[1]
                 TransLogg.RabKr        = 0
                 TransLogg.KundNr       = Reklamasjonslogg.KundeNr
                 TransLogg.LevNr        = Reklamasjonslogg.LevNr
                 TransLogg.BongId       = 0
                 TransLogg.BongLinjeNr  = 0
                 TransLogg.KassaNr      = 0
                 TransLogg.ForsNr       = 0
                 TransLogg.Plukket      = TRUE 
                 TransLogg.Dato         = TODAY 
                 TransLogg.Tid          = TIME 
                 TransLogg.SelgerNr     = 0
                 TransLogg.BestNr       = 0
                 TransLogg.Postert      = FALSE
                 TransLogg.Mva          = 0
                 TransLogg.Storl        = ArtLag.Storl
                 .

          /* Skaper Reklamasjonslinje */
          CREATE Reklamasjonslinje.
          ASSIGN
              Reklamasjonslinje.ReklamasjonsNr = Reklamasjonslogg.ReklamasjonsNr
              Reklamasjonslinje.LinjeNr        = piLinjeNr
              .
          ASSIGN
              Reklamasjonslinje.Butik          = ArtLag.Butik
              Reklamasjonslinje.ArtikkelNr     = dec(ArtBas.ArtikkelNr)
              Reklamasjonslinje.TTId           = 4
              Reklamasjonslinje.BongId         = 0
              Reklamasjonslinje.BongLinjeNr    = 0
              Reklamasjonslinje.KassaNr        = 0
              Reklamasjonslinje.Storl          = ArtLag.Storl
              Reklamasjonslinje.Dato           = TODAY 
              Reklamasjonslinje.Tid            = TIME
              Reklamasjonslinje.SeqNr          = 0
              Reklamasjonslinje.ForsNr         = 0
              Reklamasjonslinje.SelgerNr       = 0
              Reklamasjonslinje.SolgtIButikk   = 0
              Reklamasjonslinje.SolgtBongId    = 0
              Reklamasjonslinje.SolgtForsNr    = 0
              Reklamasjonslinje.SolgtDato      = ?
              Reklamasjonslinje.FeilKode       = 0
              Reklamasjonslinje.Antall         = ArtLag.LagAnt
              Reklamasjonslinje.Pris           = ArtPris.InnkjopsPris[1]
              Reklamasjonslinje.SubtotalRab    = 0
              Reklamasjonslinje.RabKr          = 0
              Reklamasjonslinje.Mva            = ArtPris.MvaKr[1] /* Regnes senere om for 001 */
              Reklamasjonslinje.VVareKost      = ArtPris.InnkjopsPris[1]
              .
          ASSIGN
              Reklamasjonslinje.ReklamVerdi    = ArtPris.InnkjopsPris[1] * ArtLag.LagAnt
              Reklamasjonslinje.ReklamUtgifter = 0
              Reklamasjonslinje.ReklamTotal    = Reklamasjonslinje.VVarekost * ArtLag.LagAnt
              Reklamasjonslinje.AkseptertVerdi = Reklamasjonslinje.ReklamVerdi
              Reklamasjonslinje.Vg             = ArtBas.Vg
              Reklamasjonslinje.LopNr          = ArtBas.LopNr
              Reklamasjonslinje.Varetekst      = ArtBas.Beskr
              Reklamasjonslinje.LevKod         = ArtBas.LevKod
              Reklamasjonslinje.FeilNotat      = ""
              Reklamasjonslinje.TransNr        = 0
              .
          
          RELEASE TransLogg.
          RELEASE Reklamasjonslinje.
      END. /* TRANSACTION */
  END. /* RESTLAGER */

  /* Setter summer */
  IF lReklamasjonsNr > 0 THEN
      RUN reklamasjonslogg_recalc.p (STRING(lReklamasjonsNr),?,'',OUTPUT ocValue,OUTPUT obOk).

  END. /* BUTIKKLOOP */
  RUN batchstatus.p (iBatchNr, 2).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-OpprettReklamasjon) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OpprettReklamasjon Procedure 
PROCEDURE OpprettReklamasjon :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF BUFFER bufReklamasjonslogg FOR Reklamasjonslogg.
      
  DO FOR bufReklamasjonslogg TRANSACTION:
      FIND LevBAs OF ArtBas NO-LOCK NO-ERROR.

      FIND LAST Reklamasjonslogg NO-ERROR.

      CREATE bufReklamasjonslogg.
      ASSIGN
          bufReklamasjonslogg.ReklamasjonsNr = IF AVAILABLE Reklamasjonslogg
                                                 THEN Reklamasjonslogg.ReklamasjonsNr + 1
                                                 ELSE 1
          .
      ASSIGN
        lReklamasjonsNr                        = bufReklamasjonsLogg.ReklamasjonsNr
        bufReklamasjonslogg.LevNr              = ArtBas.LevNr
        bufReklamasjonslogg.KundeNr            = 0
        bufReklamasjonslogg.BetalesAv          = 2
        bufReklamasjonslogg.ReklamStatus       = 1
        bufReklamasjonslogg.AkseptertKunde     = 4
        bufReklamasjonslogg.AkseptertDato      = TODAY
        bufReklamasjonslogg.AkseptertAv        = USERID("SkoTex")
        bufReklamasjonslogg.AkseptertBesluttet = TRUE
        bufReklamasjonslogg.AkseptertDato      = TODAY
        bufReklamasjonslogg.AkseptertAv        = USERID("SkoTex")
        bufReklamasjonslogg.AkseptertBesluttet = TRUE

        bufReklamasjonslogg.OppdLager           = TRUE
        bufReklamasjonslogg.OppdLagerDato       = TODAY
        bufReklamasjonslogg.OppdLagerAv         = USERID('SkoTex')
        bufReklamasjonslogg.Frist-Dato          = TODAY + 14
        .
      IF bufReklamasjonslogg.KundeNr <> 0 THEN
      DO:
          FIND Kunde NO-LOCK WHERE
              Kunde.KundeNr = bufReklamasjonslogg.KundeNr NO-ERROR.
          IF AVAILABLE Kunde THEN
              ASSIGN
              bufReklamasjonslogg.KundeNavn    = Kunde.Navn
              bufReklamasjonslogg.KundeAdresse = Kunde.Adresse1
              bufReklamasjonslogg.PostNr       = Kunde.PostNr
              bufReklamasjonslogg.KundeTelefon = Kunde.Telefon 
              bufReklamasjonslogg.KundeMobil   = Kunde.MobilTlf 
              bufReklamasjonslogg.KundeE-Mail  = ePostAdresse
              .

      END.
        
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

