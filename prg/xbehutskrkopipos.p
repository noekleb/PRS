&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        :  xbehutskrkopipos.p
    Purpose     :  

    Syntax      :

    Description :  

    Author(s)   :  Tom Nøkleby
    Created     :  23/10-01
    Notes       :
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEF INPUT  PARAMETER lDataSettId AS DEC    NO-UNDO.
DEF INPUT  PARAMETER h_Parent    AS HANDLE NO-UNDO.
DEF INPUT  PARAMETER h_Logg      AS HANDLE NO-UNDO.
DEF OUTPUT PARAMETER iAntLinjer  AS INT    NO-UNDO.

DEF VAR cError          AS CHAR NO-UNDO.
DEF VAR piLoop1         AS INT  NO-UNDO.

DEF VAR iButikkNr       AS INT  NO-UNDO.
DEF VAR iGruppeNr       AS INT  NO-UNDO.
DEF VAR iKasseNr        AS INT  NO-UNDO.
DEF VAR cBehKvittering  AS CHAR NO-UNDO.
DEF VAR iTotAntLinjer   AS INT  NO-UNDO.
DEF VAR cDatoListe      AS CHAR NO-UNDO.
DEF VAR lFilId          AS CHAR NO-UNDO.
DEF VAR iStart          AS INT  NO-UNDO.
DEF VAR dDato           AS DATE NO-UNDO.
DEF VAR lMedlemsNr      AS CHAR NO-UNDO.

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

FIND DataSett NO-LOCK WHERE
    DataSett.DataSettId = lDataSettId NO-ERROR.
IF NOT AVAILABLE Datasett THEN
    RETURN " ** Ukjent datasett (" + STRING(lDataSettId) + ").".

FIND Filer OF DataSett NO-LOCK NO-ERROR.
IF NOT AVAILABLE Filer THEN
    RETURN " ** Ukjent filkobling på datasett (" + STRING(lDataSettId) + ").".
   
RUN TellOppLinjer.

RUN OppdaterDatasett.

RETURN cError.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-OppdaterDatasett) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OppdaterDatasett Procedure 
PROCEDURE OppdaterDatasett :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR piLinjeNr     AS INT   NO-UNDO.
  DEF VAR piBongNr      AS INT   NO-UNDO.
  DEF VAR pbDobbel      AS LOG   NO-UNDO.
  DEF VAR piBongLinje   AS INT   NO-UNDO.
  DEF VAR piLoop1       AS INT   NO-UNDO.
  DEF VAR prBongRowId   AS ROWID NO-UNDO.
  DEF VAR pcError       AS CHAR  NO-UNDO.
  DEF VAR pcFilError    AS CHAR  NO-UNDO.
  
  ASSIGN
      iAntLinjer  = 0
      iStart      = TIME
      pbDobbel    = FALSE
      prBongRowId = ?
      .

  /* Leser alle fillinjene som hører til datasettet. */
  /* Transaksjon rundt hver enkelt post.             */
  DATASETT:
  FOR EACH FilLinjer OF DataSett EXCLUSIVE-LOCK WHERE
      FilLinjer.Behandlet = FALSE:
    ASSIGN
        iAntLinjer  = iAntLinjer + 1
        iButikkNr   = int(ENTRY(1,FilLinje.Tekst,"|")) 
        iGruppeNr   = int(ENTRY(2,FilLinje.Tekst,"|")) 
        iKasseNr    = int(ENTRY(3,FilLinje.Tekst,"|")) 
        dDato       = DATE(ENTRY(4,FilLinje.Tekst,"|"))
        piBongNr    = int(ENTRY(5,FilLinje.Tekst,"|"))
        lMedlemsNr  = TRIM(ENTRY(6,FilLinje.Tekst,"|"),"0")
        .

    IF iAntLinjer MODULO 25 = 0 THEN
    DO:
      RUN Telleverk IN h_Parent ("Datasett: " + 
                                 STRING(FilLinje.DataSettId) + 
                                 " LinjeNr: " +
                                 STRING(iAntLinjer) +
                                 " av " +
                                 string(iTotAntLinjer) + ".") NO-ERROR.
    END.
    
    /* Henter eller oppretter BongHode.                                  */
    /* Bongen kan eksistere fra før, hvis f.eks utskriftskopi er innlest */ 
    IF NOT can-find(BongHode where
                    BongHode.ButikkNr = iButikkNr and
                    BongHode.GruppeNr = iGruppeNr AND
                    BongHode.KasseNr  = iKasseNr  AND
                    BongHode.Dato     = dDato     AND
                    BOngHode.BongNr   = piBongNr) THEN
    BONGHODE:
    DO:
      
      /* Setter status på siste klargjorte kvittering.                      */
      /* NB: Dette gjøres også utenfor loppen for å få med siste kvittering */
      IF prBongRowId <> ? THEN
      KONVBONG:
      DO:
        {xbehutskrkopipos.i}
      END. /* KONVBONG */

      /* Sjekker 10 dager tilbake i tid for dobbeloppdatering */
      SJEKKDOBBEL:
      DO piLoop1 = 0 TO 9:
          FIND BongHode EXCLUSIVE-LOCK WHERE
              BongHode.ButikkNr = iButikkNr         AND
              BongHode.GruppeNr = iGruppeNr         AND
              BongHode.KasseNr  = iKasseNr          AND
              BongHode.Dato     = (dDato - piLoop1) AND
              BongHode.BongNr   = piBongNr NO-ERROR.
          IF AVAILABLE BongHode THEN
              LEAVE SJEKKDOBBEL.
      END. /* SJEKKDOBBEL */
      
      /* Sjekker og eventuelt flagger dobbelinnlesning */
      IF AVAILABLE BongHode THEN
      DO:
          /* Flagger dobbeloppdatert kvittering */
          IF BongHode.OpdUtskKopi THEN
            ASSIGN
              pbDobbel = TRUE
              .
         
          IF cError = "" THEN
              cError  = cError + 
                       (IF cError = ""
                          THEN ""
                          ELSE "|") +
                       STRING(TODAY) + " " + 
                                STRING(TIME,"HH:MM:SS") + " " + userid("skotex") +
                       " - Datasett " + 
                       string(DataSett.DataSettId) + 
                       " inneholder en eller flere dobbeloppdatering av utskriftskopi på bonger. " + 
                       " Deriblant bongnr: " + STRING(piBongNr) + "." + CHR(1) + "2".
              .
          LEAVE BONGHODE.
      END.
      ELSE DO:

        CREATE BongHode.
        ASSIGN
          piBongLinje            = 1
          BongHode.ButikkNr      = iButikkNr 
          BongHode.GruppeNr      = iGruppeNr 
          BongHode.KasseNr       = iKasseNr  
          BongHode.Dato          = dDato
          BongHode.BongNr        = piBongNr
          BongHode.BongStatus    = 1 /* Ikke klar */
          BongHode.OpdUtskKopi   = TRUE
          Bonghode.DataSettId    = DataSett.DataSettId
          pbDobbel               = FALSE
          prBongRowId            = ROWID(BongHode)
          .
      
      END.
    END. /* BONGHODE */
    /* Henter den eksisterende bongen. */
    ELSE FIND BongHode EXCLUSIVE-LOCK where
              BongHode.ButikkNr = iButikkNr and
              BongHode.GruppeNr = iGruppeNr AND
              BongHode.KasseNr  = iKasseNr  AND
              BongHode.Dato     = dDato     AND
              BOngHode.BongNr   = piBongNr.
              

    /* Skipper behandling av alle linjer på den dobbelinnleste bongen. */
    IF pbDobbel THEN
    DO:
        ASSIGN
            piBongLinje = piBongLinje + 1
            .
        NEXT DATASETT.
    END.

    /*Skriver inn utskriftskopien. */
    ASSIGN
        BongHode.OpdUtskKopi   = TRUE
        BongHode.Utskriftskopi = FilLinje.StorTekst
        BongHode.MedlemsKort   = (IF BongHode.MedlemsKort = ""
                                    THEN ENTRY(6,FilLinje.Tekst,"|")
                                    ELSE BongHode.MedlemsKort)
        FilLinje.Behandlet     = TRUE
        .

  END. /* DATSETT */

  /* Markerer Datasettet som oppdatert */
  DO TRANSACTION:
      IF prBongRowId <> ? THEN
      KONVBLOKK:
      DO:
        {xbehutskrkopipos.i}
      END.

      FIND CURRENT DataSett EXCLUSIVE-LOCK.
      /* Setter behandletstatus */
      IF CAN-FIND(FIRST FilLinje OF DataSett WHERE
                  FilLinje.Behandlet = FALSE) THEN
          DataSett.Behandlet = 2.
      ELSE
          DataSett.Behandlet = 3.
  END.
  FIND CURRENT DataSett NO-LOCK.
  
  RUN Telleverk IN h_Parent (" ") NO-ERROR.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-TellOppLinjer) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE TellOppLinjer Procedure 
PROCEDURE TellOppLinjer :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  ASSIGN
      iTotAntLinjer = 0
      iStart        = TIME
      .
  FOR EACH FilLinjer NO-LOCK WHERE
    FilLinjer.DataSettId = lDataSettId:  
    ASSIGN
        iTotAntLinjer = iTotAntLinjer + 1
        .
  END.

  /*
  ASSIGN
    cError = cError + 
             (IF cError = ""
                THEN ""
                ELSE CHR(1)) +
             STRING(TODAY) + " " + 
             STRING(TIME,"HH:MM:SS") + " " + userid("skotex") + 
             " - Tidsbruk i xbehkvitteringspos.p - TellOppLinjer: " + 
             STRING(TIME - iStart,"HH:MM:SS").
  */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

