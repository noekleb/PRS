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

DEF INPUT PARAMETER iTelleNr    AS INT NO-UNDO.
DEF VAR lFilId      AS DEC NO-UNDO.
DEF VAR lDataSettId AS DEC NO-UNDO.
DEF VAR iButikkNr   AS INT NO-UNDO.
DEF VAR iGruppeNr   AS INT NO-UNDO.
DEF VAR iKasseNr    AS INT NO-UNDO.

def var wEDB-System   as char no-undo.
def var wTabell       as char no-undo.  
def var wCl           as int  no-undo.
DEF VAR wAntLinjer    AS INT NO-UNDO.


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

{syspara.i 1 2 4 wEDB-System}
assign wTabell = "ArtBas".

{syspara.i 5 1 1 wCl INT}
find clButiker no-lock where
  clButiker.Butik = wCl no-error.

FIND TelleHode NO-LOCK WHERE
    TelleHode.TelleNr = iTelleNr NO-ERROR.
IF NOT AVAILABLE TelleHode THEN
    RETURN.

RUN KobleKasse.
IF RETURN-VALUE = "AVBRYT" THEN
    RETURN "** Klarte ikke å koble kasse.".

RUN OpprettFil.

RUN OpprettDatasett.

RUN OpprettBonger.

RUN FerdigDatasett.

RUN FerdigFil.

run OppdatTelleHode (input today). /* Varesalg */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-FerdigDatasett) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE FerdigDatasett Procedure 
PROCEDURE FerdigDatasett :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DO TRANSACTION:
      FIND DataSett EXCLUSIVE-LOCK WHERE
          DataSett.DataSettId = lDataSEttId NO-ERROR.
      IF AVAILABLE DataSEtt THEN
      DO:
          FOR EACH BongHode OF DataSett:
              ASSIGN BongHode.BongStatus = 5.
          END.
          ASSIGN
              DataSett.SettStatus   = 2  /* Ankommet         */
              DataSett.Behandlet    = 3  /* Ferdig oppdatert */
              DataSett.AntallLinjer = wAntLinjer
              .
          RELEASE Datasett.
      END.
      
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-FerdigFil) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE FerdigFil Procedure 
PROCEDURE FerdigFil :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DO TRANSACTION:
      FIND Filer EXCLUSIVE-LOCK WHERE
          Filer.FilId = lFilId NO-ERROR.
      IF AVAILABLE Filer THEN
      DO:
          ASSIGN
              Filer.Innlest       = TRUE
              Filer.InnlestDato   = TODAY
              Filer.InnlestKl     = TIME
              Filer.InnlestAv     = USERID("dictdb")
              Filer.Oppdatert     = TRUE
              Filer.OppdatertDato = TODAY
              Filer.OppdatertKl   = TIME
              Filer.OppdatertAv   = USERID("dictdb")
              .
          RELEASE Filer.
      END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-KobleKasse) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE KobleKasse Procedure 
PROCEDURE KobleKasse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
  
  DEF VAR iButikkNr   AS INT NO-UNDO.
  DEF VAR iGruppeNr   AS INT NO-UNDO.
  DEF VAR iKasseNr    AS INT NO-UNDO.

------------------------------------------------------------------------------*/

  FIND LAST Kasse NO-LOCK WHERE
      Kasse.ButikkNr = INT(TelleHode.ButikkListe) NO-ERROR.
  IF NOT AVAILABLE Kasse THEN
      RETURN "AVBRYT".
  ASSIGN
      iButikkNr = int(TelleHode.ButikkListe)
      iGruppeNr = 1
      iKasseNr  = IF AVAILABLE Kasse
                    THEN Kasse.KasseNr
                    ELSE 0
      .
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-NyFilLogg) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE NyFilLogg Procedure 
PROCEDURE NyFilLogg :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER plFilId LIKE Filer.FilId NO-UNDO.
  DEF INPUT PARAMETER pcTekst AS   CHAR        NO-UNDO.
  IF LENGTH(ENTRY(1,pcTekst,CHR(1))) > 170 THEN
      ASSIGN ENTRY(1,pcTekst,CHR(1)) = SUBSTR(ENTRY(1,pcTekst,CHR(1)),1,170).
  PUBLISH "NyFilLogg" (INPUT plFilId, INPUT pcTekst).

  
  DEF VAR piLinjeNr   AS INT NO-UNDO.
  DEF VAR piGradering AS INT NO-UNDO.

  ASSIGN
      piGradering = 0
      .
  IF NUM-ENTRIES(pcTekst,CHR(1)) >= 2 THEN
    ASSIGN
      piGradering = INT(ENTRY(2,pcTekst,CHR(1)))
      NO-ERROR.
  IF ERROR-STATUS:ERROR THEN
      piGradering = 0.

  DO TRANSACTION:
    FIND LAST FilLogg NO-LOCK WHERE
        FilLogg.FilId = plFilId NO-ERROR.
    IF AVAILABLE FilLogg THEN
        piLinjeNr = FilLogg.LinjeNr + 1.
    ELSE
        piLinjeNr = 1.
    CREATE FilLogg.
    ASSIGN
        FilLogg.FilId     = plFilId
        FilLogg.LinjeNr   = piLinjeNr
        FilLogg.Tekst     = entry(1,pcTekst,CHR(1))
        FilLogg.Gradering = piGradering
        .
    RELEASE FilLogg.
  END.
  
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
  def input parameter wToday as date.
   
  DEF BUFFER bTelleHode FOR TelleHode.

  do TRANSACTION:
    find bTelleHode exclusive-lock where
      rowid(bTelleHode) = rowid(TelleHode).
    assign
      bTelleHode.Oppdatert  = if wToday <> ? 
                                then wToday
                                else bTelleHode.Oppdatert
      bTelleHode.AntallPar  = 0
      bTelleHode.AntallTalt = 0
      bTelleHode.OpptVerdi  = 0
      bTelleHode.VerdiDiff  = 0
      bTelleHode.AntallDiff = 0
      bTelleHode.OpprVerdi  = 0
      bTelleHode.AntLinjer  = 0.
    for each TelleLinje of bTelleHode no-lock:

      assign
        bTelleHode.AntallPar  = bTelleHode.AntallPar  + TelleLinje.AntallPar
        bTelleHode.AntallTalt = bTelleHode.AntallTalt + TelleLinje.AntallTalt
        bTelleHode.OpprVerdi  = bTelleHode.OpprVerdi  + TelleLinje.OpprVerdi      
        bTelleHode.OpptVerdi  = bTelleHode.OpptVerdi  + TelleLinje.OpptVerdi
        bTelleHode.AntLinjer  = bTelleHode.AntLinjer  + 1
        bTelleHode.VerdiDiff  = bTelleHode.OpprVerdi  - bTelleHode.OpptVerdi
        bTelleHode.AntallDiff = bTelleHode.AntallPar  - bTelleHode.AntallTalt.      

    end.
    RELEASE Tellehode.
  end.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-OpprettBonger) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OpprettBonger Procedure 
PROCEDURE OpprettBonger :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR piBongLinje AS INT NO-UNDO.
  DEF VAR piBongNr    AS INT NO-UNDO.
  DEF VAR plVVAreKost AS DEC NO-UNDO.
  DEF VAR plSum       AS DEC NO-UNDO.

  ASSIGN
      piBongNr   = TelleHode.TelleNr
      wAntLinjer = 0
      .

  FIND DataSett NO-LOCK WHERE
      DataSett.DataSettId = lDataSettId NO-ERROR.
  
  /* Renser opp i tidliger gammal dr... */
  /* Delhvis oppdaterte bonghoder.      */
  RENS:
  DO TRANSACTION:
    FOR EACH BongHode OF DataSett EXCLUSIVE-LOCK:
        FOR EACH BongLinje EXCLUSIVE-LOCK WHERE
            BongLinje.B_Id = BongHode.B_Id:
            DELETE BongLinje.
        END.
        DELETE BongHode.
    END.
  END. /* RENS */

  piBongNr = 1.
  BLOKKEN:
  DO:
/*       WHILE TRUE: */
      FIND LAST BongHode NO-LOCK WHERE
          BongHode.ButikkNr = DataSett.ButikkNr AND
          BongHode.GruppeNr = DataSett.GruppeNr AND
          BongHode.KasseNr  = DataSett.KasseNr  AND
          BongHode.Dato     = DataSett.Dato /*  AND
          BongHode.BongNr   = piBongNr */ USE-INDEX Bong NO-ERROR.
      IF AVAILABLE BongHode THEN
          piBongNr = BongHode.BongNr + 1.
/*       ELSE               */
/*           LEAVE BLOKKEN. */
  END. /* BLOKKEN */

  BONGHODE:
  DO TRANSACTION:
      /* Henter kasserer for kassen. */
      FIND FIRST ButikkForsalj NO-LOCK WHERE
          ButikkForsalj.Butik = DataSett.butikkNr NO-ERROR.
      IF AVAILABLE ButikkForsalj THEN
          FIND Forsalj OF ButikkForsalj NO-ERROR.
      CREATE BongHode.
      ASSIGN
        piBongLinje            = 0
        BongHode.ButikkNr      = DataSett.ButikkNr 
        BongHode.GruppeNr      = DataSett.GruppeNr 
        BongHode.KasseNr       = DataSett.KasseNr  
        BongHode.Dato          = DataSett.Dato
        BongHode.BongNr        = piBongNr
        BongHode.BongStatus    = 0 /* Opprettes */
        BongHode.OpdKvit       = TRUE
        Bonghode.DataSettId    = DataSett.DataSettId
        BongHode.Utskriftskopi = "Utskriftskopi ikke mottat for kvittering " + 
                                 STRING(piBongNr) + "."
        BongHode.KassererNr    = IF AVAILABLE ButikkForsalj
                                   THEN ButikkForsalj.ForsNr
                                   ELSE BongHode.KassererNr
        BongHode.KassererNavn  = IF AVAILABLE Forsalj
                                   THEN Forsalj.FoNamn
                                   ELSE BongHode.KassererNavn
        .
      FIND CURRENT BongHode NO-LOCK.
  END. /* BONGHODE */

  TELLELINJE:
  for each TelleLinje of TelleHode exclusive-lock where
    TelleLinje.Oppdatert = FALSE AND
    TelleLinje.AntallTalt <> 0
    break
    by TelleLinje.TelleNr
    by TelleLinje.Vg
    by TelleLinje.LopNr
    By TelleLinje.Butik 
    by TelleLinje.Storl TRANSACTION:

    if first-of(TelleLinje.Butik) then
      find Butiker no-lock where
        Butiker.Butik = TelleLinje.Butik no-error.

    FIND ArtBas NO-LOCK WHERE
        ArtBas.ArtikkelNr = TelleLinje.ArtikkelNr NO-ERROR.
    FIND ArtPris OF ArtBas WHERE
        ArtPris.ProfilNr = Butiker.ProfilNr NO-ERROR.
    IF NOT AVAILABLE ArtPris THEN
        FIND ArtPris OF ArtBas WHERE
            ArtPris.ProfilNr = clButiker.ProfilNr NO-ERROR.
    
    assign
        wAntLinjer           = wAntLinjer + 1
        TelleLinje.Oppdatert = true
        .      

    /* Tar bort tellelås på artikkel/butikk. */
    if last-of(TelleLinje.Butik) then
      do:
        find first KonvReg exclusive-lock where
          KonvReg.EDB-System = wEDB-System and
          KonvReg.Tabell     = wTabell     and
          KonvReg.EkstId     = string(TelleLinje.ArtikkelNr) + "," + 
                               string(TelleLinje.Butik) and
          KonvReg.InterntId  = string(TelleLinje.ArtikkelNr) + "," + 
                               string(TelleLinje.Butik) no-error.
        if available KonvReg then
          delete KonvReg.      
      end.

    BONGLINJE:
    DO:
        /* Henter lager og varekost for butikken */
        find Lager exclusive-lock where
          Lager.ArtikkelNr = TelleLinje.ArtikkelNr and
          Lager.Butik      = BongHode.ButikkNr no-error no-wait.
        if not available Lager then
          do:
            create Lager.
            assign
                Lager.ArtikkelNr = TelleLinje.ArtikkelNr
                Lager.Butik      = BongHode.ButikkNr
                Lager.VVareKost  = IF AVAILABLE ArtPris
                                     THEN ArtPris.Varekost[1]
                                     ELSE 0
                .
          end.
        IF Lager.VVareKost = 0 THEN
            Lager.VVareKost  = IF AVAILABLE ArtPris
                                 THEN ArtPris.Varekost[1]
                                 ELSE 0.

        FIND VarGR NO-LOCK OF ArtBas NO-ERROR.

        DO WHILE TRUE:
            piBongLinje = piBongLinje + 1.
            IF piBongLinje = 68 THEN piBongLinje = piBongLinje + 1.
            FIND BongLinje EXCLUSIVE-LOCK WHERE
              BongLinje.ButikkNr = BongHode.ButikkNr AND
              BongLinje.GruppeNr = BongHode.GruppeNr AND
              BongLinje.KasseNr  = BongHode.KasseNr  AND
              BongLinje.Dato     = BongHode.Dato     AND
              BongLinje.BongNr   = BongHode.BongNr   AND
              BongLinje.LinjeNr  = piBongLinje NO-ERROR.
          IF AVAILABLE BongLinje THEN
          DO:
              piBongLinje = piBongLinje + 1.
              NEXT.
          END.
          ELSE LEAVE.
        END.

        CREATE BongLinje. /* */
        ASSIGN
            piBongLinje            = piBongLinje + 1
            BongLinje.B_Id         = BongHode.B_Id
            BongLinje.ButikkNr     = BongHode.ButikkNr 
            BongLinje.GruppeNr     = BongHode.GruppeNr 
            BongLinje.KasseNr      = BongHode.KasseNr  
            BongLinje.Dato         = BongHode.Dato    
            BongLinje.TransDato    = BongHode.Dato
            BongLinje.TransTid     = BongHode.Tid
            BongLinje.BongNr       = BongHode.BongNr   
            BongLinje.TTId         = IF TelleLinje.AntallTalt < 0
                                       THEN 10  /* Retur    */
                                       ELSE 1  /* Varesalg */
            BongLinje.TBId         = 1
            BongLinje.LinjeNr      = piBongLinje /*BongLinje*/
            NO-ERROR.

        ASSIGN
          BongLinje.ArtikkelNr = string(TelleLinje.ArtikkelNr)
          BongLinje.Strekkode  = TelleLinje.Kode
          BongLinje.VareGr     = TelleLinje.Vg
          BongLinje.LopeNr     = TelleLinje.LopNr
          BongLinje.Storrelse  = TelleLinje.Storl
          BongLinje.BongTekst  = TelleLinje.Beskr
          BongLinje.Antall     = TelleLinje.AntallTalt
          BongLinje.LinjeSum   = abs(TelleLinje.VVareKost * TelleLinje.AntallTalt) /*abs(TelleLinje.OpptVerdi)*/
          BongLinje.BongPris   = abs(BongLinje.LinjeSum)
          BongLinje.VVarekost  = abs(Lager.VVarekost * BongLinje.Antall)
          BongLinje.LinjeRab   = abs(TelleLinje.RabKr  * BongLinje.Antall)
          BongLinje.VareGruppeNavn = IF AVAILABLE VarGr
                                   THEN VarGr.VgBeskr
                                   ELSE ""

          BongLinje.Mva%       = ArtPris.Mva%[1]
          BongLinje.MvaKr      = (BongLinje.LinjeSum - BongLinje.LinjeRab) - ((BongLinje.LinjeSum - BongLinje.LinjeRab) / (1 + (ArtPris.Mva%[1] / 100)))
          BongLinje.FeilKode   = 0
          BongLinje.NotatKode  = 0
          BongLinje.RefNr      = 0
          BongLinje.RefTekst   = ""
          .

       ASSIGN
           plVVareKost = plVVareKost + BongLinje.VVarekost
           plSum       = plSum + (BongLinje.LinjeSum - BongLinje.LinjeRab)
           .

       FIND FIRST Moms NO-LOCK WHERE
         Moms.MomsProc = BongLinje.Mva% NO-ERROR.
       IF AVAILABLE Moms THEN
         ASSIGN
         BongLinje.MvaGr         = Moms.MomsKod
         BongLinje.MvaGruppeNavn = Moms.Beskrivelse
         .
       RELEASE BongLinje.

    END. /* BONGLINJE */

  END. /* TELLELINJE */

  BETALING:
  DO WHILE TRUE TRANSACTION:
      CREATE BongLinje. /* */
      ASSIGN
          piBongLinje            = piBongLinje + 1
          BongLinje.B_Id         = BongHode.B_Id
          BongLinje.ButikkNr     = BongHode.ButikkNr 
          BongLinje.GruppeNr     = BongHode.GruppeNr 
          BongLinje.KasseNr      = BongHode.KasseNr  
          BongLinje.Dato         = BongHode.Dato     
          BongLinje.BongNr       = BongHode.BongNr   
          BongLinje.TTId         = 50 /* Kontant */
          BongLinje.TBId         = 1
          BongLinje.LinjeNr      = piBongLinje /*BongLinje*/
          BongLinje.TransDato    = BongHode.Dato
          BongLinje.TransTid     = BongHode.Tid
          NO-ERROR.
      IF ERROR-STATUS:ERROR THEN 
      DO:
          piBongLinje = piBongLinje + 1.
          NEXT.
      END.

      ASSIGN
          BongLinje.BongTekst  = "KONTANT"
          BongLinje.Antall     = 0
          BongLinje.LinjeSum   = plSum /*plVVareKost*/
          BongLinje.BongPris   = plSum /*plVVareKost*/
          .
      RELEASE BongLinje.

      FIND CURRENT BongHode EXCLUSIVE-LOCK.
      ASSIGN
          BongHode.Belop      = plSum /*plVVareKost*/
          BongHode.BongStatus = 0 /* 5 Oppdatert */

          .
      LEAVE.
  END. /* BETALING */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-OpprettDatasett) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OpprettDatasett Procedure 
PROCEDURE OpprettDatasett :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR piSettNr AS INT NO-UNDO.

OPPRETTDATASETT:
DO TRANSACTION:

  /* Finner neste SettNr */
  FIND LAST Datasett NO-LOCK WHERE
      Datasett.ButikkNr = iButikkNr AND
      Datasett.GruppeNr = iGruppeNr AND
      Datasett.KasseNr  = iKasseNr  AND
      Datasett.Dato     = TelleHode.StartDato AND
      DataSett.FilType  = 1 /* EL-Journal */
      USE-INDEX DataSett NO-ERROR.
  IF AVAILABLE DataSett THEN
      piSettNr = DataSett.SettNr + 1.
  ELSE DO:
      piSettNr = 1.
  END.

  /* Finner neste DataSettId */
  FIND LAST DataSett NO-LOCK
      USE-INDEX DataSettId NO-ERROR.
  IF AVAILABLE DataSett THEN
      lDataSettId = DataSett.DataSettId + 1.
  ELSE
      lDataSettId = 1.

  RELEASE DataSett. /* Ny post skal skapes. */

  IF NOT AVAILABLE DataSett THEN
  DO:
    CREATE DataSett.
    ASSIGN
        DataSett.DataSettId = lDataSettId
        DataSett.SettStatus = 0 /* Innlest */
        DataSett.Behandlet  = 0 /* Delhvis oppdatert */
        .
  END.

  ASSIGN
    DataSett.ButikkNr   = iButikkNr 
    DataSett.GruppeNr   = iGruppeNr
    DataSett.KasseNr    = iKasseNr
    DataSett.Dato       = TelleHode.StartDato
    DataSett.SettNr     = piSettNr
    DataSett.Tid        = 0
    DataSett.FilId      = lFilId
    DataSett.FilType    = 1 /* EL-Journal */
    .
  RELEASE Datasett.
END. /* OPPRETTDATASETT */


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-OpprettFil) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OpprettFil Procedure 
PROCEDURE OpprettFil :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  /* Oppretter posten i filen. */
  IF NOT CAN-FIND(Filer WHERE
                  Filer.FilNavn   = "Salg fra telling " + STRING(iTelleNr) AND
                  Filer.Dato      = TelleHode.StartDato AND
                  Filer.Kl        = STRING(TIME,"HH:MM") AND
                  Filer.Storrelse = 0 AND
                  Filer.Katalog   = "Telleliste"
                 ) THEN
  DO TRANSACTION:
    /* Finner FilId */
    FIND LAST Filer NO-LOCK NO-ERROR.
    IF AVAILABLE Filer THEN
      lFilId = Filer.FilId + 1.
    ELSE
      lFilId = 1.
    CREATE Filer.
    ASSIGN
      Filer.FilId     = lFilId
      Filer.FilNavn   = "Salg fra telling " + STRING(iTelleNr) 
      Filer.Dato      = TelleHode.StartDato 
      Filer.Kl        = STRING(TIME,"HH:MM:SS") 
      Filer.Storrelse = 0 
      Filer.Katalog   = "Telleliste"
      Filer.AntLinjer = 0
      Filer.FilType   = 1 
      .
    RUN NyFilLogg (INPUT lFilId, STRING(TODAY) + " " + 
                          STRING(TIME,"HH:MM:SS")+ " " + userid("skotex") + 
                          " - Funnet på filkatalog ").
    RELEASE Filer.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

