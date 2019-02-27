&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
/* Connected Databases 
          data             PROGRESS
*/


/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE TT_ELogg NO-UNDO LIKE ELogg.



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

DEFINE VARIABLE cHKinst AS CHARACTER  NO-UNDO.

DEFINE TEMP-TABLE TT_hgrdag            LIKE hgrdag.
DEFINE TEMP-TABLE TT_timedag           LIKE timedag.
DEFINE TEMP-TABLE TT_varedag           LIKE varedag.
DEFINE TEMP-TABLE TT_StLinje           LIKE StLinje.
DEFINE TEMP-TABLE TT_Gruppe            LIKE Gruppe.
DEFINE TEMP-TABLE TT_Kasse             LIKE Kasse.
DEFINE TEMP-TABLE TT_Forsalj           LIKE Forsalj.
DEFINE TEMP-TABLE TT_Selger            LIKE Selger.
DEFINE TEMP-TABLE TT_akt_rapp          LIKE akt_rapp.
DEFINE TEMP-TABLE TT_dags_rap          LIKE dags_rap.
DEFINE TEMP-TABLE TT_kas_rap           LIKE kas_rap.
DEFINE TEMP-TABLE TT_konto             LIKE konto.
DEFINE TEMP-TABLE TT_Kort_Spes         LIKE Kort_Spes.
DEFINE TEMP-TABLE TT_KassererDag       LIKE KassererDag.
DEFINE TEMP-TABLE TT_KassererBilag     LIKE KassererBilag.
DEFINE TEMP-TABLE TT_KassererKontanter LIKE KassererKontanter.
DEFINE TEMP-TABLE TT_kassererOppgj     LIKE kassererOppgj.
DEFINE TEMP-TABLE TT_KassererValuta    LIKE KassererValuta.
DEFINE TEMP-TABLE TT_BokforingsBilag   LIKE BokforingsBilag.
DEFINE TEMP-TABLE TT_Lager             LIKE Lager.
DEFINE TEMP-TABLE TT_ArtLag            LIKE ArtLag.
DEFINE TEMP-TABLE TT_StLager           LIKE StLager.
DEFINE TEMP-TABLE TT_Translogg         LIKE Translogg.

DEF VAR bStatTilHK      AS LOG  NO-UNDO.
DEF VAR bLagerTilHk     AS LOG  NO-UNDO.
DEF VAR bTransloggTilHk AS LOG  NO-UNDO.
DEF VAR cTekst          AS CHAR NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&IF DEFINED(EXCLUDE-CanFindTTElogg) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD CanFindTTElogg Procedure 
FUNCTION CanFindTTElogg RETURNS LOGICAL
  ( INPUT cTabell AS CHARACTER,INPUT cVerdi AS CHARACTER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-FinnsElogg) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD FinnsElogg Procedure 
FUNCTION FinnsElogg RETURNS LOGICAL
  ( INPUT cTabellNavn AS CHARACTER )  FORWARD.

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
   Temp-Tables and Buffers:
      TABLE: TT_ELogg T "?" NO-UNDO data ELogg
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Procedure ASSIGN
         HEIGHT             = 27.76
         WIDTH              = 63.6.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

  {syspara.i 1 1 18 cHKinst}
  IF NOT CAN-DO("no,FALSE,0",cHKinst) THEN
      RETURN.
  IF NOT CAN-FIND(FIRST ELogg WHERE ELogg.EksterntSystem = "HK") THEN
      RETURN "AVBRYT".

  /* Skal det sendes artikkelstatistikk? */
  {syspara.i 3 4 1 cTekst}
  IF (cTekst = "1" OR cTekst = "") THEN
      bStatTilHK = TRUE.
  ELSE
      bStatTilHK = FALSE.
  /* Skal det sendes lagerinformasjon? */
  {syspara.i 3 4 2 cTekst}
  IF (cTekst = "1" OR cTekst = "") THEN
      bLagerTilHk = TRUE.
  ELSE
      bLagerTilHk = FALSE.
  /* Skal det sendes translogg? */
  {syspara.i 3 4 2 cTekst}
  IF (cTekst = "1" OR cTekst = "") THEN
      bTransloggTilHk = TRUE.
  ELSE
      bTransloggTilHk = FALSE.
      
  /* Nær vi har ELoggposter som skall øver sænder vi før sakerhets skull    */
  /* altid alla gruppe, kassa, forsalj och selger . Vi har ingen ændringstrigger på */
  /* dem då de endast ær intressanta att få øver vid annan datatransfer     */
  /* Dærfør skapar vi "ALLA"-poster før dessa register och tar sedan hand om dem  */
  /* med vanlig hantering */
  /*
  RUN NyELogg("Gruppe","ALLE").
  RUN NyELogg("Kasse","ALLE").
  RUN NyELogg("Forsalj","ALLE").
  RUN NyELogg("Selger","ALLE").
  */
  /* Tar med alle StLager postene */
  /*RUN NyELogg("StLager","ALLE").*/
  
  RUN StartEksport.
  RUN SlettBehandlet.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-CreateELoggLager) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CreateELoggLager Procedure 
PROCEDURE CreateELoggLager :
/*------------------------------------------------------------------------------
  Purpose:     Skapar ELogg-poster från de StLinje "ARTIKKEL"
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cKey        AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cDataObjekt AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cButik      AS CHARACTER  NO-UNDO.
    &SCOPED-DEFINE KeyFelt  StTypeId
    &SCOPED-DEFINE KeyFelt2 PerId
    &SCOPED-DEFINE KeyFelt3 DataObjekt
    
    ASSIGN cKey = "ARTIKKEL" + CHR(2) + "AAR".
    FOR EACH TT_ELogg WHERE TT_ELogg.TabellNavn     = "StLinje" AND
                            TT_ELogg.EksterntSystem = "HK"      AND
                            TT_ELogg.Verdier        BEGINS cKey:

        ASSIGN cDataObjekt = ENTRY(3,TT_ELogg.Verdier,CHR(2))
               cButik      = ENTRY(5,TT_ELogg.Verdier,CHR(2)).
        /* Logger artikkelens lagerposter */
        IF bStatTilHK AND bLagerTilHk THEN
        ARTIKKEL:
        DO:
            IF NOT CAN-FIND(ELogg WHERE ELogg.TabellNavn     = "Lager" AND
                            ELogg.EksterntSystem = "HK"    AND
                            ELogg.Verdier        = cDataObjekt + CHR(1) + cButik) THEN
            DO:
                CREATE Elogg.
                ASSIGN ELogg.EksterntSystem = "HK"
                       ELogg.TabellNavn     = "Lager"
                       ELogg.Verdier        = cDataObjekt + CHR(1) + cButik
                       ELogg.EndringsType   = 1
                       ELogg.Behandlet      = FALSE.
            END.
            /* Legger ut ale størrelser for artikkelen. */
            FOR EACH ArtLag NO-LOCK WHERE
                ArtLag.ArtikkelNr = DEC(cDataObjekt) AND
                ArtLag.Butik      = INT(cButik):
                IF NOT CAN-FIND(ELogg WHERE ELogg.TabellNavn     = "ArtLag" AND
                                ELogg.EksterntSystem = "HK"    AND
                                ELogg.Verdier        = cDataObjekt + CHR(1) + 
                                                       cButik + CHR(1) +
                                                       STRING(ArtLag.StrKode)) THEN
                DO:
                    CREATE Elogg.
                    ASSIGN ELogg.EksterntSystem = "HK"
                           ELogg.TabellNavn     = "ArtLag"
                           ELogg.Verdier        = cDataObjekt  + CHR(1) + 
                                                  cButik + CHR(1) +
                                                  STRING(ArtLag.StrKode)
                           ELogg.EndringsType   = 1
                           ELogg.Behandlet      = FALSE.
                END.
            END.
        END. /* ARTIKKEL */
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-KopierArtLagElogg) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE KopierArtLagElogg Procedure 
PROCEDURE KopierArtLagElogg :
/*------------------------------------------------------------------------------
  Purpose:     Skapar TT_ELogg utifrån ELogg "ArtLag"
               Här måste vi göra en special eftersom ELoggposten bara pekar på
               ArtikkelNr Butik medan ArtLag också har storlekar.
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE OUTPUT PARAMETER iAntNyEndre AS INTEGER    NO-UNDO.
  DEFINE        VARIABLE  cTabellNavn AS CHARACTER  INIT "ArtLag" NO-UNDO.
  FOR EACH ELogg WHERE ELogg.TabellNavn = cTabellNavn AND ELogg.EksterntSystem = "HK":
      FIND FIRST ArtLag WHERE ArtLag.ArtikkelNr = DECI(ENTRY(1,ELogg.Verdier,CHR(1))) AND
                              ArtLag.Butik      = INT(ENTRY(2,ELogg.Verdier,CHR(1)))  NO-LOCK NO-ERROR.
      IF AVAIL ArtLag THEN DO:
          FOR EACH Artlag WHERE ArtLag.ArtikkelNr = DECI(ENTRY(1,ELogg.Verdier,CHR(1))) AND
                                ArtLag.Butik      = INT(ENTRY(2,ELogg.Verdier,CHR(1)))  NO-LOCK.
              CREATE TT_Elogg.
              BUFFER-COPY Elogg EXCEPT Verdier TO TT_Elogg
                  ASSIGN TT_ELogg.Verdier = ELogg.Verdier + CHR(1) + STRING(ArtLag.StrKode).
              ASSIGN iAntNyEndre = iAntNyEndre + 1.
          END.
          RELEASE TT_Elogg.
      END.
      ASSIGN Elogg.Behandlet = TRUE.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-KopierElogg) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE KopierElogg Procedure 
PROCEDURE KopierElogg :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER iMaks       AS INTEGER NO-UNDO.  
  DEFINE INPUT  PARAMETER cTabellNavn AS CHARACTER  NO-UNDO.
  DEFINE OUTPUT PARAMETER iAntSlett   AS INTEGER    NO-UNDO.
  DEFINE OUTPUT PARAMETER iAntNyEndre AS INTEGER    NO-UNDO.
  
  DEFINE VARIABLE iMaksAntPrGang AS INTEGER NO-UNDO.

  DEFINE BUFFER bufELogg FOR ELogg.

  /* For å unngå å skape for store pos filer, tar vi dem i små porsjoner.         */
  /* Ref. eksport av alle Antons statistikker som selvfølgelig feilet ved import på hk. */
  ELOGGLOOP:
  FOR EACH bufELogg NO-LOCK WHERE 
      bufELogg.TabellNavn     = cTabellNavn AND 
      bufELogg.EksterntSystem = "HK" AND 
      bufELogg.Behandlet      = FALSE:

      IF iMaks > 0 THEN DO:
          iMaksAntPrGang = iMaksAntPrGang + 1.
          IF iMaksAntPrGang > 1000 THEN 
              LEAVE ELOGGLOOP.
      END.

      FIND ELogg EXCLUSIVE-LOCK WHERE
        RECID(ELogg) = RECId(bufELogg) NO-ERROR NO-WAIT.
      IF NOT AVAILABLE ELogg OR LOCKED ELogg THEN 
        NEXT ELOGGLOOP.

      CREATE TT_Elogg.
      BUFFER-COPY Elogg TO TT_Elogg.
      RELEASE TT_Elogg.
      ASSIGN Elogg.Behandlet = TRUE
             iAntSlett   = iAntSlett   + IF Elogg.EndringsType = 3 THEN 1 ELSE 0
             iAntNyEndre = iAntNyEndre + IF Elogg.EndringsType = 1 AND ELogg.Verdier <> "ALLE" THEN 1 ELSE 0.
  END. /* ELOGGLOOP */
  
  FIND FIRST TT_ELogg WHERE TT_ELogg.TabellNavn = cTabellNavn AND 
                              TT_ELogg.EksterntSystem = "HK" AND
                              TT_ELogg.Verdier = "ALLE" NO-ERROR.
  IF AVAIL TT_ELogg THEN DO:
      DELETE TT_ELogg.
      RUN SkapaTTELoggAlle(INPUT-OUTPUT iAntNyEndre,cTabellNavn).
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-NyELogg) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE NyELogg Procedure 
PROCEDURE NyELogg :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER cTabell AS CHARACTER  NO-UNDO.
    DEFINE INPUT  PARAMETER cVerdi  AS CHARACTER  NO-UNDO.
    IF NOT FinnsELogg(cTabell) THEN DO:
        CREATE Elogg.
        ASSIGN ELogg.TabellNavn     = cTabell
               ELogg.EksterntSystem = "HK"   
               ELogg.Verdier        = cVerdi
               ELogg.EndringsType   = 1
               ELogg.Behandlet      = FALSE.
        RELEASE ELogg.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-NyTTElogg) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE NyTTElogg Procedure 
PROCEDURE NyTTElogg :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER cTabell AS CHARACTER  NO-UNDO.
    DEFINE INPUT  PARAMETER cVerdi  AS CHARACTER  NO-UNDO.
    CREATE TT_Elogg.
    ASSIGN TT_ELogg.TabellNavn     = cTabell
           TT_ELogg.EksterntSystem = "HK"   
           TT_ELogg.Verdier        = cVerdi
           TT_ELogg.EndringsType   = 1
           TT_ELogg.Behandlet      = FALSE.
    RELEASE TT_ELogg.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SendAkt_Rapp) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SendAkt_Rapp Procedure 
PROCEDURE SendAkt_Rapp :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    &SCOPED-DEFINE Tabell   akt_rapp
    &SCOPED-DEFINE KeyFelt  dato
    &SCOPED-DEFINE KeyFelt2 butik
    &SCOPED-DEFINE KeyFelt3 kasse
    &SCOPED-DEFINE KeyFelt4 tid
    
    DEFINE VARIABLE iAntSlett   AS INTEGER    NO-UNDO.
    DEFINE VARIABLE iAntNyEndre AS INTEGER    NO-UNDO.
    DEFINE VARIABLE cTabellNavn AS CHARACTER  INIT "{&Tabell}" NO-UNDO.
    
    EMPTY TEMP-TABLE TT_{&Tabell}.
    RUN KopierElogg (0,cTabellNavn,OUTPUT iAntSlett,OUTPUT iAntNyEndre).
    
    iAntSlett = 0.
    FOR EACH TT_ELogg WHERE TT_ELogg.TabellNavn = cTabellNavn AND TT_ELogg.EksterntSystem = "HK"
                        AND TT_Elogg.EndringsType = 3:
        iAntSlett = iAntSlett + 1.
    END.
        
    IF iAntSlett > 0 THEN DO:
        EXPORT "H" cTabellNavn 3 "1.0" iAntSlett.
        CREATE TT_{&Tabell}.
        FOR EACH TT_ELogg WHERE TT_ELogg.TabellNavn = cTabellNavn AND TT_ELogg.EksterntSystem = "HK"
                            AND TT_Elogg.EndringsType = 3:
            ASSIGN TT_{&Tabell}.{&KeyFelt}  = DATE(ENTRY(1,TT_Elogg.Verdier,CHR(1)))
                   TT_{&Tabell}.{&KeyFelt2} = INT(ENTRY(2,TT_Elogg.Verdier,CHR(1)))
                   TT_{&Tabell}.{&KeyFelt3} = INT(ENTRY(3,TT_Elogg.Verdier,CHR(1)))
                   TT_{&Tabell}.{&KeyFelt4} = INT(ENTRY(4,TT_Elogg.Verdier,CHR(1))).
            EXPORT TT_{&Tabell}.dato       
                   TT_{&Tabell}.uke_dag    
                   TT_{&Tabell}.uke_nr     
                   TT_{&Tabell}.mnd        
                   TT_{&Tabell}.butik      
                   TT_{&Tabell}.kasse      
                   TT_{&Tabell}.tid        
                   TT_{&Tabell}.tid_txt    
                   TT_{&Tabell}.oms_ant    
                   TT_{&Tabell}.oms_verd   
                   TT_{&Tabell}.ant_kunder 
                   TT_{&Tabell}.svk        
                   TT_{&Tabell}.ant_ret    
                   TT_{&Tabell}.verd_ret   
                   TT_{&Tabell}.ant_kvitto.
        END.
    END.
    
    iAntNyEndre = 0.
    FOR EACH TT_ELogg WHERE TT_ELogg.TabellNavn = cTabellNavn AND TT_ELogg.EksterntSystem = "HK"
                        AND TT_Elogg.EndringsType = 1:
        iAntNyEndre = iAntNyEndre + 1.                
    END.
    
    IF iAntNyEndre > 0 THEN DO:
        EXPORT "H" cTabellNavn 1 "1.0" iAntNyEndre.
        FOR EACH TT_ELogg WHERE TT_ELogg.TabellNavn = cTabellNavn AND TT_ELogg.EksterntSystem = "HK"
                            AND TT_Elogg.EndringsType = 1:
            FIND {&Tabell} WHERE {&Tabell}.{&KeyFelt}  = DATE(ENTRY(1,TT_Elogg.Verdier,CHR(1))) AND 
                                 {&Tabell}.{&KeyFelt2} = INT(ENTRY(2,TT_Elogg.Verdier,CHR(1))) AND
                                 {&Tabell}.{&KeyFelt3} = INT(ENTRY(3,TT_Elogg.Verdier,CHR(1))) AND
                                 {&Tabell}.{&KeyFelt4} = INT(ENTRY(4,TT_Elogg.Verdier,CHR(1))) NO-LOCK NO-ERROR.
            IF AVAIL {&Tabell} THEN
                EXPORT {&Tabell}.dato       
                       {&Tabell}.uke_dag    
                       {&Tabell}.uke_nr     
                       {&Tabell}.mnd        
                       {&Tabell}.butik      
                       {&Tabell}.kasse      
                       {&Tabell}.tid        
                       {&Tabell}.tid_txt    
                       {&Tabell}.oms_ant    
                       {&Tabell}.oms_verd   
                       {&Tabell}.ant_kunder 
                       {&Tabell}.svk        
                       {&Tabell}.ant_ret    
                       {&Tabell}.verd_ret   
                       {&Tabell}.ant_kvitto.
        END.
    END.
    EMPTY TEMP-TABLE TT_{&Tabell}.
    &UNDEFINE Tabell
    &UNDEFINE KeyFelt
    &UNDEFINE KeyFelt2
    &UNDEFINE KeyFelt3
    &UNDEFINE KeyFelt4
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SendArtLag) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SendArtLag Procedure 
PROCEDURE SendArtLag :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    &SCOPED-DEFINE Tabell   ArtLag
    &SCOPED-DEFINE KeyFelt  ArtikkelNr
    &SCOPED-DEFINE KeyFelt2 Butik
    &SCOPED-DEFINE KeyFelt3 StrKode
    
    DEFINE VARIABLE iAntNyEndre AS INTEGER    NO-UNDO.
    DEFINE VARIABLE cTabellNavn AS CHARACTER  INIT "{&Tabell}" NO-UNDO.
    
    EMPTY TEMP-TABLE TT_{&Tabell}.
    RUN KopierArtLagElogg (OUTPUT iAntNyEndre).

    iAntNyEndre = 0.
    FOR EACH TT_ELogg WHERE TT_ELogg.TabellNavn = cTabellNavn AND TT_ELogg.EksterntSystem = "HK"
                        AND TT_Elogg.EndringsType = 1:
        iAntNyEndre = iAntNyEndre + 1.
    END.
    
    IF iAntNyEndre > 0 THEN DO:
        EXPORT "H" cTabellNavn 1 "1.0" iAntNyEndre.
        FOR EACH TT_ELogg WHERE TT_ELogg.TabellNavn = cTabellNavn AND TT_ELogg.EksterntSystem = "HK"
                            AND TT_Elogg.EndringsType = 1:
            FIND {&Tabell} WHERE {&Tabell}.{&KeyFelt}  = INT(ENTRY(1,TT_Elogg.Verdier,CHR(1))) AND 
                                 {&Tabell}.{&KeyFelt2} = INT(ENTRY(2,TT_Elogg.Verdier,CHR(1))) AND
                                 {&Tabell}.{&KeyFelt3} = INT(ENTRY(3,TT_Elogg.Verdier,CHR(1))) NO-LOCK NO-ERROR.
            IF AVAIL {&Tabell} THEN
                EXPORT {&Tabell}.vg         
                       {&Tabell}.lopnr      
                       {&Tabell}.storl      
                       {&Tabell}.butik      
                       {&Tabell}.lagant     
                       {&Tabell}.retant     
                       {&Tabell}.Lager      
                       {&Tabell}.ArtikkelNr 
                       {&Tabell}.AntSolgt   
                       {&Tabell}.BrekkAnt   
                       {&Tabell}.IntAnt     
                       {&Tabell}.ReklAnt    
                       {&Tabell}.ReklLAnt   
                       {&Tabell}.GjenkjopAnt
                       {&Tabell}.RetLAnt    
                       {&Tabell}.KjopAnt    
                       {&Tabell}.OvAnt      
                       {&Tabell}.JustAnt    
                       {&Tabell}.JustVerdi  
                       {&Tabell}.SvinnAnt   
                       {&Tabell}.SvinnVerdi 
                       {&Tabell}.NedAnt     
                       {&Tabell}.NedVerdi   
                       {&Tabell}.AntRab     
                       {&Tabell}.StrKode.                       
        END.
    END.
    EMPTY TEMP-TABLE TT_{&Tabell}.
    &UNDEFINE Tabell
    &UNDEFINE KeyFelt
    &UNDEFINE KeyFelt2
    &UNDEFINE KeyFelt3
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SendBokforingsBilag) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SendBokforingsBilag Procedure 
PROCEDURE SendBokforingsBilag :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    &SCOPED-DEFINE Tabell   BokforingsBilag
    &SCOPED-DEFINE KeyFelt  ButikkNr
    &SCOPED-DEFINE KeyFelt2 Aar
    &SCOPED-DEFINE KeyFelt3 BokforingsNr
    
    DEFINE VARIABLE iAntSlett   AS INTEGER    NO-UNDO.
    DEFINE VARIABLE iAntNyEndre AS INTEGER    NO-UNDO.
    DEFINE VARIABLE cTabellNavn AS CHARACTER  INIT "{&Tabell}" NO-UNDO.
    
    EMPTY TEMP-TABLE TT_{&Tabell}.
    RUN KopierElogg (0,cTabellNavn,OUTPUT iAntSlett,OUTPUT iAntNyEndre).

    iAntSlett = 0.
    FOR EACH TT_ELogg WHERE TT_ELogg.TabellNavn = cTabellNavn AND TT_ELogg.EksterntSystem = "HK"
                        AND TT_Elogg.EndringsType = 3:
        iAntSlett = iAntSlett + 1.
    END.
        
    IF iAntSlett > 0 THEN DO:
        EXPORT "H" cTabellNavn 3 "1.0" iAntSlett.
        CREATE TT_{&Tabell}.
        FOR EACH TT_ELogg WHERE TT_ELogg.TabellNavn = cTabellNavn AND TT_ELogg.EksterntSystem = "HK"
                            AND TT_Elogg.EndringsType = 3:
            ASSIGN TT_{&Tabell}.{&KeyFelt}  = INT(ENTRY(1,TT_Elogg.Verdier,CHR(1)))
                   TT_{&Tabell}.{&KeyFelt2} = INT(ENTRY(2,TT_Elogg.Verdier,CHR(1)))
                   TT_{&Tabell}.{&KeyFelt3} = INT(ENTRY(3,TT_Elogg.Verdier,CHR(1))).
            EXPORT TT_{&Tabell}.ButikkNr      
                   TT_{&Tabell}.BokforingsNr  
                   TT_{&Tabell}.EDato         
                   TT_{&Tabell}.ETid          
                   TT_{&Tabell}.BrukerID      
                   TT_{&Tabell}.RegistrertDato
                   TT_{&Tabell}.RegistrertTid 
                   TT_{&Tabell}.RegistrertAv  
                   TT_{&Tabell}.SendtDato     
                   TT_{&Tabell}.SendAv        
                   TT_{&Tabell}.SendtTid      
                   TT_{&Tabell}.OmsetningsDato
                   TT_{&Tabell}.SendtRegnskap 
                   TT_{&Tabell}.Aar           
                   TT_{&Tabell}.GodkjentDato  
                   TT_{&Tabell}.GodkjentTid   
                   TT_{&Tabell}.GodkjentAv    
                   TT_{&Tabell}.GodkjentFlagg.
        END.
    END.
    
    iAntNyEndre = 0.
    FOR EACH TT_ELogg WHERE TT_ELogg.TabellNavn = cTabellNavn AND TT_ELogg.EksterntSystem = "HK"
                        AND TT_Elogg.EndringsType = 1 BY verdier:
        iAntNyEndre = iAntNyEndre + 1.
    END.
    
    IF iAntNyEndre > 0 THEN DO:
        EXPORT "H" cTabellNavn 1 "1.0" iAntNyEndre.
        FOR EACH TT_ELogg WHERE TT_ELogg.TabellNavn = cTabellNavn AND TT_ELogg.EksterntSystem = "HK"
                            AND TT_Elogg.EndringsType = 1 BY verdier:
            FIND {&Tabell} WHERE {&Tabell}.{&KeyFelt}  = INT(ENTRY(1,TT_Elogg.Verdier,CHR(1))) AND 
                                 {&Tabell}.{&KeyFelt2} = INT(ENTRY(2,TT_Elogg.Verdier,CHR(1))) AND
                                 {&Tabell}.{&KeyFelt3} = INT(ENTRY(3,TT_Elogg.Verdier,CHR(1))) NO-LOCK NO-ERROR.
            IF AVAIL {&Tabell} THEN
                EXPORT {&Tabell}.ButikkNr      
                       {&Tabell}.BokforingsNr  
                       {&Tabell}.EDato         
                       {&Tabell}.ETid          
                       {&Tabell}.BrukerID      
                       {&Tabell}.RegistrertDato
                       {&Tabell}.RegistrertTid 
                       {&Tabell}.RegistrertAv  
                       {&Tabell}.SendtDato     
                       {&Tabell}.SendAv        
                       {&Tabell}.SendtTid      
                       {&Tabell}.OmsetningsDato
                       {&Tabell}.SendtRegnskap 
                       {&Tabell}.Aar           
                       {&Tabell}.GodkjentDato  
                       {&Tabell}.GodkjentTid   
                       {&Tabell}.GodkjentAv    
                       {&Tabell}.GodkjentFlagg.
        END.
    END.
    EMPTY TEMP-TABLE TT_{&Tabell}.
    &UNDEFINE Tabell
    &UNDEFINE KeyFelt
    &UNDEFINE KeyFelt2
    &UNDEFINE KeyFelt3
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SendDags_Rap) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SendDags_Rap Procedure 
PROCEDURE SendDags_Rap :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    &SCOPED-DEFINE Tabell   dags_rap
    &SCOPED-DEFINE KeyFelt  butikk
    &SCOPED-DEFINE KeyFelt2 dato
    DEFINE VARIABLE iAntSlett   AS INTEGER    NO-UNDO.
    DEFINE VARIABLE iAntNyEndre AS INTEGER    NO-UNDO.
    DEFINE VARIABLE cTabellNavn AS CHARACTER  INIT "{&Tabell}" NO-UNDO.
    
    EMPTY TEMP-TABLE TT_{&Tabell}.
    RUN KopierElogg (0,cTabellNavn,OUTPUT iAntSlett,OUTPUT iAntNyEndre).
    
    iAntSlett = 0.
    FOR EACH TT_ELogg WHERE TT_ELogg.TabellNavn = cTabellNavn AND TT_ELogg.EksterntSystem = "HK"
                        AND TT_Elogg.EndringsType = 3:
        iAntSlett = iAntSlett + 1.
    END.
    
    IF iAntSlett > 0 THEN DO:
        EXPORT "H" cTabellNavn 3 "1.0" iAntSlett.
        CREATE TT_{&Tabell}.
        FOR EACH TT_ELogg WHERE TT_ELogg.TabellNavn = cTabellNavn AND TT_ELogg.EksterntSystem = "HK"
                            AND TT_Elogg.EndringsType = 3:
            ASSIGN TT_{&Tabell}.{&KeyFelt}  = INT(ENTRY(1,TT_Elogg.Verdier,CHR(1)))
                   TT_{&Tabell}.{&KeyFelt2} = DATE(ENTRY(2,TT_Elogg.Verdier,CHR(1))).
            EXPORT TT_{&Tabell}.butikk    
                   TT_{&Tabell}.dato      
                   TT_{&Tabell}.mnd       
                   TT_{&Tabell}.hg1_oms   
                   TT_{&Tabell}.hg2_oms   
                   TT_{&Tabell}.hg3_oms   
                   TT_{&Tabell}.hg4_oms   
                   TT_{&Tabell}.hg5_oms   
                   TT_{&Tabell}.hg6_oms   
                   TT_{&Tabell}.retur_korr
                   TT_{&Tabell}.tb1       
                   TT_{&Tabell}.tb2       
                   TT_{&Tabell}.tb3       
                   TT_{&Tabell}.tb4       
                   TT_{&Tabell}.tb5       
                   TT_{&Tabell}.tb6       
                   TT_{&Tabell}.aar       
                   TT_{&Tabell}.MvaVerdi  
                   TT_{&Tabell}.hg7_oms   
                   TT_{&Tabell}.hg8_oms   
                   TT_{&Tabell}.hg9_oms   
                   TT_{&Tabell}.hg10_oms  
                   TT_{&Tabell}.tb7       
                   TT_{&Tabell}.tb8       
                   TT_{&Tabell}.tb9       
                   TT_{&Tabell}.tb10.      
        END.
    END.
    
    iAntNyEndre = 0.
    FOR EACH TT_ELogg WHERE TT_ELogg.TabellNavn = cTabellNavn AND TT_ELogg.EksterntSystem = "HK"
                        AND TT_Elogg.EndringsType = 1:
        iAntNyEndre = iAntNyEndre + 1.
    END.
    
    IF iAntNyEndre > 0 THEN DO:
        EXPORT "H" cTabellNavn 1 "1.0" iAntNyEndre.
        FOR EACH TT_ELogg WHERE TT_ELogg.TabellNavn = cTabellNavn AND TT_ELogg.EksterntSystem = "HK"
                            AND TT_Elogg.EndringsType = 1:
            FIND {&Tabell} WHERE {&Tabell}.{&KeyFelt}  = INT(ENTRY(1,TT_Elogg.Verdier,CHR(1))) AND 
                                 {&Tabell}.{&KeyFelt2} = DATE(ENTRY(2,TT_Elogg.Verdier,CHR(1))) NO-LOCK NO-ERROR.
            IF AVAIL {&Tabell} THEN
                EXPORT {&Tabell}.butikk    
                       {&Tabell}.dato      
                       {&Tabell}.mnd       
                       {&Tabell}.hg1_oms   
                       {&Tabell}.hg2_oms   
                       {&Tabell}.hg3_oms   
                       {&Tabell}.hg4_oms   
                       {&Tabell}.hg5_oms   
                       {&Tabell}.hg6_oms   
                       {&Tabell}.retur_korr
                       {&Tabell}.tb1       
                       {&Tabell}.tb2       
                       {&Tabell}.tb3       
                       {&Tabell}.tb4       
                       {&Tabell}.tb5       
                       {&Tabell}.tb6       
                       {&Tabell}.aar       
                       {&Tabell}.MvaVerdi  
                       {&Tabell}.hg7_oms   
                       {&Tabell}.hg8_oms   
                       {&Tabell}.hg9_oms   
                       {&Tabell}.hg10_oms  
                       {&Tabell}.tb7       
                       {&Tabell}.tb8       
                       {&Tabell}.tb9       
                       {&Tabell}.tb10.      
        END.
    END.
    EMPTY TEMP-TABLE TT_{&Tabell}.
    &UNDEFINE Tabell
    &UNDEFINE KeyFelt
    &UNDEFINE KeyFelt2
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SendForsalj) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SendForsalj Procedure 
PROCEDURE SendForsalj :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    &SCOPED-DEFINE Tabell   Forsalj
    &SCOPED-DEFINE KeyFelt  ForsNr
    
    DEFINE VARIABLE iAntSlett   AS INTEGER    NO-UNDO.
    DEFINE VARIABLE iAntNyEndre AS INTEGER    NO-UNDO.
    DEFINE VARIABLE cTabellNavn AS CHARACTER  INIT "{&Tabell}" NO-UNDO.
    
    EMPTY TEMP-TABLE TT_{&Tabell}.
    RUN KopierElogg (0,cTabellNavn,OUTPUT iAntSlett,OUTPUT iAntNyEndre).

    iAntSlett = 0.
    FOR EACH TT_ELogg WHERE TT_ELogg.TabellNavn = cTabellNavn AND TT_ELogg.EksterntSystem = "HK"
                        AND TT_Elogg.EndringsType = 3:
        iAntSlett = iAntSlett + 1.
    END.
        
    IF iAntSlett > 0 THEN DO:
        EXPORT "H" cTabellNavn 3 "1.0" iAntSlett.
        CREATE TT_{&Tabell}.
        FOR EACH TT_ELogg WHERE TT_ELogg.TabellNavn = cTabellNavn AND TT_ELogg.EksterntSystem = "HK"
                            AND TT_Elogg.EndringsType = 3:
            ASSIGN TT_{&Tabell}.{&KeyFelt}  = INT(ENTRY(1,TT_Elogg.Verdier,CHR(1))).
            EXPORT TT_{&Tabell}.ForsNr         
                   TT_{&Tabell}.FoAnstNr       
                   TT_{&Tabell}.FoNamn         
                   TT_{&Tabell}.FoAdr          
                   TT_{&Tabell}.FoPoNr         
                   TT_{&Tabell}.FoPadr         
                   TT_{&Tabell}.FoTel          
                   TT_{&Tabell}.FoPersNr       
                   TT_{&Tabell}.LevNr          
                   TT_{&Tabell}.EDato          
                   TT_{&Tabell}.ETid           
                   TT_{&Tabell}.BrukerID       
                   TT_{&Tabell}.RegistrertDato 
                   TT_{&Tabell}.RegistrertTid  
                   TT_{&Tabell}.RegistrertAv   
                   TT_{&Tabell}.AnsattNr       
                   TT_{&Tabell}.Rabatt         
                   TT_{&Tabell}.Prisendring    
                   TT_{&Tabell}.Retur          
                   TT_{&Tabell}.slettTidligere
                   TT_{&Tabell}.SlettBong      
                   TT_{&Tabell}.SletteForste   
                   TT_{&Tabell}.FodtDato       
                   TT_{&Tabell}.navnikasse     
                   TT_{&Tabell}.passord        
                   TT_{&Tabell}.ForsaljAktiv
                   TT_{&Tabell}.ButikkNr
                .  
        END.
    END.
    
    iAntNyEndre = 0.
    FOR EACH TT_ELogg WHERE TT_ELogg.TabellNavn = cTabellNavn AND TT_ELogg.EksterntSystem = "HK"
                        AND TT_Elogg.EndringsType = 1:
        iAntNyEndre = iAntNyEndre + 1.
    END.
    
    IF iAntNyEndre > 0 THEN DO:
        EXPORT "H" cTabellNavn 1 "1.0" iAntNyEndre.
        FOR EACH TT_ELogg WHERE TT_ELogg.TabellNavn = cTabellNavn AND TT_ELogg.EksterntSystem = "HK"
                            AND TT_Elogg.EndringsType = 1:
            FIND {&Tabell} WHERE {&Tabell}.{&KeyFelt}  = INT(ENTRY(1,TT_Elogg.Verdier,CHR(1))) NO-LOCK NO-ERROR.
            IF AVAIL {&Tabell} THEN
                EXPORT {&Tabell}.ForsNr         
                       {&Tabell}.FoAnstNr       
                       {&Tabell}.FoNamn         
                       {&Tabell}.FoAdr          
                       {&Tabell}.FoPoNr         
                       {&Tabell}.FoPadr         
                       {&Tabell}.FoTel          
                       {&Tabell}.FoPersNr       
                       {&Tabell}.LevNr          
                       {&Tabell}.EDato          
                       {&Tabell}.ETid           
                       {&Tabell}.BrukerID       
                       {&Tabell}.RegistrertDato 
                       {&Tabell}.RegistrertTid  
                       {&Tabell}.RegistrertAv   
                       {&Tabell}.AnsattNr       
                       {&Tabell}.Rabatt         
                       {&Tabell}.Prisendring    
                       {&Tabell}.Retur          
                       {&Tabell}.slettTidligere
                       {&Tabell}.SlettBong      
                       {&Tabell}.SletteForste   
                       {&Tabell}.FodtDato       
                       {&Tabell}.navnikasse     
                       {&Tabell}.passord        
                       {&Tabell}.ForsaljAktiv
                       {&Tabell}.ButikkNr.  
        END.
    END.
    EMPTY TEMP-TABLE TT_{&Tabell}.
    &UNDEFINE Tabell
    &UNDEFINE KeyFelt
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SendGruppe) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SendGruppe Procedure 
PROCEDURE SendGruppe :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    &SCOPED-DEFINE Tabell   Gruppe
    &SCOPED-DEFINE KeyFelt  ButikkNr
    &SCOPED-DEFINE KeyFelt2 GruppeNr
    DEFINE VARIABLE iAntSlett   AS INTEGER    NO-UNDO.
    DEFINE VARIABLE iAntNyEndre AS INTEGER    NO-UNDO.
    DEFINE VARIABLE cTabellNavn AS CHARACTER  INIT "{&Tabell}" NO-UNDO.
    EMPTY TEMP-TABLE TT_{&Tabell}.
    RUN KopierElogg (0,cTabellNavn,OUTPUT iAntSlett,OUTPUT iAntNyEndre).
    IF iAntSlett > 0 THEN DO:
        EXPORT "H" cTabellNavn 3 "1.0" iAntSlett.
        CREATE TT_{&Tabell}.
        FOR EACH TT_ELogg WHERE TT_ELogg.TabellNavn = cTabellNavn AND TT_ELogg.EksterntSystem = "HK"
                            AND TT_Elogg.EndringsType = 3:
            ASSIGN TT_{&Tabell}.{&KeyFelt}  = INT(ENTRY(1,TT_Elogg.Verdier,CHR(1)))
                   TT_{&Tabell}.{&KeyFelt2} = INT(ENTRY(2,TT_Elogg.Verdier,CHR(1))).
            EXPORT TT_{&Tabell}.ButikkNr      
                   TT_{&Tabell}.GruppeNr      
                   TT_{&Tabell}.Navn          
                   TT_{&Tabell}.EDato         
                   TT_{&Tabell}.ETid          
                   TT_{&Tabell}.BrukerId      
                   TT_{&Tabell}.RegistrertDato
                   TT_{&Tabell}.RegistrertTid 
                   TT_{&Tabell}.RegistrertAv.
        END.
    END.
    IF iAntNyEndre > 0 THEN DO:
        EXPORT "H" cTabellNavn 1 "1.0" iAntNyEndre.
        FOR EACH TT_ELogg WHERE TT_ELogg.TabellNavn = cTabellNavn AND TT_ELogg.EksterntSystem = "HK"
                            AND TT_Elogg.EndringsType = 1:
            FIND {&Tabell} WHERE {&Tabell}.{&KeyFelt}  = INT(ENTRY(1,TT_Elogg.Verdier,CHR(1))) AND 
                                 {&Tabell}.{&KeyFelt2} = INT(ENTRY(2,TT_Elogg.Verdier,CHR(1))) NO-LOCK NO-ERROR.
            IF AVAIL {&Tabell} THEN
                EXPORT {&Tabell}.ButikkNr      
                       {&Tabell}.GruppeNr      
                       {&Tabell}.Navn          
                       {&Tabell}.EDato         
                       {&Tabell}.ETid          
                       {&Tabell}.BrukerId      
                       {&Tabell}.RegistrertDato
                       {&Tabell}.RegistrertTid 
                       {&Tabell}.RegistrertAv.
        END.
    END.
    EMPTY TEMP-TABLE TT_{&Tabell}.
    &UNDEFINE Tabell
    &UNDEFINE KeyFelt
    &UNDEFINE KeyFelt2
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SendHgrDag) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SendHgrDag Procedure 
PROCEDURE SendHgrDag :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    &SCOPED-DEFINE Tabell   hgrdag
    &SCOPED-DEFINE KeyFelt  butnr
    &SCOPED-DEFINE KeyFelt2 hg
    &SCOPED-DEFINE KeyFelt3 dato
    
    DEFINE VARIABLE iAntSlett   AS INTEGER    NO-UNDO.
    DEFINE VARIABLE iAntNyEndre AS INTEGER    NO-UNDO.
    DEFINE VARIABLE cTabellNavn AS CHARACTER  INIT "{&Tabell}" NO-UNDO.
    
    EMPTY TEMP-TABLE TT_{&Tabell}.
    RUN KopierElogg (0,cTabellNavn,OUTPUT iAntSlett,OUTPUT iAntNyEndre).
    
    iAntSlett = 0.
    FOR EACH TT_ELogg WHERE TT_ELogg.TabellNavn = cTabellNavn AND TT_ELogg.EksterntSystem = "HK"
                        AND TT_Elogg.EndringsType = 3:
        iAntSlett = iAntSlett + 1.
    END.    
    
    IF iAntSlett > 0 THEN DO:
        EXPORT "H" cTabellNavn 3 "1.0" iAntSlett.
        CREATE TT_{&Tabell}.
        FOR EACH TT_ELogg WHERE TT_ELogg.TabellNavn = cTabellNavn AND TT_ELogg.EksterntSystem = "HK"
                            AND TT_Elogg.EndringsType = 3:
            ASSIGN TT_{&Tabell}.{&KeyFelt}  = INT(ENTRY(1,TT_Elogg.Verdier,CHR(1)))
                   TT_{&Tabell}.{&KeyFelt2} = INT(ENTRY(2,TT_Elogg.Verdier,CHR(1)))
                   TT_{&Tabell}.{&KeyFelt3} = DATE(ENTRY(3,TT_Elogg.Verdier,CHR(1))).
            EXPORT TT_{&Tabell}.butnr   
                   TT_{&Tabell}.hgr     
                   TT_{&Tabell}.dato    
                   TT_{&Tabell}.kostpris
                   TT_{&Tabell}.mvakr   
                   TT_{&Tabell}.salgssum
                   TT_{&Tabell}.kostkamp
                   TT_{&Tabell}.mvakamp 
                   TT_{&Tabell}.salgkamp
                   TT_{&Tabell}.kostmix 
                   TT_{&Tabell}.mvamix  
                   TT_{&Tabell}.salgmix 
                   TT_{&Tabell}.kostmed 
                   TT_{&Tabell}.mvamed  
                   TT_{&Tabell}.salgmed 
                   TT_{&Tabell}.medrabkr
                   TT_{&Tabell}.kunrabkr
                   TT_{&Tabell}.perrabkr
                   TT_{&Tabell}.genrabkr.
        END.
    END.
    
    iAntNyEndre = 0.
    FOR EACH TT_ELogg WHERE TT_ELogg.TabellNavn = cTabellNavn AND TT_ELogg.EksterntSystem = "HK"
                        AND TT_Elogg.EndringsType = 1:
        iAntNyEndre = iAntNyEndre + 1.
    END.
    
    IF iAntNyEndre > 0 THEN DO:
        EXPORT "H" cTabellNavn 1 "1.0" iAntNyEndre.
        FOR EACH TT_ELogg WHERE TT_ELogg.TabellNavn = cTabellNavn AND TT_ELogg.EksterntSystem = "HK"
                            AND TT_Elogg.EndringsType = 1:
            FIND {&Tabell} WHERE {&Tabell}.{&KeyFelt}  = INT(ENTRY(1,TT_Elogg.Verdier,CHR(1))) AND 
                                 {&Tabell}.{&KeyFelt2} = INT(ENTRY(2,TT_Elogg.Verdier,CHR(1))) AND
                                 {&Tabell}.{&KeyFelt3} = DATE(ENTRY(3,TT_Elogg.Verdier,CHR(1))) NO-LOCK NO-ERROR.
            IF AVAIL {&Tabell} THEN
                EXPORT {&Tabell}.butnr   
                       {&Tabell}.hgr     
                       {&Tabell}.dato    
                       {&Tabell}.kostpris
                       {&Tabell}.mvakr   
                       {&Tabell}.salgssum
                       {&Tabell}.kostkamp
                       {&Tabell}.mvakamp 
                       {&Tabell}.salgkamp
                       {&Tabell}.kostmix 
                       {&Tabell}.mvamix  
                       {&Tabell}.salgmix 
                       {&Tabell}.kostmed 
                       {&Tabell}.mvamed  
                       {&Tabell}.salgmed 
                       {&Tabell}.medrabkr
                       {&Tabell}.kunrabkr
                       {&Tabell}.perrabkr
                       {&Tabell}.genrabkr.
        END.
    END.
    EMPTY TEMP-TABLE TT_{&Tabell}.
    &UNDEFINE Tabell
    &UNDEFINE KeyFelt
    &UNDEFINE KeyFelt2
    &UNDEFINE KeyFelt3
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SendKasse) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SendKasse Procedure 
PROCEDURE SendKasse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    &SCOPED-DEFINE Tabell   Kasse
    &SCOPED-DEFINE KeyFelt  ButikkNr
    &SCOPED-DEFINE KeyFelt2 GruppeNr
    &SCOPED-DEFINE KeyFelt3 KasseNr
    
    DEFINE VARIABLE iAntSlett   AS INTEGER    NO-UNDO.
    DEFINE VARIABLE iAntNyEndre AS INTEGER    NO-UNDO.
    DEFINE VARIABLE cTabellNavn AS CHARACTER  INIT "{&Tabell}" NO-UNDO.
    
    EMPTY TEMP-TABLE TT_{&Tabell}.
    RUN KopierElogg (0,cTabellNavn,OUTPUT iAntSlett,OUTPUT iAntNyEndre).
    
    iAntSlett = 0.
    FOR EACH TT_ELogg WHERE TT_ELogg.TabellNavn = cTabellNavn AND TT_ELogg.EksterntSystem = "HK"
                        AND TT_Elogg.EndringsType = 3:
        iAntSlett = iAntSlett + 1.
    END.
    
    IF iAntSlett > 0 THEN DO:
        EXPORT "H" cTabellNavn 3 "1.0" iAntSlett.
        CREATE TT_{&Tabell}.
        FOR EACH TT_ELogg WHERE TT_ELogg.TabellNavn = cTabellNavn AND TT_ELogg.EksterntSystem = "HK"
                            AND TT_Elogg.EndringsType = 3:
            ASSIGN TT_{&Tabell}.{&KeyFelt}  = INT(ENTRY(1,TT_Elogg.Verdier,CHR(1)))
                   TT_{&Tabell}.{&KeyFelt2} = INT(ENTRY(2,TT_Elogg.Verdier,CHR(1)))
                   TT_{&Tabell}.{&KeyFelt3} = INT(ENTRY(3,TT_Elogg.Verdier,CHR(1))).
            EXPORT TT_{&Tabell}.ButikkNr
                   TT_{&Tabell}.GruppeNr
                   TT_{&Tabell}.EDato
                   TT_{&Tabell}.ETid
                   TT_{&Tabell}.BrukerId
                   TT_{&Tabell}.Navn
                   TT_{&Tabell}.KasseNr
                   TT_{&Tabell}.LayoutNr
                   TT_{&Tabell}.TekstHode[ 1]
                   TT_{&Tabell}.TekstHode[ 2]
                   TT_{&Tabell}.TekstHode[ 3]
                   TT_{&Tabell}.TekstHode[ 4]
                   TT_{&Tabell}.TekstHode[ 5]
                   TT_{&Tabell}.TekstHode[ 6]
                   TT_{&Tabell}.TekstHode[ 7]
                   TT_{&Tabell}.TekstHode[ 8]
                   TT_{&Tabell}.TekstHode[ 9]
                   TT_{&Tabell}.TekstHode[10]
                   TT_{&Tabell}.TekstSlutt[ 1]
                   TT_{&Tabell}.TekstSlutt[ 2]
                   TT_{&Tabell}.TekstSlutt[ 3]
                   TT_{&Tabell}.TekstSlutt[ 4]
                   TT_{&Tabell}.TekstSlutt[ 5]
                   TT_{&Tabell}.TekstSlutt[ 6]
                   TT_{&Tabell}.TekstSlutt[ 7]
                   TT_{&Tabell}.TekstSlutt[ 8]
                   TT_{&Tabell}.TekstSlutt[ 9]
                   TT_{&Tabell}.TekstSlutt[10]
                   TT_{&Tabell}.TekstHStil[ 1]
                   TT_{&Tabell}.TekstHStil[ 2]
                   TT_{&Tabell}.TekstHStil[ 3]
                   TT_{&Tabell}.TekstHStil[ 4]
                   TT_{&Tabell}.TekstHStil[ 5]
                   TT_{&Tabell}.TekstHStil[ 6]
                   TT_{&Tabell}.TekstHStil[ 7]
                   TT_{&Tabell}.TekstHStil[ 8]
                   TT_{&Tabell}.TekstHStil[ 9]
                   TT_{&Tabell}.TekstHStil[10]
                   TT_{&Tabell}.TekstSStil[ 1]
                   TT_{&Tabell}.TekstSStil[ 2]
                   TT_{&Tabell}.TekstSStil[ 3]
                   TT_{&Tabell}.TekstSStil[ 4]
                   TT_{&Tabell}.TekstSStil[ 5]
                   TT_{&Tabell}.TekstSStil[ 6]
                   TT_{&Tabell}.TekstSStil[ 7]
                   TT_{&Tabell}.TekstSStil[ 8]
                   TT_{&Tabell}.TekstSStil[ 9]
                   TT_{&Tabell}.TekstSStil[10]
                   TT_{&Tabell}.RegistrertDato
                   TT_{&Tabell}.RegistrertTid
                   TT_{&Tabell}.RegistrertAv
                   TT_{&Tabell}.Aktiv
                   TT_{&Tabell}.ElJournal[1]
                   TT_{&Tabell}.ElJournal[2]
                   TT_{&Tabell}.Kvittering[1]
                   TT_{&Tabell}.Kvittering[2]
                   TT_{&Tabell}.Utskriftskopi[1]
                   TT_{&Tabell}.Utskriftskopi[2]
                   TT_{&Tabell}.KassererOpgj[1]
                   TT_{&Tabell}.KassererOpgj[2]
                   TT_{&Tabell}.DagsOpgj[1]
                   TT_{&Tabell}.DagsOpgj[2]
                   TT_{&Tabell}.ElJournalId
                   TT_{&Tabell}.KvitteringId
                   TT_{&Tabell}.UtskriftsKopiId
                   TT_{&Tabell}.KassererOppgjId
                   TT_{&Tabell}.DagsOppgj
                   TT_{&Tabell}.ElJournalAktiv
                   TT_{&Tabell}.KvitteringAktiv
                   TT_{&Tabell}.UtskriftskopiAktiv
                   TT_{&Tabell}.KassererOppgjAktiv
                   TT_{&Tabell}.DagsOppgjAktiv
                   TT_{&Tabell}.ElJournalKatalog
                   TT_{&Tabell}.KvitteringKatalog
                   TT_{&Tabell}.UtskriftskopiKatalog
                   TT_{&Tabell}.KassererOppgjKatalog
                   TT_{&Tabell}.DagsOppgjKatalog
                   TT_{&Tabell}.ElJournalKonv
                   TT_{&Tabell}.KvitteringKonv
                   TT_{&Tabell}.UTskriftskopiKonv
                   TT_{&Tabell}.KassererOppgjKonv
                   TT_{&Tabell}.DagsOppgjKonv
                   TT_{&Tabell}.DagsOppgjId
                   TT_{&Tabell}.ElJournalOperand
                   TT_{&Tabell}.KvitteringOperand
                   TT_{&Tabell}.UtskriftsKopiOperand
                   TT_{&Tabell}.KassererOppgjOperand
                   TT_{&Tabell}.DagsOppgjOperand
                   TT_{&Tabell}.ElJournalInnles
                   TT_{&Tabell}.KvitteringInnles
                   TT_{&Tabell}.UtskriftskopiInnles
                   TT_{&Tabell}.KassererOppgjInnles
                   TT_{&Tabell}.DagsOppgjInnles
                   TT_{&Tabell}.ElJournalBehandle
                   TT_{&Tabell}.KvitteringBehandle
                   TT_{&Tabell}.UtskriftskopiBehandle
                   TT_{&Tabell}.KassererOppgjBehandle
                   TT_{&Tabell}.DagsOppgjBehandle
                   TT_{&Tabell}.ModellNr.
        END.
    END.
    
    iAntNyEndre = 0.
    FOR EACH TT_ELogg WHERE TT_ELogg.TabellNavn = cTabellNavn AND TT_ELogg.EksterntSystem = "HK"
                        AND TT_Elogg.EndringsType = 1:
        iAntNyEndre = iAntNyEndre + 1.
    END.
    
    IF iAntNyEndre > 0 THEN DO:
        EXPORT "H" cTabellNavn 1 "1.0" iAntNyEndre.
        FOR EACH TT_ELogg WHERE TT_ELogg.TabellNavn = cTabellNavn AND TT_ELogg.EksterntSystem = "HK"
                            AND TT_Elogg.EndringsType = 1:
            FIND {&Tabell} WHERE {&Tabell}.{&KeyFelt}  = INT(ENTRY(1,TT_Elogg.Verdier,CHR(1))) AND 
                                 {&Tabell}.{&KeyFelt2} = INT(ENTRY(2,TT_Elogg.Verdier,CHR(1))) AND
                                 {&Tabell}.{&KeyFelt3} = INT(ENTRY(3,TT_Elogg.Verdier,CHR(1))) NO-LOCK NO-ERROR.
            IF AVAIL {&Tabell} THEN
                EXPORT {&Tabell}.ButikkNr
                       {&Tabell}.GruppeNr
                       {&Tabell}.EDato
                       {&Tabell}.ETid
                       {&Tabell}.BrukerId
                       {&Tabell}.Navn
                       {&Tabell}.KasseNr
                       {&Tabell}.LayoutNr
                       {&Tabell}.TekstHode[ 1]
                       {&Tabell}.TekstHode[ 2]
                       {&Tabell}.TekstHode[ 3]
                       {&Tabell}.TekstHode[ 4]
                       {&Tabell}.TekstHode[ 5]
                       {&Tabell}.TekstHode[ 6]
                       {&Tabell}.TekstHode[ 7]
                       {&Tabell}.TekstHode[ 8]
                       {&Tabell}.TekstHode[ 9]
                       {&Tabell}.TekstHode[10]
                       {&Tabell}.TekstSlutt[ 1]
                       {&Tabell}.TekstSlutt[ 2]
                       {&Tabell}.TekstSlutt[ 3]
                       {&Tabell}.TekstSlutt[ 4]
                       {&Tabell}.TekstSlutt[ 5]
                       {&Tabell}.TekstSlutt[ 6]
                       {&Tabell}.TekstSlutt[ 7]
                       {&Tabell}.TekstSlutt[ 8]
                       {&Tabell}.TekstSlutt[ 9]
                       {&Tabell}.TekstSlutt[10]
                       {&Tabell}.TekstHStil[ 1]
                       {&Tabell}.TekstHStil[ 2]
                       {&Tabell}.TekstHStil[ 3]
                       {&Tabell}.TekstHStil[ 4]
                       {&Tabell}.TekstHStil[ 5]
                       {&Tabell}.TekstHStil[ 6]
                       {&Tabell}.TekstHStil[ 7]
                       {&Tabell}.TekstHStil[ 8]
                       {&Tabell}.TekstHStil[ 9]
                       {&Tabell}.TekstHStil[10]
                       {&Tabell}.TekstSStil[ 1]
                       {&Tabell}.TekstSStil[ 2]
                       {&Tabell}.TekstSStil[ 3]
                       {&Tabell}.TekstSStil[ 4]
                       {&Tabell}.TekstSStil[ 5]
                       {&Tabell}.TekstSStil[ 6]
                       {&Tabell}.TekstSStil[ 7]
                       {&Tabell}.TekstSStil[ 8]
                       {&Tabell}.TekstSStil[ 9]
                       {&Tabell}.TekstSStil[10]
                       {&Tabell}.RegistrertDato
                       {&Tabell}.RegistrertTid
                       {&Tabell}.RegistrertAv
                       {&Tabell}.Aktiv
                       {&Tabell}.ElJournal[1]
                       {&Tabell}.ElJournal[2]
                       {&Tabell}.Kvittering[1]
                       {&Tabell}.Kvittering[2]
                       {&Tabell}.Utskriftskopi[1]
                       {&Tabell}.Utskriftskopi[2]
                       {&Tabell}.KassererOpgj[1]
                       {&Tabell}.KassererOpgj[2]
                       {&Tabell}.DagsOpgj[1]
                       {&Tabell}.DagsOpgj[2]
                       {&Tabell}.ElJournalId
                       {&Tabell}.KvitteringId
                       {&Tabell}.UtskriftsKopiId
                       {&Tabell}.KassererOppgjId
                       {&Tabell}.DagsOppgj
                       {&Tabell}.ElJournalAktiv
                       {&Tabell}.KvitteringAktiv
                       {&Tabell}.UtskriftskopiAktiv
                       {&Tabell}.KassererOppgjAktiv
                       {&Tabell}.DagsOppgjAktiv
                       {&Tabell}.ElJournalKatalog
                       {&Tabell}.KvitteringKatalog
                       {&Tabell}.UtskriftskopiKatalog
                       {&Tabell}.KassererOppgjKatalog
                       {&Tabell}.DagsOppgjKatalog
                       {&Tabell}.ElJournalKonv
                       {&Tabell}.KvitteringKonv
                       {&Tabell}.UTskriftskopiKonv
                       {&Tabell}.KassererOppgjKonv
                       {&Tabell}.DagsOppgjKonv
                       {&Tabell}.DagsOppgjId
                       {&Tabell}.ElJournalOperand
                       {&Tabell}.KvitteringOperand
                       {&Tabell}.UtskriftsKopiOperand
                       {&Tabell}.KassererOppgjOperand
                       {&Tabell}.DagsOppgjOperand
                       {&Tabell}.ElJournalInnles
                       {&Tabell}.KvitteringInnles
                       {&Tabell}.UtskriftskopiInnles
                       {&Tabell}.KassererOppgjInnles
                       {&Tabell}.DagsOppgjInnles
                       {&Tabell}.ElJournalBehandle
                       {&Tabell}.KvitteringBehandle
                       {&Tabell}.UtskriftskopiBehandle
                       {&Tabell}.KassererOppgjBehandle
                       {&Tabell}.DagsOppgjBehandle
                       {&Tabell}.ModellNr.
        END.
    END.
    EMPTY TEMP-TABLE TT_{&Tabell}.
    &UNDEFINE Tabell
    &UNDEFINE KeyFelt
    &UNDEFINE KeyFelt2
    &UNDEFINE KeyFelt3
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SendKassererBilag) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SendKassererBilag Procedure 
PROCEDURE SendKassererBilag :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    &SCOPED-DEFINE Tabell   KassererBilag
    &SCOPED-DEFINE KeyFelt  ButikkNr
    &SCOPED-DEFINE KeyFelt2 Dato
    &SCOPED-DEFINE KeyFelt3 KassererNr
    &SCOPED-DEFINE KeyFelt4 z_nummer
    &SCOPED-DEFINE KeyFelt5 BilagsNr
    
    DEFINE VARIABLE iAntSlett   AS INTEGER    NO-UNDO.
    DEFINE VARIABLE iAntNyEndre AS INTEGER    NO-UNDO.
    DEFINE VARIABLE cTabellNavn AS CHARACTER  INIT "{&Tabell}" NO-UNDO.
    
    EMPTY TEMP-TABLE TT_{&Tabell}.
    RUN KopierElogg (0,cTabellNavn,OUTPUT iAntSlett,OUTPUT iAntNyEndre).
    
    iAntSlett = 0.
    FOR EACH TT_ELogg WHERE TT_ELogg.TabellNavn = cTabellNavn AND TT_ELogg.EksterntSystem = "HK"
                        AND TT_Elogg.EndringsType = 3:
        iAntSlett = iAntSlett + 1.
    END.
        
    IF iAntSlett > 0 THEN DO:
        EXPORT "H" cTabellNavn 3 "1.0" iAntSlett.
        CREATE TT_{&Tabell}.
        FOR EACH TT_ELogg WHERE TT_ELogg.TabellNavn = cTabellNavn AND TT_ELogg.EksterntSystem = "HK"
                            AND TT_Elogg.EndringsType = 3:
            ASSIGN TT_{&Tabell}.{&KeyFelt}  = INT(ENTRY(1,TT_Elogg.Verdier,CHR(1)))
                   TT_{&Tabell}.{&KeyFelt2} = DATE(ENTRY(2,TT_Elogg.Verdier,CHR(1)))
                   TT_{&Tabell}.{&KeyFelt3} = INT(ENTRY(3,TT_Elogg.Verdier,CHR(1)))
                   TT_{&Tabell}.{&KeyFelt4} = INT(ENTRY(4,TT_Elogg.Verdier,CHR(1)))
                   TT_{&Tabell}.{&KeyFelt5} = INT(ENTRY(5,TT_Elogg.Verdier,CHR(1))).
            EXPORT TT_{&Tabell}.ButikkNr
                   TT_{&Tabell}.Dato
                   TT_{&Tabell}.KassererNr
                   TT_{&Tabell}.z_nummer
                   TT_{&Tabell}.EDato
                   TT_{&Tabell}.ETid
                   TT_{&Tabell}.BrukerID
                   TT_{&Tabell}.RegistrertDato
                   TT_{&Tabell}.RegistrertTid
                   TT_{&Tabell}.RegistrertAv
                   TT_{&Tabell}.BilagsNr
                   TT_{&Tabell}.Meknad
                   TT_{&Tabell}.Belop. 
        END.
    END.
    
    iAntNyEndre = 0.
    FOR EACH TT_ELogg WHERE TT_ELogg.TabellNavn = cTabellNavn AND TT_ELogg.EksterntSystem = "HK"
                        AND TT_Elogg.EndringsType = 1:
        iAntNyEndre = iAntNyEndre + 1.
    END.
        
    IF iAntNyEndre > 0 THEN DO:
        EXPORT "H" cTabellNavn 1 "1.0" iAntNyEndre.
        FOR EACH TT_ELogg WHERE TT_ELogg.TabellNavn = cTabellNavn AND TT_ELogg.EksterntSystem = "HK"
                            AND TT_Elogg.EndringsType = 1:
            FIND {&Tabell} WHERE {&Tabell}.{&KeyFelt}  = INT(ENTRY(1,TT_Elogg.Verdier,CHR(1))) AND 
                                 {&Tabell}.{&KeyFelt2} = DATE(ENTRY(2,TT_Elogg.Verdier,CHR(1))) AND
                                 {&Tabell}.{&KeyFelt3} = INT(ENTRY(3,TT_Elogg.Verdier,CHR(1))) AND
                                 {&Tabell}.{&KeyFelt4} = INT(ENTRY(4,TT_Elogg.Verdier,CHR(1))) AND
                                 {&Tabell}.{&KeyFelt5} = INT(ENTRY(5,TT_Elogg.Verdier,CHR(1))) NO-LOCK NO-ERROR.
            IF AVAIL {&Tabell} THEN
                EXPORT {&Tabell}.ButikkNr
                       {&Tabell}.Dato
                       {&Tabell}.KassererNr
                       {&Tabell}.z_nummer
                       {&Tabell}.EDato
                       {&Tabell}.ETid
                       {&Tabell}.BrukerID
                       {&Tabell}.RegistrertDato
                       {&Tabell}.RegistrertTid
                       {&Tabell}.RegistrertAv
                       {&Tabell}.BilagsNr
                       {&Tabell}.Meknad
                       {&Tabell}.Belop. 
        END.
    END.
    EMPTY TEMP-TABLE TT_{&Tabell}.
    &UNDEFINE Tabell
    &UNDEFINE KeyFelt
    &UNDEFINE KeyFelt2
    &UNDEFINE KeyFelt3
    &UNDEFINE KeyFelt4
    &UNDEFINE KeyFelt5
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SendKassererDag) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SendKassererDag Procedure 
PROCEDURE SendKassererDag :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/* TVEKSAM */
/*     &SCOPED-DEFINE Tabell   kassererDag                                                                          */
/*     &SCOPED-DEFINE KeyFelt  ButikkNr                                                                             */
/*     &SCOPED-DEFINE KeyFelt2 Dato                                                                                 */
/*     &SCOPED-DEFINE KeyFelt3 KassererNr                                                                           */
/*     &SCOPED-DEFINE KeyFelt4 z_nummer                                                                             */
/*     DEFINE VARIABLE iAntSlett   AS INTEGER    NO-UNDO.                                                           */
/*     DEFINE VARIABLE iAntNyEndre AS INTEGER    NO-UNDO.                                                           */
/*     DEFINE VARIABLE cTabellNavn AS CHARACTER  INIT "{&Tabell}" NO-UNDO.                                          */
/*     EMPTY TEMP-TABLE TT_{&Tabell}.                                                                               */
/*     RUN KopierElogg (0,cTabellNavn,OUTPUT iAntSlett,OUTPUT iAntNyEndre).                                           */
/*     /* vi gør inget med eventuella sletteposter */                                                               */
/* /*     IF iAntSlett > 0 THEN DO:                                                                        */       */
/* /*         EXPORT "H" cTabellNavn 3 "1.0" iAntSlett.                                                    */       */
/* /*         CREATE TT_{&Tabell}.                                                                         */       */
/* /*         FOR EACH TT_ELogg WHERE TT_ELogg.TabellNavn = cTabellNavn AND TT_ELogg.EksterntSystem = "HK" */       */
/* /*                             AND TT_Elogg.EndringsType = 3:                                           */       */
/* /*             ASSIGN TT_{&Tabell}.{&KeyFelt}  = INT(ENTRY(1,TT_Elogg.Verdier,CHR(1)))                  */       */
/* /*                    TT_{&Tabell}.{&KeyFelt2} = DATE(ENTRY(2,TT_Elogg.Verdier,CHR(1)))                 */       */
/* /*                    TT_{&Tabell}.{&KeyFelt3} = INT(ENTRY(3,TT_Elogg.Verdier,CHR(1)))                  */       */
/* /*                    TT_{&Tabell}.{&KeyFelt4} = INT(ENTRY(4,TT_Elogg.Verdier,CHR(1))).                 */       */
/* /*             EXPORT ...                                                                               */       */
/* /*             END.                                                                                     */       */
/* /*     END.                                                                                             */       */
/*     IF iAntNyEndre > 0 THEN DO:                                                                                  */
/*         EXPORT "H" cTabellNavn 1 "1.0" iAntNyEndre.                                                              */
/*         FOR EACH TT_ELogg WHERE TT_ELogg.TabellNavn = cTabellNavn AND TT_ELogg.EksterntSystem = "HK"             */
/*                             AND TT_Elogg.EndringsType = 1:                                                       */
/*             FIND {&Tabell} WHERE {&Tabell}.{&KeyFelt}  = INT(ENTRY(1,TT_Elogg.Verdier,CHR(1))) AND               */
/*                                  {&Tabell}.{&KeyFelt2} = DATE(ENTRY(2,TT_Elogg.Verdier,CHR(1))) AND              */
/*                                  {&Tabell}.{&KeyFelt3} = INT(ENTRY(3,TT_Elogg.Verdier,CHR(1))) AND               */
/*                                  {&Tabell}.{&KeyFelt4} = INT(ENTRY(4,TT_Elogg.Verdier,CHR(1))) NO-LOCK NO-ERROR. */
/*             IF AVAIL {&Tabell} THEN                                                                              */
/*                 EXPORT {&Tabell}.ButikkNr                                                                        */
/*         END.                                                                                                     */
/*     END.                                                                                                         */
/*     EMPTY TEMP-TABLE TT_{&Tabell}.                                                                               */
/*     &UNDEFINE Tabell                                                                                             */
/*     &UNDEFINE KeyFelt                                                                                            */
/*     &UNDEFINE KeyFelt2                                                                                           */
/*     &UNDEFINE KeyFelt3                                                                                           */
/*     &UNDEFINE KeyFelt4                                                                                           */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SendKassererKontanter) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SendKassererKontanter Procedure 
PROCEDURE SendKassererKontanter :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    &SCOPED-DEFINE Tabell   KassererKontanter
    &SCOPED-DEFINE KeyFelt  ButikkNr
    &SCOPED-DEFINE KeyFelt2 Dato
    &SCOPED-DEFINE KeyFelt3 KassererNr
    &SCOPED-DEFINE KeyFelt4 z_nummer
    
    DEFINE VARIABLE iAntSlett   AS INTEGER    NO-UNDO.
    DEFINE VARIABLE iAntNyEndre AS INTEGER    NO-UNDO.
    DEFINE VARIABLE cTabellNavn AS CHARACTER  INIT "{&Tabell}" NO-UNDO.
    
    EMPTY TEMP-TABLE TT_{&Tabell}.
    RUN KopierElogg (0,cTabellNavn,OUTPUT iAntSlett,OUTPUT iAntNyEndre).
    
    iAntSlett = 0.
    FOR EACH TT_ELogg WHERE TT_ELogg.TabellNavn = cTabellNavn AND TT_ELogg.EksterntSystem = "HK"
                        AND TT_Elogg.EndringsType = 3:
        iAntSlett = iAntSlett + 1.
    END.
    
    IF iAntSlett > 0 THEN DO:
        EXPORT "H" cTabellNavn 3 "1.0" iAntSlett.
        CREATE TT_{&Tabell}.
        FOR EACH TT_ELogg WHERE TT_ELogg.TabellNavn = cTabellNavn AND TT_ELogg.EksterntSystem = "HK"
                            AND TT_Elogg.EndringsType = 3:
            ASSIGN TT_{&Tabell}.{&KeyFelt}  = INT(ENTRY(1,TT_Elogg.Verdier,CHR(1)))
                   TT_{&Tabell}.{&KeyFelt2} = DATE(ENTRY(2,TT_Elogg.Verdier,CHR(1)))
                   TT_{&Tabell}.{&KeyFelt3} = INT(ENTRY(3,TT_Elogg.Verdier,CHR(1)))
                   TT_{&Tabell}.{&KeyFelt4} = INT(ENTRY(4,TT_Elogg.Verdier,CHR(1))).
            EXPORT TT_{&Tabell}.ButikkNr       
                   TT_{&Tabell}.Dato           
                   TT_{&Tabell}.KassererNr     
                   TT_{&Tabell}.z_nummer       
                   TT_{&Tabell}.Belop[ 1]      
                   TT_{&Tabell}.Belop[ 2]      
                   TT_{&Tabell}.Belop[ 3]      
                   TT_{&Tabell}.Belop[ 4]      
                   TT_{&Tabell}.Belop[ 5]      
                   TT_{&Tabell}.Belop[ 6]      
                   TT_{&Tabell}.Belop[ 7]      
                   TT_{&Tabell}.Belop[ 8]      
                   TT_{&Tabell}.Belop[ 9]      
                   TT_{&Tabell}.Belop[10]      
                   TT_{&Tabell}.Belop[11]      
                   TT_{&Tabell}.Belop[12]      
                   TT_{&Tabell}.Belop[13]      
                   TT_{&Tabell}.Belop[14]      
                   TT_{&Tabell}.Belop[15]      
                   TT_{&Tabell}.EDato          
                   TT_{&Tabell}.ETid           
                   TT_{&Tabell}.BrukerID       
                   TT_{&Tabell}.RegistrertDato 
                   TT_{&Tabell}.RegistrertTid  
                   TT_{&Tabell}.RegistrertAv   
                   TT_{&Tabell}.AntallValor[ 1]
                   TT_{&Tabell}.AntallValor[ 2]
                   TT_{&Tabell}.AntallValor[ 3]
                   TT_{&Tabell}.AntallValor[ 4]
                   TT_{&Tabell}.AntallValor[ 5]
                   TT_{&Tabell}.AntallValor[ 6]
                   TT_{&Tabell}.AntallValor[ 7]
                   TT_{&Tabell}.AntallValor[ 8]
                   TT_{&Tabell}.AntallValor[ 9]
                   TT_{&Tabell}.AntallValor[10]
                   TT_{&Tabell}.AntallValor[11]
                   TT_{&Tabell}.AntallValor[12]
                   TT_{&Tabell}.AntallValor[13]
                   TT_{&Tabell}.AntallValor[14]
                   TT_{&Tabell}.AntallValor[15].
        END.
    END.
    
    iAntNyEndre = 0.
    FOR EACH TT_ELogg WHERE TT_ELogg.TabellNavn = cTabellNavn AND TT_ELogg.EksterntSystem = "HK"
                        AND TT_Elogg.EndringsType = 1:
        iAntNyEndre = iAntNyEndre + 1.                
    END.
    
    IF iAntNyEndre > 0 THEN DO:
        EXPORT "H" cTabellNavn 1 "1.0" iAntNyEndre.
        FOR EACH TT_ELogg WHERE TT_ELogg.TabellNavn = cTabellNavn AND TT_ELogg.EksterntSystem = "HK"
                            AND TT_Elogg.EndringsType = 1:
            FIND {&Tabell} WHERE {&Tabell}.{&KeyFelt}  = INT(ENTRY(1,TT_Elogg.Verdier,CHR(1))) AND 
                                 {&Tabell}.{&KeyFelt2} = DATE(ENTRY(2,TT_Elogg.Verdier,CHR(1))) AND
                                 {&Tabell}.{&KeyFelt3} = INT(ENTRY(3,TT_Elogg.Verdier,CHR(1))) AND
                                 {&Tabell}.{&KeyFelt4} = INT(ENTRY(4,TT_Elogg.Verdier,CHR(1))) NO-LOCK NO-ERROR.
            IF AVAIL {&Tabell} THEN
                EXPORT {&Tabell}.ButikkNr       
                       {&Tabell}.Dato           
                       {&Tabell}.KassererNr     
                       {&Tabell}.z_nummer       
                       {&Tabell}.Belop[ 1]      
                       {&Tabell}.Belop[ 2]      
                       {&Tabell}.Belop[ 3]      
                       {&Tabell}.Belop[ 4]      
                       {&Tabell}.Belop[ 5]      
                       {&Tabell}.Belop[ 6]      
                       {&Tabell}.Belop[ 7]      
                       {&Tabell}.Belop[ 8]      
                       {&Tabell}.Belop[ 9]      
                       {&Tabell}.Belop[10]      
                       {&Tabell}.Belop[11]      
                       {&Tabell}.Belop[12]      
                       {&Tabell}.Belop[13]      
                       {&Tabell}.Belop[14]      
                       {&Tabell}.Belop[15]      
                       {&Tabell}.EDato          
                       {&Tabell}.ETid           
                       {&Tabell}.BrukerID       
                       {&Tabell}.RegistrertDato 
                       {&Tabell}.RegistrertTid  
                       {&Tabell}.RegistrertAv   
                       {&Tabell}.AntallValor[ 1]
                       {&Tabell}.AntallValor[ 2]
                       {&Tabell}.AntallValor[ 3]
                       {&Tabell}.AntallValor[ 4]
                       {&Tabell}.AntallValor[ 5]
                       {&Tabell}.AntallValor[ 6]
                       {&Tabell}.AntallValor[ 7]
                       {&Tabell}.AntallValor[ 8]
                       {&Tabell}.AntallValor[ 9]
                       {&Tabell}.AntallValor[10]
                       {&Tabell}.AntallValor[11]
                       {&Tabell}.AntallValor[12]
                       {&Tabell}.AntallValor[13]
                       {&Tabell}.AntallValor[14]
                       {&Tabell}.AntallValor[15].
        END.
    END.
    EMPTY TEMP-TABLE TT_{&Tabell}.
    &UNDEFINE Tabell
    &UNDEFINE KeyFelt
    &UNDEFINE KeyFelt2
    &UNDEFINE KeyFelt3
    &UNDEFINE KeyFelt4
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SendKassererOppgj) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SendKassererOppgj Procedure 
PROCEDURE SendKassererOppgj :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    &SCOPED-DEFINE Tabell   KassererOppgj
    &SCOPED-DEFINE KeyFelt  ButikkNr
    &SCOPED-DEFINE KeyFelt2 Dato
    &SCOPED-DEFINE KeyFelt3 KassererNr
    &SCOPED-DEFINE KeyFelt4 z_nummer
    
    DEFINE VARIABLE iAntSlett   AS INTEGER    NO-UNDO.
    DEFINE VARIABLE iAntNyEndre AS INTEGER    NO-UNDO.
    DEFINE VARIABLE cTabellNavn AS CHARACTER  INIT "{&Tabell}" NO-UNDO.
    
    EMPTY TEMP-TABLE TT_{&Tabell}.
    RUN KopierElogg (0,cTabellNavn,OUTPUT iAntSlett,OUTPUT iAntNyEndre).
    
    iAntSlett = 0.
    FOR EACH TT_ELogg WHERE TT_ELogg.TabellNavn = cTabellNavn AND TT_ELogg.EksterntSystem = "HK"
                        AND TT_Elogg.EndringsType = 3:
        iAntSlett = iAntSlett + 1.
    END.
        
    IF iAntSlett > 0 THEN DO:
        EXPORT "H" cTabellNavn 3 "1.0" iAntSlett.
        CREATE TT_{&Tabell}.
        FOR EACH TT_ELogg WHERE TT_ELogg.TabellNavn = cTabellNavn AND TT_ELogg.EksterntSystem = "HK"
                            AND TT_Elogg.EndringsType = 3:
            ASSIGN TT_{&Tabell}.{&KeyFelt}  = INT(ENTRY(1,TT_Elogg.Verdier,CHR(1)))
                   TT_{&Tabell}.{&KeyFelt2} = DATE(ENTRY(2,TT_Elogg.Verdier,CHR(1)))
                   TT_{&Tabell}.{&KeyFelt3} = INT(ENTRY(3,TT_Elogg.Verdier,CHR(1)))
                   TT_{&Tabell}.{&KeyFelt4} = INT(ENTRY(4,TT_Elogg.Verdier,CHR(1))).
            EXPORT TT_{&Tabell}.ButikkNr
                   TT_{&Tabell}.Dato
                   TT_{&Tabell}.KassererNr
                   TT_{&Tabell}.z_nummer
                   TT_{&Tabell}.OpptaltVeksel
                   TT_{&Tabell}.OpptaltKontanter
                   TT_{&Tabell}.OpptaltSjekk
                   TT_{&Tabell}.OpptaltReserve
                   TT_{&Tabell}.OpptaltGavekort
                   TT_{&Tabell}.OpptaltTilgode
                   TT_{&Tabell}.EDato
                   TT_{&Tabell}.ETid
                   TT_{&Tabell}.BrukerID
                   TT_{&Tabell}.RegistrertDato
                   TT_{&Tabell}.RegistrertTid
                   TT_{&Tabell}.RegistrertAv
                   TT_{&Tabell}.OpptaltGavekortAndre
                   TT_{&Tabell}.OpptaltGavekortUtlevert
                   TT_{&Tabell}.OpptaltTilgodeAndre
                   TT_{&Tabell}.OpptaltTilgodeUtlevert
                   TT_{&Tabell}.OpptaltInnVeksel
                   TT_{&Tabell}.OpptaltValuta
                   TT_{&Tabell}.OpptaltLevertBank
                   TT_{&Tabell}.OpptaltBilag
                   TT_{&Tabell}.PoseNr.
        END.
    END.
    
    iAntNyEndre = 0.
    FOR EACH TT_ELogg WHERE TT_ELogg.TabellNavn = cTabellNavn AND TT_ELogg.EksterntSystem = "HK"
                        AND TT_Elogg.EndringsType = 1:
        iAntNyEndre = iAntNyEndre + 1.
    END. 
    
    IF iAntNyEndre > 0 THEN DO:
        EXPORT "H" cTabellNavn 1 "1.0" iAntNyEndre.
        FOR EACH TT_ELogg WHERE TT_ELogg.TabellNavn = cTabellNavn AND TT_ELogg.EksterntSystem = "HK"
                            AND TT_Elogg.EndringsType = 1:
            FIND {&Tabell} WHERE {&Tabell}.{&KeyFelt}  = INT(ENTRY(1,TT_Elogg.Verdier,CHR(1))) AND 
                                 {&Tabell}.{&KeyFelt2} = DATE(ENTRY(2,TT_Elogg.Verdier,CHR(1))) AND
                                 {&Tabell}.{&KeyFelt3} = INT(ENTRY(3,TT_Elogg.Verdier,CHR(1))) AND
                                 {&Tabell}.{&KeyFelt4} = INT(ENTRY(4,TT_Elogg.Verdier,CHR(1))) NO-LOCK NO-ERROR.
            IF AVAIL {&Tabell} THEN
                EXPORT {&Tabell}.ButikkNr
                       {&Tabell}.Dato
                       {&Tabell}.KassererNr
                       {&Tabell}.z_nummer
                       {&Tabell}.OpptaltVeksel
                       {&Tabell}.OpptaltKontanter
                       {&Tabell}.OpptaltSjekk
                       {&Tabell}.OpptaltReserve
                       {&Tabell}.OpptaltGavekort
                       {&Tabell}.OpptaltTilgode
                       {&Tabell}.EDato
                       {&Tabell}.ETid
                       {&Tabell}.BrukerID
                       {&Tabell}.RegistrertDato
                       {&Tabell}.RegistrertTid
                       {&Tabell}.RegistrertAv
                       {&Tabell}.OpptaltGavekortAndre
                       {&Tabell}.OpptaltGavekortUtlevert
                       {&Tabell}.OpptaltTilgodeAndre
                       {&Tabell}.OpptaltTilgodeUtlevert
                       {&Tabell}.OpptaltInnVeksel
                       {&Tabell}.OpptaltValuta
                       {&Tabell}.OpptaltLevertBank
                       {&Tabell}.OpptaltBilag
                       {&Tabell}.PoseNr.
        END.
    END.
    EMPTY TEMP-TABLE TT_{&Tabell}.
    &UNDEFINE Tabell
    &UNDEFINE KeyFelt
    &UNDEFINE KeyFelt2
    &UNDEFINE KeyFelt3
    &UNDEFINE KeyFelt4
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SendKassererValuta) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SendKassererValuta Procedure 
PROCEDURE SendKassererValuta :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    &SCOPED-DEFINE Tabell   KassererValuta
    &SCOPED-DEFINE KeyFelt  ButikkNr
    &SCOPED-DEFINE KeyFelt2 KasseNr
    &SCOPED-DEFINE KeyFelt3 Dato
    &SCOPED-DEFINE KeyFelt4 KassererNr
    &SCOPED-DEFINE KeyFelt5 z_nummer
    &SCOPED-DEFINE KeyFelt6 ValKod
    
    DEFINE VARIABLE iAntSlett   AS INTEGER    NO-UNDO.
    DEFINE VARIABLE iAntNyEndre AS INTEGER    NO-UNDO.
    DEFINE VARIABLE cTabellNavn AS CHARACTER  INIT "{&Tabell}" NO-UNDO.
    
    EMPTY TEMP-TABLE TT_{&Tabell}.
    RUN KopierElogg (0,cTabellNavn,OUTPUT iAntSlett,OUTPUT iAntNyEndre).

    iAntSlett = 0.
    FOR EACH TT_ELogg WHERE TT_ELogg.TabellNavn = cTabellNavn AND TT_ELogg.EksterntSystem = "HK"
                        AND TT_Elogg.EndringsType = 3:
        iAntSlett = iAntSlett + 1.
    END.
    
    IF iAntSlett > 0 THEN DO:
        EXPORT "H" cTabellNavn 3 "1.0" iAntSlett.
        CREATE TT_{&Tabell}.
        FOR EACH TT_ELogg WHERE TT_ELogg.TabellNavn = cTabellNavn AND TT_ELogg.EksterntSystem = "HK"
                            AND TT_Elogg.EndringsType = 3:
            ASSIGN TT_{&Tabell}.{&KeyFelt}  = INT(ENTRY(1,TT_Elogg.Verdier,CHR(1)))
                   TT_{&Tabell}.{&KeyFelt2} = INT(ENTRY(2,TT_Elogg.Verdier,CHR(1)))
                   TT_{&Tabell}.{&KeyFelt3} = DATE(ENTRY(3,TT_Elogg.Verdier,CHR(1)))
                   TT_{&Tabell}.{&KeyFelt4} = INT(ENTRY(4,TT_Elogg.Verdier,CHR(1)))
                   TT_{&Tabell}.{&KeyFelt5} = INT(ENTRY(5,TT_Elogg.Verdier,CHR(1)))
                   TT_{&Tabell}.{&KeyFelt6} = ENTRY(6,TT_Elogg.Verdier,CHR(1)).
            EXPORT TT_{&Tabell}.ButikkNr
                   TT_{&Tabell}.KasseNr
                   TT_{&Tabell}.Dato
                   TT_{&Tabell}.KassererNr
                   TT_{&Tabell}.z_nummer
                   TT_{&Tabell}.ValKod
                   TT_{&Tabell}.KasseValKurs
                   TT_{&Tabell}.Valuta
                   TT_{&Tabell}.Belop
                   TT_{&Tabell}.EDato
                   TT_{&Tabell}.ETid
                   TT_{&Tabell}.BrukerID
                   TT_{&Tabell}.RegistrertDato
                   TT_{&Tabell}.RegistrertTid
                   TT_{&Tabell}.RegistrertAv.
        END.
    END.
    
    iAntNyEndre = 0.
    FOR EACH TT_ELogg WHERE TT_ELogg.TabellNavn = cTabellNavn AND TT_ELogg.EksterntSystem = "HK"
                        AND TT_Elogg.EndringsType = 1:
        iAntNyEndre = iAntNyEndre + 1.
    END.
        
    IF iAntNyEndre > 0 THEN DO:
        EXPORT "H" cTabellNavn 1 "1.0" iAntNyEndre.
        FOR EACH TT_ELogg WHERE TT_ELogg.TabellNavn = cTabellNavn AND TT_ELogg.EksterntSystem = "HK"
                            AND TT_Elogg.EndringsType = 1:
            FIND {&Tabell} WHERE {&Tabell}.{&KeyFelt}  = INT(ENTRY(1,TT_Elogg.Verdier,CHR(1))) AND 
                                 {&Tabell}.{&KeyFelt2} = INT(ENTRY(2,TT_Elogg.Verdier,CHR(1))) AND
                                 {&Tabell}.{&KeyFelt3} = DATE(ENTRY(3,TT_Elogg.Verdier,CHR(1))) AND
                                 {&Tabell}.{&KeyFelt4} = INT(ENTRY(4,TT_Elogg.Verdier,CHR(1))) AND
                                 {&Tabell}.{&KeyFelt5} = INT(ENTRY(5,TT_Elogg.Verdier,CHR(1))) AND
                                 {&Tabell}.{&KeyFelt6} = ENTRY(6,TT_Elogg.Verdier,CHR(1)) NO-LOCK NO-ERROR.
            IF AVAIL {&Tabell} THEN
                EXPORT {&Tabell}.ButikkNr
                       {&Tabell}.KasseNr
                       {&Tabell}.Dato
                       {&Tabell}.KassererNr
                       {&Tabell}.z_nummer
                       {&Tabell}.ValKod
                       {&Tabell}.KasseValKurs
                       {&Tabell}.Valuta
                       {&Tabell}.Belop
                       {&Tabell}.EDato
                       {&Tabell}.ETid
                       {&Tabell}.BrukerID
                       {&Tabell}.RegistrertDato
                       {&Tabell}.RegistrertTid
                       {&Tabell}.RegistrertAv.
        END.
    END.
    EMPTY TEMP-TABLE TT_{&Tabell}.
    &UNDEFINE Tabell
    &UNDEFINE KeyFelt
    &UNDEFINE KeyFelt2
    &UNDEFINE KeyFelt3
    &UNDEFINE KeyFelt4
    &UNDEFINE KeyFelt5
    &UNDEFINE KeyFelt6
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SendKas_Rap) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SendKas_Rap Procedure 
PROCEDURE SendKas_Rap :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    &SCOPED-DEFINE Tabell   kas_rap
    &SCOPED-DEFINE KeyFelt  dato
    &SCOPED-DEFINE KeyFelt2 butikk
    &SCOPED-DEFINE KeyFelt3 kasse
    &SCOPED-DEFINE KeyFelt4 z_nummer
    
    DEFINE VARIABLE iAntSlett   AS INTEGER    NO-UNDO.
    DEFINE VARIABLE iAntNyEndre AS INTEGER    NO-UNDO.
    DEFINE VARIABLE cTabellNavn AS CHARACTER  INIT "{&Tabell}" NO-UNDO.
    
    EMPTY TEMP-TABLE TT_{&Tabell}.
    
    RUN KopierElogg (0,cTabellNavn,OUTPUT iAntSlett,OUTPUT iAntNyEndre).

    /* Teller opp antall poster som skal sendes. */
    iAntSlett = 0.
    FOR EACH TT_ELogg WHERE TT_ELogg.TabellNavn = cTabellNavn AND TT_ELogg.EksterntSystem = "HK"
                      AND TT_Elogg.EndringsType = 3:
        iAntSlett = iAntSlett + 1.
    END.
      
    IF iAntSlett > 0 THEN DO:
        EXPORT "H" cTabellNavn 3 "1.0" iAntSlett.
        CREATE TT_{&Tabell}.
        FOR EACH TT_ELogg WHERE TT_ELogg.TabellNavn = cTabellNavn AND TT_ELogg.EksterntSystem = "HK"
                            AND TT_Elogg.EndringsType = 3:
            ASSIGN TT_{&Tabell}.{&KeyFelt}  = DATE(ENTRY(1,TT_Elogg.Verdier,CHR(1)))
                   TT_{&Tabell}.{&KeyFelt2} = INT(ENTRY(2,TT_Elogg.Verdier,CHR(1)))
                   TT_{&Tabell}.{&KeyFelt3} = INT(ENTRY(3,TT_Elogg.Verdier,CHR(1)))
                   TT_{&Tabell}.{&KeyFelt4} = INT(ENTRY(4,TT_Elogg.Verdier,CHR(1))).
            EXPORT TT_{&Tabell}.dato             
                   TT_{&Tabell}.butikk           
                   TT_{&Tabell}.kasse            
                   TT_{&Tabell}.z_nummer         
                   TT_{&Tabell}.kontant          
                   TT_{&Tabell}.sjekk            
                   TT_{&Tabell}.kort             
                   TT_{&Tabell}.kredit           
                   TT_{&Tabell}.kupong1          
                   TT_{&Tabell}.kupong2          
                   TT_{&Tabell}.tilgode          
                   TT_{&Tabell}.layaway_inn      
                   TT_{&Tabell}.layaway_ut       
                   TT_{&Tabell}.kont_inn         
                   TT_{&Tabell}.kont_ut          
                   TT_{&Tabell}.Gavekort         
                   TT_{&Tabell}.Rekvisisasjon    
                   TT_{&Tabell}.Pant             
                   TT_{&Tabell}.Bank             
                   TT_{&Tabell}.Dropp            
                   TT_{&Tabell}.Overfort         
                   TT_{&Tabell}.CashBack         
                   TT_{&Tabell}.Veksel           
                   TT_{&Tabell}.Avrunding        
                   TT_{&Tabell}.Reklamasjon      
                   TT_{&Tabell}.Retur            
                   TT_{&Tabell}.InnbetaltKunde   
                   TT_{&Tabell}.Medlemssalg      
                   TT_{&Tabell}.AntCashBack      
                   TT_{&Tabell}.AntMedlemssalg   
                   TT_{&Tabell}.AntInnbetaltKunde
                   TT_{&Tabell}.AntRetur         
                   TT_{&Tabell}.AntKontant       
                   TT_{&Tabell}.AntSjekk         
                   TT_{&Tabell}.AntKort          
                   TT_{&Tabell}.AntKredit        
                   TT_{&Tabell}.AntKupong1       
                   TT_{&Tabell}.AntKupong2       
                   TT_{&Tabell}.AntTilgode       
                   TT_{&Tabell}.AntBank          
                   TT_{&Tabell}.AntGavekort      
                   TT_{&Tabell}.AntRekvisisjon   
                   TT_{&Tabell}.AntVeksel        
                   TT_{&Tabell}.AntAvrunding     
                   TT_{&Tabell}.AntDropp         
                   TT_{&Tabell}.AntOverfort      
                   TT_{&Tabell}.AntKont_Inn      
                   TT_{&Tabell}.AntKont_Ut       
                   TT_{&Tabell}.AntLayAway_Inn   
                   TT_{&Tabell}.AntLayAway_Ut    
                   TT_{&Tabell}.AntReturer       
                   TT_{&Tabell}.TilgodeInn       
                   TT_{&Tabell}.TilgodeUt        
                   TT_{&Tabell}.AntTilgodeInn    
                   TT_{&Tabell}.AntTilgodeUt     
                   TT_{&Tabell}.GavekortUt       
                   TT_{&Tabell}.GavekortInn      
                   TT_{&Tabell}.AntGavekortUt    
                   TT_{&Tabell}.AntGavekortInn   
                   TT_{&Tabell}.Medlemsrabatt

                   TT_{&Tabell}.GavekortAndreInn    
                   TT_{&Tabell}.AntGavekortAndreInn 
                   TT_{&Tabell}.GavekortRabatt      
                   TT_{&Tabell}.AntGavekortRabUt    
                   TT_{&Tabell}.Kunderabatt         
                   TT_{&Tabell}.Personalrabatt      
                   TT_{&Tabell}.GenerellRabatt      
                   TT_{&Tabell}.AntPersonalrabatt   
                   TT_{&Tabell}.AntMedlemsrabatt    
                   TT_{&Tabell}.AntKunderabatt      
                   TT_{&Tabell}.AntGenerellRabatt   
                   TT_{&Tabell}.OverfortInn         
                   TT_{&Tabell}.OverfortUt          
                   TT_{&Tabell}.AntOverfortInn      
                   TT_{&Tabell}.AntOverfortUt       
                   TT_{&Tabell}.MvaGrp[ 1]          
                   TT_{&Tabell}.MvaGrp[ 2]          
                   TT_{&Tabell}.MvaGrp[ 3]          
                   TT_{&Tabell}.MvaGrp[ 4]          
                   TT_{&Tabell}.MvaGrp[ 5]          
                   TT_{&Tabell}.MvaGrp[ 6]          
                   TT_{&Tabell}.MvaGrp[ 7]          
                   TT_{&Tabell}.MvaGrp[ 8]          
                   TT_{&Tabell}.MvaGrp[ 9]          
                   TT_{&Tabell}.MvaGrp[10]          
                   TT_{&Tabell}.MvaGrunnlag[ 1]     
                   TT_{&Tabell}.MvaGrunnlag[ 2]     
                   TT_{&Tabell}.MvaGrunnlag[ 3]     
                   TT_{&Tabell}.MvaGrunnlag[ 4]     
                   TT_{&Tabell}.MvaGrunnlag[ 5]     
                   TT_{&Tabell}.MvaGrunnlag[ 6]     
                   TT_{&Tabell}.MvaGrunnlag[ 7]     
                   TT_{&Tabell}.MvaGrunnlag[ 8]     
                   TT_{&Tabell}.MvaGrunnlag[ 9]     
                   TT_{&Tabell}.MvaGrunnlag[10]     
                   TT_{&Tabell}.MvaBelop[ 1]        
                   TT_{&Tabell}.MvaBelop[ 2]        
                   TT_{&Tabell}.MvaBelop[ 3]        
                   TT_{&Tabell}.MvaBelop[ 4]        
                   TT_{&Tabell}.MvaBelop[ 5]        
                   TT_{&Tabell}.MvaBelop[ 6]        
                   TT_{&Tabell}.MvaBelop[ 7]        
                   TT_{&Tabell}.MvaBelop[ 8]        
                   TT_{&Tabell}.MvaBelop[ 9]        
                   TT_{&Tabell}.MvaBelop[10]        
                   TT_{&Tabell}.AntReklamasjoner    
                   TT_{&Tabell}.Vekselbeholdning    
                   TT_{&Tabell}.Kontantbeholdning   
                   TT_{&Tabell}.Sjekkbeholdning     
                   TT_{&Tabell}.Lagerjustering      
                   TT_{&Tabell}.Varemottak          
                   TT_{&Tabell}.AntLagerjustering   
                   TT_{&Tabell}.AntVaremottak       
                   TT_{&Tabell}.Brekkasje           
                   TT_{&Tabell}.InterntForbruk      
                   TT_{&Tabell}.AntBrekkasje        
                   TT_{&Tabell}.AntInterntForbruk   
                   TT_{&Tabell}.Reservelosning      
                   TT_{&Tabell}.AntReservelosning   
                   TT_{&Tabell}.AntPakkerabatt      
                   TT_{&Tabell}.Pakkerabatt         
                   TT_{&Tabell}.TilgodeAndre        
                   TT_{&Tabell}.GavekortAndreInn    
                   TT_{&Tabell}.AntGavekortAndreInn 
                   TT_{&Tabell}.GavekortRabatt      
                   TT_{&Tabell}.AntGavekortRabUt    
                .    
        END.
    END.

    iAntNyEndre = 0.
    FOR EACH TT_ELogg WHERE TT_ELogg.TabellNavn = cTabellNavn AND TT_ELogg.EksterntSystem = "HK"
                        AND TT_Elogg.EndringsType = 1:
            FIND {&Tabell} WHERE {&Tabell}.{&KeyFelt}  = DATE(ENTRY(1,TT_Elogg.Verdier,CHR(1))) AND 
                                 {&Tabell}.{&KeyFelt2} = INT(ENTRY(2,TT_Elogg.Verdier,CHR(1))) AND
                                 {&Tabell}.{&KeyFelt3} = INT(ENTRY(3,TT_Elogg.Verdier,CHR(1))) AND
                                 {&Tabell}.{&KeyFelt4} = INT(ENTRY(4,TT_Elogg.Verdier,CHR(1))) NO-LOCK NO-ERROR.
            IF AVAIL {&Tabell} THEN
              iAntNyEndre = iAntNyEndre + 1.
    END.
    
    IF iAntNyEndre > 0 THEN DO:    
        EXPORT "H" cTabellNavn 1 "1.0" iAntNyEndre.
        FOR EACH TT_ELogg WHERE TT_ELogg.TabellNavn = cTabellNavn AND TT_ELogg.EksterntSystem = "HK"
                            AND TT_Elogg.EndringsType = 1:
            FIND {&Tabell} WHERE {&Tabell}.{&KeyFelt}  = DATE(ENTRY(1,TT_Elogg.Verdier,CHR(1))) AND 
                                 {&Tabell}.{&KeyFelt2} = INT(ENTRY(2,TT_Elogg.Verdier,CHR(1))) AND
                                 {&Tabell}.{&KeyFelt3} = INT(ENTRY(3,TT_Elogg.Verdier,CHR(1))) AND
                                 {&Tabell}.{&KeyFelt4} = INT(ENTRY(4,TT_Elogg.Verdier,CHR(1))) NO-LOCK NO-ERROR.
            IF AVAIL {&Tabell} THEN
                EXPORT {&Tabell}.dato             
                       {&Tabell}.butikk           
                       {&Tabell}.kasse            
                       {&Tabell}.z_nummer         
                       {&Tabell}.kontant          
                       {&Tabell}.sjekk            
                       {&Tabell}.kort             
                       {&Tabell}.kredit           
                       {&Tabell}.kupong1          
                       {&Tabell}.kupong2          
                       {&Tabell}.tilgode          
                       {&Tabell}.layaway_inn      
                       {&Tabell}.layaway_ut       
                       {&Tabell}.kont_inn         
                       {&Tabell}.kont_ut          
                       {&Tabell}.Gavekort         
                       {&Tabell}.Rekvisisasjon    
                       {&Tabell}.Pant             
                       {&Tabell}.Bank             
                       {&Tabell}.Dropp            
                       {&Tabell}.Overfort         
                       {&Tabell}.CashBack         
                       {&Tabell}.Veksel           
                       {&Tabell}.Avrunding        
                       {&Tabell}.Reklamasjon      
                       {&Tabell}.Retur            
                       {&Tabell}.InnbetaltKunde   
                       {&Tabell}.Medlemssalg      
                       {&Tabell}.AntCashBack      
                       {&Tabell}.AntMedlemssalg   
                       {&Tabell}.AntInnbetaltKunde
                       {&Tabell}.AntRetur         
                       {&Tabell}.AntKontant       
                       {&Tabell}.AntSjekk         
                       {&Tabell}.AntKort          
                       {&Tabell}.AntKredit        
                       {&Tabell}.AntKupong1       
                       {&Tabell}.AntKupong2       
                       {&Tabell}.AntTilgode       
                       {&Tabell}.AntBank          
                       {&Tabell}.AntGavekort      
                       {&Tabell}.AntRekvisisjon   
                       {&Tabell}.AntVeksel        
                       {&Tabell}.AntAvrunding     
                       {&Tabell}.AntDropp         
                       {&Tabell}.AntOverfort      
                       {&Tabell}.AntKont_Inn      
                       {&Tabell}.AntKont_Ut       
                       {&Tabell}.AntLayAway_Inn   
                       {&Tabell}.AntLayAway_Ut    
                       {&Tabell}.AntReturer       
                       {&Tabell}.TilgodeInn       
                       {&Tabell}.TilgodeUt        
                       {&Tabell}.AntTilgodeInn    
                       {&Tabell}.AntTilgodeUt     
                       {&Tabell}.GavekortUt       
                       {&Tabell}.GavekortInn      
                       {&Tabell}.AntGavekortUt    
                       {&Tabell}.AntGavekortInn   
                       {&Tabell}.Medlemsrabatt

                       {&Tabell}.GavekortAndreInn    
                       {&Tabell}.AntGavekortAndreInn 
                       {&Tabell}.GavekortRabatt      
                       {&Tabell}.AntGavekortRabUt    
                       {&Tabell}.Kunderabatt         
                       {&Tabell}.Personalrabatt      
                       {&Tabell}.GenerellRabatt      
                       {&Tabell}.AntPersonalrabatt   
                       {&Tabell}.AntMedlemsrabatt    
                       {&Tabell}.AntKunderabatt      
                       {&Tabell}.AntGenerellRabatt   
                       {&Tabell}.OverfortInn         
                       {&Tabell}.OverfortUt          
                       {&Tabell}.AntOverfortInn      
                       {&Tabell}.AntOverfortUt       
                       {&Tabell}.MvaGrp[ 1]          
                       {&Tabell}.MvaGrp[ 2]          
                       {&Tabell}.MvaGrp[ 3]          
                       {&Tabell}.MvaGrp[ 4]          
                       {&Tabell}.MvaGrp[ 5]          
                       {&Tabell}.MvaGrp[ 6]          
                       {&Tabell}.MvaGrp[ 7]          
                       {&Tabell}.MvaGrp[ 8]          
                       {&Tabell}.MvaGrp[ 9]          
                       {&Tabell}.MvaGrp[10]          
                       {&Tabell}.MvaGrunnlag[ 1]     
                       {&Tabell}.MvaGrunnlag[ 2]     
                       {&Tabell}.MvaGrunnlag[ 3]     
                       {&Tabell}.MvaGrunnlag[ 4]     
                       {&Tabell}.MvaGrunnlag[ 5]     
                       {&Tabell}.MvaGrunnlag[ 6]     
                       {&Tabell}.MvaGrunnlag[ 7]     
                       {&Tabell}.MvaGrunnlag[ 8]     
                       {&Tabell}.MvaGrunnlag[ 9]     
                       {&Tabell}.MvaGrunnlag[10]     
                       {&Tabell}.MvaBelop[ 1]        
                       {&Tabell}.MvaBelop[ 2]        
                       {&Tabell}.MvaBelop[ 3]        
                       {&Tabell}.MvaBelop[ 4]        
                       {&Tabell}.MvaBelop[ 5]        
                       {&Tabell}.MvaBelop[ 6]        
                       {&Tabell}.MvaBelop[ 7]        
                       {&Tabell}.MvaBelop[ 8]        
                       {&Tabell}.MvaBelop[ 9]        
                       {&Tabell}.MvaBelop[10]        
                       {&Tabell}.AntReklamasjoner    
                       {&Tabell}.Vekselbeholdning    
                       {&Tabell}.Kontantbeholdning   
                       {&Tabell}.Sjekkbeholdning     
                       {&Tabell}.Lagerjustering      
                       {&Tabell}.Varemottak          
                       {&Tabell}.AntLagerjustering   
                       {&Tabell}.AntVaremottak       
                       {&Tabell}.Brekkasje           
                       {&Tabell}.InterntForbruk      
                       {&Tabell}.AntBrekkasje        
                       {&Tabell}.AntInterntForbruk   
                       {&Tabell}.Reservelosning      
                       {&Tabell}.AntReservelosning   
                       {&Tabell}.AntPakkerabatt      
                       {&Tabell}.Pakkerabatt         
                       {&Tabell}.TilgodeAndre        
                       {&Tabell}.GavekortAndreInn    
                       {&Tabell}.AntGavekortAndreInn 
                       {&Tabell}.GavekortRabatt      
                       {&Tabell}.AntGavekortRabUt    
                .    
        END.
    END.
    EMPTY TEMP-TABLE TT_{&Tabell}.
    &UNDEFINE Tabell
    &UNDEFINE KeyFelt
    &UNDEFINE KeyFelt2
    &UNDEFINE KeyFelt3
    &UNDEFINE KeyFelt4
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SendKonto) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SendKonto Procedure 
PROCEDURE SendKonto :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    &SCOPED-DEFINE Tabell   konto
    &SCOPED-DEFINE KeyFelt  dato
    &SCOPED-DEFINE KeyFelt2 butikk
    &SCOPED-DEFINE KeyFelt3 kontonummer
    
    DEFINE VARIABLE iAntSlett   AS INTEGER    NO-UNDO.
    DEFINE VARIABLE iAntNyEndre AS INTEGER    NO-UNDO.
    DEFINE VARIABLE cTabellNavn AS CHARACTER  INIT "{&Tabell}" NO-UNDO.
    
    EMPTY TEMP-TABLE TT_{&Tabell}.
    RUN KopierElogg (0,cTabellNavn,OUTPUT iAntSlett,OUTPUT iAntNyEndre).
 
    iAntSlett = 0.
    FOR EACH TT_ELogg WHERE TT_ELogg.TabellNavn = cTabellNavn AND TT_ELogg.EksterntSystem = "HK"
                        AND TT_Elogg.EndringsType = 3:
        iAntSlett = iAntSlett + 1.
    END.
        
    IF iAntSlett > 0 THEN DO:
        EXPORT "H" cTabellNavn 3 "1.0" iAntSlett.
        CREATE TT_{&Tabell}.
        FOR EACH TT_ELogg WHERE TT_ELogg.TabellNavn = cTabellNavn AND TT_ELogg.EksterntSystem = "HK"
                            AND TT_Elogg.EndringsType = 3:
            ASSIGN TT_{&Tabell}.{&KeyFelt}  = DATE(ENTRY(1,TT_Elogg.Verdier,CHR(1)))
                   TT_{&Tabell}.{&KeyFelt2} = INT(ENTRY(2,TT_Elogg.Verdier,CHR(1)))
                   TT_{&Tabell}.{&KeyFelt3} = INT(ENTRY(3,TT_Elogg.Verdier,CHR(1))).
            EXPORT TT_{&Tabell}.butikk     
                   TT_{&Tabell}.dato       
                   TT_{&Tabell}.kontonummer
                   TT_{&Tabell}.vg         
                   TT_{&Tabell}.lopnr      
                   TT_{&Tabell}.storl      
                   TT_{&Tabell}.pris       
                   TT_{&Tabell}.antall     
                   TT_{&Tabell}.kvitto     
                   TT_{&Tabell}.kasse      
                   TT_{&Tabell}.forsnr.
        END.
    END.
    
    iAntNyEndre = 0.
    FOR EACH TT_ELogg WHERE TT_ELogg.TabellNavn = cTabellNavn AND TT_ELogg.EksterntSystem = "HK"
                        AND TT_Elogg.EndringsType = 1 BY verdier:
        iAntNyEndre = iAntNyEndre + 1.
    END.
    
    IF iAntNyEndre > 0 THEN DO:
        EXPORT "H" cTabellNavn 1 "1.0" iAntNyEndre.
        FOR EACH TT_ELogg WHERE TT_ELogg.TabellNavn = cTabellNavn AND TT_ELogg.EksterntSystem = "HK"
                            AND TT_Elogg.EndringsType = 1 BY verdier:
            FIND {&Tabell} WHERE {&Tabell}.{&KeyFelt}  = DATE(ENTRY(1,TT_Elogg.Verdier,CHR(1))) AND 
                                 {&Tabell}.{&KeyFelt2} = INT(ENTRY(2,TT_Elogg.Verdier,CHR(1))) AND
                                 {&Tabell}.{&KeyFelt3} = INT(ENTRY(3,TT_Elogg.Verdier,CHR(1))) NO-LOCK NO-ERROR.
            IF AVAIL {&Tabell} THEN
                EXPORT {&Tabell}.butikk     
                       {&Tabell}.dato       
                       {&Tabell}.kontonummer
                       {&Tabell}.vg         
                       {&Tabell}.lopnr      
                       {&Tabell}.storl      
                       {&Tabell}.pris       
                       {&Tabell}.antall     
                       {&Tabell}.kvitto     
                       {&Tabell}.kasse      
                       {&Tabell}.forsnr.
        END.
    END.
    EMPTY TEMP-TABLE TT_{&Tabell}.
    &UNDEFINE Tabell
    &UNDEFINE KeyFelt
    &UNDEFINE KeyFelt2
    &UNDEFINE KeyFelt3
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SendKort_Spes) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SendKort_Spes Procedure 
PROCEDURE SendKort_Spes :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    &SCOPED-DEFINE Tabell   Kort_Spes
    &SCOPED-DEFINE KeyFelt  dato
    &SCOPED-DEFINE KeyFelt2 butikk
    &SCOPED-DEFINE KeyFelt3 kasse
    &SCOPED-DEFINE KeyFelt4 z_nummer
    &SCOPED-DEFINE KeyFelt5 KortType
    
    DEFINE VARIABLE iAntSlett   AS INTEGER    NO-UNDO.
    DEFINE VARIABLE iAntNyEndre AS INTEGER    NO-UNDO.
    DEFINE VARIABLE cTabellNavn AS CHARACTER  INIT "{&Tabell}" NO-UNDO.
    
    EMPTY TEMP-TABLE TT_{&Tabell}.
    RUN KopierElogg (0,cTabellNavn,OUTPUT iAntSlett,OUTPUT iAntNyEndre).
    
    iAntSlett = 0.
    FOR EACH TT_ELogg WHERE TT_ELogg.TabellNavn = cTabellNavn AND TT_ELogg.EksterntSystem = "HK"
                        AND TT_Elogg.EndringsType = 3:
        iAntSlett = iAntSlett + 1.
    END.
    
    IF iAntSlett > 0 THEN DO:
        EXPORT "H" cTabellNavn 3 "1.0" iAntSlett.
        CREATE TT_{&Tabell}.
        FOR EACH TT_ELogg WHERE TT_ELogg.TabellNavn = cTabellNavn AND TT_ELogg.EksterntSystem = "HK"
                            AND TT_Elogg.EndringsType = 3:
            ASSIGN TT_{&Tabell}.{&KeyFelt}  = DATE(ENTRY(1,TT_Elogg.Verdier,CHR(1)))
                   TT_{&Tabell}.{&KeyFelt2} = INT(ENTRY(2,TT_Elogg.Verdier,CHR(1)))
                   TT_{&Tabell}.{&KeyFelt3} = INT(ENTRY(3,TT_Elogg.Verdier,CHR(1)))
                   TT_{&Tabell}.{&KeyFelt4} = INT(ENTRY(4,TT_Elogg.Verdier,CHR(1)))
                   TT_{&Tabell}.{&KeyFelt5} = INT(ENTRY(5,TT_Elogg.Verdier,CHR(1))).
            EXPORT TT_{&Tabell}.dato           
                   TT_{&Tabell}.butikk         
                   TT_{&Tabell}.kasse          
                   TT_{&Tabell}.z_nummer       
                   TT_{&Tabell}.AntKort        
                   TT_{&Tabell}.KortType       
                   TT_{&Tabell}.Belop          
                   TT_{&Tabell}.EDato          
                   TT_{&Tabell}.ETid           
                   TT_{&Tabell}.BrukerID       
                   TT_{&Tabell}.RegistrertDato 
                   TT_{&Tabell}.RegistrertTid  
                   TT_{&Tabell}.RegistrertAv.   
        END.
    END.
    
    iAntNyEndre = 0.
    FOR EACH TT_ELogg WHERE TT_ELogg.TabellNavn = cTabellNavn AND TT_ELogg.EksterntSystem = "HK"
                        AND TT_Elogg.EndringsType = 1:
        iAntNyEndre = iAntNyEndre + 1.
    END.
    
    IF iAntNyEndre > 0 THEN DO:
        EXPORT "H" cTabellNavn 1 "1.0" iAntNyEndre.
        FOR EACH TT_ELogg WHERE TT_ELogg.TabellNavn = cTabellNavn AND TT_ELogg.EksterntSystem = "HK"
                            AND TT_Elogg.EndringsType = 1:
            FIND {&Tabell} WHERE {&Tabell}.{&KeyFelt}  = DATE(ENTRY(1,TT_Elogg.Verdier,CHR(1))) AND 
                                 {&Tabell}.{&KeyFelt2} = INT(ENTRY(2,TT_Elogg.Verdier,CHR(1))) AND
                                 {&Tabell}.{&KeyFelt3} = INT(ENTRY(3,TT_Elogg.Verdier,CHR(1))) AND
                                 {&Tabell}.{&KeyFelt4} = INT(ENTRY(4,TT_Elogg.Verdier,CHR(1))) AND
                                 {&Tabell}.{&KeyFelt5} = INT(ENTRY(5,TT_Elogg.Verdier,CHR(1))) NO-LOCK NO-ERROR.
            IF AVAIL {&Tabell} THEN
                EXPORT {&Tabell}.dato           
                       {&Tabell}.butikk         
                       {&Tabell}.kasse          
                       {&Tabell}.z_nummer       
                       {&Tabell}.AntKort        
                       {&Tabell}.KortType       
                       {&Tabell}.Belop          
                       {&Tabell}.EDato          
                       {&Tabell}.ETid           
                       {&Tabell}.BrukerID       
                       {&Tabell}.RegistrertDato 
                       {&Tabell}.RegistrertTid  
                       {&Tabell}.RegistrertAv.   
        END.
    END.
    EMPTY TEMP-TABLE TT_{&Tabell}.
    &UNDEFINE Tabell
    &UNDEFINE KeyFelt
    &UNDEFINE KeyFelt2
    &UNDEFINE KeyFelt3
    &UNDEFINE KeyFelt4
    &UNDEFINE KeyFelt5
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SendLager) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SendLager Procedure 
PROCEDURE SendLager :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    &SCOPED-DEFINE Tabell   Lager
    &SCOPED-DEFINE KeyFelt  ArtikkelNr
    &SCOPED-DEFINE KeyFelt2 Butik
    DEFINE VARIABLE iAntSlett   AS INTEGER    NO-UNDO.
    DEFINE VARIABLE iAntNyEndre AS INTEGER    NO-UNDO.
    DEFINE VARIABLE cTabellNavn AS CHARACTER  INIT "{&Tabell}" NO-UNDO.

    EMPTY TEMP-TABLE TT_{&Tabell}.

    RUN KopierElogg (0,cTabellNavn,OUTPUT iAntSlett,OUTPUT iAntNyEndre).
/*     Inga slettposter, dessa Elogg är skapade inifrån detta pgm                                       */
/*     IF iAntSlett > 0 THEN DO:                                                                        */
/*         EXPORT "H" cTabellNavn 3 "1.0" iAntSlett.                                                    */
/*         CREATE TT_{&Tabell}.                                                                         */
/*         FOR EACH TT_ELogg WHERE TT_ELogg.TabellNavn = cTabellNavn AND TT_ELogg.EksterntSystem = "HK" */
/*                             AND TT_Elogg.EndringsType = 3:                                           */
/*             ASSIGN TT_{&Tabell}.{&KeyFelt}  = INT(ENTRY(1,TT_Elogg.Verdier,CHR(1)))                  */
/*                    TT_{&Tabell}.{&KeyFelt2} = INT(ENTRY(2,TT_Elogg.Verdier,CHR(1))).                 */
/*             EXPORT TT_{&Tabell}.ButikkNr                                                             */
/*                    TT_{&Tabell}.GruppeNr                                                             */
/*                    TT_{&Tabell}.Navn                                                                 */
/*                    TT_{&Tabell}.EDato                                                                */
/*                    TT_{&Tabell}.ETid                                                                 */
/*                    TT_{&Tabell}.BrukerId                                                             */
/*                    TT_{&Tabell}.RegistrertDato                                                       */
/*                    TT_{&Tabell}.RegistrertTid                                                        */
/*                    TT_{&Tabell}.RegistrertAv.                                                        */
/*         END.                                                                                         */
/*     END.                                                                                             */

    iAntNyEndre = 0.
    FOR EACH TT_ELogg WHERE TT_ELogg.TabellNavn = cTabellNavn AND TT_ELogg.EksterntSystem = "HK"
                        AND TT_Elogg.EndringsType = 1:
        iAntNyEndre = iAntNyEndre + 1.
    END.
    
    IF iAntNyEndre > 0 THEN DO:
        EXPORT "H" cTabellNavn 1 "1.0" iAntNyEndre.
        FOR EACH TT_ELogg WHERE TT_ELogg.TabellNavn = cTabellNavn AND TT_ELogg.EksterntSystem = "HK"
                            AND TT_Elogg.EndringsType = 1:
            FIND {&Tabell} WHERE {&Tabell}.{&KeyFelt}  = DECI(ENTRY(1,TT_Elogg.Verdier,CHR(1))) AND 
                                 {&Tabell}.{&KeyFelt2} = INT(ENTRY(2,TT_Elogg.Verdier,CHR(1))) NO-LOCK NO-ERROR.
            IF AVAIL {&Tabell} THEN
                EXPORT {&Tabell}.EDato         
                       {&Tabell}.ETid          
                       {&Tabell}.BrukerID      
                       {&Tabell}.RegistrertDato
                       {&Tabell}.RegistrertTid 
                       {&Tabell}.RegistrertAv  
                       {&Tabell}.ArtikkelNr    
                       {&Tabell}.VVarekost     
                       {&Tabell}.LagAnt        
                       {&Tabell}.SistInnlevert 
                       {&Tabell}.Butik         
                       {&Tabell}.AntSolgt      
                       {&Tabell}.BrekkAnt      
                       {&Tabell}.IntAnt        
                       {&Tabell}.ReklAnt       
                       {&Tabell}.ReklLAnt      
                       {&Tabell}.GjenkjopAnt   
                       {&Tabell}.RetLAnt       
                       {&Tabell}.KjopAnt       
                       {&Tabell}.OvAnt         
                       {&Tabell}.JustAnt       
                       {&Tabell}.JustVerdi     
                       {&Tabell}.SvinnAnt      
                       {&Tabell}.SvinnVerdi    
                       {&Tabell}.NedAnt        
                       {&Tabell}.NedVerdi      
                       {&Tabell}.VerdiSolgt    
                       {&Tabell}.KjopVerdi     
                       {&Tabell}.BrekkVerdi    
                       {&Tabell}.IntVerdi      
                       {&Tabell}.ReklVerdi     
                       {&Tabell}.ReklLVerdi    
                       {&Tabell}.GjenkjopVerdi 
                       {&Tabell}.OvVerdi       
                       {&Tabell}.VerdiRabatt   
                       {&Tabell}.AntRab
                       {&Tabell}.Svk
                .        
        END.
    END.
    EMPTY TEMP-TABLE TT_{&Tabell}.
    &UNDEFINE Tabell
    &UNDEFINE KeyFelt
    &UNDEFINE KeyFelt2
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SendSelger) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SendSelger Procedure 
PROCEDURE SendSelger :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    &SCOPED-DEFINE Tabell   Selger
    &SCOPED-DEFINE KeyFelt  SelgerNr
    
    DEFINE VARIABLE iAntSlett   AS INTEGER    NO-UNDO.
    DEFINE VARIABLE iAntNyEndre AS INTEGER    NO-UNDO.
    DEFINE VARIABLE cTabellNavn AS CHARACTER  INIT "{&Tabell}" NO-UNDO.
    
    EMPTY TEMP-TABLE TT_{&Tabell}.
    RUN KopierElogg (0,cTabellNavn,OUTPUT iAntSlett,OUTPUT iAntNyEndre).
    
    iAntSlett = 0.
    FOR EACH TT_ELogg WHERE TT_ELogg.TabellNavn = cTabellNavn AND TT_ELogg.EksterntSystem = "HK"
                        AND TT_Elogg.EndringsType = 3:
        iAntSlett = iAntSlett + 1.
    END.    
    
    IF iAntSlett > 0 THEN DO:
        EXPORT "H" cTabellNavn 3 "1.0" iAntSlett.
        CREATE TT_{&Tabell}.
        FOR EACH TT_ELogg WHERE TT_ELogg.TabellNavn = cTabellNavn AND TT_ELogg.EksterntSystem = "HK"
                            AND TT_Elogg.EndringsType = 3:
            ASSIGN TT_{&Tabell}.{&KeyFelt}  = INT(ENTRY(1,TT_Elogg.Verdier,CHR(1))).
            EXPORT TT_{&Tabell}.SelgerNr      
                   TT_{&Tabell}.Navn           
                   TT_{&Tabell}.EDato          
                   TT_{&Tabell}.ETid           
                   TT_{&Tabell}.BrukerID       
                   TT_{&Tabell}.RegistrertDato 
                   TT_{&Tabell}.RegistrertTid  
                   TT_{&Tabell}.RegistrertAv   
                   TT_{&Tabell}.AnsattNr       
                   TT_{&Tabell}.Adresse1       
                   TT_{&Tabell}.Telefon        
                   TT_{&Tabell}.PersonNr       
                   TT_{&Tabell}.Mobiltelefon   
                   TT_{&Tabell}.PostNr         
                   TT_{&Tabell}.Adresse2       
                   TT_{&Tabell}.NavnIKasse
                   TT_{&Tabell}.ButikkNr
                .    
        END.
    END.
    
    iAntNyEndre = 0.
    FOR EACH TT_ELogg WHERE TT_ELogg.TabellNavn = cTabellNavn AND TT_ELogg.EksterntSystem = "HK"
                        AND TT_Elogg.EndringsType = 1:
        iAntNyEndre = iAntNyEndre + 1.
    END.
        
    IF iAntNyEndre > 0 THEN DO:
        EXPORT "H" cTabellNavn 1 "1.0" iAntNyEndre.
        FOR EACH TT_ELogg WHERE TT_ELogg.TabellNavn = cTabellNavn AND TT_ELogg.EksterntSystem = "HK"
                            AND TT_Elogg.EndringsType = 1:
            FIND {&Tabell} WHERE {&Tabell}.{&KeyFelt}  = INT(ENTRY(1,TT_Elogg.Verdier,CHR(1))) NO-LOCK NO-ERROR.
            IF AVAIL {&Tabell} THEN
                EXPORT {&Tabell}.SelgerNr      
                       {&Tabell}.Navn          
                       {&Tabell}.EDato         
                       {&Tabell}.ETid          
                       {&Tabell}.BrukerID      
                       {&Tabell}.RegistrertDato
                       {&Tabell}.RegistrertTid 
                       {&Tabell}.RegistrertAv  
                       {&Tabell}.AnsattNr      
                       {&Tabell}.Adresse1      
                       {&Tabell}.Telefon       
                       {&Tabell}.PersonNr      
                       {&Tabell}.Mobiltelefon  
                       {&Tabell}.PostNr        
                       {&Tabell}.Adresse2      
                       {&Tabell}.NavnIKasse
                       {&Tabell}.ButikkNr.   
        END.
    END.
    EMPTY TEMP-TABLE TT_{&Tabell}.
    &UNDEFINE Tabell
    &UNDEFINE KeyFelt
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SendStLager) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SendStLager Procedure 
PROCEDURE SendStLager :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    &SCOPED-DEFINE Tabell   StLager
    &SCOPED-DEFINE KeyFelt  StTypeId
    &SCOPED-DEFINE KeyFelt2 Butik
    &SCOPED-DEFINE KeyFelt3 DataObjekt
    
    DEFINE VARIABLE iAntSlett   AS INTEGER    NO-UNDO.
    DEFINE VARIABLE iAntNyEndre AS INTEGER    NO-UNDO.
    DEFINE VARIABLE cTabellNavn AS CHARACTER  INIT "{&Tabell}" NO-UNDO.

    EMPTY TEMP-TABLE TT_{&Tabell}.

    /* Vi använder CHR(2) i stf CHR(1), Vissa ststistiker innhåller */
    /* CHR(1) i DataObjekt ex. SELGER-VG */
    RUN KopierElogg (0,cTabellNavn,OUTPUT iAntSlett,OUTPUT iAntNyEndre).
    
    iAntSlett = 0.
    FOR EACH TT_ELogg WHERE TT_ELogg.TabellNavn = cTabellNavn AND TT_ELogg.EksterntSystem = "HK"
                        AND TT_Elogg.EndringsType = 3:
        iAntSlett = iAntSlett + 1.
    END.
    
    IF iAntSlett > 0 THEN DO:
        EXPORT "H" cTabellNavn 3 "1.0" iAntSlett.
        CREATE TT_{&Tabell}.
        FOR EACH TT_ELogg WHERE TT_ELogg.TabellNavn = cTabellNavn AND TT_ELogg.EksterntSystem = "HK"
                            AND TT_Elogg.EndringsType = 3:
            ASSIGN TT_{&Tabell}.{&KeyFelt}  = ENTRY(1,TT_Elogg.Verdier,CHR(2))
                   TT_{&Tabell}.{&KeyFelt2} = int(ENTRY(2,TT_Elogg.Verdier,CHR(2)))
                   TT_{&Tabell}.{&KeyFelt3} = ENTRY(3,TT_Elogg.Verdier,CHR(2)).
                EXPORT 
                    TT_{&Tabell}.EDato         
                    TT_{&Tabell}.ETid          
                    TT_{&Tabell}.BrukerID      
                    TT_{&Tabell}.RegistrertDato
                    TT_{&Tabell}.RegistrertTid 
                    TT_{&Tabell}.RegistrertAv  
                    TT_{&Tabell}.VVarekost      
                    TT_{&Tabell}.LagAnt         
                    TT_{&Tabell}.SistInnlevert  
                    TT_{&Tabell}.Butik          
                    TT_{&Tabell}.AntSolgt       
                    TT_{&Tabell}.BrekkAnt       
                    TT_{&Tabell}.IntAnt         
                    TT_{&Tabell}.ReklAnt        
                    TT_{&Tabell}.ReklLAnt       
                    TT_{&Tabell}.GjenkjopAnt    
                    TT_{&Tabell}.RetLAnt        
                    TT_{&Tabell}.KjopAnt        
                    TT_{&Tabell}.OvAnt          
                    TT_{&Tabell}.JustAnt        
                    TT_{&Tabell}.JustVerdi      
                    TT_{&Tabell}.SvinnAnt       
                    TT_{&Tabell}.SvinnVerdi     
                    TT_{&Tabell}.NedAnt         
                    TT_{&Tabell}.NedVerdi       
                    TT_{&Tabell}.VerdiSolgt     
                    TT_{&Tabell}.KjopVerdi      
                    TT_{&Tabell}.BrekkVerdi     
                    TT_{&Tabell}.IntVerdi       
                    TT_{&Tabell}.ReklVerdi      
                    TT_{&Tabell}.ReklLVerdi     
                    TT_{&Tabell}.GjenkjopVerdi  
                    TT_{&Tabell}.OvVerdi        
                    TT_{&Tabell}.VerdiRabatt    
                    TT_{&Tabell}.AntRab         
                    TT_{&Tabell}.StTypeId       
                    TT_{&Tabell}.DataObjekt     
                    TT_{&Tabell}.vSnittKostPris 
                    TT_{&Tabell}.Svk
                    .
        END.
    END.
    
    iAntNyEndre = 0.
    FOR EACH TT_ELogg WHERE TT_ELogg.TabellNavn = cTabellNavn AND TT_ELogg.EksterntSystem = "HK"
                        AND TT_Elogg.EndringsType = 1:
        iAntNyEndre = iAntNyEndre + 1.
    END.
    
    IF iAntNyEndre > 0 THEN DO:
        EXPORT "H" cTabellNavn 1 "1.0" iAntNyEndre.
        FOR EACH TT_ELogg WHERE TT_ELogg.TabellNavn = cTabellNavn AND TT_ELogg.EksterntSystem = "HK"
                            AND TT_Elogg.EndringsType = 1:
            FIND {&Tabell} WHERE {&Tabell}.{&KeyFelt}  = ENTRY(1,TT_Elogg.Verdier,CHR(2)) AND 
                                 {&Tabell}.{&KeyFelt2} = int(ENTRY(2,TT_Elogg.Verdier,CHR(2))) AND
                                 {&Tabell}.{&KeyFelt3} = ENTRY(3,TT_Elogg.Verdier,CHR(2)) NO-LOCK NO-ERROR.
                EXPORT 
                    {&Tabell}.EDato         
                    {&Tabell}.ETid          
                    {&Tabell}.BrukerID      
                    {&Tabell}.RegistrertDato
                    {&Tabell}.RegistrertTid 
                    {&Tabell}.RegistrertAv  
                    {&Tabell}.VVarekost      
                    {&Tabell}.LagAnt         
                    {&Tabell}.SistInnlevert  
                    {&Tabell}.Butik          
                    {&Tabell}.AntSolgt       
                    {&Tabell}.BrekkAnt       
                    {&Tabell}.IntAnt         
                    {&Tabell}.ReklAnt        
                    {&Tabell}.ReklLAnt       
                    {&Tabell}.GjenkjopAnt    
                    {&Tabell}.RetLAnt        
                    {&Tabell}.KjopAnt        
                    {&Tabell}.OvAnt          
                    {&Tabell}.JustAnt        
                    {&Tabell}.JustVerdi      
                    {&Tabell}.SvinnAnt       
                    {&Tabell}.SvinnVerdi     
                    {&Tabell}.NedAnt         
                    {&Tabell}.NedVerdi       
                    {&Tabell}.VerdiSolgt     
                    {&Tabell}.KjopVerdi      
                    {&Tabell}.BrekkVerdi     
                    {&Tabell}.IntVerdi       
                    {&Tabell}.ReklVerdi      
                    {&Tabell}.ReklLVerdi     
                    {&Tabell}.GjenkjopVerdi  
                    {&Tabell}.OvVerdi        
                    {&Tabell}.VerdiRabatt    
                    {&Tabell}.AntRab         
                    {&Tabell}.StTypeId       
                    {&Tabell}.DataObjekt     
                    {&Tabell}.vSnittKostPris 
                    {&Tabell}.Svk
                    .
        END.
    END.
    EMPTY TEMP-TABLE TT_{&Tabell}.
    &UNDEFINE Tabell
    &UNDEFINE KeyFelt
    &UNDEFINE KeyFelt2
    &UNDEFINE KeyFelt3
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SendStLinje) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SendStLinje Procedure 
PROCEDURE SendStLinje :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    &SCOPED-DEFINE Tabell   StLinje
    &SCOPED-DEFINE KeyFelt  StTypeId
    &SCOPED-DEFINE KeyFelt2 PerId
    &SCOPED-DEFINE KeyFelt3 DataObjekt
    &SCOPED-DEFINE KeyFelt4 Diverse
    &SCOPED-DEFINE KeyFelt5 Butik
    &SCOPED-DEFINE KeyFelt6 Aar
    &SCOPED-DEFINE KeyFelt7 PerLinNr
    
    DEFINE VARIABLE iAntSlett   AS INTEGER    NO-UNDO.
    DEFINE VARIABLE iAntNyEndre AS INTEGER    NO-UNDO.
    DEFINE VARIABLE cTabellNavn AS CHARACTER  INIT "{&Tabell}" NO-UNDO.

    EMPTY TEMP-TABLE TT_{&Tabell}.

    /* Vi använder CHR(2) i stf CHR(1), Vissa ststistiker innhåller */
    /* CHR(1) i DataObjekt ex. SELGER-VG */
    RUN KopierElogg (1000,cTabellNavn,OUTPUT iAntSlett,OUTPUT iAntNyEndre).
    RUN CreateELoggLager. /* Oppretter Elogg for Lager og ArtLag for alle StLinje poster. */
    
    iAntSlett = 0.
    FOR EACH TT_ELogg WHERE TT_ELogg.TabellNavn = cTabellNavn AND TT_ELogg.EksterntSystem = "HK"
                        AND TT_Elogg.EndringsType = 3:
        IF (ENTRY(1,TT_Elogg.Verdier,CHR(2)) BEGINS "ARTIKKEL" AND bStatTilHK = FALSE) THEN. /* Ikke send. */
        ELSE iAntSlett = iAntSlett + 1.
    END.
    
    IF iAntSlett > 0 THEN DO:
        EXPORT "H" cTabellNavn 3 "1.0" iAntSlett.
        CREATE TT_{&Tabell}.
        FOR EACH TT_ELogg WHERE TT_ELogg.TabellNavn = cTabellNavn AND TT_ELogg.EksterntSystem = "HK"
                            AND TT_Elogg.EndringsType = 3:
            ASSIGN TT_{&Tabell}.{&KeyFelt}  = ENTRY(1,TT_Elogg.Verdier,CHR(2))
                   TT_{&Tabell}.{&KeyFelt2} = ENTRY(2,TT_Elogg.Verdier,CHR(2))
                   TT_{&Tabell}.{&KeyFelt3} = ENTRY(3,TT_Elogg.Verdier,CHR(2))
                   TT_{&Tabell}.{&KeyFelt4} = ENTRY(4,TT_Elogg.Verdier,CHR(2))
                   TT_{&Tabell}.{&KeyFelt5} = INT(ENTRY(5,TT_Elogg.Verdier,CHR(2)))
                   TT_{&Tabell}.{&KeyFelt6} = INT(ENTRY(6,TT_Elogg.Verdier,CHR(2)))
                   TT_{&Tabell}.{&KeyFelt7} = INT(ENTRY(7,TT_Elogg.Verdier,CHR(2))).

            IF (TT_{&Tabell}.StTypeId BEGINS "ARTIKKEL" AND bStatTilHK = FALSE) THEN. /* Ikke send. */
            ELSE
                EXPORT TT_{&Tabell}.EDato         
                      TT_{&Tabell}.ETid          
                      TT_{&Tabell}.BrukerID      
                      TT_{&Tabell}.RegistrertDato
                      TT_{&Tabell}.RegistrertTid 
                      TT_{&Tabell}.RegistrertAv  
                      TT_{&Tabell}.VVarekost     
                      TT_{&Tabell}.Butik         
                      TT_{&Tabell}.AntSolgt      
                      TT_{&Tabell}.BrekkAnt      
                      TT_{&Tabell}.IntAnt        
                      TT_{&Tabell}.ReklAnt       
                      TT_{&Tabell}.ReklLAnt      
                      TT_{&Tabell}.GjenkjopAnt   
                      TT_{&Tabell}.KjopAnt       
                      TT_{&Tabell}.OvAnt         
                      TT_{&Tabell}.JustAnt       
                      TT_{&Tabell}.JustVerdi     
                      TT_{&Tabell}.SvinnAnt      
                      TT_{&Tabell}.SvinnVerdi
                      TT_{&Tabell}.NedAnt        
                      TT_{&Tabell}.NedVerdi      
                      TT_{&Tabell}.VerdiSolgt    
                      TT_{&Tabell}.KjopVerdi     
                      TT_{&Tabell}.BrekkVerdi    
                      TT_{&Tabell}.IntVerdi      
                      TT_{&Tabell}.ReklVerdi     
                      TT_{&Tabell}.ReklLVerdi    
                      TT_{&Tabell}.GjenkjopVerdi 
                      TT_{&Tabell}.OvVerdi       
                      TT_{&Tabell}.DataObjekt    
                      TT_{&Tabell}.StTypeId      
                      TT_{&Tabell}.Beskrivelse   
                      TT_{&Tabell}.PerId         
                      TT_{&Tabell}.Aar           
                      TT_{&Tabell}.PerLinNr      
                      TT_{&Tabell}.MvaVerdi      
                      TT_{&Tabell}.Diverse       
                      TT_{&Tabell}.AntTilbSolgt  
                      TT_{&Tabell}.VerdiTilbSolgt
                      TT_{&Tabell}.TilbVVarekost 
                      TT_{&Tabell}.TilbMvaVerdi  
                      TT_{&Tabell}.AntRabatt     
                      TT_{&Tabell}.VerdiRabatt   
                      TT_{&Tabell}.LagerAnt      
                      TT_{&Tabell}.PrimoAnt      
                      TT_{&Tabell}.OmlHast       
                      TT_{&Tabell}.Hg            
                      TT_{&Tabell}.VisBut        
                      TT_{&Tabell}.PerLinTxt     
                      TT_{&Tabell}.DbKr          
                      TT_{&Tabell}.Db%           
                      TT_{&Tabell}.Utsolgt%      
                      TT_{&Tabell}.LagerVerdi    
                      TT_{&Tabell}.Primoverdi    
                      TT_{&Tabell}.DiverseAnt    
                      TT_{&Tabell}.Diverseverdi  
                      TT_{&Tabell}.TotalPost
                      .
        END.
    END.
    
    iAntNyEndre = 0.
    FOR EACH TT_ELogg WHERE TT_ELogg.TabellNavn = cTabellNavn AND TT_ELogg.EksterntSystem = "HK"
                        AND TT_Elogg.EndringsType = 1:
        IF (ENTRY(1,TT_Elogg.Verdier,CHR(2)) BEGINS "ARTIKKEL" AND bStatTilHK = FALSE) THEN. /* Ikke send. */
        ELSE iAntNyEndre = iAntNyEndre + 1.
    END.
    
    IF iAntNyEndre > 0 THEN DO:
        EXPORT "H" cTabellNavn 1 "1.0" iAntNyEndre.
        FOR EACH TT_ELogg WHERE TT_ELogg.TabellNavn = cTabellNavn AND TT_ELogg.EksterntSystem = "HK"
                            AND TT_Elogg.EndringsType = 1:
            FIND {&Tabell} WHERE {&Tabell}.{&KeyFelt}  = ENTRY(1,TT_Elogg.Verdier,CHR(2)) AND 
                                 {&Tabell}.{&KeyFelt2} = ENTRY(2,TT_Elogg.Verdier,CHR(2)) AND
                                 {&Tabell}.{&KeyFelt3} = ENTRY(3,TT_Elogg.Verdier,CHR(2)) AND
                                 {&Tabell}.{&KeyFelt4} = ENTRY(4,TT_Elogg.Verdier,CHR(2)) AND
                                 {&Tabell}.{&KeyFelt5} = INT(ENTRY(5,TT_Elogg.Verdier,CHR(2))) AND
                                 {&Tabell}.{&KeyFelt6} = INT(ENTRY(6,TT_Elogg.Verdier,CHR(2))) AND
                                 {&Tabell}.{&KeyFelt7} = INT(ENTRY(7,TT_Elogg.Verdier,CHR(2))) NO-LOCK NO-ERROR.
            IF ({&Tabell}.StTypeId BEGINS "ARTIKKEL" AND bStatTilHK = FALSE) THEN. /* Ikke send. */
            ELSE DO:
                FIND ArtBas NO-LOCK WHERE
                  ArtBas.ArtikkelNr = DECIMAL({&Tabell}.DataObjekt) NO-ERROR. 
                EXPORT {&Tabell}.EDato         
                       {&Tabell}.ETid          
                       {&Tabell}.BrukerID      
                       {&Tabell}.RegistrertDato
                       {&Tabell}.RegistrertTid 
                       {&Tabell}.RegistrertAv  
                       {&Tabell}.VVarekost     
                       {&Tabell}.Butik         
                       {&Tabell}.AntSolgt      
                       {&Tabell}.BrekkAnt      
                       {&Tabell}.IntAnt        
                       {&Tabell}.ReklAnt       
                       {&Tabell}.ReklLAnt      
                       {&Tabell}.GjenkjopAnt   
                       {&Tabell}.KjopAnt       
                       {&Tabell}.OvAnt         
                       {&Tabell}.JustAnt       
                       {&Tabell}.JustVerdi     
                       {&Tabell}.SvinnAnt      
                       {&Tabell}.SvinnVerdi   
                       {&Tabell}.NedAnt        
                       {&Tabell}.NedVerdi      
                       {&Tabell}.VerdiSolgt    
                       {&Tabell}.KjopVerdi     
                       {&Tabell}.BrekkVerdi    
                       {&Tabell}.IntVerdi      
                       {&Tabell}.ReklVerdi     
                       {&Tabell}.ReklLVerdi    
                       {&Tabell}.GjenkjopVerdi 
                       {&Tabell}.OvVerdi       
                       {&Tabell}.DataObjekt    
                       {&Tabell}.StTypeId      
                       {&Tabell}.Beskrivelse   
                       {&Tabell}.PerId         
                       {&Tabell}.Aar           
                       {&Tabell}.PerLinNr      
                       {&Tabell}.MvaVerdi      
                       {&Tabell}.Diverse       
                       {&Tabell}.AntTilbSolgt  
                       {&Tabell}.VerdiTilbSolgt
                       {&Tabell}.TilbVVarekost 
                       {&Tabell}.TilbMvaVerdi  
                       {&Tabell}.AntRabatt     
                       {&Tabell}.VerdiRabatt   
                       {&Tabell}.LagerAnt      
                       {&Tabell}.PrimoAnt      
                       {&Tabell}.OmlHast       
                       {&Tabell}.Hg            
                       {&Tabell}.VisBut        
                       {&Tabell}.PerLinTxt     
                       {&Tabell}.DbKr          
                       {&Tabell}.Db%           
                       {&Tabell}.Utsolgt%      
                       {&Tabell}.LagerVerdi    
                       {&Tabell}.Primoverdi    
                       {&Tabell}.DiverseAnt    
                       {&Tabell}.Diverseverdi  
                       {&Tabell}.TotalPost
                       (IF AVAILABLE ArtBas THEN ArtBas.Vg ELSE 0 )
                       (IF AVAILABLE ArtBas THEN ArtBas.Sasong ELSE 0 )
                       (IF AVAILABLE ArtBas THEN ArtBas.Farg ELSE 0 )
                       (IF AVAILABLE ArtBas THEN ArtBas.MatKod ELSE 0 )
                       (IF AVAILABLE ArtBas THEN ArtBas.Beskr ELSE '' )
                       (IF AVAILABLE ArtBas THEN ArtBas.LevNr ELSE 0 )
                       (IF AVAILABLE ArtBas THEN ArtBas.LevKod ELSE '' )
                       (IF AVAILABLE ArtBas THEN ArtBas.VmId ELSE 0 )
                       (IF AVAILABLE ArtBas THEN ArtBas.LevFargKod ELSE '' )
                       (IF AVAILABLE ArtBas THEN ArtBas.ProdNr ELSE 0 )
                       .
            END.               
        END.
    END.
    EMPTY TEMP-TABLE TT_{&Tabell}.
    &UNDEFINE Tabell
    &UNDEFINE KeyFelt
    &UNDEFINE KeyFelt2
    &UNDEFINE KeyFelt3
    &UNDEFINE KeyFelt4
    &UNDEFINE KeyFelt5
    &UNDEFINE KeyFelt6
    &UNDEFINE KeyFelt7
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SendTimeDag) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SendTimeDag Procedure 
PROCEDURE SendTimeDag :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    &SCOPED-DEFINE Tabell   timedag
    &SCOPED-DEFINE KeyFelt  butnr
    &SCOPED-DEFINE KeyFelt2 dato
    
    DEFINE VARIABLE iAntSlett   AS INTEGER    NO-UNDO.
    DEFINE VARIABLE iAntNyEndre AS INTEGER    NO-UNDO.
    DEFINE VARIABLE cTabellNavn AS CHARACTER  INIT "{&Tabell}" NO-UNDO.
    
    EMPTY TEMP-TABLE TT_{&Tabell}.
    RUN KopierElogg (0,cTabellNavn,OUTPUT iAntSlett,OUTPUT iAntNyEndre).

    iAntSlett = 0.
    FOR EACH TT_ELogg WHERE TT_ELogg.TabellNavn = cTabellNavn AND TT_ELogg.EksterntSystem = "HK"
                        AND TT_Elogg.EndringsType = 3:
        iAntSlett = iAntSlett + 1.
    END.
    
    IF iAntSlett > 0 THEN DO:
        EXPORT "H" cTabellNavn 3 "1.0" iAntSlett.
        CREATE TT_{&Tabell}.
        FOR EACH TT_ELogg WHERE TT_ELogg.TabellNavn = cTabellNavn AND TT_ELogg.EksterntSystem = "HK"
                            AND TT_Elogg.EndringsType = 3:
            ASSIGN TT_{&Tabell}.{&KeyFelt}  = INT(ENTRY(1,TT_Elogg.Verdier,CHR(1)))
                   TT_{&Tabell}.{&KeyFelt2} = DATE(ENTRY(2,TT_Elogg.Verdier,CHR(1))).
            EXPORT TT_{&Tabell}.
        END.
    END.
    
    iAntNyEndre = 0.
    FOR EACH TT_ELogg WHERE TT_ELogg.TabellNavn = cTabellNavn AND TT_ELogg.EksterntSystem = "HK"
                        AND TT_Elogg.EndringsType = 1:
        iAntNyEndre = iAntNyEndre + 1.
    END.
    
    IF iAntNyEndre > 0 THEN DO:
        EXPORT "H" cTabellNavn 1 "1.0" iAntNyEndre.
        FOR EACH TT_ELogg WHERE TT_ELogg.TabellNavn = cTabellNavn AND TT_ELogg.EksterntSystem = "HK"
                            AND TT_Elogg.EndringsType = 1:
            FIND {&Tabell} WHERE {&Tabell}.{&KeyFelt}  = INT(ENTRY(1,TT_Elogg.Verdier,CHR(1))) AND 
                                 {&Tabell}.{&KeyFelt2} = DATE(ENTRY(2,TT_Elogg.Verdier,CHR(1))) NO-LOCK NO-ERROR.
            IF AVAIL {&Tabell} THEN
                EXPORT {&Tabell}.
        END.
    END.
    EMPTY TEMP-TABLE TT_{&Tabell}.
    &UNDEFINE Tabell
    &UNDEFINE KeyFelt
    &UNDEFINE KeyFelt2
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SendTranslogg) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SendTranslogg Procedure 
PROCEDURE SendTranslogg :
/*------------------------------------------------------------------------------
                        Purpose:                                                                                                                                          
                        Notes:                                                                                                                                            
        ------------------------------------------------------------------------------*/
    &SCOPED-DEFINE Tabell   Translogg
    &SCOPED-DEFINE KeyFelt  Butik
    &SCOPED-DEFINE KeyFelt2 TransNr
    &SCOPED-DEFINE KeyFelt3 SeqNr
    
    DEFINE VARIABLE iAntSlett   AS INTEGER    NO-UNDO.
    DEFINE VARIABLE iAntNyEndre AS INTEGER    NO-UNDO.
    DEFINE VARIABLE cTabellNavn AS CHARACTER  INIT "{&Tabell}" NO-UNDO.
    
    EMPTY TEMP-TABLE TT_{&Tabell}.
    RUN KopierElogg (0,cTabellNavn,OUTPUT iAntSlett,OUTPUT iAntNyEndre).

    iAntNyEndre = 0.
    FOR EACH TT_ELogg WHERE TT_ELogg.TabellNavn = cTabellNavn AND TT_ELogg.EksterntSystem = "HK"
                        AND TT_Elogg.EndringsType = 1:
        iAntNyEndre = iAntNyEndre + 1.
    END.
    
    IF iAntNyEndre > 0 AND bTransloggTilHk THEN DO:
        EXPORT "H" cTabellNavn 1 "1.0" iAntNyEndre.
        FOR EACH TT_ELogg WHERE TT_ELogg.TabellNavn = cTabellNavn AND TT_ELogg.EksterntSystem = "HK"
                            AND TT_Elogg.EndringsType = 1:
            FIND {&Tabell} WHERE {&Tabell}.{&KeyFelt}  = INT(ENTRY(1,TT_Elogg.Verdier,CHR(1))) AND 
                                 {&Tabell}.{&KeyFelt2} = INT(ENTRY(2,TT_Elogg.Verdier,CHR(1))) AND
                                 {&Tabell}.{&KeyFelt3} = INT(ENTRY(3,TT_Elogg.Verdier,CHR(1))) NO-LOCK NO-ERROR.
            IF AVAIL {&Tabell} THEN
                EXPORT 
    {&Tabell}.BatchNr
    {&Tabell}.Butik
    {&Tabell}.TransNr
    {&Tabell}.ForsNr
    {&Tabell}.TTId
    {&Tabell}.TBId
    {&Tabell}.ArtikkelNr
    {&Tabell}.LevNr
    {&Tabell}.RegistrertDato
    {&Tabell}.RegistrertTid
    {&Tabell}.RegistrertAv
    {&Tabell}.BongId
    {&Tabell}.BongLinjeNr
    {&Tabell}.KassaNr
    {&Tabell}.Vg
    {&Tabell}.LopNr
    {&Tabell}.Storl
    {&Tabell}.Antall
    {&Tabell}.Pris
    {&Tabell}.RabKr
    {&Tabell}.Mva
    {&Tabell}.Plukket
    {&Tabell}.Dato
    {&Tabell}.Tid
    {&Tabell}.Postert
    {&Tabell}.PostertDato
    {&Tabell}.PostertTid
    {&Tabell}.BestNr
    {&Tabell}.OvButik
    {&Tabell}.OvTransNr
    {&Tabell}.SeqNr
    {&Tabell}.FeilKode
    {&Tabell}.TilStorl
    {&Tabell}.VVarekost
    {&Tabell}.SattVVareKost
    {&Tabell}.MedlemsNr
    {&Tabell}.KortNr
    {&Tabell}.KortType
    {&Tabell}.KundNr
    {&Tabell}.KalkylePris
    {&Tabell}.ProfilNr
    {&Tabell}.SelgerNr
    {&Tabell}.SubtotalRab
    {&Tabell}.RefTekst
    {&Tabell}.Kode
    {&Tabell}.RefNr
    {&Tabell}.Ordreforslag
    {&Tabell}.LinjeRab
    {&Tabell}.PersonalRab
    {&Tabell}.BongTekst
    {&Tabell}.NegLager
    {&Tabell}.individnr
    {&Tabell}.Mva%
    {&Tabell}.Varekost
    {&Tabell}.KampId
    {&Tabell}.KampEierId
    {&Tabell}.KampTilbId.                       
        END.
    END.
    EMPTY TEMP-TABLE TT_{&Tabell}.
    &UNDEFINE Tabell
    &UNDEFINE KeyFelt
    &UNDEFINE KeyFelt2
    &UNDEFINE KeyFelt3
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SendVareDag) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SendVareDag Procedure 
PROCEDURE SendVareDag :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    &SCOPED-DEFINE Tabell   varedag
    &SCOPED-DEFINE KeyFelt  butnr
    &SCOPED-DEFINE KeyFelt2 ean
    &SCOPED-DEFINE KeyFelt3 dato
    DEFINE VARIABLE iAntSlett   AS INTEGER    NO-UNDO.
    DEFINE VARIABLE iAntNyEndre AS INTEGER    NO-UNDO.
    DEFINE VARIABLE cTabellNavn AS CHARACTER  INIT "{&Tabell}" NO-UNDO.
    EMPTY TEMP-TABLE TT_{&Tabell}.
    RUN KopierElogg (0,cTabellNavn,OUTPUT iAntSlett,OUTPUT iAntNyEndre).
    IF iAntSlett > 0 THEN DO:
        EXPORT "H" cTabellNavn 3 "1.0" iAntSlett.
        CREATE TT_{&Tabell}.
        FOR EACH TT_ELogg WHERE TT_ELogg.TabellNavn = cTabellNavn AND TT_ELogg.EksterntSystem = "HK"
                            AND TT_Elogg.EndringsType = 3:
            ASSIGN TT_{&Tabell}.{&KeyFelt}  = INT(ENTRY(1,TT_Elogg.Verdier,CHR(1)))
                   TT_{&Tabell}.{&KeyFelt2} = DECI(ENTRY(2,TT_Elogg.Verdier,CHR(1)))
                   TT_{&Tabell}.{&KeyFelt3} = DATE(ENTRY(3,TT_Elogg.Verdier,CHR(1))).
            EXPORT TT_{&Tabell}.butnr   
                   TT_{&Tabell}.ean     
                   TT_{&Tabell}.dato    
                   TT_{&Tabell}.antall  
                   TT_{&Tabell}.kostpris
                   TT_{&Tabell}.mvakr   
                   TT_{&Tabell}.salgssum
                   TT_{&Tabell}.antkamp 
                   TT_{&Tabell}.kostkamp
                   TT_{&Tabell}.mvakamp 
                   TT_{&Tabell}.salgkamp
                   TT_{&Tabell}.antmix  
                   TT_{&Tabell}.kostmix 
                   TT_{&Tabell}.mvamix  
                   TT_{&Tabell}.salgmix 
                   TT_{&Tabell}.antmed  
                   TT_{&Tabell}.kostmed 
                   TT_{&Tabell}.mvamed  
                   TT_{&Tabell}.salgmed 
                   TT_{&Tabell}.medrabkr
                   TT_{&Tabell}.kunrabkr
                   TT_{&Tabell}.perrabkr
                   TT_{&Tabell}.genrabkr.
        END.
    END.
    IF iAntNyEndre > 0 THEN DO:
        EXPORT "H" cTabellNavn 1 "1.0" iAntNyEndre.
        FOR EACH TT_ELogg WHERE TT_ELogg.TabellNavn = cTabellNavn AND TT_ELogg.EksterntSystem = "HK"
                            AND TT_Elogg.EndringsType = 1:
            FIND {&Tabell} WHERE {&Tabell}.{&KeyFelt}  = INT(ENTRY(1,TT_Elogg.Verdier,CHR(1))) AND 
                                 {&Tabell}.{&KeyFelt2} = DECI(ENTRY(2,TT_Elogg.Verdier,CHR(1))) AND
                                 {&Tabell}.{&KeyFelt3} = DATE(ENTRY(3,TT_Elogg.Verdier,CHR(1))) NO-LOCK NO-ERROR.
            IF AVAIL {&Tabell} THEN
                EXPORT {&Tabell}.butnr   
                       {&Tabell}.ean     
                       {&Tabell}.dato    
                       {&Tabell}.antall  
                       {&Tabell}.kostpris
                       {&Tabell}.mvakr   
                       {&Tabell}.salgssum
                       {&Tabell}.antkamp 
                       {&Tabell}.kostkamp
                       {&Tabell}.mvakamp 
                       {&Tabell}.salgkamp
                       {&Tabell}.antmix  
                       {&Tabell}.kostmix 
                       {&Tabell}.mvamix  
                       {&Tabell}.salgmix 
                       {&Tabell}.antmed  
                       {&Tabell}.kostmed 
                       {&Tabell}.mvamed  
                       {&Tabell}.salgmed 
                       {&Tabell}.medrabkr
                       {&Tabell}.kunrabkr
                       {&Tabell}.perrabkr
                       {&Tabell}.genrabkr.
        END.
    END.
    EMPTY TEMP-TABLE TT_{&Tabell}.
    &UNDEFINE Tabell
    &UNDEFINE KeyFelt
    &UNDEFINE KeyFelt2
    &UNDEFINE KeyFelt3
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SkapaTTELoggAlle) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SkapaTTELoggAlle Procedure 
PROCEDURE SkapaTTELoggAlle :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  
  Notes:      iAntNyEndre har ett värde med sig 
------------------------------------------------------------------------------*/
  DEFINE INPUT-OUTPUT PARAMETER iAntNyEndre AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER cTabell AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cVerdi AS CHARACTER  NO-UNDO.
  CASE cTabell:
      WHEN "Gruppe" THEN DO:
          FOR EACH Gruppe NO-LOCK:
              ASSIGN cVerdi = STRING(Gruppe.ButikkNr) + CHR(1) + STRING(Gruppe.GruppeNr).
              IF NOT CanFindTTElogg(cTabell,cVerdi) THEN DO:
                  RUN NyTTElogg(cTabell,cVerdi).
                  ASSIGN iAntNyEndre = iAntNyEndre + 1.
              END.
          END.
      END.
      WHEN "Kasse" THEN DO:
          FOR EACH Kasse NO-LOCK:
              ASSIGN cVerdi = STRING(Kasse.ButikkNr) + CHR(1) + STRING(Kasse.GruppeNr) + CHR(1) + STRING(Kasse.KasseNr).
              IF NOT CanFindTTElogg(cTabell,cVerdi) THEN DO:
                  RUN NyTTElogg(cTabell,cVerdi).
                  ASSIGN iAntNyEndre = iAntNyEndre + 1.
              END.
          END.
      END.
      WHEN "Forsalj" THEN DO:
          FOR EACH Forsalj NO-LOCK:
              ASSIGN cVerdi = STRING(Forsalj.ForsNr).
              IF NOT CanFindTTElogg(cTabell,cVerdi) THEN DO:
                  RUN NyTTElogg(cTabell,cVerdi).
                  ASSIGN iAntNyEndre = iAntNyEndre + 1.
              END.
          END.
      END.
      WHEN "Selger" THEN DO:
          FOR EACH Selger NO-LOCK:
              ASSIGN cVerdi = STRING(Selger.SelgerNr).
              IF NOT CanFindTTElogg(cTabell,cVerdi) THEN DO:
                  RUN NyTTElogg(cTabell,cVerdi).
                  ASSIGN iAntNyEndre = iAntNyEndre + 1.
              END.
          END.
      END.
      WHEN "HgrDag" THEN DO:
          FOR EACH hgrdag NO-LOCK:
              ASSIGN cVerdi = STRING(hgrdag.butnr) + CHR(1) + STRING(hgrdag.hg) + CHR(1) + STRING(hgrdag.dato).
              IF NOT CanFindTTElogg(cTabell,cVerdi) THEN DO:
                  RUN NyTTElogg(cTabell,cVerdi).
                  ASSIGN iAntNyEndre = iAntNyEndre + 1.
              END.
          END.
      END.
      WHEN "TimeDag" THEN DO:
          FOR EACH timedag NO-LOCK:
              ASSIGN cVerdi = STRING(timedag.butnr) + CHR(1) + STRING(timedag.dato).
              IF NOT CanFindTTElogg(cTabell,cVerdi) THEN DO:
                  RUN NyTTElogg(cTabell,cVerdi).
                  ASSIGN iAntNyEndre = iAntNyEndre + 1.
              END.
          END.
      END.
      WHEN "VareDag" THEN DO:
          FOR EACH varedag NO-LOCK:
              ASSIGN cVerdi = STRING(varedag.butnr) + CHR(1) + STRING(varedag.ean) + CHR(1) + STRING(varedag.dato).
              IF NOT CanFindTTElogg(cTabell,cVerdi) THEN DO:
                  RUN NyTTElogg(cTabell,cVerdi).
                  ASSIGN iAntNyEndre = iAntNyEndre + 1.
              END.
          END.
      END.
      WHEN "akt_rapp" THEN DO:
          FOR EACH akt_rapp NO-LOCK:
              ASSIGN cVerdi = STRING(akt_rapp.dato) + CHR(1) + STRING(akt_rapp.butik) + CHR(1) + STRING(akt_rapp.kasse) + CHR(1) + STRING(akt_rapp.tid).
              IF NOT CanFindTTElogg(cTabell,cVerdi) THEN DO:
                  RUN NyTTElogg(cTabell,cVerdi).
                  ASSIGN iAntNyEndre = iAntNyEndre + 1.
              END.
          END.
      END.
      WHEN "dags_rap" THEN DO:
          FOR EACH dags_rap NO-LOCK:
              ASSIGN cVerdi = STRING(dags_rap.butikk) + CHR(1) + STRING(dags_rap.dato).
              IF NOT CanFindTTElogg(cTabell,cVerdi) THEN DO:
                  RUN NyTTElogg(cTabell,cVerdi).
                  ASSIGN iAntNyEndre = iAntNyEndre + 1.
              END.
          END.
      END.
      WHEN "kas_rap" THEN DO:
          FOR EACH kas_rap NO-LOCK:
              ASSIGN cVerdi = STRING(kas_rap.dato) + CHR(1) + STRING(kas_rap.butikk) + CHR(1) + STRING(kas_rap.kasse) + CHR(1) + STRING(kas_rap.z_nummer).
              IF NOT CanFindTTElogg(cTabell,cVerdi) THEN DO:
                  RUN NyTTElogg(cTabell,cVerdi).
                  ASSIGN iAntNyEndre = iAntNyEndre + 1.
              END.
          END.
      END.
      WHEN "konto" THEN DO:
          FOR EACH konto NO-LOCK:
              ASSIGN cVerdi = STRING(konto.dato) + CHR(1) + STRING(konto.butikk) + CHR(1) + STRING(konto.kontonummer).
              IF NOT CanFindTTElogg(cTabell,cVerdi) THEN DO:
                  RUN NyTTElogg(cTabell,cVerdi).
                  ASSIGN iAntNyEndre = iAntNyEndre + 1.
              END.
          END.
      END.
      WHEN "Kort_Spes" THEN DO:
          FOR EACH Kort_Spes NO-LOCK:
              ASSIGN cVerdi = STRING(Kort_Spes.dato) + CHR(1) + STRING(Kort_Spes.butikk) + CHR(1) + STRING(Kort_Spes.kasse) + CHR(1) + STRING(Kort_Spes.z_nummer) + CHR(1) + STRING(Kort_Spes.KortType).
              IF NOT CanFindTTElogg(cTabell,cVerdi) THEN DO:
                  RUN NyTTElogg(cTabell,cVerdi).
                  ASSIGN iAntNyEndre = iAntNyEndre + 1.
              END.
          END.
      END.
      WHEN "KassererBilag" THEN DO:
          FOR EACH KassererBilag NO-LOCK:
              ASSIGN cVerdi = STRING(KassererBilag.ButikkNr) + CHR(1) + STRING(KassererBilag.Dato) + CHR(1) + STRING(KassererBilag.KassererNr) + CHR(1) + STRING(KassererBilag.z_nummer) + CHR(1) + STRING(KassererBilag.BilagsNr).
              IF NOT CanFindTTElogg(cTabell,cVerdi) THEN DO:
                  RUN NyTTElogg(cTabell,cVerdi).
                  ASSIGN iAntNyEndre = iAntNyEndre + 1.
              END.
          END.
      END.
      WHEN "KassererDag" THEN DO:
          FOR EACH KassererDag NO-LOCK:
              ASSIGN cVerdi = STRING(kassererDag.ButikkNr) + CHR(1) + STRING(kassererDag.Dato) + CHR(1) + STRING(kassererDag.KassererNr) + CHR(1) + STRING(kassererDag.z_nummer).
              IF NOT CanFindTTElogg(cTabell,cVerdi) THEN DO:
                  RUN NyTTElogg(cTabell,cVerdi).
                  ASSIGN iAntNyEndre = iAntNyEndre + 1.
              END.
          END.
      END.
      WHEN "KassererKontanter" THEN DO:
          FOR EACH KassererKontanter NO-LOCK:
              ASSIGN cVerdi = STRING(KassererKontanter.ButikkNr) + CHR(1) + STRING(KassererKontanter.Dato) + CHR(1) + STRING(KassererKontanter.KassererNr) + CHR(1) + STRING(KassererKontanter.z_nummer).
              IF NOT CanFindTTElogg(cTabell,cVerdi) THEN DO:
                  RUN NyTTElogg(cTabell,cVerdi).
                  ASSIGN iAntNyEndre = iAntNyEndre + 1.
              END.
          END.
      END.
      WHEN "kassererOppgj" THEN DO:
          FOR EACH kassererOppgj NO-LOCK:
              ASSIGN cVerdi = STRING(kassererOppgj.ButikkNr) + CHR(1) + STRING(kassererOppgj.Dato) + CHR(1) + STRING(kassererOppgj.KassererNr) + CHR(1) + STRING(kassererOppgj.z_nummer).
              IF NOT CanFindTTElogg(cTabell,cVerdi) THEN DO:
                  RUN NyTTElogg(cTabell,cVerdi).
                  ASSIGN iAntNyEndre = iAntNyEndre + 1.
              END.
          END.
      END.
      WHEN "KassererValuta" THEN DO:
          FOR EACH KassererValuta NO-LOCK:
              ASSIGN cVerdi = STRING(KassererValuta.ButikkNr) + CHR(1) + STRING(KassererValuta.KasseNr) + CHR(1) + STRING(KassererValuta.Dato) + CHR(1) + STRING(KassererValuta.KassererNr) + CHR(1) + STRING(KassererValuta.z_nummer) + CHR(1) + STRING(KassererValuta.ValKod).
              IF NOT CanFindTTElogg(cTabell,cVerdi) THEN DO:
                  RUN NyTTElogg(cTabell,cVerdi).
                  ASSIGN iAntNyEndre = iAntNyEndre + 1.
              END.
          END.
      END.
      WHEN "BokforingsBilag" THEN DO:
          FOR EACH BokforingsBilag NO-LOCK:
              ASSIGN cVerdi = STRING(BokforingsBilag.ButikkNr) + CHR(1) + STRING(BokforingsBilag.Aar) + CHR(1) + STRING(BokforingsBilag.BokforingsNr).
              IF NOT CanFindTTElogg(cTabell,cVerdi) THEN DO:
                  RUN NyTTElogg(cTabell,cVerdi).
                  ASSIGN iAntNyEndre = iAntNyEndre + 1.
              END.
          END.
      END.
      WHEN "StLinje" THEN DO:
          FOR EACH StLinje NO-LOCK:
              IF StLinje.StTypeId = "ARTIKKEL" AND DECI(StLinje.DataObjekt) < 8500000 THEN
                  NEXT.
               /* Vi använder CHR(2) i stf CHR(1), Vissa ststistiker innhåller */
               /* CHR(1) i DataObjekt ex. SELGER-VG */
              ASSIGN cVerdi = StLinje.StTypeId + CHR(2) + Stlinje.PerId + CHR(2) + StLinje.DataObjekt + CHR(2) + 
                              StLinje.Diverse  + CHR(2) + STRING(StLinje.Butik) + CHR(2) + 
                              STRING(StLinje.Aar) + CHR(2) + STRING(StLinje.PerLinNr).
              IF NOT CanFindTTElogg(cTabell,cVerdi) THEN DO:
                  RUN NyTTElogg(cTabell,cVerdi).
                  ASSIGN iAntNyEndre = iAntNyEndre + 1.
              END.
          END.
      END.
  END CASE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SlettBehandlet) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SlettBehandlet Procedure 
PROCEDURE SlettBehandlet :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    FOR EACH Elogg WHERE ELogg.EksterntSystem = "HK" AND
                         Elogg.Behandlet = TRUE:
        DELETE Elogg.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-StartEksport) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE StartEksport Procedure 
PROCEDURE StartEksport :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cFilnavn           AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cNumericFormat     AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cDateFormat        AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cTmpFil            AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cCL                AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cSendesFil AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cKatalog           AS CHAR       NO-UNDO.

  ASSIGN cNumericFormat         = SESSION:NUMERIC-FORMAT
         cDateFormat            = SESSION:DATE-FORMAT
         SESSION:NUMERIC-FORMAT = "EUROPEAN"
         SESSION:DATE-FORMAT    = "dmy".
         
  {syspara.i 5 1 1 cCL}  /* centrallager */
  {syspara.i 1 1 51 cKatalog}
  IF cKatalog = '' THEN
      cKatalog = "C:\home\lindbak\sendes\".
  cKatalog = RIGHT-TRIM(cKatalog,'\') + '\'.

  ASSIGN cFilnavn   = "POS" + STRING(YEAR(TODAY)) + STRING(MONTH(TODAY),"99") + STRING(DAY(TODAY),"99") + "_" + REPLACE(STRING(TIME,"HH:MM:SS"),":","") + "." + cCL
         cTmpFil    = SESSION:TEMP-DIRECTORY + cFilNavn
         cSendesFil = cKatalog + cFilNavn.
         
  EMPTY TEMP-TABLE TT_Elogg.
  OUTPUT TO VALUE(cTmpFil).
  DO:
      IF FinnsElogg("Gruppe") THEN
          RUN SendGruppe.
      IF FinnsElogg("Kasse") THEN
          RUN SendKasse.
      IF FinnsElogg("Forsalj") THEN
          RUN SendForsalj.
      IF FinnsElogg("Selger") THEN
          RUN SendSelger.
      IF FinnsElogg("hgrdag") THEN
          RUN SendHgrDag.
      IF FinnsElogg("timedag") THEN
          RUN SendTimeDag.
      IF FinnsElogg("varedag") THEN
          RUN SendVareDag.
      IF FinnsElogg("akt_rapp") THEN
          RUN SendAkt_Rapp.
      IF FinnsElogg("dags_rap") THEN
          RUN SendDags_Rap.
      IF FinnsElogg("kas_rap") THEN
          RUN SendKas_Rap.
/*       tillsvidare ingen loggning, indexproblem, inget unikt; vad skall vi anvænda den till? */
/*       IF FinnsElogg("konto") THEN                                                           */
/*           RUN SendKonto.                                                                    */
      IF FinnsElogg("Kort_Spes") THEN
          RUN SendKort_Spes.
/*       tillsvidare ingen loggning        */
/*       IF FinnsElogg("kassererDag") THEN */
/*           RUN SendKassererDag.          */
      IF FinnsElogg("KassererBilag") THEN
          RUN SendKassererBilag.
      IF FinnsElogg("KassererKontanter") THEN
          RUN SendKassererKontanter.
      IF FinnsElogg("kassererOppgj") THEN
          RUN SendKassererOppgj.
      IF FinnsElogg("KassererValuta") THEN
          RUN SendKassererValuta.
      IF FinnsElogg("BokforingsBilag") THEN
          RUN SendBokforingsBilag.
      IF FinnsElogg("StLinje") THEN
          RUN SendStLinje.
      IF FinnsElogg("Lager") THEN  /* skall behandlas efter stlinje */
          RUN SendLager.
      IF FinnsElogg("ArtLag") THEN  /* skall behandlas efter stlinje */
          RUN SendArtLag.
      IF FinnsElogg("StLager") THEN  /* skall behandlas efter stlinje */
          RUN SendStLager.
      IF FinnsElogg("Translogg") THEN  /* skall behandlas efter stlinje */
          RUN SendTranslogg.
  END.
  OUTPUT CLOSE.

  ASSIGN SESSION:NUMERIC-FORMAT = cNumericFormat
         SESSION:DATE-FORMAT    = cDateFormat.
  FILE-INFO:FILE-NAME = cTmpFil.
  IF FILE-INFO:FILE-SIZE > 0 THEN DO:
      OS-COPY VALUE(cTmpFil) VALUE(cSendesFil).
  END.
  OS-DELETE VALUE(cTmpFil).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

/* ************************  Function Implementations ***************** */

&IF DEFINED(EXCLUDE-CanFindTTElogg) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION CanFindTTElogg Procedure 
FUNCTION CanFindTTElogg RETURNS LOGICAL
  ( INPUT cTabell AS CHARACTER,INPUT cVerdi AS CHARACTER ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  RETURN CAN-FIND(FIRST TT_ELogg WHERE TT_ELogg.TabellNavn = cTabell AND
                                    TT_ELogg.EksterntSystem = "HK" AND
                                    TT_ELogg.Verdier = cVerdi).

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-FinnsElogg) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION FinnsElogg Procedure 
FUNCTION FinnsElogg RETURNS LOGICAL
  ( INPUT cTabellNavn AS CHARACTER ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  
  RETURN CAN-FIND(FIRST ELogg WHERE ELogg.TabellNavn = cTabellNavn AND
                                    ELogg.EksterntSystem = "HK").   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

