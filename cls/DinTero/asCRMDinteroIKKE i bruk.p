&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : asCRMDintero.p (Fra asMayflower.p) 
    Purpose     :

    Syntax      :

    Description :

    Author(s)   : Tom Nøkleby
    Created     : 1/11-20
    Notes       : Interface fra POS mot Dintero.
    
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEF INPUT  PARAMETER cMethod     AS CHAR NO-UNDO.
DEF INPUT  PARAMETER cFunction   AS CHAR NO-UNDO.
DEF INPUT  PARAMETER cSearchType AS CHAR NO-UNDO.
DEF INPUT  PARAMETER cParam      AS CHAR NO-UNDO.
DEF INPUT-OUTPUT  PARAMETER TABLE-HANDLE hParam . 

DEF OUTPUT PARAMETER obOk        AS LOG  NO-UNDO. /* Gikk kall fra POS bra (Bare kallet, ikke svaret)    */
DEF OUTPUT PARAMETER ocReturn    AS CHAR NO-UNDO. /* Eventuelle feilmeldinger                            */
DEF OUTPUT PARAMETER ocRetParam  AS CHAR NO-UNDO. /* Pipe separerte Parameterverdier som kassen skal ha. */
                        
DEF VAR hFunctionService AS HANDLE NO-UNDO.

DEF VAR cMemberId           AS CHAR NO-UNDO. /* Medlemsnr. Kassen sender inn telefonnr.            */

DEF VAR lConError        AS LOG NO-UNDO.
DEF VAR gcErrorMessage AS CHAR NO-UNDO. 

DEF VAR cReq             AS CHAR NO-UNDO. 
DEF VAR cTekst           AS CHAR NO-UNDO.
DEF VAR lok              AS LOGICAL NO-UNDO. 
/* Sax */

/* Initieres fra systemparametre */
DEF VAR cUsername        AS CHARACTER NO-UNDO.
DEF VAR cPassword        AS CHARACTER NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&IF DEFINED(EXCLUDE-getNextidNumber) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getNextidNumber Procedure 
FUNCTION getNextidNumber RETURNS INTEGER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-isMayFlowerMember) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD isMayFlowerMember Procedure 
FUNCTION isMayFlowerMember RETURNS LOGICAL
 (INPUT EksterntMedlemsNr AS CHAR , OUTPUT iMayFlowerid AS INT ) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-isRegisteredFromEcom) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD isRegisteredFromEcom Procedure 
FUNCTION isRegisteredFromEcom RETURNS LOGICAL
    (INPUT mobiltlf AS CHAR , INPUT  email AS CHAR ) FORWARD.

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
   Other Settings: CODE-ONLY
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Procedure ASSIGN
         HEIGHT             = 24.57
         WIDTH              = 84.4.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

DEFINE VARIABLE hParameterTable AS HANDLE NO-UNDO. 
LOG-MANAGER:LOGGING-LEVEL = 3.

hParameterTable = hParam:DEFAULT-BUFFER-HANDLE.
IF NOT hParameterTable:AVAIL THEN hParameterTable:FIND-FIRST() NO-ERROR.


/* Call main method/Service Member/Check/Transaction/Offer */
RUN VALUE(cMethod) (cFunction,cSearchType,cParam, OUTPUT obok, OUTPUT ocReturn) NO-ERROR.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-POSData) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE POSData Procedure 
PROCEDURE POSData :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

   DEFINE INPUT  PARAMETER cFunction     AS CHAR NO-UNDO. 
   DEFINE INPUT  PARAMETER cSearchType   AS CHAR NO-UNDO. 
   DEFINE INPUT  PARAMETER cParameter    AS CHAR NO-UNDO. 

   DEFINE OUTPUT PARAMETER obok      AS LOG NO-UNDO.
   DEFINE OUTPUT PARAMETER ocReturn  AS CHAR NO-UNDO. 

   LOG-MANAGER:WRITE-MESSAGE("POSData."  + cFunction + ":Search " + cSearchType + ":" + cParameter ,"WSERROR").
   RUN VALUE('POSData.' + cFunction) (cSearchType,cParameter, OUTPUT obOk, OUTPUT ocReturn) .
   IF ERROR-STATUS:ERROR THEN
       ocReturn = 'Feil funksjonsnavn eller parametre send til MemberService.' + cFunction + '.'. 


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-POSData.getMember) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE POSData.getMember Procedure 
PROCEDURE POSData.getMember :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER ipcSearchType  AS CHAR NO-UNDO.
DEFINE INPUT  PARAMETER picParam  AS CHAR NO-UNDO.
DEFINE OUTPUT PARAMETER pobOk     AS LOG  NO-UNDO.
DEFINE OUTPUT PARAMETER pocReturn AS CHAR NO-UNDO.

DEFINE VARIABLE hbf AS HANDLE NO-UNDO. 
DEFINE VARIABLE wslOk AS LOG NO-UNDO. 
DEFINE VARIABLE wsRet AS CHAR NO-UNDO. 
DEFINE VARIABLE lMedlemsNr AS INT NO-UNDO.
DEFINE VARIABLE cKortNr AS CHAR NO-UNDO. 
DEFINE VARIABLE cPostSted AS CHAR NO-UNDO. 

 
hBf = hParam:DEFAULT-BUFFER-HANDLE.

 IF ipcSearchType = "TelephoneNumber" OR ipcSearchType = "" THEN
     FIND FIRST Medlem NO-LOCK WHERE
               Medlem.MobilTlf = picParam NO-ERROR.

 ELSE IF ipcSearchType = "MemberNumber" THEN
     FIND FIRST Medlem NO-LOCK WHERE
                Medlem.MedlemsNr = INTEGER(picParam)   
                NO-ERROR.


 IF AVAILABLE Medlem THEN
 DO:
    cPostSted = "". 
    FIND FIRST post WHERE
               post.PostNr      = Medlem.PostNr NO-LOCK NO-ERROR. 
    
    IF AVAIL post THEN
    cPostSted = post.beskrivelse. 

    IF NOT hbf:AVAIL THEN hbf:BUFFER-CREATE().

    hbf::MedlemsNr         = Medlem.MedlemsNr.
    hbf::Adresse1          = Medlem.Adresse1.
    hbf::Adresse2          = Medlem.Adresse2.
    hbf::ePostAdresse      = Medlem.ePostAdresse.
    hbf::ForNavn           = Medlem.ForNavn.
    hbf::EtterNavn         = Medlem.EtterNavn.
    hbf::PostNr            = Medlem.PostNr.
    hbf::MobilTlf          = Medlem.MobilTlf.
    hbf::Kjonn             = Medlem.Kjonn.
    hbf::EksterntMedlemsNr = Medlem.EksterntMedlemsNr.
    hbf::beskrivelse       = cPostSted.
    hbf::gift              = Medlem.Bonus_Berettiget NO-ERROR.
    /* mangler på kasse -- må legges inn curt - bug */
    hbf::FodselsDato       = Medlem.FodselsDato NO-ERROR. 
                                                    
    pobOk = TRUE.  
 END. 

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-POSData.insertMember) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE POSData.insertMember Procedure 
PROCEDURE POSData.insertMember :
/*------------------------------------------------------------------------------
                Purpose:                                                                                                                                          
                Notes:  
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER ipcSearchType  AS CHAR NO-UNDO.
DEFINE INPUT  PARAMETER picParam  AS CHAR NO-UNDO.
DEFINE OUTPUT PARAMETER pobOk     AS LOG  NO-UNDO.
DEFINE OUTPUT PARAMETER pocReturn AS CHAR NO-UNDO.

DEFINE VARIABLE cButKlubbListe AS CHARACTER NO-UNDO.
DEFINE VARIABLE cLengdeListe AS CHARACTER NO-UNDO.
DEFINE VARIABLE cMKlubbId AS CHARACTER NO-UNDO.
DEFINE VARIABLE iMedlemsNr AS INT NO-UNDO. 
DEFINE VARIABLE lMedlemsNr AS DECIMAL NO-UNDO.
DEFINE VARIABLE cKortNr AS CHARACTER NO-UNDO.
DEFINE VARIABLE iKortNr AS INT NO-UNDO. 
DEFINE VARIABLE lMayflowerMember AS LOGICAL NO-UNDO. 

DEFINE BUFFER medlem FOR medlem. 
DEFINE BUFFER medlemskort FOR medlemskort. 
DISABLE TRIGGERS FOR DUMP OF medlem. 
DISABLE TRIGGERS FOR LOAD OF medlem. 


FIND FIRST  SysPara NO-LOCK WHERE
            SysPara.SysHId = 14 AND
            SysPara.SysGr  = 1 AND
            SysPara.ParaNr >= 31 AND 
            SysPara.ParaNr <= 39 AND
            CAN-DO(SysPara.Parameter1,hParameterTable::EksterntMedlemsNr) NO-ERROR.

IF AVAILABLE SysPara THEN
  ASSIGN 
      cButKlubbListe = SysPara.Parameter1
      cMKlubbId      = SysPara.Parameter2.  

IF NOT AVAILABLE SysPara OR 
  cMKlubbId = ''
THEN DO:
    {syspara.i 14 1  7 cMKlubbId}
END.

DEF VAR iMKlubbIdDefault AS INTEGER NO-UNDO. 
iMKlubbIdDefault = int(cMKlubbId).

FIND FIRST MedlemsGruppe NO-LOCK NO-ERROR.
FIND FIRST MedlemsType   NO-LOCK NO-ERROR.

  

DO TRANSACTION:

        FIND FIRST Post WHERE 
          Post.PostNr = TRIM(hParameterTable::PostNr) NO-LOCK NO-ERROR.
        IF NOT AVAILABLE Post THEN 
        DO:
            CREATE Post.
            ASSIGN
                Post.PostNr      = TRIM(hParameterTable::PostNr)
                Post.Beskrivelse = TRIM(hParameterTable::Beskrivelse)
                .
            FIND CURRENT Post NO-LOCK.
        END.
        
        CREATE Medlem.
        Medlem.MedlemsNr       = getNextidNumber(). 
        Medlem.Registrertdato  = TODAY. 
        Medlem.RegistrertTid   = TIME.  
        Medlem.Registrertav    = "Kasse". 
        Medlem.edato =  Medlem.Registrertdato .
        medlem.etid  = Medlem.RegistrertTid.

        LOG-MANAGER:WRITE-MESSAGE("Create:" + STRING( Medlem.MedlemsNr),"Create").
      
        ASSIGN 
            Medlem.PersonNr  = SUBSTRING(TRIM(hParameterTable::PersonNr),3)
            Medlem.ForNavn   = TRIM(hParameterTable::ForNavn)
            Medlem.EtterNavn = TRIM(hParameterTable::EtterNavn)
            Medlem.PostNr    = TRIM(hParameterTable::PostNr)
            Medlem.Adresse2  = TRIM(hParameterTable::Adresse2)
            Medlem.Adresse1  = TRIM(hParameterTable::Adresse1)
            Medlem.MedGruppe = IF AVAILABLE MedlemsGruppe THEN MedlemsGruppe.MedGruppe ELSE 0
            Medlem.MedType   = IF AVAILABLE MedlemsType   THEN MedlemsType.MedType     ELSE 0
           
            Medlem.ButikkNr  = INT(hParameterTable::ButikkNr)
            Medlem.Kjonn     = CAN-DO('male,man,m,yes,true',TRIM(hParameterTable::Kjonn))
            Medlem.EksterntMedlemsNr = TRIM(hParameterTable::EksterntMedlemsNr)
            Medlem.Kilde        = 'kasse'
            Medlem.ePostAdresse = TRIM(hParameterTable::epostAdresse)
            Medlem.MobilTlf     = TRIM(hParameterTable::MobilTlf)
            Medlem.Telefon      = TRIM(hParameterTable::Telefon).
            Medlem.FodselsDato       = hParameterTable::FodselsDato . 
            Medlem.Bonus_Berettiget  = hParameterTable::gift . 
            
            IF Medlem.FodselsDato NE ? THEN Medlem.fodtar = YEAR( Medlem.FodselsDato).

            /* default 1 hvis ikke butikk 0 eller ? */ 
            Medlem.ButikkNr         =  IF  Medlem.ButikkNr = 0 OR 
                                           Medlem.ButikkNr = ?     
                                          THEN 1 ELSE  Medlem.ButikkNr.

           
    
            FIND FIRST MedlemsKlubb  WHERE MedlemsKlubb.MKlubbId   = iMKlubbIdDefault   NO-LOCK NO-ERROR.
            Medlem.MKlubbId  = IF AVAILABLE MedlemsKlubb  THEN MedlemsKlubb.MKlubbId   ELSE 0.
    

            iKortNr =  INT( Medlem.MobilTlf) NO-ERROR.
            IF ERROR-STATUS:ERROR THEN
              iKortNr =  INT(Medlem.MedlemsNr).
        
            FIND FIRST  Medlemskort WHERE 
                        MedlemsKort.KortNr  = STRING(iKortNr)  
                        NO-ERROR.
        
            IF NOT AVAIL MedlemsKort THEN
            DO:
                CREATE MedlemsKort.
                ASSIGN
                    MedlemsKort.MedlemsNr      = Medlem.MedlemsNr
                    MedlemsKort.KortNr         = STRING(iKortNr)
                    MedlemsKort.AktivertDato   = TODAY 
                    MedlemsKort.UtgarDato      = TODAY + 999
                    MedlemsKort.Innehaver      = Medlem.Fornavn + " " + Medlem.EtterNavn
                    MedlemsKort.KortType       = 2  /* type 2 for access from Kasse */  
                    MedlemsKort.Merknad        = "Registered from Cloud:" + Medlem.EksterntMedlemsNr
                    MedlemsKort.Sperret        = FALSE
                    MedlemsKort.InterntKKortId = 0.
        
                LOG-MANAGER:WRITE-MESSAGE("Created MedlemsKort:" + STRING( MedlemsKort.MedlemsNr),"BATCH").
            END. 
            ELSE 
            DO:
                ASSIGN 
                MedlemsKort.Innehaver      = Medlem.Fornavn + " " + Medlem.EtterNavn
                MedlemsKort.KortType       = 2  /* type 2 for access from Kasse */  
                MedlemsKort.InterntKKortId =  0 .
                LOG-MANAGER:WRITE-MESSAGE("Endret MedlemsKort:" + STRING( MedlemsKort.MedlemsNr),"BATCH").
            END. 

pobOk = TRUE. 
pocReturn = "". 

    IF AVAILABLE Medlem      THEN RELEASE medlem.
    IF AVAILABLE Medlemskort THEN RELEASE MedlemsKort.
END. /* TRANSACTION */
                                                                   

        

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

/* ************************  Function Implementations ***************** */

&IF DEFINED(EXCLUDE-getNextidNumber) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getNextidNumber Procedure 
FUNCTION getNextidNumber RETURNS INTEGER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
    DEFINE BUFFER bfMedlem_ FOR medlem. 
    DEFINE VARIABLE iId AS INT NO-UNDO. 

    FIND LAST bfmedlem_ WHERE bfMedlem_.medlemsnr NE ? NO-LOCK NO-ERROR.
    IF AVAIL bfmedlem_ THEN
    REPEAT WHILE AVAIL bfmedlem_:
         iId = bfMedlem_.MedlemsNr + 1. 
         FIND FIRST bfMedlem_ WHERE bfMedlem_.medlemsnr  =  iId NO-LOCK NO-ERROR.
    END.
    ELSE iId = 1. 

    RETURN iId. 
 

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-isMayFlowerMember) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION isMayFlowerMember Procedure 
FUNCTION isMayFlowerMember RETURNS LOGICAL
 (INPUT EksterntMedlemsNr AS CHAR , OUTPUT iMayFlowerid AS INT ):
          
    DEF VAR lMayflowerMember AS LOGICAL NO-UNDO. 
    lMayFlowerMember = FALSE. 
    DEF VAR cMayFlowerId AS CHAR NO-UNDO. 
    /* test for mayflower member */
          IF Medlem.EksterntMedlemsNr BEGINS "M-" THEN
          DO:
              lMayFlowerMember = TRUE. 
              cMayFlowerId     = LEFT-TRIM(REPLACE(TRIM(EksterntMedlemsNr),'M-',''),'0').
              iMayFlowerId     = INTEGER(cMayFlowerid) NO-ERROR. 
              IF iMayFlowerid  =  0 OR iMayFlowerid = ?  THEN
              lMayflowerMember  = FALSE. 
          END. 
          ELSE lMayFlowerMember = FALSE. 

   RETURN lMayFlowerMember. 
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-isRegisteredFromEcom) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION isRegisteredFromEcom Procedure 
FUNCTION isRegisteredFromEcom RETURNS LOGICAL
    (INPUT mobiltlf AS CHAR , INPUT  email AS CHAR ):

   DEFINE BUFFER bfMedlem FOR medlem. 
   DEFINE BUFFER bfMedlemskort FOR medlemskort. 

   FIND FIRST bfmedlem WHERE 
              bfmedlem.mobiltlf = mobiltlf AND 
              bfmedlem.MKlubbId = 1    NO-LOCK NO-ERROR. 

   IF AVAIL bfmedlem THEN
   FIND FIRST  bfMedlemskort WHERE 
               bfMedlemsKort.medlemsnr  = bfmedlem.medlemsnr AND 
               bfMedlemsKort.KortType   = 1 /* fra ecom */ NO-LOCK NO-ERROR.

   IF AVAIL bfMedlemskort  THEN RETURN TRUE. 

   FIND FIRST bfmedlem WHERE 
              bfmedlem.epost = email AND 
              bfmedlem.MKlubbId = 1    NO-LOCK NO-ERROR. 

   IF AVAIL bfmedlem THEN
   FIND FIRST  bfMedlemskort WHERE 
               bfMedlemsKort.medlemsnr  = bfmedlem.medlemsnr AND 
               bfMedlemsKort.KortType   = 1 /* fra ecom */ NO-LOCK NO-ERROR.

   IF AVAIL bfMedlemskort  THEN RETURN TRUE. 

   RETURN FALSE. 

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

