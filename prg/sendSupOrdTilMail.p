&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : sendsupordtilmail.p
    Purpose     : Sender suppleringsordre som er logget i ELogg 
                  via eMail.

    Syntax      :

    Description :

    Author(s)   : Tom Nøkleby
    Created     : 30/4-10
    Notes       : 
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE OUTPUT PARAMETER ocRetur AS CHARACTER  NO-UNDO.

DEFINE VARIABLE cOrdrefil  AS CHARACTER NO-UNDO.
DEFINE VARIABLE iButikkNr  AS INTEGER   NO-UNDO.
DEFINE VARIABLE dPlListeId AS DECIMAL   NO-UNDO.

DEFINE VARIABLE cMailhub   AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cDoAUTH    AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cAuthType  AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cUser      AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cPassword  AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cEmailCC   AS CHARACTER   NO-UNDO.
DEFINE VARIABLE FI-MailTo  AS CHARACTER   NO-UNDO.
DEFINE VARIABLE FI-MailFra AS CHARACTER   NO-UNDO.
DEFINE VARIABLE FI-Emne    AS CHARACTER   NO-UNDO.
DEFINE VARIABLE FI-Vedlegg AS CHARACTER   NO-UNDO.
DEFINE VARIABLE E-Meddelande AS CHARACTER NO-UNDO.
DEFINE VARIABLE lMailOK    AS LOG         NO-UNDO.
DEFINE VARIABLE crlf       AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cMessage   AS CHARACTER   NO-UNDO.
DEFINE VARIABLE hBuffer    AS HANDLE      NO-UNDO.
DEFINE VARIABLE iCl        AS INTEGER     NO-UNDO.

DEFINE TEMP-TABLE TT_ELogg NO-UNDO LIKE ELogg.
DEFINE BUFFER   bTT_Elogg FOR TT_Elogg.
DEFINE BUFFER clButiker FOR Butiker.

DEFINE TEMP-TABLE TT_plListeHode NO-UNDO
    FIELD plListeId LIKE plListeHode.PlListeId
    FIELD Slette     AS LOGICAL
    INDEX PlListeId IS PRIMARY plListeId Slette.
    
DEFINE TEMP-TABLE TT_plListeLinje LIKE plListeLinje
  FIELD LevNr LIKE ArtBas.LevNr 
  FIELD BildNr LIKE ArtBas.BildNr
  FIELD LevNamn LIKE LevBas.LevNamn 
  FIELD Vg LIKE VarGr.Vg
  FIELD VgBeskr LIKE VarGr.VgBeskr
  FIELD Hg LIKE HuvGr.Hg
  FIELD HgBeskr LIKE HuvGr.HgBeskr
  FIELD AvdelingNr LIKE Avdeling.AvdelingNr
  FIELD AvdelingNavn LIKE Avdeling.AvdelingNavn 
  FIELD Sasong LIKE Sasong.Sasong
  FIELD SasBeskr LIKE Sasong.SasBeskr
  FIELD plListeStatus LIKE plListeHode.plListeStatus
  FIELD LagAnt LIKE ArtLag.Lagant
  FIELD StrSeq LIKE StrTStr.SeqNr 
  FIELD AlfaStr AS CHARACTER 
  FIELD KostForslag AS DECIMAL
  FIELD KostBekreftet AS DECIMAL
  .    

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

  {syspara.i 50 50 1 cMailhub }
  {syspara.i 50 50 2 cDoAUTH  }
  {syspara.i 50 50 3 cAuthType}
  {syspara.i 50 50 4 cUser    }
  {syspara.i 50 50 5 cPassword}
  {syspara.i 50 50 20 FI-MailTo}
  {syspar2.i 50 50 20 cEmailCC}
  {syspara.i 5 1 1 iCL INT}
  {syspara.i 50 50 29 FI-MailFra}
  FI-MailFra = TRIM(FI-MailFra).

  FIND clButiker NO-LOCK WHERE
    clButiker.Butik = iCL NO-ERROR.
    
  crlf = CHR(13) + CHR(10).
    
  RUN KopierElogg.
  RUN FixPlListeHode.
  RUN SendEMail.    

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



/* **********************  Internal Procedures  *********************** */

 
 
 
&IF DEFINED(EXCLUDE-FixPlListeHode) = 0 &THEN
		
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE FixPlListeHode Procedure
PROCEDURE FixPlListeHode:

	/*------------------------------------------------------------------------------
			Purpose:  																	  
			Notes:  																	  
	------------------------------------------------------------------------------*/
DEFINE BUFFER bTT_ELogg      FOR TT_ELogg.
DEFINE BUFFER bufPlListeHode FOR PlListeHode.

FOR EACH TT_ELogg WHERE 
            TT_ELogg.TabellNavn     = "plListeHode" AND
            TT_ELogg.EksterntSystem = "MAILSUPORD"  AND
            NUM-ENTRIES(TT_ELogg.Verdier,CHR(1)) = 1 BY TT_ELogg.Verdier:

    FIND plListeHode WHERE plListeHode.PlListeId = DECI(TT_ELogg.Verdier) NO-LOCK NO-ERROR.
    IF AVAIL plListeHode THEN 
    DO:
       FIND TT_plListeHode WHERE TT_plListeHode.plListeId = plListeHode.PlListeId AND
                                 TT_plListeHode.Slette = FALSE NO-ERROR.
       IF AVAIL TT_plListeHode THEN
          ASSIGN TT_plListeHode.plListeId = plListeHode.plListeId.
       ELSE DO:
          CREATE TT_plListeHode.
          ASSIGN TT_plListeHode.plListeId = DECI(TT_ELogg.Verdier)
                 TT_plListeHode.Slette    = FALSE.
       END.
    END.
END.

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF



&IF DEFINED(EXCLUDE-KopierElogg) = 0 &THEN
		
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE KopierElogg Procedure
PROCEDURE KopierElogg:

	/*------------------------------------------------------------------------------
			Purpose:  																	  
			Notes:  																	  
	------------------------------------------------------------------------------*/
    DEFINE BUFFER bElogg FOR Elogg.
    DO:
        FOR EACH ELogg WHERE ELogg.TabellNavn = "plListeHode" AND
                             ELogg.EksterntSystem = "MAILSUPORD" NO-LOCK:
            BUFFER-COPY ELogg TO TT_ELogg NO-ERROR.
            FIND bElogg WHERE ROWID(bElogg) = ROWID(Elogg) EXCLUSIVE NO-WAIT NO-ERROR.
            IF AVAIL bElogg THEN
                DELETE bELogg.
            IF AVAILABLE TT_Elogg THEN
                RELEASE TT_ELogg.
        END.
    END.
END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF



&IF DEFINED(EXCLUDE-SendEMail) = 0 &THEN
		
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SendEMail Procedure
PROCEDURE SendEMail:

	/*------------------------------------------------------------------------------
			Purpose:  																	  
			Notes:  																	  
	------------------------------------------------------------------------------*/

  LISTEHODE:
  FOR EACH TT_plListeHode:
    FIND PlListeHode NO-LOCK WHERE
      PlListeHode.PlListeId = TT_plListeHode.PlListeId NO-ERROR.
         
    FIND Butiker WHERE 
      Butiker.Butik = PlListeHode.FraButikkNr NO-LOCK NO-ERROR.
  
    ASSIGN
      FI-Emne      = "Suppleringsordre fra " + Butiker.Butnamn
      FI-MailFra   = (IF FI-MailFra = '' THEN Butiker.ePostAdresse ELSE FI-MailFra)
      E-Meddelande = REPLACE(plListeHode.PlMerknad,CHR(10),crlf)
      hBuffer      = BUFFER TT_PlListeLinje:HANDLE
      .
    
    IF cDoAUTH = "0" THEN
        ASSIGN cDoAUTH   = "FALSE"
               cAuthType = ""
               cUser     = ""
               cPassword = "".
    ELSE
        cDoAUTH = "TRUE".
    
    /* Bygger temp-tabell som skal sendes til utskriftsrutine. */
    FOR EACH plListeLinje OF plListeHode NO-LOCK:
      FIND ArtBas OF plListeLinje NO-LOCK NO-ERROR.
      FIND VarGr OF ArtBas NO-LOCK NO-ERROR.
      IF AVAILABLE VarGr THEN 
        FIND HuvGr OF VarGr NO-LOCK NO-ERROR.
      FIND Sasong OF ArtBas NO-LOCK NO-ERROR.
      FIND LevBas OF ArtBas NO-LOCK NO-ERROR.
      FIND FIRST ArtLag NO-LOCK WHERE
        ArtLag.ArtikkelNr = plListeLinje.ArtikkelNr AND
        ArtLag.Butik      = plListeHode.FraButikkNr AND
        ArtLag.StrKode    = plListeLinje.StrKode NO-ERROR.
       
      FIND FIRST StrKonv NO-LOCK
           WHERE StrKonv.StrKode = PlListeLinje.StrKode
           NO-ERROR.
      IF AVAIL StrKonv THEN 
        FIND FIRST StrTStr NO-LOCK
             WHERE StrTStr.StrTypeID     = ArtBas.StrTypeID
               AND TRIM(StrTStr.SoStorl) = TRIM(StrKonv.Storl) NO-ERROR.
      FIND FIRST ArtPris NO-LOCK 
           WHERE ArtPris.ArtikkelNr = PlListeLinje.ArtikkelNr AND ArtPris.ProfilNr = Butiker.ProfilNr NO-ERROR. 
      IF NOT AVAILABLE ArtPRis THEN             
        FIND FIRST ArtPris NO-LOCK 
           WHERE ArtPris.ArtikkelNr = PlListeLinje.ArtikkelNr AND ArtPris.ProfilNr = clButiker.ProfilNr NO-ERROR. 
               
      CREATE TT_PlListeLinje.
      BUFFER-COPY plListeLinje TO TT_PlListeLinje
        ASSIGN
          TT_PlListeLinje.LopNr         = (IF ArtBas.LopNr <> ? THEN ArtBas.LopNr ELSE 0)
          TT_PlListeLinje.LevKod        = ArtBas.LevKod
          TT_PlListeLinje.Beskr         = ArtBas.Beskr
          TT_PlListeLinje.LevNr         = ArtBas.LevNr
          TT_PlListeLinje.BildNr        = ArtBas.BildNr
          TT_PlListeLinje.LevFargKod    = ArtBas.LevFargKod
          TT_PlListeLinje.LevNamn       = (IF AVAILABLE LevBas THEN LevBas.LevNamn ELSE '')
          TT_PlListeLinje.Vg            = ArtBas.Vg
          TT_PlListeLinje.VgBeskr       = (IF AVAILABLE VarGr THEN VarGr.VgBeskr ELSE '')
          TT_PlListeLinje.Hg            = (IF AVAILABLE HuvGr THEN HuvGr.Hg ELSE 0)
          TT_PlListeLinje.HgBeskr       = (IF AVAILABLE HuvGr THEN HuvGr.HgBeskr ELSE '')
          TT_PlListeLinje.AvdelingNr    = ArtBas.AvdelingNr
          TT_PlListeLinje.AvdelingNavn  = (IF AVAILABLE Avdeling THEN Avdeling.AvdelingNavn ELSE '')
          TT_PlListeLinje.Sasong        = ArtBas.Sasong
          TT_PlListeLinje.SasBeskr      = (IF AVAILABLE Sasong THEN Sasong.SasBeskr ELSE '')
          TT_PlListeLinje.plListeStatus = plListeHode.plListeStatus
          TT_PlListeLinje.LagAnt        = (IF AVAILABLE ArtLag THEN ArtLag.LagAnt ELSE 0)
          TT_PlListeLinje.StrSeq        = (IF AVAIL StrTStr THEN StrTStr.SeqNr ELSE 0)
          TT_PlListeLinje.AlfaStr       = (IF AVAIL StrTStr THEN StrTStr.SoStorl ELSE '')
          TT_PlListeLinje.KostForslag   = (IF AVAIL ArtPris THEN (ArtPris.VareKost[1] * PlListeLinje.Antall) ELSE 0)
          TT_PlListeLinje.KostBekreftet = (IF AVAIL ArtPris THEN (ArtPris.VareKost[1] * PlListeLinje.AntallPlukket) ELSE 0)          
          .
    END.
        
    RUN skrivPllistelinjePDF.p (hBuffer,"O",OUTPUT FI-Vedlegg).
    IF FI-Vedlegg <> "" THEN 
    DO:
      RUN prssmtpmailv5_7a.p (
        /*mailhub    */   cMailhub,
        /*EmailTo    */   FI-MailTo,
        /*EmailFrom  */   FI-MailFra,
        /*EmailCC    */   cEmailCC,
        /*Attachments*/   ENTRY(NUM-ENTRIES(FI-Vedlegg,"\"),FI-Vedlegg,"\"),
        /*LocalFiles */   FI-Vedlegg,
        /*Subject    */   FI-Emne,
        /*Body       */   E-Meddelande,
        /*MIMEHeader */   "CharSet=iso8859-1",
        /*BodyType   */   "",
        /*Importance */   0,
        /*L_DoAUTH   */   cDoAUTH,
        /*C_AuthType */   cAuthType,
        /*C_User     */   cUser,
        /*C_Password */   cPassword,
        /*oSuccessful*/  OUTPUT lMailOK,
        /*vMessage   */  OUTPUT cMessage) NO-ERROR.
      IF lMailOk = FALSE THEN DO:
        ocRetur = "Sending av suppl.ordre avbrutt med melding: " + cMessage.
      END.
      ELSE ocRetur = "OK,1".     
    END.
    ELSE ocRetur = 'Klarte ikke å skape utskriftsfil. Sending avbrudt.'.
  END. /* LISTEHODE */
END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF




