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

DEFINE INPUT  PARAMETER dNyArtikkelnr AS DECIMAL    NO-UNDO.
DEFINE INPUT  PARAMETER dOldArtikkelnr AS DECIMAL    NO-UNDO.

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

RUN LoggSanertArtikkel.

RUN FlyttaEAN.
IF RETURN-VALUE = "AVBRYT" THEN
DO:
    RETURN.
END.

RUN FlyttData.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-FlyttaEAN) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE FlyttaEAN Procedure 
PROCEDURE FlyttaEAN :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/* om vi skall flytta en artikel måste vi först ta hand om EAN */
/*  */
DEFINE BUFFER bNyArtbas FOR artbas.
DO TRANSACTION:
    FIND bNyArtbas EXCLUSIVE-LOCK WHERE bNyArtbas.artikkelnr = dNyArtikkelnr NO-ERROR.
    FIND Artbas EXCLUSIVE-LOCK WHERE artbas.artikkelnr = dOldArtikkelnr NO-ERROR.

    IF NOT AVAIL bNyArtbas OR NOT AVAIL Artbas THEN
        RETURN "AVBRYT".

    IF Artbas.aktivert = TRUE OR artbas.ikasse = TRUE OR
         CAN-FIND(FIRST strekkode OF artbas) THEN DO:
        ASSIGN artbas.beskr      = "KORR: " + artbas.beskr
               artbas.bongtekst  = "KORR: " + artbas.bongtekst
               artbas.IKasse     = FALSE
               artbas.Aktivert   = FALSE
               artbas.sanertdato = TODAY
               artbas.notat      = "KORR_til: " + STRING(bNyArtbas.artikkelnr) + " " + artbas.notat.
        ASSIGN bNyArtbas.notat   = "KORR_fra: " + STRING(Artbas.artikkelnr) + " " + bNyartbas.notat
               bNyArtBas.StrTypeId = ArtBas.StrTypeId.
               
        FOR EACH strekkode OF artbas:
            Strekkode.artikkelnr = bNyArtbas.artikkelnr.
        END.
    END.
    IF AVAILABLE ArtBas    THEN FIND CURRENT ArtBas NO-LOCK.
    IF AVAILABLE bNyArtBas THEN FIND CURRENT bNyArtBas NO-LOCK.
END. /* TRANSACTION */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-FlyttData) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE FlyttData Procedure 
PROCEDURE FlyttData :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/*     dNyArtikkelnr  */
/*     dOldArtikkelnr */
    DEFINE VARIABLE dNyArtBatchNr AS INTEGER    NO-UNDO.
    DEFINE VARIABLE dOldArtBatchNr AS INTEGER    NO-UNDO.
    DEFINE BUFFER bufTranslogg FOR Translogg.
    DEFINE BUFFER seqTranslogg FOR Translogg.
    DEFINE BUFFER NyArtbas FOR Artbas.
    DEFINE BUFFER bNyArtPris FOR ArtPris.
    
    FIND NyArtbas WHERE NyArtbas.artikkelnr = dNyArtikkelnr  NO-LOCK.
    FIND Artbas   WHERE artbas.artikkelnr   = dOldArtikkelnr NO-LOCK.
    
    FOR EACH butiker NO-LOCK TRANSACTION:
        /* TN 29/8-18 Priskøposter for den gamle artikkelen slettes.  */
        IF CAN-FIND(FIRST PrisKo WHERE
                          PrisKo.ArtikkelNr = dOldArtikkelNr AND
                          PrisKo.ProfilNr   = Butiker.ProfilNr) THEN
          DO:
            FOR EACH PrisKo EXCLUSIVE-LOCK WHERE
              PrisKo.ArtikkelNr = dOldArtikkelNr AND
              PrisKo.ProfilNr   = Butiker.ProfilNr:
              DELETE PrisKo.
            END.
          END.

        IF CAN-FIND(FIRST translogg WHERE translogg.butik      = butiker.butik AND
                                          translogg.artikkelnr = dOldArtikkelnr) THEN DO:
            /* Här skapar vi en batch för nya artikeln och butiken */
            RUN batchlogg.w (PROGRAM-NAME(1), 
                             "KORR: " + STRING(dNyArtikkelnr) + "<-" + STRING(dOldArtikkelnr),
                              OUTPUT dNyArtBatchNr).
                              
            /* Här skapar vi en batch för gamla artikeln och butiken */
            RUN batchlogg.w (PROGRAM-NAME(1), 
                             "KORR: " + STRING(dOldArtikkelnr) + "->" + STRING(dNyArtikkelnr),
                              OUTPUT dOldArtBatchNr).

            FOR EACH translogg WHERE translogg.butik = butiker.butik AND translogg.artikkeln = dOldArtikkelnr.
                /* translogg kopieras till ny artikkel */
                CREATE bufTranslogg.
                TRANSNR: REPEAT:
                    FIND LAST seqTranslogg WHERE seqTranslogg.butik = butiker.butik USE-INDEX translogg NO-LOCK NO-ERROR.
                    ASSIGN bufTranslogg.butik   = butiker.butik
                           bufTranslogg.transnr = IF AVAIL seqTranslogg THEN seqTranslogg.transnr + 1 ELSE 1
                           bufTranslogg.seqnr   = 1 NO-ERROR.
                    IF ERROR-STATUS:ERROR = FALSE THEN
                        LEAVE TRANSNR.
                END.
                BUFFER-COPY translogg EXCEPT butik transnr seqnr postert antall artikkelnr batchnr levnr lopnr vg TO bufTranslogg 
                    ASSIGN bufTranslogg.postert = FALSE
                           bufTranslogg.antall  = Translogg.antall
                           bufTranslogg.batchnr = dNyArtBatchnr
                           bufTranslogg.artikkelnr  = NyArtbas.Artikkelnr
                           bufTranslogg.levnr   = NyArtbas.levnr
                           bufTranslogg.lopnr   = NyArtbas.lopnr
                           bufTranslogg.vg      = NyArtbas.vg.
                IF translogg.postert = FALSE THEN
                    DELETE translogg.
            END.
            FOR EACH translogg WHERE translogg.butik = butiker.butik AND translogg.artikkeln = dOldArtikkelnr AND translogg.postert = TRUE NO-LOCK.
                /* här motposterar vi alla gamla */
                FIND LAST seqTranslogg WHERE seqTranslogg.butik = translogg.butik AND
                                             seqTranslogg.transnr = translogg.transnr USE-INDEX translogg NO-LOCK.
                CREATE bufTranslogg.
                BUFFER-COPY translogg EXCEPT seqnr postert antall batchnr TO bufTranslogg 
                    ASSIGN bufTranslogg.seqnr   = seqTranslogg.seqnr + 1
                           bufTranslogg.postert = FALSE
                           bufTranslogg.antall  = Translogg.antall * -1
                           bufTranslogg.batchnr = dOldArtBatchnr.

            END.
            
            FOR EACH PksdlHode NO-LOCK WHERE 
                PkSdlHode.PksdlStatus = 10,
                EACH PkSdlLinje OF PkSdlHode EXCLUSIVE-LOCK WHERE 
                    PkSdlLinje.ArtikkelNr = dOldArtikkelnr:
                        
                ASSIGN 
                    PkSdlLinje.ArtikkelNr = dNyArtikkelnr NO-ERROR.
            END.
                        
            FOR EACH PksdlHode NO-LOCK WHERE 
                PkSdlHode.PksdlStatus = 10,
                EACH PkSdlPris OF PkSdlHode EXCLUSIVE-LOCK WHERE 
                    PkSdlPris.ArtikkelNr = dOldArtikkelnr:
                        
                ASSIGN 
                    PkSdlPris.ArtikkelNr = dNyArtikkelnr NO-ERROR.
            END.            
            
            RUN batchstatus.p (dOldArtBatchnr, 2).
            RUN batchstatus.p (dNyArtBatchnr, 2).
        END.
    END.                                                         

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

 
&IF DEFINED(EXCLUDE-LoggSanertArtikkel) = 0 &THEN
		
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LoggSanertArtikkel Procedure
PROCEDURE LoggSanertArtikkel:

	/*------------------------------------------------------------------------------
			Purpose:  																	  
			Notes:  																	  
	------------------------------------------------------------------------------*/
  DEF VAR cFil AS CHAR NO-UNDO.

  FIND ArtBas NO-LOCK WHERE
    ArtBas.ArtikkelNr = dOldArtikkelnr NO-ERROR.

  ASSIGN
      cFil = "Saneringslogg.Txt".

  IF AVAILABLE ArtBas THEN 
  DO:
    OUTPUT TO VALUE(cFil) APPEND.
    PUT UNFORMATTED
      STRING(TODAY) " " STRING(TIME,'HH:MM:SS')
      " Artikkel " 
      STRING(dOldArtikkelnr) "/"
      ArtBas.LevKod "/"
      ArtBAs.Beskr "/"
      ArtBas.LevFargKod " "
      " er sanert til artikkel "
      STRING(dNyArtikkelnr) " " 
      SKIP
      .
    OUTPUT CLOSE.
  END.
END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

