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

&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
    
&ELSE
    
&ENDIF

DEFINE OUTPUT PARAMETER ocRetur     AS CHARACTER  NO-UNDO.
DEFINE        VARIABLE  iAntEksport AS INTEGER    NO-UNDO.

DEF VAR iAntLinjer   AS INT        NO-UNDO.
DEF VAR iAlle        AS INT        NO-UNDO.
DEF VAR bStream      AS LOG        NO-UNDO.
DEFINE VARIABLE lDec AS DECIMAL NO-UNDO.

/* Filhåndtering */
DEF VAR cFilNavn   AS CHAR FORMAT "x(40)"     NO-UNDO.
DEF VAR cKatalog   AS CHAR                    NO-UNDO.
DEF VAR cPrefix    AS CHAR                    NO-UNDO.
DEF VAR cEkstent   AS CHAR                    NO-UNDO.
DEF VAR iSekvens   AS INT  FORMAT ">>>>>>>9"  NO-UNDO.
DEF VAR cEDBSystem AS CHAR INITIAL "EkspKOAU" NO-UNDO.

DEFINE VARIABLE ocReturn AS CHARACTER NO-UNDO.
DEFINE VARIABLE obOk     AS CHARACTER NO-UNDO.

/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE TT_ELogg NO-UNDO LIKE ELogg
  FIELD B_Id AS DECIMAL FORMAT '>>>>>>>>>>>>9'
  FIELD ButikkNr AS INTEGER FORMAT '>>>>>9'
  INDEX EksportIdx ButikkNr B_Id.

DEFINE TEMP-TABLE ttMva NO-UNDO 
  FIELD MomsKod LIKE Moms.MomsKod
  FIELD Belop   AS DECIMAL FORMAT '->>>>>>>9.99'
  INDEX MomsKod MomsKod.
  
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB Procedure 
/* ************************* Included-Libraries *********************** */

{incl/DevMode.i}
{incl/CustDevMode.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

/* IF lDirekte AND NOT CAN-DO(SESSION:GET-PRINTERS(),cPrinter) THEN */
/*     RETURN.                                                      */

{syspara.i 19 101 1 iAlle INT}

RUN KopierElogg.

RUN Eksporter.

RUN SlettELogg. /* */

ocRetur = "OK," + String(iAntEksport).


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-EDBSystem) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE EDBSystem Procedure 
PROCEDURE EDBSystem :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DO TRANSACTION:
    FIND FIRST EkstEDBSystem WHERE
        EkstEDBSystem.EDBSystem = cEDBSystem EXCLUSIVE-LOCK NO-ERROR.
    IF NOT AVAILABLE EkstEDBSystem THEN DO:
        LEAVE.
    END.
    ELSE DO:
        ASSIGN
            cKatalog           = TRIM(EkstEDBSystem.Filkatalog,"\")
            cPrefix            = 'dirdebr' /*EkstEDBSystem.FilPrefix*/
            cEkstent           = 'edi' /*trim(EkstEDBSystem.FilEkstent,".")*/
            iSekvens           = IF (EkstEDBSystem.SeqNr + 1) > EkstEDBSystem.MaksSeq
                                    THEN 1
                                    ELSE EkstEDBSystem.SeqNr + 1
            EkstEDBSystem.SeqNr = iSekvens
            cFilNavn           = cKatalog + "\" +
                                 cPrefix  + 
                                 /*STRING(iSekvens,"99999999") + */  "." + 
                                 cEkstent
            .
    END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

 
&IF DEFINED(EXCLUDE-Eksporter) = 0 &THEN
		
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Eksporter Procedure
PROCEDURE Eksporter:

	/*------------------------------------------------------------------------------
			Purpose:  																	  
			Notes:  																	  
	------------------------------------------------------------------------------*/
    DEFINE VARIABLE cDatoTime AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE lSaldo    AS DECIMAL    NO-UNDO.
    DEFINE VARIABLE iKundeNr  AS INTEGER    NO-UNDO.
    
    ASSIGN
      cDatoTime = "_" + SUBSTR(STRING(YEAR(TODAY)),3) + STRING(MONTH(TODAY)) + STRING(DAY(TODAY)) + STRING(TIME)
      iKundeNr  = 3000.

    IF CAN-FIND(FIRST TT_ELogg) THEN       
    DO:
      /* Setter filnavn */
      RUN EDBSystem.        /* (EkstEDBSystem.EDBSystem)  */
      IF cFilnavn = "" THEN
        RETURN.
      bStream = TRUE.
      IF SEARCH(cFilnavn) = ? THEN 
      DO:
        OUTPUT TO VALUE(cFilnavn) NO-ECHO APPEND.
        PUT UNFORMATTED
            'H'
           + ';' + STRING(YEAR(TODAY),'9999') + 
                   STRING(MONTH(TODAY),"99") +
                   STRING(DAY(TODAY),'99')
           + ';5' 
          SKIP.
        iAntEksport = iAntEksport + 1.
      END.
      ELSE OUTPUT TO VALUE(cFilnavn) NO-ECHO APPEND.        
    END.
    ELOGG:
    FOR EACH TT_ELogg 
      BREAK BY ButikkNr
            BY B_Id:
        FIND BongHode WHERE BongHode.B_Id = DECIMAL(TT_Elogg.Verdier) NO-LOCK NO-ERROR.
        
        IF AVAIL BongHode THEN 
        DO:
          /* Ikke ferdigbehandlet bong. */
          IF BongHode.BongStatus < 5 THEN 
            NEXT ELOGG.
          /* Bongen er eksportert fra Før */
          IF BongHode.EksportertDato <> ? THEN 
            NEXT ELOGG.
          /* Kreditsalg skal ikke med her. TN 12/12-09 */
          IF CAN-FIND(FIRST BongLinje WHERE
                    BongLinje.B_Id = BongHode.B_id AND
                    CAN-DO("065",STRING(BongLinje.TTId,"999"))) THEN
            NEXT ELOGG.
          /* Nullstiller før ny summering. */
          FOR EACH ttMva: 
            DELETE ttMva. 
          END.
          /* Summerer opp de ulike mva nivåene. */
          FOR EACH BongLinje NO-LOCK WHERE
            BongLinje.B_Id = BongHode.B_Id AND
            CAN-DO('1,3,10',string(BongLinje.TTId)) AND 
            BongLinje.Makulert = FALSE:
            
            DO: 
              FIND ttMva WHERE
                ttMva.MomsKod = BongLinje.MvaGr NO-ERROR.
              IF NOT AVAILABLE ttMva THEN DO:
                CREATE ttMva.
                ASSIGN ttMva.MomsKod = BongLinje.MvaGr.
              END.
              
              ASSIGN 
                ttMva.Belop = ttMva.Belop + 
                (BongLinje.LinjeSum - (BongLinje.LinjeRab + BongLinje.SubtotalRab)) *
                (IF CAN-DO('3,10',string(BongLinje.TTId)) THEN -1 ELSE 1)
                .
            END.
          END.
 
          FIND Butiker NO-LOCK WHERE
            Butiker.Butik = BongHode.ButikkNr NO-ERROR.
 
          FOR EACH ttMva BREAK BY ttMva.MomsKod:
            iAntEksport = iAntEksport + 1.

            PUT UNFORMATTED
                'L'
              + ';' + STRING(Bonghode.BongNr)
              + ';' + STRING(YEAR(Bonghode.Dato),'9999') + 
                      STRING(MONTH(Bonghode.Dato),"99") +
                      STRING(DAY(Bonghode.Dato),'99')
              + ';' + STRING(YEAR(Bonghode.Dato),'9999') + 
                      STRING(MONTH(Bonghode.Dato),"99") +
                      STRING(DAY(Bonghode.Dato),'99')
              + ';1900'
              + ';' + STRING(iKundeNr + (IF ttMva.MomsKod = 0 THEN 120 
                                         ELSE IF ttMva.Momskod = 1 THEN 10
                                         ELSE IF ttMva.MomsKod = 2 THEN 0
                                         ELSE 0)) 
              + ';' + TRIM(REPLACE(STRING(ttMva.Belop,'->>>>>>>>9.99'),',','.'))                            
              + ';' + (IF AVAILABLE Butiker THEN Butiker.KortNavn ELSE '')
              SKIP.
          END.
          
          ASSIGN 
            TT_ELogg.EndringsType = 2
            iAntEksport = iAntEksport + 1.
              
          DO TRANSACTION:
            FIND CURRENT Bonghode EXCLUSIVE-LOCK.
            ASSIGN
              BongHode.EksportertDato = TODAY.
            RELEASE BongHode.            
          END.
        END.
    END. /* ELOGG */

    /* Lukker pr. butikk. */
    IF bStream THEN 
      OUTPUT CLOSE.

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

    KOPIER:
    FOR EACH ELogg WHERE ELogg.TabellNavn = "Bonghode" AND
                         ELogg.EksterntSystem = "KONTAUTO" NO-LOCK:

        FIND bElogg WHERE ROWID(bElogg) = ROWID(Elogg) EXCLUSIVE NO-WAIT NO-ERROR.
        IF AVAIL bElogg THEN DO:
            BUFFER-COPY ELogg TO TT_ELogg NO-ERROR.
            IF AVAILABLE TT_ELogg THEN
              DO: 
                ASSIGN TT_ELogg.EndringsType = 1.
                FIND Bonghode WHERE Bonghode.B_Id = DECIMAL(TT_Elogg.Verdier) NO-LOCK NO-ERROR.
                IF AVAILABLE TT_ELogg THEN 
                  ASSIGN
                    TT_ELogg.B_Id     = Bonghode.B_Id
                    TT_ELogg.ButikkNr = BongHode.ButikkNr.                 
              END.
        END.
        IF AVAILABLE TT_Elogg THEN
            RELEASE TT_ELogg.
    END. /* KOPIER */


END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF
 
&IF DEFINED(EXCLUDE-SlettELogg) = 0 &THEN
		
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SlettELogg Procedure
PROCEDURE SlettELogg:

	/*------------------------------------------------------------------------------
			Purpose:  																	  
			Notes:  																	  
	------------------------------------------------------------------------------*/
    FOR EACH TT_Elogg WHERE TT_ELogg.EndringsType   = 2:
        FIND ELogg WHERE ELogg.TabellNavn     = TT_ELogg.TabellNavn AND
                         ELogg.EksterntSystem = TT_ELogg.EksterntSystem AND
                         ELogg.Verdier        = TT_ELogg.Verdier
                      EXCLUSIVE NO-WAIT NO-ERROR.
        IF AVAIL ELogg THEN DO:
            DELETE ELogg.
            DELETE TT_ELogg.
        END.
    END.

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

