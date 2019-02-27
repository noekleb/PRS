&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
/* Connected Databases 
          skotex           PROGRESS
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

DEFINE OUTPUT PARAMETER ocRetur     AS CHARACTER  NO-UNDO.
DEFINE        VARIABLE  iAntEksport AS INTEGER    NO-UNDO.

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
   Temp-Tables and Buffers:
      TABLE: TT_ELogg T "?" NO-UNDO SkoTex ELogg
   END-TABLES.
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

/* MESSAGE "Fra eksportPkSdl.p"           */
/*     VIEW-AS ALERT-BOX INFO BUTTONS OK. */
/*                                        */
RUN KopierElogg.
/* För att undvika krasch av pågående orderbekräftelse */
/*PAUSE 30 NO-MESSAGE.*/

RUN Eksportera.

RUN SlettELogg. /* */

ocRetur = "OK," + String(iAntEksport).

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-Eksportera) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Eksportera Procedure 
PROCEDURE Eksportera :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF VAR cDatoTime   AS CHAR NO-UNDO.
    DEF VAR cOrdreListe AS CHAR NO-UNDO.
    DEF VAR piLoop      AS INT  NO-UNDO.
    DEFINE VARIABLE cTekst AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cKAtalog AS CHARACTER NO-UNDO.
    
    DEF BUFFER lokPkSdlHode FOR PkSdlHode.
    DEF BUFFER lokPkSdlLinje FOR PkSdlLinje.

    ASSIGN
      cKatalog = "C:\home\lindbak\sendes".
   {syspara.i 1 1 51 cTekst}
   IF cTekst <> '' THEN 
      cKatalog = RIGHT-TRIM(cTekst,'\').
    
    
    cDatoTime = "_" + SUBSTR(STRING(YEAR(TODAY)),3) + STRING(MONTH(TODAY)) + STRING(DAY(TODAY)) + STRING(TIME).    
    
    ELOGG:
    FOR EACH TT_ELogg:
        FIND PkSdlHode WHERE PkSdlHode.PkSdlId = dec(TT_Elogg.Verdier) NO-LOCK NO-ERROR.
        IF AVAIL PkSdlHode THEN DO:
            IF PkSdlHode.EDato = TODAY AND TIME - PkSdlHode.etid < 120 THEN
                NEXT ELOGG.

            IF CAN-FIND(FIRST PkSdlLinje WHERE PkSdlLinje.PkSdlId = PkSdlHode.PkSdlId) THEN DO:
                FIND FIRST PkSdlLinje NO-LOCK WHERE PkSdlLinje.PkSdlId = PkSdlHode.PkSdlId NO-ERROR. 
                FIND Butiker WHERE Butiker.Butik = PkSdlHode.CL NO-LOCK NO-ERROR.
                IF AVAIL Butiker AND Butiker.harButikksystem = TRUE THEN DO:
                    /* Legger ut opprinnelig ordre til butikken */
                    cOrdreListe = ''.
                    FOR EACH lokPkSdlLinje OF PkSdlHode NO-LOCK WHERE
                        lokPkSdlLinje.OrdreNr > 0 AND CAN-FIND(Ordre OF lokPkSdlLinje):
                        IF NOT CAN-DO(cOrdreListe,STRING(lokPkSdlLinje.OrdreNr)) THEN
                            cOrdreListe = cOrdreListe +
                                          (IF cOrdreListe <> '' THEN ',' ELSE '') +
                                          STRING(lokPkSdlLinje.OrdreNr).
                    END.

                    IF cOrdreListe <> '' THEN
                    DO piLoop = 1 TO NUM-ENTRIES(cOrdreListe):
                        RUN HK_Ordre_to_Butikk.p (int(ENTRY(piLoop,cOrdreListe)),cKatalog,"PKSDLHKO" + TRIM(TT_ELogg.Verdier) + '_' + string(piLoop) + '_' + cDatoTime).
                        PAUSE 2 NO-MESSAGE. /* For å hindre overskrivning */
                    END.
                    /* Legger ut pakkseddelen */
                    RUN HK_PkSdl_to_Butikk.p (INT(TT_Elogg.Verdier),cKatalog,"PKSDLHK" + TRIM(TT_ELogg.Verdier) + cDatoTime).
                    TT_ELogg.EndringsType = 2.
                    IF RETURN-VALUE = "OK" THEN DO:
                        ASSIGN TT_ELogg.EndringsType = 2
                               iAntEksport = iAntEksport + 1.
                    END.
                END.
                /* Pakksedler til butikker som ikke har butikksystem, skal flagges som sendt. */
                ELSE DO TRANSACTION:
                    ASSIGN TT_ELogg.EndringsType = 2. /* fel butikkoppling -- kanske borde loggas */
                    FIND CURRENT PkSdlHode EXCLUSIVE-LOCK.
                    ASSIGN
                      PkSdlHode.PkSdlStatus = 9. /* Sendt til butikk */
                    FIND CURRENT PkSdlHode NO-LOCK.
                END. /* TRANSACTION */
            END.
            ELSE
                ASSIGN TT_ELogg.EndringsType = 2. /* inge bestilling på ordren -- kanske borde loggas */
        END.
        ELSE
            ASSIGN TT_ELogg.EndringsType = 2. /* elogg där inte ordren finns  */
    END. /* ELOGG */

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
    DEFINE BUFFER bElogg FOR Elogg.
    
    KOPIER:
    FOR EACH ELogg WHERE ELogg.TabellNavn = "PkSdlHode" AND
                         ELogg.EksterntSystem = "POS" NO-LOCK:

        FIND bElogg WHERE ROWID(bElogg) = ROWID(Elogg) EXCLUSIVE NO-WAIT NO-ERROR.
        IF AVAIL bElogg THEN DO:
            BUFFER-COPY ELogg TO TT_ELogg NO-ERROR.
            ASSIGN TT_ELogg.EndringsType = 1.
/*             bELogg.EndringsType = 2. */
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
PROCEDURE SlettELogg :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    FOR EACH TT_Elogg WHERE TT_ELogg.EndringsType   = 2:
        FIND ELogg WHERE ELogg.TabellNavn     = TT_ELogg.TabellNavn AND
                         ELogg.EksterntSystem = TT_ELogg.EksterntSystem AND
                         ELogg.Verdier        = TT_ELogg.Verdier
/*                       AND ELogg.EndringsType   = TT_ELogg.EndringsType  */
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

