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

DEFINE INPUT PARAMETER wQY          AS CHAR NO-UNDO.

DEF VAR hField1      AS HANDLE NO-UNDO.
DEF VAR hField2      AS HANDLE NO-UNDO.
DEF VAR hField3      AS HANDLE NO-UNDO.
DEF VAR hField4      AS HANDLE NO-UNDO.
DEF VAR hField5      AS HANDLE NO-UNDO.
DEF VAR hQuery       AS HANDLE NO-UNDO.
DEF VAR hBuffer      AS HANDLE NO-UNDO.
DEF VAR cFilnavn     AS CHAR   NO-UNDO.
DEF VAR iLoop        AS INT    NO-UNDO.
DEF VAR iCount       AS INTE   NO-UNDO.
DEF VAR cModellFarge AS CHAR NO-UNDO.
DEF VAR dTmpArtikkel LIKE ArtBas.ArtikkelNr NO-UNDO.
DEF VAR cErstattId   AS CHAR NO-UNDO.
DEFINE BUFFER bArtBas FOR ArtBas.
CREATE QUERY  hQuery.
CREATE BUFFER hBuffer FOR TABLE "ArtBas".

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

hQuery:SET-BUFFERS(hBuffer).
hQuery:QUERY-PREPARE(wQY).
hQuery:QUERY-OPEN().

STATUS DEFAULT "".
REPEAT:
    hQuery:GET-NEXT() NO-ERROR.
    IF NOT hBuffer:AVAILABLE THEN LEAVE.
    ASSIGN hField1      = hBuffer:BUFFER-FIELD("Artikkelnr")
           hField2      = hBuffer:BUFFER-FIELD("ModellFarge")
           hField3      = hBuffer:BUFFER-FIELD("Pakke")
           hField4      = hBuffer:BUFFER-FIELD("OPris")
           hField5      = hBuffer:BUFFER-FIELD("IKasse")
           cModellFarge = TRIM(hField2:STRING-VALUE())
           cErstattId   = "".
    IF hField4:BUFFER-VALUE() = TRUE THEN /* Öppet pris */
        NEXT.
    ELSE IF hField3:BUFFER-VALUE() = TRUE AND hField5:BUFFER-VALUE() = FALSE THEN
        NEXT.

    RUN EloggVPI ("ArtBas",TRIM(hField1:STRING-VALUE())).
    
    IF hField3:BUFFER-VALUE() = FALSE THEN DO: /* ikke pakkevare */
        FIND Erstattningsvare WHERE Erstattningsvare.ArtikkelNr = hField1:BUFFER-VALUE() NO-LOCK NO-ERROR.
        IF AVAIL Erstattningsvare THEN
            ASSIGN cErstattId = STRING(Erstattningsvare.ErstattId).
/*         FIND Erstattningsvare WHERE Erstattningsvare.ArtikkelNr = hField1:BUFFER-VALUE() NO-LOCK NO-ERROR. */
        IF cModellFarge <> "0" THEN DO:
            FOR EACH bArtBas WHERE bArtBas.ModellFarge = DECI(cModellFarge) AND 
                                   bArtBas.ArtikkelNr <> hField1:BUFFER-VALUE():
                RUN EloggVPI ("ArtBas",TRIM(STRING(bArtBas.ArtikkelNr))).                    
                IF cErstattId = "" THEN DO:
                    FIND Erstattningsvare WHERE Erstattningsvare.ArtikkelNr = hField1:BUFFER-VALUE() NO-LOCK NO-ERROR.
                    IF AVAIL Erstattningsvare THEN
                        ASSIGN cErstattId = STRING(Erstattningsvare.ErstattId).
                END.
            END.
        END.
    END.
    ELSE DO: /* pakkevare */
        FOR EACH Pakkelinje WHERE Pakkelinje.ArtikkelNr = hField1:BUFFER-VALUE() NO-LOCK.
            FIND bArtBas WHERE bArtBas.Artikkelnr = PakkeLinje.PkArtikkelNr NO-LOCK NO-ERROR.
            IF AVAIL bArtBas THEN DO:
                RUN EloggVPI ("ArtBas",TRIM(STRING(bArtBas.ArtikkelNr))).                    
                RUN EloggVPI ("PakkeLinje",TRIM(STRING(bArtBas.ArtikkelNr))).                    
                FIND Erstattningsvare WHERE Erstattningsvare.ArtikkelNr = bArtBas.ArtikkelNr NO-LOCK NO-ERROR.
                IF AVAIL Erstattningsvare AND NOT CAN-DO(cErstattId,STRING(Erstattningsvare.ErstattId)) THEN
                    ASSIGN cErstattId = cErstattId + (IF cErstattId = "" THEN "" ELSE ",") 
                                                   + STRING(Erstattningsvare.ErstattId).
                IF bArtBas.ModellFarge <> 0 THEN DO:
                    ASSIGN cModellFarge = TRIM(STRING(bArtBas.ModellFarge))
                           dTmpArtikkel = bArtBas.ArtikkelNr.
                    FOR EACH bArtBas WHERE bArtBas.ModellFarge = DECI(cModellFarge) AND 
                                           bArtBas.ArtikkelNr <> dTmpArtikkel NO-LOCK.
                        RUN EloggVPI ("ArtBas",TRIM(STRING(bArtBas.ArtikkelNr))).
                        IF bArtBas.ArtikkelNr = bArtBas.ModellFarge THEN DO:
                            FIND Erstattningsvare WHERE Erstattningsvare.ArtikkelNr = bArtBas.ArtikkelNr NO-LOCK NO-ERROR.
                            IF AVAIL Erstattningsvare AND NOT CAN-DO(cErstattId,STRING(Erstattningsvare.ErstattId)) THEN
                                ASSIGN cErstattId = cErstattId + (IF cErstattId = "" THEN "" ELSE ",") 
                                                               + STRING(Erstattningsvare.ErstattId).
                        END.
                    END.
                END.
            END.
        END.
    END.
    IF cErstattId <> "" THEN DO iCount = 1 TO NUM-ENTRIES(cErstattId):
        FOR EACH Erstattningsvare WHERE Erstattningsvare.ErstattId = INT(ENTRY(iCount,cErstattId)) NO-LOCK:
            FIND bArtBas WHERE bArtBas.ArtikkelNr = Erstattningsvare.ArtikkelNr NO-LOCK NO-ERROR.
            IF AVAIL bArtBas THEN DO:
                ASSIGN cModellFarge = TRIM(STRING(bArtBas.ModellFarge))
                       dTmpArtikkel = bArtBas.ArtikkelNr.
                RUN EloggVPI ("ArtBas",TRIM(STRING(bArtBas.ArtikkelNr))).                    
                RUN EloggVPI ("Erstattningsvare",TRIM(ENTRY(iCount,cErstattId)) + CHR(1) + TRIM(STRING(bArtBas.ArtikkelNr))).                    
                IF bArtBas.ModellFarge <> 0 THEN DO:
                    ASSIGN cModellFarge = TRIM(STRING(bArtBas.ModellFarge))
                           dTmpArtikkel = bArtBas.ArtikkelNr.
                    FOR EACH bArtBas WHERE bArtBas.ModellFarge = DECI(cModellFarge) AND 
                                           bArtBas.ArtikkelNr <> dTmpArtikkel NO-LOCK.
                        RUN EloggVPI ("ArtBas",TRIM(STRING(bArtBas.ArtikkelNr))).                    
                    END.
                END.
            END.
        END.
    END.
END.
STATUS DEFAULT "".
MESSAGE "Antall varelinjer til VPI-registeret: " + STRING(iLoop) + "."
    VIEW-AS ALERT-BOX INFO BUTTONS OK.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-EloggVPI) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE EloggVPI Procedure 
PROCEDURE EloggVPI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER cTabellNavn AS CHARACTER  NO-UNDO.
    DEFINE INPUT  PARAMETER cVerdi AS CHARACTER  NO-UNDO.
    FIND ELogg WHERE ELogg.TabellNavn     = cTabellNavn AND
                 ELogg.EksterntSystem = "VPI"    AND
                 ELogg.Verdier        = cVerdi NO-ERROR.
    IF NOT AVAIL Elogg THEN DO:
        CREATE Elogg.
        ASSIGN ELogg.TabellNavn     = cTabellNavn
               ELogg.EksterntSystem = "VPI"   
               ELogg.Verdier        = cVerdi
               iLoop                = iLoop + 1.
    END.
    ASSIGN ELogg.EndringsType = 1
           ELogg.Behandlet    = FALSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

