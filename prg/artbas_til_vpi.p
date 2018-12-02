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


DEF INPUT PARAMETER iEkstVPILevNr AS INTEGER    NO-UNDO.
DEF INPUT PARAMETER lArtikkelNr   AS DEC        NO-UNDO.

DEF VAR lHK           AS LOG        NO-UNDO.
DEF VAR iCL           AS INT        NO-UNDO.
DEF VAR cTekst        AS CHAR       NO-UNDO.
DEF VAR cSystemer     AS CHAR       NO-UNDO.
DEF VAR iLoop         AS INT        NO-UNDO.
DEF VAR lPkArtikkelNr AS DEC        NO-UNDO.
DEF VAR ocReturn      AS CHAR       NO-UNDO.

DEF BUFFER bufErstattningsvare FOR Erstattningsvare.

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

{syspara.i 5 1 1 iCL INT}
{syspara.i 1 1 18 cTekst}
IF CAN-DO("J,Ja,yes,true,1",cTekst) THEN
    lHK = TRUE.
ELSE
    lHK = FALSE.

STATUS DEFAULT "".

FIND ArtBas NO-LOCK WHERE
     ArtBas.ArtikkelNr = lArtikkelNr NO-ERROR.

IF ArtBas.Pakke = FALSE THEN
    RUN OverforArtBas. /* Artbas + streckkoder                */
                       /* AretPris                            */
                       /* Skall vi testa störrelsetyper ++ ?? */
                       /* Bilderegister, Bildedata            */
ELSE IF ArtBAs.Pakke = TRUE THEN
PAKKE: 
DO:
    /* Er det en pakke, skal også alle artikler som ligger som pakkelinjer følge med. */
    ASSIGN
        lPkArtikkelNr = ArtBas.ArtikkelNr.
    RUN OverforArtBas.     /* Legger ut pakkeartikkelen */
    RUN OverforPakkeLinje. /* Legger over pakkelinjene. */

    /* Og her går pakkelinjene. */
    FOR EACH PakkeLinje WHERE 
        PakkeLinje.ArtikkelNr = lPkArtikkelNr NO-LOCK:
        FIND ArtBas NO-LOCK WHERE
            ArtBas.ArtikkelNr = PakkeLinje.PkArtikkelNr NO-ERROR.
        IF AVAILABLE ArtBas THEN
            RUN OverforArtBas. /* Artbas + streckkoder                */
                               /* AretPris                            */
                               /* Skall vi testa störrelsetyper ++ ?? */
                               /* Bilderegister, Bildedata            */
    END.
END.

/* Henter den igjen ... */
FIND ArtBas NO-LOCK WHERE
     ArtBas.ArtikkelNr = lArtikkelNr NO-ERROR.

/* Erstattningsvarer - Disse skal også med over */
FIND FIRST bufErstattningsvare OF ArtBas NO-LOCK NO-ERROR.
IF AVAILABLE bufErstattningsvare THEN
DO:
    ERSTATT:
    FOR EACH Erstattningsvare NO-LOCK WHERE
        Erstattningsvare.ErstattId = Erstattningsvare.ErstattId:
        FIND ArtBas NO-LOCK WHERE
            ArtBas.ArtikkelNr = Erstattningsvare.ArtikkelNr NO-ERROR.
        IF AVAILABLE ArtBas THEN
        DO:
            IF ArtBas.ArtikkelNr = lArtikkelNr THEN
                NEXT ERSTATT.
        END.
        RUN OverforArtBas. /* Artbas + streckkoder                */
                           /* AretPris                            */
                           /* Skall vi testa störrelsetyper ++ ?? */
                           /* Bilderegister, Bildedata            */
    END.
END. /* ERSTATT */


RUN OverforErstattningsvare.

STATUS DEFAULT "".
RETURN ocReturn.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-OverforArtBas) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OverforArtBas Procedure 
PROCEDURE OverforArtBas :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  ARTIKKEL:
  DO:
      IF NOT AVAILABLE ArtBas THEN
          LEAVE ARTIKKEL.

      FIND VPIArtBas NO-LOCK WHERE VPIArtBas.EkstVPILevNr = iEkstVPILevNr AND
                           VPIArtBas.VareNr       = string(ArtBas.ArtikkelNr) NO-ERROR.

      /* Renser bort VPI hvis den finnes fra før */
      IF AVAIL VPIArtBas THEN DO TRANSACTION:
          FOR EACH VPIArtPris OF VPIArtBas:
              DELETE VPIArtPris.
          END.
          FIND VPIBilderegister WHERE VPIBilderegister.EkstVPILevNr = VPIArtBas.EkstVPILevNr AND
                                      VPIBilderegister.VareNr       = VPIArtBas.VareNr       AND
                                      VPIBilderegister.BildNr       = VPIArtBas.BildNr NO-ERROR.
          IF AVAIL VPIBilderegister THEN DO:
              FOR EACH VPIBildeData OF VPIBilderegister:
                  DELETE VPIBildeData.
              END.
              DELETE VPIBilderegister.
          END.
          FOR EACH VPIStrekKode OF VPIArtBas:
              DELETE VPIStrekKode.
          END.
          FOR EACH VPIAltLevBas WHERE
              VPIAltLevBas.EkstVPILevNr = VPIArtBas.EkstVPILevNr AND
              VPIAltLevBas.VareNr       = VPIArtBas.VareNr:
              DELETE VPIAltLevBas.
          END.
          FOR EACH VPIArtBestPkt OF VPIArtBas:
              DELETE VPIArtBestPkt.
          END.
          FIND CURRENT VPIArtBas EXCLUSIVE-LOCK.
          DELETE VPIArtBas.
      END. /* TRANSACTION */

      IF AVAIL ArtBas THEN DO:
          DO TRANSACTION:
              CREATE VPIArtBas.
              BUFFER-COPY ArtBas 
                  EXCEPT KatalogPris ForhRab% Utvidetsok
                  TO VPIArtBas
                  ASSIGN VPIArtBas.EkstVPILevNr   = iEkstVPILevNr
                         VPIArtBas.VareNr         = STRING(ArtBas.ArtikkelNr)
                         VPIArtBas.KatalogPris[1] = ArtBas.KatalogPris
                         VPIArtBas.forhRab%[1]    = ArtBas.forhRab%  
                         VPIArtBas.suppRab%[1]    = ArtBas.supRab%
                         VPIArtBas.LopNr          = ?
                         .
              FIND CURRENT VPIArtBAs NO-LOCK.
          END. 
          FOR EACH StrekKode OF ArtBas NO-LOCK TRANSACTION:
              CREATE VPIStrekKode.
              BUFFER-COPY StrekKode TO VPIStrekKode
                  ASSIGN VPIStrekKode.EkstVPILevNr = iEkstVPILevNr  
                         VPIStrekKode.VareNr       = STRING(Strekkode.ArtikkelNr).
          END.
          FOR EACH ArtPris OF ArtBas NO-LOCK TRANSACTION.
              CREATE VPIArtPris.
              BUFFER-COPY ArtPris TO VPIArtPris
                  ASSIGN VPIArtPris.EkstVPILevNr = iEkstVPILevNr 
                         VPIArtPris.VareNr       = STRING(ArtPris.ArtikkelNr).
          END.
          FOR EACH AltLevBas NO-LOCK WHERE
              AltLevBas.ArtikkelNr = ArtBas.ArtikkelNr AND
              AltLevBas.LevNr      > 0 TRANSACTION:
              IF NOT CAN-FIND(VPIAltLevBas WHERE
                              VPIAltLevBas.EkstVPILevNr = iEkstVPILevNr AND
                              VPIAltLevBas.VareNr       = STRING(AltLevBas.ArtikkelNr) AND
                              VPIAltLevBas.LevNr        = AltLevBas.LevNr) THEN
              DO:
                  CREATE VPIAltLevBas.
                  BUFFER-COPY AltLevBas TO VPIAltLevBas
                      ASSIGN VPIAltLevBas.EkstVPILevNr = iEkstVPILevNr 
                             VPIAltLevBas.VareNr       = STRING(AltLevBas.ArtikkelNr)
                      NO-ERROR.
              END.
          END.
          FOR EACH ArtBestPk OF ArtBas NO-LOCK TRANSACTION:
              CREATE VPIArtBestPk.
              BUFFER-COPY ArtBestPk TO VPIArtBestPk
                  ASSIGN VPIArtBestPk.EkstVPILevNr = iEkstVPILevNr 
                         VPIArtBestPk.VareNr       = STRING(ArtBestPk.ArtikkelNr)
                  NO-ERROR.
          END.
          IF ArtBas.BildNr > 0 THEN
              FIND BildeRegister OF ArtBas NO-LOCK NO-ERROR.
          ELSE DO:
              IF AVAILABLE BildeRegister THEN
                  RELEASE BildeRegister.
          END.

          IF AVAIL BildeRegister  AND 
              (NOT CAN-FIND(VPIBilderegister WHERE VPIBilderegister.EkstVPILevNr = VPIArtBas.EkstVPILevNr AND
                                          VPIBilderegister.VareNr       = VPIArtBas.VareNr AND
                                          VPIBilderegister.BildNr       = VPIArtBas.BildNr))
              THEN 
          DO TRANSACTION:
              CREATE VPIBildeRegister.
              BUFFER-COPY BildeRegister TO VPIBilderegister
                  ASSIGN VPIBilderegister.EkstVPILevNr = iEkstVPILevNr  
                         VPIBilderegister.VareNr       = VPIArtBas.VareNr.

              FOR EACH BildeData OF BildeRegister NO-LOCK:
                  CREATE VPIBildeData.
                  BUFFER-COPY BildeData TO VPIBildeData
                  ASSIGN VPIBildeData.EkstVPILevNr = iEkstVPILevNr  
                         VPIBildeData.VareNr       = VPIArtBas.VareNr.
              END.
              RELEASE VPIBildeRegister.
          END.
          ELSE DO TRANSACTION:
              FIND CURRENT VPIArtBAs EXCLUSIVE-LOCK.          
              VPIArtBas.BildNr = 0.
              FIND CURRENT VPIArtBAs NO-LOCK.              
          END.
      END.

  END. /* ARTIKKEL */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-OverforErstattningsvare) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OverforErstattningsvare Procedure 
PROCEDURE OverforErstattningsvare :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEF BUFFER bufErstattningsvare FOR Erstattningsvare.

ERSTATTNINGSVARE:
DO:
    FIND ArtBas NO-LOCK WHERE
        ArtBas.ArtikkelNr = lArtikkelNr NO-ERROR.
    IF NOT AVAILABLE ArtBas THEN
        LEAVE ERSTATTNINGSVARE.
    FIND FIRST Erstattningsvare OF ArtBas NO-LOCK NO-ERROR.
    IF AVAILABLE Erstattningsvare THEN
    DO:
        FOR EACH VPIErstattningsvare WHERE VPIErstattningsvare.EkstVPILevNr = iEkstVPILevNr AND
                                     VPIErstattningsvare.ErstattId          = Erstattningsvare.ErstattId:
            DELETE VPIErstattningsvare.
        END.
        FOR EACH bufErstattningsvare NO-LOCK WHERE
            bufErstattningsvare.ErstattId = Erstattningsvare.ErstattId:
            CREATE VPIErstattningsvare.
            BUFFER-COPY bufErstattningsvare TO VPIErstattningsvare
                ASSIGN VPIErstattningsvare.EkstVPILevNr = iEkstVPILevNr
                       VPIErstattningsvare.VareNr       = STRING(bufErstattningsvare.ArtikkelNr).
        END.
    END.

END. /* ERSTATTNINGSVARE */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-OverforPakkeLinje) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OverforPakkeLinje Procedure 
PROCEDURE OverforPakkeLinje :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEF BUFFER bufPakkeLinje FOR PakkeLinje.

PAKKELINJE:
DO:
    IF NOT AVAILABLE ArtBas THEN
        LEAVE PAKKELINJE.
    IF ArtBas.Pakke = FALSE THEN
        LEAVE PAKKELINJE.

    /* Pakke og pakkemedlemmer */
    FOR EACH VPIPakkeLinje WHERE VPIPakkeLinje.EkstVPILevNr = iEkstVPILevNr AND
                                 VPIPakkeLinje.PakkeNr      = ArtBas.PakkeNr:
        DELETE VPIPakkeLinje.
    END.
    FOR EACH PakkeLinje WHERE PakkeLinje.ArtikkelNr = ArtBas.ArtikkelNr NO-LOCK:
        CREATE VPIPakkeLinje.
        BUFFER-COPY PakkeLinje TO VPIPakkeLinje
            ASSIGN VPIPakkeLinje.EkstVPILevNr = iEkstVPILevNr
                   VPIPakkeLinje.VareNr       = STRING(PakkeLinje.ArtikkelNr)
                   .
    END.


END. /* PAKKELINJE */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

