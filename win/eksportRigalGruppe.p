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
DEFINE INPUT  PARAMETER cLanButiker  AS CHARACTER  NO-UNDO.
DEFINE INPUT  PARAMETER cFtpButiker  AS CHARACTER  NO-UNDO.
DEFINE INPUT  PARAMETER cExportDir      AS CHARACTER  NO-UNDO.
DEFINE OUTPUT PARAMETER cGruppeFiler AS CHARACTER  NO-UNDO.
DEFINE OUTPUT PARAMETER iAntGruppe   AS INTEGER    NO-UNDO.
DEFINE        VARIABLE  iCount       AS INTEGER    NO-UNDO.
/* DEFINE        VARIABLE  cExportDir   AS CHARACTER INIT "c:\home\lindbak\sendes\" NO-UNDO. */
DEFINE VARIABLE iWeekNum         AS INTEGER    NO-UNDO.
DEFINE VARIABLE cTabellNavnListe AS CHARACTER INIT "Avdeling,HuvGr,VarGr" NO-UNDO.
DEFINE VARIABLE cKodeListe       AS CHARACTER INIT "GAV,GVO,GHG" NO-UNDO.

DEFINE TEMP-TABLE TT_RigalGruppe
    FIELD cKode            AS CHARACTER   /* Kode      se cKodeListe */
    FIELD dEdato           AS DECI        /* Ändringsdato */
/*     FIELD dEdato           AS DATE        /* Ändringsdato */ */
    FIELD iGRP_nummer      AS INTEGER   /* C(30)*/
    FIELD cGRP_navn        AS CHARACTER   /* Adresse   C(30) */
    FIELD iGRP_link        AS INTEGER   /* Adresse   C(30) */
    FIELD dGRP_brutto      AS DECIMAL
    FIELD iGRP_bonusgrp    AS INTEGER
    FIELD dGRP-avgiftssats AS DECIMAL
    FIELD dGRP_persrab     AS DECIMAL
    FIELD cFlag            AS CHARACTER
    FIELD dKunderabatt     AS DECIMAL EXTENT 9
    FIELD iStrTypeNr       AS INTEGER
    INDEX Kodenummer cKode ASCENDING iGRP_nummer ASCENDING
    .


/*
10.1.1 Kode             "GHG"                    Varegruppe     -> VarGr
                        "GEN"                    ENVA          
                        "GVO"                    Vareområde     -> HuvGr
                        "GAV"                    Avdeling       -> Avdeling
                        "GMD"                    Modell        
                        "GFA"                    Farge          ->
                        "GSØ"                    Størrelse     
                        "GST"                    Størrelsestype
                        "GFB"                    Fabrikat      
                        "GSE"                    Sesongkode    
                        "GMA"                    Matrialkode   
10.1.2 Dato             Endringsdato  
10.1.3 GRP_nummer       Gruppenummer      I (8)  Inntil 8 siffer kun for Modell, inntil 2 - 4 siffer for de andre
10.1.4 GRP_navn         Gruppenavn        C (30) 
10.1.5 GRP_link         Vareområde        I      Link til overliggende gruppe (vareområde) over varegruppe og over vareområde (avdeling) ), størrelsestype for størrelse
10.1.6 GRP_brutto       Bruttofortjeneste D      Standard bruttofortjeneste for varegruppen
10.1.7 GRP_bonusgrp     Bonusgruppe       I      Standard bonusgruppe for varer i varegruppen
10.1.8 GRP_avgiftssats  MVA-sats          D      Standard mva-sats for varer i varegruppen
10.1.9 GRP_persrab      Personalrabatt    D      Standard personalrabatt for gruppen
10.1.10 Flag            Funksjonskode     C(1)   "N" = Nyopplegg"E" = Endring"S" = Sletting
10.1.11 Kndrabatt       Kunderabatt       D x 9  9 %-satser som kan angi rabatt pr kundegruppe. Kun for varegruppe
10.1.12 Strtypenr       Størrelsestypenr  I (3)  Modellens størrelsestype (inndeling av farge/størrelse)

*/

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
      TABLE: TT_ELogg T "?" NO-UNDO data ELogg
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Procedure ASSIGN
         HEIGHT             = 12.91
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */
DO iCount = 1 TO NUM-ENTRIES(cTabellNavnListe):
    RUN KopierElogg (ENTRY(iCount,cTabellNavnListe)).
END.
RUN weeknum.p (INPUT today,OUTPUT iWeekNum).
ASSIGN iWeekNum = INT(SUBSTR(STRING(iWeekNum),5)).
RUN FixSlettePoster.
DO iCount = 1 TO NUM-ENTRIES(cTabellNavnListe):
    CASE ENTRY(iCount,cTabellNavnListe):
        WHEN "Avdeling" THEN
            RUN FixAvdelingEndringer ("Avdeling",ENTRY(iCount,cKodeListe)).
        WHEN "HuvGr" THEN
            RUN FixHuvGrEndringer ("HuvGr",ENTRY(iCount,cKodeListe)).
        WHEN "VarGr" THEN
            RUN FixVarGrEndringer ("VarGr",ENTRY(iCount,cKodeListe)).
    END CASE.
    
END.

IF cLanButiker <> "" THEN DO:
  DO iCount = 1 TO NUM-ENTRIES(cLanButiker):
      RUN ExportGruppe IN THIS-PROCEDURE (INT(ENTRY(iCount,cLanButiker))). /* parameter = den loopade butiken */
  END.
END.
                                   
DO iCount = 1 TO NUM-ENTRIES(cTabellNavnListe):
    RUN SlettTT_ELoggGruppe (ENTRY(iCount,cTabellNavnListe)).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-ExportGruppe) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ExportGruppe Procedure 
PROCEDURE ExportGruppe :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER iButik AS INTEGER    NO-UNDO.
    DEFINE VARIABLE  iCount     AS INTEGER    NO-UNDO.
    DEFINE VARIABLE  cNumericFormat    AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE  cDateFormat       AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE  cFilNavn          AS CHARACTER  NO-UNDO.
    ASSIGN cNumericFormat         = SESSION:NUMERIC-FORMAT
           cDateFormat            = SESSION:DATE-FORMAT
           SESSION:NUMERIC-FORMAT = "American"
           SESSION:DATE-FORMAT    = "dmy"
           cFilNavn = cExportDir + "G00" + STRING(MONTH(TODAY),"99") + STRING(DAY(TODAY),"99") + STRING(iButik / 1000,"9.999").
    IF CAN-FIND(FIRST TT_RigalGruppe) THEN DO:
        IF SEARCH(cFilNavn) = ? THEN DO:
            OUTPUT TO VALUE(cFilNavn) CONVERT TARGET "IBM850".
            PUT UNFORMATTED "RIGAL02,8.0,......." SKIP.
        END.
        ELSE
            OUTPUT TO VALUE(cFilNavn) APPEND CONVERT TARGET "IBM850".
        FOR EACH TT_RigalGruppe:
            EXPORT DELIMITER ","
                   TT_RigalGruppe.
            ASSIGN iAntGruppe = iAntGruppe + 1.
        END.
        OUTPUT CLOSE.
        FILE-INFO:FILE-NAME = cFilNavn.
        IF FILE-INFO:FILE-SIZE = 0 THEN
            OS-DELETE VALUE(cFilNavn).
        ELSE IF NOT CAN-DO(cGruppeFiler,cFilNavn) THEN
            ASSIGN cGruppeFiler = cGruppeFiler + (IF cGruppeFiler = "" THEN "" ELSE ",") + cFilNavn.
    END.
    ASSIGN SESSION:NUMERIC-FORMAT = cNumericFormat
           SESSION:DATE-FORMAT    = cDateFormat.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-FixAvdelingEndringer) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE FixAvdelingEndringer Procedure 
PROCEDURE FixAvdelingEndringer PRIVATE :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER cTabellNavn AS CHARACTER  NO-UNDO.
  DEFINE INPUT  PARAMETER cKode       AS CHARACTER  NO-UNDO.
  IF CAN-FIND(FIRST TT_ELogg WHERE TT_ELogg.TabellNavn = cTabellNavn AND
              TT_ELogg.EksterntSystem = "POS"    AND
              TT_ELogg.EndringsType   = 1  AND
              TT_Elogg.Verdier       = "ALLE") THEN DO:
      FOR EACH Avdeling NO-LOCK:
          CREATE TT_RigalGruppe.
          ASSIGN TT_RigalGruppe.cKode            = cKode
                 TT_RigalGruppe.dEdato           = DECI(STRING(YEAR(TODAY),"9999") + STRING(MONTH(TODAY),"99") + STRING(DAY(TODAY),"99") + STRING(iweeknum,"99"))
/*                  TT_RigalGruppe.dEdato           = TODAY */
                 TT_RigalGruppe.iGRP_nummer      = Avdeling.AvdelingNr
                 TT_RigalGruppe.cGRP_navn        = SUBSTR(Avdeling.AvdelingNavn,1,30)
                 TT_RigalGruppe.iGRP_link        = 0
                 TT_RigalGruppe.dGRP_brutto      = 0
                 TT_RigalGruppe.iGRP_bonusgrp    = 0
                 TT_RigalGruppe.dGRP-avgiftssats = 0
                 TT_RigalGruppe.dGRP_persrab     = 0
                 TT_RigalGruppe.cFlag            = "E"
                 TT_RigalGruppe.dKunderabatt     = 0
                 TT_RigalGruppe.iStrTypeNr       = 0
              .
      END.
  END.
  ELSE DO:
      FOR EACH TT_ELogg WHERE 
                  TT_ELogg.TabellNavn     = cTabellNavn AND
                  TT_ELogg.EksterntSystem = "POS"  AND
                  TT_ELogg.EndringsType   = 1:
          FIND Avdeling WHERE Avdeling.AvdelingNr = INT(TT_ELogg.Verdier) NO-LOCK NO-ERROR.
          IF AVAIL Avdeling THEN DO:
              CREATE TT_RigalGruppe.
              ASSIGN TT_RigalGruppe.cKode            = cKode
                     TT_RigalGruppe.dEdato           = DECI(STRING(YEAR(TODAY),"9999") + STRING(MONTH(TODAY),"99") + STRING(DAY(TODAY),"99") + STRING(iweeknum,"99"))
 /*                  TT_RigalGruppe.dEdato           = TODAY */
                     TT_RigalGruppe.iGRP_nummer      = Avdeling.AvdelingNr
                     TT_RigalGruppe.cGRP_navn        = SUBSTR(Avdeling.AvdelingNavn,1,30)
                     TT_RigalGruppe.iGRP_link        = 0
                     TT_RigalGruppe.dGRP_brutto      = 0
                     TT_RigalGruppe.iGRP_bonusgrp    = 0
                     TT_RigalGruppe.dGRP-avgiftssats = 0
                     TT_RigalGruppe.dGRP_persrab     = 0
                     TT_RigalGruppe.cFlag            = "E"
                     TT_RigalGruppe.dKunderabatt     = 0
                     TT_RigalGruppe.iStrTypeNr       = 0
                  .
          END.
      END.
  END.
  RELEASE TT_RigalGruppe.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-FixHuvGrEndringer) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE FixHuvGrEndringer Procedure 
PROCEDURE FixHuvGrEndringer :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER cTabellNavn AS CHARACTER  NO-UNDO.
  DEFINE INPUT  PARAMETER cKode       AS CHARACTER  NO-UNDO.
  IF CAN-FIND(FIRST TT_ELogg WHERE TT_ELogg.TabellNavn = cTabellNavn AND
              TT_ELogg.EksterntSystem = "POS"    AND
              TT_ELogg.EndringsType   = 1  AND
              TT_Elogg.Verdier       = "ALLE") THEN DO:
      FOR EACH HuvGr NO-LOCK:
          CREATE TT_RigalGruppe.
          ASSIGN TT_RigalGruppe.cKode            = cKode
                 TT_RigalGruppe.dEdato           = DECI(STRING(YEAR(TODAY),"9999") + STRING(MONTH(TODAY),"99") + STRING(DAY(TODAY),"99") + STRING(iweeknum,"99"))
/*                  TT_RigalGruppe.dEdato           = TODAY */
                 TT_RigalGruppe.iGRP_nummer      = HuvGr.Hg
                 TT_RigalGruppe.cGRP_navn        = SUBSTR(HuvGr.HgBeskr,1,30)
                 TT_RigalGruppe.iGRP_link        = HuvGr.AvdelingNr
                 TT_RigalGruppe.dGRP_brutto      = 0
                 TT_RigalGruppe.iGRP_bonusgrp    = 0
                 TT_RigalGruppe.dGRP-avgiftssats = 0
                 TT_RigalGruppe.dGRP_persrab     = 0
                 TT_RigalGruppe.cFlag            = "E"
                 TT_RigalGruppe.dKunderabatt     = 0
                 TT_RigalGruppe.iStrTypeNr       = 0
              .
      END.
  END.
  ELSE DO:
      FOR EACH TT_ELogg WHERE 
                  TT_ELogg.TabellNavn     = cTabellNavn AND
                  TT_ELogg.EksterntSystem = "POS"  AND
                  TT_ELogg.EndringsType   = 1:
          FIND HuvGr WHERE HuvGr.Hg = INT(TT_ELogg.Verdier) NO-LOCK NO-ERROR.
          IF AVAIL HuvGr THEN DO:
              CREATE TT_RigalGruppe.
              ASSIGN TT_RigalGruppe.cKode            = cKode
                     TT_RigalGruppe.dEdato           = DECI(STRING(YEAR(TODAY),"9999") + STRING(MONTH(TODAY),"99") + STRING(DAY(TODAY),"99") + STRING(iweeknum,"99"))
 /*                  TT_RigalGruppe.dEdato           = TODAY */
                     TT_RigalGruppe.iGRP_nummer      = HuvGr.Hg
                     TT_RigalGruppe.cGRP_navn        = SUBSTR(HuvGr.HgBeskr,1,30)
                     TT_RigalGruppe.iGRP_link        = HuvGr.AvdelingNr
                     TT_RigalGruppe.dGRP_brutto      = 0
                     TT_RigalGruppe.iGRP_bonusgrp    = 0
                     TT_RigalGruppe.dGRP-avgiftssats = 0
                     TT_RigalGruppe.dGRP_persrab     = 0
                     TT_RigalGruppe.cFlag            = "E"
                     TT_RigalGruppe.dKunderabatt     = 0
                     TT_RigalGruppe.iStrTypeNr       = 0
                  .
          END.
      END.
  END.
  RELEASE TT_RigalGruppe.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-FixSlettePoster) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE FixSlettePoster Procedure 
PROCEDURE FixSlettePoster :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE iCount  AS INTEGER    NO-UNDO.
  DO iCount = 1 TO NUM-ENTRIES(cTabellNavnListe):
    FOR EACH TT_ELogg WHERE 
                TT_ELogg.TabellNavn     = ENTRY(iCount,cTabellNavnListe) AND
                TT_ELogg.EksterntSystem = "POS"    AND
                TT_ELogg.EndringsType   = 3:
            CREATE TT_RigalGruppe.
            ASSIGN TT_RigalGruppe.cKode       = ENTRY(iCount,cKodeListe)
                   TT_RigalGruppe.dEdato           = DECI(STRING(YEAR(TODAY),"9999") + STRING(MONTH(TODAY),"99") + STRING(DAY(TODAY),"99") + STRING(iweeknum,"99"))
/*                  TT_RigalGruppe.dEdato           = TODAY */
                   TT_RigalGruppe.iGRP_nummer = INT(TT_ELogg.Verdier)
                   TT_RigalGruppe.cFlag       = "S".
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-FixVarGrEndringer) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE FixVarGrEndringer Procedure 
PROCEDURE FixVarGrEndringer :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER cTabellNavn AS CHARACTER  NO-UNDO.
  DEFINE INPUT  PARAMETER cKode       AS CHARACTER  NO-UNDO.
  IF CAN-FIND(FIRST TT_ELogg WHERE TT_ELogg.TabellNavn = cTabellNavn AND
              TT_ELogg.EksterntSystem = "POS"    AND
              TT_ELogg.EndringsType   = 1  AND
              TT_Elogg.Verdier       = "ALLE") THEN DO:
      FOR EACH VarGr NO-LOCK:
          CREATE TT_RigalGruppe.
          ASSIGN TT_RigalGruppe.cKode            = cKode
                 TT_RigalGruppe.dEdato           = DECI(STRING(YEAR(TODAY),"9999") + STRING(MONTH(TODAY),"99") + STRING(DAY(TODAY),"99") + STRING(iweeknum,"99"))
/*                  TT_RigalGruppe.dEdato           = TODAY */
                 TT_RigalGruppe.iGRP_nummer      = VarGr.Vg
                 TT_RigalGruppe.cGRP_navn        = SUBSTR(VarGr.VgBeskr,1,30)
                 TT_RigalGruppe.iGRP_link        = VarGr.Hg
                 TT_RigalGruppe.dGRP_brutto      = 0
                 TT_RigalGruppe.iGRP_bonusgrp    = 0
                 TT_RigalGruppe.dGRP-avgiftssats = 0
                 TT_RigalGruppe.dGRP_persrab     = 0
                 TT_RigalGruppe.cFlag            = "E"
                 TT_RigalGruppe.dKunderabatt     = 0
                 TT_RigalGruppe.iStrTypeNr       = 0
              .
      END.
  END.
  ELSE DO:
      FOR EACH TT_ELogg WHERE 
                  TT_ELogg.TabellNavn     = cTabellNavn AND
                  TT_ELogg.EksterntSystem = "POS"  AND
                  TT_ELogg.EndringsType   = 1:
          FIND VarGr WHERE VarGr.Vg = INT(TT_ELogg.Verdier) NO-LOCK NO-ERROR.
          IF AVAIL VarGr THEN DO:
              CREATE TT_RigalGruppe.
              ASSIGN TT_RigalGruppe.cKode            = cKode
                     TT_RigalGruppe.dEdato           = DECI(STRING(YEAR(TODAY),"9999") + STRING(MONTH(TODAY),"99") + STRING(DAY(TODAY),"99") + STRING(iweeknum,"99"))
 /*                  TT_RigalGruppe.dEdato           = TODAY */
                     TT_RigalGruppe.iGRP_nummer      = VarGr.Vg
                     TT_RigalGruppe.cGRP_navn        = SUBSTR(VarGr.VgBeskr,1,30)
                     TT_RigalGruppe.iGRP_link        = VarGr.Hg
                     TT_RigalGruppe.dGRP_brutto      = 0
                     TT_RigalGruppe.iGRP_bonusgrp    = 0
                     TT_RigalGruppe.dGRP-avgiftssats = 0
                     TT_RigalGruppe.dGRP_persrab     = 0
                     TT_RigalGruppe.cFlag            = "E"
                     TT_RigalGruppe.dKunderabatt     = 0
                     TT_RigalGruppe.iStrTypeNr       = 0
                  .
          END.
      END.
  END.
  RELEASE TT_RigalGruppe.
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
    DEFINE INPUT  PARAMETER cTabellNavn AS CHARACTER  NO-UNDO.
    DEFINE BUFFER bElogg FOR Elogg.
    FOR EACH ELogg WHERE ELogg.TabellNavn = cTabellNavn AND
                         ELogg.EksterntSystem = "POS" NO-LOCK:
        BUFFER-COPY ELogg TO TT_ELogg NO-ERROR.
        FIND bElogg WHERE ROWID(bElogg) = ROWID(Elogg) EXCLUSIVE NO-WAIT NO-ERROR.
        IF AVAIL bElogg THEN
            DELETE bELogg.
        IF AVAILABLE TT_Elogg THEN
            RELEASE TT_ELogg.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SlettTT_ELoggGruppe) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SlettTT_ELoggGruppe Procedure 
PROCEDURE SlettTT_ELoggGruppe :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER cTabellNavn AS CHARACTER  NO-UNDO.
    FOR EACH TT_ELogg WHERE TT_ELogg.TabellNavn     = cTabellNavn AND
                       TT_ELogg.EksterntSystem = "POS".
        DELETE TT_ELogg.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

