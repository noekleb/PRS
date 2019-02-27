&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
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

DEFINE INPUT  PARAMETER cButikkListe AS CHARACTER   NO-UNDO.
DEFINE        VARIABLE  cPDFfiler    AS CHARACTER   NO-UNDO.

DEFINE VARIABLE cCLMailfra AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cSubject AS CHARACTER   NO-UNDO.

DEFINE TEMP-TABLE ttPlliste NO-UNDO
    FIELD Pllisteid AS DECI FORMAT ">>>>>>>>9"
    FIELD LevNr      AS INTE
    FIELD Levnamn   AS CHAR
    FIELD LevFargKod AS CHAR
    FIELD Alfastr    AS CHAR
    FIELD Antall     AS INTE
    FIELD Kostforslag AS DECI
    FIELD Artikkelnr AS DECI
    FIELD StrSeq     AS INTE
    FIELD Beskr      AS CHARACTER
    FIELD StrKode    AS INTEGER 

    INDEX LBA IS PRIMARY levnr beskr artikkelnr.

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

RUN butikkloop.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-butikkloop) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE butikkloop Procedure 
PROCEDURE butikkloop :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE ii          AS INTEGER     NO-UNDO.
DEFINE VARIABLE iCL         AS INTEGER     NO-UNDO.
DEFINE VARIABLE iCLprofilnr AS INTEGER     NO-UNDO.
DEFINE VARIABLE cRappfil    AS CHARACTER   NO-UNDO.
DEFINE VARIABLE TTh AS HANDLE      NO-UNDO.

TTh = TEMP-TABLE ttPlliste:DEFAULT-BUFFER-HANDLE.
{syspara.i 5 1 1 iCL INT}

/* Default avsender. */
{syspara.i 50 50 29 cCLMailfra}
cCLMailfra = TRIM(cCLMailfra). 

/* Henter info fra sentrallager */
FIND butiker WHERE butiker.butik = iCL NO-LOCK NO-ERROR.
IF AVAIL butiker THEN DO:
    cCLMailfra  = (IF cCLMailfra = '' THEN Butiker.ePostAdresse ELSE cCLMailfra).
    iCLprofilnr = butiker.profilnr.
    cSubject    = "Forslag supplering " + Butiker.butnamn.
END.

DO ii = 1 TO NUM-ENTRIES(cButikkListe):
    EMPTY TEMP-TABLE ttPlliste.
    FIND butiker WHERE butiker.butik    = INT(ENTRY(ii,cButikkListe)) NO-LOCK NO-ERROR.
    IF NOT AVAIL butiker THEN
        NEXT.
    FIND FIRST plListeHode NO-LOCK WHERE
               plListeHode.PlListeStatus = 1 AND
               plListeHode.PlLType       = 2 AND 
               plListeHode.FraButikkNr   = butiker.butik NO-ERROR.
    IF NOT AVAIL plListeHode OR NOT CAN-FIND(FIRST pllistelinje OF plListeHode WHERE plListelinje.antall > 0) THEN
        NEXT.
    LISTELINJE:
    FOR EACH plListelinje OF plListeHode NO-LOCK:
        FIND artbas WHERE artbas.artikkelnr = pllistelinje.artikkelnr NO-LOCK NO-ERROR.
        IF NOT AVAILABLE ArtBas THEN NEXT LISTELINJE.
        FIND artpris WHERE artpris.artikkelnr = artbas.artikkelnr AND artpris.profilnr = butiker.profilnr NO-LOCK NO-ERROR.
        FIND levbas OF artbas NO-LOCK NO-ERROR.
        IF NOT AVAIL artpris THEN
            FIND artpris WHERE artpris.artikkelnr = artbas.artikkelnr AND artpris.profilnr = iCLprofilnr NO-LOCK NO-ERROR.
        IF NOT AVAIL artpris THEN
            FIND FIRST artpris WHERE artpris.artikkelnr = artbas.artikkelnr NO-LOCK NO-ERROR.
        FIND FIRST StrKonv NO-LOCK
             WHERE StrKonv.StrKode = PlListeLinje.StrKode
             NO-ERROR.
        IF AVAIL StrKonv THEN DO:
          FIND FIRST StrTStr NO-LOCK
               WHERE StrTStr.StrTypeID     = ArtBas.StrTypeID
                 AND TRIM(StrTStr.SoStorl) = TRIM(StrKonv.Storl)
               NO-ERROR.
          CREATE ttPlliste.
          ASSIGN ttPlliste.Pllisteid   = plListeHode.plListeId
                 ttPlliste.LevNr       = Artbas.levnr
                 ttPlliste.LevNamn     = (IF AVAILABLE LevBas THEN levbas.levnamn ELSE '')
                 ttPlliste.LevFargKod  = artbas.LevFargKod
                 ttPlliste.Alfastr     = IF AVAIL strkonv THEN strkonv.storl ELSE ""
                 ttPlliste.Antall      = plListeLinje.Antall
                 ttPlliste.Kostforslag = artpris.varekos[1] * plListeLinje.Antall
                 ttPlliste.Artikkelnr  = artbas.artikkelnr
                 ttPlliste.StrSeq      = IF AVAIL StrTStr THEN StrTStr.SeqNr ELSE 0
                 ttPlliste.Beskr       = Artbas.beskr
                 ttPlListe.StrKode     = plListeLinje.StrKode.

        END.
    END. /* LISTELINJE */
    IF CAN-FIND(FIRST ttPlliste) THEN DO:
        RUN skrivPllistelinjePDF.p (TTh,"F",OUTPUT cRappfil).
        IF TRIM(cRappFil) <> "" THEN DO:
            ASSIGN cPDFfiler = cPDFfiler + (IF cPDFfiler <> "" THEN "," ELSE "") + trim(cRappFil).
            RUN SendEmailButikk (plListeHode.FraButikkNr,TRIM(cRappFil)).
            DO TRANSACTION:
                FIND CURRENT PlListeHode EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
                IF AVAILABLE PlListeHode AND NOT LOCKED PlListeHode THEN 
                    ASSIGN 
                    PlListeHode.plListeStatus = 20. /* Status satt til 'Rediger' */
                IF AVAILABLE PlListeHode THEN 
                    FIND CURRENT PlListeHode NO-LOCK NO-ERROR.
            END.
        END.
    END.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SendEmailButikk) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SendEmailButikk Procedure 
PROCEDURE SendEmailButikk :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 DEFINE INPUT PARAMETER iButikkNr AS INTEGER   NO-UNDO.
 DEFINE INPUT PARAMETER icFiler   AS CHARACTER NO-UNDO.

 DEFINE BUFFER mailButiker FOR Butiker.
 
 ASSIGN 
   cSubject = "Forslag supplering" + STRING(iButikkNr) + '.'
   .
    
  FILE-INFO:FILE-NAME = icFiler.
  
  RUN sendmail_tsl.p ("SUPPLORDRE",
                      cSubject,
                      FILE-INFO:FULL-PATHNAME,
                      'Automatisk generert suppleringsforslag ' + STRING(NOW) + '.',                        
                      "",
                      "") NO-ERROR.


END PROCEDURE.

&ENDIF
