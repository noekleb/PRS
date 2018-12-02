&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : asTilgode 
    Purpose     : Oppslag mot tilgodelapp regsiteret for å se om en tilgodelapp er brukt.

    Syntax      :

    Description :

    Author(s)   :
    Created     :
    Notes       :
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/
/*
  butnr FORMAT ">>>>9" LABEL "Butnr"
  IdentNr FORMAT "X(20)" LABEL "Identnr"
  identtype FORMAT ">9" LABEL "Identtype"
  Modus FORMAT "9" LABEL "Modus"
  Dato FORMAT "99/99/99" LABEL "Dato"
  Tid FORMAT ">>>>9" LABEL "Tid"
  Kassenr FORMAT ">>9" LABEL "Kassenr"
  kassnr FORMAT ">>>>>>>9" LABEL "Kassnr"
  Bongnr FORMAT ">>>>>>>9" LABEL "Bongnr"
  Gyldigdato FORMAT "99/99/99" LABEL "Gyldigdato"
  Belop FORMAT "->>,>>>,>>9.99" LABEL "Belop"
  BruktDato FORMAT "99/99/99" LABEL "Bruktdato"
  BruktTid FORMAT ">>>>9" LABEL "Tid"
  EDato FORMAT "99/99/9999" LABEL "Endret"
  ETid FORMAT "->,>>>,>>9" LABEL "Endret tid"
  BrukerID FORMAT "X(10)" LABEL "Bruker"
  RegistrertDato FORMAT "99/99/9999" LABEL "Registrert dato"
  RegistrertTid FORMAT "->,>>>,>>9" LABEL "Registreringstidspunkt"
  RegistrertAv FORMAT "X(10)" LABEL "Registrert av"
  FraB_Id FORMAT "->>>>>>>>>>>>9" LABEL "BongId"
  BruktB_Id FORMAT "->>>>>>>>>>>>9" LABEL "Brukt BongId"
  BruktButNr FORMAT ">>>>>9" LABEL "Brukt ButNr"
  BruktBongNr FORMAT "zz,zzz,zz9" LABEL "Brukt BongNr"
  MedlemsNr FORMAT ">>>>>>>>>>>>9" LABEL "Medlemsnummer"
  MForNavn FORMAT "X(40)" LABEL "Navn"
  MAdresse1 FORMAT "X(40)" LABEL "Adresse"
  MPostNr FORMAT "X(10)" LABEL "PostNr"
  MTelefon FORMAT "X(15)" LABEL "Telefon"
  MEtterNavn FORMAT "X(40)" LABEL "Etternavn"
  KundeNr FORMAT ">>>>>>>>>>>>9" LABEL "Kundenummer"
  KNavn FORMAT "X(40)" LABEL "Navn"
  KAdresse1 FORMAT "X(40)" LABEL "Adresse"
  KPostNr FORMAT "X(10)" LABEL "PostNr"
  KTelefon FORMAT "X(15)" LABEL "Telefon"
  SelgerNr FORMAT ">>>>>>>>>>>>9" LABEL "Selgernummer"
  BruktKassNr FORMAT ">>9" LABEL "Kassnr"
  BruktSelgerNr FORMAT ">>>>>>>>>>>>9" LABEL "Selgernummer"
  Bruktkassenr FORMAT ">9" LABEL "Kassenr"
  Utgatt FORMAT "J/N" LABEL "Utgått"
  UtgattDato FORMAT "99/99/99" LABEL "Utgått dato"
  UtgattTid FORMAT "->,>>>,>>9" LABEL "Utg.tid"
  UtgattRegAv FORMAT "X(10)" LABEL "Reg.utgått"
  Eget FORMAT "yes/no" LABEL "Eget"
  RabKr FORMAT "->>,>>9.99" LABEL "Rabatt"
  Fakturert FORMAT "yes/no" LABEL "Fakturert"
  FakturertDato FORMAT "99/99/99" LABEL "Fakt.dato"
  FakturaNr FORMAT ">>>>>>>>>>>>9" LABEL "Fakturanummer"
  Faktura_Id FORMAT ">>>>>>>>>>>>9" LABEL "Faktura Id"
*/

/* ***************************  Definitions  ************************** */

DEFINE INPUT  PARAMETER iButikkNr     AS INTEGER     NO-UNDO.
DEFINE INPUT  PARAMETER iKasseNr      AS INTEGER     NO-UNDO.
DEFINE INPUT  PARAMETER iBongNr       AS INTEGER     NO-UNDO.
DEFINE INPUT  PARAMETER cTilgodeNr    AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER cType         AS CHARACTER   NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER dBelopp AS DECIMAL     NO-UNDO.
DEFINE OUTPUT PARAMETER lOK           AS LOGICAL     NO-UNDO.
DEFINE OUTPUT PARAMETER lBrukt        AS LOGICAL     NO-UNDO.
DEFINE OUTPUT PARAMETER cMelding      AS CHARACTER   NO-UNDO.

DEFINE VARIABLE iUtvidetSjekk AS INTEGER   NO-UNDO.
DEFINE VARIABLE cTekst        AS CHARACTER NO-UNDO.
DEFINE VARIABLE iButNr        AS INTEGER   NO-UNDO.
DEFINE VARIABLE lDebug        AS LOG       NO-UNDO.

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

{syspara.i 1 1 63 iUtvidetSjekk INT}


/* OUTPUT TO "C:\tmp\logfile.txt" APPEND. */
/* PUT UNFORMATTED                        */
/* "iButikkNr " iButikkNr   skip          */
/* "iKasseNr  " iKasseNr    skip          */
/* "iBongNr   " iBongNr     skip          */
/* "cTilgodeNr" cTilgodeNr  skip          */
/* "cType     " cType       skip          */
/* "dBelopp   " dBelopp     skip          */
/* "lOK       " lOK         skip          */
/* "lBrukt    " lBrukt      skip          */
/* "cMelding  " cMelding.                 */
/*                                        */
/* OUTPUT CLOSE.                          */


ASSIGN
    lDebug = TRUE.
    
/* Sjekker om tilgodelappen finnes fra før. */
CASE cType:
  WHEN '1' THEN RUN sjekkOmFinnes.
  WHEN '2' THEN RUN hentBelop.
END CASE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-hentBelop) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE hentBelop Procedure 
PROCEDURE hentBelop :
/*------------------------------------------------------------------------------
                        Purpose:                                                                                                                                          
                        Notes:                                                                                                                                            
        ------------------------------------------------------------------------------*/
    ASSIGN 
        lOK      = FALSE
        cMelding = "Ukjent tilgodelapp".
        
    /* Tilgodelapper utstedt fra PRS. */
    IF LENGTH(cTilgodeNr) = 22 THEN 
    SJEKK_PRSTILGODE:
    FOR EACH Butiker NO-LOCK:
        FIND FIRST Tilgode WHERE 
            Tilgode.Butnr   = Butiker.Butik AND
            Tilgode.Identnr = cTilgodeNr NO-LOCK NO-ERROR.
        /* DEBUG */ IF lDebug THEN LOG-MANAGER:WRITE-MESSAGE('Søk etter tilgodelapp ' + cTilgodeNr + 
                                                             (IF AVAILABLE Tilgode 
                                                                 THEN '. Tilgodelapp funnet i butikk' + STRING(Butiker.Butik) + '.' 
                                                                 ELSE '. Ukjent tilgodelapp i butikk ' + STRING(Butiker.Butik) + '.')
                                                             ,'DEBUG').    
        IF AVAIL Tilgode AND Tilgode.bruktdato = ? THEN 
        DO:
            ASSIGN 
                lOK      = TRUE
                cMelding = STRING(tilgode.butnr)   /* Används vid ok i kassan */
                dBelopp  = Tilgode.Belop.
            /* DEBUG */ IF lDebug THEN LOG-MANAGER:WRITE-MESSAGE('Tilgode ' + cTilgodeNr + ' Beløp ' + STRING(Tilgode.Belop) + '.','DEBUG').    
            LEAVE SJEKK_PRSTILGODE.
        END.
        ELSE DO:
            IF AVAIL tilgode AND tilgode.bruktdato <> ? THEN DO:
                ASSIGN 
                    lOK      = FALSE
                    lBrukt   = TRUE
                    cMelding = "Tidligere brukt".
                /* DEBUG */ IF lDebug THEN LOG-MANAGER:WRITE-MESSAGE('Tilgode ' + cTilgodeNr + ' tidligere brukt ' + 
                                                                      STRING(tilgode.bruktdato) + ' i butikk ' + 
                                                                      STRING(Tilgode.BruktButNr) + '.','DEBUG').    
                LEAVE SJEKK_PRSTILGODE.
            END.
            ELSE DO:
                /* DEBUG */ IF lDebug THEN LOG-MANAGER:WRITE-MESSAGE('Ukjent tilgodelapp ' + cTilgodeNr + '.','DEBUG').    
            END.
        END.
    END. /* SJEKK_TILGODE */
    
    ELSE IF iUtvidetSjekk > 0 THEN 
    UTVIDET_SJEKK:
    DO:
        ASSIGN iButNr = INT(SUBSTRING(cTilgodeNr,1,iUtvidetSjekk)) NO-ERROR.
        IF ERROR-STATUS:ERROR THEN
        DO: 
            ASSIGN 
                lOK      = FALSE
                cMelding = "Feil i angitt butikknr. prefix ".
            /* DEBUG */ IF lDebug THEN LOG-MANAGER:WRITE-MESSAGE('Feil i angitt butikknr. prefix  ' + cTilgodeNr + '.','DEBUG').    
            LEAVE UTVIDET_SJEKK.
        END.
        IF iButNr = 0 OR NOT CAN-FIND(FIRST Butiker WHERE Butiker.Butik = iButNr) THEN
        DO: 
            ASSIGN 
                lOK      = FALSE
                cMelding = "Butikknr. 0 eller ukjent butikk i prefix".
            /* DEBUG */ IF lDebug THEN LOG-MANAGER:WRITE-MESSAGE('Butikknr. 0 eller ukjent butikk i prefix ' + cTilgodeNr + '.','DEBUG').    
            LEAVE UTVIDET_SJEKK.
        END.
        FIND FIRST Tilgode WHERE 
            Tilgode.Butnr   = iButNr AND
            Tilgode.Identnr = SUBSTRING(cTilgodeNr,iUtvidetSjekk + 1) NO-LOCK NO-ERROR.
        /* DEBUG */ IF lDebug THEN LOG-MANAGER:WRITE-MESSAGE('Søk etter tilgodelapp ' + SUBSTRING(cTilgodeNr,iUtvidetSjekk + 1) + 
                                                             (IF AVAILABLE Tilgode 
                                                                 THEN '. Tilgodelapp funnet i butikk ' + STRING(iButNr) + '.' 
                                                                 ELSE '. Ukjent tilgodelapp i butikk ' + STRING(iButNr) + '.')
                                                             ,'DEBUG').    
        IF AVAIL Tilgode AND Tilgode.bruktdato = ? THEN 
        DO:
            ASSIGN 
                lOK      = TRUE
                cMelding = ""
                dBelopp  = Tilgode.Belop.
            /* DEBUG */ IF lDebug THEN LOG-MANAGER:WRITE-MESSAGE('Tilgode ' + cTilgodeNr + ' Beløp ' + STRING(Tilgode.Belop) + '.','DEBUG').    
            LEAVE UTVIDET_SJEKK.
        END.
        ELSE DO:
            IF AVAIL tilgode AND tilgode.bruktdato <> ? THEN DO:
                ASSIGN 
                    lOK      = FALSE
                    lBrukt   = TRUE
                    cMelding = "Tidligere brukt".
                /* DEBUG */ IF lDebug THEN LOG-MANAGER:WRITE-MESSAGE('Tilgode ' + cTilgodeNr + ' tidligere brukt ' + 
                                                                      STRING(tilgode.bruktdato) + ' i butikk ' + 
                                                                      STRING(Tilgode.BruktButNr) + '.','DEBUG').    
                LEAVE UTVIDET_SJEKK.
            END.
            ELSE DO:
                /* DEBUG */ IF lDebug THEN LOG-MANAGER:WRITE-MESSAGE('Ukjent tilgodelapp ' + cTilgodeNr + '.','DEBUG').    
            END.
        END.
    END. /* UTVIDET_SJEKK */
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-sjekkOmFinnes) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE sjekkOmFinnes Procedure 
PROCEDURE sjekkOmFinnes :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    ASSIGN 
        lOK = TRUE
        cMelding = ''. 
        
    SJEKK_TILGODE:
    FOR EACH Butiker NO-LOCK:
        FIND FIRST Tilgode WHERE 
            Tilgode.butnr   = Butiker.Butik AND
            Tilgode.Identnr = cTilgodeNr NO-LOCK NO-ERROR.
        IF AVAIL Tilgode THEN 
        DO:
            ASSIGN 
                lOK      = FALSE.
                cMelding = "Tilgodelapp (" + cTilgodeNr + ") finnes fra før i butikk ." + STRING(Butiker.Butik) + ".".
            LEAVE SJEKK_TILGODE.
        END.
    END. /* SJEKK_TILGODE */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

