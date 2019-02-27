&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : asGavekort 
    Purpose     : Oppslag mot gavekort registeret for å se om et gavekort er brukt.

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
DEFINE INPUT  PARAMETER cGavekortNr    AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER cType         AS CHARACTER   NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER dBelopp AS DECIMAL     NO-UNDO.
DEFINE OUTPUT PARAMETER iGKbutikknr   AS INTEGER     NO-UNDO.
DEFINE OUTPUT PARAMETER lOK           AS LOGICAL     NO-UNDO.
DEFINE OUTPUT PARAMETER cMelding      AS CHARACTER   NO-UNDO.

DEFINE VARIABLE cSprak AS CHARACTER   NO-UNDO.

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

/* Sjekker om tilgodelappen finnes fra før. */
FIND bruker WHERE bruker.brukerid = "kasse" NO-LOCK NO-ERROR.
IF bruker.lng = "SE" OR bruker.lng = "SVE" THEN
    cSprak = "".

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
        cMelding = STRING(cSprak = "","Okänt presentkort/Ukjent gavekort").
        
    SJEKK_GAVEKORT:
    FOR EACH Butiker NO-LOCK:
        FIND FIRST Gavekort WHERE 
            Gavekort.Butnr   = Butiker.Butik AND
            Gavekort.Identnr = cGavekortNr NO-LOCK NO-ERROR.
        IF AVAIL Gavekort THEN 
        DO:
            ASSIGN lOK      = Gavekort.Bruktdato = ? AND Gavekort.Gyldigdato >= TODAY AND Gavekort.UtgattDato = ?
                   dBelopp  = Gavekort.Belop
                iGKbutikknr = Gavekort.Butnr.
            IF lOK = TRUE THEN
                cMelding = "".
            ELSE DO:
                IF Gavekort.Bruktdato <> ? THEN
                    cMelding = STRING(cSprak = "","Presentkort använt/Gavekort brukt").
                ELSE IF Gavekort.Gyldigdato < TODAY OR Gavekort.UtgattDato <> ?  THEN
                    cMelding = STRING(cSprak = "","Presentkort utgått/Gavekort utgått").
            END.
            LEAVE SJEKK_GAVEKORT.
        END.
    END. /* SJEKK_GAVEKORT */
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
        
    SJEKK_GAVEKORT:
    FOR EACH Butiker NO-LOCK:
        FIND FIRST Gavekort WHERE 
            Gavekort.Butnr   = Butiker.Butik AND
            Gavekort.Identnr = cGavekortNr NO-LOCK NO-ERROR.
        IF AVAIL Gavekort THEN 
        DO:
            ASSIGN 
                lOK      = FALSE
                cMelding = "Gavekort finnes fra før i butikk: " + STRING(Butiker.Butik).
            LEAVE SJEKK_GAVEKORT.
        END.
    END. /* SJEKK_GAVEKORT */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

