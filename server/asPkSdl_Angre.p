
/*------------------------------------------------------------------------
    File        : asPkSdl_Angre.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : 
    Created     : Mon Feb 27 18:31:18 CET 2017
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE INPUT  PARAMETER cPkSdlNr AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER iButNr   AS INTEGER   NO-UNDO. /* Opprinnelig overført fra.               */
DEFINE INPUT  PARAMETER iTilBut  AS INTEGER   NO-UNDO. /* Butikk som skal ha overføringen.        */
DEFINE OUTPUT PARAMETER bOk      AS LOG INITIAL TRUE NO-UNDO.
DEFINE OUTPUT PARAMETER cReturn  AS CHARACTER NO-UNDO.

DEFINE VARIABLE iFeilbut AS INTEGER  NO-UNDO. /* Butikk som feilaktig mottok overføring. */
DEFINE VARIABLE iSentralLager AS INTEGER NO-UNDO.

DEFINE BUFFER feilButiker FOR Butiker.

{syspara.i 22 20 1 iSentrallager INT}


/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */

/* Henter pakkseddel, pakkseddellinje og feil butikk. */
FIND LAST PkSdlHode NO-LOCK WHERE 
    PkSdlHode.PkSdlNr = cPkSdlNr NO-ERROR.
IF AVAILABLE PkSdlHode THEN 
    FIND FIRST PkSdlLinje OF PkSdlHode NO-ERROR.
IF AVAILABLE PkSdlLinje THEN 
    FIND feilButiker NO-LOCK WHERE 
        feilButiker.Butik = PkSdlLinje.ButikkNr NO-ERROR.

/* Setter iFeilBut */
IF AVAILABLE PkSdlLinje THEN 
    iFeilbut = PkSdlLinje.ButikkNr.
ELSE 
    iFeilBut = 0.

/* Sjekker at parametre er gyldige. */
RUN validerParametre.
IF bOk = FALSE THEN
    RETURN cReturn. 

/* Sjekker om pakkseddel er gyldig og ikke mottatt. */
RUN validerPkSdl.
IF bOk = FALSE THEN
    RETURN cReturn. 

/* Skape overføring som fører varene tilbake til butikken og krediter kunde. */
RUN tilbakeforVarer.

/* Pakkseddel flyttes til rktig butikk. */
RUN bytButNrPkSdl.

/* Opprett ny overføring til riktig butikk, og fakturer. */
RUN overforTilbut.
    
/* **********************  Internal Procedures  *********************** */

PROCEDURE bytButNrPkSdl:
/*------------------------------------------------------------------------------
 Purpose: Pakkseddel flyttes til rktig butikk.
            - Butikknr byttes på alle varelinjer.
            - Merknadsfelt blankes, og initieres på nytt. 
            - skriv ut ny pakkseddel i butikken det overføres fra.
 Notes:
------------------------------------------------------------------------------*/


END PROCEDURE.

PROCEDURE overforTilbut:
/*------------------------------------------------------------------------------
 Purpose: Opprett ny overføring til riktig butikk, og fakturer.
            - Opprett overføring og poster denne. TBId = 2.
            - Skriv ut faktura i butikken det overføres til.
 Notes:
------------------------------------------------------------------------------*/


END PROCEDURE.

PROCEDURE tilbakeforVarer:
/*------------------------------------------------------------------------------
 Purpose: Skape overføring som fører varene tilbake til butikken og krediter kunde.
            - Overføring opprettes med neg. antall.
            - Overføringsbong opprettes med TBId = 2. 
            - Overføring posteres, translogg opprettes.
            - Kreditnota opprettes og skrives ut i butikk som feilaktig mottok overføringen.
 Notes:
------------------------------------------------------------------------------*/


END PROCEDURE.

PROCEDURE validerParametre:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    IF cPkSdlNr = '' THEN 
    DO:
        ASSIGN 
            cReturn = '** Pakkseddelnr. er ikke angitt.'
            bOk     = FALSE 
        .
    END. 
    ELSE IF (iFeilBut = 0 OR iTilBut = 0) THEN 
    DO:
        ASSIGN 
            cReturn = '** Feil butikk og ny til butikk nå angis.'
            bOk     = FALSE 
        .
    END.
    ELSE IF NOT AVAILABLE feilButiker THEN  
    DO:
        ASSIGN 
            cReturn = "** Ugyldig 'Feil' butikk angitt."
            bOk     = FALSE 
        .
    END.
    ELSE IF NOT CAN-FIND(Butiker WHERE 
                    Butiker.Butik = iTilBut) THEN  
    DO:
        ASSIGN 
            cReturn = "** Ugyldig 'Til' butikk angitt."
            bOk     = FALSE 
        .
    END.
END PROCEDURE.

PROCEDURE validerPkSdl:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    
    IF NOT AVAILABLE PkSdlHode THEN  
    DO:
        ASSIGN 
            cReturn = "** Ukjent pakkseddnr. angitt (Pksdlnr: " + cPkSdlNr + ")."
            bOk     = FALSE 
        .
    END.
    ELSE IF NOT AVAILABLE PkSdlLinje THEN  
    DO:
        ASSIGN 
            cReturn = "** Det er ingen varelinjer på angitt pakkseddel (Pksdlnr: " + cPkSdlNr + ")."
            bOk     = FALSE 
        .
    END.
    ELSE IF PkSdlHode.PkSdlStatus <> 10 THEN  
    DO:
        ASSIGN 
            cReturn = "** Kun pakksedler med status 'Ny' kan angres (Pksdlnr: " + cPkSdlNr + ")."
            bOk     = FALSE 
        .
    END.
    
    /* Finnes ikke denne fakturaen, er ikke ibutNr butikken som har gjort overføringen. */
    FIND FIRST FakturaHode NO-LOCK WHERE 
        FakturaHode.ButikkNr  = iButNr AND 
        FakturaHode.FakturaNr = DEC(PkSdlHode.PkSdlNr) NO-ERROR.
    IF AVAILABLE FakturaHode THEN  
    DO:
        ASSIGN 
            cReturn = "** Kun utsteder av pakkseddel kan angre overføringen (Pksdlnr: " + cPkSdlNr + ")."
            bOk     = FALSE 
        .
    END.
    
        


END PROCEDURE.

