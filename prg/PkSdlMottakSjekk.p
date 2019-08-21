
/*------------------------------------------------------------------------
    File        : LCPrisImport.p
    Purpose     : Få satt inn LC så hurtig og korrekt som mulig i PRS.

    Syntax      :

    Description : Import av Landed cost (LC) fra Gant Global    

    Author(s)   : Tom Nøkleby
    Created     : Tue Jul 17 10:08:20 CEST 2018
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE VARIABLE cImpFilLst    AS CHARACTER NO-UNDO.
DEFINE VARIABLE cBkufilLst    AS CHARACTER NO-UNDO.
DEFINE VARIABLE cImpFil       AS CHARACTER NO-UNDO.
DEFINE VARIABLE cBkufil       AS CHARACTER NO-UNDO.
DEFINE VARIABLE cUtFil AS CHAR NO-UNDO. 
DEFINE VARIABLE cImpKatalog   AS CHARACTER NO-UNDO.
DEFINE VARIABLE cBkuKatalog   AS CHARACTER NO-UNDO.
DEFINE VARIABLE cTekst        AS CHARACTER NO-UNDO.
DEFINE VARIABLE cLogg         AS CHARACTER NO-UNDO.
DEFINE VARIABLE cTimeStamp    AS CHARACTER NO-UNDO.
DEFINE VARIABLE bOk           AS LOG       INITIAL FALSE NO-UNDO.
DEFINE VARIABLE iAntLinjer    AS INTEGER   NO-UNDO.
DEFINE VARIABLE iLoop         AS INTEGER   NO-UNDO.
DEFINE VARIABLE lDec          AS DECIMAL   NO-UNDO.
DEFINE VARIABLE lDato         AS DATE      NO-UNDO.
DEFINE VARIABLE bFunnet       AS LOG       NO-UNDO.
DEFINE VARIABLE iSelgerId     AS INTEGER   NO-UNDO.
DEFINE VARIABLE cExcelFilNavn AS CHARACTER NO-UNDO.
DEFINE VARIABLE iSheets       AS INTEGER   NO-UNDO.

DEFINE TEMP-TABLE ttPkSdl
    FIELD PksdlNr AS CHARACTER FORMAT "x(20)"
    INDEX idxPksdlNrAnsattNr AS PRIMARY PkSdlNr.

DEFINE VARIABLE rStandardFunksjoner AS CLASS cls.StdFunk.StandardFunksjoner NO-UNDO.

DEFINE STREAM Inn.
DEFINE STREAM Ut.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
rStandardFunksjoner = NEW cls.StdFunk.StandardFunksjoner( ).

IF SEARCH('tnc.txt') <> ? THEN  
    ASSIGN 
        cImpKatalog = 'kom\in\'
        cBkuKatalog = 'kom\in\bku\'
        .
ELSE         
    ASSIGN 
        cImpKatalog = 'c:\home\lindbak\ankommet\'
        cBkuKatalog = 'c:\home\lindbak\ankommet\bku\'
        .
ASSIGN
    cUtFil     = 'konv\PRSPkSdlSjekk' + REPLACE(STRING(TODAY),'/','') + '_' + REPLACE(STRING(TIME,"HH:MM:SS"),':','') + '.csv'
    cTimeStamp = REPLACE(STRING(TODAY),'/','') + '_' + REPLACE(STRING(TIME,"HH:MM:SS"),':','')
    cLogg      = 'PkSdlMottakSjekk' + REPLACE(STRING(TODAY),'/','') 
/*    cImpfilLst = 'PRSPkSdlSjekk.csv,PRSPkSdlSjekk.xls,PRSPkSdlSjekk.xlsx'*/
    cImpfilLst = 'PRSPkSdlSjekk.csv' /* Opentext metoden i excel er ikke tilgjengelig hos Gant. Kan ikke konvertere. */
    cBkufilLst = REPLACE(cImpfilLst,'.csv',cTimeStamp + '.csv')
    .

RUN bibl_loggDbFri.p (cLogg, 'Start.'). 

IF cImpfilLst <> '' THEN 
DO iLoop = 1 TO NUM-ENTRIES(cImpFilLst):
    ASSIGN 
        cImpFil = ENTRY(iLoop,cImpFilLst)
        cBkuFil = ENTRY(iLoop,cBkuFilLst)
        .
        
    IF SEARCH(cImpKatalog + cImpfil) <> ? THEN
    DO: 
        RUN importerfil.
        IF bOk AND CAN-FIND(FIRST ttPkSdl) THEN 
            RUN SjekkPakkseddel.
    END. 
    ELSE 
    DO:
        RUN bibl_loggDbFri.p (cLogg, '  Ingen fil å importere.'). 
    END.
    
    IF bOk THEN 
        RUN bkuAvfil.
END.

RUN bibl_loggDbFri.p (cLogg, 'Ferdig.'). 


/* **********************  Internal Procedures  *********************** */

PROCEDURE importerfil:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE BUFFER bufArtBas  FOR ArtBas.
    DEFINE BUFFER bufArtPris FOR ArtPris.

    IF SEARCH(cImpKatalog + cImpfil) = ? THEN
        RETURN.

    /* Er det en excel fil, skal den konverteres til csv. */
    IF CAN-DO('xls,xlsx',ENTRY(NUM-ENTRIES(cImpfil,'.'),cImpfil,'.')) THEN 
    DO:
        cExcelFilNavn = cImpfil.
        rStandardFunksjoner:konvExcel2csv(cExcelFilNavn,'',OUTPUT cImpfil, OUTPUT iSheets).
        RUN bibl_loggDbFri.p (cLogg,
            '  Konvertert excel fil fra: ' + cExcelFilNavn + ' til ' + cImpfil + '.'
            ).    
    END.

    ASSIGN 
        iAntLinjer = 0
        .
    
    RUN bibl_loggDbFri.p (cLogg, '  Importerer fil ' + cImpKatalog + cImpfil + '.'). 
    
    INPUT STREAM Inn FROM VALUE(cImpKatalog + cImpfil).
    IMPLOOP:
    REPEAT TRANSACTION:
        iAntLinjer = iAntLinjer + 1.
        CREATE ttPkSdl.
        IMPORT STREAM Inn DELIMITER ';' 
            ttPkSdl
            NO-ERROR.
        /* Overskriftsrad */
        IF ERROR-STATUS:ERROR OR ttPkSdl.PkSdlNr = '' THEN
        DO:
            IF AVAILABLE ttPkSdl THEN 
                DELETE ttPkSdl.  
            NEXT IMPLOOP.
        END.

        /* Ugyldig ansattnr. */
        ASSIGN 
            lDec = DEC(ttPkSdl.PkSdlNr)
            NO-ERROR.
        IF ERROR-STATUS:ERROR THEN 
            NEXT IMPLOOP.
        
    END. /* IMPLOOP TRANSACTION */
    INPUT STREAM Inn CLOSE.
    
    bOk = TRUE.

    RUN bibl_loggDbFri.p (cLogg, '    Antall linjer i fil: ' + STRING(iantLinjer) + '.'). 
    
END PROCEDURE.

PROCEDURE bkuAvfil:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    OS-CREATE-DIR VALUE(RIGHT-TRIM(cBkuKatalog,'\')) NO-ERROR.
    OS-COPY VALUE(cImpKatalog + cImpfil) VALUE(cBkuKatalog + cBkufil).
    IF SEARCH(cExcelFilNavn) <> ? THEN 
        OS-COPY VALUE(cImpKatalog + cExcelFilNavn) VALUE(cBkuKatalog + cExcelFilNavn).
    IF SEARCH(cBkuKatalog + cBkufil) <> ? THEN
    DO: 
        OS-DELETE VALUE(cImpKatalog + cImpfil). 
        IF SEARCH(cExcelFilNavn) <> ? THEN 
            OS-DELETE VALUE(cImpKatalog + cExcelFilNavn). 
        RUN bibl_loggDbFri.p (cLogg, '  Bku fil ' + cBkuKatalog + cBkufil + '.'). 
    END.
END PROCEDURE.

PROCEDURE SjekkPakkseddel:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    OUTPUT STREAM Ut TO VALUE(cUtFil) APPEND.
    
    RUN bibl_loggDbFri.p (cLogg,';PkSdlNr;Status;ButikkNr;MottattDato').
    PUT STREAM Ut UNFORMATTED  
        'PkSdlNr;Status;ButikkNr;MottattDato'
        SKIP.
        
    FOR EACH ttPkSdl:
        IF AVAILABLE PkSdlLinje THEN 
            RELEASE PkSdlLinje.
        FIND LAST PkSdlHode NO-LOCK WHERE 
            PkSdlHode.PkSdlNr = ttPkSdl.PkSdlNr AND 
            CAN-FIND(FIRST PkSdlLinje OF PkSdlHode WHERE 
                            PkSdlLinje.ButikkNr = 10) NO-ERROR.
        IF NOT AVAILABLE PksdlHode THEN 
            FIND LAST PkSdlHode NO-LOCK WHERE 
                PkSdlHode.PkSdlNr = ttPkSdl.PkSdlNr AND 
                CAN-FIND(FIRST PkSdlLinje OF PkSdlHode WHERE 
                                PkSdlLinje.ButikkNr = 40) NO-ERROR.
        IF NOT AVAILABLE PksdlHode THEN 
            FIND LAST PkSdlHode NO-LOCK WHERE 
                PkSdlHode.PkSdlNr = ttPkSdl.PkSdlNr NO-ERROR.
        IF AVAILABLE PkSdlHode THEN
            FIND FIRST PkSdlLinje OF PkSdlHode NO-ERROR.
        IF AVAILABLE PkSdlHode THEN
            FIND FIRST PkSdlMottak OF PkSdlhode NO-ERROR.

        RUN bibl_loggDbFri.p (cLogg, 
            ';' + 
            ttPkSdl.PkSdlNr + ';' +
            (IF AVAILABLE PkSdlHode THEN STRING(PkSdlHode.PkSdlStatus) ELSE '*Ukjent pksdl') + ';' +
            (IF AVAILABLE PksdlLinje THEN STRING(PksdlLinje.butikkNr) ELSE '') + ';' +
            (IF AVAILABLE PkSdlMottak THEN STRING(PksdlMottak.MottattDato) ELSE '')
            ).
        PUT STREAM Ut UNFORMATTED 
            ttPkSdl.PkSdlNr + ';' +
            (IF AVAILABLE PkSdlHode THEN STRING(PkSdlHode.PkSdlStatus) ELSE '*Ukjent pksdl') + ';' +
            (IF AVAILABLE PksdlLinje THEN STRING(PksdlLinje.butikkNr) ELSE '') + ';' +
            (IF AVAILABLE PkSdlMottak THEN STRING(PksdlMottak.MottattDato) ELSE '')
            SKIP.
    END.
    OUTPUT STREAM Ut CLOSE.
END PROCEDURE.

