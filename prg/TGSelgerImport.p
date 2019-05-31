
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
DEFINE VARIABLE cImpFilLst AS CHARACTER NO-UNDO.
DEFINE VARIABLE cBkufilLst AS CHARACTER NO-UNDO.
DEFINE VARIABLE cImpFil AS CHARACTER NO-UNDO.
DEFINE VARIABLE cBkufil AS CHARACTER NO-UNDO.
DEFINE VARIABLE cImpKatalog AS CHARACTER NO-UNDO.
DEFINE VARIABLE cBkuKatalog AS CHARACTER NO-UNDO.
DEFINE VARIABLE cUtKatalog AS CHARACTER NO-UNDO.
DEFINE VARIABLE cTekst AS CHARACTER NO-UNDO.
DEFINE VARIABLE cLogg AS CHARACTER NO-UNDO.
DEFINE VARIABLE cTimeStamp AS CHARACTER NO-UNDO.
DEFINE VARIABLE bOk AS LOG INITIAL FALSE NO-UNDO.
DEFINE VARIABLE iAntLinjer AS INTEGER NO-UNDO.
DEFINE VARIABLE iAntNye AS INTEGER NO-UNDO.
DEFINE VARIABLE iAntOppdatert AS INTEGER NO-UNDO.
DEFINE VARIABLE iLoop AS INTEGER NO-UNDO.
DEFINE VARIABLE lDec AS DECIMAL NO-UNDO.
DEFINE VARIABLE lDato AS DATE NO-UNDO.
DEFINE VARIABLE bFunnet AS LOG NO-UNDO.
DEFINE VARIABLE iSelgerId AS INTEGER NO-UNDO.
DEFINE VARIABLE iantAnsDato AS INTEGER NO-UNDO.
DEFINE VARIABLE iantNavn AS INTEGER NO-UNDO.
DEFINE VARIABLE iX AS INTEGER NO-UNDO.
DEFINE VARIABLE cAnsatt AS CHARACTER NO-UNDO.

DEFINE TEMP-TABLE ttSelger
    FIELD AnsattNr AS CHARACTER FORMAT "x(20)"
    FIELD Fornavn AS CHARACTER FORMAT "x(20)"
    FIELD Etternavn AS CHARACTER FORMAT "x(20)"
    FIELD AnsattDato AS CHARACTER FORMAT "x(20)"
    FIELD SelgerId AS INTEGER FORMAT ">>>>9"
    INDEX idxAnsattNr AS PRIMARY AnsattNr.

DEFINE STREAM Inn.
DEFINE STREAM Ut.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
IF SEARCH('tnc.txt') <> ? THEN  
    ASSIGN 
        cImpKatalog = 'c:\home\lindbak\timegripget\'
        cBkuKatalog = 'c:\home\lindbak\timegripget\bku\'
        .
ELSE         
    ASSIGN 
        cImpKatalog = 'c:\home\lindbak\timegripget\'
        cBkuKatalog = 'c:\home\lindbak\timegripget\bku\'
        .
ASSIGN
    cTimeStamp = REPLACE(STRING(TODAY),'/','') + '_' + REPLACE(STRING(TIME,"HH:MM:SS"),':','')
    cLogg      = 'TGSelgerImport' + REPLACE(STRING(TODAY),'/','') 
    cImpfilLst = 'lc_export_new.csv,lc_export_quit.csv'
    cBkufilLst = REPLACE(cImpfilLst,'.csv',cTimeStamp + '.csv')
    .

{syspara.i 1 1 61 cUtKatalog}
IF cUtKatalog = '' THEN 
DO:
{syspara.i 1 1 51 cUtKatalog}
    IF cUtKatalog = '' THEN
        cUtKatalog = 'c:\home\lindbak\sendes'. 
END.
cUtKatalog = RIGHT-TRIM(cUtKatalog,'\').

RUN bibl_loggDbFri.p (cLogg, 'Start.'). 

IF cImpfilLst <> '' THEN 
DO iLoop = 1 TO NUM-ENTRIES(cImpFilLst):
    ASSIGN 
        cImpFil = ENTRY(iLoop,cImpFilLst)
        cBkuFil = ENTRY(iLoop,cBkuFilLst)
        cAnsatt = ''
        .
        
    IF SEARCH(cImpKatalog + cImpfil) <> ? THEN 
        RUN importerfil. 
    ELSE DO:
        RUN bibl_loggDbFri.p (cLogg, '  Ingen fil å importere.'). 
    END.
    
    RUN bkuAvfil.
        
    /* Det er opprettet nye selgere som er tildelt selgerid. Disse skal legges ut til timeGrip. */    
    IF CAN-FIND(FIRST ttSelger WHERE 
                ttSelger.SelgerId > 0) THEN 
        RUN eksportSelger.
END.

RUN bibl_loggDbFri.p (cLogg, 'Ferdig.'). 


/* **********************  Internal Procedures  *********************** */

PROCEDURE eksportSelger:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
DEFINE VARIABLE pcUtfil AS CHARACTER NO-UNDO.

IF NOT CAN-FIND(FIRST ttSelger WHERE 
            ttSelger.SelgerId > 0) THEN
    RETURN.             

ASSIGN 
    pcUtfil = cUtKatalog + '\' + '_TGSelgerSelgerId' + REPLACE(STRING(TODAY),'/','') + '.csv'
    .

OUTPUT STREAM Ut TO VALUE(pcUtFil) APPEND.
FOR EACH ttSelger WHERE 
    ttSelger.SelgerId > 0:
        
    PUT STREAM Ut UNFORMATTED  
        ttSelger.AnsattNr ';'
        ttSelger.SelgerId 
    SKIP.
END.
OUTPUT STREAM Ut CLOSE.
/* Gjør filen klar til sending. */
IF SEARCH(pcUtfil) <> ? THEN 
DO:
    OS-RENAME VALUE(pcUtFil) VALUE(REPLACE(pcUtFil,'_','')).
    IF SEARCH(pcUtFil) <> ? THEN
            OS-DELETE VALUE(pcUtFil). 
END.

END PROCEDURE.

PROCEDURE importerfil:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE BUFFER bufArtBas  FOR ArtBas.
    DEFINE BUFFER bufArtPris FOR ArtPris.

    IF SEARCH(cImpKatalog + cImpfil) = ? THEN
        RETURN.

    ASSIGN 
        iantNye = 0
        iantNavn = 0
        iantAnsDato = 0
        iantOppdatert = 0
        .
    
    RUN bibl_loggDbFri.p (cLogg, '  Importerer fil ' + cImpKatalog + cImpfil + '.'). 
    
    INPUT STREAM Inn FROM VALUE(cImpKatalog + cImpfil) CONVERT SOURCE 'UTF-8' .
    IMPLOOP:
    REPEAT TRANSACTION:
        iAntLinjer = iAntLinjer + 1.
        CREATE ttSelger.
        IMPORT STREAM Inn DELIMITER ';' 
            ttSelger
            NO-ERROR.
        /* Overskriftsrad */
        IF ERROR-STATUS:ERROR OR ttSelger.AnsattNr = '' THEN
        DO:
            IF AVAILABLE ttSelger THEN 
                DELETE ttSelger.  
            NEXT IMPLOOP.
        END.

        /* Ugyldig ansattnr. */
        ASSIGN 
            lDec = DEC(ttSelger.AnsattNr)
            NO-ERROR.
        IF ERROR-STATUS:ERROR THEN 
            NEXT IMPLOOP.
        
        FIND FIRST Selger EXCLUSIVE-LOCK WHERE
            Selger.AnsattNr = ttSelger.AnsattNr NO-ERROR.
        IF AVAILABLE Selger THEN 
        DO:
            /* Setter Ansatt dato. */
            IF iLoop = 1 AND ttSelger.AnsattDato <> ? AND Selger.AnsattDato <> DATE(ttSelger.AnsattDato) THEN 
            DO:
                /* Logger endret ansatt dato. */
                RUN bibl_loggDbFri.p (cLogg, '    Endret ansattdato: ' + 
                    STRING(Selger.SelgerNr) + ' ' + 
                    STRING(Selger.AnsattDato) + ' ' +
                    Selger.ForNavn + ' ' + 
                    Selger.Navn + ' ' + 
                    'Ny ansattdato: ' + IF ttSelger.AnsattDato <> ? THEN STRING(ttSelger.AnsattDato) ELSE '' 
                    ). 
                Selger.AnsattDato = DATE(ttSelger.AnsattDato).
                iantAnsDato = iantAnsDato + 1.
            END.

            /* Setter Sluttet dato. */
            IF iLoop = 2 AND ttSelger.AnsattDato <> ? AND Selger.SluttetDato <> DATE(ttSelger.AnsattDato) THEN 
            DO:
                Selger.SluttetDato = DATE(ttSelger.AnsattDato).
                iAntNye = iAntNye + 1.
                
                /* Sletter koblinger mot butikk */
                FOR EACH ButikkSelger OF Selger EXCLUSIVE-LOCK:
                    DELETE ButikkSelger.
                END.

                /* Logger sluttet dato. */
                RUN bibl_loggDbFri.p (cLogg, '    Sluttet: ' + 
                    STRING(Selger.SelgerNr) + ' ' + 
                    STRING(Selger.AnsattDato) + ' ' +
                    Selger.ForNavn + ' ' + 
                    Selger.Navn + ' ' + 
                    'Sluttetdato: ' + IF Selger.SluttetDato <> ? THEN STRING(Selger.SluttetDato) ELSE '' 
                    ). 
            END.
        
            /* Bare for oppdatering av selgere. */    
            IF (ttSelger.ForNavn <> '' OR ttSelger.EtterNavn <> '') AND iLoop = 1 AND 
            (ttSelger.ForNavn <> Selger.forNavn OR ttSelger.EtterNavn <> Selger.Navn) THEN
            DO: 
                ASSIGN
                Selger.ForNavn = ttSelger.Fornavn
                Selger.Navn    = ttSelger.Etternavn
                iAntNavn       = iantNavn + 1
                .
                /* Endret navn. */
                RUN bibl_loggDbFri.p (cLogg, '    Oppdatert navn: ' + 
                    STRING(Selger.SelgerNr) + ' ' + 
                    IF Selger.AnsattDato <> ? THEN STRING(Selger.AnsattDato) ELSE '' + ' ' +
                    Selger.ForNavn + ' ' + 
                    Selger.Navn).
            END. 
        END.  
        
        /* Opprettelse av nye selgere. */
        IF iLoop = 1 AND NOT AVAILABLE Selger THEN DO:
            CREATE Selger.
            ASSIGN
            Selger.AnsattNr = ttSelger.AnsattNr
            Selger.AnsattDato = DATE(ttSelger.ansattDato)
            Selger.ForNavn = ttSelger.Fornavn
            Selger.Navn = IF ttSelger.EtterNavn <> '' THEN ttSelger.EtterNavn ELSE 'Selger ' + STRING(Selger.SelgerNr)
            Selger.NavnIKasse = ENTRY(1,Selger.ForNavn,' ') + SUBSTRING(Selger.Navn,1,1)            
            iantNye = iantNye + 1.
            .
            LOOPEN2:
            DO iSelgerId = 1 TO 9999:
                FOR EACH Butiker NO-LOCK WHERE 
                    Butiker.Butik > 0 AND 
                    Butiker.ApningsDato <> ? AND 
                    Butiker.NedlagtDato = ? AND 
                    Butiker.harButikksystem = TRUE:
                    IF CAN-FIND(FIRST ButikkSelger WHERE 
                                butikkSelger.butikkNr = butiker.butik AND 
                                ButikkSelger.SelgerId = iSelgerId) THEN 
                        NEXT LOOPEN2.    
                END.
                
                cTekst = ''.
                /* Ledig, da oppretter vi koblinger. */
                FOR EACH Butiker NO-LOCK WHERE 
                    Butiker.Butik > 0 AND 
                    Butiker.ApningsDato <> ? AND 
                    Butiker.NedlagtDato = ? AND 
                    Butiker.harButikksystem = TRUE:
                    CREATE ButikkSelger.
                    ASSIGN 
                        ButikkSelger.SelgerNr = Selger.SelgerNr
                        ButikkSelger.ButikkNr = Butiker.butik
                        ButikkSelger.SelgerId = iSelgerId                        
                        cTekst = cTekst + 
                                 (IF cTekst = '' THEN '' ELSE ',') + 
                                 STRING(Butiker.butik)
                        .
                    /* Logger tildelt SelgerId */
                    IF ttSelger.SelgerId = 0 THEN 
                        ASSIGN
                            ttSelger.SelgerId = iSelgerId. 
                END.
        
                ASSIGN 
                    cAnsatt = cAnsatt + (IF cAnsatt <> '' THEN CHR(1) ELSE '') + 
                              'SelgerNr: ' + STRING(Selger.SelgerNr) + ' AnsattNr: ' + 
                              STRING(Selger.AnsattNr) + ' ' +
                              IF Selger.AnsattDato <> ? THEN STRING(Selger.AnsattDato) ELSE '' + ' ' +
                              Selger.ForNavn + ' ' + 
                              Selger.Navn + ' ' + 
                              'SelgerId: ' + STRING(iSelgerId) + ' ' + 
                              'Koblet til butikker: ' + cTekst.
                    .        
                /* Logger ny selger. */
                RUN bibl_loggDbFri.p (cLogg, '    Ny selger: ' + 
                                             STRING(Selger.SelgerNr) + ' ' + 
                                             IF Selger.AnsattDato <> ? THEN STRING(Selger.AnsattDato) ELSE '' + ' ' +
                                             Selger.ForNavn + ' ' + 
                                             Selger.Navn + ' ' + 
                                             'SelgerId: ' + STRING(iSelgerId) + ' ' + 
                                             'Koblet til butikker: ' + cTekst
                                             ). 
                
                /* FERDIG. */
                LEAVE LOOPEN2.
            END. /* LOOPEN2*/
        END.
        
    END. /* IMPLOOP TRANSACTION */
    INPUT STREAM Inn CLOSE.
    
    bOk = TRUE.

    /* Sender mail med nye ansatte. */
    IF cAnsatt <> '' AND iLoop = 1 THEN 
        RUN sendEMail (cImpKatalog + cImpfil, cAnsatt).
    
    RUN bibl_loggDbFri.p (cLogg, '    Antall linjer i fil: ' + STRING(iantLinjer) + '.'). 
    IF iLoop = 1 THEN 
    DO:
        RUN bibl_loggDbFri.p (cLogg, '    Antall nye selgere    : ' + STRING(iAntNye) + '.'). 
        RUN bibl_loggDbFri.p (cLogg, '    Antall endret ans.dato: ' + STRING(iAntAnsDato) + '.').
        RUN bibl_loggDbFri.p (cLogg, '    Antall endret navn    : ' + STRING(iAntNavn) + '.').
    END.
    ELSE DO:
        RUN bibl_loggDbFri.p (cLogg, '    Antall selgere sluttet: ' + STRING(iAntNye) + '.'). 
    END. 
    
END PROCEDURE.

PROCEDURE bkuAvfil:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    OS-CREATE-DIR VALUE(RIGHT-TRIM(cBkuKatalog,'\')) NO-ERROR.
    OS-COPY VALUE(cImpKatalog + cImpfil) VALUE(cBkuKatalog + cBkufil).
    IF SEARCH(cBkuKatalog + cBkufil) <> ? THEN
    DO: 
        OS-DELETE VALUE(cImpKatalog + cImpfil). 
        RUN bibl_loggDbFri.p (cLogg, '  Bku fil ' + cBkuKatalog + cBkufil + '.'). 
    END.
END PROCEDURE.

PROCEDURE sendEMail:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER picFil AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER pcAnsatt AS CHARACTER NO-UNDO.

DEFINE VARIABLE piLoop AS INTEGER NO-UNDO.

FILE-INFO:FILE-NAME = picFil.

ANSATTLOOP:
DO piLoop = 1 TO NUM-ENTRIES(pcAnsatt,CHR(1)):
    RUN sendmail_tsl.p ("TimeGrip",
                        "Ny ansatt importert fra fil " + picFil + '.',
                        FILE-INFO:FULL-PATHNAME,
                        "Ny ansatt: " + ENTRY(piLoop,pcAnsatt,CHR(1)) + '.',
                        "",
                        "") NO-ERROR.
    IF ERROR-STATUS:ERROR THEN 
        DO:
            RUN bibl_loggDbFri.p (cLogg,'    **FEIL. eMail ikke sendt. Vedlegg ' + FILE-INFO:FULL-PATHNAME + '.').
            DO ix = 1 TO ERROR-STATUS:NUM-MESSAGES:
                RUN bibl_loggDbFri.p (cLogg, '          ' 
                    + STRING(ERROR-STATUS:GET-NUMBER(ix)) + ' ' + ERROR-STATUS:GET-MESSAGE(ix)    
                    ).
            END.            
        END.
END. /* ANSATTLOOP */

END PROCEDURE.

