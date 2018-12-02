
/*------------------------------------------------------------------------
    File        : TGSelgerAnonym.p
    Purpose     : Anonymiserer selgere hvor det er mer enn 30 dager siden de sluttet.

    Syntax      :

    Description : Anonymiseringer av selgere    

    Author(s)   : Tom Nøkleby
Tom Nøkleby    
    Created     : Mon Jul 23 13:39:26 CEST 2018
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE VARIABLE dDato AS DATE NO-UNDO.
DEFINE VARIABLE iantDager AS INTEGER NO-UNDO.
DEFINE VARIABLE cLogg AS CHARACTER NO-UNDO.
DEFINE VARIABLE cTekst AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFilNavn AS CHARACTER NO-UNDO.
DEFINE VARIABLE cKatalog AS CHARACTER NO-UNDO.
DEFINE VARIABLE bOpen AS LOG NO-UNDO.

DEFINE STREAM Ut.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */

{syspara.i 1 1 61 cKatalog}
IF cKatalog = '' THEN 
DO:
{syspara.i 1 1 51 cKatalog}
    IF cKatalog = '' THEN
        cKatalog = 'c:\home\lindbak\sendes'. 
END.

ASSIGN
    bOpen     = FALSE 
    cFilNavn  = 'TGSelgerAnonym' + REPLACE(STRING(TODAY),'/','') + '.csv'
    iantDager = 30 
    dDato     = TODAY - iAntDager
    cLogg     = 'TGSelgerAnonym' + REPLACE(STRING(TODAY),'/','')
    cTekst    = '*Anonym'     
    .
    
RUN bibl_loggDbFri.p (cLogg, 'Start.'). 
    
FOR EACH Selger EXCLUSIVE-LOCK WHERE 
    Selger.SluttetDato <> ? AND 
    Selger.SluttetDato < dDato AND 
    Selger.Navn <> cTekst:

    IF bOpen = FALSE THEN 
    DO:
        bOpen = TRUE.        
        OUTPUT STREAM Ut TO VALUE(RIGHT-TRIM(cKatalog,'\') + '\' + cFilNavn).
        PUT STREAM Ut UNFORMATTED 
            'AnsattNr;Anonymisert'
        SKIP.
    END.
        
    ASSIGN 
        Selger.ForNavn = '' 
        Selger.Navn    = cTekst
        Selger.Adresse1 = ''
        Selger.Adresse2 = ''
        Selger.ArbeidsProsent = 0.0
        Selger.BrukeridPRS = ''
        Selger.deciPWD = 0.0
        Selger.FastLonn = 0.0
        Selger.FodtDato = ?
        Selger.JobTittel = ''
        Selger.Telefon = ''
        Selger.PersonNr = 0.0
        Selger.Mobiltelefon = ''
        Selger.PostNr = ''
        Selger.NavnIKasse = ''
        Selger.ButikkNr = 0
        Selger.LonnProfil = ''
        Selger.TimeLonn = 0.0
        .
        
    PUT STREAM Ut UNFORMATTED 
        Selger.AnsattNr ';'
        TODAY
        SKIP.

    /* Logger endret ansatt dato. */
    RUN bibl_loggDbFri.p (cLogg, '    Anonymisert selger: ' + 
        STRING(Selger.SelgerNr) + ' ' + 
        STRING(Selger.AnsattNr) + ' Ansatt: ' +
        STRING(Selger.AnsattDato) + ' Sluttet: ' + 
        STRING(Selger.SluttetDato) + ' Anonymisert: ' + 
        STRING(TODAY) 
        ). 
END.

IF bOpen = FALSE THEN 
    OUTPUT STREAM Ut CLOSE.

RUN bibl_loggDbFri.p (cLogg, 'Ferdig.'). 
