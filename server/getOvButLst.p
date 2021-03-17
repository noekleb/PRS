
/*------------------------------------------------------------------------
    File        : getOvButLst.p
    Purpose     : Redusere mulige feiloverføringer ved å redusere listen over butikker det kan overføres til.

    Syntax      : RUN getOvButLst.p (iButNr, OUTPUT cOKButLst, output lOk, OUTPUT cReturn).

    Description : Henter liste med butikker som den butikken som spør kan overføre til.

    Author(s)   : Tom Nøkleby
    Created     : Wed May 10 10:55:04 CEST 2017
    Notes       : Under er oppstillingen slik den ser ut hos Gant. Hvis butikken som spør ikke ligger i noen 
                  av butikklistene, er det tillatt å overføre til alle aktive butikker som har butikksystem.
                  Denne listen ligger i cDefbutLst.
                  Finnes butikken i en av listene, kan den bare overføre til de butikker som ligger i den motsvarende ok listen.
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE INPUT  PARAMETER iButNr          AS INTEGER   NO-UNDO.
DEFINE INPUT  PARAMETER cIkkePlussMinus AS CHARACTER NO-UNDO.
 
DEFINE OUTPUT PARAMETER cOKButLst   AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER cOKButNavn  AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER lOK         AS LOGICAL   NO-UNDO.
DEFINE OUTPUT PARAMETER cReturn     AS CHARACTER NO-UNDO.

DEFINE VARIABLE cDefButLst          AS CHARACTER     NO-UNDO.
DEFINE VARIABLE iAktiv              AS INTEGER       NO-UNDO.

DEFINE VARIABLE cVanligeButikker      AS CHAR EXTENT 2 NO-UNDO.
DEFINE VARIABLE cSentralagre          AS CHAR EXTENT 2 NO-UNDO.
DEFINE VARIABLE cOutlet               AS CHAR EXTENT 2 NO-UNDO.
DEFINE VARIABLE cNettbutikkLager      AS CHAR EXTENT 2 NO-UNDO.
DEFINE VARIABLE cNettbutikkVentelager AS CHAR EXTENT 2 NO-UNDO.
DEFINE VARIABLE cEksternebutikker     AS CHAR EXTENT 2 NO-UNDO.
DEFINE VARIABLE cPlussMinusbutikker   AS CHAR EXTENT 2 NO-UNDO.
DEFINE VARIABLE cKanIkkeOverfore      AS CHAR EXTENT 2 NO-UNDO.

DEFINE VARIABLE iLoop AS INTEGER NO-UNDO.

DEFINE BUFFER bufButiker FOR Butiker.

{syspara.i 11 7 1 iAktiv INT}

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */

FIND Butiker NO-LOCK WHERE 
    Butiker.Butik = ibutNr NO-ERROR.
IF NOT AVAILABLE Butiker THEN 
    DO:
        ASSIGN 
        cOKButLst = ''
        lOk       = FALSE
        cReturn   = '** Ukjent butikk.'
        .
        RETURN cReturn.
    END.
IF Butiker.harButikksystem = FALSE OR  
   Butiker.NedlagtDato    <> ?  OR 
   Butiker.ApningsDato     = ? THEN 
    DO:
        ASSIGN 
        cOKButLst = ''
        lOk       = FALSE
        cReturn   = '** Ikke aktiv butikk.'
        .
        RETURN cReturn.
    END.

RUN SetDefButLst.    /* Setter opp listen som skal benyttes hvis butikken ikke ligger i noen av de adnre listene.   */
IF iAktiv = 0 THEN 
  DO:
      cOKButLst = cDefbutLst.
      RETURN.
  END.
RUN SetButikkLister. /* Setter opp listen som skal benyttes for de ulike typer av butikker.                         */
RUN ByggOkListe.     /* Tar bort butikker fra default listen som butikken ikke kan overføre til og lager ok listen. */
RUN ByggNavneListe.  /* Lager liste med navn på butikkene som ligger i OK listen.                                   */

ASSIGN 
    lOk = TRUE 
    .

RETURN cReturn.

/* **********************  Internal Procedures  *********************** */

PROCEDURE ByggNavneListe:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
IF cOKButLst <> '' THEN 
DO:
    cOKButNavn = ''.
    DO iLoop = 1 TO NUM-ENTRIES(cOKButLst):
        FIND bufButiker NO-LOCK WHERE 
            bufButiker.butik = INT(ENTRY(iLoop,cOKButLst)) NO-ERROR.
        IF AVAILABLE bufButiker THEN 
        DO:
            cOKButNavn = cOkButNavn + 
                         (IF cOkButNavn <> '' THEN ',' ELSE '') + 
                         bufButiker.ButNamn + '|' + bufButiker.KortNavn.            
        END.
        ELSE DO:
            cOKButNavn = cOkButNavn + 
                         (IF cOkButNavn <> '' THEN ',' ELSE '') + 
                         'Ukjent|Ukjent'.                        
        END. 
    END.
END.

END PROCEDURE.

PROCEDURE ByggOkListe:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cIkkeListe AS CHARACTER NO-UNDO.
    
    IF CAN-DO(cVanligeButikker[1],STRING(iButNr)) THEN
        cIkkeListe = cVanligeButikker[2].
    ELSE IF CAN-DO(cSentralagre[1],STRING(iButNr)) THEN
        cIkkeListe = cSentralagre[2].
    ELSE IF CAN-DO(cOutlet[1],STRING(iButNr)) THEN
        cIkkeListe = cOutlet[2].
    ELSE IF CAN-DO(cNettbutikkLager[1],STRING(iButNr)) THEN
        cIkkeListe = cNettbutikkLager[2].
    ELSE IF CAN-DO(cNettbutikkVenteLager[1],STRING(iButNr)) THEN
        cIkkeListe = cNettbutikkVenteLager[2].
    ELSE IF CAN-DO(cEksternebutikker[1],STRING(iButNr)) THEN
        cIkkeListe = cEksternebutikker[2].
    ELSE IF CAN-DO(cPlussMinusbutikker[1],STRING(iButNr)) THEN
        cIkkeListe = cPlussMinusbutikker[2].
    ELSE IF CAN-DO(cKanIkkeOverfore[1],STRING(iButNr)) THEN
        cIkkeListe = cKanIkkeOverfore[2].

    /* Det kan ikke overføres til noen butikker. */ 
    IF cIkkeListe = '*' THEN 
        cOKButLst = ''.
    ELSE DO:    
        /* Bygger en ren liste med butikker det kan overføres til. */
        IF cDefbutLst <> '' THEN 
            DO iLoop = 1 TO NUM-ENTRIES(cDefButLst):
                
                /* butikken skal heller ikke kunne overføre til seg selv. */
                IF INT(ENTRY(iLoop,cDefbutLst)) = iButNr THEN 
                    NEXT.
                    
                /* Disse butikkene (Fra butikktype) skal ikke kunne overføres til. */
                IF CAN-DO(cIkkeListe,ENTRY(iLoop,cDefbutLst)) THEN 
                    NEXT.
                
                /* Denne gruppen butikker skal ikke kunne overføres til. */ 
                IF CAN-DO(cKanIkkeOverfore[1],ENTRY(iLoop,cDefbutLst)) THEN 
                    NEXT.
                
                /* Bygger listen */
                cOKButLst = cOKButLst + 
                            (IF cOKButLst <> '' THEN ',' ELSE '') + 
                            ENTRY(iLoop,cDefbutLst).
            END.
    END.
    
END PROCEDURE.

PROCEDURE SetButikkLister:
/*------------------------------------------------------------------------------
 Purpose: Setter opp butikklister for de ulike typer av butikker.
          Denne oppstillingen skal erstattes av lesing av data fra systemparametre.
          
          Viser hvilke butikker som en butikk ikke kan overføre til.
          
 Notes:
------------------------------------------------------------------------------*/

/* ------ Oppsett for Gant.
    ASSIGN 
        /* Butikk lister for typer av butikker */
        cVanligeButikker[1]    = '2,3,4,5,6,9,11' 
        cSentralagre[1]        = '20'
        cOutlet[1]             = '10,40'
        cNettbutikkLager[1]    = '16'
        cNettbutikkVenteLager[2] = '50'
        cEksternebutikker[1]   = '14'
        cPlussMinusbutikker[1] = '848,849'
        cKanIkkeOverfore[1]    = '1,8,15,30'
        
        /* Lister over hve de ikke kan overføre til. */
        cVanligeButikker[2]    = '10,15,40,50' 
        cSentralagre[2]        = '50'
        cOutlet[2]             = '2,3,4,5,6,9,11,14,15,16,20,50'
        cNettbutikkLager[2]    = '10,15,40'
        cNettbutikkVenteLager[2] = '2,3,4,5,6,9,11,14,20,50'
        cEksternebutikker[2]   = '10,15,40,50'
        cPlussMinusbutikker[2] = '*' /* Kan ikke overføre til noen butikker */
        cKanIkkeOverfore[2]    = '*' /* Kan ikke overføre til noen butikker */
        .
------ */

{syspara.i 11 7 2 cVanligeButikker[1]}      
{syspar2.i 11 7 2 cVanligeButikker[2]}

{syspara.i 11 7 3 cSentralagre[1]}          
{syspar2.i 11 7 3 cSentralagre[2]}

{syspara.i 11 7 4 cOutlet[1]}               
{syspar2.i 11 7 4 cOutlet[2]}

{syspara.i 11 7 5 cNettbutikkLager[1]}      
{syspar2.i 11 7 5 cNettbutikkLager[2]}

{syspara.i 11 7 6 cNettbutikkVenteLager[1]} 
{syspar2.i 11 7 6 cNettbutikkVenteLager[2]}

{syspara.i 11 7 7 cEksternebutikker[1]}     
{syspar2.i 11 7 7 cEksternebutikker[2]}

{syspara.i 11 7 8 cPlussMinusbutikker[1]}   
{syspar2.i 11 7 8 cPlussMinusbutikker[2]}

{syspara.i 11 7 9 cKanIkkeOverfore[1]}      
{syspar2.i 11 7 9 cKanIkkeOverfore[2]}
  
END PROCEDURE.

PROCEDURE SetDefButLst:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    cDefbutLst = ''.
    FOR EACH Butiker NO-LOCK WHERE 
        Butiker.harButikksystem = TRUE AND 
        Butiker.NedlagtDato     = ? AND 
        Butiker.ApningsDato     <> ?:

        /* Det er valgt å ikke ta med disse butikkene. */
        IF CAN-DO(cIkkePlussMinus,STRING(Butiker.butik)) THEN 
            NEXT.

        cDefButLst = cDefButLst + 
                     (IF cDefButLst <> '' THEN ',' ELSE '') + 
                     STRING(Butiker.butik).
    END.
    
END PROCEDURE.




