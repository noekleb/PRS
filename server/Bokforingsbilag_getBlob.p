/* Bokforingsbilag_getBlob.p
-----------------------------------------------------------------------------------*/
DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.

DEFINE VARIABLE lBokforingsId AS DECIMAL NO-UNDO.
DEFINE VARIABLE cFilNavn AS CHARACTER NO-UNDO.
DEFINE VARIABLE cLogg AS CHARACTER NO-UNDO.
DEFINE VARIABLE cSider AS CHARACTER NO-UNDO.
DEFINE VARIABLE cBrukerId AS CHARACTER NO-UNDO.
DEFINE VARIABLE bBatch AS LOG NO-UNDO.

DEFINE VARIABLE rStandardFunksjoner AS cls.StdFunk.StandardFunksjoner NO-UNDO.

DEFINE TEMP-TABLE ttBokforingsbilag NO-UNDO
  FIELD lBokforingsId AS DECIMAL FORMAT ">>>>>>>>>>>>>>>9"
  FIELD TYPE AS INTEGER 
  FIELD SeqNr AS INTEGER 
  FIELD PdfFil AS BLOB 
  .

DEFINE TEMP-TABLE bttBokforingsbilag LIKE ttBokforingsbilag.

ASSIGN 
  lBokforingsId = DEC(ENTRY(1,icParam,'|'))
  cBrukerId     = ENTRY(2,icParam,'|')
  ocReturn      = 'Fant ikke PDF filen.'
  cLogg         = 'Bokforings_getBlob' + REPLACE(STRING(TODAY),'/','')
/*  cSider        = '99,7,8,9,10' /* Denne koden genererer pdf fil uten å skrive den ut. */*/
  cSider        = '99' /* Denne koden genererer pdf fil uten å skrive den ut. */
  .
/* Utskrift skal direkte til skriver. */  
IF (NUM-ENTRIES(icParam,'|') > 2 AND ENTRY(3,icParam,'|') = 'SKRIVER') THEN 
  ASSIGN 
    bBatch = FALSE.    
ELSE 
  bBatch = TRUE.    
 
rStandardFunksjoner  = NEW cls.StdFunk.StandardFunksjoner() NO-ERROR.

rStandardFunksjoner:SkrivTilLogg(cLogg,
            '  Start.' 
            ).   
            
SUBSCRIBE TO 'bokforingsbilag_bruker' ANYWHERE.
            
IF lBokforingsId > 0 THEN 
DO: 
  FIND Bokforingsbilag NO-LOCK WHERE 
    Bokforingsbilag.BokforingsId = lBokforingsId NO-ERROR.
  IF AVAILABLE Bokforingsbilag THEN 
  GENFIL:
  DO:
      FIND Butiker NO-LOCK WHERE 
        Butiker.butik = BokforingsBilag.butik NO-ERROR.
      IF NOT AVAILABLE Butiker THEN 
      DO:
        ocReturn = '**Ukjent butikk på bokføringsbilag.'.
        LEAVE GENFIL.
      END.
      
      IF CAN-FIND(FIRST kas_rap WHERE kas_rap.dato = BokforingsBilag.OmsetningsDato AND 
                  kas_rap.butikk = butiker.butik) THEN 
          RUN dagsrapp_utskrift.p (cSider,BokforingsBilag.ButikkNr,BokforingsBilag.OmsetningsDato,BokforingsBilag.OmsetningsDato,bBatch,OUTPUT cFilNavn) NO-ERROR.
      IF SEARCH(cFilNavn) <> '' THEN 
      DO:
        CREATE bttBokforingsbilag. /* Må ha en record å legge inn data i. */
        COPY-LOB FROM FILE(cFilNavn) TO bttBokforingsbilag.PDFFil NO-ERROR.
        
        /* Kopierer tabellen inn i handle. */
        ihBuffer:COPY-TEMP-TABLE (BUFFER bttBokforingsbilag:HANDLE,NO,NO,YES).
        ocReturn = ''.
        
        rStandardFunksjoner:SkrivTilLogg(cLogg,
                   '   Sender pdf fil - cFilNavn: ' + cFilNavn  
                   ).
      END.
      ELSE 
        ocReturn = '**Finner ikke PF fil (' + (IF cFilNavn <> ? THEN cFilNavn ELSE '?') + ').'.
   
  END. /* GENFIL */
END.

obOk = ocReturn = "".

 rStandardFunksjoner:SkrivTilLogg(cLogg,
            '   Resultat: ' + STRING(obOk) + ' ' + ocReturn  
            ).   
 rStandardFunksjoner:SkrivTilLogg(cLogg,
            '  Slutt.' 
            ).   


/* **********************  Internal Procedures  *********************** */

PROCEDURE bokforingsbilag_bruker:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
  DEFINE OUTPUT PARAMETER pcBrukerId AS CHARACTER NO-UNDO.
  
  ASSIGN 
    pcBrukerId = cBrukerId
    . 

END PROCEDURE.

