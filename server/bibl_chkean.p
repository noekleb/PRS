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

DEF INPUT-OUTPUT PARAMETER cEAN AS CHAR NO-UNDO.

DEFINE VARIABLE cEAN13        AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cVikt         AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cTidskrift    AS CHARACTER   NO-UNDO.
DEFINE VARIABLE lDec          AS DECIMAL     NO-UNDO.
DEFINE VARIABLE cEANOrg       AS CHARACTER   NO-UNDO.
DEFINE VARIABLE bTillatUgyldigeTegnIEAN AS LOG NO-UNDO.
DEFINE VARIABLE iTillatBlankEAN         AS INTEGER NO-UNDO.
DEFINE VARIABLE cTekst        AS CHARACTER   NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&IF DEFINED(EXCLUDE-chkEan) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD chkEan Procedure 
FUNCTION chkEan RETURNS CHARACTER
    ( INPUT cKode AS CHARACTER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-EkspanderUPC) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD EkspanderUPC Procedure  _DB-REQUIRED
FUNCTION EkspanderUPC RETURNS CHARACTER
  ( INPUT cUPCtoExpand AS CHARACTER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF


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

/* Sjekker om vektkoder 20 koder skal konverteres */
{syspara.i 2 4 22 cVikt}
IF cVikt = "" THEN
    cVikt = "0".
/* Sjekker om tidskrift koder skal konverteres. */
{syspara.i 2 4 23 cTidskrift}
IF cTidskrift = "0" THEN
    cTidskrift = "".

/* Sjekker omd et er tillatt med blankt strekkode */
/* Kan overstyre avbryt for blank EAN kode. */
{syspara.i 50 15 3 iTillatBlankEAN INT}.

/* Sjekker om det er tillatt med alfanumeriske tegn i strekkoden. */
{syspara.i 50 15 6 cTekst}
IF CAN-DO('1,j,ja,true,y,yes',cTekst) THEN 
  bTillatUgyldigeTegnIEAN = TRUE.

/* Ledende blanke skal tas bort. */
cEAN = TRIM(cEAN).

/* Ugyldige tegn i koden? */
ASSIGN lDec = DECIMAL(cEAN) NO-ERROR.
IF ERROR-STATUS:ERROR THEN
DO: 
  IF bTillatUgyldigeTegnIEAN THEN 
    RETURN ''.
  ELSE 
    RETURN 'Ugyldige tegn i strekkoden(' + cEAN + ').'.
END.

/* Blank ean tillatt */
IF cEAN = '' THEN 
DO:
  IF iTillatBlankEAN = 1 THEN 
    RETURN ''.
  ELSE 
    RETURN 'Blank strekkode ikke tillatt'.  
END.

cEANOrg = cEAN.

/* Koder som er opptil 7 siffer lange, skal returneres uten ledende nuller. */
cEAN = LEFT-TRIM(cEAN,'0').
  
/* Strekkoder vi skal gjøre noe med */
IF LENGTH(cEAN) >= 8 THEN 
STREKKODESJEKK:
DO:
  /* Legger på eventuelle manglende ledende nuller. */
  ASSIGN cEAN = FILL("0",13 - LENGTH(cEAN)) + cEAN.

  IF cVikt = "1" THEN 
  DO:                                                                                               
    IF LENGTH(cEAN) = 13 AND                                                                     
           CAN-DO("20,21,22,23,24,25,26,27,28",SUBSTR(cEAN,1,2)) THEN
    VEKT: 
    DO:                        
        cEAN13 = SUBSTR(cEAN,1,8) + "00000".                                                      
        /* Setter prefix. */
        CASE SUBSTR(cEAN13,1,2):                                                                                      
            WHEN "20" OR WHEN "21" OR WHEN "22" THEN                                                                  
                cEAN13 = "20" + SUBSTR(cEAN13,3).                                                                     
            WHEN "23" OR WHEN "24" OR WHEN "25" THEN                                                                  
                /*cEAN13 = "23" + SUBSTR(cEAN13,3).  - Riktig ifølge EAN boken.*/                                                                     
                cEAN13 = "20" + SUBSTR(cEAN13,3). /* Riktig i henhold til Lindbak. */                                                                     
            WHEN "26" OR WHEN "27" OR WHEN "28" THEN                                                                  
                cEAN13 = "26" + SUBSTR(cEAN13,3).                                                                     
        END CASE.           
        /* Setter sjekksiffer. */                                                                                          
        /*cEAN13 = chkEan(cEAN13). TN Sjekksifer skal stå til 0 */
        /* Ferdig konvertert EAN kode. */                                         
        cEAN = cEAN13.
        LEAVE STREKKODESJEKK.
    END. /* VEKT */                                                                                                              
  END.                        
                                                                                          
  /* Hantering av tidskriftskoder */                                                                                    
  IF cTidskrift <> "" THEN 
  TIDSKRIFT:
  DO:                                                                                     
    IF LENGTH(cEAN) = 13 AND 
       cEAN BEGINS cTidskrift THEN 
    DO:                
        cEAN13 = chkEan(SUBSTR(cEAN,1,8) + "0000").
        cEAN = cEAN13.
        LEAVE STREKKODESJEKK.                                                                     
    END.                                                                                                              
  END. /* TIDSKRIFT */                                                                                                                 

  /* UPC-E til UPC-A konvertering */
  /*
  IF LENGTH(cEAN) = 6 THEN 
  DO:
    cEAN = EkspanderUPC(cEAN).  
    cEAN = chkEAN(cEAN).
    LEAVE STREKKODESJEKK.                                                                     
  END.
  */
  
  /* Sjekker sjekksiffer. */
  ASSIGN
    cEAN13 = cEAN.
  ASSIGN
    cEAN13 = chkEAN(SUBSTRING(cEAN13,1,12)).
    
  /* Feil sjekksiffer. */
  IF cEAN <> cEAN13 THEN DO:
    cEAN = cEAN13.
    RETURN 'Strekkoden har feil sjekksiffer (' + cEANOrg + '). Riktig kode er ' + cEAN13 + '.'.
  END.  
  ELSE RETURN ''.
END. /* STREKKODESJEKK */

RETURN ''.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Implementations ***************** */

&IF DEFINED(EXCLUDE-chkEan) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION chkEan Procedure 
FUNCTION chkEan RETURNS CHARACTER
    ( INPUT cKode AS CHARACTER ) :
  /*------------------------------------------------------------------------------
    Purpose:  Räknar ut checksiffra för ean EAN-kod - parameter utan chksiffra
              i.e 12 lång
      Notes:  
  ------------------------------------------------------------------------------*/
      DEF VAR iCount1 AS INTE NO-UNDO.
      DEF VAR iMulti  AS INTE INIT 1 NO-UNDO.
      DEF VAR iSum AS INTE NO-UNDO.
      DO iCount1 = LENGTH(cKode) TO 1 BY -1:  
          ASSIGN iMulti = IF iMulti = 1 THEN 3 ELSE 1
                 iSum = iSum + INT(SUBSTR(cKode,iCount1,1)) * iMulti.
      END.
      RETURN cKode + string((10 - iSum MODULO 10) MODULO 10).
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-EkspanderUPC) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION EkspanderUPC Procedure 
FUNCTION EkspanderUPC RETURNS CHARACTER
  ( INPUT cUPCtoExpand AS CHARACTER ) :
/*------------------------------------------------------------------------------
  Purpose:  Expanderar en UPC-E kod till UPC-A
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cNewUpc AS CHARACTER  NO-UNDO.
  CASE SUBSTR(cUPCtoExpand,6,1):
      WHEN "0" OR WHEN "1" OR WHEN "2" THEN 
          ASSIGN cNewUpc = "00" + SUBSTR(cUPCtoExpand,1,2) + 
                                  SUBSTR(cUPCtoExpand,6,1) +
                           "0000" + SUBSTR(cUPCtoExpand,3,3).
      WHEN "3" THEN           
          ASSIGN cNewUpc = "00" + SUBSTR(cUPCtoExpand,1,3) +
                           "00000" + SUBSTR(cUPCtoExpand,4,2).
      WHEN "4" THEN 
          ASSIGN cNewUpc = "00" + SUBSTR(cUPCtoExpand,1,4) +
                           "00000" + SUBSTR(cUPCtoExpand,5,1).
      OTHERWISE 
          ASSIGN cNewUpc = "00" + SUBSTR(cUPCtoExpand,1,5) +
                           "0000" + SUBSTR(cUPCtoExpand,6,1).
  END CASE.
  RETURN cNewUpc.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

