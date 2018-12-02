&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : bibl_OpprettMesseSuppOrd.p
    Purpose     : Automatisk opprette messe og suppleringsordrebok.

    Syntax      : RUN bibl_OpprettMesseSuppOrd.p (iMesseType, OUTPUT dVareBehNr).

    Description : Messe og suppleringsbok som opprettes, opprettes slik at den dekker
                  hele inneværende år. Meningen er at den skal justeres manuelt 
                  etterpå. Men at bruker ikke skal behøve å mått opprette 
                  messe og varehåndteringsbok under regsitrering av ordre eller varemottak.

    Author(s)   : Tom Nøkleby
    Created     : 4/10-10
    Notes       :
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE INPUT  PARAMETER iMesseType AS INTEGER NO-UNDO.
DEFINE OUTPUT PARAMETER dVareBehNr AS DECIMAL NO-UNDO.

DEFINE VARIABLE dMesseNr AS DECIMAL NO-UNDO.
DEFINE VARIABLE iCl      AS INTEGER NO-UNDO.

DEFINE BUFFER clButiker      FOR Butiker.


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

{syspara.i 5 1 1 iCL INT}
FIND clButiker NO-LOCK WHERE
  clButiker.Butik = iCL NO-ERROR.
IF NOT AVAILABLE clButiker THEN
  RETURN '** Ukjent sentrallager (bibl_opprettmessesuppord.p).'. 
    
RUN OpprettMesse.

FIND Messe NO-LOCK WHERE
  Messe.MesseNr = dMesseNr NO-ERROR.
IF AVAILABLE Messe THEN 
  RUN OpprettSuppleringsOrdreBok.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



/* **********************  Internal Procedures  *********************** */

 
 
&IF DEFINED(EXCLUDE-OpprettMesse) = 0 &THEN
		
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OpprettMesse Procedure
PROCEDURE OpprettMesse:
	/*------------------------------------------------------------------------------
			Purpose:  																	  
			Notes:  																	  
	------------------------------------------------------------------------------*/
  
  DEFINE BUFFER bufMesse       FOR Messe.
  
  DO FOR bufMesse TRANSACTION:
    CREATE bufMesse.
    ASSIGN
      bufMesse.MesseType         = iMesseType
      
      bufMesse.MesseBeskrivelse  = IF iMesseType = 1 
                                     THEN 'Forh.ordre - Innkjøpsperiode'
                                   ELSE IF iMesseType = 2
                                     THEN 'Suppl.ordre - Innkjøpsperiode'
                                   ELSE '** Ukjent messetype: ' + string(iMesseType) + '.'
      bufMesse.FraDato           = DATE (1,1,YEAR(TODAY))
      bufMesse.FraTid            = 0
      bufMesse.TilDato           = DATE (12,31,YEAR(TODAY))
      bufMesse.TilTid            = (23 * 3600) + (59 * 60)
      bufMesse.FraAar            = YEAR(TODAY)
      bufMesse.TilAar            = YEAR(TODAY)
      bufMesse.FraUke            = 1
      bufMesse.TilUke            = 52

      bufMesse.MesseFraDato      = bufMesse.FraDato
      bufMesse.MesseTilDato      = bufMesse.TilDato
      bufMesse.PubliserStartDato = bufMesse.FraDato
      bufMesse.PubliserStoppDato = bufMesse.TilDato
      
      bufMesse.KontaktPers       = clButiker.LevKontakt
      bufMesse.Telefon           = clButiker.BuTel
      bufMesse.MobilTlf          = clButiker.LevTelefon
      bufMesse.Telefaks          = clButiker.Telefaks
      bufMesse.Adresse1          = clButiker.BuAdr
      bufMesse.PostBoks          = clButiker.BuPadr
      bufMesse.PostNr            = clButiker.BuPoNr
      /*
      bufMesse.Oppmerking        = {2}.Oppmerking
      bufMesse.Fargekoder        = {2}.Fargekoder
      bufMesse.Sortimentkoder    = {2}.Sortimentkoder
      bufMesse.Kampanjeuker      = {2}.Kampanjeuker
      bufMesse.Kampanjestotte    = {2}.Kampanjestotte
      bufMesse.Lagerkoder        = {2}.Lagerkoder.
      */
      dMesseNr                  = bufMesse.MesseNr
      .    
    RELEASE bufMesse.
  END. /* TRANSACTION */

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF



&IF DEFINED(EXCLUDE-OpprettSuppleringsordreBok) = 0 &THEN
		
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OpprettSuppleringsordreBok Procedure
PROCEDURE OpprettSuppleringsordreBok:
	/*------------------------------------------------------------------------------
			Purpose:  																	  
			Notes:  																	  
	------------------------------------------------------------------------------*/
  DEFINE VARIABLE plVareBehNr   AS DECIMAL   NO-UNDO.
  DEFINE VARIABLE pcButikkListe AS CHARACTER NO-UNDO.
  
  DEFINE BUFFER bufVareBehHode FOR VareBehHode.

  /* Bygger butikkliste */
  FOR EACH Butiker NO-LOCK WHERE
    Butiker.ApningsDato <= TODAY AND 
    Butiker.NedlagtDato = ? AND
    Butiker.harButikksystem = TRUE:
    pcButikkListe = pcButikkListe + 
                    (IF pcButikkListe <> '' THEN ',' ELSE '') +
                    string(Butiker.Butik).
  END.

  DO FOR bufVareBehhode TRANSACTION:
    FIND LAST bufVareBehHode NO-LOCK NO-ERROR.
    IF AVAILABLE bufVareBehHode THEN 
      plVareBehNr = bufVareBehHode.VareBehNr + 1.
    ELSE
      plVareBehNr = 1.  
  
    CREATE bufVareBehhode.
    ASSIGN
      bufVareBehhode.VareBehNr          = plVareBehNr
      bufVareBehhode.VareBehBeskrivelse = Messe.MesseBeskrivelse
      bufVareBehhode.MesseNr            = Messe.MesseNr
      bufVareBehhode.ProfilNr           = clButiker.ProfilNr
      bufVareBehhode.VareBehType        = 1 /* Her skal det alltit være type i for forhånd og supplering. */
      bufVareBehHode.ButikkListe        = pcButikkListe
      dVareBehNr                        = bufVareBehHode.VareBehNr
      .

  END. /* TRANSACTION */
  
END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF




