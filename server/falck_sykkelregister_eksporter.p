/* Registrer Eksporter Falck_Sykkelregister record
   Parameter:  
   Opprettet: 5.4.2011             
-----------------------------------------------------------------------------------*/
DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.

DEFINE VARIABLE cFilNavn AS CHARACTER NO-UNDO.
DEFINE VARIABLE cKatalog AS CHARACTER NO-UNDO.
DEFINE VARIABLE cExtent  AS CHARACTER NO-UNDO.

DEFINE TEMP-TABLE bTGTimeStamp LIKE TGTimeStamp.

DEFINE STREAM Ut.

DEF VAR hQuery          AS HANDLE NO-UNDO.

{syspara.i 1 1 51 cKatalog}
IF cKatalog = '' THEN
  cKatalog = 'c:\home\lindbak\sendes'.
ELSE
  cKatalog = RIGHT-TRIM(cKatalog,'\').

CREATE QUERY hQuery.
hQuery:SET-BUFFERS(ihBuffer).
hQuery:QUERY-PREPARE("FOR EACH " + ihBuffer:NAME + " NO-LOCK").
hQuery:QUERY-OPEN().

hQuery:GET-FIRST().
REPEAT WHILE NOT hQuery:QUERY-OFF-END:
  ASSIGN 
    ocReturn = ''
    obOk     = TRUE
  .

  DO TRANSACTION:
    FIND FIRST FalckEksport WHERE FalckEksport.EksportId = DEC(ihBuffer:BUFFER-FIELD('EksportId'):BUFFER-VALUE)
                         EXCLUSIVE-LOCK NO-ERROR.
    
    IF AVAIL FalckEksport THEN
    DO:
      
      RUN eksportFalck_Sykkelregister.  /* Utlegg av registerinformasjon.    */
    
      ASSIGN
        FalckEksport.EksportDato = TODAY 
        FalckEksport.EksportTid  = TIME 
        FalckEksport.Notat       = FalckEksport.Notat 
                                + (IF FalckEksport.Notat <> '' THEN CHR(10) ELSE '')
                                + 'Eksportert ' + STRING(TODAY) + ' ' + STRING(TIME,"HH:MM:SS") + ' ' + USERID('SkoTex')
        .
        
      obOk = NOT ERROR-STATUS:ERROR.
      IF NOT obOk THEN
      DO:
        ocReturn = ERROR-STATUS:GET-MESSAGE(1).
        LEAVE.
      END.
    END.
  END.
  IF AVAIL FalckEksport THEN RELEASE FalckEksport.
  hQuery:GET-NEXT().
END.



/* **********************  Internal Procedures  *********************** */

PROCEDURE eksportFalck_Sykkelregister:
/*------------------------------------------------------------------------------
		Purpose:  																	  
		Notes:  	
		
		Felt:
		Rammenr;fabrikat;sykkeltype;Aarsmodell;Pris;E_fnvn;E_envn;E_adresse;E_postnr;E_Poststed;E_Telefon;Epost;E_fodt;Dato_solgt;Frst_fnvn;Frst_envn;Frst_fodt;K_kode;v_nokkel;
		
		Fødselsdato og kjøpsdato skal ha følgende format: YYYYMMDD
		Filnavn skal være som følger: Forhandlersnavn+kampanjekode.txt (f.eks Sport1SuperstoreSentrum3316.txt)
																  
------------------------------------------------------------------------------*/
DEFINE VARIABLE cCl AS CHARACTER NO-UNDO.

ASSIGN
  cExtent  = STRING(FalckEksport.ButikkNr)
  cFilNavn = '_POSFalck' + STRING(YEAR(FalckEksport.RegistrertDato),'9999') 
                         + STRING(MONTH(FalckEksport.RegistrertDato),'99')  
                         + STRING(DAY(FalckEksport.RegistrertDato),'99')
                         + '_' 
                         + REPLACE(STRING(FalckEksport.RegistrertTid,'HH:MM:SS'),':','') 
                         + '.' + cExtent.
IF CAN-FIND(FIRST Falck_Sykkelregister OF FalckEksport) THEN
DO:
    OUTPUT STREAM Ut TO VALUE(cKatalog + '\' + cFilNavn) NO-ECHO.
    FOR EACH Falck_Sykkelregister OF FalckEksport NO-LOCK:

      FIND Butiker NO-LOCK WHERE
        Butiker.Butik = INT(Falck_Sykkelregister.ButikkNr) NO-ERROR.
      IF AVAILABLE Butiker THEN 
        cCl = STRING(Butiker.FalckMedlNr).
      ELSE
        cCl = ''.

      PUT STREAM Ut UNFORMATTED
        Falck_Sykkelregister.Rammenummer ';' 
        Falck_Sykkelregister.Fabrikat ';'
        Falck_Sykkelregister.Sykkeltype ';'
        Falck_Sykkelregister.Arsmodell ';'
        Falck_Sykkelregister.Pris ';'
        Falck_Sykkelregister.Eier_Fornavn ';'
        Falck_Sykkelregister.Eier_Etternavn ';'
        Falck_Sykkelregister.Eier_Adresse ';'
        Falck_Sykkelregister.Eier_Postnr ';'
        Falck_Sykkelregister.Eier_Poststed ';'
        (IF Falck_Sykkelregister.Eier_Mobil <> ''
           THEN Falck_Sykkelregister.Eier_Mobil
           ELSE Falck_Sykkelregister.Eier_Telefon) ';'
        Falck_Sykkelregister.Eier_ePost ';'

        (STRING(DAY(Falck_Sykkelregister.Eier_fodt),'99')
         + '.' + STRING(MONTH(Falck_Sykkelregister.Eier_fodt),'99')  
         + '.' + STRING(YEAR(Falck_Sykkelregister.Eier_fodt),'9999')) ';' 
        
        (STRING(DAY(Falck_Sykkelregister.Dato_solgt),'99')
         + '.' + STRING(MONTH(Falck_Sykkelregister.Dato_solgt),'99')  
         + '.' + STRING(YEAR(Falck_Sykkelregister.Dato_solgt),'9999')) ';' 

        Falck_Sykkelregister.Foresatt_Fornavn ';'
        Falck_Sykkelregister.Foresatt_Etternavn ';'

        (STRING(DAY(Falck_Sykkelregister.Foresatt_fodt),'99')
         + '.' + STRING(MONTH(Falck_Sykkelregister.Foresatt_fodt),'99')  
         + '.' + STRING(YEAR(Falck_Sykkelregister.Foresatt_fodt),'9999')) ';' 

        cCl ';'
        '0'
        SKIP. 
    END.
    OUTPUT STREAM Ut CLOSE.  
    
    /* Håndterer tmpfil. */
    RUN flyttTmpFil.
END.

END PROCEDURE.

PROCEDURE flyttTmpFil:
/*------------------------------------------------------------------------------
		Purpose:  																	  
		Notes:  																	  
------------------------------------------------------------------------------*/

/* Gir filen dens riktige navn og tar bort den temporære filen. */
IF SEARCH(cKatalog + '\' + cFilNavn) <> ? THEN 
DO:
  OS-COPY VALUE(cKatalog + '\' + cFilNavn) VALUE(cKatalog + '\' + LEFT-TRIM(cFilNavn,'_')).
  IF SEARCH(cKatalog + '\' + LEFT-TRIM(cFilNavn,'_')) <> ? THEN
      OS-DELETE VALUE(cKatalog + '\' + cFilNavn).
END.

END PROCEDURE.
