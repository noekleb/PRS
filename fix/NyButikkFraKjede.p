&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
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
DEFINE INPUT  PARAMETER dButikkNr LIKE KjedensButikker.ButikkNr  NO-UNDO.
DEFINE INPUT  PARAMETER iModellNr LIKE Kasse.ModellNr            NO-UNDO.

DEFINE BUFFER bKasse FOR Kasse.

DEF VAR piCL AS INT NO-UNDO.

{syspara.i 5 1 5 piCL INT}

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
         WIDTH              = 57.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */
/* IF NOT CAN-FIND(Butiker WHERE Butiker.Butik = dButikkNr) THEN */
DO:
    RUN ButikFraKjedens.
    IF RETURN-VALUE = "AVBRYT" THEN
        RETURN "AVBRYT".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-ButikFraKjedens) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ButikFraKjedens Procedure 
PROCEDURE ButikFraKjedens :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    FIND KjedensButikker WHERE KjedensButikker.ButikkNr = dButikkNr NO-LOCK NO-ERROR.
    IF NOT AVAIL KjedensButikker THEN
        RETURN "AVBRYT".
    DO TRANSACTION:
        FIND Post WHERE Post.Postnr = KjedensButikker.PostNr NO-LOCK NO-ERROR.
        FIND Butiker EXCLUSIVE-LOCK WHERE
            Butiker.Butik = KjedensButikker.ButikkNr NO-ERROR.
        IF NOT AVAILABLE Butiker THEN
        DO:
            CREATE Butiker.
            ASSIGN 
                Butiker.Butik            = KjedensButikker.ButikkNr 
                .

        END.
        ASSIGN
            Butiker.ProfilNr         = 1
            Butiker.ButNamn          = KjedensButikker.ButikkNavn 
            Butiker.KortNavn         = STRING(KjedensButikker.ButikkNr,"99999")
            Butiker.BuAdr            = KjedensButikker.Adresse1 
            Butiker.BuPonr           = KjedensButikker.PostNr 
            Butiker.BuPadr           = IF AVAIL Post THEN Post.Beskrivelse ELSE ""
            Butiker.BuTel            = KjedensButikker.Telefon
            Butiker.BuKon            = KjedensButikker.Kontaktperson 
            Butiker.LanButikk        = FALSE
            Butiker.Sentrallager     = IF KjedensButikker.ButikkNr = piCl
                                         THEN TRUE
                                         ELSE FALSE.
            .
        FIND Gruppe EXCLUSIVE-LOCK WHERE
             Gruppe.ButikkNr = KjedensButikker.ButikkNr AND
             Gruppe.GruppeNr = 1 NO-ERROR.
        IF NOT AVAILABLE Gruppe THEN
        DO:
            CREATE Gruppe.
            ASSIGN Gruppe.ButikkNr   = KjedensButikker.ButikkNr
                   Gruppe.GruppeNr   = 1
                   Gruppe.Navn       = "Gruppe 1".
        END.
        /* Tar bort gamle kassedefinisjoner. */
        FOR EACH Kasse WHERE Kasse.ButikkNr = KjedensButikker.ButikkNr AND
            Kasse.GruppeNr = 1:
            DELETE Kasse.
        END.
        CASE iModellNr:
            WHEN 20 THEN DO:
                CREATE Kasse.
                ASSIGN Kasse.ButikkNr              = KjedensButikker.ButikkNr
                       Kasse.GruppeNr              = 1
                       Kasse.Navn                  = "Kasse 1"
                       Kasse.KasseNr               = 1
                       Kasse.LayoutNr              = 0
                       Kasse.Aktiv                 = yes
                       Kasse.ElJournal[1]          = "LD"
                       Kasse.ElJournal[2]          = "dbf"
                       Kasse.ElJournalId           = STRING(KjedensButikker.ButikkNr,"99999")
                       Kasse.KvitteringId          = "*"
                       Kasse.UtskriftsKopiId       = "*"
                       Kasse.KassererOppgjId       = "*"
                       Kasse.DagsOppgj             = "*"
                       Kasse.ElJournalAktiv        = yes
                       Kasse.KvitteringAktiv       = no
                       Kasse.UtskriftskopiAktiv    = no
                       Kasse.KassererOppgjAktiv    = no
                       Kasse.DagsOppgjAktiv        = no
                       Kasse.ElJournalKatalog      = "c:\home\pressbyran\ankommet\" + STRING(KjedensButikker.ButikkNr,"99999")
                       Kasse.ElJournalKonv         = yes
                       Kasse.KvitteringKonv        = no
                       Kasse.UTskriftskopiKonv     = no
                       Kasse.KassererOppgjKonv     = no
                       Kasse.DagsOppgjKonv         = no
                       Kasse.DagsOppgjId           = "*"
                       Kasse.ElJournalOperand      = 2
                       Kasse.KvitteringOperand     = 1
                       Kasse.UtskriftsKopiOperand  = 1
                       Kasse.KassererOppgjOperand  = 1
                       Kasse.DagsOppgjOperand      = 1
                       Kasse.ElJournalInnles       = "xinnhseljournalpos"
                       Kasse.ElJournalBehandle     = "xbehhseljournalpos"
                       Kasse.ModellNr              = iModellNr.
            END.
            WHEN 30 THEN DO:
                CREATE Kasse.
                ASSIGN Kasse.ButikkNr              = KjedensButikker.ButikkNr
                       Kasse.GruppeNr              = 1                    
                       Kasse.Navn                  = "Kassa 1"            
                       Kasse.KasseNr               = 1                    
                       Kasse.LayoutNr              = 0                    
                       Kasse.Aktiv                 = yes                  
                       Kasse.Kvittering[1]         = "MD"                 
                       Kasse.Kvittering[2]         = "txt"                
                       Kasse.ElJournalId           = "ej"                 
                       Kasse.KvitteringId          = '"' + STRING(KjedensButikker.ButikkNr,"99999") + '"'          
                       Kasse.UtskriftsKopiId       = "*"                  
                       Kasse.KassererOppgjId       = "*"                  
                       Kasse.DagsOppgj             = "*"                  
                       Kasse.ElJournalAktiv        = no                   
                       Kasse.KvitteringAktiv       = yes                  
                       Kasse.UtskriftskopiAktiv    = no                   
                       Kasse.KassererOppgjAktiv    = no                   
                       Kasse.DagsOppgjAktiv        = no                   
                       Kasse.KvitteringKatalog     = "c:\home\pressbyran\ankommet\" + STRING(KjedensButikker.ButikkNr,"99999")
                       Kasse.ElJournalKonv         = no                   
                       Kasse.KvitteringKonv        = yes                  
                       Kasse.UTskriftskopiKonv     = no                   
                       Kasse.KassererOppgjKonv     = no                   
                       Kasse.DagsOppgjKonv         = no                   
                       Kasse.DagsOppgjId           = "*"                  
                       Kasse.ElJournalOperand      = 1                    
                       Kasse.KvitteringOperand     = 4                    
                       Kasse.UtskriftsKopiOperand  = 1                    
                       Kasse.KassererOppgjOperand  = 1                    
                       Kasse.DagsOppgjOperand      = 1                    
                       Kasse.KvitteringInnles      = "xinnmdkvitteringpos"
                       Kasse.KvitteringBehandle    = "xbehmdkvitteringpos"
                       Kasse.ModellNr              = iModellNr.
            END.
        END CASE.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

