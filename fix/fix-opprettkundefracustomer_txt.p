DEFINE TEMP-TABLE TT_Kunde
/* 1  */ FIELD butnr    AS INTEGER FORMAT ">>9"                   /* I (3)           Butikknr                                    */ 
/* 2  */ FIELD kundenr  AS INTEGER FORMAT ">>>>>9"                /* I (6)           Kundenr                                     */ 
/* 3  */ FIELD slagnr   AS INTEGER FORMAT ">>>9"                  /* I (4)           S-lagsnr                                    */
/* 4  */ FIELD mednr    AS INTEGER FORMAT ">>>>>9"                /* I (6)           Medlemsmr                                   */ 
/* 5  */ FIELD kundegr  AS INTEGER FORMAT "9"                     /* I (1)           Kundegr                                     */ 
/* 6  */ FIELD navn     AS CHARACTER FORMAT "x(50)"               /* C (50)          Kundenavn                                   */ 
/* 7  */ FIELD sperret  AS LOGICAL                                /* L (yes/no)      Flagg for om en kunde er sperret for salg   */ 
/* 8  */ FIELD limit    AS DECIMAL DECIMALS 2 FORMAT ">>>>>9.99"  /* De (6,2)        Kredittgrense                               */ 
/* 9  */ FIELD saldo    AS DECIMAL DECIMALS 2 FORMAT ">>>>>>9.99" /* De (7,2)        Kundens saldo pr siste EOD med endring      */ 
/* 10 */ FIELD aksjon   AS INTE FORMAT "9"                        /* I (1)           Posttype (1=ny/endring, 2=sletting) */
/* 11 */ FIELD adresse  AS CHARACTER FORMAT "x(30)"               /* C (30)          Adresse                                     */ 
/* 12 */ FIELD postnr   AS INTEGER FORMAT ">>>>9"                 /* I (5)           Postnr                                      */ 
/* 13 */ FIELD poststed AS CHARACTER FORMAT "x(30)"               /* C (30)          Poststed                                    */ 
/* 14 */ FIELD dato     AS DATE                                   /* Da                  Dato for siste endring på saldo             */ 
/* 15 */ FIELD kreditt  AS LOGICAL                                /* yes = kreditkunde no = kontantkunde */
         .
DEFINE VARIABLE iButikkNr AS INTEGER    NO-UNDO.
DEFINE VARIABLE cOrg-numFormat AS CHARACTER  NO-UNDO.
ASSIGN cOrg-numFormat = SESSION:NUMERIC-FORMAT.
SESSION:NUMERIC-FORMAT = "American".

iButikkNr = 154.
FIND butiker WHERE butiker.butik = iButikkNr NO-LOCK NO-ERROR.
IF NOT AVAIL butiker THEN DO:
    MESSAGE "Finner ikke butikk " iButikkNr
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
    RETURN.
END.
MESSAGE "Butikk: " iButikkNr butiker.butnamn " Riktig?"
    VIEW-AS ALERT-BOX QUESTION BUTTONS OK-CANCEL UPDATE lOk AS LOGICAL.
IF NOT lOK THEN
    RETURN.
INPUT FROM c:\tmp\customer.txt CONVERT SOURCE 'IBM850'.
REPEAT:
    CREATE TT_Kunde.
    IMPORT TT_Kunde.
END.
IF TT_Kunde.Kundenr = 0 THEN
    DELETE TT_Kunde.
INPUT CLOSE.
SESSION:NUMERIC-FORMAT = cOrg-numFormat.
FOR EACH TT_Kunde.
    CREATE Kunde.
    ASSIGN Kunde.ButikkNr   = iButikkNr
           Kunde.GruppeId   = TT_Kunde.kundegr
           Kunde.Navn       = TT_Kunde.navn   
           Kunde.MaksKredit = TT_Kunde.limit  
           Kunde.Adresse1   = TT_Kunde.adresse
           Kunde.PostNr     = string(TT_Kunde.postnr)
           Kunde.BetType    = 2               
           Kunde.TypeId     = 1.
    /* Vi måste deleta automatuppraæætade kundekort */
    FOR EACH kundekort OF kunde.
        DELETE kundekort.
    END.
    CREATE Kundekort.
    ASSIGN  KundeKort.AktivertDato = TODAY - 180
            KundeKort.Innehaver    = Kunde.Navn
            KundeKort.KortNr       = STRING(TT_Kunde.kundenr)
            KundeKort.KundeNr      = Kunde.Kundenr.
END.      

