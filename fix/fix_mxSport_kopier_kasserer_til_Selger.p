FOR EACH Forsalj WHERE
    /*Forsalj.ForsNr = 172002 AND*/
    NOT CAN-FIND(Selger WHERE Selger.SelgerNr = Forsalj.ForsNr) TRANSACTION:


    DISPLAY
        Forsalj.ForsNr
        Forsalj.FoForNavn
        Forsalj.FoNamn
        .

    CREATE Selger.
    ASSIGN
      Selger.SelgerNr       = Forsalj.ForsNr
      Selger.Navn           = Forsalj.FoNamn  
      Selger.BrukerID       = Forsalj.BrukerID
      Selger.AnsattNr       = Forsalj.AnsattNr
      Selger.Adresse1       = Forsalj.FoAdr
      Selger.Adresse2       = Forsalj.FoAdr2
      Selger.Telefon        = Forsalj.FoTel
      Selger.PersonNr       = Forsalj.FoPersNr
      Selger.Mobiltelefon   = Forsalj.FoTel
      Selger.PostNr         = Forsalj.FoPoNr
      Selger.NavnIKasse     = Forsalj.navnikasse
      Selger.ButikkNr       = Forsalj.ButikkNr
      Selger.BrukeridPRS    = Forsalj.BrukeridPRS
      Selger.ForNavn        = Forsalj.FoForNavn
      Selger.LonnProfil     = Forsalj.LonnProfil
      Selger.ArbeidsProsent = Forsalj.ArbeidsProsent
      Selger.TimeLonn       = Forsalj.TimeLonn
      Selger.FastLonn       = Forsalj.FastLonn
      Selger.AnsattDato     = Forsalj.AnsattDato
      Selger.SluttetDato    = Forsalj.SluttetDato
      Selger.JobTittel      = Forsalj.Jobbtittel
      Selger.FodtDato       = Forsalj.FodtDato
      .   

    FOR EACH ButikkForsalj OF Forsalj NO-LOCK:
        IF NOT CAN-FIND(ButikkSelger WHERE
                        ButikkSelger.SelgerNr = ButikkForsalj.ForsNr AND 
                        ButikkSelger.ButikkNr = ButikkForsalj.Butik AND 
                        ButikkSelger.SelgerId = ButikkForsalj.KassererId) THEN
        DO:
            CREATE ButikkSelger.
            ASSIGN
                ButikkSelger.SelgerNr = ButikkForsalj.ForsNr  
                ButikkSelger.ButikkNr = ButikkForsalj.Butik  
                ButikkSelger.SelgerId = ButikkForsalj.KassererId
                .
        END.
    END.
END.
