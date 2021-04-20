FOR EACH Bokforingsbilag EXCLUSIVE-LOCK WHERE 
    BokforingsBilag.SendtRegnskap = TRUE AND 
    BokforingsBilag.OmsetningsDato >= TODAY - 20:

                ASSIGN
                  BokforingsBilag.SendtRegnskap = FALSE
                  BokforingsBilag.SendtDato     = ?
                  BokforingsBilag.SendtTid      = 0 
                  BokforingsBilag.SendAv        = ''
                  .

END.
