  JBoxServerAPI:Instance:LookupDialog("<table>"
                    + ";<field1>"
                    + ";<field2>"
                   ,"WHERE true"
                   ,"<field1>,<field2>"   /* <- return values for these fields */
                    ).

  IF JBoxServerAPI:Instance:LookupOk THEN DO:
    ASSIGN <field1>:SCREEN-VALUE = JBoxServerAPI:Instance:LookupValue(<field1>)
           .

    APPLY "any-printable" TO <field1>.
  END.
