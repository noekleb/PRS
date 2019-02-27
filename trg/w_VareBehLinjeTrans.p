TRIGGER PROCEDURE FOR WRITE OF VareBehLinjeTrans.

IF NOT NEW VareBehLinjeTrans THEN
  {trg\c_w_trg.i &Fil=SkoTex.VareBehLinjeTrans &TYPE=W}


