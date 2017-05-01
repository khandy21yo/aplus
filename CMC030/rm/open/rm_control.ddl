DEFINE RECORD CDD$TOP.RM.RM_CONTROL

        DESCRIPTION IS /*Restaurant Control File*/.

        RM_CONTROL_CDD STRUCTURE.

        /* Element = TRANSTYPE
        Description = Transaction type code for issue */
        TTISSUE                 DATATYPE IS TEXT SIZE IS 2.

        /* Element = TRANSTYPE
        Description = Transaction type code fot receiver */
        TTREC                   DATATYPE IS TEXT SIZE IS 2.

        /* Element = TRANSTYPE
        Description = Transaction type code for promotionals */
        TTPROM                  DATATYPE IS TEXT SIZE IS 2.

        /* Element = TRANSTYPE
        Description = Transaction type code for employee meal */
        TTEMP                   DATATYPE IS TEXT SIZE IS 2.

        /* Element = TRANSTYPE
        Description = Transaction type code for sales units */
        TTSALES                 DATATYPE IS TEXT SIZE IS 2.

        /* Element = TRANSTYPE
        Description = Transaction type code for waste */
        TTWASTE                 DATATYPE IS TEXT SIZE IS 2.

        /* Element = PCTYPE
        Description = Price type code for menu price */
        PRCMENU                 DATATYPE IS TEXT SIZE IS 2.

        /* Element = PCTYPE
        Description = Price type code for indented menu price */
        INDMENU                 DATATYPE IS TEXT SIZE IS 2.

        /* Element = PCTYPE
        Description = Price type code for employee menu price */
        PRCEMP                  DATATYPE IS TEXT SIZE IS 2.

        /* Element = CONTROLFLAG
        Description = Status flag in the control files */
        CONTROLFLAG             DATATYPE IS TEXT SIZE IS 1.

        END RM_CONTROL_CDD STRUCTURE.

END RM_CONTROL.
