DEFINE RECORD CDD$TOP.IC.IC_CONTROL

        DESCRIPTION IS /*Inventory Control*/.

        IC_CONTROL_CDD STRUCTURE.

        /* Element = ERA
        Description = Era code */
        ERA                     DATATYPE IS TEXT SIZE IS 2.

        /* Element = PERIOD
        Description = Last period closed */
        PERIOD                  DATATYPE IS TEXT SIZE IS 6.

        /* Element = CONTROLFLAG
        Description = Status flag in the control files */
        CONTROLFLAG             DATATYPE IS TEXT SIZE IS 1.

        END IC_CONTROL_CDD STRUCTURE.

END IC_CONTROL.
