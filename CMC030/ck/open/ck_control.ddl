DEFINE RECORD CDD$TOP.CK.CK_CONTROL

        DESCRIPTION IS /*Check Control File*/.

        CK_CONTROL_CDD STRUCTURE.

        /* Element =
        Description = Year */
        YEAR                    DATATYPE IS TEXT SIZE IS 4.

        /* Element =
        Description = Period */
        PERIOD                  DATATYPE IS SIGNED WORD.

        /* Element =
        Description = Flag */
        FLAG                    DATATYPE IS TEXT SIZE IS 1.

        END CK_CONTROL_CDD STRUCTURE.

END CK_CONTROL.
