DEFINE RECORD CDD$TOP.UTL.UTL_FOB

        DESCRIPTION IS /*FOB Table*/.

        UTL_FOB_CDD STRUCTURE.

        /* Element = CODE
        Description = FOB Code */
        FOBCODE                 DATATYPE IS TEXT SIZE IS 2.

        /* Element = DESCRIPTION
        Description = Description */
        DESCR                   DATATYPE IS TEXT SIZE IS 40.

        END UTL_FOB_CDD STRUCTURE.

END UTL_FOB.
