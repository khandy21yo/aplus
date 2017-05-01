DEFINE RECORD CDD$TOP.UTL.UTL_MEASURE

        DESCRIPTION IS /*Units Measure Description*/.

        UTL_MEASURE_CDD STRUCTURE.

        /* Element = UOM
        Description = Units of measure code */
        CODE                    DATATYPE IS TEXT SIZE IS 2.

        /* Element =
        Description = Description */
        DESCRIPTION             DATATYPE IS TEXT SIZE IS 20.

        END UTL_MEASURE_CDD STRUCTURE.

END UTL_MEASURE.
