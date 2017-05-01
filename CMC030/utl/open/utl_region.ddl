DEFINE RECORD CDD$TOP.UTL.UTL_REGION

        DESCRIPTION IS /*Region Description File*/.

        UTL_REGION_CDD STRUCTURE.

        /* Element = COUNTRY
        Description = Country */
        COUNTRY                 DATATYPE IS TEXT SIZE IS 2.

        /* Element = REGION
        Description = Region number */
        REGION                  DATATYPE IS TEXT SIZE IS 2.

        /* Element = DESCRIPTION
        Description = Description */
        DESCRIPTION             DATATYPE IS TEXT SIZE IS 30.

        END UTL_REGION_CDD STRUCTURE.

END UTL_REGION.
