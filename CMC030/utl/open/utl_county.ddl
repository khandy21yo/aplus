DEFINE RECORD CDD$TOP.UTL.UTL_COUNTY

        DESCRIPTION IS /*County, Etc.*/.

        UTL_COUNTY_CDD STRUCTURE.

        /* Element = COUNTRY
        Description = Country */
        COUNTRY                 DATATYPE IS TEXT SIZE IS 2.

        /* Element = STATE
        Description = State */
        STATE                   DATATYPE IS TEXT SIZE IS 2.

        /* Element = COUNTY
        Description = County */
        COUNTY                  DATATYPE IS TEXT SIZE IS 2.

        /* Element =
        Description = Description */
        DESCR                   DATATYPE IS TEXT SIZE IS 40.

        END UTL_COUNTY_CDD STRUCTURE.

END UTL_COUNTY.
