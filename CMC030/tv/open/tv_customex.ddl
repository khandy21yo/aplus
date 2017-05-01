DEFINE RECORD CDD$TOP.TV.TV_CUSTOMEX

        DESCRIPTION IS /*TV Customer Extra Info File*/.

        TV_CUSTOMEX_CDD STRUCTURE.

        /* Element =
        Description = Customer number */
        CUSNUM                  DATATYPE IS TEXT SIZE IS 10.

        /* Element =
        Description = Salesman number */
        SALESMAN                DATATYPE IS TEXT SIZE IS 10.

        /* Element =
        Description = Agency number */
        AGENCY                  DATATYPE IS TEXT SIZE IS 10.

        /* Element =
        Description = Customer Type */
        CUSTYP                  DATATYPE IS TEXT SIZE IS 2.

        /* Element =
        Description = Bill Type */
        BILTYP                  DATATYPE IS TEXT SIZE IS 2.

        /* Element =
        Description = Representative */
        REP                     DATATYPE IS TEXT SIZE IS 10.

        END TV_CUSTOMEX_CDD STRUCTURE.

END TV_CUSTOMEX.
