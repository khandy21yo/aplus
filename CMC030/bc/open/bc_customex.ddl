DEFINE RECORD CDD$TOP.BC.BC_CUSTOMEX

        DESCRIPTION IS /*Customer Extra Information*/.

        BC_CUSTOMEX_CDD STRUCTURE.

        /* Element = CUSTOMER
        Description = Customer Number */
        CUSNUM                  DATATYPE IS TEXT SIZE IS 10.

        /* Element =
        Description = Customer Type */
        CUSTYP                  DATATYPE IS TEXT SIZE IS 2.

        END BC_CUSTOMEX_CDD STRUCTURE.

END BC_CUSTOMEX.
