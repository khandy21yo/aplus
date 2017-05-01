DEFINE RECORD CDD$TOP.BA.BA_CUSTOMEX

        DESCRIPTION IS /*Customer File Extra Information*/.

        BA_CUSTOMEX_CDD STRUCTURE.

        /* Element = CUSTOMER
        Description = Customer Number */
        CUSNUM                  DATATYPE IS TEXT SIZE IS 10.

        /* Element =
        Description = Customer type */
        CUSTYP                  DATATYPE IS TEXT SIZE IS 2.

        END BA_CUSTOMEX_CDD STRUCTURE.

END BA_CUSTOMEX.
