DEFINE RECORD CDD$TOP.SS.SS_LICENSE

        DESCRIPTION IS /*Support System Customer Licensing File*/.

        SS_LICENSE_CDD STRUCTURE.

        /* Element = CUSTOMER
        Description = Customer Number */
        CUSNUM                  DATATYPE IS TEXT SIZE IS 10.

        /* Element =
        Description = System License Number */
        LICENSE_NUM             DATATYPE IS TEXT SIZE IS 10.

        /* Element = DATE
        Description = License Expiration Date (YYYYMMDD) */
        EXPIR_DATE              DATATYPE IS TEXT SIZE IS 8.

        /* Element =
        Description = Product (System) Make Number */
        MAKE                    DATATYPE IS TEXT SIZE IS 10.

        /* Element =
        Description = Product (System) Model Number */
        MODEL                   DATATYPE IS TEXT SIZE IS 10.

        /* Element = SERIAL
        Description = Product (System) Serial number */
        SERIAL_NUM              DATATYPE IS TEXT SIZE IS 20.

        /* Element =
        Description = Operating System */
        OPER_SYS                DATATYPE IS TEXT SIZE IS 6.

        /* Element =
        Description = Version Number */
        VERSION                 DATATYPE IS TEXT SIZE IS 6.

        /* Element = PHONE
        Description = Phone number */
        PHONE_NUM               DATATYPE IS TEXT SIZE IS 10.

        /* Element = PHONE_EXTENSION
        Description = Phone Extension */
        PHONE_EXT               DATATYPE IS TEXT SIZE IS 4.

        END SS_LICENSE_CDD STRUCTURE.

END SS_LICENSE.
