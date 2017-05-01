DEFINE RECORD CDD$TOP.AP.AP_VENDOR

        DESCRIPTION IS /*Vendor Description*/.

        AP_VENDOR_CDD STRUCTURE.

        /* Element =
        Description = Vendor number */
        VENNUM                  DATATYPE IS TEXT SIZE IS 10.

        /* Element =
        Description = Vendor name */
        VENNAM                  DATATYPE IS TEXT SIZE IS 40.

        /* Element =
        Description = Vendor remittance address line 1 */
        ADD1                    DATATYPE IS TEXT SIZE IS 25.

        /* Element =
        Description = Vendor remittance address line 2 */
        ADD2                    DATATYPE IS TEXT SIZE IS 21.

        /* Element =
        Description = Vendor remittance city */
        CITY                    DATATYPE IS TEXT SIZE IS 15.

        /* Element =
        Description = Vendor remittance state */
        STATE                   DATATYPE IS TEXT SIZE IS 2.

        /* Element =
        Description = Vendor remittance zip */
        ZIP                     DATATYPE IS TEXT SIZE IS 10.

        /* Element =
        Description = Vendor remittance country */
        COUNTRY                 DATATYPE IS TEXT SIZE IS 8.

        /* Element =
        Description = Vendor phone number */
        PHONE                   DATATYPE IS TEXT SIZE IS 10.

        /* Element =
        Description = Vendor address to send PO, line 1 */
        POADD1                  DATATYPE IS TEXT SIZE IS 25.

        /* Element =
        Description = Vendor address to send PO, line 2 */
        POADD2                  DATATYPE IS TEXT SIZE IS 21.

        /* Element =
        Description = Vendor city to send PO */
        POCITY                  DATATYPE IS TEXT SIZE IS 15.

        /* Element =
        Description = Vendor state to send PO */
        POSTATE                 DATATYPE IS TEXT SIZE IS 2.

        /* Element =
        Description = Vendor Zip Code to send PO */
        POZIP                   DATATYPE IS TEXT SIZE IS 10.

        /* Element =
        Description = Vendor Country to send PO */
        POCOUNTRY               DATATYPE IS TEXT SIZE IS 8.

        /* Element =
        Description = Vendor Phone number to send PO */
        POPHONE                 DATATYPE IS TEXT SIZE IS 10.

        /* Element =
        Description = Purge (Y/N) */
        PURGE                   DATATYPE IS TEXT SIZE IS 1.

        /* Element =
        Description = */
        FEDID                   DATATYPE IS TEXT SIZE IS 13.

        /* Element =
        Description = 1099 Flag (Y/N) */
        FLG1099                 DATATYPE IS TEXT SIZE IS 1.

        /* Element =
        Description = Number of days until payment due */
        DUEDAYS                 DATATYPE IS SIGNED WORD.

        /* Element =
        Description = Date payment is due */
        DUEDATE                 DATATYPE IS TEXT SIZE IS 2.

        /* Element =
        Description = Number of days until discount is lost */
        DISDAYS                 DATATYPE IS SIGNED WORD.

        /* Element =
        Description = Date discount is lost */
        DISDATE                 DATATYPE IS TEXT SIZE IS 2.

        /* Element =
        Description = Discount percentage */
        DISCPER                 DATATYPE IS SIGNED WORD.

        /* Element =
        Description = Sort Key */
        ALPSRT                  DATATYPE IS TEXT SIZE IS 15.

        END AP_VENDOR_CDD STRUCTURE.

END AP_VENDOR.
