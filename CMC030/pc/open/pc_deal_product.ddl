DEFINE RECORD CDD$TOP.PC.PC_DEAL_PRODUCT

        DESCRIPTION IS /*Products in a deal*/.

        PC_DEAL_PRODUCT_CDD STRUCTURE.

        /* Element =
        Description = Deal number */
        DEAL                    DATATYPE IS TEXT SIZE IS 20.

        /* Element = PRODUCT
        Description = Product Number */
        PRODUCT                 DATATYPE IS TEXT SIZE IS 14.

        /* Element = AMOUNT
        Description = Price in deal */
        PRICE                   DATATYPE IS G_FLOATING.

        /* Element = PERCENTAGE
        Description = Percentage Discount */
        PERCENT                 DATATYPE IS G_FLOATING.

        /* Element = ACCOUNT
        Description = General Ledger Account Number */
        ACCOUNT                 DATATYPE IS TEXT SIZE IS 18.

        END PC_DEAL_PRODUCT_CDD STRUCTURE.

END PC_DEAL_PRODUCT.
