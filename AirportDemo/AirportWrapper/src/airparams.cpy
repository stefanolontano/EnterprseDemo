      ******************************************************************
      *
      * (C) Copyright 1984-2014 Micro Focus or one of its affiliates.
      *
      * The only warranties for products and services of Micro Focus and
      * its affiliates and licensors ("Micro Focus") are set forth in the
      * express warranty statements accompanying such products and services.
      * Nothing herein should be construed as constituting an additional
      * warranty.  Micro Focus shall not be liable for technical or editorial
      * errors or omissions contained herein.  The information contained
      * herein is subject to change without notice.
      *
      * This sample code is supplied for demonstration purposes only
      * on an "as is" basis and is for use at your own risk.
      *
      ******************************************************************

       01 (ap-prefix)-function                      pic x.
           88 get-matches                           value "1".
           88 get-distance                          value "2".
           88 get-details                           value "3".
           88 open-file                             value "4".
           88 close-file                            value "5".
           88 display-record                        value "6".
       01 (ap-prefix)-airport1                      pic x(3).
       01 (ap-prefix)-airport2                      pic x(3).
       01 (ap-prefix)-prefix-text                   pic x(3).
       01 (ap-prefix)-distance-result.
           03 distance-km               pic zz,zz9.
           03 distance-miles            pic zz,zz9.
       01 (ap-prefix)-matched-codes-array           pic x(300).
       01 (ap-prefix)-matched-codes                 redefines 
          (ap-prefix)-matched-codes-array pic x(30) occurs 10.
