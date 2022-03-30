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
	  
           03  (prefix)-code       pic x(4).
           03  (prefix)-name       pic x(30).
           03  (prefix)-city       pic x(30).
           03  (prefix)-country    pic x(20).
           03  (prefix)-geo.
               05  (prefix)-latitude.
                   07  (prefix)-lat-sign     pic x.
                   07  (prefix)-lat-degs     pic 9(3).
                   07  (prefix)-lat-mins     pic 9(6).
               05  (prefix)-longitude.
                   07  (prefix)-long-sign    pic x.
                   07  (prefix)-long-degs    pic 9(3).
                   07  (prefix)-long-mins    pic 9(6).

