      ******************************************************************
      *
      * (C) Copyright 2014 Micro Focus or one of its affiliates.
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
      $set sourceformat(variable) 
       class-id com.microfocus.airport.AirportWrapper.
       
       *>> <summary>
       *>> Constructor.
       *>> Opens the airprots.dat file 
       *>> </summary>
       method-id New 
       local-storage section.
       copy "airparams.cpy" replacing ==(ap-prefix)== by ==ls==.
       01 ls-rec.
       copy "airrec.cpy" replacing ==(prefix)== by ==ap==.
       procedure division using by value filename as string.
           display "dd_airports" upon environment-name
           display filename upon environment-value
           set open-file to true
           call "aircode" using
                                by value ls-function
                                by value ls-airport1
                                by value ls-airport2
                                by value ls-prefix-text
                                by reference ls-rec
                                by reference ls-distance-result
                                by reference ls-matched-codes-array
           goback.
       end method.

       *>> <summary>
       *>> This method finds strings matching (starting with) a particular string.
       *>> </summary>
       *>> <param name="prefix">String to match against.</param>
       *>> <returns>Array of strings.</returns>
       method-id GetMatches.
       local-storage section.
       copy "airparams.cpy" replacing ==(ap-prefix)== by ==ls==.
       01 ls-rec.
       copy "airrec.cpy" replacing ==(prefix)== by ==ap==.
       01 matched-code-length  binary-long.
       procedure division using by value prefix as string
                                         returning return-item as string occurs any.
            set ls-prefix-text to prefix
            set get-matches to true
            call "aircode" using
                                by value ls-function
                                by value ls-airport1
                                by value ls-airport2
                                by value ls-prefix-text
                                by reference ls-rec
                                by reference ls-distance-result
                                by reference ls-matched-codes-array
           set size of return-item to 10
           perform varying i as binary-long from 1 by 1 until i > 10
               perform varying matched-code-length from length of ls-matched-codes(i) by -1
                       until matched-code-length = 0 or ls-matched-codes(i)(matched-code-length:1) not = space
               end-perform
               if matched-code-length > 0
                  set return-item(i) to ls-matched-codes(i)(1:matched-code-length)
               else
                  set return-item(i) to null
               end-if
           end-perform
            goback
       end method.

       *>> <summary>
       *>> Calculate distance between two aiports. The airport records are obtained from the
       *>> name of airport that is passed.
       *>> </summary>
       *>> <param name="airport1">Airport name as string.</param>
       *>> <param name="airport2">Airport name as string.</param>
       *>> <returns>Distance between airports as integer.</returns>
       method-id GetDistance.
       local-storage section.
       copy "airparams.cpy" replacing ==(ap-prefix)== by ==ls==.
       01 ls-rec.
       copy "airrec.cpy" replacing ==(prefix)== by ==ap==.
       procedure division using by value airport-1 as string
                                         airport-2 as string
                      returning return-item as binary-long.

           set ls-airport1 to airport-1
           set ls-airport2 to airport-2
           set get-distance to true

           call "aircode" using
                                   by value ls-function
                                   by value ls-airport1
                                   by value ls-airport2
                                   by value ls-prefix-text
                                   by reference ls-rec
                                   by reference ls-distance-result
                                   by reference ls-matched-codes-array
           set return-item to distance-km as binary-long
           goback
       end method.
       
       *>> <summary>
       *>> Get an airport record for a given airport name.
       *>> </summary>
       *>> <param name="airport">Airport name as string.</param>
       *>> <returns>Airport record as Details object.</returns>
       method-id GetDescription.
       01  a-rec.
       copy "airrec.cpy" replacing ==(prefix)== by ==a==. 

       procedure division using by value airport as string returning details as type AirportDetails.
           if self::FindAirport(airport, a-rec)
               set details to new AirportDetails
               set details::AirportCode to a-code
               set details::AirportName to a-name
               set details::city to a-city
               set details::country to a-country
               set details::latlng to a-lat-sign & a-lat-degs & "." & a-lat-mins 
               & ',' &
               a-long-sign & a-long-degs & "." & a-long-mins
               
           end-if
       end method.


       *>> <summary>
       *>> Convert a given airport record to string.
       *>> </summary>
       *>> <param name="a-rec">Airport record.</param>
       *>> <returns>Airport record in string format.</returns>
       method-id GetRecordDescription private.
       linkage section.
       01  a-rec.
       copy "airrec.cpy" replacing ==(prefix)== by ==a==. 
       
       procedure division using by reference a-rec returning description as string.
           set description to a-code & " " & a-name 
                   & "     " & a-country  
                   & "  Lat:" & a-lat-sign & a-lat-degs & "." & a-lat-mins 
                   & " Lon:" & a-long-sign & a-long-degs & "." & a-long-mins
       end method.


       *>> <summary>
       *>> Check if a given airport name and record exists.
       *>> </summary>
       *>> <param name="airport">Airport name as string.</param>
       *>> <param name="lnk-rec">Airport record.</param>
       *>> <returns>True if found, else false.</returns>
       method-id FindAirport private.
       
       local-storage section.
       copy "airparams.cpy" replacing ==(ap-prefix)== by ==ls==.
       01 ls-rec.
       copy "airrec.cpy" replacing ==(prefix)== by ==ap==.
       linkage section.
       01  lnk-record.
       copy "airrec.cpy" replacing ==(prefix)== by ==a1==.

       procedure division using by value airport as string
                                by reference lnk-record
                                returning found as condition-value.
           set found to false
           set get-details to true
           set ap-code of ls-rec to ""
           set ls-airport1 to airport
           set ls-airport2 to airport
           call "aircode" using
                                by value ls-function
                                by value ls-airport1
                                by value ls-airport2
                                by value ls-prefix-text
                                by reference ls-rec
                                by reference ls-distance-result
                                by reference ls-matched-codes-array
           if ap-code of ls-rec not = ""
           then
               set found to true
               move ls-rec to lnk-record
       end method.
       
       *>> <summary>
       *>> Closes the airprots.dat file 
       *>> </summary>
       method-id Dispose public.
       local-storage section.
       copy "airparams.cpy" replacing ==(ap-prefix)== by ==ls==.
       01 ls-rec.
       copy "airrec.cpy" replacing ==(prefix)== by ==ap==.
       procedure division.
           set close-file to true
           call "aircode" using
                                by value ls-function
                                by value ls-airport1
                                by value ls-airport2
                                by value ls-prefix-text
                                by reference ls-rec
                                by reference ls-distance-result
                                by reference ls-matched-codes-array
           goback.
       end method.

       end class.

       